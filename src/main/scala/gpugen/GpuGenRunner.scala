package main.scala.gpugen

import java.io.{FileWriter, ByteArrayOutputStream, PrintWriter}
import scalan.sequential.ScalanSequential

object GpuGenRunner {
  val oGpu = new GpuArrayOperations with GpuGen
  val seq = new ScalanSequential {}

  def main(args: Array[String]): Unit = {
    import seq._

    val f = compile[((oGpu.PArray[oGpu.PArray[(Int, Float)]], oGpu.PArray[Float])) => oGpu.PArray[Float],
      ((seq.PArray[seq.PArray[(Int, Float)]], seq.PArray[Float])) => seq.PArray[Float]](seq)(oGpu.smvm)

    val cols: PA[Int] = fromArray(Array(0, 2, 0, 1, 2, 3))
    val vals: PA[Float] = fromArray(Array(1f, 2f, 3f, 4f, 5f, 6f))
    val rows: PA[(Int, Float)] = cols zip vals
    val segsLen: PA[Int] = fromArray(Array(2, 3, 1))
    val segsIdx: PA[Int] = fromArray(Array(0, 2, 5))
    val segs: PA[(Int, Int)] = segsIdx zip segsLen
    val m: PA[PA[(Int, Float)]] = mkNestedArray(rows, segs)
    val v: PA[Float] = fromArray(Array(1f, 2f, 3f, 4f, 5f))
    val res = f(m, v)
    System.out.println(res)
  }

  def compile[A, B](seq: ScalanSequential)(l: oGpu.Rep[A]): seq.Rep[B] = {
    import oGpu._

    //globDefsArr = globalDefs.toArray

    val bytesStream = new ByteArrayOutputStream
    val stream = new PrintWriter(bytesStream, true) {
      override def println(s: String) = {
        //System.out.println(s)
        super.println(s)
        //System.out.println()
      }

      override def print(s: String) = {
        //System.out.print(s)
        super.print(s)
      }
    }

    val thrust_lib_code = io.Source.fromFile("src/scalan-thrust-lib.cpp")
    stream.println("// ----------------------------------------")
    stream.println("// ----- Scalan-Thrust CUDA/C library -----")
    stream.println("// ----------------------------------------")
    stream.println(thrust_lib_code.mkString)
    thrust_lib_code.close
    stream.println("// ----------------------------------------")

    findDefinition(l.asInstanceOf[Sym[_]]).get.definition.get match {
      case lam: Lambda[_, _] =>
        val tp = lam.y.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[Float]" =>
            "base_array<float>"
        }

        lam.x.Elem.manifest.toString match {
          case "scala.Tuple2[scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]], scalan.dsl.ArraysBase$PArray[Float]]" =>
            stream.println(tp + " fun(const pair<nested_array<pair<int, float> >, base_array<float> >& " + quote(lam.x) + ") {")
        }
        emitBlock(lam.y)(stream)
        stream.println("return " + quote(lam.y) + ";")
        stream.println("}")

        stream.flush
        val programText = new String(bytesStream.toByteArray)
        //System.out.println(programText)

        val fw = new FileWriter("tmp/fun.cpp")
        fw.write(programText)
        fw.flush
        fw.close

      case _ =>
        !!!("Not implemented")
    }

    // Program compilation
    compileCpp()

    val r: ((seq.PArray[seq.PArray[(Int, Float)]], seq.PArray[Float])) => seq.PArray[Float] = (x1: (seq.PArray[seq.PArray[(Int, Float)]], seq.PArray[Float])) => {
      import seq._
      x1 match {
        //    case (x: Float) =>
        //      !!!("not implemented")
        //    case (x: Int) =>
        //      !!!("not implemented")
        //    case (arr: Array[Float]) =>
        //      !!!("Unexpected type")
        //    case (x: Array[Int]) =>
        //      !!!("not implemented")
        case input: ((PArray[PArray[(Int, Float)]], PArray[Float])) =>
          import ThrustLib._

          val m = input._1
          val v = {
            val x = new DeviceVectorFloatPointer()
            input._2.toArray.foreach(v_i => x.push_back(v_i))
            x
          }

          val cols = new DeviceVectorIntPointer()
          val vals = new DeviceVectorFloatPointer()
          val segs = new DeviceVectorIntPointer()
          m.toArray.foreach(parr => {
            segs.push_back(parr.length)
            parr.toArray.foreach(v => {
              cols.push_back(v._1)
              vals.push_back(v._2)
            })
          })

          val resBA: BaseArrayFloat = mainFun1(
            new InputType(
              new NestedArrayPairIntFloat(
                new PairArrayIntFloat(
                  new BaseArrayInt(cols), new BaseArrayFloat(vals)),
                new BaseArrayInt(segs)), new BaseArrayFloat(v)))

          val resA = 0.until(resBA.length().toInt).map(i => resBA.get(i)).toArray
          seq.SeqStdArray(resA)(seq.floatElement)
        case _ =>
          !!!("Unexpected type")
      }
    }
    r.asInstanceOf[B]
  }

  def compileCpp() = {
    val javaLibs = List("charsets.jar", "deploy.jar", "javaws.jar", "jce.jar", "jsse.jar", "management-agent.jar",
      "plugin.jar", "resources.jar", "ext\\dnsns.jar", "ext\\localedata.jar", "ext\\sunec.jar",
      "ext\\sunjce_provider.jar", "ext\\sunmscapi.jar", "ext\\zipfs.jar")
    val scalaLibs = List("out\\production\\Scalan-v2", "lib\\junit-4.10.jar", "lib\\maven-plugin-api-2.0.10.jar",
      "lib\\scala-compiler.jar", "lib\\scala-library.jar")
    val classPath = "-classpath \"" + javaLibs.map(libName => """C:\Java\jdk1.7.0\jre\lib\""" + libName).mkString("", ";", ";") +
      scalaLibs.map(libName => """D:\phd\Scalan-v2\""" + libName).mkString("", ";", "") + "\""

    runProcess(
      """C:\Java\jdk1.7.0\bin\java -Dfile.encoding=UTF-8 """ + classPath +
        """ com.googlecode.javacpp.Builder main.scala.gpugen.ThrustLib """ +
        """-propertyfile D:\phd\Scalan-v2\src\javacpp-thrust-openmp-win-x86_64.properties""")
  }

  def runProcess(procCmd: String) = {
    System.out.println("===== Running: \n" + procCmd)
    val buffer = new Array[Byte](1024)
    val proc = Runtime.getRuntime.exec(procCmd)
    System.out.println("===== Input stream: ")
    Stream.continually(proc.getInputStream.read(buffer)).takeWhile(-1 !=).foreach(len => System.out.print(new String(buffer, 0, len, "UTF-8")))
    System.out.println("===== Error stream: ")
    Stream.continually(proc.getErrorStream.read(buffer)).takeWhile(-1 !=).foreach(len => System.out.print(new String(buffer, 0, len, "UTF-8")))
  }
}
