package scala.virtualization.lms

import internal._
import scalan.staged._
import scalan.samples.DslSamples
import scalan.dsl.ArraysBase
import java.io.{FileWriter, ByteArrayOutputStream, PrintWriter}
import scalan.dsl._
import scalan.common.Monoid
import main.scala.gpugen.ThrustLib
import scalan.sequential.ScalanSequential

class GenerationFailedException(msg: String) extends Exception(msg)

trait GpuArrayOperations extends ScalanStaged {
  // TODO: Should be DSL instead of direct graph nodes generation.
  def arraySum[T](s: Sym[Array[T]])(implicit m: Monoid[T]) = ArraySum(s, m)

  def sumLifted[B](s: PA[PArray[B]])(implicit e: Elem[B], m: Monoid[B]) = SumLiftedPA(s, m)

  lazy val smvm = mkLambda((input: Rep[(PArray[PArray[(Int, Float)]], PArray[Float])]) => {
    val m = First(input)
    val v = Second(input)

    val naVals = NestedArrayValues(m)
    val bp = BackPermute(v, FirstPA(naVals))
    val ba = ExpBinopArray(NumericPlus(Const(0f), Const(0f), null), bp, SecondPA(naVals))
    val res: PA[Float] = sumLifted(ExpNestedArray(ba, NestedArraySegments(m)))
    res
  })

  //  lazy val smvm = mkLambda((input: Rep[(PArray[PArray[(Int, Float)]], PArray[Float])]) => {
  //    val m = First(input)
  //    val v = Second(input)
  //  })
}

trait GpuGenImproved extends GenericCodegen {
  self: ArraysBase with StagedImplementation =>

  //var globDefsArr: Array[TP[_]] = null

  def remap[A](m: Manifest[A]): String = m.toString match {
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Unit" => "void"
    case "java.lang.String" => "char *"
    case "Array[Float]" => "device_vector<float>*"
    case "Array[Int]" => "device_vector<int>*"
    case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]]" => "nested_array<pair<int, float> >"
    case "scalan.dsl.ArraysBase$PArray[Float]" => "base_array<float>" // TODO: Fix Thrust-library then this line. Should be 'parray<float>*'
    case _ => throw new GenerationFailedException("CGen: remap(m) : Unknown data type (%s)".format(m.toString))
  }

  override def emitNode(s: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Unit = {
    rhs match {
      case (c: Const[_]) =>
      //stream.println(quote(s) + " = const(" + c.x + ")")

      case (fst: First[_, _]) =>
        val tp = remap(fst.pair.Elem.manifest.typeArguments(0))
        stream.println(tp + " " + quote(s) + " = " + quote(fst.pair) + ".fst();")

      case (snd: Second[_, _]) =>
        val tp = remap(snd.pair.Elem.manifest.typeArguments(1))
        stream.println(tp + " " + quote(s) + " = " + quote(snd.pair) + ".snd();")

      case (sl: SumLiftedPA[_]) =>
        val tp = "base_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(tp + " " + quote(s) + " = sum_lifted(" + quote(sl.source) + ");")

      case (na: ExpNestedArray[_]) =>
        val tp = "nested_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(tp + " " + quote(s) + " = " + tp + "(&" + quote(na.arr) + ", " + quote(na.segments) + ");")

      case (ba: ExpBinopArray[_]) =>
        // TODO: analyse ba.op
        val tp = "base_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(tp + " " + quote(s) + " = binop_array(" + quote(ba.lhs) + ", " + quote(ba.rhs) + ");")

      case (nav: NestedArrayValues[_]) =>
        val tp = nav.nested.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]]" =>
            "pair_array<int, float>"
          case _ => !!!("Unsupported")
        }
        stream.println(tp + " " + quote(s) + " = " + quote(nav.nested) + ".values();")

      case (nas: NestedArraySegments[_]) =>
        val tp = nas.nested.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]]" =>
            "base_array<int>"
          case _ => !!!("Unsupported")
        }
        stream.println(tp + " " + quote(s) + " = " + quote(nas.nested) + ".segments();")

      case (bp: BackPermute[_]) =>
        val tp = "base_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(tp + " " + quote(s) + " = " + quote(bp.x) + ".back_permute(" + quote(bp.idxs) + ");")

      case (fpa: FirstPA[_, _]) =>
        val tp = fpa.source.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]" =>
            "base_array<int>"
          case _ => !!!("Unsupported")
        }
        stream.println(tp + " " + quote(s) + " = " + quote(fpa.source) + ".first();")

      case (spa: SecondPA[_, _]) =>
        val tp = spa.source.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]" =>
            "base_array<float>"
          case _ => !!!("Unsupported")
        }
        stream.println(tp + " " + quote(s) + " = " + quote(spa.source) + ".second();")

      case _ => super.emitNode(s, rhs)
    }
  }

  def remapOp(op: String) = op match {
    case "+" => "thrust::plus"
    case _ => !!!("Unexpected operation")
  }

  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }

  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println("var " + quote(sym) + " = " + rhs)
  }

  def emitAssignment(lhs: String, rhs: String)(implicit stream: PrintWriter): Unit = {
    stream.println(lhs + " = " + rhs)
  }
}

object GpuGenTest {
  val oGpu = new GpuArrayOperations with GpuGenImproved
  val seq = new ScalanSequential {}

  def main(args: Array[String]): Unit = {
    import seq._

    val f = compile[((oGpu.PArray[oGpu.PArray[(Int, Float)]], oGpu.PArray[Float])) => oGpu.PArray[Float],
      ((seq.PArray[seq.PArray[(Int, Float)]], seq.PArray[Float])) => seq.PArray[Float]](seq)(oGpu.smvm)

    val colsArr: Array[Int] = Array(0, 2, 0, 1, 2, 3)
    val cols: PA[Int] = fromArray(colsArr)

    val valsArr: Array[Float] = Array(1f, 2f, 3f, 4f, 5f, 6f)
    val vals: PA[Float] = fromArray(valsArr)

    val rows: PA[(Int, Float)] = cols zip vals

    val segsLenArr: Array[Int] = Array(2, 3, 1)
    val segsLen: PA[Int] = fromArray(segsLenArr)
    val segsIdxArr: Array[Int] = Array(0, 2, 5)
    val segsIdx: PA[Int] = fromArray(segsIdxArr)
    val segs: PA[(Int, Int)] = segsIdx zip segsLen

    val m: PA[PA[(Int, Float)]] = mkNestedArray(rows, segs)

    val vArr: Array[Float] = Array(1f, 2f, 3f, 4f, 5f)
    val v: PA[Float] = fromArray(vArr)

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
