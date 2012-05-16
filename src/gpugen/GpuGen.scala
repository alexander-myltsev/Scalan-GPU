package scala.virtualization.lms

import internal._
import scalan.staged._
import scalan.samples.DslSamples
import scalan.dsl.ArraysBase
import java.io.{FileWriter, ByteArrayOutputStream, PrintWriter}
import scalan.dsl._

/*
trait GpuStagedBase extends DslSamples {
  val scln = new ScalanStaged with ScalanExportGraph { override val isDebug = false }
  import scln._
  val prefix: String
}
*/

class GenerationFailedException(msg: String) extends Exception(msg) {
}

trait GpuGen extends GenericCodegen {
  self: ArraysBase with StagedImplementation =>

  var globDefsArr: Array[TP[_]] = null

  def emitSource[A, B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit eA: Elem[A], eB: Elem[B]): Unit = {

    //stream.println(globalDefs.mkString("globalDefs:[", ", ", "]"))

    val x = fresh[A]
    val y = f(x)

    val sA = eA.manifest.toString
    val sB = eB.manifest.toString

    stream.println(globalDefs.mkString("globalDefs:[\n\t", ",\n\t", "\n]"))
    stream.flush

    stream.println("/*****************************************\n" +
      "  Emitting Generated Code                  \n" +
      "*******************************************/")
    stream.println("class " + className + " extends ((" + sA + ")=>(" + sB + ")) {")
    stream.println("def apply(" + quote(x) + ":" + sA + "): " + sB + " = {")

    emitBlock(y)(stream)
    stream.println(quote(getBlockResult(y)))

    stream.println("}")
    stream.println("}")
    stream.println("/*****************************************\n" +
      "  End of Generated Code                  \n" +
      "*******************************************/")

    stream.flush
  }

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Unit = {
    rhs match {
      case (esa: ExpStdArray[_]) =>
        stream.println("// -----")
        //stream.println(sym.Elem.toString)
        //stream.println(esa.et.toString)
        //stream.println(esa.arr.toString)
        val arrSym = esa.arr.asInstanceOf[Sym[_]] // TODO: any better way?
        stream.print("device_vector<" + remap(esa.manifest) + "> ")
        stream.println(quote(sym) + " = " + quote(arrSym) + ";")
      case (a: Const[_]) =>
        stream.println("// -----")
        def codegen[T](arr: Array[T], tp: String) = {
          stream.println("host_vector<" + tp + "> " + quote(sym) + "(" + arr.length + ");")
          for (i <- 0 until arr.length) {
            stream.write(quote(sym) + "[" + i + "] = " + arr(i) + "f;\n")
          }
        }

        // TODO: Fix it via Elements.
        // ??: How x.isInstanceOf[Array[Int]] works in spite of type erasure?
        a.matchArrayConst(a) {
          codegen(_, "int")
        } {
          codegen(_, "float")
        } {
          codegen(_, "boolean")
        } {
          throw new GenerationFailedException("Don'n know how to generate " + a)
        }
      case eba@ExpBinopArray((np: NumericPlus[_]), s1, s2) =>
        stream.println("// -----")
        //stream.println(np)
        //stream.println(s1)
        //stream.println(s2)
        val tp = remap(eba.elem.manifest)
        val s1q = quote(s1)
        val s2q = quote(s2)
        val op = "thrust::plus<" + tp + ">()" // NOTE: because NumericPlus

        stream.println("device_vector<" + tp + "> " + quote(sym) + "(size);") // TODO: what is size?
        stream.print("thrust::transform(" + s1q + ".begin(), " + s1q + ".end(), ")
        stream.println(s2q + ".begin(), " + quote(sym) + ".begin(), " + op + ");")
      case (al: ArrayLength[_]) =>
        stream.println("al")
        throw new Exception("Not implemented")
      case (aa: ArrayApply[_]) =>
        stream.println("aa")
        throw new Exception("Not implemented")
      case (nt: NumericTimes[_]) =>
        stream.println("nt")
        throw new Exception("Not implemented")
      case (as: ArraySum[_]) =>
        //stream.println(as.m.opName)
        //stream.println(as.m.zero.toString)

        val typeS = as.x.Elem.manifest.toString match {
          // TODO: how to analyse types instead of strings?
          // TODO: Actually result type should be most appropriate of Array[T] and Zero type
          case "Array[Float]" => "float"
          case "Array[Int]" => "int"
        }
        stream.print(typeS + " ")

        as.x.isVar match {
          // TODO: Why 'if (as.x.isVar)' throws cast exception?
          case true =>
            stream.print(quote(sym) + " = ")
            stream.print("thrust::reduce(" + quote(as.x) + "->begin(), " + quote(as.x) + "->end(), ")
            stream.println(as.m.zero.toString + ", " + remapOp(as.m.opName) + "<" + typeS + ">());")
          case false =>
            !!!("Implement it")
        }

      //throw new Exception("Not implemented")
      case (lam: Lambda[_, _]) =>
        stream.println("lambda")
        throw new Exception("Not implemented")
      case _ => super.emitNode(sym, rhs)
    }
    //stream.flush
  }

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
    case "scalan.dsl.ArraysBase$PArray[Float]" => "float*"
    case _ => throw new GenerationFailedException("CGen: remap(m) : Unknown data type (%s)".format(m.toString))
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

trait GpuArrayOperations extends ScalanStaged {
  /*
  def stdArrayOperations(): Rep[Boolean] = {
    val arr = Array(1, 2, 3)
    val parr = fromArray(toRep(arr))
    parr.length == toRep(3) && !(parr.length == toRep(3))
  }
*/

  type Vector = PArray[Float]

  def dotProduct(v1: Rep[Vector], v2: Rep[Vector]): Rep[Float] =
    sum((v1 zip v2) map {
      case Pair(f1, f2) => f1 * f2
    })

  val arr1 = Array(1, 2, 3)
  val arr2 = Array(10, 20, 30)

  val dotP = (arrs: Rep[(Array[Float], Array[Float])]) => {
    val Pair(a, b) = arrs
    val v1 = fromArray(a)
    val v2 = fromArray(b)
    dotProduct(v1, v2)
  }

  val simpleSum = (arr: Rep[Array[Int]]) => {
    val v = fromArray(arr)
    sum(v)
  }

  val simpleUnpair = (arrs: Rep[(Array[Int], Array[(Int, Float)])]) => {
    val Pair(a, b) = arrs
    //val v1 = fromArray(a)
    val v2 = fromArray(b)
    v2
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

object GpuGenGraphTest {
  val oGrp = new GpuArrayOperations with GraphVizExport {
    override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      super.emitNode(sym, rhs)
      rhs match {
        case d: PADef[_] => stream.println("color=blue")
        case arr: ArrayDef[_] => stream.println("color=green")
        case l: Lambda[_, _] => stream.println("color=red")
        case _ =>
      }
    }
  }

  import oGrp._

  def generate[A, B](lam: Rep[A => B])(implicit eA: Elem[A], eB: Elem[B]): Unit = {
    val x = fresh[A]
    val y = lam(x)

    case class Result[T](x: Rep[T]) extends Def[T]
    System.out.println(globalDefs.mkString("globalDefs:[\n\t", ",\n\t", "\n]"))
    emitDepGraph(toExp(Result(y)), "/host/Keldysh/prj/Scalan-v2/tmp/gpugen.dot", false)
    runProcess("dot /host/Keldysh/prj/Scalan-v2/tmp/gpugen.dot -Tpng -O")
  }

  def main(args: Array[String]): Unit = {
    //generate(mkLambda(simpleSum))
    generate(mkLambda(dotP))
  }
}

object GpuGenTest {
  //val scln = new ScalanStaged with GpuGen
  //val scln1 = new Scalan with GraphVizExport

  //import scln._

  val oGpu = new GpuArrayOperations with GpuGen

  import oGpu._

  def main(args: Array[String]): Unit = {
    //compile(oGpu.mkLambda(oGpu.simpleSum)(oGpu.arrayElement[Int], oGpu.intElement))(oGpu.arrayElement[Int], oGpu.intElement)
    val f: (Array[Int]) => Int = compile(mkLambda(simpleSum))
    val arr = (10 to 16).toArray
    val res = f(arr)
    System.out.println(res)

    /*
    val test2 = (a: Rep[Array[Float]]) => {
      val v1 = fromArray(Array(1f, 2f, 3f, 4f, 5f, 6f, 7f))
      val v2 = fromArray(a)
      binPlus(v1, v2)
    }
    */

    /*
    val test3 = (a: Rep[Float]) => {
      a + 3f
    }
    compile(mkLambda(test3))
    */

    //val fexpr: Rep[Array[Float] => PArray[Float]] = mkLambda(test2)
    // some user defined function for example dotP
    //val fexpr: Rep[Pair[Array[Float], Array[Float]] => Float] = mkLambda(dotP)
    //val fexpr: Rep[(Array[Float], Array[Float]) => Float] = mkLambda(dotP) // TODO: Why compilation Error?

    //val f: Pair[Array[Float], Array[Float]] => Float = compile(fexpr) // thus compile just removes Rep[_]
    //val f: (Array[Float]) => PArray[Float] = compile(fexpr) // thus compile just removes Rep[_]
    //val arr = (10 to 16).map(_.toFloat).toArray
    //val res = f(arr)
    //System.out.println(res)
  }

  def generateFunSignature(sx: Sym[_], eRes: Elem[_])(implicit stream: PrintWriter): Unit = {
    // TODO: Any better way to analyze types?
    // TODO: Should be pattern: stream.println(remap(RES_TYPE_str) + " test(" + remap(IN_TYPE_str) + ") {")
    eRes.manifest.erasure.getSimpleName match {
      case "int" => stream.print(remap(eRes.manifest) + " ")
      case _ => !!!("Unexpected. Implement it")
    }

    sx.Elem.manifest.erasure.getSimpleName match {
      case "int[]" => stream.println("test(" + remap(sx.Elem.manifest) + " " + quote(sx) + ") {")
      case _ => !!!("Unexpected type")
    }

    /*
    // NOTE: Does not work because of erasure :(
    sx.Elem match { 
      case (p: PairElem[_, _]) =>
        !!!("TODO: implement Pair")
      //generateFunSignature(p.ea, eRes)
      //generateFunSignature(p.eb, eRes)
      case (f: Element[Float]) =>
        !!!("TODO: implement Float")
      case (f: Element[Int]) => stream.println("test(int " + quote(sx) + ") {")
      //case (i: Elem[Int]) => stream.println("test(int " + quote(sx) + ") {")
      case (arr: Elem[Array[_]]) =>
        val xq = quote(sx)
        stream.println("""extern "C" """ + remap(eRes.manifest) + " fun(" + remap(sx.Elem.manifest) + " " + xq + "_tmp, int size, int& out_size) {")
        stream.println("host_vector<int> " + xq + "(" + xq + "_tmp, " + xq + "_tmp + size);")
      case t =>
        !!!("Unexpected type " + t)
    }
    */
  }

  //  import oGpu._

  def compile[A, B](lam: Rep[A => B])(implicit eA: Elem[A], eB: Elem[B]): A => B = {

    val bytesStream = new ByteArrayOutputStream
    val stream = new PrintWriter(bytesStream, true) {
      override def println(s: String) = {
        //System.out.println(s)
        super.println(s)
        System.out.println()
      }

      override def print(s: String) = {
        System.out.print(s)
        super.print(s)
      }
    }
    //val stream = new PrintWriter(System.out, true)
    //stream.println(globalDefs.mkString("globalDefs:[", ", ", "]"))

    val x = fresh[A]
    val y: Rep[B] = lam(x)

    System.out.println("x: " + x.toString)
    System.out.println("y: " + y.toString)
    System.out.println(globalDefs.mkString("globalDefs:[\n\t", ",\n\t", "\n]"))

    //val sA = eA.manifest.toString
    //val sB = eB.manifest.toString

    globDefsArr = globalDefs.toArray

    stream.println("/*****************************************\n" +
      "  Emitting Generated Code                  \n" +
      "*******************************************/")

    stream.println( """
#include <thrust/device_vector.h>
#include <thrust/transform.h>
#include <thrust/sequence.h>
#include <thrust/copy.h>
#include <thrust/fill.h>
#include <thrust/replace.h>
#include <thrust/functional.h>
#include <iostream>

using namespace thrust;
                    """)

    generateFunSignature(x, eB)(stream)
    emitBlock(y)(stream)
    val resq = quote(getBlockResult(y))
    //    stream.println("out_size = " + resq + ".size();")
    //    stream.println("float* " + resq + "_tmp = new float[out_size];")
    //    stream.println("thrust::copy(" + resq + ".begin(), " + resq + ".end(), " + resq + "_tmp);")
    //    stream.println("return " + resq + "_tmp;")
    stream.println("return " + resq + ";")

    // Test output
    /*
    stream.println("""
device_vector<int>* test(device_vector<int>* x) {
  device_vector<int>* x_out = new device_vector<int>(x->size());
  thrust::transform(x->begin(), x->end(), x_out->begin(), thrust::negate<int>());
  //cout << "from C++: " << flush;
  //for (int i = 0; i < x_out->size(); i++) {
  //    cout << (*x_out)[i] << " " << flush;
  //}
  return x_out;""")
  */

    stream.println("}")
    stream.println("/*****************************************\n" +
      "  End of Generated Code                  \n" +
      "*******************************************/")

    // stream.flush // autoflush <- true

    val programText = new String(bytesStream.toByteArray)
    //System.out.println(programText)
    val fw = new FileWriter("tmp/fun.cu")
    fw.write(programText)
    fw.flush
    fw.close

    //runProcess("nvcc --ptxas-options=-v --compiler-options '-fPIC' -o tmp/fun.so --shared tmp/fun.cu")
    runProcess("/usr/lib/jvm/java-oracle-jdk1.7.0_01/bin/java -Dfile.encoding=UTF-8 -classpath /usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/plugin.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/deploy.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/rt.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/jce.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/javaws.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/charsets.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/alt-rt.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/jsse.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/management-agent.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/resources.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/ext/dnsns.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/ext/localedata.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/ext/sunec.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/ext/sunpkcs11.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/ext/zipfs.jar:/usr/lib/jvm/java-oracle-jdk1.7.0_01/jre/lib/ext/sunjce_provider.jar:/host/Keldysh/prj/Scalan-v2/out/production/Scalan-v2:/host/Keldysh/prj/scala-virtualized-pack/pack/lib/jline.jar:/host/Keldysh/prj/scala-virtualized-pack/pack/lib/scala-compiler.jar:/host/Keldysh/prj/scala-virtualized-pack/pack/lib/scala-dbc.jar:/host/Keldysh/prj/scala-virtualized-pack/pack/lib/scala-library.jar:/host/Keldysh/prj/scala-virtualized-pack/pack/lib/scala-partest.jar:/host/Keldysh/prj/scala-virtualized-pack/pack/lib/scala-swing.jar:/host/Keldysh/prj/scala-virtualized-pack/pack/lib/scalacheck.jar:/host/Keldysh/prj/scala-virtualized-pack/pack/lib/scalap.jar:/host/Keldysh/prj/Scalan-v2/lib/junit-4.10.jar:/host/Keldysh/prj/Scalan-v2/lib/maven-plugin-api-2.0.10.jar:/home/kate/idea-IC-117.105/lib/idea_rt.jar com.googlecode.javacpp.Builder gpugen.ThrustLib -properties linux-x86_64-cuda")

    // How to use ThrustLib.java
    //val vp_in = new gpugen.ThrustLib.DeviceVectorPointer(Array[Int](1, 2, 3, 4, 5))
    val vp_in = new gpugen.ThrustLib.DeviceVectorPointer
    for (i <- 0 to 5) {
      System.out.print(vp_in.get(i) + " ")
    }
    (0 to 128).foreach(x => vp_in push_back x)
    val val_out = gpugen.ThrustLib.test(vp_in)
    /*
        val vp_out = gpugen.ThrustLib.test(vp_in)
        for (i <- 0 to 127) {
          System.out.print(vp_out.get(i) + " ")
        }
    */

    val r: A => B = (x: A) => {
      x match {
        case (x: Float) =>
          !!!("not implemented")
        case (x: Int) =>
          !!!("not implemented")
        case (arr: Array[Float]) =>
          /*
          val len_out = new IntByReference(0)
          val res = CLibrary.FunLibInstance.fun(arr, arr.length, len_out)
          val arr_out = res.getPointer.getFloatArray(0, len_out.getValue)
          arr_out.asInstanceOf[B] // TODO: How to return PArray(Array(Float))
          */
          !!!("Unexpected type")
        case (x: Array[Int]) =>
          val vp_in = new gpugen.ThrustLib.DeviceVectorPointer()
          (0 to 128).foreach(x => vp_in push_back x) // TODO: replace (0 to 128) with x.
          val out = gpugen.ThrustLib.test(vp_in)
          System.out.println(out)
          !!!("not implemented") // TODO: return out
        case _ =>
          !!!("Unexpected type")
      }
    }
    r
  }

  /*
  // JNA Deprecated
  trait CLibrary extends Library {
    def fun(a: Array[Float], len: Int, len_out: IntByReference): FloatByReference
  }

  object CLibrary {
    def FunLibInstance = Native.loadLibrary("tmp/fun.so", classOf[CLibrary]).asInstanceOf[CLibrary]
  }
  */

  /*
  def test() = {
    val a1 = Array(10f, 20f, 30f)
    val a2 = Array(10f, 10f, 10f)

    val dotP = (arrs: Rep[(Array[Float], Array[Float])]) => {
      val Pair(a, b) = arrs
      val v1 = fromArray(a)
      val v2 = fromArray(b)
      dotProduct(v1, v2)
    }

    val rowinds = Array(0, 1)
    val smvm = (arrs: Rep[(Array[Int], (Int, Float))]) => {
      val Pair(inds, Pair(len, v)) = arrs
      val rowinds = fromArray(inds)
      val rowvals = replicate(len, v)
      val row = rowinds zip (rowvals)
      val matr = replicate(toRep(2), row)
      val vec = replicate(toRep(2), toRep(0.5f))
      matrixVectorMul(matr, vec)
    }

    emitDepGraph(vectorOfSquares(10), prefix + "vectorOfSquares.dot", false)
    emitDepGraph(vectorOfSquares(10).toArray, prefix + "vectorOfSquaresToArray.dot", false)
    emitDepGraph(dotP(Pair(a1, a2)), prefix + "dotProduct.dot", false)
    val rowinds2 = fromArray(rowinds)
    emitDepGraph(rowinds2, prefix + "fromArray.dot", false)

    val rowvals = replicate(2, 0.5f)
    emitDepGraph(rowvals, prefix + "replicate.dot", false)

    val row = rowinds2 zip (rowvals)
    emitDepGraph(row, prefix + "zip.dot", false)

    val matr = replicate(2, row)
    emitDepGraph(matr, prefix + "replicateRow.dot", false)

    emitDepGraph(smvm(Pair(rowinds, Pair(toRep(2), toRep(0.5f)))), prefix + "matrixVectorMul.dot", false)

  }
*/

}

