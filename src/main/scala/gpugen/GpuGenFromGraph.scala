// NOTE: SMVM operation is encoded in form of FlatGraph nodes

package scala.virtualization.lms

import internal._
import scalan.staged._
import scalan.samples.DslSamples
import scalan.dsl.ArraysBase
import java.io.{FileWriter, ByteArrayOutputStream, PrintWriter}
import scalan.dsl._
import scalan.common.Monoid
import main.scala.gpugen.ThrustLib

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
  // TODO: Should be DSL instead of direct graph nodes generation.
  def svmv = {
    val x = fresh[PArray[PArray[Pair[Int, Float]]]]
    val y = fresh[PArray[Float]]
    val mRow = fresh[PArray[Float]]

    val narrVals = NestedArrayValues(x)
    val narrValsFst = FirstPA(narrVals)
    val repl = ReplicatePA(LengthPA(narrValsFst), Const(1))
    val expBinop = ExpBinopArray(NumericPlus(Const(0), Const(0), null), repl, narrValsFst)
    val backPerm = BackPermute(mRow,expBinop)
    val narrValsSnd = SecondPA(narrVals)
    val parr = ExpBinopArray(NumericPlus(Const(0f), Const(0f), null), backPerm, narrValsSnd)
    val segments = NestedArraySegments(x)
    val narr = ExpNestedArray(parr, segments)
    //val sumL = SumLiftedPA(narr, scalan.common.Monoid.monoid)
    val sumL = sumLifted(narr)

    val lam1 = Lambda(null, y, sumL)
    val lam = Lambda(null, x, lam1)
    lam
  }
}

object GpuGenTest {
  val oGpu = new GpuArrayOperations with GpuGen

  import oGpu._

  def main(args: Array[String]): Unit = {
    val f: (Array[Int]) => Int = compile(mkLambda(simpleSum))
    val arr = (10 to 16).toArray
    val res = f(arr)
    System.out.println(res)
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
    // NOTE: Does not work because of erasure
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
          val vp_in = new ThrustLib.DeviceVectorPointer()
          (0 to 128).foreach(x => vp_in push_back x) // TODO: replace (0 to 128) with x.
          val out = ThrustLib.test(vp_in)
          System.out.println(out)
          !!!("not implemented") // TODO: return out
        case _ =>
          !!!("Unexpected type")
      }
    }
    r
  }
}
