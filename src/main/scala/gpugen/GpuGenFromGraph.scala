// NOTE: This is implementation when SVMV operation is encoded in form of FlatGraph nodes
package scala.virtualization.lms

import internal._
import scalan.staged._
import scalan.samples.DslSamples
import scalan.dsl.ArraysBase
import java.io.{FileWriter, ByteArrayOutputStream, PrintWriter}
import scalan.dsl._
import scalan.common.Monoid
import main.scala.gpugen.ThrustLib

class GenerationFailedException(msg: String) extends Exception(msg)

trait GpuArrayOperations extends ScalanStaged {
  // TODO: Should be DSL instead of direct graph nodes generation.
  def arraySum[T](s: Sym[Array[T]])(implicit m: Monoid[T]) = ArraySum(s, m)

  def sumLifted[B](s: PA[PArray[B]])(implicit e: Elem[B], m: Monoid[B]) = SumLiftedPA(s, m)

  def smvm = {
    val input = fresh[Pair[ PArray[PArray[Pair[Int, Float]]], PArray[Float] ]]

    val m = First(input)
    val v = Second(input)

    val naVals = NestedArrayValues(m)
    val bp = BackPermute(v, FirstPA(naVals))
    val ba = ExpBinopArray(NumericPlus(Const(0f), Const(0f), null), bp, SecondPA(naVals))
    val res: SumLiftedPA[Float] = sumLifted(ExpNestedArray(ba, NestedArraySegments(m)))

    val lam = Lambda(null, input, res)
    lam
  }
}

trait GpuGenImproved extends GenericCodegen {
  self: ArraysBase with StagedImplementation =>

  var globDefsArr: Array[TP[_]] = null

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

  def find[T](x: Exp[T]) = {
    x.isVar match {
      case false => findDefinition(x.asInstanceOf[Sym[T]])
      case true => None // TODO: Should be not None
    }
  }

  override def emitNode(s: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Unit = {
    rhs match {
      case (c: Const[_]) =>

      case (lam: Lambda[_, _]) =>
        find(lam.y) match {
          case Some(y) =>
            stream.println("header for " + lam.x.toString)
            emitNode(y.sym, y.rhs)
          case None => !!!("Error")
        }
      case (sl: SumLiftedPA[_]) =>
        find(sl.source) match {
          case Some(x) =>
            stream.println(quote(s) + " = sum_lifted(" + quote(sl.source) + ")")
            emitNode(x.sym, x.rhs)
          case None => !!!("Error")
        }
      case (na: ExpNestedArray[_]) =>
        (find(na.arr), find(na.segments)) match {
          case (Some(arr), Some(segs)) =>
            stream.println(quote(s) + " = nested_array(" + quote(na.arr) + ", " + quote(na.segments) + ")")
            emitNode(arr.sym, arr.rhs)
            emitNode(segs.sym, segs.rhs)
          case _ => !!!("Error")
        }
      case (ba: ExpBinopArray[_]) => {
        (find(ba.lhs), find(ba.rhs)) match {
          case (Some(lhs), Some(rhs)) =>
            stream.println(quote(s) + " = binop_array(" + ba.op + ", " + quote(ba.lhs) + ", " + quote(ba.rhs) + ")")
            emitNode(lhs.sym, lhs.rhs)
            emitNode(rhs.sym, rhs.rhs)
          case _ => !!!("Error")
        }
      }
      case (nav: NestedArrayValues[_]) => {
        find(nav.nested) match {
          case Some(x) => !!!("Unimplemented case 1")
          case None =>
            stream.println(quote(s) + " = nested_arr_vals(" + quote(nav.nested) + ")")
        }
      }
      case (nas: NestedArraySegments[_]) => {
        find(nas.nested) match {
          case Some(x) =>
            !!!("Unimplemented case")
          case None =>
            stream.println(quote(s) + " = nested_arr_segs(" + quote(nas.nested) + ")")
        }
      }
      case (bp: BackPermute[_]) => {
        (find(bp.x), find(bp.idxs)) match {
          case (None, Some(x)) =>
            stream.println(quote(s) + " = back_permute(" + quote(bp.x) + ", " + quote(bp.idxs) + ")")
            emitNode(x.sym, x.rhs)
          case _ => !!!("Unimplemented case")
        }
      }
      case (fpa: FirstPA[_, _]) => {
        find(fpa.source) match {
          case Some(x) =>
            stream.println(quote(s) + " = first_pa(" + quote(fpa.source) + ")")
            emitNode(x.sym, x.rhs)
          case None => !!!("Unimplemented case 2")
        }
      }
      case (spa: SecondPA[_, _]) => {
        find(spa.source) match {
          case Some(x) =>
            stream.println(quote(s) + " = second_pa(" + quote(spa.source) + ")")
            emitNode(x.sym, x.rhs)
          case None => !!!("Unimplemented case 2")
        }
      }
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

  import oGpu._

  def main(args: Array[String]): Unit = {
    //    val f: (Array[Int]) => Int = compile(mkLambda(smvm))
    //val f: (PArray[PArray[(Int,Float)]], PArray[Float]) => PArray[Float] = (x, y) => compile(smvm(x)(y))
    val f = compile1(smvm)
    val arr = (10 to 16).toArray
    //    val res = f(arr)
    //    System.out.println(res)
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

  def compile1[A, B](lam: Lambda[A, B]) = {
    globDefsArr = globalDefs.toArray

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

    //emitNode(null, lam)(stream)
    emitBlock(lam.y)(stream)

    stream.flush
    val programText = new String(bytesStream.toByteArray)
    System.out.println(programText)
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
