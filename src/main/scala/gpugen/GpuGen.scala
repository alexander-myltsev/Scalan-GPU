package main.scala.gpugen

import scala.virtualization.lms.internal.GenericCodegen
import scalan.dsl.ArraysBase
import scalan.staged.StagedImplementation
import java.io.PrintWriter

trait GpuGen extends GenericCodegen {
  self: ArraysBase with StagedImplementation =>

  class GenerationFailedException(msg: String) extends Exception(msg)

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
