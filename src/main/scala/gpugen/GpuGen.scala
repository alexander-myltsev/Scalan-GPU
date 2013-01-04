package main.scala.gpugen

import scala.virtualization.lms.internal.GenericCodegen
import scalan.dsl.ArraysBase
import scalan.staged.StagedImplementation
import java.io.PrintWriter

trait GpuGen extends GenericCodegen {
  self: ArraysBase with StagedImplementation =>

  class GenerationFailedException(msg: String) extends Exception(msg)

  var globDefsArr: Array[TP[_]] = null

  def remap[A](m: Manifest[A]): String = m.toString match {
    //case "scala.Tuple2[scala.Tuple2[scala.Tuple2[scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[Int]], scalan.dsl.ArraysBase$PArray[Int]], scalan.dsl.ArraysBase$PArray[Int]], Int]"
    case "Int" => "int"
    case "Long" => "long"
    case "Float" => "float"
    case "Double" => "double"
    case "Boolean" => "bool"
    case "Unit" => "void"
    case "java.lang.String" => "char *"
    case x if x.startsWith("scala.Tuple2") => "pair<" + remap(m.typeArguments(0)) + ", " + remap(m.typeArguments(1)) + ">"

    case "Array[Float]" => "device_vector<float>*"
    case "Array[Int]" => "device_vector<int>*"

    case "scalan.dsl.ArraysBase$PArray[Boolean]" => "base_array<bool>"
    case "scalan.dsl.ArraysBase$PArray[Int]" => "base_array<int>"
    case "scalan.dsl.ArraysBase$PArray[Float]" => "base_array<float>" // TODO: Fix Thrust-library then this line. Should be 'parray<float>*'

    case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[Int]]" => "nested_array<int>"
    case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]]" => "nested_array<pair<int, float> >"

    case _ => throw new GenerationFailedException("CGen: remap(m) : Unknown data type (%s)".format(m.toString))
  }

  override def emitNode(s: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Unit = {
    stream.println("// " + rhs)
    rhs match {
      case (c: Const[_]) =>
        val typ = "int"
        stream.println(typ + " " + quote(s) + " = " + c.x + ";")

      case (vpa: VarPA[_]) =>
        val typ = remap(vpa.a.Elem.manifest)
        stream.println(typ + " " + quote(s) + " = " + quote(vpa.a) + ";")

      case (fst: First[_, _]) =>
        val typ = remap(fst.pair.Elem.manifest.typeArguments(0))
        stream.println(typ + " " + quote(s) + " = " + quote(fst.pair) + ".fst();")

      case (snd: Second[_, _]) =>
        val typ = remap(snd.pair.Elem.manifest.typeArguments(1))
        stream.println(typ + " " + quote(s) + " = " + quote(snd.pair) + ".snd();")

      case (sl: SumLiftedPA[_]) =>
        val typ = "base_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(typ + " " + quote(s) + " = sum_lifted(" + quote(sl.source) + ");")

      case (na: ExpNestedArray[_]) =>
        val typ = "nested_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(typ + " " + quote(s) + " = " + typ + "(&" + quote(na.arr) + ", " + quote(na.segments) + ");")

      case (ba: ExpBinopArray[_]) =>
        // TODO: analyse ba.op
        val typ = "base_array<float>" // TODO: Fix generic type as it can be not 'float'
        stream.println(typ + " " + quote(s) + " = binop_array(" + quote(ba.lhs) + ", " + quote(ba.rhs) + ");")

      case (nav: NestedArrayValues[_]) =>
        val typ = remap(nav.nested.Elem.manifest)
        stream.println(typ + " " + quote(s) + " = " + quote(nav.nested) + ".values();")

      case (nas: NestedArraySegments[_]) =>
        val typ = nas.nested.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]]" =>
            "base_array<int>"
          case _ => !!!("Unsupported")
        }
        stream.println(typ + " " + quote(s) + " = " + quote(nas.nested) + ".segments();")

      case (bp: BackPermute[_]) =>
        val typ = remap(bp.x.Elem.manifest)
        stream.println(typ + " " + quote(s) + " = " + quote(bp.x) + ".back_permute(" + quote(bp.idxs) + ");")

      case (fpa: FirstPA[_, _]) =>
        val typ = fpa.source.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]" =>
            "base_array<int>"
          case _ => !!!("Unsupported")
        }
        stream.println(typ + " " + quote(s) + " = " + quote(fpa.source) + ".first();")

      case (spa: SecondPA[_, _]) =>
        val typ = spa.source.Elem.manifest.toString match {
          case "scalan.dsl.ArraysBase$PArray[scala.Tuple2[Int, Float]]" =>
            "base_array<float>"
          case _ => !!!("Unsupported")
        }
        stream.println(typ + " " + quote(s) + " = " + quote(spa.source) + ".second();")

      case (lenpa: LengthPA[_]) =>
        stream.println("int " + quote(s) + " = " + quote(lenpa.arr) + ".length();")

      case (eq: Equal[_, _]) =>
        stream.println("bool " + quote(s) + " = " + quote(eq.a) + " == " + quote(eq.b) + ";")

      case (reppa: ReplicatePA[_]) =>
        // TODO: extend code generation for more types, not only for int
        stream.println("base_array<int> " + quote(s) + " = base_array<int>(" + quote(reppa.count) + ", " + quote(reppa.v) + ");")

      case (ebaEq: ExpBinopArrayEquals[_]) =>
        stream.println("base_array<bool> " + quote(s) + " = binop_array_equal(" + quote(ebaEq.a) + ", " + quote(ebaEq.b) + ");")

      case (flgSplt: FlagSplit[_]) =>
        stream.println("pair<base_array<float>, base_array<float> > " + quote(s) + " = " + quote(flgSplt.arr) + ".flag_split(" + quote(flgSplt.flags) + ");");

      case (notLg: Not) =>
        stream.println("bool " + quote(s) + " = !(" + quote(notLg.lhs) + ");")

      case (orLg: Or) =>
        stream.println("bool " + quote(s) + " = (" + quote(orLg.lhs) + "||" + quote(orLg.rhs) + ");")

      case (expandBy: ExpandBy[_, _]) =>
        // TODO: Generalize from base_array<int>
        stream.println("base_array<int> " + quote(s) + " = " + quote(expandBy.source) + ".expand_by(" + quote(expandBy.nested) + ");")

      case (pairArr: PairArray[_, _]) =>
        // TODO: Generalize from pair_array<int, int>
        stream.println("pair_array<int, int> " + quote(s) + "(" + quote(pairArr.a) + ", " + quote(pairArr.b) + ");")

      case (wrt: WritePA[_]) =>
        // TODO: Generalize from base_array<int>
        stream.println("base_array<int> " + quote(s) + " = " + quote(wrt.a) + ".write_pa(" + quote(wrt.vals) + ");")

      case (ifArr: ExpIfArray[_]) =>
        val typ = "base_array<int>"
        stream.println(typ + " " + quote(s) + ";")
        stream.println("if (" + quote(ifArr.cond) + ") " + quote(s) + " = " + quote(ifArr.thenp) + " else " + quote(s) + " = " + quote(ifArr.elsep) + ";")

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
