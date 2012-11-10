import java.io.PrintWriter
import scala.virtualization.lms.GenerationFailedException
import scala.virtualization.lms.internal.GenericCodegen
import scalan.dsl.ArraysBase
import scalan.staged.StagedImplementation

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
          throw new GenerationFailedException("Don't know how to generate " + a)
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
