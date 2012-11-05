package scala.virtualization.lms

import internal._
import scalan.staged._
import scalan.samples.DslSamples
import scalan.dsl.ArraysBase
import java.io.{FileWriter, ByteArrayOutputStream, PrintWriter}
import scalan.dsl._
import scalan.common._

object FlatGraphGen {
  val s = new ScalanStaged with GraphVizExport {
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

  import s._

  def runProcess(procCmd: String) = {
    System.out.println("===== Running: \n" + procCmd)
    val buffer = new Array[Byte](1024)
    val proc = Runtime.getRuntime.exec(procCmd)
    System.out.println("===== Input stream: ")
    Stream.continually(proc.getInputStream.read(buffer)).takeWhile(-1 !=).foreach(len => System.out.print(new String(buffer, 0, len, "UTF-8")))
    System.out.println("===== Error stream: ")
    Stream.continually(proc.getErrorStream.read(buffer)).takeWhile(-1 !=).foreach(len => System.out.print(new String(buffer, 0, len, "UTF-8")))
  }

  def arraySum[T](s: Sym[Array[T]])(implicit m: Monoid[T]) = ArraySum(s, m)

  def sumLifted[B](s: PA[PArray[B]])(implicit e: Elem[B], m: Monoid[B]) = SumLiftedPA(s, m)

  // TODO: Should be DSL instead of direct graph nodes generation.
  def simpleSum = {
    val x = fresh[Array[Int]]
    val lam = Lambda(null, x, arraySum(x))
    lam
  }

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

  def main(args: Array[String]): Unit = {
    //val r = simpleSum
    val r = svmv

    val prefix = "tmp/"
    emitDepGraph(toExp(r), prefix + "flatgraph.dot", false)
    runProcess("dot " + prefix + "flatgraph.dot -Tpng -O")
  }
}
