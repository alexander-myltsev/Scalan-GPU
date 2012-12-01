package main.scala.gpugen

import scala.virtualization.lms.internal.GraphVizExport
import java.io.PrintWriter
import scala.virtualization.lms.GpuArrayOperations

object GraphVizGen {
  val s = new GpuArrayOperations with GraphVizExport {
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

  def main(args: Array[String]): Unit = {
    import s._
    val prefix = "graphs/"
    emitDepGraph(smvm, prefix + "smvm.dot", false)
  }
}