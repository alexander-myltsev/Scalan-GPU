import java.io.PrintWriter
import scala.virtualization.lms.internal.GraphVizExport
import scalan.common.Semigroup
import scalan.samples.DslSamples
import scalan.staged.ScalanStaged

object AdvancedLiftingTests {

  val sc = new DslSamples with ScalanStaged with GraphVizExport {
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
  import sc._

  val prefix = "tmp/advanced-lifting/"

  def runProcess(procCmd: String) = {
    System.out.println("===== Running: \n" + procCmd)
    val buffer = new Array[Byte](1024)
    val proc = Runtime.getRuntime.exec(procCmd)
    System.out.println("===== Input stream: ")
    Stream.continually(proc.getInputStream.read(buffer)).takeWhile(-1 !=).foreach(len => System.out.print(new String(buffer, 0, len, "UTF-8")))
    System.out.println("===== Error stream: ")
    Stream.continually(proc.getErrorStream.read(buffer)).takeWhile(-1 !=).foreach(len => System.out.print(new String(buffer, 0, len, "UTF-8")))
  }

  def sparseMatrixVectorMul_Tests = {
    val s = mkLambda2((vec: Rep[Vector]) => (cx: Rep[(Int, Float)]) => vec(cx._1) * cx._2)
    emitDepGraph(s, prefix + "s.dot", false)
    runProcess("dot " + prefix + "s.dot -Tpng -O")

    val l = mkLambda2((vecs: PA[Vector]) => (row: Rep[SparseVector]) => {
      val mulV = vectorizeBinOp(NumericTimes(0f, 0f, implicitly[Numeric[Float]]))
      val sumV = vectorizeBinOp(NumericPlus(0, 0, implicitly[Numeric[Int]]))
      val inds = row.fst
      val mulL = replicate(inds.length, mulV) //TODO: overcome requirement to use inds.length
      val sumL = replicate(inds.length, sumV) // or this is not a problem for automatically lifted code
      // since length is in the lifting context
      mulL(vecs.indexLifted(sumL(inds)(replicate(inds.length, 1))))(row.snd)
    })
    emitDepGraph(l, prefix + "l.dot", false)
    runProcess("dot " + prefix + "l.dot -Tpng -O")

    val clos = mkLambda2((e: Rep[Vector]) => (row:Rep[SparseVector]) => {
      val fV = Clo(e, s, l)
      val inds = row.fst
      val fL = replicate(inds.length, fV)
      sum(fL(row))
    })
    emitDepGraph(clos, prefix + "clos.dot", false)
    runProcess("dot " + prefix + "clos.dot -Tpng -O")

    val clol = mkLambda2((vecs: PA[Vector]) => (sm: PA[SparseVector]) => {
      val fL = mkFuncArray(vecs, s, l)
      val m2 = sm mapLifted fL
      m2.sumLifted
    })
    emitDepGraph(clol, prefix + "clol.dot", false)
    runProcess("dot " + prefix + "clol.dot -Tpng -O")

    val smvm = mkLambda2((m:Rep[Matrix]) => (v: Rep[Vector]) => {
      val fV = Clo(v, clos, clol)
      val fL = replicate(m.length, fV)
      fL(m)
    })
    emitDepGraph(smvm, prefix + "smvm.dot", false)
    runProcess("dot " + prefix + "smvm.dot -Tpng -O")

    //    val clolL = mkLambda2((vecss: PA[PArray[Vector]]) => (sms: PA[Matrix]) => {
    //      val fL = mkFuncArray(vecss, clos, clol)
    //      val m2 = sm mapLifted fL
    //      m2
    //    })

    val smvmL = mkLambda2((ms:PA[Matrix]) => (vs: PA[Vector]) => {
      val fL = mkFuncArray(vs, clos, clol)
      val ms2 = ms mapLifted fL
      ms2
      //      val fsV = Clo(vs, clol, clolL)
      //      val fsL = replicate(ms.length, fsV)
      //      doApplyPA(fsL, ms)
    })
    emitDepGraph(smvmL, prefix + "smvmL.dot", false)
    runProcess("dot " + prefix + "smvmL.dot -Tpng -O")

    val matrVecs = mkLambda2((m:Rep[Matrix]) => (vs: PA[Vector]) => vs map { v => smvm(m)(v) })
    emitDepGraph(matrVecs, prefix + "matrVecs.dot", false)
    runProcess("dot " + prefix + "matrVecs.dot -Tpng -O")

    val matrVecsL = mkLambda2((m:Rep[Matrix]) => (vs: PA[Vector]) => {
      val fV = Clo(m, smvm, smvmL)
      val fL = replicate(vs.length, fV)
      fL(vs)
    })
    emitDepGraph(matrVecsL, prefix + "matrVecsL.dot", false)
    runProcess("dot " + prefix + "matrVecsL.dot -Tpng -O")
  }

  def main(args: Array[String]) = {
    sparseMatrixVectorMul_Tests
  }
}
