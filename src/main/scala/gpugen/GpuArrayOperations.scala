package main.scala.gpugen

import scalan.staged.ScalanStaged
import scalan.common.Monoid

trait GpuArrayOperations extends ScalanStaged {
  type VectorElem = (Int, Float)
  type SparseVector = PArray[VectorElem]
  type Vector = PArray[Float]
  type Matrix = PArray[SparseVector]

  def sumLifted[B](s: PA[PArray[B]])(implicit e: Elem[B], m: Monoid[B]) = SumLiftedPA(s, m)

  def binopArr[A](lhs: PA[A], rhs: PA[A])(implicit e: Elem[A]) =
    ExpBinopArray(NumericPlus[A](null, null, null), lhs, rhs)

  lazy val smvm = mkLambda((input: Rep[(Matrix, Vector)]) => {
    val Pair(m, v) = input
    val naVals = m.values
    val bp = v.backPermute(naVals.fst)
    val ba = binopArr(bp, naVals.snd)
    val res: PA[Float] = sumLifted(mkNestedArray(ba, m.segments))
    res
  })

  // ------------------------------------

  lazy val sparseVectorMul = mkLambda((input: Rep[(SparseVector, Vector)]) => {
    val Pair(sv, v) = input
    sum(sv map {
      case Pair(i, value) => v(i) * value
    })
  })

  lazy val matrixVectorMul = mkLambda((input: Rep[(Matrix, Vector)]) => {
    val Pair(mat, vec) = input
    mat map {
      row => sparseVectorMul(row, vec)
    }
  })
}