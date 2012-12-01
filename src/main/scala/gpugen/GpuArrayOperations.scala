package main.scala.gpugen

import scalan.staged.ScalanStaged
import scalan.common.Monoid

trait GpuArrayOperations extends ScalanStaged {
  // TODO: Should be DSL instead of direct graph nodes generation.
  def arraySum[T](s: Sym[Array[T]])(implicit m: Monoid[T]) = ArraySum(s, m)

  def sumLifted[B](s: PA[PArray[B]])(implicit e: Elem[B], m: Monoid[B]) = SumLiftedPA(s, m)

  lazy val smvm = mkLambda((input: Rep[(PArray[PArray[(Int, Float)]], PArray[Float])]) => {
    val m = First(input)
    val v = Second(input)

    val naVals = NestedArrayValues(m)
    val bp = BackPermute(v, FirstPA(naVals))
    val ba = ExpBinopArray(NumericPlus(Const(0f), Const(0f), null), bp, SecondPA(naVals))
    val res: PA[Float] = sumLifted(ExpNestedArray(ba, NestedArraySegments(m)))
    res
  })

  // ------------------------------------

  type VectorElem = (Int,Float)
  type SparseVector = PArray[VectorElem]
  type Vector = PArray[Float]
  type Matrix = PArray[SparseVector]

  lazy val sparseVectorMul = mkLambda((input: Rep[(SparseVector, Vector)]) => input match {
      case Pair(sv, v) => sum(sv map { case Pair(i,value) =>  v(i) * value })
  })

  lazy val matrixVectorMul = mkLambda((input: Rep[(Matrix, Vector)]) => input match {
    case Pair(mat, vec) => mat map {row => sparseVectorMul(row, vec)}
  })
}
