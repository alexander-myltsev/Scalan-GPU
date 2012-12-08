package main.scala.gpugen

import scalan.sequential.ScalanSequential

object BFSSeqTests {
  val samples = new ScalanSequential with Samples
  import samples._

  def test_sparseVectorMul() = {
    val sparseVector = fromArray(Array(0, 2, 3)) zip fromArray(Array(1f, 2f, 3f))
    val vector = fromArray(Array(1f, 2f, 3f, 4f))
    val res = sparseVectorMul(sparseVector, vector)
    assert(math.abs(res - 19f) < 0.01f)
  }

  def main(arr: Array[String]) = {
    test_sparseVectorMul()
  }
}
