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

  def test_BFS() = {
    val segsIdxs = fromArray(Array(0, 1, 4, 6, 7))
    val segsLens = fromArray(Array(1, 3, 2, 1, 1))
    val data = fromArray(Array(1, 0, 2, 3, 1, 4, 1, 2))
    val graph = mkNestedArray(data, segsIdxs zip segsLens)

    // --- Input 1 ---
//    val frontierNodes = fromArray(Array(0))
//    val bfsTree = fromArray(Array(0,-1,-1,-1,-1))
    // assert res equals to (4, 2, 1, 0)
    // ---

    // --- Input 2 ---
    val frontierNodes = fromArray(Array(1))
    val bfsTree = fromArray(Array(-1,1,-1,-1,-1))
    // assert res equals to (4, 2, 1)
    // ---

    val endNode = 4

    val bfsTreeRes = breadthFirstSearch((((graph, frontierNodes), bfsTree), endNode))
    val res = treePath(endNode, bfsTreeRes)
    System.out.println(res)
  }

  def main(arr: Array[String]) = {
    //test_sparseVectorMul()
    test_BFS()
  }
}
