package main.scala.gpugen

import scalan.dsl.Scalan

trait Samples extends Scalan {
  type VectorElem = (Int, Float)
  type SparseVector = PArray[VectorElem]
  type Vector = PArray[Float]

  lazy val sparseVectorMul = mkLambda((input: Rep[(SparseVector, Vector)]) => {
    val Pair(sv, v) = input
    sum(sv map {
      case Pair(i, value) => v(i) * value
    })
  })

  type GraphNode = Int
  type Graph = PArray[PArray[GraphNode]]
  type FrontierNodes = PArray[Int]
  type BFSTree = PArray[Int]

  val any = (arr: Rep[PArray[Boolean]]) => (arr filter (x => x)).length > 0

  lazy val breadthFirstSearch = mkLambda((input: Rep[(((Graph, FrontierNodes), BFSTree), Int)]) => {
    val Pair(Pair(Pair(graph, frontierNodes), bfsTree), endNode) = input
    if (frontierNodes.length == 0 || any(frontierNodes map (x => x == endNode)))
      bfsTree
    else
      bfsTree
  })
}
