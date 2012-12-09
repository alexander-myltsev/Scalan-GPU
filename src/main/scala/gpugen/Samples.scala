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

  def id[T](x: Rep[T]) = x
  def isEmpty[T](arr: Rep[PArray[T]]) = arr.length == 0
  def any(arr: Rep[PArray[Boolean]]) = !isEmpty(arr filter id)
  def indexPA[T:Elem](arr: Rep[PArray[T]], idxs: Rep[PArray[Int]]) = idxs map {case i => arr.index(i)}

  lazy val breadthFirstSearch = mkLambda((input: Rep[(((Graph, FrontierNodes), BFSTree), Int)]) => {
    val Pair(Pair(Pair(graph, frontierNodes), bfsTree), endNode) = input
    (frontierNodes.length == 0 || any(frontierNodes map (x => x == endNode))) match {
      case true => bfsTree
      case false =>
        val neighbors = indexPA(graph, frontierNodes)
        val next = frontierNodes zip neighbors
        !!!("Not implemented")
    }
  })
}
