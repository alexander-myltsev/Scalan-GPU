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
  type FrontierNodes = PArray[GraphNode]
  type BFSTree = PArray[GraphNode]

  def id[T](x: Rep[T]) = x
  def isEmpty[T](arr: Rep[PArray[T]]) = arr.length == 0
  def any(arr: Rep[PArray[Boolean]]) = !isEmpty(arr filter id)
  //def indexPA[T:Elem](arr: Rep[PArray[T]], idxs: Rep[PArray[Int]]) = idxs map {case i => arr.index(i)}
  def firstPA[T1:Elem, T2:Elem](arr: PA[(T1, T2)]) = arr map {case Pair(a, b) => a}
  def secondPA[T1:Elem, T2:Elem](arr: PA[(T1, T2)]) = arr map {case Pair(a, b) => b}

  lazy val breadthFirstSearch: Rep[((((Graph, FrontierNodes), BFSTree), GraphNode)) => BFSTree] =
    mkLambda((input: Rep[(((Graph, FrontierNodes), BFSTree), GraphNode)]) => {
    val Pair(Pair(Pair(graph, frontierNodes), bfsTree), endNode) = input
    (frontierNodes.length == 0 || any(frontierNodes map (x => x == endNode))) match {
      case true => bfsTree
      case false =>
        val neighbors: PA[PArray[(GraphNode, GraphNode)]] =
          frontierNodes map {case idx => graph.index(idx) map {case neighNode => (neighNode, idx)}} // TODO: Replace PA indexing
        val next1: PA[(GraphNode, GraphNode)] = neighbors flatMap id
        val next2: PA[(GraphNode, GraphNode)] =
          firstPA((next1 zip (firstPA(next1) map {case neighNd => bfsTree.index(neighNd)})) // TODO: Replace PA indexing
                  filter {case Pair(a, b) => b == -1})

        // NESL '<-' function
        val bfsTree1: PA[GraphNode] = {
          var tempArr: Array[Int] = (bfsTree.toArray).asInstanceOf[Array[Int]]
          next2.toArray.asInstanceOf[Array[(Int,Int)]] map {case ((a:Int), (b:Int)) => tempArr(a) = b}
          fromArray(tempArr)
        }

        val next3: PA[GraphNode] =
          firstPA(firstPA(
          next2 zip (firstPA(next2) map {case neighNd => bfsTree1.index(neighNd)}) // TODO: Replace PA indexing
          filter {case Pair(Pair(ne, n), p) => n == p}))

        val input1: Rep[(((Graph, FrontierNodes), BFSTree), GraphNode)] = Pair(Pair(Pair(graph, next3), bfsTree1), endNode)
        breadthFirstSearch(input1)
    }
  })
}
