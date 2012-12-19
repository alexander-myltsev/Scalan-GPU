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
  def firstPA[T1: Elem, T2: Elem](arr: PA[(T1, T2)]) = arr map {
    case Pair(a, b) => a
  }

  def secondPA[T1: Elem, T2: Elem](arr: PA[(T1, T2)]) = arr map {
    case Pair(a, b) => b
  }

  lazy val breadthFirstSearch: Rep[((((Graph, FrontierNodes), BFSTree), GraphNode)) => BFSTree] =
    mkLambda((input: Rep[(((Graph, FrontierNodes), BFSTree), GraphNode)]) => {
      val Pair(Pair(Pair(graph, frontierNodes), bfsTree), endNode) = input
      (isEmpty(frontierNodes) || any(frontierNodes map (x => x == endNode))) match {
        case true => bfsTree
        case false =>
          val neighbors: PA[PArray[(GraphNode, GraphNode)]] = {
            val t = graph.backPermute(frontierNodes) zip frontierNodes
            t map {case Pair(nds, idx) => nds map {case nd => (nd, idx)}}
          }
          val next1: PA[(GraphNode, GraphNode)] = neighbors flatMap id
          val next2: PA[(GraphNode, GraphNode)] =
            firstPA((next1 zip (bfsTree.backPermute(firstPA(next1))))
              filter { case Pair(a, b) => b == -1 })

          val bfsTree1: PA[GraphNode] = bfsTree <-- next2

          val next3: PA[GraphNode] =
            firstPA(firstPA(
              next2 zip (bfsTree1.backPermute(firstPA(next2)))
                filter { case Pair(Pair(ne, n), p) => n == p }))

          val input1: Rep[(((Graph, FrontierNodes), BFSTree), GraphNode)] = Pair(Pair(Pair(graph, next3), bfsTree1), endNode)
          breadthFirstSearch(input1)
      }
    })

  lazy val treePath: Rep[((GraphNode, BFSTree)) => PArray[GraphNode]] =
    mkLambda((input: Rep[(GraphNode, BFSTree)]) => {
      val Pair(node, bfsTree) = input
      val next: Rep[GraphNode] = bfsTree.index(node)
      val b: Rep[Boolean] = next == node
      (next == node) match {
        case true => tabulate(1)(x => node) // TO replicate
        case false => tabulate(1)(x => node) ++ treePath(next, bfsTree)
      }
    })
}
