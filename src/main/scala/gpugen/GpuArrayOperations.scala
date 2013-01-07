package main.scala.gpugen

import scalan.staged.ScalanStaged
import scalan.common.Monoid

trait GpuArrayOperations extends ScalanStaged {
  type VectorElem = (Int, Float)
  type SparseVector = PArray[VectorElem]
  type Vector = PArray[Float]
  type Matrix = PArray[SparseVector]

  def sumLifted[B](s: PA[PArray[B]])(implicit e: Elem[B], m: Monoid[B]) = SumLiftedPA(s, m)

//  def binopArr[A](lhs: PA[A], rhs: PA[A])(implicit e: Elem[A]) =
//    ExpBinopArray(NumericPlus[A](null, null, null), lhs, rhs)

//  lazy val smvm = mkLambda((input: Rep[(Matrix, Vector)]) => {
//    val Pair(m, v) = input
//    val naVals = m.values
//    val bp = v.backPermute(naVals.fst)
//    val ba = binopArr(bp, naVals.snd)
//    val res: PA[Float] = sumLifted(mkNestedArray(ba, m.segments))
//    res
//  })

  lazy val smvm = mkLambda((input: Rep[(Matrix, Vector)]) => {
    val Pair(m, v) = input
    val naVals: PA[(Int, Float)] = m.values
    val bp: PA[Float] = v.backPermute(naVals.fst)
    val ba: PA[Float] = bp |+| naVals.snd
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

  // ------------------------------------

  //  zs = xs zip ys map { (x,y) => x + y - 10 }
  //  val zs = xs |+| ys |-| replicate(xs.length, 10)
  //
  //  ys = xs filter { case x => x != 0 }
  //  val ys = (xs flagSplit (xs |!=| replicate(xs.length, 0)))._1
  //
  //  xs.expandBy(ns)  //  xs.length == ns.length
  //  xs.nestBy(segments)      // ExpNestedArray(xs, segments)

  type GraphNode = Int
  type Graph = PArray[PArray[GraphNode]]
  type FrontierNodes = PArray[GraphNode]
  type BFSTree = PArray[GraphNode]

  def isEmpty[T](arr: PA[T]) = arr.length == 0
  def any(arr: PA[Boolean]) = !isEmpty((arr flagSplit arr)._1)

  lazy val breadthFirstSearch =
    letrec((bfs: Rep[((((Graph, FrontierNodes), BFSTree), GraphNode)) => BFSTree]) =>
           (input: Rep[((((Graph, FrontierNodes), BFSTree), GraphNode))]) => {
      val Pair(Pair(Pair(graph, frontierNodes), bfsTree), endNode) = input
      if (isEmpty(frontierNodes) || any(frontierNodes |==| replicate(frontierNodes.length, endNode))) bfsTree
      else {
        val neighbors: PA[PArray[GraphNode]] = graph.backPermute(frontierNodes)
        val next1: PA[(GraphNode, GraphNode)] = neighbors.values zip (frontierNodes.expandBy(neighbors))
        val next2: PA[(GraphNode, GraphNode)] = {
          val t1 = bfsTree.backPermute(next1.fst)
          val t2 = t1 |==| replicate(t1.length, -1)
          (next1 flagSplit t2)._1
        }

        val bfsTree1: PA[GraphNode] = bfsTree <-- next2

        val next3: PA[GraphNode] = {
          val t1 = next2.snd |==| (bfsTree1.backPermute(next2.fst))
          (next2.fst flagSplit t1)._1
        }

        val input1: Rep[(((Graph, FrontierNodes), BFSTree), GraphNode)] = Pair(Pair(Pair(graph, next3), bfsTree1), endNode)
        bfs(input1)
      }
    })

  lazy val treePath: Rep[((GraphNode, BFSTree)) => PArray[GraphNode]] =
    mkLambda((input: Rep[(GraphNode, BFSTree)]) => {
      val Pair(node, bfsTree) = input
      val next: Rep[GraphNode] = bfsTree.index(node)
      val b: Rep[Boolean] = next == node
      if (next == node) tabulate(1)(x => node)
      else tabulate(1)(x => node) ++ treePath(next, bfsTree)
    })
}