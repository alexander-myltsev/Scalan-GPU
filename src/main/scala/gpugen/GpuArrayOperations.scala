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
    val nArr1: PA[PArray[Float]] = mkNestedArray(ba, m.segments)
    val res: PA[Float] = sumLifted(nArr1)
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

//  zs = xs zip ys map { (x,y) => x + y - 10 }
//  val zs = xs |+| ys |-| replicate(xs.length, 10)
//
//  ys = xs filter { case x => x != 0 }
//  val ys = (xs flagSplit (xs |!=| replicate(xs.length, 0)))._1
//
//  xs.expandBy(ns)  //  xs.length == ns.length
//  xs.nestBy(segments)      // ExpNestedArray(xs, segments)

  lazy val breadthFirstSearch =
    letrec((bfs: Rep[((((Graph, FrontierNodes), BFSTree), GraphNode)) => BFSTree]) =>
           (input: Rep[((((Graph, FrontierNodes), BFSTree), GraphNode))]) => {
      val Pair(Pair(Pair(graph, frontierNodes), bfsTree), endNode) = input
      if (isEmpty(frontierNodes) || any(frontierNodes map (x => x == endNode))) bfsTree
      else {
        val neighbors: PA[PArray[(GraphNode, GraphNode)]] = {
          val t = graph.backPermute(frontierNodes) zip frontierNodes
          t map {
            case Pair(nds, idx) => nds map {
              case nd => (nd, idx)
            }
          }
        }
        val next1: PA[(GraphNode, GraphNode)] = neighbors flatMap id
        val next2: PA[(GraphNode, GraphNode)] =
          firstPA((next1 zip (bfsTree.backPermute(firstPA(next1))))
            filter {
            case Pair(a, b) => b == -1
          })

        val bfsTree1: PA[GraphNode] = bfsTree <-- next2

        val next3: PA[GraphNode] =
          firstPA(firstPA(
            next2 zip (bfsTree1.backPermute(firstPA(next2)))
              filter {
              case Pair(Pair(ne, n), p) => n == p
            }))

        bfsTree1
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