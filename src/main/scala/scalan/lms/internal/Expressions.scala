package scala.virtualization.lms
package internal

import common.Base
import scalan.dsl.ArraysBase
import annotation.unchecked.uncheckedVariance

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions { self: ArraysBase =>

  //override def __equal(expr1: Any, expr2: Any): Boolean = expr1 equals expr2
  /**
   * constants/symbols (atomic)
   */
  abstract class Exp[+T] {
    def Elem: Elem[T @uncheckedVariance]
    private[lms] var isRec = false
    def isRecursive: Boolean = isRec
    private[lms] def isRecursive_=(b: Boolean) = { isRec = b }

    def isVar: Boolean = this match {
      case Def(_) => false
      case _ => true
    }

    def isConst: Boolean = this match {
      case Def(Const(_)) => true
      case _ => false
    }
    def asSymbol = this.asInstanceOf[Sym[T]]
  }

  /**
   * operations (composite) including constants
   */
  abstract class Def[+T] {
    def name = getClass.getSimpleName
  }

  case class Const[T](x: T) extends Def[T] {
    override def hashCode: Int = (41 + x.hashCode)

    //TODO: this is a hack since direct pattern matching with arrays don't work as expected for some reason
    def matchArrayConst[A](arr: Def[_])
                       (ai: Array[Int] => A)
                       (af: Array[Float] => A)
                       (ab: Array[Boolean] => A)
                       (orElse: => A): A =
      arr match {
        case Const(x) if x.isInstanceOf[Array[Int]] => ai(x.asInstanceOf[Array[Int]])
        case Const(x) if x.isInstanceOf[Array[Float]] => af(x.asInstanceOf[Array[Float]])
        case Const(x) if x.isInstanceOf[Array[Boolean]] => ab(x.asInstanceOf[Array[Boolean]])
        case _ => orElse
      }

    override def equals(other: Any) =
      other match {
        case that: Const[_] => matchArrayConst(that)
          { i => matchArrayConst(this) { y => i sameElements y } { _ => false } { _ => false } { this.x equals that.x }}
          { f => matchArrayConst(this) { _ => false } { y => f sameElements y } { _ => false } { this.x equals that.x }}
          { b => matchArrayConst(this) { _ => false } { _ => false } { y => b sameElements y } { this.x equals that.x }}
          { this.x equals that.x }
        case _ => false
      }

    override def toString = matchArrayConst(this)
      { _.mkString("(",", ",")") }
      { _.mkString("(",", ",")") }
      { _.mkString("(",", ",")") }
      { getClass.getSimpleName + "(" + x + ")" }
  }

  abstract class BinOp[T] extends Def[T] {
    def lhs: Rep[T]
    def rhs: Rep[T]
    def copyWith(l: Rep[T], r:Rep[T]): BinOp[T]
    override def toString = this.getClass.getSimpleName + "(" + lhs + ", " + rhs + ")"
  }

  /**
   * A Sym is a symbolic reference used internally to refer to expressions.
   */
  object Sym { private var currId = 0 }
  case class Sym[+T](id: Int = {Sym.currId += 1; Sym.currId})
                    (implicit et: Elem[T]) extends Exp[T]
  {
    override def Elem: Elem[T @uncheckedVariance] = et

    override def toString = {
      val res = isDebug match {
        case false => {
          val s = "Sym(" + id + ")"
          val suffix = isVar match {
            case true => ": " + Elem.name
            case _ => ""
          }
          s + suffix
        }
        case _ =>
          val rhs = findDefinition(this) match { case Some(TP(_, d)) => "->" + d.toString case _ => "" }
          "Sym(" + id + ")" + rhs
      }
      res.replace("scalan.dsl.ArraysBase$", "").replace("scala.math.Numeric$", "").replace("scala.Tuple2", "Tuple2")
    }
    lazy val definition = findDefinition(this).map(_.rhs)
  }
  def fresh[T](implicit et: Elem[T]) = new Sym[T]()

  class TP[T](val sym: Sym[T], val definition: Option[Def[T]]) {
    def rhs: Def[T] = definition.getOrElse(evaluate)
    def evaluate: Def[T] = !!!("invalid definition " + this, sym.asInstanceOf[Rep[T]])

    override def toString = "TP[" + sym + ": " + rhs + "]"
  }
  object TP {
    def apply[T](sym: Sym[T])(eval: => Def[T]) = new TP(sym, None) { override def evaluate = eval }
    def apply[T](sym: Sym[T], rhs: Def[T]) = new TP(sym, Some(rhs))
    def unapply[T](tp: TP[T]): Option[(Sym[T], Def[T])] = Some((tp.sym, tp.rhs))
  }

  var globalDefs: List[TP[_]] = Nil

  def findDefinition[T](s: Sym[T]): Option[TP[T]] =
    globalDefs.find(_.sym == s).asInstanceOf[Option[TP[T]]]

  def findDefinition[T](d: Def[T]): Option[TP[T]] =
    globalDefs.find(_.rhs == d).asInstanceOf[Option[TP[T]]]

//  def findOrCreateDefinition[T](d: Def[T]): TP[T] =
//    findDefinition[T](d).getOrElse {
//      createDefinition(fresh[T], d)
//    }

  def createDefinition[T](s: Sym[T], d: Def[T]): TP[T] = {
    val f = TP(s, d)
    globalDefs = globalDefs:::List(f)
    f
  }

  def rewrite[T](d: Def[T])(implicit eT: Elem[T]): Exp[_] = {
    rewriteRules.foreach(r =>
      r.lift(d) match {
        case Some(e) => return e
        case _ =>
      })
    null
  }

  var rewriteRules = List[PartialFunction[Def[_], Exp[_]]]()

  def addRules(rules: PartialFunction[Def[_], Exp[_]]) {
    rewriteRules ::= rules
  }
  //def rewriteExp[T](e: Exp[T]): Exp[_] = e

//  implicit def toExp[T](d: Def[T])(implicit et: Elem[T]): Exp[T] = {
//    findDefinition(d) match {
//      case Some(TP(s, _)) => s
//      case None => {
//        val TP(fsym, _) = createDefinition(fresh[T], d)
//        var contender: Exp[T] = fsym
//        var res = rewrite(contender).asInstanceOf[Exp[T]]
//        while (res != contender) {
//          contender = res
//          res = rewrite(contender).asInstanceOf[Exp[T]]
//        }
//        res
//      }
//    }
//  }

  implicit def toExp[T](d: Def[T])(implicit et: Elem[T]): Exp[T] = {
    findDefinition(d) match {
      case Some(TP(s, _)) => s
      case None =>
        var ns = rewrite(d)
        ns match {
          case null =>
            val TP(res, _) = createDefinition(fresh[T], d)
            res
          case _ => {
            val res = ns match {
              case Var(_) => ns
              case Def(newD) => {
                implicit val eAny = ns.Elem
                toExp(newD)
              }
            }
            res.asInstanceOf[Exp[T]]
          }
        }
    }
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).map(_.rhs)
      case _ =>
        None
    }
//    def unapply[T](e: Exp[T]): Option[(Def[T], Elem[T])] = {
//      val d = unapply(e)
//      (d, e.Elem)
//    }
  }
  object Var {
    def unapply[T](e: Exp[T]): Option[Sym[T]] = e match {
      case s: Sym[_] if findDefinition(s).isEmpty => Some(s)
      case _ => None
    }
  }


  // dependencies
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[_] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def dep(e: Exp[Any]): List[Sym[Any]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }
  def dep(e: Def[Any]): List[Sym[Any]] = e match {
    case d: Product => syms(d)
    case _ => Nil
  }

}
