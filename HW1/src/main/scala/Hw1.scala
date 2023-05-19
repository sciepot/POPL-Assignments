sealed trait IntList
case object Nil extends IntList
case class Cons(v: Int, t: IntList) extends IntList

sealed trait BTree
case object Leaf extends BTree
case class IntNode(v: Int, left: BTree, right: BTree)
extends BTree

sealed trait Formula
case object True extends Formula
case object False extends Formula
case class Not(f: Formula) extends Formula
case class Andalso(left: Formula, right: Formula) extends Formula
case class Orelse(left: Formula, right: Formula)  extends Formula
case class Implies(left: Formula, right: Formula) extends Formula

object Hw1 extends App {

  println("Hw1!")

  def fibo(n: Int): Int = {
    if (n < 2) 1
    else fibo(n - 2) + fibo(n - 1)
  }

  def sum(f: Int=>Int, n: Int): Int = {
    if (n < 1) 0
    else f(n) + sum(f, n - 1)
  }

  def foldRight(init: Int, ftn: (Int, Int)=>Int, list: IntList): Int = {
    list match {
      case Cons(h, Cons(t, Nil)) => ftn(ftn(init, t), h)
    }
  }

  def filter(f: Int => Boolean, list: IntList): IntList = {
    list match {
      case Nil => list
      case Cons(h, t) => {
        if (f(h) == true) Cons(h, filter(f, t))
        else filter(f, t)
      }
    }
  }

  def iter[A](f: A => A, n: Int): A => A = {
    if (n > 1) {
      val ret = iter(f, n - 1)
      (x: A) => f(ret(x))
    }
    else f
  }
  
  def insert(t: BTree, a: Int): BTree = {
    t match {
      case Leaf => IntNode(a, Leaf, Leaf)
      case IntNode(v, Leaf, Leaf)  => {
        if (v == a) t
        else {
          if (v < a) IntNode(v, Leaf, IntNode(a, Leaf, Leaf))
          else IntNode(v, IntNode(a, Leaf, Leaf), Leaf)
        }
      }
      case IntNode(v, lNode, Leaf) => {
        if (v == a) t
        else {
          if (v < a) IntNode(v, lNode, IntNode(a, Leaf, Leaf))
          else IntNode(v, insert(lNode, a), Leaf)
        }
      }
      case IntNode(v, Leaf, rNode) => {
        if (v == a) t
        else {
          if (v < a) IntNode(v, Leaf, insert(rNode, a))
          else IntNode(v, IntNode(a, Leaf, Leaf), rNode)
        }
      }
      case IntNode(v, lNode, rNode) => {
        if (v == a) t
        else {
          if (v < a) IntNode(v, lNode, insert(rNode, a))
          else IntNode(v, insert(lNode, a), rNode)
        }
      }
    }
  }

  def eval(f: Formula): Boolean = {
    f match {
      case True => true
      case False => false
      case Not(t) => !eval(t)
      case Andalso(a, b) => eval(a) && eval(b)
      case Orelse(a, b) => eval(a) || eval(b)
      case Implies(a, b) => (!eval(a)) || eval(b)
    }
  }
}