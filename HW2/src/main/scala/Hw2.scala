package Hw2

import fastparse._
import MultiLineWhitespace._
import scala.collection.immutable.HashMap 

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, expr: Expr, env: Env) extends Val

case class Env(hashmap: HashMap[Var,Val]) {
  def apply(variable: Var): Val = hashmap(variable)
  def exists(v: Var): Boolean = 
    hashmap.exists((a: (Var, Val)) => a._1 == v)
  def add(v: Var, value: Val) = Env(hashmap + (v -> value))
}


sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(name: Var, value: Expr, body: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr
case class LetRec(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr

sealed trait IntExpr
case class IntConst(n: Int) extends IntExpr
case object IntVar extends IntExpr
case class IntAdd(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSub(l: IntExpr, r: IntExpr) extends IntExpr
case class IntMul(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSigma(f: IntExpr, t: IntExpr, b: IntExpr) extends IntExpr
case class IntPow(b: IntExpr, e: IntExpr) extends IntExpr

package object Hw2 {

}

object IntInterpreter {
  def evalInt(expr: IntExpr, env: List[Int]): Int = expr match {
    case IntConst(c) => c
    case IntVar => env match {
      case Nil => throw new Exception("x is undefined")
      case v::l => v
    }
    case IntAdd(l: IntExpr, r: IntExpr) => (evalInt(l, env), evalInt(r, env)) match {
      case (x: Int, y: Int) => x + y
      case _ => throw new Exception("Type Error")
    }
    case IntSub(l: IntExpr, r: IntExpr) => (evalInt(l, env), evalInt(r, env)) match {
      case (x: Int, y: Int) => x - y
      case _ => throw new Exception("Type Error")
    }
    case IntMul(l: IntExpr, r: IntExpr) => (evalInt(l, env), evalInt(r, env)) match {
      case (x: Int, y: Int) => x * y
      case _ => throw new Exception("Type Error")
    }
    case IntSigma(f: IntExpr, t: IntExpr, b: IntExpr) => (evalInt(f, env), evalInt(t, env)) match {
      case (i: Int, j: Int) =>
        if (i > j) 0
        else evalInt(b, i::env) + evalInt(IntSigma(IntConst(i + 1), IntConst(j), b: IntExpr), env)
      case _ => throw new Exception("Type Error")
    }
    case IntPow(b: IntExpr, e: IntExpr) => (evalInt(b, env), evalInt(e, env)) match {
      case (b: Int, e: Int) =>
        if (e == 0) 1
        else if (e < 0) throw new Exception("Negative power is not defined")
        else b * evalInt(IntPow(IntConst(b), IntConst(e - 1)), env)
      case _ => throw new Exception("Type Error")
    }

    case _ => throw new Exception("Undefined Expression")
  }

  def apply(s: String): Int = {
    val parsed = IntParser(s)
    evalInt(parsed, Nil)
  }
}

object LetRecInterpreter {
  def eval(env: Env, expr: Expr): Val = expr match {
    case Const(n) => IntVal(n)
    
    case Var(s) =>
      if (env.exists(Var(s))) env.apply(Var(s))
      else throw new Exception(s"$s is undefined")
    
    case Add(l: Expr, r: Expr) => (eval(env, l), eval(env, r)) match {
      case (IntVal(x), IntVal(y)) => IntVal(x + y)
      case _ => throw new Exception("Type error in add")
    }

    case Sub(l: Expr, r: Expr) => (eval(env, l), eval(env, r)) match {
      case (IntVal(x), IntVal(y)) => IntVal(x - y)
      case _ => throw new Exception("Type error in sub")
    }

    case Iszero(c: Expr) => eval(env, c) match {
      case IntVal(x) => if (x == 0) BoolVal(true) else BoolVal(false)
      case _ => throw new Exception("Type Error in iszero")
    }

    case Ite(i: Expr, t: Expr, e: Expr) => eval(env, i) match {
      case BoolVal(x) => if (x) eval(env, t) else eval(env, e)
      case _ => throw new Exception("Type Error in ite")
    }

    case Let(name: Var, value: Expr, body: Expr) => eval(env, value) match {
      case x: Val => eval(env.add(name, x), body)
      case _ => throw new Exception("Type Error in let")
    }

    case Paren(e: Expr) => eval(env, e)

    case Proc(v: Var, e: Expr) => ProcVal(v, e, env)

    case PCall(v: Expr, a: Expr) => eval(env, v) match {
      case ProcVal(x: Var, e: Expr, p: Env) => eval(p.add(x, eval(env, a)), e)
      case RecProcVal(f: Var, x: Var, e: Expr, p: Env) => eval((env.add(x, eval(env, a))).add(f, RecProcVal(f, x, e, p)), e)
      case _ => throw new Exception("Type Error in pcall")
    }

    case LetRec(f: Var, x: Var, e1: Expr, e2: Expr) => eval(env.add(f, RecProcVal(f, x, e1, env)), e2)

    case _ => throw new Exception("Undefined Expression")
  }
  def apply(program: String): Val = {
    val parsed = LetRecParserDriver(program)
    eval(Env(new HashMap[Var,Val]()), parsed)
  }

}

object LetRecToString {
  val cast: HashMap[Int,String] =  HashMap(0->"0",1->"1",2->"2",3->"3",4->"4",5->"5",6->"6",7->"7",8->"8",9->"9") 

  def apply(expr: Expr): String = expr match {
    case Const(n: Int) =>
      if (n/10 == 0) cast(n%10)
      else apply(Const(n/10)) + cast(n%10)
    
    case Var(s: String) => s
    
    case Add(l: Expr, r: Expr) => apply(l) + " + " + apply(r)

    case Sub(l: Expr, r: Expr) => apply(l) + " - " + apply(r)

    case Iszero(c: Expr) => "iszero " + apply(c)

    case Ite(i: Expr, t: Expr, e: Expr) => "if " + apply(i) + " then " + apply(t) + " else " + apply(e)

    case Let(name: Var, value: Expr, body: Expr) => "let " + apply(name) + " = " + apply(value) + " in " + apply(body)

    case Paren(e: Expr) => "(" + apply(e) + ")"

    case Proc(v: Var, e: Expr) => "proc " + apply(v) + " " + apply(e)

    case PCall(v: Expr, a: Expr) => apply(v) + " " + apply(a)

    case LetRec(f: Var, x: Var, e1: Expr, e2: Expr) => "letrec " + apply(f) + "(" + apply(x) + ") = " + apply(e1) + " in " + apply(e2)

    case _ => throw new Exception("Undefined Expression") 
  }
}

object Hw2App extends App {
  
  println("Hello from Hw2!")

  val int_prog = """x + 1"""
  val parsed = IntParser(int_prog)
  println(parsed)

}