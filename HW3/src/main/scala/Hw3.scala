package hw3

import scala.collection.immutable.HashMap 
import hw3._


package object hw3 {
  type Env = HashMap[Var,Val]
  type Loc = Int
  
}

case class Mem(m: HashMap[Loc,Val], top: Loc) {
  def exists(l: Loc): Boolean = {
    m.exists((a: (Loc, Val)) => a._1 == l)
  }

  def write(value: Val, address: Loc = top): Mem = {
    Mem(m + (address -> value), top + 1)
  }

  def read(l: Loc): Val = {
    m(l)
  }
}

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, env: Env) extends Val
case class LocVal(l: Loc) extends Val


sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class GTExpr(l: Expr, r: Expr) extends Expr
case class GEQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class ValExpr(name: Var, value: Expr, body: Expr) extends Expr
case class VarExpr(name: Var, value: Expr, body: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr







object MiniScalaInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)


  def doInterpret(env: Env, mem: Mem, expr: Expr): Result = {
      def eval(env: Env, mem: Mem, expr: Expr): (Val, Mem) = expr match {
        case Const(n) => (IntVal(n), mem)
        case Var(x) => {
          if (env.exists((a: (Var, Val)) => a._1 == Var(x))) env(Var(x)) match {
            case LocVal(l) => {
              if (mem.exists(l)) (mem.read(l), mem)
              else throw new UndefinedSemantics("Segmentation fault (core dumped)")
            }
            case _ => (env(Var(x)), mem)
          }
          else throw new UndefinedSemantics("Undefined reference to " + x)
        }

        case Add(l, r) => eval(env, mem, l) match {
          case (x: IntVal, mem1) => eval(env, mem1, r) match {
            case (y: IntVal, mem2) => (IntVal(x.n + y.n), mem2)
            case _ => throw new UndefinedSemantics("Type error")
          }
          case _ => throw new UndefinedSemantics("Type error")
        }

        case Sub(l, r) => eval(env, mem, l) match {
          case (x: IntVal, mem1) => eval(env, mem1, r) match {
            case (y: IntVal, mem2) => (IntVal(x.n - y.n), mem2)
            case _ => throw new UndefinedSemantics("Type error")
          }
          case _ => throw new UndefinedSemantics("Type error")
        }

        case Mul(l, r) => eval(env, mem, l) match {
          case (x: IntVal, mem1) => eval(env, mem1, r) match {
            case (y: IntVal, mem2) => (IntVal(x.n * y.n), mem2)
            case _ => throw new UndefinedSemantics("Type error")
          }
          case _ => throw new UndefinedSemantics("Type error")
        }

        case Div(l, r) => eval(env, mem, l) match {
          case (x: IntVal, mem1) => eval(env, mem1, r) match {
            case (y: IntVal, mem2) => 
              if (y.n == 0) throw new UndefinedSemantics("Division by zero")
              else (IntVal(x.n / y.n), mem2)
            case _ => throw new UndefinedSemantics("Type error")
          }
          case _ => throw new UndefinedSemantics("Type error")
        }

        case GTExpr(l, r) => eval(env, mem, l) match {
          case (x: IntVal, mem1) => eval(env, mem1, r) match {
            case (y: IntVal, mem2) => (BoolVal(x.n > y.n), mem2)
            case _ => throw new UndefinedSemantics("Type error")
          }
          case _ => throw new UndefinedSemantics("Type error")
        }
        
        case GEQExpr(l, r) => eval(env, mem, l) match {
          case (x: IntVal, mem1) => eval(env, mem1, r) match {
            case (y: IntVal, mem2) => (BoolVal(x.n >= y.n), mem2)
            case _ => throw new UndefinedSemantics("Type error")
          }
          case _ => throw new UndefinedSemantics("Type error")
        }

        case Iszero(e) => eval(env, mem, e) match {
          case (x: IntVal, mem1) => if (x.n == 0) (BoolVal(true), mem1) else (BoolVal(false), mem1)
          case _ => throw new UndefinedSemantics("Type error")
        }

        case Ite(i, t, e) => eval(env, mem, i) match {
          case (v: BoolVal, mem1) => if (v.b) eval(env, mem1, t) else eval(env, mem1, e)
          case _ => throw new UndefinedSemantics("Type Error")
        }

        case ValExpr(name, value, body) => {
          val res = eval(env, mem, value)
          val new_env: Env = env + (name -> res._1)
          eval(new_env, res._2, body)
        }

        case VarExpr(name, value, body) => {
          val res = eval(env, mem, value)
          val new_env: Env = env + (name -> LocVal(res._2.top))
          eval(new_env, res._2.write(res._1), body)
        }

        case Proc(v, e) => (ProcVal(v, e, env), mem)

        case DefExpr(fname, aname, fbody, ibody) => {
          val new_env: Env = env + (fname -> RecProcVal(fname, aname, fbody, env))
          eval(new_env, mem, ibody)
        }

        case Asn(v, e) => env(v) match {
          case LocVal(l) => {
            val res = eval(env, mem, e)
            (res._1, res._2.write(res._1, l))
          }
          case _ => throw new UndefinedSemantics("Segmentation fault (core dumped)")
        }

        case Paren(expr: Expr) => eval(env, mem, expr)

        case Block(f, s) => {
          val res = eval(env, mem, f)
          eval(env, res.m, s)
        }

        case PCall(func, arg) => eval(env, mem, func) match {
          case (ProcVal(x, e, p), mem1) => {
            val res = eval(env, mem1, arg)
            val new_env: Env = p + (x -> res._1)
            eval(new_env, res._2, e)
          }

          case (RecProcVal(f, x, e, p), mem1) => {
            val res = eval(env, mem1, arg)
            val new_env: Env = p + (x -> res._1) + (f -> RecProcVal(f, x, e, p))
            eval(new_env, res._2, e)
          }
          case _ => throw new UndefinedSemantics("Type Error")
        }
      }
    val res =  eval(env, mem, expr)
    Result(res._1, res._2)
  }
  def apply(program: String): Val = {
    val parsed = MiniScalaParserDriver(program)
    doInterpret(new Env(), Mem(new HashMap[Loc,Val],0), parsed).v
  }

}


object Hw3App extends App {

  println("Hello from Hw3!")

}


package object hw4 {
  type Env = HashMap[Var,LocVal]
}

case class Mem(m: HashMap[LocVal,Val], top: Int) {
  def extended(v: Val): (Mem, LocVal) = {
    val new_mem = Mem(m.updated(LocVal(top),v), top+1)
    (new_mem,LocVal(top))
  }
  def updated(l: LocVal, new_val: Val): Option[Mem] = {
    m.get(l) match {
      case Some(v) => Some(Mem(m.updated(l, new_val), top))
      case None => None
    }
  }
  def get(l: LocVal): Option[Val] = m.get(l)
  def getLocs(): List[LocVal] = m.keySet.toList
}

sealed trait Val
case object SkipVal extends Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(args: List[Var], expr: Expr, env: Env) extends Val
case class LocVal(l: Int) extends Val
sealed trait RecordValLike extends Val
case object EmptyRecordVal extends RecordValLike
case class RecordVal(field: Var, loc: LocVal, next: RecordValLike) extends RecordValLike