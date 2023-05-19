package hw4

import scala.collection.immutable.HashMap 
import hw4._


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
  def added(l: LocVal, new_val: Val): Mem = {
    Mem(m.updated(l, new_val), top)
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


sealed trait Program
sealed trait Expr extends Program
case object Skip extends Expr
case object False extends Expr
case object True extends Expr
case class NotExpr(expr: Expr) extends Expr
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr {
  override def toString = s"Var(${"\""}${s}${"\""})"
}
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class LTEExpr(l: Expr, r: Expr) extends Expr
case class EQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(i: Var, v: Expr, body: Expr) extends Expr
case class Proc(args: List[Var], expr: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class BeginEnd(expr: Expr) extends Expr
case class FieldAccess(record: Expr, field: Var) extends Expr
case class FieldAssign(record: Expr, field: Var, new_val: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCallV(ftn: Expr, arg: List[Expr]) extends Expr
case class PCallR(ftn: Expr, arg: List[Var]) extends Expr
case class WhileExpr(cond: Expr, body: Expr) extends Expr
sealed trait RecordLike extends Expr
case object EmptyRecordExpr extends RecordLike
case class RecordExpr(field: Var, initVal: Expr, next: RecordLike) extends RecordLike








object MiniCInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
    
  def search(r: RecordValLike, x: Var): LocVal = r match {
    case RecordVal(field: Var, loc: LocVal, next: RecordValLike) => {
      if (field == x) loc
      else search(next, x)
    }
    case EmptyRecordVal => throw new UndefinedSemantics(s"${x.s} is undefined")
  }

  def eval(env: Env, mem: Mem, expr: Expr): Result = expr match {
    case Skip => Result(SkipVal, mem)

    case False => Result(BoolVal(false), mem)

    case True => Result(BoolVal(true), mem)

    case NotExpr(e: Expr) => eval(env, mem, e) match {
      case Result(BoolVal(b), mem1) => Result(BoolVal(!b), mem1)
      case _ => throw new UndefinedSemantics("Type Error")
    }

    case Const(c: Int) => Result(IntVal(c), mem)

    case Var(x: String) => {
      if (env.exists((a: (Var, LocVal)) => a._1 == Var(x))) {
        val v: Val = mem.get(env(Var(x))) match {
          case Some(n: Val) => n
          case None => throw new UndefinedSemantics("Segmentation fault (core dumped)")
        }
        Result(v, mem)
      }
      else throw new UndefinedSemantics("Undefined reference to " + x)
    }


    case Add(l: Expr, r: Expr) => eval(env, mem, l) match {
      case Result(IntVal(x), mem1) => eval(env, mem1, r) match {
        case Result(IntVal(y), mem2) => Result(IntVal(x + y), mem2)
        case _ => throw new UndefinedSemantics("Type error")
      }
      case _ => throw new UndefinedSemantics("Type error")
    }

    case Sub(l: Expr, r: Expr) => eval(env, mem, l) match {
      case Result(IntVal(x), mem1) => eval(env, mem1, r) match {
        case Result(IntVal(y), mem2) => Result(IntVal(x - y), mem2)
        case _ => throw new UndefinedSemantics("Type error")
      }
      case _ => throw new UndefinedSemantics("Type error")
    }

    case Mul(l: Expr, r: Expr) => eval(env, mem, l) match {
      case Result(IntVal(x), mem1) => eval(env, mem1, r) match {
        case Result(IntVal(y), mem2) => Result(IntVal(x * y), mem2)
        case _ => throw new UndefinedSemantics("Type error")
      }
      case _ => throw new UndefinedSemantics("Type error")
    }

    case Div(l: Expr, r: Expr) => eval(env, mem, l) match {
      case Result(IntVal(x), mem1) => eval(env, mem1, r) match {
        case Result(IntVal(y), mem2) => 
          if (y == 0) throw new UndefinedSemantics("Division by zero")
          else Result(IntVal(x / y), mem2)
        case _ => throw new UndefinedSemantics("Type error")
      }
      case _ => throw new UndefinedSemantics("Type error")
    }

    case LTEExpr(l: Expr, r: Expr) => eval(env, mem, l) match {
      case Result(IntVal(x), mem1) => eval(env, mem1, r) match {
        case Result(IntVal(y), mem2) => Result(BoolVal(x <= y), mem2)
        case _ => throw new UndefinedSemantics("Type error")
      }
      case _ => throw new UndefinedSemantics("Type error")
    }

    case EQExpr(l: Expr, r: Expr) => eval(env, mem, l) match {
      case Result(IntVal(x), mem1) => eval(env, mem1, r) match {
        case Result(IntVal(y), mem2) => Result(BoolVal(x == y), mem2)
        case _ => throw new UndefinedSemantics("Type error")
      }
      case _ => throw new UndefinedSemantics("Type error")
    }

    case Iszero(e: Expr) => eval(env, mem, e) match {
      case Result(IntVal(n), mem1) => Result(BoolVal(n == 0), mem1)
      case _ => throw new UndefinedSemantics("Type error")
    }

    case Ite(i: Expr, t: Expr, e: Expr) => eval(env, mem, i) match {
      case Result(BoolVal(b), mem1) => if (b) eval(env, mem1, t) else eval(env, mem1, e)
      case _ => throw new UndefinedSemantics("Type Error")
    }

    case Let(name: Var, value: Expr, body: Expr) => {
      val res: Result = eval(env, mem, value)
      val ml: (Mem, LocVal) = res.m.extended(res.v)
      val new_env: Env = env + (name -> ml._2)
      eval(new_env, ml._1, body)
    }

    case Proc(args: List[Var], expr: Expr) => Result(ProcVal(args, expr, env), mem)

    case Asn(v: Var, value: Expr) => {
      if (env.exists((a: (Var, LocVal)) => a._1 == v)) {
        val res: Result = eval(env, mem, value)
        val mem1: Mem = res.m.updated(env(v), res.v) match {
          case Some(m) => m
          case None => throw new UndefinedSemantics("Segmentation fault (core dumped)")
        }
        Result(res.v, mem1)
      }
      else throw new UndefinedSemantics("Undefined reference to " + v.s)
    }

    case BeginEnd(expr: Expr) => eval(env, mem, expr)

    case Block(f, s) => {
      val res: Result = eval(env, mem, f)
      eval(env, res.m, s)
    }

    case PCallV(func: Expr, args: List[Expr]) => eval(env, mem, func) match {
      case Result(ProcVal(vars: List[Var], body: Expr, new_env: Env), mem1) => {
        def loop(xs: List[Var], vs: List[Expr], m: Mem): (Env, Mem) = (xs, vs) match {
          case (Nil, Nil) => (new_env, m)
          case (x::nxs, v::nvs) => {
            val res: Result = eval(env, m, v)
            val ret: (Env, Mem) = loop(nxs, nvs, res.m)
            val ml: (Mem, LocVal) = ret._2.extended(res.v)
            val ne: Env = ret._1 + (x -> ml._2)
            (ne, ml._1)
          }
          case _ => throw new UndefinedSemantics("The number of parameters does not match")
        }
        val em: (Env, Mem) = loop(vars, args, mem1)
        eval(em._1, em._2, body)
      }
      case _ => throw new UndefinedSemantics("Type Error")
    }

    case PCallR(func: Expr, args: List[Var]) => eval(env, mem, func) match {
      case Result(ProcVal(vars: List[Var], body: Expr, new_env: Env), mem1) => {
        def loop(xs: List[Var], ys: List[Var], ne: Env): Env = (xs, ys) match {
          case (Nil, Nil) => ne
          case (x::nxs, y::nys) => {
            if (env.exists((a: (Var, LocVal)) => a._1 == y)) {
              val une: Env = ne + (x -> env(y))
              loop(nxs, nys, une)
            }
            else throw new UndefinedSemantics("Undefined reference to " + x.s)
          }
          case _ => throw new UndefinedSemantics("The number of parameters does not match")
        }
        val finEnv: Env = loop(vars, args, new_env)
        eval(finEnv, mem1, body)
      }
      case _ => throw new UndefinedSemantics("Type Error")
    }

    case WhileExpr(cond, body) => eval(env, mem, cond) match {
      case Result(BoolVal(b), mem1) => { 
        if (b) {
          val res: Result = eval(env, mem1, body)
          eval(env, res.m, WhileExpr(cond, body))
        }
        else Result(SkipVal, mem1)
      }
      case _ => throw new UndefinedSemantics("Type Error")
    }

    case EmptyRecordExpr => Result(EmptyRecordVal, mem)

    case RecordExpr(field: Var, initVal: Expr, next: RecordLike) => {
      val res: Result = eval(env, mem, initVal)
      val ml: (Mem, LocVal) = res.m.extended(res.v)
      eval(env, ml._1, next) match {
        case Result(r: RecordValLike, m: Mem) => Result(RecordVal(field, ml._2, r), m)
        case _ => throw new UndefinedSemantics("Type Error")
      }
    }

    case FieldAccess(record: Expr, field: Var) => eval(env, mem, record) match {
      case Result(rec: RecordVal, mem1) => {
        val loc: LocVal = search(rec, field)
        val v: Val = mem1.get(loc) match {
          case Some(n: Val) => n
          case None => throw new UndefinedSemantics("Segmentation fault (core dumped)")
        }
        Result(v, mem)
      }
      case Result(EmptyRecordVal, mem1) => throw new UndefinedSemantics(s"${field.s} is undefined")
      case _ => throw new UndefinedSemantics("Type Error")
    }

    case FieldAssign(record: Expr, field: Var, expr: Expr) => eval(env, mem, record) match {
      case Result(recVal: RecordVal, mem1) => {
        val res: Result = eval(env, mem1, expr)
        val loc: LocVal = search(recVal, field)

        val mem2: Mem = res.m.updated(loc, res.v) match {
          case Some(m: Mem) => m
          case None => throw new UndefinedSemantics("Segmentation fault (core dupmed)")
        }
        Result(res.v, mem2)
      }
      case Result(EmptyRecordVal, mem1) => throw new UndefinedSemantics(s"${field} is undefined")
      case _ =>  throw new UndefinedSemantics("Type Error")
    }
    case _ => throw new UndefinedSemantics("Undefined Semantics")
  }

  def gc(env: Env, mem: Mem): Mem = {
    def search(r: RecordValLike): List[(Var, LocVal)] = r match {
      case RecordVal(_, loc, next) => (Var("0"), loc)::search(next)
      case EmptyRecordVal => Nil
    }

    def loop(e: List[(Var, LocVal)], m: Mem): Mem = e match {
      case Nil => m
      case (_, loc)::e1 => mem.get(loc) match {
        case Some(v) => v match {
          case LocVal(x) => {
            val e2: List[(Var, LocVal)] = e1:::List((Var("0"), LocVal(x)))
            val m1 = m.added(loc, v)
            loop(e2, m1)
          }
          case r: RecordVal => {
            val e2: List[(Var, LocVal)] = e1:::search(r)
            val m1 = m.added(loc, v)
            loop(e2, m1)
          }
          case _ => {
            val m1 = m.added(loc, v)
            loop(e1, m1)
          }
        }
        case None => loop(e, m)
      }
    }
    val list: List[(Var, LocVal)] = env.toList
    loop(list, Mem(new HashMap[LocVal, Val], mem.top))
  }
  
  def apply(program: String): (Val, Mem) = {
    val parsed = MiniCParserDriver(program)
    val res = eval(new Env(), Mem(new HashMap[LocVal,Val],0), parsed)
    (res.v, res.m)
  }

}


object Hw4App extends App {
  
  println("Hello from Hw4!")

}