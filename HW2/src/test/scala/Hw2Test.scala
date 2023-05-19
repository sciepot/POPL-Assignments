import Hw2._
import scala.collection.immutable.HashMap 


class Hw2Test extends org.scalatest.funsuite.AnyFunSuite {
  test("Hw2.IntInterpreter") {
    val testCases = List(
      ("1 + 1", 2)
      ,("pow 2 2",4)
      ,("sigma 1 1 2",2)
      ,("sigma 1 2 x+1",5)
      ,("sigma 1 10 x",55)
      ,("sigma 1 10 x + x",110)
      ,("sigma 1 10 1",10)
      ,("pow 2 10",1024)
      ,("sigma 1 2 pow x 2",5)
      ,("sigma 1 + 1 2 + 3 x + x",28)
      ,("sigma 1 2 sigma 1 x x",4)
      ,("sigma 2 10 sigma 1 x sigma x 10 x",2475)
      ,("sigma 1 3 sigma 1 sigma x 4 x x",128)
      ,("pow 10 0", 1)
      ,("sigma 10 1 x",0)
    )
    val a: List[Int] = testCases.map((a: (String, Int)) => {assert(IntInterpreter(a._1) === a._2); 0} )
  }
val letRecTestCases = List(
      ("1",IntVal(1))
      ,("1 + 1", IntVal(2))
      ,("iszero 1",BoolVal(false))
      ,("if iszero 0 then 1 else 2",IntVal(1))
      ,("let x = 1 in x+1",IntVal(2))
      ,("proc x x + 10",ProcVal(Var("x"),Add(Var("x"),Const(10)),Env(new HashMap[Var,Val]())))
      ,("proc x x + 1 1",IntVal(2))
      ,("letrec f(x) = if iszero x then 0 else x + (f x-1) in f 1",IntVal(1))
      ,("proc x x - 8 5",IntVal(-3))
      ,("let f = proc x x-11 in (f (f 77))",IntVal(55))
      ,("let x = 1 in let f = proc y x + y in let x = 2 in let g = proc y x + y in (f 1) + (g 1)",IntVal(5))
      ,("let x = 1 in let g = proc y x + y in let f = proc z let x = 2 in x + (g z) in f 1", IntVal(4))
      ,("let x = 1 in let f = proc y x + y in let x = 2 in (f 3)", IntVal(4))
    )
  test("Hw2.LetRecInterpreter") {
    
    letRecTestCases.foreach((a: (String, Val)) => {assert(LetRecInterpreter(a._1) === a._2)} )
    assertThrows[Exception] {
      LetRecInterpreter("proc x y - 7 4")
    }
  }

  test("Hw.aa") {
    letRecTestCases.foreach(((a: (String, Val))) => {
      //println(a._1)
      val program = LetRecParserDriver(a._1)
      //println(LetRecToString(program))
      assert(program === LetRecParserDriver(LetRecToString(program)))
      })
    
  }

}
