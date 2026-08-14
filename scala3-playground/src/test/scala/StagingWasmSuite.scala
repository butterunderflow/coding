import scala.quoted.staging
import scala.quoted.Quotes
import java.util.concurrent.atomic.AtomicReference

class StagingWasmSuite extends munit.FunSuite {
  import StagingWasm.*
  import StagingWasm.AST.*

  private def showCompact(expression: scala.quoted.Expr[?])(using Quotes): String = {
    expression.show
      .replace("scala.Predef.", "")
      .replace("scala.collection.immutable.", "")
      .replace("scala.", "")
      .replace("java.lang.", "")
      .replace("StagingWasm.", "")
  }

  private def expectStackOverflowWithin(timeoutMillis: Long)(body: => Any): Unit = {
    val failure = AtomicReference[Throwable]()
    val worker = Thread(
      null,
      () =>
        try body
        catch case error: Throwable => failure.set(error),
      "divergent-expansion",
      512 * 1024
    )
    worker.setDaemon(true)
    worker.start()
    worker.join(timeoutMillis)

    assert(!worker.isAlive, s"Expansion did not finish within $timeoutMillis ms")
    assert(failure.get().isInstanceOf[StackOverflowError])
  }

  test("minimal") {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Add(Const(20), Const(1))
    val mod = Module(Nil)
    val generatedExpression = staging.withQuotes {
      showCompact(compile0(expr, mod))
    }
    val expected =
      """{
        |  val left: Int = 20
        |  val right: Int = 1
        |  left.+(right)
        |}""".stripMargin

    println(generatedExpression)
    assertEquals(generatedExpression, expected)
  }

  test("branch") {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Branch(Const(1), Const(42), Const(0))
    val mod = Module(Nil)
    val generatedExpression = staging.withQuotes {
      showCompact(compile0(expr, mod))
    }
    val expected =
      """{
        |  val condition: Int = 1
        |  if (condition.!=(0)) 42 else 0
        |}""".stripMargin

    println(generatedExpression)
    assertEquals(generatedExpression, expected)
  }

  test("unroll") {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Call("double", Add(Const(20), Const(1)))
    val mod = Module(List(
      Function("double", Add(Parameter, Parameter))
    ))
    val generatedExpression = staging.withQuotes {
      showCompact(compile0(expr, mod))
    }
    val expected =
      """{
        |  val left: Int = {
        |    val `left₂`: Int = 20
        |    val right: Int = 1
        |    `left₂`.+(right)
        |  }
        |  val `right₂`: Int = {
        |    val `left₃`: Int = 20
        |    val `right₃`: Int = 1
        |    `left₃`.+(`right₃`)
        |  }
        |  left.+(`right₂`)
        |}""".stripMargin

    println(generatedExpression)
    assertEquals(generatedExpression, expected)
  }

  test("divergent") {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Call("fakeFib", Const(3))
    val mod = Module(List(
      Function(
        "fakeFib",
        Add(
          Call("fakeFib", Sub(Parameter, Const(1))),
          Call("fakeFib", Sub(Parameter, Const(2)))
        )
      )
    ))

    expectStackOverflowWithin(10000) {
      staging.withQuotes {
        compile0(expr, mod)
      }
    }
  }

  test("divergent-sum") {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Call("sum", Const(3))
    val mod = Module(List(
      Function(
        "sum",
        Branch(
          Parameter,
          Add(Parameter, Call("sum", Sub(Parameter, Const(1)))),
          Const(0)
        )
      )
    ))

    expectStackOverflowWithin(10000) {
      staging.withQuotes {
        compile0(expr, mod)
      }
    }
  }

  test("tying the knots") {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Call("fakeFib", Const(3))
    val mod = Module(List(
      Function(
        "fakeFib",
        Add(
          Call("fakeFib", Sub(Parameter, Const(1))),
          Call("fakeFib", Sub(Parameter, Const(2)))
        )
      )
    ))

    val generatedExpression = staging.withQuotes {
      showCompact(compile1(expr, mod))
    }
    val expected =
      """{
  val initMemo: Map[String, Function2[Int, Memo, Int]] = Map.empty[String, Function2[Int, Memo, Int]]
  val finalMemo: Memo = {
    val updatedMemo: Map[String, Function2[Int, Memo, Int]] = initMemo.+[Function2[Int, Memo, Int]](ArrowAssoc[String]("fakeFib").->[Function2[Int, Memo, Int]](((arg: Int, m: Memo) => {
      val left: Int = {
        val calleeFunc: Function2[Int, Memo, Int] = m.apply("fakeFib")
        val argVal: Int = {
          val `left₂`: Int = arg
          val right: Int = 1
          `left₂`.-(right)
        }
        calleeFunc.apply(argVal, m)
      }
      val `right₂`: Int = {
        val `calleeFunc₂`: Function2[Int, Memo, Int] = m.apply("fakeFib")
        val `argVal₂`: Int = {
          val `left₃`: Int = arg
          val `right₃`: Int = 2
          `left₃`.-(`right₃`)
        }
        `calleeFunc₂`.apply(`argVal₂`, m)
      }
      left.+(`right₂`)
    })))
    val `finalMemo₂`: Memo = updatedMemo

    (`finalMemo₂`: Memo)
  }
  val `calleeFunc₃`: Function2[Int, Memo, Int] = finalMemo.apply("fakeFib")
  val `argVal₃`: Int = 3
  `calleeFunc₃`.apply(`argVal₃`, finalMemo)
        |}""".stripMargin

    println(generatedExpression)
    assertEquals(generatedExpression, expected)
  }

  test("tying sum") {
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Call("sum", Const(3))
    val mod = Module(List(
      Function(
        "sum",
        Branch(
          Parameter,
          Add(Parameter, Call("sum", Sub(Parameter, Const(1)))),
          Const(0)
        )
      )
    ))

    val generatedExpression = staging.withQuotes {
      showCompact(compile1(expr, mod))
    }
    val expected =
      """{
  val initMemo: Map[String, Function2[Int, Memo, Int]] = Map.empty[String, Function2[Int, Memo, Int]]
  val finalMemo: Memo = {
    val updatedMemo: Map[String, Function2[Int, Memo, Int]] = initMemo.+[Function2[Int, Memo, Int]](ArrowAssoc[String]("sum").->[Function2[Int, Memo, Int]](((arg: Int, m: Memo) => {
      val condition: Int = arg
      if (condition.!=(0)) {
        val left: Int = arg
        val right: Int = {
          val calleeFunc: Function2[Int, Memo, Int] = m.apply("sum")
          val argVal: Int = {
            val `left₂`: Int = arg
            val `right₂`: Int = 1
            `left₂`.-(`right₂`)
          }
          calleeFunc.apply(argVal, m)
        }
        left.+(right)
      } else 0
    })))
    val `finalMemo₂`: Memo = updatedMemo

    (`finalMemo₂`: Memo)
  }
  val `calleeFunc₂`: Function2[Int, Memo, Int] = finalMemo.apply("sum")
  val `argVal₂`: Int = 3
  `calleeFunc₂`.apply(`argVal₂`, finalMemo)
        |}""".stripMargin
    val result = staging.run {
      compile1(expr, mod)
    }

    println(generatedExpression)
    println(s"result: $result")
    assertEquals(generatedExpression, expected)
    assertEquals(result, 6)
  }
}
