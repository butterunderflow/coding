import scala.quoted.staging
import scala.quoted.Quotes
import java.util.concurrent.atomic.AtomicReference

class StagingWasmSuite extends munit.FunSuite:
  import StagingWasm.*
  import StagingWasm.AST.*

  private def showCompact(expression: scala.quoted.Expr[?])(using Quotes): String =
    expression.show
      .replace("scala.Predef.", "")
      .replace("scala.collection.immutable.", "")
      .replace("scala.", "")
      .replace("java.lang.", "")
      .replace("StagingWasm.", "")

  private def expectStackOverflowWithin(timeoutMillis: Long)(body: => Any): Unit =
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

  test("minimal"):
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Add(Const(20), Const(1))
    val mod = Module(Nil)
    val generatedExpression = staging.withQuotes {
      showCompact(compile0(expr, mod))
    }
    val expected =
      """{
        |  val leftRes: Int = 20
        |  val rightRes: Int = 1
        |  leftRes.+(rightRes)
        |}""".stripMargin

    println(generatedExpression)
    assertEquals(generatedExpression, expected)

  test("unroll"):
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
        |  val leftRes: Int = {
        |    val `leftRes₂`: Int = 20
        |    val rightRes: Int = 1
        |    `leftRes₂`.+(rightRes)
        |  }
        |  val `rightRes₂`: Int = {
        |    val `leftRes₃`: Int = 20
        |    val `rightRes₃`: Int = 1
        |    `leftRes₃`.+(`rightRes₃`)
        |  }
        |  leftRes.+(`rightRes₂`)
        |}""".stripMargin

    println(generatedExpression)
    assertEquals(generatedExpression, expected)

  test("divergent"):
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

  test("tying the knots"):
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
      val leftRes: Int = {
        val calleeFunc: Function2[Int, Memo, Int] = m.apply("fakeFib")
        val argVal: Int = {
          val `leftRes₂`: Int = arg
          val rightRes: Int = 1
          `leftRes₂`.-(rightRes)
        }
        calleeFunc.apply(argVal, m)
      }
      val `rightRes₂`: Int = {
        val `calleeFunc₂`: Function2[Int, Memo, Int] = m.apply("fakeFib")
        val `argVal₂`: Int = {
          val `leftRes₃`: Int = arg
          val `rightRes₃`: Int = 2
          `leftRes₃`.-(`rightRes₃`)
        }
        `calleeFunc₂`.apply(`argVal₂`, m)
      }
      leftRes.+(`rightRes₂`)
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
