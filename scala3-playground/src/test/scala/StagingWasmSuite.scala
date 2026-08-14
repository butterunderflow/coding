import scala.quoted.staging
import java.util.concurrent.atomic.AtomicReference

class StagingWasmSuite extends munit.FunSuite:
  import StagingWasm.*
  import StagingWasm.Expr.*

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
      compile0(expr, mod).show
    }
    val expected =
      """{
        |  val leftResult: scala.Int = 20
        |  val rightResult: scala.Int = 1
        |  leftResult.+(rightResult)
        |}""".stripMargin

    println(generatedExpression)
    assertEquals(generatedExpression, expected)

  test("unroll"):
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Call("double", Add(Const(20), Const(1)))
    val mod = Module(List(
      Function("double", value => Add(value, value))
    ))
    val generatedExpression = staging.withQuotes {
      compile0(expr, mod).show
    }
    val expected =
      """{
        |  val leftResult: scala.Int = {
        |    val `leftResult₂`: scala.Int = 20
        |    val rightResult: scala.Int = 1
        |    `leftResult₂`.+(rightResult)
        |  }
        |  val `rightResult₂`: scala.Int = {
        |    val `leftResult₃`: scala.Int = 20
        |    val `rightResult₃`: scala.Int = 1
        |    `leftResult₃`.+(`rightResult₃`)
        |  }
        |  leftResult.+(`rightResult₂`)
        |}""".stripMargin

    println(generatedExpression)
    assertEquals(generatedExpression, expected)

  test("divergent"):
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    val expr = Call("fib", Const(3))
    val mod = Module(List(
      Function(
        "fib",
        value => Add(
          Call("fib", Subtract(value, Const(1))),
          Call("fib", Subtract(value, Const(2)))
        )
      )
    ))

    expectStackOverflowWithin(10000) {
      staging.withQuotes {
        compile0(expr, mod)
      }
    }
