package codereusemsp

import scala.quoted.{Expr, Quotes}
import scala.quoted.staging

type Code = Expr[Int]

trait Mode[R]:
  def lit(value: Int): R
  extension (left: R) infix def mul(right: R): R

object Mode:
  given Evaluate: Mode[Int] with
    def lit(value: Int): Int = value

    extension (left: Int)
      infix def mul(right: Int): Int = left * right

  given Generate(using Quotes): Mode[Code] with
    def lit(value: Int): Code = Expr(value)

    extension (left: Code)
      infix def mul(right: Code): Code = '{ $left * $right }

def power[R](x: R, n: Int)(using mode: Mode[R]): R =
  if n == 0 then mode.lit(1)
  else x mul power(x, n - 1)

@main def testCodeReuseMSP(): Unit =
  given staging.Compiler = staging.Compiler.make(Thread.currentThread().getContextClassLoader)

  assert(power(2, 0) == 1)
  assert(power(2, 5) == 32)
  staging.withQuotes {
    val generated = power[Code]('{ 2 }, 5)

    assert(generated.show == "2.*(2.*(2.*(2.*(2.*(1)))))")
  }
