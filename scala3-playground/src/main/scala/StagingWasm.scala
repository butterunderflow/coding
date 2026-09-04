import scala.quoted.{Quotes, Expr}

// λx. x

object StagingWasm:

  enum AST:
    case Const(value: Int)
    case Parameter // we support function with only zero or one parameter
    case Add(left: AST, right: AST)
    case Sub(left: AST, right: AST)
    case Mul(left: AST, right: AST)
    case Branch(condition: AST, whenTrue: AST, whenFalse: AST)
    case Call(function: String, argument: AST)

  final case class Function(name: String, body: AST)

  final case class Module(functions: List[Function])

  /** unstaged interpreter */
  def exec(module: Module, function: String, input: Int): Int =
    val functionMap = moduleIndexMap(module)

    def evaluate(expression: AST, parameter: Int): Int = expression match
      case AST.Const(value)          => value
      case AST.Parameter             => parameter
      case AST.Add(left, right)      => evaluate(left, parameter) + evaluate(right, parameter)
      case AST.Sub(left, right) => evaluate(left, parameter) - evaluate(right, parameter)
      case AST.Mul(left, right) => evaluate(left, parameter) * evaluate(right, parameter)
      case AST.Branch(condition, whenTrue, whenFalse) =>
        if evaluate(condition, parameter) != 0 then
          evaluate(whenTrue, parameter)
        else
          evaluate(whenFalse, parameter)
      case AST.Call(name, argument)  =>
        val callee = lookup(functionMap, name)
        val calleeInput = evaluate(argument, parameter)
        evaluate(callee.body, calleeInput)

    val entry = lookup(functionMap, function)
    evaluate(entry.body, input)

  def compile0(
      expression: AST,
      module: Module
  )(using quotes: Quotes): Expr[Int] =
    val funcNameDict = moduleIndexMap(module)

    def eval(
        expression: AST,
        boundArg: Option[Expr[Int]]
    ): Expr[Int] = expression match
      case AST.Const(value) => Expr(value)
      case AST.Parameter =>
        boundArg.getOrElse:
          quotes.reflect.report.errorAndAbort("A function parameter cannot appear outside a function")
      case AST.Add(leftExpr, rightExpr) =>
        '{
          val left = ${eval(leftExpr, boundArg)}
          val right = ${eval(rightExpr, boundArg)}
          left + right
        }
      case AST.Sub(leftExpr, rightExpr) =>
        '{
          val left = ${eval(leftExpr, boundArg)}
          val right = ${eval(rightExpr, boundArg)}
          left - right
        }
      case AST.Mul(leftExpr, rightExpr) =>
        '{
          val left = ${eval(leftExpr, boundArg)}
          val right = ${eval(rightExpr, boundArg)}
          left * right
        }
      case AST.Branch(conditionExpr, whenTrueExpr, whenFalseExpr) =>
        '{
          val condition = ${eval(conditionExpr, boundArg)}
          if condition != 0 then
            ${eval(whenTrueExpr, boundArg)}
          else
            ${eval(whenFalseExpr, boundArg)}
        }
      case AST.Call(name, argExpr) =>
        val callee = lookup(funcNameDict, name)
        val argVal = eval(argExpr, boundArg)
        eval(callee.body, Some(argVal))

    eval(expression, None)

  type Memo = Map[String, (Int, Memo) => Int]
  // Memo should be Map[String, Expr[Int => Int]]


  def lift(value: String)(using Quotes): scala.quoted.Expr[String] = {
    scala.quoted.Expr(value)
  }

  def compile1(
      expression: AST,
      module: Module
  )(using quotes: Quotes): Expr[Int] =
    val funcNameDict = moduleIndexMap(module)

    def eval(
        expression: AST,
        memo: Expr[Memo],
        boundArg: Option[Expr[Int]]
    ): Expr[Int] = expression match
      case AST.Const(value) => Expr(value)
      case AST.Parameter =>
        boundArg.getOrElse:
          quotes.reflect.report.errorAndAbort("A function parameter cannot appear outside a function")
      case AST.Add(leftExpr, rightExpr) =>
        '{
          val left = ${eval(leftExpr, memo, boundArg)}
          val right = ${eval(rightExpr, memo, boundArg)}
          left + right
        }
      case AST.Sub(leftExpr, rightExpr) =>
        '{
          val left = ${eval(leftExpr, memo, boundArg)}
          val right = ${eval(rightExpr, memo, boundArg)}
          left - right
        }
      case AST.Mul(leftExpr, rightExpr) =>
        '{
          val left = ${eval(leftExpr, memo, boundArg)}
          val right = ${eval(rightExpr, memo, boundArg)}
          left * right
        }
      case AST.Branch(conditionExpr, whenTrueExpr, whenFalseExpr) =>
        '{
          val condition = ${eval(conditionExpr, memo, boundArg)}
          if condition != 0 then
            ${eval(whenTrueExpr, memo, boundArg)}
          else
            ${eval(whenFalseExpr, memo, boundArg)}
        }
      case AST.Call(name, argExpr) =>
        '{
          val calleeFunc = $memo(${lift(name)})
          val argVal = ${eval(argExpr, memo, boundArg)}
          calleeFunc(argVal, $memo)
        }

    def evalModule(
        mod: Module,
        memo: Expr[Memo]
    ): Expr[Memo] = 
      def loop(funcs: List[Function], memo: Expr[Memo]): Expr[Memo] = funcs match
        case Nil => '{
          val finalMemo = $memo
          finalMemo
        }
        case func :: rest =>
          '{
            val updatedMemo = 
              $memo + (${lift(func.name)} -> ((arg: Int, m: Memo) => ${eval(func.body, 'm, Some('arg))}))
            ${loop(rest, 'updatedMemo)}
          }
      loop(mod.functions, memo)
    
    // This definition doesn't work, will duplicate the memo's definition every time it's used
    // val memo = {
    //   val initMemo = '{
    //     Map.empty[String, (Int, Memo) => Int]
    //   }
    //   evalModule(module, initMemo)
    // }
    // eval(expression, memo, None)

    '{
      val initMemo = Map.empty[String, (Int, Memo) => Int]
      val finalMemo = ${evalModule(module, 'initMemo)}
      ${eval(expression, 'finalMemo, None)}
    }

  private def moduleIndexMap(module: Module): Map[String, Function] =
    val duplicateNames = module.functions.groupBy(_.name).collect:
      case (name, functions) if functions.sizeIs > 1 => name

    require(duplicateNames.isEmpty, s"Duplicate function names: ${duplicateNames.mkString(", ")}")
    module.functions.map(function => function.name -> function).toMap

  private def lookup(functions: Map[String, Function], name: String): Function =
    functions.getOrElse(name, throw new IllegalArgumentException(s"Unknown function: $name"))
