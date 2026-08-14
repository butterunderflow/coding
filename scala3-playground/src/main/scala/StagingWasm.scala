import scala.quoted.Quotes

object StagingWasm:

  enum Expr:
    case Const(value: Int)
    case Parameter
    case Add(left: Expr, right: Expr)
    case Subtract(left: Expr, right: Expr)
    case Multiply(left: Expr, right: Expr)
    case Call(function: String, argument: Expr)

  final case class Function(name: String, output: Expr => Expr):
    def apply(input: Expr): Expr = output(input)

  final case class Module(functions: List[Function])

  /** unstaged interpreter */
  def exec(module: Module, function: String, input: Int): Int =
    val functionMap = moduleIndexMap(module)

    def evaluate(expression: Expr, parameter: Int): Int = expression match
      case Expr.Const(value)          => value
      case Expr.Parameter             => parameter
      case Expr.Add(left, right)      => evaluate(left, parameter) + evaluate(right, parameter)
      case Expr.Subtract(left, right) => evaluate(left, parameter) - evaluate(right, parameter)
      case Expr.Multiply(left, right) => evaluate(left, parameter) * evaluate(right, parameter)
      case Expr.Call(name, argument)  =>
        val callee = lookup(functionMap, name)
        val calleeInput = evaluate(argument, parameter)
        evaluate(callee(Expr.Parameter), calleeInput)

    val entry = lookup(functionMap, function)
    evaluate(entry(Expr.Parameter), input)

  def compile0(
      expression: Expr,
      module: Module
  )(using quotes: Quotes): scala.quoted.Expr[Int] =
    val funcNameDic = moduleIndexMap(module)

    def eval(
        expression: Expr,
        parameter: Option[scala.quoted.Expr[Int]]
    ): scala.quoted.Expr[Int] = expression match
      case Expr.Const(value) => scala.quoted.Expr(value)
      case Expr.Parameter =>
        parameter.getOrElse:
          quotes.reflect.report.errorAndAbort("A function parameter cannot appear outside a function")
      case Expr.Add(left, right) =>
        val stagedLeft = eval(left, parameter)
        val stagedRight = eval(right, parameter)
        '{
          val leftResult = $stagedLeft
          val rightResult = $stagedRight
          leftResult + rightResult
        }
      case Expr.Subtract(left, right) =>
        val stagedLeft = eval(left, parameter)
        val stagedRight = eval(right, parameter)
        '{
          val leftResult = $stagedLeft
          val rightResult = $stagedRight
          leftResult - rightResult
        }
      case Expr.Multiply(left, right) =>
        val stagedLeft = eval(left, parameter)
        val stagedRight = eval(right, parameter)
        '{
          val leftResult = $stagedLeft
          val rightResult = $stagedRight
          leftResult * rightResult
        }
      case Expr.Call(name, argument) =>
        val callee = lookup(funcNameDic, name)
        val stagedArgument = eval(argument, parameter)
        eval(callee(Expr.Parameter), Some(stagedArgument))

    eval(expression, None)

  private def moduleIndexMap(module: Module): Map[String, Function] =
    val duplicateNames = module.functions.groupBy(_.name).collect:
      case (name, functions) if functions.sizeIs > 1 => name

    require(duplicateNames.isEmpty, s"Duplicate function names: ${duplicateNames.mkString(", ")}")
    module.functions.map(function => function.name -> function).toMap

  private def lookup(functions: Map[String, Function], name: String): Function =
    functions.getOrElse(name, throw new IllegalArgumentException(s"Unknown function: $name"))
