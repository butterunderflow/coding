package completeeasy

enum Ty:
  case TUnit
  case TVar(name: String)
  // existential type variable, can only be bound to monotype
  case TEVar(name: String)
  case TFun(input: Ty, output: Ty)
  case TForall(x: String, t: Ty)

extension (t: Ty)
  def isMono: Boolean = t match
    case Ty.TUnit               => true
    case Ty.TVar(_)             => true
    case Ty.TEVar(_)            => true
    case Ty.TFun(input, output) => input.isMono && output.isMono
    case Ty.TForall(_, _)       => false

enum Expr:
  case EUnit
  case EVar(x: String)
  case ELam(x: String, e: Expr)
  case EApp(e1: Expr, e2: Expr)
  case EAnnot(e: Expr, t: Ty)

object Freshness:
  var counter: Int = 0

  def fresh(prefix: String = "α"): Ty.TEVar =
    counter += 1
    Ty.TEVar(counter.toString)

enum Binding:
  case BTVar(α: String)
  case BTEVar(αᵉ: String) // existential type variable binding
  case BVar(x: String, t: Ty)
  case BResolve(
      αᵉ: String,
      t: Ty
  ) // resolved existential type, t must be monotype
  case BMarker(αᵉ: String)

type Context = List[Binding]

case class Destruct[T <: Binding](pre: Context, binding: T, post: Context)

enum Resolution:
  case Resolved(destruct: Destruct[Binding.BResolve])
  case Unresolved(destruct: Destruct[Binding.BTEVar])
  case Unbound

enum LookupTVar:
  case Found(destruct: Destruct[Binding.BTVar])
  case Unbound

enum LookupVar:
  case Found(destruct: Destruct[Binding.BVar])
  case Unbound

extension (ctx: Context)
  def apply(ty: Ty): Ty =
    import Resolution._
    import Binding._
    ty match
      case Ty.TUnit         => Ty.TUnit
      case ty @ Ty.TVar(_)  => ty
      case αᵉ @ Ty.TEVar(_) =>
        ctx.resolve(αᵉ) match
          case Resolved(Destruct(_, Binding.BResolve(_, t), _)) => t
          case Unresolved(Destruct(_, Binding.BTEVar(_), _)) => αᵉ
          case Unbound           =>
            throw new Exception(
              s"Unbound existential type variable: ${αᵉ.name}"
            )
      case Ty.TFun(input, output) => Ty.TFun(ctx(input), ctx(output))
      case Ty.TForall(x, t)       => Ty.TForall(x, ctx(t))

  def resolve(αᵉ: Ty.TEVar): Resolution =
    import Resolution._
    import Binding._
    @annotation.tailrec
    def go(
        xs: List[Binding],
        preRev: List[Binding]
    ): Resolution =
      xs match
        case Nil =>
          Unbound
        case Binding.BTEVar(_) :: rest =>
          Unresolved(Destruct(preRev.reverse, Binding.BTEVar(αᵉ.name), rest))
        case Binding.BResolve(x, ty) :: rest if x == αᵉ.name =>
          Resolved(Destruct(preRev.reverse, Binding.BResolve(x, ty), rest))
        case binding :: rest =>
          go(rest, binding :: preRev)
    go(ctx, Nil)

  def lookupTVar(α: Ty.TVar): LookupTVar =
    import LookupTVar._
    import Binding._
    @annotation.tailrec
    def go(
        xs: List[Binding],
        preRev: List[Binding]
    ): LookupTVar =
      xs match
        case Nil =>
          Unbound
        case Binding.BTVar(x) :: rest if x == α.name =>
          Found(Destruct(preRev.reverse, Binding.BTVar(x), rest))
        case binding :: rest =>
          go(rest, binding :: preRev)
    go(ctx, Nil)

  def lookupVar(x: String): LookupVar =
    import Binding._
    import LookupVar._
    @annotation.tailrec
    def go(xs: List[Binding]): LookupVar =
      xs match
        case Nil => Unbound
        case BVar(y, t) :: rest if y == x => Found(Destruct(Nil, BVar(y, t), rest))
        case _ :: rest                      => go(rest)
    go(ctx)
def wellform(ctx: Context, ty: Ty): Boolean = ???

def wellform(ctx: Context): Boolean = ???

object Bidirectional:
  def infer(ctxΓ: Context, expr: Expr): (Context, Ty) =
    import Expr._
    import LookupVar._
    expr match
      case EUnit => (ctxΓ, Ty.TUnit)
      case EVar(x) => ctxΓ.lookupVar(x) match
        case Found(Destruct(_, Binding.BVar(_, t), _)) => (ctxΓ, t)
        case Unbound =>
          throw new Exception(s"Unbound variable: $x")
      case EAnnot(e, t) =>
        if (!wellform(ctxΓ, t))
          throw new Exception(s"Type annotation is not well-formed: $t")
        val ctx1 = check(ctxΓ, e, t)
        (ctx1, t)
      case ELam(x, e) =>
        val αᵉ = Freshness.fresh("αᵉ")
        val βᵉ = Freshness.fresh("βᵉ")
        val ctxΓ1 = Binding.BVar(x, αᵉ) :: Binding.BTEVar(αᵉ.name) :: Binding.BTEVar(βᵉ.name) :: ctxΓ
        val ctxΔ1 = check(ctxΓ1, e, βᵉ)
        val Found(Destruct(ctxΘ, Binding.BVar(_, _), ctxΔ)) = ctxΔ1.lookupVar(x)
        (ctxΔ, Ty.TFun(αᵉ, βᵉ))
      case EApp(e1, e2) =>
        val (ctxΘ, t1) = infer(ctxΓ, e1)
        inferApp(ctxΘ, t1, e2)

  def inferApp(ctxΓ: Context, t: Ty, e: Expr): (Context, Ty) = ???

  def check(ctxΓ: Context, expr: Expr, ty: Ty): Context =
    import Expr._
    import LookupVar._
    (expr, ty) match
      case (EUnit, Ty.TUnit) => ctxΓ
      case (ELam(x, e), Ty.TFun(input, output)) =>
        val ctxΓ1 = Binding.BVar(x, input) :: ctxΓ
        val ctxΔ1 = check(ctxΓ1, e, output)
        val Found(Destruct(ctxΘ, Binding.BVar(_, _), ctxΔ)) = ctxΔ1.lookupVar(x)
        ctxΔ
      case (e, t) =>
        val (ctxΔ, t1) = infer(ctxΓ, e)
        ???


