package scalus.uplc

import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.collection.immutable

// TODO match Haskell errors
class UnexpectedBuiltinTermArgumentMachineError(term: Term)
    extends Exception(s"Unexpected builtin term argument: $term")

class KnownTypeUnliftingError(expected: TypeScheme, actual: DefaultUni)
    extends Exception(s"Expected $expected, but got $actual")

object Cek:

  sealed trait Context
  case class FrameApplyFun(f: CekValue, ctx: Context) extends Context
  case class FrameApplyArg(env: CekValEnv, arg: Term, ctx: Context) extends Context
  case class FrameForce(ctx: Context) extends Context
  case object NoFrame extends Context

  type CekValEnv = immutable.List[(String, CekValue)]
  // 'Values' for the modified CEK machine.
  sealed trait CekValue
  case class VCon(const: Constant) extends CekValue
  case class VDelay(term: Term, env: CekValEnv) extends CekValue
  case class VLamAbs(name: String, term: Term, env: CekValEnv) extends CekValue
  case class VBuiltin(bn: DefaultFun, term: Term, runtime: Runtime) extends CekValue

  def evalUPLC(term: Term): Term =
    computeCek(NoFrame, Nil, term)

  @tailrec def computeCek(ctx: Context, env: CekValEnv, term: Term): Term =
    term match
      case Var(name)          => returnCek(ctx, lookupVarName(env, name))
      case LamAbs(name, term) => returnCek(ctx, VLamAbs(name, term, env))
      case Apply(fun, arg)    => computeCek(FrameApplyArg(env, arg, ctx), env, fun)
      case Force(term)        => computeCek(FrameForce(ctx), env, term)
      case Delay(term)        => returnCek(ctx, VDelay(term, env))
      case Const(const)       => returnCek(ctx, VCon(const))
      case Builtin(bn)        =>
        // The @term@ is a 'Builtin', so it's fully discharged.
        val rt = Meaning.AddInteger
        returnCek(ctx, VBuiltin(bn, term, rt))
      case Error => throw new RuntimeException("Error")

  def returnCek(ctx: Context, value: CekValue): Term =
    ctx match
      case FrameApplyArg(env, arg, ctx) => computeCek(FrameApplyFun(value, ctx), env, arg)
      case FrameApplyFun(fun, ctx)      => applyEvaluate(ctx, fun, value)
      case FrameForce(ctx)              => forceEvaluate(ctx, value)
      case NoFrame                      => dischargeCekValue(value)

  def lookupVarName(env: CekValEnv, name: String): CekValue = env.collectFirst {
    case (n, v) if n == name => v
  }.get

  def applyEvaluate(ctx: Context, fun: CekValue, arg: CekValue): Term =
    fun match
      case VLamAbs(name, term, env) => computeCek(ctx, (name, arg) :: env, term)
      case VBuiltin(fun, term, runtime) =>
        val argTerm = dischargeCekValue(arg)
        val term1 = Apply(term, argTerm)
        runtime.typeScheme match
          case (TypeScheme.Arrow(_, rest)) =>
            val f = runtime.f.asInstanceOf[CekValue => AnyRef]
            val applied = f(arg)
            val runtime1 = runtime.copy(f = applied, typeScheme = rest)
            val res = evalBuiltinApp(fun, term1, runtime1)
            returnCek(ctx, res)
          case _ => throw new UnexpectedBuiltinTermArgumentMachineError(term1)
      case _ =>
        throw new RuntimeException("NonFunctionalApplicationMachineError") // FIXME MachineException

  def forceEvaluate(ctx: Context, value: CekValue): Term =
    value match
      case VDelay(term, env) => computeCek(ctx, env, term)
      case VBuiltin(bn, term, rt) =>
        val term1 = Force(term)
        rt.typeScheme match
          // It's only possible to force a builtin application if the builtin expects a type
          // argument next.
          case TypeScheme.All(_, t) =>
            val runtime1 = rt.copy(typeScheme = t)
            // We allow a type argument to appear last in the type of a built-in function,
            // otherwise we could just assemble a 'VBuiltin' without trying to evaluate the
            // application.
            val res = evalBuiltinApp(bn, term1, runtime1)
            returnCek(ctx, res)
          case _ => throw new UnexpectedBuiltinTermArgumentMachineError(term1)
      case _ =>
        // TODO proper exception
        throw new RuntimeException("NonPolymorphicInstantiationMachineError")

  def dischargeCekValue(value: CekValue): Term =
    value match
      case VCon(const)              => Const(const)
      case VDelay(term, env)        => dischargeCekValEnv(env, Delay(term))
      case VLamAbs(name, term, env) => dischargeCekValEnv(env, LamAbs(name, term))
      case VBuiltin(_, term, _)     => term

  def dischargeCekValEnv(env: CekValEnv, term: Term): Term =
    term match
      case Var(name) =>
        try
          dischargeCekValue(lookupVarName(env, name))
        catch {
          case _: Throwable => term
        } // TODO proper exception
      case LamAbs(name, term) => term // FIXME it's not implemented correctly yet
      case Apply(fun, arg) =>
        Apply(dischargeCekValEnv(env, fun), dischargeCekValEnv(env, arg))
      case Force(term) => Force(dischargeCekValEnv(env, term))
      case Delay(term) => Delay(dischargeCekValEnv(env, term))
      case _           => term

  def evalBuiltinApp(builtinName: DefaultFun, term: Term, runtime: Runtime): CekValue =
    runtime.typeScheme match
      case TypeScheme.Type(_) | TypeScheme.TVar(_) =>
        // spendBudgetCek
        // eval the builtin and return result
        println(runtime.f.getClass.getName)
        val f = runtime.f.asInstanceOf[() => Constant]
        val r = f()
        VCon(r)
      case _ => VBuiltin(builtinName, term, runtime)
