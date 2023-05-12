package scalus.uplc

import scalus.builtins.ByteString
import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.collection.immutable

// TODO match Haskell errors
class EvaluationFailure(msg: String) extends Exception(msg)
class BuiltinError(term: Term, cause: Throwable)
    extends Exception(s"Error during builtin invocation: $term", cause)

class UnexpectedBuiltinTermArgumentMachineError(term: Term)
    extends Exception(s"Unexpected builtin term argument: $term")

class KnownTypeUnliftingError(expected: TypeScheme, actual: DefaultUni)
    extends Exception(s"Expected $expected, but got $actual")

/*
  TODO:
  - proper exception handling
  - execution budget calculation
 */
object Cek:

  sealed trait Context
  case class FrameApplyFun(f: CekValue, ctx: Context) extends Context
  case class FrameApplyArg(env: CekValEnv, arg: Term, ctx: Context) extends Context
  case class FrameForce(ctx: Context) extends Context
  case object NoFrame extends Context

  type CekValEnv = immutable.List[(String, CekValue)]
  // 'Values' for the modified CEK machine.
  sealed trait CekValue {
    def asInteger: BigInt = this match {
      case VCon(Constant.Integer(i)) => i
      case _                         => throw new RuntimeException(s"Expected integer, got $this")
    }
    def asString: String = this match {
      case VCon(Constant.String(s)) => s
      case _                        => throw new RuntimeException(s"Expected string, got $this")
    }
    def asBool: Boolean = this match {
      case VCon(Constant.Bool(b)) => b
      case _                      => throw new RuntimeException(s"Expected bool, got $this")
    }
    def asByteString: ByteString = this match {
      case VCon(Constant.ByteString(bs)) => bs
      case _ => throw new RuntimeException(s"Expected bytestring, got $this")
    }
    def asData: Data = this match {
      case VCon(Constant.Data(d)) => d
      case _                      => throw new RuntimeException(s"Expected data, got $this")
    }
    def asList: List[Constant] = this match {
      case VCon(Constant.List(_, l)) => l
      case _                         => throw new RuntimeException(s"Expected list, got $this")
    }
    def asPair: (Constant, Constant) = this match {
      case VCon(Constant.Pair(l, r)) => (l, r)
      case _                         => throw new RuntimeException(s"Expected pair, got $this")
    }
  }
  case class VCon(const: Constant) extends CekValue
  case class VDelay(term: Term, env: CekValEnv) extends CekValue
  case class VLamAbs(name: String, term: Term, env: CekValEnv) extends CekValue
  case class VBuiltin(bn: DefaultFun, term: Term, runtime: Runtime) extends CekValue

  def evalUPLC(term: Term): Term = computeCek(NoFrame, Nil, term)

  def evalUPLCProgram(p: Program): Term = evalUPLC(p.term)

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
        Meaning.BuiltinMeanings.get(bn) match
          case Some(meaning) => returnCek(ctx, VBuiltin(bn, term, meaning))
          case None          => throw new UnexpectedBuiltinTermArgumentMachineError(term)
      case Error(msg) => throw new EvaluationFailure(msg)

  def returnCek(ctx: Context, value: CekValue): Term =
    ctx match
      case FrameApplyArg(env, arg, ctx) => computeCek(FrameApplyFun(value, ctx), env, arg)
      case FrameApplyFun(fun, ctx)      => applyEvaluate(ctx, fun, value)
      case FrameForce(ctx)              => forceEvaluate(ctx, value)
      case NoFrame                      => dischargeCekValue(value)

  private def lookupVarName(env: CekValEnv, name: NamedDeBruijn): CekValue =
    env.collectFirst {
      case (n, v) if n == name.name => v
    } match
      case Some(value) => value
      case None =>
        throw new EvaluationFailure(
          s"Variable ${name.name} not found in environment: ${env.map(_._1).mkString(", ")}"
        )

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
    def go(localEnv: List[String], term: Term): Term =
      term match
        case Var(name) =>
          if localEnv.contains(name.name) then term
          else
            try
              dischargeCekValue(lookupVarName(env, name))
            catch {
              case _: Throwable => term
            } // TODO proper exception
        case LamAbs(name, body) => LamAbs(name, go(name :: localEnv, body))
        case Apply(fun, arg) =>
          Apply(go(localEnv, fun), go(localEnv, arg))
        case Force(term) => Force(go(localEnv, term))
        case Delay(term) => Delay(go(localEnv, term))
        case _           => term
    go(Nil, term)

  def evalBuiltinApp(builtinName: DefaultFun, term: Term, runtime: Runtime): CekValue =
    runtime.typeScheme match
      case TypeScheme.Type(_) | TypeScheme.TVar(_) =>
        // spendBudgetCek
        // eval the builtin and return result
        val f = runtime.f.asInstanceOf[() => CekValue]
//        println(s"evaluating builtin $builtinName with runtime $runtime")
        try f()
        catch case e: Throwable => throw new BuiltinError(term, e)
      case _ => VBuiltin(builtinName, term, runtime)
