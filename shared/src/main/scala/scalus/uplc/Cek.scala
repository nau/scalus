package scalus.uplc

import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.collection.immutable

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
  case class VBuiltin(bn: DefaultFun, term: Term) extends CekValue

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
        returnCek(ctx, VBuiltin(bn, term))
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
      case VBuiltin(fun, term) =>
        val argTerm = dischargeCekValue(arg)
        val term1 = Apply(term, argTerm)
        val res = evalBuiltinApp(fun, term1)
        returnCek(ctx, res)
      case _ =>
        throw new RuntimeException("NonFunctionalApplicationMachineError") // FIXME MachineException

  def forceEvaluate(ctx: Context, value: CekValue): Term =
    value match
      case VDelay(term, env)  => computeCek(ctx, env, term)
      case VBuiltin(bn, term) => ??? // TODO implement
      case _                  =>
        // TODO proper exception
        throw new RuntimeException("NonPolymorphicInstantiationMachineError")

  def dischargeCekValue(value: CekValue): Term =
    value match
      case VCon(const)              => Const(const)
      case VDelay(term, env)        => dischargeCekValEnv(env, Delay(term))
      case VLamAbs(name, term, env) => dischargeCekValEnv(env, LamAbs(name, term))
      case VBuiltin(fun, term)      => term

  def dischargeCekValEnv(env: CekValEnv, term: Term): Term =
    term match
      case Var(name) =>
        try
          dischargeCekValue(lookupVarName(env, name))
        catch { case _: Throwable => term } // TODO proper exception
      case LamAbs(name, term) => term // FIXME it's not implemented correctly yet
      case Apply(fun, arg)    => Apply(dischargeCekValEnv(env, fun), dischargeCekValEnv(env, arg))
      case Force(term)        => Force(dischargeCekValEnv(env, term))
      case Delay(term)        => Delay(dischargeCekValEnv(env, term))
      case _                  => term

  def evalBuiltinApp(builtinName: DefaultFun, term: Term) = ???
