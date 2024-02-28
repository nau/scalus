package scalus.uplc
package eval

import cats.Monoid
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.PlatformSpecific
import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.collection.immutable

opaque type ExCPU <: Long = Long
object ExCPU {
    inline def apply(l: Long): ExCPU = l
}
opaque type ExMemory <: Long = Long
object ExMemory {
    inline def apply(l: Long): ExMemory = l
}

case class ExBudget(cpu: ExCPU, memory: ExMemory)
object ExBudget {
    val zero: ExBudget = ExBudget(ExCPU(0), ExMemory(0))
    def fromCpuAndMemory(cpu: Long, memory: Long): ExBudget = ExBudget(ExCPU(cpu), ExMemory(memory))
    given Monoid[ExBudget] with
        def combine(x: ExBudget, y: ExBudget): ExBudget =
            ExBudget(ExCPU(x.cpu + y.cpu), ExMemory(x.memory + y.memory))
        def empty: ExBudget = ExBudget.zero
    given Ordering[ExBudget] with
        def compare(x: ExBudget, y: ExBudget): Int =
            val c = x.cpu.compareTo(y.cpu)
            if c != 0 then c else x.memory.compareTo(y.memory)
}

enum ExBudgetCategory:
    case Step(term: Term)
    case Startup
    case BuiltinApp(costingFunction: CostingFun[CostModel], args: Seq[CekValue])

case class CekMachineCosts(
    startupCost: ExBudget,
    varCost: ExBudget,
    constCost: ExBudget,
    lamCost: ExBudget,
    delayCost: ExBudget,
    forceCost: ExBudget,
    applyCost: ExBudget,
    builtinCost: ExBudget
)

case class MachineParameters(
    machineCosts: CekMachineCosts,
    builtinCostModel: BuiltinCostModel
)

case class EvaluationContext(
    params: MachineParameters,
    platformSpecific: PlatformSpecific
)

// TODO match Haskell errors
class EvaluationFailure(msg: String) extends Exception(msg)
class BuiltinError(term: Term, cause: Throwable)
    extends Exception(s"Error during builtin invocation: $term", cause)

class UnexpectedBuiltinTermArgumentMachineError(term: Term)
    extends Exception(s"Unexpected builtin term argument: $term")

class KnownTypeUnliftingError(expected: TypeScheme, actual: DefaultUni)
    extends Exception(s"Expected $expected, but got $actual")

type CekValEnv = immutable.List[(String, CekValue)]

// 'Values' for the modified CEK machine.
sealed trait CekValue {
    def asUnit: Unit = this match {
        case VCon(Constant.Unit) => ()
        case _                   => throw new RuntimeException(s"Expected unit, got $this")
    }
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

/*
  TODO:
  - proper exception handling
  - execution budget calculation
 */
object Cek {
    def evalUPLC(term: Term)(using ps: PlatformSpecific): Term = {
        val evaluationContext = EvaluationContext(
          MachineParameters(
            machineCosts = defaultMachineCosts,
            builtinCostModel = BuiltinCostModel.defaultBuiltinCostModel
          ),
          platformSpecific = ps
        )
        new CekMachine(evaluationContext).evalUPLC(term)
    }

    def evalUPLCProgram(p: Program)(using ps: PlatformSpecific): Term = evalUPLC(p.term)

    val defaultMachineCosts = CekMachineCosts(
      startupCost = ExBudget(ExCPU(100), ExMemory(100)),
      varCost = ExBudget(ExCPU(23000), ExMemory(100)),
      constCost = ExBudget(ExCPU(23000), ExMemory(100)),
      lamCost = ExBudget(ExCPU(23000), ExMemory(100)),
      delayCost = ExBudget(ExCPU(23000), ExMemory(100)),
      forceCost = ExBudget(ExCPU(23000), ExMemory(100)),
      applyCost = ExBudget(ExCPU(23000), ExMemory(100)),
      builtinCost = ExBudget(ExCPU(23000), ExMemory(100))
    )

    def defaultEvaluationContext(using ps: PlatformSpecific) = EvaluationContext(
      MachineParameters(
        machineCosts = defaultMachineCosts,
        builtinCostModel = BuiltinCostModel.defaultBuiltinCostModel
      ),
      platformSpecific = ps
    )
}

enum CekResult:
    case Success(term: Term, budget: ExBudget)
    case Failure(msg: String, budget: ExBudget)

class CekMachine(val evaluationContext: EvaluationContext) {

    sealed trait Context
    case class FrameApplyFun(f: CekValue, ctx: Context) extends Context
    case class FrameApplyArg(env: CekValEnv, arg: Term, ctx: Context) extends Context
    case class FrameForce(ctx: Context) extends Context
    case object NoFrame extends Context

    def evalUPLC(term: Term): Term =
        spendBudget(ExBudgetCategory.Startup)
        computeCek(NoFrame, Nil, term)

    /** Evaluate a UPLC term.
      *
      * @param term
      *   The term to evaluate
      * @return
      *   CekResult, either a success with the resulting term and the execution budget, or a failure
      *   with an error message and the execution budget.
      */
    def runCek(term: Term): CekResult =
        spendBudget(ExBudgetCategory.Startup)
        try
            val res = computeCek(NoFrame, Nil, term)
            CekResult.Success(res, getExBudget)
        catch case e: Exception => CekResult.Failure(e.getMessage, getExBudget)

    @tailrec private final def computeCek(ctx: Context, env: CekValEnv, term: Term): Term =
        spendBudget(ExBudgetCategory.Step(term))
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
            case Error => throw new EvaluationFailure("Error")

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
                        val runtime1 = runtime.copy(args = runtime.args :+ arg, typeScheme = rest)
                        val res = evalBuiltinApp(fun, term1, runtime1)
                        returnCek(ctx, res)
                    case _ => throw new UnexpectedBuiltinTermArgumentMachineError(term1)
            case _ =>
                throw new RuntimeException(
                  "NonFunctionalApplicationMachineError"
                ) // FIXME MachineException

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
            case TypeScheme.Type(_) | TypeScheme.TVar(_) | TypeScheme.App(_, _) =>
                spendBudget(ExBudgetCategory.BuiltinApp(runtime.costFunction, runtime.args))
                // eval the builtin and return result
                try
                    // eval builtin when it's fully saturated, i.e. when all arguments were applied
                    val applied = runtime.args.foldLeft(runtime.f) { case (f, arg) =>
                        f(arg).asInstanceOf[AnyRef => AnyRef]
                    }
                    val f = applied.asInstanceOf[PlatformSpecific => CekValue]
                    f(evaluationContext.platformSpecific)
                catch case e: Throwable => throw new BuiltinError(term, e)
            case _ => VBuiltin(builtinName, term, runtime)

    private[this] var budgetCPU = 0L
    private[this] var budgetMemory = 0L

    def getExBudget: ExBudget = ExBudget(ExCPU(budgetCPU), ExMemory(budgetMemory))

    private def spendBudget(cat: ExBudgetCategory) =
        val machineCosts = evaluationContext.params.machineCosts
        cat match
            case ExBudgetCategory.Startup =>
                budgetCPU += machineCosts.startupCost.cpu
                budgetMemory += machineCosts.startupCost.memory
            case ExBudgetCategory.Step(term) =>
                term match
                    case _: Var =>
                        budgetCPU += machineCosts.varCost.cpu
                        budgetMemory += machineCosts.varCost.memory
                    case _: Const =>
                        budgetCPU += machineCosts.constCost.cpu
                        budgetMemory += machineCosts.constCost.memory
                    case _: LamAbs =>
                        budgetCPU += machineCosts.lamCost.cpu
                        budgetMemory += machineCosts.lamCost.memory
                    case _: Delay =>
                        budgetCPU += machineCosts.delayCost.cpu
                        budgetMemory += machineCosts.delayCost.memory
                    case _: Force =>
                        budgetCPU += machineCosts.forceCost.cpu
                        budgetMemory += machineCosts.forceCost.memory
                    case _: Apply =>
                        budgetCPU += machineCosts.applyCost.cpu
                        budgetMemory += machineCosts.applyCost.memory
                    case _: Builtin =>
                        budgetCPU += machineCosts.builtinCost.cpu
                        budgetMemory += machineCosts.builtinCost.memory
                    case Error => // do nothing
            case ExBudgetCategory.BuiltinApp(costingFun, args) =>
                val budget = costingFun.calculateCost(args)
                budgetCPU += budget.cpu
                budgetMemory += budget.memory
}
