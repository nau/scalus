package scalus.uplc
package eval

import cats.kernel.Group
import scalus.builtin.{ByteString, Data, PlatformSpecific}
import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.ArraySeq

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

    /** The zero budget */
    val zero: ExBudget = ExBudget(ExCPU(0), ExMemory(0))

    /** Constructs an 'ExBudget' from CPU and memory components. */
    def fromCpuAndMemory(cpu: Long, memory: Long): ExBudget = ExBudget(ExCPU(cpu), ExMemory(memory))

    /// Cats Group instance for ExBudget
    given Group[ExBudget] with
        def combine(x: ExBudget, y: ExBudget): ExBudget =
            ExBudget(ExCPU(x.cpu + y.cpu), ExMemory(x.memory + y.memory))
        def empty: ExBudget = ExBudget.zero
        def inverse(x: ExBudget): ExBudget = ExBudget(ExCPU(-x.cpu), ExMemory(-x.memory))

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

type CekValEnv = immutable.ArraySeq[(String, CekValue)]

// 'Values' for the modified CEK machine.
enum CekValue {
    case VCon(const: Constant)
    case VDelay(term: Term, env: CekValEnv)
    case VLamAbs(name: String, term: Term, env: CekValEnv)
    case VBuiltin(bn: DefaultFun, term: Term, runtime: Runtime)

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
/*
  TODO:
  - proper exception handling
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
        val debruijnedTerm = DeBruijn.deBruijnTerm(term)
        new CekMachine(evaluationContext).evalUPLC(debruijnedTerm)
    }

    def evalUPLCProgram(p: Program)(using ps: PlatformSpecific): Term = evalUPLC(p.term)

    val defaultMachineCosts: CekMachineCosts = CekMachineCosts(
      startupCost = ExBudget(ExCPU(100), ExMemory(100)),
      varCost = ExBudget(ExCPU(23000), ExMemory(100)),
      constCost = ExBudget(ExCPU(23000), ExMemory(100)),
      lamCost = ExBudget(ExCPU(23000), ExMemory(100)),
      delayCost = ExBudget(ExCPU(23000), ExMemory(100)),
      forceCost = ExBudget(ExCPU(23000), ExMemory(100)),
      applyCost = ExBudget(ExCPU(23000), ExMemory(100)),
      builtinCost = ExBudget(ExCPU(23000), ExMemory(100))
    )

    def defaultEvaluationContext(using ps: PlatformSpecific): EvaluationContext = EvaluationContext(
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

    enum Context {
        case FrameApplyFun(f: CekValue, ctx: Context)
        case FrameApplyArg(env: CekValEnv, arg: Term, ctx: Context)
        case FrameForce(ctx: Context)
        case NoFrame
    }
    import CekValue.*
    import Context.*

    def evalUPLC(term: Term): Term = {
        spendBudget(ExBudgetCategory.Startup)
        computeCek(NoFrame, ArraySeq.empty, term)
    }

    /** Evaluate a UPLC term.
      *
      * @param term
      *   The term to evaluate
      * @return
      *   CekResult, either a success with the resulting term and the execution budget, or a failure
      *   with an error message and the execution budget.
      */
    def runCek(term: Term): CekResult = {
        try
            val res = evalUPLC(term)
            CekResult.Success(res, getExBudget)
        catch case e: Exception => CekResult.Failure(e.getMessage, getExBudget)
    }

    @tailrec private final def computeCek(ctx: Context, env: CekValEnv, term: Term): Term = {
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
    }

    private def returnCek(ctx: Context, value: CekValue): Term = {
        ctx match
            case FrameApplyArg(env, arg, ctx) => computeCek(FrameApplyFun(value, ctx), env, arg)
            case FrameApplyFun(fun, ctx)      => applyEvaluate(ctx, fun, value)
            case FrameForce(ctx)              => forceEvaluate(ctx, value)
            case NoFrame                      => dischargeCekValue(value)
    }

    private def lookupVarName(env: CekValEnv, name: NamedDeBruijn): CekValue = {
        if name.index > env.size then
            throw new EvaluationFailure(
              s"Variable ${name.name} not found in environment: ${env.reverse.map(_._1).mkString(", ")}"
            )
        else env(env.size - name.index)._2
    }

    private def applyEvaluate(ctx: Context, fun: CekValue, arg: CekValue): Term = {
        fun match
            case VLamAbs(name, term, env) => computeCek(ctx, env :+ (name, arg), term)
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
    }

    private def forceEvaluate(ctx: Context, value: CekValue): Term = {
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
    }

    /** Converts a 'CekValue' into a 'Term' by replacing all bound variables with the terms they're
      * bound to (which themselves have to be obtain by recursively discharging values).
      */
    private def dischargeCekValue(value: CekValue): Term = {
        def dischargeCekValEnv(env: CekValEnv, term: Term): Term = {
            def go(lamCnt: Int, term: Term): Term = {
                term match
                    case Var(name) =>
                        if lamCnt >= name.index
                            // the index n is less-than-or-equal than the number of lambdas we have descended
                            // this means that n points to a bound variable, so we don't discharge it.
                        then term
                        else
                            // index relative to (as seen from the point of view of) the environment
                            val relativeIdx = env.size - (name.index - lamCnt)
                            if env.isDefinedAt(relativeIdx) then
                                // var is in the env, discharge its value
                                dischargeCekValue(env(relativeIdx)._2)
                            else
                                // var is free, leave it alone
                                term

                    case LamAbs(name, body) => LamAbs(name, go(lamCnt + 1, body))
                    case Apply(fun, arg)    => Apply(go(lamCnt, fun), go(lamCnt, arg))
                    case Force(term)        => Force(go(lamCnt, term))
                    case Delay(term)        => Delay(go(lamCnt, term))
                    case _                  => term
            }

            go(0, term)
        }

        value match
            case VCon(const)       => Const(const)
            case VDelay(term, env) => dischargeCekValEnv(env, Delay(term))
            // `computeCek` turns @LamAbs _ name body@ into @VLamAbs name body env@ where @env@ is an
            // argument of 'computeCek' and hence we need to start discharging outside of the reassembled
            // lambda, otherwise @name@ could clash with the names that we have in @env@.
            case VLamAbs(name, term, env) => dischargeCekValEnv(env, LamAbs(name, term))
            case VBuiltin(_, term, _)     => term
    }

    private def evalBuiltinApp(builtinName: DefaultFun, term: Term, runtime: Runtime): CekValue = {
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
    }

    private var budgetCPU = 0L
    private var budgetMemory = 0L

    def getExBudget: ExBudget = ExBudget(ExCPU(budgetCPU), ExMemory(budgetMemory))

    private def spendBudget(cat: ExBudgetCategory): Unit = {
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
}
