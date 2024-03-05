package scalus.uplc
package eval

import scalus.builtin.{ByteString, Data, PlatformSpecific}
import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.ArraySeq
import scala.util.control.NonFatal

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

class MachineError(msg: String) extends RuntimeException(msg)

class StackTraceMachineError(msg: String, val env: CekValEnv) extends MachineError(msg) {
    def getCekStack: Array[String] = env.view.reverse.map(_._1).toArray
}

class NonPolymorphicInstantiationMachineError(value: CekValue, env: CekValEnv)
    extends StackTraceMachineError(s"Non-polymorphic instantiation: $value", env)

class NonFunctionalApplicationMachineError(arg: CekValue, env: CekValEnv)
    extends StackTraceMachineError(s"Non-functional application: $arg", env)

class OpenTermEvaluatedMachineError(name: NamedDeBruijn, env: CekValEnv)
    extends StackTraceMachineError(
      s"Variable ${name.name} not found in environment: ${env.reverse.map(_._1).mkString(", ")}",
      env
    )

class BuiltinTermArgumentExpectedMachineError(term: Term, env: CekValEnv)
    extends StackTraceMachineError(s"Expected builtin term argument, got $term", env)

class UnexpectedBuiltinTermArgumentMachineError(term: Term, env: CekValEnv)
    extends StackTraceMachineError(s"Unexpected builtin term argument: $term", env)

class UnknownBuiltin(builtin: DefaultFun, env: CekValEnv)
    extends StackTraceMachineError(s"Unknown builtin: $builtin", env)

class EvaluationFailure(env: CekValEnv) extends StackTraceMachineError("Error evaluated", env)

class BuiltinException(msg: String) extends MachineError(msg)

class DeserializationError(fun: DefaultFun, value: CekValue)
    extends BuiltinException(s"Deserialization error in $fun: $value")

class KnownTypeUnliftingError(expected: DefaultUni, actual: CekValue)
    extends BuiltinException(s"Expected type $expected, got $actual")

class BuiltinError(builtin: DefaultFun, term: Term, cause: Throwable, env: CekValEnv)
    extends StackTraceMachineError(s"Builtin error: $builtin $term, caused by $cause", env)

type CekValEnv = immutable.ArraySeq[(String, CekValue)]

// 'Values' for the modified CEK machine.
enum CekValue {
    case VCon(const: Constant)
    case VDelay(term: Term, env: CekValEnv)
    case VLamAbs(name: String, term: Term, env: CekValEnv)
    case VBuiltin(bn: DefaultFun, term: Term, runtime: Runtime)

    def asUnit: Unit = this match {
        case VCon(Constant.Unit) => ()
        case _                   => throw new KnownTypeUnliftingError(DefaultUni.Unit, this)
    }
    def asInteger: BigInt = this match {
        case VCon(Constant.Integer(i)) => i
        case _ => throw new KnownTypeUnliftingError(DefaultUni.Integer, this)
    }
    def asString: String = this match {
        case VCon(Constant.String(s)) => s
        case _                        => throw new KnownTypeUnliftingError(DefaultUni.String, this)
    }
    def asBool: Boolean = this match {
        case VCon(Constant.Bool(b)) => b
        case _                      => throw new KnownTypeUnliftingError(DefaultUni.Bool, this)
    }
    def asByteString: ByteString = this match {
        case VCon(Constant.ByteString(bs)) => bs
        case _ => throw new KnownTypeUnliftingError(DefaultUni.ByteString, this)
    }
    def asData: Data = this match {
        case VCon(Constant.Data(d)) => d
        case _                      => throw new KnownTypeUnliftingError(DefaultUni.Data, this)
    }
    def asList: List[Constant] = this match {
        case VCon(Constant.List(_, l)) => l
        case _ => throw new KnownTypeUnliftingError(DefaultUni.ProtoList, this)
    }
    def asPair: (Constant, Constant) = this match {
        case VCon(Constant.Pair(l, r)) => (l, r)
        case _ => throw new KnownTypeUnliftingError(DefaultUni.ProtoPair, this)
    }
}

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
        new CekMachine(evaluationContext).evalCek(debruijnedTerm)
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

sealed trait CekResult
object CekResult:
    case class Success(term: Term, budget: ExBudget) extends CekResult
    case class Failure(msg: String, env: CekValEnv, budget: ExBudget) extends CekResult {
        def getCekStack: Array[String] = env.view.reverse.map(_._1).toArray
    }

class CekMachine(val evaluationContext: EvaluationContext) {

    private enum Context {
        case FrameApplyFun(f: CekValue, ctx: Context)
        case FrameApplyArg(env: CekValEnv, arg: Term, ctx: Context)
        case FrameForce(ctx: Context)
        case NoFrame
    }

    private enum CekState {
        case Return(ctx: Context, env: CekValEnv, value: CekValue)
        case Compute(ctx: Context, env: CekValEnv, term: Term)
        case Done(term: Term)

    }
    import CekState.*
    import CekValue.*
    import Context.*

    /** Evaluates a UPLC term.
      *
      * @param term
      *   The term to evaluate
      * @return
      *   The resulting term
      * @throws StackTraceMachineError
      */
    def evalCek(term: Term): Term = {
        @tailrec def loop(state: CekState): Term = {
            state match
                case Compute(ctx, env, term) => loop(computeCek(ctx, env, term))
                case Return(ctx, env, value) => loop(returnCek(ctx, env, value))
                case Done(term)              => term
        }
        spendBudget(ExBudgetCategory.Startup)
        loop(Compute(NoFrame, ArraySeq.empty, term))
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
            val res = evalCek(term)
            CekResult.Success(res, getExBudget)
        catch case e: StackTraceMachineError => CekResult.Failure(e.getMessage, e.env, getExBudget)
    }

    private final def computeCek(ctx: Context, env: CekValEnv, term: Term): CekState = {
        spendBudget(ExBudgetCategory.Step(term))
        term match
            case Var(name)          => Return(ctx, env, lookupVarName(env, name))
            case LamAbs(name, term) => Return(ctx, env, VLamAbs(name, term, env))
            case Apply(fun, arg)    => Compute(FrameApplyArg(env, arg, ctx), env, fun)
            case Force(term)        => Compute(FrameForce(ctx), env, term)
            case Delay(term)        => Return(ctx, env, VDelay(term, env))
            case Const(const)       => Return(ctx, env, VCon(const))
            case Builtin(bn)        =>
                // The @term@ is a 'Builtin', so it's fully discharged.
                Meaning.BuiltinMeanings.get(bn) match
                    case Some(meaning) => Return(ctx, env, VBuiltin(bn, term, meaning))
                    case None          => throw new UnknownBuiltin(bn, env)
            case Error => throw new EvaluationFailure(env)
    }

    private def returnCek(ctx: Context, env: CekValEnv, value: CekValue): CekState = {
        ctx match
            case FrameApplyArg(env, arg, ctx) => Compute(FrameApplyFun(value, ctx), env, arg)
            case FrameApplyFun(fun, ctx)      => applyEvaluate(ctx, env, fun, value)
            case FrameForce(ctx)              => forceEvaluate(ctx, env, value)
            case NoFrame                      => Done(dischargeCekValue(value))
    }

    private def lookupVarName(env: CekValEnv, name: NamedDeBruijn): CekValue = {
        if name.index > env.size then throw new OpenTermEvaluatedMachineError(name, env)
        else env(env.size - name.index)._2
    }

    private def applyEvaluate(
        ctx: Context,
        env: CekValEnv,
        fun: CekValue,
        arg: CekValue
    ): CekState = {
        fun match
            case VLamAbs(name, term, env) => Compute(ctx, env :+ (name, arg), term)
            case VBuiltin(fun, term, runtime) =>
                val argTerm = dischargeCekValue(arg)
                val term1 = Apply(term, argTerm)
                runtime.typeScheme match
                    case TypeScheme.Arrow(_, rest) =>
                        val runtime1 = runtime.copy(args = runtime.args :+ arg, typeScheme = rest)
                        val res = evalBuiltinApp(env, fun, term1, runtime1)
                        Return(ctx, env, res)
                    case _ => throw new UnexpectedBuiltinTermArgumentMachineError(term1, env)
            case _ =>
                throw new NonFunctionalApplicationMachineError(fun, env)
    }

    /** `force`` a term and proceed.
      *
      * If `value` is a delay then compute the body of `value`; if v is a builtin application then
      * check that it's expecting a type argument, and either calculate the builtin application or
      * stick a 'Force' on top of its 'Term' representation depending on whether the application is
      * saturated or not, if v is anything else, fail.
      */
    private def forceEvaluate(ctx: Context, env: CekValEnv, value: CekValue): CekState = {
        value match
            case VDelay(term, env) => Compute(ctx, env, term)
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
                        val res = evalBuiltinApp(env, bn, term1, runtime1)
                        Return(ctx, env, res)
                    case _ => throw new BuiltinTermArgumentExpectedMachineError(term1, env)
            case _ =>
                throw new NonPolymorphicInstantiationMachineError(value, env)
    }

    private def evalBuiltinApp(
        env: CekValEnv,
        builtinName: DefaultFun,
        term: Term,
        runtime: Runtime
    ): CekValue = {
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
                catch case NonFatal(e) => throw new BuiltinError(builtinName, term, e, env)
            case _ => VBuiltin(builtinName, term, runtime)
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
                val budget = costingFun.calculateCost(args: _*)
                budgetCPU += budget.cpu
                budgetMemory += budget.memory
    }
}
