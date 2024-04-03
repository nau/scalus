package scalus.uplc
package eval

import cats.syntax.group.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.ledger.babbage.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.immutable
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.control.NonFatal

enum StepKind:
    case Const, Var, LamAbs, Apply, Delay, Force, Builtin

enum ExBudgetCategory:
    case Step(kind: StepKind)
    case Startup
    case BuiltinApp(bi: DefaultFun)

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

object CekMachineCosts {
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

    def fromMap(map: Map[String, Int]): CekMachineCosts = {
        def get(key: String) = {
            val cpu = s"${key}-exBudgetCPU"
            val memory = s"${key}-exBudgetMemory"
            ExBudget.fromCpuAndMemory(
              cpu = map.getOrElse(cpu, throw new IllegalArgumentException(s"Missing key: $cpu")),
              memory =
                  map.getOrElse(memory, throw new IllegalArgumentException(s"Missing key: $memory"))
            )
        }

        CekMachineCosts(
          startupCost = get("cekStartupCost"),
          varCost = get("cekVarCost"),
          constCost = get("cekConstCost"),
          lamCost = get("cekLamCost"),
          delayCost = get("cekDelayCost"),
          forceCost = get("cekForceCost"),
          applyCost = get("cekApplyCost"),
          builtinCost = get("cekBuiltinCost")
        )
    }
}

/** The Plutus CEK machine parameters.
  *
  * @param machineCosts
  *   The machine costs
  * @param builtinCostModel
  *   The builtin cost model
  */
case class MachineParams(
    machineCosts: CekMachineCosts,
    builtinCostModel: BuiltinCostModel
)

object MachineParams {

    /** The default machine parameters.
      * @note
      *   The default machine parameters use machine costs and builtin cost model that may be
      *   outdated, making budget calculation not precise. Please use
      *   `fromCardanoCliProtocolParamsJson` etc to create machine parameters with the latest costs.
      */
    val defaultParams: MachineParams = MachineParams(
      machineCosts = CekMachineCosts.defaultMachineCosts,
      builtinCostModel = BuiltinCostModel.defaultCostModel
    )

    /** Creates `MachineParams` from a Cardano CLI protocol parameters JSON.
      *
      * @param json
      *   The Cardano CLI protocol parameters JSON
      * @param plutus
      *   The plutus version
      * @return
      *   The machine parameters
      */
    def fromCardanoCliProtocolParamsJson(
        json: String,
        plutus: PlutusLedgerLanguage
    ): MachineParams = {
        import upickle.default.*
        val pparams = read[ProtocolParams](json)
        fromProtocolParams(pparams, plutus)
    }

    /** Creates `MachineParams` from a Blockfrost protocol parameters JSON.
      *
      * @param json
      *   The Blockfrost protocol parameters JSON
      * @param plutus
      *   The plutus version
      * @return
      *   The machine parameters
      */
    def fromBlockfrostProtocolParamsJson(
        json: String,
        plutus: PlutusLedgerLanguage
    ): MachineParams = {
        import upickle.default.*
        val pparams = read[ProtocolParams](json)(using ProtocolParams.blockfrostParamsRW)
        fromProtocolParams(pparams, plutus)
    }

    /** Creates [[MachineParams]] from a [[ProtocolParams]] and a [[PlutusLedgerLanguage]]
      */
    def fromProtocolParams(pparams: ProtocolParams, plutus: PlutusLedgerLanguage): MachineParams = {
        import upickle.default.*
        val paramsMap = plutus match
            case PlutusLedgerLanguage.PlutusV1 =>
                val costs = pparams.costModels("PlutusV1")
                val params = PlutusV1Params.fromSeq(costs)
                writeJs(params).obj.map { case (k, v) => (k, v.num.toInt) }.toMap
            case PlutusLedgerLanguage.PlutusV2 =>
                val costs = pparams.costModels("PlutusV2")
                val params = PlutusV2Params.fromSeq(costs)
                writeJs(params).obj.map { case (k, v) =>
                    (k, v.num.toInt)
                }.toMap
            case PlutusLedgerLanguage.PlutusV3 =>
                throw new NotImplementedError("PlutusV3 not supported yet")

        val builtinCostModel = BuiltinCostModel.fromCostModelParams(paramsMap)
        val machineCosts = CekMachineCosts.fromMap(paramsMap)
        MachineParams(machineCosts = machineCosts, builtinCostModel = builtinCostModel)
    }
}

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
    case VBuiltin(bn: DefaultFun, term: () => Term, runtime: BuiltinRuntime)

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

trait VMBase {
    type ScriptForEvaluation = Array[Byte]

    /** Evaluates a script, returning the minimum budget that the script would need to evaluate.
      * This will take as long as the script takes, if you need to limit the execution time of the
      * script also, you can use 'evaluateScriptRestricting', which also returns the used budget.
      *
      * @note:
      *   Parameterized over the ledger-plutus-version since the builtins allowed (during decoding)
      *   differs.
      * @param params
      *   The machine parameters
      * @param script
      *   Flat encoded script to evaluate
      * @param args
      *   The arguments to the script
      * @return
      *   [[CekResult]], either a success with the resulting term and the execution budget, or a
      *   failure with an error message and the execution budget.
      */
    @varargs
    def evaluateScriptCounting(
        params: MachineParams,
        script: ScriptForEvaluation,
        args: Data*
    ): CekResult = {
        val namedProgram = DeBruijn.fromDeBruijnProgram(ProgramFlatCodec.decodeFlat(script))
        val applied = args.foldLeft(namedProgram.term) { (acc, arg) =>
            Apply(acc, Const(asConstant(arg)))
        }
        evaluateTerm(params, applied)
    }

    /** Evaluates a UPLC term.
      *
      * @param params
      *   The machine parameters
      * @param term
      *   The debruijned term to evaluate
      * @return
      *   [[CekResult]], either a success with the resulting term and the execution budget, or a
      *   failure with an error message and the execution budget.
      */
    def evaluateTerm(params: MachineParams, term: Term): CekResult
}

sealed trait CekResult {
    def budget: ExBudget
    def logs: Array[String]
    def isSuccess: Boolean
    def isFailure: Boolean
}

object CekResult {
    case class Success(term: Term, logs: Array[String], budget: ExBudget) extends CekResult {
        def isSuccess: Boolean = true
        def isFailure: Boolean = false

        override def toString(): String =
            s"Success($term, ${logs.mkString("[", "\n", "]")}, $budget)"
    }
    case class Failure(msg: String, env: CekValEnv, logs: Array[String], budget: ExBudget)
        extends CekResult {
        def getCekStack: Array[String] = env.view.reverse.map(_._1).toArray
        def isSuccess: Boolean = false
        def isFailure: Boolean = true
    }
}

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

/** CEK machine implementation based on Cardano Plutus CEK machine.
  *
  * The CEK machine is a stack-based abstract machine that is used to evaluate UPLC terms.
  *
  * @note
  *   The machine is statuful and should not be shared between threads. It stores the logs and the
  *   budget spent during the evaluation.
  *
  * @param params
  *   The machine parameters [[MachineParams]]
  * @see
  *   https://github.com/input-output-hk/plutus/blob/41a7afebc4cee277bab702ee1678c070e5e38810/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Evaluation/Machine/Cek/Internal.hs
  * @example
  *   {{{
  *   val term = LamAbs("x", Apply(Var(NamedDeBruijn("x", 0)), Var(NamedDeBruijn("x", 0))))
  *   val cek = new CekMachine(MachineParams.defaultParams)
  *   val res = cek.runCek(term)
  *   }}}
  */
abstract class AbstractCekMachine(val params: MachineParams)
    extends BuiltinsMeaning(params.builtinCostModel) {
    import CekState.*
    import CekValue.*
    import Context.*

    private[uplc] val logs: ArrayBuffer[String] = ArrayBuffer.empty[String]
    def getLogs: Array[String] = logs.toArray
    def log(msg: String): Unit = logs.append(msg)

    /** Evaluates a UPLC term.
      *
      * @param term
      *   The debruijned term to evaluate
      * @return
      *   The resulting term
      * @throws StackTraceMachineError
      */
    def evaluateTerm(term: Term): Term = {
        @tailrec def loop(state: CekState): Term = {
            state match
                case Compute(ctx, env, term) => loop(computeCek(ctx, env, term))
                case Return(ctx, env, value) => loop(returnCek(ctx, env, value))
                case Done(term)              => term
        }
        spendBudget(ExBudgetCategory.Startup, params.machineCosts.startupCost)
        loop(Compute(NoFrame, ArraySeq.empty, term))
    }

    /** Evaluate a UPLC term.
      *
      * @param term
      *   The debruijned term to evaluate
      * @return
      *   [[CekResult]], either a success with the resulting term and the execution budget, or a
      *   failure with an error message and the execution budget.
      */
    def runCek(term: Term): CekResult = {
        try
            val res = DeBruijn.fromDeBruijnTerm(evaluateTerm(term))
            CekResult.Success(res, getLogs, getExBudget)
        catch
            case e: StackTraceMachineError =>
                CekResult.Failure(e.getMessage, e.env, getLogs, getExBudget)
    }

    private final def computeCek(ctx: Context, env: CekValEnv, term: Term): CekState = {
        val costs = params.machineCosts
        term match
            case Var(name) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Var), costs.varCost)
                Return(ctx, env, lookupVarName(env, name))
            case LamAbs(name, term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.LamAbs), costs.lamCost)
                Return(ctx, env, VLamAbs(name, term, env))
            case Apply(fun, arg) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Apply), costs.applyCost)
                Compute(FrameApplyArg(env, arg, ctx), env, fun)
            case Force(term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Force), costs.forceCost)
                Compute(FrameForce(ctx), env, term)
            case Delay(term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Delay), costs.delayCost)
                Return(ctx, env, VDelay(term, env))
            case Const(const) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Const), costs.constCost)
                Return(ctx, env, VCon(const))
            case Builtin(bn) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Builtin), costs.builtinCost)
                // The @term@ is a 'Builtin', so it's fully discharged.
                BuiltinMeanings.get(bn) match
                    case Some(meaning) => Return(ctx, env, VBuiltin(bn, () => term, meaning))
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
                val term1 = () => Apply(term(), dischargeCekValue(arg))
                runtime.typeScheme match
                    case TypeScheme.Arrow(_, rest) =>
                        val runtime1 = runtime.copy(args = runtime.args :+ arg, typeScheme = rest)
                        val res = evalBuiltinApp(env, fun, term1, runtime1)
                        Return(ctx, env, res)
                    case _ => throw new UnexpectedBuiltinTermArgumentMachineError(term1(), env)
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
                val term1 = () => Force(term())
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
                    case _ => throw new BuiltinTermArgumentExpectedMachineError(term1(), env)
            case _ =>
                throw new NonPolymorphicInstantiationMachineError(value, env)
    }

    private def evalBuiltinApp(
        env: CekValEnv,
        builtinName: DefaultFun,
        term: () => Term, // lazily discharge the term as it might not be needed
        runtime: BuiltinRuntime
    ): CekValue = {
        runtime.typeScheme match
            case TypeScheme.Type(_) | TypeScheme.TVar(_) | TypeScheme.App(_, _) =>
                spendBudget(ExBudgetCategory.BuiltinApp(builtinName), runtime.calculateCost)
                // eval the builtin and return result
                try
                    // eval builtin when it's fully saturated, i.e. when all arguments were applied
                    runtime.apply()
                catch case NonFatal(e) => throw new BuiltinError(builtinName, term(), e, env)
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
            case VBuiltin(_, term, _)     => term()
    }

    private var budgetCPU = 0L
    private var budgetMemory = 0L

    val builtinBudgetCosts: HashMap[DefaultFun, ExBudget] = HashMap.empty

    def getExBudget: ExBudget = ExBudget(ExCPU(budgetCPU), ExMemory(budgetMemory))

    private def spendBudget(cat: ExBudgetCategory, budget: ExBudget): Unit = {
        budgetCPU += budget.cpu
        budgetMemory += budget.memory
        cat match
            case ExBudgetCategory.BuiltinApp(builtin) =>
                builtinBudgetCosts.updateWith(builtin) {
                    case Some(b) => Some(b |+| budget)
                    case None    => Some(budget)
                }
            case _ =>
    }
}
