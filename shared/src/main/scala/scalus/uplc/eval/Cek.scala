package scalus.uplc
package eval

import cats.syntax.group.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.PlatformSpecific
import scalus.ledger.api.BuiltinSemanticsVariant
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.ledger.api.ProtocolVersion
import scalus.ledger.babbage.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*

import scala.annotation.tailrec
import scala.annotation.varargs
import scala.collection.immutable
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.util.Try
import scala.util.control.NonFatal

enum StepKind:
    case Const, Var, LamAbs, Apply, Delay, Force, Builtin, Constr, Case

enum ExBudgetCategory:
    case Startup
    case Step(kind: StepKind)
    case BuiltinApp(bi: DefaultFun)

case class CekMachineCosts(
    startupCost: ExBudget,
    varCost: ExBudget,
    constCost: ExBudget,
    lamCost: ExBudget,
    delayCost: ExBudget,
    forceCost: ExBudget,
    applyCost: ExBudget,
    builtinCost: ExBudget,
    constrCost: ExBudget,
    caseCost: ExBudget
)

object CekMachineCosts {
    val defaultMachineCostsA: CekMachineCosts = CekMachineCosts(
      startupCost = ExBudget(ExCPU(100), ExMemory(100)),
      varCost = ExBudget(ExCPU(23000), ExMemory(100)),
      constCost = ExBudget(ExCPU(23000), ExMemory(100)),
      lamCost = ExBudget(ExCPU(23000), ExMemory(100)),
      delayCost = ExBudget(ExCPU(23000), ExMemory(100)),
      forceCost = ExBudget(ExCPU(23000), ExMemory(100)),
      applyCost = ExBudget(ExCPU(23000), ExMemory(100)),
      builtinCost = ExBudget(ExCPU(23000), ExMemory(100)),
      constrCost = ExBudget(ExCPU(23000), ExMemory(100)),
      caseCost = ExBudget(ExCPU(23000), ExMemory(100))
    )

    val defaultMachineCostsB: CekMachineCosts = CekMachineCosts(
      startupCost = ExBudget(ExCPU(100), ExMemory(100)),
      varCost = ExBudget(ExCPU(16000), ExMemory(100)),
      constCost = ExBudget(ExCPU(16000), ExMemory(100)),
      lamCost = ExBudget(ExCPU(16000), ExMemory(100)),
      delayCost = ExBudget(ExCPU(16000), ExMemory(100)),
      forceCost = ExBudget(ExCPU(16000), ExMemory(100)),
      applyCost = ExBudget(ExCPU(16000), ExMemory(100)),
      builtinCost = ExBudget(ExCPU(16000), ExMemory(100)),
      constrCost = ExBudget(ExCPU(16000), ExMemory(100)),
      caseCost = ExBudget(ExCPU(16000), ExMemory(100))
    )

    val defaultMachineCostsC: CekMachineCosts = defaultMachineCostsB
    val defaultMachineCosts: CekMachineCosts = defaultMachineCostsC

    def fromMap(map: Map[String, Long]): CekMachineCosts = {
        def get(key: String) = {
            val cpu = s"${key}-exBudgetCPU"
            val memory = s"${key}-exBudgetMemory"
            ExBudget.fromCpuAndMemory(
              cpu = map
                  .getOrElse(cpu, throw new IllegalArgumentException(s"Missing key: $cpu in $map")),
              memory = map.getOrElse(
                memory,
                throw new IllegalArgumentException(s"Missing key: $memory in $map")
              )
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
          builtinCost = get("cekBuiltinCost"),
          constrCost = Try(get("cekConstrCost")).getOrElse(defaultMachineCosts.constrCost),
          caseCost = Try(get("cekCaseCost")).getOrElse(defaultMachineCosts.caseCost)
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
    builtinCostModel: BuiltinCostModel,
    semanticVariant: BuiltinSemanticsVariant
)

object MachineParams {

    val defaultPlutusV1PostConwayParams: MachineParams =
        defaultParamsFor(PlutusLedgerLanguage.PlutusV1, ProtocolVersion.conwayPV)

    val defaultPlutusV2PostConwayParams: MachineParams =
        defaultParamsFor(PlutusLedgerLanguage.PlutusV2, ProtocolVersion.conwayPV)

    val defaultPlutusV3Params: MachineParams =
        defaultParamsFor(PlutusLedgerLanguage.PlutusV3, ProtocolVersion.conwayPV)

    /** The default machine parameters. Uses [[BuiltinSemanticsVariant.B]]
      * @note
      *   The default machine parameters use machine costs and builtin cost model that may be
      *   outdated, making budget calculation not precise. Please use
      *   `fromCardanoCliProtocolParamsJson` etc to create machine parameters with the latest costs.
      */
    @deprecated("Use defaultPlutusV2PostConwayParams or defaultParamsFor", "0.8.0")
    val defaultParams: MachineParams = defaultPlutusV2PostConwayParams

    /** Creates default machine parameters for a given Plutus version and protocol version.
      *
      * @param plutus
      *   The plutus version
      * @param protocolVersion
      *   The protocol version
      * @return
      *   The machine parameters
      */
    def defaultParamsFor(
        plutus: PlutusLedgerLanguage,
        protocolVersion: ProtocolVersion
    ): MachineParams = {
        val variant = BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(protocolVersion, plutus)
        variant match
            case BuiltinSemanticsVariant.A =>
                MachineParams(
                  machineCosts = CekMachineCosts.defaultMachineCostsA,
                  builtinCostModel = BuiltinCostModel.defaultCostModelA,
                  semanticVariant = variant
                )
            case BuiltinSemanticsVariant.B =>
                MachineParams(
                  machineCosts = CekMachineCosts.defaultMachineCostsB,
                  builtinCostModel = BuiltinCostModel.defaultCostModelB,
                  semanticVariant = variant
                )
            case BuiltinSemanticsVariant.C =>
                MachineParams(
                  machineCosts = CekMachineCosts.defaultMachineCostsC,
                  builtinCostModel = BuiltinCostModel.defaultCostModelC,
                  semanticVariant = variant
                )
    }

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
                writeJs(params).obj.map { (k, v) => (k, v.num.toLong) }.toMap
            case PlutusLedgerLanguage.PlutusV2 =>
                val costs = pparams.costModels("PlutusV2")
                val params = PlutusV2Params.fromSeq(costs)
                writeJs(params).obj.map { (k, v) => (k, v.num.toLong) }.toMap
            case PlutusLedgerLanguage.PlutusV3 =>
                val costs = pparams.costModels("PlutusV3")
                val params = PlutusV3Params.fromSeq(costs)
                writeJs(params).obj.map { (k, v) => (k, v.num.toLong) }.toMap

        val semvar =
            BuiltinSemanticsVariant.fromProtocolAndPlutusVersion(pparams.protocolVersion, plutus)
        val builtinCostModel = BuiltinCostModel.fromCostModelParams(plutus, semvar, paramsMap)
        val machineCosts = CekMachineCosts.fromMap(paramsMap)
        MachineParams(
          machineCosts = machineCosts,
          builtinCostModel = builtinCostModel,
          semanticVariant = semvar
        )
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
      s"Variable ${name.name}@${name.index} not found in environment: ${env.reverse.map(_._1).mkString(", ")}",
      env
    )

class BuiltinTermArgumentExpectedMachineError(term: Term, env: CekValEnv)
    extends StackTraceMachineError(s"Expected builtin term argument, got $term", env)

class UnexpectedBuiltinTermArgumentMachineError(term: Term, env: CekValEnv)
    extends StackTraceMachineError(s"Unexpected builtin term argument: $term", env)

class UnknownBuiltin(builtin: DefaultFun, env: CekValEnv)
    extends StackTraceMachineError(s"Unknown builtin: $builtin", env)

class EvaluationFailure(env: CekValEnv) extends StackTraceMachineError("Error evaluated", env)

class MissingCaseBranch(val tag: Long, env: CekValEnv)
    extends StackTraceMachineError(
      s"Case expression missing the branch required by the scrutinee tag: $tag",
      env
    )
class NonConstrScrutinized(val value: CekValue, env: CekValEnv)
    extends StackTraceMachineError(
      s"A non-constructor value was scrutinized in a case expression: $value",
      env
    )

class BuiltinException(msg: String) extends MachineError(msg)

class DeserializationError(fun: DefaultFun, value: CekValue)
    extends BuiltinException(s"Deserialization error in $fun: $value")

class KnownTypeUnliftingError(expected: DefaultUni, actual: CekValue)
    extends BuiltinException(s"Expected type $expected, got $actual")

class BuiltinError(
    val builtin: DefaultFun,
    val term: Term,
    val cause: Throwable,
    env: CekValEnv
) extends StackTraceMachineError(s"Builtin error: $builtin $term, caused by $cause", env)

class OutOfExBudgetError(budget: ExBudget, env: CekValEnv)
    extends StackTraceMachineError(s"Out of budget: $budget", env)

type CekValEnv = immutable.Seq[(String, CekValue)]

// 'Values' for the modified CEK machine.
enum CekValue {
    case VCon(const: Constant)
    case VDelay(term: Term, env: CekValEnv)
    case VLamAbs(name: String, term: Term, env: CekValEnv)
    case VBuiltin(bn: DefaultFun, term: () => Term, runtime: BuiltinRuntime)
    case VConstr(tag: Long, args: Seq[CekValue])

    override def toString: String = this match
        case VCon(const)            => s"VCon($const)"
        case VDelay(term, _)        => s"VDelay($term)"
        case VLamAbs(name, term, _) => s"VLamAbs($name, $term)"
        case VBuiltin(bn, _, _)     => s"VBuiltin($bn)"
        case VConstr(tag, args)     => s"VConstr($tag, ${args.mkString(", ")})"

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

/** Plutus VM facade.
  *
  * @param platformSpecific
  *   The platform specific implementation of certain functions used by VM builtins
  */
open class PlutusVM(platformSpecific: PlatformSpecific) {
    type ScriptForEvaluation = Array[Byte]

    /** Evaluates a script, returning the minimum budget that the script would need to evaluate.
      * This will take as long as the script takes.
      *
      * @param params
      *   The machine parameters
      * @param script
      *   Flat encoded script to evaluate
      * @param args
      *   The arguments to the script
      * @return
      *   [[CekResult]] with the resulting term and the execution budget
      * @throws MachineError
      */
    @varargs
    def evaluateScriptCounting(
        params: MachineParams,
        script: ScriptForEvaluation,
        args: Data*
    ): CekResult = {
        val program = ProgramFlatCodec.decodeFlat(script)
        val applied = args.foldLeft(program.term) { (acc, arg) =>
            Apply(acc, Const(asConstant(arg)))
        }
        val spender = CountingBudgetSpender()
        val logger = Log()
        val cek = new CekMachine(params, spender, logger, platformSpecific)
        val resultTerm = cek.evaluateTerm(applied)
        CekResult(resultTerm, spender.getSpentBudget, logger.getLogs)
    }

    /** Evaluates a script, returning the execution budget and logs.
      *
      * @param params
      *   The machine parameters
      * @param budget
      *   The budget to restrict the evaluation
      * @param script
      *   Flat encoded script to evaluate
      * @param args
      *   The arguments to the script
      * @return
      *   [[CekResult]] with the resulting term and the execution budget
      * @throws MachineError
      */
    @varargs
    def evaluateScriptRestricting(
        params: MachineParams,
        budget: ExBudget,
        script: ScriptForEvaluation,
        args: Data*
    ): CekResult = {
        val program = ProgramFlatCodec.decodeFlat(script)
        val applied = args.foldLeft(program.term) { (acc, arg) =>
            Apply(acc, Const(asConstant(arg)))
        }
        val spender = RestrictingBudgetSpender(budget)
        val logger = Log()
        val cek = new CekMachine(params, spender, logger, platformSpecific)
        val resultTerm = cek.evaluateTerm(applied)
        CekResult(resultTerm, spender.getSpentBudget, logger.getLogs)
    }

    /** Evaluates a UPLC term using default CEK machine parameters and no budget calculation.
      *
      * Useful for testing and debugging.
      *
      * @param term
      *   The debruijned term to evaluate
      * @return
      *   The resulting term
      * @throws StackTraceMachineError
      *   subtypes if the evaluation fails
      */
    def evaluateTerm(
        term: Term,
        params: MachineParams = MachineParams.defaultPlutusV2PostConwayParams
    ): Term = {
        val cek = new CekMachine(params, NoBudgetSpender, NoLogger, platformSpecific)
        val debruijnedTerm = DeBruijn.deBruijnTerm(term)
        cek.evaluateTerm(debruijnedTerm)
    }

    /** Evaluates a UPLC term, capturing the execution budget, evaluation logs and costs.
      *
      * @param term
      * @param params
      * @return
      *   [[Result]] with the resulting term, the execution budget, evaluation logs and costs, and
      *   an exception if the evaluation failed
      */
    def evaluateDebug(
        term: Term,
        params: MachineParams = MachineParams.defaultPlutusV2PostConwayParams
    ): Result = {
        val spenderLogger = TallyingBudgetSpenderLogger(CountingBudgetSpender())
        val cekMachine = CekMachine(params, spenderLogger, spenderLogger, platformSpecific)
        val debruijnedTerm = DeBruijn.deBruijnTerm(term)
        try
            Result.Success(
              DeBruijn.fromDeBruijnTerm(cekMachine.evaluateTerm(debruijnedTerm)),
              spenderLogger.getSpentBudget,
              spenderLogger.costs.toMap,
              spenderLogger.getLogsWithBudget
            )
        catch
            case e: Exception =>
                Result.Failure(
                  e,
                  spenderLogger.getSpentBudget,
                  spenderLogger.costs.toMap,
                  spenderLogger.getLogsWithBudget
                )
    }

    /** Evaluates a UPLC Program using default CEK machine parameters and no budget calculation.
      *
      * Useful for testing and debugging.
      */
    def evaluateProgram(
        p: Program,
        params: MachineParams = MachineParams.defaultPlutusV2PostConwayParams
    ): Term = evaluateTerm(p.term, params)
}

enum Result:
    val budget: ExBudget
    val logs: Seq[String]
    val costs: collection.Map[ExBudgetCategory, collection.Seq[ExBudget]]
    case Success(
        term: Term,
        budget: ExBudget,
        costs: Map[ExBudgetCategory, collection.Seq[ExBudget]],
        logs: Seq[String]
    )
    case Failure(
        exception: Exception,
        budget: ExBudget,
        costs: Map[ExBudgetCategory, collection.Seq[ExBudget]],
        logs: Seq[String]
    )

    override def toString(): String =
        import scalus.*

        def sumBudget(budgets: collection.Seq[ExBudget]): ExBudget =
            budgets.foldLeft(ExBudget.zero)(_ |+| _)

        def showCosts = costs.toArray
            .sortWith:
                case (
                      (ExBudgetCategory.BuiltinApp(bn1), _),
                      (ExBudgetCategory.BuiltinApp(bn2), _)
                    ) =>
                    bn1.ordinal < bn2.ordinal
                case ((k1, _), (k2, _)) => k1.ordinal < k2.ordinal
            .map:
                case (ExBudgetCategory.Startup, v) =>
                    s"Startup: ${v.length} ${sumBudget(v).showJson}"
                case (ExBudgetCategory.Step(kind), v) =>
                    s"$kind: ${v.length} ${sumBudget(v).showJson}"
                case (ExBudgetCategory.BuiltinApp(bn), v) =>
                    s"$bn: ${v.length} ${sumBudget(v).showJson}"
            .mkString("\n")

        this match
            case Success(term, budget, costs, logs) =>
                s"""Success executing script:
              | term: ${term.show}
              | budget: ${budget.showJson}
              | costs:\n${showCosts}
              | logs: ${logs.mkString("\n")}""".stripMargin
            case Failure(exception, budget, costs, logs) =>
                s"""Failure executing script:
              | exception: ${exception.getMessage()}
              | budget: ${budget.showJson}
              | costs:\n${showCosts}
              | logs: ${logs.mkString("\n")}""".stripMargin

class CekResult(t: Term, val budget: ExBudget, val logs: Array[String]) {
    lazy val term = DeBruijn.fromDeBruijnTerm(t)
    override def toString(): String = s"CekResult($term, $budget, ${logs.mkString(", ")})"
}

type ArgStack = Seq[CekValue]

private enum Context {
    case FrameAwaitArg(f: CekValue, ctx: Context)
    case FrameAwaitFunTerm(env: CekValEnv, term: Term, ctx: Context)
    case FrameAwaitFunValue(value: CekValue, ctx: Context)
    case FrameForce(ctx: Context)
    case FrameConstr(env: CekValEnv, tag: Long, rest: Seq[Term], args: ArgStack, ctx: Context)
    case FrameCases(env: CekValEnv, cases: Seq[Term], ctx: Context)
    case NoFrame
}

private enum CekState {
    case Return(ctx: Context, env: CekValEnv, value: CekValue)
    case Compute(ctx: Context, env: CekValEnv, term: Term)
    case Done(term: Term)
}

trait Logger {
    def log(msg: String): Unit
}

object NoLogger extends Logger {
    def log(msg: String): Unit = ()
}

class Log extends Logger {
    private val logs: ArrayBuffer[String] = ArrayBuffer.empty[String]
    def getLogs: Array[String] = logs.toArray
    def log(msg: String): Unit = logs.append(msg)
    def clear(): Unit = logs.clear()
}

trait BudgetSpender {
    def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit
    def getSpentBudget: ExBudget
}

object NoBudgetSpender extends BudgetSpender {
    def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = ()
    def getSpentBudget: ExBudget = ExBudget.zero
}

final class RestrictingBudgetSpender(val maxBudget: ExBudget) extends BudgetSpender {
    private var cpuLeft: Long = maxBudget.cpu
    private var memoryLeft: Long = maxBudget.memory

    def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
        cpuLeft -= budget.cpu
        memoryLeft -= budget.memory
        if cpuLeft < 0 || memoryLeft < 0 then throw new OutOfExBudgetError(maxBudget, env)
    }

    def getSpentBudget: ExBudget =
        ExBudget.fromCpuAndMemory(maxBudget.cpu - cpuLeft, maxBudget.memory - memoryLeft)

    def reset(): Unit = {
        cpuLeft = maxBudget.cpu
        memoryLeft = maxBudget.memory
    }
}

final class TallyingBudgetSpender(val budgetSpender: BudgetSpender) extends BudgetSpender {
    val costs: HashMap[ExBudgetCategory, ExBudget] = HashMap.empty

    def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
        budgetSpender.spendBudget(cat, budget, env)
        costs.updateWith(cat) {
            case Some(b) => Some(b |+| budget)
            case None    => Some(budget)
        }
    }

    def getSpentBudget: ExBudget = budgetSpender.getSpentBudget
}

final class TallyingBudgetSpenderLogger(val budgetSpender: BudgetSpender)
    extends BudgetSpender
    with Logger {
    val costs: collection.mutable.Map[ExBudgetCategory, Buffer[ExBudget]] =
        HashMap[ExBudgetCategory, Buffer[ExBudget]]().withDefault(_ => ArrayBuffer.empty[ExBudget])

    def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
        budgetSpender.spendBudget(cat, budget, env)
        costs.update(cat, costs(cat) += budget)
    }

    def getSpentBudget: ExBudget = budgetSpender.getSpentBudget

    private val _logs: ArrayBuffer[(String, ExBudget)] = ArrayBuffer.empty
    val logs: collection.IndexedSeq[(String, ExBudget)] = _logs
    def getLogs: Array[String] = _logs.map(_._1).toArray
    def getLogsWithBudget: Seq[String] =
        _logs.map((log, budget) => s"$log: ${budget.showJson}").toSeq
    def log(msg: String): Unit = _logs.append((msg, getSpentBudget))
}

final class CountingBudgetSpender extends BudgetSpender {
    private var cpu: Long = 0
    private var memory: Long = 0

    def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
        cpu += budget.cpu
        memory += budget.memory
    }

    def getSpentBudget: ExBudget = ExBudget.fromCpuAndMemory(cpu, memory)
}

/** CEK machine implementation based on Cardano Plutus CEK machine.
  *
  * The CEK machine is a stack-based abstract machine that is used to evaluate UPLC terms.
  *
  * @note
  *   The machine is stateless and can be reused for multiple evaluations. All the state is expected
  *   to be in the `budgetSpender` and `logger` implementations.
  *
  * @param params
  *   The machine parameters [[MachineParams]]
  * @param budgetSpender
  *   The budget spender implementation
  * @param logger
  *   The logger implementation
  * @param platformSpecific
  *   The platform specific implementation of certain functions used by builtins
  * @see
  *   https://github.com/input-output-hk/plutus/blob/41a7afebc4cee277bab702ee1678c070e5e38810/plutus-core/untyped-plutus-core/src/UntypedPlutusCore/Evaluation/Machine/Cek/Internal.hs
  * @example
  *   {{{
  *   val term = LamAbs("x", Apply(Var(NamedDeBruijn("x", 0)), Var(NamedDeBruijn("x", 0))))
  *   val cek = new CekMachine(MachineParams.defaultParams, NoBudgetSpender, NoLogger, JVMPlatformSpecific)
  *   val res = cek.runCek(term)
  *   }}}
  */
class CekMachine(
    val params: MachineParams,
    budgetSpender: BudgetSpender,
    logger: Logger,
    platformSpecific: PlatformSpecific
) extends BuiltinsMeaning(params.builtinCostModel, platformSpecific, params.semanticVariant) {
    import CekState.*
    import CekValue.*
    import Context.*

    private[uplc] val logs: ArrayBuffer[String] = ArrayBuffer.empty[String]

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
        spendBudget(ExBudgetCategory.Startup, params.machineCosts.startupCost, ArraySeq.empty)
        loop(Compute(NoFrame, ArraySeq.empty, term))
    }

    private final def computeCek(ctx: Context, env: CekValEnv, term: Term): CekState = {
        val costs = params.machineCosts
        term match
            case Var(name) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Var), costs.varCost, env)
                Return(ctx, env, lookupVarName(env, name))
            case LamAbs(name, term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.LamAbs), costs.lamCost, env)
                Return(ctx, env, VLamAbs(name, term, env))
            case Apply(fun, arg) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Apply), costs.applyCost, env)
                Compute(FrameAwaitFunTerm(env, arg, ctx), env, fun)
            case Force(term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Force), costs.forceCost, env)
                Compute(FrameForce(ctx), env, term)
            case Delay(term) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Delay), costs.delayCost, env)
                Return(ctx, env, VDelay(term, env))
            case Const(const) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Const), costs.constCost, env)
                Return(ctx, env, VCon(const))
            case Builtin(bn) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Builtin), costs.builtinCost, env)
                // The @term@ is a 'Builtin', so it's fully discharged.
                BuiltinMeanings.get(bn) match
                    case Some(meaning) => Return(ctx, env, VBuiltin(bn, () => term, meaning))
                    case None          => throw new UnknownBuiltin(bn, env)
            case Error => throw new EvaluationFailure(env)
            case Constr(tag, args) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Constr), costs.constrCost, env)
                args match
                    case arg :: rest =>
                        Compute(FrameConstr(env, tag, rest, ArraySeq.empty, ctx), env, arg)
                    case Nil => returnCek(ctx, env, VConstr(tag, Nil))
            case Case(scrut, cases) =>
                spendBudget(ExBudgetCategory.Step(StepKind.Case), costs.caseCost, env)
                Compute(FrameCases(env, cases, ctx), env, scrut)
    }

    private def returnCek(ctx: Context, env: CekValEnv, value: CekValue): CekState = {
        ctx match
            case NoFrame                          => Done(dischargeCekValue(value))
            case FrameForce(ctx)                  => forceEvaluate(ctx, env, value)
            case FrameAwaitFunTerm(env, arg, ctx) => Compute(FrameAwaitArg(value, ctx), env, arg)
            case FrameAwaitArg(fun, ctx)          => applyEvaluate(ctx, env, fun, value)
            case FrameAwaitFunValue(arg, ctx)     => applyEvaluate(ctx, env, value, arg)
            case FrameConstr(env, tag, todo, evaled, ctx) =>
                val newEvaled = evaled :+ value
                todo match
                    case arg :: rest =>
                        Compute(FrameConstr(env, tag, rest, newEvaled, ctx), env, arg)
                    case Nil => returnCek(ctx, env, VConstr(tag, newEvaled))
            case FrameCases(env, cases, ctx) =>
                value match
                    case VConstr(tag, args) =>
                        if 0 <= tag && tag < cases.size then
                            Compute(transferArgStack(args, ctx), env, cases(tag.toInt))
                        else throw new MissingCaseBranch(tag, env)
                    case _ => throw new EvaluationFailure(env)
    }

    private def transferArgStack(args: ArgStack, ctx: Context): Context = {
        args.foldRight(ctx) { (arg, c) => FrameAwaitFunValue(arg, c) }
    }

    private def lookupVarName(env: CekValEnv, name: NamedDeBruijn): CekValue = {
        if name.index == 0 then
            throw MachineError(
              s"Got a de Bruijn index 0: $name, it should be > 0. Run `DeBruijn.deBruijnTerm(term)` first."
            )
        if name.index <= 0 || env.size < name.index then
            throw new OpenTermEvaluatedMachineError(name, env)
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
                value match
                    case VLamAbs(name, term, env) =>
                        import scalus.pretty
                        println(s"non-plymorhic lambda beofre force, name=${name}")
                        println(term.pretty.render(100))
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
                spendBudget(ExBudgetCategory.BuiltinApp(builtinName), runtime.calculateCost, env)
                // eval the builtin and return result
                try
                    // eval builtin when it's fully saturated, i.e. when all arguments were applied
                    runtime.apply(logger)
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
            case VConstr(tag, args)       => Constr(tag, args.map(dischargeCekValue).toList)
    }

    private def spendBudget(cat: ExBudgetCategory, budget: ExBudget, env: CekValEnv): Unit = {
        budgetSpender.spendBudget(cat, budget, env)
    }
}
