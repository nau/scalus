package scalus.testkit

import org.scalacheck.Arbitrary
import scalus.*
import scalus.builtin.Builtins.blake2b_224
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.sir.SIR
import scalus.uplc.*
import scalus.uplc.eval.*

trait ScalusTest extends ArbitraryInstances {
    protected given PlutusVM = PlutusVM.makePlutusV3VM()

    extension (self: SIR)
        def runScript(using
            scalusOptions: scalus.Compiler.Options = scalus.Compiler.defaultOptions
        )(
            scriptContext: ScriptContext,
            param: Option[Data] = None
        ): Result =
            // UPLC program: (ScriptContext as Data) -> ()
            val script = self.toUplc().plutusV3
            // println(s"uplc: ${script.pretty.render(100)}")
            val appliedScript = param.map(script $ _).getOrElse(script) $ scriptContext.toData
            appliedScript.evaluateDebug

        def scriptV3(using
            scalusOptions: scalus.Compiler.Options = scalus.Compiler.defaultOptions
        )(errorTraces: Boolean = true): Program =
            self.toUplc(generateErrorTraces = errorTraces).plutusV3

    extension (self: Program)
        def runWithDebug(scriptContext: ScriptContext): Result =
            val appliedScript = self $ scriptContext.toData
            appliedScript.evaluateDebug

        def hash: ValidatorHash = blake2b_224(ByteString.fromArray(3 +: self.cborEncoded))

    protected def random[A: Arbitrary]: A = {
        Arbitrary.arbitrary[A].sample.get
    }

    protected def makeSpendingScriptContext(
        datum: Data,
        redeemer: Redeemer,
        signatories: List[PubKeyHash]
    ): ScriptContext = {
        val ownInput =
            TxInInfo(
              outRef = random[TxOutRef],
              resolved = TxOut(
                address = Address(
                  Credential.ScriptCredential(genByteStringOfN(28).sample.get),
                  Option.None
                ),
                value = Value.zero
              )
            )
        ScriptContext(
          txInfo = TxInfo(
            inputs = List(ownInput),
            fee = 188021,
            signatories = signatories,
            id = random[TxId]
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = ownInput.outRef,
            datum = Option.Some(datum)
          )
        )
    }

    protected def makePubKeyHashInput(pkh: Hash, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(random[TxId], 0),
          resolved = TxOut(
            address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    protected def makeScriptHashInput(scriptHash: ValidatorHash, value: BigInt): TxInInfo = {
        TxInInfo(
          outRef = TxOutRef(random[TxId], 0),
          resolved = TxOut(
            address = Address(ScriptCredential(scriptHash), Option.None),
            value = Value.lovelace(value)
          )
        )
    }

    protected def makePubKeyHashOutput(
        pkh: Hash,
        value: BigInt,
        datum: OutputDatum = OutputDatum.NoOutputDatum
    ): TxOut = {
        TxOut(
          address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
          value = Value.lovelace(value),
          datum = datum
        )
    }

    protected def makeScriptHashOutput(
        scriptHash: ValidatorHash,
        value: BigInt,
        datum: OutputDatum = OutputDatum.NoOutputDatum
    ): TxOut = {
        TxOut(
          address = Address(ScriptCredential(scriptHash), Option.None),
          value = Value.lovelace(value),
          datum = datum
        )
    }

    final protected def failure(message: String): (String, Option[ExBudget]) =
        (message, Option.None)
    final protected def failure(message: String, budget: ExBudget): (String, Option[ExBudget]) =
        (message, Option.Some(budget))
    protected val success: (Unit, Option[ExBudget]) = ((), Option.None)
    final protected def success(budget: ExBudget): (Unit, Option[ExBudget]) =
        ((), Option.Some(budget))

    protected def checkResult(
        expected: (String | Unit, Option[ExBudget]),
        actual: Result
    ): Unit = {
        expected._1 match
            case errorMsg: String =>
                assert(
                  actual.isFailure,
                  s"Expected failure with: $errorMsg, but got success"
                )
                // If a specific error message is provided, check it matches
                assert(
                  actual.logs.exists(_.contains(errorMsg)),
                  s"Expected error containing: $errorMsg, but got: ${actual.logs.mkString(", ")}"
                )
            case () =>
                actual match
                    case Result.Failure(ex, budget, cost, logs) =>
                        ex match
                            case be: scalus.uplc.eval.BuiltinError =>
                                be.cause.printStackTrace()
                            case _ =>
                    case _ =>
                assert(
                  actual.isSuccess,
                  s"Expected success, but got: ${actual.toString}, logs0: ${actual.logs.mkString(", ")}"
                )

        expected._2 match
            case Option.Some(budget) if budget != ExBudget(ExCPU(0), ExMemory(0)) =>
                assert(
                  actual.budget == budget,
                  s"Expected budget: $budget, but got: ${actual.budget}"
                )
            case _ =>
    }

    def compareBudgetWithReferenceValue(
        testName: String,
        scalusBudget: ExBudget,
        refBudget: ExBudget,
        isPrintComparison: Boolean = false
    ): Unit = {
        import ScalusTest.BenchmarkConfig
        extension (scalus: Long)
            def comparisonAsJsonString(ref: Long): String = {
                val comparison = f"${scalus.toDouble / ref.toDouble * 100}%.2f"
                s"{scalus: $scalus, ref: $ref, comparison: $comparison%}"
            }

        end extension

        if isPrintComparison || BenchmarkConfig.isPrintAllComparisonsOfBudgetWithReferenceValue then
            println(
              s"${BenchmarkConfig.logPrefix}[$testName]: {" +
                  s"cpu: ${scalusBudget.cpu.comparisonAsJsonString(refBudget.cpu)}, " +
                  s"memory: ${scalusBudget.memory.comparisonAsJsonString(refBudget.memory)}" +
                  "}"
            )
    }
}

object ScalusTest {
    private object BenchmarkConfig {
        inline val logPrefix = "BenchmarkComparison"
        val isPrintAllComparisonsOfBudgetWithReferenceValue: Boolean = false
    }
}
