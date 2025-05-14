package scalus.testkit

import org.scalacheck.{Arbitrary, Gen}
import scalus.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, given}
import scalus.builtin.Builtins.blake2b_224
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.sir.SIR
import scalus.uplc.*
import scalus.uplc.eval.*

trait ScalusTest {
    protected given PlutusVM = PlutusVM.makePlutusV3VM()

    extension (self: SIR)
        def runScript(scriptContext: ScriptContext): Result =
            // UPLC program: (ScriptContext as Data) -> ()
            val script = self.toUplc(generateErrorTraces = true).plutusV3
            val appliedScript = script $ scriptContext.toData
            appliedScript.evaluateDebug

        def scriptV3(errorTraces: Boolean = true): Program =
            self.toUplc(generateErrorTraces = errorTraces).plutusV3

    extension (self: Program)
        def runWithDebug(scriptContext: ScriptContext): Result =
            val appliedScript = self $ scriptContext.toData
            appliedScript.evaluateDebug

        def hash: ValidatorHash = blake2b_224(ByteString.fromArray(3 +: self.cborEncoded))

    protected def genByteStringOfN(n: Int): Gen[ByteString] = {
        Gen
            .containerOfN[Array, Byte](n, Arbitrary.arbitrary[Byte])
            .map(a => ByteString.unsafeFromArray(a))
    }

    given Arbitrary[TxId] = Arbitrary(genByteStringOfN(32).map(TxId.apply))
    given Arbitrary[TxOutRef] = Arbitrary {
        for
            txId <- Arbitrary.arbitrary[TxId]
            index <- Gen.choose(0, 1000)
        yield TxOutRef(txId, index)
    }

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
              outRef = Arbitrary.arbitrary[TxOutRef].sample.get,
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

    protected def makePubKeyHashOutput(pkh: Hash, value: BigInt): TxOut = {
        TxOut(
          address = Address(PubKeyCredential(PubKeyHash(pkh)), Option.None),
          value = Value.lovelace(value)
        )
    }

    protected def makeScriptHashOutput(scriptHash: ValidatorHash, value: BigInt): TxOut = {
        TxOut(
          address = Address(ScriptCredential(scriptHash), Option.None),
          value = Value.lovelace(value)
        )
    }

    final protected def failure(message: String): Either[String, Option[ExBudget]] = Left(message)
    final protected def success: Either[String, Option[ExBudget]] = Right(Option.None)
    final protected def success(budget: ExBudget): Either[String, Option[ExBudget]] = Right(
      Option.Some(budget)
    )

    protected def checkResult(
        expected: Either[String, Option[ExBudget]],
        actual: Result
    ): Unit = {
        expected match
            case Left(errorMsg) =>
                assert(
                  actual.isFailure,
                  s"Expected failure with: $errorMsg, but got success"
                )
                // If a specific error message is provided, check it matches
                assert(
                  actual.logs.exists(_.contains(errorMsg)),
                  s"Expected error containing: $errorMsg, but got: ${actual.logs.mkString(", ")}"
                )
            case Right(success) =>
                success match
                    case Option.None =>
                        assert(
                          actual.isSuccess,
                          s"Expected success, but got: ${actual.toString}, logs0: ${actual.logs.mkString(", ")}"
                        )
                    case Option.Some(budget) =>
                        assert(
                          actual.isSuccess,
                          s"Expected success with budget: $budget, but got: ${actual.toString}, logs0: ${actual.logs
                                  .mkString(", ")}"
                        )
                        if budget != ExBudget(ExCPU(0), ExMemory(0))
                        then // Check if budget verification is requested
                            assert(
                              actual.budget == budget,
                              s"Expected budget: $budget, but got: ${actual.budget}"
                            )
    }

    def compareResultWithReferenceValue(
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

        if isPrintComparison || BenchmarkConfig.isPrintAllComparisonsOfResultWithReferenceValue then
            println(
              s"${BenchmarkConfig.prefixOfLog}$testName: {" +
                  s"cpu: ${scalusBudget.cpu.comparisonAsJsonString(refBudget.cpu)}, " +
                  s"memory: ${scalusBudget.memory.comparisonAsJsonString(refBudget.memory)}" +
                  "}"
            )
    }
}

object ScalusTest {
    private object BenchmarkConfig {
        inline val prefixOfLog = ""
        val isPrintAllComparisonsOfResultWithReferenceValue: Boolean = false
    }
}
