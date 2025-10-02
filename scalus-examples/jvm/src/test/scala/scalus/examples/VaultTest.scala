package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.testkit.ScalusTest
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.Builtins.{appendByteString, blake2b_224}
import scalus.ledger.api.v1.{Address, PubKeyHash}
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.examples.Vault.{Datum, Redeemer, State}
import scalus.prelude.Option
import scalus.uplc.eval.Result

object VaultMock {
    val rootKeyHash: ByteString =
        ByteString.fromHex("a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2")

    private def mockHash(variation: BigInt, root: ByteString): ByteString =
        val variationBytes = ByteString.fromArray(variation.toByteArray)
        blake2b_224(appendByteString(variationBytes, root))

    private def mockKeyHash(variation: BigInt): ByteString =
        mockHash(variation, rootKeyHash)

    def mockPubKeyHash(variation: BigInt): PubKeyHash =
        PubKeyHash(mockKeyHash(variation))

    def mockScriptHash(variation: BigInt): ValidatorHash =
        mockKeyHash(variation + 200)
}

class VaultTest extends AnyFunSuite, ScalusTest {

    private val ownerPKH: PubKeyHash = VaultMock.mockPubKeyHash(0)
    private val senderPKH: PubKeyHash = VaultMock.mockPubKeyHash(1)
    private val contractHash: ValidatorHash = VaultMock.mockScriptHash(0)

    private val defaultInitialAmount: Lovelace = BigInt(10_000_000L)
    private val defaultFee: Lovelace = BigInt(1_000_000L)

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false
    )

    case class TestCase(
        datum: Datum,
        redeemer: Redeemer,
        outputDatum: Option[Datum],
        vaultInputAmount: Lovelace,
        vaultOutputAmount: Option[Lovelace],
        outputToSender: Boolean = false,
        fee: Lovelace = defaultFee
    )

    def checkTestCase(testCase: TestCase): Result = {
        val inputs = scalus.prelude.List(
          makeScriptHashInput(contractHash, testCase.vaultInputAmount)
        )

        val outputs = testCase.outputDatum match {
            case Option.Some(outDatum) =>
                scalus.prelude.List(
                  TxOut(
                    address = Address(ScriptCredential(contractHash), Option.None),
                    value = Value.lovelace(
                      testCase.vaultOutputAmount.getOrElse(testCase.vaultInputAmount)
                    ),
                    datum = OutputDatum.OutputDatum(outDatum.toData)
                  )
                )
            case Option.None if testCase.outputToSender =>
                scalus.prelude.List(
                  TxOut(
                    address = Address(PubKeyCredential(senderPKH), Option.None),
                    value = Value.lovelace(
                      testCase.vaultOutputAmount.getOrElse(testCase.vaultInputAmount)
                    ),
                    datum = OutputDatum.NoOutputDatum
                  )
                )
            case _ => scalus.prelude.List()
        }

        val txInfo = TxInfo(
          inputs = inputs,
          id = random[TxId],
          signatories = scalus.prelude.List(),
          outputs = outputs,
          validRange = Interval.always,
          fee = testCase.fee
        )

        val scriptContext = ScriptContext(
          txInfo = txInfo,
          redeemer = toData(testCase.redeemer),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.head.outRef,
            datum = Option.Some(testCase.datum.toData)
          )
        )

        VaultContract.compiled.runScript(scriptContext)
    }

    test("Successful deposit to idle vault") {
        val datum = Datum(
          owner = ownerPKH.hash,
          sender = senderPKH.hash,
          state = State.Idle,
          amount = defaultInitialAmount
        )

        val depositAmount = BigInt(5_000_000L)
        val newAmount = datum.amount + depositAmount
        val newDatum = datum.copy(amount = newAmount)

        val result = checkTestCase(
          TestCase(
            datum = datum,
            redeemer = Redeemer.Deposit,
            outputDatum = Option.Some(newDatum),
            vaultInputAmount = datum.amount,
            vaultOutputAmount = Option.Some(newAmount)
          )
        )

        assert(result.isSuccess, "Deposit to idle vault should succeed")
    }

    test("Successful withdrawal request from idle vault") {
        val datum = Datum(
          owner = ownerPKH.hash,
          sender = senderPKH.hash,
          state = State.Idle,
          amount = defaultInitialAmount
        )

        val newDatum = datum.copy(state = State.Pending)

        val result = checkTestCase(
          TestCase(
            datum = datum,
            redeemer = Redeemer.Withdraw,
            outputDatum = Option.Some(newDatum),
            vaultInputAmount = datum.amount,
            vaultOutputAmount = Option.None
          )
        )

        assert(result.isSuccess, "Withdrawal request should succeed")
    }

    test("Successful finalize withdrawal from pending vault") {
        val datum = Datum(
          owner = ownerPKH.hash,
          sender = senderPKH.hash,
          state = State.Pending,
          amount = defaultInitialAmount
        )

        val result = checkTestCase(
          TestCase(
            datum = datum,
            redeemer = Redeemer.Finalize,
            outputDatum = Option.None,
            vaultInputAmount = datum.amount,
            vaultOutputAmount = Option.Some(datum.amount),
            outputToSender = true
          )
        )

        val dbg = true
        assert(result.isSuccess, "Finalize withdrawal should succeed")
    }

    test("Successful cancel withdrawal from pending vault") {
        val datum = Datum(
          owner = ownerPKH.hash,
          sender = senderPKH.hash,
          state = State.Pending,
          amount = defaultInitialAmount
        )

        val newDatum = datum.copy(state = State.Idle)

        val result = checkTestCase(
          TestCase(
            datum = datum,
            redeemer = Redeemer.Cancel,
            outputDatum = Option.Some(newDatum),
            vaultInputAmount = datum.amount,
            vaultOutputAmount = Option.None
          )
        )

        assert(result.isSuccess, "Cancel withdrawal should succeed")
    }

    test("Successful deposit with large amount") {
        val datum = Datum(
          owner = ownerPKH.hash,
          sender = senderPKH.hash,
          state = State.Idle,
          amount = defaultInitialAmount
        )

        val depositAmount = BigInt(100_000_000L)
        val newAmount = datum.amount + depositAmount
        val newDatum = datum.copy(amount = newAmount)

        val result = checkTestCase(
          TestCase(
            datum = datum,
            redeemer = Redeemer.Deposit,
            outputDatum = Option.Some(newDatum),
            vaultInputAmount = datum.amount,
            vaultOutputAmount = Option.Some(newAmount)
          )
        )

        assert(result.isSuccess, "Deposit with large amount should succeed")
    }

    test("Successful deposit with minimal amount") {
        val datum = Datum(
          owner = ownerPKH.hash,
          sender = senderPKH.hash,
          state = State.Idle,
          amount = BigInt(1_000_000L)
        )

        val depositAmount = BigInt(1L)
        val newAmount = datum.amount + depositAmount
        val newDatum = datum.copy(amount = newAmount)

        val result = checkTestCase(
          TestCase(
            datum = datum,
            redeemer = Redeemer.Deposit,
            outputDatum = Option.Some(newDatum),
            vaultInputAmount = datum.amount,
            vaultOutputAmount = Option.Some(newAmount)
          )
        )

        assert(result.isSuccess, "Deposit with minimal amount should succeed")
    }
}
