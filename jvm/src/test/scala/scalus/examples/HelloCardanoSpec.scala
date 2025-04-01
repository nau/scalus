package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.ByteString.given
import scalus.builtin.Data.toData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.{ByteString, Data, given}
import scalus.ledger.api.v1.{Interval, PubKeyHash}
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.ledger.api.v3.{*, given}
import scalus.prelude.*
import scalus.uplc.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.*

import scala.language.implicitConversions

class HelloCardanoSpec extends AnyFunSuite {
    enum Expected {
        case Success(budget: ExBudget)
        case Failure(reason: String)
    }
    import Expected.*

    // Plutus V3 VM with default machine parameters
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("Hello Cardano") {
        val ownerPubKey =
            PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef")
        val message = "Hello, Cardano!".toData
        val context =
            makeScriptContext(
              datum = ownerPubKey.toData,
              redeemer = message,
              signatories = List(ownerPubKey)
            )
        val appliedScript = HelloCardanoValidator.script $ context.toData
        assertEval(
          appliedScript,
          Success(ExBudget.fromCpuAndMemory(cpu = 49427700, memory = 185591))
        )
    }

    private def makeScriptContext(datum: Data, redeemer: Redeemer, signatories: List[PubKeyHash]) =
        ScriptContext(
          txInfo = TxInfo(
            inputs = List.Nil,
            referenceInputs = List.Nil,
            outputs = List.Nil,
            fee = BigInt("188021"),
            mint = Value.zero,
            certificates = List.Nil,
            withdrawals = AssocMap.empty,
            validRange = Interval.always,
            signatories = signatories,
            redeemers = AssocMap.empty,
            data = AssocMap.empty,
            id = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9"),
            votes = AssocMap.empty,
            proposalProcedures = List.Nil,
            currentTreasuryAmount = Maybe.Nothing,
            treasuryDonation = Maybe.Nothing
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = TxOutRef(TxId(hex"deadbeef"), 0),
            datum = Maybe.Just(datum)
          )
        )

    private def assertEval(p: Program, expected: Expected): Unit = {
        val result = p.evaluateDebug
        (result, expected) match
            case (result: Result.Success, Expected.Success(expected)) =>
                assert(result.budget == expected)
            case (result: Result.Failure, Expected.Failure(expected)) =>
                assert(result.exception.getMessage == expected)
            case _ => fail(s"Unexpected result: $result, expected: $expected")
    }
}
