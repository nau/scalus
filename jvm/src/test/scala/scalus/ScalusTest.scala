package scalus

import scalus.*
import scalus.sir.SIR
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

trait ScalusTest {
    protected given PlutusVM = PlutusVM.makePlutusV3VM()

    extension (sir: SIR)
        inline def runScript(scriptContext: ScriptContext): Result =
            // UPLC program: (ScriptContext as Data) -> ()
            val script = sir.toUplc(generateErrorTraces = true).plutusV3
            val appliedScript = script $ scriptContext.toData
            appliedScript.evaluateDebug

    protected def makeSpendingScriptContext(
        datum: Data,
        redeemer: Redeemer,
        signatories: List[PubKeyHash]
    ): ScriptContext = {
        ScriptContext(
          txInfo = TxInfo(
            inputs = List.Nil,
            referenceInputs = List.Nil,
            outputs = List.Nil,
            fee = BigInt(188021),
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
            currentTreasuryAmount = Option.None,
            treasuryDonation = Option.None
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = TxOutRef(TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9"), 0),
            datum = Option.Some(datum)
          )
        )
    }
}
