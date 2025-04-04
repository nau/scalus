package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data.toData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.{Address, Interval, PubKeyHash, Value}
import scalus.ledger.api.v2.OutputDatum.NoOutputDatum
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.{ScriptContext, ScriptInfo, TxId, TxInInfo, TxInfo, TxOutRef}
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.prelude.{Option, *}
import scalus.uplc.*
import scalus.uplc.eval.*

class PaymentSplitterSpec extends AnyFunSuite with ScalusTest {
    val lockTxId = TxId(hex"2e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
    val txId = TxId(hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9")
    val scriptHash = hex"1e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a"

    def makeScriptContext(scriptHash: ByteString): ScriptContext =
        ScriptContext(
          txInfo = TxInfo(
            inputs = List(
              TxInInfo(
                outRef = TxOutRef(lockTxId, 1),
                resolved = TxOut(
                  address = Address(
                    PubKeyCredential(
                      PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
                    ),
                    Option.None
                  ),
                  value = Value.lovelace(100),
                  datum = NoOutputDatum,
                  referenceScript = Option.None
                )
              ),
              TxInInfo(
                outRef = TxOutRef(lockTxId, 0),
                resolved = TxOut(
                  address = Address(ScriptCredential(scriptHash), Option.None),
                  value = Value.lovelace(100),
                  datum = NoOutputDatum,
                  referenceScript = Option.None
                )
              )
            ),
            referenceInputs = List.Nil,
            outputs = List(
              TxOut(
                address = Address(
                  PubKeyCredential(
                    PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
                  ),
                  Option.None
                ),
                value = Value.lovelace(200),
                datum = NoOutputDatum,
                referenceScript = Option.None
              )
            ),
            fee = BigInt(10),
            mint = Value.zero,
            certificates = List.Nil,
            withdrawals = AssocMap.empty,
            validRange = Interval.always,
            signatories = List.Nil,
            redeemers = AssocMap.empty,
            data = AssocMap.empty,
            id = txId,
            votes = AssocMap.empty,
            proposalProcedures = List.Nil,
            currentTreasuryAmount = Option.None,
            treasuryDonation = Option.None
          ),
          redeemer = Data.unit,
          scriptInfo =
              ScriptInfo.SpendingScript(txOutRef = TxOutRef(lockTxId, 0), datum = Option.None)
        )

    test("success when payments are correctly split") {
        val context = makeScriptContext(scriptHash).toData
        val payees = List(
          List(
            hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678"
          )
        ).toData
//      uncomment for debugging
        PaymentSplitter.validator(payees)(context)

        val program = compile(PaymentSplitter.validator).toUplc().plutusV3 $ payees $ context

        println(program.flatEncoded.length)

        val result = program.evaluateDebug

        assert(result.isSuccess, clue = result.toString)
        assert(result.budget == ExBudget(ExCPU(4144100), ExMemory(26000)))
    }
}
