package scalus.bugtracking

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.builtin.ByteString.hex
import scalus.builtin.Data
import scalus.builtin.Data.*
import scalus.builtin.given
import scalus.ledger.api.v3.*
import scalus.*
import scalus.prelude.*
import scalus.prelude.List.{Cons, Nil}
import scalus.uplc.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

class PaymentSplitter1Minimized1Test extends AnyFunSuite {

    val pkA = PubKeyHash(hex"62ac1f2d48217ff2caea06fd3ba0907940172b58889cd124f8806919")
    val pkB = PubKeyHash(hex"c80b8fc290c12618112ff4965000d875de4b9886a5667788eaa90f19")
    val pkC = PubKeyHash(hex"680c740ac9c115f8aaaac556ee1dbfe4dda151af4676488cc1ce9c79")
    val pkD = PubKeyHash(hex"1a1d436f77abfe78738b1b28700bafa75c98e2081ca990e65420623e")
    val pkE = PubKeyHash(hex"7a7084f5572681b0048bd9ffafc9884be58a3f4582dca33898c3bf7d")

    val lockTxId = TxId(hex"dbd4317339164ade328fbc6e854201d79a1baf92e9698173fd7bae44a2bd76f7")
    val a1FeeTx1 = TxId(hex"32ca42b5cd606173499f31e2547d7f664e917fbb824328c2dbe0d6775aefb08c")

    test("run payment validator ") {
        val sir = compile { PaymentSplitter1Minimized.validate }
        val uplc = sir.toUplc(generateErrorTraces = true)
        val script = uplc.plutusV3
        val txId = TxId(hex"85389ba34e63dd29e88ca54c2c52ef1b9f595258d0d26f5ceec96556f9917231")

        val input1 = TxInInfo(
          TxOutRef(a1FeeTx1, 1),
          TxOut(
            Address(
              credential = Credential.PubKeyCredential(pkA),
              stakingCredential = Option.None
            ),
            Value.lovelace(9809525639L)
          )
        )
        val input2 = TxInInfo(
          TxOutRef(lockTxId, 0),
          TxOut(
            Address(
              credential = Credential.ScriptCredential(
                hex"b2b6b428aca17b0db8165bb7aa691d7f7a5ece235dde8c1a7c5f1e07"
              ),
              stakingCredential = Option.None
            ),
            Value.lovelace(10000000L)
          ),
        )

        val output1 =
            TxOut(
              address = Address(
                credential = Credential.PubKeyCredential(pkA),
                stakingCredential = Option.None
              ),
              value = Value.lovelace(2000000),
            )
        val output2 =
            TxOut(
              address = Address(
                credential = Credential.PubKeyCredential(pkB),
                stakingCredential = Option.None
              ),
              value = Value.lovelace(2000000),
            )
        val output3 =
            TxOut(
              address = Address(
                credential = Credential.PubKeyCredential(pkC),
                stakingCredential = Option.None
              ),
              value = Value.lovelace(2000000),
            )
        val output4 = TxOut(
          address = Address(
            credential = Credential.PubKeyCredential(pkD),
            stakingCredential = Option.None
          ),
          value = Value.lovelace(2000000),
        )
        val output5 = TxOut(
          address = Address(
            credential = Credential.PubKeyCredential(pkE),
            stakingCredential = Option.None
          ),
          value = Value.lovelace(2000000),
        )
        val output6 = TxOut(
          address = Address(
            credential = Credential.PubKeyCredential(pkA),
            stakingCredential = Option.None
          ),
          value = Value.lovelace(9808531730L),
        )

        val scriptContext = ScriptContext(
          TxInfo(
            scalus.prelude.List(input1, input2),
            Nil,
            scalus.prelude.List(output1, output2, output3, output4, output5, output6),
            BigInt(0),
            Value.zero,
            Nil,
            AssocMap.empty,
            Interval.always,
            Cons(PubKeyHash(hex"deadbeef"), Nil),
            redeemers = AssocMap.empty,
            data = AssocMap.empty,
            id = txId
          ),
          ScriptPurpose.Spending(TxOutRef(lockTxId, 0)),
          ScriptInfo.SpendingScript(TxOutRef(lockTxId, 0), Option.None)
        )
        val data = scalus.prelude.List(pkA, pkB, pkC, pkD, pkE).toData
        val applied = script $ data $ scriptContext.toData

        PaymentSplitter1Minimized.validate(data)(scriptContext)

        given PlutusVM = PlutusVM.makePlutusV3VM()
        val result = applied.evaluateDebug
        println(result)
        println(s"logs=${result.logs}")
        assert(result.isSuccess)
    }

    /*
    {
        "data":
            {"contents": {"contents": {"contents": {"era": "ShelleyBasedEraConway",
                "error": ["ConwayUtxowFailure (UtxoFailure (UtxosFailure (ValidationTagMismatch (IsValid True) (FailedUnexpectedly " +
                \\nThe plutus evaluation error is: CekError An error has occurred:\\nAttempted to apply a non-function.\\nCaused by: delay (\\\\i i -> force i)"" +
                "The protocol version is: Version 10\\n" +
                ScriptInfo:
                   SpendingScript(
                     TxOutRef {txOutRefId = dbd4317339164ade328fbc6e854201d79a1baf92e9698173fd7bae44a2bd76f7, txOutRefIdx = 0})
                     (Just (Datum {getDatum = Constr 0 ADAR}))
                     TxInfo:
                        TxId: 85389ba34e63dd29e88ca54c2c52ef1b9f595258d0d26f5ceec96556f9917231
                        Inputs: [ 32ca42b5cd606173499f31e2547d7f664e917fbb824328c2dbe0d6775aefb08c!1 ->  9809525639 to PubKeyCredential: 7a7084f5572681b0048bd9ffafc9884be58a3f4582dca33898c3bf7d (StakingHash PubKeyCredential: 656dc0dcd841835e923cb677488b534e587293bda4547e6b6730f26d)\\n
                                  dbd4317339164ade328fbc6e854201d79a1baf92e9698173fd7bae44a2bd76f7!0 -> 10000000 to ScriptCredential: b2b6b428aca17b0db8165bb7aa691d7f7a5ece235dde8c1a7c5f1e07
                                  datum hash:  2266b1e8256d33d19f7d34d63068020540ee948bbe768e71db59b6c11b32d81a
                     ]
                  Outputs: [ 2000000 addressed to PubKeyCredential: 62ac1f2d48217ff2caea06fd3ba0907940172b58889cd124f8806919 (3bd1236c9d506e78521a1f9630696f2481a9d7ad8d9844fc78584bf3)
                    2000000 addressed to PubKeyCredential: c80b8fc290c12618112ff4965000d875de4b9886a5667788eaa90f19 (a10fd4dbe0bfe4b085b272b5fda2147a57a72d64e4e6b66ec8f5a937)
                    2000000 addressed to PubKeyCredential: 7a7084f5572681b0048bd9ffafc9884be58a3f4582dca33898c3bf7d (StakingHash PubKeyCredential: 656dc0dcd841835e923cb677488b534e587293bda4547e6b6730f26d)
                    2000000 addressed to PubKeyCredential: 680c740ac9c115f8aaaac556ee1dbfe4dda151af4676488cc1ce9c79 (StakingHash PubKeyCredential: e5d4fce20b1bcea577ac720387f17f9df44f76fe5565ed68bec3e0d8)
                    2000000 addressed to PubKeyCredential: 1a1d436f77abfe78738b1b28700bafa75c98e2081ca990e65420623e (StakingHash PubKeyCredential: 7878e754c1467b16bbe53b6c8fda5038780df3d3490abf0d1d8a74cd)
                    9808531730 addressed to PubKeyCredential: 7a7084f5572681b0048bd9ffafc9884be58a3f4582dca33898c3bf7d (StakingHash PubKeyCredential: 656dc0dcd841835e923cb677488b534e587293bda4547e6b6730f26d)
                  ]

    }
    
     */

}
