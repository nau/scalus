package scalus

import org.scalacheck.{Arbitrary, Gen}
import scalus.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data, given}
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.prelude.*
import scalus.sir.SIR
import scalus.uplc.*
import scalus.uplc.eval.*

trait ScalusTest {
    protected given PlutusVM = PlutusVM.makePlutusV3VM()

    extension (sir: SIR)
        inline def runScript(scriptContext: ScriptContext): Result =
            // UPLC program: (ScriptContext as Data) -> ()
            val script = sir.toUplc(generateErrorTraces = true).plutusV3
            val appliedScript = script $ scriptContext.toData
            appliedScript.evaluateDebug

    def genByteStringOfN(n: Int): Gen[ByteString] = {
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
            id = Arbitrary.arbitrary[TxId].sample.get
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = ownInput.outRef,
            datum = Option.Some(datum)
          )
        )
    }
}
