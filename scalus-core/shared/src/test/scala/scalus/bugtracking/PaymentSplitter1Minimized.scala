package scalus.bugtracking

import scalus.prelude.DataParameterizedValidator

import scalus.*
import scalus.builtin.{Builtins, ByteString, Data}
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.prelude.AssocMap.*
import scalus.prelude.{*, given}

@scalus.Compile
object PaymentSplitter1Minimized extends DataParameterizedValidator {

    override def spend(
        payeesData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {

        // val sumsPerPayee = AssocMap.empty[Credential.PubKeyCredential, BigInt]
        val sumsPerPayee = AssocMap.empty[BigInt, BigInt]
        AssocMap.get(sumsPerPayee)(BigInt(1))

    }

}
