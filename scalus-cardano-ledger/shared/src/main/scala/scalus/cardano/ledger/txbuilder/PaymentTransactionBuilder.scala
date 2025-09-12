package scalus.cardano.ledger.txbuilder

import com.bloxbean.cardano.client.api.ProtocolParamsSupplier
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import scalus.cardano.ledger.{Coin, Hash, KeepRaw, Redeemers, Sized, TaggedOrderedSet, TaggedSet, Transaction, TransactionBody, TransactionInput, TransactionOutput, TransactionWitnessSet, Value}
import com.bloxbean.cardano.client.transaction.spec.Transaction as CclTransaction
import com.bloxbean.cardano.client.transaction.spec.TransactionBody as CclTransactionBody
import com.bloxbean.cardano.client.transaction.spec.TransactionOutput as CclTransactionOutput
import com.bloxbean.cardano.client.transaction.spec.TransactionInput as CclTransactionInput
import com.bloxbean.cardano.client.address.Address as CclAddress
import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.txbuilder.Converters.toScalus

import scala.jdk.CollectionConverters.*
import java.math.BigInteger
import scala.collection.immutable.SortedSet

case class PaymentTransactionBuilder(
    context: BuilderContext,
    var sender: Address,
    var paymentAddress: Address,
    var payment: Value,
) {

    def buildAndSign(signer: TxSigner) = {
        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .from(sender.encode.get)
                  .payToAddress(
                    paymentAddress.encode.get,
                    Amount.lovelace(BigInteger.valueOf(payment.coin.value))
                  )
            )
            .withSigner(t => t)
            .build()

        // Convert CCL transaction to Scalus transaction by copying fields
        val scalusTx = cclTx.toScalus
        
        signer.signTx(scalusTx)
    }
}



object Converters {

    extension (cclTx: CclTransaction) {
        def toScalus: Transaction = {
            val body = cclTx.getBody.toScalus
            val witnessSet = TransactionWitnessSet.empty

            Transaction(
                body = KeepRaw(body),
                witnessSet = witnessSet,
                isValid = true,
                auxiliaryData = None
            )
        }
    }

    extension (cclBody: CclTransactionBody) {
        def toScalus: TransactionBody = {
            val inputs = cclBody.getInputs.asScala.map(_.toScalus)
            val outputs = cclBody.getOutputs.asScala.map(output => Sized(output.toScalus)).toVector
            val fee = Coin(cclBody.getFee.longValue())

            TransactionBody(
                inputs = TaggedOrderedSet.from(inputs),
                outputs = outputs,
                fee = fee,
                ttl = Option(cclBody.getTtl).map(_.longValue()),
                networkId = Option(cclBody.getNetworkId).map(network => network.ordinal())
            )
        }
    }

    extension (cclInput: CclTransactionInput) {
        def toScalus: TransactionInput = {
            TransactionInput(
                Hash(ByteString.fromHex(cclInput.getTransactionId)),
                cclInput.getIndex
            )
        }
    }

    extension (cclOutput: CclTransactionOutput) {
        def toScalus: TransactionOutput = {
            val address = Address.fromBech32(cclOutput.getAddress)
            val value = Value.lovelace(cclOutput.getValue.getCoin.longValue())

            TransactionOutput(
                address = address,
                value = value,
                datumOption = None
            )
        }
    }
}
