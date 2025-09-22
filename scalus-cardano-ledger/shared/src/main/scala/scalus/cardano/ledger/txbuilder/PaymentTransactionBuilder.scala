package scalus.cardano.ledger.txbuilder

import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx, Tx}
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.{Native, PlutusV3}

import java.math.BigInteger
import scala.jdk.CollectionConverters.*

case class PaymentTransactionBuilder(
    context: BuilderContext,
    var sender: Address,
    var paymentAddress: Address,
    var payment: Value,
    var script: Option[PlutusV3],
    var attachedNativeScript: Option[Native],
    var collateralPayer: Address = null
) {

    def buildAndSign(signer: TxSigner) = signer.signTx(build)

    def build = {
        (script, attachedNativeScript) match {
            case (Some(plutusScript), _) => buildScriptTx(plutusScript)
            case (_, Some(nativeScript)) => buildNativeScriptTx(nativeScript)
            case (None, None)            => buildSimpleTx
        }
    }

    def setCollateralPayer(address: Address) = {
        this.collateralPayer = address
        this
    }

    private def buildScriptTx(v: Script.PlutusV3) = {
        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new ScriptTx()
                  .collectFrom(
                    context.backendService.getUtxoService
                        .getUtxos(sender.encode.get, 1, 1)
                        .getValue
                        .get(0),
                    PlutusData.unit()
                  )
                  .attachSpendingValidator(
                    PlutusV3Script.deserialize(co.nstant.in.cbor.model.ByteString(v.script.bytes))
                  )
                  .payToAddress(
                    paymentAddress.encode.get,
                    Amount.lovelace(BigInteger.valueOf(payment.coin.value))
                  )
                  .withChangeAddress(sender.encode.get)
            )
            .validFrom(0)
            .feePayer(sender.encode.get)
            .collateralPayer(collateralPayer.encode.get)
            .withSigner((c, t) => t)
            .build()

        val result = Transaction.fromCbor(cclTx.serialize())
        val redeemers = context.evaluator.evalPlutusScripts(result, resolveUtxos(result))
        result.copy(
          witnessSet = result.witnessSet.copy(
            redeemers = Some(KeepRaw(Redeemers.from(redeemers)))
          )
        )
    }

    private def buildNativeScriptTx(nativeScript: Script.Native) = {
        val cclNativeScript = convertToCCLNativeScript(nativeScript)

        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .from(sender.encode.get)
                  .payToAddress(
                    paymentAddress.encode.get,
                    Amount.lovelace(BigInteger.valueOf(payment.coin.value))
                  )
            )
            .preBalanceTx((ctx, tx) => {
                tx.getWitnessSet.getNativeScripts.add(cclNativeScript)
            })
            .validTo(Long.MaxValue)
            .validFrom(0)
            .withSigner((c, t) => t)
            .build()

        Transaction.fromCbor(cclTx.serialize())
    }

    private def convertToCCLNativeScript(scalusScript: Script.Native) =
        TxBuilderUtils.convertToCCLNativeScript(scalusScript)

    private def buildSimpleTx = {
        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .from(sender.encode.get)
                  .payToAddress(
                    paymentAddress.encode.get,
                    Amount.lovelace(BigInteger.valueOf(payment.coin.value))
                  )
            )
            .validTo(Long.MaxValue)
            .validFrom(0)
            .withSigner((c, t) => t)
            .build()

        Transaction.fromCbor(cclTx.serialize())
    }

    def resolveUtxos(transaction: Transaction): Map[TransactionInput, TransactionOutput] =
        TxBuilderUtils.resolveUtxos(transaction, context)

    def resolveUtxos(
        transaction: Transaction,
        inputUtxos: Map[TransactionInput, TransactionOutput]
    ): Map[TransactionInput, TransactionOutput] =
        TxBuilderUtils.resolveUtxos(transaction, inputUtxos, context)

}
