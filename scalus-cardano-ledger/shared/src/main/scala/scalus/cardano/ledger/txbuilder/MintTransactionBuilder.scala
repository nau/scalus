package scalus.cardano.ledger.txbuilder

import com.bloxbean.cardano.client.plutus.spec.{PlutusData, PlutusV3Script}
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx, Tx}
import com.bloxbean.cardano.client.transaction.spec.Asset
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.{Native, PlutusV3}
import scalus.cardano.ledger.ProtocolParams

import java.math.BigInteger
import scala.collection.immutable.TreeSet
import scala.jdk.CollectionConverters.SeqHasAsJava

case class MintTransactionBuilder(
    context: BuilderContext,
    var payer: Address,
    var receiver: Address,
    var tokens: MultiAsset,
    var script: Option[PlutusV3],
    var attachedNativeScript: Option[Native],
    var collateralPayer: Address = null
) {

    def buildAndSign(signer: TxSigner) = signer.signTx(build)

    def build = {
        (script, attachedNativeScript) match {
            case (Some(plutusScript), _) => buildScriptTx(plutusScript)
            case (_, Some(nativeScript)) => buildNativeScriptTx(nativeScript)
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
                        .getUtxos(payer.encode.get, 1, 1)
                        .getValue
                        .get(0),
                    PlutusData.unit()
                  )
                  .mintAsset(
                    PlutusV3Script.deserialize(co.nstant.in.cbor.model.ByteString(v.script.bytes)),
                    cclAssets.asJava,
                    PlutusData.unit(),
                    receiver.encode.get
                  )
                  .withChangeAddress(payer.encode.get)
            )
            .validFrom(0)
            .feePayer(payer.encode.get)
            .collateralPayer(collateralPayer.encode.get)
            .withSigner((c, t) => t)
            .build()

        val r = Transaction.fromCbor(cclTx.serialize())
        val redeemers =
            context.evaluator.evalPlutusScripts(r, TxBuilderUtils.resolveUtxos(r, context))
        setupRedeemers(context.protocolParams, r, redeemers)

    }

    private def buildNativeScriptTx(nativeScript: Script.Native) = {
        val cclNativeScript = TxBuilderUtils.convertToCCLNativeScript(nativeScript)

        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .from(payer.encode.get)
                  .mintAssets(cclNativeScript, cclAssets.asJava, receiver.encode.get)
            )
            // add the native script before fee balancing
            .preBalanceTx((ctx, tx) => {
                tx.getWitnessSet.getNativeScripts.add(cclNativeScript)
            })
            .validTo(Long.MaxValue)
            .validFrom(0)
            .withSigner((c, t) => t)
            .build()

        Transaction.fromCbor(cclTx.serialize())
    }

    private def cclAssets = {
        for {
            (policyId, assets) <- tokens.assets.toSeq
            asset <- assets
        } yield Asset
            .builder()
            .name(asset._1.toString)
            .value(BigInteger.valueOf(asset._2))
            .build()
    }

    def setupRedeemers(
        protocolParams: ProtocolParams,
        tx: Transaction,
        redeemers: Seq[Redeemer]
    ) = {
        if redeemers.isEmpty then {
            tx
        } else {
            val rawRedeemers = KeepRaw(Redeemers.from(redeemers))
            val usedModels = ScriptDataHashGenerator.getUsedCostModels(
              protocolParams,
              tx.witnessSet,
              TreeSet.empty[Language]
            )
            val scriptDataHash: Hash[Blake2b_256, HashPurpose.ScriptDataHash] =
                ScriptDataHashGenerator.computeScriptDataHash(
                  Era.Conway,
                  Some(rawRedeemers),
                  tx.witnessSet.plutusData,
                  usedModels
                )

            tx.copy(
              body = KeepRaw(tx.body.value.copy(scriptDataHash = Some(scriptDataHash))),
              witnessSet = tx.witnessSet.copy(redeemers = Some(rawRedeemers)),
            )
        }
    }
}
