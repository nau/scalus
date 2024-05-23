package scalus.bloxbean

import co.nstant.in.cbor.model.UnsignedInteger
import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.backend.api.DefaultProtocolParamsSupplier
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.api.ScriptService
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.backend.blockfrost.service.http.ScriptApi
import com.bloxbean.cardano.client.coinselection.impl.DefaultUtxoSelector
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.common.CardanoConstants
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.plutus.util.PlutusUtil
import com.bloxbean.cardano.client.quicktx.QuickTxBuilder
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.spec.Script
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import com.bloxbean.cardano.yaci.core.util.CborSerializationUtil
import com.fasterxml.jackson.databind.ObjectMapper
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.builtin.Data
import scalus.examples.MintingPolicyV2
import scalus.examples.PubKeyValidator
import scalus.prelude.AssocMap
import scalus.uplc.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.eval.ExBudget
import scalus.utils.Utils

import java.io.File
import java.math.BigInteger
import java.nio.file.Files
import java.nio.file.Paths
import java.util
import java.util.Optional
import co.nstant.in.cbor.model as cbor
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

class TxEvaluatorSpec extends AnyFunSuite:
    val senderMnemonic: String =
        "drive useless envelope shine range ability time copper alarm museum near flee wrist live type device meadow allow churn purity wisdom praise drop code";
    val sender1 = new Account(Networks.testnet(), senderMnemonic)
    val sender1Addr: String = sender1.baseAddress()

    test("TxEvaluator ") {
        import scala.jdk.CollectionConverters.*
        val costMdls = CostMdls()
        costMdls.add(CostModelUtil.PlutusV1CostModel)
        costMdls.add(CostModelUtil.PlutusV2CostModel)
        val evaluator = TxEvaluator(
          SlotConfig.default,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = 8,
          costMdls = costMdls
        )
        val pubKeyValidator =
            compile(PubKeyValidator.validatorV2(hex"deadbeef")).toPlutusProgram((1, 0, 0))
        val s: PlutusV2Script =
            PlutusV2Script
                .builder()
                .cborHex(pubKeyValidator.doubleCborHex)
                .build()
                .asInstanceOf[PlutusV2Script]
        val scripts: util.List[PlutusScript] = util.List.of(s)
        val pubKeyScriptAddress = AddressProvider.getEntAddress(s, Networks.testnet())
        val input = TransactionInput.builder().transactionId("deadbeef").index(0).build()
        val inputs = util.List.of(input)

        val utxo = Map(
          input -> TransactionOutput
              .builder()
              .value(Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(pubKeyScriptAddress.getAddress)
              .datumHash(Utils.hexToBytes(PlutusData.unit().getDatumHash))
              .build()
        )
        val redeemer = Redeemer
            .builder()
            .tag(RedeemerTag.Spend)
            .data(PlutusData.unit())
            .index(BigInteger.ZERO)
            .exUnits(
              ExUnits
                  .builder()
                  .mem(BigInteger.valueOf(10_000_000L))
                  .steps(BigInteger.valueOf(10_000_000_000L))
                  .build()
            )
            .build()
        val tx = Transaction
            .builder()
            .body(
              TransactionBody
                  .builder()
                  .fee(ADAConversionUtil.adaToLovelace(0.2))
                  .ttl(1000)
                  .inputs(inputs)
                  .requiredSigners(util.List.of(hex"deadbeef".bytes))
                  .build()
            )
            .witnessSet(
              TransactionWitnessSet
                  .builder()
                  .plutusV2Scripts(util.List.of(s))
                  .redeemers(util.List.of(redeemer))
                  .plutusDataList(util.List.of(PlutusData.unit()))
                  .build()
            )
            .build()
        val redeemers = evaluator.evaluateTx(tx, utxo)
        assert(redeemers.size == 1)
        val redeemerResult = redeemers.head
        assert(redeemerResult.getExUnits.getMem.longValue == 13975L)
        assert(redeemerResult.getExUnits.getSteps.longValue == 4871088L)
    }
