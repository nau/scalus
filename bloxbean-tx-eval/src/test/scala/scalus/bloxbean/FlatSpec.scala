package scalus.flat

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.backend.api.DefaultProtocolParamsSupplier
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.coinselection.impl.DefaultUtxoSelector
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.common.CardanoConstants
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.CostMdls
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import com.bloxbean.cardano.client.plutus.spec.PlutusScript
import com.bloxbean.cardano.client.plutus.spec.PlutusV2Script
import com.bloxbean.cardano.client.quicktx.QuickTxBuilder
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.Asset
import com.bloxbean.cardano.client.transaction.spec.Transaction
import com.bloxbean.cardano.client.transaction.spec.Transaction.TransactionBuilder
import com.bloxbean.cardano.client.transaction.spec.TransactionWitnessSet
import com.bloxbean.cardano.client.transaction.spec.TransactionWitnessSet.TransactionWitnessSetBuilder
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.bloxbean.ScalusTransactionEvaluator
import scalus.bloxbean.SlotConfig
import scalus.bloxbean.TxEvaluator
import scalus.builtin
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.builtin.ToDataInstances.given
import scalus.examples.MintingPolicy
import scalus.examples.MintingPolicyV2
import scalus.examples.MintingPolicyV2.simpleCtxV2Deserializer
import scalus.examples.PubKeyValidator
import scalus.prelude.AssocMap
import scalus.uplc.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.eval.ExBudget
import scalus.utils.Utils

import java.math.BigInteger
import java.util.List
import java.util.Set
import scala.util.Random

class FlatSpec extends AnyFunSuite:
    test("TxEvaluator ") {
        import scala.jdk.CollectionConverters.*
        val evaluator = TxEvaluator(
          SlotConfig.default,
          initialBudgetConfig = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L)
        )
        val pubKeyValidator = compile(PubKeyValidator.validatorV2).toPlutusProgram((1, 0, 0))
        val s: PlutusV2Script =
            PlutusV2Script
                .builder()
                .cborHex(pubKeyValidator.doubleCborHex)
                .build()
                .asInstanceOf[PlutusV2Script]
        val scripts: List[PlutusScript] = List.of(s)
        val tx = Transaction
            .builder()
            .witnessSet(
              TransactionWitnessSet
                  .builder()
                  .plutusV2Scripts(List.of(s))
                  .build()
            )
            .build()
        val costMdls = CostMdls()
        evaluator.evaluateTx(tx, Set.of(), scripts, costMdls)
    }

    ignore("Blockfrost testnet evaluate tx with minting policy v2") {
        val apiKey = System.getenv("BLOCKFROST_API_KEY")
        val backendService = new BFBackendService(Constants.BLOCKFROST_TESTNET_URL, apiKey)
        val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
        val protocolParamsSupplier =
            new DefaultProtocolParamsSupplier(backendService.getEpochService)
        val evaluator = ScalusTransactionEvaluator(utxoSupplier, protocolParamsSupplier)
        // addr_test1qp73ljurtknpm5fgey5r2y9aympd33ksgw0f8rc5khheg83y35rncur9mjvs665cg4052985ry9rzzmqend9sqw0cdksxvefah
        val senderMnemonic =
            "drive useless envelope shine range ability time copper alarm museum near flee wrist live type device meadow allow churn purity wisdom praise drop code";
        val sender1 = new Account(Networks.testnet(), senderMnemonic);
        val sender1Addr = sender1.baseAddress();
        val utxoSelector = new DefaultUtxoSelector(utxoSupplier);
        val utxoOptional = utxoSelector.findFirst(
          sender1Addr,
          utxo =>
              utxo
                  .getAmount()
                  .stream()
                  .anyMatch(a =>
                      CardanoConstants.LOVELACE.equals(a.getUnit()) && a
                          .getQuantity()
                          .compareTo(ADAConversionUtil.adaToLovelace(2)) >= 0
                  )
        ); // Find an utxo with at least 2 ADA

        val utxo = utxoOptional.orElseThrow();
        val txId = ByteString.fromHex(utxo.getTxHash())
        val idx = BigInt(utxo.getOutputIndex())
        val validator =
            MintingPolicyV2.compiledMintingPolicyScriptV2.toUplc(generateErrorTraces = true)
        val evaledTokens =
            val tokensSIR = compile(AssocMap.singleton(hex"484f534b59", BigInt("1000000000000000")))
            tokensSIR.toUplc()

        val appliedValidator =
            validator $ txId $ idx $ evaledTokens
        val program = Program((1, 0, 0), appliedValidator)
        val script = PlutusV2Script.builder().cborHex(program.doubleCborHex).build()
        val scriptTx = new ScriptTx()
            .collectFrom(utxo)
            .mintAsset(
              script,
              new Asset("SCALUS", BigInteger.valueOf(1)),
              PlutusData.unit(),
              sender1Addr
            );
        val result = new QuickTxBuilder(backendService)
            .compose(scriptTx)
            .feePayer(sender1Addr)
            .withSigner(SignerProviders.signerFrom(sender1))
            .withTxEvaluator(evaluator)
            .withTxInspector(transaction => {
                System.out.println(transaction);
            })
            .completeAndWait();
    }
