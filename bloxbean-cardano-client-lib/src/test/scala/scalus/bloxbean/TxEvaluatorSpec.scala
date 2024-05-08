package scalus.flat

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.model.{Amount, Utxo}
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.backend.api.{DefaultProtocolParamsSupplier, DefaultUtxoSupplier}
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.coinselection.impl.DefaultUtxoSelector
import com.bloxbean.cardano.client.common.{ADAConversionUtil, CardanoConstants}
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx}
import com.bloxbean.cardano.client.transaction.spec.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.bloxbean.{ScalusTransactionEvaluator, SlotConfig, TxEvaluator}
import scalus.*
import scalus.builtin.ByteString.given
import scalus.builtin.{ByteString, Data}
import scalus.examples.{MintingPolicyV2, PubKeyValidator}
import scalus.prelude.AssocMap
import scalus.uplc.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.eval.ExBudget

import java.math.BigInteger
import java.util.{List, Set}

class TxEvaluatorSpec extends AnyFunSuite:
    val senderMnemonic =
        "drive useless envelope shine range ability time copper alarm museum near flee wrist live type device meadow allow churn purity wisdom praise drop code";
    val sender1 = new Account(Networks.testnet(), senderMnemonic);
    val sender1Addr = sender1.baseAddress();

    test("TxEvaluator ") {
        import scala.jdk.CollectionConverters.*
        val evaluator = TxEvaluator(
          SlotConfig.default,
          initialBudgetConfig = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L)
        )
        val pubKeyValidator =
            compile(PubKeyValidator.validatorV2(hex"deadbeef")).toPlutusProgram((1, 0, 0))
        val s: PlutusV2Script =
            PlutusV2Script
                .builder()
                .cborHex(pubKeyValidator.doubleCborHex)
                .build()
                .asInstanceOf[PlutusV2Script]
        val scripts: List[PlutusScript] = List.of(s)
        val pubKeyScriptAddress = AddressProvider.getEntAddress(s, Networks.testnet())
        println(
          s"Pubkey script address: ${pubKeyScriptAddress.getAddress}, type hash: ${pubKeyScriptAddress.getPaymentCredentialHash.map(ByteString.fromArray)}"
        )
        val utxo = Set.of(
          Utxo.builder
              .txHash("deadbeef")
              .outputIndex(0)
              .amount(List.of(Amount.ada(20)))
              .address(pubKeyScriptAddress.getAddress)
              .dataHash(PlutusData.unit().getDatumHash)
              .build()
        )
        val inputs = List.of(TransactionInput.builder().transactionId("deadbeef").index(0).build())
        val redeemer = Redeemer
            .builder()
            .tag(RedeemerTag.Spend)
            .data(PlutusData.unit())
            .index(BigInteger.ZERO)
            .exUnits(
              ExUnits
                  .builder()
                  .mem(BigInteger.valueOf(1000000L))
                  .steps(BigInteger.valueOf(1000000L))
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
                  .requiredSigners(List.of(hex"deadbeef".bytes))
                  .build()
            )
            .witnessSet(
              TransactionWitnessSet
                  .builder()
                  .plutusV2Scripts(List.of(s))
                  .redeemers(List.of(redeemer))
                  .plutusDataList(List.of(PlutusData.unit()))
                  .build()
            )
            .build()
        val costMdls = CostMdls()
        costMdls.add(CostModelUtil.PlutusV1CostModel)
        costMdls.add(CostModelUtil.PlutusV2CostModel)
        val redeemers = evaluator.evaluateTx(tx, utxo, scripts, costMdls)
        println(redeemers)
    }

    ignore("Blockfrost testnet evaluate tx with minting policy v2") {
        val apiKey = System.getenv("BLOCKFROST_API_KEY")
        val backendService = new BFBackendService(Constants.BLOCKFROST_TESTNET_URL, apiKey)
        val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
        val protocolParamsSupplier =
            new DefaultProtocolParamsSupplier(backendService.getEpochService)
        val evaluator = ScalusTransactionEvaluator(utxoSupplier, protocolParamsSupplier)
        val utxoSelector = new DefaultUtxoSelector(utxoSupplier);
        val utxoOptional = utxoSelector.findFirst(
          sender1Addr,
          utxo =>
              utxo
                .getAmount
                  .stream()
                  .anyMatch(a =>
                      CardanoConstants.LOVELACE.equals(a.getUnit) && a
                        .getQuantity
                          .compareTo(ADAConversionUtil.adaToLovelace(2)) >= 0
                  )
        ); // Find an utxo with at least 2 ADA

        val utxo = utxoOptional.orElseThrow();
        val txId = ByteString.fromHex(utxo.getTxHash)
        val idx = BigInt(utxo.getOutputIndex)
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
