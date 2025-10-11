package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.testkit.ScalusTest
import scalus.builtin.{ByteString, Data}
import scalus.builtin.ByteString.*
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.txbuilder.{BuilderContext, Environment, PubKeyWitness, TransactionUnspentOutput, Wallet as WalletTrait, Witness}
import scalus.examples.vault.{Transactions, Vault}
import scalus.examples.vault.Vault.{Datum, State}
import scalus.examples.vault.VaultContract
import scalus.uplc.eval.ExBudget
import scalus.ledger.api.v3

class VaultTest extends AnyFunSuite, ScalusTest {

    // Minimal protocol params for testing
    private val testProtocolParams = CardanoInfo.mainnet.protocolParams

    // Test environment setup
    private val env = Environment(
      protocolParams = testProtocolParams,
      evaluator = PlutusScriptEvaluator(
        CardanoInfo.mainnet.slotConfig,
        initialBudget = ExBudget.enormous,
        protocolMajorVersion = CardanoInfo.mainnet.majorProtocolVersion,
        costModels = CostModels.fromProtocolParams(testProtocolParams)
      ),
      network = CardanoInfo.mainnet.network
    )

    // Create a simple test address
    private val ownerAddress = ShelleyAddress(
      network = CardanoInfo.mainnet.network,
      payment =
          ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(ByteString.fromHex("a".repeat(56)))),
      delegation = ShelleyDelegationPart.Null
    )

    private val defaultInitialAmount: BigInt = BigInt(10_000_000L)

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false
    )

    def createTestWallet(address: Address, ada: BigInt): WalletTrait = new WalletTrait {
        // Create a simple test UTxO
        private val testInput = TransactionInput(
          TransactionHash.fromByteString(ByteString.fromHex("0" * 64)),
          0
        )
        private val testOutput = TransactionOutput.Babbage(
          address = address,
          value = Value(Coin(ada.toLong)),
          datumOption = None,
          scriptRef = None
        )
        private val txUnspentOutput = TransactionUnspentOutput(testInput, testOutput)

        override def owner: Address = address
        override def utxo: UTxO = Map((testInput, testOutput))
        override def collateralInputs: Seq[(TransactionUnspentOutput, Witness)] = Seq(
          (txUnspentOutput, PubKeyWitness)
        )
        override def selectInputs(
            required: Value
        ): Option[Seq[(TransactionUnspentOutput, Witness)]] = {
            val available = testOutput.value
            if available.coin.value >= required.coin.value then {
                Some(Seq((txUnspentOutput, PubKeyWitness)))
            } else {
                None
            }
        }
    }

    def lockVault(amount: BigInt) = {
        val wallet = createTestWallet(ownerAddress, amount + 10_000_000L)
        val context = BuilderContext(env, wallet)
        new Transactions(context).lock(Value(Coin(amount.toLong)), ownerAddress) match {
            case Right(tx)   => tx
            case Left(error) => throw new Exception(s"Failed to lock vault: $error")
        }
    }

    def getVaultUtxo(tx: Transaction): (TransactionInput, TransactionOutput) = {
        val outputs = tx.body.value.outputs

        val scriptOutputWithIndex = outputs.zipWithIndex
            .find { case (output, _) => output.value.address.hasScript }
            .getOrElse(throw new Exception("No script output found in transaction"))

        val (scriptOutput, index) = scriptOutputWithIndex

        val txHash = tx.id
        val input = TransactionInput(txHash, index)

        (input, scriptOutput.value)
    }

    def getScriptContext(
        tx: Transaction,
        utxos: UTxO,
        spentInput: TransactionInput
    ): v3.ScriptContext = {
        val inputs = tx.body.value.inputs
        // assume 1 script input
        val inputIdx = inputs.toSeq.indexWhere(_ == spentInput)

        val redeemersMap = tx.witnessSet.redeemers.get.value.toMap
        val (data, exUnits) = redeemersMap.getOrElse(
          (RedeemerTag.Spend, inputIdx),
          throw new Exception(s"No redeemer found for spend input at index $inputIdx")
        )
        val redeemer = scalus.cardano.ledger.Redeemer(RedeemerTag.Spend, inputIdx, data, exUnits)

        val spentOutput = utxos.getOrElse(
          spentInput,
          throw new Exception(s"Spent output not found in UTxO set: $spentInput")
        )
        val datum = spentOutput match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) => Some(d)
            case _                                                               => None
        }

        LedgerToPlutusTranslation.getScriptContextV3(
          redeemer,
          datum,
          tx,
          utxos,
          CardanoInfo.mainnet.slotConfig,
          CardanoInfo.mainnet.majorProtocolVersion
        )
    }

    test("vault withdrawal request") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = getVaultUtxo(lockTx)

        val wallet = createTestWallet(ownerAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        val withdrawTx = new Transactions(context).withdraw(vaultUtxo) match {
            case Right(tx)   => tx
            case Left(error) => fail(s"Failed to build withdraw transaction: $error")
        }
        val utxos: UTxO = Map(vaultUtxo) ++ wallet.utxo

        val scriptContext = getScriptContext(withdrawTx, utxos, vaultUtxo._1)

        val result = VaultContract.compiled.runScript(scriptContext)

        assert(result.isSuccess)

        val newVaultUtxo = getVaultUtxo(withdrawTx)
        newVaultUtxo._2 match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                val newDatum = d.to[Vault.Datum]
                assert(
                  newDatum.state == Vault.State.Pending,
                  s"Vault state should be Pending, got ${newDatum.state}"
                )
                assert(
                  newDatum.amount == defaultInitialAmount,
                  "Vault amount should remain unchanged"
                )
        }
    }
}
