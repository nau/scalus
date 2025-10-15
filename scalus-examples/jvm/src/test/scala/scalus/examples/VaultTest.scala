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

    private val testProtocolParams = CardanoInfo.mainnet.protocolParams

    private val env = Environment(
      protocolParams = testProtocolParams,
      evaluator = PlutusScriptEvaluator(
        CardanoInfo.mainnet.slotConfig,
        initialBudget = ExBudget.enormous,
        protocolMajorVersion = CardanoInfo.mainnet.majorProtocolVersion,
        costModels = testProtocolParams.costModels
      ),
      network = CardanoInfo.mainnet.network
    )

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
        val wallet = createTestWallet(ownerAddress, amount + 50_000_000L)
        val context = BuilderContext(env, wallet)
        Transactions(context).lock(Value(Coin(amount.toLong)), ownerAddress).right.get
    }

    def withdrawVault(vaultUtxo: (TransactionInput, TransactionOutput)): Transaction = {
        val wallet = createTestWallet(ownerAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        new Transactions(context).withdraw(vaultUtxo).right.get
    }

    def depositVault(
        vaultUtxo: (TransactionInput, TransactionOutput),
        depositAmount: BigInt
    ): Transaction = {
        val wallet = createTestWallet(ownerAddress, depositAmount + 50_000_000L)
        val context = BuilderContext(env, wallet)
        new Transactions(context).deposit(vaultUtxo, Value(Coin(depositAmount.toLong))).right.get
    }

    def finalizeVault(vaultUtxo: (TransactionInput, TransactionOutput)): Transaction = {
        val wallet = createTestWallet(ownerAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        new Transactions(context).finalize(vaultUtxo, ownerAddress).right.get
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

        val withdrawTx = withdrawVault(vaultUtxo)

        val wallet = createTestWallet(ownerAddress, 50_000_000L)
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

    test("vault deposit adds funds") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = getVaultUtxo(lockTx)

        val depositAmount = BigInt(5_000_000L)
        val depositTx = depositVault(vaultUtxo, depositAmount)

        val wallet = createTestWallet(ownerAddress, depositAmount + 50_000_000L)
        val utxos: UTxO = Map(vaultUtxo) ++ wallet.utxo

        val scriptContext = getScriptContext(depositTx, utxos, vaultUtxo._1)
        val result = VaultContract.compiled.runScript(scriptContext)

        assert(result.isSuccess, s"Deposit should succeed: $result")
        val newVaultUtxo = getVaultUtxo(depositTx)

        newVaultUtxo._2 match {
            case TransactionOutput.Babbage(_, value, Some(DatumOption.Inline(d)), _) =>
                val newDatum = d.to[Vault.Datum]
                assert(
                  newDatum.state == Vault.State.Idle,
                  s"Vault state should remain Idle, got ${newDatum.state}"
                )
                assert(
                  newDatum.amount == defaultInitialAmount + depositAmount,
                  s"Vault amount should be ${defaultInitialAmount + depositAmount}, got ${newDatum.amount}"
                )
                assert(
                  value.coin.value == (defaultInitialAmount + depositAmount).toLong,
                  s"Vault value should match datum amount"
                )
            case _ => fail("Vault output should have inline datum")
        }
    }

    test("vault finalization fails when vault is in Idle state") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = getVaultUtxo(lockTx)

        val wallet = createTestWallet(ownerAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        val result = new Transactions(context).finalize(vaultUtxo, ownerAddress)

        assert(result.isLeft, "Finalize on Idle vault should fail during transaction building")
        val errorMsg = result.swap.getOrElse("")
        assert(
          errorMsg.contains("Error evaluated"),
          s"Expected script evaluation failure, got: $errorMsg"
        )
    }

    test("vault finalization succeeds after withdrawal request") {
        val lockTx = lockVault(defaultInitialAmount)
        val vaultUtxo = getVaultUtxo(lockTx)

        val withdrawTx = withdrawVault(vaultUtxo)

        val withdrawWallet = createTestWallet(ownerAddress, 50_000_000L)
        val withdrawUtxos: UTxO = Map(vaultUtxo) ++ withdrawWallet.utxo
        val withdrawContext = getScriptContext(withdrawTx, withdrawUtxos, vaultUtxo._1)
        val withdrawResult = VaultContract.compiled.runScript(withdrawContext)

        assert(withdrawResult.isSuccess, s"Withdraw should succeed: $withdrawResult")

        val pendingVaultUtxo = getVaultUtxo(withdrawTx)

        val finalizeTx = finalizeVault(pendingVaultUtxo)

        val finalizeWallet = createTestWallet(ownerAddress, 50_000_000L)
        val finalizeUtxos: UTxO = Map(pendingVaultUtxo) ++ finalizeWallet.utxo

        val finalizeContext = getScriptContext(finalizeTx, finalizeUtxos, pendingVaultUtxo._1)
        val finalizeResult = VaultContract.compiled.runScript(finalizeContext)

        assert(finalizeResult.isSuccess, s"Finalize should succeed: $finalizeResult")

        val scriptOutputs = finalizeTx.body.value.outputs.filter(_.value.address.hasScript)
        assert(scriptOutputs.isEmpty, "Finalize should close vault (no script outputs)")

        val ownerOutputs = finalizeTx.body.value.outputs.filter { output =>
            output.value.address == ownerAddress
        }
        assert(ownerOutputs.nonEmpty, "Finalize should send funds to owner")

        val ownerReceivedValue = ownerOutputs.head.value.value.coin.value
        assert(
          ownerReceivedValue >= defaultInitialAmount.toLong,
          s"Owner should receive at least the vault amount, got $ownerReceivedValue"
        )
    }
}
