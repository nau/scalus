package scalus.examples

import scalus.builtin.ByteString
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{Environment, PubKeyWitness, TransactionUnspentOutput, Wallet as WalletTrait, Witness}
import scalus.ledger.api.v3
import scalus.uplc.Program
import scalus.plutusV3
import scalus.testkit.ScalusTest

object TestUtil extends ScalusTest {

    val testProtocolParams: ProtocolParams = CardanoInfo.mainnet.protocolParams

    val testEnvironment: Environment = Environment(
      protocolParams = testProtocolParams,
      slotConfig = CardanoInfo.mainnet.slotConfig,
      evaluator = (tx: Transaction, utxos: Map[TransactionInput, TransactionOutput]) => Seq.empty,
      network = CardanoInfo.mainnet.network
    )

    def createTestAddress(keyHash: String): ShelleyAddress = {
        ShelleyAddress(
          network = CardanoInfo.mainnet.network,
          payment = ShelleyPaymentPart.Key(AddrKeyHash.fromByteString(ByteString.fromHex(keyHash))),
          delegation = ShelleyDelegationPart.Null
        )
    }

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
        override def utxo: Utxos = Map((testInput, testOutput))
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

    def getScriptUtxo(tx: Transaction): (TransactionInput, TransactionOutput) = {
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
        utxos: Utxos,
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

    def runValidator(
        validatorProgram: Program,
        tx: Transaction,
        utxo: Utxos,
        wallet: WalletTrait,
        scriptInput: TransactionInput
    ) = {
        val scriptContext = TestUtil.getScriptContext(tx, utxo, scriptInput)
        validatorProgram.term.plutusV3.runWithDebug(scriptContext)
    }
}
