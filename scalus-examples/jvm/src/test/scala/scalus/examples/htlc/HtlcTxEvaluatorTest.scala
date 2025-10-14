package scalus.examples.htlc

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.plutus.spec.{Redeemer, *}
import com.bloxbean.cardano.client.transaction.spec
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.bloxbean.{Interop, SlotConfig, TxEvaluator}
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.builtin.Data
import scalus.ledger.api.v3.*
import scalus.testkit.ScalusTest
import scalus.uplc.*
import scalus.uplc.eval.*

import java.math.BigInteger
import java.util

class HtlcTxEvaluatorTest extends AnyFunSuite with ScalusTest {
    test("successfully unlock HTLC with valid preimage") {
        val preimage = genByteStringOfN(32).sample.get
        val image = sha3_256(preimage)
        val committer = genByteStringOfN(28).sample.get
        val receiver = genByteStringOfN(28).sample.get
        val contractDatum = ContractDatum(
          committer = committer,
          receiver = receiver,
          image = image,
          timeout = 1745261347000L
        )

        val txid = random[TxId].hash.toHex
        val htlcInput = new TransactionInput(txid, 0)
        val scriptRefInput = new TransactionInput(txid, 1)

        // Use a transaction builder to create the transaction
        val tx = makeUnlockingTransaction(
          input = htlcInput,
          scriptRefInput = scriptRefInput,
          startTimeMillis = 1745261346000L,
          action = Action.Reveal(preimage),
          signatories = Seq(PubKeyHash(receiver))
        )

        val datum = contractDatum.toData

        val (utxos: Map[TransactionInput, TransactionOutput], context: ScriptContext) =
            makeContext(htlcInput, scriptRefInput, tx, datum)

        // run as Scala function
        HtlcValidator.validate(context.toData)

        // run as UPLC script
        checkResult(expected = success, actual = script.runWithDebug(context))

        // Use Scalus TxEvaluator calculate redeemers/costs/fees for the transaction
        val evaluator = TxEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = 10,
          costMdls = costMdls
        )
        // Runs Cardano Transaction Validation Ledger rules
        val redeemers = evaluator.evaluateTx(tx, utxos)
        assert(redeemers.size == 1)
    }

    private def makeContext(
        htlcInput: TransactionInput,
        scriptRefInput: TransactionInput,
        tx: Transaction,
        datum: ChangedParameters
    ) = {
        import Interop.*
        val payeeAddress = sender.baseAddress()

        val plutusScript = PlutusV3Script
            .builder()
            .`type`("PlutusScriptV3")
            .cborHex(script.doubleCborHex)
            .build()
            .asInstanceOf[PlutusV3Script]
        val scriptRefUtxo = Map(
          scriptRefInput -> TransactionOutput
              .builder()
              .value(spec.Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(payeeAddress)
              .scriptRef(
                plutusScript
              )
              .build()
        )
        val htlcAddress =
            AddressProvider.getBaseAddress(plutusScript, plutusScript, Networks.mainnet())
        val htlcUtxo = Map(
          htlcInput -> TransactionOutput
              .builder()
              .value(spec.Value.builder().coin(BigInteger.valueOf(20)).build())
              .address(htlcAddress.getAddress)
              .inlineDatum(toPlutusData(datum))
              .build()
        )

        val utxos = htlcUtxo ++ scriptRefUtxo

        // get ScriptContext from the transaction
        val context = Interop.getScriptContextV3(
          redeemer = tx.getWitnessSet.getRedeemers.get(0),
          datum = Some(datum),
          tx = tx,
          txhash = TransactionUtil.getTxHash(tx.serialize()),
          utxos = utxos,
          slotConfig = SlotConfig.Mainnet,
          protocolVersion = 10
        )
        (utxos, context)
    }

    private def makeUnlockingTransaction(
        input: TransactionInput,
        scriptRefInput: TransactionInput,
        startTimeMillis: PosixTime,
        action: Action,
        signatories: Seq[PubKeyHash]
    ): Transaction = {
        import Interop.*

        import scala.jdk.CollectionConverters.*
        val redeemer = action.toData

        val rdmr = Redeemer
            .builder()
            .tag(RedeemerTag.Spend)
            .data(toPlutusData(redeemer))
            .index(0)
            .exUnits(
              ExUnits
                  .builder()
                  .steps(BigInteger.valueOf(1000))
                  .mem(BigInteger.valueOf(1000))
                  .build()
            )
            .build()

        val inputs = util.List.of(input)

        val tx = Transaction
            .builder()
            .body(
              TransactionBody
                  .builder()
                  .validityStartInterval(
                    SlotConfig.Mainnet.timeToSlot(startTimeMillis.toLong)
                  )
                  .fee(ADAConversionUtil.adaToLovelace(0.2))
                  .inputs(inputs)
                  .referenceInputs(util.List.of(scriptRefInput))
                  .requiredSigners(signatories.map(_.hash.bytes).asJava)
                  .build()
            )
            .witnessSet(TransactionWitnessSet.builder().redeemers(util.List.of(rdmr)).build())
            .build()
        tx
    }

    private lazy val sender = new Account()
    private lazy val script = HtlcValidator.script
    private lazy val scriptHash = script.hash

    private val costMdls = {
        val result = CostMdls()
        result.add(CostModelUtil.PlutusV3CostModel)
        result
    }
}
