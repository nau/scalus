package scalus.examples

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
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testkit.ScalusTest
import scalus.uplc.*
import scalus.uplc.eval.*

import java.math.BigInteger
import java.util
import scala.util.Try

class HtlcValidatorTest extends AnyFunSuite with ScalusTest {
    import HtlcValidator.{*, given}
    import Person.*

    test("committer successfully unlocks HTLC after timeout") {
        TestCase(
          inputs = List(Committer gives 100),
          outputs = List(Committer gets 1050),
          value = 1000,
          fee = 50,
          timeout = 500,
          validFrom = 1000,
          signatories = List(Committer),
          action = Action.Timeout,
          preimage = genByteStringOfN(32).sample.get,
          expected = success
        ).runWithDebug()
    }

    test("receiver successfully unlocks HTLC with valid preimage before timeout") {
        val preimage = genByteStringOfN(32).sample.get
        TestCase(
          inputs = List(Receiver gives 100),
          outputs = List(Receiver gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validFrom = 500,
          signatories = List(Receiver),
          action = Action.Reveal(preimage),
          preimage = preimage,
          expected = success
        ).runWithDebug()
    }

    test("committer can't unlock HTLC before timeout") {
        TestCase(
          inputs = List(Committer gives 100),
          outputs = List(Committer gets 1050),
          value = 1000,
          fee = 50,
          timeout = 500,
          validFrom = 10,
          signatories = List(Committer),
          action = Action.Timeout,
          preimage = genByteStringOfN(32).sample.get,
          expected = failure("Committer Transaction must be exclusively after timeout")
        ).runWithDebug()
    }

    test("successfully unlock HTLC with valid preimage") {
        val preimage = genByteStringOfN(32).sample.get
        val image = sha3_256(preimage)
        val contractDatum = ContractDatum(
          committer = Person.Committer.pkh,
          receiver = Person.Receiver.pkh,
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
          signatories = Seq(PubKeyHash(Person.Receiver.pkh))
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

    def makeUnlockingTransaction(
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

    enum Person(val pkh: Hash):
        case Committer extends Person(genByteStringOfN(28).sample.get)
        case Receiver extends Person(genByteStringOfN(28).sample.get)

    extension (self: Person)
        infix def gives(value: BigInt): Input = Input(self, value)
        infix def gets(value: BigInt): Output = Output(self, value)

    case class Input(person: Person, value: BigInt)
    case class Output(person: Person, value: BigInt)

    case class TestCase(
        inputs: List[Input] = List.empty,
        outputs: List[Output] = List.empty,
        value: BigInt = 0,
        fee: BigInt = 0,
        timeout: BigInt = 0,
        validFrom: BigInt = 0,
        signatories: List[Person] = List.empty,
        action: Action,
        preimage: ByteString,
        expected: (String | Unit, Option[ExBudget]) = success
    ):
        def runWithDebug(): Unit = {
            val contractDatum = ContractDatum(
              committer = Person.Committer.pkh,
              receiver = Person.Receiver.pkh,
              image = sha3_256(preimage),
              timeout = timeout
            )

            val context = makeSpendingScriptContext(
              inputs = inputs
                  .map(input => makePubKeyHashInput(input.person.pkh, input.value))
                  .prepended(makeScriptHashInput(scriptHash, value)),
              outputs = outputs
                  .map(output => makePubKeyHashOutput(output.person.pkh, output.value)),
              fee = fee,
              validRange = Interval.after(validFrom),
              signatories = signatories.map(_.pkh),
              action = Option.Some(action),
              contractDatum = Option.Some(contractDatum)
            )

            val actual = Try(HtlcValidator.validate(context.toData))
            (expected, actual) match
                case ((msg: String, _), scala.util.Failure(exception)) =>
                case (((), _), scala.util.Success(_))                  =>
                case (_, actual) => fail(s"Expected: $expected, but got: $actual")

            checkResult(expected = expected, actual = script.runWithDebug(context))
        }

    private def makeSpendingScriptContext(
        inputs: List[TxInInfo] = List.empty,
        outputs: List[TxOut] = List.empty,
        fee: BigInt = 0,
        validRange: Interval = Interval.always,
        signatories: List[ByteString] = List.empty,
        action: Option[Action] = Option.empty,
        contractDatum: Option[ContractDatum] = Option.empty,
        indexOfInputWithHtlcScript: BigInt = 0
    ): ScriptContext = {
        ScriptContext(
          txInfo = TxInfo(
            inputs = inputs,
            outputs = outputs,
            fee = fee,
            validRange = validRange,
            signatories = signatories.map(PubKeyHash(_)),
            id = random[TxId]
          ),
          redeemer = action.map(_.toData).getOrElse(Data.unit),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.at(indexOfInputWithHtlcScript).outRef,
            datum = contractDatum.map(_.toData)
          )
        )
    }

    private lazy val sender = new Account()
    private lazy val script = HtlcValidator.script
    private lazy val scriptHash = script.hash
    private val costMdls = CostMdls()
    costMdls.add(CostModelUtil.PlutusV3CostModel)
}
