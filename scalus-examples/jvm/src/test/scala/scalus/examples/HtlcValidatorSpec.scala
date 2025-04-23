package scalus.examples

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.common.OrderEnum
import com.bloxbean.cardano.client.api.model.{Amount, ProtocolParams, Utxo}
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.api.{model, ProtocolParamsSupplier}
import com.bloxbean.cardano.client.backend.api.{BackendService, DefaultUtxoSupplier, UtxoService}
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.plutus.spec.{Redeemer, *}
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, ScriptTx}
import com.bloxbean.cardano.client.transaction.spec
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.bloxbean.{Interop, ScalusTransactionEvaluator, SlotConfig, TxEvaluator}
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testkit.ScalusTest
import scalus.uplc.*
import scalus.uplc.eval.*
import scalus.utils.Hex.*

import java.math.BigInteger
import java.util

class HtlcValidatorSpec extends AnyFunSuite with ScalusTest {
    import HtlcValidator.{*, given}
    import Person.*

    test("successful committer") {
        TestCase(
          inputs = List(Committer gives 100),
          outputs = List(Committer gets 1050),
          value = 1000,
          fee = 50,
          timeout = 500,
          validRange = 1000,
          signatories = List(Committer),
          redeemer = Committer,
          expected = success
        ).runWithDebug()
    }

    test("successful receiver") {
        TestCase(
          inputs = List(Receiver gives 100),
          outputs = List(Receiver gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validRange = 500,
          signatories = List(Receiver),
          redeemer = Receiver,
          expected = success
        ).runWithDebug()
    }

    test("unsigned committer transaction") {
        TestCase(
          inputs = List(Committer gives 100),
          outputs = List(Committer gets 1050),
          value = 1000,
          fee = 50,
          timeout = 500,
          validRange = 1000,
          signatories = List.empty,
          redeemer = Committer,
          expected = failure(UnsignedCommitterTransaction)
        ).runWithDebug()
    }

    test("unsigned receiver transaction") {
        TestCase(
          inputs = List(Receiver gives 100),
          outputs = List(Receiver gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validRange = 500,
          signatories = List.empty,
          redeemer = Receiver,
          expected = failure(UnsignedReceiverTransaction)
        ).runWithDebug()
    }

    test("invalid committer time point") {
        TestCase(
          inputs = List(Committer gives 100),
          outputs = List(Committer gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validRange = 500,
          signatories = List(Committer),
          redeemer = Committer,
          expected = failure(InvalidCommitterTimePoint)
        ).runWithDebug()
    }

    test("invalid receiver time point") {
        TestCase(
          inputs = List(Receiver gives 100),
          outputs = List(Receiver gets 1050),
          value = 1000,
          fee = 50,
          timeout = 500,
          validRange = 1000,
          signatories = List(Receiver),
          redeemer = Receiver,
          expected = failure(InvalidReceiverTimePoint)
        ).runWithDebug()
    }

    test("invalid receiver preimage") {
        TestCase(
          inputs = List(ReceiverWithInvalidPreimage gives 100),
          outputs = List(ReceiverWithInvalidPreimage gets 1050),
          value = 1000,
          fee = 50,
          timeout = 1000,
          validRange = 500,
          signatories = List(ReceiverWithInvalidPreimage),
          redeemer = ReceiverWithInvalidPreimage,
          expected = failure(InvalidReceiverPreimage)
        ).runWithDebug()
    }

    test("successfully unlock HTLC with valid preimage") {
        import Interop.*

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

        // run as Scala function
        import scalus.ledger.api.v3.ToDataInstances.given
        HtlcValidator.validate(context.toData)
        // run as UPLC script
        checkResult(expected = success, actual = script.runWithDebug(context))

        val costMdls = CostMdls()
        costMdls.add(CostModelUtil.PlutusV3CostModel)
        val evaluator = TxEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = 10,
          costMdls = costMdls
        )
        val redeemers = evaluator.evaluateTx(tx, utxos)
        assert(redeemers.size == 1)

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

    enum Person(val pkh: ByteString):
        case Committer extends Person(genByteStringOfN(28).sample.get)
        case Receiver extends Person(genByteStringOfN(28).sample.get)
        case ReceiverWithInvalidPreimage extends Person(genByteStringOfN(28).sample.get)
        case A extends Person(genByteStringOfN(28).sample.get)
        case B extends Person(genByteStringOfN(28).sample.get)
        case C extends Person(genByteStringOfN(28).sample.get)
        case D extends Person(genByteStringOfN(28).sample.get)
        case E extends Person(genByteStringOfN(28).sample.get)
        case F extends Person(genByteStringOfN(28).sample.get)
        case G extends Person(genByteStringOfN(28).sample.get)
        case H extends Person(genByteStringOfN(28).sample.get)

    extension (self: Person)
        inline infix def gives(value: BigInt): Input = Input(self, value)
        inline infix def gets(value: BigInt): Output = Output(self, value)

    case class Input(person: Person, value: BigInt)
    case class Output(person: Person, value: BigInt)

    case class TestCase(
        inputs: List[Input] = List.empty,
        outputs: List[Output] = List.empty,
        value: BigInt = 0,
        fee: BigInt = 0,
        timeout: BigInt = 0,
        validRange: BigInt = 0,
        signatories: List[Person] = List.empty,
        redeemer: Committer.type | Receiver.type | ReceiverWithInvalidPreimage.type = Committer,
        expected: Either[String, Option[ExBudget]] = success
    ):
        def runWithDebug(): Unit = {
            val (action, contractDatum) = redeemer match
                case Person.Committer =>
                    makeActionAndContractDatumForCommitterTransaction(timeout)
                case Person.Receiver =>
                    makeActionAndContractDatumForReceiverTransaction(timeout)
                case Person.ReceiverWithInvalidPreimage =>
                    makeActionAndContractDatumForReceiverTransaction(
                      timeout,
                      isValidPreimage = false
                    )

            val context = makeSpendingScriptContext(
              inputs = inputs
                  .map(input => makePubKeyHashInput(input.person.pkh, input.value))
                  .prepended(makeScriptHashInput(scriptHash, value)),
              outputs = outputs
                  .map(output => makePubKeyHashOutput(output.person.pkh, output.value)),
              fee = fee,
              validRange = Interval.after(validRange),
              signatories = signatories.map(_.pkh),
              action = Option.Some(action),
              contractDatum = Option.Some(contractDatum)
            )

            checkResult(expected = expected, actual = script.runWithDebug(context))
        }

    private def makeActionAndContractDatumForCommitterTransaction(
        timeout: BigInt
    ): (Action, ContractDatum) = {
        val contractDatum = ContractDatum(
          committer = Person.Committer.pkh,
          receiver = Person.Receiver.pkh,
          image = genByteStringOfN(32).sample.get,
          timeout = timeout
        )

        (Action.Timeout, contractDatum)
    }

    private def makeActionAndContractDatumForReceiverTransaction(
        timeout: BigInt,
        isValidPreimage: Boolean = true
    ): (Action, ContractDatum) = {
        val action: Action.Reveal =
            Action.Reveal(preimage = genByteStringOfN(32).sample.get)

        val contractDatum = ContractDatum(
          committer = Person.Committer.pkh,
          receiver =
              if isValidPreimage then Person.Receiver.pkh
              else Person.ReceiverWithInvalidPreimage.pkh,
          image = if isValidPreimage then sha3_256(action.preimage) else ByteString.empty,
          timeout = timeout
        )

        (action, contractDatum)
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
    private lazy val script = compile(HtlcValidator.validate).scriptV3()
    private lazy val scriptHash = script.hash

    private def makeUnlockingTransaction2(
        input: TransactionInput,
        scriptRefInput: TransactionInput,
        contractDatum: ContractDatum,
        action: Action,
        signatories: Seq[PubKeyHash]
    ): Transaction = {
        import Interop.*

        import scala.jdk.CollectionConverters.*

        lazy val backendService: BackendService = BFBackendService("", "")

        val plutusScript = PlutusV3Script
            .builder()
            .`type`("PlutusScriptV3")
            .cborHex(script.doubleCborHex)
            .build()
            .asInstanceOf[PlutusV3Script]

        val htlcAddress: com.bloxbean.cardano.client.address.Address =
            AddressProvider.getBaseAddress(plutusScript, plutusScript, Networks.mainnet())
        val scriptHash = plutusScript.getScriptHash.toHex
        val asdf = this.scriptHash.toHex
        val ddd = htlcAddress.getPaymentCredentialHash.get.toHex

        val htlcUtxo = Utxo
            .builder()
            .address(htlcAddress.getAddress)
            .txHash(input.getTransactionId)
            .inlineDatum(toPlutusData(contractDatum.toData).serializeToHex())
            .amount(Seq(Amount.ada(10)).asJava)
            .build()

        val collateralUtxo = Utxo
            .builder()
            .address(sender.baseAddress())
            .txHash(input.getTransactionId)
            .amount(Seq(Amount.ada(5)).asJava)
            .build()

        val utxoSupplier = new DefaultUtxoSupplier(new UtxoService {
            def getUtxos(address: String, count: Int, page: Int): model.Result[util.List[Utxo]] =
                ???

            def getUtxos(
                address: String,
                count: Int,
                page: Int,
                order: OrderEnum
            ): model.Result[util.List[Utxo]] = {
                println(s"$address")
                val r = model.Result.success("asdf").asInstanceOf[model.Result[util.List[Utxo]]]
                r.withValue(util.List.of(collateralUtxo))
                    .asInstanceOf[model.Result[util.List[Utxo]]]
            }

            def getUtxos(
                address: String,
                unit: String,
                count: Int,
                page: Int
            ): model.Result[util.List[Utxo]] = ???

            def getUtxos(
                address: String,
                unit: String,
                count: Int,
                page: Int,
                order: OrderEnum
            ): model.Result[util.List[Utxo]] = ???

            def getTxOutput(txHash: String, outputIndex: Int): model.Result[Utxo] = {
                val r = model.Result.success("asdf").asInstanceOf[model.Result[Utxo]]
                r.withValue(collateralUtxo).asInstanceOf[model.Result[Utxo]]
            }
        })

        //        val params =
        //            val input = this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        //            read[ProtocolParams](input)(using ProtocolParams.blockfrostParamsRW)
        val protocolParams = ProtocolParams
            .builder()
            .coinsPerUtxoSize("12")
            .build()

        val scalusEvaluator = new ScalusTransactionEvaluator(protocolParams, utxoSupplier)

        val redeemer = Interop.toPlutusData(action.toData)

        val scriptTx = new ScriptTx()
            .collectFrom(htlcUtxo, redeemer)
            .payToAddress(sender.baseAddress(), htlcUtxo.getAmount)
            .attachSpendingValidator(plutusScript)

        val pubKeyHashBytes = sender.hdKeyPair().getPublicKey.getKeyHash
        val quickTxBuilder = new QuickTxBuilder(
          utxoSupplier,
          new ProtocolParamsSupplier {
              def getProtocolParams: ProtocolParams = protocolParams
          },
          null
        )
        val tx = quickTxBuilder
            .compose(scriptTx)
            .feePayer(sender.getBaseAddress.getAddress)
            //            .withSigner(SignerProviders.signerFrom(acc))
            .withTxEvaluator(scalusEvaluator)
            .withCollateralInputs(input)
            .validFrom(contractDatum.timeout.toLong - 1000)
            .withRequiredSigners(pubKeyHashBytes)
            .ignoreScriptCostEvaluationError(false)
            .buildAndSign()
        tx
    }

}
