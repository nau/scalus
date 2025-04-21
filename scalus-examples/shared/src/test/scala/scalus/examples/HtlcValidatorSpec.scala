package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.testkit.ScalusTest
import scalus.Compiler.compile
import scalus.builtin.{ByteString, Data}
import scalus.builtin.ByteString.*
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.Data.toData
import scalus.builtin.ToDataInstances.given
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.ledger.api.v3.ScriptInfo.SpendingScript
import scalus.prelude.{*, given}
import scalus.uplc.*
import scalus.uplc.eval.*

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
            this match
                case TestCase(
                      inputs,
                      outputs,
                      value,
                      fee,
                      timeout,
                      validRange,
                      signatories,
                      redeemer,
                      expected
                    ) =>
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

    private val script = compile(HtlcValidator.validate).scriptV3()
    private val scriptHash = script.hash
}
