package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
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

    TestCases(
      TestCase(
        description = "successful benefactor",
        inputs = List(Benefactor gives 100),
        outputs = List(Benefactor gets 1050),
        value = 1000,
        fee = 50,
        timeout = 500,
        validRange = 1000,
        signatories = List(Benefactor),
        redeemer = Benefactor,
        expected = success
      ),
      TestCase(
        description = "successful beneficiary",
        inputs = List(Beneficiary gives 100),
        outputs = List(Beneficiary gets 1050),
        value = 1000,
        fee = 50,
        timeout = 1000,
        validRange = 500,
        signatories = List(Beneficiary),
        redeemer = Beneficiary,
        expected = success
      ),
      TestCase(
        description = "unsigned benefactor transaction",
        inputs = List(Benefactor gives 100),
        outputs = List(Benefactor gets 1050),
        value = 1000,
        fee = 50,
        timeout = 500,
        validRange = 1000,
        signatories = List.empty,
        redeemer = Benefactor,
        expected = failure(UnsignedBenefactorTransaction)
      ),
      TestCase(
        description = "unsigned beneficiary transaction",
        inputs = List(Beneficiary gives 100),
        outputs = List(Beneficiary gets 1050),
        value = 1000,
        fee = 50,
        timeout = 1000,
        validRange = 500,
        signatories = List.empty,
        redeemer = Beneficiary,
        expected = failure(UnsignedBeneficiaryTransaction)
      ),
      TestCase(
        description = "invalid benefactor time point",
        inputs = List(Benefactor gives 100),
        outputs = List(Benefactor gets 1050),
        value = 1000,
        fee = 50,
        timeout = 1000,
        validRange = 500,
        signatories = List(Benefactor),
        redeemer = Benefactor,
        expected = failure(InvalidBenefactorTimePoint)
      ),
      TestCase(
        description = "invalid beneficiary time point",
        inputs = List(Beneficiary gives 100),
        outputs = List(Beneficiary gets 1050),
        value = 1000,
        fee = 50,
        timeout = 500,
        validRange = 1000,
        signatories = List(Beneficiary),
        redeemer = Beneficiary,
        expected = failure(InvalidBeneficiaryTimePoint)
      ),
      TestCase(
        description = "invalid beneficiary preimage",
        inputs = List(BeneficiaryWithInvalidPreimage gives 100),
        outputs = List(BeneficiaryWithInvalidPreimage gets 1050),
        value = 1000,
        fee = 50,
        timeout = 1000,
        validRange = 500,
        signatories = List(BeneficiaryWithInvalidPreimage),
        redeemer = BeneficiaryWithInvalidPreimage,
        expected = failure(InvalidPreimage)
      )
    )

    enum Person(val pkh: ByteString):
        case Benefactor extends Person(genByteStringOfN(28).sample.get)
        case Beneficiary extends Person(genByteStringOfN(28).sample.get)
        case BeneficiaryWithInvalidPreimage extends Person(genByteStringOfN(28).sample.get)
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
        description: String,
        inputs: List[Input] = List.empty,
        outputs: List[Output] = List.empty,
        value: BigInt = 0,
        fee: BigInt = 0,
        timeout: BigInt = 0,
        validRange: BigInt = 0,
        signatories: List[Person] = List.empty,
        redeemer: Benefactor.type | Beneficiary.type | BeneficiaryWithInvalidPreimage.type =
            Benefactor,
        expected: Either[String, Option[ExBudget]] = success
    )

    object TestCases {
        def apply(testCases: TestCase*): Unit = {
            testCases.zipWithIndex.foreach {
                case (
                      TestCase(
                        description,
                        inputs,
                        outputs,
                        value,
                        fee,
                        timeout,
                        validRange,
                        signatories,
                        redeemer,
                        expected
                      ),
                      testCaseIndex
                    ) =>
                    test(s"${testCaseIndex + 1}: $description") {
                        val (contractRedeemer, contractDatum) = redeemer match
                            case Person.Benefactor  => makeBenefactorRedeemerAndDatum(timeout)
                            case Person.Beneficiary => makeBeneficiaryRedeemerAndDatum(timeout)
                            case Person.BeneficiaryWithInvalidPreimage =>
                                makeBeneficiaryRedeemerAndDatum(timeout, isValidPreimage = false)

                        val context = makeSpendingScriptContext(
                          inputs = inputs
                              .map(input => makePubKeyHashInput(input.person.pkh, input.value))
                              .prepended(makeScriptHashInput(scriptHash, value)),
                          outputs = outputs
                              .map(output => makePubKeyHashOutput(output.person.pkh, output.value)),
                          fee = fee,
                          validRange = Interval.after(validRange),
                          signatories = signatories.map(_.pkh),
                          contractRedeemer = Option.Some(contractRedeemer),
                          contractDatum = Option.Some(contractDatum)
                        )

                        checkResult(expected = expected, actual = script.runWithDebug(context))
                    }
            }
        }
    }

    private def makeBenefactorRedeemerAndDatum(
        timeout: BigInt
    ): (ContractRedeemer, ContractDatum) = {
        val contractDatum = ContractDatum(
          benefactor = Person.Benefactor.pkh,
          beneficiary = Person.Beneficiary.pkh,
          image = genByteStringOfN(32).sample.get,
          timeout = timeout
        )

        (ContractRedeemer.Benefactor, contractDatum)
    }

    private def makeBeneficiaryRedeemerAndDatum(
        timeout: BigInt,
        isValidPreimage: Boolean = true
    ): (ContractRedeemer, ContractDatum) = {
        val contractRedeemer: ContractRedeemer.Beneficiary =
            ContractRedeemer.Beneficiary(preimage = genByteStringOfN(32).sample.get)

        val contractDatum = ContractDatum(
          benefactor = Person.Benefactor.pkh,
          beneficiary =
              if isValidPreimage then Person.Beneficiary.pkh
              else Person.BeneficiaryWithInvalidPreimage.pkh,
          image = if isValidPreimage then sha3_256(contractRedeemer.preimage) else ByteString.empty,
          timeout = timeout
        )

        (contractRedeemer, contractDatum)
    }

    private def makeSpendingScriptContext(
        inputs: List[TxInInfo] = List.empty,
        outputs: List[TxOut] = List.empty,
        fee: BigInt = 0,
        validRange: Interval = Interval.always,
        signatories: List[ByteString] = List.empty,
        contractRedeemer: Option[ContractRedeemer] = Option.empty,
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
          redeemer = contractRedeemer.map(_.toData).getOrElse(Data.unit),
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = inputs.at(indexOfInputWithHtlcScript).outRef,
            datum = contractDatum.map(_.toData)
          )
        )
    }

    private val script = compile(HtlcValidator.validate).scriptV3()
    private val scriptHash = script.hash
}
