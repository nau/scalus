package scalus.examples.twopartyescrow

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.Interval
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testkit.ScalusTest
import scalus.uplc.*
import scalus.uplc.eval.*

import scala.util.Try

class TwoPartyEscrowTest extends AnyFunSuite with ScalusTest {
    import TwoPartyEscrowTest.*

    test("buyer successfully deposits funds") {
        val depositTime = BigInt(100)
        TestCase(
          inputs = List(Buyer gives 200),
          outputs = List(
            ScriptOutput(
              EscrowDatum(
                state = EscrowState.Deposited,
                depositTime = depositTime,
                buyerKeyHash = Buyer.pkh,
                sellerKeyHash = Seller.pkh,
                escrowPrice = defaultEscrowPrice,
                refundTime = defaultRefundTime
              ),
              defaultEscrowPrice
            )
          ),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime,
          signatories = List(Buyer),
          action = Action.Deposit,
          contractDatum = Option.None,
          expected = ((), Option.Some(ExBudget(ExCPU(96429204), ExMemory(356853))))
        ).runWithDebug()
    }

    test("deposit fails without buyer signature") {
        val depositTime = BigInt(100)
        TestCase(
          inputs = List(Buyer gives 200),
          outputs = List(
            ScriptOutput(
              EscrowDatum(
                state = EscrowState.Deposited,
                depositTime = depositTime,
                buyerKeyHash = Buyer.pkh,
                sellerKeyHash = Seller.pkh,
                escrowPrice = defaultEscrowPrice,
                refundTime = defaultRefundTime
              ),
              defaultEscrowPrice
            )
          ),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime,
          signatories = List.empty,
          action = Action.Deposit,
          contractDatum = Option.None,
          expected = failure(BuyerSignatureMissing)
        ).runWithDebug()
    }

    test("deposit fails with wrong amount") {
        val depositTime = BigInt(100)
        TestCase(
          inputs = List(Buyer gives 200),
          outputs = List(
            ScriptOutput(
              EscrowDatum(
                state = EscrowState.Deposited,
                depositTime = depositTime,
                buyerKeyHash = Buyer.pkh,
                sellerKeyHash = Seller.pkh,
                escrowPrice = defaultEscrowPrice,
                refundTime = defaultRefundTime
              ),
              defaultEscrowPrice - 100 // Wrong amount
            )
          ),
          value = defaultEscrowPrice - 100,
          fee = 50,
          validFrom = depositTime,
          signatories = List(Buyer),
          action = Action.Deposit,
          contractDatum = Option.None,
          expected = failure(WrongScriptOutputAmount)
        ).runWithDebug()
    }

    test("deposit fails with wrong state") {
        val depositTime = BigInt(100)
        TestCase(
          inputs = List(Buyer gives 200),
          outputs = List(
            ScriptOutput(
              EscrowDatum(
                state = EscrowState.Initial, // Wrong state
                depositTime = depositTime,
                buyerKeyHash = Buyer.pkh,
                sellerKeyHash = Seller.pkh,
                escrowPrice = defaultEscrowPrice,
                refundTime = defaultRefundTime
              ),
              defaultEscrowPrice
            )
          ),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime,
          signatories = List(Buyer),
          action = Action.Deposit,
          contractDatum = Option.None,
          expected = failure(InvalidDepositState)
        ).runWithDebug()
    }

    test("seller successfully accepts escrow") {
        val depositTime = BigInt(100)
        val escrowDatum = EscrowDatum(
          state = EscrowState.Deposited,
          depositTime = depositTime,
          buyerKeyHash = Buyer.pkh,
          sellerKeyHash = Seller.pkh,
          escrowPrice = defaultEscrowPrice,
          refundTime = defaultRefundTime
        )

        TestCase(
          inputs = List(Seller gives 100),
          outputs = List(Seller gets defaultEscrowPrice),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime + 50,
          signatories = List(Seller),
          action = Action.Accept,
          contractDatum = Option.Some(escrowDatum),
          expected = ((), Option.Some(ExBudget(ExCPU(100911448), ExMemory(382091))))
        ).runWithDebug()
    }

    test("accept fails without seller signature") {
        val depositTime = BigInt(100)
        val escrowDatum = EscrowDatum(
          state = EscrowState.Deposited,
          depositTime = depositTime,
          buyerKeyHash = Buyer.pkh,
          sellerKeyHash = Seller.pkh,
          escrowPrice = defaultEscrowPrice,
          refundTime = defaultRefundTime
        )

        TestCase(
          inputs = List(Seller gives 100),
          outputs = List(Seller gets defaultEscrowPrice),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime + 50,
          signatories = List.empty,
          action = Action.Accept,
          contractDatum = Option.Some(escrowDatum),
          expected = failure(SellerSignatureMissing)
        ).runWithDebug()
    }

    test("accept fails from non-deposited state") {
        val depositTime = BigInt(100)
        val escrowDatum = EscrowDatum(
          state = EscrowState.Initial, // Wrong state
          depositTime = depositTime,
          buyerKeyHash = Buyer.pkh,
          sellerKeyHash = Seller.pkh,
          escrowPrice = defaultEscrowPrice,
          refundTime = defaultRefundTime
        )

        TestCase(
          inputs = List(Seller gives 100),
          outputs = List(Seller gets defaultEscrowPrice),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime + 50,
          signatories = List(Seller),
          action = Action.Accept,
          contractDatum = Option.Some(escrowDatum),
          expected = failure(AcceptOnlyFromDeposited)
        ).runWithDebug()
    }

    test("buyer successfully refunds after deadline") {
        val depositTime = BigInt(100)
        val escrowDatum = EscrowDatum(
          state = EscrowState.Deposited,
          depositTime = depositTime,
          buyerKeyHash = Buyer.pkh,
          sellerKeyHash = Seller.pkh,
          escrowPrice = defaultEscrowPrice,
          refundTime = defaultRefundTime
        )

        TestCase(
          inputs = List(Buyer gives 100),
          outputs = List(Buyer gets defaultEscrowPrice),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime + defaultRefundTime + 2, // After deadline
          signatories = List(Buyer),
          action = Action.Refund,
          contractDatum = Option.Some(escrowDatum),
          expected = ((), Option.Some(ExBudget(ExCPU(97981073), ExMemory(363692))))
        ).runWithDebug()
    }

    test("refund fails before deadline") {
        val depositTime = BigInt(100)
        val escrowDatum = EscrowDatum(
          state = EscrowState.Deposited,
          depositTime = depositTime,
          buyerKeyHash = Buyer.pkh,
          sellerKeyHash = Seller.pkh,
          escrowPrice = defaultEscrowPrice,
          refundTime = defaultRefundTime
        )

        TestCase(
          inputs = List(Buyer gives 100),
          outputs = List(Buyer gets defaultEscrowPrice),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime + 50, // Before deadline
          signatories = List(Buyer),
          action = Action.Refund,
          contractDatum = Option.Some(escrowDatum),
          expected = failure(RefundTimeNotReached)
        ).runWithDebug()
    }

    test("refund fails without buyer signature") {
        val depositTime = BigInt(100)
        val escrowDatum = EscrowDatum(
          state = EscrowState.Deposited,
          depositTime = depositTime,
          buyerKeyHash = Buyer.pkh,
          sellerKeyHash = Seller.pkh,
          escrowPrice = defaultEscrowPrice,
          refundTime = defaultRefundTime
        )

        TestCase(
          inputs = List(Buyer gives 100),
          outputs = List(Buyer gets defaultEscrowPrice),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime + defaultRefundTime + 2,
          signatories = List.empty,
          action = Action.Refund,
          contractDatum = Option.Some(escrowDatum),
          expected = failure(BuyerSignatureMissing)
        ).runWithDebug()
    }

    test("refund fails from non-deposited state") {
        val depositTime = BigInt(100)
        val escrowDatum = EscrowDatum(
          state = EscrowState.Initial, // Wrong state
          depositTime = depositTime,
          buyerKeyHash = Buyer.pkh,
          sellerKeyHash = Seller.pkh,
          escrowPrice = defaultEscrowPrice,
          refundTime = defaultRefundTime
        )

        TestCase(
          inputs = List(Buyer gives 100),
          outputs = List(Buyer gets defaultEscrowPrice),
          value = defaultEscrowPrice,
          fee = 50,
          validFrom = depositTime + defaultRefundTime + 2,
          signatories = List(Buyer),
          action = Action.Refund,
          contractDatum = Option.Some(escrowDatum),
          expected = failure(RefundOnlyFromDeposited)
        ).runWithDebug()
    }
}

object TwoPartyEscrowTest extends ScalusTest {
    export TwoPartyEscrow.*
    export Person.*

    val defaultEscrowPrice: BigInt = BigInt(1000)
    val defaultRefundTime: BigInt = BigInt(500)

    enum Person(val pkh: Hash):
        case Buyer extends Person(genByteStringOfN(28).sample.get)
        case Seller extends Person(genByteStringOfN(28).sample.get)

        infix def gives(value: BigInt): Input = Input(this, value)
        infix def gets(value: BigInt): PubKeyOutput = PubKeyOutput(this, value)

    case class Input(person: Person, value: BigInt)
    case class PubKeyOutput(person: Person, value: BigInt)
    case class ScriptOutput(datum: EscrowDatum, value: BigInt)

    case class TestCase(
        inputs: List[Input] = List.empty,
        outputs: List[PubKeyOutput | ScriptOutput] = List.empty,
        value: BigInt = 0,
        fee: BigInt = 0,
        validFrom: BigInt = 0,
        signatories: List[Person] = List.empty,
        action: Action,
        contractDatum: Option[EscrowDatum] = Option.None,
        expected: (String | Unit, Option[ExBudget]) = success
    ):
        def runWithDebug(): Unit = {
            val context = makeSpendingScriptContext(
              inputs = inputs
                  .map(input => makePubKeyHashInput(input.person.pkh, input.value))
                  .prepended(makeScriptHashInput(scriptHash, value)),
              outputs = outputs.map {
                  case PubKeyOutput(person, value) => makePubKeyHashOutput(person.pkh, value)
                  case ScriptOutput(datum, value)  => makeScriptOutput(scriptHash, value, datum)
              },
              fee = fee,
              validRange = Interval.after(validFrom),
              signatories = signatories.map(_.pkh),
              action = Option.Some(action),
              contractDatum = contractDatum
            )

            val actual = Try(TwoPartyEscrow.validate(context.toData))
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
        action: Option[Action] = Option.None,
        contractDatum: Option[EscrowDatum] = Option.None,
        indexOfInputWithEscrowScript: BigInt = 0
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
            txOutRef = inputs.at(indexOfInputWithEscrowScript).outRef,
            datum = contractDatum.map(_.toData)
          )
        )
    }

    private def makeScriptOutput(
        scriptHash: Hash,
        value: BigInt,
        datum: EscrowDatum
    ): TxOut = {
        TxOut(
          address = ledger.api.v1.Address(
            ledger.api.v1.Credential.ScriptCredential(scriptHash),
            Option.None
          ),
          value = Value.lovelace(value),
          datum = OutputDatum.OutputDatum(datum.toData),
          referenceScript = Option.None
        )
    }

    private lazy val scriptHash = script.hash
}
