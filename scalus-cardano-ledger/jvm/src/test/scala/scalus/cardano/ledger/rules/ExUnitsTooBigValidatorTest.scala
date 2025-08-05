package scalus.cardano.ledger.rules
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.cardano.ledger.RedeemerTag.Spend
import scalus.cardano.ledger.{ExUnits, Redeemer, Redeemers, TransactionWitnessSet}

class ExUnitsTooBigValidatorTest extends AnyFunSuite, ValidatorRulesTestKit{
    test("ExUnitsTooBigValidator success") {
        val context = Context()
        val exUnits = ExUnits(1, 1)
        val tx =
            randomValidTransaction
                .copy(
                  witnessSet = TransactionWitnessSet(
                    redeemers = Some(KeepRaw(Redeemers.from(
                      Seq(Redeemer(Spend, 0, Data.unit, exUnits))
                    )))
                  )
                )
        val result = ExUnitsTooBigValidator.validate(tx)
        assert(result.isRight)
    }
    test("ExUnitsTooBigValidator failure") {
        val context = Context()
        val exUnits = ExUnits(Long.MaxValue, Long.MaxValue)
        val tx =
            randomValidTransaction
                .copy(
                  witnessSet = TransactionWitnessSet(
                    redeemers = Some(
                      KeepRaw(
                        Redeemers.from(
                          Seq(Redeemer(Spend, 0, Data.unit, exUnits))
                        )
                      )
                    )
                  )
                )
        val result = ExUnitsTooBigValidator.validate(tx)
        assert(result.isLeft)
    }
}
