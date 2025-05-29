package scalus.cardano.ledger.rules

import scalus.cardano.ledger.*
import scala.util.boundary
import scala.util.boundary.break
import EqualLedgerValidator.*
import EqualLedgerValidator.Expression.*

class EqualLedgerValidator[T](lhs: Expression[T])(rhs: Expression[T]) extends LedgerValidator {
    override def validate(state: State, event: Event): Either[Error, Unit] = {
        for
            lhsResult <- lhs(state, event)
            rhsResult <- rhs(state, event)
            result <-
                if lhsResult == rhsResult then Right(())
                else
                    Left(
                      IllegalArgumentException(
                        s"Expressions $lhs = $lhsResult and $rhs = $rhsResult are not equal"
                      )
                    )
        yield result
    }
}

object EqualLedgerValidator {
    object InputsEqualsSumOfOutputsAndFee
        extends EqualLedgerValidator[Long](InputsAmount)(Sum(OutputsAmount, FeeAmount))

    trait Expression[T]
        extends (
            (LedgerValidator#State, LedgerValidator#Event) => Either[LedgerValidator#Error, T]
        ) {
        final type State = LedgerValidator#State
        final type Event = LedgerValidator#Event
        final type Error = LedgerValidator#Error

        def name: String
        def evaluate(state: State, event: Event): Either[Error, T]

        override final def apply(state: State, event: Event): Either[Error, T] =
            evaluate(state, event)
    }

    object Expression {
        object InputsAmount extends Expression[Long] {
            override def name: String = "InputsAmount"

            override def evaluate(state: State, event: Event): Either[Error, Long] = boundary {
                val outputs =
                    for input <- event.body.inputs
                    yield
                        val output = state.utxo.get(input)
                        if output.isEmpty then
                            break(
                              Left(IllegalArgumentException(s"Input: $input doesn't exist in utxo"))
                            )
                        output.get

                val result = outputs.foldLeft(0L) { (acc, output) =>
                    val value = output match
                        case shelley: TransactionOutput.Shelley => shelley.value
                        case babbage: TransactionOutput.Babbage => babbage.value

                    val amount = value match
                        case ada: Value.Ada               => ada.coin.value
                        case multiAsset: Value.MultiAsset => multiAsset.coin.value

                    acc + amount
                }

                Right(result)
            }
        }

        object OutputsAmount extends Expression[Long] {
            override def name: String = "OutputsAmount"

            override def evaluate(state: State, event: Event): Either[Error, Long] = {
                val result = event.body.outputs.foldLeft(0L) { (acc, output) =>
                    val value = output match
                        case shelley: TransactionOutput.Shelley => shelley.value
                        case babbage: TransactionOutput.Babbage => babbage.value

                    val amount = value match
                        case ada: Value.Ada               => ada.coin.value
                        case multiAsset: Value.MultiAsset => multiAsset.coin.value

                    acc + amount
                }

                Right(result)
            }
        }

        object FeeAmount extends Expression[Long] {
            override def name: String = "FeeAmount"

            override def evaluate(state: State, event: Event): Either[Error, Long] = {
                Right(event.body.fee.value)
            }
        }

        class Sum[T](val expressions: Expression[T]*)(using num: Numeric[T]) extends Expression[T] {
            override def name: String = s"Sum of ${expressions.view.map(_.name).mkString(", ")}"

            override def evaluate(state: State, event: Event): Either[Error, T] = boundary {
                val result = expressions.view
                    .map { expression =>
                        expression.evaluate(state, event) match
                            case Right(value)       => value
                            case left @ Left(error) => break(left)
                    }
                    .foldLeft(num.zero)(num.plus)

                Right(result)
            }
        }
    }
}
