package scalus.cardano.ledger
package rules

import scala.util.boundary
import scala.util.boundary.break
import EqualValidator.*
import EqualValidator.Expression.*

class EqualValidator[T](lhs: Expression[T])(rhs: Expression[T]) extends STS.Validator {
    override def validate(context: Context, state: State, event: Event): Result = {
        for
            lhsResult <- lhs(state, event)
            rhsResult <- rhs(state, event)
            result <-
                if lhsResult == rhsResult then success
                else
                    failure(
                      IllegalArgumentException(
                        s"Expressions ${lhs.description} = $lhsResult and ${rhs.description} = $rhsResult are not equal for transactionId ${event.id}"
                      )
                    )
        yield result
    }
}

// TODO inputs + minted + refind + withdrawals == outputs + fee + TotalDeposits + burnedMultiAsset + treasuryDonation
object EqualValidator {
    // It's part of Shelley.validateValueNotConservedUTxO in cardano-ledger
    object InputsAmountEqualsSumOfOutputsAmountAndFeeAmount
        extends EqualValidator[Long](InputsAmount)(Sum(OutputsAmount, FeeAmount))

    trait Expression[T] {
        final type State = scalus.cardano.ledger.rules.State
        final type Event = Transaction
        final type Value = T
        final type Error = Throwable
        final type Result = Either[Error, Value]

        def description: String
        def evaluate(state: State, event: Event): Result
        final def apply(state: State, event: Event): Result = evaluate(state, event)
        protected final def failure(error: Error): Result = Left(error)
        protected final def success(value: T): Result = Right(value)
    }

    object Expression {
        object InputsAmount extends Expression[Long] {
            override def description: String = "InputsAmount"

            override def evaluate(state: State, event: Event): Result = boundary {
                val outputs =
                    for input <- event.body.value.inputs
                    yield state.utxo.get(input) match
                        case Some(output) => output
                        case None =>
                            break(
                              failure(
                                IllegalArgumentException(
                                  s"Input: $input doesn't exist in utxo for transactionId ${event.id}"
                                )
                              )
                            )

                val result = outputs.foldLeft(0L) { (acc, output) =>
                    val value = output match
                        case shelley: TransactionOutput.Shelley => shelley.value
                        case babbage: TransactionOutput.Babbage => babbage.value

                    val amount = value match
                        case ada: Value.Ada               => ada.coin.value
                        case multiAsset: Value.MultiAsset => multiAsset.coin.value

                    acc + amount
                }

                success(result)
            }
        }

        object OutputsAmount extends Expression[Long] {
            override def description: String = "OutputsAmount"

            override def evaluate(state: State, event: Event): Result = {
                val result = event.body.value.outputs.foldLeft(0L) { (acc, output) =>
                    val value = output match
                        case shelley: TransactionOutput.Shelley => shelley.value
                        case babbage: TransactionOutput.Babbage => babbage.value

                    val amount = value match
                        case ada: Value.Ada               => ada.coin.value
                        case multiAsset: Value.MultiAsset => multiAsset.coin.value

                    acc + amount
                }

                success(result)
            }
        }

        object FeeAmount extends Expression[Long] {
            override def description: String = "FeeAmount"

            override def evaluate(state: State, event: Event): Result = {
                success(event.body.value.fee.value)
            }
        }

        class Sum[T](val expressions: Expression[T]*)(using num: Numeric[T]) extends Expression[T] {
            override def description: String =
                s"Sum of ${expressions.view.map(_.description).mkString(", ")}"

            override def evaluate(state: State, event: Event): Result = boundary {
                val result = expressions.view
                    .map { expression =>
                        expression.evaluate(state, event) match
                            case Right(value)             => value
                            case left: Left[Error, Value] => break(left)
                    }
                    .foldLeft(num.zero)(num.plus)

                success(result)
            }
        }
    }
}
