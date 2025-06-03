package scalus.cardano.ledger.rules

import scalus.cardano.ledger.*
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
                        s"Expressions ${lhs.description} = $lhsResult and ${rhs.description} = $rhsResult are not equal"
                      )
                    )
        yield result
    }
}

object EqualValidator {
    object InputsAmountEqualsSumOfOutputsAmountAndFeeAmount
        extends EqualValidator[Long](InputsAmount)(Sum(OutputsAmount, FeeAmount))

    trait Expression[T] {
        final type State = scalus.cardano.ledger.rules.State
        final type Event = Transaction
        final type Error = Throwable
        final type Result = Either[Error, T]

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
                    for input <- event.body.inputs
                    yield
                        val output = state.utxo.get(input)
                        if output.isEmpty then
                            break(
                              failure(
                                IllegalArgumentException(s"Input: $input doesn't exist in utxo")
                              )
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

                success(result)
            }
        }

        object OutputsAmount extends Expression[Long] {
            override def description: String = "OutputsAmount"

            override def evaluate(state: State, event: Event): Result = {
                val result = event.body.outputs.foldLeft(0L) { (acc, output) =>
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
                success(event.body.fee.value)
            }
        }

        class Sum[T](val expressions: Expression[T]*)(using num: Numeric[T]) extends Expression[T] {
            override def description: String =
                s"Sum of ${expressions.view.map(_.description).mkString(", ")}"

            override def evaluate(state: State, event: Event): Result =
                boundary {
                    val result = expressions.view
                        .map { expression =>
                            expression.evaluate(state, event) match
                                case Right(value)       => value
                                case left @ Left(error) => break(left)
                        }
                        .foldLeft(num.zero)(num.plus)

                    success(result)
                }
        }
    }
}
