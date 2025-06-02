package scalus.cardano.ledger.rules

import scalus.cardano.ledger.*
import scala.util.boundary
import scala.util.boundary.break
import EqualLedgerValidator.*
import EqualLedgerValidator.Expression.*

class EqualLedgerValidator[StateF[_] <: Ledger.StateI[_], T](
    lhs: Expression[? >: StateF <: Ledger.StateI, T]
)(
    rhs: Expression[? >: StateF <: Ledger.StateI, T]
) extends Ledger.STS.Validator[StateF] {
    override def validate[StateT: StateI](
        context: Context,
        state: StateT,
        event: Event
    ): Either[Error, Unit] = {
        for
            lhsResult <- lhs(state, event)
            rhsResult <- rhs(state, event)
            result <-
                if lhsResult == rhsResult then Right(())
                else
                    Left(
                      IllegalArgumentException(
                        s"Expressions ${lhs.description} = $lhsResult and ${rhs.description} = $rhsResult are not equal"
                      )
                    )
        yield result
    }
}

object EqualLedgerValidator {
    object InputsAmountEqualsSumOfOutputsAmountAndFeeAmount
        extends EqualLedgerValidator[Ledger.StateI.Utxo, Long](InputsAmount)(
          Sum(OutputsAmount, FeeAmount)
        )

    trait Expression[StateF[_] <: Ledger.StateI[_], +T] {
        final type StateI[StateT] = StateF[StateT]
        final type Event = Ledger.Event
        final type Error = Ledger.Error

        def description: String
        def evaluate[StateT: StateI](state: StateT, event: Event): Either[Error, T]

        final def apply[StateT: StateI](state: StateT, event: Event): Either[Error, T] =
            evaluate(state, event)
    }

    object Expression {
        object InputsAmount extends Expression[Ledger.StateI.Utxo, Long] {
            override def description: String = "InputsAmount"

            override def evaluate[StateT: StateI](
                state: StateT,
                event: Event
            ): Either[Error, Long] = boundary {
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

        object OutputsAmount extends Expression[Ledger.StateI, Long] {
            override def description: String = "OutputsAmount"

            override def evaluate[StateT: StateI](
                state: StateT,
                event: Event
            ): Either[Error, Long] = {
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

        object FeeAmount extends Expression[Ledger.StateI, Long] {
            override def description: String = "FeeAmount"

            override def evaluate[StateT: StateI](
                state: StateT,
                event: Event
            ): Either[Error, Long] = {
                Right(event.body.fee.value)
            }
        }

        class Sum[StateF[_] <: Ledger.StateI[_], T](
            val expressions: Expression[? >: StateF <: Ledger.StateI, T]*
        )(using
            num: Numeric[T]
        ) extends Expression[StateF, T] {
            override def description: String =
                s"Sum of ${expressions.view.map(_.description).mkString(", ")}"

            override def evaluate[StateT: StateI](state: StateT, event: Event): Either[Error, T] =
                boundary {
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
