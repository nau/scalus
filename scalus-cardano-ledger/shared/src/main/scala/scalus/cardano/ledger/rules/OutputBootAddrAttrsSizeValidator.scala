package scalus.cardano.ledger.rules

import scalus.cardano.address.ByronAddress
import scalus.cardano.ledger.*

// validateOutputBootAddrAttrsTooBig in cardano-ledger
object OutputBootAddrAttrsSizeValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        val transaction = event
        val txBody = transaction.body.value
        val maxBootstrapAttrsSize = 64

        val outputsWithOversizedBootAttrs = txBody.outputs.filter { output =>
            output.value.address match {
                case byronAddr: ByronAddress => byronAddr.bytes.length > maxBootstrapAttrsSize
                case _                       => false
            }
        }

        if outputsWithOversizedBootAttrs.nonEmpty then {
            failure(
              TransactionException.IllegalArgumentException(
                s"Bootstrap address attributes too big for transaction ${transaction.id}: " +
                    s"outputs with oversized attrs: ${outputsWithOversizedBootAttrs.map(_.value.address).toList}, " +
                    s"max allowed size: $maxBootstrapAttrsSize"
              )
            )
        } else {
            success
        }
    }
}
