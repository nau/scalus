package scalus.cardano.ledger
package rules

// It's part of Shelley.validateOutputBootAddrAttrsTooBig in cardano-ledger
object OutputBootAddrAttrsTooBigValidator extends STS.Validator {
    override final type Error = TransactionException

    override def validate(context: Context, state: State, event: Event): Result = {
        ???
    }
}
