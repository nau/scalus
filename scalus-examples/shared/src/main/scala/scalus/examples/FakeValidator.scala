package scalus.examples
import scalus.builtin.Data
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.{prelude, Compile}
import scalus.prelude.Validator

@Compile
object FakeValidator extends Validator {

    override def spend(
        datum: prelude.Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = ()
}
