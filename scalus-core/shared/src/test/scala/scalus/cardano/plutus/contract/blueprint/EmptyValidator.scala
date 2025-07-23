package scalus.cardano.plutus.contract.blueprint
import scalus.prelude.{Option, Validator}
import scalus.builtin.Data
import scalus.ledger.api.v3.{TxInfo, TxOutRef}
import scalus.Compile

@Compile
object EmptyValidator extends Validator:
    override def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = ()
