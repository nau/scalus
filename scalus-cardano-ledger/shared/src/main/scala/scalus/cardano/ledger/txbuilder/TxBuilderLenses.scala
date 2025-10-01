package scalus.cardano.ledger.txbuilder

import monocle.Lens
import scalus.cardano.ledger.{DatumOption, KeepRaw, Transaction, TransactionBody, TransactionOutput}
import scalus.cardano.ledger.TransactionOutput.Babbage

object TxBuilderLenses {

    val txBodyL: Lens[Transaction, TransactionBody] =
        Lens[Transaction, TransactionBody](_.body.value) { newBody => tx =>
            tx.copy(body = KeepRaw(newBody))
        }
}

/** Extension methods for TransactionOutput to access datumOption */
extension (self: TransactionOutput)
    def datumOption: Option[DatumOption] =
        self match {
            case TransactionOutput.Shelley(_, _, datumHash) =>
                datumHash.map(DatumOption.Hash(_))
            case Babbage(_, _, datumOption, _) =>
                datumOption match {
                    case Some(value) => Some(value)
                    case None        => None
                }
        }
