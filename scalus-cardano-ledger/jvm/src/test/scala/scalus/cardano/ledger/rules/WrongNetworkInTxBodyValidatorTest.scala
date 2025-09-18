package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network
import scalus.cardano.address.Network.{Mainnet, Testnet}

class WrongNetworkInTxBodyValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("WrongNetworkInTxBodyValidator rule success") {
        val transaction = randomValidTransaction.withNetwork(Testnet)
        val result = WrongNetworkInTxBodyValidator.validate(Context(), State(), transaction)
        assert(result.isRight, result)
    }

    test("WrongNetworkInTxBodyValidator rule failure") {
        val transaction = randomValidTransaction.withNetwork(Mainnet)
        val result = WrongNetworkInTxBodyValidator.validate(Context(), State(), transaction)
        assert(result.isLeft, result)
        assert(result.left.exists(_.isInstanceOf[TransactionException.WrongNetworkInTxBody]))
    }

}
