package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.{Network, ShelleyAddress}

class WrongNetworkValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {

    test("WrongNetworkValidator rule success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  networkId = Some(Testnet.value),
                  outputs = tx.body.value.outputs.map(x =>
                      Sized(
                        TransactionOutput(
                          Arbitrary.arbitrary[ShelleyAddress].sample.get.copy(network = Testnet),
                          x.value.value
                        )
                      )
                  ),
                  withdrawals = tx.body.value.withdrawals.map(w =>
                      w.copy(withdrawals =
                          w.withdrawals.map((k, v) =>
                              (k.copy(address = k.address.copy(network = Testnet)), v)
                          )
                      )
                  )
                )
              )
            )
        }

        val result = for
            _ <- WrongNetworkValidator.validate(context, state, transaction)
            _ <- WrongNetworkWithdrawalValidator.validate(context, state, transaction)
            _ <- WrongNetworkInTxBodyValidator.validate(context, state, transaction)
        yield ()
        println(result)
        assert(result.isRight)
    }

    test("WrongNetworkValidator rule failure 1") {
        val context = Context(env = UtxoEnv.default.copy(network = Mainnet))
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(body = KeepRaw(tx.body.value.copy(networkId = Some(Testnet.value))))
        }
        val result = for
            _ <- WrongNetworkValidator.validate(context, state, transaction)
            _ <- WrongNetworkWithdrawalValidator.validate(context, state, transaction)
            _ <- WrongNetworkInTxBodyValidator.validate(context, state, transaction)
        yield ()
        println(result)
        assert(result.isLeft)
    }

    test("WrongNetworkValidator rule failure 2") {
        val context = Context(env = UtxoEnv.default.copy(network = Mainnet))
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(outputs =
                    tx.body.value.outputs.map(x =>
                        Sized(
                          TransactionOutput(
                            Arbitrary
                                .arbitrary[ShelleyAddress]
                                .sample
                                .get
                                .copy(network = Testnet),
                            x.value.value
                          )
                        )
                    )
                )
              )
            )
        }
        val result = for
            _ <- WrongNetworkValidator.validate(context, state, transaction)
            _ <- WrongNetworkWithdrawalValidator.validate(context, state, transaction)
            _ <- WrongNetworkInTxBodyValidator.validate(context, state, transaction)
        yield ()
        println(result)
        assert(result.isLeft)
    }

    test("WrongNetworkValidator rule failure 3") {
        val context = Context(env = UtxoEnv.default.copy(network = Mainnet))
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(withdrawals =
                    tx.body.value.withdrawals.map(w =>
                        w.copy(withdrawals =
                            w.withdrawals.map((k, v) =>
                                (k.copy(address = k.address.copy(network = Testnet)), v)
                            )
                        )
                    )
                )
              )
            )
        }
        val result = for
            _ <- WrongNetworkValidator.validate(context, state, transaction)
            _ <- WrongNetworkWithdrawalValidator.validate(context, state, transaction)
            _ <- WrongNetworkInTxBodyValidator.validate(context, state, transaction)
        yield ()
        println(result)
        assert(result.isLeft)
    }
}
