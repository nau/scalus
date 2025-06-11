package scalus.bloxbean

import com.bloxbean.cardano.client.transaction.spec
import com.bloxbean.cardano.client.transaction.util.TransactionUtil.getTxHash
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, PlatformSpecific, given}
import scalus.cardano.ledger.*

import java.math.BigInteger

class TransactionTest extends AnyFunSuite {
    private val addr =
        "addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"

    private val crypto = summon[PlatformSpecific]

    test("CCL and Scalus transaction hash match") {
        def scalusTransactionHash = {
            val tx = Transaction(
              KeepRaw(
                TransactionBody(
                  inputs = Set(
                    TransactionInput(Hash(ByteString.fill(32, 0)), 0)
                  ),
                  outputs = Vector(
                    TransactionOutput.Shelley(
                      address = AddressBytes.fromBech32(addr),
                      value = Value.lovelace(2)
                    )
                  ),
                  fee = Coin(0)
                )
              ),
              witnessSet = TransactionWitnessSet(),
              isValid = true
            )
            //        println(tx)
            //        println(AddressBytes.fromBech32(addr).toHex)
            //        println(Cbor.encode(tx).toByteArray.toHex)
            val txbody = Cbor.encode(tx.body).toByteArray
            val txhash = crypto.blake2b_256(ByteString.fromArray(txbody))
            //        println(txhash)
            txhash.toHex
        }

        def cclTransactionHash = {
            import com.bloxbean.cardano.client.transaction.spec.*
            val tx = Transaction
                .builder()
                .body(
                  TransactionBody
                      .builder()
                      .inputs(
                        java.util.List.of(
                          TransactionInput
                              .builder()
                              .transactionId(ByteString.fill(32, 0).toHex)
                              .index(0)
                              .build()
                        )
                      )
                      .outputs(
                        java.util.List.of(
                          TransactionOutput
                              .builder()
                              .address(addr)
                              .value(Value.builder().coin(BigInteger.valueOf(2)).build())
                              .build()
                        )
                      )
                      .fee(BigInteger.valueOf(0))
                      .build()
                )
                .build()
            //        println(tx)
            //        println(tx.serialize().toHex)
            val txhash = getTxHash(tx)
            //        println(txhash)
            txhash
        }

        assert(scalusTransactionHash == cclTransactionHash)
    }
}
