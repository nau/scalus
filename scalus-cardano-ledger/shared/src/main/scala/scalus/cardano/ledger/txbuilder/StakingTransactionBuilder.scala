package scalus.cardano.ledger.txbuilder

import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import scalus.cardano.address.{Address, StakeAddress}
import scalus.cardano.ledger.{AddrKeyHash, Coin, PoolMetadata, Relay, RewardAccount, Transaction, UnitInterval, VrfKeyHash}
import com.bloxbean.cardano.client.transaction.spec.cert.PoolRegistration
import scalus.builtin.ByteString
import scalus.cardano
import scalus.cardano.ledger

import java.math.BigInteger
import java.net.{Inet4Address, Inet6Address}
import scala.jdk.CollectionConverters.*

case class StakingTransactionBuilder(
    context: BuilderContext,
    var baseAddress: Address,
    var stakeAddress: StakeAddress
) {

    def registerStakeAddress: Transaction = {
        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .registerStakeAddress(baseAddress.encode.get)
                  .from(baseAddress.encode.get)
            )
            .validFrom(0)
            .withSigner((c, t) => t)
            .build()

        Transaction.fromCbor(cclTx.serialize())
    }

    def delegateToPool(poolId: String): Transaction = {
        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .delegateTo(baseAddress.encode.get, poolId)
                  .from(baseAddress.encode.get)
            )
            .validFrom(0)
            .withSigner((c, t) => t)
            .build()

        Transaction.fromCbor(cclTx.serialize())
    }

    def withdraw(amount: Long): Transaction = {
        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .withdraw(stakeAddress.encode.get, BigInteger.valueOf(amount))
                  .from(baseAddress.encode.get)
            )
            .validFrom(0)
            .withSigner((c, t) => t)
            .build()

        Transaction.fromCbor(cclTx.serialize())
    }

    def registerPool(
        operator: AddrKeyHash,
        vrfKeyHash: VrfKeyHash,
        pledge: Coin,
        cost: Coin,
        margin: UnitInterval,
        rewardAccount: RewardAccount,
        poolOwners: Set[AddrKeyHash] = Set.empty,
        relays: IndexedSeq[Relay] = IndexedSeq.empty
    ): Transaction = {
        val cclInterval = new com.bloxbean.cardano.client.spec.UnitInterval(
          BigInteger.valueOf(margin.numerator),
          BigInteger.valueOf(margin.denominator)
        )

        val cclPoolOwners = poolOwners.map(_.toHex).asJava
        val cclRelays =
            new java.util.ArrayList[com.bloxbean.cardano.client.transaction.spec.cert.Relay]()

        val cclTx = QuickTxBuilder(context.backendService)
            .compose(
              new Tx()
                  .registerPool(
                    new PoolRegistration(
                      operator.bytes,
                      vrfKeyHash.bytes,
                      BigInteger.valueOf(pledge.value),
                      BigInteger.valueOf(cost.value),
                      cclInterval,
                      rewardAccount.address.encode.get,
                      cclPoolOwners,
                      cclRelays,
                      "",
                      ""
                    )
                  )
                  .from(baseAddress.encode.get)
            )
            .validFrom(0)
            .withSigner((c, t) => t)
            .build()

        Transaction.fromCbor(cclTx.serialize())
    }

}
