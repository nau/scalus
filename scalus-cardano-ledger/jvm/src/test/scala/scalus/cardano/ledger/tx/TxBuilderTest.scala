package scalus.cardano.ledger.tx
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, ArbitraryInstances as ArbAddresses, Network}
import scalus.cardano.ledger.{ArbitraryInstances as ArbLedger, Coin, TransactionHash, TransactionInput, TransactionOutput, UTxO, Value}
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

class TxBuilderTest extends AnyFunSuite with ArbAddresses with ArbLedger {

    test("should balance the transaction when the inputs exceed the outputs") {
        val params: ProtocolParams = read[ProtocolParams](
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )(using ProtocolParams.blockfrostParamsRW)
        val myAddress = arbitrary[Address].sample.get
        val faucet = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        // Input tx produced 1_000_000 lovelace
        val availableLovelace = Value.lovelace(1_000_000L)
        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(
            myAddress,
            availableLovelace
          )
        )

        val paymentAmount = Value.lovelace(500L)
        val tx = TxBuilder(params, Network.Testnet)
            .payTo(faucet)
            .amount(paymentAmount)
            .using(utxo)
            .prepareTx
            .right
            .get
            .balanceAndCalculateFees(myAddress)
        assert(tx.body.value.outputs.size == 2)
        assert(tx.body.value.outputs.exists(_.value.address == myAddress))
        assert(tx.body.value.outputs.exists(_.value.address == faucet))

        val sumOutputs = tx.body.value.outputs
            .map(_.value.value.coin)
            .foldLeft(Coin.zero)(_ + _)
        val fee = tx.body.value.fee
        assert(Value(sumOutputs + fee) == availableLovelace)

    }
    test("should throw when trying to create a transaction where outputs exceed the inputs") {
        val params: ProtocolParams = read[ProtocolParams](
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )(using ProtocolParams.blockfrostParamsRW)
        val myAddress = arbitrary[Address].sample.get
        val faucet = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        // Input tx produced 1K lovelace
        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(1_000L)
          )
        )

        // exceeds available lovelace
        val paymentAmount = 10_000L
        val txBuilder = TxBuilder(params, Network.Testnet)
            .payTo(faucet)
            .amount(Value.lovelace(paymentAmount))
            .using(utxo)
            .prepareTx
            .right
            .get

        assertThrows[IllegalStateException] {
            txBuilder.balanceAndCalculateFees(myAddress)
        }
    }
}
