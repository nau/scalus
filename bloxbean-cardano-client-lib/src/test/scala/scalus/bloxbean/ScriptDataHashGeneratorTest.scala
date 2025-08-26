package scalus.bloxbean

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.utils.Hex.toHex

import java.math.BigInteger
import java.util

class ScriptDataHashGeneratorTest extends AnyFunSuite {
    test("ScriptDataHashGenerator should generate correct hash for empty inputs") {
        import com.bloxbean.cardano.client.plutus.spec.CostMdls
        import com.bloxbean.cardano.client.spec
        import scalus.cardano.ledger
        import scalus.cardano.ledger.*

        val era = ledger.Era.Conway
        val datums = KeepRaw(TaggedSet.empty[KeepRaw[Data]])
        val costModels = CostModels(Map.empty)
        val hash = ScriptDataHashGenerator.computeScriptDataHash(era, None, datums, costModels)

        import com.bloxbean.cardano.client.plutus.util.ScriptDataHashGenerator.generate
        val costMdls = new CostMdls()
        val bbgenerated = generate(
          spec.Era.Conway,
          util.List.of(),
          util.List.of(),
          costMdls
        )
        assert(hash.toHex == bbgenerated.toHex)
        assert(hash.toHex == "9eb0251b2e85b082c3706a3e79b4cf2a2e96f936e912a398591e2486c757f8c1")
    }

    private def makeBBTx(): String = {
        import com.bloxbean.cardano.client.account.Account
        import com.bloxbean.cardano.client.api.UtxoSupplier
        import com.bloxbean.cardano.client.api.model.Utxo
        import com.bloxbean.cardano.client.common.ADAConversionUtil.adaToLovelace
        import com.bloxbean.cardano.client.common.CardanoConstants.LOVELACE
        import com.bloxbean.cardano.client.common.model.Network
        import com.bloxbean.cardano.client.function.Output
        import com.bloxbean.cardano.client.function.helper.InputBuilders
        import com.bloxbean.cardano.client.transaction
        import com.bloxbean.cardano.client.transaction.spec.{Transaction, TransactionBody, TransactionOutput, Value}

        //            .txHash("f3b8c0d1e2f3b4c5d6e7f8a9b0c1d2e3f4b5c6d7e8f9a0b1c2d3e4f5g6h7i8j")
        val network = new Network(0, 42)
        val mnemonic =
            "test test test test test test test test test test test test test test test test test test test test test test test sauce"
        val sender = new Account(network, mnemonic)
        val myaddr =
            "addr_test1qzwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clq68fray"
        val output = Output
            .builder()
            .address(myaddr)
            .assetName(LOVELACE)
            .qty(adaToLovelace(10))
            .build()

        val txBuilder = output
            .outputBuilder()
            .buildInputs(InputBuilders.createFromUtxos(util.List.of(new Utxo()), myaddr))

        val utxoSupplier: UtxoSupplier = null
        //        val protocolParamsSupplier: ProtocolParamsSupplier = () => null
        //        val signedTransaction = TxBuilderContext
        //            .init(utxoSupplier, protocolParamsSupplier)
        //            .buildAndSign(txBuilder, signerFrom(sender))

        val body = TransactionBody
            .builder()
            .inputs(util.List.of())
            .outputs(
              util.List
                  .of(
                    TransactionOutput
                        .builder()
                        .address(myaddr)
                        .value(Value.builder.coin(BigInteger.valueOf(1234234)).build())
                        .build()
                  )
            )
            .fee(adaToLovelace(2))
            .ttl(1000)
            .build()
        val transaction = new Transaction
        transaction.setEra(com.bloxbean.cardano.client.spec.Era.Conway)
        transaction.setBody(body)
        transaction.setValid(true)
        transaction.serialize.toHex
    }

    private def makeScalusTx(): String = {
        import scalus.cardano.address.Address
        import scalus.cardano.ledger.*

        // Create transaction components
        val myaddr =
            "addr_test1qzwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clq68fray"
        val address = Address.fromBech32(myaddr)

        // Create empty input set
        val inputs = Set.empty[TransactionInput]

        // Create an output with lovelace (ADA)
        val output = TransactionOutput.Shelley(
          address = address,
          value = Value(Coin(1234234))
        )

        // Create transaction body
        val txBody = TransactionBody(
          inputs = TaggedOrderedSet.from(inputs),
          outputs = IndexedSeq(Sized(output)),
          fee = Coin(2_000000), // 2 ADA in lovelace
          ttl = Some(1000)
        )

        // Wrap body in KeepRaw to preserve CBOR encoding
        val wrappedBody = KeepRaw(txBody)

        // Create the transaction
        val transaction = Transaction(
          body = wrappedBody,
          witnessSet = TransactionWitnessSet(),
          isValid = true
        )

        // Serialize to CBOR and convert to hex
        Cbor.encode(transaction).toByteArray.toHex
    }

    test("Make transaction") {
        assert(makeBBTx() == makeScalusTx())
    }
}
