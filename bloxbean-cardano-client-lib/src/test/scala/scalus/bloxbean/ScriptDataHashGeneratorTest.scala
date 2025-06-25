package scalus.bloxbean

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.common.model.Network
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.CostMdls
import com.bloxbean.cardano.client.quicktx.{QuickTxBuilder, Tx}
import com.bloxbean.cardano.client.spec
import org.scalatest.funsuite.AnyFunSuite
import scalus.bloxbean.Interop.toPlutusData
import scalus.builtin.Data
import scalus.cardano.ledger
import scalus.cardano.ledger.*
import scalus.utils.Hex.toHex

import java.util

class ScriptDataHashGeneratorTest extends AnyFunSuite {
    test("ScriptDataHashGenerator should generate correct hash for empty inputs") {
        val era = ledger.Era.Conway
        val redeemers = Seq.empty[Redeemer]
        val datums = KeepRaw(TaggedSet.empty[Data])
        val costModels = CostModels(Map.empty)
        val hash = ScriptDataHashGenerator.generate(era, redeemers, datums, costModels)

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

/*
    test("Make transaction") {
        val backendService = new BFBackendService("http://localhost:8080/api/v1/", "")
        val network = new Network(0, 42)
        val mnemonic =
            "test test test test test test test test test test test test test test test test test test test test test test test sauce"
        val sender = new Account(network, mnemonic)
        val tx = new Tx()
            .from(sender.getBaseAddress.getAddress)
            .payToAddress(sender.baseAddress(), Amount.ada(10))

        val quickTxBuilder = QuickTxBuilder(backendService)
        val signedTx = quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.signerFrom(sender))
            .buildAndSign()

        println(signedTx)

        backendService.getTransactionService.submitTransaction(signedTx.serialize)
    }
*/

    // Additional tests can be added here to cover more scenarios
}
