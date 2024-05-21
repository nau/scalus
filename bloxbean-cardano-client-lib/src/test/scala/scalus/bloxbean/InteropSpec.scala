package scalus.bloxbean

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.AddressProvider
import com.bloxbean.cardano.client.api.model.Amount
import com.bloxbean.cardano.client.api.model.Utxo
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.backend.api.DefaultProtocolParamsSupplier
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.coinselection.impl.DefaultUtxoSelector
import com.bloxbean.cardano.client.common.ADAConversionUtil
import com.bloxbean.cardano.client.common.CardanoConstants
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.function.helper.SignerProviders
import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.quicktx.QuickTxBuilder
import com.bloxbean.cardano.client.quicktx.ScriptTx
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.transaction.util.TransactionUtil
import com.bloxbean.cardano.yaci.core.model.Era
import com.bloxbean.cardano.yaci.core.model.serializers.TransactionBodyExtractor
import com.bloxbean.cardano.yaci.core.util.CborSerializationUtil
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.examples.MintingPolicyV2
import scalus.examples.PubKeyValidator
import scalus.prelude.AssocMap
import scalus.uplc.*
import scalus.uplc.TermDSL.*
import scalus.uplc.eval.ExBudget
import scalus.utils.Utils

import java.math.BigInteger
import java.util
import scala.collection.mutable

class InteropSpec extends AnyFunSuite:
    test("toScalusData <=> toPlutusData ") {
        // TODO: write isomorphism test
        // refactor to use ArbitraryInstances
    }
