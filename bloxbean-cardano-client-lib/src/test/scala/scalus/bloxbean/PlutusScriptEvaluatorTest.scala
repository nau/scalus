package scalus.bloxbean

import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.bloxbean.BlocksValidation.apiKey
import scalus.builtin.ByteString.*
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.examples.PubKeyValidator
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.*
import scalus.uplc.eval.ExBudget
import upickle.default.read

import java.nio.file.{Files, Path, Paths}

class PlutusScriptEvaluatorTest extends AnyFunSuite {
    private val params: ProtocolParams = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)
    private val costModels = CostModels.fromProtocolParams(params)

    test("TxEvaluator PlutusV2") {
        val evaluator = PlutusScriptEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costModels = costModels
        )

        inline def requiredPubKeyHash =
            hex"e1ac75d278929abc5e113cd1cd611a35af2520e7b3056ecac3da186b"

        val pubKeyValidator =
            compile(PubKeyValidator.validatorV2(requiredPubKeyHash))
                .toUplc()
                .plutusV2
        val s = Script.PlutusV2(ByteString.unsafeFromArray(pubKeyValidator.cborEncoded))
        val input = TransactionInput(Hash(platform.blake2b_256(ByteString.fromString("asdf"))), 0)
        val datum = Data.unit
        val dataHash: DataHash = Hash(platform.blake2b_256(datum.toCborByteString))
        val addr =
            "addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"
        val utxo = Map(
          input -> TransactionOutput(
            address = Address.Shelley(
              ShelleyAddress(
                Network.Mainnet,
                payment = ShelleyPaymentPart.Script(s.scriptHash),
                delegation = Null
              ),
            ),
            datumOption = Some(DatumOption.Hash(dataHash)),
            value = Value.lovelace(2)
          )
        )
        val redeemer = Redeemer(RedeemerTag.Spend, 0, Data.unit, ExUnits(0, 0))
        val tx = Transaction(
          TransactionBody(
            inputs = Set(input),
            outputs = Vector(
              Sized(
                TransactionOutput(
                  address = Address.fromBech32(addr),
                  value = Value.lovelace(2)
                )
              )
            ),
            fee = Coin(0),
            requiredSigners = Set(Hash(requiredPubKeyHash)),
          ),
          witnessSet = TransactionWitnessSet(
            redeemers = Some(KeepRaw(Redeemers(redeemer))),
            plutusV2Scripts = Set(s),
            plutusData = KeepRaw(TaggedSet(KeepRaw(datum))),
          ),
        )
        val redeemers = evaluator.evalPhaseTwo(tx, utxo)
        assert(redeemers.size == 1)
        val redeemerResult = redeemers.head
        assert(redeemerResult.exUnits.memory == 13375L)
        assert(redeemerResult.exUnits.steps == 3732764L)
    }

    test("evaluate block 11544748") {
        pending
        validateBlock(11544748)
    }

    test("evaluate block 11544518") {
        pending
        validateBlock(11544518)
    }

    test("evaluate block 11553070") {
        pending
        validateBlock(11553070)
    }

    private def validateBlock(num: Long): Unit = {
        val bytes = getClass.getResourceAsStream(s"/blocks/block-$num.cbor").readAllBytes()
        given OriginalCborByteArray = OriginalCborByteArray(bytes)
        val block = readBlock(bytes).block
        validateTransactions(block.transactions) // skip first 24 txs
    }

    private def validateTransactions(txs: Seq[Transaction]): Unit = {
        for tx <- txs do {
            if tx.witnessSet.redeemers.nonEmpty && tx.isValid then {
                validateTransaction(tx)
            }
        }
    }

    private def validateTransaction(tx: Transaction): Unit = {
        val cwd = Paths.get(".")
        val backendService = new BFBackendService(Constants.BLOCKFROST_MAINNET_URL, apiKey)
        val utxoSupplier = CachedUtxoSupplier(
          cwd.resolve("utxos"),
          DefaultUtxoSupplier(backendService.getUtxoService)
        )
        // memory and file cached script supplier using the script service
        val scriptSupplier = InMemoryCachedScriptSupplier(
          FileScriptSupplier(
            cwd.resolve("scripts"),
            ScriptServiceSupplier(backendService.getScriptService)
          )
        )
        val utxoResolver = ScalusUtxoResolver(utxoSupplier, scriptSupplier)
        val utxos = utxoResolver.resolveUtxos(tx)
        val evaluator = PlutusScriptEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costModels = costModels,
          debugDumpFilesForTesting = false
        )
        //        dumpTxInfo(tx, utxos)

        val redeemers = evaluator.evalPhaseTwo(tx, utxos)
        for (actual, expected) <- redeemers.zip(tx.witnessSet.redeemers.get.value.toIndexedSeq) do
            assert(actual.exUnits.memory <= expected.exUnits.memory)
            assert(actual.exUnits.steps <= expected.exUnits.steps, actual)
    }

    private def dumpTxInfo(
        tx: Transaction,
        utxos: Map[TransactionInput, TransactionOutput]
    ): Unit = {
        val txhash = tx.id.toHex
        Files.write(Paths.get(s"tx-$txhash.cbor"), Cbor.encode(tx).toByteArray)
        Files.deleteIfExists(Paths.get("scalus.log"))
        storeInsOutsInCborFiles(utxos, txhash)
    }

    private def storeInsOutsInCborFiles(
        utxos: Map[TransactionInput, TransactionOutput],
        txhash: String
    ): Unit = {
        val ins = Cbor.encode(utxos.keys.toIndexedSeq).toByteArray
        val outs = Cbor.encode(utxos.values.toIndexedSeq).toByteArray
        Files.write(Path.of(s"ins-$txhash.cbor"), ins)
        Files.write(Path.of(s"outs-$txhash.cbor"), outs)
    }

    private def readBlock(blockBytes: Array[Byte]): BlockFile = {
        BlockFile.fromCborArray(blockBytes)
    }
}
