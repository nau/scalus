package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{BuilderContext, Wallet}
import scalus.examples.TestUtil
import scalus.ledger.api.v1.PosixTime
import scalus.testkit.ScalusTest

class HtlcTransactionTest extends AnyFunSuite, ScalusTest {

    private val env = TestUtil.testEnvironment
    private val compiledContract = HtlcContract.debugCompiledContract

    private val committerAddress = TestUtil.createTestAddress("a" * 56)
    private val receiverAddress = TestUtil.createTestAddress("b" * 56)

    private val committerPkh = ByteString.fromArray(committerAddress.payment.asHash.bytes)
    private val receiverPkh = ByteString.fromArray(receiverAddress.payment.asHash.bytes)
    private val wrongCommitterPkh =
        ByteString.fromArray(TestUtil.createTestAddress("c" * 56).payment.asHash.bytes)
    private val wrongReceiverPkh =
        ByteString.fromArray(TestUtil.createTestAddress("d" * 56).payment.asHash.bytes)

    private val defaultLockAmount: Long = 10_000L

    private val defaultTimeout: PosixTime = 1_745_261_347_000L
    private val beforeTimeout: PosixTime = 1_745_261_346_000L
    private val afterTimeout: PosixTime = 1_745_261_348_000L

    private val validPreimage: ByteString = genByteStringOfN(32).sample.get
    private val wrongPreimage = genByteStringOfN(12).sample.get
    private val validImage: ByteString = sha3_256(validPreimage)

    private val lockHtlc: Transaction = {
        val wallet = TestUtil.createTestWallet(committerAddress, defaultLockAmount + 50_000_000L)
        val context = BuilderContext(env, wallet)
        val value = Value.lovelace(defaultLockAmount)
        new Transactions(context, compiledContract)
            .lock(value, committerPkh, receiverPkh, validImage, defaultTimeout)
            .getOrElse(???)
    }

    private val htlcUtxo = TestUtil
        .findUtxoByAddress(
          lockHtlc,
          Address(env.network, Credential.ScriptHash(compiledContract.script.scriptHash))
        )
        .getOrElse(???)

    private def revealHtlc(
        preimage: ByteString,
        receiverPkh: ByteString,
        time: PosixTime
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(receiverAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(time.toLong)
        new Transactions(context, compiledContract)
            .reveal(htlcUtxo, preimage, receiverAddress, receiverPkh, validityStartSlot)
            .getOrElse(???)
    }

    private def timeoutHtlc(
        committerPkh: ByteString,
        time: PosixTime
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(committerAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(time.toLong)
        new Transactions(context, compiledContract)
            .timeout(htlcUtxo, committerAddress, committerPkh, validityStartSlot)
            .getOrElse(???)
    }

    def runValidator(tx: Transaction, utxo: Utxos, wallet: Wallet, scriptInput: TransactionInput) =
        TestUtil.runValidator(
          compiledContract.program,
          tx,
          utxo,
          wallet,
          scriptInput,
          RedeemerTag.Spend,
          env
        )

    test("receiver reveals preimage before timeout") {
        val revealTx = revealHtlc(validPreimage, receiverPkh, beforeTimeout)
        val receiverWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(receiverAddress, receiverWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(revealTx, utxos, wallet, htlcUtxo._1)

        assert(result.isSuccess)

        val scriptOutputs = revealTx.body.value.outputs.filter(_.value.address.hasScript)
        assert(scriptOutputs.isEmpty, "no script outputs after revealing")

        val receiverOutputs = revealTx.body.value.outputs.filter { output =>
            output.value.address == receiverAddress
        }
        assert(receiverOutputs.nonEmpty, "reveal should send funds to receiver")

        // verify that the locked funds were successfully withdrawn, and the sum of the outputs exceeds the starting wallet funds.
        val totalReceiverOutput = receiverOutputs.map(_.value.value.coin.value).sum
        assert(totalReceiverOutput > receiverWalletInput)
    }

    test("receiver fails with wrong preimage") {
        val revealTx = revealHtlc(wrongPreimage, receiverPkh, beforeTimeout)
        val receiverWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(receiverAddress, receiverWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(revealTx, utxos, wallet, htlcUtxo._1)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverPreimage))
    }

    test("receiver fails with wrong receiver pubkey hash") {
        val revealTx = revealHtlc(validPreimage, wrongReceiverPkh, beforeTimeout)
        val receiverWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(receiverAddress, receiverWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(revealTx, utxos, wallet, htlcUtxo._1)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.UnsignedReceiverTransaction))
    }

    test("receiver fails after timeout") {
        val revealTx = revealHtlc(validPreimage, receiverPkh, afterTimeout)
        val receiverWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(receiverAddress, receiverWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(revealTx, utxos, wallet, htlcUtxo._1)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverTimePoint))
    }

    test("committer reclaims after timeout") {
        val timeoutTx = timeoutHtlc(committerPkh, afterTimeout)
        val committerWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(committerAddress, committerWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(timeoutTx, utxos, wallet, htlcUtxo._1)

        assert(result.isSuccess)

        val scriptOutputs = timeoutTx.body.value.outputs.filter(_.value.address.hasScript)
        assert(scriptOutputs.isEmpty, "no script outputs after timeout")

        val committerOutputs = timeoutTx.body.value.outputs.filter { output =>
            output.value.address == committerAddress
        }
        assert(committerOutputs.nonEmpty)

        val totalCommitterOutput = committerOutputs.map(_.value.value.coin.value).sum
        assert(totalCommitterOutput > committerWalletInput)
    }

    test("committer fails before timeout") {
        val timeoutTx = timeoutHtlc(committerPkh, beforeTimeout)
        val committerWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(committerAddress, committerWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(timeoutTx, utxos, wallet, htlcUtxo._1)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidCommitterTimePoint))
    }

    test("committer fails with wrong committer pubkey hash") {
        val timeoutTx = timeoutHtlc(wrongCommitterPkh, afterTimeout)
        val committerWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(committerAddress, committerWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo
        val result = runValidator(timeoutTx, utxos, wallet, htlcUtxo._1)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.UnsignedCommitterTransaction))
    }
}
