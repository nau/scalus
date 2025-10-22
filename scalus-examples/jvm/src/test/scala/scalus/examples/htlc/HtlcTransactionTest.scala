package scalus.examples.htlc

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Builtins.sha3_256
import scalus.builtin.ByteString
import scalus.cardano.ledger.*
import scalus.cardano.txbuilder.{BuilderContext, Wallet}
import scalus.examples.TestUtil
import scalus.ledger.api.v1.PosixTime
import scalus.testkit.ScalusTest
import scalus.plutusV3

class HtlcTransactionTest extends AnyFunSuite, ScalusTest {

    private val env = TestUtil.testEnvironment

    private val committerAddress = TestUtil.createTestAddress("a" * 56)
    private val receiverAddress = TestUtil.createTestAddress("b" * 56)

    private val committerPkh = ByteString.fromArray(committerAddress.payment.asHash.bytes)
    private val receiverPkh = ByteString.fromArray(receiverAddress.payment.asHash.bytes)

    private val defaultLockAmount: Long = 10_000L
    private val defaultTimeout: PosixTime = 1_745_261_347_000L
    private val beforeTimeout: PosixTime = 1_745_261_346_000L
    private val afterTimeout: PosixTime = 1_745_261_348_000L

    private val validPreimage: ByteString = genByteStringOfN(32).sample.get
    private val validImage: ByteString = sha3_256(validPreimage)

    given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false
    )

    def lockHtlc(
        lockAmount: Long = defaultLockAmount,
        committer: ByteString = committerPkh,
        receiver: ByteString = receiverPkh,
        image: ByteString = validImage,
        timeout: PosixTime = defaultTimeout
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(committerAddress, lockAmount + 50_000_000L)
        val context = BuilderContext(env, wallet)
        val value = Value.lovelace(lockAmount)
        new Transactions(context)
            .lock(value, committer, receiver, image, timeout)
            .getOrElse(???)
    }

    def revealHtlc(
        htlcUtxo: (TransactionInput, TransactionOutput),
        preimage: ByteString
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(receiverAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(beforeTimeout.toLong)
        new Transactions(context)
            .reveal(htlcUtxo, preimage, receiverAddress, receiverPkh, validityStartSlot)
            .getOrElse(???)
    }

    def timeoutHtlc(
        htlcUtxo: (TransactionInput, TransactionOutput)
    ): Transaction = {
        val wallet = TestUtil.createTestWallet(committerAddress, 50_000_000L)
        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(afterTimeout.toLong)
        new Transactions(context)
            .timeout(htlcUtxo, committerAddress, committerPkh, validityStartSlot)
            .getOrElse(???)
    }

    def runValidator(tx: Transaction, utxo: Utxos, wallet: Wallet, scriptInput: TransactionInput) =
        TestUtil.runValidator(
          HtlcContract.defaultCompiledContract.program,
          tx,
          utxo,
          wallet,
          scriptInput
        )

    test("receiver reveals preimage before timeout") {
        val lockTx = lockHtlc()
        val htlcUtxo = TestUtil.getScriptUtxo(lockTx)

        val revealTx = revealHtlc(htlcUtxo, validPreimage)

        val receiverWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(receiverAddress, receiverWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo

        val scriptContext = TestUtil.getScriptContext(revealTx, utxos, htlcUtxo._1)
        val result =
            HtlcContract.defaultCompiledContract.program.runWithDebug(scriptContext)

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
        val lockTx = lockHtlc()
        val htlcUtxo = TestUtil.getScriptUtxo(lockTx)

        val wrongPreimage = genByteStringOfN(12).sample.get
        val revealTx = revealHtlc(htlcUtxo, wrongPreimage)

        val receiverWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(receiverAddress, receiverWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo

        val scriptContext = TestUtil.getScriptContext(revealTx, utxos, htlcUtxo._1)
        val result =
            HtlcContract.defaultCompiledContract.program.runWithDebug(scriptContext)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverPreimage))
    }

    test("receiver fails after timeout") {
        val lockTx = lockHtlc()
        val htlcUtxo = TestUtil.getScriptUtxo(lockTx)

        val receiverWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(receiverAddress, receiverWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo

        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(afterTimeout.toLong)

        val revealTx = new Transactions(context)
            .reveal(htlcUtxo, validPreimage, receiverAddress, receiverPkh, validityStartSlot)
            .getOrElse(???)

        val scriptContext = TestUtil.getScriptContext(revealTx, utxos, htlcUtxo._1)
        val result =
            HtlcContract.defaultCompiledContract.program.runWithDebug(scriptContext)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidReceiverTimePoint))
    }

    test("committer reclaims after timeout") {
        val lockTx = lockHtlc()
        val htlcUtxo = TestUtil.getScriptUtxo(lockTx)

        val timeoutTx = timeoutHtlc(htlcUtxo)

        val committerWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(committerAddress, committerWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo

        val scriptContext = TestUtil.getScriptContext(timeoutTx, utxos, htlcUtxo._1)
        val result =
            HtlcContract.defaultCompiledContract.program.runWithDebug(scriptContext)

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
        val lockTx = lockHtlc()
        val htlcUtxo = TestUtil.getScriptUtxo(lockTx)

        val committerWalletInput = 50_000_000L
        val wallet = TestUtil.createTestWallet(committerAddress, committerWalletInput)
        val utxos: Utxos = Map(htlcUtxo) ++ wallet.utxo

        val context = BuilderContext(env, wallet)
        val validityStartSlot =
            CardanoInfo.mainnet.slotConfig.timeToSlot(beforeTimeout.toLong)

        val timeoutTx = new Transactions(context)
            .timeout(htlcUtxo, committerAddress, committerPkh, validityStartSlot)
            .getOrElse(???)

        val scriptContext = TestUtil.getScriptContext(timeoutTx, utxos, htlcUtxo._1)
        val result =
            HtlcContract.defaultCompiledContract.program.runWithDebug(scriptContext)

        assert(result.isFailure)
        assert(result.logs.last.contains(HtlcValidator.InvalidCommitterTimePoint))
    }
}
