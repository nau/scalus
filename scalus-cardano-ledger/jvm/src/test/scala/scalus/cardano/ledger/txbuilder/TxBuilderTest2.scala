package scalus.cardano.ledger.txbuilder

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{ArbitraryInstances as ArbAddresses, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.Script.PlutusV3
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.utils.TxBalance
import scalus.cardano.ledger.{AddrKeyHash, ArbitraryInstances as ArbLedger, CertState, CostModels, PlutusScriptEvaluator, Script, SlotConfig, TransactionInput, TransactionOutput, Value}
import scalus.ledger.api.MajorProtocolVersion
import scalus.cardano.ledger.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read

class TxBuilderTest2
    extends AnyFunSuite
    with ArbAddresses
    with ArbLedger
    with ValidatorRulesTestKit {

    val params: ProtocolParams = ProtocolParams.fromBlockfrostJson(
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )

    private val costModels = CostModels.fromProtocolParams(params)
    private val evaluator = PlutusScriptEvaluator(
      SlotConfig.Mainnet,
      initialBudget = ExBudget.enormous,
      protocolMajorVersion = MajorProtocolVersion.plominPV,
      costModels = costModels
    )
    private val env = Environment(params, evaluator, Network.Mainnet)
    private val alwaysOkValidator = ByteString.fromArray(Array(69, 1, 1, 0, 36, -103))

    test("pay") {
        val utxoToSpend = ResolvedTxInput.Script(
          makeScriptUtxo(20_000_000L),
          PlutusV3(alwaysOkValidator),
          Data.unit
        )
        val wallet = Wallet.create(
          paymentInputs = Set(
            utxoToSpend
          ),
          collat = Set(ResolvedTxInput.Pubkey(makeUtxo(5_000_00L)))
        )

        val targetAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(arbitrary[AddrKeyHash].sample.get),
          Null
        )

        val changeAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(arbitrary[AddrKeyHash].sample.get),
          Null
        )

        val paymentAmount = Value.lovelace(1_000_000L)
        val intention = Intention.Pay(targetAddress, paymentAmount)
        val tx = InterpreterWithProvidedData(
          wallet,
          env,
          ChangeReturnStrategy.toAddress(changeAddress),
          FeePayerStrategy.subtractFromAddress(utxoToSpend.output.address),
          evaluator
        ).realize(intention)

        assert(tx.body.value.inputs.toSeq.nonEmpty, "Transaction should have inputs")
        assert(tx.body.value.outputs.nonEmpty, "Transaction should have outputs")
        assert(
          tx.body.value.fee > scalus.cardano.ledger.Coin.zero,
          "Transaction should have non-zero fee"
        )

        val consumed =
            TxBalance.consumed(tx, CertState.empty, wallet.utxo, params).toTry.get
        val produced = TxBalance.produced(tx)
        assert(
          consumed == produced,
          s"Transaction should be balanced: consumed=$consumed, produced=$produced"
        )

        val paymentOutput = tx.body.value.outputs.find(_.value.address == targetAddress)
        assert(paymentOutput.isDefined, "Payment output should exist")
        assert(
          paymentOutput.get.value.value == paymentAmount,
          "Payment output should have correct amount"
        )
    }

    private def makeScriptUtxo(amount: Long): (TransactionInput, TransactionOutput) =
        makeScriptUtxo(amount, PlutusV3(alwaysOkValidator))

    private def makeScriptUtxo(
        amount: Long,
        script: PlutusV3
    ): (TransactionInput, TransactionOutput) = {
        val in = arbitrary[TransactionInput].sample.get
        val outAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Script(script.scriptHash),
          Null
        )
        val out = TransactionOutput(outAddress, Value.lovelace(amount))
        in -> out
    }

    private def makeUtxo(amount: Long): (TransactionInput, TransactionOutput) = {
        val in = arbitrary[TransactionInput].sample.get
        val outAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(arbitrary[AddrKeyHash].sample.get),
          Null
        )
        val out = TransactionOutput(outAddress, Value.lovelace(amount))
        in -> out
    }

//    test("validate against ledger rules") {
//        val keyPair@(privateKey, publicKey) = generateKeyPair()
//
//        val keyHash = AddrKeyHash(platform.blake2b_224(publicKey))
//        val myAddress = ShelleyAddress(
//            Network.Testnet,
//            ShelleyPaymentPart.Key(keyHash),
//            ShelleyDelegationPart.Null
//        )
//
//        val targetAddress = arbitrary[ShelleyAddress].sample.get
//        val hash = arbitrary[TransactionHash].sample.get
//
//        val scriptBytes = ByteString.unsafeFromArray(Array(69, 3, 1, 0, 36, -103).map(_.toByte))
//        val mintingScript = Script.PlutusV3(scriptBytes)
//        val policyId = mintingScript.scriptHash
//
//        val inputScriptBytes =
//            ByteString.unsafeFromArray(Array(69, 1, 1, 0, 36, -103).map(_.toByte))
//        val inputScript = Script.PlutusV3(inputScriptBytes)
//        val inputScriptAddress = ShelleyAddress(
//            Network.Mainnet,
//            ShelleyPaymentPart.Script(inputScript.scriptHash),
//            ShelleyDelegationPart.Null
//        )
//
//        val assetName = AssetName(ByteString.fromString("co2"))
//        val mintAmount = 1000L
//        val mintValue = Mint(MultiAsset(SortedMap(policyId -> SortedMap(assetName -> mintAmount))))
//
//        val availableLovelace = Value.lovelace(100_000_000L)
//        val txInputs = Map(
//            TransactionInput(hash, 0) -> TransactionOutput(inputScriptAddress, availableLovelace)
//        )
//        val collateral = Map(
//            TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
//                myAddress,
//                Value.lovelace(100_000_000L)
//            )
//        )
//        val utxo: UTxO = txInputs ++ collateral
//
//        val wallet = Wallet.create(
//            Set(ResolvedTxInput.Script(txInputs.head, inputScript, Data.unit)),
//            Set(ResolvedTxInput.Pubkey(collateral.head))
//        )
//
//        val interpreter = InterpreterWithProvidedData(
//            wallet,
//            env,
//            ChangeReturnStrategy.toAddress(myAddress),
//            FeePayerStrategy.subtractFromAddress(myAddress),
//            evaluator
//        )
//        val unsignedTx = interpreter.realize(
//            Intention.Mint(
//                mintValue,
//                MintIntention.UsingPlutus(mintingScript, Data.unit),
//                targetAddress
//            )
//        )
//        val signed = TxSigner
//            .usingKeyPairs(keyPair.swap)
//            .signTx(unsignedTx) // mind the swap, public goes first. should it?
//
//        val result = CardanoMutator(
//            Context(signed.body.value.fee, UtxoEnv(0L, env.protocolParams, CertState.empty)),
//            State(utxo, CertState.empty),
//            signed
//        )
//        assert(result.isRight)
//    }

//    test("mint tokens with a PlutusV3 script") {
//        val myAddress = arbitrary[ShelleyAddress].sample.get
//        val targetAddress = arbitrary[ShelleyAddress].sample.get
//        val hash = arbitrary[TransactionHash].sample.get
//
//        val scriptBytes = ByteString.unsafeFromArray(Array(69, 3, 1, 0, 36, -103).map(_.toByte))
//        val mintingScript = Script.PlutusV3(scriptBytes)
//        val policyId = mintingScript.scriptHash
//
//        val inputScriptBytes =
//            ByteString.unsafeFromArray(Array(69, 1, 1, 0, 36, -103).map(_.toByte))
//        val inputScript = Script.PlutusV3(inputScriptBytes)
//        val inputScriptAddress = ShelleyAddress(
//          Network.Mainnet,
//          ShelleyPaymentPart.Script(inputScript.scriptHash),
//          ShelleyDelegationPart.Null
//        )
//
//        val assetName = AssetName(ByteString.fromString("co2"))
//        val mintAmount = 1000L
//        val mintValue = Mint(MultiAsset(SortedMap(policyId -> SortedMap(assetName -> mintAmount))))
//
//        val availableLovelace = Value.lovelace(10_000_000L)
//        val txInputs = Map(
//          TransactionInput(hash, 0) -> TransactionOutput(inputScriptAddress, availableLovelace)
//        )
//        val collateral = Map(
//          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
//            myAddress,
//            Value.lovelace(100_000_000L)
//          )
//        )
//        val utxo: UTxO = txInputs ++ collateral
//
//        val inputSelector = InputSelector(
//          Set(ResolvedTxInput.Script(txInputs.head, inputScript, Data.unit)),
//          Set(ResolvedTxInput.Pubkey(collateral.head))
//        )
//
//        val interpreter = InterpreterWithProvidedData(
//          inputSelector,
//          utxo,
//          env,
//          ChangeReturnStrategy.toAddress(myAddress),
//          FeePayerStrategy.subtractFromAddress(myAddress),
//          evaluator
//        )
//
//        val tx = interpreter.realize(
//          Intention.Mint(mintValue, UsingPlutus(mintingScript, Data.unit), targetAddress)
//        )
//
//        assert(tx.body.value.mint.isDefined)
//        val mint = tx.body.value.mint.get
//        assert(mint.assets.contains(policyId))
//        assert(mint.assets(policyId).contains(assetName))
//        assert(mint.assets(policyId)(assetName) == mintAmount)
//
//        val targetOutput = tx.body.value.outputs.find(_.value.address == targetAddress)
//        assert(targetOutput.isDefined)
//        assert(targetOutput.get.value.value.assets.assets.contains(policyId))
//        assert(targetOutput.get.value.value.assets.assets(policyId).contains(assetName))
//        assert(targetOutput.get.value.value.assets.assets(policyId)(assetName) == mintAmount)
//
//        assert(tx.witnessSet.plutusV3Scripts.contains(mintingScript))
//        assert(tx.witnessSet.redeemers.isDefined)
//    }
//
//    test("mint tokens with a native script") {
//        val myAddress = arbitrary[ByronAddress].sample.get
//        val targetAddress = arbitrary[ShelleyAddress].sample.get
//        val hash = arbitrary[TransactionHash].sample.get
//
//        val keyHash = arbitrary[AddrKeyHash].sample.get
//        val nativeScript = Script.Native(Timelock.Signature(keyHash))
//        val policyId = nativeScript.scriptHash
//
//        val assetName = AssetName(ByteString.fromString("co2"))
//        val mintAmount = 500L
//        val mintValue = Mint(MultiAsset(SortedMap(policyId -> SortedMap(assetName -> mintAmount))))
//
//        val availableLovelace = Value.lovelace(10_000_000L)
//        val txInputs = Map(
//          TransactionInput(hash, 0) -> TransactionOutput(myAddress, availableLovelace)
//        )
//        val collateral = Map(
//          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
//            myAddress,
//            Value.lovelace(100_000_000L)
//          )
//        )
//        val utxo: UTxO = txInputs ++ collateral
//
//        val inputSelector = InputSelector(
//          Set(ResolvedTxInput.Pubkey(txInputs.head)),
//          Set(ResolvedTxInput.Pubkey(collateral.head))
//        )
//
//        val interpreter = InterpreterWithProvidedData(
//          inputSelector,
//          utxo,
//          env,
//          ChangeReturnStrategy.toAddress(myAddress),
//          FeePayerStrategy.subtractFromAddress(myAddress),
//          evaluator
//        )
//
//        val tx = interpreter.realize(
//          Intention.Mint(mintValue, MintIntention.UsingNative(nativeScript), targetAddress)
//        )
//
//        assert(tx.body.value.mint.isDefined)
//        val mint = tx.body.value.mint.get
//        assert(mint.assets.contains(policyId))
//        assert(mint.assets(policyId).contains(assetName))
//        assert(mint.assets(policyId)(assetName) == mintAmount)
//
//        val targetOutput = tx.body.value.outputs.find(_.value.address == targetAddress)
//        assert(targetOutput.isDefined)
//        assert(targetOutput.get.value.value.assets.assets.contains(policyId))
//        assert(targetOutput.get.value.value.assets.assets(policyId).contains(assetName))
//        assert(targetOutput.get.value.value.assets.assets(policyId)(assetName) == mintAmount)
//
//        assert(tx.witnessSet.nativeScripts.contains(nativeScript))
//        assert(tx.witnessSet.redeemers.isEmpty)
//    }
//
//    test("fail if the validator script throws an error") {
//        val myAddress = arbitrary[ShelleyAddress].sample.get
//        val faucet = arbitrary[ShelleyAddress].sample.get
//        val hash = arbitrary[TransactionHash].sample.get
//
//        val failingScriptCbor =
//            "582c0101002533573892011f616c77617973206661696c696e672076616c696461746f722063616c6c6564001601"
//        val script = Script.PlutusV3(ByteString.fromString(failingScriptCbor))
//
//        val scriptAddress = ShelleyAddress(
//          Network.Testnet,
//          ShelleyPaymentPart.Script(script.scriptHash),
//          ShelleyDelegationPart.Null
//        )
//
//        val availableLovelace = Value.lovelace(10_000_000L)
//        val txInputs = Map(
//          TransactionInput(hash, 0) -> TransactionOutput(scriptAddress, availableLovelace)
//        )
//        val collateral = Map(
//          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
//            myAddress,
//            Value.lovelace(100_000_000L)
//          )
//        )
//        val utxo: UTxO = txInputs ++ collateral
//
//        val payment = Value.lovelace(500L)
//
//        val inputSelector = InputSelector(
//          Set(ResolvedTxInput.Script(txInputs.head, script, Data.unit)),
//          Set(ResolvedTxInput.Pubkey(collateral.head))
//        )
//
//        assertThrows[Exception] {
//            InterpreterWithProvidedData(
//              inputSelector,
//              utxo,
//              env,
//              ChangeReturnStrategy.toAddress(myAddress),
//              FeePayerStrategy.subtractFromAddress(myAddress),
//              evaluator
//            ).realize(Intention.Pay(faucet, payment))
//        }
//    }
}
