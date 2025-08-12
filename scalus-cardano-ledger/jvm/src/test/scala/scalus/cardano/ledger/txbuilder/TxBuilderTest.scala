package scalus.cardano.ledger.txbuilder
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.{Address, ArbitraryInstances as ArbAddresses, Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.TransactionException.ValueNotConservedUTxOException
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.{ArbitraryInstances as ArbLedger, Coin, CostModels, TransactionException, TransactionHash, TransactionInput, TransactionOutput, UTxO, Value, *}
import scalus.ledger.api.{MajorProtocolVersion, Timelock}
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read

import scala.collection.immutable.SortedMap

class TxBuilderTest
    extends AnyFunSuite
    with ArbAddresses
    with ArbLedger
    with ValidatorRulesTestKit {
    val params: ProtocolParams = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)
    private val costModels = CostModels.fromProtocolParams(params)
    private val evaluator = PlutusScriptEvaluator(
      SlotConfig.Mainnet,
      initialBudget = ExBudget.enormous,
      protocolMajorVersion = MajorProtocolVersion.plominPV,
      costModels = costModels
    )

    private def builderContext(utxo: UTxO, validators: Seq[Validator] = Seq(FeesOkValidator)) =
        BuilderContext(
          params,
          evaluator,
          Network.Mainnet,
          UtxoProvider.from(utxo),
          OnSurplus.toFirstPayer,
          validators
        )

    test("should balance the transaction when the inputs exceed the outputs") {
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
        val tx = builderContext(utxo).buildNewTx
            .selectInputs(SelectInputs.all)
            .payToAddress(faucet, paymentAmount)
            .doFinalize

        assert(tx.body.value.outputs.size == 2)
        assert(tx.body.value.outputs.exists(_.value.address == myAddress))
        assert(tx.body.value.outputs.exists(_.value.address == faucet))

        val sumOutputs = tx.body.value.outputs
            .map(_.value.value.coin)
            .foldLeft(Coin.zero)(_ + _)
        val fee = tx.body.value.fee
        assert(Value(sumOutputs + fee) == availableLovelace)

    }

    test(
      "should create a pay do address tx that passes a full suit of cardano ledger rule validators"
    ) {
        val (privateKey, publicKey) = generateKeyPair()

        val keyHash = AddrKeyHash(platform.blake2b_224(publicKey))
        val myAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(keyHash),
          ShelleyDelegationPart.Null
        )
        val faucet = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val availableLovelace = Value.lovelace(100 * 1_000_000L)
        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(
            myAddress,
            availableLovelace
          )
        )

        val paymentAmount = Value.lovelace(50 * 1_000_000L)

        val contextWithKeys =
            builderContext(utxo, Seq(fullSuiteValidator)).withSigningKey(publicKey, privateKey)

        val tx = contextWithKeys.buildNewTx
            .selectInputs(SelectInputs.all)
            .payToAddress(faucet, paymentAmount)
            .doFinalize

        assert(tx.body.value.outputs.size == 2)
        assert(tx.body.value.outputs.exists(_.value.address == myAddress))
        assert(tx.body.value.outputs.exists(_.value.address == faucet))

        val sumOutputs = tx.body.value.outputs
            .map(_.value.value.coin)
            .foldLeft(Coin.zero)(_ + _)
        val fee = tx.body.value.fee
        assert(Value(sumOutputs + fee) == availableLovelace)
    }

    test(
      "should create a script tx that that passes a full suit of cardano ledger rule validators"
    ) {
        val emptyScriptBytes = Array(69, 1, 1, 0, 36, -103).map(_.toByte)
        val scriptBytes = ByteString.unsafeFromArray(emptyScriptBytes)
        val script = Script.PlutusV3(scriptBytes)

        val (privateKey, publicKey) = generateKeyPair()
        val keyHash = AddrKeyHash(platform.blake2b_224(publicKey))
        val myAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(keyHash),
          ShelleyDelegationPart.Null
        )

        val hash = arbitrary[TransactionHash].sample.get

        val scriptAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Script(script.scriptHash),
          ShelleyDelegationPart.Null
        )
        val availableLovelace = Value.lovelace(100_000_000L)
        val txInputs = Map(
          TransactionInput(hash, 0) -> TransactionOutput(scriptAddress, availableLovelace)
        )
        val hugeCollateral = Map(
          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(100_000_000L)
          )
        )
        val utxo: UTxO = txInputs ++ hugeCollateral

        val datum = Data.unit
        val redeemer = Data.unit

        val contextWithAllValidators =
            builderContext(utxo, Seq(fullSuiteValidator)).withSigningKey(publicKey, privateKey)

        val tx = contextWithAllValidators.buildNewTx
            .payToAddress(myAddress, Value.lovelace(50_000_000L))
            .withInputs(txInputs.keySet)
            .withScript(script, datum, redeemer, 0)
            .withCollateral(hugeCollateral.keySet)
            .doFinalize

        assert(tx.body.value.outputs.nonEmpty)
        assert(tx.witnessSet.redeemers.isDefined)
        assert(tx.witnessSet.redeemers.get.value.toSeq.size == 1)
    }

    test("should throw when trying to create a transaction where outputs exceed the inputs") {
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

        assertThrows[ValueNotConservedUTxOException] {
            builderContext(utxo).buildNewTx
                .selectInputs(SelectInputs.all)
                .payToAddress(faucet, Value.lovelace(paymentAmount))
                .doFinalize
        }
    }

    test("should throw when trying to create a tx where outputs cover the input, but not the fee") {
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

        // Technically available, but not with a fee
        val paymentAmount = availableLovelace - Value.lovelace(1L)
        assertThrows[TransactionException.IllegalArgumentException] {
            builderContext(utxo).buildNewTx
                .selectInputs(SelectInputs.all)
                .payToAddress(faucet, paymentAmount)
                .doFinalize
        }

    }

    test("should balance script transaction with proper fees") {
        val emptyScriptBytes = Array(69, 1, 1, 0, 36, -103).map(_.toByte)
        val scriptBytes = ByteString.unsafeFromArray(emptyScriptBytes)
        val script = Script.PlutusV3(scriptBytes)

        val myAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(arbitrary[AddrKeyHash].sample.get),
          ShelleyDelegationPart.Null
        )

        val hash = arbitrary[TransactionHash].sample.get

        val scriptAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Script(script.scriptHash),
          ShelleyDelegationPart.Null
        )
        val availableLovelace = Value.lovelace(10_000_000L)
        val txInputs = Map(
          TransactionInput(hash, 0) -> TransactionOutput(scriptAddress, availableLovelace)
        )
        val hugeCollateral = Map(
          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(100_000_000L)
          )
        )
        val utxo: UTxO = txInputs ++ hugeCollateral

        val datum = Data.unit
        val redeemer = Data.unit

        val tx = builderContext(utxo).buildNewTx
            .payToAddress(myAddress, Value.lovelace(1_000_000L))
            .withInputs(txInputs.keySet)
            .withScript(script, datum, redeemer, 0)
            .withCollateral(hugeCollateral.keySet)
            .doFinalize

        assert(tx.body.value.outputs.nonEmpty)
        assert(tx.witnessSet.redeemers.isDefined)
        assert(tx.witnessSet.redeemers.get.value.toSeq.size == 1)
    }

    test("should throw when a tx contains insufficient collateral") {
        val emptyScriptBytes = Array(69, 1, 1, 0, 36, -103).map(_.toByte)
        val scriptBytes = ByteString.unsafeFromArray(emptyScriptBytes)
        val script = Script.PlutusV3(scriptBytes)

        val myAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Key(arbitrary[AddrKeyHash].sample.get),
          ShelleyDelegationPart.Null
        )

        val hash = arbitrary[TransactionHash].sample.get

        val scriptAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Script(script.scriptHash),
          ShelleyDelegationPart.Null
        )
        val availableLovelace = Value.lovelace(10_000_000L)
        val txInputs = Map(
          TransactionInput(hash, 0) -> TransactionOutput(scriptAddress, availableLovelace)
        )

        val insufficientCollateral = Map(
          TransactionInput(arbitrary[TransactionHash].sample.get, 0) -> TransactionOutput(
            myAddress,
            Value.lovelace(1L)
          )
        )
        val utxo: UTxO = txInputs ++ insufficientCollateral

        val evaluator = PlutusScriptEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.enormous,
          protocolMajorVersion = MajorProtocolVersion.plominPV,
          costModels = costModels
        )

        val datum = Data.unit
        val redeemer = Data.unit

        assertThrows[TransactionException.InsufficientTotalSumOfCollateralCoinsException] {
            builderContext(utxo).buildNewTx
                .payToAddress(myAddress, Value.lovelace(1_000_000L))
                .withInputs(txInputs.keySet)
                .withScript(script, datum, redeemer, 0)
                .withCollateral(insufficientCollateral.keySet)
                .doFinalize
        }
    }

    // todo: verify hydrozoa minting `InitTxBuilder`
    ignore("should create a transaction with native script minting") {
        val myAddress = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        val availableLovelace = Value.lovelace(10_000_000L)
        val utxo: UTxO = Map(
          TransactionInput(hash, 0) -> TransactionOutput(myAddress, availableLovelace)
        )

        val keyHash = arbitrary[AddrKeyHash].sample.get
        val nativeScript = Script.Native(Timelock.Signature(keyHash))

        // Create script address for the minting policy
        val scriptAddress = ShelleyAddress(
          Network.Testnet,
          ShelleyPaymentPart.Script(nativeScript.scriptHash),
          ShelleyDelegationPart.Null
        )

        // Create tokens to mint using the native script's hash as policy ID
        val tokenName = AssetName(ByteString.fromString("co2"))
        val tokenAmount = 1000L
        val tokensToMint = MultiAsset(
          SortedMap(
            nativeScript.scriptHash -> SortedMap(tokenName -> tokenAmount)
          )
        )
        val mintValue = Value(Coin.zero, tokensToMint)

        val tx = builderContext(utxo, Seq(fullSuiteValidator)).buildNewTx
            .selectInputs(SelectInputs.all)
            .payAndMint(myAddress, mintValue)
            .withScript(nativeScript, 0)
            .doFinalize

        assert(tx.body.value.mint.isDefined)
        assert(tx.body.value.mint.get == tokensToMint)
        assert(tx.witnessSet.nativeScripts.contains(nativeScript))

        // Verify output contains minted tokens
        val outputWithTokens = tx.body.value.outputs.find(_.value.value.assets == tokensToMint)
        assert(outputWithTokens.isDefined)
    }

    private lazy val fullSuiteValidator: Validator { type Error = TransactionException } =
        new Validator {
            override type Error = TransactionException
            override def validate(context: Context, state: State, event: Event): Result =
                for
                    _ <- EmptyInputsValidator.validate(context, state, event)
                    _ <- InputsAndReferenceInputsDisjointValidator.validate(context, state, event)
                    _ <- AllInputsMustBeInUtxoValidator.validate(context, state, event)
                    _ <- ValueNotConservedUTxOValidator.validate(context, state, event)
                    _ <- VerifiedSignaturesInWitnessesValidator.validate(context, state, event)
                    _ <- MissingKeyHashesValidator.validate(context, state, event)
                    _ <- MissingOrExtraScriptHashesValidator.validate(context, state, event)
                    _ <- NativeScriptsValidator.validate(context, state, event)
                    _ <- TransactionSizeValidator.validate(context, state, event)
                    _ <- FeesOkValidator.validate(context, state, event)
                    _ <- OutputsHaveNotEnoughCoinsValidator.validate(context, state, event)
                    _ <- OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, event)
                    _ <- OutsideValidityIntervalValidator.validate(context, state, event)
                    _ <- OutsideForecastValidator.validate(context, state, event)
                    _ <- ExUnitsTooBigValidator.validate(context, state, event)
                    _ <- TooManyCollateralInputsValidator.validate(context, state, event)
                yield ()
        }
}
