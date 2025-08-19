package scalus.cardano.ledger.txbuilder
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.{Address, ArbitraryInstances as ArbAddresses, Network}
import scalus.cardano.ledger.rules.*
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.{ArbitraryInstances as ArbLedger, Coin, CostModels, TransactionException, TransactionHash, TransactionInput, TransactionOutput, UTxO, Value, *}
import scalus.ledger.api.MajorProtocolVersion
import scalus.ledger.babbage.ProtocolParams
import scalus.uplc.eval.ExBudget
import upickle.default.read

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
    private val env = Environment(params, evaluator, Network.Mainnet)

    test("should balance the transaction when the inputs exceed the outputs") {
        val myAddress = arbitrary[Address].sample.get
        val faucet = arbitrary[Address].sample.get
        val hash = arbitrary[TransactionHash].sample.get

        // Input tx produced 1_000_000 lovelace
        val availableLovelace = Value.lovelace(1_000_000L)
        val myInput = TransactionInput(hash, 0)
        val utxo: UTxO = Map(
          myInput -> TransactionOutput(
            myAddress,
            availableLovelace
          )
        )
        val intention = PayTo(faucet, Value.lovelace(500_000L), TransactionResolver(utxo))
            .withEnvironment(env)
            .usingInputs(myInput)
            .usingFeePayerStrategy(FeePayerStrategy.subtractFromAddress(myAddress))
            .usingChangeReturnStrategy(ChangeReturnStrategy.toAddress(myAddress))

        val tx = realize(intention)()

        assert(tx.body.value.outputs.size == 2)
        assert(tx.body.value.outputs.exists(_.value.address == myAddress))
        assert(tx.body.value.outputs.exists(_.value.address == faucet))

        val sumOutputs = tx.body.value.outputs
            .map(_.value.value.coin)
            .foldLeft(Coin.zero)(_ + _)
        val fee = tx.body.value.fee
        assert(Value(sumOutputs + fee) == availableLovelace)

    }

//    test("should throw when trying to create a transaction where outputs exceed the inputs") {
//        val myAddress = arbitrary[Address].sample.get
//        val faucet = arbitrary[Address].sample.get
//        val hash = arbitrary[TransactionHash].sample.get
//
//        // Input tx produced 1K lovelace
//        val utxo: UTxO = Map(
//          TransactionInput(hash, 0) -> TransactionOutput(
//            myAddress,
//            Value.lovelace(1_000L)
//          )
//        )
//        // exceeds available lovelace
//        val paymentAmount = 10_000L
//
//        assertThrows[ValueNotConservedUTxOException] {
//            builderContext(utxo).buildNewTx
//                .selectInputs(SelectInputs.all)
//                .payTo(faucet, Value.lovelace(paymentAmount))
//                .build
//        }
//    }
//
//    test("should throw when trying to create a tx where outputs cover the input, but not the fee") {
//        val myAddress = arbitrary[Address].sample.get
//        val faucet = arbitrary[Address].sample.get
//        val hash = arbitrary[TransactionHash].sample.get
//
//        // Input tx produced 1_000_000 lovelace
//        val availableLovelace = Value.lovelace(1_000_000L)
//        val utxo: UTxO = Map(
//          TransactionInput(hash, 0) -> TransactionOutput(
//            myAddress,
//            availableLovelace
//          )
//        )
//
//        // Technically available, but not with a fee
//        val paymentAmount = availableLovelace - Value.lovelace(1L)
//        assertThrows[TransactionException.IllegalArgumentException] {
//            builderContext(utxo).buildNewTx
//                .selectInputs(SelectInputs.all)
//                .payTo(faucet, paymentAmount)
//                .build
//        }
//
//    }

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
