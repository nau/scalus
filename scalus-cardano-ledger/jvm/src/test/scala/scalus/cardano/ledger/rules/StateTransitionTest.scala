package scalus.cardano.ledger.rules

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.address.Address
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read
import scalus.builtin.{ByteString, PlatformSpecific, given}
import java.security.SecureRandom
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}

class StateTransitionTest extends AnyFunSuite, ArbitraryInstances {
    private val params = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)

    test("EmptyInputsValidator rule success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                )
              )
            )
        }

        val result = EmptyInputsValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.value.inputs.nonEmpty)
    }

    test("EmptyInputsValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty
                )
              )
            )
        }

        val result = EmptyInputsValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.value.inputs.isEmpty)
    }

    test("InputsAndReferenceInputsDisjointValidator rule success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                  referenceInputs = Set.empty
                )
              )
            )
        }

        val result = InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(
          transaction.body.value.inputs.nonEmpty && transaction.body.value.referenceInputs.isEmpty
        )
    }

    test("InputsAndReferenceInputsDisjointValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(tx.body.value.copy(inputs = inputs, referenceInputs = inputs))
            )
        }

        val result = InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          transaction.body.value.inputs.nonEmpty && transaction.body.value.referenceInputs.nonEmpty
        )
        assert(transaction.body.value.inputs == transaction.body.value.referenceInputs)
    }

    test("AllInputsMustBeInUtxoValidator rule success") {
        val context = Context()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                  collateralInputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                  referenceInputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                )
              )
            )
        }
        val state = State(
          utxo = transaction.body.value.inputs.view
              .concat(transaction.body.value.collateralInputs)
              .concat(transaction.body.value.referenceInputs)
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.value.inputs.forall(state.utxo.contains))
        assert(transaction.body.value.collateralInputs.forall(state.utxo.contains))
        assert(transaction.body.value.referenceInputs.forall(state.utxo.contains))
    }

    test("AllInputsMustBeInUtxoValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                  collateralInputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                  referenceInputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
                )
              )
            )
        }

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(!transaction.body.value.inputs.forall(state.utxo.contains))
        assert(!transaction.body.value.collateralInputs.forall(state.utxo.contains))
        assert(!transaction.body.value.referenceInputs.forall(state.utxo.contains))
    }

    test("VerifiedWitnessesValidator VkeyWitnesses rule success") {
        val context = Context()
        val transaction = {
            val (privateKey1, publicKey1) = generateKeyPair()
            val (privateKey2, publicKey2) = generateKeyPair()
            val (privateKey3, publicKey3) = generateKeyPair()
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, summon[PlatformSpecific].signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, summon[PlatformSpecific].signEd25519(privateKey2, tx.id)),
                  VKeyWitness(publicKey3, summon[PlatformSpecific].signEd25519(privateKey3, tx.id))
                ),
                bootstrapWitnesses = Set.empty
              )
            )
        }
        val state = State()

        val result = VerifiedWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("VerifiedWitnessesValidator VkeyWitnesses rule failure") {
        val context = Context()
        val transaction = {
            val (privateKey1, publicKey1) = generateKeyPair()
            val (privateKey2, publicKey2) = generateKeyPair()
            val (privateKey3, publicKey3) = generateKeyPair()
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, summon[PlatformSpecific].signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, summon[PlatformSpecific].signEd25519(privateKey2, tx.id)),
                  VKeyWitness(
                    publicKey3, {
                        val signature = summon[PlatformSpecific].signEd25519(privateKey3, tx.id)
                        signature.bytes(0) =
                            (signature.bytes(0) + 1).toByte // Intentionally corrupt the signature
                        signature
                    }
                  )
                ),
                bootstrapWitnesses = Set.empty
              )
            )
        }
        val state = State()

        val result = VerifiedWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("VerifiedWitnessesValidator BootstrapWitnesses rule success") {
        val context = Context()
        val transaction = {
            val (privateKey1, publicKey1) = generateKeyPair()
            val (privateKey2, publicKey2) = generateKeyPair()
            val (privateKey3, publicKey3) = generateKeyPair()
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set(
                  BootstrapWitness(
                    publicKey1,
                    summon[PlatformSpecific].signEd25519(privateKey1, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  ),
                  BootstrapWitness(
                    publicKey2,
                    summon[PlatformSpecific].signEd25519(privateKey2, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  ),
                  BootstrapWitness(
                    publicKey3,
                    summon[PlatformSpecific].signEd25519(privateKey3, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  )
                )
              )
            )
        }
        val state = State()

        val result = VerifiedWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("VerifiedWitnessesValidator BootstrapWitnesses rule failure") {
        val context = Context()
        val transaction = {
            val (privateKey1, publicKey1) = generateKeyPair()
            val (privateKey2, publicKey2) = generateKeyPair()
            val (privateKey3, publicKey3) = generateKeyPair()
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set(
                  BootstrapWitness(
                    publicKey1,
                    summon[PlatformSpecific].signEd25519(privateKey1, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  ),
                  BootstrapWitness(
                    publicKey2,
                    summon[PlatformSpecific].signEd25519(privateKey2, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  ),
                  BootstrapWitness(
                    publicKey3, {
                        val signature = summon[PlatformSpecific].signEd25519(privateKey3, tx.id)
                        signature.bytes(0) =
                            (signature.bytes(0) + 1).toByte // Intentionally corrupt the signature
                        signature
                    },
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  )
                )
              )
            )
        }
        val state = State()

        val result = VerifiedWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeeMutator success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  fee = Arbitrary.arbitrary[Coin].sample.get
                )
              )
            )
        }

        val result = FeeMutator.transit(context, state, transaction)
        assert(result.isRight)
        assert(context.fee == transaction.body.value.fee)
    }

    test("RemoveInputsFromUtxoMutator success") {
        val context = Context()
        val state = State(
          utxo = genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](1, 4).sample.get
        )
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = state.utxo.keySet
                )
              )
            )
        }

        val result = RemoveInputsFromUtxoMutator.transit(context, state, transaction)
        assert(state.utxo.nonEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxo.isEmpty)
    }

    test("AddOutputsToUtxoMutator success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = genVectorOfSizeFromArbitrary[TransactionOutput](1, 4).sample.get
                )
              )
            )
        }

        val result = AddOutputsToUtxoMutator.transit(context, state, transaction)
        assert(state.utxo.isEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxo.values.toSeq == transaction.body.value.outputs)
    }

//    test("CardanoMutator success") {
//        val context = Context()
//        val transaction = {
//            val tx = randomValidTransaction
//            tx.copy(
//              body = KeepRaw(
//                tx.body.value.copy(
//                  inputs = Set(
//                    Arbitrary.arbitrary[TransactionInput].sample.get
//                  ),
//                  outputs = Vector(
//                    TransactionOutput.Shelley(
//                      Arbitrary.arbitrary[Address].sample.get,
//                      Value(Coin(Gen.choose(0L, 1000000L).sample.get))
//                    )
//                  ),
//                  fee = Coin(Gen.choose(0L, 1000000L).sample.get),
//                  collateralInputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
//                  referenceInputs = Set.empty
//                )
//              )
//            )
//        }
//        val state = State(
//          utxo = transaction.body.value.collateralInputs.view
//              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
//              .concat(
//                Seq(
//                  transaction.body.value.inputs.head -> TransactionOutput.Shelley(
//                    Arbitrary.arbitrary[Address].sample.get,
//                    Value(
//                      Coin(
//                        transaction.body.value.outputs.head
//                            .asInstanceOf[TransactionOutput.Shelley]
//                            .value
//                            .coin
//                            .value +
//                            transaction.body.value.fee.value
//                      )
//                    )
//                  )
//                )
//              )
//              .toMap
//        )
//
//        val result = CardanoMutator.transit(context, state, transaction)
//        assert(result.isRight)
//        assert(transaction.body.value.inputs.nonEmpty)
//        assert(transaction.body.value.referenceInputs.isEmpty)
//        assert(transaction.body.value.inputs.forall(state.utxo.contains))
//        assert(transaction.body.value.collateralInputs.forall(state.utxo.contains))
//        assert(context.fee == transaction.body.value.fee)
//        assert(state.utxo.nonEmpty)
//        assert(!transaction.body.value.inputs.forall(result.toOption.get.utxo.contains))
//        assert(
//          transaction.body.value.outputs.forall(result.toOption.get.utxo.values.toSeq.contains)
//        )
//    }

    private[this] def randomValidTransaction =
        Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)

    private val keyPairGenerator = {
        val keyPairGenerator = new Ed25519KeyPairGenerator()
        keyPairGenerator.init(new Ed25519KeyGenerationParameters(new SecureRandom()))
        keyPairGenerator
    }
    private def generateKeyPair(): (ByteString, ByteString) = {
        val asymmetricCipherKeyPair: AsymmetricCipherKeyPair = keyPairGenerator.generateKeyPair();
        val privateKeyParams: Ed25519PrivateKeyParameters =
            asymmetricCipherKeyPair.getPrivate.asInstanceOf[Ed25519PrivateKeyParameters];
        val publicKeyParams: Ed25519PublicKeyParameters =
            asymmetricCipherKeyPair.getPublic.asInstanceOf[Ed25519PublicKeyParameters];
        val privateKey: ByteString = ByteString.fromArray(privateKeyParams.getEncoded)
        val publicKey: ByteString = ByteString.fromArray(publicKeyParams.getEncoded)
        (privateKey, publicKey)
    }
}
