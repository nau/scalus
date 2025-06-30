package scalus.cardano.ledger.rules

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.cardano.address.{Address, ByronAddress, ShelleyAddress, ShelleyPaymentPart}
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read
import scalus.builtin.{platform, ByteString, PlatformSpecific, given}

import java.security.SecureRandom
import org.bouncycastle.crypto.generators.Ed25519KeyPairGenerator
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.{Ed25519KeyGenerationParameters, Ed25519PrivateKeyParameters, Ed25519PublicKeyParameters}
import scalus.ledger.api.Timelock

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
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id)),
                  VKeyWitness(publicKey3, platform.signEd25519(privateKey3, tx.id))
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
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id)),
                  VKeyWitness(
                    publicKey3, {
                        val signature = platform.signEd25519(privateKey3, tx.id)
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
                    platform.signEd25519(privateKey1, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  ),
                  BootstrapWitness(
                    publicKey2,
                    platform.signEd25519(privateKey2, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  ),
                  BootstrapWitness(
                    publicKey3,
                    platform.signEd25519(privateKey3, tx.id),
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
                    platform.signEd25519(privateKey1, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  ),
                  BootstrapWitness(
                    publicKey2,
                    platform.signEd25519(privateKey2, tx.id),
                    genByteStringOfN(32).sample.get,
                    genByteStringOfN(32).sample.get
                  ),
                  BootstrapWitness(
                    publicKey3, {
                        val signature = platform.signEd25519(privateKey3, tx.id)
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

    test("NativeScriptsValidator rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()
        val (privateKey3, publicKey3) = generateKeyPair()

        val signatureTimelock1 =
            Timelock.Signature(Hash(platform.blake2b_224(publicKey1)))
        val signatureTimelock2 =
            Timelock.Signature(Hash(platform.blake2b_224(publicKey2)))
        val signatureTimelock3 =
            Timelock.Signature(Hash(platform.blake2b_224(publicKey3)))
        val allOfTimelock = Timelock.AllOf(Seq(signatureTimelock1, signatureTimelock2))
        val anyOfTimelock =
            Timelock.AnyOf(Seq(signatureTimelock1, signatureTimelock2, signatureTimelock3))
        val mOfTimelock =
            Timelock.MOf(2, Seq(signatureTimelock1, signatureTimelock2, signatureTimelock3))
        val timeStartTimelock = Timelock.TimeStart(5)
        val timeExpireTimelock = Timelock.TimeExpire(20)

        val input1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input2 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input3 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input4 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input5 = Arbitrary.arbitrary[TransactionInput].sample.get

        val referenceInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val referenceInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set(input1, input2, input3, input4, input5),
                  collateralInputs = Set.empty,
                  referenceInputs = Set(referenceInput1, referenceInput2),
                  validityStartSlot = Some(10),
                  ttl = Some(15)
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                ),
                nativeScripts = Set(
                  signatureTimelock1,
                  signatureTimelock2,
                  allOfTimelock,
                  anyOfTimelock,
                  mOfTimelock
                )
              )
            )
        }
        val state = {
            val signatureTimelock1Address = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(signatureTimelock1.scriptHash)
                )

            val signatureTimelock2Address = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(signatureTimelock2.scriptHash)
                )

            val allOfTimelockAddress = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(allOfTimelock.scriptHash)
                )

            val anyOfTimelockAddress = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(anyOfTimelock.scriptHash)
                )

            val mOfTimelockAddress = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(mOfTimelock.scriptHash)
                )

            val timeStartTimelockAddress = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(timeStartTimelock.scriptHash)
                )

            val timeExpireTimelockAddress = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(timeExpireTimelock.scriptHash)
                )

            State(
              utxo = Map(
                input1 -> TransactionOutput.Babbage(
                  Address.Shelley(signatureTimelock1Address),
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                input2 -> TransactionOutput.Babbage(
                  Address.Shelley(signatureTimelock2Address),
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                input3 -> TransactionOutput.Babbage(
                  Address.Shelley(allOfTimelockAddress),
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                input4 -> TransactionOutput.Babbage(
                  Address.Shelley(anyOfTimelockAddress),
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                input5 -> TransactionOutput.Babbage(
                  Address.Shelley(mOfTimelockAddress),
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                referenceInput1 -> TransactionOutput
                    .Babbage(
                      Address.Shelley(timeStartTimelockAddress),
                      Value(Coin(1000L)),
                      None,
                      Some(ScriptRef(Script.Native(timeStartTimelock)))
                    ),
                referenceInput2 -> TransactionOutput
                    .Babbage(
                      Address.Shelley(timeExpireTimelockAddress),
                      Value(Coin(1000L)),
                      None,
                      Some(ScriptRef(Script.Native(timeExpireTimelock)))
                    )
              )
            )
        }

        val result = NativeScriptsValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("NativeScriptsValidator rule failure") {
        val context = Context()

        val timeStartTimelock = Timelock.TimeStart(5)
        val timeExpireTimelock = Timelock.TimeExpire(20)

        val input1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set(input1, input2),
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  validityStartSlot = Some(10),
                  ttl = Some(25)
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                nativeScripts = Set(timeExpireTimelock)
              )
            )
        }
        val state = {
            val timeStartTimelockAddress = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(timeStartTimelock.scriptHash)
                )

            val timeExpireTimelockAddress = Arbitrary
                .arbitrary[ShelleyAddress]
                .sample
                .get
                .copy(
                  payment = ShelleyPaymentPart.Script(timeExpireTimelock.scriptHash)
                )

            State(
              utxo = Map(
                input1 -> TransactionOutput
                    .Babbage(
                      Address.Shelley(timeStartTimelockAddress),
                      Value(Coin(1000L)),
                      None,
                      Some(ScriptRef(Script.Native(timeStartTimelock)))
                    ),
                input2 -> TransactionOutput
                    .Babbage(
                      Address.Shelley(timeExpireTimelockAddress),
                      Value(Coin(1000L)),
                      None,
                      None
                    )
              )
            )
        }

        val result = NativeScriptsValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("NeededWitnessesValidator Inputs rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val input1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set(input1, input2),
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set.empty,
                  withdrawals = None,
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            input1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Key(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                    )
              ),
              Value(Coin(1000000L))
            ),
            input2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Key(
                        Hash(platform.blake2b_224(publicKey2))
                      )
                    )
              ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("NeededWitnessesValidator Inputs rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val input1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val input2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set(input1, input2),
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set.empty,
                  withdrawals = None,
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            input1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Key(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                    )
              ),
              Value(Coin(1000000L))
            ),
            input2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Key(
                        Hash(platform.blake2b_224(publicKey2))
                      )
                    )
              ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("NeededWitnessesValidator CollateralInputs rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set(collateralInput1, collateralInput2),
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set.empty,
                  withdrawals = None,
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            collateralInput1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Key(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                    )
              ),
              Value(Coin(1000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Key(
                        Hash(platform.blake2b_224(publicKey2))
                      )
                    )
              ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("NeededWitnessesValidator CollateralInputs rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set(collateralInput1, collateralInput2),
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set.empty,
                  withdrawals = None,
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            collateralInput1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Key(
                        Hash(platform.blake2b_224(publicKey1))
                      )
                    )
              ),
              Value(Coin(1000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Key(
                        Hash(platform.blake2b_224(publicKey2))
                      )
                    )
              ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("NeededWitnessesValidator VotingProcedures success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()
        val (privateKey3, publicKey3) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = Some(
                    VotingProcedures(
                      Map(
                        Voter.ConstitutionalCommitteeHotKey(
                          Hash(platform.blake2b_224(publicKey1))
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get,
                        Voter.StakingPoolKey(
                          Hash(platform.blake2b_224(publicKey2))
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get,
                        Voter.DRepKey(
                          Hash(platform.blake2b_224(publicKey3))
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get,
                        Voter.ConstitutionalCommitteeHotScript(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get,
                        Voter.DRepScript(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get
                      )
                    )
                  ),
                  certificates = Set.empty,
                  withdrawals = None,
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id)),
                  VKeyWitness(publicKey3, platform.signEd25519(privateKey3, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("NeededWitnessesValidator VotingProcedures failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()
        val (privateKey3, publicKey3) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = Some(
                    VotingProcedures(
                      Map(
                        Voter.ConstitutionalCommitteeHotKey(
                          Hash(platform.blake2b_224(publicKey1))
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get,
                        Voter.StakingPoolKey(
                          Hash(platform.blake2b_224(publicKey2))
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get,
                        Voter.DRepKey(
                          Hash(platform.blake2b_224(publicKey3))
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get,
                        Voter.ConstitutionalCommitteeHotScript(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get,
                        Voter.DRepScript(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        ) -> genMapOfSizeFromArbitrary(0, 4).sample.get
                      )
                    )
                  ),
                  certificates = Set.empty,
                  withdrawals = None,
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("NeededWitnessesValidator Withdrawals rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set.empty,
                  withdrawals = Some(
                    Withdrawals(
                      Map(
                        RewardAccount(
                          Address.Shelley(
                            Arbitrary
                                .arbitrary[ShelleyAddress]
                                .sample
                                .get
                                .copy(
                                  payment = ShelleyPaymentPart.Key(
                                    Hash(platform.blake2b_224(publicKey1))
                                  )
                                )
                          )
                        ) -> Coin(1000000L),
                        RewardAccount(
                          Address.Shelley(
                            Arbitrary
                                .arbitrary[ShelleyAddress]
                                .sample
                                .get
                                .copy(
                                  payment = ShelleyPaymentPart.Key(
                                    Hash(platform.blake2b_224(publicKey2))
                                  )
                                )
                          )
                        ) -> Coin(2000000L)
                      )
                    )
                  ),
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, platform.signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("NeededWitnessesValidator Withdrawals rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set.empty,
                  withdrawals = Some(
                    Withdrawals(
                      Map(
                        RewardAccount(
                          Address.Shelley(
                            Arbitrary
                                .arbitrary[ShelleyAddress]
                                .sample
                                .get
                                .copy(
                                  payment = ShelleyPaymentPart.Key(
                                    Hash(platform.blake2b_224(publicKey1))
                                  )
                                )
                          )
                        ) -> Coin(1000000L),
                        RewardAccount(
                          Address.Shelley(
                            Arbitrary
                                .arbitrary[ShelleyAddress]
                                .sample
                                .get
                                .copy(
                                  payment = ShelleyPaymentPart.Key(
                                    Hash(platform.blake2b_224(publicKey2))
                                  )
                                )
                          )
                        ) -> Coin(2000000L)
                      )
                    )
                  ),
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, platform.signEd25519(privateKey1, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("NeededWitnessesValidator Certificates rule success") {
        val context = Context()

        val (privateKey, publicKey) = generateKeyPair()
        val credential = Credential.KeyHash(
          Hash(platform.blake2b_224(publicKey))
        )

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set(
                    Certificate.StakeRegistration(credential),
                    Certificate.StakeDeregistration(credential),
                    Certificate
                        .StakeDelegation(credential, Arbitrary.arbitrary[PoolKeyHash].sample.get),
                    Certificate.PoolRegistration(
                      Hash(platform.blake2b_224(publicKey)),
                      Arbitrary.arbitrary[VrfKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[UnitInterval].sample.get,
                      Arbitrary.arbitrary[RewardAccount].sample.get,
                      Set(Hash(platform.blake2b_224(publicKey))),
                      Arbitrary.arbitrary[IndexedSeq[Relay]].sample.get,
                      Arbitrary.arbitrary[Option[PoolMetadata]].sample.get
                    ),
                    Certificate
                        .PoolRetirement(Hash(platform.blake2b_224(publicKey)), 1),
                    Certificate.RegCert(credential, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.UnregCert(credential, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.VoteDelegCert(credential, Arbitrary.arbitrary[DRep].sample.get),
                    Certificate.StakeVoteDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get
                    ),
                    Certificate.StakeRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.VoteRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.StakeVoteRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.AuthCommitteeHotCert(
                      credential,
                      Arbitrary.arbitrary[Credential].sample.get
                    ),
                    Certificate.ResignCommitteeColdCert(
                      credential,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.RegDRepCert(
                      credential,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.UnregDRepCert(credential, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.UpdateDRepCert(
                      credential,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    )
                  ),
                  withdrawals = None,
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey, platform.signEd25519(privateKey, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("NeededWitnessesValidator Certificates rule failure") {
        val context = Context()

        val (privateKey, publicKey) = generateKeyPair()
        val credential = Credential.KeyHash(
          Hash(platform.blake2b_224(publicKey))
        )

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set(
                    Certificate.StakeRegistration(credential),
                    Certificate.StakeDeregistration(credential),
                    Certificate
                        .StakeDelegation(credential, Arbitrary.arbitrary[PoolKeyHash].sample.get),
                    Certificate.PoolRegistration(
                      Hash(platform.blake2b_224(publicKey)),
                      Arbitrary.arbitrary[VrfKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[UnitInterval].sample.get,
                      Arbitrary.arbitrary[RewardAccount].sample.get,
                      Set(Hash(platform.blake2b_224(publicKey))),
                      Arbitrary.arbitrary[IndexedSeq[Relay]].sample.get,
                      Arbitrary.arbitrary[Option[PoolMetadata]].sample.get
                    ),
                    Certificate
                        .PoolRetirement(Hash(platform.blake2b_224(publicKey)), 1),
                    Certificate.RegCert(credential, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.UnregCert(credential, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.VoteDelegCert(credential, Arbitrary.arbitrary[DRep].sample.get),
                    Certificate.StakeVoteDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get
                    ),
                    Certificate.StakeRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.VoteRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.StakeVoteRegDelegCert(
                      credential,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.AuthCommitteeHotCert(
                      credential,
                      Arbitrary.arbitrary[Credential].sample.get
                    ),
                    Certificate.ResignCommitteeColdCert(
                      credential,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.RegDRepCert(
                      credential,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.UnregDRepCert(credential, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.UpdateDRepCert(
                      credential,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    )
                  ),
                  withdrawals = None,
                  requiredSigners = Set.empty
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty
              )
            )
        }

        val state = State()

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("NeededWitnessesValidator RequiredSigners rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set.empty,
                  withdrawals = None,
                  requiredSigners = Set(
                    Hash(summon[PlatformSpecific].blake2b_224(publicKey1)),
                    Hash(summon[PlatformSpecific].blake2b_224(publicKey2))
                  )
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, summon[PlatformSpecific].signEd25519(privateKey1, tx.id)),
                  VKeyWitness(publicKey2, summon[PlatformSpecific].signEd25519(privateKey2, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("NeededWitnessesValidator RequiredSigners rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  votingProcedures = None,
                  certificates = Set.empty,
                  withdrawals = None,
                  requiredSigners = Set(
                    Hash(summon[PlatformSpecific].blake2b_224(publicKey1)),
                    Hash(summon[PlatformSpecific].blake2b_224(publicKey2))
                  )
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey1, summon[PlatformSpecific].signEd25519(privateKey1, tx.id))
                )
              )
            )
        }

        val state = State()

        val result = NeededWitnessesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingScriptsValidator rule success") {
        val context = Context()

        val (privateKey, publicKey) = generateKeyPair()

        val nativeScript =
            Timelock.Signature(Hash(platform.blake2b_224(publicKey)))
        val plutusV1Script = Arbitrary.arbitrary[PlutusV1Script].sample.get
        val plutusV2Script = Arbitrary.arbitrary[PlutusV2Script].sample.get
        val plutusV3Script = Arbitrary.arbitrary[PlutusV3Script].sample.get

        val credential1 = Credential.ScriptHash(plutusV1Script.scriptHash)
        val credential2 = Credential.ScriptHash(plutusV2Script.scriptHash)
        val credential3 = Credential.ScriptHash(plutusV3Script.scriptHash)

        val input = Arbitrary.arbitrary[TransactionInput].sample.get
        val referenceInput = Arbitrary.arbitrary[TransactionInput].sample.get
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set(input),
                  collateralInputs = Set.empty,
                  referenceInputs = Set(referenceInput),
                  mint = Some(
                    Map(
                      plutusV1Script.scriptHash -> Map.empty,
                      plutusV2Script.scriptHash -> Map.empty,
                      plutusV3Script.scriptHash -> Map.empty
                    )
                  ),
                  votingProcedures = Some(
                    VotingProcedures(
                      Map(
                        Voter.ConstitutionalCommitteeHotScript(
                          nativeScript.scriptHash
                        ) -> Map.empty,
                        Voter.DRepScript(nativeScript.scriptHash) -> Map.empty
                      )
                    )
                  ),
                  withdrawals = Some(
                    Withdrawals(
                      Map(
                        RewardAccount(
                          Address.Shelley(
                            Arbitrary
                                .arbitrary[ShelleyAddress]
                                .sample
                                .get
                                .copy(
                                  payment = ShelleyPaymentPart.Script(nativeScript.scriptHash)
                                )
                          )
                        ) -> Arbitrary.arbitrary[Coin].sample.get
                      )
                    )
                  ),
                  proposalProcedures = Set(
                    Arbitrary
                        .arbitrary[ProposalProcedure]
                        .sample
                        .get
                        .copy(govAction =
                            GovAction.ParameterChange(
                              None,
                              Arbitrary.arbitrary[ProtocolParamUpdate].sample.get,
                              Some(nativeScript.scriptHash)
                            )
                        ),
                    Arbitrary
                        .arbitrary[ProposalProcedure]
                        .sample
                        .get
                        .copy(govAction =
                            GovAction.TreasuryWithdrawals(
                              Map.empty,
                              Some(nativeScript.scriptHash)
                            )
                        )
                  ),
                  certificates = Set(
                    Certificate.StakeRegistration(credential1),
                    Certificate.StakeDeregistration(credential2),
                    Certificate
                        .StakeDelegation(credential3, Arbitrary.arbitrary[PoolKeyHash].sample.get),
                    Certificate.PoolRegistration(
                      Hash(platform.blake2b_224(publicKey)),
                      Arbitrary.arbitrary[VrfKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[UnitInterval].sample.get,
                      Arbitrary.arbitrary[RewardAccount].sample.get,
                      Arbitrary.arbitrary[Set[AddrKeyHash]].sample.get,
                      Arbitrary.arbitrary[IndexedSeq[Relay]].sample.get,
                      Arbitrary.arbitrary[Option[PoolMetadata]].sample.get
                    ),
                    Certificate
                        .PoolRetirement(Hash(platform.blake2b_224(publicKey)), 1),
                    Certificate.RegCert(credential1, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.UnregCert(credential2, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.VoteDelegCert(credential3, Arbitrary.arbitrary[DRep].sample.get),
                    Certificate.StakeVoteDelegCert(
                      credential1,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get
                    ),
                    Certificate.StakeRegDelegCert(
                      credential2,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.VoteRegDelegCert(
                      credential3,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.StakeVoteRegDelegCert(
                      credential1,
                      Arbitrary.arbitrary[PoolKeyHash].sample.get,
                      Arbitrary.arbitrary[DRep].sample.get,
                      Arbitrary.arbitrary[Coin].sample.get
                    ),
                    Certificate.AuthCommitteeHotCert(
                      credential2,
                      Arbitrary.arbitrary[Credential].sample.get
                    ),
                    Certificate.ResignCommitteeColdCert(
                      credential3,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.RegDRepCert(
                      credential1,
                      Arbitrary.arbitrary[Coin].sample.get,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    ),
                    Certificate.UnregDRepCert(credential2, Arbitrary.arbitrary[Coin].sample.get),
                    Certificate.UpdateDRepCert(
                      credential3,
                      Arbitrary.arbitrary[Option[Anchor]].sample.get
                    )
                  ),
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set(
                  VKeyWitness(publicKey, platform.signEd25519(privateKey, tx.id))
                ),
                nativeScripts = Set.empty,
                plutusV1Scripts = Set(plutusV1Script),
                plutusV2Scripts = Set(plutusV2Script),
                plutusV3Scripts = Set(plutusV3Script)
              )
            )
        }

        val state = State(
          utxo = Map(
            input -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(
                      payment = ShelleyPaymentPart.Script(nativeScript.scriptHash)
                    )
              ),
              Value(Coin(1000000L))
            ),
            referenceInput -> TransactionOutput
                .Babbage(
                  Arbitrary.arbitrary[Address].sample.get,
                  Value(Coin(1000L)),
                  None,
                  Some(ScriptRef(Script.Native(nativeScript)))
                )
          )
        )

        val result = MissingScriptsValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("FeesOkValidator rule success") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set(collateralInput1, collateralInput2),
                  collateralReturnOutput = Some(
                    TransactionOutput.Shelley(
                      Address.Shelley(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get
                      ),
                      Value(Coin(20000000L))
                    )
                  ),
                  totalCollateral = Some(Coin(60000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = Set.empty,
                  certificates = Set.empty,
                  requiredSigners = Set.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet(Set.empty)),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            collateralInput1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey1))
                        )
                    )
              ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey2))
                        )
                    )
              ),
              Value(Coin(30000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("FeesOkValidator feePaidIsGreeterOrEqualThanMinimumFee rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set(collateralInput1, collateralInput2),
                  collateralReturnOutput = Some(
                    TransactionOutput.Shelley(
                      Address.Shelley(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get
                      ),
                      Value(Coin(20000000L))
                    )
                  ),
                  totalCollateral = Some(Coin(60000000L)),
                  fee = Coin(1L),
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = Set.empty,
                  certificates = Set.empty,
                  requiredSigners = Set.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet(Set.empty)),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            collateralInput1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey1))
                        )
                    )
              ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey2))
                        )
                    )
              ),
              Value(Coin(30000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator collateralConsistsOnlyOfVKeyAddress rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set(collateralInput1, collateralInput2),
                  collateralReturnOutput = None,
                  totalCollateral = None,
                  fee = Coin(10000000L),
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = Set.empty,
                  certificates = Set.empty,
                  requiredSigners = Set.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet(Set.empty)),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            collateralInput1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey1))
                        )
                    )
              ),
              Value(Coin(20000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.Script(
                          Arbitrary.arbitrary[ScriptHash].sample.get
                        )
                    )
              ),
              Value(Coin(20000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator collateralDoesNotContainAnyNonADA rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set(collateralInput1, collateralInput2),
                  collateralReturnOutput = Some(
                    TransactionOutput.Shelley(
                      Address.Shelley(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get
                      ),
                      Value(Coin(20000000L))
                    )
                  ),
                  totalCollateral = Some(Coin(60000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = Set.empty,
                  certificates = Set.empty,
                  requiredSigners = Set.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet(Set.empty)),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            collateralInput1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey1))
                        )
                    )
              ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey2))
                        )
                    )
              ),
              Value(Coin(30000000L), Arbitrary.arbitrary[MultiAsset].sample.get)
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator totalSumOfCollateralCoinsIsSufficient rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set(collateralInput1, collateralInput2),
                  collateralReturnOutput = Some(
                    TransactionOutput.Shelley(
                      Address.Shelley(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get
                      ),
                      Value(Coin(60000000L))
                    )
                  ),
                  totalCollateral = Some(Coin(60000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = Set.empty,
                  certificates = Set.empty,
                  requiredSigners = Set.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet(Set.empty)),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            collateralInput1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey1))
                        )
                    )
              ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey2))
                        )
                    )
              ),
              Value(Coin(30000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator totalSumOfCollateralCoinsIsEquivalentToTotalCollateral rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val collateralInput1 = Arbitrary.arbitrary[TransactionInput].sample.get
        val collateralInput2 = Arbitrary.arbitrary[TransactionInput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set(collateralInput1, collateralInput2),
                  collateralReturnOutput = Some(
                    TransactionOutput.Shelley(
                      Address.Shelley(
                        Arbitrary.arbitrary[ShelleyAddress].sample.get
                      ),
                      Value(Coin(20000000L))
                    )
                  ),
                  totalCollateral = Some(Coin(50000000L)),
                  fee = Coin(10000000L),
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = Set.empty,
                  certificates = Set.empty,
                  requiredSigners = Set.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet(Set.empty)),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State(
          utxo = Map(
            collateralInput1 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey1))
                        )
                    )
              ),
              Value(Coin(30000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Address.Shelley(
                Arbitrary
                    .arbitrary[ShelleyAddress]
                    .sample
                    .get
                    .copy(payment =
                        ShelleyPaymentPart.keyHash(
                          Hash(summon[PlatformSpecific].blake2b_224(publicKey2))
                        )
                    )
              ),
              Value(Coin(30000000L))
            )
          )
        )

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("FeesOkValidator isAtLeastOneCollateralInput rule failure") {
        given Arbitrary[scalus.builtin.Data] = Arbitrary(
          Gen.const(scalus.builtin.Data.unit) // Simplified for testing
        )

        val context = Context()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set.empty,
                  collateralInputs = Set.empty,
                  collateralReturnOutput = None,
                  totalCollateral = None,
                  fee = Coin(10000000L),
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq.empty,
                  mint = None,
                  votingProcedures = None,
                  withdrawals = None,
                  proposalProcedures = Set.empty,
                  certificates = Set.empty,
                  requiredSigners = Set.empty
                )
              ),
              auxiliaryData = None,
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet(Set.empty)),
                redeemers = Some(
                  KeepRaw(
                    Redeemers.Array(
                      IndexedSeq(
                        Redeemer(
                          tag = Arbitrary.arbitrary[RedeemerTag].sample.get,
                          index = Gen.choose(0, Int.MaxValue).sample.get,
                          data = scalus.builtin.Data.unit,
                          exUnits = Arbitrary.arbitrary[ExUnits].sample.get
                        )
                      )
                    )
                  )
                )
              )
            )
        }

        val state = State()

        val result = FeesOkValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("TransactionSizeValidator rule success") {
        val context = Context()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = None
              ),
              auxiliaryData = None,
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set(Arbitrary.arbitrary[TransactionInput].sample.get),
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq(
                    TransactionOutput.Shelley(
                      Address.Byron(Arbitrary.arbitrary[ByronAddress].sample.get),
                      Value(Coin(1000000L))
                    )
                  ),
                  votingProcedures = None,
                  proposalProcedures = Set.empty,
                  withdrawals = None,
                  certificates = Set.empty,
                  mint = None,
                  requiredSigners = Set.empty,
                  collateralReturnOutput = None
                )
              )
            )
        }

        val result = TransactionSizeValidator.validate(context, State(), transaction)
        assert(result.isRight)
    }

    test("TransactionSizeValidator rule failure") {
        val context = Context()
        val inputs = Set.fill(1000) { // Arbitrary large number of inputs
            Arbitrary.arbitrary[TransactionInput].sample.get
        }

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = inputs
                )
              )
            )
        }

        val result = TransactionSizeValidator.validate(context, State(), transaction)
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
