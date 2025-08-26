package scalus.cardano.ledger
package rules

import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import org.scalacheck.Arbitrary
import scalus.ledger.api.Timelock
import scalus.builtin.platform
import org.scalatest.funsuite.AnyFunSuite

class NativeScriptsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
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
        val allOfTimelock = Timelock.AllOf(IndexedSeq(signatureTimelock1, signatureTimelock2))
        val anyOfTimelock =
            Timelock.AnyOf(IndexedSeq(signatureTimelock1, signatureTimelock2, signatureTimelock3))
        val mOfTimelock =
            Timelock.MOf(2, IndexedSeq(signatureTimelock1, signatureTimelock2, signatureTimelock3))
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
                  inputs = TaggedOrderedSet.from(Set(input1, input2, input3, input4, input5)),
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
                ).map(Script.Native.apply)
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
                  signatureTimelock1Address,
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                input2 -> TransactionOutput.Babbage(
                  signatureTimelock2Address,
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                input3 -> TransactionOutput.Babbage(
                  allOfTimelockAddress,
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                input4 -> TransactionOutput.Babbage(
                  anyOfTimelockAddress,
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                input5 -> TransactionOutput.Babbage(
                  mOfTimelockAddress,
                  Value(Coin(1000L)),
                  None,
                  None
                ),
                referenceInput1 -> TransactionOutput
                    .Babbage(
                      timeStartTimelockAddress,
                      Value(Coin(1000L)),
                      None,
                      Some(ScriptRef(Script.Native(timeStartTimelock)))
                    ),
                referenceInput2 -> TransactionOutput
                    .Babbage(
                      timeExpireTimelockAddress,
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
                  inputs = TaggedOrderedSet.from(Set(input1, input2)),
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  validityStartSlot = Some(10),
                  ttl = Some(25)
                )
              ),
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                nativeScripts = Set(Script.Native(timeExpireTimelock))
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
                      timeStartTimelockAddress,
                      Value(Coin(1000L)),
                      None,
                      Some(ScriptRef(Script.Native(timeStartTimelock)))
                    ),
                input2 -> TransactionOutput
                    .Babbage(
                      timeExpireTimelockAddress,
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
}
