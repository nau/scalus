package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import scalus.builtin.platform
import scalus.cardano.address.{Address, StakeAddress, StakePayload}
import org.scalatest.funsuite.AnyFunSuite

class MissingKeyHashesValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("MissingKeyHashesValidator Inputs rule success") {
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
                  inputs = TaggedOrderedSet.from(Set(input1, input2)),
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet.empty,
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
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey1)))),
              Value(Coin(1000000L))
            ),
            input2 -> TransactionOutput.Shelley(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Inputs rule failure") {
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
                  inputs = TaggedOrderedSet.from(Set(input1, input2)),
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet.empty,
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
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                  ),
              Value(Coin(1000000L))
            ),
            input2 -> TransactionOutput.Shelley(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator CollateralInputs rule success") {
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
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.from(Set(collateralInput1, collateralInput2)),
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet.empty,
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
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                  ),
              Value(Coin(1000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey2))
                    )
                  ),
              Value(Coin(1000000L))
            )
          )
        )

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator CollateralInputs rule failure") {
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
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.from(Set(collateralInput1, collateralInput2)),
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet.empty,
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
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(
                    payload = StakePayload.Stake(
                      Hash(platform.blake2b_224(publicKey1))
                    )
                  ),
              Value(Coin(1000000L))
            ),
            collateralInput2 -> TransactionOutput.Shelley(
              Arbitrary
                  .arbitrary[StakeAddress]
                  .sample
                  .get
                  .copy(payload = StakePayload.Stake(Hash(platform.blake2b_224(publicKey2)))),
              Value(Coin(1000000L))
            )
          )
        )

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator VotingProcedures success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()
        val (privateKey3, publicKey3) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
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
                  certificates = TaggedSet.empty,
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

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator VotingProcedures failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()
        val (privateKey3, publicKey3) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
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
                  certificates = TaggedSet.empty,
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

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator Withdrawals rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet.empty,
                  withdrawals = Some(
                    Withdrawals(
                      Map(
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(
                                payload = StakePayload.Stake(
                                  Hash(platform.blake2b_224(publicKey1))
                                )
                              )
                        ) -> Coin(1000000L),
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(
                                payload = StakePayload.Stake(
                                  Hash(platform.blake2b_224(publicKey2))
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

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Withdrawals rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet.empty,
                  withdrawals = Some(
                    Withdrawals(
                      Map(
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(
                                payload = StakePayload.Stake(
                                  Hash(platform.blake2b_224(publicKey1))
                                )
                              )
                        ) -> Coin(1000000L),
                        RewardAccount(
                          Arbitrary
                              .arbitrary[StakeAddress]
                              .sample
                              .get
                              .copy(
                                payload = StakePayload.Stake(
                                  Hash(platform.blake2b_224(publicKey2))
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

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator Certificates rule success") {
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
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet(
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
                    Certificate.RegCert(credential, Arbitrary.arbitrary[Option[Coin]].sample.get),
                    Certificate.UnregCert(credential, Arbitrary.arbitrary[Option[Coin]].sample.get),
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

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator Certificates rule failure") {
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
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet(
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
                    Certificate.RegCert(credential, Arbitrary.arbitrary[Option[Coin]].sample.get),
                    Certificate.UnregCert(credential, Arbitrary.arbitrary[Option[Coin]].sample.get),
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

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("MissingKeyHashesValidator RequiredSigners rule success") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet.empty,
                  withdrawals = None,
                  requiredSigners = Set(
                    Hash(platform.blake2b_224(publicKey1)),
                    Hash(platform.blake2b_224(publicKey2))
                  )
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

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("MissingKeyHashesValidator RequiredSigners rule failure") {
        val context = Context()

        val (privateKey1, publicKey1) = generateKeyPair()
        val (privateKey2, publicKey2) = generateKeyPair()

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedOrderedSet.empty,
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  votingProcedures = None,
                  certificates = TaggedSet.empty,
                  withdrawals = None,
                  requiredSigners = Set(
                    Hash(platform.blake2b_224(publicKey1)),
                    Hash(platform.blake2b_224(publicKey2))
                  )
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

        val result = MissingKeyHashesValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
