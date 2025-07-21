package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import scalus.ledger.api.Timelock
import scalus.builtin.platform
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.TreeMap

class MissingOrExtraScriptHashesValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("MissingOrExtraScriptHashesValidator rule success") {
        val context = Context()

        val (privateKey, publicKey) = generateKeyPair()

        val nativeScript =
            Timelock.Signature(Hash(platform.blake2b_224(publicKey)))
        val plutusV1Script = Arbitrary.arbitrary[Script.PlutusV1].sample.get
        val plutusV2Script = Arbitrary.arbitrary[Script.PlutusV2].sample.get
        val plutusV3Script = Arbitrary.arbitrary[Script.PlutusV3].sample.get

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
                    Mint(
                      MultiAsset(
                        TreeMap(
                          plutusV1Script.scriptHash -> TreeMap.empty,
                          plutusV2Script.scriptHash -> TreeMap.empty,
                          plutusV3Script.scriptHash -> TreeMap.empty
                        )
                      )
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
                          Arbitrary
                              .arbitrary[ShelleyAddress]
                              .sample
                              .get
                              .copy(
                                payment = ShelleyPaymentPart.Script(nativeScript.scriptHash)
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
                  certificates = TaggedSet(
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
                    Certificate.RegCert(credential1, Arbitrary.arbitrary[Option[Coin]].sample.get),
                    Certificate
                        .UnregCert(credential2, Arbitrary.arbitrary[Option[Coin]].sample.get),
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
              Arbitrary
                  .arbitrary[ShelleyAddress]
                  .sample
                  .get
                  .copy(
                    payment = ShelleyPaymentPart.Script(nativeScript.scriptHash)
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

        val result = MissingOrExtraScriptHashesValidator.validate(context, state, transaction)
        assert(result.isRight)
    }
}
