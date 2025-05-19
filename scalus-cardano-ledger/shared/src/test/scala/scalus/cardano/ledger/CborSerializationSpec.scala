package scalus.cardano.ledger

import io.bullet.borer.{Cbor, Decoder, Encoder}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.ledger.api.Timelock

class CborSerializationSpec extends AnyFunSuite, ScalaCheckPropertyChecks, ArbitraryInstances {
    test(s"Hash28 should serialize and deserialize correctly"):
        testSerializationRoundTrip[Hash28]()

    test(s"Hash32 should serialize and deserialize correctly"):
        testSerializationRoundTrip[Hash32]()

    test(s"Coin should serialize and deserialize correctly"):
        testSerializationRoundTrip[Coin]()

    test(s"ScriptHash should serialize and deserialize correctly"):
        testSerializationRoundTrip[ScriptHash]()

    test(s"PolicyId should serialize and deserialize correctly"):
        testSerializationRoundTrip[PolicyId]()

    test(s"AssetName should serialize and deserialize correctly"):
        testSerializationRoundTrip[AssetName]()

    test(s"Mint should serialize and deserialize correctly"):
        testSerializationRoundTrip[Mint]()

    test(s"Language should serialize and deserialize correctly"):
        testSerializationRoundTrip[Language]()

    test(s"Address should serialize and deserialize correctly"):
        testSerializationRoundTrip[Address]()

    test(s"Slot should serialize and deserialize correctly"):
        testSerializationRoundTrip[Slot]()

    test(s"AuxiliaryDataHash should serialize and deserialize correctly"):
        testSerializationRoundTrip[AuxiliaryDataHash]()

    test(s"ScriptDataHash should serialize and deserialize correctly"):
        testSerializationRoundTrip[ScriptDataHash]()

    test(s"ExUnits should serialize and deserialize correctly"):
        testSerializationRoundTrip[ExUnits]()

    test(s"ExUnitPrices should serialize and deserialize correctly"):
        testSerializationRoundTrip[ExUnitPrices]()

    test(s"CostModels should serialize and deserialize correctly"):
        testSerializationRoundTrip[CostModels]()

    test(s"Constitution should serialize and deserialize correctly"):
        testSerializationRoundTrip[Constitution]()

    test(s"AddrKeyHash should serialize and deserialize correctly"):
        testSerializationRoundTrip[AddrKeyHash]()

    test(s"Anchor should serialize and deserialize correctly"):
        testSerializationRoundTrip[Anchor]()

    test(s"Credential should serialize and deserialize correctly"):
        testSerializationRoundTrip[Credential]()

    test(s"Value should serialize and deserialize correctly"):
        testSerializationRoundTrip[Value]()

    test(s"DRep should serialize and deserialize correctly"):
        testSerializationRoundTrip[DRep]()

    test(s"GovActionId should serialize and deserialize correctly"):
        testSerializationRoundTrip[GovActionId]()

    test(s"OperationalCert should serialize and deserialize correctly"):
        testSerializationRoundTrip[OperationalCert]()

    test(s"PoolMetadata should serialize and deserialize correctly"):
        testSerializationRoundTrip[PoolMetadata]()

    test(s"DatumOption should serialize and deserialize correctly"):
        testSerializationRoundTrip[DatumOption]()

    test(s"Timelock should serialize and deserialize correctly"):
        testSerializationRoundTrip[Timelock]()

    test(s"Script should serialize and deserialize correctly"):
        testSerializationRoundTrip[Script]()

    test(s"ScriptRef should serialize and deserialize correctly"):
        testSerializationRoundTrip[ScriptRef]()

    test(s"TransactionInput should serialize and deserialize correctly"):
        testSerializationRoundTrip[TransactionInput]()

    test(s"TransactionOutput should serialize and deserialize correctly"):
        testSerializationRoundTrip[TransactionOutput]()

    test(s"ProtocolVersion should serialize and deserialize correctly"):
        testSerializationRoundTrip[ProtocolVersion]()

    test(s"RewardAccount should serialize and deserialize correctly"):
        testSerializationRoundTrip[RewardAccount]()

    test(s"NonNegativeInterval should serialize and deserialize correctly"):
        testSerializationRoundTrip[NonNegativeInterval]()

    test(s"VrfCert should serialize and deserialize correctly"):
        testSerializationRoundTrip[VrfCert]()

    test(s"BlockHeaderBody should serialize and deserialize correctly"):
        testSerializationRoundTrip[BlockHeaderBody]()

    test(s"BlockHeader should serialize and deserialize correctly"):
        testSerializationRoundTrip[BlockHeader]()

    test(s"TransactionMetadatumLabel should serialize and deserialize correctly"):
        testSerializationRoundTrip[TransactionMetadatumLabel]()

    test(s"TransactionMetadatum should serialize and deserialize correctly"):
        testSerializationRoundTrip[TransactionMetadatum]()

    test(s"AuxiliaryData should serialize and deserialize correctly"):
        testSerializationRoundTrip[AuxiliaryData]()

    test(s"VKeyWitness should serialize and deserialize correctly"):
        testSerializationRoundTrip[VKeyWitness]()

    test(s"BootstrapWitness should serialize and deserialize correctly"):
        testSerializationRoundTrip[BootstrapWitness]()

    test(s"RedeemerTag should serialize and deserialize correctly"):
        testSerializationRoundTrip[RedeemerTag]()

    test(s"Redeemer should serialize and deserialize correctly"):
        testSerializationRoundTrip[Redeemer]()

    test(s"TransactionWitnessSet should serialize and deserialize correctly"):
        testSerializationRoundTrip[TransactionWitnessSet]()

    test(s"UnitInterval should serialize and deserialize correctly"):
        testSerializationRoundTrip[UnitInterval]()

    test(s"PoolVotingThresholds should serialize and deserialize correctly"):
        testSerializationRoundTrip[PoolVotingThresholds]()

    test(s"DRepVotingThresholds should serialize and deserialize correctly"):
        testSerializationRoundTrip[DRepVotingThresholds]()

    test(s"ProtocolParamUpdate should serialize and deserialize correctly"):
        testSerializationRoundTrip[ProtocolParamUpdate]()

    test(s"Vote should serialize and deserialize correctly"):
        testSerializationRoundTrip[Vote]()

    test(s"Voter should serialize and deserialize correctly"):
        testSerializationRoundTrip[Voter]()

    test(s"VotingProcedure should serialize and deserialize correctly"):
        testSerializationRoundTrip[VotingProcedure]()

    test(s"VotingProcedures should serialize and deserialize correctly"):
        testSerializationRoundTrip[VotingProcedures]()

//    test(s"GovAction should serialize and deserialize correctly"):
//        testSerializationRoundTrip[GovAction]()

//    test(s"ProposalProcedure should serialize and deserialize correctly"):
//        testSerializationRoundTrip[ProposalProcedure]()

    // Helper method to test serialization/deserialization for a given type
    private def testSerializationRoundTrip[A: Arbitrary: Encoder: Decoder](): Unit = {
        forAll: (a: A) =>
            val encoded = Cbor.encode(a).toByteArray
            val decoded = Cbor.decode(encoded).to[A].value
            assert(a == decoded)
    }
}
