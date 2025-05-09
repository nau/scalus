package scalus.cardano.ledger

import io.bullet.borer.{Cbor, Decoder, Encoder}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.ledger.api.Timelock

class CborSerializationSpec extends AnyFunSuite, ScalaCheckPropertyChecks, ArbitraryInstances {
    test(s"Hash28 should serialize and deserialize correctly"):
        testSerializationRoundtrip[Hash28]()

    test(s"Hash32 should serialize and deserialize correctly"):
        testSerializationRoundtrip[Hash32]()

    test(s"AddrKeyHash should serialize and deserialize correctly"):
        testSerializationRoundtrip[AddrKeyHash]()

    test(s"Anchor should serialize and deserialize correctly"):
        testSerializationRoundtrip[Anchor]()

    test(s"Credential should serialize and deserialize correctly"):
        testSerializationRoundtrip[Credential]()

    test(s"Value should serialize and deserialize correctly"):
        testSerializationRoundtrip[Value]()

    test(s"DRep should serialize and deserialize correctly"):
        testSerializationRoundtrip[DRep]()

    test(s"GovActionId should serialize and deserialize correctly"):
        testSerializationRoundtrip[GovActionId]()

    test(s"OperationalCert should serialize and deserialize correctly"):
        testSerializationRoundtrip[OperationalCert]()

    test(s"PoolMetadata should serialize and deserialize correctly"):
        testSerializationRoundtrip[PoolMetadata]()

    test(s"DatumOption should serialize and deserialize correctly"):
        testSerializationRoundtrip[DatumOption]()

    test(s"Timelock should serialize and deserialize correctly"):
        testSerializationRoundtrip[Timelock]()

    test(s"Script should serialize and deserialize correctly"):
        testSerializationRoundtrip[Script]()

    test(s"ScriptRef should serialize and deserialize correctly"):
        testSerializationRoundtrip[ScriptRef]()

    test(s"TransactionInput should serialize and deserialize correctly"):
        testSerializationRoundtrip[TransactionInput]()

    test(s"TransactionOutput should serialize and deserialize correctly"):
        testSerializationRoundtrip[TransactionOutput]()

    // Helper method to test serialization/deserialization for a given type
    private def testSerializationRoundtrip[A: Arbitrary: Encoder: Decoder](): Unit = {
        forAll: (a: A) =>
            val encoded = Cbor.encode(a).toByteArray
            val decoded = Cbor.decode(encoded).to[A].value
            assert(a == decoded)
    }
}
