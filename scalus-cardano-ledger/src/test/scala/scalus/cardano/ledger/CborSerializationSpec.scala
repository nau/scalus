package scalus.cardano.ledger

import io.bullet.borer.{Cbor, Decoder, Encoder}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.ledger.api.Timelock

class CborSerializationSpec extends AnyFunSuite, ScalaCheckPropertyChecks, ArbitraryInstances {

    checkCborSerialization[Hash28]()
    checkCborSerialization[Hash32]()
    checkCborSerialization[AddrKeyHash]()
    checkCborSerialization[Anchor]()
    checkCborSerialization[Credential]()
    checkCborSerialization[Value]()
    checkCborSerialization[DRep]()
    checkCborSerialization[GovActionId]()
    checkCborSerialization[OperationalCert]()
    checkCborSerialization[PoolMetadata]()
    checkCborSerialization[DatumOption]()
    checkCborSerialization[Timelock]()
    checkCborSerialization[Script]()
    checkCborSerialization[ScriptRef]()
    checkCborSerialization[TransactionInput]()
    checkCborSerialization[TransactionOutput]()

    private inline def checkCborSerialization[A: Manifest: Arbitrary: Encoder: Decoder](): Unit = {
        test(
          s"${manifest[A].runtimeClass.getSimpleName} should serialize and deserialize correctly"
        ) {
            forAll { (a: A) =>
                val encoded = Cbor.encode(a).toByteArray
                val decoded = Cbor.decode(encoded).to[A].value
                assert(a == decoded)
            }
        }
    }
}
