package scalus.ledger

import io.bullet.borer.{Cbor, Decoder, Encoder}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CborSerializationSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {

    checkCborSerialization[Hash28]()
    checkCborSerialization[Hash32]()
    checkCborSerialization[AddrKeyHash]()
    checkCborSerialization[Anchor]()
    checkCborSerialization[Credential]()

    private inline def checkCborSerialization[A: Manifest: Arbitrary: Encoder: Decoder](): Unit = {
        test(
            s"${manifest[A].runtimeClass.getSimpleName} should serialize and deserialize correctly"
        ) {
            forAll { (a: A) =>
                val encoded = Cbor.encode(a).toByteArray
                val decoded = Cbor.decode(encoded).to[A].value
                println(s"a: $a, encoded: ${encoded.toSeq}, decoded: $decoded")
                assert(a == decoded)
            }
        }
    }
}
