package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.{Cbor, Codec}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.utils.Hex.*

class KeepRawTest extends AnyFunSuite, ScalaCheckPropertyChecks {
    test(s"KeepRaw should equal to itself") {
        case class Body(a: Int) derives Codec
        val body1 = Body(100)
        val body2 = Body(200)
        val kr1 = KeepRaw(body1)
        val kr2 = KeepRaw(body2)
        assert(kr1 == kr1)
        assert(kr2 == kr2)
        assert(kr1 != kr2)
        assert(kr2 != kr1)
    }

    test(s"KeepRaw should serialize and deserialize correctly") {
        case class Body(a: Int) derives Codec
        val bodies = Seq(Body(100), Body(200), Body(300))
        given encoded: OriginalCborByteArray =
            OriginalCborByteArray(Cbor.encode(bodies).toByteArray)
        val decoded = Cbor.decode(encoded: Array[Byte]).to[Seq[KeepRaw[Body]]].value
        val decodedBodies = decoded.map(_.value)
        assert(encoded.toHex == "9f186418c819012cff")
        assert(decodedBodies == bodies)
        assert(decoded(0).raw.toHex == "1864")
        assert(decoded(1).raw.toHex == "18c8")
        assert(decoded(2).raw.toHex == "19012c")
        val reencoded = Cbor.encode(decoded).toByteArray
        assert(reencoded.toHex == encoded.toHex)
    }
}
