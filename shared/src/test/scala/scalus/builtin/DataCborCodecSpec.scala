package scalus.builtin

import io.bullet.borer.{Cbor, Decoder, Encoder}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin
import scalus.builtin.ByteString.given
import scalus.builtin.Data.*
import scalus.uplc.ArbitraryInstances
import scalus.utils.Utils

import scala.collection.immutable

class DataCborCodecSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:

    implicit val plutusDataCborEncoder: Encoder[Data] = PlutusDataCborEncoder
    implicit val plutusDataCborDecoder: Decoder[Data] = PlutusDataCborDecoder

    def encodeAsHexString(d: Data) = Utils.bytesToHex(Cbor.encode(d).toByteArray)
    def roundtrip(d: Data): Unit =
        val ba = Cbor.encode(d).toByteArray
//    println(s"$d => ${ba.map("%02X" format _).mkString(" ")}")
        val dd = Cbor.decode(ba).to[Data].value
        //      println(s"$dd")
        assert(d == dd)

    test("Encoder <-> Decoder") {
        forAll { (d: Data) =>
            roundtrip(d)
        }
    }

    test("PlutusDataCborEncoder") {

        // byte array to hex string

        assert(
          encodeAsHexString(Constr(3, Constr(3, Nil) :: Nil)) == "d87c9fd87c80ff"
        )

        assert(
          Cbor.decode(Utils.hexToBytes("d8 7c 9f d8 7c 80 ff")).to[Data].value == Constr(
            3,
            Constr(3, Nil) :: Nil
          )
        )

        assert(
          encodeAsHexString(List(Constr(3, Nil) :: Nil)) == "9fd87c80ff"
        )
        assert(
          encodeAsHexString(List(Constr(7, Nil) :: Nil)) == "9fd9050080ff"
        )
        assert(
          encodeAsHexString(
            List(Constr(1234567890, Nil) :: Nil)
          ) == "9fd866821a499602d280ff"
        )
        assert(
          encodeAsHexString(
            List(
              I(0) :: I(-1) :: I(100) :: I(1000) :: I(BigInt("1234567890111213141516")) :: Nil
            ): Data
          ) == "9f002018641903e8c24942ed123b08fe58fe0cff"
        )
        assert(
          encodeAsHexString(B(builtin.ByteString.unsafeFromArray("12".getBytes))) == "423132"
        )
    }

    test("ByteString < 64") {
        val longBs = "1234567890"
        val expected =
            "4a31323334353637383930"
        assert(
          encodeAsHexString(B(builtin.ByteString.unsafeFromArray(longBs.getBytes))) == expected
        )
        assert(
          Cbor.decode(Utils.hexToBytes(expected)).to[Data].value == B(
            builtin.ByteString.unsafeFromArray(longBs.getBytes)
          )
        )
    }
    test("ByteString chunks") {
        val longBs = "1234567890" * 7
        val expected =
            "5f58403132333435363738393031323334353637383930313233343536373839303132333435363738393031323334353637383930313233343536373839303132333446353637383930ff"
        assert(
          encodeAsHexString(B(builtin.ByteString.unsafeFromArray(longBs.getBytes))) == expected
        )
        assert(
          Cbor.decode(Utils.hexToBytes(expected)).to[Data].value == B(
            builtin.ByteString.unsafeFromArray(longBs.getBytes)
          )
        )
    }

    test("Map can have duplicate keys") {
        val data =
            Map(
              immutable.List(
                (
                  Map(
                    immutable.List(
                      (
                        Map(Nil),
                        B(
                          hex"95800c7f4d004080660000dec201ffd1ff01ff9d00ce9058267fa001807f62bd7f7f80baffbccf567fffbb06ff9b7f4eff39807fc7010b0001ffca00d0c1f8"
                        )
                      ),
                      (
                        Map(Nil),
                        B(hex"4f0138e6010000b2017f7f01de01")
                      )
                    )
                  ),
                  B(
                    hex"6a017fd28a7f007fff80009cb6997f8bbcb2f080ff97ff0eca0a0142ffff007f808480ffbd6c5e606580ab6101fffffba13b7f7f0101dbffff62"
                  )
                )
              )
            )
        roundtrip(data)
    }
