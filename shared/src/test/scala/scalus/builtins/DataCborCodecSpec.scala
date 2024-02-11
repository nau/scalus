package scalus.builtins

import io.bullet.borer.{Cbor, Decoder, Encoder}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtins
import scalus.builtins.ByteString.given
import scalus.builtins.Data.*
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
          encodeAsHexString(Constr(3, Constr(3, Nil) :: Nil)) == "D87C9FD87C80FF"
        )

        assert(
          Cbor.decode(Utils.hexToBytes("D8 7C 9F D8 7C 80 FF")).to[Data].value == Constr(
            3,
            Constr(3, Nil) :: Nil
          )
        )

        assert(
          encodeAsHexString(List(Constr(3, Nil) :: Nil)) == "9FD87C80FF"
        )
        assert(
          encodeAsHexString(List(Constr(7, Nil) :: Nil)) == "9FD9050080FF"
        )
        assert(
          encodeAsHexString(
            List(Constr(1234567890, Nil) :: Nil)
          ) == "9FD866821A499602D280FF"
        )
        assert(
          encodeAsHexString(
            List(
              I(0) :: I(-1) :: I(100) :: I(1000) :: I(BigInt("1234567890111213141516")) :: Nil
            ): Data
          ) == "9F002018641903E8C24942ED123B08FE58FE0CFF"
        )
        assert(
          encodeAsHexString(B(builtins.ByteString.unsafeFromArray("12".getBytes))) == "423132"
        )
    }

    test("ByteString < 64") {
        val longBs = "1234567890"
        val expected =
            "4A31323334353637383930"
        assert(
          encodeAsHexString(B(builtins.ByteString.unsafeFromArray(longBs.getBytes))) == expected
        )
        assert(
          Cbor.decode(Utils.hexToBytes(expected)).to[Data].value == B(
            builtins.ByteString.unsafeFromArray(longBs.getBytes)
          )
        )
    }
    test("ByteString chunks") {
        val longBs = "1234567890" * 7
        val expected =
            "5F58403132333435363738393031323334353637383930313233343536373839303132333435363738393031323334353637383930313233343536373839303132333446353637383930FF"
        assert(
          encodeAsHexString(B(builtins.ByteString.unsafeFromArray(longBs.getBytes))) == expected
        )
        assert(
          Cbor.decode(Utils.hexToBytes(expected)).to[Data].value == B(
            builtins.ByteString.unsafeFromArray(longBs.getBytes)
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
                          hex"95800C7F4D004080660000DEC201FFD1FF01FF9D00CE9058267FA001807F62BD7F7F80BAFFBCCF567FFFBB06FF9B7F4EFF39807FC7010B0001FFCA00D0C1F8"
                        )
                      ),
                      (
                        Map(Nil),
                        B(hex"4F0138E6010000B2017F7F01DE01")
                      )
                    )
                  ),
                  B(
                    hex"6A017FD28A7F007FFF80009CB6997F8BBCB2F080FF97FF0ECA0A0142FFFF007F808480FFBD6C5E606580AB6101FFFFFBA13B7F7F0101DBFFFF62"
                  )
                )
              )
            )
        roundtrip(data)
    }
