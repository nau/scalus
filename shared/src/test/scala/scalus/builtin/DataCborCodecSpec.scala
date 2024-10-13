package scalus.builtin

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtin
import scalus.builtin.ByteString.*
import scalus.builtin.Data.*
import scalus.uplc.ArbitraryInstances
import scalus.utils.Utils

import scala.collection.immutable

class DataCborCodecSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
    private def encodeHex(d: Data) = Utils.bytesToHex(Cbor.encode(d).toByteArray)
    private def decodeHex(d: String) = Cbor.decode(Utils.hexToBytes(d)).to[Data].value

    private def roundtrip(d: Data): Unit =
        val ba = Cbor.encode(d).toByteArray
        val dd = Cbor.decode(ba).to[Data].value
        assert(d == dd)

    test("BigInt encoding/decoding of < 64 bits") {
        val bigInt = BigInt("1234567890111213141516")
        val expected = "c24942ed123b08fe58fe0c"
        assert(encodeHex(I(bigInt)) == expected)
        assert(decodeHex(expected) == I(bigInt))
    }

    test("BigInt encoding/decoding of > 64 bits is chunked") {
        val twoTo520 = BigInt(2).pow(520)
        val expectedTwo =
            "c25f584001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000420000ff"
        assert(encodeHex(I(twoTo520)) == expectedTwo)
        assert(decodeHex(expectedTwo) == I(twoTo520))

        val minusTwoTo520 = -BigInt(2).pow(520)
        val expectedMinusTwo =
            "c35f584000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff42ffffff"
        assert(encodeHex(I(minusTwoTo520)) == expectedMinusTwo)
        assert(decodeHex(expectedMinusTwo) == I(minusTwoTo520))
    }

    test("All BigInt values are encoded/decoded correctly") {
        forAll { (bigInt: BigInt) =>
            roundtrip(I(bigInt))
        }
    }

    test("Encoding/decoding of Constrs") {
        assert(encodeHex(Constr(3, Constr(3, Nil) :: Nil)) == "d87c9fd87c80ff")
        assert(decodeHex("d8 7c 9f d8 7c 80 ff") == Constr(3, Constr(3, Nil) :: Nil))

        assert(encodeHex(List(Constr(3, Nil) :: Nil)) == "9fd87c80ff")
        assert(decodeHex("9f d8 7c 80 ff") == List(Constr(3, Nil) :: Nil))

        assert(encodeHex(List(Constr(7, Nil) :: Nil)) == "9fd9050080ff")
        assert(decodeHex("9f d9 05 00 80 ff") == List(Constr(7, Nil) :: Nil))

        assert(encodeHex(List(Constr(1234567890, Nil) :: Nil)) == "9fd866821a499602d280ff")
        assert(
          decodeHex("9f d8 66 82 1a 49 96 02 d2 80 ff") == List(Constr(1234567890, Nil) :: Nil)
        )
    }

    test("Encoding/decoding of Lists") {
        assert(
          encodeHex(
            List(
              I(0) :: I(-1) :: I(100) :: I(1000) :: I(BigInt("1234567890111213141516")) :: Nil
            ): Data
          ) == "9f002018641903e8c24942ed123b08fe58fe0cff"
        )
        assert(
          decodeHex("9f 00 20 18 64 19 03 e8 c2 49 42 ed 12 3b 08 fe 58 fe 0c ff") ==
              (List(
                I(0) :: I(-1) :: I(100) :: I(1000) :: I(BigInt("1234567890111213141516")) :: Nil
              ): Data)
        )
    }

    test("ByteString <= 64") {
        val longBs = "1234567890"
        val expected = "4a31323334353637383930"
        assert(encodeHex(B(builtin.ByteString.unsafeFromArray(longBs.getBytes))) == expected)
        assert(decodeHex(expected) == B(builtin.ByteString.unsafeFromArray(longBs.getBytes)))
    }

    test("ByteString chunks") {
        val longBs = "1234567890" * 7
        val expected =
            "5f58403132333435363738393031323334353637383930313233343536373839303132333435363738393031323334353637383930313233343536373839303132333446353637383930ff"
        assert(encodeHex(B(builtin.ByteString.fromArray(longBs.getBytes))) == expected)
        assert(decodeHex(expected) == B(builtin.ByteString.fromArray(longBs.getBytes)))
    }

    test("All ByteString values are encoded/decoded correctly") {
        forAll { (bs: ByteString) =>
            roundtrip(B(bs))
        }
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

    test("Encoder <-> Decoder roundtrip") {
        forAll { (d: Data) =>
            roundtrip(d)
        }
    }
