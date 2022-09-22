package scalus.flat

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.utils.Utils

import scala.util.Random

class FlatSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
  test("Flat bits") {
    val enc = new EncoderState(3)
    enc.bits(7, 64)
//    println(enc)
    enc.bits(5, 22)
//    println(enc)
    enc.bits(6, 47)
    enc.nextWord()
    val dec = new DecoderState(enc.buffer)
    assert(dec.bits8(7) == 64)
    assert(dec.bits8(5) == 22)
    assert(dec.bits8(6) == 47)
  }

  test("encode/decode random bytes") {
    val gen = Gen.nonEmptyListOf(Arbitrary.arbitrary[Byte])
    implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

    forAll(gen) { bs =>
      val bitlens =
        bs.map(b =>
          if b == 0 then 1
          else if b < 0 then 8
          else Integer.numberOfTrailingZeros(Integer.highestOneBit(b)) + 1
        )
      val enc = new EncoderState(bs.length + 1)
      val valuesWithBitSizes = bs.zip(bitlens)
//      println(valuesWithBitSizes)
      valuesWithBitSizes.foreach { case (vl, sz) => enc.bits(sz, vl) }
      enc.nextWord()
//      println(enc)
      val dec = new DecoderState(enc.buffer)
      valuesWithBitSizes.foreach { case (vl, sz) =>
        val decodedValue = dec.bits8(sz)
        assert(decodedValue == vl)
      }
    }
  }

  test("encode/decode Array[Byte]") {
    val fl = summon[Flat[Array[Byte]]]

    {
      val arr = Array.empty[Byte]
      assert(fl.bitSize(arr) == 16)
      val enc = EncoderState(2)
      fl.encode(arr, enc)
      assert(Utils.bytesToHex(enc.result) == "0100")
      val dec = DecoderState(enc.result)
      assert(fl.decode(dec).length == 0)
    }

    {
      val arr = Array[Byte](11, 22, 33)
      assert(fl.bitSize(arr) == 5 * 8)
      val enc = EncoderState(6)
      fl.encode(arr, enc)
      assert(Utils.bytesToHex(enc.result) == "01030B162100")
      val dec = DecoderState(enc.result)
      assert(Utils.bytesToHex(fl.decode(dec)) == "0B1621")
    }

    def check(n: Int) =
      val arr = new Array[Byte](n)
      Random.nextBytes(arr)
      val enc = EncoderState(fl.bitSize(arr) / 8 + 1)
      fl.encode(arr, enc)
      val result = enc.result
      val dec = DecoderState(result)
      val decoded = fl.decode(dec)
      assert(decoded.sameElements(arr))

    check(1)
    check(255)
    check(256)
    check(510)
    check(Random.between(1, 3 * 255))
  }

  test("Zagzig/zigZag") {
    forAll { (n: BigInt) =>
      assert(zagZig(zigZag(n)) == n)
    }
  }

  test("encode/decode BigInt") {
    val fl = summon[Flat[BigInt]]
    assert(fl.bitSize(BigInt(0)) == 8)
    assert(fl.bitSize(BigInt(1)) == 8)
    assert(fl.bitSize(BigInt(-1)) == 8)
    assert(fl.bitSize(BigInt(2) << 120) == 144)

    def check(n: BigInt, encodedHex: String) =
      val enc = EncoderState(fl.bitSize(n) / 8 + 1)
      fl.encode(n, enc)
      assert(Utils.bytesToHex(enc.result) == encodedHex)
      val dec = DecoderState(enc.result)
      assert(fl.decode(dec) == n)

    check(BigInt(0), "00")
    check(BigInt(1), "02")
    check(BigInt(-1), "01")
    check(BigInt(64), "8001")
    check(BigInt(-80), "9F01")

    forAll { (n: BigInt) =>
      val enc = EncoderState(fl.bitSize(n) / 8 + 1)
      fl.encode(n, enc)
      val result = enc.result
      val dec = DecoderState(result)
      val decoded = fl.decode(dec)
      assert(decoded == n)
    }
  }

  test("encode/decode String") {
    val fl = summon[Flat[String]]
    assert(fl.bitSize("") == 16)
    assert(fl.bitSize("aaa") == 6 * 8)

    def check(n: String, encodedHex: String) =
      val enc = EncoderState(fl.bitSize(n) / 8 + 1)
      fl.encode(n, enc)
      assert(Utils.bytesToHex(enc.result) == encodedHex)
      val dec = DecoderState(enc.result)
      assert(fl.decode(dec) == n)

    check("", "0100")
    check("a", "01016100")
    check("Ї", "0102D08700")

    forAll { (n: String) =>
      val enc = EncoderState(fl.bitSize(n) / 8 + 1)
      fl.encode(n, enc)
      val result = enc.result
      val dec = DecoderState(result)
      val decoded = fl.decode(dec)
      assert(decoded == n)
    }
  }
