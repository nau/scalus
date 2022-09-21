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
      val enc = EncoderState(2)
      fl.encode(arr, enc)
      assert(Utils.bytesToHex(enc.result) == "0100")
      val dec = DecoderState(enc.result)
      assert(fl.decode(dec).length == 0)
    }

    {
      val arr = Array[Byte](11, 22, 33)
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
