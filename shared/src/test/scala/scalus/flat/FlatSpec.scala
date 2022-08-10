package scalus.flat

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

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
