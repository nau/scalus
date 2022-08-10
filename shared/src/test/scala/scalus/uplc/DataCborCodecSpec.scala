package scalus.uplc

import io.bullet.borer.{Cbor, Decoder, Encoder}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.Data.*
import scalus.utils.Utils.{StringInterpolators, bytesToHex}

import scala.util.control.NonFatal

class DataCborCodecSpec extends AnyFunSuite with ScalaCheckPropertyChecks:

  implicit val plutusDataCborEncoder: Encoder[Data] = PlutusDataCborEncoder
  implicit val plutusDataCborDecoder: Decoder[Data] = PlutusDataCborDecoder

  implicit val iArb: Arbitrary[I] = Arbitrary(Arbitrary.arbitrary[BigInt].map(l => I(l)))
  implicit val bArb: Arbitrary[B] = Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(B.apply))
  implicit val arbData: Arbitrary[Data] = Arbitrary {
    def constrGen(sz: Int): Gen[Constr] = for
      c <- Arbitrary.arbitrary[Long].map(Math.abs)
      n <- Gen.choose(sz / 3, sz / 2)
      args <- Gen.listOfN(n, sizedTree(sz / 2))
    yield Constr(c, args)
    def listGen(sz: Int): Gen[List] = for
      n <- Gen.choose(sz / 3, sz / 2)
      args <- Gen.listOfN(n, sizedTree(sz / 2))
    yield List(args)
    def mapGen(sz: Int): Gen[Map] = for
      n <- Gen.choose(sz / 3, sz / 2)
      tuple = Gen.zip(sizedTree(sz / 2), sizedTree(sz / 2))
      args <- Gen.mapOfN(n, tuple)
    yield Map(args.toList)
    def sizedTree(sz: Int): Gen[Data] =
      if sz <= 0 then Gen.oneOf(iArb.arbitrary, bArb.arbitrary)
      else
        Gen.frequency(
          (1, iArb.arbitrary),
          (1, bArb.arbitrary),
          (3, Gen.oneOf(constrGen(sz), listGen(sz), mapGen(sz)))
//          (3, listGen(sz))
//          (3, constrGen(sz))
//          (3, mapGen(sz))
        )
    Gen.sized(sizedTree)
  }

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

    def encodeAsHexString(d: Data) = bytesToHex(Cbor.encode(d).toByteArray)

    assert(
      encodeAsHexString(Constr(3, Constr(3, Nil) :: Nil)) == "D87C9FD87C80FF"
    )

    assert(
      Cbor.decode(hex"D8 7C 9F D8 7C 80 FF").to[Data].value == Constr(3, Constr(3, Nil) :: Nil)
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
      encodeAsHexString(B("12".getBytes)) == "423132"
    )
  }
