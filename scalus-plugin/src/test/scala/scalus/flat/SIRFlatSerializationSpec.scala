package scalus.flat

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.flat.Flat.given
import scalus.flat.FlatInstantces.given
import scalus.flat.Flat.EncoderState
import scalus.flat.Flat.DecoderState
import scalus.flat.Flat.Flat
import scalus.sir.SIR


class SIRFlatSerializationSpec extends AnyFunSuite with ScalaCheckPropertyChecks:

  test("serialize and deserialize SIR") {
    val fl = summon[Flat[SIR]]
    val sir = SIR.Const(scalus.uplc.Constant.Integer(23))
    val enc = EncoderState(fl.bitSize(sir) / 8 + 1)
    Flat.encode(sir, enc)
    enc.filler()
    println(enc.buffer.mkString(", "))
    val dec = DecoderState(enc.buffer)
    val sir2 = Flat.decode[SIR](dec)
    assert(sir == sir2)
  }
