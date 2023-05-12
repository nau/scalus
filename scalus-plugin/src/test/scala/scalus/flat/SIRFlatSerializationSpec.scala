package scalus.flat

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.flat
import scalus.flat.given
import scalus.flat.FlatInstantces.given
import scalus.flat.EncoderState
import scalus.flat.DecoderState
import scalus.flat.Flat
import scalus.sir.SIR

class SIRFlatSerializationSpec extends AnyFunSuite with ScalaCheckPropertyChecks:

  test("serialize and deserialize SIR") {
    val fl = summon[Flat[SIR]]
    val sir = SIR.Const(scalus.uplc.Constant.Integer(23))
    val enc = EncoderState(fl.bitSize(sir) / 8 + 1)
    flat.encode(sir, enc)
    enc.filler()
    val dec = DecoderState(enc.buffer)
    val sir2 = flat.decode[SIR](dec)
    assert(sir == sir2)
  }
