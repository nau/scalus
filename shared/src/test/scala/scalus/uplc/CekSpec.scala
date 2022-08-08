package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
class CekSpec extends AnyFunSuite {
  test("Scalus") {
    val h = Const(Constant(DefaultUniByteString, "Hello"))
    val id = LamAbs("x", Var("x"))
    val app = Apply(id, h)
    assert(Cek.evalUPLC(app) == h)
  }
}
