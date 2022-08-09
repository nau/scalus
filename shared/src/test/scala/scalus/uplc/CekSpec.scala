package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DefaultUni.ByteString
import scalus.uplc.Term.*
class CekSpec extends AnyFunSuite {
  test("Scalus") {
    val h = Const(Constant(ByteString, "Hello"))
    val id = LamAbs("x", Var("x"))
    val app = Apply(id, h)
    assert(Cek.evalUPLC(app) == h)
  }
}
