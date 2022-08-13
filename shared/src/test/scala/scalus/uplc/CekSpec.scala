package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DefaultUni.ByteString
import scalus.uplc.Term.*

import java.io.ByteArrayInputStream
class CekSpec extends AnyFunSuite:
  test("Scalus") {
    val h = Const(Constant(ByteString, "Hello"))
    val id = LamAbs("x", Var("x"))
    val app = Apply(id, h)
    assert(Cek.evalUPLC(app) == h)
  }

  def runUPLC(code: String): String = {
    import scala.sys.process.*
    val cmd = "/Users/nau/projects/scalus/uplc evaluate"
    cmd.#<(new ByteArrayInputStream(code.getBytes("UTF-8"))).!!
  }

  test("UPLC") {
//    println(runUPLC("(program 1.0.0 [(builtin addInteger) (con bool True)])"))
    println(
      runUPLC(
        "(program 1.0.0 [[[(force (builtin ifThenElse)) (con bool False)] (con bool True)] (con integer 1)])"
      )
    )

    println(
      runUPLC(
        "(program 1.0.0 (con (list bool) []))"
      )
    )

    /*println(
      runUPLC(
        "(program 1.0.0 [[(force (builtin mkCons)) (con integer 1)] (con (list bool) [])])"
      )
    )*/
    println(
      runUPLC(
        "(program 1.0.0 [ (force(force (builtin chooseList))) (con (list bool) []) ])"
      )
    )
    println(
      runUPLC(
        """(program 1.0.0 
          |[
          | [
          |   [ (force(force (builtin chooseList))) (con (list bool) [True]) 
          |   ] 
          |   (con integer 12) 
          | ] 
          | (con unit ()) 
          |])""".stripMargin
      )
    )
  }
