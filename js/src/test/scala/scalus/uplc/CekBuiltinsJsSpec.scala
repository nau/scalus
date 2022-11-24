package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.ledger.api.v1.*
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.{Bool, ByteString, asConstant}
import scalus.uplc.ExprBuilder.{sndPair, unConstrData}
import scalus.uplc.Program
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.utils.Utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.math.RoundingMode
import scala.collection.immutable
import scala.io.Source.fromFile
import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("child_process", JSImport.Namespace)
object ChildProcess extends js.Object {
  def execSync(command: String, options: js.UndefOr[js.Object] = js.undefined): js.Object | String =
    js.native
}

class CekBuiltinsJsSpec extends CekBuiltinsSpec:
  def eval(t: Term): Term =
    import js.Dynamic.global as g
    import js.DynamicImplicits.*

    val program = Program((1, 0, 0), t)
    val cp = g.require("child_process")
    val r = cp.spawnSync(
      "uplc",
      js.Array("evaluate"),
      js.Dynamic.literal(input = program.pretty.render(80))
    )
    if (r.status.asInstanceOf[Int] != 0) throw new Exception(r.stderr.toString)
//    println(r.stdout.toString())
    UplcParser.term.parse(r.stdout.toString()) match
      case Left(value)       => throw new Exception(s"Parse error: $value")
      case Right((_, value)) => value

  override def assertEvalEq(a: Term, b: Term): Unit =
    assert(eval(a) == b, s"$a != $b")
    assert(Cek.evalUPLC(a) == b, s"$a != $b")

  override def assertEvalThrows[A <: AnyRef: ClassTag](a: Term): Unit =
    assertThrows[A](Cek.evalUPLC(a))
