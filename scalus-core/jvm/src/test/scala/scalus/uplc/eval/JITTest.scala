package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.Compiler.compile
import scalus.uplc.Term

import scala.util.Try

class JITTest extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("UPLC JIT compilation works") {
        val uplc: Term = compile:
            ((i: BigInt) => if i > 0 then throw new Exception("Not implemented") else i + 1)(2)
        .toUplc(true)

        println(uplc.showHighlighted)
        println(uplc.evaluateDebug)
        val logger = Log()
        val r = Try(JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams))
        println(r)
        println(logger.getLogs.mkString)
    }
}
