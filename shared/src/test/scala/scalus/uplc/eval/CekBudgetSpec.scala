package scalus.uplc
package eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.*
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.given
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*

class CekBudgetSpec extends AnyFunSuite:
    test("Budget") {
        val h = Const(asConstant("Hello"))
        val id = LamAbs("x", Var(NamedDeBruijn("x")))
        val app = Apply(id, h)
        val cek = CekMachine(scalus.builtin.given_PlatformSpecific)
        cek.evalUPLC(h)
        // assert(cek.evalUPLC(h) == h)
        println(s"${cek.budgetCPU} ${cek.budgetMemory}")
    }
