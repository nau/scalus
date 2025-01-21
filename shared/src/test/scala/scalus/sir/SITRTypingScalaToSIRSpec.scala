package scalus.sir

import org.scalatest.funsuite.AnyFunSuite

import scalus.Compiler.compile
import scalus.*
import scalus.builtin.given




object SITRTypingScalaToSIRSpecScope {

    case class ClassA1(a: BigInt)

}

class SITRTypingScalaToSIRSpec extends AnyFunSuite {

    test("check that simple case class is mapped to case class") {
        import SITRTypingScalaToSIRSpecScope.*

        val sir = compile {
            (x:BigInt) => new ClassA1(x)
        }

        sir.tp match {
            case SIRType.Fun(SIRType.Integer, SIRType.CaseClass(constrDecl, Nil)) =>
                assert(constrDecl.name == "ClassA1")
            case _ => fail(s"unexpected type ${sir.tp}")
        }

        //println(sir.pretty.render(100))

    }

}
