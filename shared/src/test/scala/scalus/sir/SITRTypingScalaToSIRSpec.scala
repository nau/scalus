package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.Compiler.compileDebug
import scalus.*
import scalus.builtin.given

object SITRTypingScalaToSIRSpecScope {

    case class ClassA1(a: BigInt)

}

class SITRTypingScalaToSIRSpec extends AnyFunSuite {

    /*
    test("check that simple case class is mapped to case class in fun") {
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

     */

    test("check that simple case class is mapped to case class") {
        import SITRTypingScalaToSIRSpecScope.*

        val sir = compile {
            new ClassA1(10)
        }

        sir.tp match {
            case SIRType.CaseClass(constrDecl, Nil) =>
                assert(constrDecl.name == "scalus.sir.SITRTypingScalaToSIRSpecScope$.ClassA1")
            case _ => fail(s"unexpected type ${sir.tp}")
        }

        // println(sir.pretty.render(100))

    }

    test("check that scalus.prelude.List is mapped to SumCaseClass") {
        val sir = compile {
            scalus.prelude.List.single(BigInt(1))
        }
        sir.tp match
            case SIRType.SumCaseClass(dataDecl, typeArgs) =>
                assert(dataDecl.name == "scalus.prelude.List")
                assert(typeArgs == List(SIRType.Integer))
            case _ => fail(s"unexpected type ${sir.tp}")

    }

    test("check that scalus.prelude.List is mapped to SumCaseClass in fun") {
        val sir = compile {
            (x:BigInt) => scalus.prelude.List.single(x)
        }
        sir.tp match
            case SIRType.Fun(SIRType.Integer, SIRType.SumCaseClass(dataDecl, typeArgs)) =>
                assert(dataDecl.name == "scalus.prelude.List")
                assert(typeArgs == List(SIRType.Integer))
            case _ => fail(s"unexpected type ${sir.tp}")

    }
    
}
