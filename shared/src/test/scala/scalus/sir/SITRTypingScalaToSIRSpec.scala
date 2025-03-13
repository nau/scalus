package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.Compiler.compileDebug
import scalus.*
import scalus.builtin.given

object SITRTypingScalaToSIRSpecScope {

    case class ClassA1(a: BigInt)

    sealed trait HierarchicalLevel1[A]
    case class LeafLevel1A[A](a: A) extends HierarchicalLevel1[A]
    sealed trait HierarchicalLevel2[B] extends HierarchicalLevel1[Future[B]]
    case class LeafLevel2A[B](b: Future[B]) extends HierarchicalLevel2[B]
    case class LeafLevel2B[B](ib: Int) extends HierarchicalLevel2[Int]

    // DataDecl(
    //    "scalus.sir.SITRTypingScalaToSIRSpecScope$.HierarchicalLevel1",
    //    List(
    //      ConstrDecl("LeafLevel1A", D, List(SIRType.Var("A",Some(1))),List(SIRType.Var("A",Some(1))))),
    //      ConstrDecl("_narrow_HierarchialLevel2", D, List(SIRType.Var("B",Some(2))),List(SIRType.Future(SIRType.Var("B",Some(1))))),
    //    ),
    //    None
    //  )
    //  DataDecl(
    //    "scalus.sir.SITRTypingScalaToSIRSpecScope$.HierarchicalLevel2",
    //    List(
    
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
        val sir = compile { (x: BigInt) =>
            scalus.prelude.List.single(x)
        }
        sir.tp match
            case SIRType.Fun(SIRType.Integer, SIRType.SumCaseClass(dataDecl, typeArgs)) =>
                assert(dataDecl.name == "scalus.prelude.List")
                assert(typeArgs == List(SIRType.Integer))
            case _ => fail(s"unexpected type ${sir.tp}")

    }

}
