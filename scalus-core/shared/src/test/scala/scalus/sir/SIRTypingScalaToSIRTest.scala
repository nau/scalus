package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.*

object SIRTypingScalaToSIRSpecScope {

    case class ClassA1(a: BigInt)

    case class Wrapper[X](value: X)

    sealed trait HierarchicalLevel1[A]
    case class LeafLevel1A[A](a: A) extends HierarchicalLevel1[A]
    sealed trait HierarchicalLevel2[B] extends HierarchicalLevel1[Wrapper[B]]
    case class LeafLevel2A[B](b: Wrapper[B]) extends HierarchicalLevel2[B]
    case class LeafLevel2B[B](ib: Int) extends HierarchicalLevel2[Int]

    // DataDecl(
    //    "scalus.sir.SIRTypingScalaToSIRSpecScope$.HierarchicalLevel1",
    //    List(
    //      ConstrDecl("LeafLevel1A", D, List(SIRType.Var("A",Some(1))),List(SIRType.Var("A",Some(1))))),
    //      ConstrDecl("_narrow_HierarchialLevel2", D, List(SIRType.Var("B",Some(2))),List(SIRType.Future(SIRType.Var("B",Some(1))))),
    //    ),
    //    None
    //  )
    //  DataDecl(
    //    "scalus.sir.SIRTypingScalaToSIRSpecScope$.HierarchicalLevel2",
    //    List(

}

class SIRTypingScalaToSIRTest extends AnyFunSuite {

    /*
    test("check that simple case class is mapped to case class in fun") {
        import SIRTypingScalaToSIRSpecScope.*

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
        import SIRTypingScalaToSIRSpecScope.*

        val sir = compile {
            new ClassA1(10)
        }

        sir.tp match {
            case SIRType.CaseClass(constrDecl, Nil, _) =>
                assert(constrDecl.name == "scalus.sir.SIRTypingScalaToSIRSpecScope$.ClassA1")
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
