package scalus.sir

import org.scalatest.funsuite.AnyFunSuite

import scalus.*
import scalus.Compiler.compile

class SIRTypeUnifyTest extends AnyFunSuite {

    /*
    test("Unification with upcasting [List and Cons]") {
        val list1FunSir = compile { (x: scalus.prelude.List[BigInt]) => x }
        val consFunSir =
            compile((x: BigInt) => scalus.prelude.List.Cons(x, scalus.prelude.List.Nil))

        val listTp = list1FunSir.tp match {
            case SIRType.Fun(x, tp) => tp
            case _                  => fail("Expected a function type")
        }

        val consTp = consFunSir.tp match {
            case SIRType.Fun(x, tp) => tp
            case _                  => fail("Expected a function type")
        }

        // println(s"listTp = ${listTp.show}, consTp = ${consTp.show}")

        val lub = SIRType.leastUpperBound(listTp, consTp)

        // println(s"lub = ${lub.show}")

        assert(lub ~=~ SIRType.List(SIRType.Integer))

    }

    test("Unification with upcasting [List[?] and Cons[A]]") {
        val tA = SIRType.TypeVar("A", Some(11L), false)

        val listTp = SIRType.List(SIRType.FreeUnificator)

        val consTp = SIRType.List.Cons(tA)

        println(s"listTp = ${listTp.show}, consTp = ${consTp.show}")

        val lub = SIRType.leastUpperBound(listTp, consTp)

        println(s"lub = ${lub.show}")

        assert(
          lub ~=~ SIRType.List(SIRType.FreeUnificator)
        )

    }*/

    test("parentSeq fron Cons[A] to List[?]") {
        val tA = SIRType.TypeVar("A", Some(11L), false)
        val consTp = SIRType.List.Cons(tA)
        val listTp = SIRType.List(SIRType.FreeUnificator)

        val parentsSeq =
            SIRUnify.subtypeSeq(consTp, listTp, SIRUnify.Env.empty)

        assert(parentsSeq.nonEmpty)

    }

    test("parentSeq fron Nil to List[Tuple2[BigInt, String]]") {
        pending
        val nilFun = compile { (x: scalus.prelude.List.Nil.type) => x }
        val abList: SIR = compile { (x: scalus.prelude.List[(BigInt, String)]) =>
            x
        }

        val nilType = nilFun.tp match {
            case SIRType.Fun(_, tp) => tp
            case _                  => fail("Expected a function type")
        }

        val listTupleType = abList.tp match {
            case SIRType.Fun(_, tp) => tp
            case _                  => fail("Expected a function type")
        }

        val parentsSeq =
            SIRUnify.subtypeSeq(nilType, listTupleType, SIRUnify.Env.empty)

        assert(parentsSeq.nonEmpty)

    }

}
