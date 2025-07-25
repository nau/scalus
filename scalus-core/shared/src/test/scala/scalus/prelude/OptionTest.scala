package scalus.prelude

import scalus.*
import scalus.builtin.ByteString.hex
import scalus.prelude.Option.{None, Some}
import scalus.sir.SIR
import scalus.uplc.Term

import scala.language.implicitConversions

class OptionTest extends StdlibTestKit {
    test("eq") {
        assert(Option.empty[String] === None)
        assert(Option.Some(BigInt(1)) === Option.Some(BigInt(1)))
        assert(Option.Some(BigInt(1)) !== Option.Some(BigInt(2)))
        assertEval(Option.Some(true) !== None)
        assertEval(Option.Some(true) === Option.Some(true))
        assertEval(Option.Some(true) !== Option.Some(false))
    }

    test("ord") {
        check { (pair: (Int, Int)) =>
            val (left, right) = pair
            val leftOpt = prelude.Option(BigInt(left))
            val rightOpt = prelude.Option(BigInt(right))
            leftOpt.gt(Option.empty) && rightOpt.gt(
              Option.empty
            ) && (if left > right then {
                      leftOpt gt rightOpt
                  } else if left < right then {
                      leftOpt lt rightOpt
                  } else leftOpt equiv rightOpt)
        }

//        assertEval(
//          Some(BigInt(-5)).gt(Option.empty[BigInt])
//        )
//        assertEval(
//          Some(BigInt(-5)).gt(Some(BigInt(-10)))
//        )
    }

    test("ToData <-> FromData") {
        check { (opt: Option[BigInt]) =>
            val data = opt.toData
            val fromDataOpt = fromData[Option[BigInt]](data)

            opt === fromDataOpt
        }
    }

    test("flatten") {
        assertEvalEq(None.flatten, None)
        assertEvalEq(Some(Some("")).flatten, Some(""))
        assertEvalEq(Some(Option.empty[String]).flatten, None)
    }

    test("empties") {
        check { (asScala: scala.Option[Boolean]) =>
            val asScalus = asScala.asScalus
            asScalus.asScala == asScala && asScalus.isDefined == asScalus.nonEmpty && asScalus.isDefined != asScalus.isEmpty
        }
        assertEval(None.isEmpty)
        assertEval(!None.isDefined)
        assertEval(!None.nonEmpty)
        assertEval(Some("").nonEmpty)
        assertEval(Some("").isDefined)
        assertEval(!Some("").isEmpty)
    }

    test("throwing accessors") {
        assertEvalFails[NoSuchElementException](None.get)
        assertEvalFails[NoSuchElementException](None.getOrFail())
        assertEvalFails[NoSuchElementException](None.orFail())

        assertEvalEq(Some("").get, "")
        assertEvalEq(Some(BigInt(5)).getOrFail(), BigInt(5))
        assertEvalEq(Some(hex"deadbeef").orFail(), ())
    }

    test("getOrElse") {
        assertEvalEq(Some("foo").getOrElse("bar"), "foo")
        assertEvalEq(None.getOrElse("bar"), "bar")
    }

    test("orElse") {
        assertEvalEq(Some("foo").orElse(Some("bar")), Some("foo"))
        assertEvalEq(Some("foo").orElse(None), Some("foo"))
        assertEvalEq(None.orElse(Some("bar")), Some("bar"))
        assertEvalEq(None.orElse(None), None)
    }

    test("map") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).map(x => x / BigInt(0)))
        assertEvalEq(Option.empty[BigInt].map(x => x / BigInt(0)), None)

        assertEvalEq(Some(BigInt(42)).map(_ / 2), Some(BigInt(21)))
        assertEval(Option.empty[BigInt].map(_ / 2).isEmpty)
    }

    test("flatMap") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).flatMap(x => Some(x / BigInt(0))))
        assertEvalEq(Option.empty[BigInt].flatMap(x => Some(x / BigInt(0))), None)

        assertEvalEq(Some(BigInt(5)).flatMap(x => Some(x * BigInt(5))), Some(BigInt(25)))
        assertEvalEq(Some(BigInt(42)).flatMap(_ => None), None)
        assertEvalEq(Option.empty[BigInt].flatMap(x => Some(x * BigInt(5))), None)
        assertEvalEq(None.flatMap(_ => None), None)
    }

    test("filter + filterNot") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).filter(x => (x / 0) > BigInt(10)))
        assertEvalFails[ArithmeticException](Some(BigInt(5)).filterNot(x => (x / 0) > BigInt(10)))
        assertEvalEq(Option.empty[BigInt].filter(x => (x / 0) > BigInt(10)), None)
        assertEvalEq(Option.empty[BigInt].filterNot(x => (x / 0) > BigInt(10)), None)

        assertEval(None.filter(_ => true).isEmpty)
        assertEval(None.filter(_ => false).isEmpty)
        assertEvalEq(Some(BigInt(5)).filter(_ > 4), Some(BigInt(5)))
        assertEvalEq(Some(BigInt(5)).filter(_ % 2 == BigInt(0)), None)

        assertEval(None.filterNot(_ => true).isEmpty)
        assertEval(None.filterNot(_ => false).isEmpty)
        assertEvalEq(Some(BigInt(5)).filterNot(_ > 4), None)
        assertEvalEq(Some(BigInt(5)).filterNot(_ % 2 == BigInt(0)), Some(BigInt(5)))
    }

    test("contains") {
        assertEval(!None.contains(BigInt(5)))
        assertEval(Some(BigInt(5)).contains(BigInt(5)))
        assertEval(!Some(BigInt(5)).contains(BigInt(0)))

        assertEval(!Some(BigInt(5)).contains(BigInt(5))(using (_, _) => false))
    }

    test("exists") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).exists(x => (x / 0) > BigInt(2)))
        assertEval(!Option.empty[BigInt].exists(x => (x / 0) > BigInt(2)))

        assertEval(!None.exists(_ => true))
        assertEval(Some(BigInt(5)).exists(_ > BigInt(0)))
        assertEval(!Some(BigInt(5)).exists(_ < BigInt(0)))
    }

    test("forall") {
        assertEvalFails[ArithmeticException](Some(BigInt(5)).forall(x => (x / 0) > BigInt(2)))
        assertEval(Option.empty[BigInt].forall(x => (x / 0) > BigInt(2)))

        assertEval(None.forall(_ => true))
        assertEval(Some(BigInt(5)).forall(_ > BigInt(0)))
        assertEval(!Some(BigInt(5)).forall(_ < BigInt(0)))
    }

}
