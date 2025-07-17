package scalus.prelude

import scalus.*
import org.scalacheck.Arbitrary
import scalus.uplc.test.ArbitraryInstances
import org.scalacheck.Prop
import org.scalatestplus.scalacheck.Checkers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.{ImpossibleLedgerStateError, OnchainError, RequirementError}
import scalus.uplc.Term
import scalus.uplc.eval.{PlutusVM, Result}
import scala.reflect.ClassTag
import Eq.given
import scala.util.control.NonFatal

class SortedMapTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("empty") {
        assertEvalEq(SortedMap.empty[BigInt, BigInt].toList, List.empty[(BigInt, BigInt)])
    }

    test("singleton") {
        check { (key: BigInt, value: BigInt) =>
            SortedMap.singleton(key, value).toList === List.single((key, value))
        }

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(2)).toList,
          List.single((BigInt(1), BigInt(2)))
        )
    }

    test("unsafeFromList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                list.unique(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
            SortedMap.unsafeFromList(strictlyAscendingList).toList === strictlyAscendingList
        }

        assertEvalEq(
          SortedMap
              .unsafeFromList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .toList,
          List.Cons(
            (BigInt(1), BigInt(1)),
            List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
          )
        )
    }

    test("fromList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                list.unique(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
            SortedMap.fromList(list).toList === strictlyAscendingList
        }

        assertEvalEq(
          SortedMap
              .fromList(
                List.Cons(
                  (BigInt(2), BigInt(2)),
                  List.Cons((BigInt(2), BigInt(3)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
                )
              )
              .toList,
          List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
        )
    }

    test("fromStrictlyAscendingList") {
        check { (list: List[(BigInt, BigInt)]) =>
            val strictlyAscendingList =
                list.unique(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
            SortedMap
                .fromStrictlyAscendingList(strictlyAscendingList)
                .toList === strictlyAscendingList
        }

        assertThrows[RequirementError] {
            SortedMap.fromStrictlyAscendingList(
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
            )
        }

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .toList,
          List.Cons(
            (BigInt(1), BigInt(1)),
            List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
          )
        )

        assertEvalFails[RequirementError](
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
          )
        )

    }

    test("union") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val union = SortedMap.union(map, SortedMap.empty[BigInt, BigInt])
            val expected = map.mapValues[These[BigInt, BigInt]](These.This(_))

            union === expected
        }

        check { (map: SortedMap[BigInt, BigInt]) =>
            val union = SortedMap.union(SortedMap.empty[BigInt, BigInt], map)
            val expected = map.mapValues[These[BigInt, BigInt]](These.That(_))

            union === expected
        }

        check { (lhs: SortedMap[BigInt, BigInt], rhs: SortedMap[BigInt, BigInt]) =>
            val union = SortedMap.union(lhs, rhs)
            val keys = (lhs.keys ++ rhs.keys).unique
            val expected = keys.foldLeft(SortedMap.empty[BigInt, These[BigInt, BigInt]]) {
                (acc, key) =>
                    acc.insert(
                      key,
                      (lhs.get(key), rhs.get(key)) match
                          case (Option.Some(lv), Option.Some(rv)) => These.These(lv, rv)
                          case (Option.Some(lv), Option.None)     => These.This(lv)
                          case (Option.None, Option.Some(rv))     => These.That(rv)
                          case (Option.None, Option.None)         => fail("Both values are None")
                    )
            }

            union === expected
        }

//        assertEvalEq(
//          SortedMap
//              .union(
//                SortedMap.singleton(BigInt(1), BigInt(1)),
//                SortedMap.singleton(BigInt(3), BigInt(4))
//              ),
//          SortedMap.List.Cons(
//            (BigInt(1), These.This(BigInt(2))),
//            List.Cons((BigInt(3), These.That(BigInt(4))), List.Nil)
//          )
//        )
    }

    private inline def assertEvalFails[E <: Throwable: ClassTag](inline code: Any): Unit = {
        var isExceptionThrown = false

        val _ =
            try code
            catch
                case NonFatal(exception) =>
                    assert(
                      ClassTag(exception.getClass) == summon[ClassTag[E]],
                      s"Expected exception of type ${summon[ClassTag[E]]}, but got $exception"
                    )

                    val result = Compiler.compileInline(code).toUplc(true).evaluateDebug
                    result match
                        case failure: Result.Failure =>
                            val errorMessage = result.logs.last

                            assert(
                              errorMessage.startsWith(exception.getMessage),
                              s"Expected error message '${exception.getMessage}', but got '$errorMessage'"
                            )
                        case _ =>
                            fail(s"Expected failure, but got success: $result")

                    isExceptionThrown = true

        if !isExceptionThrown then
            fail(s"Expected exception of type ${summon[ClassTag[E]]}, but no exception was thrown")
    }

    private inline def assertEvalEq[T: Eq](inline code: T, inline expected: T): Unit = {
        assert(code === expected, s"Expected $expected, but got $code")

        val codeTerm = Compiler.compileInline(code).toUplc(true).evaluate
        val expectedTerm = Compiler.compileInline(expected).toUplc(true).evaluate
        assert(
          Term.alphaEq(codeTerm, expectedTerm),
          s"Expected term $expectedTerm, but got $codeTerm"
        )
    }
}
