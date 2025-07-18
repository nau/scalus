package scalus.prelude

import scalus.*
import scalus.builtin.Data.{fromData, toData}
import org.scalacheck.Arbitrary
import scalus.uplc.test.ArbitraryInstances
import org.scalacheck.Prop
import org.scalatestplus.scalacheck.Checkers.*
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.onchain.RequirementError
import scalus.uplc.Term
import scalus.uplc.eval.{PlutusVM, Result}
import scala.reflect.ClassTag
import Eq.given
import Ord.*
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
          SortedMap.singleton(BigInt(1), BigInt(1)).toList,
          List.single((BigInt(1), BigInt(1)))
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

        assertThrows[RequirementError] {
            SortedMap.fromStrictlyAscendingList(
              List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
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

        assertEvalFails[RequirementError](
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(1), BigInt(1)), List.Nil))
          )
        )
    }

    test("union") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.union(map, SortedMap.empty[BigInt, BigInt])
            val expected = map.mapValues[These[BigInt, BigInt]](These.This(_))

            result === expected
        }

        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.union(SortedMap.empty[BigInt, BigInt], map)
            val expected = map.mapValues[These[BigInt, BigInt]](These.That(_))

            result === expected
        }

        check { (lhs: SortedMap[BigInt, BigInt], rhs: SortedMap[BigInt, BigInt]) =>
            val result = SortedMap.union(lhs, rhs)
            val keys = (lhs.keys ++ rhs.keys).unique
            val expected = keys.foldLeft(SortedMap.empty[BigInt, These[BigInt, BigInt]]) {
                (acc, key) =>
                    acc.insert(
                      key,
                      (lhs.get(key), rhs.get(key)) match
                          case (Option.Some(lv), Option.Some(rv)) => These.These(lv, rv)
                          case (Option.Some(lv), Option.None)     => These.This(lv)
                          case (Option.None, Option.Some(rv))     => These.That(rv)
                          case (Option.None, Option.None) =>
                              fail("unreachable: Both values are None")
                    )
            }

            result === expected
        }

        assertEvalEq(
          SortedMap
              .union(
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Cons((BigInt(3), BigInt(3)), List.Nil)
                        )
                      )
                    ),
                SortedMap.empty[BigInt, BigInt]
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.This(BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.This(BigInt(2))),
                    List.Cons((BigInt(3), These.This(BigInt(3))), List.Nil)
                  )
                )
              )
        )

        assertEvalEq(
          SortedMap
              .union(
                SortedMap.empty[BigInt, BigInt],
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Cons((BigInt(3), BigInt(3)), List.Nil)
                        )
                      )
                    )
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.That(BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.That(BigInt(2))),
                    List.Cons((BigInt(3), These.That(BigInt(3))), List.Nil)
                  )
                )
              )
        )

        assertEvalEq(
          SortedMap
              .union(
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(2), BigInt(2)),
                          List.Nil
                        )
                      )
                    ),
                SortedMap
                    .fromStrictlyAscendingList(
                      List.Cons(
                        (BigInt(1), BigInt(1)),
                        List.Cons(
                          (BigInt(3), BigInt(3)),
                          List.Nil
                        )
                      )
                    )
              ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), These.These(BigInt(1), BigInt(1))),
                  List.Cons(
                    (BigInt(2), These.This(BigInt(2))),
                    List.Cons((BigInt(3), These.That(BigInt(3))), List.Nil)
                  )
                )
              )
        )
    }

    test("sortedMapEq") {
        assertEval(
          SortedMap.empty[BigInt, BigInt] === SortedMap.empty[BigInt, BigInt]
        )

        assertEval(
          SortedMap.singleton(BigInt(1), BigInt(1)) === SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ) ===
              SortedMap
                  .fromStrictlyAscendingList(
                    List.Cons(
                      (BigInt(1), BigInt(1)),
                      List.Cons(
                        (BigInt(2), BigInt(2)),
                        List.Nil
                      )
                    )
                  )
        )

        assertEval(
          SortedMap.empty[BigInt, BigInt] !== SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ) !==
              SortedMap
                  .fromStrictlyAscendingList(
                    List.Cons(
                      (BigInt(1), BigInt(1)),
                      List.Cons(
                        (BigInt(3), BigInt(3)),
                        List.Nil
                      )
                    )
                  )
        )
    }

    test("sortedMapOrd") {
        assertEvalEq(
          SortedMap.empty[BigInt, BigInt] <=> SortedMap.empty[BigInt, BigInt],
          Order.Equal
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(0), BigInt(0)) <=> SortedMap.singleton(BigInt(0), BigInt(0)),
          Order.Equal
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ) <=> SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ),
          Order.Equal
        )

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt] <=> SortedMap.singleton(BigInt(1), BigInt(1)),
          Order.Less
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(0), BigInt(0)) <=> SortedMap.singleton(BigInt(1), BigInt(1)),
          Order.Less
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ) <=> SortedMap.singleton(BigInt(3), BigInt(3)),
          Order.Less
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)) <=> SortedMap.empty[BigInt, BigInt],
          Order.Greater
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)) <=> SortedMap.singleton(BigInt(0), BigInt(0)),
          Order.Greater
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ) <=> SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(0), BigInt(0)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Nil
                  )
                )
              ),
          Order.Greater
        )
    }

    test("ToData <-> FromData") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val data = map.toData
            val map2 = fromData[SortedMap[BigInt, BigInt]](data)
            map === map2
        }

        assertEvalEq(
          fromData[SortedMap[BigInt, BigInt]](SortedMap.empty[BigInt, BigInt].toData),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          fromData[SortedMap[BigInt, BigInt]](SortedMap.singleton(BigInt(1), BigInt(1)).toData),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          fromData[SortedMap[BigInt, BigInt]](
            SortedMap
                .fromStrictlyAscendingList(
                  List.Cons(
                    (BigInt(1), BigInt(1)),
                    List.Cons(
                      (BigInt(2), BigInt(2)),
                      List.Cons((BigInt(3), BigInt(3)), List.Nil)
                    )
                  )
                )
                .toData
          ),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons(
                    (BigInt(2), BigInt(2)),
                    List.Cons((BigInt(3), BigInt(3)), List.Nil)
                  )
                )
              )
        )
    }

    test("isEmpty") {
        assertEval(SortedMap.empty[BigInt, BigInt].isEmpty)

        assertEval(!SortedMap.singleton(BigInt(1), BigInt(1)).isEmpty)

        assertEval(
          !SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .isEmpty
        )
    }

    test("nonEmpty") {
        assertEval(!SortedMap.empty[BigInt, BigInt].nonEmpty)

        assertEval(SortedMap.singleton(BigInt(1), BigInt(1)).nonEmpty)

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .nonEmpty
        )
    }

    test("length") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.length
            val expected = map.toList.length

            result === expected
        }

        assertEvalEq(SortedMap.empty[BigInt, BigInt].length, BigInt(0))

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1)).length, BigInt(1))

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .length,
          BigInt(3)
        )
    }

    test("size") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.size
            val expected = map.toList.length

            result === expected
        }

        assertEvalEq(SortedMap.empty[BigInt, BigInt].size, BigInt(0))

        assertEvalEq(SortedMap.singleton(BigInt(1), BigInt(1)).size, BigInt(1))

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .size,
          BigInt(3)
        )
    }

    test("keys") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.keys
            val expected = map.toList.map(_._1)

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].keys,
          List.Nil
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), "1").keys,
          List.single(BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), "1"),
                  List.Cons((BigInt(2), "2"), List.Cons((BigInt(3), "3"), List.Nil))
                )
              )
              .keys,
          List.Cons(BigInt(1), List.Cons(BigInt(2), List.Cons(BigInt(3), List.Nil)))
        )
    }

    test("values") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.values
            val expected = map.toList.map(_._2)

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].values,
          List.Nil
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), "1").values,
          List.single("1")
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), "1"),
                  List.Cons((BigInt(2), "2"), List.Cons((BigInt(3), "3"), List.Nil))
                )
              )
              .values,
          List.Cons("1", List.Cons("2", List.Cons("3", List.Nil)))
        )
    }

    test("forall") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.forall(predicate)
            val expected = map.toList.forall(predicate)

            result === expected
        }

        assertEval(
          SortedMap.empty[BigInt, BigInt].forall(_ => true)
        )

        assertEval(
          SortedMap.singleton(BigInt(1), BigInt(1)).forall(_._1 > 0)
        )

        assertEval(
          !SortedMap.singleton(BigInt(1), BigInt(1)).forall(_._1 < 0)
        )

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .forall(_._1 > 0)
        )

        assertEval(
          !SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .forall(_._1 > 2)
        )
    }

    test("exists") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.exists(predicate)
            val expected = map.toList.exists(predicate)

            result === expected
        }

        assertEval(
          !SortedMap.empty[BigInt, BigInt].exists(_ => true)
        )

        assertEval(
          SortedMap.singleton(BigInt(1), BigInt(1)).exists(_._1 > 0)
        )

        assertEval(
          !SortedMap.singleton(BigInt(1), BigInt(1)).exists(_._1 < 0)
        )

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .exists(_._1 > 2)
        )

        assertEval(
          !SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .exists(_._1 < 0)
        )
    }

    test("mapValues") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val result = map.mapValues(_ + 1)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.map { case (k, v) =>
                (k, v + 1)
            })

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].mapValues(_ + 1),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).mapValues(_ + 1),
          SortedMap.fromStrictlyAscendingList(List.single((BigInt(1), BigInt(2))))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .mapValues(_ + 1),
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(2)),
              List.Cons((BigInt(2), BigInt(3)), List.Cons((BigInt(3), BigInt(4)), List.Nil))
            )
          )
        )
    }

    test("filter") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.filter(predicate)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filter(predicate))

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].filter(_ => true),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).filter(_._1 > 0),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filter(_._1 > 2),
          SortedMap.fromStrictlyAscendingList(List.Cons((BigInt(3), BigInt(3)), List.Nil))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filter(_._1 < 0),
          SortedMap.empty[BigInt, BigInt]
        )
    }

    test("filterNot") {
        check { (map: SortedMap[BigInt, BigInt]) =>
            val predicate: ((BigInt, BigInt)) => Boolean = _._1 > 0
            val result = map.filterNot(predicate)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filterNot(predicate))

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].filterNot(_ => true),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).filterNot(_._1 > 0),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filterNot(_._1 > 2),
          SortedMap.fromStrictlyAscendingList(
            List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
          )
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .filterNot(_._1 < 0),
          SortedMap.fromStrictlyAscendingList(
            List.Cons(
              (BigInt(1), BigInt(1)),
              List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
            )
          )
        )
    }

    test("find") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.find(_._1 === key)
            val expected = map.toList.find(_._1 === key)

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].find(_._1 === BigInt(1)),
          Option.None
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).find(_._1 === BigInt(1)),
          Option.Some((BigInt(1), BigInt(1)))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).find(_._1 === BigInt(0)),
          Option.None
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .find(_._1 === BigInt(2)),
          Option.Some((BigInt(2), BigInt(2)))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .find(_._1 === BigInt(4)),
          Option.None
        )
    }

    test("findMap") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.findMap { case (k, v) =>
                if k === key then Option.Some(v) else Option.None
            }
            val expected = map.toList.findMap { case (k, v) =>
                if k === key then Option.Some(v) else Option.None
            }

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].findMap { case (k, v) => Option.Some(v) },
          Option.None
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).findMap { case (k, v) =>
              if k === BigInt(1) then Option.Some(v) else Option.None
          },
          Option.Some(BigInt(1))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).findMap { case (k, v) =>
              if k === BigInt(0) then Option.Some(v) else Option.None
          },
          Option.None
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .findMap { case (k, v) => if k === BigInt(2) then Option.Some(v) else Option.None },
          Option.Some(BigInt(2))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .findMap { case (k, v) => if k === BigInt(4) then Option.Some(v) else Option.None },
          Option.None
        )
    }

    test("foldLeft") {
        check { (map: SortedMap[BigInt, BigInt], initial: BigInt) =>
            val result = map.foldLeft(initial) { case (acc, (k, v)) => acc + k + v }
            val expected = map.toList.foldLeft(initial) { case (acc, (k, v)) => acc + k + v }

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v },
          BigInt(0)
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).foldLeft(BigInt(0)) { case (acc, (k, v)) =>
              acc + k + v
          },
          BigInt(2)
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .foldLeft(BigInt(0)) { case (acc, (k, v)) => acc + k + v },
          BigInt(12)
        )
    }

    test("foldRight") {
        check { (map: SortedMap[BigInt, BigInt], initial: BigInt) =>
            val result = map.foldRight(initial) { case ((k, v), acc) => acc + k + v }
            val expected = map.toList.foldRight(initial) { case ((k, v), acc) => acc + k + v }

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].foldRight(BigInt(0)) { case ((k, v), acc) =>
              acc + k + v
          },
          BigInt(0)
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).foldRight(BigInt(0)) { case ((k, v), acc) =>
              acc + k + v
          },
          BigInt(2)
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .foldRight(BigInt(0)) { case ((k, v), acc) => acc + k + v },
          BigInt(12)
        )
    }

    test("get") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.get(key)
            val expected = map.toList.findMap { case (k, v) =>
                if k === key then Option.Some(v) else Option.None
            }

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].get(BigInt(1)),
          Option.None
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).get(BigInt(1)),
          Option.Some(BigInt(1))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).get(BigInt(0)),
          Option.None
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .get(BigInt(2)),
          Option.Some(BigInt(2))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .get(BigInt(4)),
          Option.None
        )
    }

    test("contains") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.contains(key)
            val expected = map.toList.exists(_._1 === key)

            result === expected
        }

        assertEval(
          !SortedMap.empty[BigInt, BigInt].contains(BigInt(1))
        )

        assertEval(
          SortedMap.singleton(BigInt(1), BigInt(1)).contains(BigInt(1))
        )

        assertEval(
          !SortedMap.singleton(BigInt(1), BigInt(1)).contains(BigInt(0))
        )

        assertEval(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .contains(BigInt(2))
        )

        assertEval(
          !SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
              .contains(BigInt(4))
        )
    }

    test("insert") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt, value: BigInt) =>
            val result = map.insert(key, value)
            val expected =
                SortedMap.fromList(map.toList.filterNot(_._1 === key) ++ List.single((key, value)))

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].insert(BigInt(1), BigInt(1)),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).insert(BigInt(2), BigInt(2)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
              )
              .insert(BigInt(2), BigInt(2)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons(
                  (BigInt(1), BigInt(1)),
                  List.Cons((BigInt(2), BigInt(2)), List.Cons((BigInt(3), BigInt(3)), List.Nil))
                )
              )
        )
    }

    test("delete") {
        check { (map: SortedMap[BigInt, BigInt], key: BigInt) =>
            val result = map.delete(key)
            val expected = SortedMap.fromStrictlyAscendingList(map.toList.filterNot(_._1 === key))

            result === expected
        }

        assertEvalEq(
          SortedMap.empty[BigInt, BigInt].delete(BigInt(1)),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).delete(BigInt(1)),
          SortedMap.empty[BigInt, BigInt]
        )

        assertEvalEq(
          SortedMap.singleton(BigInt(1), BigInt(1)).delete(BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
              .delete(BigInt(2)),
          SortedMap.singleton(BigInt(1), BigInt(1))
        )

        assertEvalEq(
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
              .delete(BigInt(3)),
          SortedMap
              .fromStrictlyAscendingList(
                List.Cons((BigInt(1), BigInt(1)), List.Cons((BigInt(2), BigInt(2)), List.Nil))
              )
        )
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
                              errorMessage.contains(exception.getMessage),
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

    private inline def assertEval(inline code: Boolean): Unit = {
        assert(code)

        val codeTerm = Compiler.compileInline(code).toUplc(true).evaluate
        assert(Term.alphaEq(codeTerm, trueTerm))
    }

    private val trueTerm = Compiler.compileInline(true).toUplc(true).evaluate
}
