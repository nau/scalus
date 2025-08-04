package scalus.ledger.api.v1

import scalus.cardano.onchain.RequirementError
import scalus.prelude.{List, Option, SortedMap}
import scalus.builtin.ByteString
import scalus.prelude.given
import scalus.prelude.StdlibTestKit
import scalus.prelude.Ord.*

class ValueTest extends StdlibTestKit with ArbitraryInstances {
    test("zero") {
        assertEvalEq(
          Value.zero.toSortedMap,
          SortedMap.empty[CurrencySymbol, SortedMap[TokenName, BigInt]]
        )
    }

    test("apply") {
        checkEval { (currencySymbol: CurrencySymbol, tokenName: TokenName, value: BigInt) =>
            Value(currencySymbol, tokenName, value).toSortedMap ===
                (
                  if value !== BigInt(0) then
                      SortedMap.singleton(
                        currencySymbol,
                        SortedMap.singleton(tokenName, value)
                      )
                  else Value.zero.toSortedMap
                )
        }

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ).toSortedMap,
          SortedMap.singleton(
            ByteString.fromString("CurrencySymbol"),
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1))
          )
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ),
          Value.zero
        )
    }

    test("lovelace") {
        checkEval { (value: BigInt) =>
            Value.lovelace(value).toSortedMap ===
                (
                  if value !== BigInt(0) then
                      SortedMap.singleton(
                        Value.adaCurrencySymbol,
                        SortedMap.singleton(Value.adaTokenName, value)
                      )
                  else Value.zero.toSortedMap
                )
        }

        assertEvalEq(
          Value.lovelace(BigInt(1000)).toSortedMap,
          SortedMap.singleton(
            Value.adaCurrencySymbol,
            SortedMap.singleton(Value.adaTokenName, BigInt(1000))
          )
        )

        assertEvalEq(
          Value.lovelace(BigInt(0)),
          Value.zero
        )
    }

    test("unsafeFromList") {
        check { (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
            val validList =
                list.distinct(using Eq.keyPairEq)
                    .quicksort(using Ord.keyPairOrd)
                    .filterMap { case (cs, tokens) =>
                        val validTokens = tokens
                            .distinct(using Eq.keyPairEq)
                            .quicksort(using Ord.keyPairOrd)
                            .filter { case (_, value) =>
                                value !== BigInt(0)
                            }

                        if validTokens.nonEmpty then Option.Some((cs, validTokens)) else Option.None
                    }

            Value.unsafeFromList(validList).toSortedMap === SortedMap.unsafeFromList(
              validList.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
            )
        }

        assertEvalEq(
          Value
              .unsafeFromList(
                List(
                  (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(10)))),
                  (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(20))))
                )
              )
              .toSortedMap,
          SortedMap.unsafeFromList(
            List(
              (
                ByteString.fromString("CS1"),
                SortedMap.unsafeFromList(List((ByteString.fromString("TN1"), BigInt(10))))
              ),
              (
                ByteString.fromString("CS2"),
                SortedMap.unsafeFromList(List((ByteString.fromString("TN2"), BigInt(20))))
              )
            )
          )
        )
    }

    test("fromList") {
        check { (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
            Value.fromList(list).toSortedMap === SortedMap.fromList(
              list.filterMap { case (cs, tnList) =>
                  val tokens = tnList.filter { _._2 !== BigInt(0) }

                  if tokens.nonEmpty then Option.Some((cs, SortedMap.fromList(tokens)))
                  else Option.None
              }
            )
        }

        assertEvalEq(
          Value
              .fromList(
                List(
                  (
                    ByteString.fromString("CS1"),
                    List(
                      (ByteString.fromString("TN1"), BigInt(10)),
                      (ByteString.fromString("TN1"), BigInt(20)),
                      (ByteString.fromString("TN2"), BigInt(0)),
                    )
                  ),
                  (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(20)))),
                  (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(30)))),
                  (ByteString.fromString("CS3"), List((ByteString.fromString("TN3"), BigInt(0))))
                )
              )
              .toSortedMap,
          SortedMap.fromList(
            List(
              (
                ByteString.fromString("CS1"),
                SortedMap.fromList(List((ByteString.fromString("TN1"), BigInt(10))))
              ),
              (
                ByteString.fromString("CS2"),
                SortedMap.fromList(List((ByteString.fromString("TN2"), BigInt(20))))
              )
            )
          )
        )
    }

    test("fromStrictlyAscendingListWithNonZeroAmounts") {
        check { (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
            val validList =
                list.distinct(using Eq.keyPairEq)
                    .quicksort(using Ord.keyPairOrd)
                    .filterMap { case (cs, tokens) =>
                        val validTokens = tokens
                            .distinct(using Eq.keyPairEq)
                            .quicksort(using Ord.keyPairOrd)
                            .filter { case (_, value) =>
                                value !== BigInt(0)
                            }

                        if validTokens.nonEmpty then Option.Some((cs, validTokens)) else Option.None
                    }

            Value.fromStrictlyAscendingListWithNonZeroAmounts(validList).toSortedMap ===
                SortedMap.unsafeFromList(
                  validList.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
                )
        }

//        assertEvalEq(
//          Value
//              .fromStrictlyAscendingListWithNonZeroAmounts(
//                List(
//                  (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(10)))),
//                  (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(20))))
//                )
//              )
//              .toSortedMap,
//          SortedMap.unsafeFromList(
//            List(
//              (
//                ByteString.fromString("CS1"),
//                SortedMap.unsafeFromList(List((ByteString.fromString("TN1"), BigInt(10))))
//              ),
//              (
//                ByteString.fromString("CS2"),
//                SortedMap.unsafeFromList(List((ByteString.fromString("TN2"), BigInt(20))))
//              )
//            )
//          )
//        )
    }

    test("adaCurrencySymbol") {
        assertEvalEq(Value.adaCurrencySymbol, ByteString.empty)
    }

    test("adaTokenName") {
        assertEvalEq(Value.adaTokenName, ByteString.empty)
    }

    test("equalsAssets") {
        assertEval(
          Value.equalsAssets(
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1)),
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1))
          )
        )

        assertEval(
          !Value.equalsAssets(
            SortedMap.singleton(ByteString.fromString("TokenName1"), BigInt(1)),
            SortedMap.singleton(ByteString.fromString("TokenName2"), BigInt(1))
          )
        )

        assertEval(
          !Value.equalsAssets(
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1)),
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(-1))
          )
        )
    }

    test("Eq") {
        checkEval { (value: Value) => value === value }

        assertEval(Value.zero === Value.zero)

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ) === Value.zero
        )

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ) ===
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(1)
              )
        )

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol1"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ) !==
              Value(
                ByteString.fromString("CurrencySymbol2"),
                ByteString.fromString("TokenName"),
                BigInt(1)
              )
        )

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName1"),
            BigInt(1)
          ) !==
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName2"),
                BigInt(1)
              )
        )

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ) !==
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(2)
              )
        )
    }

    test("Ord") {
        checkEval { (value: Value) => value equiv value }

        assertEval(Value.zero equiv Value.zero)

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ) equiv Value.zero
        )

        assertEval(Value.lovelace(BigInt(0)) equiv Value.zero)

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ) equiv
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(1)
              )
        )

        assertEval(Value.lovelace(BigInt(1)) equiv Value.lovelace(BigInt(1)))

        assertEval(
          Value.zero nonEquiv Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          )
        )

        assertEval(
          Value.zero nonEquiv Value.lovelace(BigInt(1))
        )

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ) nonEquiv Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          )
        )

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ) nonEquiv Value.lovelace(BigInt(1))
        )

//        assertEval(
//          Value(
//            ByteString.fromString("A"),
//            ByteString.fromString("Token"),
//            BigInt(1)
//          ) <
//              Value(
//                ByteString.fromString("A"),
//                ByteString.fromString("Token"),
//                BigInt(2)
//              )
//        )
//
//        assertEval(
//          Value.lovelace(BigInt(1)) < Value.lovelace(BigInt(2))
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("B"),
//            ByteString.fromString("Token"),
//            BigInt(1)
//          ) <
//              Value(
//                ByteString.fromString("A"),
//                ByteString.fromString("Token"),
//                BigInt(1)
//              )
//        )
//
//        assertEval(
//          Value.lovelace(BigInt(1)) <= Value.lovelace(BigInt(1))
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("A"),
//            ByteString.fromString("Token"),
//            BigInt(1)
//          ) <=
//              Value(
//                ByteString.fromString("A"),
//                ByteString.fromString("Token"),
//                BigInt(2)
//              )
//        )
//
//        assertEval(
//          Value.lovelace(BigInt(2)) > Value.lovelace(BigInt(1))
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("B"),
//            ByteString.fromString("Token"),
//            BigInt(1)
//          ) >
//              Value(
//                ByteString.fromString("A"),
//                ByteString.fromString("Token"),
//                BigInt(1)
//              )
//        )
//
//        assertEval(
//          Value.lovelace(BigInt(1)) >= Value.lovelace(BigInt(1))
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("B"),
//            ByteString.fromString("Token"),
//            BigInt(2)
//          ) >=
//              Value(
//                ByteString.fromString("B"),
//                ByteString.fromString("Token"),
//                BigInt(1)
//              )
//        )
    }

    test("toData <-> FromData") {
        checkEval { (value: Value) =>
            val data = value.toData
            val fromDataValue = fromData[Value](data)
            fromDataValue === value
        }
    }

    test("valueFromDataWithValidation") {
        check { (value: Value) =>
            given FromData[Value] = Value.valueFromDataWithValidation

            val data = value.toData
            val fromDataValue = fromData[Value](data)
            fromDataValue === value
        }

//        assertEvalFails[RequirementError] {
//            given FromData[Value] = Value.valueFromDataWithValidation
//
//            val invalidValue = Value.unsafeFromList(
//              List(
//                (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(0))))
//              )
//            )
//
//            val data = invalidValue.toData
//            fromData[Value](data)
//        }
    }

    test("unary_") {
        checkEval { (value: Value) =>
            val negatedValue = -value
            negatedValue.toSortedMap === value.toSortedMap.mapValues(_.mapValues(-_))
        }

        assertEvalEq(-Value.zero, Value.zero)

        assertEvalEq(
          -Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ),
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(-1000)
          )
        )

        assertEvalEq(
          -Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(-1000))
        )
    }

    test("+") {
        checkEval { (value: Value) =>
            (value + Value.zero) === value && (Value.zero + value) === value
        }

        check { (value1: Value, value2: Value) =>
            val sumValue = value1 + value2
            sumValue.flatten.forall { case (cs, token, value) =>
                val v1 = value1.toSortedMap.get(cs).flatMap { _.get(token) }
                val v2 = value2.toSortedMap.get(cs).flatMap { _.get(token) }

                v1 match
                    case Option.Some(v1Value) =>
                        v2 match
                            case Option.Some(v2Value) => (v1Value + v2Value) === value
                            case Option.None          => v1Value === value
                    case Option.None =>
                        v2 match
                            case Option.Some(v2Value) => v2Value === value
                            case Option.None          => false

            }
        }

        assertEvalEq(Value.zero + Value.zero, Value.zero)

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) +
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(2000)
              ),
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(3000)
          )
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) +
              Value.zero,
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          )
        )

        assertEvalEq(
          Value.zero +
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(1000)
              ),
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          )
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)) + Value.lovelace(BigInt(2000)),
          Value.lovelace(BigInt(3000))
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)) + Value.zero,
          Value.lovelace(BigInt(1000))
        )

        assertEvalEq(
          Value.zero + Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(1000))
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) +
              Value.lovelace(BigInt(1000)),
          Value.fromList(
            List(
              (
                ByteString.fromString("CurrencySymbol"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
            )
          )
        )
    }

    test("-") {
        checkEval { (value: Value) =>
            (value - Value.zero) === value && (Value.zero - value) === -value
        }

        check { (value1: Value, value2: Value) =>
            val diffValue = value1 - value2
            diffValue.flatten.forall { case (cs, token, value) =>
                val v1 = value1.toSortedMap.get(cs).flatMap { _.get(token) }
                val v2 = value2.toSortedMap.get(cs).flatMap { _.get(token) }

                v1 match
                    case Option.Some(v1Value) =>
                        v2 match
                            case Option.Some(v2Value) => (v1Value - v2Value) === value
                            case Option.None          => v1Value === value
                    case Option.None =>
                        v2 match
                            case Option.Some(v2Value) => -v2Value === value
                            case Option.None          => false

            }
        }

        assertEvalEq(Value.zero - Value.zero, Value.zero)

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) -
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(2000)
              ),
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(-1000)
          )
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) -
              Value.zero,
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          )
        )

        assertEvalEq(
          Value.zero -
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(1000)
              ),
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(-1000)
          )
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)) - Value.lovelace(BigInt(2000)),
          Value.lovelace(BigInt(-1000))
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)) - Value.zero,
          Value.lovelace(BigInt(1000))
        )

        assertEvalEq(
          Value.zero - Value.lovelace(BigInt(1000)),
          Value.lovelace(BigInt(-1000))
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) -
              Value.lovelace(BigInt(1000)),
          Value.fromList(
            List(
              (
                ByteString.fromString("CurrencySymbol"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(-1000))))
            )
          )
        )
    }

    test("*") {
        checkEval { (value: Value) =>
            (value * Value.zero) === Value.zero &&
            (Value.zero * value) === Value.zero
        }

        check { (value1: Value, value2: Value) =>
            val multiplyValue = value1 * value2
            multiplyValue.flatten.forall { case (cs, token, value) =>
                val v1 = value1.toSortedMap.get(cs).flatMap { _.get(token) }
                val v2 = value2.toSortedMap.get(cs).flatMap { _.get(token) }

                v1 match
                    case Option.Some(v1Value) =>
                        v2 match
                            case Option.Some(v2Value) => (v1Value * v2Value) === value
                            case Option.None          => false
                    case Option.None =>
                        v2 match
                            case Option.Some(v2Value) => false
                            case Option.None          => false

            }
        }

        assertEvalEq(Value.zero * Value.zero, Value.zero)

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) *
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(2)
              ),
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(2000)
          )
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) *
              Value.zero,
          Value.zero
        )

        assertEvalEq(
          Value.zero *
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(1000)
              ),
          Value.zero
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)) * Value.lovelace(BigInt(2)),
          Value.lovelace(BigInt(2000))
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)) * Value.zero,
          Value.zero
        )

        assertEvalEq(
          Value.zero * Value.lovelace(BigInt(1000)),
          Value.zero
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) *
              Value.lovelace(BigInt(2)),
          Value.zero
        )
    }

    test("showDebug") {
        assert(Value.zero.showDebug === "{  }")

        println(Value
              .fromList(
                List.Cons(
                  (
                    Value.adaCurrencySymbol,
                    List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)
                  ),
                  List.Cons(
                    (
                      ByteString.fromString("ff"),
                      List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)
                    ),
                    List.Nil
                  )
                )
              )
              .showDebug )
        
        assert(
          Value
              .fromList(
                List.Cons(
                  (
                    Value.adaCurrencySymbol,
                    List.Cons((Value.adaTokenName, BigInt(1000000)), List.Nil)
                  ),
                  List.Cons(
                    (
                      ByteString.fromString("ff"),
                      List.Cons((ByteString.fromString("TOKEN"), BigInt(100)), List.Nil)
                    ),
                    List.Nil
                  )
                )
              )
              .showDebug === "{ policy# -> { #: 1000000 }, policy#ff -> { #544f4b454e: 100 } }"
        )
    }
}
