package scalus.ledger.api.v1

import scalus.prelude.{List, SortedMap}
import scalus.builtin.ByteString
import scalus.prelude.given
import scalus.prelude.StdlibTestKit

class ValueTest extends StdlibTestKit with ArbitraryInstances {
    test("zero") {
        assertEvalEq(
          Value.zero.toSortedMap,
          SortedMap.empty[CurrencySymbol, SortedMap[TokenName, BigInt]]
        )
    }

    test("apply") {
        check { (currencySymbol: CurrencySymbol, tokenName: TokenName, value: BigInt) =>
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
        check { (value: BigInt) =>
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

//    test("unsafeFromList") {
////        check { (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
////            val strictlyAscendingList =
////                list.distinct(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
////            SortedMap.unsafeFromList(strictlyAscendingList).toList === strictlyAscendingList
////
////            Value.unsafeFromList(list) === SortedMap.unsafeFromList(
////              list.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
////            )
////        }
//
////        assertEvalEq(
////          Value.unsafeFromList(
////            List(
////              (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(10)))),
////              (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(20))))
////            )
////          ),
////          SortedMap.unsafeFromList(
////            List(
////              (ByteString.fromString("CS1"), SortedMap.unsafeFromList(List((ByteString.fromString("TN1"), BigInt(10))))),
////              (ByteString.fromString("CS2"), SortedMap.unsafeFromList(List((ByteString.fromString("TN2"), BigInt(20)))))
////            )
////          )
////        )
//    }

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

//    test("Eq") {
//        import Value.given
//
//        assertEval(Value.zero === Value.zero)
//
//        assertEval(
//          Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(0)
//          ) === Value.zero
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(1)
//          ) ===
//              Value(
//                ByteString.fromString("CurrencySymbol"),
//                ByteString.fromString("TokenName"),
//                BigInt(1)
//              )
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("CurrencySymbol1"),
//            ByteString.fromString("TokenName"),
//            BigInt(1)
//          ) !==
//              Value(
//                ByteString.fromString("CurrencySymbol2"),
//                ByteString.fromString("TokenName"),
//                BigInt(1)
//              )
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName1"),
//            BigInt(1)
//          ) !==
//              Value(
//                ByteString.fromString("CurrencySymbol"),
//                ByteString.fromString("TokenName2"),
//                BigInt(1)
//              )
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(1)
//          ) !==
//              Value(
//                ByteString.fromString("CurrencySymbol"),
//                ByteString.fromString("TokenName"),
//                BigInt(2)
//              )
//        )
//    }

    test("Ord") {
        import scalus.prelude.Ord.*
        import Value.given
//
//        assertEval(Value.zero equiv Value.zero)
//
//        assertEval(
//          Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(0)
//          ) equiv Value.zero
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(1)
//          ) equiv
//              Value(
//                ByteString.fromString("CurrencySymbol"),
//                ByteString.fromString("TokenName"),
//                BigInt(1)
//              )
//        )
//
//        assertEval(
//          Value.zero nonEquiv Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(1)
//          )
//        )
//
//        assertEval(
//          Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(0)
//          ) nonEquiv Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(1)
//          )
//        )
//
//        assertEval(
//          Value.lovelace(BigInt(1)) < Value.lovelace(BigInt(2))
//        )

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
        check { (value: Value) =>
            val data = value.toData
            val fromDataValue = fromData[Value](data)
            fromDataValue === value
        }
    }
}
