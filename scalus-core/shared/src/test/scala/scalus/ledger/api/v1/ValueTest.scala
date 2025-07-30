package scalus.ledger.api.v1

import scalus.prelude.SortedMap
import scalus.builtin.ByteString
import scalus.prelude.given
import scalus.prelude.StdlibTestKit

class ValueTest extends StdlibTestKit with ArbitraryInstances {
    test("zero") {
        assertEvalEq(Value.zero, SortedMap.empty[CurrencySymbol, SortedMap[TokenName, BigInt]])
    }

    test("apply") {
        check { (currencySymbol: CurrencySymbol, tokenName: TokenName, value: BigInt) =>
            Value(currencySymbol, tokenName, value) ===
                (
                  if value !== BigInt(0) then
                      SortedMap.singleton(
                        currencySymbol,
                        SortedMap.singleton(tokenName, value)
                      )
                  else Value.zero
                )
        }

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1)
          ),
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

//    test("lovelace") {
//        check { (value: BigInt) =>
//            Value.lovelace(value) === SortedMap.singleton(
//              Value.adaCurrencySymbol,
//              SortedMap.singleton(Value.adaTokenName, value)
//            )
//        }
//
//        assertEvalEq(
//          Value.lovelace(BigInt(1000)),
//          SortedMap.singleton(
//            Value.adaCurrencySymbol,
//            SortedMap.singleton(Value.adaTokenName, BigInt(1000))
//          )
//        )
//    }

//    test("unsafeFromList") {
//        check { (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
//            val strictlyAscendingList =
//                list.distinct(using Eq.keyPairEq).quicksort(using Ord.keyPairOrd)
//            SortedMap.unsafeFromList(strictlyAscendingList).toList === strictlyAscendingList
//
//            Value.unsafeFromList(list) === SortedMap.unsafeFromList(
//              list.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
//            )
//        }
//
//        assertEvalEq(
//          Value.unsafeFromList(
//            List(
//              (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(10)))),
//              (ByteString.fromString("CS2"), List((ByteString.fromString("TN2"), BigInt(20))))
//            )
//          ),
//          SortedMap.unsafeFromList(
//            List(
//              (ByteString.fromString("CS1"), SortedMap.unsafeFromList(List((ByteString.fromString("TN1"), BigInt(10))))),
//              (ByteString.fromString("CS2"), SortedMap.unsafeFromList(List((ByteString.fromString("TN2"), BigInt(20)))))
//            )
//          )
//        )
//    }
}
