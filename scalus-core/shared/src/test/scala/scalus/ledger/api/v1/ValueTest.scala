package scalus.ledger.api.v1

import scalus.cardano.onchain.RequirementError
import scalus.prelude.{List, Option, SortedMap}
import scalus.builtin.ByteString
import scalus.prelude.StdlibTestKit
import scalus.uplc.eval.Result.{Failure, Success}

class ValueTest extends StdlibTestKit with ArbitraryInstances {
    given [T: Arbitrary]: Arbitrary[List[T]] = Arbitrary {
        for
            size <- Gen.choose(0, 10)
            elements <- Gen.listOfN(size, Arbitrary.arbitrary[T])
        yield List.from(elements)
    }

    test("toSortedMap") {
        checkEval { (value: Value) =>
            value.toSortedMap.forall { case (currencySymbol, tokens) =>
                tokens.forall { case (tokenName, amount) =>
                    amount === value.quantityOf(currencySymbol, tokenName)
                }
            }
        }

        assertEvalEq(Value.zero.toSortedMap, SortedMap.empty)

        assertEvalEq(
          Value.lovelace(BigInt(1000)).toSortedMap,
          SortedMap.singleton(
            Value.adaCurrencySymbol,
            SortedMap.singleton(Value.adaTokenName, BigInt(1000))
          )
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).toSortedMap,
          SortedMap.singleton(
            ByteString.fromString("CurrencySymbol"),
            SortedMap.singleton(ByteString.fromString("TokenName"), BigInt(1000))
          )
        )
    }

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

        val sir = scalus.Compiler.compile {
            // (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
            (d: Data) =>
                import scalus.prelude.*
                val list = Data.fromData[
                  scalus.prelude.List[
                    (CurrencySymbol, scalus.prelude.List[(TokenName, BigInt)])
                  ]
                ](d)
                // val validList = list
                // list.distinct(using Eq.keyPairEq)
                /* .quicksort(using Ord.keyPairOrd)
                        .filterMap { case (cs, tokens) =>
                            val validTokens = tokens
                                .distinct(using Eq.keyPairEq)
                                .quicksort(using Ord.keyPairOrd)
                                .filter { case (_, value) =>
                                    value !== BigInt(0)
                                }

                            if validTokens.nonEmpty then Option.Some((cs, validTokens))
                            else Option.None
                        }
                        
                 */
                // list.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
                // val (cs, tnList) = list.head
                list.headOption match {
                    case Option.Some(v) =>
                        log("negora assign")
                        // val (cs, tnList) = v
                        v match
                            case (cs, tnList) =>
                                log("parsed pair")
                                val q = (cs, tnList)
                                log("after assign")
                            case _ =>
                                log("not a pair")
                        log("after assign")
                    case Option.None =>
                        throw new RuntimeException("List is empty, cannot extract head")
                }
                // SortedMap.unsafeFromList(tnList)
                /*
                Value.unsafeFromList(validList).toSortedMap === SortedMap.unsafeFromList(
                  validList.map { case (cs, tnList) => (cs, SortedMap.unsafeFromList(tnList)) }
                )
                
                 */
        }
        /*
        import scalus.*
        import scalus.prelude.*
        import scalus.uplc.*

        println(s"sir=${sir.pretty.render(100)}")

        val lw = sir.toLoweredValue()
        println(s"lwe=${lw.pretty.render(100)}")

        val uplc = sir.toUplc()
        println(s"uplc=${uplc.pretty.render(100)}")

        val listFromDataSir = scalus.Compiler.compile { (x: Data) =>
            Data.fromData[
              scalus.prelude.List[(CurrencySymbol, scalus.prelude.List[(TokenName, BigInt)])]
            ](x)
        }

        val arg0 =
            List.Cons(
              (
                "fc7f1f7f1380d97aff4eff207fabdcffbe7f80f4012300ec807a7fbf9701017fffb4347f7fc401",
                List.Cons(
                  ("000e01628001", BigInt(104929086447450L)),
                  List.Cons(("fbff7f011b530100ff8a01a3", BigInt(-173061477122556L)), List.Nil)
                )
              ),
              List.Nil
            )
        val arg0Data = arg0.toData
        println(s"arg0Data=${arg0Data}")

        val arg0DataUplc = listFromDataSir.toUplc() $ Term.Const(Constant.Data(arg0Data))

        val arg0DataTerm = arg0DataUplc.evaluateDebug match {
            case Success(term, budget, costs, logs) =>
                term
            case Failure(ex, budget, cost, logs) =>
                println(s"UPLC evaluation failed (1): $ex")
                println(s"logs: $logs")
                assert(false, "UPLC evaluation failed")
                throw ex
        }

        // val uplcWithArg = uplc $ arg0DataTerm
        val uplcWithArg = uplc $ Term.Const(Constant.Data(arg0Data))
        // println(s"uplcWithArg= ${uplcWithArg.pretty.render(100)}")

        val result = uplcWithArg.evaluateDebug match {
            case Success(term, budget, costs, logs) =>
                term
            case Failure(ex, budget, cost, logs) =>
                println(s"UPLC evaluation failed (2): $ex")
                println(s"logs: $logs")
                assert(false, "UPLC evaluation failed")
        }


         */

        checkEval { (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
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

        checkEval { (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
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
        checkEval { (list: List[(CurrencySymbol, List[(TokenName, BigInt)])]) =>
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

        assertEvalEq(
          Value
              .fromStrictlyAscendingListWithNonZeroAmounts(
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

    test("toData <-> FromData") {
        checkEval { (value: Value) =>
            val data = value.toData
            val fromDataValue = fromData[Value](data)
            fromDataValue === value
        }
    }

    test("valueFromDataWithValidation") {

        checkEval { (value: Value) =>
            given FromData[Value] = Value.valueFromDataWithValidation

            val data = value.toData
            val fromDataValue = fromData[Value](data)
            fromDataValue === value
        }

        // TODO: this gragment evaluated succesfully, because UPLC compiler optimize out fromData calls.
        //  Maybe implement somethong like annotation
        // assertEvalFails[RequirementError] {
        //    given FromData[Value] = Value.valueFromDataWithValidation
        //
        //    val invalidValue = Value.unsafeFromList(
        //      List(
        //        (ByteString.fromString("CS1"), List((ByteString.fromString("TN1"), BigInt(0))))
        //      )
        //    )
        //
        //    val data = invalidValue.toData
        //    fromData[Value](data)
        //  //fromData[Vaue](data): @keepInUplc  ??
        // }
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

        checkEval { (value1: Value, value2: Value) =>
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

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) +
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(-1000)
              ),
          Value.zero
        )

        assertEvalEq(Value.lovelace(BigInt(1000)) + Value.lovelace(BigInt(-1000)), Value.zero)

        assertEvalEq(
          Value.fromList(
            List(
              (
                ByteString.fromString("CurrencySymbol"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
            )
          ) +
              Value.fromList(
                List(
                  (
                    ByteString.fromString("CurrencySymbol"),
                    List((ByteString.fromString("TokenName"), BigInt(-1000)))
                  ),
                  (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(-1000))))
                )
              ),
          Value.zero
        )

        assertEvalEq(
          Value.fromList(
            List(
              (
                ByteString.fromString("CurrencySymbol"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
            )
          ) +
              Value.fromList(
                List(
                  (
                    ByteString.fromString("CurrencySymbol"),
                    List((ByteString.fromString("TokenName"), BigInt(-1000)))
                  )
                )
              ),
          Value.lovelace(BigInt(1000))
        )

        assertEvalEq(
          Value.fromList(
            List(
              (
                ByteString.fromString("CurrencySymbol"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
            )
          ) +
              Value.fromList(
                List(
                  (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(-1000))))
                )
              ),
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          )
        )
    }

    test("-") {
        checkEval { (value: Value) =>
            (value - Value.zero) === value && (Value.zero - value) === -value
        }

        // TODO: UPLC error
        checkEval { (value1: Value, value2: Value) =>
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

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) -
              Value(
                ByteString.fromString("CurrencySymbol"),
                ByteString.fromString("TokenName"),
                BigInt(1000)
              ),
          Value.zero
        )

        assertEvalEq(Value.lovelace(BigInt(1000)) - Value.lovelace(BigInt(1000)), Value.zero)

        assertEvalEq(
          Value.fromList(
            List(
              (
                ByteString.fromString("CurrencySymbol"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
            )
          ) -
              Value.fromList(
                List(
                  (
                    ByteString.fromString("CurrencySymbol"),
                    List((ByteString.fromString("TokenName"), BigInt(1000)))
                  ),
                  (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
                )
              ),
          Value.zero
        )

        assertEvalEq(
          Value.fromList(
            List(
              (
                ByteString.fromString("CurrencySymbol"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
            )
          ) -
              Value.fromList(
                List(
                  (
                    ByteString.fromString("CurrencySymbol"),
                    List((ByteString.fromString("TokenName"), BigInt(1000)))
                  )
                )
              ),
          Value.lovelace(BigInt(1000))
        )

        assertEvalEq(
          Value.fromList(
            List(
              (
                ByteString.fromString("CurrencySymbol"),
                List((ByteString.fromString("TokenName"), BigInt(1000)))
              ),
              (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
            )
          ) -
              Value.fromList(
                List(
                  (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
                )
              ),
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          )
        )
    }

    test("*") {
        checkEval { (value: Value) => (value * BigInt(0)) === Value.zero }

        checkEval { (value: Value, factor: BigInt) =>
            (value * factor).toSortedMap === (
              if factor !== BigInt(0) then
                  value.toSortedMap.mapValues { _.mapValues { _ * factor } }
              else SortedMap.empty
            )
        }

        assertEvalEq(Value.zero * BigInt(0), Value.zero)

        assertEvalEq(Value.zero * BigInt(1), Value.zero)

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ) * BigInt(2),
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
          ) * BigInt(0),
          Value.zero
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)) * BigInt(2),
          Value.lovelace(BigInt(2000))
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)) * BigInt(0),
          Value.zero
        )
    }

    test("showDebug") {
        assert(Value.zero.showDebug === "{  }")

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
              .showDebug === "{ policy# -> { #: 1000000 }, policy#6666 -> { #544f4b454e: 100 } }"
        )
    }

    test("getLovelace") {
        checkEval { (value: Value) =>
            value.getLovelace ===
                value.toSortedMap
                    .get(Value.adaCurrencySymbol)
                    .flatMap(_.get(Value.adaTokenName))
                    .getOrElse(BigInt(0))
        }

        assertEvalEq(Value.zero.getLovelace, BigInt(0))

        assertEvalEq(
          Value.lovelace(BigInt(1000)).getLovelace,
          BigInt(1000)
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).getLovelace,
          BigInt(0)
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ).getLovelace,
          BigInt(0)
        )
    }

    test("isZero") {
        checkEval { (value: Value) =>
            if value.isZero then value.toSortedMap.isEmpty else value.nonZero
        }

        assertEval(Value.zero.isZero)

        assertEval(Value.lovelace(BigInt(0)).isZero)

        assertEval(!Value.lovelace(BigInt(1000)).isZero)

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ).isZero
        )

        assertEval(
          !Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).isZero
        )
    }

    test("nonZero") {
        checkEval { (value: Value) =>
            if value.nonZero then
                value.toSortedMap.nonEmpty && value.toSortedMap.forall { case (_, tokens) =>
                    tokens.nonEmpty && tokens.forall { case (_, amount) => amount !== BigInt(0) }
                }
            else value.isZero
        }

        assertEval(!Value.zero.nonZero)

        assertEval(!Value.lovelace(BigInt(0)).nonZero)

        assertEval(Value.lovelace(BigInt(1000)).nonZero)

        assertEval(
          !Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(0)
          ).nonZero
        )

        assertEval(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).nonZero
        )
    }

    test("quantityOf") {
        checkEval { (value: Value, currencySymbol: CurrencySymbol, tokenName: TokenName) =>
            value.quantityOf(currencySymbol, tokenName) ===
                value.toSortedMap
                    .get(currencySymbol)
                    .flatMap(_.get(tokenName))
                    .getOrElse(BigInt(0))
        }

        assertEvalEq(Value.zero.quantityOf(Value.adaCurrencySymbol, Value.adaTokenName), BigInt(0))

        assertEvalEq(
          Value.zero.quantityOf(ByteString.fromString("CS"), ByteString.fromString("TN")),
          BigInt(0)
        )

        assertEvalEq(
          Value.lovelace(BigInt(1000)).quantityOf(Value.adaCurrencySymbol, Value.adaTokenName),
          BigInt(1000)
        )

        assertEvalEq(
          Value
              .lovelace(BigInt(1000))
              .quantityOf(ByteString.fromString("CS"), ByteString.fromString("TN")),
          BigInt(0)
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).quantityOf(Value.adaCurrencySymbol, Value.adaTokenName),
          BigInt(0)
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).quantityOf(ByteString.fromString("CurrencySymbol"), ByteString.fromString("TokenName")),
          BigInt(1000)
        )
    }

    test("withoutLovelace") {
        checkEval { (value: Value) =>
            value.withoutLovelace.getLovelace === BigInt(0)
        }

        assertEvalEq(Value.zero.withoutLovelace, Value.zero)

        assertEvalEq(
          Value.lovelace(BigInt(1000)).withoutLovelace,
          Value.zero
        )

        assertEvalEq(
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          ).withoutLovelace,
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          )
        )

        assertEvalEq(
          Value
              .fromList(
                List(
                  (
                    ByteString.fromString("CurrencySymbol"),
                    List((ByteString.fromString("TokenName"), BigInt(1000)))
                  ),
                  (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
                )
              )
              .withoutLovelace,
          Value(
            ByteString.fromString("CurrencySymbol"),
            ByteString.fromString("TokenName"),
            BigInt(1000)
          )
        )
    }

    test("flatten") {
        // TODO: UPLC error
        //       checkEval { (value: Value) =>
        //           value.flatten ===
        //               value.toSortedMap.toList.flatMap { case (cs, tokens) =>
        //                   tokens.toList.map { case (tn, amount) => (cs, tn, amount) }
        //               }
        //       }

        assertEvalEq(Value.zero.flatten, List.empty)

        // TODO: UPLC error
//        assertEvalEq(
//          Value.lovelace(BigInt(1000)).flatten,
//          List((Value.adaCurrencySymbol, Value.adaTokenName, BigInt(1000)))
//        )

        // TODO: UPLC error
//        assertEvalEq(
//          Value(
//            ByteString.fromString("CurrencySymbol"),
//            ByteString.fromString("TokenName"),
//            BigInt(1000)
//          ).flatten,
//          List(
//            (
//              ByteString.fromString("CurrencySymbol"),
//              ByteString.fromString("TokenName"),
//              BigInt(1000)
//            )
//          )
//        )

        // TODO: UPLC error
//        assertEvalEq(
//          Value
//              .fromList(
//                List(
//                  (
//                    ByteString.fromString("CurrencySymbol"),
//                    List((ByteString.fromString("TokenName"), BigInt(1000)))
//                  ),
//                  (Value.adaCurrencySymbol, List((Value.adaTokenName, BigInt(1000))))
//                )
//              )
//              .flatten,
//          List(
//            (Value.adaCurrencySymbol, Value.adaTokenName, BigInt(1000)),
//            (
//              ByteString.fromString("CurrencySymbol"),
//              ByteString.fromString("TokenName"),
//              BigInt(1000)
//            )
//          )
//        )
    }

}
