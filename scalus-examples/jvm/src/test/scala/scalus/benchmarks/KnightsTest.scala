package scalus.benchmarks

import scalus.*
import scalus.builtin.Builtins.{multiplyInteger, remainderInteger}
import scalus.prelude.{*, given}
import scalus.prelude.Eq.given
import scalus.prelude.Ord.{*, given}
import scalus.uplc.*
import scalus.uplc.eval.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.testkit.ScalusTest

class KnightsTest extends AnyFunSuite, ScalusTest:
    import KnightsTest.{*, given}

    test("100_4x4") {
        val result = Compiler
            .compile {
                val result = runKnights(100, 4)
                val expected: Solution = List.empty
                require(result === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val scalusBudget = ExBudget(ExCPU(44806039120L), ExMemory(247940395L))
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "KnightsTest.100_4x4",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(54958831939L), ExMemory(160204421L)),
          isPrintComparison = false
        )
    }

    test("100_6x6") {
        val result = Compiler
            .compile {
                val result = runKnights(100, 6)

                import scalus.prelude.List.*
                val expected: Solution = Cons(
                  (
                    0,
                    ChessSet(
                      size = 6,
                      moveNumber = 36,
                      start = Option.Some((1, 1)),
                      visited = Cons(
                        (3, 2),
                        Cons(
                          (5, 3),
                          Cons(
                            (6, 1),
                            Cons(
                              (4, 2),
                              Cons(
                                (3, 4),
                                Cons(
                                  (2, 6),
                                  Cons(
                                    (4, 5),
                                    Cons(
                                      (6, 6),
                                      Cons(
                                        (5, 4),
                                        Cons(
                                          (6, 2),
                                          Cons(
                                            (4, 1),
                                            Cons(
                                              (2, 2),
                                              Cons(
                                                (1, 4),
                                                Cons(
                                                  (3, 3),
                                                  Cons(
                                                    (2, 1),
                                                    Cons(
                                                      (1, 3),
                                                      Cons(
                                                        (2, 5),
                                                        Cons(
                                                          (4, 6),
                                                          Cons(
                                                            (6, 5),
                                                            Cons(
                                                              (4, 4),
                                                              Cons(
                                                                (5, 2),
                                                                Cons(
                                                                  (6, 4),
                                                                  Cons(
                                                                    (5, 6),
                                                                    Cons(
                                                                      (3, 5),
                                                                      Cons(
                                                                        (1, 6),
                                                                        Cons(
                                                                          (2, 4),
                                                                          Cons(
                                                                            (1, 2),
                                                                            Cons(
                                                                              (3, 1),
                                                                              Cons(
                                                                                (4, 3),
                                                                                Cons(
                                                                                  (5, 1),
                                                                                  Cons(
                                                                                    (6, 3),
                                                                                    Cons(
                                                                                      (5, 5),
                                                                                      Cons(
                                                                                        (3, 6),
                                                                                        Cons(
                                                                                          (1, 5),
                                                                                          Cons(
                                                                                            (2, 3),
                                                                                            Cons(
                                                                                              (
                                                                                                1,
                                                                                                1
                                                                                              ),
                                                                                              Nil
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  Cons(
                    (
                      0,
                      ChessSet(
                        size = 6,
                        moveNumber = 36,
                        start = Option.Some((1, 1)),
                        visited = Cons(
                          (3, 2),
                          Cons(
                            (5, 3),
                            Cons(
                              (6, 1),
                              Cons(
                                (4, 2),
                                Cons(
                                  (3, 4),
                                  Cons(
                                    (2, 2),
                                    Cons(
                                      (4, 1),
                                      Cons(
                                        (6, 2),
                                        Cons(
                                          (5, 4),
                                          Cons(
                                            (6, 6),
                                            Cons(
                                              (4, 5),
                                              Cons(
                                                (2, 6),
                                                Cons(
                                                  (1, 4),
                                                  Cons(
                                                    (3, 3),
                                                    Cons(
                                                      (2, 1),
                                                      Cons(
                                                        (1, 3),
                                                        Cons(
                                                          (2, 5),
                                                          Cons(
                                                            (4, 6),
                                                            Cons(
                                                              (6, 5),
                                                              Cons(
                                                                (4, 4),
                                                                Cons(
                                                                  (5, 2),
                                                                  Cons(
                                                                    (6, 4),
                                                                    Cons(
                                                                      (5, 6),
                                                                      Cons(
                                                                        (3, 5),
                                                                        Cons(
                                                                          (1, 6),
                                                                          Cons(
                                                                            (2, 4),
                                                                            Cons(
                                                                              (1, 2),
                                                                              Cons(
                                                                                (3, 1),
                                                                                Cons(
                                                                                  (4, 3),
                                                                                  Cons(
                                                                                    (5, 1),
                                                                                    Cons(
                                                                                      (6, 3),
                                                                                      Cons(
                                                                                        (5, 5),
                                                                                        Cons(
                                                                                          (3, 6),
                                                                                          Cons(
                                                                                            (1, 5),
                                                                                            Cons(
                                                                                              (
                                                                                                2,
                                                                                                3
                                                                                              ),
                                                                                              Cons(
                                                                                                (
                                                                                                  1,
                                                                                                  1
                                                                                                ),
                                                                                                Nil
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    Cons(
                      (
                        0,
                        ChessSet(
                          size = 6,
                          moveNumber = 36,
                          start = Option.Some((1, 1)),
                          visited = Cons(
                            (3, 2),
                            Cons(
                              (5, 3),
                              Cons(
                                (6, 1),
                                Cons(
                                  (4, 2),
                                  Cons(
                                    (3, 4),
                                    Cons(
                                      (2, 2),
                                      Cons(
                                        (1, 4),
                                        Cons(
                                          (2, 6),
                                          Cons(
                                            (4, 5),
                                            Cons(
                                              (6, 6),
                                              Cons(
                                                (5, 4),
                                                Cons(
                                                  (6, 2),
                                                  Cons(
                                                    (4, 1),
                                                    Cons(
                                                      (3, 3),
                                                      Cons(
                                                        (2, 1),
                                                        Cons(
                                                          (1, 3),
                                                          Cons(
                                                            (2, 5),
                                                            Cons(
                                                              (4, 6),
                                                              Cons(
                                                                (6, 5),
                                                                Cons(
                                                                  (4, 4),
                                                                  Cons(
                                                                    (5, 2),
                                                                    Cons(
                                                                      (6, 4),
                                                                      Cons(
                                                                        (5, 6),
                                                                        Cons(
                                                                          (3, 5),
                                                                          Cons(
                                                                            (1, 6),
                                                                            Cons(
                                                                              (2, 4),
                                                                              Cons(
                                                                                (1, 2),
                                                                                Cons(
                                                                                  (3, 1),
                                                                                  Cons(
                                                                                    (4, 3),
                                                                                    Cons(
                                                                                      (5, 1),
                                                                                      Cons(
                                                                                        (6, 3),
                                                                                        Cons(
                                                                                          (5, 5),
                                                                                          Cons(
                                                                                            (3, 6),
                                                                                            Cons(
                                                                                              (
                                                                                                1,
                                                                                                5
                                                                                              ),
                                                                                              Cons(
                                                                                                (
                                                                                                  2,
                                                                                                  3
                                                                                                ),
                                                                                                Cons(
                                                                                                  (
                                                                                                    1,
                                                                                                    1
                                                                                                  ),
                                                                                                  Nil
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      Cons(
                        (
                          0,
                          ChessSet(
                            size = 6,
                            moveNumber = 36,
                            start = Option.Some((1, 1)),
                            visited = Cons(
                              (3, 2),
                              Cons(
                                (5, 3),
                                Cons(
                                  (6, 1),
                                  Cons(
                                    (4, 2),
                                    Cons(
                                      (3, 4),
                                      Cons(
                                        (2, 6),
                                        Cons(
                                          (1, 4),
                                          Cons(
                                            (2, 2),
                                            Cons(
                                              (4, 1),
                                              Cons(
                                                (6, 2),
                                                Cons(
                                                  (5, 4),
                                                  Cons(
                                                    (6, 6),
                                                    Cons(
                                                      (4, 5),
                                                      Cons(
                                                        (3, 3),
                                                        Cons(
                                                          (2, 1),
                                                          Cons(
                                                            (1, 3),
                                                            Cons(
                                                              (2, 5),
                                                              Cons(
                                                                (4, 6),
                                                                Cons(
                                                                  (6, 5),
                                                                  Cons(
                                                                    (4, 4),
                                                                    Cons(
                                                                      (5, 2),
                                                                      Cons(
                                                                        (6, 4),
                                                                        Cons(
                                                                          (5, 6),
                                                                          Cons(
                                                                            (3, 5),
                                                                            Cons(
                                                                              (1, 6),
                                                                              Cons(
                                                                                (2, 4),
                                                                                Cons(
                                                                                  (1, 2),
                                                                                  Cons(
                                                                                    (3, 1),
                                                                                    Cons(
                                                                                      (4, 3),
                                                                                      Cons(
                                                                                        (5, 1),
                                                                                        Cons(
                                                                                          (6, 3),
                                                                                          Cons(
                                                                                            (5, 5),
                                                                                            Cons(
                                                                                              (
                                                                                                3,
                                                                                                6
                                                                                              ),
                                                                                              Cons(
                                                                                                (
                                                                                                  1,
                                                                                                  5
                                                                                                ),
                                                                                                Cons(
                                                                                                  (
                                                                                                    2,
                                                                                                    3
                                                                                                  ),
                                                                                                  Cons(
                                                                                                    (1, 1),
                                                                                                    Nil
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        Nil
                      )
                    )
                  )
                )

                require(result === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val scalusBudget = ExBudget(ExCPU(115801212696L), ExMemory(645943580L))
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "KnightsTest.100_6x6",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(131954064320L), ExMemory(292216349L)),
          isPrintComparison = false
        )
    }

    test("100_8x8") {
        val result = Compiler
            .compile {
                val result = runKnights(100, 8)

                import scalus.prelude.List.*
                val expected: Solution = Cons(
                  (
                    0,
                    ChessSet(
                      size = 8,
                      moveNumber = 64,
                      start = Option.Some((1, 1)),
                      visited = Cons(
                        (3, 2),
                        Cons(
                          (4, 4),
                          Cons(
                            (5, 6),
                            Cons(
                              (6, 4),
                              Cons(
                                (8, 5),
                                Cons(
                                  (7, 7),
                                  Cons(
                                    (6, 5),
                                    Cons(
                                      (8, 4),
                                      Cons(
                                        (7, 2),
                                        Cons(
                                          (5, 3),
                                          Cons(
                                            (3, 4),
                                            Cons(
                                              (4, 6),
                                              Cons(
                                                (5, 8),
                                                Cons(
                                                  (6, 6),
                                                  Cons(
                                                    (4, 5),
                                                    Cons(
                                                      (3, 7),
                                                      Cons(
                                                        (1, 8),
                                                        Cons(
                                                          (2, 6),
                                                          Cons(
                                                            (4, 7),
                                                            Cons(
                                                              (5, 5),
                                                              Cons(
                                                                (6, 3),
                                                                Cons(
                                                                  (5, 1),
                                                                  Cons(
                                                                    (4, 3),
                                                                    Cons(
                                                                      (3, 5),
                                                                      Cons(
                                                                        (5, 4),
                                                                        Cons(
                                                                          (7, 3),
                                                                          Cons(
                                                                            (8, 1),
                                                                            Cons(
                                                                              (6, 2),
                                                                              Cons(
                                                                                (4, 1),
                                                                                Cons(
                                                                                  (2, 2),
                                                                                  Cons(
                                                                                    (1, 4),
                                                                                    Cons(
                                                                                      (3, 3),
                                                                                      Cons(
                                                                                        (2, 5),
                                                                                        Cons(
                                                                                          (1, 3),
                                                                                          Cons(
                                                                                            (2, 1),
                                                                                            Cons(
                                                                                              (
                                                                                                4,
                                                                                                2
                                                                                              ),
                                                                                              Cons(
                                                                                                (
                                                                                                  6,
                                                                                                  1
                                                                                                ),
                                                                                                Cons(
                                                                                                  (
                                                                                                    8,
                                                                                                    2
                                                                                                  ),
                                                                                                  Cons(
                                                                                                    (7, 4),
                                                                                                    Cons(
                                                                                                      (8, 6),
                                                                                                      Cons(
                                                                                                        (7, 8),
                                                                                                        Cons(
                                                                                                          (5, 7),
                                                                                                          Cons(
                                                                                                            (3, 8),
                                                                                                            Cons(
                                                                                                              (1, 7),
                                                                                                              Cons(
                                                                                                                (3, 6),
                                                                                                                Cons(
                                                                                                                  (2, 8),
                                                                                                                  Cons(
                                                                                                                    (1, 6),
                                                                                                                    Cons(
                                                                                                                      (2, 4),
                                                                                                                      Cons(
                                                                                                                        (1, 2),
                                                                                                                        Cons(
                                                                                                                          (3, 1),
                                                                                                                          Cons(
                                                                                                                            (5, 2),
                                                                                                                            Cons(
                                                                                                                              (7, 1),
                                                                                                                              Cons(
                                                                                                                                (8, 3),
                                                                                                                                Cons(
                                                                                                                                  (7, 5),
                                                                                                                                  Cons(
                                                                                                                                    (8, 7),
                                                                                                                                    Cons(
                                                                                                                                      (6, 8),
                                                                                                                                      Cons(
                                                                                                                                        (7, 6),
                                                                                                                                        Cons(
                                                                                                                                          (8, 8),
                                                                                                                                          Cons(
                                                                                                                                            (6, 7),
                                                                                                                                            Cons(
                                                                                                                                              (4, 8),
                                                                                                                                              Cons(
                                                                                                                                                (2, 7),
                                                                                                                                                Cons(
                                                                                                                                                  (1, 5),
                                                                                                                                                  Cons(
                                                                                                                                                    (2, 3),
                                                                                                                                                    Cons(
                                                                                                                                                      (1, 1),
                                                                                                                                                      Nil
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  Cons(
                    (
                      0,
                      ChessSet(
                        size = 8,
                        moveNumber = 64,
                        start = Option.Some((1, 1)),
                        visited = Cons(
                          (3, 2),
                          Cons(
                            (4, 4),
                            Cons(
                              (5, 6),
                              Cons(
                                (7, 7),
                                Cons(
                                  (8, 5),
                                  Cons(
                                    (6, 4),
                                    Cons(
                                      (7, 2),
                                      Cons(
                                        (8, 4),
                                        Cons(
                                          (6, 5),
                                          Cons(
                                            (5, 3),
                                            Cons(
                                              (3, 4),
                                              Cons(
                                                (4, 6),
                                                Cons(
                                                  (5, 8),
                                                  Cons(
                                                    (6, 6),
                                                    Cons(
                                                      (4, 5),
                                                      Cons(
                                                        (3, 7),
                                                        Cons(
                                                          (1, 8),
                                                          Cons(
                                                            (2, 6),
                                                            Cons(
                                                              (4, 7),
                                                              Cons(
                                                                (5, 5),
                                                                Cons(
                                                                  (6, 3),
                                                                  Cons(
                                                                    (5, 1),
                                                                    Cons(
                                                                      (4, 3),
                                                                      Cons(
                                                                        (3, 5),
                                                                        Cons(
                                                                          (5, 4),
                                                                          Cons(
                                                                            (7, 3),
                                                                            Cons(
                                                                              (8, 1),
                                                                              Cons(
                                                                                (6, 2),
                                                                                Cons(
                                                                                  (4, 1),
                                                                                  Cons(
                                                                                    (2, 2),
                                                                                    Cons(
                                                                                      (1, 4),
                                                                                      Cons(
                                                                                        (3, 3),
                                                                                        Cons(
                                                                                          (2, 5),
                                                                                          Cons(
                                                                                            (1, 3),
                                                                                            Cons(
                                                                                              (
                                                                                                2,
                                                                                                1
                                                                                              ),
                                                                                              Cons(
                                                                                                (
                                                                                                  4,
                                                                                                  2
                                                                                                ),
                                                                                                Cons(
                                                                                                  (
                                                                                                    6,
                                                                                                    1
                                                                                                  ),
                                                                                                  Cons(
                                                                                                    (8, 2),
                                                                                                    Cons(
                                                                                                      (7, 4),
                                                                                                      Cons(
                                                                                                        (8, 6),
                                                                                                        Cons(
                                                                                                          (7, 8),
                                                                                                          Cons(
                                                                                                            (5, 7),
                                                                                                            Cons(
                                                                                                              (3, 8),
                                                                                                              Cons(
                                                                                                                (1, 7),
                                                                                                                Cons(
                                                                                                                  (3, 6),
                                                                                                                  Cons(
                                                                                                                    (2, 8),
                                                                                                                    Cons(
                                                                                                                      (1, 6),
                                                                                                                      Cons(
                                                                                                                        (2, 4),
                                                                                                                        Cons(
                                                                                                                          (1, 2),
                                                                                                                          Cons(
                                                                                                                            (3, 1),
                                                                                                                            Cons(
                                                                                                                              (5, 2),
                                                                                                                              Cons(
                                                                                                                                (7, 1),
                                                                                                                                Cons(
                                                                                                                                  (8, 3),
                                                                                                                                  Cons(
                                                                                                                                    (7, 5),
                                                                                                                                    Cons(
                                                                                                                                      (8, 7),
                                                                                                                                      Cons(
                                                                                                                                        (6, 8),
                                                                                                                                        Cons(
                                                                                                                                          (7, 6),
                                                                                                                                          Cons(
                                                                                                                                            (8, 8),
                                                                                                                                            Cons(
                                                                                                                                              (6, 7),
                                                                                                                                              Cons(
                                                                                                                                                (4, 8),
                                                                                                                                                Cons(
                                                                                                                                                  (2, 7),
                                                                                                                                                  Cons(
                                                                                                                                                    (1, 5),
                                                                                                                                                    Cons(
                                                                                                                                                      (2, 3),
                                                                                                                                                      Cons(
                                                                                                                                                        (1, 1),
                                                                                                                                                        Nil
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    Cons(
                      (
                        0,
                        ChessSet(
                          size = 8,
                          moveNumber = 64,
                          start = Option.Some((1, 1)),
                          visited = Cons(
                            (3, 2),
                            Cons(
                              (4, 4),
                              Cons(
                                (6, 5),
                                Cons(
                                  (8, 4),
                                  Cons(
                                    (7, 2),
                                    Cons(
                                      (5, 3),
                                      Cons(
                                        (3, 4),
                                        Cons(
                                          (4, 6),
                                          Cons(
                                            (5, 8),
                                            Cons(
                                              (7, 7),
                                              Cons(
                                                (5, 6),
                                                Cons(
                                                  (6, 4),
                                                  Cons(
                                                    (8, 5),
                                                    Cons(
                                                      (6, 6),
                                                      Cons(
                                                        (4, 5),
                                                        Cons(
                                                          (3, 7),
                                                          Cons(
                                                            (1, 8),
                                                            Cons(
                                                              (2, 6),
                                                              Cons(
                                                                (4, 7),
                                                                Cons(
                                                                  (5, 5),
                                                                  Cons(
                                                                    (6, 3),
                                                                    Cons(
                                                                      (5, 1),
                                                                      Cons(
                                                                        (4, 3),
                                                                        Cons(
                                                                          (3, 5),
                                                                          Cons(
                                                                            (5, 4),
                                                                            Cons(
                                                                              (7, 3),
                                                                              Cons(
                                                                                (8, 1),
                                                                                Cons(
                                                                                  (6, 2),
                                                                                  Cons(
                                                                                    (4, 1),
                                                                                    Cons(
                                                                                      (2, 2),
                                                                                      Cons(
                                                                                        (1, 4),
                                                                                        Cons(
                                                                                          (3, 3),
                                                                                          Cons(
                                                                                            (2, 5),
                                                                                            Cons(
                                                                                              (
                                                                                                1,
                                                                                                3
                                                                                              ),
                                                                                              Cons(
                                                                                                (
                                                                                                  2,
                                                                                                  1
                                                                                                ),
                                                                                                Cons(
                                                                                                  (
                                                                                                    4,
                                                                                                    2
                                                                                                  ),
                                                                                                  Cons(
                                                                                                    (6, 1),
                                                                                                    Cons(
                                                                                                      (8, 2),
                                                                                                      Cons(
                                                                                                        (7, 4),
                                                                                                        Cons(
                                                                                                          (8, 6),
                                                                                                          Cons(
                                                                                                            (7, 8),
                                                                                                            Cons(
                                                                                                              (5, 7),
                                                                                                              Cons(
                                                                                                                (3, 8),
                                                                                                                Cons(
                                                                                                                  (1, 7),
                                                                                                                  Cons(
                                                                                                                    (3, 6),
                                                                                                                    Cons(
                                                                                                                      (2, 8),
                                                                                                                      Cons(
                                                                                                                        (1, 6),
                                                                                                                        Cons(
                                                                                                                          (2, 4),
                                                                                                                          Cons(
                                                                                                                            (1, 2),
                                                                                                                            Cons(
                                                                                                                              (3, 1),
                                                                                                                              Cons(
                                                                                                                                (5, 2),
                                                                                                                                Cons(
                                                                                                                                  (7, 1),
                                                                                                                                  Cons(
                                                                                                                                    (8, 3),
                                                                                                                                    Cons(
                                                                                                                                      (7, 5),
                                                                                                                                      Cons(
                                                                                                                                        (8, 7),
                                                                                                                                        Cons(
                                                                                                                                          (6, 8),
                                                                                                                                          Cons(
                                                                                                                                            (7, 6),
                                                                                                                                            Cons(
                                                                                                                                              (8, 8),
                                                                                                                                              Cons(
                                                                                                                                                (6, 7),
                                                                                                                                                Cons(
                                                                                                                                                  (4, 8),
                                                                                                                                                  Cons(
                                                                                                                                                    (2, 7),
                                                                                                                                                    Cons(
                                                                                                                                                      (1, 5),
                                                                                                                                                      Cons(
                                                                                                                                                        (2, 3),
                                                                                                                                                        Cons(
                                                                                                                                                          (1, 1),
                                                                                                                                                          Nil
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      Nil
                    )
                  )
                )

                require(result === expected)
            }
            .toUplcOptimized(false)
            .evaluateDebug

        val scalusBudget = ExBudget(ExCPU(235854599301L), ExMemory(1315265845L))
        assert(result.isSuccess)
        assert(result.budget == scalusBudget)

        compareBudgetWithReferenceValue(
          testName = "KnightsTest.100_8x8",
          scalusBudget = scalusBudget,
          refBudget = ExBudget(ExCPU(270266226527L), ExMemory(540217437L)),
          isPrintComparison = false
        )
    }

end KnightsTest

@Compile
object KnightsTest:
    enum Direction:
        case UL, UR, DL, DR, LU, LD, RU, RD

    val directions: List[Direction] = {
        import Direction.*
        List.Cons(
          UL,
          List.Cons(
            UR,
            List.Cons(
              DL,
              List.Cons(DR, List.Cons(LU, List.Cons(LD, List.Cons(RU, List.Cons(RD, List.Nil)))))
            )
          )
        )
    }

    type Tile = (BigInt, BigInt)

    extension (self: Tile)
        def move(direction: Direction): Tile =
            val (x, y) = self
            import Direction.*
            direction match
                case UL => (x - 1, y - 2)
                case UR => (x + 1, y - 2)
                case DL => (x - 1, y + 2)
                case DR => (x + 1, y + 2)
                case LU => (x - 2, y - 1)
                case LD => (x - 2, y + 1)
                case RU => (x + 2, y - 1)
                case RD => (x + 2, y + 1)

    end extension

    type Solution = List[(BigInt, ChessSet)]

    case class ChessSet(
        size: BigInt,
        moveNumber: BigInt,
        start: Option[Tile],
        visited: List[Tile]
    )

    def createBoard(size: BigInt, initSquare: Tile): ChessSet =
        ChessSet(
          size = size,
          moveNumber = BigInt(1),
          start = Option.Some(initSquare),
          visited = List.single(initSquare)
        )

    def startTour(tile: Tile, size: BigInt): ChessSet =
        require(remainderInteger(size, BigInt(2)) === BigInt(0))
        createBoard(size, tile)

    given Eq[ChessSet] =
        import Eq.orElseBy
        Eq.by[ChessSet, BigInt](_.size).orElseBy(_.moveNumber).orElseBy(_.start).orElseBy(_.visited)

    given Ord[ChessSet] = Ord.by[ChessSet, List[Tile]](_.visited)

    extension (self: ChessSet)
        def addPiece(tile: Tile): ChessSet =
            ChessSet(
              size = self.size,
              moveNumber = self.moveNumber + 1,
              start = self.start,
              visited = self.visited.prepended(tile)
            )

        def firstPiece: Tile = self.start.get
        def lastPiece: Tile = self.visited.head

        def deleteFirst: ChessSet =
            extension [A](self: List[A])
                def secondLast: Option[A] =
                    self.reverse match
                        case List.Nil => fail()
                        case List.Cons(_, tail) =>
                            tail match
                                case List.Nil            => Option.None
                                case List.Cons(value, _) => Option.Some(value)

            end extension

            val newVisited = self.visited.init

            ChessSet(
              size = self.size,
              moveNumber = self.moveNumber - 1,
              start = self.visited.secondLast,
              visited = newVisited
            )

        def isSquareFree(tile: Tile): Boolean = !self.visited.contains(tile)

        def canMoveTo(tile: Tile): Boolean =
            val (x, y) = tile
            val size = self.size
            x >= 1 && x <= size && y >= 1 && y <= size && isSquareFree(tile)

        def canMove(direction: Direction): Boolean = canMoveTo(lastPiece.move(direction))
        def moveKnight(direction: Direction): ChessSet = addPiece(lastPiece.move(direction))
        def possibleMoves: List[Direction] = directions.filter(canMove)
        def allDescend: List[ChessSet] = possibleMoves.map(moveKnight)

        def descAndNo: Solution = allDescend.map { item =>
            (item.deleteFirst.possibleMoves.length, item)
        }

        def singleDescend: List[ChessSet] =
            descAndNo.filterMap { item =>
                val (moves, board) = item
                if moves === BigInt(1) then Option.Some(board) else Option.empty
            }

        def isDeadEnd: Boolean = possibleMoves.isEmpty
        def canJumpFirst: Boolean = deleteFirst.canMoveTo(firstPiece)

        def descendants: List[ChessSet] = {
            extension [A](self: List[A])
                def quicksort[B >: A: Ord]: List[A] =
                    self match
                        case List.Nil => List.Nil
                        case List.Cons(head, tail) =>
                            val before = tail.filter { elem => (elem <=> head).isLess }.quicksort
                            val after = tail.filter { elem => !(elem <=> head).isLess }.quicksort
                            before ++ after.prepended(head)
            end extension

            if canJumpFirst && addPiece(firstPiece).isDeadEnd then List.empty
            else
                val singles = singleDescend
                singles match
                    case List.Nil              => descAndNo.quicksort.map { _._2 }
                    case List.Cons(head, tail) => if tail.isEmpty then singles else List.empty
        }

        def isTourFinished: Boolean =
            self.moveNumber === multiplyInteger(self.size, self.size) && canJumpFirst

    end extension

    case class Queue[A](list: List[A])

    def emptyQueue[A]: Queue[A] = Queue(List.empty[A])

    extension [A](self: Queue[A])
        def toList: List[A] = self.list
        def isEmpty: Boolean = toList.isEmpty
        def appendFront(item: A): Queue[A] = Queue(toList.prepended(item))
        def appendAllFront(list: List[A]): Queue[A] = Queue(list ++ toList)
        def removeFront: Queue[A] = Queue(toList.tail)
        def head: A = toList.head

    end extension

    def isDone(item: (BigInt, ChessSet)): Boolean = item._2.isTourFinished

    def grow(item: (BigInt, ChessSet)): List[(BigInt, ChessSet)] =
        val (x, board) = item
        board.descendants.map { (x + 1, _) }

    def makeStarts(size: BigInt): List[(BigInt, ChessSet)] =
        val it = List.range(1, size)
        val l = it.flatMap { x => it.map { y => startTour((x, y), size) } }
        val length = l.length
        require(length == size * size)
        List.fill(1 - length, length).zip(l)

    def root(size: BigInt): Queue[(BigInt, ChessSet)] =
        emptyQueue[(BigInt, ChessSet)].appendAllFront(makeStarts(size))

    def depthSearch[A](
        depth: BigInt,
        queue: Queue[A],
        grow: A => List[A],
        done: A => Boolean
    ): Queue[A] =
        if depth === BigInt(0) || queue.isEmpty then emptyQueue[A]
        else if done(queue.head) then
            depthSearch(depth - 1, queue.removeFront, grow, done).appendFront(queue.head)
        else depthSearch(depth - 1, queue.removeFront.appendAllFront(grow(queue.head)), grow, done)

    def runKnights(depth: BigInt, boardSize: BigInt): Solution =
        depthSearch(depth, root(boardSize), grow, isDone).toList

end KnightsTest
