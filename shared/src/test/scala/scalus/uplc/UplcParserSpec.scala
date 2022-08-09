package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite

class UplcParserSpec extends AnyFunSuite {
  val parser = new UplcParser
  test("Parse program version") {
    def p(input: String) = parser.programVersion.parse(input)
    def check(input: String, expected: (Int, Int, Int)) = {
      val Right((_, result)) = p(input)
      assert(result == expected)
    }
    check("111.2.33333   ", (111, 2, 33333))
    check("1.0.03   ", (1, 0, 3))
    assert(!p("1 . 2 . 3").isRight)
    assert(!p("1.2").isRight)
    assert(!p("1.2.a").isRight)
  }

  test("Parse var") {
    val r = parser.parseProgram("(program 1.0.0 x )")
    assert(
      r == Right(
        Program(version = (1, 0, 0), term = Var("x"))
      )
    )
  }

  test("Parse lam") {
    val r = parser.parseProgram("(program 1.0.0 (lam x x) )")
    assert(
      r == Right(
        Program(version = (1, 0, 0), term = LamAbs("x", Var("x")))
      )
    )
  }

  test("Parse lam/app") {
    val r = parser.parseProgram("(program 1.0.0 [(lam x x) y z])")
    assert(
      r == Right(
        Program(version = (1, 0, 0), term = Apply(Apply(LamAbs("x", Var("x")), Var("y")), Var("z")))
      )
    )
  }

  test("Parse constant types") {
    def p(input: String) = parser.defaultUni.parse(input).map(_._2)
    assert(p("bool") == Right(DefaultUniBool))
    assert(p("bytestring") == Right(DefaultUniByteString))
    assert(p("data") == Right(DefaultUniData))
    assert(p("integer") == Right(DefaultUniInteger))
    assert(p("list (integer )") == Right(DefaultUniApply(DefaultUniProtoList, DefaultUniInteger)))
    assert(
      p("list (list(unit) )") == Right(
        DefaultUniApply(DefaultUniProtoList, DefaultUniApply(DefaultUniProtoList, DefaultUniUnit))
      )
    )
    assert(
      p("pair (integer)(bool)") == Right(
        DefaultUniApply(DefaultUniApply(DefaultUniProtoPair, DefaultUniInteger), DefaultUniBool)
      )
    )
    assert(
      p("pair (list(list(unit))) (pair(integer)(bool) )") == Right(
        DefaultUniApply(
          DefaultUniApply(
            DefaultUniProtoPair,
            DefaultUniApply(
              DefaultUniProtoList,
              DefaultUniApply(DefaultUniProtoList, DefaultUniUnit)
            )
          ),
          DefaultUniApply(DefaultUniApply(DefaultUniProtoPair, DefaultUniInteger), DefaultUniBool)
        )
      )
    )
    assert(p("string") == Right(DefaultUniString))
    assert(p("unit") == Right(DefaultUniUnit))
  }

  test("Parse constants") {
    import cats.implicits.toShow
    def p(input: String) = parser.conTerm.parse(input).map(_._2).left.map(e => e.show)
    assert(
      p("(con list(integer) [1,2, 3333])") == Right(
        Const(
          Constant(
            DefaultUniApply(DefaultUniProtoList, DefaultUniInteger),
            Constant(DefaultUniInteger, 1) :: Constant(DefaultUniInteger, 2) :: Constant(
              DefaultUniInteger,
              3333
            ) :: Nil
          )
        )
      )
    )

    assert(
      p("(con pair (integer) (bool) (12, False))") == Right(
        Const(
          Constant(
            DefaultUniApply(
              DefaultUniApply(DefaultUniProtoPair, DefaultUniInteger),
              DefaultUniBool
            ),
            (Constant(DefaultUniInteger, 12), Constant(DefaultUniBool, false))
          )
        )
      )
    )

    val r = parser.parseProgram("""(program 1.0.0 [
        |  (con bytestring #001234ff)
        |  (con bool True)
        |  (con bool False)
        |  (con unit () )
        |  (con string "Hello")
        |  ])""".stripMargin)
    assert(
      r == Right(
        Program(
          version = (1, 0, 0),
          term = Apply(
            Apply(
              Apply(
                Apply(
                  Const(Constant(DefaultUniByteString, Seq[Byte](0, 18, 52, -1))),
                  Const(Constant(DefaultUniBool, true))
                ),
                Const(Constant(DefaultUniBool, false))
              ),
              Const(Constant(DefaultUniUnit, ()))
            ),
            Const(Constant(DefaultUniString, "Hello"))
          )
        )
      )
    )
  }

  test("Parse delay/force/error") {
    val r = parser.parseProgram("(program 1.0.0 (force (delay (error))))")
    assert(
      r == Right(
        Program(version = (1, 0, 0), term = Force(Delay(Error)))
      )
    )
  }

  test("Parse program") {
    val r = parser.parseProgram("(program 1.0.0 [(lam x x) (con integer 0)])")
    assert(
      r == Right(
        Program(
          version = (1, 0, 0),
          term = Apply(LamAbs("x", Var("x")), Const(Constant(DefaultUniInteger, BigInt(0))))
        )
      )
    )
  }
}
