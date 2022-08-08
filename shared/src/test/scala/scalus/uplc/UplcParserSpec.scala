package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite

class UplcParserSpec extends AnyFunSuite {
  val parser = new UplcParser
  test("Parse program version") {
    def p(input: String) = fastparse.parse(input, parser.programVersion(_))
    def check(input: String, expected: (Int, Int, Int)) = {
      val result = p(input)
      assert(result.isSuccess)
      assert(result.get.value == expected)
    }
    check("111.2.33333   ", (111, 2, 33333))
    check("1.0.03   ", (1, 0, 3))
    assert(!p("1 . 2 . 3").isSuccess)
    assert(!p("1.2").isSuccess)
    assert(!p("1.2.a").isSuccess)
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

  test("Parse constants") {
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
