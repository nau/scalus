package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DefaultUni
import scalus.uplc.DefaultUni.{Integer, ProtoList, ProtoPair}
import scalus.uplc.Term.*

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
    assert(p("bool") == Right(DefaultUni.Bool))
    assert(p("bytestring") == Right(DefaultUni.ByteString))
    assert(p("data") == Right(DefaultUni.Data))
    assert(p("integer") == Right(DefaultUni.Integer))
    assert(
      p("list (integer )") == Right(DefaultUni.Apply(ProtoList, DefaultUni.Integer))
    )
    assert(
      p("list (list(unit) )") == Right(
        DefaultUni.Apply(
          ProtoList,
          DefaultUni.Apply(ProtoList, DefaultUni.Unit)
        )
      )
    )
    assert(
      p("pair (integer)(bool)") == Right(
        DefaultUni.Apply(
          DefaultUni.Apply(ProtoPair, DefaultUni.Integer),
          DefaultUni.Bool
        )
      )
    )
    assert(
      p("pair (list(list(unit))) (pair(integer)(bool) )") == Right(
        DefaultUni.Apply(
          DefaultUni.Apply(
            ProtoPair,
            DefaultUni.Apply(
              ProtoList,
              DefaultUni.Apply(ProtoList, DefaultUni.Unit)
            )
          ),
          DefaultUni.Apply(DefaultUni.Apply(ProtoPair, DefaultUni.Integer), DefaultUni.Bool)
        )
      )
    )
    assert(p("string") == Right(DefaultUni.String))
    assert(p("unit") == Right(DefaultUni.Unit))
  }

  test("Parse constants") {
    import cats.implicits.toShow
    def p(input: String) = parser.conTerm.parse(input).map(_._2).left.map(e => e.show)
    assert(
      p("(con list(integer) [1,2, 3333])") == Right(
        Const(
          Constant(
            DefaultUni.Apply(ProtoList, Integer),
            Constant(Integer, 1) :: Constant(Integer, 2) :: Constant(
              Integer,
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
            DefaultUni.Apply(
              DefaultUni.Apply(ProtoPair, Integer),
              DefaultUni.Bool
            ),
            (Constant(Integer, 12), Constant(DefaultUni.Bool, false))
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
                  Const(Constant(DefaultUni.ByteString, Seq[Byte](0, 18, 52, -1))),
                  Const(Constant(DefaultUni.Bool, true))
                ),
                Const(Constant(DefaultUni.Bool, false))
              ),
              Const(Constant(DefaultUni.Unit, ()))
            ),
            Const(Constant(DefaultUni.String, "Hello"))
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
          term = Apply(LamAbs("x", Var("x")), Const(Constant(Integer, BigInt(0))))
        )
      )
    )
  }
}
