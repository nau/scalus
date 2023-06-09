package scalus.uplc

import cats.implicits.toShow
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtins
import scalus.uplc.Constant.given
import scalus.uplc.DefaultUni.Bool
import scalus.uplc.DefaultUni.ByteString
import scalus.uplc.DefaultUni.Integer
import scalus.uplc.DefaultUni.ProtoList
import scalus.uplc.DefaultUni.ProtoPair
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{_, given}

class UplcParserSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  val parser = UplcParser
  test("Parse program version") {
    def p(input: String) = parser.programVersion.parse(input)
    def check(input: String, expected: (Int, Int, Int)) =
      p(input) match
        case Left(value)        => fail(value.show)
        case Right((_, result)) => assert(result == expected)
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
        Program(version = (1, 0, 0), term = Var(NamedDeBruijn("x")))
      )
    )
  }

  test("Parse lam") {
    val r = parser.parseProgram("(program 1.0.0 (lam x x) )")
    assert(
      r == Right(
        Program(version = (1, 0, 0), term = LamAbs("x", Var(NamedDeBruijn("x"))))
      )
    )
  }

  test("Parse lam/app") {
    val r = parser.parseProgram("(program 1.0.0 [(lam x x) y z])")
    assert(
      r == Right(
        Program(
          version = (1, 0, 0),
          term = Apply(
            Apply(LamAbs("x", Var(NamedDeBruijn("x"))), Var(NamedDeBruijn("y"))),
            Var(NamedDeBruijn("z"))
          )
        )
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
      p("(list integer )") == Right(DefaultUni.Apply(ProtoList, DefaultUni.Integer))
    )
    assert(
      p("(list (list unit) )") == Right(
        DefaultUni.Apply(
          ProtoList,
          DefaultUni.Apply(ProtoList, DefaultUni.Unit)
        )
      )
    )
    assert(
      p("(pair integer bool)") == Right(
        DefaultUni.Apply(
          DefaultUni.Apply(ProtoPair, DefaultUni.Integer),
          DefaultUni.Bool
        )
      )
    )
    assert(
      p("(pair (list (list unit)) (pair integer bool) )") == Right(
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
      p("(con (list integer) [1,2, 3333])") == Right(Seq(1, 2, 3333): Term)
    )

    assert(
      p("(con (pair integer bool) (12, False))") == Right((12, false): Term)
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
          term = builtins.ByteString.fromHex("001234ff") $ true $ false $ () $ "Hello"
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

  test("Parse builtins") {
    import cats.implicits.toShow
    def p(input: String) = parser.builtinTerm.parse(input).map(_._2).left.map(e => e.show)

    assert(p("(builtin addInteger)") == Right(Builtin(DefaultFun.AddInteger)))
    assert(p("(builtin appendByteString)") == Right(Builtin(DefaultFun.AppendByteString)))
    assert(p("(builtin nonexistent)").left.get.contains("unknown builtin function: nonexistent"))
  }

  test("Parse program") {
    val r = parser.parseProgram("(program 1.0.0 [(lam x x) (con integer 0)])")
    assert(
      r == Right(
        Program(
          version = (1, 0, 0),
          term = Apply(LamAbs("x", Var(NamedDeBruijn("x"))), Const(Constant.Integer(BigInt(0))))
        )
      )
    )
  }

  test("Pretty-printer <-> parser isomorphism") {

    forAll { (t: DefaultUni) =>
      val pretty = t.pretty.render(80)
      val parsed = parser.defaultUni.parse(pretty).map(_._2).left.map(e => e.show)
      assert(parsed == Right(t))
    }

    forAll { (t: Constant) =>
      val pretty = t.pretty.render(80)
      val parsed = parser.constant.parse(pretty).map(_._2).left.map(e => e.show)
      assert(parsed == Right(t))
    }

    forAll { (t: Term) =>
      val pretty = t.pretty.render(80)
      val parsed = parser.term.parse(pretty).map(_._2).left.map(e => e.show)
      assert(parsed == Right(t))
    }

    forAll { (t: Program) =>
      val pretty = t.pretty.render(80)
      val parsed = parser.parseProgram(pretty)
      assert(parsed == Right(t))
    }
  }
