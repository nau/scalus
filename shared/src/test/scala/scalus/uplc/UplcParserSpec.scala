package scalus.uplc

import cats.implicits.toShow
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.Constant.given
import scalus.uplc.Data.{B, Constr, I, List, Map}
import scalus.uplc.DefaultUni
import scalus.uplc.DefaultUni.{Bool, ByteString, Integer, ProtoList, ProtoPair, asConstant}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.utils.Utils.*

import scala.collection.immutable

trait ArbitraryInstances:

  implicit val iArb: Arbitrary[I] = Arbitrary(Arbitrary.arbitrary[BigInt].map(l => I(l)))
  implicit val bArb: Arbitrary[B] = Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(B.apply))
  implicit val arbData: Arbitrary[Data] = Arbitrary {
    def constrGen(sz: Int): Gen[Constr] = for
      c <- Arbitrary.arbitrary[Long].map(Math.abs)
      n <- Gen.choose(sz / 3, sz / 2)
      args <- Gen.listOfN(n, sizedTree(sz / 2))
    yield Constr(c, args)

    def listGen(sz: Int): Gen[List] = for
      n <- Gen.choose(sz / 3, sz / 2)
      args <- Gen.listOfN(n, sizedTree(sz / 2))
    yield List(args)

    def mapGen(sz: Int): Gen[Map] = for
      n <- Gen.choose(sz / 3, sz / 2)
      tuple = Gen.zip(sizedTree(sz / 2), sizedTree(sz / 2))
      args <- Gen.mapOfN(n, tuple)
    yield Map(args.toList)

    def sizedTree(sz: Int): Gen[Data] =
      if sz <= 0 then Gen.oneOf(iArb.arbitrary, bArb.arbitrary)
      else
        Gen.frequency(
          (1, iArb.arbitrary),
          (1, bArb.arbitrary),
          (3, Gen.oneOf(constrGen(sz), listGen(sz), mapGen(sz)))
          //          (3, listGen(sz))
          //          (3, constrGen(sz))
          //          (3, mapGen(sz))
        )

    Gen.sized(sizedTree)
  }

  implicit lazy val arbitraryDefaultUni: Arbitrary[DefaultUni] = Arbitrary {
    def listGen(sz: Int): Gen[DefaultUni] = for a <- sizedTree(sz / 2)
    yield DefaultUni.Apply(ProtoList, a)

    def pairGen(sz: Int): Gen[DefaultUni] = for
      a <- sizedTree(sz / 2)
      b <- sizedTree(sz / 2)
    yield DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b)

    def sizedTree(sz: Int): Gen[DefaultUni] =
      val simple = Gen.oneOf(
        DefaultUni.Bool,
        DefaultUni.ByteString,
        //      DefaultUni.Data,
        DefaultUni.Integer,
        DefaultUni.String,
        DefaultUni.Unit
      )
      if sz <= 0 then simple
      else
        Gen.frequency(
          (3, simple),
          (1, Gen.oneOf(listGen(sz), pairGen(sz)))
        )
    Gen.sized(sizedTree)
  }

  def arbConstantByType(t: DefaultUni): Gen[Constant] =
    t match
      case DefaultUni.Integer    => Arbitrary.arbitrary[BigInt].map(Constant.Integer.apply)
      case DefaultUni.ByteString => Arbitrary.arbitrary[Array[Byte]].map(Constant.ByteString.apply)
      case DefaultUni.String     => Arbitrary.arbitrary[String].map(Constant.String.apply)
      case DefaultUni.Unit       => Gen.const(Constant.Unit)
      case DefaultUni.Bool       => Gen.oneOf(Constant.Bool(true), Constant.Bool(false))
      case DefaultUni.Apply(ProtoList, arg) =>
        for
          n <- Gen.choose(0, 10)
          elems <- Gen.listOfN(n, arbConstantByType(arg))
        yield Constant.List(arg, elems)
      // don't generate data for now, Plutus doesn't support it yet
      //        case DefaultUni.Data          => ???
      case DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b) =>
        for
          vala <- arbConstantByType(a)
          valb <- arbConstantByType(b)
        yield Constant.Pair(vala, valb)
      case _ => sys.error(s"unsupported type: $t")

  implicit lazy val arbitraryConstant: Arbitrary[Constant] = Arbitrary(
    for
      a <- arbitraryDefaultUni.arbitrary
      value <- arbConstantByType(a)
    yield value
  )
  implicit lazy val arbitraryTerm: Arbitrary[Term] = Arbitrary {
    val nameGen = for
      alpha <- Gen.alphaChar
      n <- Gen.choose(0, 10)
      rest <- Gen
        .listOfN(n, Gen.oneOf(Gen.alphaNumChar, Gen.const("_"), Gen.const("'")))
        .map(_.mkString)
    yield alpha + rest
    val varGen = nameGen.map(n => Var(NamedDeBruijn(n)))
    val builtinGen: Gen[Term] = for b <- Gen.oneOf(DefaultFun.values) yield Term.Builtin(b)
    val constGen: Gen[Term] = for c <- Arbitrary.arbitrary[Constant] yield Term.Const(c)

    def sizedTermGen(sz: Int): Gen[Term] =
      val simple = Gen.oneOf(varGen, Gen.const(Term.Error("error")), builtinGen, constGen)
      if sz <= 0 then simple
      else
        Gen.frequency(
          (1, simple),
          (2, Gen.oneOf(forceGen(sz), delayGen(sz))),
          (3, Gen.oneOf(lamGen(sz), appGen(sz)))
        )

    def forceGen(sz: Int): Gen[Term] = for t <- sizedTermGen(sz / 2) yield Term.Force(t)
    def delayGen(sz: Int): Gen[Term] = for t <- sizedTermGen(sz / 2) yield Term.Delay(t)
    def lamGen(sz: Int): Gen[Term] = for
      name <- nameGen
      t <- sizedTermGen(sz / 2)
    yield Term.LamAbs(name, t)
    def appGen(sz: Int): Gen[Term] = for
      t1 <- sizedTermGen(sz / 2)
      t2 <- sizedTermGen(sz / 2)
    yield Term.Apply(t1, t2)

    Gen.sized(sizedTermGen)
  }

  implicit lazy val arbitraryProgram: Arbitrary[Program] = Arbitrary {
    for
      maj <- Gen.posNum[Int]
      min <- Gen.posNum[Int]
      patch <- Gen.posNum[Int]
      term <- Arbitrary.arbitrary[Term]
    yield Program((maj, min, patch), term)
  }

class UplcParserSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  val parser = UplcParser
  test("Parse program version") {
    def p(input: String) = parser.programVersion.parse(input)
    def check(input: String, expected: (Int, Int, Int)) =
      val Right((_, result)) = p(input)
      assert(result == expected)
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
          term = hex"001234ff" $ true $ false $ () $ "Hello"
        )
      )
    )
  }

  test("Parse delay/force/error") {
    val r = parser.parseProgram("(program 1.0.0 (force (delay (error))))")
    assert(
      r == Right(
        Program(version = (1, 0, 0), term = Force(Delay(Error("error"))))
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
