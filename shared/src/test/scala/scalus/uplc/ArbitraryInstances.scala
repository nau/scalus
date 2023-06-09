package scalus.uplc

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scalus.*
import scalus.builtins
import scalus.uplc.Data.B
import scalus.uplc.Data.Constr
import scalus.uplc.Data.I
import scalus.uplc.Data.List
import scalus.uplc.Data.Map
import scalus.uplc.DefaultUni.Bool
import scalus.uplc.DefaultUni.ByteString
import scalus.uplc.DefaultUni.Integer
import scalus.uplc.DefaultUni.ProtoList
import scalus.uplc.DefaultUni.ProtoPair
import scalus.uplc.Term.*

import scala.collection.immutable
import org.scalacheck.Shrink

trait ArbitraryInstances:

  implicit val byteStringArb: Arbitrary[builtins.ByteString] = Arbitrary(
    for
      sz <- Gen.frequency((0, Gen.choose(1000, 10000)), (10, Gen.choose(0, 100)))
      bytes <- Gen.containerOfN[Array, Byte](sz, Arbitrary.arbitrary)
    yield builtins.ByteString.unsafeFromArray(bytes)
  )
  implicit val iArb: Arbitrary[I] = Arbitrary(
    for n <- Gen.oneOf[BigInt](
        Gen.const[BigInt](0),
        Gen.const[BigInt](-1),
        Gen.const[BigInt](1),
        Gen.const[BigInt](-1000),
        Gen.const[BigInt](1000),
        Gen.const[BigInt](Int.MaxValue),
        Gen.const[BigInt](Int.MinValue),
        Gen.const[BigInt](Long.MaxValue),
        Gen.const[BigInt](Long.MinValue),
        Gen.choose[BigInt](2 ^ 32, 2 ^ 64)
      )
    yield I(n)
  )
  implicit val bArb: Arbitrary[B] = Arbitrary(Arbitrary.arbitrary[builtins.ByteString].map(B.apply))

  given Shrink[Data] =
    Shrink {
      case I(i) => Shrink.shrink(i).map(I.apply)
      case B(b) => Shrink.shrink(b).map(B.apply)
      case Constr(c, args) =>
        Shrink.shrink(args).map(Constr(c, _))
      case List(args) => Shrink.shrink(args).map(List.apply)
      case Map(args)  => Shrink.shrink(args).map(Map.apply)
    }

  implicit val arbData: Arbitrary[Data] = Arbitrary {
    def constrGen(sz: Int): Gen[Constr] = for
      c <- Gen.posNum[Long]
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
          (1, Gen.oneOf(constrGen(sz), listGen(sz), mapGen(sz)))
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
        // don't generate Data, because current Plutus uplc doesn't support its parsing
//        DefaultUni.Data,
        DefaultUni.Integer,
        // FIXME: don't generate strings because we don't handle Haskell style escapes
        // and we get errors parsing results of calling uplc on generated terms
//        DefaultUni.String,
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
      case DefaultUni.Integer => Arbitrary.arbitrary[BigInt].map(Constant.Integer.apply)
      case DefaultUni.ByteString =>
        Arbitrary.arbitrary[builtins.ByteString].map(Constant.ByteString.apply)
      case DefaultUni.String => Arbitrary.arbitrary[String].map(Constant.String.apply)
      case DefaultUni.Data   => Arbitrary.arbitrary[Data].map(Constant.Data.apply)
      case DefaultUni.Unit   => Gen.const(Constant.Unit)
      case DefaultUni.Bool   => Gen.oneOf(Constant.Bool(true), Constant.Bool(false))
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

    def varGen(env: immutable.List[String]) = Gen.oneOf(env).map(n => Var(NamedDeBruijn(n)))
    val builtinGen: Gen[Term] = for b <- Gen.oneOf(DefaultFun.values) yield Term.Builtin(b)
    val constGen: Gen[Term] = for c <- Arbitrary.arbitrary[Constant] yield Term.Const(c)

    def sizedTermGen(sz: Int, env: immutable.List[String]): Gen[Term] =
      val maybeVarTerm = if env.isEmpty then Seq.empty else Seq(varGen(env))
      val simple = Gen.oneOf(
        Gen.const(Term.Error),
        builtinGen,
        (Seq(constGen) ++ maybeVarTerm): _*
      )
      if sz <= 0 then simple
      else
        Gen.frequency(
          (1, simple),
          (2, Gen.oneOf(forceGen(sz, env), delayGen(sz, env))),
          (3, Gen.oneOf(lamGen(sz, env), appGen(sz, env)))
        )

    def forceGen(sz: Int, env: immutable.List[String]): Gen[Term] =
      for t <- sizedTermGen(sz / 2, env)
      yield Term.Force(t)
    def delayGen(sz: Int, env: immutable.List[String]): Gen[Term] =
      for t <- sizedTermGen(sz / 2, env)
      yield Term.Delay(t)
    def lamGen(sz: Int, env: immutable.List[String]): Gen[Term] = for
      name <- nameGen
      t <- sizedTermGen(sz / 2, name :: env)
    yield Term.LamAbs(name, t)
    def appGen(sz: Int, env: immutable.List[String]): Gen[Term] = for
      t1 <- sizedTermGen(sz / 2, env)
      t2 <- sizedTermGen(sz / 2, env)
    yield Term.Apply(t1, t2)

    Gen.sized(sizedTermGen(_, Nil))
  }

  implicit lazy val arbitraryProgram: Arbitrary[Program] = Arbitrary {
    for
      maj <- Gen.posNum[Int]
      min <- Gen.posNum[Int]
      patch <- Gen.posNum[Int]
      term <- Arbitrary.arbitrary[Term]
    yield Program((maj, min, patch), term)
  }

  given arbList[A: Arbitrary]: Arbitrary[scalus.prelude.List[A]] = Arbitrary {
    for lst <- Arbitrary.arbitrary[immutable.List[A]]
    yield scalus.prelude.List(lst: _*)
  }

  given arbMaybe[A: Arbitrary]: Arbitrary[scalus.prelude.Maybe[A]] = Arbitrary {
    for o <- Arbitrary.arbitrary[Option[A]]
    yield o match
      case None        => scalus.prelude.Maybe.Nothing
      case Some(value) => scalus.prelude.Maybe.Just(value)
  }

  given arbAssocMap[A: Arbitrary, B: Arbitrary]: Arbitrary[scalus.prelude.AssocMap[A, B]] =
    Arbitrary {
      for map <- Arbitrary.arbitrary[immutable.Map[A, B]]
      yield scalus.prelude.AssocMap.fromList(scalus.prelude.List(map.toSeq: _*))
    }
