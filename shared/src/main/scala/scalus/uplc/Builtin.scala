package scalus.uplc

import scalus.uplc.Cek.{CekValue, VCon}
import scalus.uplc.Constant.given
import scalus.uplc.DefaultUni.{asConstant, Bool, Integer, given}

import scala.annotation.targetName
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scalus.utils.Utils

enum TypeScheme:
  case Type(argType: DefaultUni)
  case Arrow(argType: TypeScheme, t: TypeScheme)
  case All(name: String, t: TypeScheme)
  case TVar(name: String)

  def ->:(t: TypeScheme): TypeScheme = Arrow(t, this)
  def ->:(t: DefaultUni): TypeScheme = Arrow(Type(t), this)

extension (x: DefaultUni)
  def ->:(t: DefaultUni): TypeScheme = TypeScheme.Arrow(TypeScheme.Type(t), TypeScheme.Type(x))

//  def ->:(t: TypeScheme): TypeScheme = TypeScheme.Arrow(t, TypeScheme.Type(x))

case class Runtime(
    typeScheme: TypeScheme,
    f: AnyRef
)

trait Meaning:
  def typeScheme: TypeScheme

object Meaning:
  def mkMeaning(t: TypeScheme, f: AnyRef) = Runtime(t, f)
  import TypeScheme.*

  val AddInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () => Cek.VCon(asConstant(aa + bb))
    )
  val SubtractInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () => Cek.VCon(asConstant(aa - bb))
    )
  val MultiplyInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () => Cek.VCon(asConstant(aa * bb))
    )
  val DivideInteger =
    import java.math.{BigDecimal, RoundingMode}
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () =>
            val r = new BigDecimal(aa.bigInteger)
              .divide(new BigDecimal(bb.bigInteger), RoundingMode.FLOOR)
              .toBigInteger()
            Cek.VCon(asConstant(BigInt(r)))
    )
  val QuotientInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () => Cek.VCon(asConstant(aa / bb))
    )
  val RemainderInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () => Cek.VCon(asConstant(aa % bb))
    )
  val ModInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () =>
            /*divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                                     where qr@(q,r) = quotRem n d */
            val r = aa % bb
            if r.signum == -bb.signum then Cek.VCon(asConstant(r + bb)) else Cek.VCon(asConstant(r))
    )
  val EqualsInteger =
    mkMeaning(
      Integer ->: Integer ->: Bool,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () => Cek.VCon(asConstant(aa == bb))
    )
  val LessThanEqualsInteger =
    mkMeaning(
      Integer ->: Integer ->: Bool,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () => Cek.VCon(asConstant(aa <= bb))
    )
  val LessThanInteger =
    mkMeaning(
      Integer ->: Integer ->: Bool,
      (a: CekValue) =>
        val aa = a.asInteger
        (b: CekValue) =>
          val bb = b.asInteger
          () => Cek.VCon(asConstant(aa < bb))
    )

  val EqualsByteString =
    mkMeaning(
      DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
      (a: CekValue) =>
        val aa = a.asByteString
        (b: CekValue) =>
          val bb = b.asByteString
          () => Cek.VCon(asConstant(aa == bb))
    )

  val EqualsString =
    mkMeaning(
      DefaultUni.String ->: DefaultUni.String ->: Bool,
      (a: CekValue) =>
        val aa = a.asString
        (b: CekValue) =>
          val bb = b.asString
          () => Cek.VCon(asConstant(aa == bb))
    )

  val IfThenElse =
    mkMeaning(
      All("a", Bool ->: TVar("a") ->: TVar("a") ->: TVar("a")),
      (b: CekValue) =>
        val bb = b.asBool
        (t: CekValue) => (f: CekValue) => () => if bb then t else f
    )

  /*
    unConstrData : [ data ] -> pair(integer, list(data))
   */
  val UnConstrData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.Pair(Integer, DefaultUni.List(DefaultUni.Data)),
      (a: CekValue) =>
        a match
          case VCon(Constant.Data(Data.Constr(i, ls))) =>
            () =>
              Cek.VCon(
                Constant.Pair(asConstant(i), asConstant(ls))
              )
          case _ => throw new Exception(s"unConstrData: not a constructor, but $a")
    )

  /*  unMapData : [ data ] -> list(pair(data, data))
   */
  val UnMapData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)),
      (a: CekValue) =>
        a match
          case VCon(Constant.Data(Data.Map(values))) =>
            () =>
              Cek.VCon(
                Constant.List(
                  DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                  values.map { case (k, v) =>
                    Constant.Pair(asConstant(k), asConstant(v))
                  }
                )
              )
          case _ => throw new Exception(s"unMapData: not a map, but $a")
    )
  /*  unListData : [ data ] -> list(data)
   */
  val UnListData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.List(DefaultUni.Data),
      (a: CekValue) =>
        a match
          case VCon(Constant.Data(Data.List(values))) =>
            () => Cek.VCon(Constant.List(DefaultUni.Data, values.map(asConstant)))
          case _ => throw new Exception(s"unListData: not a list, but $a")
    )

  /*  unIData : [ data ] -> integer
   */
  val UnIData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.Integer,
      (a: CekValue) =>
        a match
          case VCon(Constant.Data(Data.I(i))) =>
            () => Cek.VCon(asConstant(i))
          case _ => throw new Exception(s"unIData: not an integer, but $a")
    )

  /*  unBData : [ data ] -> bytestring
   */
  val UnBData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.ByteString,
      (a: CekValue) =>
        a match
          case VCon(Constant.Data(Data.B(b))) =>
            () => Cek.VCon(asConstant(b))
          case _ => throw new Exception(s"unBData: not a bytestring, but $a")
    )

  // [ forall a, list(a) ] -> bool
  val NullList =
    mkMeaning(
      // FIXME wrong type
      All("a", Bool ->: Bool),
      (a: CekValue) =>
        val ls = a.asList
        () => Cek.VCon(asConstant(ls.isEmpty))
    )

  // [ forall a, list(a) ] -> a
  val HeadList =
    mkMeaning(
      // FIXME wrong type
      All("a", Bool ->: Bool),
      (a: CekValue) =>
        val ls = a.asList
        () => Cek.VCon(ls.head)
    )

  // [ forall a, list(a) ] -> list(a)
  val TailList =
    mkMeaning(
      // FIXME wrong type
      All("a", Bool ->: Bool),
      (a: CekValue) =>
        a match
          case VCon(Constant.List(tpe, ls)) =>
            () => Cek.VCon(Constant.List(tpe, ls.tail))
          case _ => throw new Exception(s"TailList: not a list, but $a")
    )

  // [ forall a, forall b, list(a), b, b ] -> b
  val ChooseList =
    mkMeaning(
      // FIXME wrong type
      All("a", All("b", Bool ->: Bool ->: Bool ->: Bool)),
      (a: CekValue) =>
        val ls = a.asList
        (b: CekValue) => (c: CekValue) => () => if ls.isEmpty then b else c
    )

  // [ forall a, forall b, pair(a, b) ] -> a
  val FstPair =
    mkMeaning(
      // FIXME wrong type
      All("a", All("b", DefaultUni.Pair(Integer, Bool) ->: Integer)),
      (a: CekValue) =>
        val (fst, _) = a.asPair
        () => Cek.VCon(fst)
    )

  // [ forall a, forall b, pair(a, b) ] -> b
  val SndPair =
    mkMeaning(
      // FIXME wrong type
      All("a", All("b", DefaultUni.Pair(Integer, Bool) ->: Bool)),
      (a: CekValue) =>
        val (_, snd) = a.asPair
        () => Cek.VCon(snd)
    )

  val ConstrData =
    mkMeaning(
      Integer ->: DefaultUni.List(DefaultUni.Data) ->: DefaultUni.Data,
      (a: CekValue) =>
        val i = a.asInteger
        (b: CekValue) =>
          val args = b match {
            case VCon(Constant.List(DefaultUni.Data, l)) =>
              l.map {
                case Constant.Data(d) => d
                case _                => throw new Exception(s"ConstrData: not a data, but $b")
              }
            case _ => throw new RuntimeException(s"Expected list, got $this")
          }
          () =>
            Cek.VCon(
              Constant.Data(Data.Constr(i.longValue, args))
            )
    )
  val MkCons =
    mkMeaning(
      All("a", Integer ->: DefaultUni.List(Integer) ->: DefaultUni.List(Integer)),
      (a: CekValue) =>
        (b: CekValue) =>
          (a, b) match
            case (VCon(aCon), VCon(Constant.List(tp, l))) => // fixme chek type
              () => Cek.VCon(Constant.List(tp, aCon :: l))
            case _ => throw new RuntimeException(s"Expected list, got $this")
    )

  val BData =
    mkMeaning(
      DefaultUni.ByteString ->: DefaultUni.Data,
      (a: CekValue) =>
        val aa = a.asByteString
        () => Cek.VCon(Constant.Data(Data.B(aa)))
    )

  val ListData =
    mkMeaning(
      DefaultUni.List(DefaultUni.Data) ->: DefaultUni.Data,
      (a: CekValue) =>
        val aa = a.asList
        val datas = aa.map {
          case Constant.Data(value) => value
          case _                    => throw new RuntimeException(s"ListData: not a data, but $a")
        }
        () => Cek.VCon(Constant.Data(Data.List(datas)))
    )

  val Sha2_256 =
    mkMeaning(
      DefaultUni.ByteString ->: DefaultUni.ByteString,
      (a: CekValue) =>
        val aa = a.asByteString
        () =>
          // FIXME: this is not correct
          Cek.VCon(Constant.ByteString(scalus.builtins.ByteString.fromHex("00")))
    )

  val BuiltinMeanings: immutable.Map[DefaultFun, Runtime] = immutable.Map.apply(
    (DefaultFun.AddInteger, Meaning.AddInteger),
    (DefaultFun.SubtractInteger, Meaning.SubtractInteger),
    (DefaultFun.MultiplyInteger, Meaning.MultiplyInteger),
    (DefaultFun.DivideInteger, Meaning.DivideInteger),
    (DefaultFun.QuotientInteger, Meaning.QuotientInteger),
    (DefaultFun.RemainderInteger, Meaning.RemainderInteger),
    (DefaultFun.ModInteger, Meaning.ModInteger),
    (DefaultFun.EqualsInteger, Meaning.EqualsInteger),
    (DefaultFun.LessThanEqualsInteger, Meaning.LessThanEqualsInteger),
    (DefaultFun.LessThanInteger, Meaning.LessThanInteger),
    (DefaultFun.EqualsByteString, Meaning.EqualsByteString),
    (DefaultFun.EqualsString, Meaning.EqualsString),
    (DefaultFun.IfThenElse, Meaning.IfThenElse),
    (DefaultFun.UnConstrData, Meaning.UnConstrData),
    (DefaultFun.UnMapData, Meaning.UnMapData),
    (DefaultFun.UnListData, Meaning.UnListData),
    (DefaultFun.UnIData, Meaning.UnIData),
    (DefaultFun.UnBData, Meaning.UnBData),
    (DefaultFun.NullList, Meaning.NullList),
    (DefaultFun.HeadList, Meaning.HeadList),
    (DefaultFun.TailList, Meaning.TailList),
    (DefaultFun.ChooseList, Meaning.ChooseList),
    (DefaultFun.FstPair, Meaning.FstPair),
    (DefaultFun.SndPair, Meaning.SndPair),
    (DefaultFun.ConstrData, Meaning.ConstrData),
    (DefaultFun.MkCons, Meaning.MkCons),
    (DefaultFun.BData, Meaning.BData),
    (DefaultFun.ListData, Meaning.ListData),
    (DefaultFun.Sha2_256, Meaning.Sha2_256)
  )
