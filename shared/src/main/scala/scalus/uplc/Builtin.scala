package scalus.uplc

import scalus.uplc.Cek.{CekValue, VCon}
import scalus.uplc.Constant.given
import scalus.uplc.DefaultUni.{Bool, Integer, asConstant, given}

import scala.annotation.targetName
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

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
        val VCon(Constant.Integer(aa)) = a
        (b: CekValue) =>
          val VCon(Constant.Integer(bb)) = b
          () => Cek.VCon(asConstant(aa + bb))
    )
  val MultiplyInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val VCon(Constant.Integer(aa)) = a
        (b: CekValue) =>
          val VCon(Constant.Integer(bb)) = b
          () => Cek.VCon(asConstant(aa * bb))
    )
  val EqualsInteger =
    mkMeaning(
      Integer ->: Integer ->: Bool,
      (a: CekValue) =>
        val VCon(Constant.Integer(aa)) = a
        (b: CekValue) =>
          val VCon(Constant.Integer(bb)) = b
          () => Cek.VCon(asConstant(aa == bb))
    )
  val LessThanEqualsInteger =
    mkMeaning(
      Integer ->: Integer ->: Bool,
      (a: CekValue) =>
        val VCon(Constant.Integer(aa)) = a
        (b: CekValue) =>
          val VCon(Constant.Integer(bb)) = b
          () => Cek.VCon(asConstant(aa <= bb))
    )

  val EqualsByteString =
    mkMeaning(
      DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
      (a: CekValue) =>
        val VCon(aa: Constant.ByteString) = a
        (b: CekValue) =>
          val VCon(bb: Constant.ByteString) = b
          () => Cek.VCon(asConstant(aa == bb))
    )

  val IfThenElse =
    mkMeaning(
      All("a", Bool ->: TVar("a") ->: TVar("a") ->: TVar("a")),
      (b: CekValue) =>
        val VCon(Constant.Bool(bb)) = b
        (t: CekValue) => (f: CekValue) => () => if bb then t else f
    )

  /*
    unConstrData : [ data ] -> pair(integer, list(data))
   */
  val UnConstrData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.Pair(Integer, DefaultUni.List(DefaultUni.Data)),
      (a: CekValue) =>
        val VCon(Constant.Data(Data.Constr(i, ls))) = a
        () =>
          Cek.VCon(
            Constant.Pair(asConstant(i), asConstant(ls))
          )
    )

  /*  unMapData : [ data ] -> list(pair(data, data))
   */
  val UnMapData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)),
      (a: CekValue) =>
        val VCon(Constant.Data(Data.Map(values))) = a
        () =>
          Cek.VCon(
            Constant.List(
              DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
              values.map { case (k, v) =>
                Constant.Pair(asConstant(k), asConstant(v))
              }
            )
          )
    )
  /*  unListData : [ data ] -> list(data)
   */
  val UnListData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.List(DefaultUni.Data),
      (a: CekValue) =>
        val VCon(Constant.Data(Data.List(values))) = a
        () => Cek.VCon(Constant.List(DefaultUni.Data, values.map(asConstant)))
    )

  /*  unIData : [ data ] -> integer
   */
  val UnIData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.Integer,
      (a: CekValue) =>
        val VCon(Constant.Data(Data.I(v))) = a
        () => Cek.VCon(asConstant(v))
    )

  /*  unBData : [ data ] -> bytestring
   */
  val UnBData =
    mkMeaning(
      DefaultUni.Data ->: DefaultUni.ByteString,
      (a: CekValue) =>
        val VCon(Constant.Data(Data.B(v))) = a
        () => Cek.VCon(asConstant(v))
    )

  // [ forall a, list(a) ] -> bool
  val NullList =
    mkMeaning(
      // FIXME wrong type
      All("a", Bool ->: Bool),
      (a: CekValue) =>
        val VCon(Constant.List(_, ls)) = a
        () => Cek.VCon(asConstant(ls.isEmpty))
    )

  // [ forall a, list(a) ] -> a
  val HeadList =
    mkMeaning(
      // FIXME wrong type
      All("a", Bool ->: Bool),
      (a: CekValue) =>
        val VCon(Constant.List(_, ls)) = a
        () => Cek.VCon(ls.head)
    )

  // [ forall a, list(a) ] -> list(a)
  val TailList =
    mkMeaning(
      // FIXME wrong type
      All("a", Bool ->: Bool),
      (a: CekValue) =>
        val VCon(Constant.List(tpe, ls)) = a
        () => Cek.VCon(Constant.List(tpe, ls.tail))
    )

  // [ forall a, forall b, pair(a, b) ] -> a
  val FstPair =
    mkMeaning(
      // FIXME wrong type
      All("a", All("b", DefaultUni.Pair(Integer, Bool) ->: Integer)),
      (a: CekValue) =>
        val VCon(Constant.Pair(fst, _)) = a
        () => Cek.VCon(fst)
    )

  // [ forall a, forall b, pair(a, b) ] -> b
  val SndPair =
    mkMeaning(
      // FIXME wrong type
      All("a", All("b", DefaultUni.Pair(Integer, Bool) ->: Bool)),
      (a: CekValue) =>
        val VCon(Constant.Pair(_, snd)) = a
        () => Cek.VCon(snd)
    )

  val BuiltinMeanings: immutable.Map[DefaultFun, Runtime] = immutable.Map.apply(
    (DefaultFun.AddInteger, Meaning.AddInteger),
    (DefaultFun.MultiplyInteger, Meaning.MultiplyInteger),
    (DefaultFun.EqualsInteger, Meaning.EqualsInteger),
    (DefaultFun.LessThanEqualsInteger, Meaning.LessThanEqualsInteger),
    (DefaultFun.EqualsByteString, Meaning.EqualsByteString),
    (DefaultFun.IfThenElse, Meaning.IfThenElse),
    (DefaultFun.UnConstrData, Meaning.UnConstrData),
    (DefaultFun.UnMapData, Meaning.UnMapData),
    (DefaultFun.UnListData, Meaning.UnListData),
    (DefaultFun.UnIData, Meaning.UnIData),
    (DefaultFun.UnBData, Meaning.UnBData),
    (DefaultFun.NullList, Meaning.NullList),
    (DefaultFun.HeadList, Meaning.HeadList),
    (DefaultFun.TailList, Meaning.TailList),
    (DefaultFun.FstPair, Meaning.FstPair),
    (DefaultFun.SndPair, Meaning.SndPair)
  )
