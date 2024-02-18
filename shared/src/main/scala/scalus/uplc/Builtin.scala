package scalus.uplc

import scalus.builtin.Builtins.*
import scalus.builtin.Data
import scalus.builtin.PlatformSpecific
import scalus.uplc.Constant.given
import scalus.uplc.DefaultUni.Bool
import scalus.uplc.DefaultUni.Integer
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.DefaultUni.given
import scalus.uplc.eval.CekValue
import scalus.uplc.eval.KnownTypeUnliftingError
import scalus.uplc.eval.VCon

import scala.collection.immutable

enum TypeScheme:
    case Type(argType: DefaultUni)
    case App(f: TypeScheme, arg: TypeScheme)
    case Arrow(argType: TypeScheme, t: TypeScheme)
    case All(name: String, t: TypeScheme)
    case TVar(name: String)

    def ->:(t: TypeScheme): TypeScheme = Arrow(t, this)
    def ->:(t: DefaultUni): TypeScheme = Arrow(Type(t), this)
    infix def $(t: TypeScheme): TypeScheme = App(this, t)
    infix def $(t: String): TypeScheme = App(this, TVar(t))

//  def ->:(t: TypeScheme): TypeScheme = TypeScheme.Arrow(t, TypeScheme.Type(x))

case class Runtime(
    typeScheme: TypeScheme,
    f: AnyRef => AnyRef,
    args: Seq[AnyRef]
)

object Meaning:
    // local extension used to create a TypeScheme from a DefaultUni
    extension (x: DefaultUni)
        def ->:(t: TypeScheme): TypeScheme = TypeScheme.Arrow(t, TypeScheme.Type(x))
        def ->:(t: DefaultUni): TypeScheme =
            TypeScheme.Arrow(TypeScheme.Type(t), TypeScheme.Type(x))
        infix def $(t: TypeScheme): TypeScheme = TypeScheme.Type(x) $ t
        infix def $(t: String): TypeScheme = TypeScheme.Type(x) $ t

    def mkMeaning(t: TypeScheme, f: AnyRef) = Runtime(t, f.asInstanceOf[AnyRef => AnyRef], Seq.empty)
    import TypeScheme.*

    val AddInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(addInteger(aa, bb)))
        )
    val SubtractInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(subtractInteger(aa, bb)))
        )
    val MultiplyInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(multiplyInteger(aa, bb)))
        )
    val DivideInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(divideInteger(aa, bb)))
        )
    val QuotientInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(quotientInteger(aa, bb)))
        )
    val RemainderInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(remainderInteger(aa, bb)))
        )
    val ModInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(modInteger(aa, bb)))
        )
    val EqualsInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(equalsInteger(aa, bb)))
        )
    val LessThanEqualsInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(lessThanEqualsInteger(aa, bb)))
        )
    val LessThanInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(lessThanInteger(aa, bb)))
        )

    val AppendByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asByteString
                  (ps: PlatformSpecific) => VCon(asConstant(appendByteString(aa, bb)))
        )

    val ConsByteString =
        mkMeaning(
          DefaultUni.Integer ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asByteString
                  (ps: PlatformSpecific) => VCon(asConstant(consByteString(aa, bb)))
        )

    val SliceByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer ->: DefaultUni.Integer ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val bs = a.asByteString
              (b: CekValue) =>
                  val start = b.asInteger
                  (c: CekValue) =>
                      val end = c.asInteger
                      (ps: PlatformSpecific) =>
                          VCon(asConstant(sliceByteString(bs, start, end)))
        )

    val IndexByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer ->: DefaultUni.Integer,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asInteger
                  (ps: PlatformSpecific) => VCon(asConstant(indexByteString(aa, bb)))
        )

    val LengthOfByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer,
          (a: CekValue) =>
              val aa = a.asByteString
              (ps: PlatformSpecific) => VCon(asConstant(lengthOfByteString(aa)))
        )

    val EqualsByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asByteString
                  (ps: PlatformSpecific) => VCon(asConstant(aa == bb))
        )

    val LessThanByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asByteString
                  (ps: PlatformSpecific) => VCon(asConstant(lessThanByteString(aa, bb)))
        )

    val LessThanEqualsByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asByteString
                  (ps: PlatformSpecific) => VCon(asConstant(lessThanEqualsByteString(aa, bb)))
        )

    val Sha2_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asByteString
              (ps: PlatformSpecific) => VCon(asConstant(sha2_256(using ps)(aa)))
        )

    val Sha3_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asByteString
              (ps: PlatformSpecific) => VCon(asConstant(sha3_256(using ps)(aa)))
        )

    val Blake2b_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asByteString
              (ps: PlatformSpecific) => VCon(asConstant(blake2b_256(using ps)(aa)))
        )

    val VerifyEd25519Signature = {
        val tpe =
            DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool
        mkMeaning(
          tpe,
          (a: CekValue) =>
              val pk = a.asByteString
              (b: CekValue) =>
                  val msg = b.asByteString
                  (c: CekValue) =>
                      val sig = c.asByteString
                      (ps: PlatformSpecific) =>
                          VCon(
                            asConstant(verifyEd25519Signature(using ps)(pk, msg, sig))
                          )
        )
    }

    val VerifyEcdsaSecp256k1Signature = {
        val tpe =
            DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool
        mkMeaning(
          tpe,
          (a: CekValue) =>
              val pk = a.asByteString
              (b: CekValue) =>
                  val msg = b.asByteString
                  (c: CekValue) =>
                      val sig = c.asByteString
                      (ps: PlatformSpecific) =>
                          VCon(
                            asConstant(
                              verifyEcdsaSecp256k1Signature(using ps)(pk, msg, sig)
                            )
                          )
        )
    }

    val VerifySchnorrSecp256k1Signature = {
        val tpe =
            DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool
        mkMeaning(
          tpe,
          (a: CekValue) =>
              val pk = a.asByteString
              (b: CekValue) =>
                  val msg = b.asByteString
                  (c: CekValue) =>
                      val sig = c.asByteString
                      (ps: PlatformSpecific) =>
                          VCon(
                            asConstant(
                              verifySchnorrSecp256k1Signature(using ps)(pk, msg, sig)
                            )
                          )
        )
    }

    val AppendString =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.String ->: DefaultUni.String,
          (a: CekValue) =>
              val aa = a.asString
              (b: CekValue) =>
                  val bb = b.asString
                  (ps: PlatformSpecific) => VCon(asConstant(appendString(aa, bb)))
        )

    val EqualsString =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.String ->: Bool,
          (a: CekValue) =>
              val aa = a.asString
              (b: CekValue) =>
                  val bb = b.asString
                  (ps: PlatformSpecific) => VCon(asConstant(equalsString(aa, bb)))
        )

    val EncodeUtf8 = {
        val tpe = DefaultUni.String ->: DefaultUni.ByteString
        mkMeaning(
          tpe,
          (a: CekValue) =>
              val aa = a.asString
              (ps: PlatformSpecific) => VCon(asConstant(encodeUtf8(aa)))
        )
    }

    val DecodeUtf8 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.String,
          (a: CekValue) =>
              val aa = a.asByteString
              (ps: PlatformSpecific) => VCon(asConstant(decodeUtf8(aa)))
        )

    val IfThenElse =
        mkMeaning(
          All("a", Bool ->: TVar("a") ->: TVar("a") ->: TVar("a")),
          (b: CekValue) =>
              val bb = b.asBool
              (t: CekValue) => (f: CekValue) => (ps: PlatformSpecific) => ifThenElse(bb, t, f)
        )

    val ChooseUnit =
        mkMeaning(
          All("a", DefaultUni.Unit ->: TVar("a") ->: TVar("a")),
          (unit: CekValue) =>
              unit match
                  case VCon(Constant.Unit) => (a: CekValue) => (ps: PlatformSpecific) => a
                  case _                   => throw new Error("impossible")
        )

    val Trace =
        mkMeaning(
          All("a", DefaultUni.String ->: TVar("a") ->: TVar("a")),
          (a: CekValue) =>
              val aa = a.asString
              (b: CekValue) => (ps: PlatformSpecific) => trace(aa)(b)
        )

    // [ forall a, forall b, pair(a, b) ] -> a
    val FstPair =
        mkMeaning(
          All("a", All("b", (DefaultUni.ProtoPair $ "a" $ "b") ->: TVar("a"))),
          (a: CekValue) =>
              val (fst, _) = a.asPair
              (ps: PlatformSpecific) => VCon(fst)
        )

    // [ forall a, forall b, pair(a, b) ] -> b
    val SndPair =
        mkMeaning(
          All("a", All("b", (DefaultUni.ProtoPair $ "a" $ "b") ->: TVar("b"))),
          (a: CekValue) =>
              val (_, snd) = a.asPair
              (ps: PlatformSpecific) => VCon(snd)
        )

    // [ forall a, forall b, list(a), b, b ] -> b
    val ChooseList =
        mkMeaning(
          All(
            "a",
            All("b", (DefaultUni.ProtoList $ "a") ->: TVar("b") ->: TVar("b") ->: TVar("b"))
          ),
          (a: CekValue) =>
              val ls = a.asList
              (b: CekValue) =>
                  (c: CekValue) => (ps: PlatformSpecific) => if ls.isEmpty then b else c
        )

    // [ forall a, a, list(a) ] -> list(a)
    val MkCons =
        mkMeaning(
          All("a", TVar("a") ->: (DefaultUni.ProtoList $ "a") ->: (DefaultUni.ProtoList $ "a")),
          (a: CekValue) =>
              (b: CekValue) =>
                  (a, b) match
                      // Checking that the type of the constant is the same as the type of the elements
                      // of the unlifted list. Note that there's no way we could enforce this statically
                      // since in UPLC one can create an ill-typed program that attempts to prepend
                      // a value of the wrong type to a list.
                      case (VCon(aCon), VCon(Constant.List(tp, l))) =>
                          if aCon.tpe != tp then
                              throw new KnownTypeUnliftingError(TypeScheme.Type(tp), aCon.tpe)
                          else (ps: PlatformSpecific) => VCon(Constant.List(tp, aCon :: l))
                      case _ => throw new RuntimeException(s"Expected list, got $b")
        )

    // [ forall a, list(a) ] -> a
    val HeadList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: TVar("a")),
          (a: CekValue) =>
              val ls = a.asList
              (ps: PlatformSpecific) => VCon(ls.head)
        )

    // [ forall a, list(a) ] -> list(a)
    val TailList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: (DefaultUni.ProtoList $ "a")),
          (a: CekValue) =>
              a match
                  case VCon(Constant.List(tpe, ls)) =>
                      (ps: PlatformSpecific) => VCon(Constant.List(tpe, ls.tail))
                  case _ => throw new Exception(s"TailList: not a list, but $a")
        )

    // [ forall a, list(a) ] -> bool
    val NullList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: Type(Bool)),
          (a: CekValue) =>
              val ls = a.asList
              (ps: PlatformSpecific) => VCon(asConstant(ls.isEmpty))
        )

    val ChooseData =
        mkMeaning(
          All(
            "a",
            DefaultUni.Data ->: TVar("a") ->: TVar("a") ->: TVar("a") ->: TVar("a") ->: TVar(
              "a"
            ) ->: TVar("a")
          ),
          (a: CekValue) =>
              val aa = a.asData
              (b: CekValue) =>
                  (c: CekValue) =>
                      (d: CekValue) =>
                          (e: CekValue) =>
                              (f: CekValue) =>
                                  (ps: PlatformSpecific) => chooseData(aa, b, c, d, e, f)
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
                              case _ => throw new Exception(s"ConstrData: not a data, but $b")
                          }
                      case _ => throw new RuntimeException(s"Expected list, got $b")
                  }
                  (ps: PlatformSpecific) =>
                      VCon(
                        Constant.Data(Data.Constr(i.longValue, args))
                      )
        )

    val MapData =
        mkMeaning(
          DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)) ->: DefaultUni.Data,
          (a: CekValue) =>
              val aa = a.asList
              (ps: PlatformSpecific) =>
                  VCon(
                    Constant.Data(Data.Map(aa.map {
                        case Constant.Pair(Constant.Data(a), Constant.Data(b)) => (a, b)
                        case _ => throw new RuntimeException(s"MapData: not a pair, but $a")
                    }))
                  )
        )

    val ListData =
        mkMeaning(
          DefaultUni.List(DefaultUni.Data) ->: DefaultUni.Data,
          (a: CekValue) =>
              val aa = a.asList
              val datas = aa.map {
                  case Constant.Data(value) => value
                  case _ => throw new RuntimeException(s"ListData: not a data, but $a")
              }
              (ps: PlatformSpecific) => VCon(Constant.Data(Data.List(datas)))
        )

    val IData =
        mkMeaning(
          Integer ->: DefaultUni.Data,
          (a: CekValue) =>
              val aa = a.asInteger
              (ps: PlatformSpecific) => VCon(Constant.Data(Data.I(aa)))
        )

    val BData =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Data,
          (a: CekValue) =>
              val aa = a.asByteString
              (ps: PlatformSpecific) => VCon(Constant.Data(Data.B(aa)))
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
                      (ps: PlatformSpecific) =>
                          VCon(
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
                      (ps: PlatformSpecific) =>
                          VCon(
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
                      (ps: PlatformSpecific) =>
                          VCon(Constant.List(DefaultUni.Data, values.map(asConstant)))
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
                      (ps: PlatformSpecific) => VCon(asConstant(i))
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
                      (ps: PlatformSpecific) => VCon(asConstant(b))
                  case _ => throw new Exception(s"unBData: not a bytestring, but $a")
        )

    val EqualsData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Data ->: DefaultUni.Bool,
          (a: CekValue) =>
              val aa = a.asData
              (b: CekValue) =>
                  val bb = b.asData
                  (ps: PlatformSpecific) => VCon(Constant.Bool(equalsData(aa, bb)))
        )

    val SerialiseData = mkMeaning(
      DefaultUni.Data ->: DefaultUni.ByteString,
      (a: CekValue) =>
          val aa = a.asData
          (ps: PlatformSpecific) => VCon(Constant.ByteString(serialiseData(aa)))
    )

    val MkPairData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Data ->: DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
          (a: CekValue) =>
              val aa = a.asData
              (b: CekValue) =>
                  val bb = b.asData
                  (ps: PlatformSpecific) => VCon(Constant.Pair(asConstant(aa), asConstant(bb)))
        )

    val MkNilData =
        mkMeaning(
          DefaultUni.Unit ->: DefaultUni.List(DefaultUni.Data),
          (a: CekValue) =>
              val _ = a.asUnit
              (ps: PlatformSpecific) => VCon(Constant.List(DefaultUni.Data, Nil))
        )

    val MkNilPairData = mkMeaning(
      DefaultUni.Unit ->: DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
      (a: CekValue) =>
          val _ = a.asUnit
          (ps: PlatformSpecific) =>
              VCon(Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), Nil))
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
      (DefaultFun.AppendByteString, Meaning.AppendByteString),
      (DefaultFun.ConsByteString, Meaning.ConsByteString),
      (DefaultFun.SliceByteString, Meaning.SliceByteString),
      (DefaultFun.LengthOfByteString, Meaning.LengthOfByteString),
      (DefaultFun.IndexByteString, Meaning.IndexByteString),
      (DefaultFun.EqualsByteString, Meaning.EqualsByteString),
      (DefaultFun.LessThanByteString, Meaning.LessThanByteString),
      (DefaultFun.LessThanEqualsByteString, Meaning.LessThanEqualsByteString),
      (DefaultFun.Sha2_256, Meaning.Sha2_256),
      (DefaultFun.Sha3_256, Meaning.Sha3_256),
      (DefaultFun.Blake2b_256, Meaning.Blake2b_256),
      (DefaultFun.VerifyEd25519Signature, Meaning.VerifyEd25519Signature),
      (DefaultFun.VerifyEcdsaSecp256k1Signature, Meaning.VerifyEcdsaSecp256k1Signature),
      (DefaultFun.VerifySchnorrSecp256k1Signature, Meaning.VerifySchnorrSecp256k1Signature),
      (DefaultFun.AppendString, Meaning.AppendString),
      (DefaultFun.EqualsString, Meaning.EqualsString),
      (DefaultFun.EncodeUtf8, Meaning.EncodeUtf8),
      (DefaultFun.DecodeUtf8, Meaning.DecodeUtf8),
      (DefaultFun.IfThenElse, Meaning.IfThenElse),
      (DefaultFun.ChooseUnit, Meaning.ChooseUnit),
      (DefaultFun.Trace, Meaning.Trace),
      (DefaultFun.FstPair, Meaning.FstPair),
      (DefaultFun.SndPair, Meaning.SndPair),
      (DefaultFun.ChooseList, Meaning.ChooseList),
      (DefaultFun.MkCons, Meaning.MkCons),
      (DefaultFun.HeadList, Meaning.HeadList),
      (DefaultFun.TailList, Meaning.TailList),
      (DefaultFun.NullList, Meaning.NullList),
      (DefaultFun.ChooseData, Meaning.ChooseData),
      (DefaultFun.ConstrData, Meaning.ConstrData),
      (DefaultFun.MapData, Meaning.MapData),
      (DefaultFun.ListData, Meaning.ListData),
      (DefaultFun.IData, Meaning.IData),
      (DefaultFun.BData, Meaning.BData),
      (DefaultFun.UnConstrData, Meaning.UnConstrData),
      (DefaultFun.UnMapData, Meaning.UnMapData),
      (DefaultFun.UnListData, Meaning.UnListData),
      (DefaultFun.UnIData, Meaning.UnIData),
      (DefaultFun.UnBData, Meaning.UnBData),
      (DefaultFun.EqualsData, Meaning.EqualsData),
      (DefaultFun.SerialiseData, Meaning.SerialiseData),
      (DefaultFun.MkPairData, Meaning.MkPairData),
      (DefaultFun.MkNilData, Meaning.MkNilData),
      (DefaultFun.MkNilPairData, Meaning.MkNilPairData)
    )
