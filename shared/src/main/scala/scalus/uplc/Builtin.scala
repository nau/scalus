package scalus.uplc

import scalus.builtin.Builtins.*
import scalus.builtin.Data
import scalus.builtin.*
import scalus.uplc.Constant.given
import scalus.uplc.DefaultUni.Bool
import scalus.uplc.DefaultUni.Integer
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.DefaultUni.given
import scalus.uplc.eval.BuiltinCostModel
import scalus.uplc.eval.CekValue
import scalus.uplc.eval.CekValue.*
import scalus.uplc.eval.CostModel
import scalus.uplc.eval.CostingFun
import scalus.uplc.eval.KnownTypeUnliftingError

import scala.collection.immutable
import scala.collection.immutable.ArraySeq
import scalus.uplc.eval.DeserializationError
import scalus.uplc.eval.ExBudget
import scalus.uplc.eval.Logger

enum TypeScheme:
    case Type(argType: DefaultUni)
    case App(f: TypeScheme, arg: TypeScheme)
    case Arrow(argType: TypeScheme, t: TypeScheme)
    case All(name: String, t: TypeScheme)
    case TVar(name: String)

    lazy val arity: Int = this match
        case Arrow(_, t) => 1 + t.arity
        case All(_, t)   => t.arity
        case _           => 0

    lazy val numTypeVars: Int = this match
        case All(_, t) => 1 + t.numTypeVars
        case _         => 0

    def ->:(t: TypeScheme): TypeScheme = Arrow(t, this)
    def ->:(t: DefaultUni): TypeScheme = Arrow(Type(t), this)
    infix def $(t: TypeScheme): TypeScheme = App(this, t)
    infix def $(t: String): TypeScheme = App(this, TVar(t))

case class BuiltinRuntime(
    typeScheme: TypeScheme,
    f: (Logger, Seq[CekValue]) => CekValue,
    args: Seq[CekValue],
    costFunction: CostingFun[CostModel]
) {
    def apply(logger: Logger) = f(logger, args)

    def calculateCost: ExBudget = costFunction.calculateCost(args: _*)
}

class BuiltinsMeaning(builtinCostModel: BuiltinCostModel, platformSpecific: PlatformSpecific):
    // local extension used to create a TypeScheme from a DefaultUni
    extension (x: DefaultUni)
        def ->:(t: TypeScheme): TypeScheme = TypeScheme.Arrow(t, TypeScheme.Type(x))
        def ->:(t: DefaultUni): TypeScheme =
            TypeScheme.Arrow(TypeScheme.Type(t), TypeScheme.Type(x))
        infix def $(t: TypeScheme): TypeScheme = TypeScheme.Type(x) $ t
        infix def $(t: String): TypeScheme = TypeScheme.Type(x) $ t

    def mkMeaning(
        t: TypeScheme,
        f: (logger: Logger, args: Seq[CekValue]) => CekValue,
        costFunction: CostingFun[CostModel]
    ) =
        BuiltinRuntime(t, f, ArraySeq.empty, costFunction)
    import TypeScheme.*

    val AddInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(addInteger(a, b)))
          ,
          builtinCostModel.addInteger
        )

    val SubtractInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(subtractInteger(a, b)))
          ,
          builtinCostModel.subtractInteger
        )

    val MultiplyInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(multiplyInteger(a, b)))
          ,
          builtinCostModel.multiplyInteger
        )

    val DivideInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(divideInteger(a, b)))
          ,
          builtinCostModel.divideInteger
        )

    val QuotientInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(quotientInteger(a, b)))
          ,
          builtinCostModel.quotientInteger
        )

    val RemainderInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(remainderInteger(a, b)))
          ,
          builtinCostModel.remainderInteger
        )

    val ModInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(modInteger(a, b)))
          ,
          builtinCostModel.modInteger
        )

    val EqualsInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(equalsInteger(a, b)))
          ,
          builtinCostModel.equalsInteger
        )

    val LessThanEqualsInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asInteger
              val bb = args(1).asInteger
              VCon(asConstant(lessThanEqualsInteger(aa, bb)))
          ,
          builtinCostModel.lessThanEqualsInteger
        )

    val LessThanInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asInteger
              VCon(asConstant(lessThanInteger(a, b)))
          ,
          builtinCostModel.lessThanInteger
        )

    val AppendByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asByteString
              val b = args(1).asByteString
              VCon(asConstant(appendByteString(a, b)))
          ,
          builtinCostModel.appendByteString
        )

    val ConsByteString =
        mkMeaning(
          DefaultUni.Integer ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val a = args(0).asInteger
              val b = args(1).asByteString
              VCon(asConstant(consByteString(a, b)))
          ,
          builtinCostModel.consByteString
        )

    val SliceByteString =
        mkMeaning(
          DefaultUni.Integer ->: DefaultUni.Integer ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val start = args(0).asInteger
              val n = args(1).asInteger
              val bs = args(2).asByteString
              VCon(asConstant(sliceByteString(start, n, bs)))
          ,
          builtinCostModel.sliceByteString
        )

    val IndexByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer ->: DefaultUni.Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              val bb = args(1).asInteger
              VCon(asConstant(indexByteString(aa, bb)))
          ,
          builtinCostModel.indexByteString
        )

    val LengthOfByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(lengthOfByteString(aa)))
          ,
          builtinCostModel.lengthOfByteString
        )

    val EqualsByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              val bb = args(1).asByteString
              VCon(asConstant(aa == bb))
          ,
          builtinCostModel.equalsByteString
        )

    val LessThanByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              val bb = args(1).asByteString
              VCon(asConstant(lessThanByteString(aa, bb)))
          ,
          builtinCostModel.lessThanByteString
        )

    val LessThanEqualsByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              val bb = args(1).asByteString
              VCon(asConstant(lessThanEqualsByteString(aa, bb)))
          ,
          builtinCostModel.lessThanEqualsByteString
        )

    val Sha2_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.sha2_256(aa)))
          ,
          builtinCostModel.sha2_256
        )

    val Sha3_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.sha3_256(aa)))
          ,
          builtinCostModel.sha3_256
        )

    val Blake2b_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.blake2b_256(aa)))
          ,
          builtinCostModel.blake2b_256
        )

    val Blake2b_224 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.blake2b_224(aa)))
          ,
          builtinCostModel.blake2b_224
        )

    val VerifyEd25519Signature =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val pk = args(0).asByteString
              val msg = args(1).asByteString
              val sig = args(2).asByteString
              VCon(asConstant(platformSpecific.verifyEd25519Signature(pk, msg, sig)))
          ,
          builtinCostModel.verifyEd25519Signature
        )

    val VerifyEcdsaSecp256k1Signature =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val pk = args(0).asByteString
              val msg = args(1).asByteString
              val sig = args(2).asByteString
              VCon(asConstant(platformSpecific.verifyEcdsaSecp256k1Signature(pk, msg, sig)))
          ,
          builtinCostModel.verifyEcdsaSecp256k1Signature
        )

    val VerifySchnorrSecp256k1Signature =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val pk = args(0).asByteString
              val msg = args(1).asByteString
              val sig = args(2).asByteString
              VCon(asConstant(platformSpecific.verifySchnorrSecp256k1Signature(pk, msg, sig)))
          ,
          builtinCostModel.verifySchnorrSecp256k1Signature
        )

    val AppendString =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.String ->: DefaultUni.String,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asString
              val bb = args(1).asString
              VCon(asConstant(appendString(aa, bb)))
          ,
          builtinCostModel.appendString
        )

    val EqualsString =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.String ->: Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asString
              val bb = args(1).asString
              VCon(asConstant(equalsString(aa, bb)))
          ,
          builtinCostModel.equalsString
        )

    val EncodeUtf8 =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asString
              VCon(asConstant(encodeUtf8(aa)))
          ,
          builtinCostModel.encodeUtf8
        )

    val DecodeUtf8 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.String,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(decodeUtf8(aa)))
          ,
          builtinCostModel.decodeUtf8
        )

    val IfThenElse =
        mkMeaning(
          All("a", Bool ->: TVar("a") ->: TVar("a") ->: TVar("a")),
          (logger: Logger, args: Seq[CekValue]) =>
              val bb = args(0).asBool
              val t = args(1)
              val f = args(2)
              ifThenElse(bb, t, f)
          ,
          builtinCostModel.ifThenElse
        )

    val ChooseUnit =
        mkMeaning(
          All("a", DefaultUni.Unit ->: TVar("a") ->: TVar("a")),
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Unit) => args(1)
                  case _ => throw new DeserializationError(DefaultFun.ChooseUnit, args(0))
          ,
          builtinCostModel.chooseUnit
        )

    val Trace =
        mkMeaning(
          All("a", DefaultUni.String ->: TVar("a") ->: TVar("a")),
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asString
              logger.log(aa)
              args(1)
          ,
          builtinCostModel.trace
        )

    // [ forall a, forall b, pair(a, b) ] -> a
    val FstPair =
        mkMeaning(
          All("a", All("b", (DefaultUni.ProtoPair $ "a" $ "b") ->: TVar("a"))),
          (logger: Logger, args: Seq[CekValue]) =>
              val (fst, _) = args(0).asPair
              VCon(fst)
          ,
          builtinCostModel.fstPair
        )

    // [ forall a, forall b, pair(a, b) ] -> b
    val SndPair =
        mkMeaning(
          All("a", All("b", (DefaultUni.ProtoPair $ "a" $ "b") ->: TVar("b"))),
          (logger: Logger, args: Seq[CekValue]) =>
              val (_, snd) = args(0).asPair
              VCon(snd)
          ,
          builtinCostModel.sndPair
        )

    // [ forall a, forall b, list(a), b, b ] -> b
    val ChooseList =
        mkMeaning(
          All(
            "a",
            All("b", (DefaultUni.ProtoList $ "a") ->: TVar("b") ->: TVar("b") ->: TVar("b"))
          ),
          (logger: Logger, args: Seq[CekValue]) =>
              val ls = args(0).asList
              if ls.isEmpty then args(1) else args(2)
          ,
          builtinCostModel.chooseList
        )

    // [ forall a, a, list(a) ] -> list(a)
    val MkCons =
        mkMeaning(
          All("a", TVar("a") ->: (DefaultUni.ProtoList $ "a") ->: (DefaultUni.ProtoList $ "a")),
          (logger: Logger, args: Seq[CekValue]) =>
              (args(0), args(1)) match
                  // Checking that the type of the constant is the same as the type of the elements
                  // of the unlifted list. Note that there's no way we could enforce this statically
                  // since in UPLC one can create an ill-typed program that attempts to prepend
                  // a value of the wrong type to a list.
                  case (VCon(aCon), VCon(Constant.List(tp, l))) =>
                      if aCon.tpe != tp then throw new KnownTypeUnliftingError(tp, args(0))
                      else VCon(Constant.List(tp, aCon :: l))
                  case _ => throw new DeserializationError(DefaultFun.MkCons, args(1))
          ,
          builtinCostModel.mkCons
        )

    // [ forall a, list(a) ] -> a
    val HeadList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: TVar("a")),
          (logger: Logger, args: Seq[CekValue]) => VCon(args(0).asList.head),
          builtinCostModel.headList
        )

    // [ forall a, list(a) ] -> list(a)
    val TailList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: (DefaultUni.ProtoList $ "a")),
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.List(tpe, ls)) => VCon(Constant.List(tpe, ls.tail))
                  case _ => throw new DeserializationError(DefaultFun.TailList, args(0))
          ,
          builtinCostModel.tailList
        )

    // [ forall a, list(a) ] -> bool
    val NullList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: Type(Bool)),
          (logger: Logger, args: Seq[CekValue]) => VCon(asConstant(args(0).asList.isEmpty)),
          builtinCostModel.nullList
        )

    val ChooseData =
        mkMeaning(
          All(
            "a",
            DefaultUni.Data ->: TVar("a") ->: TVar("a") ->: TVar("a") ->: TVar("a") ->: TVar(
              "a"
            ) ->: TVar("a")
          ),
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asData
              chooseData(aa, args(1), args(2), args(3), args(4), args(5))
          ,
          builtinCostModel.chooseData
        )

    val ConstrData =
        mkMeaning(
          Integer ->: DefaultUni.List(DefaultUni.Data) ->: DefaultUni.Data,
          (logger: Logger, args: Seq[CekValue]) =>
              val i = args(0).asInteger
              val argsList = args(1).asList.map {
                  case Constant.Data(d) => d
                  case _ => throw new DeserializationError(DefaultFun.ConstrData, args(1))
              }
              VCon(Constant.Data(Data.Constr(i.longValue, argsList)))
          ,
          builtinCostModel.constrData
        )

    val MapData =
        mkMeaning(
          DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)) ->: DefaultUni.Data,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asList
              VCon(
                Constant.Data(Data.Map(aa.map {
                    case Constant.Pair(Constant.Data(a), Constant.Data(b)) => (a, b)
                    case _ =>
                        throw new KnownTypeUnliftingError(
                          DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                          args(0)
                        )
                }))
              )
          ,
          builtinCostModel.mapData
        )

    val ListData =
        mkMeaning(
          DefaultUni.List(DefaultUni.Data) ->: DefaultUni.Data,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asList
              VCon(Constant.Data(Data.List(aa.map {
                  case Constant.Data(value) => value
                  case _ => throw new KnownTypeUnliftingError(DefaultUni.Data, args(0))
              })))
          ,
          builtinCostModel.listData
        )

    val IData =
        mkMeaning(
          Integer ->: DefaultUni.Data,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asInteger
              VCon(Constant.Data(Data.I(aa)))
          ,
          builtinCostModel.iData
        )

    val BData =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Data,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(Constant.Data(Data.B(aa)))
          ,
          builtinCostModel.bData
        )

    /*
    unConstrData : [ data ] -> pair(integer, list(data))
     */
    val UnConstrData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Pair(Integer, DefaultUni.List(DefaultUni.Data)),
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.Constr(i, ls))) =>
                      VCon(Constant.Pair(asConstant(i), asConstant(ls)))
                  case _ => throw new DeserializationError(DefaultFun.UnConstrData, args(0))
          ,
          builtinCostModel.unConstrData
        )

    val UnMapData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)),
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.Map(values))) =>
                      VCon(
                        Constant.List(
                          DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                          values.map { case (k, v) =>
                              Constant.Pair(asConstant(k), asConstant(v))
                          }
                        )
                      )
                  case _ => throw new DeserializationError(DefaultFun.UnMapData, args(0))
          ,
          builtinCostModel.unMapData
        )

    val UnListData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.List(DefaultUni.Data),
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.List(values))) =>
                      VCon(Constant.List(DefaultUni.Data, values.map(asConstant)))
                  case _ => throw new DeserializationError(DefaultFun.UnListData, args(0))
          ,
          builtinCostModel.unListData
        )

    val UnIData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Integer,
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.I(i))) =>
                      VCon(asConstant(i))
                  case _ => throw new DeserializationError(DefaultFun.UnIData, args(0))
          ,
          builtinCostModel.unIData
        )

    val UnBData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(Data.B(b))) =>
                      VCon(asConstant(b))
                  case _ => throw new DeserializationError(DefaultFun.UnBData, args(0))
          ,
          builtinCostModel.unBData
        )

    val EqualsData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Data ->: DefaultUni.Bool,
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(aa)) =>
                      args(1) match
                          case VCon(Constant.Data(bb)) =>
                              VCon(Constant.Bool(equalsData(aa, bb)))
                          case _ => throw new DeserializationError(DefaultFun.EqualsData, args(1))

                  case _ => throw new DeserializationError(DefaultFun.EqualsData, args(0))
          ,
          builtinCostModel.equalsData
        )

    val SerialiseData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              args(0) match
                  case VCon(Constant.Data(data)) =>
                      VCon(Constant.ByteString(serialiseData(data)))
                  case _ => throw new DeserializationError(DefaultFun.SerialiseData, args(0))
          ,
          builtinCostModel.serialiseData
        )

    val MkPairData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Data ->: DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asData
              val bb = args(1).asData
              VCon(Constant.Pair(asConstant(aa), asConstant(bb)))
          ,
          builtinCostModel.mkPairData
        )

    val MkNilData =
        mkMeaning(
          DefaultUni.Unit ->: DefaultUni.List(DefaultUni.Data),
          (logger: Logger, args: Seq[CekValue]) =>
              val _ = args(0).asUnit
              VCon(Constant.List(DefaultUni.Data, Nil))
          ,
          builtinCostModel.mkNilData
        )

    val MkNilPairData = mkMeaning(
      DefaultUni.Unit ->: DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
      (logger: Logger, args: Seq[CekValue]) =>
          val _ = args(0).asUnit
          VCon(Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), Nil))
      ,
      builtinCostModel.mkNilPairData
    )

    val Keccak_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (logger: Logger, args: Seq[CekValue]) =>
              val aa = args(0).asByteString
              VCon(asConstant(platformSpecific.keccak_256(aa)))
          ,
          builtinCostModel.keccak_256
        )

    val BuiltinMeanings: immutable.Map[DefaultFun, BuiltinRuntime] = immutable.Map.apply(
      (DefaultFun.AddInteger, AddInteger),
      (DefaultFun.SubtractInteger, SubtractInteger),
      (DefaultFun.MultiplyInteger, MultiplyInteger),
      (DefaultFun.DivideInteger, DivideInteger),
      (DefaultFun.QuotientInteger, QuotientInteger),
      (DefaultFun.RemainderInteger, RemainderInteger),
      (DefaultFun.ModInteger, ModInteger),
      (DefaultFun.EqualsInteger, EqualsInteger),
      (DefaultFun.LessThanEqualsInteger, LessThanEqualsInteger),
      (DefaultFun.LessThanInteger, LessThanInteger),
      (DefaultFun.AppendByteString, AppendByteString),
      (DefaultFun.ConsByteString, ConsByteString),
      (DefaultFun.SliceByteString, SliceByteString),
      (DefaultFun.LengthOfByteString, LengthOfByteString),
      (DefaultFun.IndexByteString, IndexByteString),
      (DefaultFun.EqualsByteString, EqualsByteString),
      (DefaultFun.LessThanByteString, LessThanByteString),
      (DefaultFun.LessThanEqualsByteString, LessThanEqualsByteString),
      (DefaultFun.Sha2_256, Sha2_256),
      (DefaultFun.Sha3_256, Sha3_256),
      (DefaultFun.Blake2b_256, Blake2b_256),
      (DefaultFun.VerifyEd25519Signature, VerifyEd25519Signature),
      (DefaultFun.VerifyEcdsaSecp256k1Signature, VerifyEcdsaSecp256k1Signature),
      (DefaultFun.VerifySchnorrSecp256k1Signature, VerifySchnorrSecp256k1Signature),
      (DefaultFun.AppendString, AppendString),
      (DefaultFun.EqualsString, EqualsString),
      (DefaultFun.EncodeUtf8, EncodeUtf8),
      (DefaultFun.DecodeUtf8, DecodeUtf8),
      (DefaultFun.IfThenElse, IfThenElse),
      (DefaultFun.ChooseUnit, ChooseUnit),
      (DefaultFun.Trace, Trace),
      (DefaultFun.FstPair, FstPair),
      (DefaultFun.SndPair, SndPair),
      (DefaultFun.ChooseList, ChooseList),
      (DefaultFun.MkCons, MkCons),
      (DefaultFun.HeadList, HeadList),
      (DefaultFun.TailList, TailList),
      (DefaultFun.NullList, NullList),
      (DefaultFun.ChooseData, ChooseData),
      (DefaultFun.ConstrData, ConstrData),
      (DefaultFun.MapData, MapData),
      (DefaultFun.ListData, ListData),
      (DefaultFun.IData, IData),
      (DefaultFun.BData, BData),
      (DefaultFun.UnConstrData, UnConstrData),
      (DefaultFun.UnMapData, UnMapData),
      (DefaultFun.UnListData, UnListData),
      (DefaultFun.UnIData, UnIData),
      (DefaultFun.UnBData, UnBData),
      (DefaultFun.EqualsData, EqualsData),
      (DefaultFun.SerialiseData, SerialiseData),
      (DefaultFun.MkPairData, MkPairData),
      (DefaultFun.MkNilData, MkNilData),
      (DefaultFun.MkNilPairData, MkNilPairData),
      (DefaultFun.Blake2b_224, Blake2b_224),
      (DefaultFun.Keccak_256, Keccak_256)
    )
