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
import scalus.uplc.eval.CekMachine

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

    def ->:(t: TypeScheme): TypeScheme = Arrow(t, this)
    def ->:(t: DefaultUni): TypeScheme = Arrow(Type(t), this)
    infix def $(t: TypeScheme): TypeScheme = App(this, t)
    infix def $(t: String): TypeScheme = App(this, TVar(t))

case class Runtime(
    typeScheme: TypeScheme,
    f: AnyRef => AnyRef,
    args: Seq[CekValue],
    costFunction: BuiltinCostModel => CostingFun[CostModel]
) {
    def apply(m: CekMachine) = {
        val applied = args.foldLeft(f) { (f, arg) => f(arg).asInstanceOf[AnyRef => AnyRef] }
        applied.asInstanceOf[CekMachine => CekValue](m)
    }

    def calculateCost(costModel: BuiltinCostModel): ExBudget =
        costFunction(costModel).calculateCost(args: _*)
}

object Meaning:
    // local extension used to create a TypeScheme from a DefaultUni
    extension (x: DefaultUni)
        def ->:(t: TypeScheme): TypeScheme = TypeScheme.Arrow(t, TypeScheme.Type(x))
        def ->:(t: DefaultUni): TypeScheme =
            TypeScheme.Arrow(TypeScheme.Type(t), TypeScheme.Type(x))
        infix def $(t: TypeScheme): TypeScheme = TypeScheme.Type(x) $ t
        infix def $(t: String): TypeScheme = TypeScheme.Type(x) $ t

    def mkMeaning(
        t: TypeScheme,
        f: AnyRef,
        costFunction: BuiltinCostModel => CostingFun[CostModel]
    ) =
        Runtime(t, f.asInstanceOf[AnyRef => AnyRef], ArraySeq.empty, costFunction)
    import TypeScheme.*

    val AddInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(addInteger(aa, bb)))
          ,
          _.addInteger
        )
    val SubtractInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(subtractInteger(aa, bb)))
          ,
          _.subtractInteger
        )
    val MultiplyInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(multiplyInteger(aa, bb)))
          ,
          _.multiplyInteger
        )
    val DivideInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(divideInteger(aa, bb)))
          ,
          _.divideInteger
        )
    val QuotientInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(quotientInteger(aa, bb)))
          ,
          _.quotientInteger
        )
    val RemainderInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(remainderInteger(aa, bb)))
          ,
          _.remainderInteger
        )
    val ModInteger =
        mkMeaning(
          Integer ->: Integer ->: Integer,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(modInteger(aa, bb)))
          ,
          _.modInteger
        )
    val EqualsInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(equalsInteger(aa, bb)))
          ,
          _.equalsInteger
        )
    val LessThanEqualsInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(lessThanEqualsInteger(aa, bb)))
          ,
          _.lessThanEqualsInteger
        )
    val LessThanInteger =
        mkMeaning(
          Integer ->: Integer ->: Bool,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(lessThanInteger(aa, bb)))
          ,
          _.lessThanInteger
        )

    val AppendByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asByteString
                  (m: CekMachine) => VCon(asConstant(appendByteString(aa, bb)))
          ,
          _.appendByteString
        )

    val ConsByteString =
        mkMeaning(
          DefaultUni.Integer ->: DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asInteger
              (b: CekValue) =>
                  val bb = b.asByteString
                  (m: CekMachine) => VCon(asConstant(consByteString(aa, bb)))
          ,
          _.consByteString
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
                      (m: CekMachine) => VCon(asConstant(sliceByteString(bs, start, end)))
          ,
          _.sliceByteString
        )

    val IndexByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer ->: DefaultUni.Integer,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asInteger
                  (m: CekMachine) => VCon(asConstant(indexByteString(aa, bb)))
          ,
          _.indexByteString
        )

    val LengthOfByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Integer,
          (a: CekValue) =>
              val aa = a.asByteString
              (m: CekMachine) => VCon(asConstant(lengthOfByteString(aa)))
          ,
          _.lengthOfByteString
        )

    val EqualsByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asByteString
                  (m: CekMachine) => VCon(asConstant(aa == bb))
          ,
          _.equalsByteString
        )

    val LessThanByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asByteString
                  (m: CekMachine) => VCon(asConstant(lessThanByteString(aa, bb)))
          ,
          _.lessThanByteString
        )

    val LessThanEqualsByteString =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString ->: Bool,
          (a: CekValue) =>
              val aa = a.asByteString
              (b: CekValue) =>
                  val bb = b.asByteString
                  (m: CekMachine) => VCon(asConstant(lessThanEqualsByteString(aa, bb)))
          ,
          _.lessThanEqualsByteString
        )

    val Sha2_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asByteString
              (m: CekMachine) =>
                  VCon(asConstant(sha2_256(using m.evaluationContext.platformSpecific)(aa)))
          ,
          _.sha2_256
        )

    val Sha3_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asByteString
              (m: CekMachine) =>
                  VCon(asConstant(sha3_256(using m.evaluationContext.platformSpecific)(aa)))
          ,
          _.sha3_256
        )

    val Blake2b_256 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.ByteString,
          (a: CekValue) =>
              val aa = a.asByteString
              (m: CekMachine) =>
                  VCon(asConstant(blake2b_256(using m.evaluationContext.platformSpecific)(aa)))
          ,
          _.blake2b_256
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
                      (m: CekMachine) =>
                          VCon(
                            asConstant(
                              verifyEd25519Signature(using m.evaluationContext.platformSpecific)(
                                pk,
                                msg,
                                sig
                              )
                            )
                          )
          ,
          _.verifyEd25519Signature
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
                      (m: CekMachine) =>
                          VCon(
                            asConstant(
                              verifyEcdsaSecp256k1Signature(using
                                m.evaluationContext.platformSpecific
                              )(pk, msg, sig)
                            )
                          )
          ,
          _.verifyEcdsaSecp256k1Signature
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
                      (m: CekMachine) =>
                          VCon(
                            asConstant(
                              verifySchnorrSecp256k1Signature(using
                                m.evaluationContext.platformSpecific
                              )(pk, msg, sig)
                            )
                          )
          ,
          _.verifySchnorrSecp256k1Signature
        )
    }

    val AppendString =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.String ->: DefaultUni.String,
          (a: CekValue) =>
              val aa = a.asString
              (b: CekValue) =>
                  val bb = b.asString
                  (m: CekMachine) => VCon(asConstant(appendString(aa, bb)))
          ,
          _.appendString
        )

    val EqualsString =
        mkMeaning(
          DefaultUni.String ->: DefaultUni.String ->: Bool,
          (a: CekValue) =>
              val aa = a.asString
              (b: CekValue) =>
                  val bb = b.asString
                  (m: CekMachine) => VCon(asConstant(equalsString(aa, bb)))
          ,
          _.equalsString
        )

    val EncodeUtf8 = {
        val tpe = DefaultUni.String ->: DefaultUni.ByteString
        mkMeaning(
          tpe,
          (a: CekValue) =>
              val aa = a.asString
              (m: CekMachine) => VCon(asConstant(encodeUtf8(aa)))
          ,
          _.encodeUtf8
        )
    }

    val DecodeUtf8 =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.String,
          (a: CekValue) =>
              val aa = a.asByteString
              (m: CekMachine) => VCon(asConstant(decodeUtf8(aa)))
          ,
          _.decodeUtf8
        )

    val IfThenElse =
        mkMeaning(
          All("a", Bool ->: TVar("a") ->: TVar("a") ->: TVar("a")),
          (b: CekValue) =>
              val bb = b.asBool
              (t: CekValue) => (f: CekValue) => (m: CekMachine) => ifThenElse(bb, t, f)
          ,
          _.ifThenElse
        )

    val ChooseUnit =
        mkMeaning(
          All("a", DefaultUni.Unit ->: TVar("a") ->: TVar("a")),
          (unit: CekValue) => {
              unit match
                  case VCon(Constant.Unit) => (a: CekValue) => (m: CekMachine) => a
                  case _ => throw new DeserializationError(DefaultFun.ChooseUnit, unit)
          },
          _.chooseUnit
        )

    val Trace =
        mkMeaning(
          All("a", DefaultUni.String ->: TVar("a") ->: TVar("a")),
          (a: CekValue) =>
              val aa = a.asString
              (b: CekValue) => (m: CekMachine) => m.logs.addOne(aa)
          ,
          _.trace
        )

    // [ forall a, forall b, pair(a, b) ] -> a
    val FstPair =
        mkMeaning(
          All("a", All("b", (DefaultUni.ProtoPair $ "a" $ "b") ->: TVar("a"))),
          (a: CekValue) =>
              val (fst, _) = a.asPair
              (m: CekMachine) => VCon(fst)
          ,
          _.fstPair
        )

    // [ forall a, forall b, pair(a, b) ] -> b
    val SndPair =
        mkMeaning(
          All("a", All("b", (DefaultUni.ProtoPair $ "a" $ "b") ->: TVar("b"))),
          (a: CekValue) =>
              val (_, snd) = a.asPair
              (m: CekMachine) => VCon(snd)
          ,
          _.sndPair
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
              (b: CekValue) => (c: CekValue) => (m: CekMachine) => if ls.isEmpty then b else c
          ,
          _.chooseList
        )

    // [ forall a, a, list(a) ] -> list(a)
    val MkCons =
        mkMeaning(
          All("a", TVar("a") ->: (DefaultUni.ProtoList $ "a") ->: (DefaultUni.ProtoList $ "a")),
          (a: CekValue) =>
              (b: CekValue) =>
                  (a, b) match {
                      // Checking that the type of the constant is the same as the type of the elements
                      // of the unlifted list. Note that there's no way we could enforce this statically
                      // since in UPLC one can create an ill-typed program that attempts to prepend
                      // a value of the wrong type to a list.
                      case (VCon(aCon), VCon(Constant.List(tp, l))) =>
                          if aCon.tpe != tp then throw new KnownTypeUnliftingError(tp, a)
                          else (m: CekMachine) => VCon(Constant.List(tp, aCon :: l))
                      case _ => throw new DeserializationError(DefaultFun.MkCons, b)
                  },
          _.mkCons
        )

    // [ forall a, list(a) ] -> a
    val HeadList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: TVar("a")),
          (a: CekValue) =>
              val ls = a.asList
              (m: CekMachine) => VCon(ls.head)
          ,
          _.headList
        )

    // [ forall a, list(a) ] -> list(a)
    val TailList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: (DefaultUni.ProtoList $ "a")),
          (a: CekValue) =>
              a match {
                  case VCon(Constant.List(tpe, ls)) =>
                      (m: CekMachine) => VCon(Constant.List(tpe, ls.tail))
                  case _ => throw new DeserializationError(DefaultFun.TailList, a)
              },
          _.tailList
        )

    // [ forall a, list(a) ] -> bool
    val NullList =
        mkMeaning(
          All("a", (DefaultUni.ProtoList $ "a") ->: Type(Bool)),
          (a: CekValue) =>
              val ls = a.asList
              (m: CekMachine) => VCon(asConstant(ls.isEmpty))
          ,
          _.nullList
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
                              (f: CekValue) => (m: CekMachine) => chooseData(aa, b, c, d, e, f)
          ,
          _.chooseData
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
                              case _ => throw new DeserializationError(DefaultFun.ConstrData, b)
                          }
                      case _ => throw new DeserializationError(DefaultFun.ConstrData, b)
                  }
                  (m: CekMachine) =>
                      VCon(
                        Constant.Data(Data.Constr(i.longValue, args))
                      )
          ,
          _.constrData
        )

    val MapData =
        mkMeaning(
          DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)) ->: DefaultUni.Data,
          (a: CekValue) =>
              val aa = a.asList
              (m: CekMachine) =>
                  VCon(
                    Constant.Data(Data.Map(aa.map {
                        case Constant.Pair(Constant.Data(a), Constant.Data(b)) => (a, b)
                        case _ =>
                            throw new KnownTypeUnliftingError(
                              DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                              a
                            )
                    }))
                  )
          ,
          _.mapData
        )

    val ListData =
        mkMeaning(
          DefaultUni.List(DefaultUni.Data) ->: DefaultUni.Data,
          (a: CekValue) =>
              val aa = a.asList
              val datas = aa.map {
                  case Constant.Data(value) => value
                  case _                    => throw new KnownTypeUnliftingError(DefaultUni.Data, a)
              }
              (m: CekMachine) => VCon(Constant.Data(Data.List(datas)))
          ,
          _.listData
        )

    val IData =
        mkMeaning(
          Integer ->: DefaultUni.Data,
          (a: CekValue) =>
              val aa = a.asInteger
              (m: CekMachine) => VCon(Constant.Data(Data.I(aa)))
          ,
          _.iData
        )

    val BData =
        mkMeaning(
          DefaultUni.ByteString ->: DefaultUni.Data,
          (a: CekValue) =>
              val aa = a.asByteString
              (m: CekMachine) => VCon(Constant.Data(Data.B(aa)))
          ,
          _.bData
        )

    /*
    unConstrData : [ data ] -> pair(integer, list(data))
     */
    val UnConstrData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Pair(Integer, DefaultUni.List(DefaultUni.Data)),
          (a: CekValue) =>
              a match {
                  case VCon(Constant.Data(Data.Constr(i, ls))) =>
                      (m: CekMachine) =>
                          VCon(
                            Constant.Pair(asConstant(i), asConstant(ls))
                          )
                  case _ => throw new DeserializationError(DefaultFun.UnConstrData, a)
              },
          _.unConstrData
        )

    /*  unMapData : [ data ] -> list(pair(data, data))
     */
    val UnMapData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data)),
          (a: CekValue) =>
              a match {
                  case VCon(Constant.Data(Data.Map(values))) =>
                      (m: CekMachine) =>
                          VCon(
                            Constant.List(
                              DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                              values.map { case (k, v) =>
                                  Constant.Pair(asConstant(k), asConstant(v))
                              }
                            )
                          )
                  case _ => throw new DeserializationError(DefaultFun.UnMapData, a)
              },
          _.unMapData
        )
    /*  unListData : [ data ] -> list(data)
     */
    val UnListData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.List(DefaultUni.Data),
          (a: CekValue) => {
              a match
                  case VCon(Constant.Data(Data.List(values))) =>
                      (m: CekMachine) =>
                          VCon(Constant.List(DefaultUni.Data, values.map(asConstant)))
                  case _ => throw new DeserializationError(DefaultFun.UnListData, a)
          },
          _.unListData
        )

    /*  unIData : [ data ] -> integer
     */
    val UnIData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Integer,
          (a: CekValue) => {
              a match
                  case VCon(Constant.Data(Data.I(i))) =>
                      (m: CekMachine) => VCon(asConstant(i))
                  case _ => throw new DeserializationError(DefaultFun.UnIData, a)
          },
          _.unIData
        )

    /*  unBData : [ data ] -> bytestring
     */
    val UnBData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.ByteString,
          (a: CekValue) => {
              a match
                  case VCon(Constant.Data(Data.B(b))) =>
                      (m: CekMachine) => VCon(asConstant(b))
                  case _ => throw new DeserializationError(DefaultFun.UnBData, a)
          },
          _.unBData
        )

    val EqualsData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Data ->: DefaultUni.Bool,
          (a: CekValue) =>
              val aa = a.asData
              (b: CekValue) =>
                  val bb = b.asData
                  (m: CekMachine) => VCon(Constant.Bool(equalsData(aa, bb)))
          ,
          _.equalsData
        )

    val SerialiseData = mkMeaning(
      DefaultUni.Data ->: DefaultUni.ByteString,
      (a: CekValue) =>
          val aa = a.asData
          (m: CekMachine) => VCon(Constant.ByteString(serialiseData(aa)))
      ,
      _.serialiseData
    )

    val MkPairData =
        mkMeaning(
          DefaultUni.Data ->: DefaultUni.Data ->: DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
          (a: CekValue) =>
              val aa = a.asData
              (b: CekValue) =>
                  val bb = b.asData
                  (m: CekMachine) => VCon(Constant.Pair(asConstant(aa), asConstant(bb)))
          ,
          _.mkPairData
        )

    val MkNilData =
        mkMeaning(
          DefaultUni.Unit ->: DefaultUni.List(DefaultUni.Data),
          (a: CekValue) =>
              val _ = a.asUnit
              (m: CekMachine) => VCon(Constant.List(DefaultUni.Data, Nil))
          ,
          _.mkNilData
        )

    val MkNilPairData = mkMeaning(
      DefaultUni.Unit ->: DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
      (a: CekValue) =>
          val _ = a.asUnit
          (m: CekMachine) =>
              VCon(Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), Nil))
      ,
      _.mkNilPairData
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
