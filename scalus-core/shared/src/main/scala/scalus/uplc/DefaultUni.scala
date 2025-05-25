package scalus.uplc

import scalus.builtin.ByteString
import scalus.builtin.Data

sealed abstract class DefaultUni:
    type Unlifted

object DefaultUni:

    trait Lift[A]:
        def defaultUni: DefaultUni

    sealed abstract class LiftedUni[A] extends DefaultUni with Lift[A]:
        type Unlifted = A
        def defaultUni: DefaultUni = this

    case object Integer extends LiftedUni[BigInt]
    case object ByteString extends LiftedUni[ByteString]
    case object String extends LiftedUni[String]
    case object Unit extends LiftedUni[Unit]
    case object Bool extends LiftedUni[Boolean]
    case object BLS12_381_G1_Element extends DefaultUni
    case object BLS12_381_G2_Element extends DefaultUni
    case object BLS12_381_MlResult extends DefaultUni
    case object Data extends DefaultUni:
        type Unlifted = Data

    case object ProtoList extends DefaultUni:
        type Unlifted = Nothing // [A] =>> immutable.List[A]

    case object ProtoPair extends DefaultUni:
        type Unlifted = Nothing // [A, B] =>> (A, B)

    case class Apply(f: DefaultUni, arg: DefaultUni) extends DefaultUni:
        type Unlifted = f.Unlifted => arg.Unlifted

    def defaultUniFromValue[A: Lift](value: A): DefaultUni = summon[Lift[A]].defaultUni
    def asConstant[A: Constant.LiftValue](value: A): Constant =
        summon[Constant.LiftValue[A]].lift(value)

    given Lift[Int] with
        def defaultUni: DefaultUni = DefaultUni.Integer

    given Lift[Long] with
        def defaultUni: DefaultUni = DefaultUni.Integer

    given Lift[BigInt] = Integer
    given Lift[ByteString] = ByteString
    given Lift[String] = String
    given Lift[Unit] = Unit
    given Lift[Boolean] = Bool
    given Lift[scalus.builtin.Data] with
        def defaultUni: DefaultUni = DefaultUni.Data

    def Pair(a: DefaultUni, b: DefaultUni): DefaultUni = Apply(Apply(ProtoPair, a), b)
    def List(a: DefaultUni): DefaultUni = Apply(ProtoList, a)
