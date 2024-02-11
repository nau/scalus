package scalus.uplc

import scalus.builtins.ByteString
import scalus.builtins.Data

sealed abstract class DefaultUni:
    type Unlifted

object DefaultUni:

    trait Lift[A]:
        def defaultUni: DefaultUni

    sealed abstract class LiftedUni[A] extends DefaultUni with Lift[A]:
        type Unlifted = A
        def defaultUni: DefaultUni = this

    //  given LiftBigInt: Lift[BigInt] with
    //    def defaultUni: DefaultUni = DefaultUni.Integer

    def defaultUniFromValue[A: Lift](value: A): DefaultUni = summon[Lift[A]].defaultUni
    def asConstant[A: Constant.LiftValue](value: A): Constant =
        summon[Constant.LiftValue[A]].lift(value)

    given Lift[Int] with
        def defaultUni: DefaultUni = DefaultUni.Integer

    given Lift[Long] with
        def defaultUni: DefaultUni = DefaultUni.Integer

    implicit case object Integer extends LiftedUni[BigInt]
    implicit case object ByteString extends LiftedUni[ByteString]
    implicit case object String extends LiftedUni[String]
    implicit case object Unit extends LiftedUni[Unit]
    implicit case object Bool extends LiftedUni[Boolean]

    case object Data extends DefaultUni:
        type Unlifted = Data

    case object ProtoList extends DefaultUni:
        type Unlifted = Nothing // [A] =>> immutable.List[A]

    case object ProtoPair extends DefaultUni:
        type Unlifted = Nothing // [A, B] =>> (A, B)

    case class Apply(f: DefaultUni, arg: DefaultUni) extends DefaultUni:
        type Unlifted = f.Unlifted => arg.Unlifted

    def Pair(a: DefaultUni, b: DefaultUni): DefaultUni = Apply(Apply(ProtoPair, a), b)
    def List(a: DefaultUni): DefaultUni = Apply(ProtoList, a)

    implicit object LiftData extends Lift[scalus.builtins.Data]:
        def defaultUni: DefaultUni = DefaultUni.Data
