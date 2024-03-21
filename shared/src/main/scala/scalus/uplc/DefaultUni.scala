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

    //  given LiftBigInt: Lift[BigInt] with
    //    def defaultUni: DefaultUni = DefaultUni.Integer

    def defaultUniFromValue[A: Lift](value: A): DefaultUni = summon[Lift[A]].defaultUni
    def asConstant[A: Constant.LiftValue](value: A): Constant =
        summon[Constant.LiftValue[A]].lift(value)

    given Lift[Int] with
        def defaultUni: DefaultUni = DefaultUni.Integer

    given Lift[Long] with
        def defaultUni: DefaultUni = DefaultUni.Integer

    implicit case object Integer extends LiftedUni[BigInt] {
        override def toString(): java.lang.String = "DefaultUni.Integer"
    }
    implicit case object ByteString extends LiftedUni[ByteString] {
        override def toString(): java.lang.String = "DefaultUni.ByteString"
    }
    implicit case object String extends LiftedUni[String] {
        override def toString(): java.lang.String = "DefaultUni.String"
    }
    implicit case object Unit extends LiftedUni[Unit] {
        override def toString(): java.lang.String = "DefaultUni.Unit"
    }
    implicit case object Bool extends LiftedUni[Boolean] {
        override def toString(): java.lang.String = "DefaultUni.Bool"
    }

    case object Data extends DefaultUni:
        type Unlifted = Data
        override def toString(): java.lang.String = "DefaultUni.Data"

    case object ProtoList extends DefaultUni:
        type Unlifted = Nothing // [A] =>> immutable.List[A]
        override def toString(): java.lang.String = "DefaultUni.ProtoList"

    case object ProtoPair extends DefaultUni:
        type Unlifted = Nothing // [A, B] =>> (A, B)
        override def toString(): java.lang.String = "DefaultUni.ProtoPair"

    case class Apply(f: DefaultUni, arg: DefaultUni) extends DefaultUni:
        type Unlifted = f.Unlifted => arg.Unlifted
        override def toString(): java.lang.String = s"DefaultUni.Apply($f, $arg)"

    def Pair(a: DefaultUni, b: DefaultUni): DefaultUni = Apply(Apply(ProtoPair, a), b)
    def List(a: DefaultUni): DefaultUni = Apply(ProtoList, a)

    implicit object LiftData extends Lift[scalus.builtin.Data]:
        def defaultUni: DefaultUni = DefaultUni.Data
