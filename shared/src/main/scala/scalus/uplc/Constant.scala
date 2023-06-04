package scalus.uplc

import scalus.builtins

import scala.collection.immutable

sealed trait Constant:
  def tpe: DefaultUni

object Constant:

  trait LiftValue[-A]:
    def lift(a: A): Constant

  given LiftValue[BigInt] with { def lift(a: BigInt): Constant = Integer(a) }
  given LiftValue[Int] with { def lift(a: Int): Constant = Integer(a) }
  given LiftValue[Long] with { def lift(a: Long): Constant = Integer(a) }
  given LiftValue[builtins.ByteString] with {
    def lift(a: builtins.ByteString): Constant = ByteString(a)
  }
  given LiftValue[java.lang.String] with { def lift(a: java.lang.String): Constant = String(a) }
  given LiftValue[Boolean] with { def lift(a: Boolean): Constant = Bool(a) }
  given LiftValue[Unit] with { def lift(a: Unit): Constant = Unit }
  implicit def LiftValueData[A <: scalus.uplc.Data]: LiftValue[A] = new LiftValue[A] {
    def lift(a: A): Constant = Data(a)
  }
  given seqLiftValue[A: LiftValue: DefaultUni.Lift]: LiftValue[Seq[A]] with {
    def lift(a: Seq[A]): Constant =
      List(summon[DefaultUni.Lift[A]].defaultUni, a.map(summon[LiftValue[A]].lift).toList)
  }

  implicit def tupleLiftValue[A: LiftValue: DefaultUni.Lift, B: LiftValue: DefaultUni.Lift]
      : LiftValue[(A, B)] = new LiftValue[(A, B)] {
    def lift(a: (A, B)): Constant = Pair(
      summon[LiftValue[A]].lift(a._1),
      summon[LiftValue[B]].lift(a._2)
    )
  }

  case class Integer(value: BigInt) extends Constant:
    def tpe = DefaultUni.Integer

  case class ByteString(value: builtins.ByteString) extends Constant:
    def tpe = DefaultUni.ByteString

  case class String(value: java.lang.String) extends Constant:
    def tpe = DefaultUni.String

  case object Unit extends Constant:
    def tpe = DefaultUni.Unit

  case class Bool(value: Boolean) extends Constant:
    def tpe = DefaultUni.Bool

  case class Data(value: scalus.uplc.Data) extends Constant:
    def tpe = DefaultUni.Data

  case class List(elemType: DefaultUni, value: immutable.List[Constant]) extends Constant:
    def tpe = DefaultUni.Apply(DefaultUni.ProtoList, elemType)

  case class Pair(a: Constant, b: Constant) extends Constant:
    def tpe = DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a.tpe), b.tpe)

  def fromValue(tpe: DefaultUni, a: Any): Constant = tpe match {
    case DefaultUni.Integer    => Integer(a.asInstanceOf[BigInt])
    case DefaultUni.ByteString => ByteString(a.asInstanceOf[builtins.ByteString])
    case DefaultUni.String     => String(a.asInstanceOf[java.lang.String])
    case DefaultUni.Unit       => Unit
    case DefaultUni.Bool       => Bool(a.asInstanceOf[Boolean])
    case DefaultUni.Data =>
      Data(a.asInstanceOf[scalus.uplc.Data])
    case DefaultUni.Apply(DefaultUni.ProtoList, elemType) =>
      List(elemType, a.asInstanceOf[Seq[Any]].toList.map(fromValue(elemType, _)))
    case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, aType), bType) =>
      Pair(
        fromValue(aType, a.asInstanceOf[(Any, Any)]._1),
        fromValue(bType, a.asInstanceOf[(Any, Any)]._2)
      )
    case _ => throw new IllegalArgumentException(s"Cannot convert $a to $tpe")
  }

  def toValue(c: Constant): Any = c match
    case Integer(value)    => value
    case ByteString(value) => value
    case String(value)     => value
    case Unit              => ()
    case Bool(value)       => value
    case Data(value)       => value
    case List(_, value)    => value.map(toValue)
    case Pair(a, b)        => (toValue(a), toValue(b))
