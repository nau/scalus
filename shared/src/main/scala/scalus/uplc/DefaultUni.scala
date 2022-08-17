package scalus.uplc

import org.typelevel.paiges.Doc

sealed abstract class DefaultUni:
  type Unlifted
  def pretty: Doc = this match
    case DefaultUni.Integer    => Doc.text("integer")
    case DefaultUni.ByteString => Doc.text("bytestring")
    case DefaultUni.String     => Doc.text("string")
    case DefaultUni.Unit       => Doc.text("unit")
    case DefaultUni.Bool       => Doc.text("bool")
    case DefaultUni.Apply(DefaultUni.ProtoList, arg) =>
      Doc.text("(") + Doc.text("list") + Doc.space + arg.pretty + Doc.text(")")
    case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
      Doc.text("(") + Doc.text("pair") + Doc.space + a.pretty + Doc.space + b.pretty + Doc.text(")")
    case DefaultUni.Data => Doc.text("data")
    case _               => sys.error(s"Unexpected default uni: $this")

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
  implicit case object ByteString extends LiftedUni[Array[Byte]]
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

  implicit object LiftData extends Lift[scalus.uplc.Data]:
    def defaultUni: DefaultUni = DefaultUni.Data
