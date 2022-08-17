package scalus.uplc

import org.typelevel.paiges.Doc
import scalus.utils.Utils

import java.util
import scala.collection.immutable

sealed trait Constant:
  def tpe: DefaultUni

  def prettyValue: Doc

  def pretty: Doc = tpe.pretty + Doc.space + prettyValue

object Constant:

  trait LiftValue[-A]:
    def lift(a: A): Constant

  given LiftValue[BigInt] with { def lift(a: BigInt): Constant = Integer(a) }
  given LiftValue[Int] with { def lift(a: Int): Constant = Integer(a) }
  given LiftValue[Long] with { def lift(a: Long): Constant = Integer(a) }
  given LiftValue[Array[Byte]] with { def lift(a: Array[Byte]): Constant = ByteString(a) }
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
    def prettyValue = Doc.text(value.toString)

  case class ByteString(value: Array[Byte]) extends Constant:
    def tpe = DefaultUni.ByteString
    def prettyValue = Doc.text("#" + Utils.bytesToHex(value))

    override def equals(obj: Any): Boolean = obj match {
      case ByteString(value) => util.Arrays.equals(value, this.value)
      case _                 => false
    }

    override def hashCode(): Int = util.Arrays.hashCode(value)

  case class String(value: java.lang.String) extends Constant:
    def tpe = DefaultUni.String
    def prettyValue = Doc.text("\"" + value + "\"")

  case object Unit extends Constant:
    def tpe = DefaultUni.Unit
    def prettyValue = Doc.text("()")

  case class Bool(value: Boolean) extends Constant:
    def tpe = DefaultUni.Bool
    def prettyValue = Doc.text(if value then "True" else "False")

  case class Data(value: scalus.uplc.Data) extends Constant:
    def tpe = DefaultUni.Data
    def prettyValue = Doc.text(value.toString)

  case class List(elemType: DefaultUni, value: immutable.List[Constant]) extends Constant:
    def tpe = DefaultUni.Apply(DefaultUni.ProtoList, elemType)
    def prettyValue =
      Doc.text("[") + Doc.intercalate(Doc.text(", "), value.map(_.prettyValue)) + Doc.text("]")

  case class Pair(a: Constant, b: Constant) extends Constant:
    def tpe = DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a.tpe), b.tpe)
    def prettyValue =
      Doc.text("(") + a.prettyValue + Doc.text(", ") + b.prettyValue + Doc.text(")")
