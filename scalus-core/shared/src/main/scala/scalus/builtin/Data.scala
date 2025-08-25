package scalus.builtin

import scalus.builtin.Data.{B, Constr, I, List, Map}

import scala.collection.immutable
import scala.compiletime.asMatchable

sealed abstract class Data {
    private[scalus] def showDebug: String = this match {
        case Constr(constr, args) =>
            s"<$constr, [${args.map(_.showDebug).mkString(", ")}]>"
        case Map(values) =>
            s"{${values.map { case (k, v) => s"${k.showDebug}: ${v.showDebug}" }.mkString(", ")}}"
        case List(values) =>
            s"[${values.map(_.showDebug).mkString(", ")}]"
        case I(value) =>
            s"$value"
        case B(value) =>
            s"\"${value.toHex}\""
    }

    override def toString: String = showDebug
}

object Data extends DataApi:

    type ToData[A] = scalus.builtin.ToData[A]
    extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]](a)
    extension (inline data: Data) inline def to[A](using inline ev: FromData[A]): A = ev(data)

    // type FromData[A] = Data => A
    type FromData[A] = scalus.builtin.FromData[A]

    inline def fromData[A](inline data: Data)(using inline ev: scalus.builtin.FromData[A]): A = ev(
      data
    )

    case class Constr(constr: Long, args: immutable.List[Data]) extends Data {
        assert(constr >= 0, s"Constructor must be non-negative, got $constr")
    }

    case class Map(values: immutable.List[(Data, Data)]) extends Data {
        override def hashCode(): Int = values.toSet.hashCode()
        override def equals(x: Any): Boolean = x.asMatchable match {
            case Map(otherValues) => values.toSet == otherValues.toSet
            case _                => false
        }
    }

    case class List(values: immutable.List[Data]) extends Data:
        override def toString: String = s"List(${values.map(v => v.toString + "::").mkString}Nil)"

    case class I(value: BigInt) extends Data

    case class B(value: ByteString) extends Data:
        override def toString: String = s"B(\"${value.toHex}\")"

    val unit: Data = Constr(0, immutable.Nil)
