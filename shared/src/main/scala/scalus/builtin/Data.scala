package scalus.builtin

import scala.collection.immutable

sealed abstract class Data

object Data:
    type ToData[A] = A => Data

    extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]](a)

    type FromData[A] = Data => A
    // fromData extension method
    inline def fromData[A](inline data: Data)(using inline ev: FromData[A]): A = ev(data)

    case class Constr(constr: Long, args: immutable.List[Data]) extends Data {
        assert(constr >= 0, s"Constructor must be non-negative, got $constr")
        override def toString: String =
            if args.isEmpty then s"Constr($constr, Nil)"
            else s"Constr($constr, $args)"
    }

    case class Map(values: immutable.List[(Data, Data)]) extends Data {
        override def hashCode(): Int = values.toSet.hashCode()
        override def equals(x: Any): Boolean = x match {
            case Map(otherValues) => values.toSet == otherValues.toSet
            case _                => false
        }
        override def toString: String =
            if values.isEmpty then "Data.Map(Nil)"
            else s"Data.Map($values)"
    }

    case class List(values: immutable.List[Data]) extends Data:
        override def toString: String =
            s"Data.List($values)"

    case class I(value: BigInt) extends Data:
        override def toString(): String =
            if value.abs < Int.MaxValue then s"I(${value.toString})"
            else s"I(BigInt(\"$value\"))"

    case class B(value: ByteString) extends Data:
        override def toString: String = s"B(hex\"${value.toHex}\")"
