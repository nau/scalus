package scalus.uplc

import scalus.builtins.ByteString
import scalus.utils.Hex.bytesToHex

import scala.collection.immutable
import scala.collection.immutable.List

sealed abstract class Data

object Data:
  type ToData[A] = A => Data

  extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]](a)

  type FromData[A] = Data => A
  // fromData extension method
  inline def fromData[A](inline data: Data)(using inline ev: FromData[A]): A = ev(data)

  case class Constr(constr: Long, args: immutable.List[Data]) extends Data

  case class Map(values: immutable.List[(Data, Data)]) extends Data

  case class List(values: immutable.List[Data]) extends Data:
    override def toString: String = s"List(${values.map(v => v.toString + "::").mkString}Nil)"

  case class I(value: BigInt) extends Data

  case class B(value: ByteString) extends Data:
    override def toString: String = s"B(\"${value.toHex}\")"
