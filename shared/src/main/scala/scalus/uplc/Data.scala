package scalus.uplc

import scalus.builtins.ByteString
import scalus.utils.Hex.bytesToHex

import java.util
import scala.collection.immutable
import scala.collection.immutable.List
import scala.deriving.*
import scala.quoted.*

sealed abstract class Data

object Data:
  trait ToData[A]:
    def toData(a: A): Data

  object ToData:
    import scala.compiletime.*
    inline def summonAll[T <: Tuple]: immutable.List[ToData[_]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Nil
        case _: (t *: ts)  => summonInline[ToData[t]] :: summonAll[ts]

    def liftSum[T](m: Mirror.SumOf[T], elems: => immutable.List[ToData[_]]): ToData[T] =
      new ToData[T] {
        def toData(a: T): Data =
          Data.Constr(
            m.ordinal(a),
            elems
              .zip(a.asInstanceOf[Product].productIterator)
              .map { case (l, a) =>
                l.asInstanceOf[ToData[Any]].toData(a)
              }
          )
      }
    def liftProduct[T](
        constrIdx: Int,
        p: Mirror.ProductOf[T],
        elems: => immutable.List[ToData[_]]
    ): ToData[T] =
      new ToData[T] {
        def toData(a: T): Data =
          Data.Constr(
            constrIdx,
            elems.zip(a.asInstanceOf[Product].productIterator).map { case (l, a) =>
              l.asInstanceOf[ToData[Any]].toData(a)
            }
          )
      }

    inline def derived[T](using m: Mirror.Of[T]): ToData[T] =
      val elemInstances = summonAll[m.MirroredElemTypes]
      //      println(elemInstances)
      inline m match
        case m: Mirror.SumOf[T]     => liftSum(m, elemInstances)
        case m: Mirror.ProductOf[T] => liftProduct(0, m, elemInstances)

    inline def deriveProduct[T](constrIdx: Int)(using m: Mirror.ProductOf[T]): ToData[T] =
      val elemInstances = summonAll[m.MirroredElemTypes]
      liftProduct(constrIdx, m, elemInstances)

  extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]].toData(a)

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
