package scalus.uplc

import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable
import scala.collection.immutable.List
import scala.deriving.*
import scala.quoted.*
import scalus.macros.Macros
import scalus.Predef.Maybe
import scalus.Predef.===

sealed abstract class Data

case class TestProduct(a: BigInt)
object Data:
  trait ToData[A]:
    def toData(a: A): Data

  type FromData[A] = Data => A

  given BigIntFromData: FromData[BigInt] = Builtins.unsafeDataAsI
  given ByteStringFromData: FromData[ByteString] = Builtins.unsafeDataAsB
  given FromData[Data] = (d: Data) => d

  given BoolFromData: FromData[Boolean] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val constr = pair.fst
    if constr === BigInt(0) then false
    else if constr === BigInt(1) then true
    else throw new RuntimeException("Not a boolean")

  given ListFromData[A: FromData]: FromData[scalus.Predef.List[A]] = (d: Data) =>
    val fromA = summon[FromData[A]]
    val ls = Builtins.unsafeDataAsList(d)
    def loop(ls: scalus.builtins.List[Data]): scalus.Predef.List[A] =
      if ls.isEmpty then scalus.Predef.List.Nil
      else scalus.Predef.List.Cons(fromA(ls.head), loop(ls.tail))
    loop(ls)

  given MaybeFromData[A: FromData]: FromData[scalus.Predef.Maybe[A]] = (d: Data) =>
    val fromA = summon[FromData[A]]
    val pair = Builtins.unsafeDataAsConstr(d)
    if pair.fst === BigInt(0) then scalus.Predef.Maybe.Just(fromA(pair.snd.head))
    else scalus.Predef.Maybe.Nothing

  given tupleFromData[A, B](using fromA: FromData[A], fromB: FromData[B]): FromData[(A, B)] =
    (d: Data) =>
      val pair = Builtins.unsafeDataAsConstr(d)
      val constr = pair.fst
      val args = pair.snd
      if constr === BigInt(0) then (fromA(args.head), fromB(args.tail.head))
      else throw new RuntimeException("Not a Tuple2")

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

  given ToData[Boolean] with {
    def toData(a: Boolean): Data = if a then Constr(1, Nil) else Constr(0, Nil)
  }
  given ToData[Data] with { def toData(a: Data): Data = a }
  given ToData[BigInt] with { def toData(a: BigInt): Data = Builtins.mkI(a) }
  given ToData[Int] with { def toData(a: Int): Data = Builtins.mkI(a) }
  given ToData[ByteString] with { def toData(a: ByteString): Data = Builtins.mkB(a) }
  given seqToData[A: ToData, B[A] <: Seq[A]]: ToData[B[A]] with {
    def toData(a: B[A]): Data = List(a.map(summon[ToData[A]].toData).toList)
  }

  given listToData[A: ToData]: ToData[scalus.Predef.List[A]] with {
    def toData(a: scalus.Predef.List[A]): Data =
      val aToData = summon[ToData[A]]
      def loop(a: scalus.Predef.List[A]): scalus.builtins.List[Data] =
        a match
          case scalus.Predef.List.Nil => scalus.builtins.List.Nil
          case scalus.Predef.List.Cons(head, tail) =>
            scalus.builtins.List.Cons(aToData.toData(head), loop(tail))
      Builtins.mkList(loop(a))
  }

  given mapToData[A: ToData, B: ToData]: ToData[immutable.Map[A, B]] with {
    def toData(a: immutable.Map[A, B]): Data = Map(a.toList.map { case (a, b) =>
      (summon[ToData[A]].toData(a), summon[ToData[B]].toData(b))
    })
  }

  given tupleToData[A: ToData, B: ToData]: ToData[(A, B)] with {
    def toData(a: (A, B)): Data =
      Constr(0, summon[ToData[A]].toData(a._1) :: summon[ToData[B]].toData(a._2) :: Nil)
  }

  given OptionToData[A: ToData]: ToData[Option[A]] with
    def toData(a: Option[A]): Data = a match
      case Some(v) => Data.Constr(0, immutable.List(v.toData))
      case None    => Data.Constr(1, immutable.List.empty)

  given MaybeToData[A: ToData]: ToData[Maybe[A]] with
    def toData(a: Maybe[A]): Data = a match
      case Maybe.Just(v) => Data.Constr(0, immutable.List(v.toData))
      case Maybe.Nothing => Data.Constr(1, immutable.List.empty)

  given EitherToData[A: ToData, B: ToData]: ToData[Either[A, B]] with
    def toData(a: Either[A, B]): Data = a match
      case Left(v)  => Data.Constr(0, immutable.List(v.toData))
      case Right(v) => Data.Constr(1, immutable.List(v.toData))

  case class Constr(constr: Long, args: immutable.List[Data]) extends Data

  case class Map(values: immutable.List[(Data, Data)]) extends Data

  case class List(values: immutable.List[Data]) extends Data:
    override def toString: String = s"List(${values.map(v => v.toString + "::").mkString}Nil)"

  case class I(value: BigInt) extends Data

  case class B(value: ByteString) extends Data:
    override def toString: String = s"B(\"${value.toHex}\")"
