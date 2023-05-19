package scalus.uplc

import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable
import scala.deriving.*
import scala.quoted.*
import scalus.macros.Macros
import scalus.prelude
import scalus.prelude.Maybe
import scalus.prelude.Prelude.{===, given}
import scalus.prelude.AssocMap
import scalus.prelude.Prelude
import scalus.builtins
import scalus.builtins.Pair
import scalus.Compile
import scalus.uplc.Data.*

@Compile
object DataInstances:

  given FromData[BigInt] = (d: Data) => Builtins.unsafeDataAsI(d)
  given FromData[ByteString] = (d: Data) => Builtins.unsafeDataAsB(d)
  given FromData[Data] = (d: Data) => d

  given BoolFromData: FromData[Boolean] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val constr = pair.fst
    if constr === BigInt(0) then false
    else if constr === BigInt(1) then true
    else throw new RuntimeException("Not a boolean")

  given ListFromData[A: FromData]: FromData[scalus.prelude.List[A]] = (d: Data) =>
    val ls = Builtins.unsafeDataAsList(d)
    def loop(ls: scalus.builtins.List[Data]): scalus.prelude.List[A] =
      if ls.isEmpty then prelude.List.Nil
      else new prelude.List.Cons(fromData[A](ls.head), loop(ls.tail))
    loop(ls)

  given AssocMapFromData[A: FromData, B: FromData]: FromData[AssocMap[A, B]] =
    (d: Data) =>
      val ls = Builtins.unsafeDataAsMap(d)
      def loop(ls: scalus.builtins.List[Pair[Data, Data]]): prelude.List[(A, B)] =
        if ls.isEmpty then prelude.List.Nil
        else
          val pair = ls.head
          new prelude.List.Cons((fromData[A](pair.fst), fromData[B](pair.snd)), loop(ls.tail))
      AssocMap.fromList(loop(ls))

  given MaybeFromData[A: FromData]: FromData[scalus.prelude.Maybe[A]] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    if pair.fst === BigInt(0) then new scalus.prelude.Maybe.Just(fromData[A](pair.snd.head))
    else scalus.prelude.Maybe.Nothing

  /* given tupleFromData[A, B](using fromA: FromData[A], fromB: FromData[B]): FromData[(A, B)] =
    (d: Data) =>
      val pair = Builtins.unsafeDataAsConstr(d)
      val constr = pair.fst
      val args = pair.snd
      if constr === BigInt(0) then (fromA(args.head), fromB(args.tail.head))
      else throw new RuntimeException("Not a Tuple2") */

  given unsafeTupleFromData[A, B](using fromA: FromData[A], fromB: FromData[B]): FromData[(A, B)] =
    (d: Data) =>
      val pair = Builtins.unsafeDataAsConstr(d)
      val args = pair.snd
      (fromA(args.head), fromB(args.tail.head))

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

  given listToData[A: ToData]: ToData[scalus.prelude.List[A]] with {
    def toData(a: scalus.prelude.List[A]): Data =
      val aToData = summon[ToData[A]]
      def loop(a: scalus.prelude.List[A]): scalus.builtins.List[Data] =
        a match
          case scalus.prelude.List.Nil => scalus.builtins.List.Nil
          case scalus.prelude.List.Cons(head, tail) =>
            new scalus.builtins.List.Cons(aToData.toData(head), loop(tail))
      Builtins.mkList(loop(a))
  }

  given mapToData[A: ToData, B: ToData]: ToData[immutable.Map[A, B]] with {
    def toData(a: immutable.Map[A, B]): Data = Map(a.toList.map { case (a, b) =>
      (summon[ToData[A]].toData(a), summon[ToData[B]].toData(b))
    })
  }

  given assocMapToData[A: ToData, B: ToData]: ToData[AssocMap[A, B]] with {
    def toData(a: AssocMap[A, B]): Data =
      def go(a: prelude.List[(A, B)]): builtins.List[Pair[Data, Data]] = a match
        case prelude.List.Nil => builtins.List.empty
        case prelude.List.Cons(tuple, tail) =>
          tuple match
            case (a, b) =>
              new builtins.List.Cons(
                Pair(summon[ToData[A]].toData(a), summon[ToData[B]].toData(b)),
                go(tail)
              )
      Builtins.mkMap(go(AssocMap.toList(a)))
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
