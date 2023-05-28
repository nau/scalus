package scalus.uplc

import scalus.Compile
import scalus.builtins
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.Pair
import scalus.prelude
import scalus.prelude.AssocMap
import scalus.prelude.Maybe
import scalus.prelude.Prelude
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.uplc.Data.*
import scalus.utils.Utils.bytesToHex

@Compile
object FromDataInstances {
  import scalus.builtins.Builtins.*

  given FromData[BigInt] = (d: Data) => unsafeDataAsI(d)
  given FromData[ByteString] = (d: Data) => unsafeDataAsB(d)
  given FromData[Data] = (d: Data) => d

  given BoolFromData: FromData[Boolean] = (d: Data) =>
    val pair = unsafeDataAsConstr(d)
    val constr = pair.fst
    if constr === BigInt(0) then false
    else if constr === BigInt(1) then true
    else throw new RuntimeException("Not a boolean")

  given ListFromData[A: FromData]: FromData[scalus.prelude.List[A]] = (d: Data) =>
    val ls = unsafeDataAsList(d)
    def loop(ls: scalus.builtins.List[Data]): scalus.prelude.List[A] =
      if ls.isEmpty then prelude.List.Nil
      else new prelude.List.Cons(fromData[A](ls.head), loop(ls.tail))
    loop(ls)

  given AssocMapFromData[A: FromData, B: FromData]: FromData[AssocMap[A, B]] =
    (d: Data) =>
      val ls = unsafeDataAsMap(d)
      def loop(ls: scalus.builtins.List[Pair[Data, Data]]): prelude.List[(A, B)] =
        if ls.isEmpty then prelude.List.Nil
        else
          val pair = ls.head
          new prelude.List.Cons((fromData[A](pair.fst), fromData[B](pair.snd)), loop(ls.tail))
      AssocMap.fromList(loop(ls))

  given MaybeFromData[A: FromData]: FromData[scalus.prelude.Maybe[A]] = (d: Data) =>
    val pair = unsafeDataAsConstr(d)
    if pair.fst === BigInt(0) then new scalus.prelude.Maybe.Just(fromData[A](pair.snd.head))
    else scalus.prelude.Maybe.Nothing

  /* given tupleFromData[A, B](using fromA: FromData[A], fromB: FromData[B]): FromData[(A, B)] =
    (d: Data) =>
      val pair = unsafeDataAsConstr(d)
      val constr = pair.fst
      val args = pair.snd
      if constr === BigInt(0) then (fromA(args.head), fromB(args.tail.head))
      else throw new RuntimeException("Not a Tuple2") */

  given unsafeTupleFromData[A, B](using fromA: FromData[A], fromB: FromData[B]): FromData[(A, B)] =
    (d: Data) =>
      val pair = unsafeDataAsConstr(d)
      val args = pair.snd
      (fromA(args.head), fromB(args.tail.head))
}

@Compile
object ToDataInstances {
  import scalus.builtins.Builtins.*

  given ToData[Boolean] = (a: Boolean) =>
    if a then mkConstr(1, mkNilData) else mkConstr(0, mkNilData)
  given ToData[Data] = (a: Data) => a
  given ToData[BigInt] = (a: BigInt) => mkI(a)
  given ToData[Int] = (a: Int) => mkI(a)
  given ToData[ByteString] = (a: ByteString) => mkB(a)

  given listToData[A: ToData]: ToData[scalus.prelude.List[A]] =
    (a: scalus.prelude.List[A]) => {
      def loop(a: scalus.prelude.List[A]): scalus.builtins.List[Data] =
        a match
          case scalus.prelude.List.Nil              => mkNilData
          case scalus.prelude.List.Cons(head, tail) => mkCons(summon[ToData[A]](head), loop(tail))

      mkList(loop(a))
    }

  given assocMapToData[A: ToData, B: ToData]: ToData[AssocMap[A, B]] =
    (a: AssocMap[A, B]) => {
      def go(a: prelude.List[(A, B)]): builtins.List[Pair[Data, Data]] = a match {
        case prelude.List.Nil => mkNilPairData
        case prelude.List.Cons(tuple, tail) =>
          tuple match {
            case (a, b) =>
              mkCons(
                Pair(summon[ToData[A]](a), summon[ToData[B]](b)),
                go(tail)
              )
          }
      }
      mkMap(go(AssocMap.toList(a)))
    }

  given tupleToData[A: ToData, B: ToData]: ToData[(A, B)] =
    (a: (A, B)) =>
      mkConstr(
        0,
        mkCons(
          summon[ToData[A]](a._1),
          mkCons(summon[ToData[B]](a._2), mkNilData)
        )
      )

  given MaybeToData[A: ToData]: ToData[Maybe[A]] =
    (a: Maybe[A]) => {
      a match {
        case Maybe.Just(v) =>
          mkConstr(0, mkCons(v.toData, mkNilData))
        case Maybe.Nothing => mkConstr(1, mkNilData)
      }
    }

  given EitherToData[A: ToData, B: ToData]: ToData[Either[A, B]] =
    (a: Either[A, B]) =>
      a match
        case Left(v)  => mkConstr(0, mkCons(v.toData, mkNilData))
        case Right(v) => mkConstr(1, mkCons(v.toData, mkNilData))

}
