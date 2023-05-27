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
}

@Compile
object ToDataInstances {
  given ToData[Boolean] = (a: Boolean) =>
    if a then Builtins.mkConstr(1, builtins.List.Nil) else Builtins.mkConstr(0, builtins.List.Nil)
  given ToData[Data] = (a: Data) => a
  given ToData[BigInt] = (a: BigInt) => Builtins.mkI(a)
  given ToData[Int] = (a: Int) => Builtins.mkI(a)
  given ToData[ByteString] = (a: ByteString) => Builtins.mkB(a)

  given listToData[A: ToData]: ToData[scalus.prelude.List[A]] =
    (a: scalus.prelude.List[A]) => {
      val aToData = summon[ToData[A]]
      def loop(a: scalus.prelude.List[A]): scalus.builtins.List[Data] =
        a match
          case scalus.prelude.List.Nil => scalus.builtins.List.Nil
          case scalus.prelude.List.Cons(head, tail) =>
            new scalus.builtins.List.Cons(aToData(head), loop(tail))

      Builtins.mkList(loop(a))
    }

  given assocMapToData[A: ToData, B: ToData]: ToData[AssocMap[A, B]] =
    (a: AssocMap[A, B]) => {
      def go(a: prelude.List[(A, B)]): builtins.List[Pair[Data, Data]] = a match {
        case prelude.List.Nil => builtins.List.Nil
        case prelude.List.Cons(tuple, tail) =>
          tuple match {
            case (a, b) =>
              new builtins.List.Cons(
                Pair(summon[ToData[A]](a), summon[ToData[B]](b)),
                go(tail)
              )
          }
      }
      Builtins.mkMap(go(AssocMap.toList(a)))
    }

  given tupleToData[A: ToData, B: ToData]: ToData[(A, B)] =
    (a: (A, B)) =>
      Builtins.mkConstr(
        0,
        new builtins.List.Cons(
          summon[ToData[A]](a._1),
          new builtins.List.Cons(summon[ToData[B]](a._2), builtins.List.Nil)
        )
      )

  given MaybeToData[A: ToData]: ToData[Maybe[A]] =
    (a: Maybe[A]) => {
      a match {
        case Maybe.Just(v) =>
          Builtins.mkConstr(0, new builtins.List.Cons(v.toData, builtins.List.Nil))
        case Maybe.Nothing => Builtins.mkConstr(1, builtins.List.Nil)
      }
    }

  given EitherToData[A: ToData, B: ToData]: ToData[Either[A, B]] =
    (a: Either[A, B]) =>
      a match
        case Left(v)  => Builtins.mkConstr(0, new builtins.List.Cons(v.toData, builtins.List.Nil))
        case Right(v) => Builtins.mkConstr(1, new builtins.List.Cons(v.toData, builtins.List.Nil))

}
