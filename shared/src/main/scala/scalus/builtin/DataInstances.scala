package scalus.builtin

import scalus.Compile
import scalus.builtin

@Compile
@deprecated("Now not needed, use companion objects instead")
object FromDataInstances {
    // import scalus.builtin.Builtins.*

    // inline given FromData[BigInt] = unIData
    // inline given FromData[ByteString] = unBData
    // given FromData[String] = (d: Data) => decodeUtf8(unBData(d))
    // @nowarn // disable warning:
    // "An inline given alias with a function value as right-hand side can
    // significantly increase generated code size."
    // inline given FromData[Data] = (d: Data) => d

    // given FromData[Unit] = (d: Data) =>
    //    if unConstrData(d).fst == BigInt(0) then ()
    //    else throw new RuntimeException("Not a unit")

    // given FromData[Boolean] = (d: Data) =>
    //    val constr = unConstrData(d).fst
    //    if constr == BigInt(0) then false
    //    else if constr == BigInt(1) then true
    //    else throw new RuntimeException("Not a boolean")

    // given ListFromData[A: FromData]: FromData[scalus.prelude.List[A]] = (d: Data) =>
    //    def loop(ls: scalus.builtin.List[Data]): scalus.prelude.List[A] =
    //        if ls.isEmpty then prelude.List.Nil
    //        else new prelude.List.Cons(fromData[A](ls.head), loop(ls.tail))
    //    loop(unListData(d))

    // given AssocMapFromData[A: FromData, B: FromData]: FromData[AssocMap[A, B]] =
    //    (d: Data) =>
    //        def loop(ls: scalus.builtin.List[Pair[Data, Data]]): prelude.List[(A, B)] =
    //            if ls.isEmpty then prelude.List.Nil
    //            else
    //                val pair = ls.head
    //                new prelude.List.Cons(
    //                  (fromData[A](pair.fst), fromData[B](pair.snd)),
    //                  loop(ls.tail)
    //                )
    //        AssocMap.fromList(loop(unMapData(d)))

    // given OptionFromData[A: FromData]: FromData[scalus.prelude.Option[A]] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    if pair.fst == BigInt(0) then new scalus.prelude.Option.Some(fromData[A](pair.snd.head))
    //    else scalus.prelude.Option.None

    // given unsafeTupleFromData[A, B](using
    //    fromA: FromData[A],
    //    fromB: FromData[B]
    // ): FromData[(A, B)] =
    //    (d: Data) =>
    //        val args = unConstrData(d).snd
    //        (fromA(args.head), fromB(args.tail.head))

    // given RationalFromData: FromData[Rational] = FromData.derived[Rational]
}

@Compile
@deprecated("Now not needed, use companion objects instead")
object ToDataInstances {

    // given listToData[A: ToData]: ToData[scalus.prelude.List[A]] =
    //    (a: scalus.prelude.List[A]) => {
    //        def loop(a: scalus.prelude.List[A]): scalus.builtin.List[Data] =
    //            a match
    //                case scalus.prelude.List.Nil => mkNilData()
    //                case scalus.prelude.List.Cons(head, tail) =>
    //                    mkCons(summon[ToData[A]](head), loop(tail))
    //
    //        listData(loop(a))
    //    }

    // given assocMapToData[A: ToData, B: ToData]: ToData[AssocMap[A, B]] =
    //    (a: AssocMap[A, B]) => {
    //        def go(a: prelude.List[(A, B)]): builtin.List[Pair[Data, Data]] = a match {
    //            case prelude.List.Nil => mkNilPairData()
    //            case prelude.List.Cons(tuple, tail) =>
    //                tuple match {
    //                    case (a, b) =>
    //                        mkCons(
    //                          Pair(summon[ToData[A]](a), summon[ToData[B]](b)),
    //                          go(tail)
    //                        )
    //                }
    //        }
    //        mapData(go(a.toList))
    //    }

    // given tupleToData[A: ToData, B: ToData]: ToData[(A, B)] =
    //    (a: (A, B)) =>
    //        constrData(
    //          0,
    //          mkCons(
    //            summon[ToData[A]](a._1),
    //            mkCons(summon[ToData[B]](a._2), mkNilData())
    //          )
    //        )

    // given OptionToData[A: ToData]: ToData[Option[A]] =
    //    (a: Option[A]) => {
    //        a match {
    //            case Option.Some(v) =>
    //                constrData(0, mkCons(v.toData, mkNilData()))
    //            case Option.None => constrData(1, mkNilData())
    //        }
    //    }

    // given EitherToData[A: ToData, B: ToData]: ToData[Either[A, B]] =
    //    (a: Either[A, B]) =>
    //        a match
    //            case Left(v)  => constrData(0, mkCons(v.toData, mkNilData()))
    //            case Right(v) => constrData(1, mkCons(v.toData, mkNilData()))

    // given RationalToData: ToData[Rational] =
    //    ToData.deriveCaseClass[Rational](0)
}
