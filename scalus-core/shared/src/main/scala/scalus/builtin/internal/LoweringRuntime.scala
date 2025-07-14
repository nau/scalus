package scalus.builtin.internal

import scalus.builtin.ToData

@scalus.Compile
object LoweringRuntime {

    def fromDataListToPairDataList[A, B](
        input: scalus.prelude.List[(A, B)]
    ): scalus.prelude.List[scalus.builtin.Pair[A, B]] = {
        ???
        // input match
        //    case scalus.prelude.List.Nil => scalus.prelude.List.empty[scalus.builtin.Pair[A, B]]
        //    case scalus.prelude.List.Cons((a, b), tail) =>
        //        scalus.prelude.List
        //            .Cons(scalus.builtin.Pair(a, b), fromDataListToPairDataList(tail))
        ???
    }

    def fromPairDataListToDataList[A, B](
        input: scalus.prelude.List[scalus.builtin.Pair[A, B]]
    ): scalus.prelude.List[(A, B)] = {
        // input match
        //    case scalus.prelude.List.Nil => scalus.prelude.List.empty[(A, B)]
        //    case scalus.prelude.List.Cons(scalus.builtin.Pair(a, b), tail) =>
        //        scalus.prelude.List.Cons((a, b), fromPairDataListToDataList(tail))
        ???
    }

}
