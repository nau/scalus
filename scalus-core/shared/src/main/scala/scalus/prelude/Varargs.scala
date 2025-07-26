package scalus.prelude

/** Represents a variable number of arguments as a list.
  * @param toList
  * @tparam T
  */
case class Varargs[T](list: scalus.prelude.List[T])

extension [T](seq: scala.collection.immutable.Seq[T])

    def list: scalus.prelude.List[T] =
        scalus.prelude.List.from(seq)
