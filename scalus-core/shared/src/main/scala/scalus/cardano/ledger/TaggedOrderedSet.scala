package scalus.cardano.ledger

import scala.collection.immutable.SortedSet
import io.bullet.borer.{DataItem, Decoder, Encoder, Reader, Tag, Writer}

opaque type TaggedOrderedSet[A] = SortedSet[A]
object TaggedOrderedSet {
    inline def empty[A: Ordering]: TaggedOrderedSet[A] = SortedSet.empty[A]

    extension [A](s: TaggedOrderedSet[A]) {

        /** Converts a `TaggedOrderedSet` to a `SortedSet` */
        inline def toSortedSet: SortedSet[A] = s

        /** Converts a `TaggedOrderedSet` to a `Seq` */
        inline def toSeq: Seq[A] = s.toSeq
    }

    inline def apply[A](s: SortedSet[A]): TaggedOrderedSet[A] = s

    /** Creates a `TaggedOrderedSet` with the specified elements.
      * @tparam A
      *   the type of the `TaggedOrderedSet`'s elements
      * @param elems
      *   the elements of the created `TaggedOrderedSet`
      * @return
      *   a new `TaggedOrderedSet` with elements `elems`
      */
    def apply[A: Ordering](elems: A*): TaggedOrderedSet[A] = SortedSet(elems*)

    def from[A: Ordering](it: IterableOnce[A]): TaggedOrderedSet[A] = SortedSet.from(it)

    given [A: Encoder]: Encoder[TaggedOrderedSet[A]] with
        def write(w: Writer, value: TaggedOrderedSet[A]): Writer = {
            w.writeTag(Tag.Other(258))
            w.writeArrayHeader(value.size)
            value.foreach(w.write(_))
            w
        }

    given [A: Decoder: Ordering]: Decoder[TaggedOrderedSet[A]] with
        def read(r: Reader): TaggedOrderedSet[A] = {
            // Check for indefinite array tag (258)
            if r.dataItem() == DataItem.Tag then
                val tag = r.readTag()
                if tag.code != 258 then
                    r.validationFailure(s"Expected tag 258 for definite Set, got $tag")
            Decoder.fromFactory[A, SortedSet].read(r)
        }
}
