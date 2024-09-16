package scalus.utils

import scalus.flat.{*, given}

class HashConsedEncoderState(val encode: EncoderState,
                             val hashConsed: HashConsedRead.State)  {

    inline def lookupValue(ihc: Int, tag: HashConsedRead.Tag): Option[AnyRef] = {
        hashConsed.lookupValue(ihc, tag)
    }

    inline def setRef(ihc: Int, tag: HashConsedRead.Tag, a: AnyRef): Boolean = {
        hashConsed.setRef(ihc, tag, a)
    }

    inline def putForwardRef(fw: HashConsedRead.ForwardRef): Boolean = {
        hashConsed.putForwardRef(fw)
    }


}

class HashConsedDecoderState(val decode: DecoderState, val hashConsed: HashConsedRead.State)

trait HashConsedFlat[A] extends Flat[A] {

    final override def bitSize(a: A): Int =
        bitSizeHC(a, HashConsedRead.State.empty)

    final override def encode(a: A, encoderState: EncoderState): Unit =
        encodeHC(a, HashConsedEncoderState(encoderState, HashConsedRead.State.empty))

    def decode(decode: DecoderState): A =
        decodeHC(HashConsedDecoderState(decode, HashConsedRead.State.empty))

    def bitSizeHC(a: A, encoderState: HashConsedRead.State): Int

    def encodeHC(a: A, encoderState: HashConsedEncoderState): Unit

    def decodeHC(decoderState: HashConsedDecoderState): A

}

trait HashConsedTagged[A] {

    def tag: HashConsedRead.Tag

}


object HashConsedFlat {

    given listHashConsedFlat[A](using flatA: HashConsedFlat[A]): HashConsedFlat[List[A]] with

        def bitSizeHC(a: List[A], hashConsed: HashConsedRead.State): Int = {
            val size = a.foldLeft(0)((acc, elem) => acc + flatA.bitSizeHC(elem, hashConsed))
            size
        }
        def encodeHC(a: List[A], encode: HashConsedEncoderState): Unit = {
            val nElements = a.size
            summon[Flat[Int]].encode(nElements, encode.encode)
            a.foreach(elem => flatA.encodeHC(elem,encode))
        }
        def decodeHC(decode: HashConsedDecoderState): List[A] = {
            val size = summon[Flat[Int]].decode(decode.decode)
            (0 until size).map(_ => flatA.decodeHC(decode)).toList
        }




}


trait HashConsedReferencedFlat[A <: AnyRef] extends HashConsedFlat[A] with HashConsedTagged[A] {

    def tag: HashConsedRead.Tag

    def bitSizeHCNew(a: A, encode: HashConsedRead.State): Int

    def encodeHCNew(a: A, encode: HashConsedEncoderState): Unit

    def decodeHCNew(decoderState: HashConsedDecoderState): A

    override def bitSizeHC(a: A, hashConsed: HashConsedRead.State): Int = {
        val ihc = a.hashCode
        hashConsed.lookupValue(ihc, tag) match {
            case None =>
                val size = 4 + bitSizeHCNew(a, hashConsed)
                hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                size
            case Some(_) =>
                4
        }
    }

    override def encodeHC(a: A, encoderState: HashConsedEncoderState): Unit =
        val ihc = a.hashCode
        encoderState.hashConsed.lookupValue(ihc, tag) match {
            case None =>
                summon[Flat[Int]].encode(ihc, encoderState.encode)
                encoderState.hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                encodeHCNew(a, encoderState)
                encoderState.hashConsed.setRef(ihc, tag, a)
            case Some(_) =>
                summon[Flat[Int]].encode(ihc, encoderState.encode)
        }

    override def decodeHC(decoderState: HashConsedDecoderState): A = {
        val ihc = summon[Flat[Int]].decode(decoderState.decode)
        decoderState.hashConsed.lookupValue(ihc, tag) match {
            case None =>
                decoderState.hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                val a = decodeHCNew(decoderState)
                decoderState.hashConsed.setRef(ihc, tag, a)
                a
            case Some(a) =>
                a.asInstanceOf[A]
        }
    }



}
