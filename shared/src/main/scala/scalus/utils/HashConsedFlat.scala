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

object PlainIntFlat extends HashConsedFlat[Int] {

    def bitSizeHC(a: Int, hashConsed: HashConsedRead.State): Int = 4*8

    def encodeHC(a: Int, encoderState: HashConsedEncoderState): Unit =
        encoderState.encode.bits(8, (a >>> 24).toByte)
        encoderState.encode.bits(8, (a >>> 16).toByte)
        encoderState.encode.bits(8, (a >>> 8).toByte)
        encoderState.encode.bits(8, a.toByte)

    def decodeHC(decoderState: HashConsedDecoderState): Int =
        var retval = 0
        retval |= ((decoderState.decode.bits8(8) << 24) & 0xFF000000)
        retval |= ((decoderState.decode.bits8(8) << 16) & 0x00FF0000)
        retval |= ((decoderState.decode.bits8(8) << 8)  & 0x0000FF00)
        retval |= (decoderState.decode.bits8(8) & 0x000000FF)
        retval

}


trait HashConsedReferencedFlat[A <: B, P <: B, B <: AnyRef] extends HashConsedFlat[A|P] with HashConsedTagged[B] {

    type Proxy = P

    def makeProxy(): Proxy

    def setRef(proxy: Proxy, a: A): Unit

    def tag: HashConsedRead.Tag

    def bitSizeHCNew(a: A, encode: HashConsedRead.State): Int

    def encodeHCNew(a: A, encode: HashConsedEncoderState): Unit

    def decodeHCNew(decoderState: HashConsedDecoderState): A

    override def bitSizeHC(a: A, hashConsed: HashConsedRead.State): Int = {
        val ihc = a.hashCode
        var retval = PlainIntFlat.bitSize(ihc)
        hashConsed.lookup(ihc, tag) match
            case None =>
                hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                retval  += bitSizeHCNew(a, hashConsed)
                hsahConsed.setRef(ihc, tag, a)
            case Some(_) =>
        retval
    }

    override def encodeHC(a: A, encoderState: HashConsedEncoderState): Unit =
        val ihc = a.hashCode
        PlainIntFlat.encode(ihc, encoderState.encode)
        encoderState.hashConsed.lookup(ihc, tag) match
            case None =>
                encoderState.hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                encodeHCNew(a, encoderState)
                encoderState.hashConsed.setRef(ihc, tag, a)
            case Some(_) =>


    override def decodeHC(decoderState: HashConsedDecoderState): B = {
        val ihc = PlainIntFlat.decode(decoderState.decode)
        decoderState.hashConsed.lookup(ihc, tag) match
            case None =>
                decoderState.hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                val a = decodeHCNew(decoderState)
                decoderState.hashConsed.setRef(ihc, tag, a)
                a
            case Some(Left(fw)) =>
                val proxy = makeProxy()
                fw.addAction(setRef(proxy,_))
                proxy
            case Some(Right(a)) =>
                a.asInstanceOf[B]
    }



}



/**
 * Here we assume, that A can participate in the recursive structures, but
 *  assumes, that A does not contaoins forward references to A.
 *  (note, that subpart of A, which readed separately, can contain forward references to A)
 *
 * @tparam A
 */
trait HashConsedRefFlat[A]  extends HashConsedFlat[HashConsedRead.MutRef[A]] with HashConsedTagged[A] {

    def tag: HashConsedRead.Tag

    def bitSizeHCNew(a: A, encode: HashConsedRead.State): Int

    def encodeHCNew(a: A, encode: HashConsedEncoderState): Unit

    def decodeHCNew(decoderState: HashConsedDecoderState): A

    override def bitSizeHC(refA: MutRef[A], hashConsed: HashConsedRead.State): Int = {
        val a = refA.get
        if (a == null) {
            throw IllegalStateException("Null reference during writing")
        }
        val ihc = a.hashCode
        var retval = PlainIntFlat.bitSize(ihc)
        hashConsed.lookup(ihc, tag) match
            case None =>
                hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                retval += bitSizeHCNew(a, hashConsed)
                hsahConsed.setRef(ihc, tag, a)
            case Some(_) =>
        retval
    }

    override def encodeHC(refA: MutRef[A], encoderState: HashConsedEncoderState): Unit =
        val a = refA.get
        val ihc = a.hashCode
        PlainIntFlat.encode(ihc, encoderState.encode)
        encoderState.hashConsed.lookup(ihc, tag) match
            case None =>
                encoderState.hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                encodeHCNew(a, encoderState)
                encoderState.hashConsed.setRef(ihc, tag, a)
            case Some(_) =>

    override def decodeHC(decoderState: HashConsedDecoderState): MutRef[A] = {
        val ihc = PlainIntFlat.decode(decoderState.decode)
        decoderState.hashConsed.lookup(ihc, tag) match
            case None =>
                val retval = HashConsedRead.MutRef(null)
                decoderState.hashConsed.putForwardRef(HashConsedRead.ForwardRef(ihc, tag, Nil))
                val a = decodeHCNew(decoderState)
                decoderState.hashConsed.setRef(ihc, tag, a)
                retval.set(a)
                retval
            case Some(Left(fw)) =>
                val retval = HashConsedRead.MutRef(null)
                fw.addAction(retval.set)
                retval
            case Some(Right(a)) =>
                val a = a.asInstanceOf[A]
                HashConsedRead.MutRef(a)
    }


}