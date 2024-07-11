package scalus.utils

import scalus.flat.{*, given}

class HashConsedEncoderState(val encode: EncoderState, val hashConsed: HashConsed.Map)

class HashConsedDecoderState(val decode: DecoderState, val hashConsed: HashConsed.Map)

trait HashConsedFlat[A] extends Flat[A] {

    final override def bitSize(a: A): Int =
        bitSizeHC(a, HashConsed.empty)

    final override def encode(a: A, encoderState: EncoderState): Unit =
        encodeHC(a, HashConsedEncoderState(encoderState, HashConsed.empty))

    def decode(decode: DecoderState): A =
        decodeHC(HashConsedDecoderState(decode, HashConsed.empty))

    def bitSizeHC(a: A, hashConsed: HashConsed.Map): Int

    def encodeHC(a: A, encode: HashConsedEncoderState): Unit

    def decodeHC(decode: HashConsedDecoderState): A

}


object HashConsedFlat {

    given listHashConsedFlat[A](using flatA: HashConsedFlat[A]): HashConsedFlat[List[A]] with

        def bitSizeHC(a: List[A], hashConsed: HashConsed.Map): Int = {
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