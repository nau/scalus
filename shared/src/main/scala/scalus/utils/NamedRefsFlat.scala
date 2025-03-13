package scalus.utils

import scalus.flat.*

trait NamedRef[A] {
    def isForward: Boolean

    def isComplete: Boolean

    def finValue(state: NamedRefs.State): A
    
}

object NamedRefs {

    opaque type State = Map[String, Any]

}

class NamedRefsEncoderState(val encoderState: EncoderState, namedRefs: NamedRefs.State)

class NamedRefsDecoderState(val decoderState: DecoderState, namedRefs: NamedRefs.State)

trait NamedRefFlat[A <: AnyRef, SA <: NamedRef[A]] {

    type Repr = SA

    def toRepr(a: A): SA

    def bitSizeHC(a: A, namedRes: Map[String, NamedRef[?]]): Int

    def encodeHC(a: A, encode: HashConsedEncoderState): Unit

    def decodeHC(decode: HashConsedDecoderState): SA

}
