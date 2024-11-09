package scalus.sir

import scalus.flat.FlatInstantces.SIRTypeHashConsedFlat
import scalus.flat.FlatInstantces.SIRHashConsedFlat

import scala.quoted.*
import scalus.sir.SIRType.{unify, TypeVar}
import scalus.utils.*
import scalus.flat.*

class ToExprHS[T](
    hst: HashConsedFlat[T],
    hste: Quotes ?=> Expr[HashConsedFlat[T]],
    tt: Quotes ?=> Type[T]
) extends ToExpr[T] {

    def apply(x: T)(using Quotes): Expr[T] = {
        given Type[T] = tt
        val bitSize = hst.bitSize(x)
        val encoderState = HashConsedEncoderState.withSize(bitSize)
        hst.encodeHC(x, encoderState)
        val bytes = encoderState.encode.result
        val b64 = java.util.Base64.getEncoder.encodeToString(bytes)
        '{
            $hste.decodeHC(
              HashConsedDecoderState(
                DecoderState(java.util.Base64.getDecoder.decode(${ Expr(b64) })),
                HashConsed.State.empty
              )
            )
        }
    }

}

/** Note, that this hashconsedflat should not be resolved as given. It is used only as parameter to
  * ToExprHS. Given HashConsedFlat[(something other then Module)] is a no-no.
  */
object ToExpHSSIRTypeFlat extends HashConsedFlat[SIRType] {

    override def bitSizeHC(a: SIRType, encoderState: HashConsed.State): Int = {
        SIRTypeHashConsedFlat.bitSizeHC(a, encoderState)
    }

    override def encodeHC(a: SIRType, encoderState: HashConsedEncoderState): Unit = {
        SIRTypeHashConsedFlat.encodeHC(a, encoderState)
        encoderState.encode.filler()

        // not check that it is decoded correctly
        if (false) then
            val decoderState = HashConsedDecoderState(
              DecoderState(encoderState.encode.buffer),
              HashConsed.State.empty
            )
            val ref = SIRTypeHashConsedFlat.decodeHC(decoderState)
            val sirType1 = ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
            decoderState.runFinCallbacks()
            val unifyResult = unify(a, sirType1, Map.empty)
            if (!unifyResult.isDefined) then
                println("unification for encoding/decoding failed")
                println(s"original: ${a.show}")
                println(s"decoded: ${sirType1.show}")
                println(s"unification result: $unifyResult")
                //
                //
                throw new IllegalStateException("unification for encoding/decoding failed")

    }

    override def decodeHC(decoderState: HashConsedDecoderState): SIRType = {
        val ref = SIRTypeHashConsedFlat.decodeHC(decoderState)
        decoderState.runFinCallbacks()
        ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
    }

}

object SIRTypeToExpr
    extends ToExprHS[SIRType](ToExpHSSIRTypeFlat, '{ ToExpHSSIRTypeFlat }, summon[Type[SIRType]])
given ToExprHS[SIRType] = SIRTypeToExpr

/** Called from SIRConverter via reflection.
  */
object ToExprHSSIRFlat extends HashConsedFlat[SIR] {

    override def bitSizeHC(a: SIR, encoderState: HashConsed.State): Int = {
        SIRHashConsedFlat.bitSizeHC(a, encoderState)
    }

    override def encodeHC(a: SIR, encoderState: HashConsedEncoderState): Unit = {
        SIRHashConsedFlat.encodeHC(a, encoderState)
        encoderState.encode.filler()
    }

    override def decodeHC(decoderState: HashConsedDecoderState): SIR = {
        val ref = SIRHashConsedFlat.decodeHC(decoderState)
        ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
    }

    def decodeBase64(base64: String): SIR = {
        val bytes = java.util.Base64.getDecoder.decode(base64)
        val decoderState = HashConsedDecoderState(DecoderState(bytes), HashConsed.State.empty)
        decodeHC(decoderState)
    }

}

object SIRToExpr extends ToExprHS[SIR](ToExprHSSIRFlat, '{ ToExprHSSIRFlat }, summon[Type[SIR]])
