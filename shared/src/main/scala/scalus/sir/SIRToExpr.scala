package scalus.sir

import scalus.flat.FlatInstantces.SIRTypeHashConsedFlat

import scala.quoted.*
import scalus.sir.SIRType.{TypeVar, unify}
import scalus.utils.*
import scalus.flat.*


class ToExprHS[T](hst: HashConsedFlat[T], hste: Quotes ?=> Expr[HashConsedFlat[T]], tt: Quotes ?=> Type[T]) extends ToExpr[T] {
    
    def apply(x: T)(using Quotes): Expr[T] = {
        given Type[T] = tt
        val bitSize = hst.bitSize(x)
        val encoderState = HashConsedEncoderState.withSize(bitSize)
        hst.encodeHC(x, encoderState)
        val bytes = encoderState.encode.result
        val b64 = java.util.Base64.getEncoder.encodeToString(bytes)
        '{
            $hste.decodeHC(
                HashConsedDecoderState(DecoderState(java.util.Base64.getDecoder.decode(${ Expr(b64) })),
                                       HashConsed.State.empty))
        }
    }

}

/**
 * Note, that this hashconsedflat should not be resolved as given. It is used only as parameter to ToExprHS.
 * Given HashConsedFlat[(something other then Module)] is a no-no.
 */
object ToExpHSSIRTypeFlat extends HashConsedFlat[SIRType] {

    override def bitSizeHC(a: SIRType, encoderState: HashConsed.State): Int = {
        SIRTypeHashConsedFlat.bitSizeHC(a, encoderState)
    }

    override def encodeHC(a: SIRType, encoderState: HashConsedEncoderState): Unit = {
        println(s"Encoding SIRType: ${a.show}")
        SIRTypeHashConsedFlat.encodeHC(a, encoderState)
        encoderState.encode.filler()

        // not check that it is decoded correctly
        val decoderState = HashConsedDecoderState(DecoderState(encoderState.encode.buffer), HashConsed.State.empty)
        val ref = SIRTypeHashConsedFlat.decodeHC(decoderState)
        val sirType1 = ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
        decoderState.runFinCallbacks()
        val unifyResult = unify(a, sirType1, Map.empty)
        if (! unifyResult.isDefined ) {
            println("unification for encoding/decoding failed")
            println(s"original: ${a.show}")
            println(s"decoded: ${sirType1.show}")
            println(s"unification result: $unifyResult")

            //
            val encoderState = HashConsedEncoderState.withSize(1000)
            
            
            //
            throw new IllegalStateException("unification for encoding/decoding failed")
        }
    }

    override def decodeHC(decoderState: HashConsedDecoderState): SIRType = {
        println("Decoding SIRType")
        val ref = SIRTypeHashConsedFlat.decodeHC(decoderState)
        decoderState.runFinCallbacks()
        ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
    }

}

object SIRTypeToExpr extends ToExprHS[SIRType](ToExpHSSIRTypeFlat, '{ ToExpHSSIRTypeFlat }, summon[Type[SIRType]] )
given ToExprHS[SIRType] = SIRTypeToExpr

