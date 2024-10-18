package scalus.sir

import scalus.flat.FlatInstantces.SIRTypeHashConsedFlat

import scala.quoted.*
import scalus.sir.SIRType.TypeVar
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
        SIRTypeHashConsedFlat.encodeHC(a, encoderState)
    }

    override def decodeHC(decoderState: HashConsedDecoderState): SIRType = {
        val ref = SIRTypeHashConsedFlat.decodeHC(decoderState)
        ref.finValue(decoderState.hashConsed)
    }

}

object SIRTypeToExpr extends ToExprHS[SIRType](ToExpHSSIRTypeFlat, '{ ToExpHSSIRTypeFlat }, summon[Type[SIRType]] )
given ToExprHS[SIRType] = SIRTypeToExpr

