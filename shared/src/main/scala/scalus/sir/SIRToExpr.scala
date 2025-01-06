package scalus.sir

import scalus.flat.FlatInstantces.SIRTypeHashConsedFlat
import scalus.flat.FlatInstantces.SIRHashConsedFlat

import scala.quoted.*
import scalus.sir.SIRType.checkAllProxiesFilled
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
object ToExprHSSIRTypeFlat extends HashConsedFlat[SIRType] {

    val paranoid = true

    override def bitSizeHC(a: SIRType, encoderState: HashConsed.State): Int = {
        if (paranoid) then
            if (!SIRType.checkAllProxiesFilled(a)) then
                throw new IllegalStateException("proxy not filled in $a")
        SIRTypeHashConsedFlat.bitSizeHC(a, encoderState)
    }

    override def encodeHC(a: SIRType, encoderState: HashConsedEncoderState): Unit = {
        SIRTypeHashConsedFlat.encodeHC(a, encoderState)
        encoderState.encode.filler()

        // not check that it is decoded correctly
        if (paranoid) then
            val decoderState = HashConsedDecoderState(
              DecoderState(encoderState.encode.buffer),
              HashConsed.State.empty
            )
            val ref = SIRTypeHashConsedFlat.decodeHC(decoderState)
            val sirType1 = ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
            decoderState.runFinCallbacks()
            val unifyResult = SIRUnify.unifyType(a, sirType1, SIRUnify.Env.empty)
            if (!unifyResult.isSuccess) then
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
        val retval = ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
        if (paranoid) then
            if (!checkAllProxiesFilled(retval)) then
                throw new IllegalStateException("proxy not filled in $retval")
        retval
    }

}

object SIRTypeToExpr
    extends ToExprHS[SIRType](ToExprHSSIRTypeFlat, '{ ToExprHSSIRTypeFlat }, summon[Type[SIRType]])
given ToExprHS[SIRType] = SIRTypeToExpr

/** Called from SIRConverter via reflection.
  */
object ToExprHSSIRFlat extends HashConsedFlat[SIR] {

    val paranoid = true

    override def bitSizeHC(a: SIR, encoderState: HashConsed.State): Int = {
        SIRHashConsedFlat.bitSizeHC(a, encoderState)
    }

    override def encodeHC(a: SIR, encoderState: HashConsedEncoderState): Unit = {
        if paranoid then {
            SIRChecker.checkAndThrow(a)
        }
        SIRHashConsedFlat.encodeHC(a, encoderState)
        encoderState.encode.filler()
        // if (paranoid) {
        //    val decoderState = HashConsedDecoderState(
        //      DecoderState(encoderState.encode.buffer),
        //      HashConsed.State.empty
        //    )
        //    val ref = SIRHashConsedFlat.decodeHC(decoderState)
        //    val sir1 = ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
        //    decoderState.runFinCallbacks()
        //    if (!a.~=~(sir1)) {
        //        println("unification for encoding/decoding failed")
        //        println(s"original: ${a}")
        //        println(s"decoded: ${sir1}")
        //        throw new IllegalStateException("unification for encoding/decoding failed")
        //    }
        // }
    }

    override def decodeHC(decoderState: HashConsedDecoderState): SIR = {
        val ref = SIRHashConsedFlat.decodeHC(decoderState)
        decoderState.runFinCallbacks()
        val retval = ref.finValue(decoderState.hashConsed, 0, new HSRIdentityHashMap)
        if paranoid then {
            SIRChecker.checkAndThrow(retval)
        }
        retval
    }

    def decodeBase64(base64: String): SIR = {
        val bytes = java.util.Base64.getDecoder.decode(base64)
        val decoderState = HashConsedDecoderState(DecoderState(bytes), HashConsed.State.empty)
        decodeHC(decoderState)
    }

}

object SIRToExpr extends ToExprHS[SIR](ToExprHSSIRFlat, '{ ToExprHSSIRFlat }, summon[Type[SIR]])
