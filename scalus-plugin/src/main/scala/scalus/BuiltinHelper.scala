package scalus

import scala.util.control.NonFatal
import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import scalus.sir.SIR
import scalus.sir.SIRBuiltins


class BuiltinHelper(using Context) {
    val BuiltinsClass = requiredModule("scalus.builtin.Builtins")

    val DefaultFunSIRBuildins: Map[String, SIR.Builtin] = Map(
      "addInteger" -> {
          try
              SIRBuiltins.addInteger
          catch
              case NonFatal(ex) =>
                  println(s"NonFatal Error in addInteger:  ${ex.getMessage}")
                  ex.printStackTrace()
                  throw ex
              case ex: Throwable =>
                  println(s"Fatal Error in addInteger:  ${ex.getMessage}")
                  ex.printStackTrace()
                  throw ex
      },
      "subtractInteger" -> SIRBuiltins.subtractInteger,
      "multiplyInteger" -> SIRBuiltins.multiplyInteger,
      "divideInteger" -> SIRBuiltins.divideInteger,
      "quotientInteger" -> SIRBuiltins.quotientInteger,
      "remainderInteger" -> SIRBuiltins.remainderInteger,
      "modInteger" -> SIRBuiltins.modInteger,
      "equalsInteger" -> SIRBuiltins.equalsInteger,
      "lessThanInteger" -> SIRBuiltins.lessThanInteger,
      "lessThanEqualsInteger" -> SIRBuiltins.lessThanEqualsInteger,
      "appendByteString" -> SIRBuiltins.appendByteString,
      "consByteString" -> SIRBuiltins.consByteString,
      "sliceByteString" -> SIRBuiltins.sliceByteString,
      "lengthOfByteString" -> SIRBuiltins.lengthOfByteString,
      "indexByteString" -> SIRBuiltins.indexByteString,
      "equalsByteString" -> SIRBuiltins.equalsByteString,
      "lessThanByteString" -> SIRBuiltins.lessThanByteString,
      "lessThanEqualsByteString" -> SIRBuiltins.lessThanEqualsByteString,
      "sha2_256" -> SIRBuiltins.sha2_256,
      "sha3_256" -> SIRBuiltins.sha3_256,
      "blake2b_256" -> SIRBuiltins.blake2b_256,
      "verifyEd25519Signature" -> SIRBuiltins.verifyEd25519Signature,
      "verifyEcdsaSecp256k1Signature" -> SIRBuiltins.verifyEcdsaSecp256k1Signature,
      "verifySchnorrSecp256k1Signature" -> SIRBuiltins.verifySchnorrSecp256k1Signature,
      "appendString" -> SIRBuiltins.appendString,
      "equalsString" -> SIRBuiltins.equalsString,
      "encodeUtf8" -> SIRBuiltins.encodeUtf8,
      "decodeUtf8" -> SIRBuiltins.decodeUtf8,
      "ifThenElse" -> SIRBuiltins.ifThenElse,
      "chooseUnit" -> SIRBuiltins.chooseUnit,
      "trace" -> SIRBuiltins.trace,
      "fstPair" -> SIRBuiltins.fstPair,
      "sndPair" -> SIRBuiltins.sndPair,
      "chooseList" -> SIRBuiltins.chooseList,
      "mkCons" -> SIRBuiltins.mkCons,
      "headList" -> SIRBuiltins.headList,
      "tailList" -> SIRBuiltins.tailList,
      "nullList" -> SIRBuiltins.nullList,
      "chooseData" -> SIRBuiltins.chooseData,
      // TODO remove in 0.7
      "mkConstr" -> SIRBuiltins.constrData,
      "mkMap" -> SIRBuiltins.mapData,
      "mkList" -> SIRBuiltins.listData,
      "mkI" -> SIRBuiltins.iData,
      "mkB" -> SIRBuiltins.bData,
      "unsafeDataAsConstr" -> SIRBuiltins.unConstrData,
      "unsafeDataAsMap" -> SIRBuiltins.unMapData,
      "unsafeDataAsList" -> SIRBuiltins.unListData,
      "unsafeDataAsI" -> SIRBuiltins.unIData,
      "unsafeDataAsB" -> SIRBuiltins.unBData,
      // TODO end of remove
      "constrData" -> SIRBuiltins.constrData,
      "mapData" -> SIRBuiltins.mapData,
      "listData" -> SIRBuiltins.listData,
      "iData" -> SIRBuiltins.iData,
      "bData" -> SIRBuiltins.bData,
      "unConstrData" -> SIRBuiltins.unConstrData,
      "unMapData" -> SIRBuiltins.unMapData,
      "unListData" -> SIRBuiltins.unListData,
      "unIData" -> SIRBuiltins.unIData,
      "unBData" -> SIRBuiltins.unBData,
      "equalsData" -> SIRBuiltins.equalsData,
      "serialiseData" -> SIRBuiltins.serialiseData,
      "mkPairData" -> SIRBuiltins.mkPairData,
      "mkNilData" -> SIRBuiltins.mkNilData,
      "mkNilPairData" -> SIRBuiltins.mkNilPairData
    )

    //val DefaultFunValues: Map[Symbol, DefaultFun] = DefaultFun.values
    //    .map(v => lowerFirst(v.toString) -> v)
    //    .toMap
    //    .map { (k, v) => BuiltinsClass.requiredMethod(k) -> v }

    def builtinFun(s: Symbol)(using Context): Option[SIR.Builtin] = {
        DefaultFunSIRBuildins.get(s.name.toSimpleName.debugString)
    }

    private def lowerFirst(s: String): String =
        if s == null || s.isEmpty || !s.charAt(0).isUpper then s
        else s.updated(0, s.charAt(0).toLower)
}
