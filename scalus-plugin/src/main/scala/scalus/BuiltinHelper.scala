package scalus
import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*
import scalus.sir.SIR
import scalus.uplc.DefaultFun

class BuiltinHelper(using Context) {
    val BuiltinsClass = requiredModule("scalus.builtin.Builtins")
    val DefaultFunValues: Map[Symbol, DefaultFun] = Map(
      "addInteger" -> DefaultFun.AddInteger,
      "subtractInteger" -> DefaultFun.SubtractInteger,
      "multiplyInteger" -> DefaultFun.MultiplyInteger,
      "divideInteger" -> DefaultFun.DivideInteger,
      "quotientInteger" -> DefaultFun.QuotientInteger,
      "remainderInteger" -> DefaultFun.RemainderInteger,
      "modInteger" -> DefaultFun.ModInteger,
      "equalsInteger" -> DefaultFun.EqualsInteger,
      "lessThanInteger" -> DefaultFun.LessThanInteger,
      "lessThanEqualsInteger" -> DefaultFun.LessThanEqualsInteger,
      "appendByteString" -> DefaultFun.AppendByteString,
      "consByteString" -> DefaultFun.ConsByteString,
      "sliceByteString" -> DefaultFun.SliceByteString,
      "lengthOfByteString" -> DefaultFun.LengthOfByteString,
      "indexByteString" -> DefaultFun.IndexByteString,
      "equalsByteString" -> DefaultFun.EqualsByteString,
      "lessThanByteString" -> DefaultFun.LessThanByteString,
      "lessThanEqualsByteString" -> DefaultFun.LessThanEqualsByteString,
      "sha2_256" -> DefaultFun.Sha2_256,
      "sha3_256" -> DefaultFun.Sha3_256,
      "blake2b_256" -> DefaultFun.Blake2b_256,
      "verifyEd25519Signature" -> DefaultFun.VerifyEd25519Signature,
      "verifyEcdsaSecp256k1Signature" -> DefaultFun.VerifyEcdsaSecp256k1Signature,
      "verifySchnorrSecp256k1Signature" -> DefaultFun.VerifySchnorrSecp256k1Signature,
      "appendString" -> DefaultFun.AppendString,
      "equalsString" -> DefaultFun.EqualsString,
      "encodeUtf8" -> DefaultFun.EncodeUtf8,
      "decodeUtf8" -> DefaultFun.DecodeUtf8,
      "ifThenElse" -> DefaultFun.IfThenElse,
      "chooseUnit" -> DefaultFun.ChooseUnit,
      "trace" -> DefaultFun.Trace,
      "fstPair" -> DefaultFun.FstPair,
      "sndPair" -> DefaultFun.SndPair,
      "chooseList" -> DefaultFun.ChooseList,
      "mkCons" -> DefaultFun.MkCons,
      "headList" -> DefaultFun.HeadList,
      "tailList" -> DefaultFun.TailList,
      "nullList" -> DefaultFun.NullList,
      "chooseData" -> DefaultFun.ChooseData,
      "mkConstr" -> DefaultFun.ConstrData,
      "mkMap" -> DefaultFun.MapData,
      "mkList" -> DefaultFun.ListData,
      "mkI" -> DefaultFun.IData,
      "mkB" -> DefaultFun.BData,
      "unsafeDataAsConstr" -> DefaultFun.UnConstrData,
      "unsafeDataAsMap" -> DefaultFun.UnMapData,
      "unsafeDataAsList" -> DefaultFun.UnListData,
      "unsafeDataAsI" -> DefaultFun.UnIData,
      "unsafeDataAsB" -> DefaultFun.UnBData,
      "equalsData" -> DefaultFun.EqualsData,
      "serialiseData" -> DefaultFun.SerialiseData,
      "mkPairData" -> DefaultFun.MkPairData,
      "mkNilData" -> DefaultFun.MkNilData,
      "mkNilPairData" -> DefaultFun.MkNilPairData
    ).map { case (k, v) => BuiltinsClass.requiredMethod(k) -> v }

    def builtinFun(s: Symbol): Option[SIR.Builtin] = {
        DefaultFunValues.get(s).map(SIR.Builtin.apply)
    }
}
