package scalus
import scalus.sir.SIR
import scalus.uplc.DefaultFun

object BuiltinHelper {
  def builtinFun(tpe: String): Option[SIR.Builtin] = {
    val DefaultFunValues = Map(
      "scalus.builtins.Builtins.mkConstr" -> DefaultFun.ConstrData,
      "scalus.builtins.Builtins.mkList" -> DefaultFun.ListData,
      "scalus.builtins.Builtins.mkMap" -> DefaultFun.MapData,
      "scalus.builtins.Builtins.mkB" -> DefaultFun.BData,
      "scalus.builtins.Builtins.mkI" -> DefaultFun.IData,
      "scalus.builtins.Builtins.unsafeDataAsConstr" -> DefaultFun.UnConstrData,
      "scalus.builtins.Builtins.unsafeDataAsList" -> DefaultFun.UnListData,
      "scalus.builtins.Builtins.unsafeDataAsMap" -> DefaultFun.UnMapData,
      "scalus.builtins.Builtins.unsafeDataAsB" -> DefaultFun.UnBData,
      "scalus.builtins.Builtins.unsafeDataAsI" -> DefaultFun.UnIData,
      "scalus.builtins.Builtins.sha2_256" -> DefaultFun.Sha2_256,
      "scalus.builtins.Builtins.trace" -> DefaultFun.Trace,
      "scalus.builtins.Builtins.indexByteString" -> DefaultFun.IndexByteString,
      "scalus.builtins.Builtins.consByteString" -> DefaultFun.ConsByteString,
      "scalus.builtins.Builtins.lengthOfByteString" -> DefaultFun.LengthOfByteString,
      "scalus.builtins.Builtins.lessThanInteger" -> DefaultFun.LessThanInteger,
      "scalus.builtins.Builtins.decodeUtf8" -> DefaultFun.DecodeUtf8,
      "scalus.builtins.Builtins.equalsInteger" -> DefaultFun.EqualsInteger,
      "scalus.builtins.Builtins.equalsByteString" -> DefaultFun.EqualsByteString,
      "scalus.builtins.Builtins.equalsString" -> DefaultFun.EqualsString,
      "scalus.builtins.Builtins.equalsData" -> DefaultFun.EqualsData
    )
    DefaultFunValues.get(tpe).map(SIR.Builtin.apply)
  }
}
