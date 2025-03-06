package scalus.uplc.eval

import scalus.ledger.api.BuiltinSemanticsVariant
import scalus.ledger.api.PlutusLedgerLanguage
import scalus.macros.Macros
import upickle.default.*

case class BuiltinCostModel(
    // Integers
    addInteger: DefaultCostingFun[TwoArguments],
    subtractInteger: DefaultCostingFun[TwoArguments],
    multiplyInteger: DefaultCostingFun[TwoArguments],
    divideInteger: DefaultCostingFun[TwoArguments],
    quotientInteger: DefaultCostingFun[TwoArguments],
    remainderInteger: DefaultCostingFun[TwoArguments],
    modInteger: DefaultCostingFun[TwoArguments],
    equalsInteger: DefaultCostingFun[TwoArguments],
    lessThanInteger: DefaultCostingFun[TwoArguments],
    lessThanEqualsInteger: DefaultCostingFun[TwoArguments],
    // Bytestrings
    appendByteString: DefaultCostingFun[TwoArguments],
    consByteString: DefaultCostingFun[TwoArguments],
    sliceByteString: DefaultCostingFun[ThreeArguments],
    lengthOfByteString: DefaultCostingFun[OneArgument],
    indexByteString: DefaultCostingFun[TwoArguments],
    equalsByteString: DefaultCostingFun[TwoArguments],
    lessThanByteString: DefaultCostingFun[TwoArguments],
    lessThanEqualsByteString: DefaultCostingFun[TwoArguments],
    // // Cryptography and hashes
    sha2_256: DefaultCostingFun[OneArgument],
    sha3_256: DefaultCostingFun[OneArgument],
    blake2b_256: DefaultCostingFun[OneArgument],
    verifyEd25519Signature: DefaultCostingFun[ThreeArguments],
    verifyEcdsaSecp256k1Signature: DefaultCostingFun[ThreeArguments],
    verifySchnorrSecp256k1Signature: DefaultCostingFun[ThreeArguments],
    // Strings
    appendString: DefaultCostingFun[TwoArguments],
    equalsString: DefaultCostingFun[TwoArguments],
    encodeUtf8: DefaultCostingFun[OneArgument],
    decodeUtf8: DefaultCostingFun[OneArgument],
    // Bool
    ifThenElse: DefaultCostingFun[ThreeArguments],
    // Unit
    chooseUnit: DefaultCostingFun[TwoArguments],
    // Tracing
    trace: DefaultCostingFun[TwoArguments],
    // Pairs
    fstPair: DefaultCostingFun[OneArgument],
    sndPair: DefaultCostingFun[OneArgument],
    // Lists
    chooseList: DefaultCostingFun[ThreeArguments],
    mkCons: DefaultCostingFun[TwoArguments],
    headList: DefaultCostingFun[OneArgument],
    tailList: DefaultCostingFun[OneArgument],
    nullList: DefaultCostingFun[OneArgument],
    // Data
    chooseData: DefaultCostingFun[SixArguments],
    constrData: DefaultCostingFun[TwoArguments],
    mapData: DefaultCostingFun[OneArgument],
    listData: DefaultCostingFun[OneArgument],
    iData: DefaultCostingFun[OneArgument],
    bData: DefaultCostingFun[OneArgument],
    unConstrData: DefaultCostingFun[OneArgument],
    unMapData: DefaultCostingFun[OneArgument],
    unListData: DefaultCostingFun[OneArgument],
    unIData: DefaultCostingFun[OneArgument],
    unBData: DefaultCostingFun[OneArgument],
    equalsData: DefaultCostingFun[TwoArguments],
    // Misc constructors
    mkPairData: DefaultCostingFun[TwoArguments],
    mkNilData: DefaultCostingFun[OneArgument],
    mkNilPairData: DefaultCostingFun[OneArgument],
    serialiseData: DefaultCostingFun[OneArgument],
    blake2b_224: DefaultCostingFun[OneArgument],
    keccak_256: DefaultCostingFun[OneArgument],
    // BLS
    bls12_381_G1_add: DefaultCostingFun[TwoArguments],
    bls12_381_G1_neg: DefaultCostingFun[OneArgument],
    bls12_381_G1_scalarMul: DefaultCostingFun[TwoArguments],
    bls12_381_G1_equal: DefaultCostingFun[TwoArguments],
    bls12_381_G1_compress: DefaultCostingFun[OneArgument],
    bls12_381_G1_uncompress: DefaultCostingFun[OneArgument],
    bls12_381_G1_hashToGroup: DefaultCostingFun[TwoArguments],
    bls12_381_G2_add: DefaultCostingFun[TwoArguments],
    bls12_381_G2_neg: DefaultCostingFun[OneArgument],
    bls12_381_G2_scalarMul: DefaultCostingFun[TwoArguments],
    bls12_381_G2_equal: DefaultCostingFun[TwoArguments],
    bls12_381_G2_compress: DefaultCostingFun[OneArgument],
    bls12_381_G2_uncompress: DefaultCostingFun[OneArgument],
    bls12_381_G2_hashToGroup: DefaultCostingFun[TwoArguments],
    bls12_381_millerLoop: DefaultCostingFun[TwoArguments],
    bls12_381_mulMlResult: DefaultCostingFun[TwoArguments],
    bls12_381_finalVerify: DefaultCostingFun[TwoArguments],
    integerToByteString: IntegerToByteStringCostingFun,
    byteStringToInteger: DefaultCostingFun[TwoArguments],
    andByteString: DefaultCostingFun[ThreeArguments],
    orByteString: DefaultCostingFun[ThreeArguments],
    xorByteString: DefaultCostingFun[ThreeArguments],
    complementByteString: DefaultCostingFun[OneArgument]
) {

    /** Convert a [[BuiltinCostModel]] to a flat map of cost parameters
      *
      * @return
      *   a flat map of cost parameters, like Map("addInteger-cpu-arguments-intercept" -> 205665,
      *   ...)
      */
    def flattenCostModel: Map[String, Long] = {
        val obj = writeJs(this).obj
        // recursively flatten the object
        def flatten(obj: ujson.Obj, prefix: Option[String] = None): Map[String, Long] = {
            obj.value.flatMap {
                case (k, v: ujson.Obj) =>
                    prefix match
                        case Some(p) => flatten(v, Some(s"$p-$k"))
                        case None    => flatten(v, Some(k))
                case (k, v: ujson.Num) =>
                    prefix match
                        case Some(p) => Map(s"$p-$k" -> v.num.toLong)
                        case None    => Map(s"$k" -> v.num.toLong)
                case (k, v) => Map.empty
            }.toMap
        }
        flatten(obj)
    }

    def toJsonString: String = write(this)
}

object BuiltinCostModel {

    private inline def inlineBuiltinCostModelJson(name: String) = ${
        Macros.inlineBuiltinCostModelJsonImpl('name)
    }

    val defaultCostModelA: BuiltinCostModel =
        BuiltinCostModel.fromJsonString(inlineBuiltinCostModelJson("builtinCostModelA.json"))

    val defaultCostModelB: BuiltinCostModel =
        BuiltinCostModel.fromJsonString(inlineBuiltinCostModelJson("builtinCostModelB.json"))

    val defaultCostModelC: BuiltinCostModel =
        BuiltinCostModel.fromJsonString(inlineBuiltinCostModelJson("builtinCostModelC.json"))

    @deprecated("Use defaultCostModelA, defaultCostModelB or defaultCostModelC instead", "0.1.0")
    val defaultCostModel: BuiltinCostModel = defaultCostModelC

    given ReadWriter[BuiltinCostModel] = readwriter[ujson.Value].bimap(
      model =>
          ujson.Obj(
            "addInteger" -> writeJs(model.addInteger),
            "subtractInteger" -> writeJs(model.subtractInteger),
            "multiplyInteger" -> writeJs(model.multiplyInteger),
            "divideInteger" -> writeJs(model.divideInteger),
            "quotientInteger" -> writeJs(model.quotientInteger),
            "remainderInteger" -> writeJs(model.remainderInteger),
            "modInteger" -> writeJs(model.modInteger),
            "equalsInteger" -> writeJs(model.equalsInteger),
            "lessThanInteger" -> writeJs(model.lessThanInteger),
            "lessThanEqualsInteger" -> writeJs(model.lessThanEqualsInteger),
            "appendByteString" -> writeJs(model.appendByteString),
            "consByteString" -> writeJs(model.consByteString),
            "sliceByteString" -> writeJs(model.sliceByteString),
            "lengthOfByteString" -> writeJs(model.lengthOfByteString),
            "indexByteString" -> writeJs(model.indexByteString),
            "equalsByteString" -> writeJs(model.equalsByteString),
            "lessThanByteString" -> writeJs(model.lessThanByteString),
            "lessThanEqualsByteString" -> writeJs(model.lessThanEqualsByteString),
            "sha2_256" -> writeJs(model.sha2_256),
            "sha3_256" -> writeJs(model.sha3_256),
            "blake2b_256" -> writeJs(model.blake2b_256),
            "verifyEd25519Signature" -> writeJs(model.verifyEd25519Signature),
            "verifyEcdsaSecp256k1Signature" -> writeJs(model.verifyEcdsaSecp256k1Signature),
            "verifySchnorrSecp256k1Signature" -> writeJs(model.verifySchnorrSecp256k1Signature),
            "appendString" -> writeJs(model.appendString),
            "equalsString" -> writeJs(model.equalsString),
            "encodeUtf8" -> writeJs(model.encodeUtf8),
            "decodeUtf8" -> writeJs(model.decodeUtf8),
            "ifThenElse" -> writeJs(model.ifThenElse),
            "chooseUnit" -> writeJs(model.chooseUnit),
            "trace" -> writeJs(model.trace),
            "fstPair" -> writeJs(model.fstPair),
            "sndPair" -> writeJs(model.sndPair),
            "chooseList" -> writeJs(model.chooseList),
            "mkCons" -> writeJs(model.mkCons),
            "headList" -> writeJs(model.headList),
            "tailList" -> writeJs(model.tailList),
            "nullList" -> writeJs(model.nullList),
            "chooseData" -> writeJs(model.chooseData),
            "constrData" -> writeJs(model.constrData),
            "mapData" -> writeJs(model.mapData),
            "listData" -> writeJs(model.listData),
            "iData" -> writeJs(model.iData),
            "bData" -> writeJs(model.bData),
            "unConstrData" -> writeJs(model.unConstrData),
            "unMapData" -> writeJs(model.unMapData),
            "unListData" -> writeJs(model.unListData),
            "unIData" -> writeJs(model.unIData),
            "unBData" -> writeJs(model.unBData),
            "equalsData" -> writeJs(model.equalsData),
            "mkPairData" -> writeJs(model.mkPairData),
            "mkNilData" -> writeJs(model.mkNilData),
            "mkNilPairData" -> writeJs(model.mkNilPairData),
            "serialiseData" -> writeJs(model.serialiseData),
            "blake2b_224" -> writeJs(model.blake2b_224),
            "keccak_256" -> writeJs(model.keccak_256),
            "bls12_381_G1_add" -> writeJs(model.bls12_381_G1_add),
            "bls12_381_G1_neg" -> writeJs(model.bls12_381_G1_neg),
            "bls12_381_G1_scalarMul" -> writeJs(model.bls12_381_G1_scalarMul),
            "bls12_381_G1_equal" -> writeJs(model.bls12_381_G1_equal),
            "bls12_381_G1_compress" -> writeJs(model.bls12_381_G1_compress),
            "bls12_381_G1_uncompress" -> writeJs(model.bls12_381_G1_uncompress),
            "bls12_381_G1_hashToGroup" -> writeJs(model.bls12_381_G1_hashToGroup),
            "bls12_381_G2_add" -> writeJs(model.bls12_381_G2_add),
            "bls12_381_G2_neg" -> writeJs(model.bls12_381_G2_neg),
            "bls12_381_G2_scalarMul" -> writeJs(model.bls12_381_G2_scalarMul),
            "bls12_381_G2_equal" -> writeJs(model.bls12_381_G2_equal),
            "bls12_381_G2_compress" -> writeJs(model.bls12_381_G2_compress),
            "bls12_381_G2_uncompress" -> writeJs(model.bls12_381_G2_uncompress),
            "bls12_381_G2_hashToGroup" -> writeJs(model.bls12_381_G2_hashToGroup),
            "bls12_381_millerLoop" -> writeJs(model.bls12_381_millerLoop),
            "bls12_381_mulMlResult" -> writeJs(model.bls12_381_mulMlResult),
            "bls12_381_finalVerify" -> writeJs(model.bls12_381_finalVerify),
            "integerToByteString" -> writeJs(model.integerToByteString),
            "byteStringToInteger" -> writeJs(model.byteStringToInteger),
            "andByteString" -> writeJs(model.andByteString),
            "orByteString" -> writeJs(model.orByteString),
            "xorByteString" -> writeJs(model.xorByteString),
            "complementByteString" -> writeJs(model.complementByteString)
          ),
      json =>
          BuiltinCostModel(
            addInteger = read[DefaultCostingFun[TwoArguments]](json("addInteger")),
            subtractInteger = read[DefaultCostingFun[TwoArguments]](json("subtractInteger")),
            multiplyInteger = read[DefaultCostingFun[TwoArguments]](json("multiplyInteger")),
            divideInteger = read[DefaultCostingFun[TwoArguments]](json("divideInteger")),
            quotientInteger = read[DefaultCostingFun[TwoArguments]](json("quotientInteger")),
            remainderInteger = read[DefaultCostingFun[TwoArguments]](json("remainderInteger")),
            modInteger = read[DefaultCostingFun[TwoArguments]](json("modInteger")),
            equalsInteger = read[DefaultCostingFun[TwoArguments]](json("equalsInteger")),
            lessThanInteger = read[DefaultCostingFun[TwoArguments]](json("lessThanInteger")),
            lessThanEqualsInteger =
                read[DefaultCostingFun[TwoArguments]](json("lessThanEqualsInteger")),
            appendByteString = read[DefaultCostingFun[TwoArguments]](json("appendByteString")),
            consByteString = read[DefaultCostingFun[TwoArguments]](json("consByteString")),
            sliceByteString = read[DefaultCostingFun[ThreeArguments]](json("sliceByteString")),
            lengthOfByteString = read[DefaultCostingFun[OneArgument]](json("lengthOfByteString")),
            indexByteString = read[DefaultCostingFun[TwoArguments]](json("indexByteString")),
            equalsByteString = read[DefaultCostingFun[TwoArguments]](json("equalsByteString")),
            lessThanByteString = read[DefaultCostingFun[TwoArguments]](json("lessThanByteString")),
            lessThanEqualsByteString =
                read[DefaultCostingFun[TwoArguments]](json("lessThanEqualsByteString")),
            sha2_256 = read[DefaultCostingFun[OneArgument]](json("sha2_256")),
            sha3_256 = read[DefaultCostingFun[OneArgument]](json("sha3_256")),
            blake2b_256 = read[DefaultCostingFun[OneArgument]](json("blake2b_256")),
            verifyEd25519Signature =
                read[DefaultCostingFun[ThreeArguments]](json("verifyEd25519Signature")),
            verifyEcdsaSecp256k1Signature =
                read[DefaultCostingFun[ThreeArguments]](json("verifyEcdsaSecp256k1Signature")),
            verifySchnorrSecp256k1Signature =
                read[DefaultCostingFun[ThreeArguments]](json("verifySchnorrSecp256k1Signature")),
            appendString = read[DefaultCostingFun[TwoArguments]](json("appendString")),
            equalsString = read[DefaultCostingFun[TwoArguments]](json("equalsString")),
            encodeUtf8 = read[DefaultCostingFun[OneArgument]](json("encodeUtf8")),
            decodeUtf8 = read[DefaultCostingFun[OneArgument]](json("decodeUtf8")),
            ifThenElse = read[DefaultCostingFun[ThreeArguments]](json("ifThenElse")),
            chooseUnit = read[DefaultCostingFun[TwoArguments]](json("chooseUnit")),
            trace = read[DefaultCostingFun[TwoArguments]](json("trace")),
            fstPair = read[DefaultCostingFun[OneArgument]](json("fstPair")),
            sndPair = read[DefaultCostingFun[OneArgument]](json("sndPair")),
            chooseList = read[DefaultCostingFun[ThreeArguments]](json("chooseList")),
            mkCons = read[DefaultCostingFun[TwoArguments]](json("mkCons")),
            headList = read[DefaultCostingFun[OneArgument]](json("headList")),
            tailList = read[DefaultCostingFun[OneArgument]](json("tailList")),
            nullList = read[DefaultCostingFun[OneArgument]](json("nullList")),
            chooseData = read[DefaultCostingFun[SixArguments]](json("chooseData")),
            constrData = read[DefaultCostingFun[TwoArguments]](json("constrData")),
            mapData = read[DefaultCostingFun[OneArgument]](json("mapData")),
            listData = read[DefaultCostingFun[OneArgument]](json("listData")),
            iData = read[DefaultCostingFun[OneArgument]](json("iData")),
            bData = read[DefaultCostingFun[OneArgument]](json("bData")),
            unConstrData = read[DefaultCostingFun[OneArgument]](json("unConstrData")),
            unMapData = read[DefaultCostingFun[OneArgument]](json("unMapData")),
            unListData = read[DefaultCostingFun[OneArgument]](json("unListData")),
            unIData = read[DefaultCostingFun[OneArgument]](json("unIData")),
            unBData = read[DefaultCostingFun[OneArgument]](json("unBData")),
            equalsData = read[DefaultCostingFun[TwoArguments]](json("equalsData")),
            mkPairData = read[DefaultCostingFun[TwoArguments]](json("mkPairData")),
            mkNilData = read[DefaultCostingFun[OneArgument]](json("mkNilData")),
            mkNilPairData = read[DefaultCostingFun[OneArgument]](json("mkNilPairData")),
            serialiseData = read[DefaultCostingFun[OneArgument]](json("serialiseData")),
            blake2b_224 = read[DefaultCostingFun[OneArgument]](json("blake2b_224")),
            keccak_256 = read[DefaultCostingFun[OneArgument]](json("keccak_256")),
            bls12_381_G1_add = read[DefaultCostingFun[TwoArguments]](json("bls12_381_G1_add")),
            bls12_381_G1_neg = read[DefaultCostingFun[OneArgument]](json("bls12_381_G1_neg")),
            bls12_381_G1_scalarMul =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_G1_scalarMul")),
            bls12_381_G1_equal = read[DefaultCostingFun[TwoArguments]](json("bls12_381_G1_equal")),
            bls12_381_G1_compress =
                read[DefaultCostingFun[OneArgument]](json("bls12_381_G1_compress")),
            bls12_381_G1_uncompress =
                read[DefaultCostingFun[OneArgument]](json("bls12_381_G1_uncompress")),
            bls12_381_G1_hashToGroup =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_G1_hashToGroup")),
            bls12_381_G2_add = read[DefaultCostingFun[TwoArguments]](json("bls12_381_G2_add")),
            bls12_381_G2_neg = read[DefaultCostingFun[OneArgument]](json("bls12_381_G2_neg")),
            bls12_381_G2_scalarMul =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_G2_scalarMul")),
            bls12_381_G2_equal = read[DefaultCostingFun[TwoArguments]](json("bls12_381_G2_equal")),
            bls12_381_G2_compress =
                read[DefaultCostingFun[OneArgument]](json("bls12_381_G2_compress")),
            bls12_381_G2_uncompress =
                read[DefaultCostingFun[OneArgument]](json("bls12_381_G2_uncompress")),
            bls12_381_G2_hashToGroup =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_G2_hashToGroup")),
            bls12_381_millerLoop =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_millerLoop")),
            bls12_381_mulMlResult =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_mulMlResult")),
            bls12_381_finalVerify =
                read[DefaultCostingFun[TwoArguments]](json("bls12_381_finalVerify")),
            integerToByteString = read[IntegerToByteStringCostingFun](json("integerToByteString")),
            byteStringToInteger =
                read[DefaultCostingFun[TwoArguments]](json("byteStringToInteger")),
            andByteString =
                if json.obj.keySet.contains("andByteString") then
                    read[DefaultCostingFun[ThreeArguments]](json("andByteString"))
                else null,
            orByteString =
                if json.obj.keySet.contains("orByteString") then
                    read[DefaultCostingFun[ThreeArguments]](json("orByteString"))
                else null,
            xorByteString =
                if json.obj.keySet.contains("xorByteString") then
                    read[DefaultCostingFun[ThreeArguments]](json("xorByteString"))
                else null,
            complementByteString =
                if json.obj.keySet.contains("complementByteString") then
                    read[DefaultCostingFun[OneArgument]](json("complementByteString"))
                else null
          )
    )

    protected[eval] val defaultValue = 300_000_000L

    def fromCostModelParams(
        plutus: PlutusLedgerLanguage,
        semvar: BuiltinSemanticsVariant,
        costModelParams: Map[String, Long]
    ): BuiltinCostModel =
        val params = costModelParams.withDefaultValue(defaultValue)
        BuiltinCostModel(
          addInteger = DefaultCostingFun(
            cpu = TwoArguments.MaxSize(
              OneVariableLinearFunction(
                intercept = params("addInteger-cpu-arguments-intercept"),
                slope = params("addInteger-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.MaxSize(
              OneVariableLinearFunction(
                intercept = params("addInteger-memory-arguments-intercept"),
                slope = params("addInteger-memory-arguments-slope")
              )
            )
          ),
          subtractInteger = DefaultCostingFun(
            cpu = TwoArguments.MaxSize(
              OneVariableLinearFunction(
                intercept = params("subtractInteger-cpu-arguments-intercept"),
                slope = params("subtractInteger-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.MaxSize(
              OneVariableLinearFunction(
                intercept = params("subtractInteger-memory-arguments-intercept"),
                slope = params("subtractInteger-memory-arguments-slope")
              )
            )
          ),
          multiplyInteger = (plutus, semvar) match
              case (
                    PlutusLedgerLanguage.PlutusV1 | PlutusLedgerLanguage.PlutusV2,
                    BuiltinSemanticsVariant.A
                  ) =>
                  DefaultCostingFun(
                    cpu = TwoArguments.AddedSizes(
                      OneVariableLinearFunction(
                        intercept = params("multiplyInteger-cpu-arguments-intercept"),
                        slope = params("multiplyInteger-cpu-arguments-slope")
                      )
                    ),
                    memory = TwoArguments.AddedSizes(
                      OneVariableLinearFunction(
                        intercept = params("multiplyInteger-memory-arguments-intercept"),
                        slope = params("multiplyInteger-memory-arguments-slope")
                      )
                    )
                  )
              case (
                    PlutusLedgerLanguage.PlutusV1 | PlutusLedgerLanguage.PlutusV2,
                    BuiltinSemanticsVariant.B
                  ) | (PlutusLedgerLanguage.PlutusV3, BuiltinSemanticsVariant.C) =>
                  DefaultCostingFun(
                    cpu = TwoArguments.MultipliedSizes(
                      OneVariableLinearFunction(
                        intercept = params("multiplyInteger-cpu-arguments-intercept"),
                        slope = params("multiplyInteger-cpu-arguments-slope")
                      )
                    ),
                    memory = TwoArguments.AddedSizes(
                      OneVariableLinearFunction(
                        intercept = params("multiplyInteger-memory-arguments-intercept"),
                        slope = params("multiplyInteger-memory-arguments-slope")
                      )
                    )
                  )
              case _ =>
                  throw new IllegalArgumentException(
                    s"Unsupported combination of Plutus version $plutus and semantics variant $semvar for multiplyInteger"
                  )
          ,
          divideInteger = plutus match
              case PlutusLedgerLanguage.PlutusV1 | PlutusLedgerLanguage.PlutusV2 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params("divideInteger-cpu-arguments-constant"),
                        model = TwoArguments.MultipliedSizes(
                          OneVariableLinearFunction(
                            intercept =
                                params("divideInteger-cpu-arguments-model-arguments-intercept"),
                            slope = params("divideInteger-cpu-arguments-model-arguments-slope")
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params("divideInteger-memory-arguments-intercept"),
                        slope = params("divideInteger-memory-arguments-slope"),
                        minimum = params("divideInteger-memory-arguments-minimum")
                      )
                    )
                  )
              case PlutusLedgerLanguage.PlutusV3 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params("divideInteger-cpu-arguments-constant"),
                        model = TwoArguments.QuadraticInXAndY(
                          TwoVariableQuadraticFunction(
                            minimum = params("divideInteger-cpu-arguments-minimum"),
                            c00 = params("divideInteger-cpu-arguments-c00"),
                            c10 = params("divideInteger-cpu-arguments-c10"),
                            c01 = params("divideInteger-cpu-arguments-c01"),
                            c20 = params("divideInteger-cpu-arguments-c20"),
                            c11 = params("divideInteger-cpu-arguments-c11"),
                            c02 = params("divideInteger-cpu-arguments-c02")
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params("divideInteger-memory-arguments-intercept"),
                        slope = params("divideInteger-memory-arguments-slope"),
                        minimum = params("divideInteger-memory-arguments-minimum")
                      )
                    )
                  )
          ,
          quotientInteger = plutus match
              case PlutusLedgerLanguage.PlutusV1 | PlutusLedgerLanguage.PlutusV2 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params("quotientInteger-cpu-arguments-constant"),
                        model = TwoArguments.MultipliedSizes(
                          OneVariableLinearFunction(
                            intercept =
                                params("quotientInteger-cpu-arguments-model-arguments-intercept"),
                            slope = params("quotientInteger-cpu-arguments-model-arguments-slope")
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params("quotientInteger-memory-arguments-intercept"),
                        slope = params("quotientInteger-memory-arguments-slope"),
                        minimum = params("quotientInteger-memory-arguments-minimum")
                      )
                    )
                  )
              case PlutusLedgerLanguage.PlutusV3 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params("quotientInteger-cpu-arguments-constant"),
                        model = TwoArguments.QuadraticInXAndY(
                          TwoVariableQuadraticFunction(
                            minimum =
                                params("quotientInteger-cpu-arguments-model-arguments-minimum"),
                            c00 = params("quotientInteger-cpu-arguments-model-arguments-c00"),
                            c10 = params("quotientInteger-cpu-arguments-model-arguments-c10"),
                            c01 = params("quotientInteger-cpu-arguments-model-arguments-c01"),
                            c20 = params("quotientInteger-cpu-arguments-model-arguments-c20"),
                            c11 = params("quotientInteger-cpu-arguments-model-arguments-c11"),
                            c02 = params("quotientInteger-cpu-arguments-model-arguments-c02")
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params("quotientInteger-memory-arguments-intercept"),
                        slope = params("quotientInteger-memory-arguments-slope"),
                        minimum = params("quotientInteger-memory-arguments-minimum")
                      )
                    )
                  )
          ,
          remainderInteger = plutus match
              case PlutusLedgerLanguage.PlutusV1 | PlutusLedgerLanguage.PlutusV2 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params("remainderInteger-cpu-arguments-constant"),
                        model = TwoArguments.MultipliedSizes(
                          OneVariableLinearFunction(
                            intercept =
                                params("remainderInteger-cpu-arguments-model-arguments-intercept"),
                            slope = params("remainderInteger-cpu-arguments-model-arguments-slope")
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params("remainderInteger-memory-arguments-intercept"),
                        slope = params("remainderInteger-memory-arguments-slope"),
                        minimum = params("remainderInteger-memory-arguments-minimum")
                      )
                    )
                  )
              case PlutusLedgerLanguage.PlutusV3 =>
                  // same as modInteger
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params("remainderInteger-cpu-arguments-constant"),
                        model = TwoArguments.QuadraticInXAndY(
                          TwoVariableQuadraticFunction(
                            minimum =
                                params("remainderInteger-cpu-arguments-model-arguments-minimum"),
                            c00 = params("remainderInteger-cpu-arguments-model-arguments-c00"),
                            c10 = params("remainderInteger-cpu-arguments-model-arguments-c10"),
                            c01 = params("remainderInteger-cpu-arguments-model-arguments-c01"),
                            c20 = params("remainderInteger-cpu-arguments-model-arguments-c20"),
                            c11 = params("remainderInteger-cpu-arguments-model-arguments-c11"),
                            c02 = params("remainderInteger-cpu-arguments-model-arguments-c02")
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.LinearInY(
                      OneVariableLinearFunction(
                        intercept = params("remainderInteger-memory-arguments-intercept"),
                        slope = params("remainderInteger-memory-arguments-slope")
                      )
                    )
                  )
          ,
          modInteger = plutus match
              case PlutusLedgerLanguage.PlutusV1 | PlutusLedgerLanguage.PlutusV2 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params("modInteger-cpu-arguments-constant"),
                        model = TwoArguments.MultipliedSizes(
                          OneVariableLinearFunction(
                            intercept =
                                params("modInteger-cpu-arguments-model-arguments-intercept"),
                            slope = params("modInteger-cpu-arguments-model-arguments-slope")
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.SubtractedSizes(
                      SubtractedSizesLinearFunction(
                        intercept = params("modInteger-memory-arguments-intercept"),
                        slope = params("modInteger-memory-arguments-slope"),
                        minimum = params("modInteger-memory-arguments-minimum")
                      )
                    )
                  )
              case PlutusLedgerLanguage.PlutusV3 =>
                  DefaultCostingFun(
                    cpu = TwoArguments.ConstAboveDiagonal(
                      ConstantOrTwoArguments(
                        constant = params("modInteger-cpu-arguments-constant"),
                        model = TwoArguments.QuadraticInXAndY(
                          TwoVariableQuadraticFunction(
                            minimum = params("modInteger-cpu-arguments-model-arguments-minimum"),
                            c00 = params("modInteger-cpu-arguments-model-arguments-c00"),
                            c10 = params("modInteger-cpu-arguments-model-arguments-c10"),
                            c01 = params("modInteger-cpu-arguments-model-arguments-c01"),
                            c20 = params("modInteger-cpu-arguments-model-arguments-c20"),
                            c11 = params("modInteger-cpu-arguments-model-arguments-c11"),
                            c02 = params("modInteger-cpu-arguments-model-arguments-c02")
                          )
                        )
                      )
                    ),
                    memory = TwoArguments.LinearInY(
                      OneVariableLinearFunction(
                        intercept = params("modInteger-memory-arguments-intercept"),
                        slope = params("modInteger-memory-arguments-slope")
                      )
                    )
                  )
          ,
          equalsInteger = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params("equalsInteger-cpu-arguments-intercept"),
                slope = params("equalsInteger-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("equalsInteger-memory-arguments")
            )
          ),
          lessThanInteger = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params("lessThanInteger-cpu-arguments-intercept"),
                slope = params("lessThanInteger-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("lessThanInteger-memory-arguments")
            )
          ),
          lessThanEqualsInteger = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params("lessThanEqualsInteger-cpu-arguments-intercept"),
                slope = params("lessThanEqualsInteger-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("lessThanEqualsInteger-memory-arguments")
            )
          ),
          appendByteString = DefaultCostingFun(
            cpu = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params("appendByteString-cpu-arguments-intercept"),
                slope = params("appendByteString-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params("appendByteString-memory-arguments-intercept"),
                slope = params("appendByteString-memory-arguments-slope")
              )
            )
          ),
          consByteString = DefaultCostingFun(
            cpu = TwoArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params("consByteString-cpu-arguments-intercept"),
                slope = params("consByteString-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params("consByteString-memory-arguments-intercept"),
                slope = params("consByteString-memory-arguments-slope")
              )
            )
          ),
          sliceByteString = DefaultCostingFun(
            cpu = ThreeArguments.LinearInZ(
              OneVariableLinearFunction(
                intercept = params("sliceByteString-cpu-arguments-intercept"),
                slope = params("sliceByteString-cpu-arguments-slope")
              )
            ),
            memory = ThreeArguments.LinearInZ(
              OneVariableLinearFunction(
                intercept = params("sliceByteString-memory-arguments-intercept"),
                slope = params("sliceByteString-memory-arguments-slope")
              )
            )
          ),
          lengthOfByteString = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(cost = params("lengthOfByteString-cpu-arguments")),
            memory = OneArgument.ConstantCost(cost = params("lengthOfByteString-memory-arguments"))
          ),
          indexByteString = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(cost = params("indexByteString-cpu-arguments")),
            memory = TwoArguments.ConstantCost(cost = params("indexByteString-memory-arguments"))
          ),
          equalsByteString = DefaultCostingFun(
            cpu = TwoArguments.LinearOnDiagonal(
              ConstantOrLinear(
                constant = params("equalsByteString-cpu-arguments-constant"),
                intercept = params("equalsByteString-cpu-arguments-intercept"),
                slope = params("equalsByteString-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(cost = params("equalsByteString-memory-arguments"))
          ),
          lessThanByteString = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params("lessThanByteString-cpu-arguments-intercept"),
                slope = params("lessThanByteString-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(cost = params("lessThanByteString-memory-arguments"))
          ),
          lessThanEqualsByteString = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params("lessThanEqualsByteString-cpu-arguments-intercept"),
                slope = params("lessThanEqualsByteString-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("lessThanEqualsByteString-memory-arguments")
            )
          ),
          sha2_256 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("sha2_256-cpu-arguments-intercept"),
                slope = params("sha2_256-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params("sha2_256-memory-arguments")
            )
          ),
          sha3_256 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("sha3_256-cpu-arguments-intercept"),
                slope = params("sha3_256-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params("sha3_256-memory-arguments")
            )
          ),
          blake2b_256 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("blake2b_256-cpu-arguments-intercept"),
                slope = params("blake2b_256-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params("blake2b_256-memory-arguments")
            )
          ),
          verifyEd25519Signature = DefaultCostingFun(
            cpu = ThreeArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params("verifyEd25519Signature-cpu-arguments-intercept"),
                slope = params("verifyEd25519Signature-cpu-arguments-slope")
              )
            ),
            memory = ThreeArguments.ConstantCost(
              cost = params("verifyEd25519Signature-memory-arguments")
            )
          ),
          verifyEcdsaSecp256k1Signature = DefaultCostingFun(
            cpu = ThreeArguments.ConstantCost(
              cost = params("verifyEcdsaSecp256k1Signature-cpu-arguments")
            ),
            memory = ThreeArguments.ConstantCost(
              cost = params("verifyEcdsaSecp256k1Signature-memory-arguments")
            )
          ),
          verifySchnorrSecp256k1Signature = DefaultCostingFun(
            cpu = ThreeArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params("verifySchnorrSecp256k1Signature-cpu-arguments-intercept"),
                slope = params("verifySchnorrSecp256k1Signature-cpu-arguments-slope")
              )
            ),
            memory = ThreeArguments.ConstantCost(
              cost = params("verifySchnorrSecp256k1Signature-memory-arguments")
            )
          ),
          appendString = DefaultCostingFun(
            cpu = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params("appendString-cpu-arguments-intercept"),
                slope = params("appendString-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.AddedSizes(
              OneVariableLinearFunction(
                intercept = params("appendString-memory-arguments-intercept"),
                slope = params("appendString-memory-arguments-slope")
              )
            )
          ),
          equalsString = DefaultCostingFun(
            cpu = TwoArguments.LinearOnDiagonal(
              ConstantOrLinear(
                constant = params("equalsString-cpu-arguments-constant"),
                intercept = params("equalsString-cpu-arguments-intercept"),
                slope = params("equalsString-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("equalsString-memory-arguments")
            )
          ),
          encodeUtf8 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("encodeUtf8-cpu-arguments-intercept"),
                slope = params("encodeUtf8-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("encodeUtf8-memory-arguments-intercept"),
                slope = params("encodeUtf8-memory-arguments-slope")
              )
            )
          ),
          decodeUtf8 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("decodeUtf8-cpu-arguments-intercept"),
                slope = params("decodeUtf8-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("decodeUtf8-memory-arguments-intercept"),
                slope = params("decodeUtf8-memory-arguments-slope")
              )
            )
          ),
          ifThenElse = DefaultCostingFun(
            cpu = ThreeArguments.ConstantCost(
              cost = params("ifThenElse-cpu-arguments")
            ),
            memory = ThreeArguments.ConstantCost(
              cost = params("ifThenElse-memory-arguments")
            )
          ),
          chooseUnit = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("chooseUnit-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("chooseUnit-memory-arguments")
            )
          ),
          trace = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("trace-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("trace-memory-arguments")
            )
          ),
          fstPair = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("fstPair-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("fstPair-memory-arguments")
            )
          ),
          sndPair = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("sndPair-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("sndPair-memory-arguments")
            )
          ),
          chooseList = DefaultCostingFun(
            cpu = ThreeArguments.ConstantCost(
              cost = params("chooseList-cpu-arguments")
            ),
            memory = ThreeArguments.ConstantCost(
              cost = params("chooseList-memory-arguments")
            )
          ),
          mkCons = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("mkCons-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("mkCons-memory-arguments")
            )
          ),
          headList = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("headList-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("headList-memory-arguments")
            )
          ),
          tailList = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("tailList-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("tailList-memory-arguments")
            )
          ),
          nullList = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("nullList-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("nullList-memory-arguments")
            )
          ),
          chooseData = DefaultCostingFun(
            cpu = SixArguments.ConstantCost(
              cost = params("chooseData-cpu-arguments")
            ),
            memory = SixArguments.ConstantCost(
              cost = params("chooseData-memory-arguments")
            )
          ),
          constrData = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("constrData-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("constrData-memory-arguments")
            )
          ),
          mapData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("mapData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("mapData-memory-arguments")
            )
          ),
          listData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("listData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("listData-memory-arguments")
            )
          ),
          iData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("iData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("iData-memory-arguments")
            )
          ),
          bData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("bData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("bData-memory-arguments")
            )
          ),
          unConstrData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("unConstrData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("unConstrData-memory-arguments")
            )
          ),
          unMapData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("unMapData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("unMapData-memory-arguments")
            )
          ),
          unListData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("unListData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("unListData-memory-arguments")
            )
          ),
          unIData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("unIData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("unIData-memory-arguments")
            )
          ),
          unBData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("unBData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("unBData-memory-arguments")
            )
          ),
          equalsData = DefaultCostingFun(
            cpu = TwoArguments.MinSize(
              OneVariableLinearFunction(
                intercept = params("equalsData-cpu-arguments-intercept"),
                slope = params("equalsData-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("equalsData-memory-arguments")
            )
          ),
          mkPairData = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("mkPairData-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("mkPairData-memory-arguments")
            )
          ),
          mkNilData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("mkNilData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("mkNilData-memory-arguments")
            )
          ),
          mkNilPairData = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("mkNilPairData-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("mkNilPairData-memory-arguments")
            )
          ),
          serialiseData = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("serialiseData-cpu-arguments-intercept"),
                slope = params("serialiseData-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("serialiseData-memory-arguments-intercept"),
                slope = params("serialiseData-memory-arguments-slope")
              )
            )
          ),
          blake2b_224 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("blake2b_224-cpu-arguments-intercept"),
                slope = params("blake2b_224-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params("blake2b_224-memory-arguments")
            )
          ),
          keccak_256 = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("keccak_256-cpu-arguments-intercept"),
                slope = params("keccak_256-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.ConstantCost(
              cost = params("keccak_256-memory-arguments")
            )
          ),
          bls12_381_G1_add = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("bls12_381_G1_add-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_G1_add-memory-arguments")
            )
          ),
          bls12_381_G1_neg = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("bls12_381_G1_neg-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("bls12_381_G1_neg-memory-arguments")
            )
          ),
          bls12_381_G1_scalarMul = DefaultCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params("bls12_381_G1_scalarMul-cpu-arguments-intercept"),
                slope = params("bls12_381_G1_scalarMul-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_G1_scalarMul-memory-arguments")
            )
          ),
          bls12_381_G1_equal = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("bls12_381_G1_equal-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_G1_equal-memory-arguments")
            )
          ),
          bls12_381_G1_compress = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("bls12_381_G1_compress-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("bls12_381_G1_compress-memory-arguments")
            )
          ),
          bls12_381_G1_uncompress = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("bls12_381_G1_uncompress-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("bls12_381_G1_uncompress-memory-arguments")
            )
          ),
          bls12_381_G1_hashToGroup = DefaultCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params("bls12_381_G1_hashToGroup-cpu-arguments-intercept"),
                slope = params("bls12_381_G1_hashToGroup-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_G1_hashToGroup-memory-arguments")
            )
          ),
          bls12_381_G2_add = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("bls12_381_G2_add-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_G2_add-memory-arguments")
            )
          ),
          bls12_381_G2_neg = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("bls12_381_G2_neg-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("bls12_381_G2_neg-memory-arguments")
            )
          ),
          bls12_381_G2_scalarMul = DefaultCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params("bls12_381_G2_scalarMul-cpu-arguments-intercept"),
                slope = params("bls12_381_G2_scalarMul-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_G2_scalarMul-memory-arguments")
            )
          ),
          bls12_381_G2_equal = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("bls12_381_G2_equal-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_G2_equal-memory-arguments")
            )
          ),
          bls12_381_G2_compress = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("bls12_381_G2_compress-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("bls12_381_G2_compress-memory-arguments")
            )
          ),
          bls12_381_G2_uncompress = DefaultCostingFun(
            cpu = OneArgument.ConstantCost(
              cost = params("bls12_381_G2_uncompress-cpu-arguments")
            ),
            memory = OneArgument.ConstantCost(
              cost = params("bls12_381_G2_uncompress-memory-arguments")
            )
          ),
          bls12_381_G2_hashToGroup = DefaultCostingFun(
            cpu = TwoArguments.LinearInX(
              OneVariableLinearFunction(
                intercept = params("bls12_381_G2_hashToGroup-cpu-arguments-intercept"),
                slope = params("bls12_381_G2_hashToGroup-cpu-arguments-slope")
              )
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_G2_hashToGroup-memory-arguments")
            )
          ),
          bls12_381_millerLoop = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("bls12_381_millerLoop-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_millerLoop-memory-arguments")
            )
          ),
          bls12_381_mulMlResult = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("bls12_381_mulMlResult-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_mulMlResult-memory-arguments")
            )
          ),
          bls12_381_finalVerify = DefaultCostingFun(
            cpu = TwoArguments.ConstantCost(
              cost = params("bls12_381_finalVerify-cpu-arguments")
            ),
            memory = TwoArguments.ConstantCost(
              cost = params("bls12_381_finalVerify-memory-arguments")
            )
          ),
          integerToByteString = IntegerToByteStringCostingFun(
            cpu = ThreeArguments.QuadraticInZ(
              OneVariableQuadraticFunction(
                c0 = params("integerToByteString-cpu-arguments-c0"),
                c1 = params("integerToByteString-cpu-arguments-c1"),
                c2 = params("integerToByteString-cpu-arguments-c2")
              )
            ),
            memory = ThreeArguments.LiteralInYOrLinearInZ(
              OneVariableLinearFunction(
                intercept = params("integerToByteString-memory-arguments-intercept"),
                slope = params("integerToByteString-memory-arguments-slope")
              )
            )
          ),
          byteStringToInteger = DefaultCostingFun(
            cpu = TwoArguments.QuadraticInY(
              OneVariableQuadraticFunction(
                c0 = params("byteStringToInteger-cpu-arguments-c0"),
                c1 = params("byteStringToInteger-cpu-arguments-c1"),
                c2 = params("byteStringToInteger-cpu-arguments-c2")
              )
            ),
            memory = TwoArguments.LinearInY(
              OneVariableLinearFunction(
                intercept = params("byteStringToInteger-memory-arguments-intercept"),
                slope = params("byteStringToInteger-memory-arguments-slope")
              )
            )
          ),
          andByteString = DefaultCostingFun(
            cpu = ThreeArguments.LinearInYAndZ(
              TwoVariableLinearFunction(
                intercept = params("andByteString-cpu-arguments-intercept"),
                slope1 = params("andByteString-cpu-arguments-slope1"),
                slope2 = params("andByteString-cpu-arguments-slope2")
              )
            ),
            memory = ThreeArguments.LinearInMaxYZ(
              OneVariableLinearFunction(
                intercept = params("andByteString-memory-arguments-intercept"),
                slope = params("andByteString-memory-arguments-slope")
              )
            )
          ),
          orByteString = DefaultCostingFun(
            cpu = ThreeArguments.LinearInYAndZ(
              TwoVariableLinearFunction(
                intercept = params("orByteString-cpu-arguments-intercept"),
                slope1 = params("orByteString-cpu-arguments-slope1"),
                slope2 = params("orByteString-cpu-arguments-slope2")
              )
            ),
            memory = ThreeArguments.LinearInMaxYZ(
              OneVariableLinearFunction(
                intercept = params("orByteString-memory-arguments-intercept"),
                slope = params("orByteString-memory-arguments-slope")
              )
            )
          ),
          xorByteString = DefaultCostingFun(
            cpu = ThreeArguments.LinearInYAndZ(
              TwoVariableLinearFunction(
                intercept = params("xorByteString-cpu-arguments-intercept"),
                slope1 = params("xorByteString-cpu-arguments-slope1"),
                slope2 = params("xorByteString-cpu-arguments-slope2")
              )
            ),
            memory = ThreeArguments.LinearInMaxYZ(
              OneVariableLinearFunction(
                intercept = params("xorByteString-memory-arguments-intercept"),
                slope = params("xorByteString-memory-arguments-slope")
              )
            )
          ),
          complementByteString = DefaultCostingFun(
            cpu = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("complementByteString-cpu-arguments-intercept"),
                slope = params("complementByteString-cpu-arguments-slope")
              )
            ),
            memory = OneArgument.LinearInX(
              OneVariableLinearFunction(
                intercept = params("complementByteString-memory-arguments-intercept"),
                slope = params("complementByteString-memory-arguments-slope")
              )
            )
          )
        )

    /** Read a BuiltinCostModel from an input stream of JSON
      *
      * @param input
      *   the input stream
      * @return
      *   a BuiltinCostModel
      */
    def fromInputStream(input: java.io.InputStream): BuiltinCostModel = {
        read[BuiltinCostModel](input)
    }

    /** Read a BuiltinCostModel from a string of JSON
      * @param json
      *   the JSON string
      * @return
      *   a BuiltinCostModel
      */
    def fromJsonString(json: String): BuiltinCostModel = {
        read[BuiltinCostModel](json)
    }
}
