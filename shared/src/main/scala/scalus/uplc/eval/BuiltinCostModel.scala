package scalus.uplc.eval

import upickle.default.*

case class BuiltinCostModel(
    // Integers
    addInteger: CostingFun[TwoArguments],
    subtractInteger: CostingFun[TwoArguments],
    multiplyInteger: CostingFun[TwoArguments],
    divideInteger: CostingFun[TwoArguments],
    quotientInteger: CostingFun[TwoArguments],
    remainderInteger: CostingFun[TwoArguments],
    modInteger: CostingFun[TwoArguments],
    equalsInteger: CostingFun[TwoArguments],
    lessThanInteger: CostingFun[TwoArguments],
    lessThanEqualsInteger: CostingFun[TwoArguments],
    // Bytestrings
    appendByteString: CostingFun[TwoArguments],
    consByteString: CostingFun[TwoArguments],
    sliceByteString: CostingFun[ThreeArguments],
    lengthOfByteString: CostingFun[OneArgument],
    indexByteString: CostingFun[TwoArguments],
    equalsByteString: CostingFun[TwoArguments],
    lessThanByteString: CostingFun[TwoArguments],
    lessThanEqualsByteString: CostingFun[TwoArguments],
    // // Cryptography and hashes
    sha2_256: CostingFun[OneArgument],
    sha3_256: CostingFun[OneArgument],
    blake2b_256: CostingFun[OneArgument],
    verifyEd25519Signature: CostingFun[ThreeArguments],
    verifyEcdsaSecp256k1Signature: CostingFun[ThreeArguments],
    verifySchnorrSecp256k1Signature: CostingFun[ThreeArguments],
    // Strings
    appendString: CostingFun[TwoArguments],
    equalsString: CostingFun[TwoArguments],
    encodeUtf8: CostingFun[OneArgument],
    decodeUtf8: CostingFun[OneArgument],
    // Bool
    ifThenElse: CostingFun[ThreeArguments],
    // Unit
    chooseUnit: CostingFun[TwoArguments],
    // Tracing
    trace: CostingFun[TwoArguments],
    // Pairs
    fstPair: CostingFun[OneArgument],
    sndPair: CostingFun[OneArgument],
    // Lists
    chooseList: CostingFun[ThreeArguments],
    mkCons: CostingFun[TwoArguments],
    headList: CostingFun[OneArgument],
    tailList: CostingFun[OneArgument],
    nullList: CostingFun[OneArgument],
    // Data
    chooseData: CostingFun[SixArguments],
    constrData: CostingFun[TwoArguments],
    mapData: CostingFun[OneArgument],
    listData: CostingFun[OneArgument],
    iData: CostingFun[OneArgument],
    bData: CostingFun[OneArgument],
    unConstrData: CostingFun[OneArgument],
    unMapData: CostingFun[OneArgument],
    unListData: CostingFun[OneArgument],
    unIData: CostingFun[OneArgument],
    unBData: CostingFun[OneArgument],
    equalsData: CostingFun[TwoArguments],
    // Misc constructors
    mkPairData: CostingFun[TwoArguments],
    mkNilData: CostingFun[OneArgument],
    mkNilPairData: CostingFun[OneArgument],
    serialiseData: CostingFun[OneArgument]
)

object BuiltinCostModel {

    val defaultBuiltinCostModel = BuiltinCostModel(
      addInteger = CostingFun(
        cpu = TwoArguments.MaxSize(OneVariableLinearFunction(intercept = 205665, slope = 812)),
        memory = TwoArguments.MaxSize(OneVariableLinearFunction(intercept = 1, slope = 1))
      ),
      subtractInteger = CostingFun(
        cpu = TwoArguments.MaxSize(OneVariableLinearFunction(intercept = 205665, slope = 812)),
        memory = TwoArguments.MaxSize(OneVariableLinearFunction(intercept = 1, slope = 1))
      ),
      multiplyInteger = CostingFun(
        cpu = TwoArguments.AddedSizes(OneVariableLinearFunction(intercept = 69522, slope = 11687)),
        memory = TwoArguments.AddedSizes(OneVariableLinearFunction(intercept = 0, slope = 1))
      ),
      divideInteger = CostingFun(
        cpu = TwoArguments.ConstAboveDiagonal(
          ConstantOrTwoArguments(
            constant = 196500,
            model = TwoArguments.MultipliedSizes(
              OneVariableLinearFunction(intercept = 453240, slope = 220)
            )
          )
        ),
        memory = TwoArguments.SubtractedSizes(
          SubtractedSizesLinearFunction(intercept = 0, slope = 1, minimum = 1)
        )
      ),
      quotientInteger = CostingFun(
        cpu = TwoArguments.ConstAboveDiagonal(
          ConstantOrTwoArguments(
            constant = 196500,
            model = TwoArguments.MultipliedSizes(
              OneVariableLinearFunction(intercept = 453240, slope = 220)
            )
          )
        ),
        memory = TwoArguments.SubtractedSizes(
          SubtractedSizesLinearFunction(intercept = 0, slope = 1, minimum = 1)
        )
      ),
      remainderInteger = CostingFun(
        cpu = TwoArguments.ConstAboveDiagonal(
          ConstantOrTwoArguments(
            constant = 196500,
            model = TwoArguments.MultipliedSizes(
              OneVariableLinearFunction(intercept = 453240, slope = 220)
            )
          )
        ),
        memory = TwoArguments.SubtractedSizes(
          SubtractedSizesLinearFunction(intercept = 0, slope = 1, minimum = 1)
        )
      ),
      modInteger = CostingFun(
        cpu = TwoArguments.ConstAboveDiagonal(
          ConstantOrTwoArguments(
            constant = 196500,
            model = TwoArguments.MultipliedSizes(
              OneVariableLinearFunction(intercept = 453240, slope = 220)
            )
          )
        ),
        memory = TwoArguments.SubtractedSizes(
          SubtractedSizesLinearFunction(intercept = 0, slope = 1, minimum = 1)
        )
      ),
      equalsInteger = CostingFun(
        cpu = TwoArguments.MinSize(OneVariableLinearFunction(intercept = 208512, slope = 421)),
        memory = TwoArguments.ConstantCost(cost = 1)
      ),
      lessThanInteger = CostingFun(
        cpu = TwoArguments.MinSize(OneVariableLinearFunction(intercept = 208896, slope = 511)),
        memory = TwoArguments.ConstantCost(cost = 1)
      ),
      lessThanEqualsInteger = CostingFun(
        cpu = TwoArguments.MinSize(OneVariableLinearFunction(intercept = 204924, slope = 473)),
        memory = TwoArguments.ConstantCost(cost = 1)
      ),
      appendByteString = CostingFun(
        cpu = TwoArguments.AddedSizes(OneVariableLinearFunction(intercept = 1000, slope = 571)),
        memory = TwoArguments.AddedSizes(OneVariableLinearFunction(intercept = 0, slope = 1))
      ),
      consByteString = CostingFun(
        cpu = TwoArguments.LinearInY(OneVariableLinearFunction(intercept = 221973, slope = 511)),
        memory = TwoArguments.AddedSizes(OneVariableLinearFunction(intercept = 0, slope = 1))
      ),
      sliceByteString = CostingFun(
        cpu = ThreeArguments.LinearInZ(
          OneVariableLinearFunction(intercept = 265318, slope = 0)
        ),
        memory = ThreeArguments.LinearInZ(
          OneVariableLinearFunction(intercept = 4, slope = 0)
        )
      ),
      lengthOfByteString = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 1000),
        memory = OneArgument.ConstantCost(cost = 10)
      ),
      indexByteString = CostingFun(
        cpu = TwoArguments.ConstantCost(cost = 57667),
        memory = TwoArguments.ConstantCost(cost = 4)
      ),
      equalsByteString = CostingFun(
        cpu = TwoArguments.LinearOnDiagonal(
          ModelConstantOrLinear(constant = 245000, intercept = 216773, slope = 62)
        ),
        memory = TwoArguments.ConstantCost(cost = 1)
      ),
      lessThanByteString = CostingFun(
        cpu = TwoArguments.MinSize(OneVariableLinearFunction(intercept = 197145, slope = 156)),
        memory = TwoArguments.ConstantCost(cost = 1)
      ),
      lessThanEqualsByteString = CostingFun(
        cpu = TwoArguments.MinSize(OneVariableLinearFunction(intercept = 197145, slope = 156)),
        memory = TwoArguments.ConstantCost(cost = 1)
      ),
      sha2_256 = CostingFun(
        cpu = OneArgument.LinearCost(OneVariableLinearFunction(intercept = 806990, slope = 30482)),
        memory = OneArgument.ConstantCost(cost = 4)
      ),
      sha3_256 = CostingFun(
        cpu = OneArgument.LinearCost(OneVariableLinearFunction(intercept = 1927926, slope = 82523)),
        memory = OneArgument.ConstantCost(cost = 4)
      ),
      blake2b_256 = CostingFun(
        cpu = OneArgument.LinearCost(OneVariableLinearFunction(intercept = 117366, slope = 10475)),
        memory = OneArgument.ConstantCost(cost = 4)
      ),
      verifyEd25519Signature = CostingFun(
        cpu = ThreeArguments.LinearInZ(
          OneVariableLinearFunction(intercept = 57996947, slope = 18975)
        ),
        memory = ThreeArguments.ConstantCost(cost = 10)
      ),
      verifyEcdsaSecp256k1Signature = CostingFun(
        cpu = ThreeArguments.ConstantCost(cost = 35190005),
        memory = ThreeArguments.ConstantCost(cost = 10)
      ),
      verifySchnorrSecp256k1Signature = CostingFun(
        cpu = ThreeArguments.LinearInY(
          OneVariableLinearFunction(intercept = 39121781, slope = 32260)
        ),
        memory = ThreeArguments.ConstantCost(cost = 10)
      ),
      appendString = CostingFun(
        cpu = TwoArguments.AddedSizes(OneVariableLinearFunction(intercept = 1000, slope = 24177)),
        memory = TwoArguments.AddedSizes(OneVariableLinearFunction(intercept = 4, slope = 1))
      ),
      equalsString = CostingFun(
        cpu = TwoArguments.LinearOnDiagonal(
          ModelConstantOrLinear(constant = 187000, intercept = 1000, slope = 52998)
        ),
        memory = TwoArguments.ConstantCost(cost = 1)
      ),
      encodeUtf8 = CostingFun(
        cpu = OneArgument.LinearCost(OneVariableLinearFunction(intercept = 1000, slope = 28662)),
        memory = OneArgument.LinearCost(OneVariableLinearFunction(intercept = 4, slope = 2))
      ),
      decodeUtf8 = CostingFun(
        cpu = OneArgument.LinearCost(OneVariableLinearFunction(intercept = 497525, slope = 14068)),
        memory = OneArgument.LinearCost(OneVariableLinearFunction(intercept = 4, slope = 2))
      ),
      ifThenElse = CostingFun(
        cpu = ThreeArguments.ConstantCost(cost = 80556),
        memory = ThreeArguments.ConstantCost(cost = 1)
      ),
      chooseUnit = CostingFun(
        cpu = TwoArguments.ConstantCost(cost = 46417),
        memory = TwoArguments.ConstantCost(cost = 4)
      ),
      trace = CostingFun(
        cpu = TwoArguments.ConstantCost(cost = 212342),
        memory = TwoArguments.ConstantCost(cost = 32)
      ),
      fstPair = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 80436),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      sndPair = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 85931),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      chooseList = CostingFun(
        cpu = ThreeArguments.ConstantCost(cost = 175354),
        memory = ThreeArguments.ConstantCost(cost = 32)
      ),
      mkCons = CostingFun(
        cpu = TwoArguments.ConstantCost(cost = 65493),
        memory = TwoArguments.ConstantCost(cost = 32)
      ),
      headList = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 43249),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      tailList = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 41182),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      nullList = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 60091),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      chooseData = CostingFun(
        cpu = SixArguments.ConstantCost(cost = 19537),
        memory = SixArguments.ConstantCost(cost = 32)
      ), // Adjusted for context, assuming ModelSixArguments fits here
      constrData = CostingFun(
        cpu = TwoArguments.ConstantCost(cost = 89141),
        memory = TwoArguments.ConstantCost(cost = 32)
      ),
      mapData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 64832),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      listData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 52467),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      iData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 1000),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      bData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 1000),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      unConstrData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 32696),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      unMapData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 38314),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      unListData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 32247),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      unIData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 43357),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      unBData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 31220),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      equalsData = CostingFun(
        cpu = TwoArguments.MinSize(OneVariableLinearFunction(intercept = 1060367, slope = 12586)),
        memory = TwoArguments.ConstantCost(cost = 1)
      ),
      mkPairData = CostingFun(
        cpu = TwoArguments.ConstantCost(cost = 76511),
        memory = TwoArguments.ConstantCost(cost = 32)
      ),
      mkNilData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 22558),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      mkNilPairData = CostingFun(
        cpu = OneArgument.ConstantCost(cost = 16563),
        memory = OneArgument.ConstantCost(cost = 32)
      ),
      serialiseData = CostingFun(
        cpu =
            OneArgument.LinearCost(OneVariableLinearFunction(intercept = 1159724, slope = 392670)),
        memory = OneArgument.LinearCost(OneVariableLinearFunction(intercept = 0, slope = 2))
      )
    )

    given ReadWriter[BuiltinCostModel] = readwriter[ujson.Value].bimap(
      model =>
          ujson.Obj(
            "addInteger" -> write(model.addInteger),
            "subtractInteger" -> write(model.subtractInteger),
            "multiplyInteger" -> write(model.multiplyInteger),
            "divideInteger" -> write(model.divideInteger),
            "quotientInteger" -> write(model.quotientInteger),
            "remainderInteger" -> write(model.remainderInteger),
            "modInteger" -> write(model.modInteger),
            "equalsInteger" -> write(model.equalsInteger),
            "lessThanInteger" -> write(model.lessThanInteger),
            "lessThanEqualsInteger" -> write(model.lessThanEqualsInteger),
            "appendByteString" -> write(model.appendByteString),
            "consByteString" -> write(model.consByteString),
            "sliceByteString" -> write(model.sliceByteString),
            "lengthOfByteString" -> write(model.lengthOfByteString),
            "indexByteString" -> write(model.indexByteString),
            "equalsByteString" -> write(model.equalsByteString),
            "lessThanByteString" -> write(model.lessThanByteString),
            "lessThanEqualsByteString" -> write(model.lessThanEqualsByteString),
            "sha2_256" -> write(model.sha2_256),
            "sha3_256" -> write(model.sha3_256),
            "blake2b_256" -> write(model.blake2b_256),
            "verifyEd25519Signature" -> write(model.verifyEd25519Signature),
            "verifyEcdsaSecp256k1Signature" -> write(model.verifyEcdsaSecp256k1Signature),
            "verifySchnorrSecp256k1Signature" -> write(model.verifySchnorrSecp256k1Signature),
            "appendString" -> write(model.appendString),
            "equalsString" -> write(model.equalsString),
            "encodeUtf8" -> write(model.encodeUtf8),
            "decodeUtf8" -> write(model.decodeUtf8),
            "ifThenElse" -> write(model.ifThenElse),
            "chooseUnit" -> write(model.chooseUnit),
            "trace" -> write(model.trace),
            "fstPair" -> write(model.fstPair),
            "sndPair" -> write(model.sndPair),
            "chooseList" -> write(model.chooseList),
            "mkCons" -> write(model.mkCons),
            "headList" -> write(model.headList),
            "tailList" -> write(model.tailList),
            "nullList" -> write(model.nullList),
            "chooseData" -> write(model.chooseData),
            "constrData" -> write(model.constrData),
            "mapData" -> write(model.mapData),
            "listData" -> write(model.listData),
            "iData" -> write(model.iData),
            "bData" -> write(model.bData),
            "unConstrData" -> write(model.unConstrData),
            "unMapData" -> write(model.unMapData),
            "unListData" -> write(model.unListData),
            "unIData" -> write(model.unIData),
            "unBData" -> write(model.unBData),
            "equalsData" -> write(model.equalsData),
            "mkPairData" -> write(model.mkPairData),
            "mkNilData" -> write(model.mkNilData),
            "mkNilPairData" -> write(model.mkNilPairData),
            "serialiseData" -> write(model.serialiseData)
          ),
      json =>
          BuiltinCostModel(
            addInteger = read[CostingFun[TwoArguments]](json("addInteger")),
            subtractInteger = read[CostingFun[TwoArguments]](json("subtractInteger")),
            multiplyInteger = read[CostingFun[TwoArguments]](json("multiplyInteger")),
            divideInteger = read[CostingFun[TwoArguments]](json("divideInteger")),
            quotientInteger = read[CostingFun[TwoArguments]](json("quotientInteger")),
            remainderInteger = read[CostingFun[TwoArguments]](json("remainderInteger")),
            modInteger = read[CostingFun[TwoArguments]](json("modInteger")),
            equalsInteger = read[CostingFun[TwoArguments]](json("equalsInteger")),
            lessThanInteger = read[CostingFun[TwoArguments]](json("lessThanInteger")),
            lessThanEqualsInteger = read[CostingFun[TwoArguments]](json("lessThanEqualsInteger")),
            appendByteString = read[CostingFun[TwoArguments]](json("appendByteString")),
            consByteString = read[CostingFun[TwoArguments]](json("consByteString")),
            sliceByteString = read[CostingFun[ThreeArguments]](json("sliceByteString")),
            lengthOfByteString = read[CostingFun[OneArgument]](json("lengthOfByteString")),
            indexByteString = read[CostingFun[TwoArguments]](json("indexByteString")),
            equalsByteString = read[CostingFun[TwoArguments]](json("equalsByteString")),
            lessThanByteString = read[CostingFun[TwoArguments]](json("lessThanByteString")),
            lessThanEqualsByteString =
                read[CostingFun[TwoArguments]](json("lessThanEqualsByteString")),
            sha2_256 = read[CostingFun[OneArgument]](json("sha2_256")),
            sha3_256 = read[CostingFun[OneArgument]](json("sha3_256")),
            blake2b_256 = read[CostingFun[OneArgument]](json("blake2b_256")),
            verifyEd25519Signature =
                read[CostingFun[ThreeArguments]](json("verifyEd25519Signature")),
            verifyEcdsaSecp256k1Signature =
                read[CostingFun[ThreeArguments]](json("verifyEcdsaSecp256k1Signature")),
            verifySchnorrSecp256k1Signature =
                read[CostingFun[ThreeArguments]](json("verifySchnorrSecp256k1Signature")),
            appendString = read[CostingFun[TwoArguments]](json("appendString")),
            equalsString = read[CostingFun[TwoArguments]](json("equalsString")),
            encodeUtf8 = read[CostingFun[OneArgument]](json("encodeUtf8")),
            decodeUtf8 = read[CostingFun[OneArgument]](json("decodeUtf8")),
            ifThenElse = read[CostingFun[ThreeArguments]](json("ifThenElse")),
            chooseUnit = read[CostingFun[TwoArguments]](json("chooseUnit")),
            trace = read[CostingFun[TwoArguments]](json("trace")),
            fstPair = read[CostingFun[OneArgument]](json("fstPair")),
            sndPair = read[CostingFun[OneArgument]](json("sndPair")),
            chooseList = read[CostingFun[ThreeArguments]](json("chooseList")),
            mkCons = read[CostingFun[TwoArguments]](json("mkCons")),
            headList = read[CostingFun[OneArgument]](json("headList")),
            tailList = read[CostingFun[OneArgument]](json("tailList")),
            nullList = read[CostingFun[OneArgument]](json("nullList")),
            chooseData = read[CostingFun[SixArguments]](json("chooseData")),
            constrData = read[CostingFun[TwoArguments]](json("constrData")),
            mapData = read[CostingFun[OneArgument]](json("mapData")),
            listData = read[CostingFun[OneArgument]](json("listData")),
            iData = read[CostingFun[OneArgument]](json("iData")),
            bData = read[CostingFun[OneArgument]](json("bData")),
            unConstrData = read[CostingFun[OneArgument]](json("unConstrData")),
            unMapData = read[CostingFun[OneArgument]](json("unMapData")),
            unListData = read[CostingFun[OneArgument]](json("unListData")),
            unIData = read[CostingFun[OneArgument]](json("unIData")),
            unBData = read[CostingFun[OneArgument]](json("unBData")),
            equalsData = read[CostingFun[TwoArguments]](json("equalsData")),
            mkPairData = read[CostingFun[TwoArguments]](json("mkPairData")),
            mkNilData = read[CostingFun[OneArgument]](json("mkNilData")),
            mkNilPairData = read[CostingFun[OneArgument]](json("mkNilPairData")),
            serialiseData = read[CostingFun[OneArgument]](json("serialiseData"))
          )
    )

    /** Read a BuiltinCostModel from an input stream of JSON
      *
      * @param input
      * @return
      *   a BuiltinCostModel
      */
    def fromInputStream(input: java.io.InputStream): BuiltinCostModel = {
        read[BuiltinCostModel](input)
    }

    /** Read a BuiltinCostModel from a string of JSON
      * @param json
      * @return
      *   a BuiltinCostModel
      */
    def fromJsonString(json: String): BuiltinCostModel = {
        read[BuiltinCostModel](json)
    }
}
