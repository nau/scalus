package scalus.ledger.babbage

import upickle.default.*
/*
  Funny thing is that JVM has a limit of 255 parameters in a method if the args are Ints.
  If it's Long, then the limit is 127.
  And we can't generate a constructor call for `PlutusV1Params` or `PlutusV2Params`
  which has more than 127 parameters.
  So I'm using Ints here, and that should be enough for the protocol parameters.
  Then, I've changed the `PlutusV1Params` and `PlutusV2Params` to have Longs
  and be a class with public fields.
  I also added a `JsonUtils` object to generate a `ReadWriter` for these classes.
 */

/** Plutus V1 cost model parameters.
  *
  * The names of the fields are taken from
  * [[https://github.com/input-output-hk/plutus/blob/1.40.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-544.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV1Params {
    var `addInteger-cpu-arguments-intercept`: Long = 0L
    var `addInteger-cpu-arguments-slope`: Long = 0L
    var `addInteger-memory-arguments-intercept`: Long = 0L
    var `addInteger-memory-arguments-slope`: Long = 0L
    var `appendByteString-cpu-arguments-intercept`: Long = 0L
    var `appendByteString-cpu-arguments-slope`: Long = 0L
    var `appendByteString-memory-arguments-intercept`: Long = 0L
    var `appendByteString-memory-arguments-slope`: Long = 0L
    var `appendString-cpu-arguments-intercept`: Long = 0L
    var `appendString-cpu-arguments-slope`: Long = 0L
    var `appendString-memory-arguments-intercept`: Long = 0L
    var `appendString-memory-arguments-slope`: Long = 0L
    var `bData-cpu-arguments`: Long = 0L
    var `bData-memory-arguments`: Long = 0L
    var `blake2b_256-cpu-arguments-intercept`: Long = 0L
    var `blake2b_256-cpu-arguments-slope`: Long = 0L
    var `blake2b_256-memory-arguments`: Long = 0L
    var `cekApplyCost-exBudgetCPU`: Long = 0L
    var `cekApplyCost-exBudgetMemory`: Long = 0L
    var `cekBuiltinCost-exBudgetCPU`: Long = 0L
    var `cekBuiltinCost-exBudgetMemory`: Long = 0L
    var `cekConstCost-exBudgetCPU`: Long = 0L
    var `cekConstCost-exBudgetMemory`: Long = 0L
    var `cekDelayCost-exBudgetCPU`: Long = 0L
    var `cekDelayCost-exBudgetMemory`: Long = 0L
    var `cekForceCost-exBudgetCPU`: Long = 0L
    var `cekForceCost-exBudgetMemory`: Long = 0L
    var `cekLamCost-exBudgetCPU`: Long = 0L
    var `cekLamCost-exBudgetMemory`: Long = 0L
    var `cekStartupCost-exBudgetCPU`: Long = 0L
    var `cekStartupCost-exBudgetMemory`: Long = 0L
    var `cekVarCost-exBudgetCPU`: Long = 0L
    var `cekVarCost-exBudgetMemory`: Long = 0L
    var `chooseData-cpu-arguments`: Long = 0L
    var `chooseData-memory-arguments`: Long = 0L
    var `chooseList-cpu-arguments`: Long = 0L
    var `chooseList-memory-arguments`: Long = 0L
    var `chooseUnit-cpu-arguments`: Long = 0L
    var `chooseUnit-memory-arguments`: Long = 0L
    var `consByteString-cpu-arguments-intercept`: Long = 0L
    var `consByteString-cpu-arguments-slope`: Long = 0L
    var `consByteString-memory-arguments-intercept`: Long = 0L
    var `consByteString-memory-arguments-slope`: Long = 0L
    var `constrData-cpu-arguments`: Long = 0L
    var `constrData-memory-arguments`: Long = 0L
    var `decodeUtf8-cpu-arguments-intercept`: Long = 0L
    var `decodeUtf8-cpu-arguments-slope`: Long = 0L
    var `decodeUtf8-memory-arguments-intercept`: Long = 0L
    var `decodeUtf8-memory-arguments-slope`: Long = 0L
    var `divideInteger-cpu-arguments-constant`: Long = 0L
    var `divideInteger-cpu-arguments-model-arguments-intercept`: Long = 0L
    var `divideInteger-cpu-arguments-model-arguments-slope`: Long = 0L
    var `divideInteger-memory-arguments-intercept`: Long = 0L
    var `divideInteger-memory-arguments-minimum`: Long = 0L
    var `divideInteger-memory-arguments-slope`: Long = 0L
    var `encodeUtf8-cpu-arguments-intercept`: Long = 0L
    var `encodeUtf8-cpu-arguments-slope`: Long = 0L
    var `encodeUtf8-memory-arguments-intercept`: Long = 0L
    var `encodeUtf8-memory-arguments-slope`: Long = 0L
    var `equalsByteString-cpu-arguments-constant`: Long = 0L
    var `equalsByteString-cpu-arguments-intercept`: Long = 0L
    var `equalsByteString-cpu-arguments-slope`: Long = 0L
    var `equalsByteString-memory-arguments`: Long = 0L
    var `equalsData-cpu-arguments-intercept`: Long = 0L
    var `equalsData-cpu-arguments-slope`: Long = 0L
    var `equalsData-memory-arguments`: Long = 0L
    var `equalsInteger-cpu-arguments-intercept`: Long = 0L
    var `equalsInteger-cpu-arguments-slope`: Long = 0L
    var `equalsInteger-memory-arguments`: Long = 0L
    var `equalsString-cpu-arguments-constant`: Long = 0L
    var `equalsString-cpu-arguments-intercept`: Long = 0L
    var `equalsString-cpu-arguments-slope`: Long = 0L
    var `equalsString-memory-arguments`: Long = 0L
    var `fstPair-cpu-arguments`: Long = 0L
    var `fstPair-memory-arguments`: Long = 0L
    var `headList-cpu-arguments`: Long = 0L
    var `headList-memory-arguments`: Long = 0L
    var `iData-cpu-arguments`: Long = 0L
    var `iData-memory-arguments`: Long = 0L
    var `ifThenElse-cpu-arguments`: Long = 0L
    var `ifThenElse-memory-arguments`: Long = 0L
    var `indexByteString-cpu-arguments`: Long = 0L
    var `indexByteString-memory-arguments`: Long = 0L
    var `lengthOfByteString-cpu-arguments`: Long = 0L
    var `lengthOfByteString-memory-arguments`: Long = 0L
    var `lessThanByteString-cpu-arguments-intercept`: Long = 0L
    var `lessThanByteString-cpu-arguments-slope`: Long = 0L
    var `lessThanByteString-memory-arguments`: Long = 0L
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = 0L
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = 0L
    var `lessThanEqualsByteString-memory-arguments`: Long = 0L
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = 0L
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = 0L
    var `lessThanEqualsInteger-memory-arguments`: Long = 0L
    var `lessThanInteger-cpu-arguments-intercept`: Long = 0L
    var `lessThanInteger-cpu-arguments-slope`: Long = 0L
    var `lessThanInteger-memory-arguments`: Long = 0L
    var `listData-cpu-arguments`: Long = 0L
    var `listData-memory-arguments`: Long = 0L
    var `mapData-cpu-arguments`: Long = 0L
    var `mapData-memory-arguments`: Long = 0L
    var `mkCons-cpu-arguments`: Long = 0L
    var `mkCons-memory-arguments`: Long = 0L
    var `mkNilData-cpu-arguments`: Long = 0L
    var `mkNilData-memory-arguments`: Long = 0L
    var `mkNilPairData-cpu-arguments`: Long = 0L
    var `mkNilPairData-memory-arguments`: Long = 0L
    var `mkPairData-cpu-arguments`: Long = 0L
    var `mkPairData-memory-arguments`: Long = 0L
    var `modInteger-cpu-arguments-constant`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-intercept`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-slope`: Long = 0L
    var `modInteger-memory-arguments-intercept`: Long = 0L
    var `modInteger-memory-arguments-minimum`: Long = 0L
    var `modInteger-memory-arguments-slope`: Long = 0L
    var `multiplyInteger-cpu-arguments-intercept`: Long = 0L
    var `multiplyInteger-cpu-arguments-slope`: Long = 0L
    var `multiplyInteger-memory-arguments-intercept`: Long = 0L
    var `multiplyInteger-memory-arguments-slope`: Long = 0L
    var `nullList-cpu-arguments`: Long = 0L
    var `nullList-memory-arguments`: Long = 0L
    var `quotientInteger-cpu-arguments-constant`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-intercept`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-slope`: Long = 0L
    var `quotientInteger-memory-arguments-intercept`: Long = 0L
    var `quotientInteger-memory-arguments-minimum`: Long = 0L
    var `quotientInteger-memory-arguments-slope`: Long = 0L
    var `remainderInteger-cpu-arguments-constant`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-intercept`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-slope`: Long = 0L
    var `remainderInteger-memory-arguments-intercept`: Long = 0L
    var `remainderInteger-memory-arguments-minimum`: Long = 0L
    var `remainderInteger-memory-arguments-slope`: Long = 0L
    var `sha2_256-cpu-arguments-intercept`: Long = 0L
    var `sha2_256-cpu-arguments-slope`: Long = 0L
    var `sha2_256-memory-arguments`: Long = 0L
    var `sha3_256-cpu-arguments-intercept`: Long = 0L
    var `sha3_256-cpu-arguments-slope`: Long = 0L
    var `sha3_256-memory-arguments`: Long = 0L
    var `sliceByteString-cpu-arguments-intercept`: Long = 0L
    var `sliceByteString-cpu-arguments-slope`: Long = 0L
    var `sliceByteString-memory-arguments-intercept`: Long = 0L
    var `sliceByteString-memory-arguments-slope`: Long = 0L
    var `sndPair-cpu-arguments`: Long = 0L
    var `sndPair-memory-arguments`: Long = 0L
    var `subtractInteger-cpu-arguments-intercept`: Long = 0L
    var `subtractInteger-cpu-arguments-slope`: Long = 0L
    var `subtractInteger-memory-arguments-intercept`: Long = 0L
    var `subtractInteger-memory-arguments-slope`: Long = 0L
    var `tailList-cpu-arguments`: Long = 0L
    var `tailList-memory-arguments`: Long = 0L
    var `trace-cpu-arguments`: Long = 0L
    var `trace-memory-arguments`: Long = 0L
    var `unBData-cpu-arguments`: Long = 0L
    var `unBData-memory-arguments`: Long = 0L
    var `unConstrData-cpu-arguments`: Long = 0L
    var `unConstrData-memory-arguments`: Long = 0L
    var `unIData-cpu-arguments`: Long = 0L
    var `unIData-memory-arguments`: Long = 0L
    var `unListData-cpu-arguments`: Long = 0L
    var `unListData-memory-arguments`: Long = 0L
    var `unMapData-cpu-arguments`: Long = 0L
    var `unMapData-memory-arguments`: Long = 0L
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = 0L
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = 0L
    var `verifyEd25519Signature-memory-arguments`: Long = 0L

    def toJson: String = write(this)
}

/** Plutus V2 cost model parameters.
  *
  * The names of the fields are taken from
  * [[https://github.com/input-output-hk/plutus/blob/1.40.0.0/plutus-ledger-api/src/PlutusLedgerApi/V2/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-544.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV2Params {
    var `addInteger-cpu-arguments-intercept`: Long = 0L
    var `addInteger-cpu-arguments-slope`: Long = 0L
    var `addInteger-memory-arguments-intercept`: Long = 0L
    var `addInteger-memory-arguments-slope`: Long = 0L
    var `appendByteString-cpu-arguments-intercept`: Long = 0L
    var `appendByteString-cpu-arguments-slope`: Long = 0L
    var `appendByteString-memory-arguments-intercept`: Long = 0L
    var `appendByteString-memory-arguments-slope`: Long = 0L
    var `appendString-cpu-arguments-intercept`: Long = 0L
    var `appendString-cpu-arguments-slope`: Long = 0L
    var `appendString-memory-arguments-intercept`: Long = 0L
    var `appendString-memory-arguments-slope`: Long = 0L
    var `bData-cpu-arguments`: Long = 0L
    var `bData-memory-arguments`: Long = 0L
    var `blake2b_256-cpu-arguments-intercept`: Long = 0L
    var `blake2b_256-cpu-arguments-slope`: Long = 0L
    var `blake2b_256-memory-arguments`: Long = 0L
    var `cekApplyCost-exBudgetCPU`: Long = 0L
    var `cekApplyCost-exBudgetMemory`: Long = 0L
    var `cekBuiltinCost-exBudgetCPU`: Long = 0L
    var `cekBuiltinCost-exBudgetMemory`: Long = 0L
    var `cekConstCost-exBudgetCPU`: Long = 0L
    var `cekConstCost-exBudgetMemory`: Long = 0L
    var `cekDelayCost-exBudgetCPU`: Long = 0L
    var `cekDelayCost-exBudgetMemory`: Long = 0L
    var `cekForceCost-exBudgetCPU`: Long = 0L
    var `cekForceCost-exBudgetMemory`: Long = 0L
    var `cekLamCost-exBudgetCPU`: Long = 0L
    var `cekLamCost-exBudgetMemory`: Long = 0L
    var `cekStartupCost-exBudgetCPU`: Long = 0L
    var `cekStartupCost-exBudgetMemory`: Long = 0L
    var `cekVarCost-exBudgetCPU`: Long = 0L
    var `cekVarCost-exBudgetMemory`: Long = 0L
    var `chooseData-cpu-arguments`: Long = 0L
    var `chooseData-memory-arguments`: Long = 0L
    var `chooseList-cpu-arguments`: Long = 0L
    var `chooseList-memory-arguments`: Long = 0L
    var `chooseUnit-cpu-arguments`: Long = 0L
    var `chooseUnit-memory-arguments`: Long = 0L
    var `consByteString-cpu-arguments-intercept`: Long = 0L
    var `consByteString-cpu-arguments-slope`: Long = 0L
    var `consByteString-memory-arguments-intercept`: Long = 0L
    var `consByteString-memory-arguments-slope`: Long = 0L
    var `constrData-cpu-arguments`: Long = 0L
    var `constrData-memory-arguments`: Long = 0L
    var `decodeUtf8-cpu-arguments-intercept`: Long = 0L
    var `decodeUtf8-cpu-arguments-slope`: Long = 0L
    var `decodeUtf8-memory-arguments-intercept`: Long = 0L
    var `decodeUtf8-memory-arguments-slope`: Long = 0L
    var `divideInteger-cpu-arguments-constant`: Long = 0L
    var `divideInteger-cpu-arguments-model-arguments-intercept`: Long = 0L
    var `divideInteger-cpu-arguments-model-arguments-slope`: Long = 0L
    var `divideInteger-memory-arguments-intercept`: Long = 0L
    var `divideInteger-memory-arguments-minimum`: Long = 0L
    var `divideInteger-memory-arguments-slope`: Long = 0L
    var `encodeUtf8-cpu-arguments-intercept`: Long = 0L
    var `encodeUtf8-cpu-arguments-slope`: Long = 0L
    var `encodeUtf8-memory-arguments-intercept`: Long = 0L
    var `encodeUtf8-memory-arguments-slope`: Long = 0L
    var `equalsByteString-cpu-arguments-constant`: Long = 0L
    var `equalsByteString-cpu-arguments-intercept`: Long = 0L
    var `equalsByteString-cpu-arguments-slope`: Long = 0L
    var `equalsByteString-memory-arguments`: Long = 0L
    var `equalsData-cpu-arguments-intercept`: Long = 0L
    var `equalsData-cpu-arguments-slope`: Long = 0L
    var `equalsData-memory-arguments`: Long = 0L
    var `equalsInteger-cpu-arguments-intercept`: Long = 0L
    var `equalsInteger-cpu-arguments-slope`: Long = 0L
    var `equalsInteger-memory-arguments`: Long = 0L
    var `equalsString-cpu-arguments-constant`: Long = 0L
    var `equalsString-cpu-arguments-intercept`: Long = 0L
    var `equalsString-cpu-arguments-slope`: Long = 0L
    var `equalsString-memory-arguments`: Long = 0L
    var `fstPair-cpu-arguments`: Long = 0L
    var `fstPair-memory-arguments`: Long = 0L
    var `headList-cpu-arguments`: Long = 0L
    var `headList-memory-arguments`: Long = 0L
    var `iData-cpu-arguments`: Long = 0L
    var `iData-memory-arguments`: Long = 0L
    var `ifThenElse-cpu-arguments`: Long = 0L
    var `ifThenElse-memory-arguments`: Long = 0L
    var `indexByteString-cpu-arguments`: Long = 0L
    var `indexByteString-memory-arguments`: Long = 0L
    var `lengthOfByteString-cpu-arguments`: Long = 0L
    var `lengthOfByteString-memory-arguments`: Long = 0L
    var `lessThanByteString-cpu-arguments-intercept`: Long = 0L
    var `lessThanByteString-cpu-arguments-slope`: Long = 0L
    var `lessThanByteString-memory-arguments`: Long = 0L
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = 0L
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = 0L
    var `lessThanEqualsByteString-memory-arguments`: Long = 0L
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = 0L
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = 0L
    var `lessThanEqualsInteger-memory-arguments`: Long = 0L
    var `lessThanInteger-cpu-arguments-intercept`: Long = 0L
    var `lessThanInteger-cpu-arguments-slope`: Long = 0L
    var `lessThanInteger-memory-arguments`: Long = 0L
    var `listData-cpu-arguments`: Long = 0L
    var `listData-memory-arguments`: Long = 0L
    var `mapData-cpu-arguments`: Long = 0L
    var `mapData-memory-arguments`: Long = 0L
    var `mkCons-cpu-arguments`: Long = 0L
    var `mkCons-memory-arguments`: Long = 0L
    var `mkNilData-cpu-arguments`: Long = 0L
    var `mkNilData-memory-arguments`: Long = 0L
    var `mkNilPairData-cpu-arguments`: Long = 0L
    var `mkNilPairData-memory-arguments`: Long = 0L
    var `mkPairData-cpu-arguments`: Long = 0L
    var `mkPairData-memory-arguments`: Long = 0L
    var `modInteger-cpu-arguments-constant`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-intercept`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-slope`: Long = 0L
    var `modInteger-memory-arguments-intercept`: Long = 0L
    var `modInteger-memory-arguments-minimum`: Long = 0L
    var `modInteger-memory-arguments-slope`: Long = 0L
    var `multiplyInteger-cpu-arguments-intercept`: Long = 0L
    var `multiplyInteger-cpu-arguments-slope`: Long = 0L
    var `multiplyInteger-memory-arguments-intercept`: Long = 0L
    var `multiplyInteger-memory-arguments-slope`: Long = 0L
    var `nullList-cpu-arguments`: Long = 0L
    var `nullList-memory-arguments`: Long = 0L
    var `quotientInteger-cpu-arguments-constant`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-intercept`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-slope`: Long = 0L
    var `quotientInteger-memory-arguments-intercept`: Long = 0L
    var `quotientInteger-memory-arguments-minimum`: Long = 0L
    var `quotientInteger-memory-arguments-slope`: Long = 0L
    var `remainderInteger-cpu-arguments-constant`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-intercept`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-slope`: Long = 0L
    var `remainderInteger-memory-arguments-intercept`: Long = 0L
    var `remainderInteger-memory-arguments-minimum`: Long = 0L
    var `remainderInteger-memory-arguments-slope`: Long = 0L
    var `serialiseData-cpu-arguments-intercept`: Long = 0L
    var `serialiseData-cpu-arguments-slope`: Long = 0L
    var `serialiseData-memory-arguments-intercept`: Long = 0L
    var `serialiseData-memory-arguments-slope`: Long = 0L
    var `sha2_256-cpu-arguments-intercept`: Long = 0L
    var `sha2_256-cpu-arguments-slope`: Long = 0L
    var `sha2_256-memory-arguments`: Long = 0L
    var `sha3_256-cpu-arguments-intercept`: Long = 0L
    var `sha3_256-cpu-arguments-slope`: Long = 0L
    var `sha3_256-memory-arguments`: Long = 0L
    var `sliceByteString-cpu-arguments-intercept`: Long = 0L
    var `sliceByteString-cpu-arguments-slope`: Long = 0L
    var `sliceByteString-memory-arguments-intercept`: Long = 0L
    var `sliceByteString-memory-arguments-slope`: Long = 0L
    var `sndPair-cpu-arguments`: Long = 0L
    var `sndPair-memory-arguments`: Long = 0L
    var `subtractInteger-cpu-arguments-intercept`: Long = 0L
    var `subtractInteger-cpu-arguments-slope`: Long = 0L
    var `subtractInteger-memory-arguments-intercept`: Long = 0L
    var `subtractInteger-memory-arguments-slope`: Long = 0L
    var `tailList-cpu-arguments`: Long = 0L
    var `tailList-memory-arguments`: Long = 0L
    var `trace-cpu-arguments`: Long = 0L
    var `trace-memory-arguments`: Long = 0L
    var `unBData-cpu-arguments`: Long = 0L
    var `unBData-memory-arguments`: Long = 0L
    var `unConstrData-cpu-arguments`: Long = 0L
    var `unConstrData-memory-arguments`: Long = 0L
    var `unIData-cpu-arguments`: Long = 0L
    var `unIData-memory-arguments`: Long = 0L
    var `unListData-cpu-arguments`: Long = 0L
    var `unListData-memory-arguments`: Long = 0L
    var `unMapData-cpu-arguments`: Long = 0L
    var `unMapData-memory-arguments`: Long = 0L
    var `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long = 0L
    var `verifyEcdsaSecp256k1Signature-memory-arguments`: Long = 0L
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = 0L
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = 0L
    var `verifyEd25519Signature-memory-arguments`: Long = 0L
    var `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long = 0L
    var `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long = 0L
    var `verifySchnorrSecp256k1Signature-memory-arguments`: Long = 0L

    def toJson: String = write(this)
}

/** Plutus V3 cost model parameters.
  *
  * The names of the fields are taken from
  * [[https://github.com/input-output-hk/plutus/blob/1.40.0.0/plutus-ledger-api/src/PlutusLedgerApi/V3/ParamName.hs]]
  * and Blockfrost Protocol Parameters JSON uses these names as well in
  * `blockfrost-params-epoch-544.json`
  *
  * But what's really important is the order of the fields because that's the order of the
  * parameters in the protocol parameters array.
  */
class PlutusV3Params {
    var `addInteger-cpu-arguments-intercept`: Long = 0L
    var `addInteger-cpu-arguments-slope`: Long = 0L
    var `addInteger-memory-arguments-intercept`: Long = 0L
    var `addInteger-memory-arguments-slope`: Long = 0L
    var `appendByteString-cpu-arguments-intercept`: Long = 0L
    var `appendByteString-cpu-arguments-slope`: Long = 0L
    var `appendByteString-memory-arguments-intercept`: Long = 0L
    var `appendByteString-memory-arguments-slope`: Long = 0L
    var `appendString-cpu-arguments-intercept`: Long = 0L
    var `appendString-cpu-arguments-slope`: Long = 0L
    var `appendString-memory-arguments-intercept`: Long = 0L
    var `appendString-memory-arguments-slope`: Long = 0L
    var `bData-cpu-arguments`: Long = 0L
    var `bData-memory-arguments`: Long = 0L
    var `blake2b_256-cpu-arguments-intercept`: Long = 0L
    var `blake2b_256-cpu-arguments-slope`: Long = 0L
    var `blake2b_256-memory-arguments`: Long = 0L
    var `cekApplyCost-exBudgetCPU`: Long = 0L
    var `cekApplyCost-exBudgetMemory`: Long = 0L
    var `cekBuiltinCost-exBudgetCPU`: Long = 0L
    var `cekBuiltinCost-exBudgetMemory`: Long = 0L
    var `cekConstCost-exBudgetCPU`: Long = 0L
    var `cekConstCost-exBudgetMemory`: Long = 0L
    var `cekDelayCost-exBudgetCPU`: Long = 0L
    var `cekDelayCost-exBudgetMemory`: Long = 0L
    var `cekForceCost-exBudgetCPU`: Long = 0L
    var `cekForceCost-exBudgetMemory`: Long = 0L
    var `cekLamCost-exBudgetCPU`: Long = 0L
    var `cekLamCost-exBudgetMemory`: Long = 0L
    var `cekStartupCost-exBudgetCPU`: Long = 0L
    var `cekStartupCost-exBudgetMemory`: Long = 0L
    var `cekVarCost-exBudgetCPU`: Long = 0L
    var `cekVarCost-exBudgetMemory`: Long = 0L
    var `chooseData-cpu-arguments`: Long = 0L
    var `chooseData-memory-arguments`: Long = 0L
    var `chooseList-cpu-arguments`: Long = 0L
    var `chooseList-memory-arguments`: Long = 0L
    var `chooseUnit-cpu-arguments`: Long = 0L
    var `chooseUnit-memory-arguments`: Long = 0L
    var `consByteString-cpu-arguments-intercept`: Long = 0L
    var `consByteString-cpu-arguments-slope`: Long = 0L
    var `consByteString-memory-arguments-intercept`: Long = 0L
    var `consByteString-memory-arguments-slope`: Long = 0L
    var `constrData-cpu-arguments`: Long = 0L
    var `constrData-memory-arguments`: Long = 0L
    var `decodeUtf8-cpu-arguments-intercept`: Long = 0L
    var `decodeUtf8-cpu-arguments-slope`: Long = 0L
    var `decodeUtf8-memory-arguments-intercept`: Long = 0L
    var `decodeUtf8-memory-arguments-slope`: Long = 0L
    var `divideInteger-cpu-arguments-constant`: Long = 0L
    var `divideInteger-cpu-arguments-c00`: Long = 0L
    var `divideInteger-cpu-arguments-c01`: Long = 0L
    var `divideInteger-cpu-arguments-c02`: Long = 0L
    var `divideInteger-cpu-arguments-c10`: Long = 0L
    var `divideInteger-cpu-arguments-c11`: Long = 0L
    var `divideInteger-cpu-arguments-c20`: Long = 0L
    var `divideInteger-cpu-arguments-minimum`: Long = 0L
    var `divideInteger-memory-arguments-intercept`: Long = 0L
    var `divideInteger-memory-arguments-minimum`: Long = 0L
    var `divideInteger-memory-arguments-slope`: Long = 0L
    var `encodeUtf8-cpu-arguments-intercept`: Long = 0L
    var `encodeUtf8-cpu-arguments-slope`: Long = 0L
    var `encodeUtf8-memory-arguments-intercept`: Long = 0L
    var `encodeUtf8-memory-arguments-slope`: Long = 0L
    var `equalsByteString-cpu-arguments-constant`: Long = 0L
    var `equalsByteString-cpu-arguments-intercept`: Long = 0L
    var `equalsByteString-cpu-arguments-slope`: Long = 0L
    var `equalsByteString-memory-arguments`: Long = 0L
    var `equalsData-cpu-arguments-intercept`: Long = 0L
    var `equalsData-cpu-arguments-slope`: Long = 0L
    var `equalsData-memory-arguments`: Long = 0L
    var `equalsInteger-cpu-arguments-intercept`: Long = 0L
    var `equalsInteger-cpu-arguments-slope`: Long = 0L
    var `equalsInteger-memory-arguments`: Long = 0L
    var `equalsString-cpu-arguments-constant`: Long = 0L
    var `equalsString-cpu-arguments-intercept`: Long = 0L
    var `equalsString-cpu-arguments-slope`: Long = 0L
    var `equalsString-memory-arguments`: Long = 0L
    var `fstPair-cpu-arguments`: Long = 0L
    var `fstPair-memory-arguments`: Long = 0L
    var `headList-cpu-arguments`: Long = 0L
    var `headList-memory-arguments`: Long = 0L
    var `iData-cpu-arguments`: Long = 0L
    var `iData-memory-arguments`: Long = 0L
    var `ifThenElse-cpu-arguments`: Long = 0L
    var `ifThenElse-memory-arguments`: Long = 0L
    var `indexByteString-cpu-arguments`: Long = 0L
    var `indexByteString-memory-arguments`: Long = 0L
    var `lengthOfByteString-cpu-arguments`: Long = 0L
    var `lengthOfByteString-memory-arguments`: Long = 0L
    var `lessThanByteString-cpu-arguments-intercept`: Long = 0L
    var `lessThanByteString-cpu-arguments-slope`: Long = 0L
    var `lessThanByteString-memory-arguments`: Long = 0L
    var `lessThanEqualsByteString-cpu-arguments-intercept`: Long = 0L
    var `lessThanEqualsByteString-cpu-arguments-slope`: Long = 0L
    var `lessThanEqualsByteString-memory-arguments`: Long = 0L
    var `lessThanEqualsInteger-cpu-arguments-intercept`: Long = 0L
    var `lessThanEqualsInteger-cpu-arguments-slope`: Long = 0L
    var `lessThanEqualsInteger-memory-arguments`: Long = 0L
    var `lessThanInteger-cpu-arguments-intercept`: Long = 0L
    var `lessThanInteger-cpu-arguments-slope`: Long = 0L
    var `lessThanInteger-memory-arguments`: Long = 0L
    var `listData-cpu-arguments`: Long = 0L
    var `listData-memory-arguments`: Long = 0L
    var `mapData-cpu-arguments`: Long = 0L
    var `mapData-memory-arguments`: Long = 0L
    var `mkCons-cpu-arguments`: Long = 0L
    var `mkCons-memory-arguments`: Long = 0L
    var `mkNilData-cpu-arguments`: Long = 0L
    var `mkNilData-memory-arguments`: Long = 0L
    var `mkNilPairData-cpu-arguments`: Long = 0L
    var `mkNilPairData-memory-arguments`: Long = 0L
    var `mkPairData-cpu-arguments`: Long = 0L
    var `mkPairData-memory-arguments`: Long = 0L
    var `modInteger-cpu-arguments-constant`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c00`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c01`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c02`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c10`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c11`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c20`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-minimum`: Long = 0L
    var `modInteger-memory-arguments-intercept`: Long = 0L
    var `modInteger-memory-arguments-slope`: Long = 0L
    var `multiplyInteger-cpu-arguments-intercept`: Long = 0L
    var `multiplyInteger-cpu-arguments-slope`: Long = 0L
    var `multiplyInteger-memory-arguments-intercept`: Long = 0L
    var `multiplyInteger-memory-arguments-slope`: Long = 0L
    var `nullList-cpu-arguments`: Long = 0L
    var `nullList-memory-arguments`: Long = 0L
    var `quotientInteger-cpu-arguments-constant`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c00`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c01`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c02`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c10`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c11`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c20`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-minimum`: Long = 0L
    var `quotientInteger-memory-arguments-intercept`: Long = 0L
    var `quotientInteger-memory-arguments-minimum`: Long = 0L
    var `quotientInteger-memory-arguments-slope`: Long = 0L
    var `remainderInteger-cpu-arguments-constant`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c00`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c01`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c02`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c10`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c11`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c20`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-minimum`: Long = 0L
    var `remainderInteger-memory-arguments-intercept`: Long = 0L
    var `remainderInteger-memory-arguments-slope`: Long = 0L
    var `serialiseData-cpu-arguments-intercept`: Long = 0L
    var `serialiseData-cpu-arguments-slope`: Long = 0L
    var `serialiseData-memory-arguments-intercept`: Long = 0L
    var `serialiseData-memory-arguments-slope`: Long = 0L
    var `sha2_256-cpu-arguments-intercept`: Long = 0L
    var `sha2_256-cpu-arguments-slope`: Long = 0L
    var `sha2_256-memory-arguments`: Long = 0L
    var `sha3_256-cpu-arguments-intercept`: Long = 0L
    var `sha3_256-cpu-arguments-slope`: Long = 0L
    var `sha3_256-memory-arguments`: Long = 0L
    var `sliceByteString-cpu-arguments-intercept`: Long = 0L
    var `sliceByteString-cpu-arguments-slope`: Long = 0L
    var `sliceByteString-memory-arguments-intercept`: Long = 0L
    var `sliceByteString-memory-arguments-slope`: Long = 0L
    var `sndPair-cpu-arguments`: Long = 0L
    var `sndPair-memory-arguments`: Long = 0L
    var `subtractInteger-cpu-arguments-intercept`: Long = 0L
    var `subtractInteger-cpu-arguments-slope`: Long = 0L
    var `subtractInteger-memory-arguments-intercept`: Long = 0L
    var `subtractInteger-memory-arguments-slope`: Long = 0L
    var `tailList-cpu-arguments`: Long = 0L
    var `tailList-memory-arguments`: Long = 0L
    var `trace-cpu-arguments`: Long = 0L
    var `trace-memory-arguments`: Long = 0L
    var `unBData-cpu-arguments`: Long = 0L
    var `unBData-memory-arguments`: Long = 0L
    var `unConstrData-cpu-arguments`: Long = 0L
    var `unConstrData-memory-arguments`: Long = 0L
    var `unIData-cpu-arguments`: Long = 0L
    var `unIData-memory-arguments`: Long = 0L
    var `unListData-cpu-arguments`: Long = 0L
    var `unListData-memory-arguments`: Long = 0L
    var `unMapData-cpu-arguments`: Long = 0L
    var `unMapData-memory-arguments`: Long = 0L
    var `verifyEcdsaSecp256k1Signature-cpu-arguments`: Long = 0L
    var `verifyEcdsaSecp256k1Signature-memory-arguments`: Long = 0L
    var `verifyEd25519Signature-cpu-arguments-intercept`: Long = 0L
    var `verifyEd25519Signature-cpu-arguments-slope`: Long = 0L
    var `verifyEd25519Signature-memory-arguments`: Long = 0L
    var `verifySchnorrSecp256k1Signature-cpu-arguments-intercept`: Long = 0L
    var `verifySchnorrSecp256k1Signature-cpu-arguments-slope`: Long = 0L
    var `verifySchnorrSecp256k1Signature-memory-arguments`: Long = 0L
    var `cekConstrCost-exBudgetCPU`: Long = 0L
    var `cekConstrCost-exBudgetMemory`: Long = 0L
    var `cekCaseCost-exBudgetCPU`: Long = 0L
    var `cekCaseCost-exBudgetMemory`: Long = 0L
    var `bls12_381_G1_add-cpu-arguments`: Long = 0L
    var `bls12_381_G1_add-memory-arguments`: Long = 0L
    var `bls12_381_G1_compress-cpu-arguments`: Long = 0L
    var `bls12_381_G1_compress-memory-arguments`: Long = 0L
    var `bls12_381_G1_equal-cpu-arguments`: Long = 0L
    var `bls12_381_G1_equal-memory-arguments`: Long = 0L
    var `bls12_381_G1_hashToGroup-cpu-arguments-intercept`: Long = 0L
    var `bls12_381_G1_hashToGroup-cpu-arguments-slope`: Long = 0L
    var `bls12_381_G1_hashToGroup-memory-arguments`: Long = 0L
    var `bls12_381_G1_neg-cpu-arguments`: Long = 0L
    var `bls12_381_G1_neg-memory-arguments`: Long = 0L
    var `bls12_381_G1_scalarMul-cpu-arguments-intercept`: Long = 0L
    var `bls12_381_G1_scalarMul-cpu-arguments-slope`: Long = 0L
    var `bls12_381_G1_scalarMul-memory-arguments`: Long = 0L
    var `bls12_381_G1_uncompress-cpu-arguments`: Long = 0L
    var `bls12_381_G1_uncompress-memory-arguments`: Long = 0L
    var `bls12_381_G2_add-cpu-arguments`: Long = 0L
    var `bls12_381_G2_add-memory-arguments`: Long = 0L
    var `bls12_381_G2_compress-cpu-arguments`: Long = 0L
    var `bls12_381_G2_compress-memory-arguments`: Long = 0L
    var `bls12_381_G2_equal-cpu-arguments`: Long = 0L
    var `bls12_381_G2_equal-memory-arguments`: Long = 0L
    var `bls12_381_G2_hashToGroup-cpu-arguments-intercept`: Long = 0L
    var `bls12_381_G2_hashToGroup-cpu-arguments-slope`: Long = 0L
    var `bls12_381_G2_hashToGroup-memory-arguments`: Long = 0L
    var `bls12_381_G2_neg-cpu-arguments`: Long = 0L
    var `bls12_381_G2_neg-memory-arguments`: Long = 0L
    var `bls12_381_G2_scalarMul-cpu-arguments-intercept`: Long = 0L
    var `bls12_381_G2_scalarMul-cpu-arguments-slope`: Long = 0L
    var `bls12_381_G2_scalarMul-memory-arguments`: Long = 0L
    var `bls12_381_G2_uncompress-cpu-arguments`: Long = 0L
    var `bls12_381_G2_uncompress-memory-arguments`: Long = 0L
    var `bls12_381_finalVerify-cpu-arguments`: Long = 0L
    var `bls12_381_finalVerify-memory-arguments`: Long = 0L
    var `bls12_381_millerLoop-cpu-arguments`: Long = 0L
    var `bls12_381_millerLoop-memory-arguments`: Long = 0L
    var `bls12_381_mulMlResult-cpu-arguments`: Long = 0L
    var `bls12_381_mulMlResult-memory-arguments`: Long = 0L
    var `keccak_256-cpu-arguments-intercept`: Long = 0L
    var `keccak_256-cpu-arguments-slope`: Long = 0L
    var `keccak_256-memory-arguments`: Long = 0L
    var `blake2b_224-cpu-arguments-intercept`: Long = 0L
    var `blake2b_224-cpu-arguments-slope`: Long = 0L
    var `blake2b_224-memory-arguments`: Long = 0L
    var `integerToByteString-cpu-arguments-c0`: Long = 0L
    var `integerToByteString-cpu-arguments-c1`: Long = 0L
    var `integerToByteString-cpu-arguments-c2`: Long = 0L
    var `integerToByteString-memory-arguments-intercept`: Long = 0L
    var `integerToByteString-memory-arguments-slope`: Long = 0L
    var `byteStringToInteger-cpu-arguments-c0`: Long = 0L
    var `byteStringToInteger-cpu-arguments-c1`: Long = 0L
    var `byteStringToInteger-cpu-arguments-c2`: Long = 0L
    var `byteStringToInteger-memory-arguments-intercept`: Long = 0L
    var `byteStringToInteger-memory-arguments-slope`: Long = 0L
    var `andByteString-cpu-arguments-intercept`: Long = 0L
    var `andByteString-cpu-arguments-slope1`: Long = 0L
    var `andByteString-cpu-arguments-slope2`: Long = 0L
    var `andByteString-memory-arguments-intercept`: Long = 0L
    var `andByteString-memory-arguments-slope`: Long = 0L
    var `orByteString-cpu-arguments-intercept`: Long = 0L
    var `orByteString-cpu-arguments-slope1`: Long = 0L
    var `orByteString-cpu-arguments-slope2`: Long = 0L
    var `orByteString-memory-arguments-intercept`: Long = 0L
    var `orByteString-memory-arguments-slope`: Long = 0L
    var `xorByteString-cpu-arguments-intercept`: Long = 0L
    var `xorByteString-cpu-arguments-slope1`: Long = 0L
    var `xorByteString-cpu-arguments-slope2`: Long = 0L
    var `xorByteString-memory-arguments-intercept`: Long = 0L
    var `xorByteString-memory-arguments-slope`: Long = 0L
    var `complementByteString-cpu-arguments-intercept`: Long = 0L
    var `complementByteString-cpu-arguments-slope`: Long = 0L
    var `complementByteString-memory-arguments-intercept`: Long = 0L
    var `complementByteString-memory-arguments-slope`: Long = 0L
    var `readBit-cpu-arguments`: Long = 0L
    var `readBit-memory-arguments`: Long = 0L
    var `writeBits-cpu-arguments-intercept`: Long = 0L
    var `writeBits-cpu-arguments-slope`: Long = 0L
    var `writeBits-memory-arguments-intercept`: Long = 0L
    var `writeBits-memory-arguments-slope`: Long = 0L
    var `replicateByte-cpu-arguments-intercept`: Long = 0L
    var `replicateByte-cpu-arguments-slope`: Long = 0L
    var `replicateByte-memory-arguments-intercept`: Long = 0L
    var `replicateByte-memory-arguments-slope`: Long = 0L
    var `shiftByteString-cpu-arguments-intercept`: Long = 0L
    var `shiftByteString-cpu-arguments-slope`: Long = 0L
    var `shiftByteString-memory-arguments-intercept`: Long = 0L
    var `shiftByteString-memory-arguments-slope`: Long = 0L
    var `rotateByteString-cpu-arguments-intercept`: Long = 0L
    var `rotateByteString-cpu-arguments-slope`: Long = 0L
    var `rotateByteString-memory-arguments-intercept`: Long = 0L
    var `rotateByteString-memory-arguments-slope`: Long = 0L
    var `countSetBits-cpu-arguments-intercept`: Long = 0L
    var `countSetBits-cpu-arguments-slope`: Long = 0L
    var `countSetBits-memory-arguments`: Long = 0L
    var `findFirstSetBit-cpu-arguments-intercept`: Long = 0L
    var `findFirstSetBit-cpu-arguments-slope`: Long = 0L
    var `findFirstSetBit-memory-arguments`: Long = 0L
    var `ripemd_160-cpu-arguments-intercept`: Long = 0L
    var `ripemd_160-cpu-arguments-slope`: Long = 0L
    var `ripemd_160-memory-arguments`: Long = 0L

    def toJson: String = write(this)
}

private object JsonUtils {

    /** Generates a [[ReadWriter]] for a class with fields that are not private
      *
      * @tparam A
      *   the type of the class
      *
      * @example
      *   {{{
      *  class Foo {
      *   var a: Int = 0
      *  }
      *  given rw = mkClassFieldsReadWriter[Foo]
      *   }}}
      */
    inline def mkClassFieldsReadWriter[A]: ReadWriter[A] = ${
        scalus.macros.Macros.mkReadWriterImpl[A]
    }

    /** Generates a pair of functions to convert a class with fields to a sequence of longs and back
      * @example
      *   {{{
      *  class Foo {
      *  var a: Int = 0
      *  }
      *  val (toSeq, fromSeq) = mkClassFieldsFromSeqIso[Foo]
      *   }}}
      *   where `fromSeq` looks like this
      *   {{{
      *  val foo = new Foo()
      *  foo.a = seq(0)
      *  foo
      *   }}}
      */
    inline def mkClassFieldsFromSeqIso[A]: (A => Seq[Long], Seq[Long] => A) = ${
        scalus.macros.Macros.mkClassFieldsFromSeqIsoImpl[A]
    }
}

object PlutusV1Params:
    given ReadWriter[PlutusV1Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV1Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV1Params]

object PlutusV2Params:
    given ReadWriter[PlutusV2Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV2Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV2Params]

object PlutusV3Params:
    given ReadWriter[PlutusV3Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV3Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV3Params]
