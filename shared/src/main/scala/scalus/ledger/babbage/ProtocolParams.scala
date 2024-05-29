package scalus.ledger.babbage
import upickle.default.*
import scala.quoted.Quotes

import upickle.default.ReadWriter

case class ProtocolVersion(major: Int, minor: Int) derives ReadWriter

case class MaxTxExecutionUnits(memory: Long, steps: Long) derives ReadWriter

case class MaxBlockExecutionUnits(memory: Long, steps: Long) derives ReadWriter

case class ExecutionUnitPrices(priceMemory: Double, priceSteps: Double) derives ReadWriter

/** Protocol parameters for the Cardano blockchain of Babbage era Field names are taken from the
  * `cardano-cli query protocol-parameters` output
  * @note
  *   These names are different from CIP-55, don't ask me why.
  */
case class ProtocolParams(
    collateralPercentage: Long,
    costModels: Map[String, Seq[Long]],
    decentralization: Option[Double],
    executionUnitPrices: ExecutionUnitPrices,
    extraPraosEntropy: Option[String],
    maxBlockBodySize: Long,
    maxBlockExecutionUnits: MaxBlockExecutionUnits,
    maxBlockHeaderSize: Long,
    maxCollateralInputs: Long,
    maxTxExecutionUnits: MaxTxExecutionUnits,
    maxTxSize: Long,
    maxValueSize: Long,
    minPoolCost: Long,
    minUTxOValue: Option[Long],
    monetaryExpansion: Double,
    poolPledgeInfluence: Double,
    poolRetireMaxEpoch: Long,
    protocolVersion: ProtocolVersion,
    stakeAddressDeposit: Long,
    stakePoolDeposit: Long,
    stakePoolTargetNum: Long,
    treasuryCut: Double,
    txFeeFixed: Long,
    txFeePerByte: Long,
    utxoCostPerByte: Long,
    utxoCostPerWord: Option[Long]
) derives ReadWriter

object ProtocolParams {
    import upickle.default.{readwriter, ReadWriter}
    val blockfrostParamsRW: Reader[ProtocolParams] =
        readwriter[ujson.Value].bimap[ProtocolParams](
          params =>
              ujson.Obj(
                "min_fee_a" -> params.txFeePerByte,
                "min_fee_b" -> params.txFeeFixed,
                "max_block_size" -> params.maxBlockBodySize,
                "max_tx_size" -> params.maxTxSize,
                "max_block_header_size" -> params.maxBlockHeaderSize,
                "key_deposit" -> params.stakeAddressDeposit.toString,
                "pool_deposit" -> params.stakePoolDeposit.toString,
                "e_max" -> params.poolRetireMaxEpoch,
                "n_opt" -> params.stakePoolTargetNum,
                "a0" -> params.poolPledgeInfluence,
                "rho" -> params.monetaryExpansion,
                "tau" -> params.treasuryCut,
                "decentralisation_param" -> 0,
                "extra_entropy" -> params.extraPraosEntropy,
                "protocol_major_ver" -> params.protocolVersion.major,
                "protocol_minor_ver" -> params.protocolVersion.minor,
                "min_utxo" -> params.utxoCostPerByte.toString,
                "min_pool_cost" -> params.minPoolCost.toString,
                "nonce" -> null,
                "price_mem" -> params.executionUnitPrices.priceMemory,
                "price_step" -> params.executionUnitPrices.priceSteps,
                "max_tx_ex_mem" -> params.maxTxExecutionUnits.memory.toString,
                "max_tx_ex_steps" -> params.maxTxExecutionUnits.steps.toString,
                "max_block_ex_mem" -> params.maxBlockExecutionUnits.memory.toString,
                "max_block_ex_steps" -> params.maxBlockExecutionUnits.steps.toString,
                "max_val_size" -> params.maxValueSize,
                "collateral_percent" -> params.collateralPercentage,
                "max_collateral_inputs" -> params.maxCollateralInputs,
                "coins_per_utxo_size" -> params.utxoCostPerByte.toString,
                "coins_per_utxo_word" -> params.utxoCostPerWord.map(_.toString).getOrElse(null)
              ),
          json =>
              ProtocolParams(
                collateralPercentage = json("collateral_percent").num.toLong,
                costModels = json("cost_models").obj.map { case (k, v) =>
                    k -> v.obj.values.map(_.num.toLong).toSeq
                }.toMap,
                decentralization = None,
                executionUnitPrices = ExecutionUnitPrices(
                  priceMemory = json("price_mem").num,
                  priceSteps = json("price_step").num
                ),
                extraPraosEntropy = None,
                maxBlockBodySize = json("max_block_size").num.toLong,
                maxBlockExecutionUnits = MaxBlockExecutionUnits(
                  memory = json("max_block_ex_mem").str.toLong,
                  steps = json("max_block_ex_steps").str.toLong
                ),
                maxBlockHeaderSize = json("max_block_header_size").num.toLong,
                maxCollateralInputs = json("max_collateral_inputs").num.toLong,
                maxTxExecutionUnits = MaxTxExecutionUnits(
                  memory = json("max_tx_ex_mem").str.toLong,
                  steps = json("max_tx_ex_steps").str.toLong
                ),
                maxTxSize = json("max_tx_size").num.toLong,
                maxValueSize = json("max_val_size").str.toLong,
                minPoolCost = json("min_pool_cost").str.toLong,
                minUTxOValue = None,
                monetaryExpansion = json("rho").num,
                poolPledgeInfluence = json("a0").num,
                poolRetireMaxEpoch = json("e_max").num.toLong,
                protocolVersion = ProtocolVersion(
                  major = json("protocol_major_ver").num.toInt,
                  minor = json("protocol_minor_ver").num.toInt
                ),
                stakeAddressDeposit = json("key_deposit").str.toLong,
                stakePoolDeposit = json("pool_deposit").str.toLong,
                stakePoolTargetNum = json("n_opt").num.toLong,
                treasuryCut = json("tau").num,
                txFeeFixed = json("min_fee_b").num.toLong,
                txFeePerByte = json("min_fee_a").num.toLong,
                utxoCostPerByte = json("min_utxo").str.toLong,
                utxoCostPerWord = None
              )
        )
}

/*
  Funny thing is that JVM has a limit of 255 parameters in a method if the args are Ints.
  If it's Long, than the limit is 127.
  And we can't generate a constructor call for `PlutusV1Params` or `PlutusV2Params`
  which has more than 127 parameters.
  So I'm using Ints here, and that should be enough for the protocol parameters.
  Then, I've changed the `PlutusV1Params` and `PlutusV2Params` to have Longs
  and be a class with public fields.
  I also added a `JsonUtils` object to generate a `ReadWriter` for these classes.
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
}

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
      */
    inline def mkClassFieldsFromSeqIso[A]: (A => Seq[Long], Seq[Long] => A) = ${
        scalus.macros.Macros.mkClassFieldsFromSeqIsoImpl[A]
    }
}

object PlutusV1Params:

    given ReadWriter[PlutusV1Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV1Params]
    // given ReadWriter[PlutusV1Params] = readwriter[ujson.Value].bimap(params => ujson.Obj(), json => new PlutusV1Params)

    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV1Params]

object PlutusV2Params:
    given ReadWriter[PlutusV2Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV2Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV2Params]
