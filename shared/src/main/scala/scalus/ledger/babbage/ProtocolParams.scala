package scalus.ledger.babbage
import scalus.ledger.api.ProtocolVersion
import upickle.default.*
import upickle.default.ReadWriter

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
    committeeMaxTermLength: Long,
    committeeMinSize: Long,
    costModels: Map[String, Seq[Long]],
    dRepActivity: Long,
    dRepDeposit: Long,
    dRepVotingThresholds: DRepVotingThresholds,
    executionUnitPrices: ExecutionUnitPrices,
    govActionDeposit: Long,
    govActionLifetime: Long,
    maxBlockBodySize: Long,
    maxBlockExecutionUnits: MaxBlockExecutionUnits,
    maxBlockHeaderSize: Long,
    maxCollateralInputs: Long,
    maxTxExecutionUnits: MaxTxExecutionUnits,
    maxTxSize: Long,
    maxValueSize: Long,
    minFeeRefScriptCostPerByte: Long,
    minPoolCost: Long,
    monetaryExpansion: Double,
    poolPledgeInfluence: Double,
    poolRetireMaxEpoch: Long,
    poolVotingThresholds: PoolVotingThresholds,
    protocolVersion: ProtocolVersion,
    stakeAddressDeposit: Long,
    stakePoolDeposit: Long,
    stakePoolTargetNum: Long,
    treasuryCut: Double,
    txFeeFixed: Long,
    txFeePerByte: Long,
    utxoCostPerByte: Long
) derives ReadWriter

case class DRepVotingThresholds(
    committeeNoConfidence: Double,
    committeeNormal: Double,
    hardForkInitiation: Double,
    motionNoConfidence: Double,
    ppEconomicGroup: Double,
    ppGovGroup: Double,
    ppNetworkGroup: Double,
    ppTechnicalGroup: Double,
    treasuryWithdrawal: Double,
    updateToConstitution: Double
) derives ReadWriter

case class PoolVotingThresholds(
    committeeNoConfidence: Double,
    committeeNormal: Double,
    hardForkInitiation: Double,
    motionNoConfidence: Double,
    ppSecurityGroup: Double
) derives ReadWriter

object ProtocolParams {
    import upickle.default.{readwriter, ReadWriter}
    val blockfrostParamsRW: ReadWriter[ProtocolParams] =
        readwriter[ujson.Value].bimap[ProtocolParams](
          params =>
              ujson.Obj(
                "collateral_percent" -> params.collateralPercentage,
                "committee_max_term_length" -> params.committeeMaxTermLength.toString,
                "committee_min_size" -> params.committeeMinSize.toString,
                "cost_models" -> params.costModels.map { (k, v) =>
                    k -> v.map(v => ujson.Num(v.toDouble))
                },
                "drep_activity" -> params.dRepActivity.toString,
                "drep_deposit" -> params.dRepDeposit.toString,
                "dvt_motion_no_confidence" -> params.dRepVotingThresholds.motionNoConfidence,
                "dvt_committee_normal" -> params.dRepVotingThresholds.committeeNormal,
                "dvt_committee_no_confidence" -> params.dRepVotingThresholds.committeeNoConfidence,
                "dvt_update_to_constitution" -> params.dRepVotingThresholds.updateToConstitution,
                "dvt_hard_fork_initiation" -> params.dRepVotingThresholds.hardForkInitiation,
                "dvt_p_p_network_group" -> params.dRepVotingThresholds.ppNetworkGroup,
                "dvt_p_p_economic_group" -> params.dRepVotingThresholds.ppEconomicGroup,
                "dvt_p_p_technical_group" -> params.dRepVotingThresholds.ppTechnicalGroup,
                "dvt_p_p_gov_group" -> params.dRepVotingThresholds.ppGovGroup,
                "dvt_treasury_withdrawal" -> params.dRepVotingThresholds.treasuryWithdrawal,
                "price_mem" -> params.executionUnitPrices.priceMemory,
                "price_step" -> params.executionUnitPrices.priceSteps,
                "gov_action_deposit" -> params.govActionDeposit.toString,
                "gov_action_lifetime" -> params.govActionLifetime.toString,
                "max_block_size" -> params.maxBlockBodySize,
                "max_block_ex_mem" -> params.maxBlockExecutionUnits.memory.toString,
                "max_block_ex_steps" -> params.maxBlockExecutionUnits.steps.toString,
                "max_block_header_size" -> params.maxBlockHeaderSize,
                "max_collateral_inputs" -> params.maxCollateralInputs,
                "max_tx_ex_mem" -> params.maxTxExecutionUnits.memory.toString,
                "max_tx_ex_steps" -> params.maxTxExecutionUnits.steps.toString,
                "max_tx_size" -> params.maxTxSize,
                "max_val_size" -> params.maxValueSize.toString,
                "min_fee_ref_script_cost_per_byte" -> params.minFeeRefScriptCostPerByte,
                "min_pool_cost" -> params.minPoolCost.toString,
                "rho" -> params.monetaryExpansion,
                "a0" -> params.poolPledgeInfluence,
                "e_max" -> params.poolRetireMaxEpoch,
                "pvt_motion_no_confidence" -> params.poolVotingThresholds.motionNoConfidence,
                "pvt_committee_normal" -> params.poolVotingThresholds.committeeNormal,
                "pvt_committee_no_confidence" -> params.poolVotingThresholds.committeeNoConfidence,
                "pvt_hard_fork_initiation" -> params.poolVotingThresholds.hardForkInitiation,
                "pvtpp_security_group" -> params.poolVotingThresholds.ppSecurityGroup,
                "protocol_major_ver" -> params.protocolVersion.major,
                "protocol_minor_ver" -> params.protocolVersion.minor,
                "key_deposit" -> params.stakeAddressDeposit.toString,
                "pool_deposit" -> params.stakePoolDeposit.toString,
                "n_opt" -> params.stakePoolTargetNum,
                "tau" -> params.treasuryCut,
                "min_fee_b" -> params.txFeeFixed,
                "min_fee_a" -> params.txFeePerByte,
                "coins_per_utxo_size" -> params.utxoCostPerByte.toString
              ),
          json =>
              ProtocolParams(
                collateralPercentage = json("collateral_percent").num.toLong,
                committeeMaxTermLength = json("committee_max_term_length").str.toLong,
                committeeMinSize = json("committee_min_size").str.toLong,
                costModels = json("cost_models").obj.map { case (k, v) =>
                    k -> v.obj.values.map(_.num.toLong).toSeq
                }.toMap,
                dRepActivity = json("drep_activity").str.toLong,
                dRepDeposit = json("drep_deposit").str.toLong,
                dRepVotingThresholds = DRepVotingThresholds(
                  motionNoConfidence = json("dvt_motion_no_confidence").num,
                  committeeNormal = json("dvt_committee_normal").num,
                  committeeNoConfidence = json("dvt_committee_no_confidence").num,
                  updateToConstitution = json("dvt_update_to_constitution").num,
                  hardForkInitiation = json("dvt_hard_fork_initiation").num,
                  ppNetworkGroup = json("dvt_p_p_network_group").num,
                  ppEconomicGroup = json("dvt_p_p_economic_group").num,
                  ppTechnicalGroup = json("dvt_p_p_technical_group").num,
                  ppGovGroup = json("dvt_p_p_gov_group").num,
                  treasuryWithdrawal = json("dvt_treasury_withdrawal").num
                ),
                executionUnitPrices = ExecutionUnitPrices(
                  priceMemory = json("price_mem").num,
                  priceSteps = json("price_step").num
                ),
                govActionDeposit = json("gov_action_deposit").str.toLong,
                govActionLifetime = json("gov_action_lifetime").str.toLong,
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
                minFeeRefScriptCostPerByte = json("min_fee_ref_script_cost_per_byte").num.toLong,
                minPoolCost = json("min_pool_cost").str.toLong,
                monetaryExpansion = json("rho").num,
                poolPledgeInfluence = json("a0").num,
                poolRetireMaxEpoch = json("e_max").num.toLong,
                poolVotingThresholds = PoolVotingThresholds(
                  motionNoConfidence = json("pvt_motion_no_confidence").num,
                  committeeNormal = json("pvt_committee_normal").num,
                  committeeNoConfidence = json("pvt_committee_no_confidence").num,
                  hardForkInitiation = json("pvt_hard_fork_initiation").num,
                  ppSecurityGroup = json("pvtpp_security_group").num
                ),
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
                utxoCostPerByte = json("coins_per_utxo_size").str.toLong
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

    def toJson: String = write(this)
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

    def toJson: String = write(this)
}

// TODO: make one single style of naming
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
    var `divideInteger-cpu-arguments-minimum`: Long = 0L
    var `divideInteger-cpu-arguments-c00`: Long = 0L
    var `divideInteger-cpu-arguments-c10`: Long = 0L
    var `divideInteger-cpu-arguments-c01`: Long = 0L
    var `divideInteger-cpu-arguments-c20`: Long = 0L
    var `divideInteger-cpu-arguments-c11`: Long = 0L
    var `divideInteger-cpu-arguments-c02`: Long = 0L
    var `divideInteger-memory-arguments-intercept`: Long = 0L
    var `divideInteger-memory-arguments-slope`: Long = 0L
    var `divideInteger-memory-arguments-minimum`: Long = 0L
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
    var `modInteger-cpu-arguments-model-arguments-minimum`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c00`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c10`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c01`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c20`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c11`: Long = 0L
    var `modInteger-cpu-arguments-model-arguments-c02`: Long = 0L
    var `modInteger-memory-arguments-intercept`: Long = 0L
    var `modInteger-memory-arguments-slope`: Long = 0L
    var `multiplyInteger-cpu-arguments-intercept`: Long = 0L
    var `multiplyInteger-cpu-arguments-slope`: Long = 0L
    var `multiplyInteger-memory-arguments-intercept`: Long = 0L
    var `multiplyInteger-memory-arguments-slope`: Long = 0L
    var `nullList-cpu-arguments`: Long = 0L
    var `nullList-memory-arguments`: Long = 0L
    var `quotientInteger-cpu-arguments-constant`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-minimum`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c00`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c10`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c01`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c20`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c11`: Long = 0L
    var `quotientInteger-cpu-arguments-model-arguments-c02`: Long = 0L
    var `quotientInteger-memory-arguments-intercept`: Long = 0L
    var `quotientInteger-memory-arguments-slope`: Long = 0L
    var `quotientInteger-memory-arguments-minimum`: Long = 0L
    var `remainderInteger-cpu-arguments-constant`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-minimum`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c00`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c10`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c01`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c20`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c11`: Long = 0L
    var `remainderInteger-cpu-arguments-model-arguments-c02`: Long = 0L
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
    var cekConstrCost_exBudgetCPU: Long = 0L
    var cekConstrCost_exBudgetMemory: Long = 0L
    var cekCaseCost_exBudgetCPU: Long = 0L
    var cekCaseCost_exBudgetMemory: Long = 0L
    var bls12_381_G1_add_cpu_arguments: Long = 0L
    var bls12_381_G1_add_memory_arguments: Long = 0L
    var bls12_381_G1_compress_cpu_arguments: Long = 0L
    var bls12_381_G1_compress_memory_arguments: Long = 0L
    var bls12_381_G1_equal_cpu_arguments: Long = 0L
    var bls12_381_G1_equal_memory_arguments: Long = 0L
    var bls12_381_G1_hashToGroup_cpu_arguments_intercept: Long = 0L
    var bls12_381_G1_hashToGroup_cpu_arguments_slope: Long = 0L
    var bls12_381_G1_hashToGroup_memory_arguments: Long = 0L
    var bls12_381_G1_neg_cpu_arguments: Long = 0L
    var bls12_381_G1_neg_memory_arguments: Long = 0L
    var bls12_381_G1_scalarMul_cpu_arguments_intercept: Long = 0L
    var bls12_381_G1_scalarMul_cpu_arguments_slope: Long = 0L
    var bls12_381_G1_scalarMul_memory_arguments: Long = 0L
    var bls12_381_G1_uncompress_cpu_arguments: Long = 0L
    var bls12_381_G1_uncompress_memory_arguments: Long = 0L
    var bls12_381_G2_add_cpu_arguments: Long = 0L
    var bls12_381_G2_add_memory_arguments: Long = 0L
    var bls12_381_G2_compress_cpu_arguments: Long = 0L
    var bls12_381_G2_compress_memory_arguments: Long = 0L
    var bls12_381_G2_equal_cpu_arguments: Long = 0L
    var bls12_381_G2_equal_memory_arguments: Long = 0L
    var bls12_381_G2_hashToGroup_cpu_arguments_intercept: Long = 0L
    var bls12_381_G2_hashToGroup_cpu_arguments_slope: Long = 0L
    var bls12_381_G2_hashToGroup_memory_arguments: Long = 0L
    var bls12_381_G2_neg_cpu_arguments: Long = 0L
    var bls12_381_G2_neg_memory_arguments: Long = 0L
    var bls12_381_G2_scalarMul_cpu_arguments_intercept: Long = 0L
    var bls12_381_G2_scalarMul_cpu_arguments_slope: Long = 0L
    var bls12_381_G2_scalarMul_memory_arguments: Long = 0L
    var bls12_381_G2_uncompress_cpu_arguments: Long = 0L
    var bls12_381_G2_uncompress_memory_arguments: Long = 0L
    var bls12_381_finalVerify_cpu_arguments: Long = 0L
    var bls12_381_finalVerify_memory_arguments: Long = 0L
    var bls12_381_millerLoop_cpu_arguments: Long = 0L
    var bls12_381_millerLoop_memory_arguments: Long = 0L
    var bls12_381_mulMlResult_cpu_arguments: Long = 0L
    var bls12_381_mulMlResult_memory_arguments: Long = 0L
    var keccak_256_cpu_arguments_intercept: Long = 0L
    var keccak_256_cpu_arguments_slope: Long = 0L
    var keccak_256_memory_arguments: Long = 0L
    var blake2b_224_cpu_arguments_intercept: Long = 0L
    var blake2b_224_cpu_arguments_slope: Long = 0L
    var blake2b_224_memory_arguments: Long = 0L
    var integerToByteString_cpu_arguments_c0: Long = 0L
    var integerToByteString_cpu_arguments_c1: Long = 0L
    var integerToByteString_cpu_arguments_c2: Long = 0L
    var integerToByteString_memory_arguments_intercept: Long = 0L
    var integerToByteString_memory_arguments_slope: Long = 0L
    var byteStringToInteger_cpu_arguments_c0: Long = 0L
    var byteStringToInteger_cpu_arguments_c1: Long = 0L
    var byteStringToInteger_cpu_arguments_c2: Long = 0L
    var byteStringToInteger_memory_arguments_intercept: Long = 0L
    var byteStringToInteger_memory_arguments_slope: Long = 0L
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

object PlutusV3Params:
    given ReadWriter[PlutusV3Params] = JsonUtils.mkClassFieldsReadWriter[PlutusV3Params]
    val (toSeq, fromSeq) = JsonUtils.mkClassFieldsFromSeqIso[PlutusV3Params]
