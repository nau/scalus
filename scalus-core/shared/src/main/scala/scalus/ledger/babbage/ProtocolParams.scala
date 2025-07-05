package scalus.ledger.babbage

import scalus.ledger.api.ProtocolVersion
import upickle.default.*
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval}

/** Protocol parameters for the Cardano blockchain of Babbage era Field names are taken from the
  * `cardano-cli query protocol-parameters` output
  * @note
  *   These names are different from CIP-55, don't ask me why.
  */
case class ProtocolParams(
    collateralPercentage: Long,
    committeeMaxTermLength: Long,
    committeeMinSize: Long,
    costModels: Map[String, IndexedSeq[Long]],
    dRepActivity: Long,
    dRepDeposit: Long,
    dRepVotingThresholds: DRepVotingThresholds,
    executionUnitPrices: ExUnitPrices,
    govActionDeposit: Long,
    govActionLifetime: Long,
    maxBlockBodySize: Long,
    maxBlockExecutionUnits: ExUnits,
    maxBlockHeaderSize: Long,
    maxCollateralInputs: Long,
    maxTxExecutionUnits: ExUnits,
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

    /** Reads ProtocolParams from JSON string in Blockfrost format */
    def fromBlockfrostJson(json: String): ProtocolParams = {
        read[ProtocolParams](json)(using blockfrostParamsRW)
    }

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
                "price_mem" -> params.executionUnitPrices.priceMemory.toDouble,
                "price_step" -> params.executionUnitPrices.priceSteps.toDouble,
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
                    k -> v.obj.values.map(_.num.toLong).toIndexedSeq
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
                executionUnitPrices = ExUnitPrices(
                  priceMemory = NonNegativeInterval(json("price_mem").num),
                  priceSteps = NonNegativeInterval(json("price_step").num)
                ),
                govActionDeposit = json("gov_action_deposit").str.toLong,
                govActionLifetime = json("gov_action_lifetime").str.toLong,
                maxBlockBodySize = json("max_block_size").num.toLong,
                maxBlockExecutionUnits = ExUnits(
                  memory = json("max_block_ex_mem").str.toLong,
                  steps = json("max_block_ex_steps").str.toLong
                ),
                maxBlockHeaderSize = json("max_block_header_size").num.toLong,
                maxCollateralInputs = json("max_collateral_inputs").num.toLong,
                maxTxExecutionUnits = ExUnits(
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
