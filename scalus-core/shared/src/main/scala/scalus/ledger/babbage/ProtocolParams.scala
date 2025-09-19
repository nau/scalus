package scalus.ledger.babbage

import scalus.ledger.api.ProtocolVersion
import upickle.default.*
import scalus.cardano.ledger.{ExUnitPrices, ExUnits, NonNegativeInterval, PoolVotingThresholds, UnitInterval}

import scala.util.Try

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
                "pvt_motion_no_confidence" -> params.poolVotingThresholds.motionNoConfidence.toDouble,
                "pvt_committee_normal" -> params.poolVotingThresholds.committeeNormal.toDouble,
                "pvt_committee_no_confidence" -> params.poolVotingThresholds.committeeNoConfidence.toDouble,
                "pvt_hard_fork_initiation" -> params.poolVotingThresholds.hardForkInitiation.toDouble,
                "pvtpp_security_group" -> params.poolVotingThresholds.ppSecurityGroup.toDouble,
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
                committeeMaxTermLength =
                    json("committee_max_term_length").strOpt.map(_.toLong).getOrElse(0L),
                committeeMinSize = json("committee_min_size").strOpt.map(_.toLong).getOrElse(0L),
                costModels = json("cost_models").obj.map { case (k, v) =>
                    k -> v.obj.values.map(_.num.toLong).toIndexedSeq
                }.toMap,
                dRepActivity = json("drep_activity").strOpt.map(_.toLong).getOrElse(0L),
                dRepDeposit = json("drep_deposit").strOpt.map(_.toLong).getOrElse(0L),
                dRepVotingThresholds = DRepVotingThresholds(
                  motionNoConfidence = json("dvt_motion_no_confidence").numOpt.getOrElse(0),
                  committeeNormal = json("dvt_committee_normal").numOpt.getOrElse(0),
                  committeeNoConfidence = json("dvt_committee_no_confidence").numOpt.getOrElse(0),
                  updateToConstitution = json("dvt_update_to_constitution").numOpt.getOrElse(0),
                  hardForkInitiation = json("dvt_hard_fork_initiation").numOpt.getOrElse(0),
                  ppNetworkGroup = json("dvt_p_p_network_group").numOpt.getOrElse(0),
                  ppEconomicGroup = json("dvt_p_p_economic_group").numOpt.getOrElse(0),
                  ppTechnicalGroup = json("dvt_p_p_technical_group").numOpt.getOrElse(0),
                  ppGovGroup = json("dvt_p_p_gov_group").numOpt.getOrElse(0),
                  treasuryWithdrawal = json("dvt_treasury_withdrawal").numOpt.getOrElse(0)
                ),
                executionUnitPrices = ExUnitPrices(
                  priceMemory = NonNegativeInterval(json("price_mem").num),
                  priceSteps = NonNegativeInterval(json("price_step").num)
                ),
                govActionDeposit = json("gov_action_deposit").strOpt.map(_.toLong).getOrElse(0L),
                govActionLifetime = json("gov_action_lifetime").strOpt.map(_.toLong).getOrElse(0L),
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
                minFeeRefScriptCostPerByte =
                    json("min_fee_ref_script_cost_per_byte").numOpt.map(_.toLong).getOrElse(0L),
                minPoolCost = json("min_pool_cost").str.toLong,
                monetaryExpansion = json("rho").num,
                poolPledgeInfluence = json("a0").num,
                poolRetireMaxEpoch = json("e_max").num.toLong,
                poolVotingThresholds = PoolVotingThresholds(
                  motionNoConfidence = json("pvt_motion_no_confidence").numOpt
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  committeeNormal = json("pvt_committee_normal").numOpt
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  committeeNoConfidence = json("pvt_committee_no_confidence").numOpt
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  hardForkInitiation = json("pvt_hard_fork_initiation").numOpt
                      .map(UnitInterval.fromDouble)
                      .getOrElse(UnitInterval.zero),
                  ppSecurityGroup = Try(
                    json("pvtpp_security_group").numOpt
                        .map(UnitInterval.fromDouble)
                        .getOrElse(UnitInterval.zero)
                  ).toOption
                      .orElse(json("pvt_p_p_security_group").numOpt.map(UnitInterval.fromDouble))
                      .getOrElse(UnitInterval.zero)
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
