package scalus.cardano.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import upickle.ReadWriter

/** Represents voting thresholds for stake pools in the Cardano blockchain.
  *
  * Defines the thresholds required for different governance actions to pass when voted on by stake
  * pools.
  *
  * @param motionNoConfidence
  *   Threshold for no confidence motions
  * @param committeeNormal
  *   Threshold for normal committee updates
  * @param committeeNoConfidence
  *   Threshold for committee updates after no confidence
  * @param hardForkInitiation
  *   Threshold for hard fork initiations
  * @param ppSecurityGroup
  *   Threshold for security protocol parameter changes
  */
case class PoolVotingThresholds(
    motionNoConfidence: UnitInterval,
    committeeNormal: UnitInterval,
    committeeNoConfidence: UnitInterval,
    hardForkInitiation: UnitInterval,
    ppSecurityGroup: UnitInterval
) derives ReadWriter

object PoolVotingThresholds {

    /** CBOR Encoder for PoolVotingThresholds. Encodes as an array of 5 UnitIntervals.
      */
    given Encoder[PoolVotingThresholds] = new Encoder[PoolVotingThresholds] {
        def write(w: Writer, value: PoolVotingThresholds): Writer =
            w.writeArrayOpen(5)
                .write(value.motionNoConfidence)
                .write(value.committeeNormal)
                .write(value.committeeNoConfidence)
                .write(value.hardForkInitiation)
                .write(value.ppSecurityGroup)
                .writeArrayClose()
    }

    /** CBOR Decoder for PoolVotingThresholds. Decodes from an array of 5 UnitIntervals.
      */
    given Decoder[PoolVotingThresholds] = new Decoder[PoolVotingThresholds] {
        def read(r: Reader): PoolVotingThresholds = {
            r.readArrayHeader()
            val motionNoConfidence = r.read[UnitInterval]()
            val committeeNormal = r.read[UnitInterval]()
            val committeeNoConfidence = r.read[UnitInterval]()
            val hardFork = r.read[UnitInterval]()
            val ppSecurityGroup = r.read[UnitInterval]()

            PoolVotingThresholds(
              motionNoConfidence,
              committeeNormal,
              committeeNoConfidence,
              hardFork,
              ppSecurityGroup
            )
        }
    }
}

/** Represents voting thresholds for DReps in the Cardano blockchain.
  *
  * Defines the thresholds required for different governance actions to pass when voted on by
  * delegated representatives.
  *
  * @param motionNoConfidence
  *   Threshold for no confidence motions
  * @param committeeNormal
  *   Threshold for normal committee updates
  * @param committeeNoConfidence
  *   Threshold for committee updates after no confidence
  * @param updateToConstitution
  *   Threshold for constitution updates
  * @param hardForkInitiation
  *   Threshold for hard fork initiations
  * @param ppNetworkGroup
  *   Threshold for network protocol parameter changes
  * @param ppEconomicGroup
  *   Threshold for economic protocol parameter changes
  * @param ppTechnicalGroup
  *   Threshold for technical protocol parameter changes
  * @param ppGovGroup
  *   Threshold for governance protocol parameter changes
  * @param treasuryWithdrawal
  *   Threshold for treasury withdrawals
  */
case class DRepVotingThresholds(
    motionNoConfidence: UnitInterval,
    committeeNormal: UnitInterval,
    committeeNoConfidence: UnitInterval,
    updateToConstitution: UnitInterval,
    hardForkInitiation: UnitInterval,
    ppNetworkGroup: UnitInterval,
    ppEconomicGroup: UnitInterval,
    ppTechnicalGroup: UnitInterval,
    ppGovGroup: UnitInterval,
    treasuryWithdrawal: UnitInterval
) derives ReadWriter

object DRepVotingThresholds {

    /** CBOR Encoder for DRepVotingThresholds. Encodes as an array of 10 UnitIntervals.
      */
    given Encoder[DRepVotingThresholds] = new Encoder[DRepVotingThresholds] {
        def write(w: Writer, value: DRepVotingThresholds): Writer =
            w.writeArrayOpen(10)
                .write(value.motionNoConfidence)
                .write(value.committeeNormal)
                .write(value.committeeNoConfidence)
                .write(value.updateToConstitution)
                .write(value.hardForkInitiation)
                .write(value.ppNetworkGroup)
                .write(value.ppEconomicGroup)
                .write(value.ppTechnicalGroup)
                .write(value.ppGovGroup)
                .write(value.treasuryWithdrawal)
                .writeArrayClose()
    }

    /** CBOR Decoder for DRepVotingThresholds. Decodes from an array of 10 UnitIntervals.
      */
    given Decoder[DRepVotingThresholds] = new Decoder[DRepVotingThresholds] {
        def read(r: Reader): DRepVotingThresholds = {
            r.readArrayHeader()
            val motionNoConfidence = r.read[UnitInterval]()
            val committeeNormal = r.read[UnitInterval]()
            val committeeNoConfidence = r.read[UnitInterval]()
            val updateToConstitution = r.read[UnitInterval]()
            val hardForkInitiation = r.read[UnitInterval]()
            val ppNetworkGroup = r.read[UnitInterval]()
            val ppEconomicGroup = r.read[UnitInterval]()
            val ppTechnicalGroup = r.read[UnitInterval]()
            val ppGovGroup = r.read[UnitInterval]()
            val treasuryWithdrawal = r.read[UnitInterval]()

            DRepVotingThresholds(
              motionNoConfidence,
              committeeNormal,
              committeeNoConfidence,
              updateToConstitution,
              hardForkInitiation,
              ppNetworkGroup,
              ppEconomicGroup,
              ppTechnicalGroup,
              ppGovGroup,
              treasuryWithdrawal
            )
        }
    }
}
