package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*

import scala.collection.immutable.Map

/** Represents voting procedures in the Cardano blockchain governance system.
  *
  * VotingProcedures is a map from voters to maps of governance action IDs to voting procedures.
  * It's used to record votes on governance actions.
  *
  * @param procedures
  *   Map from voters to maps of governance action IDs to voting procedures
  */
case class VotingProcedures(procedures: Map[Voter, Map[GovActionId, VotingProcedure]])
    derives Codec {

    /** Checks if there are any voting procedures.
      *
      * @return
      *   true if there are no voting procedures, false otherwise
      */
    def isEmpty: Boolean = procedures.isEmpty || procedures.forall(_._2.isEmpty)
}
