package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import upickle.ReadWriter

import scala.collection.immutable.SortedSet

/** Represents a Cardano protocol version */
case class ProtocolVersion(
    /** Major version (1-10) */
    major: Int,

    /** Minor version */
    minor: Int
) extends Ordered[ProtocolVersion] derives Codec, ReadWriter {
    require(major >= 1, s"Major version must be positive, got $major")
    require(minor >= 0, s"Minor version must be non-negative, got $minor")
    def compare(that: ProtocolVersion): Int = {
        val majorComp = this.major.compareTo(that.major)
        if majorComp != 0 then majorComp else this.minor.compareTo(that.minor)
    }
}

object ProtocolVersion {
    // Constants
    val shelleyPV = ProtocolVersion(2, 0)
    val allegraPV = ProtocolVersion(3, 0)
    val maryPV = ProtocolVersion(4, 0)
    val alonzoPV = ProtocolVersion(5, 0)
    val vasilPV = ProtocolVersion(7, 0)
    val valentinePV = ProtocolVersion(8, 0)
    val conwayPV = ProtocolVersion(9, 0)
    val futurePV = ProtocolVersion(Int.MaxValue, 0)

    // Known protocol versions
    val knownPVs: SortedSet[ProtocolVersion] =
        SortedSet(shelleyPV, allegraPV, maryPV, alonzoPV, vasilPV, valentinePV, conwayPV)
}
