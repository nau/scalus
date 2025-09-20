package scalus.ledger.api

import scalus.cardano.ledger.Language
import upickle.default.ReadWriter
import scala.collection.immutable.SortedSet

enum PlutusLedgerLanguage extends java.lang.Enum[PlutusLedgerLanguage]:
    case PlutusV1, PlutusV2, PlutusV3

    def toLanguage: Language = this match
        case PlutusV1 => Language.PlutusV1
        case PlutusV2 => Language.PlutusV2
        case PlutusV3 => Language.PlutusV3

case class ProtocolVersion(major: Int, minor: Int) extends Ordered[ProtocolVersion]
    derives ReadWriter {
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

case class MajorProtocolVersion(version: Int) extends Ordered[MajorProtocolVersion]
    derives ReadWriter {
    def compare(that: MajorProtocolVersion): Int = this.version.compareTo(that.version)
}

object MajorProtocolVersion {
    // Constants
    val shelleyPV = MajorProtocolVersion(2)
    val allegraPV = MajorProtocolVersion(3)
    val maryPV = MajorProtocolVersion(4)
    val alonzoPV = MajorProtocolVersion(5)
    val vasilPV = MajorProtocolVersion(7)
    val valentinePV = MajorProtocolVersion(8)
    val changPV = MajorProtocolVersion(9)
    val plominPV = MajorProtocolVersion(10)
    val futurePV = MajorProtocolVersion(Int.MaxValue)

    // Known protocol versions
    val knownPVs: SortedSet[MajorProtocolVersion] =
        SortedSet(shelleyPV, allegraPV, maryPV, alonzoPV, vasilPV, valentinePV, changPV, plominPV)
}
