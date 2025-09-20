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
