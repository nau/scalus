package scalus.ledger.api

enum PlutusLedgerLanguage extends java.lang.Enum[PlutusLedgerLanguage]:
    case PlutusV1, PlutusV2, PlutusV3

case class ProtocolVersion(major: Int, minor: Int) extends Ordered[ProtocolVersion] {
    def compare(that: ProtocolVersion): Int = {
        val majorComp = this.major.compareTo(that.major)
        if (majorComp != 0) majorComp else this.minor.compareTo(that.minor)
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
    val knownPVs: Set[ProtocolVersion] =
        Set(shelleyPV, allegraPV, maryPV, alonzoPV, vasilPV, valentinePV, conwayPV)
}
