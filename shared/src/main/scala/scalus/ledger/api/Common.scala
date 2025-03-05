package scalus.ledger.api
import io.bullet.borer.{Decoder, Encoder}
import scalus.builtin.ByteString
import upickle.default.ReadWriter

enum BuiltinSemanticsVariant:
    case A, B, C

object BuiltinSemanticsVariant:
    def fromProtocolAndPlutusVersion(
        protocolVersion: ProtocolVersion,
        plutusLedgerLanguage: PlutusLedgerLanguage
    ): BuiltinSemanticsVariant = fromProtocolAndPlutusVersion(
      MajorProtocolVersion(protocolVersion.major),
      plutusLedgerLanguage
    )

    def fromProtocolAndPlutusVersion(
        protocolVersion: MajorProtocolVersion,
        plutusLedgerLanguage: PlutusLedgerLanguage
    ): BuiltinSemanticsVariant =
        (protocolVersion, plutusLedgerLanguage) match
            case (pv, PlutusLedgerLanguage.PlutusV1 | PlutusLedgerLanguage.PlutusV2) =>
                if pv < MajorProtocolVersion.changPV then BuiltinSemanticsVariant.A
                else BuiltinSemanticsVariant.B
            case (pv, PlutusLedgerLanguage.PlutusV3) if pv >= MajorProtocolVersion.changPV =>
                BuiltinSemanticsVariant.C
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported protocol version and Plutus language combination $protocolVersion $plutusLedgerLanguage"
                )

enum PlutusLedgerLanguage extends java.lang.Enum[PlutusLedgerLanguage]:
    case PlutusV1, PlutusV2, PlutusV3

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
    val knownPVs: Set[ProtocolVersion] =
        Set(shelleyPV, allegraPV, maryPV, alonzoPV, vasilPV, valentinePV, conwayPV)
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
    val knownPVs: Set[MajorProtocolVersion] =
        Set(shelleyPV, allegraPV, maryPV, alonzoPV, vasilPV, valentinePV, changPV, plominPV)
}

opaque type KeyHash = ByteString

object KeyHash:
    // KeyHash encoder/decoder
    given Encoder[KeyHash] = Encoder: (w, value) =>
        w.writeBytes(value.bytes)

    given Decoder[KeyHash] = Decoder: r =>
        KeyHash(ByteString.unsafeFromArray(r.readByteArray()))

    inline def apply(bs: ByteString): KeyHash = bs
