package scalus.uplc

import scalus.cardano.ledger.Language
import scalus.ledger.api.{MajorProtocolVersion, PlutusLedgerLanguage, ProtocolVersion}

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
    def fromProtocolAndPlutusVersion(
        protocolVersion: MajorProtocolVersion,
        plutusLedgerLanguage: Language
    ): BuiltinSemanticsVariant =
        (protocolVersion, plutusLedgerLanguage) match
            case (pv, Language.PlutusV1 | Language.PlutusV2) =>
                if pv < MajorProtocolVersion.changPV then BuiltinSemanticsVariant.A
                else BuiltinSemanticsVariant.B
            case (pv, Language.PlutusV3) if pv >= MajorProtocolVersion.changPV =>
                BuiltinSemanticsVariant.C
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported protocol version and Plutus language combination $protocolVersion $plutusLedgerLanguage"
                )
