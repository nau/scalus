package scalus.ledger.api

import scalus.cardano.ledger.Language

enum PlutusLedgerLanguage extends java.lang.Enum[PlutusLedgerLanguage]:
    case PlutusV1, PlutusV2, PlutusV3

    def toLanguage: Language = this match
        case PlutusV1 => Language.PlutusV1
        case PlutusV2 => Language.PlutusV2
        case PlutusV3 => Language.PlutusV3
