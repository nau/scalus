package scalus.ledger

package object api {

    /** @deprecated
      *   Use scalus.cardano.ledger.Timelock instead. This package location is deprecated.
      */
    @deprecated("Use scalus.cardano.ledger.Timelock instead", "0.12.0")
    type Timelock = scalus.cardano.ledger.Timelock

    /** @deprecated
      *   Use scalus.cardano.ledger.Timelock instead. This package location is deprecated.
      */
    @deprecated("Use scalus.cardano.ledger.Timelock instead", "0.12.0")
    val Timelock: scalus.cardano.ledger.Timelock.type = scalus.cardano.ledger.Timelock

    /** @deprecated
      *   Use scalus.cardano.ledger.SlotNo instead. This package location is deprecated.
      */
    @deprecated("Use scalus.cardano.ledger.SlotNo instead", "0.12.0")
    type SlotNo = scalus.cardano.ledger.SlotNo

    /** @deprecated
      *   Use scalus.cardano.ledger.ValidityInterval instead. This package location is deprecated.
      */
    @deprecated("Use scalus.cardano.ledger.ValidityInterval instead", "0.12.0")
    type ValidityInterval = scalus.cardano.ledger.ValidityInterval

    /** @deprecated
      *   Use scalus.cardano.ledger.ValidityInterval instead. This package location is deprecated.
      */
    @deprecated("Use scalus.cardano.ledger.ValidityInterval instead", "0.12.0")
    val ValidityInterval: scalus.cardano.ledger.ValidityInterval.type =
        scalus.cardano.ledger.ValidityInterval

    @deprecated("Use scalus.uplc.BuiltinSemanticsVariant instead", "0.12.0")
    type BuiltinSemanticsVariant = scalus.uplc.BuiltinSemanticsVariant

    @deprecated("Use scalus.uplc.BuiltinSemanticsVariant instead", "0.12.0")
    val BuiltinSemanticsVariant: scalus.uplc.BuiltinSemanticsVariant.type =
        scalus.uplc.BuiltinSemanticsVariant
}
