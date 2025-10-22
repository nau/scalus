package scalus.examples.vault

import scalus.cardano.blueprint.{CompilerInfo, Contract, Preamble}
import scalus.cardano.ledger.Language

lazy val VaultContract = Contract.PlutusV3Contract[State, Action](
  Preamble(
    title = "Vault",
    description = Some(
      "Keeps the funds safe by requiring a 2-stage withdrawal with a mandatory confirmation period."
    ),
    version = Some("1.0.0"),
    compiler = Some(CompilerInfo.currentScalus),
    plutusVersion = Some(Language.PlutusV3),
    license = None
  ),
  VaultValidator.validate
)
