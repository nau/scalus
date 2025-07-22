package scalus.cardano.plutus.contract.blueprint
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.plutus.contract.blueprint.HtlcValidatorInputs.{Action, ContractDatum, EmptyValidator}

import scalus.cardano.tbd.{Application, PlutusV3}

class ScriptDefinitionTest extends AnyFunSuite {

    test("") {
        val scriptDefinition = PlutusV3.create[ContractDatum, Action](EmptyValidator)
        val app = Application(
          "Test title",
          "Test description",
          "1.0.0",
          Seq(scriptDefinition)
        )
        println(app.blueprint.show())

    }
}
