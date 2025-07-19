package scalus.cardano.plutus.contract.blueprint
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.plutus.contract.blueprint
import scalus.cardano.plutus.contract.blueprint.HtlcValidatorInputs.{Action, ContractDatum}

class BlueprintTest extends AnyFunSuite {

    test("should produce a correct JSON value") {
        val title = "title"
        val description = "description"
        val bp = blueprint.mkBlueprint(title, description, emptyScript)
        val expected = blueprintedScript(title, description)

        assert(bp.show(0) == expected)
    }

    test("should produce correct schemas for `HtlcValidator` input types") {
        val datumSchema = deriveSchema[ContractDatum]
        val redeemerSchema = deriveSchema[Action]

        assert(datumSchema.show() == ContractDatum.correctSchema)
        println(redeemerSchema.show() == Action.correctSchema)
    }
}
