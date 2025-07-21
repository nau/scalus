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

    // This case is covered by the following tests, keeping this test to check compatibility with aiken.
    // https://github.com/aiken-lang/aiken/blob/main/crates/aiken-project/src/blueprint/snapshots/aiken_project__blueprint__validator__tests__generics.snap#L57
    test("should produce correct schemas for `enum` types") {
        val intervalSchema = PlutusDataSchema.derived[Interval]

        assert(intervalSchema.show() == Interval.schema)
    }

    test("should produce correct schemas for `HtlcValidator` input types") {
        val datumSchema = PlutusDataSchema.derived[ContractDatum]
        val redeemerSchema = PlutusDataSchema.derived[Action]

        assert(datumSchema.show() == ContractDatum.schema)
    }
}
