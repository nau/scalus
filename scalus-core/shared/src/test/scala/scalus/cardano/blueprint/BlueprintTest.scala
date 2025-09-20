package scalus.cardano.blueprint

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.blueprint.HtlcValidatorInputs.{Action, ContractDatum}

class BlueprintTest extends AnyFunSuite {

    test("should produce a correct JSON value") {
        val title = "title"
        val description = "description"
        val bp = Blueprint(title, description, emptyScript)
        val expected = blueprintedScript(title, description)

        assert(bp.toJson(0) == expected)
    }

    test("should be deserializable from JSON") {
        assert(Blueprint.fromJson(SampleBlueprint.stringValue) == SampleBlueprint.value)
    }

    // This case is covered by the following tests, keeping this test to check compatibility with aiken.
    // https://github.com/aiken-lang/aiken/blob/main/crates/aiken-project/src/blueprint/snapshots/aiken_project__blueprint__validator__tests__generics.snap#L57
    test("should produce correct schemas for `enum` types") {
        val intervalSchema = PlutusDataSchema.derived[Interval].get

        assert(intervalSchema.toJson() == Interval.schema)
    }

    test("should produce correct schemas for `HtlcValidator` input types") {
        val datumSchema = PlutusDataSchema.derived[ContractDatum].get
        val redeemerSchema = PlutusDataSchema.derived[Action].get

        assert(datumSchema.toJson() == ContractDatum.schema)
    }

    test("should produce correct schemas for tuples") {
        val tuple2Schema = PlutusDataSchema.derived[(Int, String)].get
        assert(tuple2Schema.dataType.contains(DataType.PairBuiltin))
        assert(tuple2Schema.title.contains("Tuple2"))
        assert(tuple2Schema.items.isDefined)
        assert(tuple2Schema.items.get.length == 2)
        val items = tuple2Schema.items.get
        assert(items(0).dataType.contains(DataType.Integer))
        assert(items(1).dataType.contains(DataType.StringBuiltin))
    }

    test("should produce correct schemas for nested tuples") {
        val nestedTupleSchema = PlutusDataSchema.derived[(Int, (String, Boolean))].get

        assert(nestedTupleSchema.dataType.contains(DataType.PairBuiltin))
        assert(nestedTupleSchema.items.isDefined)
        assert(nestedTupleSchema.items.get.length == 2)

        val secondItem = nestedTupleSchema.items.get(1)
        assert(secondItem.dataType.contains(DataType.PairBuiltin))
        assert(secondItem.items.isDefined)
        assert(secondItem.items.get.length == 2)
    }

    test("should produce correct schemas for case classes with tuple fields") {
        case class TestCaseClass(
            name: String,
            coordinates: (Int, Int)
        )

        val schema = PlutusDataSchema.derived[TestCaseClass].get

        assert(schema.dataType.contains(DataType.Constructor))
        assert(schema.title.contains("TestCaseClass"))
        assert(schema.fields.isDefined)
        assert(schema.fields.get.length == 2)

        val coordinatesField = schema.fields.get(1)
        assert(coordinatesField.title.contains("coordinates"))
        assert(coordinatesField.dataType.contains(DataType.PairBuiltin))
        assert(coordinatesField.items.isDefined)
        assert(coordinatesField.items.get.length == 2)

        val tupleItems = coordinatesField.items.get
        assert(tupleItems(0).dataType.contains(DataType.Integer))
        assert(tupleItems(1).dataType.contains(DataType.Integer))
    }

    test("should return None for Unit type") {
        val unitSchema = PlutusDataSchema.derived[Unit]

        assert(unitSchema.isEmpty)
    }
}
