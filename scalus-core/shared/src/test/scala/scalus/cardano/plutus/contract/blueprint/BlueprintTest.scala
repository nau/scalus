package scalus.cardano.plutus.contract.blueprint
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.plutus.contract.blueprint

class BlueprintTest extends AnyFunSuite {

    test("should produce a correct JSON value") {
        val title = "title"
        val description = "description"
        val bp = blueprint.mkBlueprint(title, description, emptyScript)
        val expected = blueprintedScript(title, description)

        assert(bp.show == expected)
    }
}
