package scalus.cardano.plutus.contract.blueprint
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Ignore
import scalus.cardano.plutus.contract.blueprint.HtlcValidatorInputs.{Action, ContractDatum}
import scalus.cardano.tbd.{Application, PlutusV3}
import scalus.*
import scalus.uplc.Program

@Ignore
class ScriptDefinitionTest extends AnyFunSuite {
    
    test("should compile the user program the same as they would compile it themselves") {
        val scriptDefinition = PlutusV3.create[ContractDatum, Action](EmptyValidator)
        val app = Application(
          "Test title",
          "Test description",
          "1.0.0",
          Seq(scriptDefinition)
        )

        val program: Program = Compiler.compile(EmptyValidator.validate).toUplcOptimized().plutusV3
        assert(program.cborEncoded == app.contracts.head.asProgram.cborEncoded)
        
//         val programSir = program.term.show
//         val appDefinitionSir = app.contracts.head.asProgram.term.show
        
    }
}
