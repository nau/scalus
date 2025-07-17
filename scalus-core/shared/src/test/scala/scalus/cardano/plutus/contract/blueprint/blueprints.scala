package scalus.cardano.plutus.contract.blueprint
import scalus.Compiler.compile
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.Script
import scalus.{plutusV3, toUplc}

private val bytes = compile((ctx: Data) => ()).toUplc().plutusV3.cborEncoded
val emptyScript = Script.PlutusV3(ByteString.fromArray(bytes))
def blueprintedScript(title: String, description: String) =
    s"""{"preamble":{"title":"$title","description":"$description","compiler":{"name":"scalus","version":"0.10.0"},"plutusVersion":"v3"},"validators":[{"title":"validator","compiledCode":"450101002499","hash":"186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4"}]}""".stripMargin
