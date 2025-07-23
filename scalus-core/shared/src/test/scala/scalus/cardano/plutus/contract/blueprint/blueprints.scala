package scalus.cardano.plutus.contract.blueprint
import scalus.Compiler.compile
import scalus.buildinfo.BuildInfo
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger.Script
import scalus.ledger.api.v1.PosixTime
import scalus.toUplc
import scalus.plutusV3

private val bytes = compile((ctx: Data) => ()).toUplc().plutusV3.cborEncoded
val emptyScript = Script.PlutusV3(ByteString.fromArray(bytes))
def blueprintedScript(title: String, description: String) =
    s"""{"preamble":{"title":"$title","description":"$description","compiler":{"name":"scalus","version":"${BuildInfo.version}"},"plutusVersion":"v3"},"validators":[{"title":"validator","compiledCode":"450101002499","hash":"186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4"}]}""".stripMargin

enum Interval {
    case Finite(value: Int)
    case Infinite
}
object Interval {
    def schema: String =
        """{
          |  "title": "Interval",
          |  "anyOf": [
          |    {
          |      "dataType": "constructor",
          |      "title": "Finite",
          |      "index": 0,
          |      "fields": [
          |        {
          |          "dataType": "integer",
          |          "title": "value"
          |        }
          |      ]
          |    },
          |    {
          |      "dataType": "constructor",
          |      "title": "Infinite",
          |      "index": 1,
          |      "fields": [
          |        
          |      ]
          |    }
          |  ]
          |}""".stripMargin
}

// Copied from examples
object HtlcValidatorInputs {
    type Preimage = ByteString
    type Image = ByteString
    type PubKeyHash = ByteString

    // Contract Datum
    case class ContractDatum(
        committer: PubKeyHash,
        receiver: PubKeyHash,
        image: Image,
        timeout: PosixTime
    )
    object ContractDatum {
        def schema: String =
            """{
              |  "dataType": "constructor",
              |  "title": "ContractDatum",
              |  "fields": [
              |    {
              |      "dataType": "bytes",
              |      "title": "committer"
              |    },
              |    {
              |      "dataType": "bytes",
              |      "title": "receiver"
              |    },
              |    {
              |      "dataType": "bytes",
              |      "title": "image"
              |    },
              |    {
              |      "dataType": "integer",
              |      "title": "timeout"
              |    }
              |  ]
              |}""".stripMargin
    }

    // Redeemer
    enum Action:
        case Timeout
        case Reveal(preimage: Preimage)

    object Action {
        def schema: String = """{
                                      |  "title": "Action",
                                      |  "anyOf": [
                                      |    {
                                      |      "dataType": "constructor",
                                      |      "title": "Timeout",
                                      |      "index": 0,
                                      |      "fields": [
                                      |
                                      |      ]
                                      |    },
                                      |    {
                                      |      "dataType": "constructor",
                                      |      "title": "Reveal",
                                      |      "index": 1,
                                      |      "fields": [
                                      |        {
                                      |          "dataType": "bytes",
                                      |          "title": "preimage"
                                      |        }
                                      |      ]
                                      |    }
                                      |  ]
                                      |}""".stripMargin
    }

}
