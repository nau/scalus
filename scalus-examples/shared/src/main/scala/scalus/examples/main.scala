package scalus.examples

import scalus.cardano.plutus.contract.blueprint.model.*
import scalus.prelude.Validator as ScalusValidator

object main {

    def main(args: Array[String]): Unit = {
        val bp = blueprint("title", "description", FakeValidator)
        println(bp)
//        val bp = blueprintFor("App title", "My description", FakeValidator)
//        println(bp)
    }
}
