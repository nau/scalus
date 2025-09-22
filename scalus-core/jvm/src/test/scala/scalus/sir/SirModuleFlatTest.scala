package scalus.sir

import java.io.InputStream
import org.scalatest.funsuite.AnyFunSuite
import scalus.serialization.flat
import scalus.serialization.flat.*
import scalus.serialization.flat.FlatInstances.given

class SirModuleFlatTest extends AnyFunSuite {

    test("load standard DataParametrizedValidator modules") {
        val stream = classOf[scalus.prelude.DataParameterizedValidator].getResourceAsStream(
          "/scalus/prelude/DataParameterizedValidator.sir"
        )
        val module = parseInputStream(stream)
        assert(module.defs.nonEmpty, "module should not be empty")
    }

    def parseInputStream(input: InputStream): Module = {
        val buffer = input.readAllBytes()
        val dec = DecoderState(buffer)
        val module = flat.decode[Module](dec)
        module
    }

}
