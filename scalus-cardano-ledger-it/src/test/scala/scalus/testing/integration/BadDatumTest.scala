package scalus.testing.integration

import com.bloxbean.cardano.yaci.core.model.serializers.util.WitnessUtil
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.testing.integration.BlocksTestUtils.*

import java.math.BigInteger
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

//@org.scalatest.Ignore
class BadDatumTest extends AnyFunSuite {

    test("bad datum from block 11545396") {
        val block = Files.readAllBytes(blockPath(11545396))
        for
            witness <- WitnessUtil.getWitnessRawData(block).asScala
            field <- Option(WitnessUtil.getWitnessFields(witness).get(BigInteger.valueOf(4L))).toSeq
            datum <- WitnessUtil.getArrayBytes(field).asScala
        do
            println(ByteString.fromArray(datum))
            val cbor = Cbor.decode(datum).to[Data]
            val data = cbor.value // throws "Expected End-of-Input but got Int (input position 1)"
            assert(data != null)
    }

}
