package scalus.testing.integration

import com.bloxbean.cardano.client.transaction.spec.*
import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.bloxbean.*
import scalus.bloxbean.TxEvaluator.ScriptHash
import scalus.builtin.{ByteString, Data}
import scalus.cardano.ledger
import scalus.cardano.ledger.Script
import scalus.testing.integration.BlocksTestUtils.*

import java.util
import scala.collection.{immutable, mutable}

@org.scalatest.Ignore
class BadDatumTest extends AnyFunSuite {

    test("bad datum from block 11545396") {
        val datum =
            "010281d8799fac446e616d6555546972656c65737320576f726b6572202335343838496d65646961547970654a696d6167652f6a70656745696d6167655f5840697066733a2f2f62616679626569616d746c74653767757167327a75777037616d65737a7367767436766d68643569356b7173356a756f61726777756b7a7233427a61ff4652617269747946436f6d6d6f6e48486561646765617246436f77626f794a4261636b67726f756e644d436f6d6d6f6e20507572706c65444579657344437574654543686573744d4c6561746865722061726d6f7244546f6f6c465363726f6c6c4b46616369616c206861697245506c61696e4a5370656369616c697479412d4b6465736372697074696f6e58254d61696e20636f6c6c656374696f6e206f6620746865204e6563726f204c65616775652e2001ff"
        val bytes = ByteString.fromHex(datum).bytes
        val cb = Cbor.decode(bytes).to[Data]
        val data = cb.value // throws "Expected End-of-Input but got Int (input position 1)"
        println(data)
    }

    test("bad block 11545396") {
        val txs = readTransactionsFromBlockCbor(11545396)
        val txsWithScripts =
            val r = mutable.Buffer
                .empty[(Transaction, util.List[ByteString], String, Map[ScriptHash, Script])]
            for BlocksTestUtils.BlockTx(tx, datums, txhash) <- txs do
                val utxos = utxoResolver.resolveUtxos(tx)
                val scripts = TxEvaluator.getAllResolvedScripts(tx, utxos)
                if scripts.nonEmpty then r.addOne((tx, datums, txhash, scripts))
            r.toSeq
        val evaluator = newEvaluator()
        for (tx, datums, txhash, scripts) <- txsWithScripts do {
            if tx.isValid && (datums.size() == tx.getWitnessSet.getPlutusDataList
                .size()) // FIXME: remove this check when we have the correct datums
            then
                val result = evaluator.evaluateTx(tx, util.Set.of(), datums, txhash)
                assert(result.isSuccessful, result)
        }
    }
}
