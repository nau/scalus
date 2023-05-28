import scalus.sir.SIR
import scalus.sir.PrettyPrinter
import scalus.uplc.DefaultUni
import org.typelevel.paiges.Doc
import scalus.uplc.Constant
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.ProgramFlatCodec
import scalus.uplc.Program
import scalus.utils.Hex
package object scalus {
  extension (sir: SIR) 
    def pretty: Doc = PrettyPrinter.pretty(sir)
    def doubleCborHex(version: (Int, Int, Int), generateErrorTraces: Boolean = false): String = 
      import io.bullet.borer.Cbor
      val term = new SimpleSirToUplcLowering(generateErrorTraces).lower(sir)
      val flatEncoded = ProgramFlatCodec.encodeFlat(Program(version, term))
      val cbor = Cbor.encode(flatEncoded).toByteArray
      val cbor2 = Cbor.encode(cbor).toByteArray
      Hex.bytesToHex(cbor2)
  extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
  extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)
}
