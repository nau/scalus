import org.typelevel.paiges.Doc
import scalus.sir.PrettyPrinter
import scalus.sir.SIR
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.Constant
import scalus.uplc.DefaultUni
import scalus.uplc.Program
import scalus.uplc.ProgramFlatCodec
import scalus.uplc.Term
import scalus.utils.Hex
package object scalus {
  extension (sir: SIR)
    def pretty: Doc = PrettyPrinter.pretty(sir)
    def doubleCborHex(version: (Int, Int, Int), generateErrorTraces: Boolean = false): String =
      import io.bullet.borer.Cbor
      val term = sir.toUplc(generateErrorTraces)
      val flatEncoded = ProgramFlatCodec.encodeFlat(Program(version, term))
      val cbor = Cbor.encode(flatEncoded).toByteArray
      val cbor2 = Cbor.encode(cbor).toByteArray
      Hex.bytesToHex(cbor2)

    def toUplc(generateErrorTraces: Boolean = false): Term =
      SimpleSirToUplcLowering(sir, generateErrorTraces).lower()

  extension (du: DefaultUni) def pretty: Doc = PrettyPrinter.pretty(du)
  extension (c: Constant) def pretty: Doc = PrettyPrinter.pretty(c)
}
