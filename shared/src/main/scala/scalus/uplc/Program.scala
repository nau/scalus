package scalus.uplc

import io.bullet.borer.Cbor
import org.typelevel.paiges.Doc
import scalus.*
import scalus.builtin.Data.*
import scalus.utils.Hex

import scala.annotation.targetName

case class Program(version: (Int, Int, Int), term: Term):
    /** Checks if two programs are equal.
      *
      * Two programs are equal if their versions are equal and their terms are alpha-equivalent.
      * This means that the names of the variables are not important, only their De Bruijn indices.
      * We use unique negative indices to represent free variables.
      *
      * @param that
      * @return
      */
    infix def alphaEq(that: Program): Boolean =
        version == that.version && Term.alphaEq(this.term, that.term)
    lazy val flatEncoded = ProgramFlatCodec.encodeFlat(this)
    lazy val cborEncoded = Cbor.encode(flatEncoded).toByteArray
    lazy val doubleCborEncoded = Cbor.encode(cborEncoded).toByteArray
    lazy val doubleCborHex = Hex.bytesToHex(doubleCborEncoded)
    @targetName("applyArg")
    infix def $(arg: Term): Program = Program(version, Term.Apply(term, arg))

object Program:
    def fromDoubleCborHex(doubleCborHex: String): Program =
        val cbor = Cbor.decode(Hex.hexToBytes(doubleCborHex)).to[Array[Byte]].value
        val scriptFlat = Cbor.decode(cbor).to[Array[Byte]].value
        val debruijnedProgram = ProgramFlatCodec.decodeFlat(scriptFlat)
        debruijnedProgram.toProgram
    def fromCborHex(cborHex: String): Program =
        val cbor = Cbor.decode(Hex.hexToBytes(cborHex)).to[Array[Byte]].value
        val debruijnedProgram = ProgramFlatCodec.decodeFlat(cbor)
        debruijnedProgram.toProgram

case class DeBruijnedProgram private[uplc] (version: (Int, Int, Int), term: Term):
    def pretty: Doc =
        val (major, minor, patch) = version
        Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
          s"$major.$minor.$patch"
        ) + Doc.space + term.pretty + Doc.text(")")
    def toProgram: Program = DeBruijn.fromDeBruijnProgram(this)
