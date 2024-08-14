package scalus.uplc

import io.bullet.borer.Cbor
import org.typelevel.paiges.Doc
import scalus.*
import scalus.builtin.Data.*
import scalus.utils.Hex

import scala.annotation.targetName

/** A Cardano Plutus program.
  *
  * A program is a versioned [[Term]]. The version is a tuple of three integers: major, minor, and
  * patch.
  *
  * @param version
  *   the version of the program
  * @param term
  *   the term of the program
  */
case class Program(version: (Int, Int, Int), term: Term):
    /** Checks if two programs are equal.
      *
      * Two programs are equal if their versions are equal and their terms are alpha-equivalent.
      * This means that the names of the variables are not important, only their De Bruijn indices.
      * We use unique negative indices to represent free variables.
      *
      * @param that
      *   the other program
      * @return
      *   true if the programs are alpha-equal, false otherwise
      */
    infix def alphaEq(that: Program): Boolean =
        version == that.version && Term.alphaEq(this.term, that.term)

    /** Flat-encoded representation of the program.
      *
      * The flat-encoded representation is a byte array that contains the program in a flat format.
      * This format is used to serialize the program to CBOR.
      */
    lazy val flatEncoded: Array[Byte] = ProgramFlatCodec.encodeFlat(this)

    /** CBOR-encoded representation of the program.
      *
      * The CBOR-encoded representation is a byte array that contains the program in a CBOR format.
      */
    lazy val cborEncoded: Array[Byte] = Cbor.encode(flatEncoded).toByteArray

    /** Double CBOR-encoded representation of the program.
      *
      * Cardano uses a double-CBOR encoding for Plutus scripts in many places.
      */
    lazy val doubleCborEncoded: Array[Byte] = Cbor.encode(cborEncoded).toByteArray

    /** Double CBOR-encoded hex string of the program.
      *
      * Cardano uses a double-CBOR encoding for Plutus scripts in many places.
      */
    lazy val doubleCborHex: String = Hex.bytesToHex(doubleCborEncoded)

    /** Applies an argument to the program.
      *
      * @param arg
      *   the argument
      * @return
      *   the program with the argument applied
      */
    @targetName("applyArg")
    infix def $(arg: Term): Program = Program(version, Term.Apply(term, arg))

object Program:
    /** Deserializes a program from a double-CBOR-encoded hex string.
      *
      * @param doubleCborHex
      *   the double-CBOR-encoded hex string
      * @return
      *   the program
      */
    def fromDoubleCborHex(doubleCborHex: String): Program =
        val cbor = Cbor.decode(Hex.hexToBytes(doubleCborHex)).to[Array[Byte]].value
        val scriptFlat = Cbor.decode(cbor).to[Array[Byte]].value
        val debruijnedProgram = ProgramFlatCodec.decodeFlat(scriptFlat)
        debruijnedProgram.toProgram

    /** Deserializes a program from a CBOR-encoded hex string.
      *
      * @param cborHex
      *   the CBOR-encoded hex string
      * @return
      *   the program
      */
    def fromCborHex(cborHex: String): Program =
        val cbor = Cbor.decode(Hex.hexToBytes(cborHex)).to[Array[Byte]].value
        val debruijnedProgram = ProgramFlatCodec.decodeFlat(cbor)
        debruijnedProgram.toProgram

/** A De Bruijn-indexed program.
  *
  * A De Bruijn-indexed program is a versioned [[Term]] where the variables are indexed using De
  * Bruijn indices. A program must be De Bruijn-indexed before it can be evaluated.
  *
  * @param version
  *   the version of the program
  * @param term
  *   the term of the program
  */
case class DeBruijnedProgram private[uplc] (version: (Int, Int, Int), term: Term):
    def pretty: Doc =
        val (major, minor, patch) = version
        Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
          s"$major.$minor.$patch"
        ) + Doc.space + term.pretty + Doc.text(")")
    def toProgram: Program = DeBruijn.fromDeBruijnProgram(this)
