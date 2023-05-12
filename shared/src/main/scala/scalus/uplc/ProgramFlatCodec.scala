package scalus.uplc

import io.bullet.borer.Decoder
import io.bullet.borer.Encoder
import io.bullet.borer.Reader
import io.bullet.borer.Tag.NegativeBigNum
import io.bullet.borer.Tag.Other
import io.bullet.borer.Tag.PositiveBigNum
import io.bullet.borer.Writer
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{DataItem => DI}
import org.typelevel.paiges.Doc
import scalus.*
import scalus.flat.DecoderState
import scalus.flat.EncoderState
import scalus.flat.Flat
import scalus.sir.PrettyPrinter
import scalus.sir.SIR
import scalus.uplc.Data.*
import scalus.utils.Utils
import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable

object ProgramFlatCodec:
  import FlatInstantces.given
  private val flatCodec = summon[Flat[DeBruijnedProgram]]

  def encodeFlat(p: Program): Array[Byte] =
    val deBruijned = DeBruijn.deBruijnProgram(p)
    encodeFlat(deBruijned)

  def encodeFlat(deBruijned: DeBruijnedProgram): Array[Byte] =
    // FIXME, why the hell + 2? +1 should always work with post align.
    val encoderState = new EncoderState(flatCodec.bitSize(deBruijned) / 8 + 2)
    flatCodec.encode(deBruijned, encoderState)
    encoderState.filler()
    val encoded = encoderState.result
    encoded

  def decodeFlat(encoded: Array[Byte]): DeBruijnedProgram =
    val decoderState = new DecoderState(encoded)
    flatCodec.decode(decoderState)
