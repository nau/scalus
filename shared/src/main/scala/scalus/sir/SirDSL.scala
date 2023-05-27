package scalus.sir

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{DataItem as DI, Decoder, Encoder, Reader, Writer}
import org.typelevel.paiges.{Doc, Style}
import scalus.uplc.{Constant, Data, DefaultFun, NamedDeBruijn}
import scalus.utils.Utils
import scalus.utils.Utils.bytesToHex
import cats.syntax.all.*

import java.util

object SirDSL:
  def applyToList(app: SIR): (SIR, List[SIR]) =
    app match
      case SIR.Apply(f, arg) =>
        val (f1, args) = applyToList(f)
        (f1, args :+ arg)
      case f => (f, Nil)

  // flatten LamAbs into a list of names and the body
  def lamAbsToList(lam: SIR): (List[String], SIR) =
    lam match
      case SIR.LamAbs(name, body) =>
        val (names, body1) = lamAbsToList(body)
        (name :: names, body1)
      case body => (Nil, body)

  def λ(names: String*)(term: SIR): SIR = lam(names: _*)(term)
  def lam(names: String*)(term: SIR): SIR = names.foldRight(term)(SIR.LamAbs(_, _))
  extension (term: SIR) def $(rhs: SIR) = SIR.Apply(term, rhs)

  given Conversion[DefaultFun, SIR] with
    def apply(bn: DefaultFun): SIR = SIR.Builtin(bn)

  given constantAsTerm[A: Constant.LiftValue]: Conversion[A, SIR] with
    def apply(c: A): SIR = SIR.Const(summon[Constant.LiftValue[A]].lift(c))

  given Conversion[Constant, SIR] with
    def apply(c: Constant): SIR = SIR.Const(c)
