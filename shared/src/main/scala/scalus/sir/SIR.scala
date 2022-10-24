package scalus.sir

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{Decoder, Encoder, Reader, Writer, DataItem as DI}
import org.typelevel.paiges.Doc
import scalus.uplc.Data.*
import scalus.uplc.{Constant, Data, DefaultFun, NamedDeBruijn}
import scalus.utils.Utils
import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable

case class Binding(name: String, value: SIR) {
  override def toString: String = s"Binding(\"$name\", $value)"
}

enum Recursivity:
  case NonRec, Rec

enum SIR:
  case Var(name: NamedDeBruijn) extends SIR
  case Let(recursivity: Recursivity, bindings: immutable.List[Binding], body: SIR) extends SIR
  case LamAbs(name: String, term: SIR) extends SIR
  case Apply(f: SIR, arg: SIR) extends SIR
  case Const(const: Constant) extends SIR
  case Builtin(bn: DefaultFun) extends SIR
  case Error(msg: String) extends SIR

  def pretty: Doc = this match
    case Var(name)                        => Doc.text(name.name)
    case Let(recursivity, bindings, body) => Doc.text("let")
    case LamAbs(name, term) =>
      Doc.text("(") + Doc.text(s"λ$name") + Doc.space + Doc.text(
        "->"
      ) + Doc.space + term.pretty + Doc
        .text(")")
    case a @ Apply(f, arg) =>
      val (t, args) = TermDSL.applyToList(a)
      val pa = args.foldRight(t.pretty) { (arg, acc) =>
        acc + Doc.space + arg.pretty
      }
      Doc.text("[") + pa + Doc.space + Doc.text("]")
    case Const(const) => Doc.text("(") + Doc.text("con") + Doc.space + const.pretty + Doc.text(")")
    case Builtin(bn)  => Doc.text("(") + Doc.text("builtin") + Doc.space + bn.pretty + Doc.text(")")
    case Error(_)     => Doc.text("(error)")

object TermDSL:
  def applyToList(app: SIR): (SIR, immutable.List[SIR]) =
    app match
      case SIR.Apply(f, arg) =>
        val (f1, args) = applyToList(f)
        (f1, args :+ arg)
      case f => (f, Nil)

  def λ(names: String*)(term: SIR): SIR = lam(names: _*)(term)
  def lam(names: String*)(term: SIR): SIR = names.foldRight(term)(SIR.LamAbs(_, _))
  extension (term: SIR) def $(rhs: SIR) = SIR.Apply(term, rhs)

  given Conversion[DefaultFun, SIR] with
    def apply(bn: DefaultFun): SIR = SIR.Builtin(bn)

  given constantAsTerm[A: Constant.LiftValue]: Conversion[A, SIR] with
    def apply(c: A): SIR = SIR.Const(summon[Constant.LiftValue[A]].lift(c))

  given Conversion[Constant, SIR] with
    def apply(c: Constant): SIR = SIR.Const(c)

  given constantAsData[A: Data.ToData]: Conversion[A, Data] with
    def apply(c: A): Data = summon[Data.ToData[A]].toData(c)

case class Program(version: (Int, Int, Int), term: SIR):
  def pretty: Doc =
    val (major, minor, patch) = version
    Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
      s"$major.$minor.$patch"
    ) + Doc.space + term.pretty + Doc.text(")")
