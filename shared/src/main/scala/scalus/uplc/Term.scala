package scalus.uplc

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{Decoder, Encoder, Reader, Writer, DataItem as DI}
import org.typelevel.paiges.Doc
import scalus.sir.SIR
import scalus.uplc.Data.*
import scalus.utils.Utils
import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable

object Compiler:
  def compile(e: Any): SIR = ???

case class NamedDeBruijn(name: String, index: Int = 0):
  assert(index >= 0)
  override def toString: String = s"$name@$index"

enum Term:
  case Var(name: NamedDeBruijn) extends Term
  case LamAbs(name: String, term: Term) extends Term
  case Apply(f: Term, arg: Term) extends Term
  case Force(term: Term) extends Term
  case Delay(term: Term) extends Term
  case Const(const: Constant) extends Term
  case Builtin(bn: DefaultFun) extends Term
  case Error(msg: String) extends Term

  def pretty: Doc = this match
    case Var(name) => Doc.text(name.name)
    case LamAbs(name, term) =>
      Doc.text("(") + Doc.text("lam") + Doc.space + Doc.text(name) + Doc.space + term.pretty + Doc
        .text(")")
    case a @ Apply(f, arg) =>
      Doc.text("[") + f.pretty + Doc.space + arg.pretty + Doc.text("]")
    case Force(term) =>
      Doc.text("(") + Doc.text("force") + Doc.text(" ") + term.pretty + Doc.text(")")
    case Delay(term) =>
      Doc.text("(") + Doc.text("delay") + Doc.text(" ") + term.pretty + Doc.text(")")
    case Const(const) => Doc.text("(") + Doc.text("con") + Doc.space + const.pretty + Doc.text(")")
    case Builtin(bn)  => Doc.text("(") + Doc.text("builtin") + Doc.space + bn.pretty + Doc.text(")")
    case Error(_)     => Doc.text("(error)")

object TermDSL:
  def applyToList(app: Term): (Term, immutable.List[Term]) =
    app match
      case Term.Apply(f, arg) =>
        val (f1, args) = applyToList(f)
        (f1, args :+ arg)
      case f => (f, Nil)

  def λ(names: String*)(term: Term): Term = lam(names: _*)(term)
  def lam(names: String*)(term: Term): Term = names.foldRight(term)(Term.LamAbs(_, _))
  extension (term: Term)
    def $(rhs: Term) = Term.Apply(term, rhs)
    def unary_! = Term.Force(term)
    def unary_~ = Term.Delay(term)

  given Conversion[DefaultFun, Term] with
    def apply(bn: DefaultFun): Term = Term.Builtin(bn)

  given constantAsTerm[A: Constant.LiftValue]: Conversion[A, Term] with
    def apply(c: A): Term = Term.Const(summon[Constant.LiftValue[A]].lift(c))

  given Conversion[Constant, Term] with
    def apply(c: Constant): Term = Term.Const(c)

  given constantAsData[A: Data.ToData]: Conversion[A, Data] with
    def apply(c: A): Data = summon[Data.ToData[A]].toData(c)

case class Program(version: (Int, Int, Int), term: Term):
  def pretty: Doc =
    val (major, minor, patch) = version
    Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
      s"$major.$minor.$patch"
    ) + Doc.space + term.pretty + Doc.text(")")
