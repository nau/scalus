package scalus.uplc

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{Decoder, Encoder, Reader, Writer, DataItem as DI}
import org.typelevel.paiges.Doc
import scalus.uplc.Data.*
import scalus.utils.Utils
import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable

enum Term:
  case Var(name: String) extends Term
  case LamAbs(name: String, term: Term) extends Term
  case Apply(f: Term, arg: Term) extends Term
  case Force(term: Term) extends Term
  case Delay(term: Term) extends Term
  case Const(const: Constant) extends Term
  case Builtin(bn: DefaultFun) extends Term
  case Error extends Term

  def pretty: Doc = this match
    case Var(name) => Doc.text(name)
    case LamAbs(name, term) =>
      Doc.text("(") + Doc.text("lam") + Doc.space + Doc.text(name) + Doc.space + term.pretty + Doc
        .text(")")
    case Apply(f, arg) =>
      Doc.text("[") + f.pretty + Doc.space + arg.pretty + Doc.text("]")
    case Force(term) =>
      Doc.text("(") + Doc.text("force") + Doc.text(" ") + term.pretty + Doc.text(")")
    case Delay(term) =>
      Doc.text("(") + Doc.text("delay") + Doc.text(" ") + term.pretty + Doc.text(")")
    case Const(const) => Doc.text("(") + Doc.text("con") + Doc.space + const.pretty + Doc.text(")")
    case Builtin(bn)  => Doc.text("(") + Doc.text("builtin") + Doc.space + bn.pretty + Doc.text(")")
    case Error        => Doc.text("(error)")

object TermDSL:
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

  given constantAsData[A: Data.Lift]: Conversion[A, Data] with
    def apply(c: A): Data = summon[Data.Lift[A]].lift(c)

case class Program(version: (Int, Int, Int), term: Term):
  def pretty: Doc =
    val (major, minor, patch) = version
    Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
      s"$major.$minor.$patch"
    ) + Doc.space + term.pretty + Doc.text(")")

object DefaultFun1:
  lazy val cached: immutable.Map[String, DefaultFun] =
    DefaultFun.values.map(v => Utils.lowerFirst(v.toString) -> v).toMap

enum DefaultFun:
  // Integers
  case AddInteger extends DefaultFun
  case SubtractInteger extends DefaultFun
  case MultiplyInteger extends DefaultFun
  case DivideInteger extends DefaultFun
  case QuotientInteger extends DefaultFun
  case RemainderInteger extends DefaultFun
  case ModInteger extends DefaultFun
  case EqualsInteger extends DefaultFun
  case LessThanInteger extends DefaultFun
  case LessThanEqualsInteger extends DefaultFun
  // Bytestrings
  case AppendByteString extends DefaultFun
  case ConsByteString extends DefaultFun
  case SliceByteString extends DefaultFun
  case LengthOfByteString extends DefaultFun
  case IndexByteString extends DefaultFun
  case EqualsByteString extends DefaultFun
  case LessThanByteString extends DefaultFun
  case LessThanEqualsByteString extends DefaultFun
  // Cryptography and hashes
  case Sha2_256 extends DefaultFun
  case Sha3_256 extends DefaultFun
  case Blake2b_256 extends DefaultFun
  case VerifyEd25519Signature extends DefaultFun // formerly verifySignature
  case VerifyEcdsaSecp256k1Signature extends DefaultFun
  case VerifySchnorrSecp256k1Signature extends DefaultFun

  // Strings
  case AppendString extends DefaultFun
  case EqualsString extends DefaultFun
  case EncodeUtf8 extends DefaultFun
  case DecodeUtf8 extends DefaultFun

  // Bool
  case IfThenElse extends DefaultFun

  // Unit
  case ChooseUnit extends DefaultFun

  // Tracing
  case Trace extends DefaultFun

  // Pairs
  case FstPair extends DefaultFun

  case SndPair extends DefaultFun

  // Lists
  case ChooseList extends DefaultFun
  case MkCons extends DefaultFun
  case HeadList extends DefaultFun
  case TailList extends DefaultFun
  case NullList extends DefaultFun

  // Data
  // See Note [Pattern matching on built-in types].
  // It is convenient to have a "choosing" function for a data type that has more than two
  // constructors to get pattern matching over it and we may end up having multiple such data
  // types, hence we include the name of the data type as a suffix.
  case ChooseData extends DefaultFun
  case ConstrData extends DefaultFun
  case MapData extends DefaultFun
  case List extends DefaultFun
  case IData extends DefaultFun
  case BData extends DefaultFun
  case UnConstrData extends DefaultFun
  case UnMapData extends DefaultFun
  case UnListData extends DefaultFun
  case UnIData extends DefaultFun
  case UnBData extends DefaultFun
  case EqualsData extends DefaultFun
  case SerialiseData extends DefaultFun

  // Misc monomorphized constructors.
  // We could simply replace those with constants, but we use built-in functions for consistency
  // with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
  // See note [Representable built-in functions over polymorphic built-in types].
  case MkPairData extends DefaultFun
  case MkNilData extends DefaultFun
  case MkNilPairData extends DefaultFun

  def name: String = Utils.lowerFirst(this.toString)

  def pretty: Doc = Doc.text(name)

