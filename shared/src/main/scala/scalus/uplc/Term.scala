package scalus.uplc

import io.bullet.borer.Cbor
import org.typelevel.paiges.Doc
import scalus.*
import scalus.builtin.Data.*
import scalus.utils.Hex

import scala.collection.immutable

case class NamedDeBruijn(name: String, index: Int = 0):
    assert(index >= 0)
    override def toString: String =
        if index == 0 then s"NamedDeBruijn(\"$name\")"
        else s"NamedDeBruijn(\"$name\", $index)"

enum Term:
    case Var(name: NamedDeBruijn) extends Term
    case LamAbs(name: String, term: Term) extends Term
    case Apply(f: Term, arg: Term) extends Term
    case Force(term: Term) extends Term
    case Delay(term: Term) extends Term
    case Const(const: Constant) extends Term
    case Builtin(bn: DefaultFun) extends Term
    case Error extends Term
    case Constr(tag: Long, args: immutable.List[Term])
    case Case(arg: Term, cases: immutable.List[Term])

    override def toString: String = this match
        case Var(name)          => s"Var(NamedDeBruijn(\"${name.name}\"))"
        case LamAbs(name, term) => s"LamAbs(\"$name\", $term)"
        case Apply(f, arg)      => s"Apply($f, $arg)"
        case Force(term)        => s"Force($term)"
        case Delay(term)        => s"Delay($term)"
        case Const(const)       => s"Const($const)"
        case Builtin(bn)        => s"Builtin($bn)"
        case Error              => "Error"
        case Constr(tag, args)  => s"Constr($tag, ${args.mkString(", ")})"
        case Case(arg, cases)   => s"Case($arg, ${cases.mkString(", ")})"

case class Program(version: (Int, Int, Int), term: Term):
    lazy val flatEncoded = ProgramFlatCodec.encodeFlat(this)
    lazy val cborEncoded = Cbor.encode(flatEncoded).toByteArray
    lazy val doubleCborEncoded = Cbor.encode(cborEncoded).toByteArray
    lazy val doubleCborHex = Hex.bytesToHex(doubleCborEncoded)

case class DeBruijnedProgram private[uplc] (version: (Int, Int, Int), term: Term):
    def pretty: Doc =
        val (major, minor, patch) = version
        Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
          s"$major.$minor.$patch"
        ) + Doc.space + term.pretty + Doc.text(")")
