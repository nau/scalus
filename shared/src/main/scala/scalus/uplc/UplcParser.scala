package scalus.uplc

import cats.implicits.toShow
import cats.parse.Numbers.{bigInt, digits}
import cats.parse.Rfc5234.{alpha, digit, hexdig}
import cats.parse.{Numbers, Parser0, Parser as P}
import scalus.uplc.DefaultUni
import scalus.uplc.DefaultUni.{ProtoList, ProtoPair}
import scalus.uplc.Term.*

class UplcParser:
  private[this] val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private[this] val whitespaces0: Parser0[Unit] = whitespace.rep0.void

  def number: P[Int] = digits.map(_.toList.mkString.toInt)
  def bigint: P[String] = Numbers.bigInt.map(_.toString)
  def inParens[A](p: P[A]): P[A] = p.between(symbol("("), symbol(")"))
  def inBrackets[A](p: P[A]): P[A] = p.between(symbol("["), symbol("]"))
  def lexeme[A](p: P[A]): P[A] = p <* whitespaces0
  def symbol(s: String): P[Unit] = P.string(s).void <* whitespaces0

  def name: P[String] = alpha ~ (alpha | digit | P.charIn("_'")).rep0 map { case (a, b) =>
    (a :: b).mkString
  }

  def defaultUni: P[DefaultUni] = P.recursive { self =>
    def star = lexeme(
      P.stringIn(Seq("integer", "bytestring", "string", "unit", "bool", "data"))
    ).map {
      case "integer"    => DefaultUni.Integer
      case "bytestring" => DefaultUni.ByteString
      case "string"     => DefaultUni.String
      case "unit"       => DefaultUni.Unit
      case "bool"       => DefaultUni.Bool
      case "data"       => DefaultUni.Data
      case _            => sys.error("unexpected default uni")
    }
    def list =
      symbol("list") *> inParens(self) map (in => DefaultUni.Apply(ProtoList, in))
    def pair = symbol("pair") *> inParens(self) ~ inParens(self) map { case (a, b) =>
      DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b)
    }
    star | list | pair
  }

  def hexByte: P[Byte] = hexdig ~ hexdig map { case (a, b) =>
    java.lang.Integer.valueOf(Array(a, b).mkString, 16).toByte
  }

  def stringChars(c: Char): Boolean = c != '\"' && c != '\\'
  // TODO check this is the same as in the Haskell parser
  def escape: P[String] = (P.char('\\') *> P.charIn("\"/\\\\bfnrt")).map(_.toString)
  def strChars: P[String] = P.charsWhile(stringChars)
  def string: P[String] = P.char('\"') *> (strChars | escape).rep0 <* P.char('\"') map {
    _.mkString
  }

  def conListOf(t: DefaultUni): P[Constant] =
    symbol("[") *> constantOf(t).repSep0(symbol(",")) <* symbol("]") map { ls =>
      Constant(DefaultUni.Apply(ProtoList, t), ls)
    }

  def conPairOf(a: DefaultUni, b: DefaultUni): P[Constant] =
    inParens((constantOf(a) <* symbol(",")) ~ constantOf(b)) map { p =>
      Constant(DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b), p)
    }

  def constantOf(t: DefaultUni): P[Constant] = t match
    case DefaultUni.Integer => lexeme(bigInt).map(i => Constant(t, i))
    case DefaultUni.Unit    => symbol("()").map(_ => Constant(t, ()))
    case DefaultUni.Bool =>
      lexeme(P.stringIn(Seq("True", "False"))).map {
        case "True"  => Constant(t, true)
        case "False" => Constant(t, false)
      }
    case DefaultUni.ByteString =>
      lexeme(P.char('#') *> hexByte.rep0.map(bs => Constant(t, bs)))
    case DefaultUni.String =>
      lexeme(string).map(s => Constant(t, s)) // TODO validate escape sequences
    case DefaultUni.Data                => sys.error("data constant not supported")
    case DefaultUni.Apply(ProtoList, t) => conListOf(t)
    case DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b) => conPairOf(a, b)
    case _                                                   => sys.error("not implemented")

  def constant: P[Constant] = for
    uni <- defaultUni
    const <- constantOf(uni)
  yield const

  def conTerm: P[Term] = inParens(symbol("con") *> constant).map(c => Const(c))

  def programVersion: P[(Int, Int, Int)] =
    lexeme((number <* P.char('.')) ~ (number <* P.char('.')) ~ number) map {
      case ((major, minor), patch) => (major, minor, patch)
    }

  def varTerm: P[Var] = lexeme(name).map(Var.apply)

  def term: P[Term] = P.recursive { self =>
    def lamTerm = inParens(symbol("lam") ~ lexeme(name) ~ self) map { case ((_, name), term) =>
      LamAbs(name, term)
    }
    def appTerm = inBrackets(self ~ self.rep).map { case (f, args) =>
      args.foldLeft(f) { case (acc, arg) => Apply(acc, arg) }
    }
    def forceTerm = inParens(symbol("force") *> self).map(Force.apply)
    def delayTerm = inParens(symbol("delay") *> self).map(Delay.apply)
    def errorTerm = inParens(symbol("error")).map(_ => Error)
    varTerm.backtrack | conTerm.backtrack | lamTerm.backtrack | appTerm.backtrack | forceTerm.backtrack | delayTerm.backtrack | errorTerm.backtrack
  }

  def program: P[Program] = inParens(symbol("program") *> programVersion ~ term) map {
    case (v, term) =>
      Program(v, term)
  }

  def parseProgram(s: String): Either[String, Program] =
    program.parse(s) match
      case Right((_, result)) => Right(result)
      case Left(f)            => Left(f.show)
