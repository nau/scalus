package scalus.uplc

import cats.implicits.toShow
import cats.parse.Numbers.{bigInt, digits}
import cats.parse.Rfc5234.{alpha, digit, hexdig}
import cats.parse.{Numbers, Parser0, Parser as P}

class UplcParser {
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
      case "integer"    => DefaultUniInteger
      case "bytestring" => DefaultUniByteString
      case "string"     => DefaultUniString
      case "unit"       => DefaultUniUnit
      case "bool"       => DefaultUniBool
      case "data"       => DefaultUniData
      case _            => sys.error("unexpected default uni")
    }
    def list = symbol("list") *> inParens(self) map (in => DefaultUniApply(DefaultUniProtoList, in))
    def pair = symbol("pair") *> inParens(self) ~ inParens(self) map { case (a, b) =>
      DefaultUniApply(DefaultUniApply(DefaultUniProtoPair, a), b)
    }
    star | list | pair
  }

  def hexByte: P[Byte] = hexdig ~ hexdig map { case (a, b) =>
    Integer.valueOf(Array(a, b).mkString, 16).toByte
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
      Constant(DefaultUniApply(DefaultUniProtoList, t), ls)
    }

  def conPairOf(a: DefaultUni, b: DefaultUni): P[Constant] =
    inParens((constantOf(a) <* symbol(",")) ~ constantOf(b)) map { p =>
      Constant(DefaultUniApply(DefaultUniApply(DefaultUniProtoPair, a), b), p)
    }

  def constantOf(t: DefaultUni): P[Constant] = t match {
    case DefaultUniInteger => lexeme(bigInt).map(i => Constant(t, i))
    case DefaultUniUnit    => symbol("()").map(_ => Constant(t, ()))
    case DefaultUniBool =>
      lexeme(P.stringIn(Seq("True", "False"))).map {
        case "True"  => Constant(t, true)
        case "False" => Constant(t, false)
      }
    case DefaultUniByteString =>
      lexeme(P.char('#') *> hexByte.rep0.map(bs => Constant(t, bs)))
    case DefaultUniString =>
      lexeme(string).map(s => Constant(t, s)) // TODO validate escape sequences
    case DefaultUniData                          => sys.error("data constant not supported")
    case DefaultUniApply(DefaultUniProtoList, t) => conListOf(t)
    case DefaultUniApply(DefaultUniApply(DefaultUniProtoPair, a), b) => conPairOf(a, b)
    case _                                                           => sys.error("not implemented")
  }

  def constant: P[Constant] = for {
    uni <- defaultUni
    const <- constantOf(uni)
  } yield const

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

  def parseProgram(s: String): Either[String, Program] = {
    program.parse(s) match {
      case Right((_, result)) => Right(result)
      case Left(f)            => Left(f.show)
    }
  }
}
