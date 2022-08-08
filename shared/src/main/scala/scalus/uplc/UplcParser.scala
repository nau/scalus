package scalus.uplc

import fastparse.MultiLineWhitespace._
import fastparse._

class UplcParser {

  def number[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))
  def bigint[_: P]: P[BigInt] = P(CharIn("+\\-").? ~ CharIn("0-9").rep(1).!).map(BigInt(_))
  def parens[_: P, A](p: => P[A]): P[A] = P("(" ~ p ~ ")")
  def inBrackets[_: P, A](p: => P[A]): P[A] = P("[" ~/ p ~ "]")
  def name[_: P]: P[String] = P(CharIn("a-zA-Z").! ~~ CharsWhileIn("a-zA-Z0-9_'").repX.!).map {
    case (a, b) => a + b
  }

  def defaultUni[_: P]: P[DefaultUni] =
    P(
      StringIn("integer", "bytestring", "string", "unit", "bool", "data").!
    ).map {
      case "integer"    => DefaultUniInteger
      case "bytestring" => DefaultUniByteString
      case "string"     => DefaultUniString
      case "unit"       => DefaultUniUnit
      case "bool"       => DefaultUniBool
      case "data"       => DefaultUniData
      case _            => sys.error("unexpected default uni")
    }

  def hexDigitChar[_: P] = P(CharIn("0-9a-fA-F").!.map(d => Integer.parseInt(d, 16)))
  def hexByte[_: P] = P(hexDigitChar ~~ hexDigitChar).map { case (a, b) =>
    (a * 16 + b).toByte
  }

  def stringChars(c: Char) = c != '\"' && c != '\\'
  def escape[_: P] = P("\\" ~ CharIn("\"/\\\\bfnrt"))
  def strChars[_: P] = P( CharsWhile(stringChars) )
  def string[_: P] =
    P("\"" ~/ (strChars | escape).rep.! ~ "\"")

  def constantOf[_: P](t: DefaultUni): P[Constant] = P(t match {
    case DefaultUniInteger => bigint.map(i => Constant(t, i))
    case DefaultUniUnit    => "(" ~ ")".!.map(i => Constant(t, ()))
    case DefaultUniBool =>
      ("True" | "False").!.map {
        case "True"  => Constant(t, true)
        case "False" => Constant(t, false)
      }
    case DefaultUniByteString => "#" ~~ hexByte.rep.map(bs => Constant(t, bs))
    case DefaultUniString     => string.map(s => Constant(t, s)) // TODO validate escape sequences
    case DefaultUniData       => sys.error("data constant not supported")
    case _                    => sys.error("not implemented")
  })
  def constant[_: P] = P(for {
    uni <- defaultUni
    const <- constantOf(uni)
  } yield const)
  def conTerm[_: P]: P[Term] = P(parens("con" ~/ constant)).map(c => Const(c))
  def varTerm[_: P]: P[Term] = P(name.map(Var))
  def lamTerm[_: P]: P[Term] = P(parens("lam" ~/ name ~/ term).map { case (name, term) =>
    LamAbs(name, term)
  })
  def appTerm[_: P]: P[Term] = P(inBrackets(term ~ term.rep).map { case (f, args) =>
    args.foldLeft(f) { case (acc, arg) => Apply(acc, arg) }
  })
  def forceTerm[_: P]: P[Term] = P(parens("force" ~/ term).map(term => Force(term)))
  def delayTerm[_: P]: P[Term] = P(parens("delay" ~/ term).map(term => Delay(term)))
  def errorTerm[_: P]: P[Term] = P(parens("error")).map(_ => Error)
  def term[_: P]: P[Term] = P(
    conTerm | varTerm | appTerm | lamTerm | forceTerm | delayTerm | errorTerm
  )

  def programVersion[_: P]: P[(Int, Int, Int)] = P(number ~~ "." ~~ number ~~ "." ~ number)
  def program[_: P]: P[Program] = P(parens("program" ~ programVersion ~ term) map {
    case (a, b, c, t) =>
      Program((a, b, c), t)
  })

  def parseProgram(s: String): Either[String, Program] = {
    parse(s, program(_)) match {
      case Parsed.Success(program, _) => Right(program)
      case f: Parsed.Failure          => Left(f.msg)
    }
  }
}
