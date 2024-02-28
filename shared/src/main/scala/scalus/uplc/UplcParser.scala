package scalus.uplc

import cats.implicits.toShow
import cats.parse.Numbers.{bigInt, digits}
import cats.parse.Rfc5234.{alpha, digit, hexdig}
import cats.parse.{Numbers, Parser as P, Parser0}
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.uplc.DefaultUni.{asConstant, ProtoList, ProtoPair}
import scalus.uplc.Term.*
import scalus.utils.Utils

import scala.collection.immutable

object UplcParser:
    private[this] val whitespace: P[Unit] = P.charIn(" \t\r\n").void
    private[this] val whitespaces0: Parser0[Unit] = whitespace.rep0.void
    private lazy val cached: immutable.Map[String, DefaultFun] =
        DefaultFun.values.map(v => Utils.lowerFirst(v.toString) -> v).toMap

    def number: P[Int] = digits.map(_.toList.mkString.toInt)
    def long: P[Long] = digits.map(_.toList.mkString.toLong)
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
            inParens(symbol("list") *> self) map (in => DefaultUni.Apply(ProtoList, in))
        def pair = inParens(symbol("pair") *> self ~ self) map { case (a, b) =>
            DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b)
        }
        star.backtrack | list.backtrack | pair
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
            Constant.List(t, ls)
        }

    def conPairOf(a: DefaultUni, b: DefaultUni): P[Constant] =
        inParens((constantOf(a) <* symbol(",")) ~ constantOf(b)) map { p =>
            Constant.Pair(p._1, p._2)
        }

    def bytestring: P[ByteString] = P.char('#') *> hexByte.rep0.map(bs => ByteString(bs: _*))
    def constantOf(t: DefaultUni, expectDataParens: Boolean = false): P[Constant] = t match
        case DefaultUni.Integer => lexeme(bigInt).map(i => Constant.Integer(i))
        case DefaultUni.Unit    => symbol("()").map(_ => Constant.Unit)
        case DefaultUni.Bool =>
            lexeme(P.stringIn(Seq("True", "False"))).map {
                case "True"  => Constant.Bool(true)
                case "False" => Constant.Bool(false)
            }
        case DefaultUni.ByteString =>
            lexeme(bytestring.map(asConstant))
        case DefaultUni.String =>
            lexeme(string).map(s => asConstant(s)) // TODO validate escape sequences
        case DefaultUni.Data =>
            (if expectDataParens then inParens(dataTerm) else dataTerm).map(asConstant)
        case DefaultUni.Apply(ProtoList, t)                      => conListOf(t)
        case DefaultUni.Apply(DefaultUni.Apply(ProtoPair, a), b) => conPairOf(a, b)
        case _                                                   => sys.error("not implemented")

    def dataTerm: P[Data] = P.recursive { self =>
        def args: P[List[Data]] =
            symbol("[") *> lexeme(self).repSep0(symbol(",")) <* symbol("]")
        def dataConstr: P[Data] =
            symbol("Constr") *> lexeme(long) ~ args map Data.Constr.apply

        def dataList: P[Data] = symbol("List") *> args map Data.List.apply
        def dataMap: P[Data] = symbol("Map") *>
            symbol("[") *> dataPair.repSep0(symbol(",")) <* symbol("]") map Data.Map.apply

        def dataB: P[Data] = symbol("B") *> lexeme(bytestring) map Data.B.apply
        def dataI: P[Data] = symbol("I") *> lexeme(Numbers.bigInt) map Data.I.apply
        def dataPair: P[(Data, Data)] = inParens((self <* symbol(",")) ~ self)

        dataConstr | dataMap | dataList | dataB | dataI
    }

    def constant: P[Constant] = for
        uni <- defaultUni
        const <- constantOf(uni, expectDataParens = true)
    yield const

    def conTerm: P[Term] = inParens(symbol("con") *> constant).map(c => Const(c))

    def builtinFunction: P[DefaultFun] = lexeme(
      name.flatMap(name =>
          cached.get(name) match
              case Some(f) => P.pure(f)
              case None    => P.failWith(s"unknown builtin function: $name")
      )
    )

    def builtinTerm: P[Builtin] =
        inParens(symbol("builtin") *> builtinFunction).map(n => Builtin(n))

    def programVersion: P[(Int, Int, Int)] =
        lexeme((number <* P.char('.')) ~ (number <* P.char('.')) ~ number) map {
            case ((major, minor), patch) => (major, minor, patch)
        }

    def varTerm: P[Var] = lexeme(name).map(n => Var(NamedDeBruijn(n)))

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
        varTerm.backtrack
            | builtinTerm.backtrack
            | conTerm.backtrack
            | lamTerm.backtrack
            | appTerm.backtrack
            | forceTerm.backtrack
            | delayTerm.backtrack
            | errorTerm.backtrack
    }

    def program: P[Program] = inParens(symbol("program") *> programVersion ~ term) map {
        case (v, term) =>
            Program(v, term)
    }

    def parseProgram(s: String): Either[String, Program] =
        program.parse(s) match
            case Right((_, result)) => Right(result)
            case Left(f)            => Left(f.show)
