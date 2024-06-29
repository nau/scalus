package scalus.uplc

import scalus.BaseValidatorSpec
import scalus.*
import scalus.uplc.eval.*

import scala.language.implicitConversions
import scala.io.Source.fromFile

class PlutusConformanceSpec extends BaseValidatorSpec:

    private type EvalFailure = "evaluation failure"
    private type ParseError = "parse error"
    private type Error = EvalFailure | ParseError
    private def parseExpected(code: String): Either[Error, Program] = {
        code match
            case "evaluation failure" => Left("evaluation failure")
            case "parse error"        => Left("parse error")
            case _ =>
                UplcParser().parseProgram(code) match
                    case Left(value) => fail(s"Unexpected parse error: $value")
                    case Right(program) =>
                        Right(program.copy(term = DeBruijn.deBruijnTerm(program.term)))

    }

    private def eval(code: String): Either[Error, Program] = {
        val parser = UplcParser()
        parser.parseProgram(code) match
            case Right(program) =>
                try Right(program.copy(term = VM.evaluateProgram(program)))
                catch case e: MachineError => Left("evaluation failure")
            case Left(e) =>
                Left("parse error")
    }

    private def check(name: String) =
        val path =
            s"../plutus-conformance/test-cases/uplc/evaluation"
        val code = fromFile(s"$path/$name.uplc").mkString
        val expected = fromFile(s"$path/$name.uplc.expected").mkString
        test(name) {
            // println(eval(code).pretty.render(80))
            assert(eval(code) == parseExpected(expected))
        }

    check("builtin/semantics/addInteger/addInteger1/addInteger1")
    check("builtin/semantics/addInteger/addInteger-uncurried/addInteger-uncurried")
    check("builtin/semantics/equalsInteger/equalsInteger1/equalsInteger1")
    check("builtin/semantics/ifThenElse/ifThenElse-1/ifThenElse-1")

    // constr/case
    check("term/constr/constr-1/constr-1")
    check("term/constr/constr-2/constr-2")
    check("term/constr/constr-3/constr-3")
    check("term/constr/constr-4/constr-4")
    check("term/constr/constr-5/constr-5")
    check("term/constr/constr-6/constr-6")
    check("term/case/case-1/case-1")
    check("term/case/case-2/case-2")
    check("term/case/case-3/case-3")
    check("term/case/case-4/case-4")
    check("term/case/case-5/case-5")
    check("term/case/case-6/case-6")
    check("term/case/case-7/case-7")
    check("term/case/case-8/case-8")
    check("term/case/case-9/case-9")

    // Examples
    check("example/factorial/factorial")
    check("example/fibonacci/fibonacci")
