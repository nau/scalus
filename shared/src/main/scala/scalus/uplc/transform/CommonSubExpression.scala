package scalus.uplc.transform

import scalus.*
import scalus.uplc.Meaning
import scalus.uplc.NamedDeBruijn
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CommonSubExpression {
    def apply(term: Term): Term =
        val (transformed, logs) = extractPass(term)
        transformed

    /** Main inlining function */
    def extractPass(term: Term): (Term, collection.Seq[String]) =
        val logs = ArrayBuffer.empty[String]
        var counter = 0
        def freshName(base: String, env: Map[String, Term]): String =
            var name = base
            while env.contains(name) do
                name = s"${base}_$counter"
                counter += 1
            name
        val extracted = mutable.Map.empty[Term, String]

        def go(term: Term, env: Map[String, Term]): Term = term match
            case Apply(f, arg)      => Apply(go(f, env), go(arg, env))
            case LamAbs(name, body) => LamAbs(name, go(body, env - name))
            case Force(Force(Builtin(bn)))
                if Meaning.allBuiltins.BuiltinMeanings(bn).typeScheme.numTypeVars == 2 =>
                val name = extracted.getOrElseUpdate(term, freshName(s"builtin_$bn", env))
                Var(NamedDeBruijn(name))
            case Force(Builtin(bn))
                if Meaning.allBuiltins.BuiltinMeanings(bn).typeScheme.numTypeVars == 1 =>
                val name = extracted.getOrElseUpdate(term, freshName(s"builtin_$bn", env))
                Var(NamedDeBruijn(name))
            case Force(t)          => Force(go(t, env))
            case Delay(t)          => Delay(go(t, env))
            case Constr(tag, args) => Constr(tag, args.map(arg => go(arg, env)))
            case Case(scrutinee, cases) =>
                Case(
                  go(scrutinee, env),
                  cases.map(c => go(c, env))
                )
            case _: Var | _: Const | _: Builtin | Error => term
        val term1 = go(term, Map.empty)
        val withVars = extracted.foldRight(term1) { case ((term, name), acc) =>
            LamAbs(name, acc) $ term
        }
        (withVars, logs)
}


