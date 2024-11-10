package scalus.uplc
import scalus.uplc.Term.*

object Inliner:
    /** Counts number of occurrences of a variable in a term */
    private def countOccurrences(term: Term, name: String): Int = term match
        case Var(NamedDeBruijn(n, _)) => if n == name then 1 else 0
        case LamAbs(n, body) =>
            if n == name then 0 // Stop counting if shadowed
            else countOccurrences(body, name)
        case Apply(f, arg)   => countOccurrences(f, name) + countOccurrences(arg, name)
        case Force(t)        => countOccurrences(t, name)
        case Delay(t)        => countOccurrences(t, name)
        case Constr(_, args) => args.map(countOccurrences(_, name)).sum
        case Case(scrutinee, cases) =>
            countOccurrences(scrutinee, name) + cases.map(countOccurrences(_, name)).sum
        case Const(_) | Builtin(_) | Error => 0

    /** Checks if a term is safe to inline multiple times */
    private def isSafeToInline(term: Term): Boolean = term match
        case Const(_) => true
        case Var(_)   => true // Variables are safe to duplicate
        case _        => false

    /** Implements capture-avoiding substitution [x -> s]t */
    private def substitute(term: Term, name: String, replacement: Term): Term =
        // Get all free variables in the replacement term
        def freeVars(t: Term): Set[String] = t match
            case Var(NamedDeBruijn(n, _)) => Set(n)
            case LamAbs(n, body)          => freeVars(body) - n
            case Apply(f, a)              => freeVars(f) ++ freeVars(a)
            case Force(t)                 => freeVars(t)
            case Delay(t)                 => freeVars(t)
            case Constr(_, args)          => args.flatMap(freeVars).toSet
            case Case(scrutinee, cases) =>
                freeVars(scrutinee) ++ cases.flatMap(freeVars)
            case Const(_) | Builtin(_) | Error => Set.empty

        // Generate a fresh name that doesn't clash with any names in the set
        def freshName(base: String, avoid: Set[String]): String =
            if !avoid.contains(base) then base
            else
                var i = 0
                var fresh = s"${base}_$i"
                while avoid.contains(fresh) do
                    i += 1
                    fresh = s"${base}_$i"
                fresh

        // Compute free variables of replacement term once
        lazy val replacementFreeVars = freeVars(replacement)

        def go(t: Term, boundVars: Set[String]): Term = t match
            case Var(NamedDeBruijn(n, _)) =>
                if n == name && !boundVars.contains(n) then replacement
                else t

            case LamAbs(n, body) =>
                if n == name then t
                else if replacementFreeVars.contains(n) then
                    val freshN = freshName(n, boundVars ++ replacementFreeVars)
                    LamAbs(
                      freshN,
                      go(substitute(body, n, Var(NamedDeBruijn(freshN))), boundVars + freshN)
                    )
                else LamAbs(n, go(body, boundVars + n))

            case Apply(f, arg) => Apply(go(f, boundVars), go(arg, boundVars))

            case Force(t) => Force(go(t, boundVars))
            case Delay(t) => Delay(go(t, boundVars))

            case Constr(tag, args) =>
                Constr(tag, args.map(arg => go(arg, boundVars)))

            case Case(scrutinee, cases) =>
                Case(
                  go(scrutinee, boundVars),
                  cases.map(c => go(c, boundVars))
                )

            case t @ (Const(_) | Builtin(_) | Error) => t

        go(term, Set.empty)

    /** Main inlining function */
    def inlinePass(term: Term): Term =
        def go(term: Term, env: Map[String, Term]): Term = term match
            case Var(NamedDeBruijn(name, _)) =>
                env.get(name) match
                    case Some(value) => value
                    case _           => term

            case Apply(f, arg) =>
                val inlinedF = go(f, env)
                val inlinedArg = go(arg, env)
                // Try beta reduction if possible
                inlinedF match
                    // Inline identity functions
                    case LamAbs(name, Var(NamedDeBruijn(vname, _))) if name == vname =>
                        inlinedArg
                    case LamAbs(name, body) =>
                        // Count occurrences to decide if we should inline
                        val occurrences = countOccurrences(body, name)
                        if occurrences == 0 then
                            // Dead code elimination - variable is never used
                            go(body, env)
                        else if isSafeToInline(inlinedArg) then
                            go(substitute(body, name, inlinedArg), env)
                        else
                            // non-safe term - keep the lambda
                            Apply(inlinedF, inlinedArg)
                    case _ =>
                        Apply(inlinedF, inlinedArg)

            case LamAbs(name, body) => LamAbs(name, go(body, env - name))
            case Force(t)           => Force(go(t, env))
            case Delay(t)           => Delay(go(t, env))
            case Constr(tag, args)  => Constr(tag, args.map(arg => go(arg, env)))

            case Case(scrutinee, cases) =>
                Case(
                  go(scrutinee, env),
                  cases.map(c => go(c, env))
                )

            case t @ (Const(_) | Builtin(_) | Error) => t

        go(term, Map.empty)

    /** Check if a term is an identity function */
    private def isIdentityFn(term: Term): Boolean = term match
        case LamAbs(name, Var(NamedDeBruijn(vname, _))) => name == vname
        case _                                          => false
