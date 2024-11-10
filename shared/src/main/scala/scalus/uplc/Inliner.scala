package scalus.uplc
import scalus.uplc.Term.*

/** Inlines identity function application */
object Inliner:
    /** Inlines identity function application */
    def apply(term: Term): Term = inlinePass(term)

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

        def go(t: Term, boundVars: Set[String]): Term = t match
            case Var(NamedDeBruijn(n, _)) =>
                if n == name && !boundVars.contains(n) then
                    // Only substitute if the variable is free (not bound)
                    replacement
                else t

            case LamAbs(n, body) =>
                if n == name then
                    // Variable is shadowed, don't substitute in body
                    t
                else
                    // Check if we need to alpha-rename to avoid capture
                    val replacementFreeVars = freeVars(replacement)
                    if n != name && replacementFreeVars.contains(n) then
                        // Need to alpha-rename to avoid capture
                        val freshN = freshName(n, boundVars ++ replacementFreeVars)
                        // Recursively substitute in the alpha-renamed body
                        LamAbs(
                          freshN,
                          go(substitute(body, n, Var(NamedDeBruijn(freshN))), boundVars + freshN)
                        )
                    else
                        // No capture possible, just recurse
                        LamAbs(n, go(body, boundVars + n))

            case Apply(f, arg) =>
                Apply(go(f, boundVars), go(arg, boundVars))

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
                    case Some(value) if canInline(value) => value
                    case _                               => term

            case Apply(f, arg) =>
                val inlinedF = go(f, env)
                val inlinedArg = go(arg, env)

                // Handle identity function application
                if isIdentityFn(inlinedF) then inlinedArg
                else
                    // Try beta reduction if possible
                    inlinedF match
                        case LamAbs(name, body) if canInline(inlinedArg) =>
                            // Perform capture-avoiding substitution
                            go(substitute(body, name, inlinedArg), env)
                        case _ =>
                            Apply(inlinedF, inlinedArg)

            case LamAbs(name, body) =>
                // Don't substitute in the lambda's body if name is in env
                LamAbs(name, go(body, env - name))

            case Force(t) => Force(go(t, env))
            case Delay(t) => Delay(go(t, env))

            case Constr(tag, args) =>
                Constr(tag, args.map(arg => go(arg, env)))

            case Case(scrutinee, cases) =>
                Case(
                  go(scrutinee, env),
                  cases.map(c => go(c, env))
                )

            // Leave other terms unchanged
            case t @ (Const(_) | Builtin(_) | Error) => t

        go(term, Map.empty)

    /** Check if a term is a pure value that can be inlined */
    private def canInline(term: Term): Boolean = term match
        case Const(_) => true
        case Var(_)   => true
        case _        => false

    /** Check if a term is an identity function */
    private def isIdentityFn(term: Term): Boolean = term match
        case LamAbs(name, Var(NamedDeBruijn(vname, _))) => name == vname
        case _                                          => false
