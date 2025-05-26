package scalus.sir.lowering

import scalus.sir.*
import scalus.uplc.*
import scalus.uplc.TermDSL.*

object Lowering {

    extension (fun: DefaultFun) def tpf: Term = builtinTerms(fun)

    def genError(
        msg: String
    )(using lctx: LoweringContext): Term =
        if lctx.generateErrorTraces then
            !(DefaultFun.Trace.tpf $ Term.Const(
              Constant.String(msg)
            ) $ ~Term.Error)
        else Term.Error

    private lazy val builtinTerms: Map[DefaultFun, Term] = {
        def forceBuiltin(scheme: TypeScheme, term: Term): Term = scheme match
            case TypeScheme.All(_, t) => Term.Force(forceBuiltin(t, term))
            case _                    => term

        Meaning.allBuiltins.BuiltinMeanings.map((bi, rt) =>
            bi -> forceBuiltin(rt.typeScheme, Term.Builtin(bi))
        )
    }

    def lowerSIR(sir: SIR, localScope: LocalScope)(using lctx: LoweringContext): LoweredValue = {
        sir match
            case SIR.Decl(data, term) =>
                lctx.decls.put(data.name, data).foreach { decl =>
                    // TODO: pass logger.
                    println(s"Data declaration ${data.name} already exists")
                }
                lowerSIR(term, localScope)
            case constr @ SIR.Constr(name, decl, args, tp, anns) =>
                SIRTypeUplcGenerator(decl.tp).genConstr(constr)
            case sirMatch @ SIR.Match(scrutinee, cases, rhsType, anns) =>
                SIRTypeUplcGenerator(scrutinee.tp).genMatch(sirMatch)
            case SIR.Var(name, tp, _) =>
                
                LoweredValue(
                  sir,
                  Term.Var(NamedDeBruijn(name)),
                  SIRTypeUplcGenerator(tp).defaultRepresentation
                )
            case SIR.ExternalVar(moduleName, name, tp, _) =>
                LoweredValue(
                  sir,
                  Term.Var(NamedDeBruijn(name)),
                  SIRTypeUplcGenerator(tp).defaultRepresentation
                )
            case sirLet @ SIR.Let(recursivity, bindings, body, anns) =>
                lowerLet(sirLet)
            case SIR.LamAbs(param, term, anns) =>
                val body = lowerSIR(term)
                LoweredValue(sir, Term.LamAbs(param.name, body.term), body.representation)
            case app: SIR.Apply =>
                lowerApp(app)
            case sel @ SIR.Select(scrutinee, field, tp, anns) =>
                SIRTypeUplcGenerator(scrutinee.tp).genSelect(sel)
            case SIR.Const(const, tp, anns) =>
                LoweredValue(
                  sir,
                  Term.Const(const),
                  LoweredValueRepresentation.constRepresentation(tp)
                )
            case SIR.And(lhs, rhs, anns) =>
                lowerSIR(
                  SIR.IfThenElse(
                    lhs,
                    rhs,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean, anns),
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.Or(lhs, rhs, anns) =>
                lowerSIR(
                  SIR.IfThenElse(
                    lhs,
                    SIR.Const(Constant.Bool(true), SIRType.Boolean, anns),
                    rhs,
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.Not(term, anns) =>
                lowerSIR(
                  SIR.IfThenElse(
                    term,
                    SIR.Const(Constant.Bool(false), SIRType.Boolean, anns),
                    SIR.Const(Constant.Bool(true), SIRType.Boolean, anns),
                    SIRType.Boolean,
                    anns
                  )
                )
            case SIR.IfThenElse(cond, t, f, tp, anns) =>
                val loweredCond = lowerSIR(cond)
                val loweredT = lowerSIR(t)
                val loweredF = lowerSIR(f)
                val resRepresentation = loweredT.representation // evristic - most often used
                LoweredValue(
                  sir,
                  Term.Force(
                    Term.Apply(
                      Term.Apply(
                        Term.Apply(
                          builtinTerms(DefaultFun.IfThenElse),
                          loweredCond.asTerm.term
                        ),
                        Term.Delay(loweredT.toRepresentation(resRepresentation).term)
                      ),
                      Term.Delay(loweredF.toRepresentation(resRepresentation).term)
                    )
                  ),
                  resRepresentation
                )
            case SIR.Builtin(bn, _, _) => LoweredValue(sir, bn.tpf, SIRVarStorage.ScottEncoding)
            case SIR.Error(msg, anns, cause) =>
                val term =
                    if lctx.generateErrorTraces then
                        !(DefaultFun.Trace.tpf $ Term.Const(
                          Constant.String(msg)
                        ) $ ~Term.Error)
                    else Term.Error
                LoweredValue(sir, term, SIRVarStorage.ScottEncoding)
    }

    private def lowerLet(sirLet: SIR.Let)(using lctx: LoweringContext): LoweredValue = {
        val loweredBody = lowerSIR(sirLet.body)
        val term = sirLet match
            case SIR.Let(recursivity, bindings, body, anns) =>
                if recursivity == Recursivity.NonRec then
                    bindings.foldRight(loweredBody.term) { case (Binding(name, rhs), body) =>
                        val loweredRhs = lowerSIR(rhs).toRepresentation(
                          SIRTypeUplcGenerator(rhs.tp).defaultRepresentation
                        )
                        Term.Apply(Term.LamAbs(name, body), loweredRhs.term)
                    }
                else
                    bindings match
                        case List(Binding(name, rhs)) =>
                            /*  let rec f x = f (x + 1)
                                in f 0
                                (\f -> f 0) (Z (\f. \x. f (x + 1)))
                             */
                            lctx.zCombinatorNeeded = true
                            val loweredRhs = lowerSIR(rhs).toRepresentation(
                              SIRTypeUplcGenerator(rhs.tp).defaultRepresentation
                            )
                            val fixed =
                                Term.Apply(
                                  Term.Var(NamedDeBruijn("__z_combinator__")),
                                  Term.LamAbs(name, loweredRhs.term)
                                )
                            Term.Apply(Term.LamAbs(name, loweredBody.term), fixed)
                        case Nil =>
                            sys.error(
                              s"Empty let binding at ${sirLet.anns.pos.file}:${sirLet.anns.pos.startLine}"
                            )
                        case _ =>
                            sys.error(
                              s"Mutually recursive bindings are not supported: $bindings at ${sirLet.anns.pos.file}:${sirLet.anns.pos.startLine}"
                            )
        LoweredValue(sirLet, term, loweredBody.representation)
    }

    private def lowerApp(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {

        def extractTypeParamsAndFirstArg(tp: SIRType): (List[SIRType.TypeVar], SIRType) =
            tp match
                case SIRType.TypeLambda(params, t) =>
                    val (nextParams, firstArg) = extractTypeParamsAndFirstArg(t)
                    (params ++ nextParams, firstArg)
                case SIRType.Fun(arg, res) =>
                    (Nil, arg)
                case SIRType.TypeProxy(ref) =>
                    extractTypeParamsAndFirstArg(ref)
                case SIRType.TypeVar(name, _) =>
                    sys.error(
                      s"Unexpected type variable ${tp.show} at ${app.anns.pos.file}:${app.anns.pos.startLine}"
                    )
                case _ =>
                    sys.error(
                      s"Expected a function type, but got $tp at ${app.anns.pos.file}:${app.anns.pos.startLine}"
                    )

        val (typeParams, firstArg) = extractTypeParamsAndFirstArg(app.tp)

        val expectedRepresentation = SIRTypeUplcGenerator(firstArg).defaultRepresentation

        val fun = lowerSIR(app.f)
        val arg = lowerSIR(app.arg).toRepresentation(expectedRepresentation)

        LoweredValue(
          app,
          Term.Apply(fun.term, arg.term),
          SIRTypeUplcGenerator(app.tp).defaultRepresentation
        )
    }

}
