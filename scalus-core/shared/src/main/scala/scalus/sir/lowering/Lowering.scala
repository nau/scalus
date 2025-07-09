package scalus.sir.lowering

import org.typelevel.paiges.Doc
import scalus.sir.*
import scalus.sir.Recursivity.NonRec
import scalus.sir.lowering.typegens.{ProductCaseUplcOnlySirTypeGenerator, RepresentationProxyLoweredValue, SirTypeUplcGenerator}
import scalus.sir.lowering.LoweredValue.Builder.*
import scalus.uplc.*

import scala.annotation.unused
import scala.util.control.NonFatal
import scalus.pretty

object Lowering {

    extension (fun: DefaultFun) def tpf: Term = builtinTerms(fun)

    def forcedBuiltin(fun: DefaultFun): Term = builtinTerms(fun)

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

    def lowerSIR(sir: SIR)(using lctx: LoweringContext): LoweredValue = {
        lctx.nestingLevel += 1
        val retval = sir match
            case SIR.Decl(data, term) =>
                lctx.decls.put(data.name, data).foreach { decl =>
                    // TODO: pass logger.
                    println(s"Data declaration ${data.name} already exists")
                }
                lowerSIR(term)
            case constr @ SIR.Constr(name, decl, args, tp, anns) =>
                val resolvedType = lctx.resolveTypeVarIfNeeded(tp)
                val typeGenerator = lctx.typeGenerator(resolvedType)
                typeGenerator.genConstr(constr)
            case sirMatch @ SIR.Match(scrutinee, cases, rhsType, anns) =>
                val loweredScrutinee = lowerSIR(scrutinee)
                (scrutinee.tp, loweredScrutinee.sirType) match {
                    case (tv1: SIRType.TypeVar, tv2: SIRType.TypeVar) =>
                        println("both scrutinee and loweredScrutinee are type variables")
                    case (_, tv2: SIRType.TypeVar) =>
                        println("loweredScrutinee is typed as type variable, but scrutinee is not")
                        println(s"scrutinee: ${scrutinee.pretty.render(100)}")
                        println(s"loweredScrutinee: ${loweredScrutinee.pretty.render(100)}")
                        println(s"lowering scrutinee in debug:")
                        lctx.debug = true
                        @unused val unused = lowerSIR(scrutinee)
                    case _ =>
                }
                lctx.typeGenerator(scrutinee.tp).genMatch(sirMatch, loweredScrutinee)
            case SIR.Var(name, tp, anns) =>
                lctx.scope.getByName(name) match
                    case Some(value) =>
                        // TODO: check types are correct
                        value.sirType match {
                            case tv: SIRType.TypeVar =>
                                // if this is type variable, try to resolve it
                                lctx.tryResolveTypeVar(tv) match
                                    case Some(resolvedType) =>
                                        val gen = lctx.typeGenerator(resolvedType)
                                        val representation =
                                            if tv.isBuiltin then
                                                gen.defaultRepresentation(resolvedType)
                                            else gen.defaultTypeVarReperesentation(resolvedType)
                                        new RepresentationProxyLoweredValue(
                                          value,
                                          representation,
                                          anns.pos
                                        ) {
                                            override def sirType: SIRType = resolvedType
                                        }
                                    case None =>
                                        // TODO!!!: find, hiow this can be possible and insert cheks there
                                        //  [throw]
                                        val gen = lctx.typeGenerator(tp)
                                        val repr =
                                            if tv.isBuiltin then gen.defaultRepresentation(tp)
                                            else gen.defaultTypeVarReperesentation(tp)
                                        new RepresentationProxyLoweredValue(
                                          value,
                                          repr,
                                          anns.pos
                                        ) {
                                            override def sirType: SIRType = tp
                                        }
                            case _ => value

                        }
                    case None =>
                        throw LoweringException(
                          s"Variable $name not found in the scope at ${anns.pos.file}:${anns.pos.startLine}",
                          anns.pos
                        )
            case ev @ SIR.ExternalVar(moduleName, name, tp, _) =>
                // SIRLinker made usual variable from names.
                val myVar = lctx.scope.getByName(name) match
                    case Some(value) => value
                    case None =>
                        throw LoweringException(
                          s"External variable $name not found in the scope at ${ev.anns.pos.file}:${ev.anns.pos.startLine}",
                          ev.anns.pos
                        )
                myVar
                // StaticLoweredValue(
                //  ev,
                //  Term.Var(NamedDeBruijn(name)),
                //  SirTypeUplcGenerator(tp).defaultRepresentation
                // )
            case sirLet @ SIR.Let(recursivity, bindings, body, anns) =>
                // don;t generate FromData/ToData (now handled by Data Representation)
                val nBindings =
                    bindings.filterNot(b => isFromDataName(b.name) || (isToDataName(b.name)))
                if nBindings.isEmpty then lowerSIR(body)
                else lowerLet(sirLet.copy(bindings = nBindings))
            case SIR.LamAbs(param, term, typeParams, anns) =>
                // TODO: add type params to context.  Now we do all computations in
                // new context, so type params are assumed implicitly
                lvLamAbs(
                  param,
                  lctx.typeGenerator(param.tp).defaultRepresentation(param.tp),
                  _id => summon[LoweringContext].lower(term),
                  anns.pos
                )
            case app: SIR.Apply =>
                lowerApp(app)
            case sel @ SIR.Select(scrutinee, field, tp, anns) =>
                val loweredScrutinee = lowerSIR(scrutinee)
                loweredScrutinee.sirType match {
                    case tv: SIRType.TypeVar =>
                        scrutinee.tp match
                            case tp1: SIRType.TypeVar =>
                            //
                            case other =>
                                println(
                                  "lowered scrutinee is typed as type variable, but scrutinee is not"
                                )
                                println(s"scrutinee: $scrutinee")
                                println(s"loweredScrutinee: $loweredScrutinee")
                                println(s"scrutinee.tp: ${scrutinee.tp.show}")
                                println(
                                  s"resolved typevar: ${lctx.typeUnifyEnv.filledTypes.get(tv)}, typeVae = ${tv}"
                                )
                                println(
                                  s"loweredScrutinee.sirType: ${loweredScrutinee.sirType.show}, representation: ${loweredScrutinee.representation}"
                                )
                                println(s"lowered scrutinee crerated at:")
                                loweredScrutinee.createdEx.printStackTrace()
                                ???
                    case _ =>
                }
                SirTypeUplcGenerator(loweredScrutinee.sirType).genSelect(sel, loweredScrutinee)
            case sirConst @ SIR.Const(const, tp, anns) =>
                StaticLoweredValue(
                  sirConst,
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
                val loweredCond =
                    lowerSIR(cond).toRepresentation(PrimitiveRepresentation.Constant, cond.anns.pos)
                val loweredT = lowerSIR(t)
                val loweredF = lowerSIR(f)
                lvIfThenElse(loweredCond, loweredT, loweredF, anns.pos)
            case SIR.Cast(expr, tp, anns) =>
                val loweredExpr = lowerSIR(expr)
                try lvCast(loweredExpr, tp, anns.pos)
                catch
                    case NonFatal(ex) =>
                        println(
                          s"Error lowering cast: ${sir.pretty.render(100)} at ${anns.pos.file}:${anns.pos.startLine + 1}"
                        )
                        lctx.debug = true
                        lvCast(loweredExpr, tp, anns.pos)
                        throw ex
            case sirBuiltin @ SIR.Builtin(bn, tp, anns) =>
                StaticLoweredValue(
                  sirBuiltin,
                  builtinTerms(bn),
                  SirTypeUplcGenerator(tp).defaultRepresentation(tp)
                )
            case sirError @ SIR.Error(msg, anns, cause) =>
                val term =
                    if lctx.generateErrorTraces then
                        !(DefaultFun.Trace.tpf $ Term.Const(
                          Constant.String(msg)
                        ) $ ~Term.Error)
                    else Term.Error
                StaticLoweredValue(
                  sirError,
                  term,
                  PrimitiveRepresentation.Constant
                )
        lctx.nestingLevel -= 1
        retval
    }

    private def lowerLet(sirLet: SIR.Let)(using lctx: LoweringContext): LoweredValue = {
        val retval = sirLet match
            case SIR.Let(recursivity, bindings, body, anns) =>
                val prevScope = lctx.scope
                if recursivity == NonRec then
                    val bindingValues = bindings.map { b =>
                        val rhs = lowerSIR(b.value).maybeUpcast(b.tp, anns.pos)
                        val varId = lctx.uniqueVarName(b.name)
                        val varVal = VariableLoweredValue(
                          id = varId,
                          name = b.name,
                          sir = SIR.Var(b.name, b.tp, anns),
                          representation = rhs.representation
                        )
                        lctx.scope = lctx.scope.add(varVal)
                        (varVal: IdentifiableLoweredValue, rhs)
                    }.toSeq
                    val bodyValue = lowerSIR(body)
                    lctx.scope = prevScope
                    LetNonRecLoweredValue(bindingValues, bodyValue, sirLet.anns.pos)
                else
                    bindings match
                        case List(Binding(name, tp, rhs)) =>
                            lctx.zCombinatorNeeded = true
                            val newVar = VariableLoweredValue(
                              id = lctx.uniqueVarName(name),
                              name = name,
                              sir = SIR.Var(name, tp, anns),
                              representation =
                                  SirTypeUplcGenerator(rhs.tp).defaultRepresentation(tp)
                            )
                            val prevScope = lctx.scope
                            lctx.scope = lctx.scope.add(newVar)
                            val loweredRhs = lowerSIR(rhs).maybeUpcast(tp, anns.pos)
                            println(
                              s"added let binding, name=${newVar.name}, id=${newVar.id}, tp=${tp.show}, rhs.tp=${loweredRhs.sirType.show}"
                            )
                            val loweredBody = lowerSIR(body)
                            lctx.scope = prevScope
                            LetRecLoweredValue(newVar, loweredRhs, loweredBody, sirLet.anns.pos)
                        case Nil =>
                            sys.error(
                              s"Empty let binding at ${sirLet.anns.pos.file}:${sirLet.anns.pos.startLine}"
                            )
                        case _ =>
                            sys.error(
                              s"Mutually recursive bindings are not supported: $bindings at ${sirLet.anns.pos.file}:${sirLet.anns.pos.startLine}"
                            )
        retval

    }

    private def lowerApp(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        if isFromDataApp(app) then lowerFromData(app)
        else if isToDataApp(app) then lowerToData(app)
        else lowerNormalApp(app)
    }

    private def lowerNormalApp(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        val svTypeUnifyEnv = lctx.typeUnifyEnv
        val (typeVars, fIn, fOut) = SIRType
            .collectPolyOrFun(app.f.tp)
            .getOrElse(
              throw LoweringException(
                s"""|
                    |Function type ${app.f.tp.show} is not a function or type lambda.
                    |app = ${app.pretty.render(100)}
                    |f = ${app.f.pretty.render(100)}
                    """.stripMargin('|'),
                app.anns.pos
              )
            )
        if lctx.debug then
            println(
              s"Lowering app: ${app.pretty.render(100)}\n" +
                  s"  f.tp = ${app.f.tp.show}\n" +
                  s"  f = ${app.f}\n" +
                  s"  typeVars = ${typeVars.map(_.show).mkString(", ")}"
            )
        val newEnv = SIRUnify.unifyType(
          app.arg.tp,
          fIn,
          SIRUnify.Env.empty.withUpcasting
        ) match {
            case SIRUnify.UnificationSuccess(env, _) =>
                // SIRUnify.unifyType(app.tp, fOut, env) match
                //    case SIRUnify.UnificationSuccess(env, _) =>
                //        env
                //    case SIRUnify.UnificationFailure(path, l, r) =>
                //        throw LoweringException(
                //          s"Unification failure for matchng result and out of f ${app.pretty.render(100)}\n" +
                //              s"  path: ${path.mkString(" -> ")}, l=$l, r=$r\n" +
                //              s"  f.tp = ${app.f.tp.show}\n" +
                //              s"  app.tp: ${app.tp.show}",
                //          app.anns.pos
                //        )
                env
            case SIRUnify.UnificationFailure(path, l, r) =>
                throw LoweringException(
                  s"Unification failure for matching arg and in of f ${app.pretty.render(100)}\n" +
                      s"  path: ${path.mkString(" -> ")}\n" +
                      s"  l=$l, r=$r\n" +
                      s"  app.f.tp = ${app.f.tp.show}\n" +
                      s"  app.arg.tp: ${app.arg.tp.show}\n" +
                      s"  f=: ${app.f.pretty.render(100)}\n" +
                      s"  arg=: ${app.arg.pretty.render(100)}\n",
                  app.anns.pos
                )
        }
        lctx.typeUnifyEnv = newEnv
        val fun = lowerSIR(app.f)
        val arg = lowerSIR(app.arg)
        val result =
            try
                lvApply(
                  fun,
                  arg,
                  app.anns.pos,
                  Some(app.tp),
                  None // representation can depend from fun, so should be calculated.
                )
            catch
                case NonFatal(ex) =>
                    println(
                      s"errorl lowering app: ${app.pretty.render(100)} at ${app.anns.pos.file}:${app.anns.pos.startLine + 1}"
                    )
                    lctx.debug = true
                    // redu with debug mode to see the error
                    lvApply(
                      fun,
                      arg,
                      app.anns.pos,
                      Some(app.tp),
                      None // representation can depend from fun, so should be calculated.
                    )
        lctx.typeUnifyEnv = svTypeUnifyEnv
        result
    }

    private def isFromDataType(tp: SIRType): Boolean = tp match {
        case SIRType.Fun(SIRType.Data, _)    => true
        case SIRType.TypeLambda(params, tp1) => isFromDataType(tp1)
        case _                               => false
    }

    private def isToDataType(tp: SIRType): Boolean = tp match {
        case SIRType.Fun(_, SIRType.Data)    => true
        case SIRType.TypeLambda(params, tp1) => isToDataType(tp1)
        case _                               => false
    }

    private def isFromDataName(name: String): Boolean = {
        name == "scalus.builtin.internal.UniversalDataConversion$.fromData"
    }

    private def isToDataName(name: String): Boolean = {
        name == "scalus.builtin.internal.UniversalDataConversion$.toData"
    }

    private def isFromDataApp(app: SIR.Apply): Boolean = {

        app.f match
            case SIR.ExternalVar(moduleName, name, tp, _) =>
                // extrapolation.  TODO: write annotation when compiling FromData tp and extract it here
                isFromDataName(name) && isFromDataType(tp)
            case _ => false
    }

    private def isToDataApp(app: SIR.Apply): Boolean = {

        app.f match
            case SIR.ExternalVar(moduleName, name, tp, _) =>
                // extrapolation.  TODO: write annotation when compiling ToData tp and extract it here
                name.contains("ToData") && isToDataType(tp)
            case _ => false

    }

    private def lowerFromData(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        val data = lctx.lower(app.arg)
        new ProxyLoweredValue(data) {
            override def sirType: SIRType = app.tp
            override def pos: SIRPosition = app.anns.pos
            override def representation: LoweredValueRepresentation =
                lctx.typeGenerator(app.tp).defaultDataRepresentation(app.tp)
            override def termInternal(gctx: TermGenerationContext): Term =
                data.termInternal(gctx)

            override def docDef(style: PrettyPrinter.Style): Doc = {
                val left = Doc.text("FromData(")
                val right = Doc.text(s", ${app.tp.show})")
                data.docRef(style).bracketBy(left, right)
            }

            override def docRef(style: PrettyPrinter.Style): Doc = {
                val left = Doc.text("FromData(")
                val right = Doc.text(")")
                data.docRef(style).bracketBy(left, right)
            }

        }
    }

    private def lowerToData(app: SIR.Apply)(using lctx: LoweringContext): LoweredValue = {
        val value = lctx
            .lower(app.arg)
            .toRepresentation(
              lctx.typeGenerator(app.arg.tp).defaultDataRepresentation(app.arg.tp),
              app.anns.pos
            )
        new ProxyLoweredValue(value) {
            override def sirType: SIRType = app.tp
            override def pos: SIRPosition = app.anns.pos
            override def representation: LoweredValueRepresentation =
                PrimitiveRepresentation.PackedData
            override def termInternal(gctx: TermGenerationContext): Term =
                value.termInternal(gctx)

            override def docDef(style: PrettyPrinter.Style): Doc = {
                val left = Doc.text("FromData(")
                val right = Doc.text(s", ${app.tp.show})")
                value.docRef(style).bracketBy(left, right)
            }

            override def docRef(style: PrettyPrinter.Style): Doc = {
                val left = Doc.text("FromData(")
                val right = Doc.text(")")
                value.docRef(style).bracketBy(left, right)
            }

        }
    }

    def generateDominatedUplevelVarsAccess(
        value: LoweredValue,
    )(using gctx: TermGenerationContext): Term = {

        val newVars = value.dominatingUplevelVars.filterNot(x => gctx.generatedVars.contains(x.id))
        val nGeneratedVars = gctx.generatedVars ++ newVars.map(_.id)
        val internalTerm =
            try value.termInternal(gctx.copy(generatedVars = nGeneratedVars))
            catch
                case NonFatal(ex) =>
                    println(
                      s"Error generating term for value ${value} of type ${value.sirType.show} at ${value.pos.file}:${value.pos.startLine + 1}\n" +
                          s"value:\n${value.show}\n" +
                          s"generatedVars: ${gctx.generatedVars.mkString(", ")}\n" +
                          s"newVars: ${newVars.map(_.id).mkString(", ")}"
                    )
                    value match
                        case lambda: LambdaLoweredValue if lambda.newVar.name == "param" =>
                            println(
                              s"Lambda with param name 'param' at ${value.pos.file}:${value.pos.startLine + 1}"
                            )
                            println(
                              "lambda body created at:" + lambda.body.createdEx.getStackTrace
                                  .mkString("\n")
                            )
                        case _ =>
                    throw ex

        val topSortedNewVars = topologicalSort(newVars)

        val retval = topSortedNewVars.foldLeft(internalTerm) { case (term, v) =>
            val nGctx = gctx.copy(
              generatedVars = gctx.generatedVars + v.id
            )
            v match
                case dv: DependendVariableLoweredValue =>
                    Term.Apply(Term.LamAbs(dv.id, term), dv.rhs.termWithNeededVars(nGctx))
                case v: VariableLoweredValue =>
                    v.optRhs match
                        case Some(rhs) =>
                            Term.Apply(Term.LamAbs(v.id, term), rhs.termWithNeededVars(nGctx))
                        case None =>
                            throw LoweringException(
                              s"Unexpected variable $v is not in scope",
                              value.pos
                            )
        }

        retval
    }

    def addUsedVarsToCounts(
        vars: Set[IdentifiableLoweredValue],
        counts: Map[IdentifiableLoweredValue, Int]
    ): Map[IdentifiableLoweredValue, Int] = {
        vars.foldLeft(counts) { case (acc, v) =>
            acc.updated(v, acc.getOrElse(v, 0) + 1)
        }
    }

    def filterAndCountVars(
        p: IdentifiableLoweredValue => Boolean,
        subvalues: LoweredValue*
    ): Map[IdentifiableLoweredValue, Int] = {
        subvalues.foldLeft(Map.empty[IdentifiableLoweredValue, Int]) { case (acc, leaf) =>
            val usedVars = leaf.usedUplevelVars.filter(p(_))
            addUsedVarsToCounts(usedVars, acc)
        }
    }

    def topologicalSort(values: Set[IdentifiableLoweredValue]): List[IdentifiableLoweredValue] = {
        val visited = scala.collection.mutable.Set.empty[IdentifiableLoweredValue]
        val sorted = scala.collection.mutable.ListBuffer.empty[IdentifiableLoweredValue]

        // A depends from (B,C)
        // B depends from (D)
        // C depends from (D)
        // D depends from ()
        // E deonds from (A,B,C)
        //  A, B, C, D, E
        //  visit
        //

        def visit(value: IdentifiableLoweredValue): Unit = {
            if !visited.contains(value) then
                visited.add(value)
                value.usedUplevelVars.foreach(visit)
                if values.contains(value) then sorted.append(value)
        }

        values.foreach(visit)
        sorted.toList
    }

}
