package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.NameKinds.UniqueNameKind
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.SrcPos
import scalus.sir.*
import scalus.sir.SIR.Pattern
import scalus.sir.SIR.Pattern.Constr

import scala.collection.mutable
import scala.language.implicitConversions

enum SirBinding:
    case Name(name: String, tp: SIRType)
    case CaseClass(
        name: String,
        constructorSymbol: Symbol,
        bindings: List[SirBinding],
        constType: SIRType
    )
    case Error(error: CompilationError)

case class PatternInfo(
    allBindings: Map[String, SIRType],
    generator: SIR => SIR, /// generates inner Match for nested case classes
    bindings: List[String] /// current level bindings to generate SirCase.Case
    // rhsType: SIRType /// SIR type of the rhs expression in the match case
)

enum SirCase:
    case Case(
        constructorSymbol: Symbol,
        typeParams: List[SIRType],
        bindings: List[String],
        rhs: SIR,
        srcPos: SrcPos
    )
    case Wildcard(rhs: SIR, srcPos: SrcPos)
    case Error(error: CompilationError)

/*
    enum A:
      case C0
      case C1(a)
      case C2(b, c)
      case C3(a, b, c)
    compiles to:
    Decl(DataDecl("A", List(ConstrDecl("C0", List()), ConstrDecl("C1", List("a")), ConstrDecl("C2", List("b", "c")))), ...)

    c ==> Constr("C0", constrDecl, List())

    c match
      case C0 => 0
      case C1(a) => 1
      case C2(b, c) => 2
      case _ => 3
    compiles to:
    Match(c, List(
      Case(C0, Nil, 0),
      Case(C1, List(a), 1),
      Case(C2, List(b, c), 2),
      Case(C3, List(_, _, _), 3)
    )
 */
class PatternMatchingCompiler(val compiler: SIRCompiler)(using Context) {
    import tpd.*
    private val patternName = UniqueNameKind("$pat")
    private val bindingName = UniqueNameKind("$bind")

    private def compileBinding(env: SIRCompiler.Env, pat: Tree, tp: Type): SirBinding = {
        if env.debug then println(s"compileBinding: ${pat.show} ${tp.show}")
        pat match
            // this is case Constr(name @ _) or Constr(name)
            case Bind(name, id @ Ident(nme.WILDCARD)) =>
                SirBinding.Name(name.show, sirTypeInEnv(tp, pat.srcPos, env))
            // this is case Constr(name @ Constr2(_))
            case Bind(name, body @ UnApply(fun, _, pats)) =>
                // pattern Symbol probaly incorrect here (need to be tps,  can be Any).  TODO: write test
                val typeSymbol = pat.tpe.widen.dealias.typeSymbol
                SirBinding.CaseClass(
                  name.show,
                  typeSymbol,
                  pats.map(p => compileBinding(env, p, p.tpe)),
                  sirTypeInEnv(tp, pat.srcPos, env)
                )
            case Bind(name, body) =>
                SirBinding.Error(UnsupportedBinding(name.show, pat.srcPos))
            // this is case _ =>
            case Ident(nme.WILDCARD) =>
                SirBinding.Name(bindingName.fresh().show, sirTypeInEnv(tp, pat.srcPos, env))
            // this is case case Outer(Inner(a, b)) which is case Outer(_ @ Constr.unappy(a, b)) =>
            // thus we generate a unique patternName: case Outer($pat @ Constr(a, b)) =>
            case UnApply(fun, _, pats) =>
                // pattern Symbol probaly incorrect here (need to be tps,  can be Any).  TODO: write test
                val typeSymbol = pat.tpe.widen.dealias.typeSymbol
                val name = patternName.fresh()
                SirBinding.CaseClass(
                  name.show,
                  typeSymbol,
                  pats.map(p => compileBinding(env, p, p.tpe)),
                  sirTypeInEnv(tp, pat.srcPos, env)
                )
            case Literal(_) =>
                SirBinding.Error(LiteralPattern(pat.srcPos))
            case p =>
                SirBinding.Error(UnsupportedMatchExpression(p, p.srcPos))
    }

    private def compileBindings(
        sirBindings: List[SirBinding]
    ): Either[List[SirBinding.Error], PatternInfo] = {
        sirBindings.foldRight(
          Right(PatternInfo(Map.empty, identity, Nil)): Either[List[
            SirBinding.Error
          ], PatternInfo]
        ) {
            case (e: SirBinding.Error, Left(errors)) => Left(e :: errors)
            case (_, Left(errors))                   => Left(errors)
            case (e: SirBinding.Error, Right(_))     => Left(e :: Nil)
            case (SirBinding.Name(name, tp), Right(PatternInfo(bindings, generator, names))) =>
                Right(PatternInfo(bindings + (name -> tp), generator, name :: names))
            case (
                  SirBinding.CaseClass(name, constructorSymbol, sirBindings, constrSirType),
                  Right(PatternInfo(enclosingBindings, enclosingGenerator, enclosingNames))
                ) =>
                compileBindings(sirBindings) match
                    case Left(errors) => Left(errors)
                    case Right(PatternInfo(bindings2, generator2, innerNames)) =>
                        Right(
                          PatternInfo(
                            (enclosingBindings ++ bindings2) + (name -> constrSirType),
                            cont =>
                                val (constrDecl, typeParams) = constrSirType match
                                    case SIRType.SumCaseClass(DataDecl(_, constrs, _), typeArgs) => (constrs.head, typeArgs)
                                    case SIRType.CaseClass(decl, typeArgs) => (decl, typeArgs)
                                    case _                                 => sys.error(s"AAA: $constrSirType")
                                val contExpr = enclosingGenerator(generator2(cont))
                                SIR.Match(
                                  SIR.Var(name, constrSirType),
                                  List(
                                    SIR.Case(
                                      Pattern.Constr(constrDecl, innerNames, typeParams),
                                      contExpr
                                    )
                                  ),
                                  contExpr.tp
                                )
                            ,
                            name :: enclosingNames
                          )
                        )
        }
    }

    private def compileConstructorPatterns(
        envIn: SIRCompiler.Env,
        unapplyExpr: UnApply,
        constrType: Type,
        fun: Tree,
        patterns: List[Tree],
        rhs: Tree,
        srcPos: SrcPos
    ): List[SirCase] = {

        // typoes are extracted from unapply result type, because some strange behavior
        //  of dotty,  when types of patterns (i.e. pat.tpe) are not types.
        // we have other workaround for this case, TODO: check.
        val (patternTypes, env) = fun.tpe.dealias.widen match
            case mt: MethodType =>
                val unapplyResType = mt.resultType.dealias
                val optionBase = unapplyResType.baseType(defn.OptionClass)
                if optionBase != NoType then
                    optionBase match
                        case AppliedType(tpe, List(optArgType)) =>
                            optArgType.tupleElementTypes match
                                case Some(tupleArgs) =>
                                    (tupleArgs, envIn)
                                case None =>
                                    (List(optArgType), envIn)
                        case _ =>
                            throw TypingException(
                              unapplyResType,
                              srcPos,
                              s"unapply result type is not applied type when type constructor of ${unapplyResType.classSymbol} have type params"
                            )
                else if unapplyResType.baseType(defn.ProductClass) != NoType then
                    val unapplyPrimaryConstructor = unapplyResType.classSymbol.primaryConstructor
                    val constrTypeParamss =
                        unapplyPrimaryConstructor.paramSymss.filter(_.exists(_.isType))
                    val nEnv =
                        if !constrTypeParamss.isEmpty then
                            unapplyResType match
                                case AppliedType(tpe, args) =>
                                    val newParams = (constrTypeParamss.head zip args).map {
                                        (sym, t) =>
                                            sym -> sirTypeInEnv(t, unapplyExpr.srcPos, envIn)
                                    }.toMap
                                    envIn.copy(typeVars = envIn.typeVars ++ newParams)
                                case _ =>
                                    throw TypingException(
                                      unapplyResType,
                                      srcPos,
                                      s"unapply result type is not applied type when type constructor of ${unapplyResType.classSymbol} have type params"
                                    )
                        else envIn
                    val constrParamss =
                        unapplyPrimaryConstructor.paramSymss.filter(_.exists(_.isTerm))
                    if constrParamss.isEmpty then (List.empty[Type], nEnv)
                    else (constrParamss.head.map(_.info), nEnv)
                else if unapplyResType =:= defn.BooleanType then (List.empty, envIn)
                else
                    // constructor patterns have no special forms
                    // TODO: get resuslt
                    throw TypingException(
                      unapplyResType,
                      srcPos,
                      s"unapply result type is not option or product type"
                    )
            case _ =>
                throw TypingException(
                  fun.tpe.dealias.widen,
                  srcPos,
                  "type of unapply fun is not a method type"
                )

        if patternTypes.length != patterns.length then
            throw TypingException(
              fun.tpe.widen,
              srcPos,
              s"we determinate ${patternTypes.length} types (${patterns}, but have ${patterns.length} patterns ${patterns
                      .map(_.symbol.name)}"
            )

        val sirBindings = patterns.zip(patternTypes).map { case (b, bt) =>
            compileBinding(env, b, bt)
        }

        compileBindings(sirBindings) match
            case Left(errors) => errors.map(e => SirCase.Error(e.error))
            case Right(PatternInfo(bindings, generateSir, names)) =>
                val nEnv = env ++ bindings
                val constrTypeSymbol = constrType.typeSymbol
                val constrTypeArgs = constrType match
                    case AppliedType(tpe, args) => args
                    case _                      => Nil
                val sirConstrTypeArs = constrTypeArgs.map(t => sirTypeInEnv(t, srcPos, nEnv))
                val rhsE = compiler.compileExpr(nEnv, rhs)
                SirCase.Case(
                  constrTypeSymbol,
                  sirConstrTypeArs,
                  names,
                  generateSir(rhsE),
                  rhs.srcPos
                ) :: Nil

    }

    private def scalaCaseDefToSirCase(
        env: SIRCompiler.Env,
        c: CaseDef
    ): List[SirCase] = c match
        case CaseDef(_, guard, _) if !guard.isEmpty =>
            SirCase.Error(GuardsNotSupported(guard.srcPos)) :: Nil
        // this case is for matching on a case class
        case CaseDef(unapply @ UnApply(fun, _, pats), _, rhs) =>
            if env.debug then
                println(s"Case: ${fun}, pats: ${pats}, rhs: $rhs")
                report.debuglog(s"dl: Case: ${fun}, pats: ${pats}, rhs: $rhs")
            // report.error(s"Case: ${fun}, pats: ${pats}, rhs: $rhs", t.pos)
            if unapply.tpe == defn.NothingType then
                // need to restore constructor, maybe from fun
                fun.tpe.widen match
                    case mt: MethodType =>
                        val constrType = mt.paramInfos.head.dealias
                        compileConstructorPatterns(
                          env,
                          unapply,
                          constrType,
                          fun,
                          pats,
                          rhs,
                          c.srcPos
                        )
                    case _ =>
                        // TODO: check PolyType
                        throw TypingException(
                          fun.tpe.widen,
                          c.srcPos,
                          "type of unapply fun is not a method type"
                        )
            else
                compileConstructorPatterns(
                  env,
                  unapply,
                  unapply.tpe.dealias.widen,
                  fun,
                  pats,
                  rhs,
                  c.srcPos
                )
        // this case is for matching on an enum
        case CaseDef(Typed(unapply @ UnApply(fun, _, pats), constrTpe), _, rhs) =>
            // report.info(s"Case: ${inner}, tpe ${constrTpe.tpe.widen.show}", t.pos)
            compileConstructorPatterns(
              env,
              unapply,
              constrTpe.tpe.dealias.widen,
              fun,
              pats,
              rhs,
              c.srcPos
            )
        // case _ => rhs, wildcard pattern, must be the last case
        case CaseDef(Ident(nme.WILDCARD), _, rhs) =>
            val rhsE = compiler.compileExpr(env, rhs)
            SirCase.Wildcard(rhsE, c.srcPos) :: Nil
        case CaseDef(b @ Bind(pat, _), _, _) =>
            SirCase.Error(UnsupportedTopLevelBind(pat.show, b.srcPos)) :: Nil
        // case object
        case CaseDef(pat, _, rhs) if pat.symbol.is(Flags.Case) =>
            val rhsE = compiler.compileExpr(env, rhs)
            // no-arg constructor, it's a Val, so we use termSymbol
            SirCase.Case(pat.tpe.termSymbol, Nil, Nil, rhsE, c.srcPos) :: Nil
        case a =>
            SirCase.Error(UnsupportedMatchExpression(a, a.srcPos)) :: Nil

    def compileMatch(tree: Match, env: SIRCompiler.Env): SIR = {
        if env.debug then println(s"compileMatch: ${tree.show}")
        val Match(matchTree, cases) = tree
        // val typeSymbol = matchTree.tpe.widen.dealias.typeSymbol
        // report.echo(s"Match: ${typeSymbol} ${typeSymbol.children} $adtInfo", tree.srcPos)
        val matchExpr = compiler.compileExpr(env, matchTree)
        val sirCases = cases.flatMap(cs => scalaCaseDefToSirCase(env, cs))
        if env.debug then println(s"compileMatch cases: ${sirCases}")

        var idx = 0
        val iter = sirCases.iterator
        val expandedCases = mutable.ArrayBuffer.empty[SIR.Case]

        while iter.hasNext do
            iter.next() match
                case SirCase.Case(constructorSymbol, typeParams, bindings, rhs, srcPos) =>
                    val constrDecl = compiler.makeConstrDecl(env, srcPos, constructorSymbol)
                    expandedCases += SIR.Case(Constr(constrDecl, bindings, typeParams), rhs)
                case SirCase.Wildcard(rhs, srcPos) =>
                    // If we have a wildcard case, it must be the last one
                    if idx != sirCases.length - 1 then
                        compiler.error(
                          GenericError(
                            s"Wildcard case must be the last and only one in match expression",
                            srcPos
                          ),
                          ()
                        )
                    else expandedCases += SIR.Case(Pattern.Wildcard, rhs)
                case SirCase.Error(err) => compiler.error(err, ())

            idx += 1
        end while
        SIR.Match(
          matchExpr,
          expandedCases.toList,
          sirTypeInEnv(tree.tpe.dealias.widen, tree.srcPos, env)
        )
    }

    private def sirTypeInEnv(tpe: Type, srcPos: SrcPos, env: SIRCompiler.Env): SIRType = {
        compiler.sirTypeInEnv(tpe, srcPos, env)
    }

}
