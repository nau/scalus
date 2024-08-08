package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.UniqueNameKind
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.{AppliedType, Type}
import dotty.tools.dotc.util.SrcPos
import scalus.sir.*

import scala.collection.mutable
import scala.language.implicitConversions



enum SirBinding:
    case Name(name: String, tp: SIRType )
    case CaseClass(name: String, constructorSymbol: Symbol, bindings: List[SirBinding], constType: SIRType)
    case Error(error: CompilationError)

case class PatternInfo(
    allBindings: Map[String, SIRType],
    generator: SIRExpr => SIRExpr, /// generates inner Match for nested case classes
    bindings: List[String], /// current level bindings to generate SirCase.Case
    //rhsType: SIRType /// SIR type of the rhs expression in the match case
)

enum SirCase:
    case Case(constructorSymbol: Symbol, typeParams: List[SIRType], bindings: List[String], rhs: SIRExpr, srcPos: SrcPos)
    case Wildcard(rhs: SIRExpr, srcPos: SrcPos)
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

    private def constructCase(
        constrDecl: ConstrDecl,
        bindings: List[String],
        typeArgs: List[SIRType],
        rhs: SIRExpr
    ): scalus.sir.SIR.Case = {


        //val params = compiler.primaryConstructorParams(constrSymbol).zip(bindings).map {
        //    case (paramSym, name) =>
        //        TypeBinding(paramSym.name.show, SIRTypesHelper.sirType(paramSym.info.widen))
        //}
        //val typeVars = ???
        //val constrDecl = scalus.sir.ConstrDecl(constrSymbol.name.show, SIRVarStorage.DEFAULT, params)

        scalus.sir.SIR.Case(constrDecl, bindings, typeArgs, rhs)
    }

    private def compileBinding(pat: Tree): SirBinding = {
        pat match
            // this is case Constr(name @ _) or Constr(name)
            case Bind(name, id@Ident(nme.WILDCARD)) => SirBinding.Name(name.show, sirType(id.tpe, pat.srcPos))
            // this is case Constr(name @ Constr2(_))
            case Bind(name, body @ UnApply(fun, _, pats)) =>
                val typeSymbol = pat.tpe.widen.dealias.typeSymbol
                SirBinding.CaseClass(name.show, typeSymbol, pats.map(compileBinding), sirType(pat.tpe, pat.srcPos))
            case Bind(name, body) =>
                SirBinding.Error(UnsupportedBinding(name.show, pat.srcPos))
            // this is case _ =>
            case Ident(nme.WILDCARD) => SirBinding.Name(bindingName.fresh().show, sirType(pat.tpe, pat.srcPos))
            case UnApply(fun, _, pats) =>
                val typeSymbol = pat.tpe.widen.dealias.typeSymbol
                val name = patternName.fresh()
                SirBinding.CaseClass(name.show, typeSymbol, pats.map(compileBinding), sirType(pat.tpe, pat.srcPos))
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
                    case Right(PatternInfo(bindings2, generator, innerNames)) =>
                        Right(
                          PatternInfo(
                            (enclosingBindings ++ bindings2) + (name ->  constrSirType),
                            cont =>
                                val (constrDecl, typeParams) = constrSirType match
                                    case SIRType.CaseClass(decl, typeArgs) => (decl, typeArgs)
                                    case _ => ???
                                val contExpr = generator(cont)
                                SIR.Match(
                                  SIR.Var(name, constrSirType),
                                  List(
                                    constructCase(
                                      constrDecl,
                                      innerNames,
                                      typeParams,
                                      contExpr
                                    )
                                  ),
                                  contExpr.tp
                                ),
                            name :: enclosingNames
                          )
                        )
        }
    }


    private def compileConstructorPatterns(
        env: SIRCompiler.Env,
        constrType: Type,
        patterns: List[Tree],
        rhs: Tree,
        srcPos: SrcPos
    ): List[SirCase] = {
        val sirBindings = patterns.map(compileBinding)
        compileBindings(sirBindings) match
            case Left(errors) => errors.map(e => SirCase.Error(e.error))
            case Right(PatternInfo(bindings, generateSir, names)) =>
                val constrTypeSymbol = constrType.typeSymbol
                val constrTypeArgs = constrType match
                    case AppliedType(tpe, args) => args
                    case _ => Nil
                val rhsE = compiler.compileExpr(env ++ bindings, rhs)
                SirCase.Case(constrTypeSymbol, constrTypeArgs.map(t => sirType(t, srcPos)), names, generateSir(rhsE), rhs.srcPos) :: Nil
    }

    private def scalaCaseDefToSirCase(
        env: SIRCompiler.Env,
        c: CaseDef
    ): List[SirCase] = c match
        case CaseDef(_, guard, _) if !guard.isEmpty =>
            SirCase.Error(GuardsNotSupported(guard.srcPos)) :: Nil
        // this case is for matching on a case class
        case CaseDef(unapply@UnApply(_, _, pats), _, rhs) =>
            // report.error(s"Case: ${fun}, pats: ${pats}, rhs: $rhs", t.pos)
            compileConstructorPatterns(env, unapply.tpe, pats, rhs, c.srcPos)
        // this case is for matching on an enum
        case CaseDef(Typed(UnApply(_, _, pats), constrTpe), _, rhs) =>
            // report.info(s"Case: ${inner}, tpe ${constrTpe.tpe.widen.show}", t.pos)
            compileConstructorPatterns(env, constrTpe.tpe, pats, rhs, c.srcPos)
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

    def compileMatch(tree: Match, env: SIRCompiler.Env): SIRExpr = {
        val Match(matchTree, cases) = tree
        //val typeSymbol = matchTree.tpe.widen.dealias.typeSymbol
        val adtInfo = compiler.getAdtTypeInfo(matchTree.tpe)
        // report.echo(s"Match: ${typeSymbol} ${typeSymbol.children} $adtInfo", tree.srcPos)
        val matchExpr = compiler.compileExpr(env, matchTree)
        val sirCases = cases.flatMap(cs => scalaCaseDefToSirCase(env, cs))

        // 1. If we have a wildcard case, it must be the last one
        // 2. Validate we don't have any errors
        // 3. Convert Wildcard to the rest of the cases/constructors
        // 4. Ensure we cover all constructors
        // 5. Sort the cases by constructor name

        var idx = 0
        val iter = sirCases.iterator
        val allConstructors = adtInfo.childrenSymbols.toSet
        val matchedConstructors = mutable.HashSet.empty[Symbol]
        val expandedCases = mutable.ArrayBuffer.empty[scalus.sir.SIR.Case]

        while iter.hasNext do
            iter.next() match
                case SirCase.Case(constructorSymbol, typeParams, bindings, rhs, srcPos) =>
                    matchedConstructors += constructorSymbol // collect all matched constructors
                    val constrDecl = retrieveConstrDecl(constructorSymbol, srcPos)
                    expandedCases += constructCase(constrDecl, bindings, typeParams, rhs)
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
                    else
                        // Convert Wildcard to the rest of the cases/constructors
                        val missingConstructors = allConstructors -- matchedConstructors
                        missingConstructors.foreach { constr =>
                            val bindings = compiler
                                .primaryConstructorParams(constr)
                                .map(_ => bindingName.fresh().show)
                            // TODO: extract rhs to a let binding before the match
                            // so we don't have to repeat it for each case
                            val constrDecl = retrieveConstrDecl(constr, srcPos)
                            if (constr.typeParams.nonEmpty) then
                                compiler.error(
                                  GenericError(
                                    s"Wildcard case can't be used with type parameters",
                                    srcPos
                                  ),
                                  ()
                                )
                            else
                                expandedCases += constructCase(constrDecl, bindings, Nil, rhs)
                            expandedCases += constructCase(constrDecl, bindings, Nil, rhs)
                            matchedConstructors += constr // collect all matched constructors
                        }
                case SirCase.Error(err) => compiler.error(err, ())

            idx += 1
        end while
        // Ensure we cover all constructors
        val missingConstructors = allConstructors -- matchedConstructors
        if missingConstructors.nonEmpty then
            compiler.error(
              MissingConstructors(
                adtInfo,
                missingConstructors,
                tree.srcPos
              ),
              ()
            )

        // Sort the cases by constructor name to ensure we have a deterministic order
        val sortedCases = expandedCases.sortBy(_.constr.name).toList
        SIR.Match(matchExpr, sortedCases, sirType(tree.tpe, tree.srcPos))
    }

    private def retrieveConstrDecl(constructorSymbol: Symbol, srcPos: SrcPos): ConstrDecl = {
        val params = constructorSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isTypeParam).map{ paramSym =>
            TypeBinding(paramSym.name.show, sirType(paramSym.info.widen, srcPos))
        }
        val typeParams = constructorSymbol.typeParams.map(tp => SIRType.TypeVar(tp.name.show, Some(tp.hashCode)))
        //  TODO:  define mapping of names into constructor.
        // trying to find decl.
        val optParent = constructorSymbol.info.baseClasses.find{ base =>
            base.children.nonEmpty && base.children.exists{ child =>
                child.isConstructor && child  == constructorSymbol
            }
        }
        val revTypes = constructorSymbol.typeParams.foldLeft(Map.empty[Symbol, SIRType.TypeVar])((acc, tp) =>
            acc + (tp -> SIRType.TypeVar(tp.name.show, Some(tp.hashCode)))
        )
        val parentTypeArgs = optParent.map{ parent =>
            val tpArgs = constructorSymbol.info.baseType(parent) match {
                case AppliedType(tpe, args) => args
                case _ => Nil
            }
            tpArgs.map(t => SIRTypesHelper.sirTypeInEnv(t, SIRTypesHelper.SIRTypeEnv(srcPos,revTypes)))
        }.getOrElse(Nil)

        val constrDecl = scalus.sir.ConstrDecl(constructorSymbol.show, SIRVarStorage.DEFAULT, params, typeParams, parentTypeArgs)
        constrDecl
    }

    private def sirType(tpe: Type, srcPos: SrcPos): SIRType = {
       compiler.sirType(tpe, srcPos)
    }

}
