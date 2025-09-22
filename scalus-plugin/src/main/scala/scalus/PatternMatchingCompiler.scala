package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.{comparing, Context}
import dotty.tools.dotc.core.NameKinds.UniqueNameKind
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.{SourcePosition, SrcPos}
import scalus.SirCaseDecisionTree.{ConstructorBinding, Leaf}
import scalus.SirParsedCase.{Action, Pattern}
import scalus.sir.{SIRType, *}

//import scala.language.implicitConversions

enum SirBinding:
    case Name(name: String, tp: SIRType, pos: SourcePosition)
    case CaseClass(
        name: String,
        constructorSymbol: Symbol,
        bindings: List[SirBinding],
        constType: SIRType,
        pos: SourcePosition
    )
    case Error(error: CompilationError)

case class PatternInfo(
    allBindings: Map[String, SIRType],
    generator: SIR => SIR, /// generates inner Match for nested case classes
    bindings: List[String], /// current level bindings to generate SirCase.Case
    // rhsType: SIRType /// SIR type of the rhs expression in the match case
    pos: SourcePosition
)

class PatternMatchingContext(
    val globalPrefix: String,
    val env: SIRCompiler.Env,
    // should be set after parsing.
    var originActions: IndexedSeq[Tree] = IndexedSeq.empty
) {

    private var tmpNameCounter: Int = 1

    private var actionsUsage: Map[Int, Int] = Map.empty

    def freshName(optPrefix: Option[String] = None): String = {
        val localPrefix = optPrefix.getOrElse("")
        val name = s"${globalPrefix}${localPrefix}_$tmpNameCounter"
        tmpNameCounter += 1
        name
    }

}

/*
enum SirCase:

    /*
    case Case(
        constructorSymbol: Symbol,
        typeParams: List[SIRType],
        bindings: List[String],
        rhs: SIR,
        pos: SourcePosition
    )
    
 */

    case Constructor(
        optTopLevelType: Option[SIRType],
        optTopLevelName: Option[String],
        constructorSymbol: Symbol,
        typeParams: List[SIRType],
        bindings: IndexedSeq[String],
        optGuard: Option[SIR] = None,
        rhs: SIR,
        pos: SourcePosition
    ) extends SirCase

    // case Wildcard(rhs: SIR, pos: SourcePosition)

    case Constant(const: SIR.Const, rhs: SIR, optGuard: Option[SIR], pos: SourcePosition)

    case Wildcard(
        optTopLevelType: Option[SIRType],
        optTopLevelName: Option[String],
        rhs: SIR,
        optGuard: Option[SIR],
        pos: SourcePosition
    )

    case Error(error: CompilationError)

end SirCase
 */

/** representation, suitable to optimization, simular to
  *
  * http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf
  */
case class SirParsedCase(
    pattern: SirParsedCase.Pattern,
    action: SirParsedCase.Action,
    pos: SrcPos
)

object SirParsedCase:

    // sealed trait SirParsedCase {

    //    def pos: SrcPos

    //    def withAlias(nameInfo: SirParsedCase.BindingNameInfo): SirParsedCase

    // }

    sealed trait Pattern:

        def pos: SrcPos

        def withAlias(nameInfo: BindingNameInfo): Pattern

        def withOptAlias(optNameInfo: Option[BindingNameInfo]): Pattern =
            optNameInfo match
                case None           => this
                case Some(nameInfo) => this.withAlias(nameInfo)

        def optNameInfo: Option[BindingNameInfo]

        def optGuard: Option[AnnotatedSIR]

        def collectNames: Map[String, SIRType]

    end Pattern

    object Pattern:

        case class TypeSelector(
            tp: SIRType,
            optNameInfo: Option[BindingNameInfo],
            innerPattern: Pattern,
            optGuard: Option[AnnotatedSIR],
            pos: SrcPos
        ) extends Pattern {

            def withAlias(alias: BindingNameInfo): Pattern = {
                optNameInfo match {
                    case None           => this.copy(optNameInfo = Some(alias))
                    case Some(nameInfo) => this.copy(optNameInfo = Some(nameInfo.withAlias(alias)))
                }
            }

            def collectNames = optNameInfo match {
                case None =>
                    innerPattern.collectNames
                case Some(nameInfo) =>
                    innerPattern.collectNames + (nameInfo.name -> tp)
            }

        }

        case class Constructor(
            tp: SIRType.CaseClass,
            freeTypeParams: List[SIRType.TypeVar],
            optNameInfo: Option[BindingNameInfo],
            // constructorSymbol: Symbol,
            subcases: IndexedSeq[Pattern],
            optGuard: Option[AnnotatedSIR],
            pos: SrcPos
        ) extends Pattern {

            def withAlias(alias: BindingNameInfo): Pattern = {
                optNameInfo match {
                    case None           => this.copy(optNameInfo = Some(alias))
                    case Some(nameInfo) => this.copy(optNameInfo = Some(nameInfo.withAlias(alias)))
                }
            }

            override def collectNames: Map[String, SIRType] = {
                val subcasesNames = subcases.foldLeft(Map.empty[String, SIRType]) { (m, p) =>
                    m ++ p.collectNames
                }
                optNameInfo match {
                    case None           => subcasesNames
                    case Some(nameInfo) => subcasesNames + (nameInfo.name -> tp)
                }
            }

        }

        case class Wildcard(
            tp: SIRType,
            optNameInfo: Option[BindingNameInfo],
            optGuard: Option[AnnotatedSIR],
            pos: SrcPos
        ) extends Pattern {

            def withAlias(alias: BindingNameInfo): Pattern = {
                this.optNameInfo match {
                    case None           => this.copy(optNameInfo = Some(alias))
                    case Some(nameInfo) => this.copy(optNameInfo = Some(nameInfo.withAlias(alias)))
                }
            }

            override def collectNames: Map[String, SIRType] = {
                optNameInfo match {
                    case None           => Map.empty
                    case Some(nameInfo) => Map(nameInfo.name -> tp)
                }

            }

        }

        case class PrimitiveConstant(
            value: SIR.Const,
            optNameInfo: Option[BindingNameInfo] = None,
            optGuard: Option[AnnotatedSIR] = None,
            pos: SourcePosition
        ) extends Pattern {

            def withAlias(alias: BindingNameInfo): Pattern =
                optNameInfo match
                    case Some(nameInfo) => this.copy(optNameInfo = Some(nameInfo.withAlias(alias)))
                    case None           => this.copy(optNameInfo = Some(alias))

            override def collectNames: Map[String, SIRType] =
                optNameInfo match
                    case None           => Map.empty
                    case Some(nameInfo) => Map(nameInfo.name -> value.tp)

        }

        case class OrPattern(patterns: List[Pattern], pos: SourcePosition) extends Pattern {
            def withAlias(alias: BindingNameInfo): Pattern =
                this.copy(patterns = this.patterns.map(_.withAlias(alias)))

            override def optGuard: Option[AnnotatedSIR] = None

            override def optNameInfo: Option[BindingNameInfo] = None

            override def collectNames: Map[String, SIRType] = Map.empty

        }

        /*
        case class TuplePattern(
            elementTypes: IndexedSeq[SIRType],
            elements: IndexedSeq[Pattern],
            optNameInfo: Option[BindingNameInfo],
            optGuard: Option[AnnotatedSIR],
            pos: SrcPos,
            activeColumns: Set[Int],
            rebinds: Map[String, String]
        )*/

        case class Error(error: CompilationError) extends Pattern {
            def pos = error.srcPos

            def withAlias(alias: BindingNameInfo): Pattern = this

            override def optNameInfo: Option[BindingNameInfo] = None

            override def optGuard: Option[AnnotatedSIR] = None

            override def collectNames: Map[String, SIRType] = Map.empty
        }

    end Pattern

    enum Action:
        case Origin(i: Int)
        case MergedConstructorCond(
            frsPatterm: IndexedSeq[Pattern],
            frsAction: Action,
            sndPatterm: Pattern,
            sndAction: Action,
        )
        case FailMatch
        case ContinueMatch
    end Action

    case class BindingNameInfo(
        name: String,
        scalaName: Option[String],
        tp: SIRType,
        symbol: Symbol,
        aliases: Set[BindingNameInfo] = Set.empty
    ) {
        def withAlias(alias: BindingNameInfo): BindingNameInfo =
            this.copy(aliases = this.aliases + alias)
    }

    /*
    case class JoinedConstructorCase(
        patterns: IndexedSeq[Pattern],
        optNameInfo: Option[BindingNameInfo],
        opt
        action: Action,
        pos: SrcPos,
    )

    case class JoinedConstructorCases(
        tp: SIRType.CaseClass,
        freeTypeParams: List[SIRType.TypeVar],
        argTypes: IndexedSeq[SIRType],
        cases: List[JoinedConstructorCase],
    )
     */

    case class GroupedTupleRow(
        patterns: IndexedSeq[Pattern],
        action: Action,
        optGuard: Option[AnnotatedSIR],
        pos: SrcPos
    )

    case class GroupedTuples(
        columnBinding: IndexedSeq[BindingNameInfo],
        activeColumns: Set[Int],
        rows: IndexedSeq[GroupedTupleRow]
    )

    case class GroupedTupleRowWithGuard(
        row: GroupedTupleRow,
        optGuard: Option[AnnotatedSIR],
    )

    case class JoinedConstructorCase(
        tp: SIRType.CaseClass,
        tuples: IndexedSeq[GroupedTupleRow]
    )

end SirParsedCase

case class SirParsedMatch(
    scrutinee: AnnotatedSIR,
    cases: List[SirParsedCase],
    originActions: IndexedSeq[Tree],
    scrutineeTp: SIRType,
    resTp: SIRType,
    pos: SourcePosition
)

sealed trait SirCaseDecisionTree

object SirCaseDecisionTree:

    case class ConstructorsChoice(
        columnName: Option[String],
        byConstructors: Map[String, ConstructorEntry]
    ) extends SirCaseDecisionTree

    case class ConstructorBinding(
        name: String,
        tp: SIRType,
        index: Int,
        pos: SrcPos
    )

    case class ConstructorEntry(
        caseClass: SIRType.CaseClass,
        names: IndexedSeq[ConstructorBinding],
        next: SirCaseDecisionTree,
        pos: SrcPos
    )

    case class CheckGuard(
        guard: AnnotatedSIR,
        pos: SrcPos,
        nextTrue: SirCaseDecisionTree,
        nextFalse: SirCaseDecisionTree
    ) extends SirCaseDecisionTree

    /** If first if fails, then try next, otherwise result is first resutl
      * @param first
      * @param next
      */
    case class SeqCheck(
        first: SirCaseDecisionTree,
        next: SirCaseDecisionTree
    ) extends SirCaseDecisionTree

    case class RebindUpConstructor(
        names: IndexedSeq[ConstructorBinding],
        next: SirCaseDecisionTree
    ) extends SirCaseDecisionTree

    case class RebindTopLevelBind(
        name: String,
        next: SirCaseDecisionTree
    )

    case class DropRebinds(names: String, next: SirCaseDecisionTree)

    case class InnerMatch(
        scrutinee: SIR.Var,
        matchDecisionTree: SirCaseDecisionTree,
        actions: IndexedSeq[SirCaseDecisionTree]
    )

    case class Leaf(
        action: SirParsedCase.Action,
        pos: SrcPos
    ) extends SirCaseDecisionTree

end SirCaseDecisionTree

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
                SirBinding.Name(name.show, sirTypeInEnv(tp, pat.srcPos, env), pat.srcPos.sourcePos)
            // this is case Constr(name @ Constr2(_))
            case Bind(name, body @ UnApply(fun, _, pats)) =>
                // pattern Symbol probaly incorrect here (need to be tps,  can be Any).  TODO: write test
                val typeSymbol = pat.tpe.widen.dealias.typeSymbol
                EmptyTree.srcPos
                SirBinding.CaseClass(
                  name.show,
                  typeSymbol,
                  pats.map(p => compileBinding(env, p, p.tpe)),
                  sirTypeInEnv(tp, pat.srcPos, env),
                  pat.srcPos.sourcePos
                )
            case Bind(name, body) =>
                SirBinding.Error(UnsupportedBinding(name.show, pat.srcPos))
            // this is case _ =>
            case Ident(nme.WILDCARD) =>
                SirBinding.Name(
                  bindingName.fresh().show,
                  sirTypeInEnv(tp, pat.srcPos, env),
                  pat.srcPos.sourcePos
                )
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
                  sirTypeInEnv(tp, pat.srcPos, env),
                  pat.srcPos.sourcePos
                )
            case Literal(_) =>
                SirBinding.Error(LiteralPattern(pat.srcPos))
            case p =>
                println(s"Unsupported binding expression: tree= ${p}")
                println(s"Unsupported binding expression: tree.show= ${p.show}")
                SirBinding.Error(UnsupportedMatchExpression(p, p.srcPos))
    }

    /*
    private def compileBindings(
        sirBindings: List[SirBinding]
    ): Either[List[SirBinding.Error], PatternInfo] = {
        sirBindings.foldRight(
          Right(PatternInfo(Map.empty, identity, Nil, NoSourcePosition)): Either[List[
            SirBinding.Error
          ], PatternInfo]
        ) {
            case (e: SirBinding.Error, Left(errors)) => Left(e :: errors)
            case (_, Left(errors))                   => Left(errors)
            case (e: SirBinding.Error, Right(_))     => Left(e :: Nil)
            case (
                  SirBinding.Name(name, tp, posLeft),
                  Right(PatternInfo(bindings, generator, names, posRightAcc))
                ) =>
                val newPos = posLeft union posRightAcc
                Right(PatternInfo(bindings + (name -> tp), generator, name :: names, newPos))
            case (
                  SirBinding.CaseClass(
                    name,
                    constructorSymbol,
                    sirBindings,
                    constrSirType,
                    posLeft
                  ),
                  Right(
                    PatternInfo(enclosingBindings, enclosingGenerator, enclosingNames, posRightAcc)
                  )
                ) =>
                compileBindings(sirBindings) match
                    case Left(errors) => Left(errors)
                    case Right(
                          PatternInfo(bindings2, generator2, innerNames, posInnerBindings)
                        ) =>
                        val unionPos = posLeft union posRightAcc
                        Right(
                          PatternInfo(
                            (enclosingBindings ++ bindings2) + (name -> constrSirType),
                            cont =>
                                val (constrDecl, typeParams) = constrSirType match
                                    case SIRType
                                            .SumCaseClass(
                                              DataDecl(_, constrs, _, _),
                                              typeArgs
                                            ) =>
                                        (constrs.head, typeArgs)
                                    case SIRType.CaseClass(decl, typeArgs, optParent) =>
                                        (decl, typeArgs)
                                    case _ => sys.error(s"AAA: $constrSirType")
                                val contExpr = enclosingGenerator(generator2(cont))
                                SIR.Match(
                                  SIR.Var(
                                    name,
                                    constrSirType,
                                    AnnotationsDecl.fromSourcePosition(posLeft)
                                  ),
                                  List(
                                    SIR.Case(
                                      Pattern.Constr(
                                        constrDecl,
                                        innerNames,
                                        typeParams
                                      ),
                                      contExpr,
                                      AnnotationsDecl.fromSourcePosition(posLeft)
                                    )
                                  ),
                                  contExpr.tp,
                                  AnnotationsDecl.fromSourcePosition(unionPos)
                                )
                            ,
                            name :: enclosingNames,
                            unionPos
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
                    // TODO: get result
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
            case Right(PatternInfo(bindings, generateSir, names, pos)) =>
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
                  rhs.srcPos.sourcePos
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
            SirCase.Wildcard(rhsE, c.srcPos.sourcePos) :: Nil
        case CaseDef(b @ Bind(pat, _), _, _) =>
            SirCase.Error(UnsupportedTopLevelBind(pat.show, b.srcPos)) :: Nil
        // case object
        case CaseDef(pat, _, rhs) if pat.symbol.is(Flags.Case) =>
            val rhsE = compiler.compileExpr(env, rhs)
            // no-arg constructor, it's a Val, so we use termSymbol
            SirCase.Case(pat.tpe.termSymbol, Nil, Nil, rhsE, c.srcPos.sourcePos) :: Nil
        case a =>
            println(s"Unsupported case: ${a.show}")
            println(s"Unsupported case: tree ${a}")
            val err = UnsupportedMatchExpression(a, a.srcPos)
            compiler.error(err, SirCase.Error(err) :: Nil)
            
     */

    def parseConstructorPattern(
        ctx: PatternMatchingContext,
        unapply: UnApply,
        optBindingNameInfo: Option[SirParsedCase.BindingNameInfo],
        optScalaGuard: Option[Tree],
        pos: SourcePosition,
        scrutineeType: SIRType
    ): SirParsedCase.Pattern = {
        if ctx.env.debug then
            println(s"parseConstructorPattern: ${unapply.show} ${unapply.tpe.show}")
        val cType = unapply.tpe.dealias.widen
        val sirConstrType = compiler.sirTypeInEnv(cType, unapply.srcPos, ctx.env)
        val (typeParams, constrDecl, typeArgs) = SIRType
            .collectProd(sirConstrType)
            .getOrElse(
              throw TypingException(
                cType,
                unapply.srcPos,
                s"constructor pattern type is not case class or enum: $sirConstrType"
              )
            )
        val optParent = SIRType.prodParent(sirConstrType)
        val constrSymbol = cType.typeSymbol.primaryConstructor
        val typeParamsMap = constrDecl.typeParams.zip(typeArgs).toMap
        val paramTypes =
            constrDecl.params.map(b => SIRType.substitute(b.tp, typeParamsMap, Map.empty))
        val subpatterns = unapply.patterns
            .zip(paramTypes)
            .map { case (p, tp) =>
                parsePattern(ctx, p, None, p.sourcePos, tp)
            }
            .toIndexedSeq
        // TODO: check type compatibility
        val optSirGuard = optScalaGuard.map { tree =>
            val nNames = subpatterns.foldLeft(Map.empty[String, SIRType]) { (m, p) =>
                m ++ p.collectNames
            } ++ optBindingNameInfo
                .map(b => Map(b.name -> scrutineeType))
                .getOrElse(Map.empty)
            compiler.compileExpr(ctx.env.copy(vars = ctx.env.vars ++ nNames), tree)
        }
        SirParsedCase.Pattern.Constructor(
          SIRType.CaseClass(constrDecl, typeArgs, optParent),
          typeParams,
          optBindingNameInfo,
          // constrSymbol,
          subpatterns,
          optSirGuard,
          pos
        )
    }

    def parsePatternInOptBind(
        ctx: PatternMatchingContext,
        pat: Tree,
        optBindingNameInfo: Option[SirParsedCase.BindingNameInfo],
        optGuard: Option[Tree],
        pos: SourcePosition,
        sirScrutineeType: SIRType
    ): SirParsedCase.Pattern = {

        def addBindignName(m: Map[String, SIRType]): Map[String, SIRType] = {
            optBindingNameInfo match {
                case Some(b) => m + (b.name -> sirScrutineeType)
                case None    => m
            }
        }

        pat match
            case u @ UnApply(fun, _, _) =>
                parseConstructorPattern(
                  ctx,
                  u,
                  optBindingNameInfo,
                  optGuard,
                  pos,
                  sirScrutineeType
                )
            case Ident(nme.WILDCARD) =>
                val optSirGuard = optGuard.map(guard =>
                    val nVars = addBindignName(ctx.env.vars)
                    val sir = compiler.compileExpr(ctx.env.copy(vars = nVars), guard)
                    sir
                )
                SirParsedCase.Pattern.Wildcard(
                  sirScrutineeType,
                  optBindingNameInfo,
                  optSirGuard,
                  pos
                )
            case Literal(const) =>
                val sirExpr = compiler.compileExpr(ctx.env, pat)
                sirExpr match
                    case c: SIR.Const =>
                        val optSirGuard = optGuard.map(guard =>
                            val nVars = addBindignName(ctx.env.vars)
                            val sir = compiler.compileExpr(ctx.env.copy(vars = nVars), guard)
                            sir
                        )
                        SirParsedCase.Pattern.PrimitiveConstant(c, None, optSirGuard, pat.sourcePos)
                    case _ =>
                        val err = GenericError("const pattern should be a constant", pat.srcPos)
                        compiler.error(err, SirParsedCase.Pattern.Error(err))
            case Typed(inner, tpt) =>
                val tptSirType = compiler.sirTypeInEnv(tpt.tpe, pos, ctx.env)
                if tptSirType ~=~ sirScrutineeType then {
                    parsePatternInOptBind(
                      ctx,
                      inner,
                      optBindingNameInfo,
                      optGuard,
                      inner.sourcePos,
                      tptSirType
                    )
                } else
                    val innerPattern = parsePatternInOptBind(
                      ctx,
                      inner,
                      optBindingNameInfo,
                      None,
                      inner.sourcePos,
                      tptSirType
                    )
                    val optSirGuard = optGuard.map(guard =>
                        val nVars = addBindignName(ctx.env.vars) ++ innerPattern.collectNames
                        compiler.compileExpr(ctx.env.copy(vars = nVars), guard)
                    )
                    SirParsedCase.Pattern.TypeSelector(
                      tptSirType,
                      optBindingNameInfo,
                      innerPattern,
                      optSirGuard,
                      pos
                    )
            case Alternative(trees) =>
                val patterns = trees.map { p =>
                    parsePatternInOptBind(
                      ctx,
                      p,
                      optBindingNameInfo,
                      None,
                      p.sourcePos,
                      sirScrutineeType
                    )
                }
                SirParsedCase.Pattern.OrPattern(patterns.toList, pat.sourcePos)
            case _ =>
                compiler.error(
                  UnsupportedMatchExpression(pat, pat.srcPos),
                  SirParsedCase.Pattern
                      .Wildcard(sirScrutineeType, optBindingNameInfo, None, pos)
                )

    }

    def parsePattern(
        ctx: PatternMatchingContext,
        pat: Tree,
        optGuard: Option[Tree],
        pos: SrcPos,
        sirScrutineeType: SIRType
    ): SirParsedCase.Pattern = {
        if ctx.env.debug then
            println(s"parsePattern: ${pat.show} ${pat.tpe.show} ${sirScrutineeType}")
        pat match
            // this is case Constr(name @ _) or Constr(name)
            case b @ Bind(name, patten) =>
                val tp = compiler.sirTypeInEnv(b.tpe.widen, b.srcPos, ctx.env)
                val nameInfo =
                    SirParsedCase.BindingNameInfo(name.show, Some(name.show), tp, b.symbol)
                parsePatternInOptBind(
                  ctx,
                  patten,
                  Some(nameInfo),
                  optGuard,
                  pat.srcPos.sourcePos,
                  sirScrutineeType
                )
            case _ =>
                parsePatternInOptBind(
                  ctx,
                  pat,
                  None,
                  optGuard,
                  pat.srcPos.sourcePos,
                  sirScrutineeType
                )
    }

    def parseSIRCase(
        ctx: PatternMatchingContext,
        c: CaseDef,
        i: Int,
        scrutineeType: SIRType
    ): (SirParsedCase, Tree) = {
        c match
            case CaseDef(pat, guard, rhs) =>
                val optGuard = if guard.isEmpty then None else Some(guard)
                val sirPat = parsePattern(ctx, pat, optGuard, c.srcPos, scrutineeType)
                val sirCase = SirParsedCase(
                  sirPat,
                  SirParsedCase.Action.Origin(i),
                  c.srcPos.sourcePos
                )
                (sirCase, rhs)
    }

    def parseMatch(
        ctx: PatternMatchingContext,
        tree: Match,
        env: SIRCompiler.Env
    ): SirParsedMatch = {
        if env.debug then println(s"parsedMatch: ${tree.show}")
        val Match(scrutinee, cases) = tree
        val scrutineeType = scrutinee.tpe.dealias.widen
        val scrutineeSirType = sirTypeInEnv(scrutineeType, tree.srcPos, env)
        val sirCasesAndActions = cases.zipWithIndex.map { case (cs, i) =>
            parseSIRCase(ctx, cs, i, scrutineeSirType)
        }.toIndexedSeq
        val actions = sirCasesAndActions.map(_._2)
        val matchSirType = sirTypeInEnv(tree.tpe.dealias.widen, tree.srcPos, env)
        ctx.originActions = actions
        val scrutineeSir = compiler.compileExpr(env, scrutinee)
        SirParsedMatch(
          scrutineeSir,
          sirCasesAndActions.map(_._1).toList,
          actions,
          scrutineeSirType,
          matchSirType,
          tree.srcPos.sourcePos
        )
    }

    def buildDecisionTree(
        ctx: PatternMatchingContext,
        parsedMatch: SirParsedMatch,
        env: SIRCompiler.Env
    ): SirCaseDecisionTree = {
        buildDecisionTreeRec(
          ctx,
          parsedMatch.cases,
          parsedMatch.scrutineeTp,
          parsedMatch.pos
        )

    }

    def buildDecisionTreeRec(
        ctx: PatternMatchingContext,
        cases: List[SirParsedCase],
        currentScrutineeTp: SIRType,
        topLevelPos: SrcPos
    ): SirCaseDecisionTree = {

        cases match
            case Nil =>
                SirCaseDecisionTree.Leaf(SirParsedCase.Action.FailMatch, topLevelPos)
            case head :: tail =>
                head match
                    case SirParsedCase(pattern, action, pos) =>
                        pattern match
                            case typeSelector: SirParsedCase.Pattern.TypeSelector =>
                                val nCases = unrollTypeSelector(
                                  ctx,
                                  typeSelector,
                                  action,
                                  pos,
                                  currentScrutineeTp,
                                  None
                                )
                                buildDecisionTreeRec(
                                  ctx,
                                  nCases ++ tail,
                                  currentScrutineeTp,
                                  topLevelPos
                                )
                            case alt: SirParsedCase.Pattern.OrPattern =>
                                val nCases = unrollAlternatives(alt, action, pos)
                                buildDecisionTreeRec(
                                  ctx,
                                  nCases ++ tail,
                                  currentScrutineeTp,
                                  topLevelPos
                                )
                            case const: SirParsedCase.Pattern.PrimitiveConstant =>
                                val guard =
                                    constantToWidlcardWithGuard(ctx, const, currentScrutineeTp)
                                val nCase = SirParsedCase(guard, action, pos)
                                buildDecisionTreeRec(
                                  ctx,
                                  nCase :: tail,
                                  currentScrutineeTp,
                                  topLevelPos
                                )
                            case w: SirParsedCase.Pattern.Wildcard =>
                                w.optGuard match
                                    case None =>
                                        SirCaseDecisionTree.Leaf(action, w.pos)
                                    case Some(guard) =>
                                        val nextTrue = SirCaseDecisionTree.Leaf(action, w.pos)
                                        val nextFalse = buildDecisionTreeRec(
                                          ctx,
                                          tail,
                                          currentScrutineeTp,
                                          topLevelPos
                                        )
                                        SirCaseDecisionTree.CheckGuard(
                                          guard,
                                          w.pos,
                                          nextTrue,
                                          nextFalse
                                        )
                            case constr: SirParsedCase.Pattern.Constructor =>
                                val (constructors, tail) = collectConstructors(ctx, cases)
                                val tree = SirCaseDecisionTree
                                    .ConstructorsChoice(
                                      constr.optNameInfo.map(_.name),
                                      constructors
                                          .map(c =>
                                              (
                                                c.tp.constrDecl.name,
                                                buildConstructorDecisionTree(
                                                  ctx,
                                                  c,
                                                  currentScrutineeTp,
                                                  topLevelPos
                                                )
                                              )
                                          )
                                          .toMap
                                    )
                                if tail.isEmpty then tree
                                else
                                    SirCaseDecisionTree.SeqCheck(
                                      tree,
                                      buildDecisionTreeRec(
                                        ctx,
                                        tail,
                                        currentScrutineeTp,
                                        topLevelPos
                                      )
                                    )
    }

    def buildConstructorDecisionTree(
        ctx: PatternMatchingContext,
        constructorCases: SirParsedCase.JoinedConstructorCase,
        scrutineeTp: SIRType,
        topLevelPos: SrcPos
    ): SirCaseDecisionTree.ConstructorEntry = {
        ???
    }

    /*
        else
            val head = cases(currentRowIndex)
            val nonWildcards = head.patterns.zipWithIndex.filter((p, i) =>
                p match {
                    case SirParsedCase.Pattern.Wildcard(_, _, None, _) => false
                    case _                                             => true
                }
            )
            val rebindNames =
                head.patterns.zipWithIndex.foldLeft(IndexedSeq.empty[ConstructorBinding]) {
                    case (s, (v, i)) =>
                        e.optBindingNameInfo match
                            case Some(b) =>
                                s :+ ConstructorBinding(b.name, v.tp, i, v.pos)
                            case None => s
                }

            if nonWildcards.isEmpty then
                if head.optGuard.isEmpty then
                    val leaf = Leaf(head.action, head.pos)
                    val constrRebinds = SirCaseDecisionTree.RebindUpConstructor(rebindNames, leaf)
                    val retval = head.optNameInfo match
                        case Some(b) =>
                            SirCaseDecisionTree.RebindTopLevelBind(b.name, constrRebinds)
                        case None => constrRebinds
                    if currentRowIndex < cases.size - 1 then
                        report.warning(s"Unused cases ", tail.head.pos)
                    retval
                else
                    val condLeaf = Leaf(head.action, head.pos)
                    val nextFalse = buildConstructorInternalDecisionTree(
                      ctx,
                      tail,
                      constrArgsBindings,
                      constrType,
                      topLevelPos
                    )
                    val guardCheck = SirCaseDecisionTree.CheckGuard(
                      head.optGuard.get,
                      head.pos,
                      condLeaf,
                      nextFalse
                    )
                    val constrRebinds =
                        SirCaseDecisionTree.RebindUpConstructor(rebindNames, guardCheck)
                    val retval = head.optNameInfo match
                        case Some(b) =>
                            SirCaseDecisionTree.RebindTopLevelBind(b.name, constrRebinds)
                        case None => constrRebinds
                    retval
            else
                val withConditionRows = nonWildcards.map((p, i) =>
                    (p, i, yesNoRows(cases, currentRow, i)).sortBy(_._3._2.size)
                )
                // TODO: we can use other heurisitics or do profile-driven optimization.
                val fullRows = (currentRowIndex to cases.size - 1).toSet
                val checks =
                    withConditionRows.foldLeft(
                      (
                        Seq.empty[(SirCaseDecisionTree, SIR.Var, Set[Int], Set[Int])],
                        fullRows
                      )
                    ) { (acc, e) =>
                        val (pattern, index, (yesSet, noSet)) = e
                        val argBinding = constrArgsBindings(index)
                        val newScrutinee = SIR.Var(
                          argBinding.name,
                          argBinding.tp,
                          AnnotationsDecl.fromSrcPos(argBinding.pos)
                        )
                        val newMatch = buildDecisionTree(
                          ctx,
                          SirParsedMatch(
                            newScrutinee,
                            List(
                              SirParsedCase.PatternAction(
                                pattern,
                                SirParsedCase.Action.Origin(0),
                                pattern.pos
                              )
                            )
                          )
                        )
                        val newYesIndex = acc._3 and yesSet
                        val newNoIndex = acc._3 and noSet
                        val newAcc = (
                          acc._2 :+ (newMatch, newScrutinee, newYesIndex, newNoIndex),
                          newYesIndex
                        )
                    }
                val (checkSeq, lastYes) = checks
                val restTree =
                    if currentRowIndex == cases.size - 1 then Leaf(FailMatch)
                    else
                        buildConstructorInternalDecisionTree(
                          ctx,
                          cases,
                          constrArgsBindings,
                          constrType,
                          currentRowIndex + 1
                        )
                val nextFailGuardIndex =
                    if lastYes.size == 1 then Leaf(FailMatch)
                    else restTree
                val firstFailUnique = checkSeq.head._3.size == 1
                val ifHead = head.action
                val guardedIfHead = head.optGuard match {
                    case Some(guard) =>
                        SirCaseDecisionTree.CheckGuard(
                          guard,
                          head.pos,
                          Leaf(ifHead, head.pos),
                          nextFailGuardIndex
                        )
                }
                
                val allPossibleNoRows = checksSeq.foldLeft(Set.empty[Inf])
                val tree = checks._1.fold {
                    case (
                          (tree1, scrutinee1, yesSet1, noSet1),
                          (tree2, scrutinee2, yesSet2, noSet2)
                        ) =>
                        SirCaseDecisionTree.InnerMatch(
                          scrutinee1,
                          tree,
                        )
                }
                ???
                
     */

    def compileDecisionTree(
        scrutinee: AnnotatedSIR,
        decisionTree: SirCaseDecisionTree,
        env: SIRCompiler.Env
    ): AnnotatedSIR = {
        ???
    }

    def compileMatch(tree: Match, env: SIRCompiler.Env): AnnotatedSIR = {
        val bPrefix = s"_${tree.srcPos.startPos.source.name}_${tree.srcPos.line}_match_"
        val ctx = PatternMatchingContext(bPrefix, env)
        val parsedMatch = parseMatch(ctx, tree, env)
        val decisionTree = buildDecisionTree(ctx, parsedMatch, env)
        val sirMatch = compileDecisionTree(parsedMatch.scrutinee, decisionTree, env)
        sirMatch
        /*
        if env.debug then println(s"compileMatch: ${tree.show}")
        val Match(matchTree, cases) = tree
        // val typeSymbol = matchTree.tpe.widen.dealias.typeSymbol
        // report.echo(s"Match: ${typeSymbol} ${typeSymbol.children} $adtInfo", tree.srcPos)
        val isUnchecked = matchTree match
            case Typed(selectorExpr, tp) =>
                tp.tpe match
                    case AnnotatedType(_, ann) =>
                        ann.symbol == defn.UncheckedAnnot
                    case _ => false
            case _ => false
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
                    val anns = AnnotationsDecl.fromSourcePosition(srcPos)
                    expandedCases += SIR.Case(Constr(constrDecl, bindings, typeParams), rhs, anns)
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
                        expandedCases += SIR.Case(
                          Pattern.Wildcard,
                          rhs,
                          AnnotationsDecl.fromSourcePosition(srcPos)
                        )
                case SirCase.Error(err) =>
                    compiler.error(err, ())

            idx += 1
        end while
        val annotations0 = AnnotationsDecl.fromSrcPos(tree.srcPos)
        val annotations =
            if isUnchecked then
                annotations0.copy(
                  data = annotations0.data.updated(
                    "unchecked",
                    SIR.Const(
                      scalus.uplc.Constant.Bool(true),
                      SIRType.Boolean,
                      AnnotationsDecl.empty
                    )
                  )
                )
            else annotations0
        SIR.Match(
          matchExpr,
          expandedCases.toList,
          sirTypeInEnv(tree.tpe.dealias.widen, tree.srcPos, env),
          annotations
        )
        
         */
    }

    private def sirTypeInEnv(tpe: Type, srcPos: SrcPos, env: SIRCompiler.Env): SIRType = {
        compiler.sirTypeInEnv(tpe, srcPos, env)
    }

    private def unrollTypeSelector(
        ctx: PatternMatchingContext,
        ts: SirParsedCase.Pattern.TypeSelector,
        action: SirParsedCase.Action,
        pos: SrcPos,
        scrutineeTp: SIRType,
        optParentSeq: Option[Seq[SIRType]]
    ): List[SirParsedCase] = {

        if ts.tp == SIRType.FreeUnificator then
            List(
              SirParsedCase(ts.innerPattern.withOptAlias(ts.optNameInfo), action, pos)
            )
        else if SIRType.isSum(scrutineeTp) then {
            val parentSeq = optParentSeq.getOrElse(SIRType.parentsEqSeq(ts.tp, scrutineeTp))
            parentSeq match
                case Nil =>
                    val err = GenericError(
                      s"Type selector pattern type ${ts.tp} is not a parent of scrutinee type ${scrutineeTp}",
                      pos
                    )
                    compiler.error(err, List.empty)
                case head :: Nil =>
                    List(
                      SirParsedCase(
                        ts.innerPattern.withOptAlias(ts.optNameInfo),
                        action,
                        pos
                      )
                    )
                case head :: middle :: next =>
                    if !next.isEmpty then
                        val err = GenericError(
                          s"Type selector with more than two levels of nesting is not supported",
                          pos
                        )
                        compiler.error(err, List.empty)
                    else
                        // most typical case, second is selector
                        SIRType.collectProd(ts.tp) match {
                            case Some((typeParams, constrDecl, typeArgs)) =>
                                ts.innerPattern match {
                                    case wd: SirParsedCase.Pattern.Wildcard =>
                                        val optParent = SIRType.prodParent(ts.tp)
                                        val typeMap = constrDecl.typeParams.zip(typeArgs).toMap
                                        val subpatterns = constrDecl.params.map { p =>
                                            val ptp = SIRType.substitute(p.tp, typeMap, Map.empty)
                                            SirParsedCase.Pattern.Wildcard(
                                              ptp,
                                              None,
                                              None,
                                              pos
                                            )
                                        }
                                        val constrPattern = SirParsedCase.Pattern.Constructor(
                                          SIRType.CaseClass(constrDecl, typeArgs, optParent),
                                          typeParams,
                                          ts.optNameInfo,
                                          subpatterns.toIndexedSeq,
                                          ts.optGuard,
                                          pos
                                        )
                                        if next.isEmpty then
                                            List(
                                              SirParsedCase(constrPattern, action, pos)
                                            )
                                        else {
                                            val nTypeSelector = SirParsedCase.Pattern.TypeSelector(
                                              middle,
                                              ts.optNameInfo,
                                              constrPattern,
                                              None,
                                              pos
                                            )
                                            unrollTypeSelector(
                                              ctx,
                                              nTypeSelector,
                                              action,
                                              pos,
                                              scrutineeTp,
                                              Some(next)
                                            )
                                        }
                                    case _ =>
                                        val err = GenericError(
                                          s"Type selector with non-wildcard inner pattern is not supported",
                                          pos
                                        )
                                        compiler.error(err, List.empty)
                                }
                            case None =>
                                SIRType.collectSum(ts.tp) match {
                                    case Some((freeTypeParams, dataDecl, typeArgs)) =>
                                        SIRType.collectSum(middle) match {
                                            case Some((_, middleDataDecl, middleTypeArgs)) =>
                                                val constrName =
                                                    SIRType.syntheticNarrowConstrDeclName(
                                                      middleDataDecl.name
                                                    )
                                                val constrTp = middleDataDecl.constrType(constrName)
                                                SIRType.collectProd(constrTp) match
                                                    case None =>
                                                        val err = GenericError(
                                                          s"narrowing constructor type is not case class or enum: ${constrTp.show}",
                                                          pos
                                                        )
                                                        compiler.error(err, List.empty)
                                                    case Some((prodTp, constrDecl, typeArgs)) =>
                                                        val caseClassType = SIRType.CaseClass(
                                                          constrDecl,
                                                          typeArgs,
                                                          Some(middle)
                                                        )
                                                        val newNameInfo =
                                                            SirParsedCase.BindingNameInfo(
                                                              ctx.freshName(Some("_w")),
                                                              None,
                                                              caseClassType,
                                                              NoSymbol
                                                            )
                                                        val nPattern =
                                                            SirParsedCase.Pattern.Constructor(
                                                              caseClassType,
                                                              prodTp,
                                                              Some(newNameInfo),
                                                              IndexedSeq(
                                                                ts.innerPattern.withOptAlias(
                                                                  ts.optNameInfo
                                                                )
                                                              ),
                                                              ts.optGuard,
                                                              ts.pos
                                                            )
                                                        if next.isEmpty then {
                                                            val retval =
                                                                SirParsedCase(
                                                                  nPattern,
                                                                  action,
                                                                  pos
                                                                )
                                                            List(retval)
                                                        } else {
                                                            val nTypeSelector =
                                                                SirParsedCase.Pattern.TypeSelector(
                                                                  middle,
                                                                  Some(newNameInfo),
                                                                  nPattern,
                                                                  None,
                                                                  pos
                                                                )
                                                            unrollTypeSelector(
                                                              ctx,
                                                              nTypeSelector,
                                                              action,
                                                              pos,
                                                              scrutineeTp,
                                                              Some(next)
                                                            )
                                                        }
                                            case None =>
                                                val err = GenericError(
                                                  s"Type selector pattern type ${ts.tp.show} is not found in ${middle.show} constructors",
                                                  pos
                                                )
                                                compiler.error(err, List.empty)
                                        }
                                    case None =>
                                        val err = GenericError(
                                          s"Type selector pattern type ${ts.tp.show} is not a product or sum",
                                          pos
                                        )
                                        compiler.error(err, List.empty)
                                }
                        }
        } else if SIRType.isProd(scrutineeTp) then {
            if SIRType.isProd(ts.tp) then {
                SIRUnify.topLevelUnifyType(scrutineeTp, ts.tp, SIRUnify.Env.empty) match
                    case SIRUnify.UnificationSuccess(env, u) =>
                        val nPattern = ts.innerPattern.withOptAlias(ts.optNameInfo)
                        List(SirParsedCase(nPattern, action, pos))
                    case SIRUnify.UnificationFailure(path, left, right) =>
                        val err = GenericError(
                          s"Type selector pattern type ${ts.tp.show} is not compatible with scrutinee type ${scrutineeTp.show}",
                          pos
                        )
                        compiler.error(err, List.empty)
            } else
                val err = GenericError(
                  s"Type selector pattern type ${ts.tp.show} is not a child of scrutinee type ${scrutineeTp.show}",
                  pos
                )
                compiler.error(err, List.empty)
        } else {
            SIRUnify.topLevelUnifyType(ts.tp, scrutineeTp, SIRUnify.Env.empty) match
                case SIRUnify.UnificationSuccess(env, u) =>
                    val nPattern = ts.innerPattern.withOptAlias(ts.optNameInfo)
                    val retval = SirParsedCase(nPattern, action, pos)
                    List(retval)
                case SIRUnify.UnificationFailure(path, left, right) =>
                    val err = GenericError(
                      s"Type selector pattern type ${ts.tp.show} is not compatible with scrutinee type ${scrutineeTp.show}",
                      pos
                    )
                    compiler.error(err, List.empty)
        }

    }

    private def unrollAlternatives(
        alt: SirParsedCase.Pattern.OrPattern,
        action: SirParsedCase.Action,
        pos: SrcPos,
    ): List[SirParsedCase] = alt.patterns.map(p => SirParsedCase(p, action, p.pos))

    private def constantToWidlcardWithGuard(
        ctx: PatternMatchingContext,
        constCase: SirParsedCase.Pattern.PrimitiveConstant,
        scrutineeTp: SIRType
    ): SirParsedCase.Pattern.Wildcard = {
        val nameInfo = constCase.optNameInfo.getOrElse {
            val genName = ctx.freshName(Some("const"))
            SirParsedCase.BindingNameInfo(genName, None, constCase.value.tp, NoSymbol)
        }
        val constGuard = genSIREq(ctx, nameInfo, constCase.value, constCase.pos)
        val nGuard = constCase.optGuard match {
            case None        => constGuard
            case Some(guard) => SIR.And(constGuard, guard, AnnotationsDecl.empty)
        }
        val nPattern = SirParsedCase.Pattern.Wildcard(
          scrutineeTp,
          Some(nameInfo),
          Some(nGuard),
          constCase.pos
        )
        nPattern
    }

    private def genSIREq(
        ctx: PatternMatchingContext,
        nameInfo: SirParsedCase.BindingNameInfo,
        const: SIR.Const,
        pos: SrcPos
    ): AnnotatedSIR = {
        val vtp = ctx.env.vars.getOrElse(
          nameInfo.name,
          compiler.error(
            GenericError(
              s"internal error: variable ${nameInfo.name} not found in environment",
              pos
            ),
            SIRType.TypeNothing
          )
        )
        val v = SIR.Var(nameInfo.name, vtp, AnnotationsDecl.fromSrcPos(nameInfo.symbol.srcPos))

        def genEq(builtin: SIR.Builtin, tp: SIRType): AnnotatedSIR = {
            SIR.Apply(
              SIR.Apply(
                builtin,
                v,
                SIRType.Fun(tp, SIRType.Boolean),
                AnnotationsDecl.fromSrcPos(pos)
              ),
              const,
              SIRType.Boolean,
              AnnotationsDecl.fromSrcPos(pos)
            )
        }

        val condExpr = const.tp match {
            case SIRType.Integer =>
                genEq(SIRBuiltins.equalsInteger, SIRType.Integer)
            case SIRType.ByteString =>
                genEq(SIRBuiltins.equalsByteString, SIRType.ByteString)
            case SIRType.String =>
                genEq(SIRBuiltins.equalsString, SIRType.String)
            case SIRType.Boolean =>
                SIR.IfThenElse(
                  v,
                  SIR.IfThenElse(
                    const,
                    const,
                    SIR.Const(uplc.Constant.Bool(false), SIRType.Boolean, AnnotationsDecl.empty),
                    SIRType.Boolean,
                    AnnotationsDecl.fromSrcPos(pos)
                  ),
                  SIR.IfThenElse(
                    const,
                    v,
                    SIR.Const(uplc.Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty),
                    SIRType.Boolean,
                    AnnotationsDecl.fromSrcPos(pos)
                  ),
                  SIRType.Boolean,
                  AnnotationsDecl.fromSrcPos(pos)
                )
            case SIRType.Unit =>
                compiler.error(
                  GenericError("Unit constant in pattern is not supported", pos),
                  SIR.Const(uplc.Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty)
                )
            case SIRType.Data =>
                genEq(SIRBuiltins.equalsData, SIRType.Data)
            case SIRType.BLS12_381_G1_Element =>
                genEq(SIRBuiltins.bls12_381_G1_equal, SIRType.BLS12_381_G1_Element)
            case SIRType.BLS12_381_G2_Element =>
                genEq(SIRBuiltins.bls12_381_G2_equal, SIRType.BLS12_381_G2_Element)
            case SIRType.BLS12_381_MlResult =>
                // TODO: check if this is correct. mb better to disallow it
                genEq(SIRBuiltins.bls12_381_finalVerify, SIRType.BLS12_381_MlResult)
            case _ =>
                compiler.error(
                  GenericError(
                    s"Constant of type ${const.tp.show} in pattern is not supported",
                    pos
                  ),
                  SIR.Const(uplc.Constant.Bool(false), SIRType.Boolean, AnnotationsDecl.empty)
                )

        }

        condExpr
    }

    /** Collect cases by constructors up to the next if. Received touple
      */
    private def collectConstructors(
        ctx: PatternMatchingContext,
        cases: List[SirParsedCase]
    ): (List[SirParsedCase.JoinedConstructorCase], List[SirParsedCase]) = {

        import scala.collection.mutable.ListBuffer

        var byConstructor: Map[String, (SIRType.CaseClass, ListBuffer[SirParsedCase])] = Map.empty
        var cursor = cases

        while {
            cursor.nonEmpty && {
                cursor.head.pattern match
                    case constr: SirParsedCase.Pattern.Constructor =>
                        val constrName = constr.tp.constrDecl.name
                        byConstructor.get(constrName) match
                            case Some((tp, lst)) => lst += cursor.head
                            case None =>
                                byConstructor = byConstructor.updated(
                                  constrName,
                                  (constr.tp, ListBuffer(cursor.head))
                                )
                        true
                    case _ => false
            }
        } do {
            cursor = cursor.tail
        }

        val joined = byConstructor.map { case (constrName, (constr, lst)) =>
            makeJoinedConstructorCase(constr, lst.toList)
        }.toList

        (joined, cursor)

    }

    private def makeJoinedConstructorCase(
        constrTp: SIRType.CaseClass,
        cases: List[SirParsedCase]
    ): SirParsedCase.JoinedConstructorCase = {
        ???
    }

}
