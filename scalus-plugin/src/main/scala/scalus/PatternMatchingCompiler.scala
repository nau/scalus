package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.{comparing, Context}
import dotty.tools.dotc.core.NameKinds.{Scala2MethodNameKinds, UniqueNameKind}
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.{SourcePosition, SrcPos}
import scalus.SirCaseDecisionTree.Leaf
import scalus.SirParsedCase.ActionRef.FailMatch
import scalus.SirParsedCase.{ActionRef, BindingNameInfo, Pattern}
import scalus.sir.*

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
    var parsedActions: IndexedSeq[SirParsedAction] = IndexedSeq.empty,
    var parsedGuards: IndexedSeq[Option[SirParsedGuard]] = IndexedSeq.empty
) {

    private var tmpNameCounter: Int = 1

    private var actionsUsageCount: Map[Int, Int] = Map.empty
    private var guardsUsageCount: Map[Int, Int] = Map.empty

    def freshName(prefix: String = ""): String = {
        val localPrefix = prefix
        val name = s"${globalPrefix}${localPrefix}_$tmpNameCounter"
        tmpNameCounter += 1
        name
    }

    def countActionUsage(actionIndex: Int): Int = {
        actionsUsageCount.get(actionIndex) match
            case Some(c) =>
                actionsUsageCount = actionsUsageCount + (actionIndex -> (c + 1))
                c + 1
            case None =>
                actionsUsageCount = actionsUsageCount + (actionIndex -> 1)
                1
    }

    def countGuardUsage(guardIndex: Int): Int = {
        guardsUsageCount.get(guardIndex) match
            case Some(c) =>
                guardsUsageCount = guardsUsageCount + (guardIndex -> (c + 1))
                c + 1
            case None =>
                guardsUsageCount = guardsUsageCount + (guardIndex -> 1)
                1
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
    actionRef: SirParsedCase.ActionRef,
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

        def optGuard: Option[Int]

        def withoutGuard: Pattern

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

    end Pattern

    enum ActionRef:
        case Origin(i: Int)
        case MergedConstructorCond(
            frsPatterm: IndexedSeq[Pattern],
            frsAction: ActionRef,
            sndPatterm: Pattern,
            sndAction: ActionRef,
        )
        case FailMatch
        case ContinueMatch
    end ActionRef

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

    case class GroupedTupleRow(
        patterns: IndexedSeq[Pattern],
        guard: Option[Int],
        action: ActionRef,
        pos: SrcPos
    )

    case class GroupedTuples(
        columnBindings: IndexedSeq[Option[BindingNameInfo]],
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

case class SirParsedAction(
    sir: AnnotatedSIR,
    bindedVariables: List[TypeBinding],
    pos: SrcPos
)

case class SirParsedGuard(
    sir: AnnotatedSIR,
    bindedVariables: List[TypeBinding],
    pos: SrcPos
)

case class SirParsedMatch(
    scrutinee: AnnotatedSIR,
    cases: IndexedSeq[SirParsedCase],
    compiledActions: IndexedSeq[SirParsedAction],
    comliledGuards: IndexedSeq[Option[SirParsedGuard]],
    scrutineeTp: SIRType,
    resTp: SIRType,
    pos: SourcePosition
)

sealed trait SirCaseDecisionTree

object SirCaseDecisionTree:

    case class ConstructorsChoice(
        columnName: Option[String],
        byConstructors: Map[String, ConstructorEntry],
        defaultBranch: Option[SirCaseDecisionTree],
    ) extends SirCaseDecisionTree

    case class ConstructorEntry(
        caseClass: SIRType.CaseClass,
        names: IndexedSeq[Option[SirParsedCase.BindingNameInfo]],
        next: SirCaseDecisionTree,
        pos: SrcPos
    )

    case class ConstantChoice(
        columnName: Option[String],
        byConstants: Map[SIR.Const, SirCaseDecisionTree],
        defaultBranch: Option[SirCaseDecisionTree],
    )

    case class CheckGuard(
        guard: Int,
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
        names: IndexedSeq[SirParsedCase.BindingNameInfo],
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
        action: SirParsedCase.ActionRef,
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
                        compiler.error(
                          err,
                          SirParsedCase.Pattern
                              .Wildcard(sirScrutineeType, optBindingNameInfo, None, pos)
                        )
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
    ): (SirParsedCase, Option[SirParsedGuard], SirParsedAction) = {
        c match
            case CaseDef(pat, guard, rhs) =>
                val optGuard = if guard.isEmpty then None else Some(guard)
                val sirPat = parsePattern(ctx, pat, optGuard, c.srcPos, scrutineeType)
                val sirCase = SirParsedCase(
                  sirPat,
                  SirParsedCase.ActionRef.Origin(i),
                  c.srcPos.sourcePos
                )
                val patNames = sirPat.collectNames
                val nEnv = ctx.env.copy(vars = ctx.env.vars ++ patNames)
                val optCompiledGuard = optGuard.map { g =>
                    val sirG = compiler.compileExpr(nEnv, g)
                    val (usedNames, unusedNames) =
                        SIRNameOperations.partitionUsedFreeNamesFrom(sirG, patNames.keys.toSet)
                    val typeBindings = usedNames.map { name =>
                        TypeBinding(name, patNames(name))
                    }.toList
                    SirParsedGuard(sirG, typeBindings, g.srcPos)
                }
                val compiledAction = parseCaseAction(ctx, patNames, nEnv, rhs, i)
                (sirCase, optCompiledGuard, compiledAction)
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
        val sirCasesGuardActions = cases.zipWithIndex.map { case (cs, i) =>
            parseSIRCase(ctx, cs, i, scrutineeSirType)
        }.toIndexedSeq
        val sirCases = sirCasesGuardActions.map(_._1)
        val actions = sirCasesGuardActions.map(_._3)
        ctx.parsedActions = actions
        val optGuards = sirCasesGuardActions.map(_._2)
        ctx.parsedGuards = optGuards
        val matchSirType = sirTypeInEnv(tree.tpe.dealias.widen, tree.srcPos, env)
        val scrutineeSir = compiler.compileExpr(env, scrutinee)
        SirParsedMatch(
          scrutineeSir,
          sirCases,
          actions,
          optGuards,
          scrutineeSirType,
          matchSirType,
          tree.srcPos.sourcePos
        )
    }

    private def parseCaseAction(
        ctx: PatternMatchingContext,
        patNames: Map[String, SIRType],
        env: SIRCompiler.Env,
        rhs: Tree,
        actionIndex: Int
    ): SirParsedAction = {
        val sirRhs = compiler.compileExpr(env, rhs)
        val (usedNames, unusedNames) =
            SIRNameOperations.partitionUsedFreeNamesFrom(sirRhs, patNames.keys.toSet)
        val typeBindings = usedNames.map { name =>
            TypeBinding(name, patNames(name))
        }.toList
        SirParsedAction(sirRhs, typeBindings, rhs.srcPos)
    }

    def buildDecisionTree(
        ctx: PatternMatchingContext,
        parsedMatch: SirParsedMatch,
        env: SIRCompiler.Env
    ): SirCaseDecisionTree = {

        val tupleRows = parsedMatch.cases.zipWithIndex.map { case (c, i) =>
            val optGuard = c.pattern.optGuard
            val patternNoGuard = c.pattern.withoutGuard
            SirParsedCase.GroupedTupleRow(
              IndexedSeq(c.pattern),
              optGuard,
              SirParsedCase.ActionRef.Origin(i),
              c.pos
            )
        }

        val firstOptNameInfo =
            parsedMatch.cases.find(_.pattern.optNameInfo.isDefined).flatMap(_.pattern.optNameInfo)

        val globalBindName =
            if tupleRows.size == 1 then firstOptNameInfo
            else if firstOptNameInfo.isDefined then
                // we should define fresh name, because other branches can reuse firstOptNameInfo name in other meaning.
                Some(
                  SirParsedCase.BindingNameInfo(
                    ctx.freshName(firstOptNameInfo.get.name),
                    None,
                    parsedMatch.scrutineeTp,
                    Symbols.NoSymbol
                  )
                )
            else None

        val groupedTuples = SirParsedCase.GroupedTuples(
          IndexedSeq(globalBindName),
          Set(0),
          tupleRows
        )

        buildGroupedTuplesDecisionTree(ctx, groupedTuples, parsedMatch.scrutineeTp, parsedMatch.pos)

    }

    def buildConstructorDecisionTree(
        ctx: PatternMatchingContext,
        constructorCases: SirParsedCase.JoinedConstructorCase,
        scrutineeTp: SIRType,
        topLevelPos: SrcPos
    ): SirCaseDecisionTree.ConstructorEntry = {

        val nParams = constructorCases.tp.constrDecl.params.length
        if nParams == 0 then
            val firstCase = constructorCases.tuples.head
            val leaf = SirCaseDecisionTree.Leaf(firstCase.action, firstCase.pos)
            val entry = SirCaseDecisionTree.ConstructorEntry(
              constructorCases.tp,
              IndexedSeq.empty,
              leaf,
              topLevelPos
            )
            entry
        else
            val columnBinding = constructorCases.tp.constrDecl.params
                .map(b =>
                    Option(
                      BindingNameInfo(
                        ctx.freshName(b.name),
                        None,
                        b.tp,
                        Symbols.NoSymbol,
                        Set.empty
                      )
                    )
                )
                .toIndexedSeq

            val group = SirParsedCase.GroupedTuples(
              columnBinding,
              (0 until nParams).toSet,
              constructorCases.tuples
            )

            val nextTree = buildGroupedTuplesDecisionTree(
              ctx,
              group,
              constructorCases.tp,
              topLevelPos
            )
            SirCaseDecisionTree.ConstructorEntry(
              constructorCases.tp,
              columnBinding,
              nextTree,
              topLevelPos
            )
    }

    def buildGroupedTuplesDecisionTree(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples,
        scrutineeTp: SIRType,
        topLevelPos: SrcPos,
        startRow: Int = 0,
    ): SirCaseDecisionTree = {
        val group1 = eliminateAlternativesInGroup(group, startRow)
        val group2 = eliminateTypeSelectorsInGroup(group1, startRow)
        if group2.rows.isEmpty then
            SirCaseDecisionTree.Leaf(SirParsedCase.ActionRef.FailMatch, topLevelPos)
        else
            val firstRow = group2.rows.head
            val nonWildcards = firstRow.patterns.zipWithIndex.filter { case (p, i) =>
                group2.activeColumns.contains(i) && {
                    p match {
                        case SirParsedCase.Pattern.Wildcard(_, _, _, _) => false
                        case _                                          => true
                    }
                }
            }
            if nonWildcards.isEmpty then {
                val retval = Leaf(firstRow.action, firstRow.pos)
                if group.rows.length > 1 then {
                    group.rows.tail.foreach(r =>
                        if r.action != FailMatch then
                            report.warning("Unreachable case in pattern matching", r.pos)
                    )
                }
                retval
            } else {
                val prioritized = prioritizePatterns(group2, startRow, patterns = nonWildcards)
                val (p, i) = prioritized.head
                p match
                    case constr: SirParsedCase.Pattern.Constructor =>
                        buildSpecializedConstr(ctx, group2, row, i, scrutineeTp, topLevelPos)
                    case const: SirParsedCase.Pattern.PrimitiveConstant =>
                        val restThisRow = {
                            if prioritized.tail.isEmpty then Leaf(firstRow.action, firstRow.pos)
                            else
                                var newGroup = GroupedTuples(
                                  group2.columnBindings,
                                  group2.activeColumns - i,
                                  List(
                                    SirParsedCase.GroupedTupleRow(
                                      firstRow.patterns,
                                      firstRow.action,
                                      firstRow.pos
                                    )
                                  )
                                )
                                val ifYes = buildGroupedTuplesDecisionTree(
                                  ctx,
                                  newGroup,
                                  scrutineeTp,
                                  firstRow.pos,
                                  0
                                )
                                val ifNo = buildGroupedTuplesDecisionTree(
                                  ctx,
                                  GroupedTuples(
                                    group2.columnBindings,
                                    group2.activeColumns - i,
                                    group2.rows.tail
                                  ),
                                  scrutineeTp,
                                  topLevelPos,
                                  0
                                )
                        }
                ???
            }

    }

    private def eliminateAlternativesInGroup(
        group: SirParsedCase.GroupedTuples,
        rowIndex: Int,
    ): SirParsedCase.GroupedTuples = {
        val alternatives = group.rows(row).patterns.flatMap { case (p, i) =>
            p match
                case pa @ SirParsedCase.Pattern.OrPattern(alts, pos) =>
                    Some(pa)
                case _ => None
        }
        val beforeAlternatives = group.rows.take(rowIndex)
        val afterAlternatives = group.rows.drop(rowIndex + 1)
        val row = group.rows(rowIndex)
        // make new rows with all possible alternatives
        val newRows = alternatives.flatMap { case (p, i) =>
            p.alts.map(alt =>
                SirParsedCase.GroupedTupleRow(
                  row.patterns.updated(i, alt),
                  row.action,
                  row.pos
                )
            )
        }
        val newGroup = SirParsedCase.GroupedTuples(
          group.columnBindings,
          group.activeColumns,
          beforeAlternatives ++ newRows ++ afterAlternatives
        )
        newGroup
    }

    private def eliminateTypeSelectorsInGroup(
        group: SirParsedCase.GroupedTuples,
        rowIndex: Int
    ): SirParsedCase.GroupedTuples = {
        val typeSelectors = group.rows(row).patterns.flatMap { case (p, i) =>
            p match
                case ts @ SirParsedCase.Pattern.TypeSelector(
                      tp,
                      optNameInfo,
                      innerPattern,
                      optGuard,
                      pos
                    ) =>
                    Some((ts, i))
                case _ => None
        }
        if typeSelectors.isEmpty then group
        else {
            val beforeTypeSelectors = group.rows.take(rowIndex)
            val afterTypeSelectors = group.rows.drop(rowIndex + 1)
            val row = group.rows(rowIndex)
            // make new rows with all possible unrollings of type selectors
            val newRows = typeSelectors.flatMap { case (ts, i) =>
                unrollTypeSelector(
                  ctx,
                  ts,
                  row.action,
                  ts.pos,
                  scrutineeTp,
                  None
                ).map(unrolledPattern =>
                    SirParsedCase.GroupedTupleRow(
                      row.patterns.updated(i, unrolledPattern),
                      row.action,
                      row.pos
                    )
                )
            }
            val newGroup = SirParsedCase.GroupedTuples(
              group.columnBindings,
              group.activeColumns,
              beforeTypeSelectors ++ newRows ++ afterTypeSelectors
            )
            newGroup
        }
    }

    private def prioritizePatterns(
        group: SirParsedCase.GroupedTuples,
        row: Int,
        patterns: IndexedSeq[(SirParsedCase.Pattern, Int)]
    ): IndexedSeq[(SirParsedCase.Pattern, Int)] = {
        patterns.sortBy { case (p, i) =>
            p match
                case SirParsedCase.Pattern.Constructor(_, _, _, _, _, _) => 0
                case SirParsedCase.Pattern.PrimitiveConstant(_, _, _, _) => 1
                case SirParsedCase.Pattern.TypeSelector(_, _, _, _, _)   => 2
                case SirParsedCase.Pattern.OrPattern(_, _)               => 3
                case _                                                   => 4
        }
    }

    private def collectConstructorsInGroup(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples,
        row: Int,
        col: Int
    ): List[(SirParsedCase.JoinedConstructorCase, SirParsedCase.GroupedTupleRow)] = {
        val (constructors, tail) = group.rows.partition { r =>
            r.patterns(col) match
                case SirParsedCase.Pattern.Constructor(_, _, _, _, _, _) => true
                case SirParsedCase.Pattern.Wildcard(_, _, _, _)          =>

        }
        val joined = constructors
            .groupBy { r =>
                r.patterns(col) match
                    case c: SirParsedCase.Pattern.Constructor => c
                    case _                                    => sys.error("AAA")
            }
            .map { case (p, rows) =>
                SirParsedCase.JoinedConstructorCase(p, rows)
            }
            .toList
        (joined, tail)
    }

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
        action: SirParsedCase.ActionRef,
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
                                                              ctx.freshName("_w"),
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
        action: SirParsedCase.ActionRef,
        pos: SrcPos,
    ): List[SirParsedCase] = alt.patterns.map(p => SirParsedCase(p, action, p.pos))

    private def constantToWidlcardWithGuard(
        ctx: PatternMatchingContext,
        constCase: SirParsedCase.Pattern.PrimitiveConstant,
        scrutineeTp: SIRType
    ): SirParsedCase.Pattern.Wildcard = {
        val nameInfo = constCase.optNameInfo.getOrElse {
            val genName = ctx.freshName("const")
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
        if cases.isEmpty then
            throw IllegalStateException("cases in makeJoinedConstructorCase should not be empty")
        val tuples = cases.map { c =>
            if c.pattern.optGuard.isDefined then
                throw IllegalStateException(
                  "cases in makeJoinedConstructorCase should not have guards"
                )
            c.pattern match {
                case constructor: SirParsedCase.Pattern.Constructor =>
                    if constructor.tp.constrDecl.name != constrTp.constrDecl.name then
                        throw IllegalStateException(
                          "cases in makeJoinedConstructorCase should have the same constructor"
                        )
                    SirParsedCase.GroupedTupleRow(
                      constructor.subcases,
                      c.actionRef,
                      c.pos
                    )
                case _ =>
                    throw IllegalStateException(
                      "cases in makeJoinedConstructorCase should be constructors"
                    )
            }
        }.toIndexedSeq

        SirParsedCase.JoinedConstructorCase(constrTp, tuples)
    }

}
