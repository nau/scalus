package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.{comparing, explore, Context}
import dotty.tools.dotc.core.NameKinds.{Scala2MethodNameKinds, SkolemName, UniqueNameKind}
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.{SourcePosition, SrcPos}
import scalus.SirCaseDecisionTree.Leaf
import scalus.SirCaseDecisionTree.SplitStrategy.DuplicateRows
import scalus.SirParsedCase.ActionRef.FailMatch
import scalus.SirParsedCase.{ActionRef, BindingNameInfo, GroupedTuples, Pattern}
import scalus.sir.*

import scala.collection.mutable.ListBuffer

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
    val topLevelPos: SrcPos,
    // should be set after parsing.
    var parsedActions: IndexedSeq[SirParsedAction] = IndexedSeq.empty,
    var parsedGuards: IndexedSeq[Option[SirParsedGuard]] = IndexedSeq.empty
) {

    private var tmpNameCounter: Int = 1

    private var _actionsUsageCount: Map[Int, Int] = Map.empty
    private var _guardsUsageCount: Map[Int, Int] = Map.empty

    def freshName(prefix: String = ""): String = {
        val localPrefix = prefix
        val name = s"${globalPrefix}${localPrefix}_$tmpNameCounter"
        tmpNameCounter += 1
        name
    }

    def actionsUsageCount: Map[Int, Int] = _actionsUsageCount

    def countActionUsage(actionIndex: Int): Int = {
        _actionsUsageCount.get(actionIndex) match
            case Some(c) =>
                _actionsUsageCount = _actionsUsageCount + (actionIndex -> (c + 1))
                c + 1
            case None =>
                _actionsUsageCount = _actionsUsageCount + (actionIndex -> 1)
                1
    }

    def guardsUsageCount: Map[Int, Int] = _guardsUsageCount

    def countGuardUsage(guardIndex: Int): Int = {
        _guardsUsageCount.get(guardIndex) match
            case Some(c) =>
                _guardsUsageCount = _guardsUsageCount + (guardIndex -> (c + 1))
                c + 1
            case None =>
                _guardsUsageCount = _guardsUsageCount + (guardIndex -> 1)
                1
    }

    def createLeaf(
        columnBinding: IndexedSeq[SirParsedCase.BindingNameInfo],
        row: SirParsedCase.GroupedTupleRow
    ): Leaf = {
        if columnBinding.size != row.patterns.size then
            throw IllegalStateException("columnBinding.size != row.patterns.size")
        row.actionRef match {
            case SirParsedCase.ActionRef.Origin(i) =>
                countActionUsage(i)
            case _ =>
        }
        val binding = row.patterns.zip(columnBinding).foldLeft(Map.empty[String, String]) {
            case (m, (p, cb)) =>
                p.optNameInfo match {
                    case None => m
                    case Some(nameInfo) =>
                        m + (nameInfo.name -> cb.name)
                }
        }
        Leaf(binding, row.actionRef, row.pos)
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
    guardRef: Option[Int],
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

        def collectNames: Map[String, SIRType]

    end Pattern

    object Pattern:

        case class TypeSelector(
            tp: SIRType,
            optNameInfo: Option[BindingNameInfo],
            innerPattern: Pattern,
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

            override def optNameInfo: Option[BindingNameInfo] = None

            override def collectNames: Map[String, SIRType] = Map.empty

        }

        class ErrorPattern private (val msg: String, val pos: SrcPos) extends Pattern {
            def withAlias(alias: BindingNameInfo): Pattern = this

            override def optNameInfo: Option[BindingNameInfo] = None

            override def collectNames: Map[String, SIRType] = Map.empty
        }

        object ErrorPattern {
            def apply(msg: String, pos: SrcPos)(using Context): ErrorPattern = {
                report.error(msg, pos)
                ErrorPattern(msg, pos)
            }

            def unapply(p: ErrorPattern): Option[(String, SrcPos)] = Some((p.msg, p.pos))

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
        guardRef: Option[Int],
        actionRef: ActionRef,
        pos: SrcPos
    )

    case class GroupedTuples(
        columnBindings: IndexedSeq[BindingNameInfo],
        activeColumns: Set[Int],
        rows: List[GroupedTupleRow]
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
        names: IndexedSeq[SirParsedCase.BindingNameInfo],
        next: SirCaseDecisionTree,
        pos: SrcPos
    )

    case class ConstantChoice(
        columnName: Option[String],
        byConstants: Map[SIR.Const, SirCaseDecisionTree],
        defaultBranch: SirCaseDecisionTree,
    ) extends SirCaseDecisionTree

    case class CheckGuard(
        bingingMap: Map[String, String],
        guard: Int,
        pos: SrcPos,
        nextTrue: SirCaseDecisionTree,
        nextFalse: SirCaseDecisionTree
    ) extends SirCaseDecisionTree

    /*
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
    )*/

    case class Leaf(
        binding: Map[String, String],
        action: SirParsedCase.ActionRef,
        pos: SrcPos
    ) extends SirCaseDecisionTree

    /** let wehave situation
      * ```
      * case 1, r1tail => action1
      * case *, r2tail => action2
      * case 1, r3tail => action3
      * ```
      *
      * we have to variant to choose: A) split rows futher only when next row is also have constant
      * check. After first wildcard enty we return to full check, because the cost of duplicatong
      * row can be more . then the cost of yet one constant check.
      *
      * B) the alternative will be transform this to .
      * ```
      *  case 1, r1tail => action1 .
      *  case 1, r2tail => action2 .
      *  case 1, r3tail => action3 .
      *  --------
      *  . in rest: case *, r2tail => action2 .
      * ```
      * as in original article.
      */
    enum SplitStrategy:
        case DuplicateChecks
        case DuplicateRows

    /** Whe we compile decision tree, we can embed action or guard in two ways:
      *   - inline (when we copy action each time)
      *   - byReference (when we at first create a let-binding for action, and then use it by name)
      */
    enum EmbeddingType:
        case Inline
        case ByReference

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

    def compileMatch(tree: Match, env: SIRCompiler.Env): AnnotatedSIR = {
        val bPrefix = s"_${tree.srcPos.startPos.source.name}_${tree.srcPos.line}_match_"
        val ctx = PatternMatchingContext(bPrefix, env, tree.srcPos)
        val parsedMatch = parseMatch(ctx, tree, env)
        val decisionTree = buildDecisionTree(ctx, parsedMatch, env)
        val sir = compileDecisionTree(ctx, decisionTree, parsedMatch)
        sir
    }

    /*
    private def compileBinding(env: SIRCompiler.Env, pat: Tree, tp: Type): SirBinding = {
        if env.debug then println(s"compileBinding: ${pat.show} ${tp.show}")
        pat match
            // this is case Constr(name @ _) or Constr(name)
            case Bind(name, id @ Ident(nme.WILDCARD)) =>
                SirBinding.Name(
                  name.show,
                  compiler.sirTypeInEnv(tp, pat.srcPos, env),
                  pat.srcPos.sourcePos
                )
            // this is case Constr(name @ Constr2(_))
            case Bind(name, body @ UnApply(fun, _, pats)) =>
                // pattern Symbol probaly incorrect here (need to be tps,  can be Any).  TODO: write test
                val typeSymbol = pat.tpe.widen.dealias.typeSymbol
                EmptyTree.srcPos
                SirBinding.CaseClass(
                  name.show,
                  typeSymbol,
                  pats.map(p => compileBinding(env, p, p.tpe)),
                  compiler.sirTypeInEnv(tp, pat.srcPos, env),
                  pat.srcPos.sourcePos
                )
            case Bind(name, body) =>
                SirBinding.Error(UnsupportedBinding(name.show, pat.srcPos))
            // this is case _ =>
            case Ident(nme.WILDCARD) =>
                SirBinding.Name(
                  bindingName.fresh().show,
                  compiler.sirTypeInEnv(tp, pat.srcPos, env),
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
                  compiler.sirTypeInEnv(tp, pat.srcPos, env),
                  pat.srcPos.sourcePos
                )
            case Literal(_) =>
                SirBinding.Error(LiteralPattern(pat.srcPos))
            case p =>
                println(s"Unsupported binding expression: tree= ${p}")
                println(s"Unsupported binding expression: tree.show= ${p.show}")
                SirBinding.Error(UnsupportedMatchExpression(p, p.srcPos))
    }


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
                parsePattern(ctx, p, p.sourcePos, tp)
            }
            .toIndexedSeq
        SirParsedCase.Pattern.Constructor(
          SIRType.CaseClass(constrDecl, typeArgs, optParent),
          typeParams,
          optBindingNameInfo,
          // constrSymbol,
          subpatterns,
          pos
        )
    }

    def parsePatternInOptBind(
        ctx: PatternMatchingContext,
        pat: Tree,
        optBindingNameInfo: Option[SirParsedCase.BindingNameInfo],
        pos: SourcePosition,
        sirScrutineeType: SIRType
    ): SirParsedCase.Pattern = {

        pat match
            case u @ UnApply(fun, _, _) =>
                parseConstructorPattern(
                  ctx,
                  u,
                  optBindingNameInfo,
                  pos,
                  sirScrutineeType
                )
            case Ident(nme.WILDCARD) =>
                SirParsedCase.Pattern.Wildcard(
                  sirScrutineeType,
                  optBindingNameInfo,
                  pos
                )
            case Literal(const) =>
                val sirExpr = compiler.compileExpr(ctx.env, pat)
                sirExpr match
                    case c: SIR.Const =>
                        SirParsedCase.Pattern.PrimitiveConstant(c, None, pat.sourcePos)
                    case _ =>
                        val err = GenericError("const pattern should be a constant", pat.srcPos)
                        compiler.error(
                          err,
                          SirParsedCase.Pattern
                              .Wildcard(sirScrutineeType, optBindingNameInfo, pos)
                        )
            case Typed(inner, tpt) =>
                val tptSirType = compiler.sirTypeInEnv(tpt.tpe, pos, ctx.env)
                if tptSirType ~=~ sirScrutineeType then {
                    parsePatternInOptBind(
                      ctx,
                      inner,
                      optBindingNameInfo,
                      inner.sourcePos,
                      tptSirType
                    )
                } else
                    val innerPattern = parsePatternInOptBind(
                      ctx,
                      inner,
                      optBindingNameInfo,
                      inner.sourcePos,
                      tptSirType
                    )
                    SirParsedCase.Pattern.TypeSelector(
                      tptSirType,
                      optBindingNameInfo,
                      innerPattern,
                      pos
                    )
            case Alternative(trees) =>
                val patterns = trees.map { p =>
                    parsePatternInOptBind(
                      ctx,
                      p,
                      optBindingNameInfo,
                      p.sourcePos,
                      sirScrutineeType
                    )
                }
                SirParsedCase.Pattern.OrPattern(patterns.toList, pat.sourcePos)
            case _ =>
                if pat.symbol.is(Flags.Case) then {
                    val sirPatTp = compiler.sirTypeInEnv(pat.tpe, pat.srcPos, ctx.env)
                    val constrDecl = compiler.makeConstrDecl(ctx.env, pat.srcPos, pat.symbol)
                    SIRType.collectSumCaseClass(sirPatTp) match {
                        case Some((typeVars, sumCaseClass)) =>
                            val sirConstrTp = sumCaseClass.decl.constrType(constrDecl.name)
                            SIRType.collectProdCaseClass(sirConstrTp) match
                                case Some((freeTypeVars, constrCaseClass)) =>
                                    SirParsedCase.Pattern.Constructor(
                                      constrCaseClass,
                                      freeTypeVars,
                                      optBindingNameInfo,
                                      // pat.symbol,
                                      IndexedSeq.empty,
                                      pos
                                    )
                                case None =>
                                    // Impossible, because we just found it as constructor of sum type
                                    SirParsedCase.Pattern.ErrorPattern(
                                      s"Constructor type is not a product type: $sirConstrTp",
                                      pat.srcPos
                                    )
                        case None =>
                            // constructor without parent
                            SIRType.collectProdCaseClass(sirPatTp) match
                                case Some((freeTypeVars, constrCaseClass)) =>
                                    SirParsedCase.Pattern.Constructor(
                                      constrCaseClass,
                                      freeTypeVars,
                                      optBindingNameInfo,
                                      // pat.symbol,
                                      IndexedSeq.empty,
                                      pos
                                    )
                                case None =>
                                    SirParsedCase.Pattern.ErrorPattern(
                                      s"Strange pattern: type is not a aum or product type: ${sirPatTp.show}",
                                      pat.srcPos
                                    )
                    }
                } else
                    println(s"Unsupported pattern: " + pat.show)
                    println(s"tree: $pat")
                    compiler.error(
                      UnsupportedMatchExpression(pat, pat.srcPos),
                      SirParsedCase.Pattern
                          .Wildcard(sirScrutineeType, optBindingNameInfo, pos)
                    )

    }

    def parsePattern(
        ctx: PatternMatchingContext,
        pat: Tree,
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
                  pat.srcPos.sourcePos,
                  sirScrutineeType
                )
            case _ =>
                parsePatternInOptBind(
                  ctx,
                  pat,
                  None,
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
                val sirPat = parsePattern(ctx, pat, c.srcPos, scrutineeType)
                val sirCase = SirParsedCase(
                  sirPat,
                  optGuard.map(_ => i),
                  SirParsedCase.ActionRef.Origin(i),
                  c.srcPos.sourcePos
                )
                val patNames = sirPat.collectNames
                val nEnv = ctx.env.copy(vars = ctx.env.vars ++ patNames)
                val optCompiledGuard = optGuard.map { g =>
                    val sirG = compiler.compileExpr(nEnv, g)
                    val (usedNames, unusedNames) =
                        SIR.partitionUsedFreeVarsFrom(sirG, patNames.keys.toSet)
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
        val scrutineeSirType = compiler.sirTypeInEnv(scrutineeType, tree.srcPos, env)
        val sirCasesGuardActions = cases.zipWithIndex.map { case (cs, i) =>
            parseSIRCase(ctx, cs, i, scrutineeSirType)
        }.toIndexedSeq
        val sirCases = sirCasesGuardActions.map(_._1)
        val actions = sirCasesGuardActions.map(_._3)
        ctx.parsedActions = actions
        val optGuards = sirCasesGuardActions.map(_._2)
        ctx.parsedGuards = optGuards
        val matchSirType = compiler.sirTypeInEnv(tree.tpe.dealias.widen, tree.srcPos, env)
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
            SIR.partitionUsedFreeVarsFrom(sirRhs, patNames.keys.toSet)
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
            SirParsedCase.GroupedTupleRow(
              IndexedSeq(c.pattern),
              c.guardRef,
              SirParsedCase.ActionRef.Origin(i),
              c.pos
            )
        }

        val firstOptNameInfo =
            parsedMatch.cases.find(_.pattern.optNameInfo.isDefined).flatMap(_.pattern.optNameInfo)

        val globalBindName =
            if tupleRows.size == 1 then
                firstOptNameInfo.getOrElse(
                  BindingNameInfo(
                    ctx.freshName("matchScrutinee"),
                    None,
                    parsedMatch.scrutineeTp,
                    Symbols.NoSymbol
                  )
                )
            else if firstOptNameInfo.isDefined then {
                // we should define fresh name, because other branches can reuse firstOptNameInfo name in other meaning.
                val withScalaName = parsedMatch.cases.find { x =>
                    x.pattern.optNameInfo match {
                        case Some(nameInfo) => nameInfo.scalaName.isDefined
                        case None           => false
                    }
                }
                SirParsedCase.BindingNameInfo(
                  ctx.freshName(firstOptNameInfo.get.name),
                  withScalaName.flatMap(x => x.pattern.optNameInfo.flatMap(_.scalaName)),
                  parsedMatch.scrutineeTp,
                  Symbols.NoSymbol
                )
            } else {
                SirParsedCase.BindingNameInfo(
                  ctx.freshName("matchScrutinee"),
                  None,
                  parsedMatch.scrutineeTp,
                  Symbols.NoSymbol
                )
            }

        val groupedTuples = SirParsedCase.GroupedTuples(
          IndexedSeq(globalBindName),
          Set(0),
          tupleRows.toList
        )

        buildGroupedTuplesDecisionTree(ctx, groupedTuples)

    }

    /*
    def buildConstructorDecisionTree(
        ctx: PatternMatchingContext,
        constructorCases: SirParsedCase.JoinedConstructorCase,
        scrutineeTp: SIRType,
        topLevelPos: SrcPos
    ): SirCaseDecisionTree.ConstructorEntry = {

        val nParams = constructorCases.tp.constrDecl.params.length
        if nParams == 0 then
            val firstCase = constructorCases.tuples.head
            val leaf = ctx.createLeaf( firstCase.actionRef, firstCase.pos)
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
                    BindingNameInfo(
                      ctx.freshName(b.name),
                      None,
                      b.tp,
                      Symbols.NoSymbol,
                      Set.empty
                    )
                )
                .toIndexedSeq

            val group = SirParsedCase.GroupedTuples(
              columnBinding,
              (0 until nParams).toSet,
              constructorCases.tuples.toList
            )
            val nextTree = buildGroupedTuplesDecisionTree(ctx, group)
            SirCaseDecisionTree.ConstructorEntry(
              constructorCases.tp,
              columnBinding,
              nextTree,
              topLevelPos
            )
    }
    
     */

    def buildGroupedTuplesDecisionTree(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples
    ): SirCaseDecisionTree = {
        val group1 = eliminateAlternativesInGroup(group)
        val group2 = eliminateTypeSelectorsInGroup(group1)
        if group2.rows.isEmpty then
            SirCaseDecisionTree.Leaf(Map.empty, SirParsedCase.ActionRef.FailMatch, ctx.topLevelPos)
        else
            val firstRow = group2.rows.head
            val nonWildcards = firstRow.patterns.zipWithIndex.filter { case (p, i) =>
                group2.activeColumns.contains(i) && {
                    p match {
                        case SirParsedCase.Pattern.Wildcard(_, _, _) => false
                        case _                                       => true
                    }
                }
            }
            if nonWildcards.isEmpty then {
                if firstRow.guardRef.isDefined then
                    buildSpecializedGuard(
                      ctx,
                      firstRow,
                      group2
                    )
                else
                    val retval = ctx.createLeaf(
                      group.columnBindings,
                      firstRow
                    )
                    if group.rows.length > 1 then {
                        group.rows.tail.foreach(r =>
                            if r.actionRef != FailMatch then
                                report.warning("Unreachable case in pattern matching", r.pos)
                        )
                    }
                    retval
            } else {
                val prioritized = prioritizePatterns(group2, firstRow, patterns = nonWildcards)
                val (p, i) = prioritized.head
                p match
                    case constr: SirParsedCase.Pattern.Constructor =>
                        buildSpecializedConstr(ctx, group2, firstRow, i)
                    case const: SirParsedCase.Pattern.PrimitiveConstant =>
                        buildSpecializedConst(ctx, group2, i)
                    case _ =>
                        compiler.error(
                          GenericError(s"Impossible pattern in specialization: ${p}", p.pos),
                          SirCaseDecisionTree.Leaf(
                            Map.empty,
                            SirParsedCase.ActionRef.FailMatch,
                            p.pos
                          )
                        )
            }
    }

    private def buildSpecializedGuard(
        ctx: PatternMatchingContext,
        currentRow: SirParsedCase.GroupedTupleRow,
        group: SirParsedCase.GroupedTuples,
    ): SirCaseDecisionTree = {
        val guardRef = currentRow.guardRef.get
        val nextTree = group.rows match {
            case head :: tail =>
                buildGroupedTuplesDecisionTree(ctx, group.copy(rows = tail))
            case Nil =>
                SirCaseDecisionTree.Leaf(
                  Map.empty,
                  SirParsedCase.ActionRef.FailMatch,
                  ctx.topLevelPos
                )
        }
        val actionTree =
            ctx.createLeaf(group.columnBindings, currentRow)
        ctx.countGuardUsage(guardRef)
        SirCaseDecisionTree.CheckGuard(
          actionTree.binding,
          guardRef,
          currentRow.pos,
          actionTree,
          nextTree
        )
    }

    private def buildSpecializedConst(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples,
        col: Int,
    ): SirCaseDecisionTree = {

        def checkConstantPattern(
            p: SirParsedCase.Pattern
        ): Option[(SirParsedCase.Pattern.PrimitiveConstant, SIR.Const, SIR.Const)] = {
            p match
                case c: SirParsedCase.Pattern.PrimitiveConstant =>
                    Some((c, c.value, c.value))
                case _ => None
        }

        val (withConstants, splittedRest) = collectSpecialized(
          group.rows,
          col,
          checkConstantPattern,
          SirCaseDecisionTree.SplitStrategy.DuplicateChecks
        )

        val constEntries = withConstants.map { case (const, (_, rows)) =>
            val newGroup = SirParsedCase.GroupedTuples(
              group.columnBindings,
              group.activeColumns - col,
              rows
            )
            val subtree = buildGroupedTuplesDecisionTree(ctx, newGroup)
            (const, subtree)
        }.toMap
        val nextSubtree =
            if splittedRest.isEmpty then
                SirCaseDecisionTree.Leaf(
                  Map.empty,
                  SirParsedCase.ActionRef.FailMatch,
                  ctx.topLevelPos
                )
            else {
                val newGroup = SirParsedCase.GroupedTuples(
                  group.columnBindings,
                  group.activeColumns - col,
                  splittedRest
                )
                buildGroupedTuplesDecisionTree(ctx, newGroup)
            }
        var columnName = group.columnBindings(col).name
        SirCaseDecisionTree.ConstantChoice(Some(columnName), constEntries, nextSubtree)
    }

    /** Collect rows with specialized pattern in the given column according to checkPattern function
      * and splitStrategy. Return - Map[Key, (Value, List[Rows with this key])] and List of rest
      * rows (i,e. default specialized) Key and Value are extracted from pattern by checkPattern
      * function.
      */
    private def collectSpecialized[P <: SirParsedCase.Pattern, K, V](
        rows: List[SirParsedCase.GroupedTupleRow],
        colIndex: Int,
        checkPattern: SirParsedCase.Pattern => Option[(P, K, V)],
        splitStrategy: SirCaseDecisionTree.SplitStrategy
    ): (Map[K, (V, List[SirParsedCase.GroupedTupleRow])], List[SirParsedCase.GroupedTupleRow]) = {
        var groupedRows: Map[K, (V, ListBuffer[SirParsedCase.GroupedTupleRow])] = Map.empty
        val restPrefix: ListBuffer[SirParsedCase.GroupedTupleRow] = ListBuffer.empty
        var cursor = rows
        var done = false
        while cursor.nonEmpty && !done do {
            val r = cursor.head
            checkPattern(r.patterns(colIndex)) match
                case Some((pattern, key, value)) =>
                    groupedRows.get(key) match
                        case Some((v, buf)) =>
                            buf.append(r)
                        case None =>
                            val buf = ListBuffer.empty[SirParsedCase.GroupedTupleRow]
                            buf.append(r)
                            groupedRows = groupedRows + (key -> (value, buf))
                    if splitStrategy == SirCaseDecisionTree.SplitStrategy.DuplicateRows then
                        if restPrefix.nonEmpty then
                            groupedRows.foreach:
                                case (k, (v, rows)) => rows.appendAll(restPrefix)
                            restPrefix.clear()
                case None =>
                    if splitStrategy == SirCaseDecisionTree.SplitStrategy.DuplicateRows then
                        // we should duplicate this row in all groups, and continue
                        restPrefix += r
                    else
                        // we should stop collecting, and put all next rows to rest
                        done = true
            if !done then cursor = cursor.tail
        }
        val grouped = groupedRows.map { case (k, (v, buf)) =>
            (k, (v, buf.toList))
        }
        splitStrategy match {
            case SirCaseDecisionTree.SplitStrategy.DuplicateRows =>
                (grouped, restPrefix.toList)
            case SirCaseDecisionTree.SplitStrategy.DuplicateChecks =>
                (grouped, cursor)
        }

    }

    private def buildSpecializedConstr(
        ctx: PatternMatchingContext,
        group: SirParsedCase.GroupedTuples,
        row: SirParsedCase.GroupedTupleRow,
        colIndex: Int
    ): SirCaseDecisionTree = {

        def checkPattern(
            p: SirParsedCase.Pattern
        ): Option[
          (SirParsedCase.Pattern.Constructor, String, (SIRType.CaseClass, List[SIRType.TypeVar]))
        ] = {
            p match
                case c: SirParsedCase.Pattern.Constructor =>
                    val v = (c.tp, c.freeTypeParams)
                    Some((c, c.tp.constrDecl.name, v))
                case _ => None
        }

        val (constructorCases, tail) = this.collectSpecialized(
          group.rows,
          colIndex,
          checkPattern,
          SirCaseDecisionTree.SplitStrategy.DuplicateRows
        )

        val constructorEntries = constructorCases.map {
            case (constrName, ((sirCaseClass, freeTypeParams), rows)) =>
                val newGroup = insertConstructorPatternsIntoGroup(
                  ctx,
                  sirCaseClass,
                  freeTypeParams,
                  group,
                  colIndex,
                  rows
                )
                val subtree = buildGroupedTuplesDecisionTree(ctx, newGroup)
                val constrPos = rows match
                    case head :: _ => head.pos
                    case Nil       => ctx.topLevelPos
                val entry = SirCaseDecisionTree.ConstructorEntry(
                  sirCaseClass,
                  newGroup.columnBindings,
                  subtree,
                  constrPos
                )
                (constrName, entry)
        }.toMap
        val optNextSubtree =
            if tail.isEmpty then None
            else
                val newGroup = SirParsedCase.GroupedTuples(
                  group.columnBindings,
                  group.activeColumns - colIndex,
                  tail
                )
                val nextSubtree = buildGroupedTuplesDecisionTree(ctx, newGroup)
                Some(nextSubtree)
        val columnName = group.columnBindings(colIndex).name
        SirCaseDecisionTree.ConstructorsChoice(Some(columnName), constructorEntries, optNextSubtree)
    }

    private def insertConstructorPatternsIntoGroup(
        ctx: PatternMatchingContext,
        cc: SIRType.CaseClass,
        ccFreeTypeParams: List[SIRType.TypeVar],
        prevGroup: SirParsedCase.GroupedTuples,
        colindex: Int,
        rows: List[SirParsedCase.GroupedTupleRow]
    ): SirParsedCase.GroupedTuples = {
        val rowsMoreThanOne = rows match {
            case Nil         => false
            case head :: Nil => false
            case _           => true
        }
        val nextIndex = prevGroup.columnBindings.length
        val constructorBindings = cc.constrDecl.params.map(b =>
            BindingNameInfo(
              ctx.freshName(b.name),
              None,
              b.tp,
              Symbols.NoSymbol,
              Set.empty
            )
        )
        val newColumnBindings = prevGroup.columnBindings ++ constructorBindings
        val newActiveColumns =
            (prevGroup.activeColumns - colindex) ++ (nextIndex until nextIndex + constructorBindings.length)
        val newRows = rows.map { r =>
            val constructorPattern = r.patterns(colindex) match
                case c: SirParsedCase.Pattern.Constructor => c
                case w: SirParsedCase.Pattern.Wildcard =>
                    createWildcardConstructorPattern(cc, ccFreeTypeParams, w.pos)
                case _ =>
                    throw IllegalStateException(
                      "Only constructor or wildcard patterns are expected here"
                    )
            val newPatterns = r.patterns ++ constructorPattern.subcases
            SirParsedCase.GroupedTupleRow(
              newPatterns,
              r.guardRef,
              r.actionRef,
              r.pos
            )
        }
        GroupedTuples(
          newColumnBindings,
          newActiveColumns,
          newRows
        )
    }

    private def createWildcardConstructorPattern(
        cc: SIRType.CaseClass,
        ccFreeTypeParams: List[SIRType.TypeVar],
        pos: SrcPos
    ): SirParsedCase.Pattern.Constructor = {
        val subcases = cc.constrDecl.params
            .map(b =>
                val tp = SIRType
                    .substitute(b.tp, cc.constrDecl.typeParams.zip(cc.typeArgs).toMap, Map.empty)
                SirParsedCase.Pattern.Wildcard(tp, None, pos)
            )
            .toIndexedSeq
        SirParsedCase.Pattern.Constructor(cc, ccFreeTypeParams, None, subcases, pos)
    }

    private def eliminateAlternativesInGroup(
        group: SirParsedCase.GroupedTuples,
    ): SirParsedCase.GroupedTuples = {
        group.copy(rows =
            group.rows.flatMap(r => eliminateAlternativesInRow(r, group.activeColumns))
        )
    }

    private def eliminateAlternativesInRow(
        row: SirParsedCase.GroupedTupleRow,
        columnsToExpand: Set[Int]
    ): List[SirParsedCase.GroupedTupleRow] = {
        val alternatives = row.patterns.zipWithIndex.flatMap { case (p, i) =>
            if columnsToExpand.contains(i) then
                p match
                    case pa @ SirParsedCase.Pattern.OrPattern(alts, pos) => Some((pa, i))
                    case _                                               => None
            else None
        }
        val newRows = alternatives.flatMap { case (pa, i) =>
            pa.patterns.map(alt =>
                SirParsedCase.GroupedTupleRow(
                  row.patterns.updated(i, alt),
                  row.guardRef,
                  row.actionRef,
                  row.pos
                )
            )
        }
        newRows.toList
    }

    private def eliminateTypeSelectorsInGroup(
        group: SirParsedCase.GroupedTuples
    ): SirParsedCase.GroupedTuples = {
        val newRows =
            group.rows.flatMap(row => eliminateTypeSelectorsInRaw(group.columnBindings, row))
        group.copy(rows = newRows)
    }

    private def eliminateTypeSelectorsInRaw(
        columnBindings: IndexedSeq[BindingNameInfo],
        row: SirParsedCase.GroupedTupleRow
    ) = {
        val typeSelectors = row.patterns.zipWithIndex.flatMap { case (p, i) =>
            p match
                case ts @ SirParsedCase.Pattern.TypeSelector(
                      tp,
                      optNameInfo,
                      innerPattern,
                      pos
                    ) =>
                    Some((ts, i))
                case _ => None
        }
        if typeSelectors.isEmpty then List(row)
        else {
            // make new rows with all possible unrollings of type selectors
            val newRows = typeSelectors.map { case (ts, i) =>
                val scrutineeTp = columnBindings(i).tp
                val unrolledPattern = unrollTypeSelector(
                  ts,
                  ts.pos,
                  scrutineeTp
                )
                SirParsedCase.GroupedTupleRow(
                  row.patterns.updated(i, unrolledPattern),
                  row.guardRef,
                  row.actionRef,
                  row.pos
                )
            }
            newRows
        }
    }

    private def prioritizePatterns(
        group: SirParsedCase.GroupedTuples,
        row: SirParsedCase.GroupedTupleRow,
        patterns: IndexedSeq[(SirParsedCase.Pattern, Int)]
    ): IndexedSeq[(SirParsedCase.Pattern, Int)] = {
        patterns.sortBy { case (p, i) =>
            p match
                case SirParsedCase.Pattern.Constructor(_, _, _, _, _) => 0
                case SirParsedCase.Pattern.PrimitiveConstant(_, _, _) => 1
                case SirParsedCase.Pattern.TypeSelector(_, _, _, _)   => 2
                case SirParsedCase.Pattern.OrPattern(_, _)            => 3
                case _                                                => 4
        }
    }

    private def unrollTypeSelector(
        ts: SirParsedCase.Pattern.TypeSelector,
        pos: SrcPos,
        scrutineeTp: SIRType
    ): SirParsedCase.Pattern = {

        def injectInnerPattern(
            top: SirParsedCase.Pattern,
            ts: SirParsedCase.Pattern.TypeSelector
        ): SirParsedCase.Pattern = {
            top match
                case SirParsedCase.Pattern.Constructor(
                      tp,
                      typeParams,
                      optNameInfo,
                      subcases,
                      pos
                    ) =>
                    if SIRType.isSynteticNarrowConstrDeclName(tp.constrDecl.name) then {
                        injectInnerPattern(subcases.head, ts)
                    } else {
                        SirParsedCase.Pattern.ErrorPattern(
                          s"Cannot inject inner pattern of typeSelector ${ts.tp.show} into $top",
                          pos
                        )
                    }
                case SirParsedCase.Pattern.Wildcard(tp, optNameInfo, pos) =>
                    SirParsedCase.Pattern.Wildcard(
                      tp,
                      ts.optNameInfo.orElse(optNameInfo),
                      pos
                    )
                case _ =>
                    SirParsedCase.Pattern.ErrorPattern(
                      s"Type selector inner pattern can be only constructor or wildcard, but got ${top}",
                      top.pos
                    )
        }

        ts.tp match
            case p: SIRType.Primitive =>
                if p == scrutineeTp then ts.innerPattern
                else {
                    report.error(
                      s"Type selector pattern  ${ts.tp.show} does not match scrutinee type ${scrutineeTp.show}",
                      pos
                    )
                    SirParsedCase.Pattern.Wildcard(scrutineeTp, ts.optNameInfo, pos)
                }
            case SIRType.FreeUnificator =>
                ts.innerPattern
            case _ =>
                SIRType.collectProdCaseClass(ts.tp) match {
                    case Some((patTypeVars, patCaseClass)) =>
                        SIRType.collectProdCaseClass(scrutineeTp) match
                            case Some((scrutineeTypeVars, scrutineeCaseClass)) =>
                                if patCaseClass.constrDecl.name == scrutineeCaseClass.constrDecl.name
                                then ts.innerPattern
                                else
                                    SirParsedCase.Pattern.ErrorPattern(
                                      s"Type selector pattern  ${ts.tp.show} does not match scrutinee type ${scrutineeTp.show}",
                                      pos
                                    )
                            case None =>
                                SIRType.collectSumCaseClass(scrutineeTp) match
                                    case Some((srcurineeTypeVars, scrutineeSumClass)) =>
                                        if scrutineeSumClass.decl.constructors.exists(
                                              _.name == patCaseClass.constrDecl.name
                                            )
                                        then
                                            val constrPattern = createWildcardConstructorPattern(
                                              patCaseClass,
                                              patTypeVars,
                                              pos
                                            )
                                            ts.innerPattern match {
                                                case SirParsedCase.Pattern.Wildcard(_, _, _) =>
                                                case _ =>
                                                    report.error(
                                                      "type selector inner pattern must be wildcard",
                                                      pos
                                                    )
                                            }
                                            constrPattern
                                                .withOptAlias(ts.optNameInfo)
                                                .withOptAlias(ts.innerPattern.optNameInfo)
                                        else
                                            patCaseClass.parent match
                                                case Some(parentPatCaseClass) =>
                                                    val top = unrollTypeSelector(
                                                      ts.copy(
                                                        tp = parentPatCaseClass,
                                                        optNameInfo = None
                                                      ),
                                                      pos,
                                                      scrutineeTp
                                                    )
                                                    injectInnerPattern(top, ts)
                                                case None =>
                                                    SirParsedCase.Pattern.ErrorPattern(
                                                      s"Type selector pattern  ${ts.tp.show} does not match scrutinee type ${scrutineeTp.show}",
                                                      pos
                                                    )
                                    case None =>
                                        SirParsedCase.Pattern.ErrorPattern(
                                          s"scrutinee type ${scrutineeTp.show} is not a sum case class and can't be mathed with type selector pattern ${ts.tp.show}",
                                          pos
                                        )
                    case None =>
                        // SIRtype is not prod,
                        SIRType.collectSumCaseClass(ts.tp) match {
                            case Some(typeParams, patSumClass) =>
                                SIRType.collectSumCaseClass(scrutineeTp) match
                                    case Some((scrutineeTypeVars, scrutineeSumClass)) =>
                                        if patSumClass.decl.name == scrutineeSumClass.decl.name
                                        then ts.innerPattern.withOptAlias(ts.optNameInfo)
                                        else {
                                            val constrName = SIRType.syntheticNarrowConstrDeclName(
                                              patSumClass.decl.name
                                            )
                                            scrutineeSumClass.decl.constructors.find(
                                              _.name == constrName
                                            ) match {
                                                case Some(narrowConstr) =>
                                                    val narrowConstrTp =
                                                        scrutineeSumClass.decl.constrType(
                                                          narrowConstr.name
                                                        )
                                                    SIRType.collectProdCaseClass(
                                                      narrowConstrTp
                                                    ) match {
                                                        case Some(
                                                              (typeParams, narrowConstrCaseClass)
                                                            ) =>
                                                            val constrPattern =
                                                                createWildcardConstructorPattern(
                                                                  narrowConstrCaseClass,
                                                                  typeParams,
                                                                  pos
                                                                )
                                                            injectInnerPattern(
                                                              constrPattern,
                                                              ts
                                                            )
                                                        case None =>
                                                            SirParsedCase.Pattern.ErrorPattern(
                                                              s"Type selector pattern  ${ts.tp.show} is not a product type",
                                                              pos
                                                            )
                                                    }

                                                case None =>
                                                    // in principle, we can find matching constructors navigating from the scrutineeSumClass don,
                                                    //  but multilevel selaed hoerarchies is not officially supported.
                                                    SirParsedCase.Pattern.ErrorPattern(
                                                      s"Type selector pattern  ${ts.tp.show} does not match scrutinee type ${scrutineeTp.show}",
                                                      pos
                                                    )
                                            }
                                        }
                                    case None =>
                                        SirParsedCase.Pattern.ErrorPattern(
                                          s"scrutinee type ${scrutineeTp.show} is not a sum case class and can't be mathed with type selector pattern ${ts.tp.show}",
                                          pos
                                        )
                            case None =>
                                SirParsedCase.Pattern.ErrorPattern(
                                  s"Type selector pattern  ${ts.tp.show} is not supported: shoule be prod or sum or primitive type",
                                  pos
                                )
                        }

                }

    }

    private def compileDecisionTree(
        ctx: PatternMatchingContext,
        tree: SirCaseDecisionTree,
        parsedMatch: SirParsedMatch
    ): AnnotatedSIR = {
        // now we build SIR in form:
        // Let (action = compile(action)
        //  in ......
        //   in Let( = compile(action2)
        val actionsRecords = ctx.parsedActions.zipWithIndex.map((a, i) =>
            val count = ctx.actionsUsageCount.getOrElse(i, 0)
            val embedding =
                if count <= 1 then SirCaseDecisionTree.EmbeddingType.Inline
                else
                    val actionSize = SIR.size(a.sir)
                    if actionSize * i <= 3 * a.bindedVariables.size + 4 then
                        // ApplyN(LamN(x1...xn, action(x1,..xn)) b1...bn),
                        SirCaseDecisionTree.EmbeddingType.Inline
                    else SirCaseDecisionTree.EmbeddingType.ByReference
            val name = ctx.freshName(s"caseAction$i")
            (a.sir, embedding, name)
        )
        val guardsRecords = ctx.parsedGuards.zipWithIndex.map { (og, i) =>
            val count = ctx.guardsUsageCount.getOrElse(i, 0)
            val embedding = og match {
                case None => SirCaseDecisionTree.EmbeddingType.Inline
                case Some(g) =>
                    if count <= 1 then SirCaseDecisionTree.EmbeddingType.Inline
                    else
                        val guardSize = SIR.size(g.sir)
                        if guardSize * i <= 3 * g.bindedVariables.size + 4 then
                            // ApplyN(LamN(x1...xn, guard(x1,..xn)) b1...bn),
                            SirCaseDecisionTree.EmbeddingType.Inline
                        else SirCaseDecisionTree.EmbeddingType.ByReference
            }
            og.map(g => (g.sir, embedding, ctx.freshName(s"caseGuard${i}_")))
        }
        val decisions = compileDecisions(ctx, tree, parsedMatch, actionsRecords, guardsRecords)
        val sir = addActionsAndGuardsLet(ctx, decisions, actionsRecords, guardsRecords)
        sir
    }

    private def compileDecisions(
        ctx: PatternMatchingContext,
        tree: SirCaseDecisionTree,
        parsedMatch: SirParsedMatch,
        actions: IndexedSeq[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)],
        guards: IndexedSeq[Option[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)]]
    ): AnnotatedSIR = {
        tree match {
            case Leaf(bindingMap, actionRef, pos) =>
                val trueConst = SIR.Const.bool(true, AnnotationsDecl.empty)
                actionRef match {
                    case SirParsedCase.ActionRef.FailMatch =>
                        val anns = AnnotationsDecl
                            .fromSrcPos(pos) + ("sir.FailMatch" -> trueConst)
                        SIR.Error("Match failure", anns)
                    case SirParsedCase.ActionRef.Origin(index) =>
                        val action = parsedMatch.compiledActions(index)
                        val (sir, embedding, name) = actions(index)
                        val applied = embedding match {
                            case SirCaseDecisionTree.EmbeddingType.Inline =>
                                val renamed = SIR.renameFreeVars(
                                  sir,
                                  bindingMap.filter((k, v) => action.bindedVariables.contains(k))
                                )
                                renamed
                            case SirCaseDecisionTree.EmbeddingType.ByReference =>
                                generateActionRefApply(
                                  name,
                                  action,
                                  bindingMap,
                                  pos
                                )
                        }
                }
        }
        ???
    }

    private def addActionsAndGuardsLet(
        ctx: PatternMatchingContext,
        decisions: AnnotatedSIR,
        actions: IndexedSeq[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)],
        guards: IndexedSeq[Option[(AnnotatedSIR, SirCaseDecisionTree.EmbeddingType, String)]]
    ): AnnotatedSIR = {
        ???
    }

    /** generatee ''' Apply(....(Apply(name,b1), .. bName) '''
      */
    private def generateActionRefApply(
        name: String,
        action: SirParsedAction,
        bingingMap: Map[String, String],
        pos: SrcPos
    ): AnnotatedSIR = {
        if action.bindedVariables.isEmpty then
            val actionVar = SIR.Var(
              name,
              SIRType.Fun(SIRType.Unit, action.sir.tp),
              AnnotationsDecl.fromSrcPos(pos)
            )
            SIR.Apply(
              actionVar,
              SIR.Const.unit(AnnotationsDecl.fromSrcPos(pos)),
              action.sir.tp,
              AnnotationsDecl.fromSrcPos(pos)
            )
        else
            val actionType = action.bindedVariables.foldRight(action.sir.tp) { (b, acc) =>
                SIRType.Fun(b.tp, acc)
            }
            val actionVar = SIR.Var(name, actionType, AnnotationsDecl.fromSrcPos(pos))
            val applySir = action.bindedVariables.foldLeft(actionVar: AnnotatedSIR) { (acc, b) =>
                val bName = bingingMap.getOrElse(
                  b.name,
                  throw IllegalStateException(s"Binding ${b.name} not found")
                )
                val nextTp = acc.tp match
                    case SIRType.Fun(_, retTp) => retTp
                    case _ =>
                        throw IllegalStateException(
                          s"Expected function type in action reference application, but got ${acc.tp.show}"
                        )
                val bVar = SIR.Var(bName, b.tp, AnnotationsDecl.fromSrcPos(pos))
                SIR.Apply(acc, bVar, nextTp, AnnotationsDecl.fromSrcPos(pos))
            }
            applySir
    }

}
