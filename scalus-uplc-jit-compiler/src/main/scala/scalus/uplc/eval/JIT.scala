package scalus.uplc.eval

import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, Builtins, ByteString, Data}
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.*
import scalus.uplc.eval.ExBudgetCategory.{Startup, Step}

import scala.quoted.*

/** Just-In-Time compiler for UPLC (Untyped Plutus Core) terms.
  *
  * This object provides functionality to compile UPLC terms into optimized JVM bytecode at runtime
  * using Scala 3's staging capabilities. The JIT compilation can significantly improve execution
  * performance for repeatedly evaluated UPLC programs by eliminating the interpretation overhead.
  *
  * The JIT compiler transforms UPLC terms into native Scala functions that can be executed directly
  * on the JVM, while maintaining compatibility with the Plutus VM evaluation semantics including
  * budget tracking and logging.
  *
  * @note
  *   This is an experimental feature that requires scala3-staging and scala3-compiler dependencies.
  */
object JIT {
    private given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    private given ByteStringToExpr: ToExpr[ByteString] with {
        def apply(x: ByteString)(using Quotes): Expr[ByteString] =
            '{ ByteString.fromArray(${ Expr(x.bytes) }) }
    }

    private given DataToExpr: ToExpr[Data] with {
        def apply(x: Data)(using Quotes): Expr[Data] = x match
            case Data.Constr(tag, args) =>
                val tagExpr = Expr(tag)
                val argsExpr = Expr.ofList(args.map(apply))
                '{ Data.Constr($tagExpr, $argsExpr) }
            case Data.List(value) =>
                val valueExpr = Expr.ofList(value.map(apply))
                '{ Data.List($valueExpr) }
            case Data.Map(values) =>
                val argsListOfExprTuple = values.map { case (k, v) =>
                    Expr.ofTuple(apply(k), apply(v))
                }
                val argsExpr = Expr.ofList(argsListOfExprTuple)
                '{ Data.Map($argsExpr) }
            case Data.I(value) => '{ Data.I(${ Expr(value) }) }
            case Data.B(value) => '{ Data.B(${ Expr(value) }) }
    }

    private def constantToExpr(const: Constant)(using Quotes): Expr[Any] = {
        const match
            case Constant.Integer(value)        => Expr(value)
            case Constant.ByteString(value)     => Expr(value)
            case Constant.String(value)         => Expr(value)
            case Constant.Unit                  => '{ () }
            case Constant.Bool(value)           => Expr(value)
            case Constant.Data(value)           => Expr(value)
            case Constant.List(elemType, value) => Expr.ofList(value.map(constantToExpr))
            case Constant.Pair(a, b) => Expr.ofTuple(constantToExpr(a), constantToExpr(b))
            case Constant.BLS12_381_G1_Element(value) =>
                '{ BLS12_381_G1_Element(${ Expr(value.toCompressedByteString) }) }
            case Constant.BLS12_381_G2_Element(value) =>
                '{ BLS12_381_G2_Element(${ Expr(value.toCompressedByteString) }) }
            case Constant.BLS12_381_MlResult(value) =>
                sys.error("BLS12_381_MlResult values cannot be serialized as constants in UPLC")
    }

    enum FunType:
        case Lam(f: Any => Any)
        case Builtin(f: Any => Any)

    private def genCodeFromTerm(
        term: Term
    )(using Quotes): Expr[(Logger, BudgetSpender, MachineParams) => Any] = {
        import quotes.reflect.{Lambda, MethodType, Symbol, TypeRepr, asTerm}

        def genCode(
            term: Term,
            env: List[(String, quotes.reflect.Term)],
            logger: Expr[Logger],
            budget: Expr[BudgetSpender],
            params: Expr[MachineParams]
        ): Expr[Any] = {
            term match
                case Term.Var(name) =>
                    val vr = env.find(_._1 == name.name).get._2.asExprOf[Any]
                    '{
                        $budget.spendBudget(Step(StepKind.Var), $params.machineCosts.varCost, Nil)
                        $vr
                    }
                case Term.LamAbs(name, term) =>
                    val mtpe =
                        MethodType(List(name))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Any])
                    val lambda = Lambda(
                      Symbol.spliceOwner,
                      mtpe,
                      { case (methSym, args) =>
                          genCode(
                            term,
                            (name -> args.head.asInstanceOf[quotes.reflect.Term]) :: env,
                            logger,
                            budget,
                            params
                          ).asTerm
                              .changeOwner(methSym)
                      }
                    ).asExprOf[Any]
                    '{
                        $budget.spendBudget(
                          Step(StepKind.LamAbs),
                          $params.machineCosts.varCost,
                          Nil
                        )
                        $lambda
                    }
                case Term.Apply(f, arg) =>
                    val func = genCode(f, env, logger, budget, params)
                    val a = genCode(arg, env, logger, budget, params)
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Apply),
                          $params.machineCosts.applyCost,
                          Nil
                        )
                        ${ func }.asInstanceOf[Any => Any].apply($a)
                    }
                case Term.Force(term) =>
                    val expr = genCode(term, env, logger, budget, params)
                    '{
                        val forceTerm = ${ expr }.asInstanceOf[() => Any]
                        $budget.spendBudget(
                          Step(StepKind.Force),
                          $params.machineCosts.forceCost,
                          Nil
                        )
                        forceTerm.apply()
                    }
                case Term.Delay(term) =>
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Delay),
                          $params.machineCosts.delayCost,
                          Nil
                        )
                        () => ${ genCode(term, env, logger, budget, params) }
                    }
                case Term.Const(const) =>
                    val expr = constantToExpr(const)
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Const),
                          $params.machineCosts.constCost,
                          Nil
                        )
                        $expr
                    }
                case Term.Builtin(DefaultFun.AddInteger) => '{ Builtins.addInteger.curried }
                case Term.Builtin(DefaultFun.EqualsData) => '{ Builtins.equalsData.curried }
                case Term.Builtin(DefaultFun.LessThanInteger) =>
                    '{ Builtins.lessThanInteger.curried }
                case Term.Builtin(DefaultFun.EqualsInteger) => '{ Builtins.equalsInteger.curried }
                case Term.Builtin(DefaultFun.EqualsByteString) =>
                    '{ Builtins.equalsByteString.curried }
                case Term.Builtin(DefaultFun.IfThenElse) =>
                    '{ () => (c: Boolean) => (t: Any) => (f: Any) => Builtins.ifThenElse(c, t, f) }
                case Term.Builtin(DefaultFun.Trace) =>
                    '{ () => (s: String) => (a: Any) =>
                        ${ logger }.log(s); a
                    }
                case Term.Builtin(DefaultFun.FstPair) => '{ () => () => Builtins.fstPair }
                case Term.Builtin(DefaultFun.SndPair) => '{ () => () => Builtins.sndPair }
                case Term.Builtin(DefaultFun.ChooseList) =>
                    '{ () => () => Builtins.chooseList.curried }
                case Term.Builtin(DefaultFun.Sha2_256)     => '{ Builtins.sha2_256 }
                case Term.Builtin(DefaultFun.HeadList)     => '{ () => Builtins.headList }
                case Term.Builtin(DefaultFun.TailList)     => '{ () => Builtins.tailList }
                case Term.Builtin(DefaultFun.UnConstrData) => '{ Builtins.unConstrData }
                case Term.Builtin(DefaultFun.UnListData)   => '{ Builtins.unListData }
                case Term.Builtin(DefaultFun.UnIData)      => '{ Builtins.unIData }
                case Term.Builtin(DefaultFun.UnBData)      => '{ Builtins.unBData }
                case Term.Builtin(bi) =>
                    sys.error(
                      s"Builtin $bi is not yet supported by the JIT compiler. Please add implementation in the Builtin pattern matching section."
                    )
                case Term.Error => '{ throw new RuntimeException("UPLC Error term evaluated") }
                case Term.Constr(tag, args) =>
                    val expr = Expr.ofTuple(
                      Expr(tag) -> Expr.ofList(
                        args.map(a => genCode(a, env, logger, budget, params))
                      )
                    )
                    '{
                        $budget.spendBudget(
                          Step(StepKind.Constr),
                          $params.machineCosts.constrCost,
                          Nil
                        )
                        $expr
                    }
                case Term.Case(arg, cases) =>
                    val constr =
                        genCode(arg, env, logger, budget, params).asExprOf[(Long, List[Any])]
                    val caseFuncs =
                        Expr.ofList(
                          cases.map(c =>
                              genCode(c, env, logger, budget, params).asExprOf[Any => Any]
                          )
                        )
                    '{
                        $budget.spendBudget(Step(StepKind.Case), $params.machineCosts.caseCost, Nil)
                        val (tag, args) = $constr
                        args.foldLeft($caseFuncs(tag.toInt))((f, a) =>
                            f(a).asInstanceOf[Any => Any]
                        )
                    }
        }

        '{ (logger: Logger, budget: BudgetSpender, params: MachineParams) =>
            budget.spendBudget(Startup, params.machineCosts.startupCost, Nil)
            ${ genCode(term, Nil, 'logger, 'budget, 'params) }
        }
    }

    /** Compiles a UPLC term into an optimized JVM function using JIT compilation.
      *
      * This method takes a UPLC term and generates optimized JVM bytecode that can be executed
      * directly without interpretation overhead. The resulting function maintains full
      * compatibility with Plutus VM semantics including proper budget tracking, logging, and error
      * handling.
      *
      * @param term
      *   The UPLC term to compile
      * @return
      *   A function that takes a Logger, BudgetSpender, and MachineParams and returns the
      *   evaluation result. The function signature is:
      *   `(Logger, BudgetSpender, MachineParams) => Any`
      *
      * @example
      *   {{{
      * val term: Term = ... // some UPLC term
      * val jittedFunction = JIT.jitUplc(term)
      * val result = jittedFunction(logger, budgetSpender, machineParams)
      *   }}}
      *
      * @note
      *   The compilation happens at runtime and may take some time for complex terms. The compiled
      *   function can then be cached and reused for better performance when evaluating the same
      *   term multiple times.
      *
      * @throws RuntimeException
      *   if the term contains unsupported constructs or if compilation fails
      */
    def jitUplc(term: Term): (Logger, BudgetSpender, MachineParams) => Any = staging.run {
        (quotes: Quotes) ?=>
            val expr = genCodeFromTerm(term)
            expr
    }
}
