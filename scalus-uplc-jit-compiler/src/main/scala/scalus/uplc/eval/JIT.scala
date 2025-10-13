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

    /** Get type information for a builtin from CardanoBuiltins.
      *
      * @param fun
      *   The builtin function
      * @return
      *   A tuple of (numTypeVars, arity) where numTypeVars is the number of type parameters and
      *   arity is the number of regular parameters
      */
    private def getBuiltinTypeInfo(fun: DefaultFun): (Int, Int) = {
        import scalus.uplc.Meaning.allBuiltins
        val runtime = allBuiltins.BuiltinMeanings(fun)
        (runtime.typeScheme.numTypeVars, runtime.typeScheme.arity)
    }

    /** Generate the builtin method name from DefaultFun by lowercasing the first letter.
      *
      * @param fun
      *   The builtin function
      * @return
      *   The method name in Builtins object
      */
    private def builtinMethodName(fun: DefaultFun): String = {
        val name = fun.toString
        s"${name.head.toLower}${name.tail}"
    }

    /** Generate code for a builtin function.
      *
      * @param fun
      *   The builtin function
      * @return
      *   The generated code expression
      */
    private def genBuiltinCode(fun: DefaultFun)(using Quotes): Expr[Any] = {
        import quotes.reflect.*

        val methodName = builtinMethodName(fun)
        val (numTypeVars, arity) = getBuiltinTypeInfo(fun)

        // Get the Builtins object and select the method
        val builtinsTerm = '{ Builtins }.asTerm
        val methodSymbol = builtinsTerm.tpe.typeSymbol.methodMember(methodName).head
        val methodSelect = builtinsTerm.select(methodSymbol)

        var resultTerm: quotes.reflect.Term = methodSelect

        // For arity > 1, we need to eta-expand the method to a function, then call .curried
        // For example, for a method ifThenElse[A](cond: Boolean, thenBranch: A, elseBranch: A): A
        // We first need to apply type arguments if any: ifThenElse[Any]
        // then eta-expand to get a Function3[Boolean, Any, Any, Any]
        // then select .curried to get Function1[Boolean, Function1[Any, Function1[Any, Any]]]


        // Eta-expand the method term to convert it to a function
        // e.g., Function3[Boolean, Any, Any, Any]
        val etaExpanded = resultTerm.etaExpand(Symbol.spliceOwner)
        resultTerm = etaExpanded
        if arity > 1 then
            // Apply type arguments as TypeRepr.of[Any]
            resultTerm = resultTerm.appliedToTypes(List.fill(numTypeVars)(TypeRepr.of[Any]))
            // Now select .curried on the function
            // to get Function1[Boolean, Function1[Any, Function1[Any, Any]]]
            val curriedSymbol = resultTerm.tpe.typeSymbol.methodMember("curried").headOption
            curriedSymbol match
                case Some(sym) =>
                    resultTerm = resultTerm.select(sym)
                case None =>
                    // Couldn't find curried method
                    println(
                      s"Warning: Could not find 'curried' method for builtin $fun with arity $arity"
                    )
                    ()
        // Wrap with () => for each type variable
        // () => ifThenElse[Any].curried
        // () => (Boolean) => (Any) => (Any) => Any
        for _ <- 0 until numTypeVars do
            val capturedTerm = resultTerm // Capture the current term for the lambda body
            val mtpe = MethodType(List())(_ => List(), _ => TypeRepr.of[Any])
            resultTerm = Lambda(
              Symbol.spliceOwner,
              mtpe,
              { case (methSym, _) =>
                  capturedTerm.changeOwner(methSym)
              }
            )
        try resultTerm.asExprOf[Any]
        catch
            case e: Exception =>
                println(s"Error generating code for builtin $fun: ${e.getMessage}")
                println(resultTerm.show)
                throw e
    }

    /** Generate code from a UPLC term.
      *
      * We convert LamAbs to Scala lambdas, Apply to function application, Delay to () =>, Force to
      * .apply(), Var to variable lookup in the environment, Const to constant expressions and
      * Builtin to calls to the Builtins object methods.
      */
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
                // Special handling for Trace because we use the logger instead of calling the builtin
                case Term.Builtin(DefaultFun.Trace) =>
                    '{ () => (s: String) => (a: Any) =>
                        ${ logger }.log(s); a
                    }
                case Term.Builtin(fun) => genBuiltinCode(fun)
                case Term.Error => '{ throw new RuntimeException("UPLC Error term evaluated") }
                case Term.Constr(tag, args) =>
                    val expr = Expr.ofTuple(
                      Expr(tag.value) -> Expr.ofList(
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
