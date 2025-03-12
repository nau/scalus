package scalus.uplc.eval

import scalus.builtin.{BLS12_381_G1_Element, BLS12_381_G2_Element, BLS12_381_MlResult, Builtins, ByteString, Data, PlatformSpecific, given}
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.TermDSL.given
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.*
import scalus.Compiler.compile
import scalus.sir.SIR
import scalus.uplc.Constant.toValue
import scalus.uplc.eval.ExBudgetCategory.Startup
import scalus.utils.Utils.lowerFirst

import scala.quoted.*

object JIT {
    private given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

    private val ps: PlatformSpecific = scalus.builtin.JVMPlatformSpecific

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
                '{ BLS12_381_G1_Element(${ Expr(value.value) }) }
            case Constant.BLS12_381_G2_Element(value) =>
                '{ BLS12_381_G2_Element(${ Expr(value.value) }) }
            case Constant.BLS12_381_MlResult(value) => ???
    }

    private def genCodeFromTerm(term: Term)(using Quotes): Expr[(Logger, BudgetSpender) => Any] = {
        import quotes.reflect.{Lambda, MethodType, Symbol, ValDef, TypeRepr, asTerm, Ref, Select, Flags}

        def genCode(
            term: Term,
            env: List[(String, quotes.reflect.Term)],
            logger: Expr[Logger],
            budget: Expr[BudgetSpender]
        ): Expr[Any] = {
            term match
                case Term.Var(name) =>
                    env.find(_._1 == name.name).get._2.asExprOf[Any]
                case Term.LamAbs(name, term) =>
                    val mtpe =
                        MethodType(List(name))(_ => List(TypeRepr.of[Any]), _ => TypeRepr.of[Any])
                    Lambda(
                      Symbol.spliceOwner,
                      mtpe,
                      { case (methSym, List(arg1: quotes.reflect.Term)) =>
                          genCode(term, (name -> arg1) :: env, logger, budget).asTerm
                              .changeOwner(methSym)
                      }
                    ).asExprOf[Any]
                case Term.Apply(f, arg) =>
                    val func = genCode(f, env, logger, budget)
                    val a = genCode(arg, env, logger, budget)
                    Expr.betaReduce('{
                        ${ func }.asInstanceOf[Any => Any].apply($a)
                    })
                case Term.Force(term) =>
                    val expr = genCode(term, env, logger, budget)
                    '{
                        val forceTerm = ${ expr }.asInstanceOf[() => Any]
                        forceTerm.apply()
                    }
                case Term.Delay(term)  => '{ () => ${ genCode(term, env, logger, budget) } }
                case Term.Const(const) => constantToExpr(const)
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
                case Term.Builtin(DefaultFun.Sha2_256)     => '{ Builtins.sha2_256(using ps) }
                case Term.Builtin(DefaultFun.HeadList)     => '{ () => Builtins.headList }
                case Term.Builtin(DefaultFun.TailList)     => '{ () => Builtins.tailList }
                case Term.Builtin(DefaultFun.UnConstrData) => '{ Builtins.unConstrData }
                case Term.Builtin(DefaultFun.UnListData)   => '{ Builtins.unListData }
                case Term.Builtin(DefaultFun.UnIData)      => '{ Builtins.unIData }
                case Term.Builtin(DefaultFun.UnBData)      => '{ Builtins.unBData }
                case Term.Error => '{ throw new RuntimeException("Error") }
                case Term.Constr(tag, args) =>
                    Expr.ofTuple(
                      Expr(tag) -> Expr.ofList(args.map(a => genCode(a, env, logger, budget)))
                    )
                case Term.Case(arg, cases) =>
                    val constr = genCode(arg, env, logger, budget).asExprOf[(Long, List[Any])]
                    val caseFuncs =
                        Expr.ofList(
                          cases.map(c => genCode(c, env, logger, budget).asExprOf[Any => Any])
                        )
                    '{
                        val (tag, args) = $constr
                        args.foldLeft($caseFuncs(tag.toInt))((f, a) =>
                            f(a).asInstanceOf[Any => Any]
                        )
                    }
        }

        '{ (logger: Logger, budget: BudgetSpender) =>
            budget.spendBudget(Startup, ExBudget.zero, Nil)     
            ${ genCode(term, Nil, 'logger, 'budget) } 
        }
    }

    def jitUplc(term: Term): (Logger, BudgetSpender) => Any = staging.run { (quotes: Quotes) ?=>
        val expr = genCodeFromTerm(term)
//        println(expr.show)
        expr
    }
}
