package scalus.builtin
import scalus.builtin.Data.FromData
import scala.collection.immutable.List

import scala.quoted.*

/** FromData[A] derivation
  */
object FromData {

    inline def deriveCaseClass[T]: FromData[T] = ${ deriveCaseClassMacro[T] }

    /** Derive FromData for an enum type
      *
      * @param conf
      *   a partial function mapping tag to constructor function, like
      * @return
      *   a FromData instance
      *
      * @example
      *   {{{
      *   enum Adt:
      *     case A
      *     case B(b: Boolean)
      *     case C(a: Adt, b: Adt)
      *
      *   given FromData[Adt] = FromData.deriveEnum[Adt] {
      *     case 0 => _ => Adt.A
      *     case 1 => FromData.deriveConstructor[Adt.B]
      *     case 2 => FromData.deriveConstructor[Adt.C]
      *     }
      *   }}}
      */
    inline def deriveEnum[T](
        inline conf: PartialFunction[Int, scalus.builtin.List[Data] => T]
    ): FromData[T] = ${ deriveEnumMacro[T]('{ conf }) }

    /** Derive FromData for an enum type
      *
      * @return
      *   a FromData instance
      *
      * @example
      *   {{{
      *   enum Adt:
      *     case A
      *     case B(b: Boolean)
      *     case C(a: Adt, b: Adt)
      *
      *   given FromData[Adt] = FromData.deriveEnum[Adt]
      *   }}}
      */
    inline def deriveEnum[T]: FromData[T] = ${ deriveEnumMacro2[T] }

    inline def deriveConstructor[T]: scalus.builtin.List[Data] => T = ${
        deriveConstructorMacro[T]
    }

    private def deriveConstructorMacro[T: Type](using
        Quotes
    ): Expr[scalus.builtin.List[Data] => T] =
        import quotes.reflect.*
        val classSym = TypeTree.of[T].symbol
        val constr = classSym.primaryConstructor
        val params = constr.paramSymss.flatten
        val fromDataOfArgs = params.map { param =>
            val tpe = param.termRef.widen.dealias
            tpe.asType match
                case '[t] =>
                    Expr.summon[FromData[t]] match
                        case None =>
                            report.errorAndAbort(
                              s"Could not find implicit for FromData[${tpe.show}]"
                            )
                        case Some(value) => value
        }
        def genGetter(init: Expr[scalus.builtin.List[Data]], idx: Int)(using Quotes): Expr[Data] =
            var expr = init
            var i = 0
            while i < idx do
                val exp = expr // save the current expr, otherwise it will loop forever
                expr = '{ $exp.tail }
                i += 1
            '{ $expr.head }

        def genConstructorCall(
            a: Expr[scalus.builtin.List[scalus.builtin.Data]]
        )(using Quotes): Expr[T] = {
            val args = fromDataOfArgs.zipWithIndex.map { case (appl, idx) =>
                val arg = genGetter(a, idx)
                '{ $appl($arg) }.asTerm
            }
            // Couldn't find a way to do this using quotes, so just construct the tree manually
            New(TypeTree.of[T]).select(constr).appliedToArgs(args).asExprOf[T]
        }
        '{ (args: scalus.builtin.List[scalus.builtin.Data]) => ${ genConstructorCall('{ args }) } }

    private def deriveCaseClassMacro[T: Type](using Quotes): Expr[FromData[T]] =
        '{ (d: Data) =>
            val args = scalus.builtin.Builtins.unConstrData(d).snd

            // generate f = (args) => new Constructor(args.head, args.tail.head, ...)
            // then apply to args: f(args)
            // and finally beta reduce it in compile time
            ${ Expr.betaReduce('{ ${ deriveConstructorMacro[T] }(args) }) }
        }

    private def deriveEnumMacro[T: Type](
        conf: Expr[PartialFunction[Int, scalus.builtin.List[Data] => T]]
    )(using
        Quotes
    ): Expr[FromData[T]] =
        import quotes.reflect.*
        val mapping = conf.asTerm match
            case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Match(_, cases)))), _)) =>
                cases.map { case CaseDef(Literal(IntConstant(tag)), _, code) =>
                    (tag, code.asExprOf[scalus.builtin.List[Data] => T])
                }
        // stage programming is cool, but it's hard to comprehend what's going on
        '{ (d: Data) =>
            val pair = Builtins.unConstrData(d)
            val tag = pair.fst
            val args = pair.snd
            ${
                mapping.foldRight('{ throw new Exception("Invalid tag") }.asExprOf[T]) {
                    case ((t, code), acc) =>
                        '{
                            if Builtins.equalsInteger(tag, BigInt(${ Expr(t) })) then
                                ${ Expr.betaReduce('{ $code(args) }) }
                            else $acc
                        }
                }
            }
        }

    private def deriveEnumMacro2[T: Type](using Quotes): Expr[FromData[T]] = {
        import quotes.reflect.*
        val constrTpe = TypeRepr.of[T]
        val typeSymbol = TypeRepr.of[T].widen.dealias.typeSymbol
        if !typeSymbol.flags.is(Flags.Enum) then
            report.errorAndAbort(
              s"deriveEnum can only be used with enums, got ${typeSymbol.fullName}"
            )

        val mapping = typeSymbol.children
            .map { child =>
                child.typeRef.asType match
                    case '[t] =>
                        // println(s"child: ${child}, ${child.flags.show} ${child.caseFields}")
                        if child.caseFields.isEmpty then
                            '{ (_: scalus.builtin.List[Data]) =>
                                ${ Ident(child.termRef).asExprOf[t] }
                            }
                        else deriveConstructorMacro[t]
            }
            .zipWithIndex
            .asInstanceOf[List[(Expr[scalus.builtin.List[Data] => T], Int)]]

        // stage programming is cool, but it's hard to comprehend what's going on
        '{ (d: Data) =>
            val pair = Builtins.unConstrData(d)
            val tag = pair.fst
            val args = pair.snd
            ${
                mapping.foldRight('{ throw new Exception("Invalid tag") }.asExprOf[T]) {
                    case ((code, t), acc) =>
                        '{
                            if Builtins.equalsInteger(tag, BigInt(${ Expr(t) })) then $code(args)
                            else $acc
                        }
                }
            }
        }
    }
}
