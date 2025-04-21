package scalus.builtin

import scala.collection.immutable.List
import scala.quoted.*

import Data.ToData

/** ToData[A] derivation macros.
  */
object ToData {
    inline def deriveCaseClass[T](inline constrIdx: Int): ToData[T] = ${
        deriveCaseClassMacro[T]('{ constrIdx })
    }

    /** Derive a ToData instance for an enum.
      *
      * @tparam T
      *   the enum type
      * @return
      *   a ToData instance for T
      */
    inline def deriveEnum[T]: ToData[T] = ${ deriveEnumMacro[T] }

    private def deriveEnumMacro[T: Type](using Quotes): Expr[ToData[T]] = {
        import quotes.reflect.*

        val constrTpe = TypeRepr.of[T]
        val typeSymbol = TypeRepr.of[T].widen.dealias.typeSymbol
        if !typeSymbol.flags.is(Flags.Enum) then
            report.errorAndAbort(
              s"deriveEnum can only be used with enums, got ${typeSymbol.fullName}"
            )

        def genRhs(constrIdx: Int, bindings: List[(Symbol, TypeRepr)])(using Quotes) = '{
            Builtins.constrData(
              BigInt(${ Expr(constrIdx) }),
              ${
                  val args = bindings
                      .map { case (binding, tpe) =>
                          tpe.asType match
                              case '[t] =>
                                  Expr.summon[ToData[t]] match
                                      case None =>
                                          report.errorAndAbort(
                                            s"Could not find given ToData[${tpe.widen.show}]"
                                          )
                                      case Some(toData) =>
                                          val arg = Ident(binding.termRef).asExprOf[t]
                                          '{ $toData($arg) }
                      }
                      .asInstanceOf[List[Expr[Data]]]
                  args.foldRight('{ scalus.builtin.Builtins.mkNilData() }) { (data, acc) =>
                      '{ scalus.builtin.Builtins.mkCons($data, $acc) }
                  }
              }
            )
        }

        def genMatch(prodTerm: Term)(using Quotes) = {
            val cases = typeSymbol.children.zipWithIndex.map { (childTypeSymbol, tag) =>
                if childTypeSymbol.caseFields.isEmpty then
                    val rhs = genRhs(tag, Nil).asTerm
                    CaseDef(Ident(childTypeSymbol.termRef), None, rhs)
                else
                    val classSym = childTypeSymbol
                    val companionModuleRef = classSym.companionModule
                    val unapplyRef = companionModuleRef.methodMember("unapply").head.termRef
                    val constr = classSym.primaryConstructor
                    val params = constr.paramSymss.flatten
                    val paramsNameType = params.map(p => p.name -> p.typeRef)
                    val bindingsSymbols = paramsNameType.map { (name, tpe) =>
                        (Symbol.newBind(Symbol.noSymbol, name, Flags.EmptyFlags, tpe), tpe)
                    }

                    val bindings = bindingsSymbols.map { (symbol, _) =>
                        Bind(symbol, Wildcard())
                    }
                    val rhs = genRhs(tag, bindingsSymbols).asTerm
                    CaseDef(
                      Typed(
                        Unapply(Ident(unapplyRef), Nil, bindings).asInstanceOf[Term],
                        TypeTree.ref(childTypeSymbol)
                      ),
                      None,
                      rhs
                    )
            }
            val m =
                Match(prodTerm, cases)
            m
        }

        '{ (product: T) =>
            ${
                val prodTerm = '{ product }.asTerm
                val code = genMatch(prodTerm).asExprOf[Data]
                code
            }
        }
    }

    def deriveCaseClassMacro[T: Type](constrIdx: Expr[Int])(using Quotes): Expr[ToData[T]] =
        import quotes.reflect.*
        val classSym = TypeTree.of[T].symbol
        val companionModuleRef = classSym.companionModule
        val unapplyRef = companionModuleRef.methodMember("unapply").head.termRef
        val constr = classSym.primaryConstructor
        val params = constr.paramSymss.flatten
        val paramsNameType = params.map(p => p.name -> p.typeRef)

        /*
      Generate a pattern match to introduce all the params,
      to avoid a.field1, a.field2, etc.
      Something ike:
        a match
          case A(field1, field2, ...) =>
            mkConstr(
              BigInt($constrIdx),
              mkCons(field1.toData, mkCons(field2.toData, ...))
            )
         */
        def genMatch(prodTerm: Term, params: List[(String, TypeRepr)])(using Quotes) = {
            val bindingsSymbols = params.map { (name, tpe) =>
                (Symbol.newBind(Symbol.noSymbol, name, Flags.EmptyFlags, tpe), tpe)
            }

            val bindings = bindingsSymbols.map { (symbol, _) =>
                Bind(symbol, Wildcard())
            }
            val rhs = genRhs(bindingsSymbols).asTerm
            Match(prodTerm, List(CaseDef(Unapply(Ident(unapplyRef), Nil, bindings), None, rhs)))
        }

        def genRhs(bindings: List[(Symbol, TypeRepr)])(using Quotes) = '{
            Builtins.constrData(
              BigInt($constrIdx),
              ${
                  val args = bindings
                      .map { case (binding, tpe) =>
                          tpe.asType match
                              case '[t] =>
                                  Expr.summon[ToData[t]] match
                                      case None =>
                                          report.errorAndAbort(
                                            s"Could not find given ToData[${tpe.widen.show}]"
                                          )
                                      case Some(toData) =>
                                          val arg = Ident(binding.termRef).asExprOf[t]
                                          '{ $toData($arg) }
                      }
                      .asInstanceOf[List[Expr[Data]]]
                  args.foldRight('{ scalus.builtin.Builtins.mkNilData() }) { (data, acc) =>
                      '{ scalus.builtin.Builtins.mkCons($data, $acc) }
                  }
              }
            )
        }

        '{ (product: T) =>
            ${
                val prodTerm = '{ product }.asTerm
                genMatch(prodTerm, paramsNameType).asExprOf[Data]
            }
        }

    extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]].apply(a)
}
