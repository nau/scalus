package scalus.prelude

import scalus.CompileDerivations
import scalus.builtin.Builtins

import scala.quoted.*

type Data = scalus.builtin.Data
type ByteString = scalus.builtin.ByteString

@FunctionalInterface
trait ToData[-A] extends Function1[A, Data] with CompileDerivations {
    override def apply(v1: A): Data
}

@scalus.Compile
object ToData {

    inline def derived[A]: ToData[A] = ${
        DataRepresentation.toDataImpl[A]
    }

    given scalus.prelude.ToData[Data] = (a: Data) => a
    given scalus.prelude.ToData[ByteString] = Builtins.bData
    given scalus.prelude.ToData[BigInt] = Builtins.iData

}

@FunctionalInterface
trait FromData[+A] extends Function1[Data, A] with CompileDerivations {
    override def apply(v: Data): A
}

@scalus.Compile
object FromData {

    inline def derived[A]: FromData[A] = ${
        DataRepresentation.fromDataImpl[A]
    }

    given scalus.prelude.FromData[Data] = (a: Data) => a
    given scalus.prelude.FromData[ByteString] = Builtins.unBData
    given scalus.prelude.FromData[BigInt] = Builtins.unIData

}

trait DataRepresentation[A] {

    def toData(a: A): Data

    def fromData(d: Data): A

    def select(d: Data, fieldName: String): Data

}

object DataRepresentation {

    def toDataImpl[A: Type](using Quotes): Expr[ToData[A]] = {
        import quotes.reflect.*
        '{ (a: A) =>
            ${ generateToDataApply[A]('a) }
        }
        /*
        '{
            new ToData[A] {
                override def apply(a: A): Data = {
                    ${ generateToDataApply[A]('a) }
                }
            }
        }

         */
    }

    def generateToDataApply[A: Type](a: Expr[A])(using Quotes): Expr[Data] = {
        import quotes.reflect.*
        // TODO:  SIRValue(SIRType[A]).generateToDataApply(a)
        val tpe = TypeRepr.of[A].dealias.widen
        if tpe <:< TypeRepr.of[AnyRef] then
            val typeArgs = tpe.typeArgs
            val children = tpe.typeSymbol.children
            if children.isEmpty then
                if tpe.typeSymbol.flags.is(Flags.Trait) then
                    report.errorAndAbort(
                      s"Cannot derive ToData for trait ${tpe.typeSymbol.fullName}"
                    )
                else if tpe.typeSymbol.flags.is(Flags.Case | Flags.Enum) then {
                    val constrIndex = findADTConstrIndex[A]
                    deriveToDataCaseClassApply[A](a, constrIndex)
                } else
                    report.errorAndAbort(
                      s"Cannot derive ToData for ${tpe.typeSymbol.fullName} which is not a case class or enum"
                    )
            else deriveToDataSumCaseClassApply[A](a)
        else
            report.errorAndAbort(
              s"Cannot derive ToData for ${tpe.typeSymbol.fullName}"
            )
    }

    def deriveToDataCaseClassApply[A: Type](
        a: Expr[A],
        constrIdx: Int
    )(using Quotes): Expr[Data] = {
        import quotes.reflect.*

        val classSym = TypeTree.of[A].symbol
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
        def genMatch(prodTerm: Term, params: scala.List[(String, TypeRepr)])(using Quotes) = {
            val bindingsSymbols = params.map { (name, tpe) =>
                (Symbol.newBind(Symbol.noSymbol, name, Flags.EmptyFlags, tpe), tpe)
            }

            val bindings = bindingsSymbols.map { (symbol, _) =>
                Bind(symbol, Wildcard())
            }
            val rhs = genRhs(bindingsSymbols).asTerm
            Match(
              prodTerm,
              scala.List(CaseDef(Unapply(Ident(unapplyRef), Nil, bindings), None, rhs))
            )
        }

        def genRhs(bindings: scala.List[(Symbol, TypeRepr)])(using Quotes) = '{
            Builtins.constrData(
              BigInt(${ Expr(constrIdx) }),
              ${
                  val args = bindings
                      .map { case (binding, tpe) =>
                          tpe.widen.asType match
                              case '[t] =>
                                  Expr.summon[scalus.prelude.ToData[t]] match
                                      case None =>
                                          report.errorAndAbort(
                                            s"Could not find given ToData[${tpe.widen.show}] within ${TypeRepr.of[A].show} wheb generate product"
                                          )
                                      case Some(toData) =>
                                          val arg = Ident(binding.termRef).asExprOf[t]
                                          '{ $toData($arg) }
                      }
                  args.foldRight('{ scalus.builtin.Builtins.mkNilData() }) { (data, acc) =>
                      '{ scalus.builtin.Builtins.mkCons($data, $acc) }
                  }
              }
            )
        }

        genMatch(a.asTerm, paramsNameType).asExprOf[Data]

    }

    def deriveToDataSumCaseClassApply[A: Type](value: Expr[A])(using Quotes): Expr[Data] = {
        import quotes.reflect.*
        val tpe = TypeRepr.of[A].dealias.widen
        val originTpe = tpe
        val children = tpe.typeSymbol.children
        import quotes.reflect.*

        val constrTpe = TypeRepr.of[A]
        val typeSymbol = TypeRepr.of[A].widen.dealias.typeSymbol

        def genRhs(constrIdx: Int, bindings: scala.List[(Symbol, TypeRepr)])(using Quotes) = '{
            Builtins.constrData(
              BigInt(${ Expr(constrIdx) }),
              ${
                  val args = bindings
                      .map { case (binding, atpe) =>
                          atpe.asType match
                              case '[t] =>
                                  Expr.summon[scalus.prelude.ToData[t]] match
                                      case None =>
                                          println(
                                            s"atpe=${atpe.show}, atpe.widen=${atpe.widen.show}, atpe."
                                          )
                                          println(
                                            s"constrIdex = ${constrIdx}, binding = ${binding}, bindings = ${bindings}"
                                          )
                                          report.errorAndAbort(
                                            s"Could not find given ToData[${atpe.widen.show}] within ${originTpe.widen.show} when generating sum"
                                          )
                                      case Some(toData) =>
                                          val arg = Ident(binding.termRef).asExprOf[t]
                                          '{ $toData($arg) }.asExprOf[Data]
                      }
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
                    val constrNudeType = TypeIdent(classSym).tpe
                    // val paramTypes = constrNudeType.memberType(constr) match
                    //    case PolyType(_, tpe) => tpe
                    //    case tpe              => tpe
                    // }
                    val params = constr.paramSymss.flatten

                    // println(s"constr = ${constr}, typePrams=${typeParams}, parans = ${params}")
                    val paramsNameType = params.map { p =>
                        // p.name -> p.info
                        p.name -> p.typeRef
                    }
                    val bindingsSymbols = paramsNameType.map { (name, tpe) =>
                        (Symbol.newBind(Symbol.noSymbol, name, Flags.EmptyFlags, tpe), tpe)
                    }

                    val bindings = bindingsSymbols.map { (symbol, _) =>
                        Bind(symbol, Wildcard())
                    }
                    val rhs = genRhs(tag, bindingsSymbols).asTerm
                    CaseDef(
                      // Typed(
                      Unapply(Ident(unapplyRef), Nil, bindings),
                      //  TypeTree.ref(childTypeSymbol)
                      // ),
                      None,
                      rhs
                    )
            }
            val m = Match(prodTerm, cases)
            m
        }

        genMatch(value.asTerm).asExprOf[Data]
    }

    def fromDataImpl[A: Type](using Quotes): Expr[FromData[A]] = {
        import quotes.reflect.*
        val ta = TypeRepr.of[A].dealias.widen
        if ta <:< TypeRepr.of[AnyRef] then
            val children = ta.typeSymbol.children
            if children.isEmpty then
                if ta.typeSymbol.flags.is(Flags.Trait) then
                    report.errorAndAbort(
                      s"Cannot derive FromData for trait ${ta.typeSymbol.fullName}"
                    )
                else if ta.typeSymbol.flags.is(Flags.Case | Flags.Enum) then {
                    deriveFromDataCaseClassApply[A]
                } else {
                    report.errorAndAbort(
                      s"Cannot derive FromData for ${ta.typeSymbol.fullName} which is not a case class or enum"
                    )
                }
            else {
                deriveFromDataSumCaseClassApply[A]()
            }
        else
            report.errorAndAbort(
              s"Cannot derive FromData for ${ta.typeSymbol.fullName} which is not a case class or enum"
            )
    }

    def deriveFromDataCaseClassApply[A: Type](using Quotes): Expr[FromData[A]] = {
        '{ (d: Data) =>
            val args = scalus.builtin.Builtins.unConstrData(d).snd

            // generate f = (args) => new Constructor(args.head, args.tail.head, ...)
            // then apply to args: f(args)
            // and finally beta reduce it in compile time
            ${ Expr.betaReduce('{ ${ scalus.builtin.FromData.deriveConstructorMacro[A] }(args) }) }
        }
    }

    def deriveFromDataSumCaseClassApply[A: Type](using Quotes): Expr[FromData[A]] = {
        import quotes.reflect.*
        val constrTpe = TypeRepr.of[A]
        val typeSymbol = TypeRepr.of[A].widen.dealias.typeSymbol
        if !typeSymbol.flags.is(Flags.Enum) then
            report.errorAndAbort(
              s"deriveEnum can only be used with enums, got ${typeSymbol.fullName}"
            )

        val mappingRhs: scala.List[Expr[scalus.builtin.List[Data] => A]] = typeSymbol.children.map {
            child =>
                child.typeRef.asType match
                    case '[t] =>
                        // println(s"child: ${child}, ${child.flags.show} ${child.caseFields}")
                        if child.caseFields.isEmpty then
                            '{ (_: scalus.builtin.List[Data]) =>
                                ${ Ident(child.termRef).asExprOf[t] }
                            }.asExprOf[scalus.builtin.List[Data] => A]
                        else scalus.builtin.FromData.deriveConstructorMacro[A]
                    case _ =>
                        report.errorAndAbort(
                          s"Cannot derive FromData for ${child.typeRef.show} "
                        )

        }
        // .asInstanceOf[scala.List[(Expr[scalus.builtin.List[Data] => A], Int)]]

        // stage programming is cool, but it's hard to comprehend what's going on
        '{ (d: Data) =>
            val pair = Builtins.unConstrData(d)
            val tag = pair.fst
            val args = pair.snd
            ${
                mappingRhs.zipWithIndex.foldRight('{
                    throw new Exception("Invalid tag")
                }.asExprOf[A]) { case ((code, t), acc) =>
                    '{
                        if Builtins.equalsInteger(tag, BigInt(${ Expr(t) })) then $code(args)
                        else $acc
                    }
                }
            }
        }

    }

    /** Find the index of the given type constructor in the ADT. 0 if this index is not a giving
      * type hierarchy.
      *
      * <pre> trait A; case class B(x: Int) extends A; case class C(x: Int) extends A </pre> Here
      * `A.childs` - > List(B, C) and `findADTConstrIndex[A]` will return 0. `findADTConstrIndex[B]`
      * will return 1
      */
    private def findADTConstrIndex[A: Type](using Quotes): Int = {
        import quotes.reflect.*
        val at = TypeRepr.of[A]
        val baseClasses = at.baseClasses
        val sealedBaseClasses = baseClasses.filter(_.flags.is(Flags.Sealed))
        if sealedBaseClasses.isEmpty then 0
        else
            val firstSealedBaseClass = sealedBaseClasses.head
            val otherSealedBaseClasses = sealedBaseClasses.tail
            if otherSealedBaseClasses.nonEmpty then
                report.errorAndAbort(
                  s"Cannot derive ToData for ${Type.show[A]} which is not a case class or enum"
                )
            else
                val myIndex = firstSealedBaseClass.children.indexWhere(_ == at.typeSymbol)
                if myIndex == -1 then
                    report.errorAndAbort(
                      s"Cannot derive ToData for ${Type.show[A]} (can't find index of constructor in ${firstSealedBaseClass})"
                    )
                else myIndex
    }

}
