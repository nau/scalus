package scalus.builtin

import scala.quoted.*

import scala.collection.immutable.{List => SList}


object FromDataLense {

    def make[A:Type, B:Type](f: Expr[A => B])(using quotes: Quotes): Expr[Data => Data] = {
        import quotes.reflect.*
        makeFromFunTerm(f.asTerm).asExprOf[Data => Data]
    }

    def makeFromFunTerm(using quotes: Quotes)(f: quotes.reflect.Term): quotes.reflect.Term = {
        import quotes.reflect.*
        f match
            case Inlined(origin, bindings, expansion) =>
                Inlined(origin, bindings, makeFromFunTerm(expansion))
            case Lambda(params, body) =>
                params match
                    case param::Nil =>
                       val mt = MethodType(SList("data"))(_ => SList(TypeRepr.of[Data]), _ => TypeRepr.of[Data])
                       val meth = Symbol.newMethod(Symbol.spliceOwner, "lense", mt)
                       val retval = Lambda(meth, mt, (owner, dataPrams) => {
                            val dataParam = dataPrams.head.asInstanceOf[Term]
                            makeFromBodyTerm(param, body, dataParam).changeOwner(owner)
                       })
                       retval
                    case _ =>
                       report.errorAndAbort(s"Expected a lambda with one parameter, got ${params.size} parameters")
            case _ =>
                report.errorAndAbort(s"Expected a lambda, got ${f.show}")
    }

    def makeFromBodyTerm(using quotes: Quotes)(inFunParam: quotes.reflect.ValDef,
                                               body: quotes.reflect.Term,
                                               dataParam: quotes.reflect.Term): quotes.reflect.Term = {
        import quotes.reflect.*
        body match
            case Inlined(origin, bindings, expansion) =>
                Inlined(origin, bindings, makeFromBodyTerm(inFunParam, expansion, dataParam))
            case id@Ident(name) =>
                if id.symbol == inFunParam.symbol then
                    dataParam
                else
                    report.errorAndAbort(s"In lense function no idents other then input params are allowed")
            case Block(stats, expr) =>
                Block(stats, makeFromBodyTerm(inFunParam, expr, dataParam))
            case sel@Select(qual, name) =>
                val newQual = makeFromBodyTerm(inFunParam, qual, dataParam)
                makeSelect(newQual, sel.symbol, dataParam)
            case Apply(fun, args) =>
                if (fun.symbol == Symbol.requiredMethod("scalaus.prelude.List.apply")) {
                    ???
                } else if (fun.symbol == Symbol.requiredMethod("scalus.prelude.AssocMap.apply")) {
                    ???
                } else {
                    // low priority todo: add support for user-level indexing functions,  sset by implicits
                    report.errorAndAbort(s"Only indexing functions are allowed in lense function, got ${fun.show}")
                }
            case Typed(expr, tpt) =>
                makeFromBodyTerm(inFunParam, expr, dataParam)
            case _ =>
                report.errorAndAbort(s"Unsupporte construction in lense function: ${body}")
    }


    def makeSelect(using quotes: Quotes)(qual: quotes.reflect.Term, sel: quotes.reflect.Symbol, dataParam: quotes.reflect.Term): quotes.reflect.Term = {
        import quotes.reflect.*
        ???
    }



}








