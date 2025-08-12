package scalus

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SrcPos
import scalus.sir.{AnnotatedSIR, AnnotationsDecl, Binding, DataDecl, Module, Recursivity, SIR, SIRType}

import scala.annotation.unused
import scala.collection.mutable

import scalus.sir.SIRVersion

case class SIRLinkerOptions(
    useUniversalDataConversion: Boolean,
    debugLevel: Int
)

/** Links SIR definitions and data declarations into a single SIR module.
  *
  * This class is responsible for linking SIR definitions and data declarations to create a single
  * SIR module.
  *
  * It traverses the SIR tree and links external definitions and data declarations to the global
  * definitions and data declarations.
  */
class SIRLinker(options: SIRLinkerOptions, sirLoader: SIRLoader)(using ctx: Context) {

    private val globalDefs: mutable.LinkedHashMap[FullName, CompileDef] =
        mutable.LinkedHashMap.empty
    private val globalDataDecls: mutable.LinkedHashMap[FullName, DataDecl] =
        mutable.LinkedHashMap.empty
    private val moduleDefsCache: mutable.Map[String, mutable.LinkedHashMap[FullName, SIR]] =
        mutable.LinkedHashMap.empty.withDefaultValue(mutable.LinkedHashMap.empty)

    // private val sirLoader = new SIRLoader(options.loaderOptions)

    private def error[A](error: CompilationError, defaultValue: A): A = {
        report.error(error.message, error.srcPos)
        defaultValue
    }

    def link(sir: SIR, srcPos: SrcPos): SIR = {
        if options.debugLevel > 1 then
            println(s"Linking SIR at ${srcPos.sourcePos.source}:${srcPos.line}, options=$options")
        val processed = traverseAndLink(sir, srcPos)
        val full: SIR = globalDefs.values.foldRight(processed) {
            case (CompileDef.Compiled(b), acc) =>
                SIR.Let(
                  b.recursivity,
                  List(Binding(b.fullName.name, b.body.tp, b.body)),
                  acc match {
                      case annssir: AnnotatedSIR => annssir
                      case _ =>
                          val msg =
                              s"Unexpected Decl. In binding ${b.fullName.name} in SIRLinker.link"
                          error(
                            GenericError(msg, srcPos),
                            SIR.Error(msg, AnnotationsDecl.fromSrcPos(srcPos))
                          )
                  },
                  AnnotationsDecl.fromSrcPos(srcPos)
                )
            case (d, acc) =>
                error(
                  GenericError(
                    s"""Unexpected globalDefs state: $d
                           |$globalDefs
                           |It's likely a Scalus bug. Please, report it via GitHub Issues or Discord
                           |""".stripMargin,
                    srcPos
                  ),
                  SIR.Error("", AnnotationsDecl.fromSrcPos(srcPos))
                )
        }
        val dataDecls = globalDataDecls.foldRight(full: SIR) { case ((_, decl), acc) =>
            SIR.Decl(decl, acc)
        }
        dataDecls
    }

    private def traverseAndLink(sir: SIR, srcPos: SrcPos): SIR = sir match
        case SIR.Decl(data, term) =>
            SIR.Decl(data, traverseAndLink(term, srcPos))
        case ans: AnnotatedSIR =>
            traverseAndLinkExpr(ans, srcPos)

    private def traverseAndLinkExpr(sir: AnnotatedSIR, srcPos: SrcPos): AnnotatedSIR = sir match
        case v @ SIR.ExternalVar(moduleName, name, tp, ann)
            if !globalDefs.contains(FullName(name)) =>
            linkDefinition(moduleName, FullName(name), srcPos, tp, ann)
            v
        case v @ SIR.Let(recursivity, bindings, body, anns) =>
            val nBingings =
                bindings.map(b => Binding(b.name, b.tp, traverseAndLink(b.value, srcPos)))
            val nBody = traverseAndLink(body, srcPos)
            SIR.Let(recursivity, nBingings, nBody, anns)
        case SIR.LamAbs(param, term, typeParams, anns) =>
            SIR.LamAbs(param, traverseAndLink(term, srcPos), typeParams, anns)
        case SIR.Apply(f, arg, tp, anns) =>
            val fReplaced =
                if options.useUniversalDataConversion then
                    anns.data.get("fromData") match
                        case Some(v) =>
                            SIR.ExternalVar(
                              "scalus.builtin.internal.UniversalDataConversion$",
                              "scalus.builtin.internal.UniversalDataConversion$.fromData",
                              SIRType.Fun(SIRType.Data, tp),
                              AnnotationsDecl.empty.copy(pos = f.anns.pos)
                            )
                        case None =>
                            anns.data.get("toData") match
                                case Some(v) =>
                                    SIR.ExternalVar(
                                      "scalus.builtin.internal.UniversalDataConversion$",
                                      "scalus.builtin.internal.UniversalDataConversion$.toData",
                                      SIRType.Fun(arg.tp, SIRType.Data),
                                      AnnotationsDecl.empty.copy(pos = f.anns.pos)
                                    )
                                case None => f
                else f
            val nF = traverseAndLinkExpr(fReplaced, srcPos)
            val nArg = traverseAndLinkExpr(arg, srcPos)
            SIR.Apply(nF, nArg, tp, anns)
        case SIR.And(lhs, rhs, anns) =>
            val nLhs = traverseAndLinkExpr(lhs, srcPos)
            val nRhs = traverseAndLinkExpr(rhs, srcPos)
            SIR.And(nLhs, nRhs, anns)
        case SIR.Or(lhs, rhs, anns) =>
            val nLhs = traverseAndLinkExpr(lhs, srcPos)
            val nRhs = traverseAndLinkExpr(rhs, srcPos)
            SIR.Or(nLhs, nRhs, anns)
        case SIR.Not(term, anns) => SIR.Not(traverseAndLinkExpr(term, srcPos), anns)
        case SIR.IfThenElse(cond, t, f, tp, anns) =>
            val nCond = traverseAndLinkExpr(cond, srcPos)
            val nT = traverseAndLinkExpr(t, srcPos)
            val nR = traverseAndLinkExpr(f, srcPos)
            SIR.IfThenElse(nCond, nT, nR, tp, anns)
        case SIR.Constr(name, data, args, tp, anns) =>
            globalDataDecls.put(FullName(data.name), data)
            val nArgs = args.map(a => traverseAndLink(a, srcPos))
            SIR.Constr(name, data, nArgs, tp, anns)
        case SIR.Match(scrutinee, cases, rhsType, anns) =>
            val nScrutinee = traverseAndLinkExpr(scrutinee, srcPos)
            val nCases =
                cases.map(c => SIR.Case(c.pattern, traverseAndLink(c.body, srcPos), c.anns))
            SIR.Match(nScrutinee, nCases, rhsType, anns)
        case SIR.Select(scrutinee, field, tp, anns) =>
            val nScrutinee = traverseAndLink(scrutinee, srcPos)
            SIR.Select(nScrutinee, field, tp, anns)
        case SIR.Cast(term, tp, anns) =>
            SIR.Cast(traverseAndLinkExpr(term, srcPos), tp, anns)
        case other => other

    private def findAndLinkDefinition(
        defs: collection.Map[FullName, SIR],
        fullName: FullName,
        @unused tp: SIRType,
        srcPos: SrcPos
    ): Boolean = {
        val found = defs.get(fullName)
        for sir <- found do
            globalDefs.update(fullName, CompileDef.Compiling)
            val nSir = traverseAndLink(sir, srcPos)
            // TODO: reseatch.  removeing 'remove' triggre fail of  scalus.CompilerPluginTest. 'compile fieldAsData macro'
            globalDefs.remove(fullName)
            globalDefs.update(
              fullName,
              CompileDef.Compiled(TopLevelBinding(fullName, Recursivity.Rec, nSir))
            )
        found.isDefined
    }

    private def linkDefinition(
        moduleName: String,
        fullName: FullName,
        srcPos: SrcPos,
        tp: SIRType,
        anns: AnnotationsDecl
    ): Unit = {
        // println(s"linkDefinition: ${fullName}")
        retrieveModule(moduleName, srcPos) match
            case Left(filename) =>
                report.error(
                  s"Module not found during linking: ${moduleName} , missing filename: ${filename} referenced for name ${fullName.name} from ${anns.pos.file}: ${anns.pos.startLine}",
                  srcPos
                )
            case Right(defs) =>
                if !findAndLinkDefinition(defs, fullName, tp, srcPos) then
                    error(
                      SymbolNotFound(
                        fullName.name,
                        moduleName,
                        srcPos,
                        anns.pos,
                        defs.keys.map(_.name).toSet
                      ),
                      SIR.Error("Symbol not found", AnnotationsDecl.fromSrcPos(srcPos))
                    )
    }

    private def retrieveModule(
        moduleName: String,
        srcPos: SrcPos
    ): Either[String, mutable.LinkedHashMap[FullName, SIR]] = {
        moduleDefsCache.get(moduleName) match
            case Some(defs) => Right(defs)
            case None =>
                sirLoader.findAndReadModule(moduleName) match
                    case Right(module) =>
                        validateSIRVersion(module, moduleName, srcPos)
                        val defsMap = mutable.LinkedHashMap.from(
                          module.defs.map(d => FullName(d.name) -> d.value)
                        )
                        moduleDefsCache.put(moduleName, defsMap)
                        Right(defsMap)
                    case Left(filename) =>
                        Left(filename)
    }

    private def validateSIRVersion(module: Module, moduleName: String, srcPos: SrcPos): Unit = {
        if (module.version._1 != SIRVersion._1)
            || (module.version._1 == SIRVersion._1
                && SIRVersion._2 < module.version._2)
        then
            report.error(
              s"""During linking I've found that a module '$moduleName' has an incompatible SIR version: ${module.version} (expected: ${SIRVersion}).
                   |This can happen if you try to link a module compiled with a different version of Scalus.
                   |Please, recompile the module with the version of Scalus that has the SIR version ${SIRVersion}
                   |""".stripMargin,
              srcPos
            )
    }

}
