package scalus

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.util.SrcPos
import dotty.tools.io.ClassPath
import scalus.flat.DecoderState
import scalus.flat.FlatInstantces.given
import scalus.sir.{AnnotationsDecl, Binding, DataDecl, Module, Recursivity, SIR}

import scala.collection.mutable
import scala.util.control.NonFatal

/** Links SIR definitions and data declarations into a single SIR module.
  *
  * This class is responsible for linking SIR definitions and data declarations to create a single
  * SIR module.
  *
  * It traverses the SIR tree and links external definitions and data declarations to the global
  * definitions and data declarations.
  */
class SIRLinker(using ctx: Context) {

    private val globalDefs: mutable.LinkedHashMap[FullName, CompileDef] =
        mutable.LinkedHashMap.empty
    private val globalDataDecls: mutable.LinkedHashMap[FullName, DataDecl] =
        mutable.LinkedHashMap.empty
    private val moduleDefsCache: mutable.Map[String, mutable.LinkedHashMap[FullName, SIR]] =
        mutable.LinkedHashMap.empty.withDefaultValue(mutable.LinkedHashMap.empty)

    private val sirLoader = new SIRLoader(using ctx)

    private def error[A](error: CompilationError, defaultValue: A): A = {
        report.error(error.message, error.srcPos)
        defaultValue
    }

    def link(sir: SIR, srcPos: SrcPos): SIR = {
        traverseAndLink(sir, srcPos)
        val full: SIR = globalDefs.values.foldRight(sir) {
            case (CompileDef.Compiled(b), acc) =>
                SIR.Let(
                  b.recursivity,
                  List(Binding(b.fullName.name, b.body)),
                  acc,
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
        val dataDecls = globalDataDecls.foldRight((full: SIR)) { case ((_, decl), acc) =>
            SIR.Decl(decl, acc)
        }
        dataDecls
    }

    private def traverseAndLink(sir: SIR, srcPos: SrcPos): Unit = sir match
        case SIR.ExternalVar(moduleName, name, tp, ann) if !globalDefs.contains(FullName(name)) =>
            linkDefinition(moduleName, FullName(name), srcPos, ann)
        case SIR.Let(recursivity, bindings, body, anns) =>
            bindings.foreach(b => traverseAndLink(b.value, srcPos))
            traverseAndLink(body, srcPos)
        case SIR.LamAbs(name, term, anns) => traverseAndLink(term, srcPos)
        case SIR.Apply(f, arg, tp, anns) =>
            traverseAndLink(f, srcPos)
            traverseAndLink(arg, srcPos)
        case SIR.And(lhs, rhs, anns) =>
            traverseAndLink(lhs, srcPos)
            traverseAndLink(rhs, srcPos)
        case SIR.Or(lhs, rhs, anns) =>
            traverseAndLink(lhs, srcPos)
            traverseAndLink(rhs, srcPos)
        case SIR.Not(term, anns) => traverseAndLink(term, srcPos)
        case SIR.IfThenElse(cond, t, f, tp, anns) =>
            traverseAndLink(cond, srcPos)
            traverseAndLink(t, srcPos)
            traverseAndLink(f, srcPos)
        case SIR.Decl(data, term) => traverseAndLink(term, srcPos)
        case SIR.Constr(name, data, args, tp, anns) =>
            try
                globalDataDecls.put(FullName(data.name), data)
                args.foreach(a => traverseAndLink(a, srcPos))
            catch
                case NonFatal(e) =>
                    println(s"Error in traverseAndLink: ${e.getMessage}")
                    println(s"SIR= ${sir}")
                    throw e
        case SIR.Match(scrutinee, cases, rhsType, anns) =>
            traverseAndLink(scrutinee, srcPos)
            cases.foreach(c => traverseAndLink(c.body, srcPos))
        case SIR.Select(scrutinee, _, _, _) =>
            traverseAndLink(scrutinee, srcPos)
        case _ => ()

    private def findAndLinkDefinition(
        defs: collection.Map[FullName, SIR],
        fullName: FullName,
        srcPos: SrcPos
    ): Boolean = {
        val found = defs.get(fullName)
        for sir <- found do
            globalDefs.update(fullName, CompileDef.Compiling)
            traverseAndLink(sir, srcPos)
            globalDefs.remove(fullName)
            globalDefs.update(
              fullName,
              CompileDef.Compiled(TopLevelBinding(fullName, Recursivity.Rec, sir))
            )
        found.isDefined
    }

    private def linkDefinition(
        moduleName: String,
        fullName: FullName,
        srcPos: SrcPos,
        anns: AnnotationsDecl
    ): Unit = {
        // println(s"linkDefinition: ${fullName}")
        retrieveModule(moduleName, srcPos) match
            case Left(filename) =>
                report.error(
                  s"Module not found during linking: ${moduleName}, missing filename: ${filename} referenced from ${anns.pos.file}: ${anns.pos.startLine}",
                  srcPos
                )
            case Right(defs) =>
                if !findAndLinkDefinition(defs, fullName, srcPos) then
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
        if (module.version._1 != SIRCompiler.SIRVersion._1)
            || (module.version._1 == SIRCompiler.SIRVersion._1
                && SIRCompiler.SIRVersion._2 < module.version._2)
        then
            report.error(
              s"""During linking I've found that a module '$moduleName' has an incompatible SIR version: ${module.version} (expected: ${SIRCompiler.SIRVersion}).
                   |This can happen if you try to link a module compiled with a different version of Scalus.
                   |Please, recompile the module with the version of Scalus that has the SIR version ${SIRCompiler.SIRVersion}
                   |""".stripMargin,
              srcPos
            )
    }

}
