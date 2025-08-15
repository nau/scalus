package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.transform.FirstTransform
import scalus.sir.{Module as SIRModule, *}
import scalus.utils.HashConsedReprRefFlat
import scalus.flat.FlatInstantces.ModuleHashSetReprFlat
import scalus.sir.SIRType.Integer

/** Preprocess SIR - run before the Pickiing and sbt.ExtreactApi phases and add toSIR-compiled
  * modules fields, which later set in Scalus phase and used during linking.
  *
  * from
  * ```
  * @Compile
  * object Mybjs {
  *   ....
  * }
  * ```
  * to
  *
  * ```
  * @Compile
  * object Mybjs {
  *   ...
  *   val sirHash_: Int = hash
  *   val sir_: SIR = [NOT-SET]
  *   val deps_: List[SIRCompiled] = [NOT-SETT]
  * }
  * ```
  * sir_ and dpes_ wi
  */
class SIRPreprocessor(thisPhase: ScalusPreparePhase, debugLevel: Int)(using ctx: Context) {

    val compileAnnot = requiredClassRef("scalus.Compile").symbol.asClass
    val ignoreAnnotRef = requiredClassRef("scalus.Ignore")
    val ignoreAnnot = ignoreAnnotRef.symbol.asClass
    // val sirType = requiredClassRef("scalus.sir.SIR")
    val sirModuleType = requiredClassRef("scalus.sir.Module")
    // val nakedMapType = requiredClassRef("scala.collection.immutable.Map")
    val sirCompiledType = requiredClassRef("scalus.sir.SIRCompiled")
    val sirCompiledListType =
        AppliedType(requiredClassRef("scala.collection.immutable.List"), List(sirCompiledType))

    def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree = {
        // If the template has a compile annotation, we need to add a variable for SIR
        tree.rhs match
            case template: tpd.Template =>
                // add sir to the end of the definition
                val templateHash = template.hashCode()
                val sirHashSym = Symbols
                    .newSymbol(
                      tree.symbol,
                      s"sirhash_${templateHash}".toTermName,
                      Flags.Permanent,
                      defn.IntType
                    )
                sirHashSym.addAnnotation(ignoreAnnot)
                sirHashSym.enteredAfter(thisPhase)
                val sirHashVar = tpd
                    .ValDef(
                      sirHashSym,
                      Literal(Constant(templateHash)).withSpan(template.span)
                    )
                    .withSpan(template.span)
                val sirSym = Symbols
                    .newSymbol(
                      tree.symbol,
                      Plugin.SIR_MODULE_VAL_NAME.toTermName,
                      Flags.Lazy | Flags.Permanent,
                      sirModuleType
                    )
                sirSym.addAnnotation(ignoreAnnot)
                sirSym.enteredAfter(thisPhase)
                val module = SIRModule((1, 1), List.empty)
                val moduleToExprSym = Symbols.requiredModule("scalus.sir.ModuleToExpr")
                val moduleTree =
                    convertFlatToTree(
                      module,
                      ModuleHashSetReprFlat,
                      moduleToExprSym,
                      tree.span,
                      debugLevel > 0
                    )
                val sirModuleVal = tpd
                    .ValDef(sirSym, moduleTree)
                    .withSpan(tree.span)
                val sirDepsSym = Symbols
                    .newSymbol(
                      tree.symbol,
                      Plugin.SIR_DEPS_VAL_NAME.toTermName,
                      Flags.Lazy | Flags.Permanent,
                      sirCompiledListType
                    )
                sirDepsSym.addAnnotation(ignoreAnnot)
                sirDepsSym.enteredAfter(thisPhase)
                val sirDepsVal = tpd
                    .ValDef(
                      sirDepsSym,
                      tpd.ref(defn.NilModule).withSpan(tree.span)
                    )
                    .withSpan(tree.span)
                val newTemplate = cpy.Template(template)(
                  body = template.body ++ List(sirHashVar, sirModuleVal, sirDepsVal)
                )
                val retval = cpy.TypeDef(tree)(name = tree.name, rhs = newTemplate)
                retval
            case _ =>
                report.warning(
                  s"ScalusPrepare: Expected a template for type definition, but found: ${tree.show}",
                  tree.srcPos.startPos
                )
                tree
    }

}
