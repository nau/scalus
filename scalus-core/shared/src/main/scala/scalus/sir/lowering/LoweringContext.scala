package scalus.sir.lowering

import scalus.sir.*
import scalus.sir.lowering.typegens.SirTypeUplcGenerator
import scala.collection.mutable.Map as MutableMap

class LoweringContext(
    var zCombinatorNeeded: Boolean = false,
    val decls: MutableMap[String, DataDecl] = MutableMap.empty,
    var varIdSeq: Int = 0,
    var scope: LocalScope = LocalScope.empty,
    val plutusVersion: Int = 3,
    val generateErrorTraces: Boolean = false,
    val uplcGeneratorPolicy: SIRType => SirTypeUplcGenerator = SirTypeUplcGenerator(_),
    var typeUnifyEnv: SIRUnify.Env = SIRUnify.Env.empty,
    var debug: Boolean = false,
    var nestingLevel: Int = 0
) {

    def uniqueVarName(prefix: String = "_v"): String = {
        varIdSeq += 1
        s"$prefix$varIdSeq"
    }

    def lower(sir: SIR): LoweredValue = {
        Lowering.lowerSIR(sir)(using this)
    }

    def typeGenerator(sirType: SIRType): SirTypeUplcGenerator = {
        uplcGeneratorPolicy(sirType)
    }

    /** If this is typevariable, try get the value from context, else leave it as is.
      * @param tp
      * @return
      */
    def resolveTypeVarIfNeeded(tp: SIRType): SIRType = {
        tp match {
            case tv: SIRType.TypeVar =>
                typeUnifyEnv.filledTypes.get(tv) match
                    case Some(resolvedType) => resolvedType
                    case None               => tp // leave as is
            case _ =>
                tp // leave as is
        }
    }

    def tryResolveTypeVar(tp: SIRType.TypeVar): Option[SIRType] = {
        typeUnifyEnv.filledTypes.get(tp)
    }

    def log(msg: String): Unit = {
        val nestingPrefix = "  " * nestingLevel
        val msgLines = msg.split("\n")
        for line <- msgLines do {
            println(s"${nestingPrefix}${line}")
        }
    }

    def warn(msg: String, pos: SIRPosition): Unit = {
        println(s"warning: ${msg} at ${pos.show}")
    }

}
