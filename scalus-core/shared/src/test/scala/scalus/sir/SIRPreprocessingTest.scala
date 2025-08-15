package scalus.sir

import org.scalatest.funsuite.AnyFunSuite

class SIRPreprocessingTest extends AnyFunSuite {

    test("check that module SIR is written by compiler into the module val") {
        val listSirModule: scalus.sir.Module = scalus.prelude.List.sirModule
        assert(listSirModule != null)
        assert(listSirModule.defs.head.name.startsWith("scalus.prelude.List$."))
        val optionSirModule: scalus.sir.Module = scalus.prelude.Option.sirModule
        assert(optionSirModule != null)

    }

}
