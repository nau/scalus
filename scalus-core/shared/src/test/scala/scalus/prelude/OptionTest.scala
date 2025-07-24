package scalus.prelude

import scalus.*
import scalus.prelude.given
import scalus.sir.SIR
import scalus.uplc.Term
import prelude.Option as ScalusOption

import scala.language.implicitConversions

class OptionTest extends StdlibTestKit {
    test("eq") {
        assert((ScalusOption.empty[String]) === ScalusOption.None)
        assert(ScalusOption.Some(BigInt(1)) === ScalusOption.Some(BigInt(1)))
        assert(ScalusOption.Some(BigInt(1)) !== ScalusOption.Some(BigInt(2)))
        assertEval(ScalusOption.Some(true) !== ScalusOption.None)
        assertEval(ScalusOption.Some(true) === ScalusOption.Some(true))
        assertEval(ScalusOption.Some(true) !== ScalusOption.Some(false))
    }

    test("ord") {
        import prelude.OrdCompanion.given
        import ScalusOption.given
        check { (pair: (Int, Int)) =>
            val (left, right) = pair
            val leftOpt = prelude.Option(BigInt(left))
            val rightOpt = prelude.Option(BigInt(right))
            leftOpt.gt(ScalusOption.empty) && rightOpt.gt(
              ScalusOption.empty
            ) && (if left > right then {
                      leftOpt gt rightOpt
                  } else if left < right then {
                      leftOpt lt rightOpt
                  } else leftOpt equiv rightOpt)
        }

        assertEval(
          ScalusOption.Some(BigInt(-5)).gt(ScalusOption.None)
        )
        assertEval(
          ScalusOption.Some(BigInt(-5)).gt(ScalusOption.Some(BigInt(-10)))
        )
    }
}
