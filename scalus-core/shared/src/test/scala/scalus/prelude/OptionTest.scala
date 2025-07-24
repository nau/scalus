package scalus.prelude

import scalus.*
import scalus.prelude.given
import scalus.sir.SIR
import scalus.uplc.Term

import scala.language.implicitConversions

class OptionTest extends StdlibTestKit {
    test("eq") {
        assert((prelude.Option.empty[String]) === prelude.Option.None)
        assert(prelude.Option.Some(BigInt(1)) === prelude.Option.Some(BigInt(1)))
        assert(prelude.Option.Some(BigInt(1)) !== prelude.Option.Some(BigInt(2)))
        assertEval(prelude.Option.Some(true) !== prelude.Option.None)
        assertEval(prelude.Option.Some(true) === prelude.Option.Some(true))
        assertEval(prelude.Option.Some(true) !== prelude.Option.Some(false))
    }
}
