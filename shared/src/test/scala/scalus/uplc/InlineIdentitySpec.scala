package scalus
package uplc

import scalus.uplc.Term.*
import org.scalatest.funsuite.AnyFunSuite

class InlineIdentitySpec extends AnyFunSuite {
    test("inlineIdentity should inline identity function application") {
        val term = Apply(LamAbs("x", Var(NamedDeBruijn("x"))), Var(NamedDeBruijn("y")))
        val expected = Var(NamedDeBruijn("y"))
        assert(InlineIdentity.inlineIdentity(term) == expected)
    }
}
