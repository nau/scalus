package scalus.cardano.ledger.rules

import org.scalatest.funsuite.AnyFunSuite

class StateTransitionSpec extends AnyFunSuite {
    test("simple rule") {
        StateTransition.run()
    }
}
