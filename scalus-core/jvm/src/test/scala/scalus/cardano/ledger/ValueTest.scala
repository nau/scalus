package scalus.cardano.ledger

import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite

class ValueTest extends AnyFunSuite, ArbitraryInstances {
    test("Value addition is correct for pure ADA") {
        val ada1 = Value.lovelace(1000)
        val ada2 = Value.lovelace(2000)
        assert((ada1 + ada2) == Value.lovelace(3000))
    }

    test("Value addition is correct for multi-assets") {
        val policy1 = arbitrary[PolicyId].sample.get
        val asset1 = arbitrary[AssetName].sample.get
        val asset2 = arbitrary[AssetName].sample.get
        val multi1 = Value.fromPolicy(
          policy1,
          Map(asset1 -> 10L, asset2 -> 20L),
          Coin(1000)
        )
        val multi2 = Value.fromPolicy(
          policy1,
          Map(asset1 -> 5L, asset2 -> 15L),
          Coin(2000)
        )
        val expected = Value.fromPolicy(
          policy1,
          Map(asset1 -> 15L, asset2 -> 35L),
          Coin(3000)
        )
        assert((multi1 + multi2) == expected)
    }

    test("Value addition mixes ADA and multi-assets correctly") {
        val ada = Value.lovelace(500)
        val policy = arbitrary[PolicyId].sample.get
        val asset = arbitrary[AssetName].sample.get
        val multi = Value.asset(policy, asset, 50L, Coin(1000))
        val expected = Value.asset(policy, asset, 50L, Coin(1500))
        assert((ada + multi) == expected)
        assert((multi + ada) == expected)
    }

    test("Value equality checks") {
        val ada1 = Value.lovelace(1000)
        val ada2 = Value.lovelace(1000)
        val policy = arbitrary[PolicyId].sample.get
        val asset = arbitrary[AssetName].sample.get
        val multi1 = Value.asset(policy, asset, 10L, Coin(1000))
        val multi2 = Value.asset(policy, asset, 10L, Coin(1000))
        val multi3 = Value.lovelace(1000)

        assert(ada1 == ada1) // reflexive property
        assert(ada2 == ada1) // commutative property
        assert(ada1 == ada2)
        assert(multi1 == multi1) // reflexive property
        assert(multi2 == multi1) // commutative property
        assert(multi1 == multi2)
        assert(multi1 != ada1)
        assert(ada1 != multi1)
        assert(ada1 == multi3) // ADA is equal to multi-asset with no assets
        assert(multi3 == ada1) // commutative property
    }
}
