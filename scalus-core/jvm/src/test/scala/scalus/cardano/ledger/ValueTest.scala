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
        val multi1 = Value(Coin(1000), Map(policy1 -> Map(asset1 -> 10, asset2 -> 20)))
        val multi2 = Value(Coin(2000), Map(policy1 -> Map(asset1 -> 5, asset2 -> 15)))
        val expected = Value(Coin(3000), Map(policy1 -> Map(asset1 -> 15, asset2 -> 35)))
        assert((multi1 + multi2) == expected)
    }

    test("Value addition mixes ADA and multi-assets correctly") {
        val ada = Value.lovelace(500)
        val policy = arbitrary[PolicyId].sample.get
        val asset = arbitrary[AssetName].sample.get
        val multi = Value(Coin(1000), Map(policy -> Map(asset -> 50)))
        val expected = Value(Coin(1500), Map(policy -> Map(asset -> 50)))
        assert((ada + multi) == expected)
        assert((multi + ada) == expected)
    }

    test("Value equality checks") {
        val ada1 = Value.lovelace(1000)
        val ada2 = Value.lovelace(1000)
        val policy = arbitrary[PolicyId].sample.get
        val asset = arbitrary[AssetName].sample.get
        val multi1 = Value(Coin(1000), Map(policy -> Map(asset -> 10)))
        val multi2 = Value(Coin(1000), Map(policy -> Map(asset -> 10)))
        val multi3 = Value(Coin(1000), Map.empty)

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
