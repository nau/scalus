package scalus.bugtracking

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.builtin.ByteString.hex
import scalus.builtin.Data
import scalus.builtin.ToData.toData
import scalus.builtin.ToDataInstances.given
import scalus.builtin.given
import scalus.builtin.PlatformSpecific.given
import scalus.examples.PubKeyValidator
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.ToDataInstances.given
import scalus.*
import scalus.prelude.*
import scalus.prelude.List.{Cons, Nil}
import scalus.prelude.Prelude.{*, given}
import scalus.uplc.*
import scalus.uplc.TermDSL.*
import scalus.uplc.eval.PlutusVM

@scalus.Compile
object ParseScriptInfo {

    def validate(sc: ScriptContext): Boolean = {
        sc.scriptInfo match
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, sc.redeemer, sc.txInfo, txOutRef)
            case ScriptInfo.MintingScript(currencySymbol) =>
                mint(sc.redeemer, currencySymbol, sc.txInfo)
            case ScriptInfo.RewardingScript(credential) =>
                reward(credential, sc.txInfo)
            case ScriptInfo.CertifyingScript(index, cert) =>
                certify(cert, sc.txInfo)
            case ScriptInfo.VotingScript(voter) =>
                vote(sc.redeemer, voter, sc.txInfo)
            case ScriptInfo.ProposingScript(index, procedure) =>
                propose(procedure, sc.txInfo)
    }

    def spend(
        datum: Maybe[Data],
        redeemer: Data,
        targetTxInfo: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Boolean = {
        // send the script to the blockchain
        false
    }

    def mint(
        redeemer: Data,
        currencySymbol: CurrencySymbol,
        txInfo: TxInfo
    ): Boolean = {
        false
    }

    def reward(
        stakingKey: Credential,
        txInfo: TxInfo
    ): Boolean = {
        false
    }

    def certify(
        txCert: TxCert,
        txInfo: TxInfo
    ): Boolean = {
        false
    }

    def vote(
        redeemer: Data,
        voter: Voter,
        txInfo: TxInfo
    ): Boolean = {
        false
    }

    def propose(
        proposalProcedure: ProposalProcedure,
        txInfo: TxInfo
    ): Boolean = {
        false
    }

}

class PsrseScriptInfoSpec extends AnyFunSuite:

    test("PubKey Validator example") {
        val scriptContext =
            ScriptContext(
              TxInfo(
                Nil,
                Nil,
                Nil,
                BigInt(0),
                Value.zero,
                Nil,
                AssocMap.empty,
                Interval.always,
                Cons(PubKeyHash(hex"deadbeef"), Nil),
                AssocMap.empty,
                AssocMap.empty,
                TxId(hex"bb"),
                AssocMap.empty,
                Nil,
                Maybe.Nothing,
                Maybe.Nothing
              ),
              (hex"deadbeef").toData,
              ScriptInfo.SpendingScript(
                TxOutRef(TxId(hex"deadbeef"), 0),
                Maybe.Just(hex"aaaaaaaa".toData)
              )
            )

        val compiled = compile { ParseScriptInfo.validate }

        println(compiled.pretty.render(100))
        val term = compiled.toUplc()

        println(term.pretty.render(100))

        val scriptContextData = scriptContext.toData
        val appliedValidator = term.plutusV3 $ Term.Const(Constant.Data(scriptContextData))
        given PlutusVM = PlutusVM.makePlutusV1VM()
        val result = appliedValidator.deBruijnedProgram.evaluateDebug
        println(result)
        assert(result.isSuccess)
        assert(ParseScriptInfo.validate(scriptContext) == false)
    }
