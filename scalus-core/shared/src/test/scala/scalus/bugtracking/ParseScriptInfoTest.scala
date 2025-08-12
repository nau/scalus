package scalus.bugtracking

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.builtin.ByteString.hex
import scalus.builtin.Data
import scalus.builtin.Data.*
import scalus.ledger.api.v3.*
import scalus.*
import scalus.prelude.*
import scalus.prelude.List.{Cons, Nil}
import scalus.uplc.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM
import scala.language.implicitConversions

@scalus.Compile
object ParseScriptInfo {

    def validate(scData: Data): Boolean = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(currencySymbol) =>
                mint(sc.redeemer, currencySymbol, sc.txInfo)
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, sc.redeemer, sc.txInfo, txOutRef)
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
        datum: Option[Data],
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

class ParseScriptInfoTest extends AnyFunSuite:

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("ScriptInfo parsing") {
        val scriptContext =
            ScriptContext(
              TxInfo(
                Nil,
                Nil,
                Nil,
                BigInt(0),
                Value.zero,
                Nil,
                SortedMap.empty,
                Interval.always,
                Cons(PubKeyHash(hex"deadbeef"), Nil),
                SortedMap.empty,
                SortedMap.empty,
                TxId(hex"bb"),
                SortedMap.empty,
                Nil,
                Option.None,
                Option.None
              ),
              hex"deadbeef".toData,
              ScriptInfo.SpendingScript(
                TxOutRef(TxId(hex"deadbeef"), 0),
                Option.Some(hex"aaaaaaaa".toData)
              )
            )

        val compiled = compile { ParseScriptInfo.validate }

        // println(compiled.pretty.render(100))
        // println(s"compiled type:  ${compiled.tp.show}")
        val term = compiled.toUplc()

        // println(term.pretty.render(100))

        val scriptContextData = scriptContext.toData
        val appliedValidator = term.plutusV3 $ scriptContextData

        // val appliedValidator = term $ Term.Const(Constant.Data(scriptContextData))
        given PlutusVM = PlutusVM.makePlutusV1VM()
        val result = appliedValidator.deBruijnedProgram.evaluateDebug
        // println(result)
        assert(result.isSuccess)
        assert(ParseScriptInfo.validate(scriptContext) == false)
    }

class ParseScriptInfoOldBackendTest extends AnyFunSuite:

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplc110Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    test("ScriptInfo parsing") {
        val scriptContext =
            ScriptContext(
              TxInfo(
                Nil,
                Nil,
                Nil,
                BigInt(0),
                Value.zero,
                Nil,
                SortedMap.empty,
                Interval.always,
                Cons(PubKeyHash(hex"deadbeef"), Nil),
                SortedMap.empty,
                SortedMap.empty,
                TxId(hex"bb"),
                SortedMap.empty,
                Nil,
                Option.None,
                Option.None
              ),
              hex"deadbeef".toData,
              ScriptInfo.SpendingScript(
                TxOutRef(TxId(hex"deadbeef"), 0),
                Option.Some(hex"aaaaaaaa".toData)
              )
            )

        val compiled = compile { ParseScriptInfo.validate }

        // println(compiled.pretty.render(100))
        // println(s"compiled type:  ${compiled.tp.show}")
        val term = compiled.toUplc()

        // println(term.pretty.render(100))

        val scriptContextData = scriptContext.toData
        val appliedValidator = term.plutusV3 $ scriptContextData

        // val appliedValidator = term $ Term.Const(Constant.Data(scriptContextData))
        given PlutusVM = PlutusVM.makePlutusV1VM()
        val result = appliedValidator.deBruijnedProgram.evaluateDebug
        // println(result)
        assert(result.isSuccess)
        assert(ParseScriptInfo.validate(scriptContext) == false)
    }
