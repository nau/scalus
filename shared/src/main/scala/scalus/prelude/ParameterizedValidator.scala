package scalus.prelude

import scalus.builtin.Data
import scalus.ledger.api.v3.FromDataInstances.given
import scalus.ledger.api.v3.*

@scalus.Compile
trait ParameterizedValidator[A] {

    def validate(param: A)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(currencySymbol) =>
                mint(param, sc.redeemer, currencySymbol, sc.txInfo)
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(param, datum, sc.redeemer, sc.txInfo, txOutRef)
            case ScriptInfo.RewardingScript(credential) =>
                reward(param, credential, sc.txInfo)
            case ScriptInfo.CertifyingScript(index, cert) =>
                certify(param, cert, sc.txInfo)
            case ScriptInfo.VotingScript(voter) =>
                vote(param, sc.redeemer, voter, sc.txInfo)
            case ScriptInfo.ProposingScript(index, procedure) =>
                propose(param, procedure, sc.txInfo)
    }

    def spend(
        param: A,
        datum: Option[Data],
        redeemer: Data,
        targetTxInfo: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Unit = {
        // send the script to the blockchain
        fail("Empty Validator.spend")
    }

    def mint(
        param: A,
        redeemer: Data,
        currencySymbol: CurrencySymbol,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.mint")
    }

    def reward(
        param: A,
        stakingKey: Credential,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.reward")
    }

    def certify(
        param: A,
        txCert: TxCert,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.certify")
    }

    def vote(
        param: A,
        redeemer: Data,
        voter: Voter,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.vote")
    }

    def propose(
        param: A,
        proposalProcedure: ProposalProcedure,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.propose")
    }

}

/** Validator, parametrized by Data. Keep
  *
  * Needed for compability with solutions which use existing uplc scripts and apply Data as
  * parameter on UPLC level.
  */
@scalus.Compile
trait DataParameterizedValidator {

    def validate(param: Data)(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(currencySymbol) =>
                mint(param, sc.redeemer, currencySymbol, sc.txInfo)
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(param, datum, sc.redeemer, sc.txInfo, txOutRef)
            case ScriptInfo.RewardingScript(credential) =>
                reward(param, credential, sc.txInfo)
            case ScriptInfo.CertifyingScript(index, cert) =>
                certify(param, cert, sc.txInfo)
            case ScriptInfo.VotingScript(voter) =>
                vote(param, sc.redeemer, voter, sc.txInfo)
            case ScriptInfo.ProposingScript(index, procedure) =>
                propose(param, procedure, sc.txInfo)
    }

    def spend(
        param: Data,
        datum: Option[Data],
        redeemer: Data,
        targetTxInfo: TxInfo,
        sourceTxOutRef: TxOutRef
    ): Unit = {
        // send the script to the blockchain
        fail("Empty Validator.spend")
    }

    def mint(
        param: Data,
        redeemer: Data,
        currencySymbol: CurrencySymbol,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.mint")
    }

    def reward(
        param: Data,
        stakingKey: Credential,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.reward")
    }

    def certify(
        param: Data,
        txCert: TxCert,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.certify")
    }

    def vote(
        param: Data,
        redeemer: Data,
        voter: Voter,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.vote")
    }

    def propose(
        param: Data,
        proposalProcedure: ProposalProcedure,
        txInfo: TxInfo
    ): Unit = {
        fail("Empty Validator.propose")
    }

}
