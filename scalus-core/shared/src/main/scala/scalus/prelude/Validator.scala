package scalus.prelude

import scalus.builtin.Data
import scalus.ledger.api.v3.*

@scalus.Compile
//erased [not availanle uet]
trait Validator {

    inline def validate(scData: Data): Unit = {
        val sc = scData.to[ScriptContext]
        sc.scriptInfo match
            case ScriptInfo.MintingScript(policyId) =>
                mint(sc.redeemer, policyId, sc.txInfo)
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                spend(datum, sc.redeemer, sc.txInfo, txOutRef)
            case ScriptInfo.RewardingScript(credential) =>
                reward(sc.redeemer, credential, sc.txInfo)
            case ScriptInfo.CertifyingScript(index, cert) =>
                certify(sc.redeemer, cert, sc.txInfo)
            case ScriptInfo.VotingScript(voter) =>
                vote(sc.redeemer, voter, sc.txInfo)
            case ScriptInfo.ProposingScript(index, procedure) =>
                propose(procedure, sc.txInfo)
    }

    inline def spend(
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit
    // = {
    //    // send the script to the blockchain
    //    fail("Empty Validator.spend")
    // }

    inline def mint(
        redeemer: Data,
        policyId: PolicyId,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.mint")
    // }

    inline def reward(
        redeemer: Data,
        stakingKey: Credential,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.reward")
    // }

    inline def certify(
        redeemer: Data,
        cert: TxCert,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.certify")
    // }

    inline def vote(
        redeemer: Data,
        voter: Voter,
        tx: TxInfo
    ): Unit
    // =
    // {
    //    fail("Empty Validator.vote")
    // }

    inline def propose(
        proposalProcedure: ProposalProcedure,
        tx: TxInfo
    ): Unit
    // = {
    //    fail("Empty Validator.propose")
    // }

}
