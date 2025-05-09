package scalus.prelude

import scalus.builtin.Data
import scalus.ledger.api.v3.*

@scalus.Compile
trait Validator {

    def validate(scData: Data): Unit = {
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
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {
        // send the script to the blockchain
        fail("Empty Validator.spend")
    }

    def mint(
        redeemer: Data,
        currencySymbol: CurrencySymbol,
        tx: TxInfo
    ): Unit = {
        fail("Empty Validator.mint")
    }

    def reward(
        stakingKey: Credential,
        tx: TxInfo
    ): Unit = {
        fail("Empty Validator.reward")
    }

    def certify(
        cert: TxCert,
        tx: TxInfo
    ): Unit = {
        fail("Empty Validator.certify")
    }

    def vote(
        redeemer: Data,
        voter: Voter,
        tx: TxInfo
    ): Unit = {
        fail("Empty Validator.vote")
    }

    def propose(
        proposalProcedure: ProposalProcedure,
        tx: TxInfo
    ): Unit = {
        fail("Empty Validator.propose")
    }

}
