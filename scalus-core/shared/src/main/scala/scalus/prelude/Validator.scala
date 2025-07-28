package scalus.prelude

import scalus.builtin.Data
import scalus.ledger.api.v3.*

@scalus.Compile
trait Validator {

    def validate(scData: Data): Unit = {
        log("Validator.validate:0")
        val sc = scData.to[ScriptContext]
        log("Validator.validate:1")
        sc.scriptInfo match
            case ScriptInfo.MintingScript(currencySymbol) =>
                val validator2_1 = log("Validator.validate:2.1")
                mint(sc.redeemer, currencySymbol, sc.txInfo)
            case ScriptInfo.SpendingScript(txOutRef, datum) =>
                val validator2_2 = log("Validator.validate:2.2")
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
        redeemer: Data,
        stakingKey: Credential,
        tx: TxInfo
    ): Unit = {
        fail("Empty Validator.reward")
    }

    def certify(
        redeemer: Data,
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
