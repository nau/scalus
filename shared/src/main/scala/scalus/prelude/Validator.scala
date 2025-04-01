package scalus.prelude

import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.*

trait Validator {

    @scalus.Compile
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
