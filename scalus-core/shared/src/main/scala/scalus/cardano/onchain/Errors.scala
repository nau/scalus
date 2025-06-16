package scalus.cardano.onchain

class OnchainError(msg: String) extends RuntimeException(msg) {
    def this() = this("ERROR")
}

class RequirementError(msg: String) extends OnchainError(msg) {
    def this() = this("Requirement error")
}

class ImpossibleLedgerStateError extends OnchainError("impossible ledger state error")
