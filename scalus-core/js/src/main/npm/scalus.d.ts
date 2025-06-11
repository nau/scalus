// scalus.d.ts

/** Main API exported by Scalus */
export namespace Scalus {
  /** Execution budget representation. */
  export class ExBudget {
    constructor(cpu: bigint, memory: bigint);
    cpu: bigint;
    memory: bigint;
  }

  /** Script evaluation result. */
  export class Result {
    constructor(isSuccess: boolean, budget: ExBudget, logs: string[]);
    isSuccess: boolean;
    budget: ExBudget;
    logs: string[];
  }

  /**
   * Applies a data argument to a Plutus script given its double-CBOR-encoded hex.
   * @param doubleCborHex The double-CBOR-encoded hex of the script.
   * @param data The JSON-encoded data argument.
   * @returns The double-CBOR-encoded hex of the script with the data argument applied.
   */
  export function applyDataArgToScript(
    doubleCborHex: string,
    data: string,
  ): string;

  /**
   * Evaluates a Plutus script given its double-CBOR-encoded hex.
   * @param doubleCborHex The double-CBOR-encoded hex of the script.
   * @returns A Result object with the evaluation outcome, budget, and logs.
   */
  export function evaluateScript(doubleCborHex: string): Result;
}
