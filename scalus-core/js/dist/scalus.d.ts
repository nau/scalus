/** The main namespace/object exported by scalus */
export namespace Scalus {
  /** Representation of evaluation budget as BigInt values. */
  export class ExBudget {
    constructor(cpu: bigint, memory: bigint);
    cpu: bigint;
    memory: bigint;
  }

  /** Result of script evaluation. */
  export class Result {
    constructor(isSuccess: boolean, budget: ExBudget, logs: string[]);
    isSuccess: boolean;
    budget: ExBudget;
    logs: string[];
  }

  /** Evaluate a script given as CBOR hex, returns a Result. */
  export function evaluateScript(scriptCborHex: string): Result;
}
