# Scalus - Cardano Plutus Script Evaluator for JavaScript

Scalus provides a JavaScript/TypeScript interface for evaluating Cardano Plutus scripts and calculating execution budgets.

## Installation

```bash
npm install scalus @noble/curves
```

**Note:** `@noble/curves` is a peer dependency and must be installed separately. This keeps the bundle size smaller and allows you to control which version of the cryptographic library to use.

## Usage

### Basic Script Evaluation

```typescript
import { Scalus } from 'scalus';

// Evaluate a Plutus script
const result = Scalus.evaluateScript(doubleCborHex);

if (result.isSuccess) {
  console.log('Script executed successfully');
  console.log('CPU budget:', result.budget.cpu);
  console.log('Memory budget:', result.budget.memory);
} else {
  console.log('Script failed:', result.logs);
}
```

### Apply Data Argument to Script

```typescript
import { Scalus } from 'scalus';

// Apply a data argument to a script
const scriptWithArg = Scalus.applyDataArgToScript(
  scriptDoubleCborHex,
  dataJson
);
```

### Evaluate Transaction Scripts

```typescript
import { Scalus, SlotConfig } from 'scalus';

// Evaluate all Plutus scripts in a transaction
const slotConfig = new SlotConfig(1000, 1596059091000);
const redeemers = Scalus.evalPlutusScripts(
  txCborBytes,
  utxoCborBytes,
  slotConfig
);

redeemers.forEach(redeemer => {
  console.log(`${redeemer.tag}[${redeemer.index}]:`);
  console.log('  CPU:', redeemer.budget.cpu);
  console.log('  Memory:', redeemer.budget.memory);
});
```

## API Reference

### `Scalus.evaluateScript(doubleCborHex: string): Result`

Evaluates a Plutus script from its double-CBOR-encoded hex representation.

**Parameters:**
- `doubleCborHex`: The double-CBOR-encoded hex string of the Plutus script

**Returns:** A `Result` object containing:
- `isSuccess`: boolean indicating success or failure
- `budget`: `ExBudget` with `cpu` and `memory` (as BigInt)
- `logs`: Array of log messages

### `Scalus.applyDataArgToScript(doubleCborHex: string, dataJson: string): string`

Applies a data argument to a Plutus script.

**Parameters:**
- `doubleCborHex`: The double-CBOR-encoded hex string of the Plutus script
- `dataJson`: JSON representation of the Data argument

**Returns:** The double-CBOR-encoded hex string of the script with the argument applied

### `Scalus.evalPlutusScripts(txCborBytes: Uint8Array, utxoCborBytes: Uint8Array, slotConfig: SlotConfig): Array<Redeemer>`

Evaluates all Plutus scripts in a transaction.

**Parameters:**
- `txCborBytes`: CBOR bytes of the transaction
- `utxoCborBytes`: CBOR bytes of the UTxO map
- `slotConfig`: Slot configuration (zero time and slot length)

**Returns:** Array of `Redeemer` objects containing:
- `tag`: Redeemer tag (string)
- `index`: Redeemer index (number)
- `budget`: `ExBudget` with execution costs

## Bundle Size

The bundle size is approximately 1.6 MB (optimized). The @noble/curves cryptographic library is externalized and must be installed separately, which helps keep the core bundle smaller and allows for better caching.

## License

Apache-2.0

## Repository

https://github.com/nau/scalus
