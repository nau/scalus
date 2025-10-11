import {readFileSync} from "node:fs";
//import {Scalus, SlotConfig} from "scalus";
const {Scalus, SlotConfig} = require("../../../../../scalus-cardano-ledger/js/src/main/npm/scalus-cardano-ledger-opt-bundle.js");

const script = "545301010023357389210753756363657373004981";
const applied = Scalus.applyDataArgToScript(script, JSON.stringify({"int": 42}));
const result = Scalus.evaluateScript(applied);
console.log(result);
console.log(result.budget.memory);

// Read CBOR files
const txBytes = Array.from(readFileSync("tx-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"));
const utxoBytes = Array.from(readFileSync("utxo-743042177a25ed7675d6258211df87cd7dcc208d2fa82cb32ac3c77221bd87c3.cbor"));
const slotConfig = SlotConfig.Mainnet;
// Evaluate all Plutus scripts in the transaction
console.log(Scalus.evalPlutusScripts(txBytes, utxoBytes, slotConfig));