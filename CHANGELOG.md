# Changelog

## 0.11.0 (2025-07-28)

### Added

- new more efficient UPLC codegen
- CIP-57 Blueprints support
- improved Scalus standard library and tests
- `PlutusScriptEvaluator` for evaluating Plutus scripts using Scalus ledger domain types
- Arbitrary instances for Plutus domain types

### Fixed

- avoid expensive G1/G2 zero/generator constants initialization

### Changed

- Plutus `Value` uses `SortedMap` instead of `AssocMap`

## 0.10.4 (2025-07-16)

### Fixed

- large BigInt encoding occasionally added an extra byte to the CBOR encoding
- mint value doesn't contain 0 Ada in Plutus V3 scripts

## 0.10.3 (2025-07-16)

### Added

- updated to a new Maven Central repository

## 0.10.2 (2025-07-07)

### Added

- lazy cost model loading in `ScalusTransactionEvaluator`

### Fixed

- BLS builtins serialization
- cost parameters for `divideInteger`, `modInteger`, `quotientInteger`, `remainderInteger`

## 0.10.1 (2025-06-04)

### Added

- more Cardano ledger rules

### Changed

- moved Cardano Ledger domain types to scalus project
- refactor Hash types in ledger domain types

## 0.10.0 (2025-05-28)

### Added

- improved Scalus standard library
- Scalus Design Patterns project
- Scalus Examples: HTLC, PaymentSplitter
- Scalus benchmarks: Knights and Clausify
- new UPLC 1.1 generation `SirToUplc110Lowering` (experimental)
- Cardano Ledger domain types with CBOR codecs (experimental)
- Scalus Bilinear Accumulator implementation (experimental)

### Changed

- simplify `ToData`/`FromData` derivation allowing Scala 3 `derives`
- updated to Scala 3.3.6, Scala.js 1.19.0

### Fixed

- support for Scala argumentless functions in Scalus compiler

## 0.9.0 (2025-04-17)

### Added

- all Plutus V3 builtins from the Plomin hard fork
- improved Scalus standard library
- support for TupleN types
- support for destructuring: val (a, b) = Foo(2, “b”)
- BLS12-381 primitives work on JVM and Javascript
- Groth16 SNARK implementation works on JVM and Javascript
- Cardano native scripts support: Timelock
- advanced UPLC optimization: case/constr instead of apply
- generate better error traces for UPLC
- PaymentSplitter example
- simple Validator trait for writing validators
- Scalus Testkit: a simple test framework for testing Plutus scripts

### Changed

- better SIR code generation (remove recursivity)
- better UPLC optimizations: eta-reduce
- better error messages in the compiler
- renamed `Maybe` to `Option` to follow Scala naming conventions
- cardano-client-lib 0.6.4

### Fixed

- bug with Select fields during linking of SIR

## 0.8.5 (2025-02-16)

### Added

- Scalus Scala Native support with all Plutus V1/V2 builtins
- a C example using libscalus native library for script evaluation and budget calculation
- compile synthetic DataType.apply as a primary constructor
- better error messages during Scalus on-chain script compilation
- async-profiler support for profiling

### Changed

- dependencies updated: Scala 3.3.5, Scala.js 1.18.2
- speedup MemoryUsage calculation

### Fixed

- removed unnecessary bitcoin-s-crypto dependency
- Data.unit type is Data, not Constr

## 0.8.4 (2025-01-05)

### Added

- CIP-117: fail on non-unit results in Plutus V3 scripts

### Fixed

- OptimizingSirToUplcLowering threw an exception when generateErrorTraces was enabled

## 0.8.3 (2024-12-20)

### Added

- customizable UPLC Inliner optimization pass
- Prelude List functions: single, cons, length, map2
- experimental Groth16 SNARK implementation
- Data `toCbor` and `fromCbor` extensions
- add generated ScalaDocs to website

### Changed

- code is updated to Scala 3.5+
- use latest dependencies

### Fixed

- BLS12-381 G1/G2 compressed zero points encoding

## 0.8.2 (2024-10-11)

Nothing changed, just pushed the wrong commit under 0.8.1.

## 0.8.1 (2024-10-11)

### Fixed

- cost model for `integerToBytestring` and `bytestringToInteger`
- flat encoding of SIR `Data` was wrong for `Map` and `List` types

## 0.8.0 (2024-10-03)

### Added

- initial Plutus V3 support:
    - all builtins work on JVM
    - V3 `ScriptContext` etc for script compilation
    - V3 script evaluation and cost calculation
- Cardano Client Lib can use `ScalusTransactionEvaluator` for V3 script evaluation
- improved `deriveEnum` for `ToData` and `FromData` (see Tutorial)

### Changed

- few renames and API changes in `scalus-bloxbean-cardano-client-lib` `Interop` module

## 0.7.2 (2024-08-11)

### Added

- `VM.evaluateDebug` with useful info about the execution: budget, costs, builtins, logs
- `eval`, `evalDebug`, `show`, `showHightlighted` extensions for `Term` and `Program`
- `toUplcOptimized` extension for `SIR` to generate optimized UPLC code
- `?` tracing operator and `log` function similar to Aiken's `?` operator

### Changed

- don't remove top-level eta-expansions in `compile` method

### Fixed

- fieldAsData/field didn't work for aliaed types
- small fix in `OptimizingSirToUplcLowering` with tracing errors
- multiple inner matches in pattern matching code generation

## 0.7.1 (2024-08-04)

### Added

- implemented all Plutus V1/V2 builtins on JVM and JavaScript platforms
- passing all Plutus Conformance Tests on JVM and JavaScript platforms
- `OptimizingSirToUplcLowering` SIR to UPLC lowering with built-in optimizations
- PlutusV3 constr/case support
- `Data.to*` extension methods

### Changed

- speedup `UplcParser` by 10x

### Fixed

- use Java 11 on Github CI and release builds

## 0.7.0 (2024-05-29)

Scalus CEK and cost calculation implementation is now feature complete.

We were able to validate transactions from whole Cardano Epoch 484 using Scalus and Cardano Client
Lib.

### Added

- `ScalusTransactionEvaluator` - Cardano Client Lib (CCL) `TransactionEvaluator` implementation. You
  can now use Scalus
  to evaluate scripts and their costs off-chain during transaction building.
- SIR and UPLC optimizations: `RemoveRecursivity`, `EtaReduce`
- `evaluateScriptRestricting` mode
- Advanced documentation
- `Data` JSON codec

### Changed

- optimized From/ToData instances to generate less code
- `PlutusV1Params`, `PlutusV2Params` are now classes with Long fields
- Removed type parameter from `Interval`
- bytesToHex uses lowercase hex characters

### Fixed

- memoryUsageInteger was wrong in certain cases
- sliceByteString wrong order of parameters
- Data CBOR encoding of large integers must be chunked by 64 byte chunks
- typo in `ConstAboveDiagonal` cost calculation implementation

## 0.6.1 (2024-04-18)

### Added

- compile `==`, `!=` operators as Plutus builtins for `Boolean`, `ByteString`, `String` and `Data`
  types
- `++` operator, `size` and `length` functions for `ByteString`

### Fixed

- wrong `ConstAboveDiagonal` cost calculation used in division

## 0.6.0 (2024-04-05)

### Added

- fast debruijned CEK machine implementation
- CEK machine benchmarking, runs on par with high performance Haskell implementation
- CEK machine execution budget calculation
- cost model for CEK machine loading from Cardano CLI and Blockfrost API JSONs
- updated to Scala 3.3.3
- updated to Scala.js 1.16.0, 3x reduction in JS bundle size

### Changed

- lots of internal refactoring

## 0.5.0 (2024-02-17)

### Added

- better error messages in the compiler
- Scalus Intermediate Representation (SIR) now has a version number (requires recompilation of
  existing SIR)
- SIR and UPLC pretty printers can print normal and syntax highlighted code using XTerm colors
- UPLC pretty printer outputs a better indented code
- added Blake2b 224 builtin implementation on JVM
- updated dependencies: Scala 3.3.2
- use uplc cli from Plutus 1.15.0.1

### Changed

- moved scalus.uplc.Data to scalus.builtin.Data
- renamed scalus.builtins to scalus.builtin

### Fixed

- Plutus Data Map can have duplicate keys; fixed Data CBOR codec to handle this case.
