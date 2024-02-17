# Changelog

## 0.5.0 (2024-02-17)

### Added

- better error messages in the compiler
- Scalus Intermediate Representation (SIR) now has a version number (requires recompilation of existing SIR)
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
