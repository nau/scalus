# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Scalus is a DApps development platform for Cardano that enables developers to write smart contracts in Scala 3, which are then compiled to Plutus Core. The codebase supports multi-platform development (JVM, JavaScript, Native) and provides a complete toolchain for Cardano smart contract development.

## Essential Commands

### Build and Development
```bash
# Clean compile and test everything (recommended before commits)
sbt precommit

# Continuous integration build (includes formatting checks)
sbt ci

# Compile with file-watch enabled
sbt ~compile

# Clean build
sbt clean

# Format all code
sbt scalafmtAll scalafmtSbt
```

### Testing
```bash
# Run all tests
sbt test

# Run tests for specific module
sbt scalus/test
sbt scalusExamples/test
sbt scalusTestkit/test

# Run tests with forking (recommended for integration tests)
sbt "Test/fork := true" test
```

### Platform-Specific Builds
```bash
# Build JavaScript library
sbt scalusJS/copyBundle

# Build native library
sbt scalusNative/nativeLink

# Prepare NPM package
sbt scalusJS/prepareNpmPackage
```

### Documentation
```bash
# Generate documentation website
sbt docs/docusaurusCreateSite

# Generate API documentation
sbt docs/unidoc
```

### Debugging
```bash
# Debug Scalus plugin during compilation
sbt -J-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 compile
```

## Architecture Overview

### Module Structure

- **`scalus-core/`** - Core platform with cross-compilation support (JVM/JS/Native)
  - Contains the Plutus VM implementation, UPLC evaluation, and standard library
  - Shared sources in `shared/` directory
  - Platform-specific implementations in `jvm/`, `js/`, `native/`

- **`scalus-plugin/`** - Scala 3 compiler plugin
  - Handles `@Compile` annotations and `Compiler.compile()` transformations
  - Compiles Scala code to Scalus Intermediate Representation (SIR)
  - Two-phase compilation: preparation and transformation

- **`scalus-examples/`** - Comprehensive smart contract examples
  - Multi-platform examples (JVM and JavaScript)
  - Integration examples with Cardano Client Lib

- **`scalus-testkit/`** - Testing utilities and property-based testing support

- **`scalus-cardano-ledger/`** - Cardano ledger domain models and CBOR serialization

- **`bloxbean-cardano-client-lib/`** - Integration with Bloxbean Cardano Client Library

### Compilation Pipeline

1. **Scala Source** → **SIR** (Scalus Intermediate Representation) via compiler plugin
2. **SIR** → **UPLC** (Untyped Plutus Core) via SIR compiler
3. **UPLC** → **Plutus Script** (V1/V2/V3) for Cardano deployment

### Key Source Locations

- **Core SIR definitions**: `scalus-core/shared/src/main/scala/scalus/sir/`
- **UPLC implementation**: `scalus-core/shared/src/main/scala/scalus/uplc/`
- **Plutus VM**: `scalus-core/shared/src/main/scala/scalus/uplc/eval/`
- **Compiler plugin**: `scalus-plugin/src/main/scala/scalus/plugin/`
- **Standard library**: `scalus-core/shared/src/main/scala/scalus/prelude/`
- **Examples**: `scalus-examples/shared/src/main/scala/scalus/examples/`

## Development Guidelines

### Smart Contract Development

- Use `@Compile` annotation on objects/methods to compile to Plutus Core
- Extend `Validator` trait for validator scripts
- Use `Compiler.compile()` to generate UPLC code
- Test contracts using the built-in Plutus VM or ScalaTest integration

### Plugin Development

- Shared sources are automatically copied from `scalus-core` to avoid circular dependencies
- Enable faster plugin development by uncommenting the dummy timestamp argument in `build.sbt`
- Use remote debugging for plugin development (see debugging commands above)

### Multi-Platform Development

- Shared sources go in `shared/src/main/scala/`
- Platform-specific code goes in `jvm/`, `js/`, or `native/` directories
- Use `crossProject` for modules that need multi-platform support

### Testing

- Use ScalaTest for unit testing
- Use ScalaCheck for property-based testing
- Integration tests should use `Test/fork := true`
- Use `scalusTestkit` for contract testing utilities

## Important Files

- **`build.sbt`** - Main build configuration with all modules and dependencies
- **`project/plugins.sbt`** - SBT plugins for cross-compilation and tooling
- **`.scalafmt.conf`** - Code formatting configuration
- **`CONTRIBUTING.md`** - Detailed contribution guidelines
- **`README.md`** - Project overview and examples

## Useful Aliases

The build defines several useful command aliases:
- `precommit` - Clean, format, compile, and test everything
- `ci` - Full CI build with formatting checks
- `mima` - Check binary compatibility

## Environment Setup

For development, use Nix for reproducible builds:
```bash
nix develop
```

Requires Java 11+, sbt 1.x, and optionally Cardano `uplc` CLI tool.