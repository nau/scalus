# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this
repository.

## Quick Reference for Claude Code

**Essential Commands:**
- `sbtn quick` - Format, compile, and test on JVM (use this frequently)
- `sbtn precommit` - Clean, format, compile, and test everything (ALWAYS run before completing tasks)
- `sbtn ci` - Full CI build with formatting checks
- `sbtn test` - Run all tests
- `sbtn scalafmtAll scalafmtSbt` - Format all code

**Key Patterns:**
- Always study existing code patterns before making changes
- Use `@Compile` annotation for Plutus smart contracts
- Extend `Validator` trait for validator scripts
- Place shared code in `shared/` directories for cross-platform support
- Use modern Scala 3 features: `given`, `using`, extension methods

## Project Overview

Scalus is a DApps development platform for Cardano that enables developers to write smart contracts
in Scala 3, which are then compiled to Plutus Core. The codebase supports multi-platform
development (JVM, JavaScript, Native) and provides a complete toolchain for Cardano smart contract
development.

## Key Principles for Claude Code

- **Always run `sbtn precommit` before considering any task complete** - This ensures formatting,
  compilation, and tests pass
- **Use existing patterns and conventions** - Study similar files before making changes
- **Prefer editing existing files over creating new ones** - Only create files when absolutely
  necessary
- **Follow Scala 3 idioms** - Use modern Scala 3 features like `given`, `using`, extension methods
- **Respect the multi-platform architecture** - Place shared code in `shared/` directories
- **Test thoroughly** - Use the existing test infrastructure and patterns
- **Follow Scalus Scala 3 formatting guideline** described on CONTRIBUTING.md

## Essential Commands

### Build and Development

```bash
# Format, compile on JVM, testQuick
sbtn quick

# Clean compile and test everything (recommended before commits)
sbtn precommit

# Continuous integration build (includes formatting checks)
sbtn ci

# Format all code
sbtn scalafmtAll scalafmtSbt
```

### Testing

```bash
# Run all tests
sbtn test

# Run tests for specific module
sbtn scalus/test
sbtn scalusExamples/test
sbtn scalusTestkit/test
sbtn jvm/test
```

### Platform-Specific Builds

```bash
# Build JavaScript library
sbtn scalusJS/copyBundle

# Build native library
sbtn scalusNative/nativeLink

# Prepare NPM package
sbtn scalusJS/prepareNpmPackage
```

### Documentation

```bash
# Generate documentation website
sbtn docs/mdoc
```

### Debugging

```bash
# Debug Scalus plugin during compilation
sbtn -J-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 compile
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

- **`scalus-cardano-ledger/`** - Cardano ledger rules and transaction builder

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

- `quick` - format, compile, and test everything on JVM
- `precommit` - Clean, format, compile, and test everything
- `ci` - Full CI build with formatting checks
- `mima` - Check binary compatibility

## Environment Setup

For development, use Nix for reproducible builds:

```bash
nix develop
```

Requires Java 11+, sbtn 1.x, and optionally Cardano `uplc` CLI tool.

## Claude Code Specific Guidelines

### When Working with Smart Contracts

- Look at existing examples in `scalus-examples/shared/src/main/scala/scalus/examples/`
- Smart contracts should extend appropriate traits like `Validator`
- Use `@Compile` annotation for Plutus compilation
- Test contracts using `scalusTestkit` utilities

### When Working with the Compiler Plugin

- Plugin code is in `scalus-plugin/src/main/scala/scalus/plugin/`
- Shared sources are automatically copied from `scalus-core` - don't edit them directly
- Use debugging commands for plugin development
- Plugin tests are in `scalus-plugin-tests/`

### When Working with Core Platform

- Core logic is in `scalus-core/shared/src/main/scala/`
- Platform-specific implementations go in respective `jvm/`, `js/`, `native/` directories
- SIR (Scalus Intermediate Representation) code is in `scalus/sir/`
- UPLC implementation is in `scalus/uplc/`

### File Organization Patterns

- Follow the existing package structure: `scalus.{module}.{functionality}`
- Tests mirror source structure but in `test/` instead of `main/`
- Use object-oriented design with case classes for data structures
- Prefer sealed traits for sum types

### Common Tasks

- **Adding a new validator**: Create in `scalus-examples`, extend `Validator`, add tests
- **Modifying UPLC evaluation**: Edit files in `scalus-core/shared/src/main/scala/scalus/uplc/eval/`
- **Adding standard library functions**: Add to `scalus-core/shared/src/main/scala/scalus/prelude/`
- **Plugin modifications**: Work in `scalus-plugin/`, test with `scalus-plugin-tests/`

### Before Submitting Changes

```bash
# Always run this before considering work complete
sbtn quick

# If quick fails, investigate and fix issues:
sbtn scalafmtAll scalafmtSbt  # Fix formatting
sbtn compile                 # Check compilation
sbtn test                    # Run tests
```