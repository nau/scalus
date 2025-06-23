# Project Close-out Report

## Multiplatform Plutus Script Cost & Evaluation Library (JS/JVM/LLVM)

IdeaScale
Link: [https://cardano.ideascale.com/c/cardano/idea/111227](https://cardano.ideascale.com/c/cardano/idea/111227)

## Project Number

1100198

## Name of Project Manager

Alexander Nemish

## Date Project Started

2024-03-11

## Date Project Completed

2025-06-23

## List of Challenge KPIs and How the Project Addressed Them

* **Cross-platform Plutus Execution:** Provided robust implementations of Plutus V1 and V2 across JVM, JavaScript, and
  Native platforms.
* **Developer Efficiency:** Enabled significant code reuse through multiplatform Scala 3 compilation, enhancing
  developer productivity.
* **Accuracy and Reliability:** Implemented thorough testing against \~100k Cardano mainnet scripts to ensure
  correctness and reliability.

## List of Project KPIs and How the Project Addressed Them

* **Compilation to Plutus Core:** Successfully compiled Scala 3 code to Plutus Core (V1, V2, V3).
* **Cross-platform Support:** JVM, JavaScript, and Native platforms fully supported with minimal platform-specific code.
* **Testing and Validation:** Implemented property-based testing frameworks and accurate execution budget evaluation
  mechanisms.
* **User Adoption:** Developed Scalus Starter Project, extensive documentation, and multiple real-world examples
  demonstrating practical usage.

## Key Achievements

1. Robust Scala 3 implementation of Plutus V1 and V2 CEK virtual machines.
2. Complete implementation of Plutus V1/V2 built-in functions and data model.
3. Comprehensive Plutus V1/V2 cost models and budget evaluation logic.
4. UPLC parser and pretty-printer implementation.
5. Successful compilation to JavaScript using Scala.js.
6. Successful compilation to native binaries via Scala Native and LLVM.
7. Full compatibility and availability for JVM languages, including Java and Kotlin.
8. Extensive testing on \~100k mainnet Plutus scripts, ensuring high accuracy and reliability.

These achievements collectively create a unique, multiplatform Cardano DApp development library.

## Key Learnings

Compiling Scala 3 to JVM, JavaScript, and Native from a unified codebase is achievable and highly beneficial,
significantly improving maintainability and reducing code duplication. Ensuring platform agnosticism requires deliberate
planning but is feasible with careful architectural considerations.

## Next Steps for the Product/Service Developed

Ongoing development of Scalus includes the recent implementation of Plutus V3 CEK machine and built-in functions for JVM
and JavaScript. Current applications and protocol integrations include:

* **Binocular:** A decentralized optimistic Bitcoin oracle on Cardano.
* **Hydrozoa Protocol:** A simplified and modern Hydra implementation.

Future integration plans involve Scalus JS library integration into Lucid Evolution and Mesh.JS for enhanced script
budget evaluation capabilities.

## Final Thoughts/Comments

Catalyst funding significantly accelerated our project's development, providing essential resources that led to
substantial adoption and validation. We remain committed to enhancing Scalus further as the leading platform for
full-stack Cardano DApp development.

## Links to Other Relevant Project Sources or Documents

* [Scalus GitHub Repository](https://github.com/nau/scalus)
* [Scalus Website](https://scalus.org)
* [Scalus API Reference](https://scalus.org/api/index.html)
* [Discord Community](https://discord.gg/ygwtuBybsy)
* [Scalus Starter Project](https://github.com/lantr-io/scalus-starter)
* [Lantr](https://lantr.io)

## Link to Close-out Video

Here is the link to the full Scalus multiplatform capabilities video presentation: <https://youtu.be/A8MQsGn5XFo>

There I demonstrate the Plutus V2 script evaluation and execution cost calculation on 3 platforms: JVM, JavaScript, and
Native.
