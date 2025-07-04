# Project Close-out Report

## Multiplatform Plutus Script Cost & Evaluation Library (JS/JVM/LLVM)

IdeaScale
Link: [https://cardano.ideascale.com/c/cardano/idea/111227](https://cardano.ideascale.com/c/cardano/idea/111227)

Project Number: 1100198

Project Manager: Alexander Nemish

Date Project Started: 2024-03-11

Date Project Completed: 2025-06-23

## Project Description

At the moment of project initiation, there was no library to evaluate Plutus scripts and its
execution budget except Haskell implementation.

The goal of this project was to create a multiplatform library for evaluating Plutus scripts and
calculating their execution budget on JVM, JavaScript, and Native platforms. The library is written
in Scala 3 and compiles to all three platforms, providing a unified codebase that significantly
reduces code duplication and enhances developer productivity.

## List of Challenge/Project KPIs and How the Project Addressed Them

The key performance indicators (KPIs) for this project were:

1. **Plutus Script Evaluation:** The library must accurately evaluate Plutus V1 and V2 scripts,
   including built-in functions and data models.
2. **Execution Budget Calculation:** The library must calculate the execution budget for Plutus
   scripts, ensuring compatibility with Cardano's cost model.
3. **Testing and Validation:** The library must pass all Plutus V1 and V2 conformance tests and
   be tested against a substantial number of Mainnet scripts to ensure reliability and accuracy.
4. **Cross-platform Compatibility:** The library must compile and run on JVM, JavaScript, and Native
   platforms
5. **Publish Platform Artifacts:**  The library must be published as platform artifacts for JVM and
   JavaScript platforms, ensuring easy integration into existing projects.

We successfully addressed these KPIs through the following achievements:

1. Robust Scala 3 implementation of Plutus V1/V2 CEK virtual machine.
2. Complete implementation of Plutus V1/V2 built-in functions and data model.
3. Comprehensive Plutus V1/V2 cost models and execution budget calculation logic.
4. UPLC parser and pretty-printer implementation.
5. Successful compilation to JavaScript using [Scala.js](https://www.scala-js.org/).
6. Successful compilation to native binaries via [Scala Native](https://scala-native.org/en/stable/)
   and [LLVM](https://llvm.org/).
7. Full compatibility and availability for JVM languages, including Java and Kotlin.
8. Extensive testing using property-based testing with [ScalaCheck](https://www.scalacheck.org/)
   and [ScalaTest](https://www.scalatest.org/), and comparing results with the Haskell `uplc` CLI
   tool to ensure we are getting the same results as the reference implementation.
9. Passing all Plutus V1 and V2
   [conformance tests](https://github.com/nau/scalus/blob/a87f25a91e58311cfac341ef893dc5ce1af0e571/scalus-core/shared/src/test/scala/scalus/uplc/eval/PlutusConformanceTest.scala#L17)
10. Extensive testing on \~100k mainnet Plutus scripts, ensuring high accuracy and reliability.
11. Publishing platform artifacts on [Maven Central](https://central.sonatype.com/) for JVM,
    and [NPM](https://www.npmjs.com/) for JavaScript platforms, making the library easily accessible
    for developers.
12. Integration into the
    BloxBean [Cardano Client Library (CCL)](https://cardano-client.dev/docs/integrations/scalus-integration-api) -
    a JVM transaction building library as a **TransactionEvaluator** implementation. This
    integration allows developers to calculate the exact execution budget and fees during
    Transaction construction, eliminating the need for querying an external service and speed up
    transaction building.

## Key Achievements

### Cross-platform Plutus Execution

Provided robust implementations of Plutus V1/V2 Virtual Machine across JVM, JavaScript, and Native
platforms from the same codebase.

### Developer Efficiency

Enabled significant code reuse through multiplatform Scala 3 compilation, enhancing developer
productivity.

### Accuracy and Reliability

Passed all Plutus V1 and
V2 [Plutus Conformance](https://github.com/nau/scalus/blob/a87f25a91e58311cfac341ef893dc5ce1af0e571/scalus-core/shared/src/test/scala/scalus/uplc/eval/PlutusConformanceTest.scala#L17),
demonstrating compatibility with the reference Haskell implementation.

Implemented thorough testing against \~100k Cardano mainnet scripts to ensure correctness and
reliability.

### Adoption

Scalus ability to calculate Plutus script budgets and evaluate scripts has been integrated into
BloxBean [Cardano Client Library (CCL)](https://cardano-client.dev/docs/integrations/scalus-integration-api)
and is used in production by Cardano Foundation.

Current applications and protocols using Scalus include:

* **[Hydrozoa Protocol](https://github.com/cardano-hydrozoa/hydrozoa/)** A simplified and modern
  Hydra implementation.
* **[Binocular](https://github.com/lantr-io/binocular/)** A decentralized optimistic Bitcoin oracle
  on Cardano.
* **[Cosmex](https://cosmex.io)** A specialized L2 off-chain order book exchange protocol.

## Key Learnings

Compiling Scala 3 to JVM, JavaScript, and Native from a unified codebase is achievable and highly
beneficial, significantly improving maintainability and reducing code duplication. Ensuring platform
agnosticism requires deliberate planning but is feasible with careful architectural considerations.

Overall, Scala 3's multiplatform capabilities have proven to be a powerful tool for building
cross-platform libraries, enabling us to deliver a robust and efficient solution for Plutus script
evaluation and budget calculation across multiple platforms.

## Next Steps for the Product/Service Developed

Ongoing development of Scalus includes the recent implementation of Plutus V3 CEK machine and
Conway era built-in functions on JVM and JavaScript platforms.

Future integration plans involve Scalus JS library integration
into [Lucid Evolution](https://github.com/no-witness-labs/lucid-evolution)
and [Mesh.JS](https://meshjs.dev/) for enhanced script budget evaluation capabilities.

## Final Thoughts/Comments

Catalyst funding significantly accelerated our project's development, providing essential resources
that led to substantial adoption and validation.

Completion of this project makes Scalus the *only* end‑to‑end multiplatform and well tested
Plutus execution library available today.

We remain committed to enhancing Scalus further as the leading platform for full-stack Cardano DApp
development.

## Links to Other Relevant Project Sources or Documents

* [Scalus GitHub Repository](https://github.com/nau/scalus)
* [Scalus Website](https://scalus.org)
* [Scalus API Reference](https://scalus.org/api/index.html)
* [Discord Community](https://discord.gg/ygwtuBybsy)
* [Scalus Starter Project](https://github.com/lantr-io/scalus-starter)
* [Lantr](https://lantr.io)
* [BloxBean Cardano Client Library](https://cardano-client.dev)

## Link to Close-out Video

Here is the link to the full Scalus multiplatform capabilities video
presentation: <https://youtu.be/A8MQsGn5XFo>

There I demonstrate the Plutus V2 script evaluation and execution cost calculation on 3 platforms:
JVM, JavaScript, and Native.
