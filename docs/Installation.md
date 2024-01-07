---
sidebar_position: 1
---
# Installation

## Prerequisites

You need a usual Scala development environment: JDK, SBT, IDE.

You also can use [Scala CLI](https://scala-cli.virtuslab.org/) instead of SBT.
See [AdaStream](https://github.com/nau/adastream) for an example of a project that uses Scala CLI.

The easiest way to get started is to use [Nix](https://nixos.org/) package manager.

## Nix (recommended)

Clone the [Scalus Starter Project](https://github.com/nau/scalus-starter) to get started with Scalus.

Run `nix develop` to enter a development environment with all the required tools installed: JDK, sbt, Cardano node, cardano-cli, etc.

```bash
git clone https://github.com/nau/scalus-starter.git
cd scalus-starter
nix develop
code .
```

## Sbt project setup

Add the following to your `build.sbt` file:

```scala
scalaVersion := "@SCALA3_VERSION@"
libraryDependencies += "org.scalus" %% "scalus" % "@VERSION@"
addCompilerPlugin("org.scalus" %% "scalus-plugin" % "@VERSION@")
```
