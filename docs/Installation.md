---
sidebar_position: 1
---
# Quick Start

## Prerequisites

You need a usual Scala development environment: JDK, [sbt](https://www.scala-sbt.org/), scala-cli an IDE of your choice.

You also can use [Scala CLI](https://scala-cli.virtuslab.org/) instead of SBT.
See [AdaStream](https://github.com/nau/adastream) for an example of a project that uses Scala CLI.

The easiest way to get started is to use [Nix](https://nixos.org/) package manager.

## Clone Scalus Starter Project

Clone the [Scalus Starter Project](https://github.com/nau/scalus-starter) to get started with Scalus.

If you use [Nix](https://nixos.org/), we provided `flakes.nix` file to get a development environment with all the required tools installed.

```bash
git clone https://github.com/nau/scalus-starter.git
cd scalus-starter
nix develop
code .
scala-cli run .
```

## Sbt project setup

Add the following to your `build.sbt` file:

```scala
scalaVersion := "@SCALA3_VERSION@"
libraryDependencies += "org.scalus" %% "scalus" % "@VERSION@"
addCompilerPlugin("org.scalus" %% "scalus-plugin" % "@VERSION@")
```

That's it! You can now start using Scalus in your project.
