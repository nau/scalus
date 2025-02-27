# Contributing to Scalus

## Pre-requisites

- Java 11+, sbt 1.x
- Cardano `uplc` CLI tool and Nix

## Env setup with Nix

Make sure that you have installed Nix package manager (see https://nixos.org/ ) and enabled `nix-command` and `flake`
features and
IOHK binary cache.

Typical Nix config can looks like:

```
substituters = https://cache.iog.io https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-substituters = https://cache.iog.io https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
experimental-features = nix-command flakes
accept-flake-config = true
allow-import-from-derivation = true
```

Usually you already have this setup if you have build cardano-node or cardano-cli locally.

```bash
nix develop
```

## Build

Before committing changes, make sure that the code is formatted, compiles and the tests pass:

```bash
sbt precommit
```

## Scalus Plugin Development

During compiler plugin development you want to automatically recompile the dependencies of the plugin.

For faster development make sure that in `build.sbt`:

1. `scalacOptions` contains `-Xplugin` with the path to the plugin jar with the dummy argument.

```scala
Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")
```

1. scalusPlugin project version is not manually set

This line should be commented out in scalusPlugin project settings:

```scala
version := "0.6.2-SNAPSHOT"
,
```

### Debugging Scalus Plugin during compilation

* Run sbt with the following command:

```bash
sbt -J-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005 compile
```

This makes the compiler wait for a debugger to attach on port 5005.

* Set Breakpoints in IntelliJ
* In IntelliJ, create a Remote Debug configuration (host: localhost, port: 5005) and start it.
* Once attached, resume execution to hit your breakpoints.

## Docusaurus

Run locally

```bash
cd website
yarn install
yarn run serve
```

## Deploy to GitHub Pages

```bash
cd website
USE_SSH=true yarn deploy
```

## Run benchmarks

Measurement of throughput:

```bash
sbt 'bench/jmh:run -i 1 -wi 1 -f 1 -t 1 .*'
```

Where `.*` is a regexp for benchmark names.

Profiling with [async-profiler](https://github.com/async-profiler/async-profiler) that should be downloaded from
[nightly builds](https://github.com/async-profiler/async-profiler/releases/tag/nightly) and unpacked to some directory,
like `/opt/async-profiler` for Linux in the command bellow:

```bash
sbt 'bench/jmh:run -prof "async:event=cycles;=dir=target/async-reports;interval=1000000;output=flamegraph;libPath=/opt/async-profiler/lib/libasyncProfiler.so" -jvmArgsAppend "-XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints" -f 1 -wi 1 -i 1 -t 1 .*'
```

On MacOS use this command in sbt shell:

```bash
bench/jmh:run -prof "async:event=itimer;dir=target/async-reports;interval=1000000;output=flamegraph;libPath=/nix/store/mr0adcvnv8pkalfbhsgm9p762rs2pyzg-async-profiler-3.0/lib/libasyncProfiler.dylib" -jvmArgsAppend "-XX:+UnlockDiagnosticVMOptions -XX:+DebugNonSafepoints"   -f 1 -wi 1 -i 1 -t 1 .*
```

Resulting interactive flame graphs will be stored in the `bench/target/async-reports` subdirectory of the project.

For benchmarking of allocations use `event=alloc` instead of `event=cycles` option in the command above.
