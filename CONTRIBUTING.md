# Contributing to Scalus

## Pre-requisites

- Java 11+, sbt 1.x
- Cardano `uplc` CLI tool

## Env setup with Nix

```bash
nix develop
```

## Build

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
version := "0.6.2-SNAPSHOT",
```

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

In sbt shell

```bash
jmh:run -i 1 -wi 1 -f 1 -t 1
```
