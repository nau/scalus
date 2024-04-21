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