# Contributing to Scalus

## Env setup

```bash
nix develop
```

## Docusaurus

Run locally

```bash
cd website
yarn run serve
```

## Deploy to GitHub Pages

```bash
cd website
 USE_SSH=true yarn deploy
```

## Run benchmarks

```bash
jmh:run -i 1 -wi 1 -f 1 -t 1