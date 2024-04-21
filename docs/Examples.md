---
sidebar_position: 3
---
# Examples

## AdaStream

## AdaStream Example

[AdaStream](https://github.com/nau/adastream) - Decentralized File Hosting Incentivised via Cardano Ada Payments

This project is a Cardano implementation of the [BitStream](https://github.com/RobinLinus/BitStream) protocol by Robin
Linus.

Original paper: [BitStream: Decentralized File Hosting Incentivised via Bitcoin Payments
](https://robinlinus.com/bitstream.pdf)

### TL;DR

* Alice wants to buy a file from Bob.
* Bob encrypts the file with a random key and sends it to Alice.
* Bob creates a bond contract on Cardano with a collateral and a commitment to the key and the file.
* Alice pays Bob for the file via a HTLC (Hash Time Lock Contract), using Cardano or Bitcoin Lightning Network.
* Alice decrypts the file with the key from the HTLC or takes the money back after the timeout.
* If Bob cheats, Alice can prove it and get the collateral from the bond contract.
* Bob can withdraw the collateral by revealing the key.

The project includes a bond contract and a HTLC contract for a fair exchange of files for ADA.

It's a CLI tool and a REST API server that allows you to create a bond contract, pay for a file, and decrypt it.

It has a set of tests that check the contract logic and its execution costs.

## PreImage Validator

[PreImage Validator](https://github.com/nau/scalus/blob/ce7a37edb06ef2e39794825ee4f81ff061198666/jvm/src/test/scala/scalus/PreImageExampleSpec.scala)

## Minting Policy

[MintingPolicy](https://github.com/nau/scalus/blob/612b4bd581c55cb6c68339247cfecfbe22e4e61d/shared/src/main/scala/scalus/examples/MintingPolicy.scala)
