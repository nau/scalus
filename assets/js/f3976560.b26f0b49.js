"use strict";(self.webpackChunkscalus_website=self.webpackChunkscalus_website||[]).push([[795],{9536:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>o,contentTitle:()=>s,default:()=>h,frontMatter:()=>r,metadata:()=>t,toc:()=>c});const t=JSON.parse('{"type":"mdx","permalink":"/","source":"@site/src/pages/index.md","title":"Scalus - DApps Development Platform for Cardano","description":"CI/Release","frontMatter":{"hide_table_of_contents":false,"toc_max_heading_level":2},"unlisted":false}');var i=n(4848),l=n(8453);const r={hide_table_of_contents:!1,toc_max_heading_level:2},s="Scalus - DApps Development Platform for Cardano",o={},c=[{value:"Vision",id:"vision",level:2},{value:"Write Cardano smart contracts in Scala 3",id:"write-cardano-smart-contracts-in-scala-3",level:2},{value:"How It Works",id:"how-it-works",level:2},{value:"Features",id:"features",level:2},{value:"Scalus Starter Project",id:"scalus-starter-project",level:2},{value:"Show Me The Code",id:"show-me-the-code",level:2},{value:"Preimage Validator Example",id:"preimage-validator-example",level:3},{value:"Scalus for budget calculation with Cardano Client Lib",id:"scalus-for-budget-calculation-with-cardano-client-lib",level:3},{value:"AdaStream Example",id:"adastream-example",level:3},{value:"TL;DR",id:"tldr",level:4},{value:"Minting/Burning Example",id:"mintingburning-example",level:3},{value:"Minimal Size Withdrawal Validator",id:"minimal-size-withdrawal-validator",level:3},{value:"Scalus Native",id:"scalus-native",level:2},{value:"How to build a native library",id:"how-to-build-a-native-library",level:3},{value:"Roadmap",id:"roadmap",level:2},{value:"Efficiently convert user defined data types from/to Plutus Data",id:"efficiently-convert-user-defined-data-types-fromto-plutus-data",level:3},{value:"Single transaction building and signing API for backend and frontend",id:"single-transaction-building-and-signing-api-for-backend-and-frontend",level:3},{value:"DApp development framework",id:"dapp-development-framework",level:3},{value:"Comparison to PlutusTx, Aiken, Plutarch",id:"comparison-to-plutustx-aiken-plutarch",level:2},{value:"PlutusTx",id:"plutustx",level:3},{value:"Aiken",id:"aiken",level:3},{value:"Plutarch",id:"plutarch",level:3},{value:"Plu-ts",id:"plu-ts",level:3},{value:"Scalus",id:"scalus",level:3},{value:"Support",id:"support",level:2}];function d(e){const a={a:"a",code:"code",h1:"h1",h2:"h2",h3:"h3",h4:"h4",header:"header",img:"img",li:"li",p:"p",pre:"pre",ul:"ul",...(0,l.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(a.header,{children:(0,i.jsx)(a.h1,{id:"scalus---dapps-development-platform-for-cardano",children:"Scalus - DApps Development Platform for Cardano"})}),"\n",(0,i.jsxs)(a.p,{children:[(0,i.jsx)(a.img,{src:"https://github.com/nau/scalus/actions/workflows/release.yml/badge.svg",alt:"CI/Release"}),"\n",(0,i.jsx)(a.img,{src:"https://img.shields.io/maven-central/v/org.scalus/scalus_3",alt:"Maven Central"}),"\n",(0,i.jsx)(a.a,{href:"https://discord.gg/ygwtuBybsy",children:(0,i.jsx)(a.img,{src:"https://img.shields.io/discord/1105852427346911252.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2",alt:"Discord"})})]}),"\n",(0,i.jsx)(a.h2,{id:"vision",children:"Vision"}),"\n",(0,i.jsx)(a.p,{children:"Scalus is a platform for developing decentralized applications (DApps) on the Cardano blockchain."}),"\n",(0,i.jsx)(a.p,{children:"The goal is to make a full-stack development experience for Cardano DApps as smooth as possible.\nUsing the same language, tools and code for frontend, backend and smart contracts development."}),"\n",(0,i.jsx)(a.h2,{id:"write-cardano-smart-contracts-in-scala-3",children:"Write Cardano smart contracts in Scala 3"}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsx)(a.li,{children:"Scala is a modern functional programming language that runs on JVM, JavaScript and natively via LLVM."}),"\n",(0,i.jsx)(a.li,{children:"Reuse your Scala code for your validator, frontend and backend."}),"\n",(0,i.jsx)(a.li,{children:"Write once, run anywhere. True full-stack development."}),"\n",(0,i.jsx)(a.li,{children:"Scala's powerful type system helps you write correct code."}),"\n",(0,i.jsx)(a.li,{children:"Benefit from a huge ecosystem of libraries and tools."}),"\n",(0,i.jsxs)(a.li,{children:["Utilize testing frameworks like ScalaTest and ScalaCheck\nfor ",(0,i.jsx)(a.a,{href:"https://en.wikipedia.org/wiki/Property-based_testing",children:"property-based testing"}),"."]}),"\n",(0,i.jsx)(a.li,{children:"Enjoy comprehensive IDE support: IntelliJ IDEA, VSCode and syntax highlighting on GitHub."}),"\n",(0,i.jsx)(a.li,{children:"Advanced debugging support."}),"\n",(0,i.jsx)(a.li,{children:"Enhanced code formatting and linting, navigation, and refactoring."}),"\n",(0,i.jsx)(a.li,{children:"Scala code coverage and profiling tools."}),"\n"]}),"\n",(0,i.jsx)(a.h2,{id:"how-it-works",children:"How It Works"}),"\n",(0,i.jsx)(a.p,{children:"Scalus compiles a subset of Scala code to Plutus Core, the language of Cardano smart contracts."}),"\n",(0,i.jsx)(a.p,{children:"Scalus gives full control over the generated Plutus Core code.\nWrite efficient and compact smart contracts and squeeze the most out of the Cardano blockchain."}),"\n",(0,i.jsx)(a.h2,{id:"features",children:"Features"}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsx)(a.li,{children:"Scala 3 to Cardano Plutus Core compiler"}),"\n",(0,i.jsx)(a.li,{children:"Standard library for Plutus contracts development"}),"\n",(0,i.jsx)(a.li,{children:"Plutus V1, V2 and V3 support"}),"\n",(0,i.jsx)(a.li,{children:"Plutus VM Interpreter and execution budget calculation for Plutus V1, V2 and V3"}),"\n",(0,i.jsx)(a.li,{children:"Plutus VM library works on JVM, JavaScript and Native platforms!"}),"\n",(0,i.jsx)(a.li,{children:"Property-based testing library"}),"\n",(0,i.jsx)(a.li,{children:"Untyped Plutus Core (UPLC) data types and functions"}),"\n",(0,i.jsx)(a.li,{children:"Flat, CBOR, JSON serialization"}),"\n",(0,i.jsx)(a.li,{children:"UPLC parser and pretty printer"}),"\n",(0,i.jsx)(a.li,{children:"Type safe UPLC expression builder, think of Plutarch"}),"\n",(0,i.jsxs)(a.li,{children:["Bloxbean ",(0,i.jsx)(a.a,{href:"https://cardano-client.dev",children:"Cardano Client Lib"})," integration"]}),"\n"]}),"\n",(0,i.jsx)(a.h2,{id:"scalus-starter-project",children:"Scalus Starter Project"}),"\n",(0,i.jsxs)(a.p,{children:["You can use the ",(0,i.jsx)(a.a,{href:"https://github.com/nau/scalus-starter",children:"Scalus Starter Project"})," to get started with Scalus.\nClone the repository and follow the instructions in the README."]}),"\n",(0,i.jsx)(a.h2,{id:"show-me-the-code",children:"Show Me The Code"}),"\n",(0,i.jsx)(a.h3,{id:"preimage-validator-example",children:"Preimage Validator Example"}),"\n",(0,i.jsxs)(a.p,{children:["Here is a simple validator that checks that an signer of ",(0,i.jsx)(a.code,{children:"pkh"})," PubKeyHash provided a preimage of a ",(0,i.jsx)(a.code,{children:"hash"})," in a ",(0,i.jsx)(a.code,{children:"redeemer"}),".\nBelow example is taken from ",(0,i.jsx)(a.a,{href:"https://github.com/nau/scalus/blob/master/shared/src/main/scala/scalus/examples/PreimageValidator.scala#L19",children:"PreimageValidator"})]}),"\n",(0,i.jsx)(a.pre,{children:(0,i.jsx)(a.code,{className:"language-scala",metastring:"3",children:'def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit =\n  // deserialize from Data\n  val (hash, pkh) = datum.to[(ByteString, ByteString)]\n  val preimage = redeemer.toByteString\n  val ctx = ctxData.to[ScriptContext]\n  // get the transaction signatories\n  val signatories = ctx.txInfo.signatories\n  // check that the transaction is signed by the public key hash\n  List.findOrFail(signatories) { sig => sig.hash == pkh }\n  // check that the preimage hashes to the hash\n  if sha2_256(preimage) == hash then ()\n  else throw new RuntimeException("Wrong preimage")\n// throwing an exception compiles to UPLC error\n\n// compile to Untyped Plutus Core (UPLC)\nval compiled = compile(preimageValidator).toUplc()\n// create a validator script, Plutus program version 1.0.0\nval validator = Program((1, 0, 0), compiled)\n// HEX encoded Plutus script, ready to be used in with cardano-cli or Blockfrost\nval plutusScript = validator.doubleCborHex\n// Create a Cardano .plutus file for this validator\nvalidator.writePlutusFile(path, PlutusLedgerLanguage.PlutusV2)\n'})}),"\n",(0,i.jsxs)(a.p,{children:["Look at ",(0,i.jsx)(a.a,{href:"https://github.com/nau/scalus/blob/master/examples/src/main/scala/scalus/examples/SendTx.scala#L31",children:"SendTx"})," example for a full example of how to create a transaction with this validator."]}),"\n",(0,i.jsx)(a.h3,{id:"scalus-for-budget-calculation-with-cardano-client-lib",children:"Scalus for budget calculation with Cardano Client Lib"}),"\n",(0,i.jsxs)(a.p,{children:["Scalus can calculate the execution budget for your validator using the Cardano Client Lib. Just provide ",(0,i.jsx)(a.code,{children:"ScalusTransactionEvaluator"})," to your ",(0,i.jsx)(a.code,{children:"QuickTxBuilder"}),":"]}),"\n",(0,i.jsx)(a.pre,{children:(0,i.jsx)(a.code,{className:"language-scala",metastring:"3",children:"val signedTx = quickTxBuilder\n        .compose(scriptTx)\n        .withTxEvaluator(ScalusTransactionEvaluator(protocolParams, utxoSupplier))\n        // build your transaction\n        .buildAndSign()\n"})}),"\n",(0,i.jsx)(a.p,{children:"This will calculate the execution budget for your validator and add it to the redeemer of the transaction."}),"\n",(0,i.jsx)(a.h3,{id:"adastream-example",children:"AdaStream Example"}),"\n",(0,i.jsxs)(a.p,{children:["Sources: ",(0,i.jsx)(a.a,{href:"https://github.com/nau/adastream/blob/main/src/contract.scala",children:"AdaStream Contract"})]}),"\n",(0,i.jsxs)(a.p,{children:["This project is a Cardano implementation of the ",(0,i.jsx)(a.a,{href:"https://github.com/RobinLinus/BitStream",children:"BitStream"})," protocol by Robin Linus, inventor of ",(0,i.jsx)(a.a,{href:"https://bitvm.org/",children:"BitVM"})]}),"\n",(0,i.jsxs)(a.p,{children:["Original paper: ",(0,i.jsx)(a.a,{href:"https://robinlinus.com/bitstream.pdf",children:"BitStream: Decentralized File Hosting Incentivised via Bitcoin Payments\n"})]}),"\n",(0,i.jsx)(a.h4,{id:"tldr",children:"TL;DR"}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsx)(a.li,{children:"Alice wants to buy a file from Bob."}),"\n",(0,i.jsx)(a.li,{children:"Bob encrypts the file with a random key and sends it to Alice."}),"\n",(0,i.jsx)(a.li,{children:"Bob creates a bond contract on Cardano with a collateral and a commitment to the key and the file."}),"\n",(0,i.jsx)(a.li,{children:"Alice pays Bob for the file via a HTLC (Hashed Timelock Contract), using Cardano or Bitcoin Lightning Network."}),"\n",(0,i.jsx)(a.li,{children:"Alice decrypts the file with the key from the HTLC or takes the money back after the timeout."}),"\n",(0,i.jsx)(a.li,{children:"If Bob cheats, Alice can prove it and get the collateral from the bond contract."}),"\n",(0,i.jsx)(a.li,{children:"Bob can withdraw the collateral by revealing the key."}),"\n"]}),"\n",(0,i.jsx)(a.p,{children:"The project includes a bond contract and a HTLC contract for a fair exchange of files for ADA or other Cardano Native Tokens."}),"\n",(0,i.jsx)(a.p,{children:"It's a CLI tool and a REST API server that allows you to create a bond contract, pay for a file, and decrypt it."}),"\n",(0,i.jsx)(a.p,{children:"It has a set of tests that check the contract logic and its execution costs."}),"\n",(0,i.jsx)(a.h3,{id:"mintingburning-example",children:"Minting/Burning Example"}),"\n",(0,i.jsx)(a.p,{children:"Here is a full example of a token minting/burning validator that works on both JVM and JavaScript:"}),"\n",(0,i.jsx)(a.p,{children:(0,i.jsx)(a.a,{href:"https://github.com/nau/scalus/blob/master/shared/src/main/scala/scalus/examples/MintingPolicy.scala",children:"MintingPolicy.scala"})}),"\n",(0,i.jsxs)(a.p,{children:["And here is a project that uses it in web frontend:\n",(0,i.jsx)(a.a,{href:"https://github.com/nau/scalus/tree/master/examples-js",children:"Scalus Minting Example"})]}),"\n",(0,i.jsx)(a.h3,{id:"minimal-size-withdrawal-validator",children:"Minimal Size Withdrawal Validator"}),"\n",(0,i.jsx)(a.p,{children:"The challenge was to create the smallest possible validator that checks a certain withdrawal exists in the transaction."}),"\n",(0,i.jsx)(a.p,{children:(0,i.jsx)(a.a,{href:"https://gist.github.com/nau/b8996fe3e51b0e21c20479c5d8548ec7",children:"The result is 92 bytes long script"})}),"\n",(0,i.jsx)(a.pre,{children:(0,i.jsx)(a.code,{className:"language-scala",metastring:"3",children:"val validator = compile:\n  (script_withdrawal_credential: Data, datum: Data, redeemer: Data, ctx: Data) =>\n    def list_has(list: List[Pair[Data, Data]]): Unit =\n      if list.head.fst == script_withdrawal_credential then ()\n      else list_has(list.tail) // fails on empty list\n\n    inline def withdrawal_from_ctx =\n      unMapData(fieldAsData[ScriptContext](_.txInfo.withdrawals)(ctx))\n    list_has(withdrawal_from_ctx)\n"})}),"\n",(0,i.jsx)(a.h2,{id:"scalus-native",children:"Scalus Native"}),"\n",(0,i.jsx)(a.p,{children:"Scalus implements a Plutus VM (CEK machine) that works on JVM, JavaScript and Native platforms.\nAll from the same Scala codebase."}),"\n",(0,i.jsx)(a.p,{children:"Here's how you can evaluate a Plutus script from a C program:"}),"\n",(0,i.jsx)(a.pre,{children:(0,i.jsx)(a.code,{className:"language-c",children:'#include "scalus.h"\n// Plutus V3, protocol version 10\nmachine_params* params = scalus_get_default_machine_params(3, 10); \nex_budget budget;\nchar logs_buffer[1024];\nchar error[1024];\nint ret = scalus_evaluate_script(\n    script, // script hex\n    3, // Plutus V3\n    params2, // machine params\n    &budget,\n    logs_buffer, sizeof(logs_buffer),\n    error, sizeof(error));\n\nif (ret == 0) {\n    printf("Script evaluation successful. CPU %lld, MEM %lld\\n", budget.cpu, budget.memory);\n    printf("Logs: %s\\n", logs_buffer);\n} else {\n    printf("Script evaluation failed: %d\\n", ret);\n    printf("Units spent: CPU %lld, MEM %lld\\n", budget.cpu, budget.memory);\n    printf("Error: %s\\n", error);\n    printf("Logs: %s\\n", logs_buffer);\n}\n'})}),"\n",(0,i.jsxs)(a.p,{children:["See the full example in the ",(0,i.jsx)(a.a,{href:"https://github.com/nau/scalus/blob/master/examples-native/main.c",children:"main.c"})," file."]}),"\n",(0,i.jsx)(a.h3,{id:"how-to-build-a-native-library",children:"How to build a native library"}),"\n",(0,i.jsx)(a.pre,{children:(0,i.jsx)(a.code,{className:"language-shell",children:"sbt scalusNative/nativeLink\n"})}),"\n",(0,i.jsxs)(a.p,{children:["will produce a shared library in the ",(0,i.jsx)(a.code,{children:"native/target/scala-3.3.4"})," directory."]}),"\n",(0,i.jsx)(a.h2,{id:"roadmap",children:"Roadmap"}),"\n",(0,i.jsx)(a.h3,{id:"efficiently-convert-user-defined-data-types-fromto-plutus-data",children:"Efficiently convert user defined data types from/to Plutus Data"}),"\n",(0,i.jsx)(a.p,{children:"Now, Scalus takes the same approach as PlutusTx.\nThis change makes it similar to Aiken, which will result in smaller and more efficient Plutus scripts in most cases."}),"\n",(0,i.jsx)(a.h3,{id:"single-transaction-building-and-signing-api-for-backend-and-frontend",children:"Single transaction building and signing API for backend and frontend"}),"\n",(0,i.jsx)(a.p,{children:"This will allow you to build and sign transactions in Scala and JavaScript using the same code."}),"\n",(0,i.jsx)(a.h3,{id:"dapp-development-framework",children:"DApp development framework"}),"\n",(0,i.jsx)(a.p,{children:"A framework that will help you build DApps faster and easier."}),"\n",(0,i.jsx)(a.p,{children:"You define your smart contracts, data types, and interaction endpoints in Scala.\nThe framework will generate the frontend and backend code for you."}),"\n",(0,i.jsx)(a.p,{children:"Yes, your REST API, WebSocket, and GraphQL endpoints for your DApp.\nAnd the JavaScript code to interact with your DApp from the browser."}),"\n",(0,i.jsx)(a.h2,{id:"comparison-to-plutustx-aiken-plutarch",children:"Comparison to PlutusTx, Aiken, Plutarch"}),"\n",(0,i.jsx)(a.h3,{id:"plutustx",children:"PlutusTx"}),"\n",(0,i.jsx)(a.p,{children:"PlutusTx compiles almost any Haskell program to UPLC.\nCons are that you can barely understand how the UPLC is generated and how to make it smaller.\nPlutusTx also tends to generate a lot of boilerplate code making the final script size bigger and more expensive to run."}),"\n",(0,i.jsx)(a.h3,{id:"aiken",children:"Aiken"}),"\n",(0,i.jsx)(a.p,{children:"Aiken is a new and young programming language which is a pro and a con.\nCan only be used for writing on-chain smart contracts.\nCan't reuse code for on-chain and off-chain parts.\nDoesn't have macros.\nDoesn't allow a low-level control over the generated UPLC code."}),"\n",(0,i.jsx)(a.h3,{id:"plutarch",children:"Plutarch"}),"\n",(0,i.jsx)(a.p,{children:"Plutarch is very low-level. Use it when you need precise control over a script generation.\nIt's a Haskell library so be prepared to write Haskell code with all its developer experience drawbacks."}),"\n",(0,i.jsx)(a.h3,{id:"plu-ts",children:"Plu-ts"}),"\n",(0,i.jsx)(a.p,{children:"Plu-ts is a TypeScript DSL for writing Plutus scripts.\nWith Scalus you can do the same and much more but in Scala, and produce JavaScript code."}),"\n",(0,i.jsx)(a.h3,{id:"scalus",children:"Scalus"}),"\n",(0,i.jsx)(a.p,{children:"Scalus aimes to be a better version of all the above."}),"\n",(0,i.jsxs)(a.ul,{children:["\n",(0,i.jsxs)(a.li,{children:["\n",(0,i.jsx)(a.p,{children:"You can actually reuse Scala code for your validator, frontend and backend!\nThe goal that PlutusTx failed to achieve."}),"\n"]}),"\n",(0,i.jsxs)(a.li,{children:["\n",(0,i.jsx)(a.p,{children:"You can use existing Scala libraries for testing, including ScalaCheck and ScalaTest."}),"\n"]}),"\n",(0,i.jsxs)(a.li,{children:["\n",(0,i.jsxs)(a.p,{children:["Scala has a powerful type system that helps you write correct code. Check\nout ",(0,i.jsx)(a.a,{href:"https://stainless.epfl.ch/",children:"Stainless \u2013 Formal Verification for Scala"})," for formal verification."]}),"\n"]}),"\n",(0,i.jsxs)(a.li,{children:["\n",(0,i.jsx)(a.p,{children:"Scalus leverages all the development tools that Scala has, including IntelliJ Idea, VSCode, sbt, even GitHub CoPilot\nand ChatGPT! No need to learn new tools and languages."}),"\n"]}),"\n",(0,i.jsxs)(a.li,{children:["\n",(0,i.jsx)(a.p,{children:"Debugger! It works!"}),"\n"]}),"\n",(0,i.jsxs)(a.li,{children:["\n",(0,i.jsx)(a.p,{children:"Scalus allows only a limited subset of Scala, that can be reasonably efficiently\ncompiled to UPLC without bloating the code."}),"\n"]}),"\n",(0,i.jsxs)(a.li,{children:["\n",(0,i.jsx)(a.p,{children:"It's compiled to a fairly high-level human-readable intermediate representation, SIR."}),"\n"]}),"\n",(0,i.jsxs)(a.li,{children:["\n",(0,i.jsxs)(a.p,{children:["The huge part of any usefull script is ",(0,i.jsx)(a.code,{children:"ScriptContext"})," deserialization from ",(0,i.jsx)(a.code,{children:"Data"})," representation.\nScalus also provides primitives to do your custom deserialization to reduce validator size."]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(a.h2,{id:"support",children:"Support"}),"\n",(0,i.jsxs)(a.p,{children:["You can ask questions on Scalus Discord: ",(0,i.jsx)(a.a,{href:"https://discord.gg/ygwtuBybsy",children:"https://discord.gg/ygwtuBybsy"})]}),"\n",(0,i.jsxs)(a.p,{children:["The project is looking for funding to make it production ready.\nIf you are interested, please contact me at ",(0,i.jsx)(a.a,{href:"https://twitter.com/atlanter",children:"@atlanter"})," on Twitter.\nFollow the official Scalus Twitter account: ",(0,i.jsx)(a.a,{href:"https://twitter.com/Scalus3",children:"@Scalus3"}),"."]}),"\n",(0,i.jsx)(a.p,{children:"You can support the project by donating ADA or BTC to the following addresses:"}),"\n",(0,i.jsx)(a.p,{children:"ADA: addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"}),"\n",(0,i.jsx)(a.p,{children:"Please, consider becoming a sponsor on GitHub."}),"\n",(0,i.jsx)(a.p,{children:"And vote for the project on Cardano Catalyst!"})]})}function h(e={}){const{wrapper:a}={...(0,l.R)(),...e.components};return a?(0,i.jsx)(a,{...e,children:(0,i.jsx)(d,{...e})}):d(e)}},8453:(e,a,n)=>{n.d(a,{R:()=>r,x:()=>s});var t=n(6540);const i={},l=t.createContext(i);function r(e){const a=t.useContext(l);return t.useMemo((function(){return"function"==typeof e?e(a):{...a,...e}}),[a,e])}function s(e){let a;return a=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:r(e.components),t.createElement(l.Provider,{value:a},e.children)}}}]);