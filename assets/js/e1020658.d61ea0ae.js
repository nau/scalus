"use strict";(self.webpackChunkscalus_website=self.webpackChunkscalus_website||[]).push([[990],{6293:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>c,contentTitle:()=>r,default:()=>u,frontMatter:()=>i,metadata:()=>a,toc:()=>o});const a=JSON.parse('{"id":"Tutorial","title":"Tutorial","description":"Basic workflow","source":"@site/../scalus-docs/target/mdoc/Tutorial.md","sourceDirName":".","slug":"/Tutorial","permalink":"/docs/Tutorial","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":2,"frontMatter":{"sidebar_position":2},"sidebar":"tutorialSidebar","previous":{"title":"Quick Start","permalink":"/docs/Installation"},"next":{"title":"Advanced usage","permalink":"/docs/Advanced"}}');var s=t(4848),l=t(8453);const i={sidebar_position:2},r="Tutorial",c={},o=[{value:"Basic workflow",id:"basic-workflow",level:2},{value:"How it works",id:"how-it-works",level:2},{value:"Simple validator example",id:"simple-validator-example",level:2},{value:"What Scala features are supported?",id:"what-scala-features-are-supported",level:2},{value:"Scala features that are not supported",id:"scala-features-that-are-not-supported",level:2},{value:"Constans and primitives",id:"constans-and-primitives",level:2},{value:"Builtin Functions",id:"builtin-functions",level:2},{value:"Data types",id:"data-types",level:2},{value:"Control flow",id:"control-flow",level:2},{value:"Functions",id:"functions",level:2},{value:"Modules and reusable code",id:"modules-and-reusable-code",level:2},{value:"FromData",id:"fromdata",level:2},{value:"Writing a validator",id:"writing-a-validator",level:2},{value:"Troubleshooting",id:"troubleshooting",level:2},{value:"Converting the Scalus code to Flat/CBOR encoded UPLC",id:"converting-the-scalus-code-to-flatcbor-encoded-uplc",level:2},{value:"Evaluating scripts",id:"evaluating-scripts",level:2}];function d(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",header:"header",li:"li",p:"p",pre:"pre",ul:"ul",...(0,l.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.header,{children:(0,s.jsx)(n.h1,{id:"tutorial",children:"Tutorial"})}),"\n",(0,s.jsx)(n.h2,{id:"basic-workflow",children:"Basic workflow"}),"\n",(0,s.jsx)(n.p,{children:"The basic workflow is to write a Scala program and then compile it to a Plutus script,\nsimilar to how PlutuxTx works."}),"\n",(0,s.jsxs)(n.p,{children:["You can store the Plutus script in a ",(0,s.jsx)(n.code,{children:"*.plutus"})," file and use it with the Cardano CLI.\nOr use one of the Java/JavaScript libraries to construct transactions with the script."]}),"\n",(0,s.jsxs)(n.p,{children:[(0,s.jsx)(n.a,{href:"https://github.com/nau/scalus/blob/master/examples/src/main/scala/scalus/examples/SendTx.scala",children:"This example"})," shows how\nto use the ",(0,s.jsx)(n.a,{href:"https://github.com/bloxbean/cardano-client-lib",children:"Cardano Client Lib"})," to send transactions."]}),"\n",(0,s.jsx)(n.h2,{id:"how-it-works",children:"How it works"}),"\n",(0,s.jsxs)(n.p,{children:["You write a script using a small subset of ",(0,s.jsx)(n.a,{href:"https://scala-lang.org",children:"Scala 3"})," language,\nwhich is then compiled to a Scalus Intermediate Representation (SIR) with ",(0,s.jsx)(n.code,{children:"compile"})," function."]}),"\n",(0,s.jsx)(n.p,{children:"The SIR can be pretty-printed and reviewed."}),"\n",(0,s.jsxs)(n.p,{children:["The SIR is then compiled\nto ",(0,s.jsx)(n.a,{href:"https://plutus.cardano.intersectmbo.org/docs/essential-concepts/plutus-core-and-plutus-tx#untyped-plutus-core",children:"Untyped Plutus Core"}),"\n(UPLC) that can be executed on the Cardano blockchain."]}),"\n",(0,s.jsx)(n.h2,{id:"simple-validator-example",children:"Simple validator example"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'import scalus.Compiler.compile\nimport scalus.*\nimport scalus.builtin.Data\nimport scalus.uplc.Program\n\n// Compile Scala code to Scalus Intermediate Representation (SIR)\nval validator = compile {\n    // A simple validator that always succeeds\n    (context: Data) => ()\n}\n// validator: SIR = LamAbs(name = "context", term = Const(const = Unit))\n// pretty print the SIR\nvalidator.show\n// res0: String = "{\u03bb context -> () }"\n// convert the SIR to UPLC and pretty print it with colorized syntax highlighting\nvalidator.toUplc().showHighlighted\n// res1: String = """(lam context\n//   (con unit ()))"""\n// get a double CBOR encoded optimized UPLC program as HEX formatted string\nvalidator.toUplcOptimized().plutusV3.doubleCborHex\n// res2: String = "46450101002499"\n'})}),"\n",(0,s.jsx)(n.h2,{id:"what-scala-features-are-supported",children:"What Scala features are supported?"}),"\n",(0,s.jsx)(n.p,{children:"UPLC is a form of lambda calculus, so not all Scala features are supported."}),"\n",(0,s.jsx)(n.p,{children:"Supported:"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsxs)(n.li,{children:["simple ",(0,s.jsx)(n.code,{children:"val"}),"s and ",(0,s.jsx)(n.code,{children:"def"}),"s of supported built-in types or case classes/enums"]}),"\n",(0,s.jsx)(n.li,{children:"lambda expressions"}),"\n",(0,s.jsx)(n.li,{children:"recursive functions"}),"\n",(0,s.jsx)(n.li,{children:"passing/returning functions as arguments (higher-order functions)"}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:"if-then-else"})," expressions"]}),"\n",(0,s.jsxs)(n.li,{children:["simple ",(0,s.jsx)(n.code,{children:"match"})," expressions on case classes and enums","\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsxs)(n.li,{children:["only simple bindings are supported like ",(0,s.jsx)(n.code,{children:"case Costr(field, other) => ..."})]}),"\n"]}),"\n"]}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:"given"})," arguments and ",(0,s.jsx)(n.code,{children:"using"})," clauses"]}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:"throw"})," expressions but no ",(0,s.jsx)(n.code,{children:"try-catch"})," expressions"]}),"\n",(0,s.jsx)(n.li,{children:"built-in functions and operators"}),"\n",(0,s.jsx)(n.li,{children:"simple data types: case classes and enums"}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:"inline"})," vals, functions and macros in general"]}),"\n",(0,s.jsx)(n.li,{children:"implicit conversions"}),"\n",(0,s.jsx)(n.li,{children:"opaque types (non top-level) and type aliases"}),"\n",(0,s.jsx)(n.li,{children:"extension methods"}),"\n"]}),"\n",(0,s.jsx)(n.h2,{id:"scala-features-that-are-not-supported",children:"Scala features that are not supported"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:"var"}),"s and ",(0,s.jsx)(n.code,{children:"lazy val"}),"s"]}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:"while"})," loops"]}),"\n",(0,s.jsxs)(n.li,{children:["classes, inheritance and polymorphism aka virtual dispatch","\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsxs)(n.li,{children:["you can't use ",(0,s.jsx)(n.code,{children:"isInstanceOf"})]}),"\n"]}),"\n"]}),"\n",(0,s.jsx)(n.li,{children:"pattern matching with guards"}),"\n",(0,s.jsxs)(n.li,{children:["pattern matching on multiple constructors (",(0,s.jsx)(n.code,{children:"case A | B => ..."}),")"]}),"\n",(0,s.jsxs)(n.li,{children:["pattern matching using type ascriptions (",(0,s.jsx)(n.code,{children:"case x: BigInt => ..."}),")"]}),"\n",(0,s.jsxs)(n.li,{children:[(0,s.jsx)(n.code,{children:"try-catch"})," expressions"]}),"\n",(0,s.jsx)(n.li,{children:"overloaded functions"}),"\n",(0,s.jsx)(n.li,{children:"mutually recursive functions"}),"\n"]}),"\n",(0,s.jsx)(n.h2,{id:"constans-and-primitives",children:"Constans and primitives"}),"\n",(0,s.jsx)(n.p,{children:"Plutus V3 supports the following primitive types:"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"unit"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"bool"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"integer"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"bytestring"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"string"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"data"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"list"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"pair"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"BLS12_381_G1_Element"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"BLS12_381_G2_Element"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"BLS12_381_MlResult"})}),"\n"]}),"\n",(0,s.jsx)(n.p,{children:"Those types are represented in Scalus as:"}),"\n",(0,s.jsxs)(n.ul,{children:["\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"Unit"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"Boolean"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"BigInt"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"ByteString"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"String"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"Data"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"List"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"Pair"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"BLS12_381_G1_Element"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"BLS12_381_G2_Element"})}),"\n",(0,s.jsx)(n.li,{children:(0,s.jsx)(n.code,{children:"BLS12_381_MlResult"})}),"\n"]}),"\n",(0,s.jsx)(n.p,{children:"respectively."}),"\n",(0,s.jsxs)(n.p,{children:["We use Scala native types to represent ",(0,s.jsx)(n.code,{children:"Unit"}),", ",(0,s.jsx)(n.code,{children:"Boolean"}),", ",(0,s.jsx)(n.code,{children:"BigInt"}),", and ",(0,s.jsx)(n.code,{children:"String"}),"."]}),"\n",(0,s.jsx)(n.p,{children:"Here is an example of how to define constants and use built-in types."}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'import scalus.Compiler.compile\nimport scalus.*\nimport scalus.builtin.*\nimport scalus.builtin.ByteString.*\n\nval constants = compile {\n    val unit = () // unit type\n    val bool = true || false // boolean type\n    val int = BigInt(123) // integer type\n    val bigint = BigInt("12345678901234567890") // large integer value\n    val implicitBigIng: BigInt = 123\n    val emptyByteString = ByteString.empty\n    val byteString = ByteString.fromHex("deadbeef")\n    val byteStringUtf8 = ByteString.fromString("hello") // from utf8 encoded string\n    val byteString2 = hex"deadbeef" // ByteString from hex string\n    val string = "Scalus Rocks!" // string type\n    val emptyList = List.empty[BigInt] // empty list\n    val list = List[BigInt](1, 2, 3) // list of integers\n    val pair = Pair(true, ()) // pair of boolean and unit\n}\n'})}),"\n",(0,s.jsx)(n.h2,{id:"builtin-functions",children:"Builtin Functions"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'import scalus.builtin.*\nimport scalus.builtin.ByteString.*\nimport scalus.prelude.Prelude.{*, given}\ncompile {\n    // See scalus.builtin.Builtins for what is available\n    val data = Builtins.iData(123)\n    val eqData = data == Builtins.iData(123) || data != Builtins.iData(123)\n    val eq = Builtins.equalsByteString(hex"deadbeef", ByteString.empty)\n    val byteStringEq = hex"deadbeef" == ByteString.empty || hex"deadbeef" != ByteString.empty\n    val stringEq = "deadbeef" == "" || "deadbeef" != ""\n    val a = BigInt(1)\n    val sum = a + 1 - a * 3 / 4 // arithmetic operators\n    val intEquality = a == sum || a != sum\n    val bool = !true || (false == true) != false && true // boolean operators\n    val equals = a === sum // comparison operators\n}\n'})}),"\n",(0,s.jsx)(n.h2,{id:"data-types",children:"Data types"}),"\n",(0,s.jsx)(n.p,{children:"You can define your own data types using Scala case classes and enums."}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:"import scalus.builtin.ByteString\nimport scalus.prelude.Prelude.{*, given}\n\ncase class Account(hash: ByteString, balance: BigInt)\n\nenum State:\n    case Empty\n    case Active(account: Account)\n\nimport State.*\ncompile {\n    // Tuple2 literals are supported\n    val tuple = (true, BigInt(123))\n    val empty = State.Empty // simple constructor\n    // Use `new` to create an instance\n    val account = new Account(ByteString.empty, tuple._2) // access tuple fields\n    val active: State = new State.Active(account)\n    val hash = account.hash // access case class fields\n    // A simple pattern matching is supported\n    // no guards, no type ascriptions.\n    // Inner matches can be done only on single constructor case classes\n    // Wildcard patterns are supported\n    active match\n        case Empty                                 => true\n        case Active(account @ Account(_, balance)) => balance == BigInt(123)\n    // all cases must be covered or there must be a default case\n    val isEmpty = active match\n        case Empty => true\n        case _     => false\n}\n"})}),"\n",(0,s.jsx)(n.h2,{id:"control-flow",children:"Control flow"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'import scalus.prelude.Prelude.{*, given}\ncompile {\n    val a = BigInt(1)\n    // if-then-else\n    if a == BigInt(2) then ()\n    // throwing an exception compiles to Plutus ERROR,\n    // which aborts the evaluation of the script\n    // the exception message can be translated to a trace message\n    // using sir.toUplc(generateErrorTraces = true)\n    else throw new Exception("not 2")\n}\n'})}),"\n",(0,s.jsx)(n.h2,{id:"functions",children:"Functions"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:"compile {\n    val nonRecursiveLambda = (a: BigInt) => a + 1\n\n    def recursive(a: BigInt): BigInt =\n        if a == BigInt(0) then 0\n        else recursive(a - 1)\n}\n"})}),"\n",(0,s.jsx)(n.h2,{id:"modules-and-reusable-code",children:"Modules and reusable code"}),"\n",(0,s.jsxs)(n.p,{children:["You can define reusable code in a Scala object annotated with ",(0,s.jsx)(n.code,{children:"@Compile"}),".\nScalus will compile the code to *.sir files and include them in the jar file.\nThis way you can distribute your code as a library."]}),"\n",(0,s.jsxs)(n.p,{children:["Use ",(0,s.jsx)(n.code,{children:"@Ignore"})," to exclude a definition from the compilation."]}),"\n",(0,s.jsxs)(n.p,{children:["The ",(0,s.jsx)(n.code,{children:"compile"})," will link the modules together and compile them to a single script."]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:"\n@Compile\nobject ReusableCode {\n    val constant = BigInt(1)\n    def usefulFunction(a: BigInt): BigInt = a + 1\n    @Ignore // this function is not compiled to UPLC\n    def shouldNotBeInUplc() = ???\n}\n\nval modules = compile {\n    ReusableCode.usefulFunction(ReusableCode.constant)\n}\n"})}),"\n",(0,s.jsx)(n.h2,{id:"fromdata",children:"FromData"}),"\n",(0,s.jsx)(n.p,{children:"FromData type class is used to convert a Data value to a Scalus value."}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:"import scalus.builtin.*, Builtins.*, Data.*\nimport scalus.builtin.FromDataInstances.given\n\ncase class Account(hash: ByteString, balance: BigInt)\n\nenum State:\n    case Empty\n    case Active(account: Account)\n\n\nval fromDataExample = compile {\n    // The `fromData` function is used to convert a `Data` value to a Scalus value.\n    val data = iData(123)\n    // fromData is a summoner method for the `FromData` type class\n    // there are instances for all built-in types\n    val a = fromData[BigInt](data)\n    // also you can use extension method `to` on Data\n    val b = data.to[BigInt]\n\n    // you can define your own `FromData` instances\n    {\n        given FromData[Account] = (d: Data) => {\n            val args = unConstrData(d).snd\n            new Account(args.head.to[ByteString], args.tail.head.to[BigInt])\n        }\n        val account = data.to[Account]\n    }\n\n    // or your can you a macro to derive the FromData instance\n    {\n        given FromData[Account] = FromData.deriveCaseClass\n        given FromData[State] = FromData.deriveEnum[State] {\n            case 0 => d => State.Empty\n            case 1 => FromData.deriveConstructor[State.Active]\n        }\n    }\n}\n"})}),"\n",(0,s.jsx)(n.h2,{id:"writing-a-validator",children:"Writing a validator"}),"\n",(0,s.jsx)(n.p,{children:"Here is a simple example of a PlutusV2 validator written in Scalus."}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'import scalus.ledger.api.v1.PubKeyHash\nimport scalus.ledger.api.v3.*\nimport scalus.ledger.api.v3.FromDataInstances.given\nimport scalus.builtin.ByteString.*\nimport scalus.prelude.List\n\n// Use Scala 3 indentation syntax. Look ma, no braces! Like Python!\nval pubKeyValidator = compile:\n    def validator(ctxData: Data) = {\n        val ctx = ctxData.to[ScriptContext]\n        List.findOrFail(ctx.txInfo.signatories): sig =>\n            sig.hash == hex"deadbeef"\n    }\n'})}),"\n",(0,s.jsx)(n.h2,{id:"troubleshooting",children:"Troubleshooting"}),"\n",(0,s.jsx)(n.p,{children:"Firstly, you can use a debugger and debug the Scala code."}),"\n",(0,s.jsxs)(n.p,{children:["You can use ",(0,s.jsx)(n.code,{children:"log"})," and ",(0,s.jsx)(n.code,{children:"trace"})," functions to log messages to the execution log."]}),"\n",(0,s.jsxs)(n.p,{children:["And there is a ",(0,s.jsx)(n.code,{children:"?"})," operator that can be used to log the value of a boolean expression when it is false."]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'import scalus.builtin.given\nimport scalus.builtin.Builtins.trace\nimport scalus.prelude.*\nimport scalus.prelude.Prelude.log\nimport scalus.uplc.eval.PlutusVM\ngiven PlutusVM = PlutusVM.makePlutusV2VM()\nval sir = compile {\n    val a = trace("a")(BigInt(1))\n    val b = BigInt(2)\n    log("Checking if a == b")\n    val areEqual = a == b\n    areEqual.? // logs "areEqual ? False"\n}.toUplc().evaluateDebug.toString\n// sir: String = """Success executing script:\n//  term: (con bool False)\n//  budget: { mem: 0.004298, cpu: 0.962976 }\n//  costs:\n// Startup: 1 { mem: 0.000100, cpu: 0.000100 }\n// Delay: 2 { mem: 0.000200, cpu: 0.032000 }\n// Apply: 15 { mem: 0.001500, cpu: 0.240000 }\n// Force: 5 { mem: 0.000500, cpu: 0.080000 }\n// LamAbs: 4 { mem: 0.000400, cpu: 0.064000 }\n// Const: 7 { mem: 0.000700, cpu: 0.112000 }\n// Builtin: 5 { mem: 0.000500, cpu: 0.080000 }\n// Var: 3 { mem: 0.000300, cpu: 0.048000 }\n// EqualsInteger: 1 { mem: 0.000001, cpu: 0.052333 }\n// IfThenElse: 1 { mem: 0.000001, cpu: 0.076049 }\n// Trace: 3 { mem: 0.000096, cpu: 0.178494 }\n//  logs: a: { mem: 0.000932, cpu: 0.187598 }\n// Checking if a == b: { mem: 0.002064, cpu: 0.423096 }\n// areEqual ? False: { mem: 0.004298, cpu: 0.962976 }"""\n'})}),"\n",(0,s.jsx)(n.h2,{id:"converting-the-scalus-code-to-flatcbor-encoded-uplc",children:"Converting the Scalus code to Flat/CBOR encoded UPLC"}),"\n",(0,s.jsxs)(n.p,{children:["The ",(0,s.jsx)(n.code,{children:"compile"})," function converts the Scalus code to a ",(0,s.jsx)(n.code,{children:"SIR"})," value, Scalus Intermediate Representation.\nYou then need to convert the ",(0,s.jsx)(n.code,{children:"SIR"})," value to a UPLC value and encode it to Flat and then to CBOR."]}),"\n",(0,s.jsxs)(n.p,{children:["Many APIs require the HEX encoded string of double CBOR encoded Flat encoded UPLC program,\nlike ",(0,s.jsx)(n.code,{children:"Hex(CborEncode(CborEncode(FlatEncode(Program(version, uplc)))))"}),"."]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'import scalus.*\nimport scalus.builtin.ByteString.*\nimport scalus.ledger.api.PlutusLedgerLanguage\nimport scalus.ledger.api.v1.PubKeyHash\nimport scalus.ledger.api.v3.*\nimport scalus.ledger.api.v3.FromDataInstances.given\nimport scalus.prelude.List\nimport scalus.uplc.Program\n\nval serializeToDoubleCborHex = {\n    val pubKeyValidator = compile {\n        def validator(datum: Data, redeamder: Data, ctxData: Data) = {\n            val ctx = ctxData.to[ScriptContext]\n            List.findOrFail[PubKeyHash](ctx.txInfo.signatories)(sig => sig.hash == hex"deadbeef")\n        }\n    }\n    // convert to UPLC\n    // generateErrorTraces = true will add trace messages to the UPLC program\n    val uplc = pubKeyValidator.toUplc(generateErrorTraces = true)\n    val program = uplc.plutusV2.deBruijnedProgram\n    val flatEncoded = program.flatEncoded // if needed\n    val cbor = program.cborEncoded // if needed\n    val doubleEncoded = program.doubleCborEncoded // if needed\n    // in most cases you want to use the hex representation of the double CBOR encoded program\n    program.doubleCborHex\n    // also you can produce a pubKeyValidator.plutus file for use with cardano-cli\n    import scalus.utils.Utils\n    Utils.writePlutusFile("pubKeyValidator.plutus", program, PlutusLedgerLanguage.PlutusV2)\n    // or simply\n    program.writePlutusFile("pubKeyValidator.plutus", PlutusLedgerLanguage.PlutusV2)\n}\n'})}),"\n",(0,s.jsx)(n.h2,{id:"evaluating-scripts",children:"Evaluating scripts"}),"\n",(0,s.jsx)(n.p,{children:"Scalus provides a high-level API to evaluate UPLC scripts."}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'compile(BigInt(2) + 2).toUplc().evaluateDebug.toString\n// res11: String = """Success executing script:\n//  term: (con integer 4)\n//  budget: { mem: 0.000602, cpu: 0.181308 }\n//  costs:\n// Startup: 1 { mem: 0.000100, cpu: 0.000100 }\n// Apply: 2 { mem: 0.000200, cpu: 0.032000 }\n// Const: 2 { mem: 0.000200, cpu: 0.032000 }\n// Builtin: 1 { mem: 0.000100, cpu: 0.016000 }\n// AddInteger: 1 { mem: 0.000002, cpu: 0.101208 }\n//  logs: """\n'})}),"\n",(0,s.jsxs)(n.p,{children:["You get a ",(0,s.jsx)(n.code,{children:"Result"})," object that contains the result of the evaluation, the execution budget, the execution costs, and the logs."]}),"\n",(0,s.jsx)(n.p,{children:"You can also use the low-level API to evaluate scripts."}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-scala",children:'import scalus.builtin.{*, given}\nimport scalus.ledger.api.*\nimport scalus.uplc.*, eval.*\n\ndef evaluation() = {\n    import scalus.*\n    import scalus.builtin.given // for PlatformSpecific implementation\n    import scalus.uplc.eval.PlutusVM\n    val sir = compile {\n        def usefulFunction(a: BigInt): BigInt = a + 1\n        usefulFunction(1)\n    }\n    val term = sir.toUplc()\n    // setup a given PlutusVM for the PlutusV2 language and default parameters\n    given v2VM: PlutusVM = PlutusVM.makePlutusV2VM()\n    // simply evaluate the term with CEK machine\n    term.evaluate.show // (con integer 2)\n\n    // you can get the actual execution costs from protocol parameters JSON from cardano-cli\n    lazy val machineParams = MachineParams.fromCardanoCliProtocolParamsJson(\n      "JSON with protocol parameters",\n      PlutusLedgerLanguage.PlutusV3\n    )\n    // or from blockfrost API\n    lazy val machineParams2 = MachineParams.fromBlockfrostProtocolParamsJson(\n      "JSON with protocol parameters",\n      PlutusLedgerLanguage.PlutusV3\n    )\n    // use latest PlutusV3 VM with explicit machine parameters\n    val v3vm: PlutusVM = PlutusVM.makePlutusV3VM(machineParams)\n    // evaluate a Plutus V3 script considering CIP-117\n    // calculate the execution budget, all builtins costs, and collect logs\n    val script = term.plutusV3.deBruijnedProgram\n    script.evaluateDebug(using v3vm) match\n        case r @ Result.Success(evaled, budget, costs, logs) =>\n            println(r)\n        case Result.Failure(exception, budget, costs, logs) =>\n            println(s"Exception: $exception, logs: $logs")\n\n    // evaluate a flat encoded script and calculate the execution budget and logs\n\n    // TallyingBudgetSpender is a budget spender that counts the costs of each operation\n    val tallyingBudgetSpender = TallyingBudgetSpender(CountingBudgetSpender())\n    val logger = Log()\n    // use NoLogger to disable logging\n    val noopLogger = NoLogger\n    try {\n        v3vm.evaluateScript(script, tallyingBudgetSpender, logger)\n    } catch {\n        case e: StackTraceMachineError =>\n            println(s"Error: ${e.getMessage}")\n            println(s"Stacktrace: ${e.getCekStack}")\n            println(s"Env: ${e.env}")\n    }\n    println(s"Execution budget: ${tallyingBudgetSpender.budgetSpender.getSpentBudget}")\n    println(s"Logs: ${logger.getLogs.mkString("\\n")}")\n    println(\n      s"Execution stats:\\n${tallyingBudgetSpender.costs.toArray\n              .sortBy(_._1.toString())\n              .map { case (k, v) =>\n                  s"$k: $v"\n              }\n              .mkString("\\n")}"\n    )\n}\n'})})]})}function u(e={}){const{wrapper:n}={...(0,l.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(d,{...e})}):d(e)}},8453:(e,n,t)=>{t.d(n,{R:()=>i,x:()=>r});var a=t(6540);const s={},l=a.createContext(s);function i(e){const n=a.useContext(l);return a.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:i(e.components),a.createElement(l.Provider,{value:n},e.children)}}}]);