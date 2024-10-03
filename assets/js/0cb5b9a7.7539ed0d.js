"use strict";(self.webpackChunkscalus_website=self.webpackChunkscalus_website||[]).push([[737],{2696:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>l,contentTitle:()=>r,default:()=>u,frontMatter:()=>s,metadata:()=>o,toc:()=>c});var n=i(5893),a=i(1151);const s={sidebar_position:3},r="Advanced usage",o={id:"Advanced",title:"Advanced usage",description:"Direct access to ScriptContext fields",source:"@site/../scalus-docs/target/mdoc/Advanced.md",sourceDirName:".",slug:"/Advanced",permalink:"/docs/Advanced",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_position:3},sidebar:"tutorialSidebar",previous:{title:"Tutorial",permalink:"/docs/Tutorial"},next:{title:"Examples",permalink:"/docs/Examples"}},l={},c=[{value:"Direct access to ScriptContext fields",id:"direct-access-to-scriptcontext-fields",level:2},{value:"Inlining constants",id:"inlining-constants",level:2},{value:"Conditional code generation using macros",id:"conditional-code-generation-using-macros",level:2},{value:"Pretty-printing Scalus Intermediate Representation (SIR)",id:"pretty-printing-scalus-intermediate-representation-sir",level:2},{value:"SIR optimizations",id:"sir-optimizations",level:2},{value:"UPLC optimizations",id:"uplc-optimizations",level:2}];function d(e){const t={code:"code",h1:"h1",h2:"h2",p:"p",pre:"pre",...(0,a.a)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(t.h1,{id:"advanced-usage",children:"Advanced usage"}),"\n",(0,n.jsx)(t.h2,{id:"direct-access-to-scriptcontext-fields",children:"Direct access to ScriptContext fields"}),"\n",(0,n.jsxs)(t.p,{children:["Converting full ",(0,n.jsx)(t.code,{children:"ScriptContext"})," to Scott-encoded lambdas is not always necessary. Often you just need to access a few fields. In this case, you can use the ",(0,n.jsx)(t.code,{children:"fieldAsData"})," macro to extract a field from a class represented as a ",(0,n.jsx)(t.code,{children:"Data"})," object."]}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-scala",children:'import scalus.*, Compiler.*, builtin.{Data, Builtins, ByteString}, Builtins.*, ByteString.given\nimport scalus.ledger.api.v3.*\n\nval sir = compile:\n    def validator(ctxData: Data) =\n        // this generates headList(...headList(sndPair(unConstrData(ctxData)))) code\n        // to extract the signatories field from the ScriptContext\n        val signatories = fieldAsData[ScriptContext](_.txInfo.signatories)(ctxData)\n        // or like this, which is equivalent\n        val signatories2 = ctxData.field[ScriptContext](_.txInfo.signatories)\n        val sigs = unListData(signatories)\n        // or like this, which is equivalent\n        val sigs2 = signatories2.toList\n        unBData(sigs.head) == hex"deadbeef"\n        // same as above\n        sigs2.head.toByteString == hex"deadbeef"\n'})}),"\n",(0,n.jsx)(t.h2,{id:"inlining-constants",children:"Inlining constants"}),"\n",(0,n.jsx)(t.p,{children:"Scalus can inline constants in the script."}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-scala",children:'import scalus.*, Compiler.*, builtin.{Data, Builtins, ByteString, given}, Builtins.*, ByteString.given\n\ninline def validator(inline pubKeyHash: ByteString)(datum: Data, redeemer: Data, ctxData: Data) =\n    verifyEd25519Signature(pubKeyHash, datum.toByteString, redeemer.toByteString)\nval script = compile:\n    validator(hex"deadbeef")\n'})}),"\n",(0,n.jsxs)(t.p,{children:["generates the following ",(0,n.jsx)(t.code,{children:"SIR"}),":"]}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-ocaml",children:"{\u03bb datum redeemer ctxData ->\n  verifyEd25519Signature(#deadbeef, unBData(datum), unBData(redeemer))\n}\n"})}),"\n",(0,n.jsx)(t.h2,{id:"conditional-code-generation-using-macros",children:"Conditional code generation using macros"}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-scala",children:'import scalus.*, Compiler.*, builtin.Data, builtin.Builtins\n// the `dbg` macro will generate `trace` calls only if the `debug` flag is set to `true`\ninline def dbg[A](msg: String)(a: A)(using debug: Boolean): A =\n    inline if debug then Builtins.trace(msg)(a) else a\n\ninline def validator(using debug: Boolean)(datum: Data, redeemer: Data, ctxData: Data) =\n    dbg("datum")(datum)\n\nval releaseScript = compile(validator(using false))\n// {\u03bb datum redeemer ctxData -> datum }\nval debugScript = compile(validator(using true))\n// {\u03bb datum redeemer ctxData -> trace("datum", datum) }\n'})}),"\n",(0,n.jsxs)(t.p,{children:["Here, the ",(0,n.jsx)(t.code,{children:"releaseScript"})," will not contain any ",(0,n.jsx)(t.code,{children:"trace"})," calls, while the ",(0,n.jsx)(t.code,{children:"debugScript"})," will contain them."]}),"\n",(0,n.jsx)(t.h2,{id:"pretty-printing-scalus-intermediate-representation-sir",children:"Pretty-printing Scalus Intermediate Representation (SIR)"}),"\n",(0,n.jsx)(t.p,{children:"Scalus Intermediate Representation (SIR) is a low-level representation of the script that is used by the Scalus compiler.\nIt's a good idea to print the SIR to understand what the compiler has generated."}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-scala",children:"import scalus.*, scalus.sir.SIR\nval sir: SIR = ??? // from the previous example\nprintln(sir.show) // pretty-print the SIR\nprintln(sir.showHighlighted) // pretty-print the SIR with colorized syntax highlighting\n"})}),"\n",(0,n.jsx)(t.h2,{id:"sir-optimizations",children:"SIR optimizations"}),"\n",(0,n.jsxs)(t.p,{children:["Scalus Intermediate Representation (SIR) can be optimized. Currently,\nthe only optimization is the ",(0,n.jsx)(t.code,{children:"RemoveRecursivity"})," optimization that inlines ",(0,n.jsx)(t.code,{children:"let"})," expressions."]}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-scala",children:"import scalus.*, scalus.sir.*\nval sir: SIR = ??? // from the previous example\nval optimized = RemoveRecursivity(sir)\n// or using the `|>` operator\nval optimized2 = sir |> RemoveRecursivity.apply\n"})}),"\n",(0,n.jsx)(t.h2,{id:"uplc-optimizations",children:"UPLC optimizations"}),"\n",(0,n.jsxs)(t.p,{children:["Scalus can also optimize the UPLC representation of the script. The ",(0,n.jsx)(t.code,{children:"EtaReduce"})," optimization removes unnecessary lambdas."]}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-scala",children:"import scalus.*, scalus.sir.*\nval sir: SIR = ??? // from the previous example\nval optimized = RemoveRecursivity(sir)\nval uplc = optimized.toUplc()\nval opt = uplc |> EtaReduce.apply\n"})})]})}function u(e={}){const{wrapper:t}={...(0,a.a)(),...e.components};return t?(0,n.jsx)(t,{...e,children:(0,n.jsx)(d,{...e})}):d(e)}},1151:(e,t,i)=>{i.d(t,{Z:()=>o,a:()=>r});var n=i(7294);const a={},s=n.createContext(a);function r(e){const t=n.useContext(s);return n.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function o(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:r(e.components),n.createElement(s.Provider,{value:t},e.children)}}}]);