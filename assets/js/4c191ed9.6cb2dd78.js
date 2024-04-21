"use strict";(self.webpackChunkscalus_website=self.webpackChunkscalus_website||[]).push([[666],{1756:(e,t,s)=>{s.r(t),s.d(t,{assets:()=>o,contentTitle:()=>i,default:()=>d,frontMatter:()=>a,metadata:()=>c,toc:()=>l});var n=s(5893),r=s(1151);const a={sidebar_position:1},i="Quick Start",c={id:"Installation",title:"Quick Start",description:"Prerequisites",source:"@site/../scalus-docs/target/mdoc/Installation.md",sourceDirName:".",slug:"/Installation",permalink:"/docs/Installation",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",next:{title:"Tutorial",permalink:"/docs/Tutorial"}},o={},l=[{value:"Prerequisites",id:"prerequisites",level:2},{value:"Clone Scalus Starter Project",id:"clone-scalus-starter-project",level:2},{value:"Sbt project setup",id:"sbt-project-setup",level:2}];function u(e){const t={a:"a",code:"code",h1:"h1",h2:"h2",p:"p",pre:"pre",...(0,r.a)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(t.h1,{id:"quick-start",children:"Quick Start"}),"\n",(0,n.jsx)(t.h2,{id:"prerequisites",children:"Prerequisites"}),"\n",(0,n.jsxs)(t.p,{children:["You need a usual Scala development environment: JDK, ",(0,n.jsx)(t.a,{href:"https://www.scala-sbt.org/",children:"sbt"}),", scala-cli an IDE of your choice."]}),"\n",(0,n.jsxs)(t.p,{children:["You also can use ",(0,n.jsx)(t.a,{href:"https://scala-cli.virtuslab.org/",children:"Scala CLI"})," instead of SBT.\nSee ",(0,n.jsx)(t.a,{href:"https://github.com/nau/adastream",children:"AdaStream"})," for an example of a project that uses Scala CLI."]}),"\n",(0,n.jsxs)(t.p,{children:["The easiest way to get started is to use ",(0,n.jsx)(t.a,{href:"https://nixos.org/",children:"Nix"})," package manager."]}),"\n",(0,n.jsx)(t.h2,{id:"clone-scalus-starter-project",children:"Clone Scalus Starter Project"}),"\n",(0,n.jsxs)(t.p,{children:["Clone the ",(0,n.jsx)(t.a,{href:"https://github.com/nau/scalus-starter",children:"Scalus Starter Project"})," to get started with Scalus."]}),"\n",(0,n.jsxs)(t.p,{children:["If you use ",(0,n.jsx)(t.a,{href:"https://nixos.org/",children:"Nix"}),", we provided ",(0,n.jsx)(t.code,{children:"flakes.nix"})," file to get a development environment with all the required tools installed."]}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-bash",children:"git clone https://github.com/nau/scalus-starter.git\ncd scalus-starter\nnix develop\ncode .\nscala-cli run .\n"})}),"\n",(0,n.jsx)(t.h2,{id:"sbt-project-setup",children:"Sbt project setup"}),"\n",(0,n.jsxs)(t.p,{children:["Add the following to your ",(0,n.jsx)(t.code,{children:"build.sbt"})," file:"]}),"\n",(0,n.jsx)(t.pre,{children:(0,n.jsx)(t.code,{className:"language-scala",children:'scalaVersion := "3.3.3"\nlibraryDependencies += "org.scalus" %% "scalus" % "0.6.1"\naddCompilerPlugin("org.scalus" %% "scalus-plugin" % "0.6.1")\n'})}),"\n",(0,n.jsx)(t.p,{children:"That's it! You can now start using Scalus in your project."})]})}function d(e={}){const{wrapper:t}={...(0,r.a)(),...e.components};return t?(0,n.jsx)(t,{...e,children:(0,n.jsx)(u,{...e})}):u(e)}},1151:(e,t,s)=>{s.d(t,{Z:()=>c,a:()=>i});var n=s(7294);const r={},a=n.createContext(r);function i(e){const t=n.useContext(a);return n.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function c(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:i(e.components),n.createElement(a.Provider,{value:t},e.children)}}}]);