"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2547],{42774:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>l,contentTitle:()=>o,default:()=>h,frontMatter:()=>r,metadata:()=>a,toc:()=>d});const a=JSON.parse('{"id":"learn/why-ada","title":"Why Ada?","description":"Get started quick","source":"@site/docs/learn/why-ada.md","sourceDirName":"learn","slug":"/learn/why-ada","permalink":"/docs/learn/why-ada","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":1,"frontMatter":{"sidebar_position":1},"sidebar":"tutorialSidebar","next":{"title":"Getting started","permalink":"/docs/category/getting-started"}}');var t=i(74848),s=i(28453);const r={sidebar_position:1},o="Why Ada?",l={},d=[{value:"Get started quick",id:"get-started-quick",level:2},{value:"No company &quot;owns&quot; Ada",id:"no-company-owns-ada",level:2},{value:"Continued development",id:"continued-development",level:2},{value:"Safer low level programming",id:"safer-low-level-programming",level:2},{value:"Feature Overview",id:"feature-overview",level:2}];function c(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",header:"header",li:"li",p:"p",ul:"ul",...(0,s.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.header,{children:(0,t.jsx)(n.h1,{id:"why-ada",children:"Why Ada?"})}),"\n",(0,t.jsx)(n.h2,{id:"get-started-quick",children:"Get started quick"}),"\n",(0,t.jsxs)(n.p,{children:["The ",(0,t.jsx)(n.a,{href:"https://alire.ada.dev",children:"Alire"})," tool can ",(0,t.jsx)(n.a,{href:"https://alire.ada.dev/docs/#toolchain-management",children:"install an Ada toolchain"})," and\nacts as a build and dependency management tool.\nThis simplifies compiling and running your code, while also providing access to a\n",(0,t.jsx)(n.a,{href:"https://alire.ada.dev/crates.html",children:"growing set of libraries and programs"}),"."]}),"\n",(0,t.jsxs)(n.p,{children:["Use the ",(0,t.jsx)(n.a,{href:"https://marketplace.visualstudio.com/items?itemName=AdaCore.ada",children:"plugin for Visual Studio Code"})," or the ",(0,t.jsx)(n.a,{href:"https://github.com/AdaCore/ada_language_server",children:"language server"})," with your favorite editor."]}),"\n",(0,t.jsx)(n.h2,{id:"no-company-owns-ada",children:'No company "owns" Ada'}),"\n",(0,t.jsx)(n.p,{children:"The Ada language is just an ISO-standard. Though AdaCore initially helped develop\nGNAT and continue to contribute back to FSF GNAT, they do not own the Ada\nlanguage."}),"\n",(0,t.jsxs)(n.p,{children:["While there are also commercial compilers, Ada has a front-end for GCC called ",(0,t.jsx)(n.a,{href:"https://gcc.gnu.org/wiki/GNAT",children:"GNAT"}),'.\nThe version from the Free Software Foundation (FSF) is often referred to as "FSF GNAT".']}),"\n",(0,t.jsx)(n.h2,{id:"continued-development",children:"Continued development"}),"\n",(0,t.jsx)(n.p,{children:"Ada continues to be developed with multiple released standards: Ada 83, Ada 95, Ada 2005, Ada 2012, and Ada 2022."}),"\n",(0,t.jsx)(n.h2,{id:"safer-low-level-programming",children:"Safer low level programming"}),"\n",(0,t.jsx)(n.p,{children:"Ada provides low-level control and interfacing to C to develop programs which\ncan run on embedded or on consumer computers. This includes specifying binary\nalignment and layout of types (representation clauses), calling compiling\nintrinsics, running assembly code, and controlling memory allocation."}),"\n",(0,t.jsx)(n.p,{children:"You can also do the things you'd expect in a systems programming language, in\naddition to having higher level constructs such as built-in containers,\na module system (packages), polymorphism, and concurrency features."}),"\n",(0,t.jsx)(n.h2,{id:"feature-overview",children:"Feature Overview"}),"\n",(0,t.jsx)(n.p,{children:"Covering every Ada feature would be dilute the conceptual overview of Ada, so\nrelated Ada-specific terminology is quoted in parentheses for those wanting to\ndo their own targeted research."}),"\n",(0,t.jsx)(n.p,{children:"Ada supports:"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:'Forced namespacing ("packages").'}),"\n",(0,t.jsx)(n.li,{children:"Function overloading."}),"\n",(0,t.jsx)(n.li,{children:'Sum types ("variants", "discriminants").'}),"\n",(0,t.jsx)(n.li,{children:'Static polymorphism (monomorphism, "generics");'}),"\n",(0,t.jsx)(n.li,{children:"Dynamic polymorphism (dynamic dispatch, virtual functions)."}),"\n",(0,t.jsx)(n.li,{children:'Compiler and runtime checked constraints on ranges of numerical types ("ranges", "constraints").'}),"\n",(0,t.jsx)(n.li,{children:'User-specified runtime type invariant checking on assignment and usage as parameters ("Type_Invariant", "Static_Predicate", "Dynamic_Predicate").'}),"\n",(0,t.jsx)(n.li,{children:'Semantic types, saying two things are the same backing type, but not the same\ntype of "thing", think of "Miles" vs "Kilometers" both stored as floats, which\ncannot be assigned to each other without a cast ("derived types").'}),"\n",(0,t.jsx)(n.li,{children:'Deterministic construction and destruction of objects (RAII, a term from C++, "controlled types")'}),"\n",(0,t.jsx)(n.li,{children:'Design-by-contract ("precondition" and "postcondition" "aspects").'}),"\n",(0,t.jsx)(n.li,{children:'Lifetime checks ("accessibility" of "access types", similar to, but not as extensive as Rust).'}),"\n",(0,t.jsx)(n.li,{children:'Task definition with defined synchronization and queueing strategies.\n("rendezvous", "entry", "select", "accept", "abort", "Ravenscar")'}),"\n",(0,t.jsx)(n.li,{children:'Concurrency types ("protected", which provides mutual exclusion, and "task").'}),"\n",(0,t.jsx)(n.li,{children:"Exceptions."}),"\n",(0,t.jsx)(n.li,{children:'Deterministic and configurable static initialization order ("preelaborate",\n"elaborate", "elaborate_body", "elaborate_all")'}),"\n",(0,t.jsx)(n.li,{children:'ML-style signatures ("packages", "generics")'}),"\n",(0,t.jsxs)(n.li,{children:["Formal verification, by enabling ",(0,t.jsx)(n.code,{children:"SPARK_Mode"})," for parts of the program and\nwriting in SPARK, a language which is an Ada subset. Think of this along the\nlines of using ",(0,t.jsx)(n.code,{children:'extern "C"'}),', except for "provable" parts of your code base.']}),"\n"]}),"\n",(0,t.jsx)(n.p,{children:"Ada is missing:"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"A preprocessor (GNAT has one, but it's not standard)."}),"\n",(0,t.jsx)(n.li,{children:"A (sanitary) macro system."}),"\n",(0,t.jsx)(n.li,{children:"Reflection."}),"\n",(0,t.jsx)(n.li,{children:'A concept of "move".'}),"\n",(0,t.jsx)(n.li,{children:"Variadic functions. (coming in Ada 202x)"}),"\n",(0,t.jsx)(n.li,{children:"Variadic templates."}),"\n",(0,t.jsx)(n.li,{children:"Async/Await (it has tasks/entries instead)"}),"\n",(0,t.jsx)(n.li,{children:"Mixed-mode arithmetic and the related implicit numerical casts."}),"\n",(0,t.jsx)(n.li,{children:"An equivalent of template parameter pack"}),"\n"]})]})}function h(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(c,{...e})}):c(e)}}}]);