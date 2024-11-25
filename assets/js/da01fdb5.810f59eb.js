"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[2126],{39844:(e,i,n)=>{n.r(i),n.d(i,{assets:()=>u,contentTitle:()=>l,default:()=>f,frontMatter:()=>h,metadata:()=>r,toc:()=>m});const r=JSON.parse('{"id":"arm/AA-E/AA-E.3","title":"E.3 Consistency of a Distributed System","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-E/AA-E.3.mdx","sourceDirName":"arm/AA-E","slug":"/arm/AA-E/AA-E.3","permalink":"/docs/arm/AA-E/AA-E.3","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":174,"frontMatter":{"sidebar_position":174},"sidebar":"referenceManualSidebar","previous":{"title":"E.2 Categorization of Library Units","permalink":"/docs/arm/AA-E/AA-E.2"},"next":{"title":"E.4 Remote Subprogram Calls","permalink":"/docs/arm/AA-E/AA-E.4"}}');var s=n(74848),t=n(28453),a=n(13842),o=n(91435),d=(n(21432),n(79162)),c=n(34421);const h={sidebar_position:174},l="E.3 Consistency of a Distributed System",u={},m=[{value:"Language Design Principles",id:"language-design-principles",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}];function p(e){const i={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,t.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(i.header,{children:(0,s.jsx)(i.h1,{id:"e3-consistency-of-a-distributed-system",children:"E.3 Consistency of a Distributed System"})}),"\n",(0,s.jsx)(i.admonition,{type:"danger",children:(0,s.jsxs)(i.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,s.jsx)(i.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,s.jsx)(d.A,{children:"1/3"}),"\n",(0,s.jsx)(c.A,{items:["AI05-0299-1"]}),"\n",(0,s.jsxs)("p",{children:["[This subclause defines attributes and rules associated with verifying the consistency of a distributed program.] ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(i.h4,{id:"language-design-principles",children:"Language Design Principles"}),"\n",(0,s.jsxs)(o.A,{children:[(0,s.jsx)(d.A,{children:"1.a/3"}),(0,s.jsx)(c.A,{items:["AI05-0248-1"]}),(0,s.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(i.p,{children:["The rules guarantee that remote call interface and shared passive library units are consistent among all partitions prior to the execution of a distributed program, so that the semantics of the distributed program are well defined.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(i.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,s.jsx)(d.A,{children:"2/1"}),"\n",(0,s.jsxs)("p",{children:["For a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0093",children:"prefix"})})," P that statically denotes a program unit, the following attributes are defined: ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(d.A,{children:"3"}),"\n",(0,s.jsxs)("dt",{children:[(0,s.jsx)("br",{}),"P'Version"]}),"\n",(0,s.jsxs)("dl",{children:[(0,s.jsxs)("dd",{children:["Yields a value of the predefined type String that identifies the version of the compilation unit that contains the declaration of the program unit.",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"4"}),(0,s.jsxs)("dt",{children:[(0,s.jsx)("br",{}),"P'Body","_","Version"]}),(0,s.jsxs)("dd",{children:["Yields a value of the predefined type String that identifies the version of the compilation unit that contains the body (but not any subunits) of the program unit. ",(0,s.jsx)("br",{})]})]}),"\n",(0,s.jsx)(d.A,{children:"5/1"}),"\n",(0,s.jsx)(c.A,{items:["AI95-00104-01"]}),"\n",(0,s.jsxs)("p",{children:["{",(0,s.jsx)("em",{children:"8652/0084"}),"}"," The ",(0,s.jsx)("em",{children:"version"}),' of a compilation unit changes whenever the compilation unit changes in a semantically significant way. This document does not define the exact meaning of "semantically significant". It is unspecified whether there are other events (such as recompilation) that result in the version of a compilation unit changing. ',(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(o.A,{children:[(0,s.jsx)(d.A,{children:"5.a/1"}),(0,s.jsxs)(a.A,{type:"aarm",aarm:"note",children:[(0,s.jsx)("em",{children:"This paragraph was deleted."}),(0,s.jsx)("br",{})]})]}),"\n",(0,s.jsx)(d.A,{children:"5.1/1"}),"\n",(0,s.jsx)(c.A,{items:["AI95-00104-01"]}),"\n",(0,s.jsxs)("p",{children:["{",(0,s.jsx)("em",{children:"8652/0084"}),"}"," If P is not a library unit, and P has no completion, then P'Body","_","Version returns the Body","_","Version of the innermost program unit enclosing the declaration of P. If P is a library unit, and P has no completion, then P'Body","_","Version returns a value that is different from Body","_","Version of any version of P that has a completion. ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(i.h4,{id:"bounded-run-time-errors",children:"Bounded (Run-Time) Errors"}),"\n",(0,s.jsx)(d.A,{children:"6"}),"\n",(0,s.jsxs)("p",{children:["In a distributed program, a library unit is ",(0,s.jsx)("em",{children:"consistent"})," if the same version of its declaration is used throughout. It is a bounded error to elaborate a partition of a distributed program that contains a compilation unit that depends on a different version of the declaration of a shared passive or RCI library unit than that included in the partition to which the shared passive or RCI library unit was assigned. As a result of this error, Program","_","Error can be raised in one or both partitions during elaboration; in any case, the partitions become inaccessible to one another. ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(o.A,{children:[(0,s.jsx)(d.A,{children:"6.a"}),(0,s.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,s.jsxs)(i.p,{children:[(0,s.jsx)("strong",{}),"Because a version changes if anything on which it depends undergoes a version change, requiring consistency for shared passive and remote call interface library units is sufficient to ensure consistency for the declared pure and remote types library units that define the types used for the objects and parameters through which interpartition communication takes place.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(o.A,{children:[(0,s.jsx)(d.A,{children:"6.b"}),(0,s.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(i.p,{children:["Note that we do not require matching Body","_","Versions; it is irrelevant for shared passive and remote call interface packages, since only one copy of their body exists in a distributed program (in the absence of implicit replication), and we allow the bodies to differ for declared pure and remote types packages from partition to partition, presuming that the differences are due to required error corrections that took place during the execution of a long-running distributed program. The Body","_","Version attribute provides a means for performing stricter consistency checks. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(o.A,{children:(0,s.jsx)(i.h4,{id:"wording-changes-from-ada-95",children:"Wording Changes from Ada 95"})}),"\n",(0,s.jsxs)(o.A,{children:[(0,s.jsx)(d.A,{children:"6.c/2"}),(0,s.jsx)(c.A,{items:["AI95-00104-01"]}),(0,s.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(i.p,{children:["{",(0,s.jsx)("em",{children:"8652/0084"}),"}"," ",(0,s.jsx)("strong",{children:"Corrigendum:"})," Clarified the meaning of 'Version and 'Body","_","Version. ",(0,s.jsx)("br",{})]})})]})]})}function f(e={}){const{wrapper:i}={...(0,t.R)(),...e.components};return i?(0,s.jsx)(i,{...e,children:(0,s.jsx)(p,{...e})}):p(e)}}}]);