"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8736],{52642:(e,i,n)=>{n.r(i),n.d(i,{assets:()=>p,contentTitle:()=>h,default:()=>j,frontMatter:()=>l,metadata:()=>t,toc:()=>m});const t=JSON.parse('{"id":"arm/AA-E/AA-E.1","title":"E.1 Partitions","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-E/AA-E.1.mdx","sourceDirName":"arm/AA-E","slug":"/arm/AA-E/AA-E.1","permalink":"/docs/arm/AA-E/AA-E.1","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":172,"frontMatter":{"sidebar_position":172},"sidebar":"referenceManualSidebar","previous":{"title":"Annex E Distributed Systems","permalink":"/docs/arm/AA-E/"},"next":{"title":"E.2 Categorization of Library Units","permalink":"/docs/arm/AA-E/AA-E.2"}}');var a=n(74848),r=n(28453),s=n(13842),o=n(91435),d=(n(21432),n(79162)),c=n(34421);const l={sidebar_position:172},h="E.1 Partitions",p={},m=[{value:"Post-Compilation Rules",id:"post-compilation-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}];function x(e){const i={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,r.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(i.header,{children:(0,a.jsx)(i.h1,{id:"e1-partitions",children:"E.1 Partitions"})}),"\n",(0,a.jsx)(i.admonition,{type:"danger",children:(0,a.jsxs)(i.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.jsx)(i.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,a.jsx)(d.A,{children:"1"}),"\n",(0,a.jsxs)("p",{children:["[The partitions of a distributed program are classified as either active or passive.] ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(i.h4,{id:"post-compilation-rules",children:"Post-Compilation Rules"}),"\n",(0,a.jsx)(d.A,{children:"2"}),"\n",(0,a.jsxs)("p",{children:["An ",(0,a.jsx)("em",{children:"active partition"})," is a partition as defined in ",(0,a.jsx)("a",{href:"/docs/arm/AA-10/AA-10.2",children:"10.2"}),". A ",(0,a.jsx)("em",{children:"passive partition"})," is a partition that has no thread of control of its own, whose library units are all preelaborated, and whose data and subprograms are accessible to one or more active partitions. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"2.a"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(i.p,{children:[(0,a.jsx)("strong",{}),"In most situations, a passive partition does not execute, and does not have a \u201creal\u201d environment task. Any execution involved in its elaboration and initialization occurs before it comes into existence in a distributed program (like most preelaborated entities). Likewise, there is no concrete meaning to passive partition termination. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(d.A,{children:"3"}),"\n",(0,a.jsxs)("p",{children:["A passive partition shall include only ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0287",children:"library_item"})}),"s that either are declared pure or are shared passive (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-10/AA-10.2#Subclause_10.2.1",children:"10.2.1"})," and ",(0,a.jsx)("a",{href:"/docs/arm/AA-E/AA-E.2#Subclause_E.2.1",children:"E.2.1"}),").",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(d.A,{children:"4"}),"\n",(0,a.jsxs)("p",{children:["An active partition shall be configured on a processing node. A passive partition shall be configured either on a storage node or on a processing node.",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(d.A,{children:"5"}),"\n",(0,a.jsxs)("p",{children:["The configuration of the partitions of a program onto a distributed system shall be consistent with the possibility for data references or calls between the partitions implied by their semantic dependences. Any reference to data or call of a subprogram across partitions is called a ",(0,a.jsx)("em",{children:"remote access"}),". ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"5.a"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"discussion",children:(0,a.jsxs)(i.p,{children:[(0,a.jsx)("strong",{}),"For example, an active partition that includes a unit with a semantic dependence on the declaration of another RCI package of some other active partition has to be connected to that other partition by some sort of a message passing mechanism.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"5.b"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(i.p,{children:["A passive partition that is accessible to an active partition should have its storage addressable to the processor(s) of the active partition. The processor(s) should be able to read and write from/to that storage, as well as to perform \u201cread-modify-write\u201d operations (in order to support entry-less protected objects).",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"5.c/5"}),(0,a.jsx)(c.A,{items:["AI12-0359-1"]}),(0,a.jsx)(s.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(i.p,{children:[(0,a.jsx)("strong",{}),"A passive partition has no execution resources of its own, so while a call of a subprogram in a passive partition is a remote access to that subprogram, it is ",(0,a.jsx)("em",{children:"not"})," a remote subprogram call (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-E/AA-E.4",children:"E.4"}),"). The calling active partition executes the body of the subprogram of the passive partition. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(i.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,a.jsx)(d.A,{children:"6"}),"\n",(0,a.jsxs)("p",{children:["A ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0287",children:"library_item"})})," is elaborated as part of the elaboration of each partition that includes it. If a normal library unit (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-E/AA-E.2",children:"E.2"}),") has state, then a separate copy of the state exists in each active partition that elaborates it. [The state evolves independently in each such partition.]",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"6.a"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(i.p,{children:[(0,a.jsx)("strong",{}),"Normal library units cannot be included in passive partitions. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(d.A,{children:"7"}),"\n",(0,a.jsxs)("p",{children:["[An active partition ",(0,a.jsx)("em",{children:"terminates"})," when its environment task terminates.] A partition becomes ",(0,a.jsx)("em",{children:"inaccessible"})," if it terminates or if it is ",(0,a.jsx)("em",{children:"aborted"}),". An active partition is aborted when its environment task is aborted. In addition, if a partition fails during its elaboration, it becomes inaccessible to other partitions. Other implementation-defined events can also result in a partition becoming inaccessible. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"7.a"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"implementation-defined",children:(0,a.jsxs)(i.p,{children:[(0,a.jsx)("strong",{}),"Any events that can result in a partition becoming inaccessible.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(d.A,{children:"8/1"}),"\n",(0,a.jsxs)("p",{children:["For a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0093",children:"prefix"})})," D that denotes a library-level declaration, excepting a declaration of or within a declared-pure library unit, the following attribute is defined: ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(d.A,{children:"9"}),"\n",(0,a.jsxs)("dt",{children:[(0,a.jsx)("br",{}),"D'Partition","_","Id"]}),"\n",(0,a.jsx)("dl",{children:(0,a.jsxs)("dd",{children:["Denotes a value of the type ",(0,a.jsxs)("em",{children:["universal","_","integer"]})," that identifies the partition in which D was elaborated. If D denotes the declaration of a remote call interface library unit (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-E/AA-E.2#Subclause_E.2.3",children:"E.2.3"}),") the given partition is the one where the body of D was elaborated. ",(0,a.jsx)("br",{})]})}),"\n",(0,a.jsx)(i.h4,{id:"bounded-run-time-errors",children:"Bounded (Run-Time) Errors"}),"\n",(0,a.jsx)(d.A,{children:"10/2"}),"\n",(0,a.jsx)(c.A,{items:["AI95-00226-01"]}),"\n",(0,a.jsxs)("p",{children:["It is a bounded error for there to be cyclic elaboration dependences between the active partitions of a single distributed program. The possible effects, in each of the partitions involved, are deadlock during elaboration, or the raising of Communication","_","Error or Program","_","Error. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(i.h4,{id:"implementation-permissions",children:"Implementation Permissions"}),"\n",(0,a.jsx)(d.A,{children:"11"}),"\n",(0,a.jsxs)("p",{children:["An implementation may allow multiple active or passive partitions to be configured on a single processing node, and multiple passive partitions to be configured on a single storage node. In these cases, the scheduling policies, treatment of priorities, and management of shared resources between these partitions are implementation defined. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"11.a"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"implementation-defined",children:(0,a.jsxs)(i.p,{children:[(0,a.jsx)("strong",{}),"The scheduling policies, treatment of priorities, and management of shared resources between partitions in certain cases.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(d.A,{children:"12"}),"\n",(0,a.jsxs)("p",{children:["An implementation may allow separate copies of an active partition to be configured on different processing nodes, and to provide appropriate interactions between the copies to present a consistent state of the partition to other active partitions. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"12.a"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(i.p,{children:[(0,a.jsx)("strong",{}),"The language does not specify the nature of these interactions, nor the actual level of consistency preserved. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(d.A,{children:"13/5"}),"\n",(0,a.jsx)(c.A,{items:["AI12-0444-1"]}),"\n",(0,a.jsxs)("p",{children:["In an implementation, the partitions of a distributed program may be loaded and elaborated at different times; they may be loaded and elaborated one at a time over an extended period of time. An implementation may provide facilities to abort and reload a partition during the execution of a distributed program.",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(d.A,{children:"14"}),"\n",(0,a.jsxs)("p",{children:["An implementation may allow the state of some of the partitions of a distributed program to persist while other partitions of the program terminate and are later reinvoked. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"15/5"}),(0,a.jsx)(c.A,{items:["AI12-0417-1"]}),(0,a.jsx)(s.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(i.p,{children:["NOTE 1   Library units are grouped into partitions after compile time, but before run time. At compile time, only the relevant library unit properties are identified using categorization aspects.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"16"}),(0,a.jsx)(s.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(i.p,{children:["NOTE 2   The value returned by the Partition","_","Id attribute can be used as a parameter to implementation-provided subprograms in order to query information about the partition.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:(0,a.jsx)(i.h4,{id:"wording-changes-from-ada-95",children:"Wording Changes from Ada 95"})}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(d.A,{children:"16.a/2"}),(0,a.jsx)(c.A,{items:["AI95-00226-01"]}),(0,a.jsx)(s.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(i.p,{children:["Corrected wording so that a partition that has an elaboration problem will either deadlock or raise an exception. While an Ada 95 implementation could allow some partitions to continue to execute, they could be accessing unelaborated data, which is very bad (and erroneous in a practical sense). Therefore, this isn't listed as an inconsistency. ",(0,a.jsx)("br",{})]})})]})]})}function j(e={}){const{wrapper:i}={...(0,r.R)(),...e.components};return i?(0,a.jsx)(i,{...e,children:(0,a.jsx)(x,{...e})}):x(e)}}}]);