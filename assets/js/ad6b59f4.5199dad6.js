"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3074],{7542:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>b,contentTitle:()=>h,default:()=>y,frontMatter:()=>u,metadata:()=>f,toc:()=>k});var n=i(1716),a=i(3183),o=Object.defineProperty,r=Object.defineProperties,s=Object.getOwnPropertyDescriptors,l=Object.getOwnPropertySymbols,p=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,c=(e,t,i)=>t in e?o(e,t,{enumerable:!0,configurable:!0,writable:!0,value:i}):e[t]=i,m=(e,t)=>{for(var i in t||(t={}))p.call(t,i)&&c(e,i,t[i]);if(l)for(var i of l(t))d.call(t,i)&&c(e,i,t[i]);return e};const u={sidebar_position:169},h="E.1 Partitions",f={unversionedId:"arm/AA-E/AA-E.1",id:"arm/AA-E/AA-E.1",title:"E.1 Partitions",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-E/AA-E.1.mdx",sourceDirName:"arm/AA-E",slug:"/arm/AA-E/AA-E.1",permalink:"/docs/arm/AA-E/AA-E.1",draft:!1,tags:[],version:"current",sidebarPosition:169,frontMatter:{sidebar_position:169},sidebar:"referenceManualSidebar",previous:{title:"Annex E Distributed Systems",permalink:"/docs/arm/AA-E/"},next:{title:"E.2 Categorization of Library Units",permalink:"/docs/arm/AA-E/AA-E.2"}},b={},k=[{value:"Post-Compilation Rules",id:"post-compilation-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],g={toc:k};function y(e){var t,i=e,{components:o}=i,c=((e,t)=>{var i={};for(var n in e)p.call(e,n)&&t.indexOf(n)<0&&(i[n]=e[n]);if(null!=e&&l)for(var n of l(e))t.indexOf(n)<0&&d.call(e,n)&&(i[n]=e[n]);return i})(i,["components"]);return(0,n.kt)("wrapper",(t=m(m({},g),c),r(t,s({components:o,mdxType:"MDXLayout"}))),(0,n.kt)("h1",m({},{id:"e1-partitions"}),"E.1 Partitions"),(0,n.kt)("admonition",m({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,n.kt)("a",m({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)("p",null,"[The partitions of a distributed program are classified as either active or passive.] "),(0,n.kt)("h4",m({},{id:"post-compilation-rules"}),"Post-Compilation Rules"),(0,n.kt)("p",null,"An ",(0,n.kt)("em",null,"active partition")," is a partition as defined in 10.2. A ",(0,n.kt)("em",null,"passive partition")," is a partition that has no thread of control of its own, whose library units are all preelaborated, and whose data and subprograms are accessible to one or more active partitions. "),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),'In most situations, a passive partition does not execute, and does not have a "real" environment task. Any execution involved in its elaboration and initialization occurs before it comes into existence in a distributed program (like most preelaborated entities). Likewise, there is no concrete meaning to passive partition termination. ')),(0,n.kt)("p",null,"A passive partition shall include only ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-10/AA-10.1#S0287"},"library_item")),"s that either are declared pure or are shared passive (see 10.2.1 and E.2.1)."),(0,n.kt)("p",null,"An active partition shall be configured on a processing node. A passive partition shall be configured either on a storage node or on a processing node."),(0,n.kt)("p",null,"The configuration of the partitions of a program onto a distributed system shall be consistent with the possibility for data references or calls between the partitions implied by their semantic dependences. Any reference to data or call of a subprogram across partitions is called a ",(0,n.kt)("em",null,"remote access"),". "),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"For example, an active partition that includes a unit with a semantic dependence on the declaration of another RCI package of some other active partition has to be connected to that other partition by some sort of a message passing mechanism.")),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'A passive partition that is accessible to an active partition should have its storage addressable to the processor(s) of the active partition. The processor(s) should be able to read and write from/to that storage, as well as to perform "read-modify-write" operations (in order to support entry-less protected objects).')),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Ramification: "),"{",(0,n.kt)("em",null,"AI12-0359-1"),"}"," A passive partition has no execution resources of its own, so while a call of a subprogram in a passive partition is a remote access to that subprogram, it is ",(0,n.kt)("em",null,"not")," a remote subprogram call (see E.4). The calling active partition executes the body of the subprogram of the passive partition. ")),(0,n.kt)("h4",m({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,n.kt)("p",null,"A ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-10/AA-10.1#S0287"},"library_item"))," is elaborated as part of the elaboration of each partition that includes it. If a normal library unit (see E.2) has state, then a separate copy of the state exists in each active partition that elaborates it. [The state evolves independently in each such partition.]"),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Ramification: "),"Normal library units cannot be included in passive partitions. ")),(0,n.kt)("p",null,"[An active partition ",(0,n.kt)("em",null,"terminates")," when its environment task terminates.] A partition becomes ",(0,n.kt)("em",null,"inaccessible")," if it terminates or if it is ",(0,n.kt)("em",null,"aborted"),". An active partition is aborted when its environment task is aborted. In addition, if a partition fails during its elaboration, it becomes inaccessible to other partitions. Other implementation-defined events can also result in a partition becoming inaccessible. "),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Implementation defined: "),"Any events that can result in a partition becoming inaccessible.")),(0,n.kt)("p",null,"For a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0093"},"prefix"))," D that denotes a library-level declaration, excepting a declaration of or within a declared-pure library unit, the following attribute is defined: "),(0,n.kt)("p",null,"D'Partition_IdDenotes a value of the type ",(0,n.kt)("em",null,"universal_integer")," that identifies the partition in which D was elaborated. If D denotes the declaration of a remote call interface library unit (see E.2.3) the given partition is the one where the body of D was elaborated. "),(0,n.kt)("h4",m({},{id:"bounded-run-time-errors"}),"Bounded (Run-Time) Errors"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI95-00226-01"),"}"," It is a bounded error for there to be cyclic elaboration dependences between the active partitions of a single distributed program. The possible effects, in each of the partitions involved, are deadlock during elaboration, or the raising of Communication_Error or Program_Error. "),(0,n.kt)("h4",m({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,n.kt)("p",null,"An implementation may allow multiple active or passive partitions to be configured on a single processing node, and multiple passive partitions to be configured on a single storage node. In these cases, the scheduling policies, treatment of priorities, and management of shared resources between these partitions are implementation defined. "),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Implementation defined: "),"The scheduling policies, treatment of priorities, and management of shared resources between partitions in certain cases.")),(0,n.kt)("p",null,"An implementation may allow separate copies of an active partition to be configured on different processing nodes, and to provide appropriate interactions between the copies to present a consistent state of the partition to other active partitions. "),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Ramification: "),"The language does not specify the nature of these interactions, nor the actual level of consistency preserved. ")),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0444-1"),"}"," In an implementation, the partitions of a distributed program may be loaded and elaborated at different times; they may be loaded and elaborated one at a time over an extended period of time. An implementation may provide facilities to abort and reload a partition during the execution of a distributed program."),(0,n.kt)("p",null,"An implementation may allow the state of some of the partitions of a distributed program to persist while other partitions of the program terminate and are later reinvoked. "),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"NOTE 1   ","{",(0,n.kt)("em",null,"AI12-0417-1"),"}"," Library units are grouped into partitions after compile time, but before run time. At compile time, only the relevant library unit properties are identified using categorization aspects.")),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"NOTE 2   The value returned by the Partition_Id attribute can be used as a parameter to implementation-provided subprograms in order to query information about the partition.")),(0,n.kt)("h4",m({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,n.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI95-00226-01"),"}"," Corrected wording so that a partition that has an elaboration problem will either deadlock or raise an exception. While an Ada 95 implementation could allow some partitions to continue to execute, they could be accessing unelaborated data, which is very bad (and erroneous in a practical sense). Therefore, this isn't listed as an inconsistency. ")))}y.isMDXComponent=!0}}]);