"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8848],{3140:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>f,contentTitle:()=>p,default:()=>y,frontMatter:()=>h,metadata:()=>m,toc:()=>g});var i=n(1716),a=Object.defineProperty,r=Object.defineProperties,o=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,l=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,u=(e,t,n)=>t in e?a(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,c=(e,t)=>{for(var n in t||(t={}))l.call(t,n)&&u(e,n,t[n]);if(s)for(var n of s(t))d.call(t,n)&&u(e,n,t[n]);return e};const h={sidebar_position:171},p="E.3  Consistency of a Distributed System",m={unversionedId:"arm/AA-E.3",id:"arm/AA-E.3",title:"E.3  Consistency of a Distributed System",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-E.3.mdx",sourceDirName:"arm",slug:"/arm/AA-E.3",permalink:"/docs/arm/AA-E.3",draft:!1,tags:[],version:"current",sidebarPosition:171,frontMatter:{sidebar_position:171},sidebar:"tutorialSidebar",previous:{title:"E.2  Categorization of Library Units",permalink:"/docs/arm/AA-E.2"},next:{title:"E.4  Remote Subprogram Calls",permalink:"/docs/arm/AA-E.4"}},f={},g=[{value:"Language Design Principles",id:"language-design-principles",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],b={toc:g};function y(e){var t,n=e,{components:a}=n,u=((e,t)=>{var n={};for(var i in e)l.call(e,i)&&t.indexOf(i)<0&&(n[i]=e[i]);if(null!=e&&s)for(var i of s(e))t.indexOf(i)<0&&d.call(e,i)&&(n[i]=e[i]);return n})(n,["components"]);return(0,i.kt)("wrapper",(t=c(c({},b),u),r(t,o({components:a,mdxType:"MDXLayout"}))),(0,i.kt)("h1",c({},{id:"e3--consistency-of-a-distributed-system"}),"E.3  Consistency of a Distributed System"),(0,i.kt)("admonition",c({},{type:"warning"}),(0,i.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,i.kt)("a",c({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0299-1"),"}"," [This subclause defines attributes and rules associated with verifying the consistency of a distributed program.] "),(0,i.kt)("h4",c({},{id:"language-design-principles"}),"Language Design Principles"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"AI05-0248-1"),"}"," The rules guarantee that remote call interface and shared passive library units are consistent among all partitions prior to the execution of a distributed program, so that the semantics of the distributed program are well defined."),(0,i.kt)("h4",c({},{id:"static-semantics"}),"Static Semantics"),(0,i.kt)("p",null,"For a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"./AA-4.1#S0093"},"prefix"))," P that statically denotes a program unit, the following attributes are defined: "),(0,i.kt)("p",null,"P'VersionYields a value of the predefined type String that identifies the version of the compilation unit that contains the declaration of the program unit."),(0,i.kt)("p",null,"P'Body_VersionYields a value of the predefined type String that identifies the version of the compilation unit that contains the body (but not any subunits) of the program unit. "),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"8652/0084"),"}"," ","{",(0,i.kt)("em",null,"AI95-00104-01"),"}"," The ",(0,i.kt)("em",null,"version"),' of a compilation unit changes whenever the compilation unit changes in a semantically significant way. This document does not define the exact meaning of "semantically significant". It is unspecified whether there are other events (such as recompilation) that result in the version of a compilation unit changing. '),(0,i.kt)("p",null,(0,i.kt)("em",null,"This paragraph was deleted.")),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"8652/0084"),"}"," ","{",(0,i.kt)("em",null,"AI95-00104-01"),"}"," If P is not a library unit, and P has no completion, then P'Body_Version returns the Body_Version of the innermost program unit enclosing the declaration of P. If P is a library unit, and P has no completion, then P'Body_Version returns a value that is different from Body_Version of any version of P that has a completion. "),(0,i.kt)("h4",c({},{id:"bounded-run-time-errors"}),"Bounded (Run-Time) Errors"),(0,i.kt)("p",null,"In a distributed program, a library unit is ",(0,i.kt)("em",null,"consistent")," if the same version of its declaration is used throughout. It is a bounded error to elaborate a partition of a distributed program that contains a compilation unit that depends on a different version of the declaration of a shared passive or RCI library unit than that included in the partition to which the shared passive or RCI library unit was assigned. As a result of this error, Program_Error can be raised in one or both partitions during elaboration; in any case, the partitions become inaccessible to one another. "),(0,i.kt)("p",null,(0,i.kt)("strong",null,"Ramification: "),"Because a version changes if anything on which it depends undergoes a version change, requiring consistency for shared passive and remote call interface library units is sufficient to ensure consistency for the declared pure and remote types library units that define the types used for the objects and parameters through which interpartition communication takes place."),(0,i.kt)("p",null,"Note that we do not require matching Body_Versions; it is irrelevant for shared passive and remote call interface packages, since only one copy of their body exists in a distributed program (in the absence of implicit replication), and we allow the bodies to differ for declared pure and remote types packages from partition to partition, presuming that the differences are due to required error corrections that took place during the execution of a long-running distributed program. The Body_Version attribute provides a means for performing stricter consistency checks. "),(0,i.kt)("h4",c({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"8652/0084"),"}"," ","{",(0,i.kt)("em",null,"AI95-00104-01"),"}"," ",(0,i.kt)("strong",null,"Corrigendum:")," Clarified the meaning of 'Version and 'Body_Version. "))}y.isMDXComponent=!0}}]);