"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4598],{3183:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>x,contentTitle:()=>f,default:()=>T,frontMatter:()=>A,metadata:()=>y,toc:()=>g});var a=n(1716),r=n(3050),i=n(8604),l=n(7318),o=n(4768),s=Object.defineProperty,d=Object.defineProperties,u=Object.getOwnPropertyDescriptors,m=Object.getOwnPropertySymbols,p=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,k=(e,t,n)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,h=(e,t)=>{for(var n in t||(t={}))p.call(t,n)&&k(e,n,t[n]);if(m)for(var n of m(t))c.call(t,n)&&k(e,n,t[n]);return e};const A={sidebar_position:197},f="J.7 At Clauses",y={unversionedId:"arm/AA-J/AA-J.7",id:"arm/AA-J/AA-J.7",title:"J.7 At Clauses",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-J/AA-J.7.mdx",sourceDirName:"arm/AA-J",slug:"/arm/AA-J/AA-J.7",permalink:"/docs/arm/AA-J/AA-J.7",draft:!1,tags:[],version:"current",sidebarPosition:197,frontMatter:{sidebar_position:197},sidebar:"referenceManualSidebar",previous:{title:"J.6 Numeric_Error",permalink:"/docs/arm/AA-J/AA-J.6"},next:{title:"J.8 Mod Clauses",permalink:"/docs/arm/AA-J/AA-J.8"}},x={},g=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"J.7.1  Interrupt Entries",id:"j71--interrupt-entries",level:2},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Documentation Requirements",id:"documentation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83-1",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],b={toc:g};function T(e){var t,n=e,{components:s}=n,k=((e,t)=>{var n={};for(var a in e)p.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&m)for(var a of m(e))t.indexOf(a)<0&&c.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=h(h({},b),k),d(t,u({components:s,mdxType:"MDXLayout"}))),(0,a.kt)("h1",h({},{id:"j7-at-clauses"}),"J.7 At Clauses"),(0,a.kt)("admonition",h({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",h({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("h4",h({},{id:"syntax"}),"Syntax"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"1"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"at_clause"),(0,a.kt)("a",{id:"S0368"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("strong",null,"for")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name"))," ",(0,a.kt)("strong",null,"use")," ",(0,a.kt)("strong",null,"at")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),";",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"2"),(0,a.kt)("p",null,"An ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.7#S0368"},"at_clause"))," of the form \u201cfor ",(0,a.kt)("em",null,"x")," use at ",(0,a.kt)("em",null,"y"),";\u201d is equivalent to an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.3#S0349"},"attribute_definition_clause"))," of the form \u201cfor ",(0,a.kt)("em",null,"x"),"'Address use ",(0,a.kt)("em",null,"y"),";\u201d. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"2.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The preferred syntax for specifying the address of an entity is an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.3#S0349"},"attribute_definition_clause"))," specifying the Address attribute. Therefore, the special-purpose ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.7#S0368"},"at_clause"))," syntax is now obsolete.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"2.b"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The above equivalence implies, for example, that only one ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.7#S0368"},"at_clause"))," is allowed for a given entity. Similarly, it is illegal to give both an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.7#S0368"},"at_clause"))," and an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.3#S0349"},"attribute_definition_clause"))," specifying the Address attribute. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"2.c"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"We now allow to define the address of an entity using an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.3#S0349"},"attribute_definition_clause")),". This is because Ada 83's ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.7#S0368"},"at_clause"))," is so hard to remember: programmers often tend to write \u201cfor X'Address use...;\u201d. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"2.d"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Ada 83's ",(0,a.kt)("code",null,"address_clause")," is now called an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.7#S0368"},"at_clause"))," to avoid confusion with the new term \u201cAddress clause\u201d (that is, an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.3#S0349"},"attribute_definition_clause"))," for the Address attribute). ",(0,a.kt)("br",null)),(0,a.kt)("a",{id:"Subclause_J.7.1"}),(0,a.kt)("h2",h({},{id:"j71--interrupt-entries"}),"J.7.1  Interrupt Entries"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"[Implementations are permitted to allow the attachment of task entries to interrupts via the address clause. Such an entry is referred to as an ",(0,a.kt)("em",null,"interrupt entry"),".",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"2"),(0,a.kt)("p",null,"The address of the task entry corresponds to a hardware interrupt in an implementation-defined manner. (See Ada.Interrupts.Reference in ",(0,a.kt)("a",{href:"../AA-C/AA-C.3#Subclause_C.3.2"},"C.3.2"),".)] ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"static-semantics-1"}),"Static Semantics"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"3"),(0,a.kt)("p",null,"The following attribute is defined:",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"4"),(0,a.kt)("p",null,"For any task entry X: ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"5"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"X'Address "),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"For a task entry whose address is specified (an ",(0,a.kt)("em",null,"interrupt entry"),"), the value refers to the corresponding hardware interrupt. For such an entry, as for any other task entry, the meaning of this value is implementation defined. The value of this attribute is of the type of the subtype System.Address.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"6"),(0,a.kt)("dd",null,"Address may be specified for single entries via an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.3#S0349"},"attribute_definition_clause")),". ",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"6.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Because of the equivalence of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-J/AA-J.7#S0368"},"at_clause")),"s and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.3#S0349"},"attribute_definition_clause")),"s, an interrupt entry may be specified via either notation. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"7"),(0,a.kt)("p",null,"As part of the initialization of a task object, the address clause for an interrupt entry is elaborated[, which evaluates the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," of the address clause]. A check is made that the address specified is associated with some interrupt to which a task entry may be attached. If this check fails, Program_Error is raised. Otherwise, the interrupt entry is attached to the interrupt associated with the specified address.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"8"),(0,a.kt)("p",null,"Upon finalization of the task object, the interrupt entry, if any, is detached from the corresponding interrupt and the default treatment is restored.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9"),(0,a.kt)("p",null,"While an interrupt entry is attached to an interrupt, the interrupt is reserved (see ",(0,a.kt)("a",{href:"../AA-C/AA-C.3"},"C.3"),").",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10"),(0,a.kt)("p",null,"An interrupt delivered to a task entry acts as a call to the entry issued by a hardware task whose priority is in the System.Interrupt_Priority range. It is implementation defined whether the call is performed as an ordinary entry call, a timed entry call, or a conditional entry call; which kind of call is performed can depend on the specific interrupt.",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"bounded-run-time-errors"}),"Bounded (Run-Time) Errors"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"11"),(0,a.kt)("p",null,"It is a bounded error to evaluate E'Caller (see ",(0,a.kt)("a",{href:"../AA-C/AA-C.7#Subclause_C.7.1"},"C.7.1"),") in an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0258"},"accept_statement"))," for an interrupt entry. The possible effects are the same as for calling Current_Task from an entry body. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"documentation-requirements"}),"Documentation Requirements"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"12"),(0,a.kt)("p",null,"The implementation shall document to which interrupts a task entry may be attached. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"12.a/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null,"Documentation Requirement: "),"The interrupts to which a task entry may be attached.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"13"),(0,a.kt)("p",null,"The implementation shall document whether the invocation of an interrupt entry has the effect of an ordinary entry call, conditional call, or a timed call, and whether the effect varies in the presence of pending interrupts. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"13.a/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null,"Documentation Requirement: "),"The type of entry call invoked for an interrupt entry.",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"14"),(0,a.kt)("p",null,"The support for this subclause is optional.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"15"),(0,a.kt)("p",null,"Interrupts to which the implementation allows a task entry to be attached may be designated as reserved for the entire duration of program execution[; that is, not just when they have an interrupt entry attached to them].",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"16/1"),(0,a.kt)(o.Z,{items:["AI95-00111-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"8652/0077"),"}"," ","{",(0,a.kt)("em",null,"AI95-00111-01"),"}"," ",(0,a.kt)("br",null),"Interrupt entry calls may be implemented by having the hardware execute directly the appropriate ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0258"},"accept_statement")),". Alternatively, the implementation is allowed to provide an internal interrupt handler to simulate the effect of a normal task calling the entry.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"17"),(0,a.kt)("p",null,"The implementation is allowed to impose restrictions on the specifications and bodies of tasks that have interrupt entries.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"18"),(0,a.kt)("p",null,"It is implementation defined whether direct calls (from the program) to interrupt entries are allowed.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"19"),(0,a.kt)("p",null,"If a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.7#S0269"},"select_statement"))," contains both a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.7#S0275"},"terminate_alternative"))," and an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.7#S0273"},"accept_alternative"))," for an interrupt entry, then an implementation is allowed to impose further requirements for the selection of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.7#S0275"},"terminate_alternative"))," in addition to those given in ",(0,a.kt)("a",{href:"../AA-9/AA-9.3"},"9.3"),". ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"20/1"),(0,a.kt)(o.Z,{items:["AI95-00111-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   ","{",(0,a.kt)("em",null,"8652/0077"),"}"," ","{",(0,a.kt)("em",null,"AI95-00111-01"),"}"," ",(0,a.kt)("br",null),"Queued interrupts correspond to ordinary entry calls. Interrupts that are lost if not immediately processed correspond to conditional entry calls. It is a consequence of the priority rules that an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0258"},"accept_statement"))," executed in response to an interrupt can be executed with the active priority at which the hardware generates the interrupt, taking precedence over lower priority tasks, without a scheduling action.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"21"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 2   Control information that is supplied upon an interrupt can be passed to an associated interrupt entry as one or more parameters of mode ",(0,a.kt)("strong",null,"in"),". ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"examples"}),"Examples"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"22"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Example of an interrupt entry:")," ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"23"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"task Interrupt_Handler is","\n","  entry Done;","\n","  for Done'Address use Ada.Interrupts.Reference(Ada.Interrupts.Names.Device_Done);","\n","end Interrupt_Handler;","\n"),(0,a.kt)("h4",h({},{id:"wording-changes-from-ada-83-1"}),"Wording Changes from Ada 83"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"23.a/2"),(0,a.kt)(o.Z,{items:["AI95-00114-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00114-01"),"}"," ",(0,a.kt)("br",null),"RM83-13.5.1 did not adequately address the problems associated with interrupts. This feature is now obsolescent and is replaced by the Ada 95 interrupt model as specified in the Systems Programming Annex. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"23.b/2"),(0,a.kt)(o.Z,{items:["AI95-00111-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"8652/0077"),"}"," ","{",(0,a.kt)("em",null,"AI95-00111-01"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null),"Corrigendum:")," The undefined term ",(0,a.kt)("em",null,"accept body")," was replaced by ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0258"},"accept_statement")),". ",(0,a.kt)("br",null)))}T.isMDXComponent=!0}}]);