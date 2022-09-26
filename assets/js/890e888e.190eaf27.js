"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9380],{7926:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>g,contentTitle:()=>f,default:()=>T,frontMatter:()=>h,metadata:()=>y,toc:()=>b});var a=n(1716),i=n(3050),r=n(8604),l=n(7318),o=n(4768),s=Object.defineProperty,k=Object.defineProperties,d=Object.getOwnPropertyDescriptors,m=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,A=Object.prototype.propertyIsEnumerable,p=(e,t,n)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,c=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&p(e,n,t[n]);if(m)for(var n of m(t))A.call(t,n)&&p(e,n,t[n]);return e};const h={sidebar_position:72},f="9.1 Task Units and Task Objects",y={unversionedId:"arm/AA-9/AA-9.1",id:"arm/AA-9/AA-9.1",title:"9.1 Task Units and Task Objects",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-9/AA-9.1.mdx",sourceDirName:"arm/AA-9",slug:"/arm/AA-9/AA-9.1",permalink:"/docs/arm/AA-9/AA-9.1",draft:!1,tags:[],version:"current",sidebarPosition:72,frontMatter:{sidebar_position:72},sidebar:"referenceManualSidebar",previous:{title:"9 Tasks and Synchronization",permalink:"/docs/arm/AA-9/"},next:{title:"9.2 Task Execution - Task Activation",permalink:"/docs/arm/AA-9/AA-9.2"}},g={},b=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}],x={toc:b};function T(e){var t,n=e,{components:s}=n,p=((e,t)=>{var n={};for(var a in e)u.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&m)for(var a of m(e))t.indexOf(a)<0&&A.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=c(c({},x),p),k(t,d({components:s,mdxType:"MDXLayout"}))),(0,a.kt)("h1",c({},{id:"91-task-units-and-task-objects"}),"9.1 Task Units and Task Objects"),(0,a.kt)("admonition",c({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",c({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"A task unit is declared by a ",(0,a.kt)("em",null,"task declaration"),", which has a corresponding ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body")),". A task declaration may be a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0244"},"task_type_declaration")),", in which case it declares a named task type; alternatively, it may be a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0245"},"single_task_declaration")),", in which case it defines an anonymous task type, as well as declaring a named task object of that type. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"syntax"}),"Syntax"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"2/3"),(0,a.kt)(o.Z,{items:["AI95-00345-01","AI05-0183-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{mdxType:"CodeBlock"},"{",(0,a.kt)("em",null,"AI95-00345-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," ",(0,a.kt)("br",null),(0,a.kt)("code",null,"task_type_declaration"),(0,a.kt)("a",{id:"S0244"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"task")," ",(0,a.kt)("strong",null,"type")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier"))," [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.7#S0061"},"known_discriminant_part")),"]",(0,a.kt)("br",null),"        [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification")),"] [",(0,a.kt)("strong",null,"is"),(0,a.kt)("br",null),"     [",(0,a.kt)("strong",null,"new")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.9#S0078"},"interface_list"))," ",(0,a.kt)("strong",null,"with"),"]",(0,a.kt)("br",null),"     ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition")),"];",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"3/3"),(0,a.kt)(o.Z,{items:["AI95-00399-01","AI05-0183-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{mdxType:"CodeBlock"},"{",(0,a.kt)("em",null,"AI95-00399-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," ",(0,a.kt)("br",null),(0,a.kt)("code",null,"single_task_declaration"),(0,a.kt)("a",{id:"S0245"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"task")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier"))," ",(0,a.kt)("br",null),"        [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification")),"][",(0,a.kt)("strong",null,"is"),(0,a.kt)("br",null),"     [",(0,a.kt)("strong",null,"new")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.9#S0078"},"interface_list"))," ",(0,a.kt)("strong",null,"with"),"]",(0,a.kt)("br",null),"     ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition")),"];",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"4"),(0,a.kt)(r.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"task_definition"),(0,a.kt)("a",{id:"S0246"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"     ","{",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0247"},"task_item")),"}",(0,a.kt)("br",null),"  [ ",(0,a.kt)("strong",null,"private"),(0,a.kt)("br",null),"     ","{",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0247"},"task_item")),"}","]",(0,a.kt)("br",null),"  ",(0,a.kt)("strong",null,"end")," [",(0,a.kt)("em",null,"task_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),"]",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"5/1"),(0,a.kt)(o.Z,{items:["AI95-00137-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{mdxType:"CodeBlock"},"{",(0,a.kt)("em",null,"8652/0009"),"}"," ","{",(0,a.kt)("em",null,"AI95-00137-01"),"}"," ",(0,a.kt)("br",null),(0,a.kt)("code",null,"task_item"),(0,a.kt)("a",{id:"S0247"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0257"},"entry_declaration"))," | ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0343"},"aspect_clause")),(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"6/3"),(0,a.kt)(o.Z,{items:["AI05-0267-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{mdxType:"CodeBlock"},"{",(0,a.kt)("em",null,"AI05-0267-1"),"}"," ",(0,a.kt)("br",null),(0,a.kt)("code",null,"task_body"),(0,a.kt)("a",{id:"S0248"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"task")," ",(0,a.kt)("strong",null,"body")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier")),(0,a.kt)("br",null),"        [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification")),"] ",(0,a.kt)("strong",null,"is"),(0,a.kt)("br",null),"     ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.11#S0086"},"declarative_part")),(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"begin"),(0,a.kt)("br",null),"     ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.2#S0304"},"handled_sequence_of_statements")),(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"end")," [",(0,a.kt)("em",null,"task_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),"];",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"7"),(0,a.kt)("p",null,"If a ",(0,a.kt)("em",null,"task_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," appears at the end of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition"))," or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body")),", it shall repeat the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.1#S0022"},"defining_identifier")),". ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"8.a/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("em",null,"This paragraph was deleted."),(0,a.kt)("br",null)),(0,a.kt)("p",null,(0,a.kt)("em",null,"Paragraph 8 was deleted.")," ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9"),(0,a.kt)("p",null,"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition"))," defines a task type and its first subtype. The first list of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0247"},"task_item")),"s of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition")),", together with the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.7#S0061"},"known_discriminant_part")),", if any, is called the visible part of the task unit. [ The optional list of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0247"},"task_item")),"s after the reserved word ",(0,a.kt)("strong",null,"private")," is called the private part of the task unit.] ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.a/3"),(0,a.kt)(o.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"proof",title:"Proof: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," ",(0,a.kt)("br",null),"Private part is defined in Clause ",(0,a.kt)("a",{href:"../AA-8/"},"8"),". ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.1/1"),(0,a.kt)(o.Z,{items:["AI95-00116-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"8652/0029"),"}"," ","{",(0,a.kt)("em",null,"AI95-00116-01"),"}"," ",(0,a.kt)("br",null),"For a task declaration without a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition")),", a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition"))," without ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0247"},"task_item")),"s is assumed.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.2/3"),(0,a.kt)(o.Z,{items:["AI95-00345-01","AI95-00397-01","AI95-00399-01","AI95-00419-01","AI05-0042-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00345-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00397-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00399-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00419-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0042-1"),"}"," ",(0,a.kt)("br",null),"For a task declaration with an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.9#S0078"},"interface_list")),", the task type inherits user-defined primitive subprograms from each progenitor type (see ",(0,a.kt)("a",{href:"../AA-3/AA-3.9#Subclause_3.9.4"},"3.9.4"),"), in the same way that a derived type inherits user-defined primitive subprograms from its progenitor types (see ",(0,a.kt)("a",{href:"../AA-3/AA-3.4"},"3.4"),"). If the first parameter of a primitive inherited subprogram is of the task type or an access parameter designating the task type, and there is an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0257"},"entry_declaration"))," for a single entry with the same identifier within the task declaration, whose profile is type conformant with the prefixed view profile of the inherited subprogram, the inherited subprogram is said to be ",(0,a.kt)("em",null,"implemented")," by the conforming task entry using an implicitly declared nonabstract subprogram which has the same profile as the inherited subprogram and which overrides it. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.b/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The inherited subprograms can only come from an interface given as part of the task declaration. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.b.1/3"),(0,a.kt)(o.Z,{items:["AI05-0042-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI05-0042-1"),"}"," ",(0,a.kt)("br",null),"The part about the implicitly declared subprogram is needed so that a subprogram implemented by an entry is considered to be overridden for the purpose of the other rules of the language. Without it, it would for instance be illegal for an abstract subprogram to be implemented by an entry, because the abstract subprogram would not be overridden. The Legality Rules below ensure that there is no conflict between the implicit overriding subprogram and a user-defined overriding subprogram. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.3/2"),(0,a.kt)(o.Z,{items:["AI95-00345-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00345-01"),"}"," ",(0,a.kt)("br",null),"A task declaration requires a completion[, which shall be a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body")),",] and every ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body"))," shall be the completion of some task declaration. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.c/3"),(0,a.kt)(o.Z,{items:["AI05-0229-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null,"To be honest: "),"{",(0,a.kt)("em",null,"AI05-0229-1"),"}"," ",(0,a.kt)("br",null),"If the implementation supports it, the task body can be imported (using aspect Import, see ",(0,a.kt)("a",{href:"../AA-B/AA-B.1"},"B.1"),"), in which case no explicit ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body"))," is allowed. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.4/2"),(0,a.kt)(o.Z,{items:["AI95-00345-01","AI95-00399-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00345-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00399-01"),"}"," ",(0,a.kt)("br",null),"[Each ",(0,a.kt)("em",null,"interface_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark"))," of an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.9#S0078"},"interface_list"))," appearing within a task declaration shall denote a limited interface type that is not a protected interface.] ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.d/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"proof",title:"Proof: ",mdxType:"Admonition"},(0,a.kt)("strong",null),(0,a.kt)("a",{href:"../AA-3/AA-3.9#Subclause_3.9.4"},"3.9.4")," requires that an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.9#S0078"},"interface_list"))," only name interface types, and limits the descendants of the various kinds of interface types. Only a limited, task, or synchronized interface can have a task type descendant. Nonlimited or protected interfaces are not allowed, as they offer operations that a task does not have. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.5/3"),(0,a.kt)(o.Z,{items:["AI95-00397-01","AI05-0090-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00397-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0090-1"),"}"," ",(0,a.kt)("br",null),"The prefixed view profile of an explicitly declared primitive subprogram of a tagged task type shall not be type conformant with any entry of the task type, if the subprogram has the same defining name as the entry and the first parameter of the subprogram is of the task type or is an access parameter designating the task type. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.e/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This prevents the existence of two operations with the same name and profile which could be called with a prefixed view. If the operation was inherited, this would be illegal by the following rules; this rule puts inherited and noninherited routines on the same footing. Note that this only applies to tagged task types (that is, those with an interface in their declaration); we do that as there is no problem with prefixed view calls of primitive operations for \u201cnormal\u201d task types, and having this rule apply to all tasks would be incompatible with Ada 95. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.6/2"),(0,a.kt)(o.Z,{items:["AI95-00345-01","AI95-00399-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00345-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00399-01"),"}"," ",(0,a.kt)("br",null),"For each primitive subprogram inherited by the type declared by a task declaration, at most one of the following shall apply:",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.7/2"),(0,a.kt)(o.Z,{items:["AI95-00345-01"],mdxType:"MarginInfo"}),(0,a.kt)("ul",null,(0,a.kt)("li",null,"{",(0,a.kt)("em",null,"AI95-00345-01"),"}"," ",(0,a.kt)("br",null),"the inherited subprogram is overridden with a primitive subprogram of the task type, in which case the overriding subprogram shall be subtype conformant with the inherited subprogram and not abstract; or",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.8/2"),(0,a.kt)(o.Z,{items:["AI95-00345-01","AI95-00397-01"],mdxType:"MarginInfo"}),(0,a.kt)("li",null,"{",(0,a.kt)("em",null,"AI95-00345-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00397-01"),"}"," ",(0,a.kt)("br",null),"the inherited subprogram is implemented by a single entry of the task type; in which case its prefixed view profile shall be subtype conformant with that of the task entry. ",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.f/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"An entry may implement two subprograms from the ancestors, one whose first parameter is of type ",(0,a.kt)("em",null,"T")," and one whose first parameter is of type ",(0,a.kt)("strong",null,"access")," ",(0,a.kt)("em",null,"T"),". That doesn't cause implementation problems because \u201cimplemented by\u201d (unlike \u201coverridden\u2019) probably entails the creation of wrappers. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.9/2"),(0,a.kt)("p",null,"If neither applies, the inherited subprogram shall be a null procedure. In addition to the places where Legality Rules normally apply (see ",(0,a.kt)("a",{href:"../AA-12/AA-12.3"},"12.3"),"), these rules also apply in the private part of an instance of a generic unit. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9.g/2"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Each inherited subprogram can only have a single implementation (either from overriding a subprogram or implementing an entry), and must have an implementation unless the subprogram is a null procedure. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10"),(0,a.kt)("p",null,"[ The elaboration of a task declaration elaborates the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition")),". The elaboration of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0245"},"single_task_declaration"))," also creates an object of an (anonymous) task type.] ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"proof",title:"Proof: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This is redundant with the general rules for the elaboration of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0024"},"full_type_declaration"))," and an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.3#S0032"},"object_declaration")),". ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"11"),(0,a.kt)("p",null,"[The elaboration of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition"))," creates the task type and its first subtype;] it also includes the elaboration of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0257"},"entry_declaration")),"s in the given order.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"12/1"),(0,a.kt)(o.Z,{items:["AI95-00137-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"8652/0009"),"}"," ","{",(0,a.kt)("em",null,"AI95-00137-01"),"}"," ",(0,a.kt)("br",null),"As part of the initialization of a task object, any ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0343"},"aspect_clause")),"s and any per-object constraints associated with ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0257"},"entry_declaration")),"s of the corresponding ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition"))," are elaborated in the given order. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"12.a/1"),(0,a.kt)(i.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The only ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0343"},"aspect_clause")),"s defined for task entries are ones that specify the Address of an entry, as part of defining an interrupt entry. These clearly need to be elaborated per-object, not per-type. Normally the address will be a function of a discriminant, if such an Address clause is in a task type rather than a single task declaration, though it could rely on a parameterless function that allocates sequential interrupt vectors.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"12.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"We do not mention representation pragmas, since each pragma may have its own elaboration rules. ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"13"),(0,a.kt)("p",null,"The elaboration of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body"))," has no effect other than to establish that tasks of the type can from then on be activated without failing the Elaboration_Check.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"14"),(0,a.kt)("p",null,"[The execution of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body"))," is invoked by the activation of a task of the corresponding type (see ",(0,a.kt)("a",{href:"../AA-9/AA-9.2"},"9.2"),").]",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"15"),(0,a.kt)("p",null,"The content of a task object of a given task type includes: ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"16"),(0,a.kt)("ul",null,(0,a.kt)("li",null,"The values of the discriminants of the task object, if any;",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"17"),(0,a.kt)("li",null,"An entry queue for each entry of the task object; ",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"17.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),'"For each entry" implies one queue for each single entry, plus one for each entry of each entry family. ',(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"18"),(0,a.kt)("ul",null,(0,a.kt)("li",null,"A representation of the state of the associated task. ",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"19/2"),(0,a.kt)(o.Z,{items:["AI95-00382-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   ","{",(0,a.kt)("em",null,"AI95-00382-01"),"}"," ",(0,a.kt)("br",null),"Other than in an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.10#S0084"},"access_definition")),", the name of a task unit within the declaration or body of the task unit denotes the current instance of the unit (see ",(0,a.kt)("a",{href:"../AA-8/AA-8.6"},"8.6"),"), rather than the first subtype of the corresponding task type (and thus the name cannot be used as a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark")),"). ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"19.a/2"),(0,a.kt)(o.Z,{items:["AI95-00382-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI95-00382-01"),"}"," ",(0,a.kt)("br",null),"It can be used as a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark"))," in an anonymous access type. In addition, it is possible to refer to some other subtype of the task type within its body, presuming such a subtype has been declared between the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0244"},"task_type_declaration"))," and the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body")),". ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"20"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 2   The notation of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0098"},"selected_component"))," can be used to denote a discriminant of a task (see ",(0,a.kt)("a",{href:"../AA-4/AA-4.1#Subclause_4.1.3"},"4.1.3"),"). Within a task unit, the name of a discriminant of the task type denotes the corresponding discriminant of the current instance of the unit.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"21/5"),(0,a.kt)(o.Z,{items:["AI95-00287-01","AI12-0442-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 3   ","{",(0,a.kt)("em",null,"AI95-00287-01"),"}"," ","{",(0,a.kt)("em",null,"AI12-0442-1"),"}"," ",(0,a.kt)("br",null),"A task type is a limited type (see ",(0,a.kt)("a",{href:"../AA-7/AA-7.5"},"7.5"),"), and hence precludes use of ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.2#S0173"},"assignment_statement")),"s and predefined equality operators. If a programmer wants to write an application that stores and exchanges  task identities, they  can do so by defining an access type designating the corresponding task objects and by using access values for identification purposes. Assignment is available for such an access type as for any access type. Alternatively, if the implementation supports the Systems Programming Annex, the Identity attribute can be used for task identification (see ",(0,a.kt)("a",{href:"../AA-C/AA-C.7#Subclause_C.7.1"},"C.7.1"),"). ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"examples"}),"Examples"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"22"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples of declarations of task types:")," ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"23"),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"task type Server is","\n","   entry Next_Work_Item(WI : in Work_Item);","\n","   entry Shut_Down;","\n","end Server;","\n",(0,a.kt)(l.Z,{mdxType:"MarginText"},"24/2"),(0,a.kt)(o.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),"--  ","{","AI95-00433-01","}","\n"," task type Keyboard_Driver(ID : Keyboard_ID := New_ID) is","\n","      new Serial_Device with  -- see ",(0,a.kt)("a",{href:"../AA-3/AA-3.9#Subclause_3.9.4"},"3.9.4"),"\n","   entry Read (C : out Character);","\n","   entry Write(C : in  Character);","\n","end Keyboard_Driver;","\n"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"25"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples of declarations of single tasks:")," ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"26"),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"task Controller is","\n","   entry Request(Level)(D : Item);  --  a family of entries","\n","end Controller;","\n",(0,a.kt)(l.Z,{mdxType:"MarginText"},"27"),"task Parser is","\n","   entry Next_Lexeme(L : in  Lexical_Element);","\n","   entry Next_Action(A : out Parser_Action);","\n","end;","\n",(0,a.kt)(l.Z,{mdxType:"MarginText"},"28"),"task User;  --  has no entries","\n"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"29"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples of task objects:")," ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"30"),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"Agent    : Server;","\n","Teletype : Keyboard_Driver(TTY_ID);","\n","Pool     : array(1 .. 10) of Keyboard_Driver;","\n"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"31"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Example of access type designating task objects:")," ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32"),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"type Keyboard is access Keyboard_Driver;","\n","Terminal : Keyboard := new Keyboard_Driver(Term_ID);","\n"),(0,a.kt)("h4",c({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.a/1"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The syntax rules for task declarations are modified to allow a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.7#S0061"},"known_discriminant_part")),", and to allow a private part. They are also modified to allow ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.5#S0257"},"entry_declaration")),"s and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0343"},"aspect_clause")),"s to be mixed. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.b"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The syntax rules for tasks have been split up according to task types and single tasks. In particular: The syntax rules for ",(0,a.kt)("code",null,"task_declaration")," and ",(0,a.kt)("code",null,"task_specification")," are removed. The syntax rules for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0244"},"task_type_declaration")),", ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0245"},"single_task_declaration")),", ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition"))," and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0247"},"task_item"))," are new.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.c"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The syntax rule for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body"))," now uses the nonterminal ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-11/AA-11.2#S0304"},"handled_sequence_of_statements")),".",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.d"),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.11#S0086"},"declarative_part"))," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body"))," is now required; that doesn't make any real difference, because a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.11#S0086"},"declarative_part"))," can be empty. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.e/2"),(0,a.kt)(o.Z,{items:["AI95-00345-01","AI95-00397-01","AI95-00399-01","AI95-00419-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00345-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00397-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00399-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00419-01"),"}"," ",(0,a.kt)("br",null),"Task types and single tasks can be derived from one or more interfaces. Entries of the task type can implement the primitive operations of an interface. ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-8/AA-8.3#S0234"},"Overriding_indicator")),"s can be used to specify whether or not an entry implements a primitive operation. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.f/2"),(0,a.kt)(o.Z,{items:["AI95-00116-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"8652/0029"),"}"," ","{",(0,a.kt)("em",null,"AI95-00116-01"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null),"Corrigendum:")," Clarified that a task type has an implicit empty ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0246"},"task_definition"))," if none is given.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.g/2"),(0,a.kt)(o.Z,{items:["AI95-00137-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"8652/0009"),"}"," ","{",(0,a.kt)("em",null,"AI95-00137-01"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null),"Corrigendum:")," Changed representation clauses to aspect clauses to reflect that they are used for more than just representation.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.h/2"),(0,a.kt)(o.Z,{items:["AI95-00287-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00287-01"),"}"," ",(0,a.kt)("br",null),"Revised the note on operations of task types to reflect that limited types do have an assignment operation, but not copying (",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.2#S0173"},"assignment_statement")),"s).",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.i/2"),(0,a.kt)(o.Z,{items:["AI95-00382-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00382-01"),"}"," ",(0,a.kt)("br",null),"Revised the note on use of the name of a task type within itself to reflect the exception for anonymous access types. ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"extensions-to-ada-2005"}),"Extensions to Ada 2005"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.j/3"),(0,a.kt)(o.Z,{items:["AI05-0183-1","AI05-0267-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0267-1"),"}"," ",(0,a.kt)("br",null),"An optional ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification"))," can be used in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0244"},"task_type_declaration")),", a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0245"},"single_task_declaration")),", and a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body")),". This is described in ",(0,a.kt)("a",{href:"../AA-13/AA-13.1#Subclause_13.1.1"},"13.1.1"),". ",(0,a.kt)("br",null)),(0,a.kt)("h4",c({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.k/3"),(0,a.kt)(o.Z,{items:["AI05-0042-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"correction",title:"Correction:",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0042-1"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null))," Clarified that an inherited procedure of a progenitor is overridden when it is implemented by an entry.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"32.l/3"),(0,a.kt)(o.Z,{items:["AI05-0090-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"correction",title:"Correction:",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0090-1"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null))," Added the missing defining name in the no conflicting primitive operation rule. ",(0,a.kt)("br",null)))}T.isMDXComponent=!0}}]);