"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[79],{6696:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>y,contentTitle:()=>h,default:()=>T,frontMatter:()=>f,metadata:()=>x,toc:()=>b});var a=n(1716),l=n(3050),r=n(8604),o=n(7318),i=n(4768),d=Object.defineProperty,u=Object.defineProperties,s=Object.getOwnPropertyDescriptors,c=Object.getOwnPropertySymbols,m=Object.prototype.hasOwnProperty,A=Object.prototype.propertyIsEnumerable,k=(e,t,n)=>t in e?d(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,p=(e,t)=>{for(var n in t||(t={}))m.call(t,n)&&k(e,n,t[n]);if(c)for(var n of c(t))A.call(t,n)&&k(e,n,t[n]);return e};const f={sidebar_position:55},h="6.7 Null Procedures",x={unversionedId:"arm/AA-6/AA-6.7",id:"arm/AA-6/AA-6.7",title:"6.7 Null Procedures",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-6/AA-6.7.mdx",sourceDirName:"arm/AA-6",slug:"/arm/AA-6/AA-6.7",permalink:"/docs/arm/AA-6/AA-6.7",draft:!1,tags:[],version:"current",sidebarPosition:55,frontMatter:{sidebar_position:55},sidebar:"referenceManualSidebar",previous:{title:"6.6 Overloading of Operators",permalink:"/docs/arm/AA-6/AA-6.6"},next:{title:"6.8 Expression Functions",permalink:"/docs/arm/AA-6/AA-6.8"}},y={},b=[{value:"Syntax",id:"syntax",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],g={toc:b};function T(e){var t,n=e,{components:d}=n,k=((e,t)=>{var n={};for(var a in e)m.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&c)for(var a of c(e))t.indexOf(a)<0&&A.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=p(p({},g),k),u(t,s({components:d,mdxType:"MDXLayout"}))),(0,a.kt)("h1",p({},{id:"67-null-procedures"}),"6.7 Null Procedures"),(0,a.kt)("admonition",p({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1/2"),(0,a.kt)(i.Z,{items:["AI95-00348-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00348-01"),"}"," ",(0,a.kt)("br",null),"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration"))," provides a shorthand to declare a procedure with an empty body. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"syntax"}),"Syntax"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2/3"),(0,a.kt)(i.Z,{items:["AI95-00348-01","AI05-0183-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{mdxType:"CodeBlock"},"{",(0,a.kt)("em",null,"AI95-00348-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," ",(0,a.kt)("br",null),(0,a.kt)("code",null,"null_procedure_declaration"),(0,a.kt)("a",{id:"S0227"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"   [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-8/AA-8.3#S0234"},"overriding_indicator")),"]",(0,a.kt)("br",null),"   ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.1#S0197"},"procedure_specification"))," ",(0,a.kt)("strong",null,"is")," ",(0,a.kt)("strong",null,"null"),(0,a.kt)("br",null),"       [",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification")),"];",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2.1/3"),(0,a.kt)(i.Z,{items:["AI05-0177-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0177-1"),"}"," ",(0,a.kt)("br",null),"If a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration"))," is a completion, it shall be the completion of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.1#S0195"},"subprogram_declaration"))," or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-12/AA-12.1#S0311"},"generic_subprogram_declaration")),". The profile of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration"))," that completes a declaration shall conform fully to that of the declaration. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"3/5"),(0,a.kt)(i.Z,{items:["AI95-00348-01","AI05-0177-1","AI05-0264-1","AI12-0408-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00348-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0177-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0264-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0408-1"),"}"," ",(0,a.kt)("br",null),"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration"))," that is not a completion declares a ",(0,a.kt)("em",null,"null procedure"),". A completion is not allowed for a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration")),"; however, a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration"))," can complete a previous declaration. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"3.a/2"),(0,a.kt)(l.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"There are no null functions because the return value has to be constructed somehow; a function that always raises Program_Error doesn't seem very useful or worth the complication. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4/5"),(0,a.kt)(i.Z,{items:["AI95-00348-01","AI12-0408-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00348-01"),"}"," ","{",(0,a.kt)("em",null,"AI12-0408-1"),"}"," ",(0,a.kt)("br",null),"The execution of a null procedure is invoked by a subprogram call. For the execution of a subprogram call on a null procedure, or on a procedure completed with a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration")),", the execution of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," has no effect. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.a/2"),(0,a.kt)(l.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Thus, a null procedure is equivalent to the body ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.b/2"),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"begin","\n","   null;","\n","end;","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.c/2"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"with the exception that a null procedure can be used in place of a procedure specification. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5/3"),(0,a.kt)(i.Z,{items:["AI95-00348-01","AI05-0177-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00348-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0177-1"),"}"," ",(0,a.kt)("br",null),"The elaboration of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration"))," has no other effect than to establish that the null procedure can be called without failing the Elaboration_Check. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"examples"}),"Examples"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6/5"),(0,a.kt)(i.Z,{items:["AI12-0429-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0429-1"),"}"," ",(0,a.kt)("em",null,(0,a.kt)("br",null),"Example of the declaration of a null procedure:")," ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"7/5"),(0,a.kt)(i.Z,{items:["AI95-00433-01","AI12-0440-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"--  ","{","AI95-00433-01","}","\n"," --  ","{","AI12-0440-1","}","\n"," procedure Simplify(Expr : in out Expression) is null; -- see ",(0,a.kt)("a",{href:"../AA-3/AA-3.9"},"3.9"),"\n","-- By default, Simplify does nothing, but it can  be overridden in extensions of Expression","\n"),(0,a.kt)("h4",p({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"7.a/2"),(0,a.kt)(i.Z,{items:["AI95-00348-01"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00348-01"),"}"," ",(0,a.kt)("br",null),"Null procedures are new. ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"extensions-to-ada-2005"}),"Extensions to Ada 2005"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"7.b/3"),(0,a.kt)(i.Z,{items:["AI05-0177-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0177-1"),"}"," ",(0,a.kt)("br",null),"A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration"))," can now be a completion.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"7.c/3"),(0,a.kt)(i.Z,{items:["AI05-0183-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0183-1"),"}"," ",(0,a.kt)("br",null),"An optional ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification"))," can be used in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.7#S0227"},"null_procedure_declaration")),". This is described in ",(0,a.kt)("a",{href:"../AA-13/AA-13.1#Subclause_13.1.1"},"13.1.1"),". ",(0,a.kt)("br",null)),(0,a.kt)("h4",p({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"7.d/5"),(0,a.kt)(i.Z,{items:["AI12-0408-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI12-0408-1"),"}"," ",(0,a.kt)("br",null),"Clarified the term \u201cnull procedure\u201d so it matches the meaning expected in ",(0,a.kt)("a",{href:"../AA-6/AA-6.1"},"6.1"),". ",(0,a.kt)("br",null)))}T.isMDXComponent=!0}}]);