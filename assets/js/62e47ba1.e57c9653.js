"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1485],{4675:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>g,contentTitle:()=>f,default:()=>T,frontMatter:()=>p,metadata:()=>y,toc:()=>b});var a=n(1716),l=n(3050),o=n(8604),i=n(7318),r=n(4768),s=Object.defineProperty,A=Object.defineProperties,u=Object.getOwnPropertyDescriptors,d=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,k=Object.prototype.propertyIsEnumerable,h=(e,t,n)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,m=(e,t)=>{for(var n in t||(t={}))c.call(t,n)&&h(e,n,t[n]);if(d)for(var n of d(t))k.call(t,n)&&h(e,n,t[n]);return e};const p={sidebar_position:43},f="5.4 Case Statements",y={unversionedId:"arm/AA-5/AA-5.4",id:"arm/AA-5/AA-5.4",title:"5.4 Case Statements",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-5/AA-5.4.mdx",sourceDirName:"arm/AA-5",slug:"/arm/AA-5/AA-5.4",permalink:"/docs/arm/AA-5/AA-5.4",draft:!1,tags:[],version:"current",sidebarPosition:43,frontMatter:{sidebar_position:43},sidebar:"referenceManualSidebar",previous:{title:"5.3 If Statements",permalink:"/docs/arm/AA-5/AA-5.3"},next:{title:"5.5 Loop Statements",permalink:"/docs/arm/AA-5/AA-5.5"}},g={},b=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Incompatibilities With Ada 83",id:"incompatibilities-with-ada-83",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],x={toc:b};function T(e){var t,n=e,{components:s}=n,h=((e,t)=>{var n={};for(var a in e)c.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&d)for(var a of d(e))t.indexOf(a)<0&&k.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=m(m({},x),h),A(t,u({components:s,mdxType:"MDXLayout"}))),(0,a.kt)("h1",m({},{id:"54-case-statements"}),"5.4 Case Statements"),(0,a.kt)("admonition",m({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",m({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(i.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"[A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," selects for execution one of a number of alternative ",(0,a.kt)("code",null,"sequences_of_statements"),"; the chosen alternative is defined by the value of an expression.] ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"syntax"}),"Syntax"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"2/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)(o.Z,{mdxType:"CodeBlock"},"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),(0,a.kt)("code",null,"case_statement"),(0,a.kt)("a",{id:"S0176"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"case")," ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," ",(0,a.kt)("strong",null,"is"),(0,a.kt)("br",null),"       ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0177"},"case_statement_alternative")),(0,a.kt)("br",null),"      ","{",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0177"},"case_statement_alternative")),"}",(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"end case"),";",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"3"),(0,a.kt)(o.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"case_statement_alternative"),(0,a.kt)("a",{id:"S0177"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"   ",(0,a.kt)("strong",null,"when")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0073"},"discrete_choice_list"))," =",">",(0,a.kt)("br",null),"      ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.1#S0166"},"sequence_of_statements")),(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"4/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),"The ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is expected to be of any discrete type. The expected type for each ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice"))," is the type of the ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),". ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"5/3"),(0,a.kt)(r.Z,{items:["AI05-0153-3"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0153-3"),"}"," ",(0,a.kt)("br",null),"The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0133"},"choice_expression")),"s, ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication")),"s, and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.5#S0037"},"range")),"s given as ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice")),"s of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," shall be static. [A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice"))," ",(0,a.kt)("strong",null,"others"),", if present, shall appear alone and in the last ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0073"},"discrete_choice_list")),".]",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"6/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1","AI05-0240-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0240-1"),"}"," ",(0,a.kt)("br",null),"The possible values of the ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," shall be covered (see ",(0,a.kt)("a",{href:"../AA-3/AA-3.8#Subclause_3.8.1"},"3.8.1"),") as follows: ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"6.a/3"),(0,a.kt)(r.Z,{items:["AI05-0240-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI05-0240-1"),"}"," ",(0,a.kt)("br",null),'The meaning of "covered" here and in the following rules is that of the term "cover a value" that is defined in ',(0,a.kt)("a",{href:"../AA-3/AA-3.8#Subclause_3.8.1"},"3.8.1"),". ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"7/4"),(0,a.kt)(r.Z,{items:["AI05-0003-1","AI05-0153-3","AI05-0188-1","AI05-0262-1","AI12-0071-1"],mdxType:"MarginInfo"}),(0,a.kt)("ul",null,(0,a.kt)("li",null,"{",(0,a.kt)("em",null,"AI05-0003-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0153-3"),"}"," ","{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0262-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0071-1"),"}"," ",(0,a.kt)("br",null),"If the ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," [(including a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.6#S0162"},"type_conversion")),", ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression")),", or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call")),")] having a static and constrained nominal subtype, then each non-",(0,a.kt)("strong",null,"others")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice"))," shall cover only values in that subtype that satisfy its predicates (see ",(0,a.kt)("a",{href:"../AA-3/AA-3.2#Subclause_3.2.4"},"3.2.4"),"), and each value of that subtype that satisfies its predicates shall be covered by some ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice"))," [(either explicitly or by ",(0,a.kt)("strong",null,"others"),")]. ",(0,a.kt)("br",null))),(0,a.kt)(i.Z,{mdxType:"MarginText"},"7.a"),(0,a.kt)(l.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"Although not official ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"s of objects, a value conversion still has a defined nominal subtype, namely its target subtype. See ",(0,a.kt)("a",{href:"../AA-4/AA-4.6"},"4.6"),". ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"8/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)("ul",null,(0,a.kt)("li",null,"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),"If the type of the ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is ",(0,a.kt)("em",null,"root_integer"),", ",(0,a.kt)("em",null,"universal_integer"),", or a descendant of a formal scalar type, then the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," shall have an ",(0,a.kt)("strong",null,"others")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice")),". ",(0,a.kt)("br",null))),(0,a.kt)(i.Z,{mdxType:"MarginText"},"8.a"),(0,a.kt)(l.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This is because the base range is implementation defined for ",(0,a.kt)("em",null,"root_integer")," and ",(0,a.kt)("em",null,"universal_integer"),", and not known statically in the case of a formal scalar type. ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"9/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)("ul",null,(0,a.kt)("li",null,"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),"Otherwise, each value of the base range of the type of the ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," shall be covered [(either explicitly or by ",(0,a.kt)("strong",null,"others"),")]. ",(0,a.kt)("br",null))),(0,a.kt)(i.Z,{mdxType:"MarginText"},"10"),(0,a.kt)("p",null,"Two distinct ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice")),"s of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," shall not cover the same value. ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"10.a/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),"The goal of these coverage rules is that any possible value of the ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," should be covered by exactly one ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice"))," of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement")),", and that this should be checked at compile time. The goal is achieved in most cases, but there are two minor loopholes: ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"10.b"),(0,a.kt)("ul",null,(0,a.kt)("li",null,"If the expression reads an object with an invalid representation (e.g. an uninitialized object), then the value can be outside the covered range. This can happen for static constrained subtypes, as well as nonstatic or unconstrained subtypes. It cannot, however, happen if the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," has the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice"))," ",(0,a.kt)("strong",null,"others"),", because ",(0,a.kt)("strong",null,"others")," covers all values, even those outside the subtype.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"10.c/3"),(0,a.kt)(r.Z,{items:["AI95-00114-01","AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)("li",null,"{",(0,a.kt)("em",null,"AI95-00114-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),"If the compiler chooses to represent the value of an expression of an unconstrained subtype in a way that includes values outside the bounds of the subtype, then those values can be outside the covered range. For example, if X: Integer := Integer'Last;, and the case ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is X+1, then the implementation might choose to produce the correct value, which is outside the bounds of Integer. (It might raise Constraint_Error instead.) This case can only happen for nongeneric subtypes that are either unconstrained or nonstatic (or both). It can only happen if there is no ",(0,a.kt)("strong",null,"others")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice")),". ",(0,a.kt)("br",null))),(0,a.kt)(i.Z,{mdxType:"MarginText"},"10.d"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In the uninitialized variable case, the value might be anything; hence, any alternative can be chosen, or Constraint_Error can be raised. (We intend to prevent, however, jumping to random memory locations and the like.) In the out-of-range case, the behavior is more sensible: if there is an ",(0,a.kt)("strong",null,"others"),", then the implementation may choose to raise Constraint_Error on the evaluation of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (as usual), or it may choose to correctly evaluate the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," and therefore choose the ",(0,a.kt)("strong",null,"others")," alternative. Otherwise (no ",(0,a.kt)("strong",null,"others"),"), Constraint_Error is raised either way \u2014 on the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," evaluation, or for the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," itself.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"10.e"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"For an enumeration type with a discontiguous set of internal codes (see ",(0,a.kt)("a",{href:"../AA-13/AA-13.4"},"13.4"),"), the only way to get values in between the proper values is via an object with an invalid representation; there is no \u201cout-of-range\u201d situation that can produce them. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"11/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),"For the execution of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement")),", the ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is first evaluated.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"12/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),"If the value of the ",(0,a.kt)("em",null,"selecting_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is covered by the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0073"},"discrete_choice_list"))," of some ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0177"},"case_statement_alternative")),", then the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.1#S0166"},"sequence_of_statements"))," of the ",(0,a.kt)("code",null,"_alternative")," is executed.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"13"),(0,a.kt)("p",null,"Otherwise (the value is not covered by any ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0073"},"discrete_choice_list")),", perhaps due to being outside the base range), Constraint_Error is raised. ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"13.a/5"),(0,a.kt)(r.Z,{items:["AI12-0005-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI12-0005-1"),"}"," ",(0,a.kt)("br",null),"In this case, the value fails to satisfy its (static) predicate (possible when the predicate is disabled), is outside the base range of its type, or is an invalid representation.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"14/5"),(0,a.kt)(r.Z,{items:["AI12-0440-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   ","{",(0,a.kt)("em",null,"AI12-0440-1"),"}"," ",(0,a.kt)("br",null),"The execution of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," chooses one and only one alternative. Qualification of the expression of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," by a static subtype can often be used to limit the number of choices that can  be given explicitly. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"examples"}),"Examples"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"15"),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples of case statements:")," ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"16"),(0,a.kt)(o.Z,{language:"ada",mdxType:"CodeBlock"},"case Sensor is","\n","   when Elevation\t=",">"," Record_Elevation(Sensor_Value);","\n","   when Azimuth\t=",">"," Record_Azimuth  (Sensor_Value);","\n","   when Distance\t=",">"," Record_Distance (Sensor_Value);","\n","   when others\t=",">"," null;","\n","end case;","\n",(0,a.kt)(i.Z,{mdxType:"MarginText"},"17"),"case Today is","\n","   when Mon\t=",">"," Compute_Initial_Balance;","\n","   when Fri\t=",">"," Compute_Closing_Balance;","\n","   when Tue .. Thu\t=",">"," Generate_Report(Today);","\n","   when Sat .. Sun\t=",">"," null;","\n","end case;","\n",(0,a.kt)(i.Z,{mdxType:"MarginText"},"18"),"case Bin_Number(Count) is","\n","   when 1\t=",">"," Update_Bin(1);","\n","   when 2\t=",">"," Update_Bin(2);","\n","   when 3 | 4\t=",">","\n","      Empty_Bin(1);","\n","      Empty_Bin(2);","\n","   when others\t=",">"," raise Error;","\n","end case;","\n"),(0,a.kt)("h4",m({},{id:"incompatibilities-with-ada-83"}),"Incompatibilities With Ada 83"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.a.1/1"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In Ada 95, ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call")),"s and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.6#S0162"},"type_conversion")),"s are ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"s, whereas in Ada 83, they were ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"s. Therefore, if the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," is a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call"))," or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.6#S0162"},"type_conversion")),", and the result subtype is static, it is illegal to specify a choice outside the bounds of the subtype. For this case in Ada 83 choices only are required to be in the base range of the type.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.a.2/1"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In addition, the rule about which choices must be covered is unchanged in Ada 95. Therefore, for a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," whose ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call"))," or ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.6#S0162"},"type_conversion")),", Ada 83 required covering all choices in the base range, while Ada 95 only requires covering choices in the bounds of the subtype. If the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," does not include an ",(0,a.kt)("strong",null,"others")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice")),", then a legal Ada 83 ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," will be illegal in Ada 95 if the bounds of the subtype are different than the bounds of the base type. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.a"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In Ada 83, the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," is not allowed to be of a generic formal type. This restriction is removed in Ada 95; an ",(0,a.kt)("strong",null,"others")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice"))," is required instead.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.b"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In Ada 95, a function call is the name of an object; this was not true in Ada 83 (see ",(0,a.kt)("a",{href:"../AA-4/AA-4.1"},"4.1"),", \u201c",(0,a.kt)("a",{href:"../AA-4/AA-4.1"},"Names"),"\u201d). This change makes the following ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement"))," legal: ",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.c"),(0,a.kt)(o.Z,{language:"ada",mdxType:"CodeBlock"},"subtype S is Integer range 1..2;","\n","function F return S;","\n","case F is","\n","   when 1 =",">"," ...;","\n","   when 2 =",">"," ...;","\n","   -- No others needed.","\n","end case;","\n"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.d/3"),(0,a.kt)(r.Z,{items:["AI05-0005-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0005-1"),"}"," ",(0,a.kt)("br",null),"Note that the result subtype given in a function ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-8/AA-8.5#S0238"},"renaming_declaration"))," is ignored; for a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-5/AA-5.4#S0176"},"case_statement")),' whose expression calls a such a function, the full coverage rules are checked using the result subtype of the original function. Note that predefined operators such as "+" have an unconstrained result subtype (see ',(0,a.kt)("a",{href:"../AA-4/AA-4.5#Subclause_4.5.1"},"4.5.1"),"). Note that generic formal functions do not have static result subtypes. Note that the result subtype of an inherited subprogram need not correspond to any nameable subtype; there is still a perfectly good result subtype, though. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.e"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Ada 83 forgot to say what happens for \u201clegally\u201d out-of-bounds values.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.f"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"We take advantage of rules and terms (e.g. ",(0,a.kt)("em",null,"cover a value"),") defined for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice")),"s and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0073"},"discrete_choice_list")),"s in ",(0,a.kt)("a",{href:"../AA-3/AA-3.8#Subclause_3.8.1"},"3.8.1"),", \u201c",(0,a.kt)("a",{href:"../AA-3/AA-3.8#Subclause_3.8.1"},"Variant Parts and Discrete Choices"),"\u201d.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.g"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In the Name Resolution Rule for the case expression, we no longer need RM83-5.4(3)'s \u201cwhich must be determinable independently of the context in which the expression occurs, but using the fact that the expression must be of a discrete type\u201d, because the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is now a complete context. See ",(0,a.kt)("a",{href:"../AA-8/AA-8.6"},"8.6"),", \u201c",(0,a.kt)("a",{href:"../AA-8/AA-8.6"},"The Context of Overload Resolution"),"\u201d.",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.h"),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Since ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.6#S0162"},"type_conversion")),"s are now defined as ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"s, their coverage rule is now covered under the general rule for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"s, rather than being separated out along with ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression")),"s. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.i/3"),(0,a.kt)(r.Z,{items:["AI05-0003-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0003-1"),"}"," ",(0,a.kt)("br",null),"Rewording to reflect that a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression"))," is now a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),".",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.j/3"),(0,a.kt)(r.Z,{items:["AI05-0153-3"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0153-3"),"}"," ",(0,a.kt)("br",null),"Revised for changes to ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-3/AA-3.8#S0074"},"discrete_choice")),"s made to allow static predicates (see ",(0,a.kt)("a",{href:"../AA-3/AA-3.2#Subclause_3.2.4"},"3.2.4"),") as case choices (see ",(0,a.kt)("a",{href:"../AA-3/AA-3.8#Subclause_3.8.1"},"3.8.1"),").",(0,a.kt)("br",null)),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.k/3"),(0,a.kt)(r.Z,{items:["AI05-0188-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0188-1"),"}"," ",(0,a.kt)("br",null),"Added the ",(0,a.kt)("em",null,"selecting_")," prefix to make this wording consistent with ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.5#S0151"},"case_expression")),", and to clarify which ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is being talked about in the wording. ",(0,a.kt)("br",null)),(0,a.kt)("h4",m({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)(i.Z,{mdxType:"MarginText"},"18.l/4"),(0,a.kt)(r.Z,{items:["AI12-0071-1"],mdxType:"MarginInfo"}),(0,a.kt)(l.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI12-0071-1"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null),"Corrigendum:"),' Updated wording of case coverage to use the new term "satisfies the predicates" (see ',(0,a.kt)("a",{href:"../AA-3/AA-3.2#Subclause_3.2.4"},"3.2.4"),"). ",(0,a.kt)("br",null)))}T.isMDXComponent=!0}}]);