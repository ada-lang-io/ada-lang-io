"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4554],{9661:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>b,contentTitle:()=>A,default:()=>T,frontMatter:()=>h,metadata:()=>g,toc:()=>y});var l=a(1716),n=a(3050),o=a(8604),i=a(7318),r=a(4768),s=Object.defineProperty,c=Object.defineProperties,d=Object.getOwnPropertyDescriptors,m=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,p=Object.prototype.propertyIsEnumerable,k=(e,t,a)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,f=(e,t)=>{for(var a in t||(t={}))u.call(t,a)&&k(e,a,t[a]);if(m)for(var a of m(t))p.call(t,a)&&k(e,a,t[a]);return e};const h={sidebar_position:189},A="H.7 Extensions to Global and Global'Class Aspects",g={unversionedId:"arm/AA-H/AA-H.7",id:"arm/AA-H/AA-H.7",title:"H.7 Extensions to Global and Global'Class Aspects",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-H/AA-H.7.mdx",sourceDirName:"arm/AA-H",slug:"/arm/AA-H/AA-H.7",permalink:"/docs/arm/AA-H/AA-H.7",draft:!1,tags:[],version:"current",sidebarPosition:189,frontMatter:{sidebar_position:189},sidebar:"referenceManualSidebar",previous:{title:"H.6 Pragma Partition_Elaboration_Policy",permalink:"/docs/arm/AA-H/AA-H.6"},next:{title:"Annex J Obsolescent Features",permalink:"/docs/arm/AA-J/"}},b={},y=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4},{value:"H.7.1  The Use_Formal and Dispatching Aspects",id:"h71--the-use_formal-and-dispatching-aspects",level:2},{value:"Name Resolution Rules",id:"name-resolution-rules-1",level:4},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Legality Rules",id:"legality-rules-1",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012-1",level:4}],x={toc:y};function T(e){var t,a=e,{components:s}=a,k=((e,t)=>{var a={};for(var l in e)u.call(e,l)&&t.indexOf(l)<0&&(a[l]=e[l]);if(null!=e&&m)for(var l of m(e))t.indexOf(l)<0&&p.call(e,l)&&(a[l]=e[l]);return a})(a,["components"]);return(0,l.kt)("wrapper",(t=f(f({},x),k),c(t,d({components:s,mdxType:"MDXLayout"}))),(0,l.kt)("h1",f({},{id:"h7-extensions-to-global-and-globalclass-aspects"}),"H.7 Extensions to Global and Global'Class Aspects"),(0,l.kt)("admonition",f({},{type:"warning"}),(0,l.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,l.kt)("a",f({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,l.kt)(i.Z,{mdxType:"MarginText"},"1/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ",(0,l.kt)("br",null),"In addition to the entities specified in ",(0,l.kt)("a",{href:"../AA-6/AA-6.1#Subclause_6.1.2"},"6.1.2"),", the Global aspect may be specified for a subtype (including a formal subtype), formal package, formal subprogram, and formal object of an anonymous access-to-subprogram type. ",(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"syntax"}),"Syntax"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"2/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3","AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ","{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"The following additional syntax is provided to override the mode of a formal parameter to reflect indirect effects on variables reachable from the formal parameter by one or more access-value dereferences:",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"3/5"),(0,l.kt)(o.Z,{mdxType:"CodeBlock"},(0,l.kt)("code",null,"extended_global_mode"),(0,l.kt)("a",{id:"S0361"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"    ",(0,l.kt)("strong",null,"overriding")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-6/AA-6.1#S0212"},"basic_global_mode")),(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"4/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ",(0,l.kt)("br",null),"The ",(0,l.kt)("em",null,"object_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," that is associated with an ",(0,l.kt)("strong",null,"overriding")," mode shall resolve to statically denote a formal object, or a formal parameter of the associated entity. ",(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"static-semantics"}),"Static Semantics"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"5/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ",(0,l.kt)("br",null),"The presence of the reserved word ",(0,l.kt)("strong",null,"overriding")," in a global mode indicates that the specification is overriding the mode of a formal parameter with another mode to reflect the overall effect of an invocation of the callable entity on the state associated with the corresponding actual parameter.",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"6/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"[As described in ",(0,l.kt)("a",{href:"../AA-6/AA-6.1#Subclause_6.1.2"},"6.1.2"),", the following rules are defined in terms of operations that are performed by or on behalf of an entity.]",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"7/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3","AI12-0431-1","AI12-0439-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ","{",(0,l.kt)("em",null,"AI12-0431-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0439-1"),"}"," ",(0,l.kt)("br",null),"The Global aspect for a subtype identifies the global variables that can be referenced during default initialization, adjustment as part of assignment, finalization of an object of the subtype, or conversion to the subtype, including the evaluation of any assertion expressions that apply. If not specified for the first subtype of a derived type, the aspect defaults to that of the ancestor subtype; if not specified for a nonderived composite first subtype the aspect defaults to that of the enclosing library unit; if not specified for a nonderived elementary first subtype (or scalar base subtype), the aspect defaults to ",(0,l.kt)("strong",null,"null")," in the absence of a predicate (or when the predicate is statically True), and to that of the enclosing library unit otherwise. If not specified for a nonfirst subtype ",(0,l.kt)("em",null,"S"),", the Global aspect defaults to that of the subtype identified in the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-3/AA-3.2#S0027"},"subtype_indication"))," defining ",(0,l.kt)("em",null,"S"),".",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"8/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ",(0,l.kt)("br",null),"The Global'Class aspect may be specified for the first subtype of a tagged type ",(0,l.kt)("em",null,"T"),", indicating an upper bound on the Global aspect of any descendant of ",(0,l.kt)("em",null,"T"),". If not specified, it defaults to Unspecified. ",(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"legality-rules"}),"Legality Rules"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"9/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ",(0,l.kt)("br",null),"For a tagged subtype ",(0,l.kt)("em",null,"T"),", each mode of its Global aspect shall identify a subset of the variables identified either by the corresponding mode, or by the ",(0,l.kt)("strong",null,"in out")," mode, of the Global'Class aspect of the first subtype of any ancestor of ",(0,l.kt)("em",null,"T"),". ",(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"extensions-to-ada-2012"}),"Extensions to Ada 2012"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"9.a/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3","AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ","{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"These extensions to the Global aspect are new. ",(0,l.kt)("br",null)),(0,l.kt)("a",{id:"Subclause_H.7.1"}),(0,l.kt)("h2",f({},{id:"h71--the-use_formal-and-dispatching-aspects"}),"H.7.1  The Use_Formal and Dispatching Aspects"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"1/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"The Use_Formal and Dispatching aspects are provided to more precisely describe the use of generic formal parameters and dispatching calls within the execution of an operation, enabling more precise checking of conformance with the Nonblocking and global aspects that apply at the point of invocation of the operation.",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"2/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3","AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ","{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"For any declaration within a generic unit for which a global or Nonblocking aspect may be specified, other than a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-12/AA-12.1#S0314"},"generic_formal_parameter_declaration")),", the following aspect may be specified to indicate which generic formal parameters are ",(0,l.kt)("em",null,"used")," by the associated entity:",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"3/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("dt",null,(0,l.kt)("br",null),"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"Use_Formal"),(0,l.kt)("dl",null,(0,l.kt)("dd",null,"The aspect is specified with a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0362"},"formal_parameter_set")),", with the following form:",(0,l.kt)("br",null))),(0,l.kt)(i.Z,{mdxType:"MarginText"},"3.a/5"),(0,l.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,l.kt)("strong",null,"Aspect Description for "),(0,l.kt)("strong",null,"Use_Formal: "),"Generic formal parameters used in the implementation of an entity.",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"4/5"),(0,l.kt)("dl",null,(0,l.kt)("dd",null,(0,l.kt)("code",null,"formal_parameter_set"),(0,l.kt)("a",{id:"S0362"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"    ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0363"},"formal_group_designator")),(0,l.kt)("br",null),"  | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0364"},"formal_parameter_name")),(0,l.kt)("br",null),"  | (",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0364"},"formal_parameter_name")),"{",", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0364"},"formal_parameter_name")),"}",")",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"5/5"),(0,l.kt)("dd",null,(0,l.kt)("code",null,"formal_group_designator"),(0,l.kt)("a",{id:"S0363"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("strong",null,"null")," | ",(0,l.kt)("strong",null,"all"),(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"6/5"),(0,l.kt)("dd",null,(0,l.kt)("code",null,"formal_parameter_name"),(0,l.kt)("a",{id:"S0364"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"    ",(0,l.kt)("em",null,"formal_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark")),(0,l.kt)("br",null),"  | ",(0,l.kt)("em",null,"formal_subprogram_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),(0,l.kt)("br",null),"  | ",(0,l.kt)("em",null,"formal_access_to_subprogram_object_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),(0,l.kt)("br",null))),(0,l.kt)(i.Z,{mdxType:"MarginText"},"7/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3","AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ","{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"For any declaration for which a global or Nonblocking aspect may be specified, other than for a library package, a generic library package, or a generic formal, the following aspect may be specified:",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"8/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("dt",null,(0,l.kt)("br",null),"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"Dispatching"),(0,l.kt)("dl",null,(0,l.kt)("dd",null," The aspect is specified with a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0365"},"dispatching_operation_set")),", with the following form:",(0,l.kt)("br",null))),(0,l.kt)(i.Z,{mdxType:"MarginText"},"8.a/5"),(0,l.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,l.kt)("strong",null,"Aspect Description for "),(0,l.kt)("strong",null,"Dispatching: "),"Generic formal parameters used in the implementation of an entity.",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"9/5"),(0,l.kt)("dl",null,(0,l.kt)("dd",null,(0,l.kt)("code",null,"dispatching_operation_set"),(0,l.kt)("a",{id:"S0365"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"    ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier")),(0,l.kt)("br",null),"  | (",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier")),"{",", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier")),"}",")",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"10/5"),(0,l.kt)("dd",null,(0,l.kt)("code",null,"dispatching_operation_specifier"),(0,l.kt)("a",{id:"S0366"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"    ",(0,l.kt)("em",null,"dispatching_operation_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," (",(0,l.kt)("em",null,"object_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),")",(0,l.kt)("br",null))),(0,l.kt)("h4",f({},{id:"name-resolution-rules-1"}),"Name Resolution Rules"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"11/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"A ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0364"},"formal_parameter_name"))," in a Use_Formal aspect shall resolve to statically denote a formal subtype, a formal subprogram, or a formal object of an anonymous access-to-subprogram type[ of an enclosing generic unit or visible formal package].",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"12/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"The ",(0,l.kt)("em",null,"object_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," of a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier"))," shall resolve to statically name an object (including possibly a formal parameter) of a tagged class-wide type ",(0,l.kt)("em",null,"T"),"'Class, or of an access type designating a tagged class-wide type ",(0,l.kt)("em",null,"T"),"'Class; the ",(0,l.kt)("em",null,"dispatching_operation_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," of the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier"))," shall resolve to statically denote a dispatching operation associated with ",(0,l.kt)("em",null,"T"),". ",(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"static-semantics-1"}),"Static Semantics"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"13/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1","AI12-0439-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0439-1"),"}"," ",(0,l.kt)("br",null),"The ",(0,l.kt)("em",null,"formal parameter set")," is identified by a set of ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0364"},"formal_parameter_name")),"s. Alternatively, the reserved word ",(0,l.kt)("strong",null,"null")," may be used to indicate none of the generic formal parameters, or ",(0,l.kt)("strong",null,"all")," to indicate all of the generic formal parameters, of any enclosing generic unit (or visible formal package) can be used within the execution of the operation. If there is no formal parameter set specified for an entity declared within a generic unit, it defaults to ",(0,l.kt)("strong",null,"all"),".",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"14/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1","AI12-0404-1","AI12-0444-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0404-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0444-1"),"}"," ",(0,l.kt)("br",null),"The ",(0,l.kt)("em",null,"dispatching operation set")," is identified by a set of ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier")),"s. It indicates that the Nonblocking and global effects of dispatching calls that match one of the specifiers, rather than being accounted for by the Nonblocking or global aspect, are instead to be accounted for by the invoker of the operation. A dispatching call matches a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier"))," if the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," or ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0093"},"prefix"))," of the call statically denotes the same operation(s) as that of the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier")),", and at least one of the objects controlling the call is denoted by, or designated by, a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," that statically names the same object as that denoted by the ",(0,l.kt)("em",null,"object_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," of the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier")),".",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"14.a/5"),(0,l.kt)(n.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,l.kt)("strong",null),'The object "controlling the call" is not necessarily a controlling parameter of the call if the call is a function with a controlling result or has parameters that is such a function. It is one of the objects that provide the dispatching tag used for the call; that could, for example, be a parameter of a function used as a parameter to the call, or an object being assigned to, or a parameter of an enclosing call. ',(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"15/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1","AI12-0404-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0404-1"),"}"," ",(0,l.kt)("br",null),"In the absence of any ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier")),"s, or if none of them match a dispatching call ",(0,l.kt)("em",null,"C")," within an operation ",(0,l.kt)("em",null,"P"),", Nonblocking and global aspects checks are performed at the point of the call ",(0,l.kt)("em",null,"C")," within ",(0,l.kt)("em",null,"P")," using the Nonblocking and Global'Class aspects that apply to the dispatching operation named in call ",(0,l.kt)("em",null,"C"),". If there is a match, any global access or potential blocking within the subprogram body invoked by the call ",(0,l.kt)("em",null,"C")," is ignored at the point of call within ",(0,l.kt)("em",null,"P"),". Instead, when the operation ",(0,l.kt)("em",null,"P")," itself is invoked, Nonblocking and global aspect checks are performed presuming each named dispatching operation is called at least once (with the named object controlling the call), but similarly ignoring those dispatching calls that would match a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-H/AA-H.7#S0366"},"dispatching_operation_specifier"))," applicable at the point of invocation of ",(0,l.kt)("em",null,"P"),".",(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"legality-rules-1"}),"Legality Rules"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"16/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"Within an operation to which a Use_Formal aspect applies, if the formal parameter set is anything but ",(0,l.kt)("strong",null,"all"),", then the only generic formal subtypes that may be used, the only formal subprograms that may be called, and the only formal objects of an anonymous access-to-subprogram type that may be dereferenced as part of a call or passed as the actual for an access parameter, are those included in the formal parameter set.",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"17/5"),(0,l.kt)(r.Z,{items:["AI12-0380-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0380-1"),"}"," ",(0,l.kt)("br",null),"When an operation (or instance thereof) to which a Use_Formal aspect applies is invoked, Nonblocking and global aspect checks are performed presuming each generic formal parameter (or corresponding actual parameter) of the formal parameter set is used at least once. ",(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"examples"}),"Examples"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"18/5"),(0,l.kt)(r.Z,{items:["AI12-0430-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0430-1"),"}"," ",(0,l.kt)("em",null,(0,l.kt)("br",null),"An example of use of the Dispatching aspect:")," ",(0,l.kt)("br",null)),(0,l.kt)(i.Z,{mdxType:"MarginText"},"19/5"),(0,l.kt)(o.Z,{language:"ada",mdxType:"CodeBlock"},"procedure My_Write(  --  see ",(0,l.kt)("a",{href:"../AA-13/AA-13.13#Subclause_13.13.2"},"13.13.2"),"\n","   Stream : not null access Ada.Streams.Root_Stream_Type'Class;","\n","   Item   : My_Integer'Base)","\n","   with Dispatching =",">"," Write(Stream);","\n","for My_Integer'Write use My_Write;","\n"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"20/5"),(0,l.kt)(r.Z,{items:["AI12-0430-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0430-1"),"}"," ",(0,l.kt)("em",null,(0,l.kt)("br",null),"For examples of use of the Use_Formal aspect, see the Element functions of Hashed_Sets in ",(0,l.kt)("a",{href:"../AA-A/AA-A.18#Subclause_A.18.8"},"A.18.8"),"."),(0,l.kt)("br",null)),(0,l.kt)("h4",f({},{id:"extensions-to-ada-2012-1"}),"Extensions to Ada 2012"),(0,l.kt)(i.Z,{mdxType:"MarginText"},"20.a/5"),(0,l.kt)(r.Z,{items:["AI12-0079-3"],mdxType:"MarginInfo"}),(0,l.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI12-0079-3"),"}"," ",(0,l.kt)("br",null),"The aspects Use_Formal and Dispatching are new. ",(0,l.kt)("br",null)))}T.isMDXComponent=!0}}]);