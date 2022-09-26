"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7407],{4628:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>y,contentTitle:()=>f,default:()=>T,frontMatter:()=>h,metadata:()=>x,toc:()=>g});var l=n(1716),r=n(3050),a=n(8604),o=n(7318),i=n(4768),s=Object.defineProperty,A=Object.defineProperties,k=Object.getOwnPropertyDescriptors,u=Object.getOwnPropertySymbols,d=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,p=(e,t)=>{for(var n in t||(t={}))d.call(t,n)&&c(e,n,t[n]);if(u)for(var n of u(t))m.call(t,n)&&c(e,n,t[n]);return e};const h={sidebar_position:32},f="4.4 Expressions",x={unversionedId:"arm/AA-4/AA-4.4",id:"arm/AA-4/AA-4.4",title:"4.4 Expressions",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-4/AA-4.4.mdx",sourceDirName:"arm/AA-4",slug:"/arm/AA-4/AA-4.4",permalink:"/docs/arm/AA-4/AA-4.4",draft:!1,tags:[],version:"current",sidebarPosition:32,frontMatter:{sidebar_position:32},sidebar:"referenceManualSidebar",previous:{title:"4.3 Aggregates",permalink:"/docs/arm/AA-4/AA-4.3"},next:{title:"4.5 Operators and Expression Evaluation",permalink:"/docs/arm/AA-4/AA-4.5"}},y={},g=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Inconsistencies With Ada 2012",id:"inconsistencies-with-ada-2012",level:4},{value:"Incompatibilities With Ada 2012",id:"incompatibilities-with-ada-2012",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],b={toc:g};function T(e){var t,n=e,{components:s}=n,c=((e,t)=>{var n={};for(var l in e)d.call(e,l)&&t.indexOf(l)<0&&(n[l]=e[l]);if(null!=e&&u)for(var l of u(e))t.indexOf(l)<0&&m.call(e,l)&&(n[l]=e[l]);return n})(n,["components"]);return(0,l.kt)("wrapper",(t=p(p({},b),c),A(t,k({components:s,mdxType:"MDXLayout"}))),(0,l.kt)("h1",p({},{id:"44-expressions"}),"4.4 Expressions"),(0,l.kt)("admonition",p({},{type:"warning"}),(0,l.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,l.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,l.kt)(o.Z,{mdxType:"MarginText"},"1/3"),(0,l.kt)(i.Z,{items:["AI05-0147-1","AI05-0158-1","AI05-0176-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI05-0147-1"),"}"," ","{",(0,l.kt)("em",null,"AI05-0158-1"),"}"," ","{",(0,l.kt)("em",null,"AI05-0176-1"),"}"," ",(0,l.kt)("br",null),"An ",(0,l.kt)("em",null,"expression")," is a formula that defines the computation or retrieval of a value. In this Reference Manual, the term \u201cexpression\u201d refers to a construct of the syntactic category ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," or of any of the following categories: ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0133"},"choice_expression")),", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0139"},"term")),", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0140"},"factor")),", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),", ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0153"},"quantified_expression")),". ",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"syntax"}),"Syntax"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"2"),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},(0,l.kt)("code",null,"expression"),(0,l.kt)("a",{id:"S0132"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"     ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,l.kt)("strong",null,"and")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}"," \t| ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,l.kt)("strong",null,"and")," ",(0,l.kt)("strong",null,"then")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}",(0,l.kt)("br",null),"   | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,l.kt)("strong",null,"or")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}"," \t| ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,l.kt)("strong",null,"or")," ",(0,l.kt)("strong",null,"else")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}",(0,l.kt)("br",null),"   | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,l.kt)("strong",null,"xor")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"2.1/3"),(0,l.kt)(i.Z,{items:["AI05-0158-1"],mdxType:"MarginInfo"}),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},"{",(0,l.kt)("em",null,"AI05-0158-1"),"}"," ",(0,l.kt)("br",null),(0,l.kt)("code",null,"choice_expression"),(0,l.kt)("a",{id:"S0133"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"     ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,l.kt)("strong",null,"and")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}",(0,l.kt)("br",null),"   | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,l.kt)("strong",null,"or")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}",(0,l.kt)("br",null),"   | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,l.kt)("strong",null,"xor")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}",(0,l.kt)("br",null),"   | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,l.kt)("strong",null,"and then")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}",(0,l.kt)("br",null),"   | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,l.kt)("strong",null,"or else")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"2.2/3"),(0,l.kt)(i.Z,{items:["AI05-0158-1"],mdxType:"MarginInfo"}),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},"{",(0,l.kt)("em",null,"AI05-0158-1"),"}"," ",(0,l.kt)("br",null),(0,l.kt)("code",null,"choice_relation"),(0,l.kt)("a",{id:"S0134"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"     ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," [",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0143"},"relational_operator"))," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),"]",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"3/4"),(0,l.kt)(i.Z,{items:["AI05-0158-1","AI12-0022-1","AI12-0039-1"],mdxType:"MarginInfo"}),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},"{",(0,l.kt)("em",null,"AI05-0158-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0022-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0039-1"),"}"," ",(0,l.kt)("br",null),(0,l.kt)("code",null,"relation"),(0,l.kt)("a",{id:"S0135"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"     ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," [",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0143"},"relational_operator"))," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),"]",(0,l.kt)("br",null),"   | ",(0,l.kt)("em",null,"tested_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," [",(0,l.kt)("strong",null,"not"),"] ",(0,l.kt)("strong",null,"in")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0136"},"membership_choice_list")),(0,l.kt)("br",null),"   | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression")),(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"3.1/5"),(0,l.kt)(i.Z,{items:["AI05-0158-1","AI12-0212-1"],mdxType:"MarginInfo"}),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},"{",(0,l.kt)("em",null,"AI05-0158-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0212-1"),"}"," ",(0,l.kt)("br",null),(0,l.kt)("code",null,"membership_choice_list"),(0,l.kt)("a",{id:"S0136"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0137"},"membership_choice"))," ","{","'|'  ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0137"},"membership_choice")),"}",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"3.2/4"),(0,l.kt)(i.Z,{items:["AI05-0158-1","AI12-0039-1"],mdxType:"MarginInfo"}),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},"{",(0,l.kt)("em",null,"AI05-0158-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0039-1"),"}"," ",(0,l.kt)("br",null),(0,l.kt)("code",null,"membership_choice"),(0,l.kt)("a",{id:"S0137"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("em",null,"choice_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-3/AA-3.5#S0037"},"range"))," | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark")),(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"4"),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},(0,l.kt)("code",null,"simple_expression"),(0,l.kt)("a",{id:"S0138"}),(0,l.kt)("code",null," ::= "),"[",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0145"},"unary_adding_operator")),"] ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0139"},"term"))," ","{",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0144"},"binary_adding_operator"))," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0139"},"term")),"}",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"5"),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},(0,l.kt)("code",null,"term"),(0,l.kt)("a",{id:"S0139"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0140"},"factor"))," ","{",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0146"},"multiplying_operator"))," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0140"},"factor")),"}",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"6"),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},(0,l.kt)("code",null,"factor"),(0,l.kt)("a",{id:"S0140"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," [** ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),"] | ",(0,l.kt)("strong",null,"abs")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," | ",(0,l.kt)("strong",null,"not")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"7/5"),(0,l.kt)(i.Z,{items:["AI05-0003-1","AI05-0147-1","AI05-0176-1","AI12-0236-1"],mdxType:"MarginInfo"}),(0,l.kt)(a.Z,{mdxType:"CodeBlock"},"{",(0,l.kt)("em",null,"AI05-0003-1"),"}"," ","{",(0,l.kt)("em",null,"AI05-0147-1"),"}"," ","{",(0,l.kt)("em",null,"AI05-0176-1"),"}"," ","{",(0,l.kt)("em",null,"AI12-0236-1"),"}"," ",(0,l.kt)("br",null),(0,l.kt)("code",null,"primary"),(0,l.kt)("a",{id:"S0141"}),(0,l.kt)("code",null," ::= "),(0,l.kt)("br",null),"    ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-2/AA-2.4#S0006"},"numeric_literal"))," | ",(0,l.kt)("strong",null,"null")," | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.3#S0106"},"aggregate")),(0,l.kt)("br",null),"  | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," | ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator"))," | (",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),")",(0,l.kt)("br",null),"  | (",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),") | (",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0153"},"quantified_expression")),")",(0,l.kt)("br",null),"  | (",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0156"},"declare_expression")),")",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"8"),(0,l.kt)("p",null,"A ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," used as a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," shall resolve to denote an object or a value. ",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"8.a"),(0,l.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,l.kt)("strong",null),"This replaces RM83-4.4(3). We don't need to mention named numbers explicitly, because the name of a named number denotes a value. We don't need to mention attributes explicitly, because attributes now denote (rather than yield) values in general. Also, the new wording allows attributes that denote objects, which should always have been allowed (in case the implementation chose to have such a thing). ",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"8.b"),(0,l.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,l.kt)("strong",null),"It might seem odd that this is an overload resolution rule, but it is relevant during overload resolution. For example, it helps ensure that a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," that consists of only the identifier of a parameterless function is interpreted as a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call"))," rather than directly as a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name")),". ",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"static-semantics"}),"Static Semantics"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9"),(0,l.kt)("p",null,"Each expression has a type; it specifies the computation or retrieval of a value of that type.",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9.1/5"),(0,l.kt)(i.Z,{items:["AI12-0317-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0317-1"),"}"," ",(0,l.kt)("br",null),"A ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," that is an ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," surrounded by ( and ) is known as a ",(0,l.kt)("em",null,"parenthesized expression"),".",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9.2/5"),(0,l.kt)(i.Z,{items:["AI12-0317-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0317-1"),"}"," ",(0,l.kt)("br",null),"Every ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," or ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," consists of one or more ",(0,l.kt)("em",null,"operative constituent")," ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"s or ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"s, only one of which is evaluated as part of evaluating the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," or ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (the ",(0,l.kt)("em",null,"evaluated operative constituent"),"). The operative constituents are determined as follows, according to the form of the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (or ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"):",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9.3/5"),(0,l.kt)("ul",null,(0,l.kt)("li",null,"if the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),", the operative constituents of its ",(0,l.kt)("em",null,"dependent_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"s;",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9.4/5"),(0,l.kt)("li",null,"if the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (or ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),") is a parenthesized expression, a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression")),", or a view conversion, the operative constituent(s) of its operand;",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9.5/5"),(0,l.kt)("li",null,"if the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0156"},"declare_expression")),", the operative constituent(s) of its ",(0,l.kt)("em",null,"body_"),(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),";",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9.6/5"),(0,l.kt)("li",null,"otherwise, the ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (or ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),") itself. ",(0,l.kt)("br",null))),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9.7/5"),(0,l.kt)(i.Z,{items:["AI12-0317-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0317-1"),"}"," ",(0,l.kt)("br",null),"In certain contexts, we specify that an operative constituent shall (or shall not) be ",(0,l.kt)("em",null,"newly constructed"),". This means the operative constituent shall (or shall not) be an ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.3#S0106"},"aggregate"))," or a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call")),"; in either case, a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," is permitted.",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"9.a/5"),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,l.kt)("strong",null,"To be honest: "),"If an ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0149"},"if_expression"))," does not have an ",(0,l.kt)("strong",null,"else"),' clause, "True" is an operative constituent of the ',(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," and it can be the evaluated operative constituent. ",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"10"),(0,l.kt)("p",null,"The value of a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," that is a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," denoting an object is the value of the object.",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"10.1/5"),(0,l.kt)(i.Z,{items:["AI12-0227-1"],mdxType:"MarginInfo"}),(0,l.kt)("p",null,"{",(0,l.kt)("em",null,"AI12-0227-1"),"}"," ",(0,l.kt)("br",null),"An expression of a numeric universal type is evaluated as if it has type ",(0,l.kt)("em",null,"root_integer")," (for ",(0,l.kt)("em",null,"universal_integer"),") or ",(0,l.kt)("em",null,"root_real")," (otherwise) unless the context identifies a specific type (in which case that type is used).",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"10.a/5"),(0,l.kt)(r.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,l.kt)("strong",null),"This has no effect for a static expression; its value may be arbitrarily small or large since no specific type is expected for any expression for which this rule specifies one of the root types. The only effect of this rule is to allow Constraint_Error to be raised if the value is outside of the base range of ",(0,l.kt)("em",null,"root_integer")," or ",(0,l.kt)("em",null,"root_real")," when the expression is not static. ",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"10.b/5"),(0,l.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,l.kt)("strong",null),"This rule means that implementations don't have to support unlimited range math at run time for universal expressions. Note that universal expressions for which the context doesn't specify a specific type are quite rare; attribute prefixes and results are the only known cases. (For operators, ",(0,l.kt)("a",{href:"../AA-8/AA-8.6"},"8.6")," already specifies that the operator of a root type be used, which provides a specific type.) ",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"11"),(0,l.kt)("p",null,"For the evaluation of a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," that is a ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," denoting an object of an unconstrained numeric subtype, if the value of the object is outside the base range of its type, the implementation may either raise Constraint_Error or return the value of the object. ",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"11.a/3"),(0,l.kt)(i.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,l.kt)("strong",null),"{",(0,l.kt)("em",null,"AI05-0299-1"),"}"," ",(0,l.kt)("br",null),"This means that if extra-range intermediates are used to hold the value of an object of an unconstrained numeric subtype, a Constraint_Error can be raised on a read of the object, rather than only on an assignment to it. Similarly, it means that computing the value of an object of such a subtype can be deferred until the first read of the object (presuming no side effects other than failing an Overflow_Check are possible). This permission is over and above that provided by subclause ",(0,l.kt)("a",{href:"../AA-11/AA-11.6"},"11.6"),", since this allows the Constraint_Error to move to a different handler. ",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"11.b"),(0,l.kt)(r.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,l.kt)("strong",null),"This permission is intended to allow extra-range registers to be used efficiently to hold parameters and local variables, even if they might need to be transferred into smaller registers for performing certain predefined operations. ",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"11.c"),(0,l.kt)(r.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,l.kt)("strong",null),"There is no need to mention other kinds of ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),"s, since any Constraint_Error to be raised can be \u201ccharged\u201d to the evaluation of the particular kind of ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),". ",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"examples"}),"Examples"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"12"),(0,l.kt)("p",null,(0,l.kt)("em",null,"Examples of primaries:")," ",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"13"),(0,l.kt)(a.Z,{language:"ada",mdxType:"CodeBlock"},"4.0                --  real literal","\n","Pi                 --  named number","\n","(1 .. 10 =",">"," 0)     --  array aggregate","\n","Sum                --  variable","\n","Integer'Last       --  attribute","\n","Sine(X)            --  function call","\n","Color'(Blue)       --  qualified expression","\n","Real(M*N)          --  conversion","\n","(Line_Count + 10)  --  parenthesized expression ","\n"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"14"),(0,l.kt)("p",null,(0,l.kt)("em",null,"Examples of expressions:")," ",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15/2"),(0,l.kt)(i.Z,{items:["AI95-00433-01"],mdxType:"MarginInfo"}),(0,l.kt)(a.Z,{language:"ada",mdxType:"CodeBlock"},"--  ","{","AI95-00433-01","}","\n"," Volume                      -- primary","\n","not Destroyed               -- factor","\n","2*Line_Count                -- term","\n","-4.0                        -- simple expression","\n","-4.0 + A                    -- simple expression","\n","B**2 - 4.0*A*C              -- simple expression","\n","R*Sin(\u03b8)*Cos(\u03c6)             -- simple expression","\n",'Password(1 .. 3) = "Bwv"    -- relation',"\n","Count in Small_Int          -- relation","\n","Count not in Small_Int      -- relation","\n","Index = 0 or Item_Hit       -- expression","\n","(Cold and Sunny) or Warm    -- expression (parentheses are required)","\n","A**(B**C)                   -- expression (parentheses are required)","\n"),(0,l.kt)("h4",p({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.a"),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In Ada 83, ",(0,l.kt)("strong",null,"out")," parameters and their nondiscriminant subcomponents are not allowed as ",(0,l.kt)("code",null,"primaries"),". These restrictions are eliminated in Ada 95.",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.b"),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"In various contexts throughout the language where Ada 83 syntax rules had ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),", the corresponding Ada 95 syntax rule has ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),' instead. This reflects the inclusion of modular integer types, which makes the logical operators "',(0,l.kt)("strong",null,"and"),'", "',(0,l.kt)("strong",null,"or"),'", and "',(0,l.kt)("strong",null,"xor"),'" more useful in expressions of an integer type. Requiring parentheses to use these operators in such contexts seemed unnecessary and potentially confusing. Note that the bounds of a ',(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-3/AA-3.5#S0037"},"range"))," still have to be specified by ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),"s, since otherwise ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),'s involving membership tests might be ambiguous. Essentially, the operation ".." is of higher precedence than the logical operators, and hence uses of logical operators still have to be parenthesized when used in a bound of a range. ',(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.c/3"),(0,l.kt)(i.Z,{items:["AI05-0003-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI05-0003-1"),"}"," ",(0,l.kt)("br",null),"Moved ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression"))," from ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," to ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," (see ",(0,l.kt)("a",{href:"../AA-4/AA-4.1"},"4.1"),"). This allows the use of ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression")),"s in more places.",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.d/3"),(0,l.kt)(i.Z,{items:["AI05-0147-1","AI05-0176-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI05-0147-1"),"}"," ","{",(0,l.kt)("em",null,"AI05-0176-1"),"}"," ",(0,l.kt)("br",null),"Added ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression"))," and ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0153"},"quantified_expression"))," to ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),".",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.e/3"),(0,l.kt)(i.Z,{items:["AI05-0158-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI05-0158-1"),"}"," ",(0,l.kt)("br",null),"Expanded membership test syntax (see ",(0,l.kt)("a",{href:"../AA-4/AA-4.5#Subclause_4.5.2"},"4.5.2"),"). ",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"inconsistencies-with-ada-2012"}),"Inconsistencies With Ada 2012"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.f/4"),(0,l.kt)(i.Z,{items:["AI12-0039-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI12-0039-1"),"}"," ",(0,l.kt)("strong",null,(0,l.kt)("br",null),"Corrigendum:"),' Revised membership syntax to eliminate ambiguities. In some cases, previously ambiguous membership expressions will now have an unambiguous meaning. If an Ada 2012 implementation chose the "wrong" meaning, the expression could silently change meaning. Virtually all such expressions will become illegal because of type mismatches (and thus be incompatible, not inconsistent). However, if the choices are all of a Boolean type, resolution might succeed. For instance, A ',(0,l.kt)("strong",null,"in")," B | C ",(0,l.kt)("strong",null,"and")," D now always means (A ",(0,l.kt)("strong",null,"in")," B | C) ",(0,l.kt)("strong",null,"and")," D, but the original Ada 2012 syntax would have allowed it to mean A ",(0,l.kt)("strong",null,"in")," B | (C ",(0,l.kt)("strong",null,"and")," D). If a compiler allowed the expression and interpreted it as the latter, the meaning of the expression would silently change. We expect this to be extremely rare as membership operations on Boolean types are unlikely (and this can happen only in code written for Ada 2012). ",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"incompatibilities-with-ada-2012"}),"Incompatibilities With Ada 2012"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.g/4"),(0,l.kt)(i.Z,{items:["AI12-0039-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI12-0039-1"),"}"," ",(0,l.kt)("strong",null,(0,l.kt)("br",null),"Corrigendum:")," The revised membership syntax will require parentheses in ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0136"},"membership_choice_list")),"s in some cases where the Ada 2012 grammar did not require them. For instance, A ",(0,l.kt)("strong",null,"in")," B ",(0,l.kt)("strong",null,"in")," C | D is now illegal. However, such expressions can be interpreted in multiple ways (either A ",(0,l.kt)("strong",null,"in")," (B ",(0,l.kt)("strong",null,"in")," C) | D or A ",(0,l.kt)("strong",null,"in")," (B ",(0,l.kt)("strong",null,"in")," C | D) for this example), so using such expressions is likely to be dangerous (another compiler might interpret the expression differently). In addition, all such expressions occur only in Ada 2012 syntax; so they should be rare. ",(0,l.kt)("br",null)),(0,l.kt)("h4",p({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.h/5"),(0,l.kt)(i.Z,{items:["AI12-0227-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"correction",title:"Correction:",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI12-0227-1"),"}"," ",(0,l.kt)("strong",null,(0,l.kt)("br",null))," Added wording so that universal expressions evaluated at run time can raise Constraint_Error if the value is outside of the range of ",(0,l.kt)("em",null,"root_integer")," or ",(0,l.kt)("em",null,"root_real"),". We don't document this as an inconsistency because the rule requires no implementation to change (as Constraint_Error is not required); it just allows implementations that already raise Constraint_Error (which is all of them surveyed) to be considered correct.",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.i/5"),(0,l.kt)(i.Z,{items:["AI12-0236-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI12-0236-1"),"}"," ",(0,l.kt)("br",null),"Added ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0156"},"declare_expression"))," to ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),". It shares the rules about parentheses with ",(0,l.kt)("code",null,(0,l.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),"s.",(0,l.kt)("br",null)),(0,l.kt)(o.Z,{mdxType:"MarginText"},"15.j/5"),(0,l.kt)(i.Z,{items:["AI12-0317-1"],mdxType:"MarginInfo"}),(0,l.kt)(r.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,l.kt)("em",null,"AI12-0317-1"),"}"," ",(0,l.kt)("br",null),"Added the definitions of \u201coperative constituent\u201d and \u201cnewly constructed\u201d to centralize definitions that are needed for various rules and definitions across the Reference Manual. In particular, ",(0,l.kt)("em",null,"operative constituent")," is often used when we want the semantics or legality to be unchanged by the presence of parens, qualification, or view conversions. Examples are found in ",(0,l.kt)("a",{href:"../AA-4/AA-4.3#Subclause_4.3.2"},"4.3.2"),", ",(0,l.kt)("a",{href:"../AA-6/AA-6.2"},"6.2"),", and ",(0,l.kt)("a",{href:"../AA-7/AA-7.5"},"7.5"),". ",(0,l.kt)("br",null)))}T.isMDXComponent=!0}}]);