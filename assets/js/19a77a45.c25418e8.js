"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7407],{4628:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>f,contentTitle:()=>p,default:()=>x,frontMatter:()=>h,metadata:()=>m,toc:()=>g});var o=n(1716),l=n(7556),a=n(3183),i=Object.defineProperty,r=Object.defineProperties,s=Object.getOwnPropertyDescriptors,A=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,k=(e,t,n)=>t in e?i(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,c=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&k(e,n,t[n]);if(A)for(var n of A(t))d.call(t,n)&&k(e,n,t[n]);return e};const h={sidebar_position:32},p="4.4 Expressions",m={unversionedId:"arm/AA-4/AA-4.4",id:"arm/AA-4/AA-4.4",title:"4.4 Expressions",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-4/AA-4.4.mdx",sourceDirName:"arm/AA-4",slug:"/arm/AA-4/AA-4.4",permalink:"/docs/arm/AA-4/AA-4.4",draft:!1,tags:[],version:"current",sidebarPosition:32,frontMatter:{sidebar_position:32},sidebar:"referenceManualSidebar",previous:{title:"4.3 Aggregates",permalink:"/docs/arm/AA-4/AA-4.3"},next:{title:"4.5 Operators and Expression Evaluation",permalink:"/docs/arm/AA-4/AA-4.5"}},f={},g=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Inconsistencies With Ada 2012",id:"inconsistencies-with-ada-2012",level:4},{value:"Incompatibilities With Ada 2012",id:"incompatibilities-with-ada-2012",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],y={toc:g};function x(e){var t,n=e,{components:i}=n,k=((e,t)=>{var n={};for(var o in e)u.call(e,o)&&t.indexOf(o)<0&&(n[o]=e[o]);if(null!=e&&A)for(var o of A(e))t.indexOf(o)<0&&d.call(e,o)&&(n[o]=e[o]);return n})(n,["components"]);return(0,o.kt)("wrapper",(t=c(c({},y),k),r(t,s({components:i,mdxType:"MDXLayout"}))),(0,o.kt)("h1",c({},{id:"44-expressions"}),"4.4 Expressions"),(0,o.kt)("admonition",c({},{type:"warning"}),(0,o.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,o.kt)("a",c({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0147-1"),"}"," ","{",(0,o.kt)("em",null,"AI05-0158-1"),"}"," ","{",(0,o.kt)("em",null,"AI05-0176-1"),"}"," An ",(0,o.kt)("em",null,"expression"),' is a formula that defines the computation or retrieval of a value. In this Reference Manual, the term "expression" refers to a construct of the syntactic category ',(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," or of any of the following categories: ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0133"},"choice_expression")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0139"},"term")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0140"},"factor")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0153"},"quantified_expression")),". "),(0,o.kt)("h4",c({},{id:"syntax"}),"Syntax"),(0,o.kt)("p",null,(0,o.kt)("code",null,"expression"),(0,o.kt)("a",{id:"S0132"}),(0,o.kt)("code",null," ::= "),"     ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,o.kt)("strong",null,"and")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}"," \t| ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,o.kt)("strong",null,"and")," ",(0,o.kt)("strong",null,"then")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}","   | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,o.kt)("strong",null,"or")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}"," \t| ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,o.kt)("strong",null,"or")," ",(0,o.kt)("strong",null,"else")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}","   | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation"))," ","{",(0,o.kt)("strong",null,"xor")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0135"},"relation")),"}"),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0158-1"),"}"," ",(0,o.kt)("code",null,"choice_expression"),(0,o.kt)("a",{id:"S0133"}),(0,o.kt)("code",null," ::= "),"     ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,o.kt)("strong",null,"and")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}","   | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,o.kt)("strong",null,"or")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}","   | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,o.kt)("strong",null,"xor")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}","   | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,o.kt)("strong",null,"and then")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}","   | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation"))," ","{",(0,o.kt)("strong",null,"or else")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0134"},"choice_relation")),"}"),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0158-1"),"}"," ",(0,o.kt)("code",null,"choice_relation"),(0,o.kt)("a",{id:"S0134"}),(0,o.kt)("code",null," ::= "),"     ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," [",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0143"},"relational_operator"))," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),"]"),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0158-1"),"}"," ","{",(0,o.kt)("em",null,"AI12-0022-1"),"}"," ","{",(0,o.kt)("em",null,"AI12-0039-1"),"}"," ",(0,o.kt)("code",null,"relation"),(0,o.kt)("a",{id:"S0135"}),(0,o.kt)("code",null," ::= "),"     ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," [",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0143"},"relational_operator"))," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),"]   | ",(0,o.kt)("em",null,"tested_"),(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," [",(0,o.kt)("strong",null,"not"),"] ",(0,o.kt)("strong",null,"in")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0136"},"membership_choice_list")),"   | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0158-1"),"}"," ","{",(0,o.kt)("em",null,"AI12-0212-1"),"}"," ",(0,o.kt)("code",null,"membership_choice_list"),(0,o.kt)("a",{id:"S0136"}),(0,o.kt)("code",null," ::= "),(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0137"},"membership_choice"))," ","{","'|' ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0137"},"membership_choice")),"}"),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0158-1"),"}"," ","{",(0,o.kt)("em",null,"AI12-0039-1"),"}"," ",(0,o.kt)("code",null,"membership_choice"),(0,o.kt)("a",{id:"S0137"}),(0,o.kt)("code",null," ::= "),(0,o.kt)("em",null,"choice_"),(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression"))," | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-3/AA-3.5#S0037"},"range"))," | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-3/AA-3.2#S0028"},"subtype_mark"))),(0,o.kt)("p",null,(0,o.kt)("code",null,"simple_expression"),(0,o.kt)("a",{id:"S0138"}),(0,o.kt)("code",null," ::= "),"[",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0145"},"unary_adding_operator")),"] ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0139"},"term"))," ","{",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0144"},"binary_adding_operator"))," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0139"},"term")),"}"),(0,o.kt)("p",null,(0,o.kt)("code",null,"term"),(0,o.kt)("a",{id:"S0139"}),(0,o.kt)("code",null," ::= "),(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0140"},"factor"))," ","{",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0146"},"multiplying_operator"))," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0140"},"factor")),"}"),(0,o.kt)("p",null,(0,o.kt)("code",null,"factor"),(0,o.kt)("a",{id:"S0140"}),(0,o.kt)("code",null," ::= "),(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," [** ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),"] | ",(0,o.kt)("strong",null,"abs")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," | ",(0,o.kt)("strong",null,"not")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0003-1"),"}"," ","{",(0,o.kt)("em",null,"AI05-0147-1"),"}"," ","{",(0,o.kt)("em",null,"AI05-0176-1"),"}"," ","{",(0,o.kt)("em",null,"AI12-0236-1"),"}"," ",(0,o.kt)("code",null,"primary"),(0,o.kt)("a",{id:"S0141"}),(0,o.kt)("code",null," ::= "),"    ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-2/AA-2.4#S0006"},"numeric_literal"))," | ",(0,o.kt)("strong",null,"null")," | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-2/AA-2.6#S0016"},"string_literal"))," | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.3#S0106"},"aggregate")),"  | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," | ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator"))," | (",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),")  | (",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),") | (",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0153"},"quantified_expression")),")  | (",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0156"},"declare_expression")),")"),(0,o.kt)("h4",c({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,o.kt)("p",null,"A ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," used as a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," shall resolve to denote an object or a value. "),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Discussion: "),"This replaces RM83-4.4(3). We don't need to mention named numbers explicitly, because the name of a named number denotes a value. We don't need to mention attributes explicitly, because attributes now denote (rather than yield) values in general. Also, the new wording allows attributes that denote objects, which should always have been allowed (in case the implementation chose to have such a thing). ")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Reason: "),"It might seem odd that this is an overload resolution rule, but it is relevant during overload resolution. For example, it helps ensure that a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," that consists of only the identifier of a parameterless function is interpreted as a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call"))," rather than directly as a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0092"},"direct_name")),". ")),(0,o.kt)("h4",c({},{id:"static-semantics"}),"Static Semantics"),(0,o.kt)("p",null,"Each expression has a type; it specifies the computation or retrieval of a value of that type."),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0317-1"),"}"," A ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," that is an ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," surrounded by ( and ) is known as a ",(0,o.kt)("em",null,"parenthesized expression"),"."),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0317-1"),"}"," Every ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," or ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," consists of one or more ",(0,o.kt)("em",null,"operative constituent")," ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"s or ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"s, only one of which is evaluated as part of evaluating the ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," or ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (the ",(0,o.kt)("em",null,"evaluated operative constituent"),"). The operative constituents are determined as follows, according to the form of the ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (or ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),"):"),(0,o.kt)("p",null,"if the ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),", the operative constituents of its ",(0,o.kt)("em",null,"dependent_"),(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"s;"),(0,o.kt)("p",null,"if the ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (or ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),") is a parenthesized expression, a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression")),", or a view conversion, the operative constituent(s) of its operand;"),(0,o.kt)("p",null,"if the ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," is a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0156"},"declare_expression")),", the operative constituent(s) of its ",(0,o.kt)("em",null,"body_"),(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),";"),(0,o.kt)("p",null,"otherwise, the ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," (or ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),") itself. "),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0317-1"),"}"," In certain contexts, we specify that an operative constituent shall (or shall not) be ",(0,o.kt)("em",null,"newly constructed"),". This means the operative constituent shall (or shall not) be an ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.3#S0106"},"aggregate"))," or a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call")),"; in either case, a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-11/AA-11.3#S0309"},"raise_expression"))," is permitted."),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"To be honest: "),"If an ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0149"},"if_expression"))," does not have an ",(0,o.kt)("strong",null,"else"),' clause, "True" is an operative constituent of the ',(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," and it can be the evaluated operative constituent. ")),(0,o.kt)("h4",c({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,o.kt)("p",null,"The value of a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," that is a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," denoting an object is the value of the object."),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0227-1"),"}"," An expression of a numeric universal type is evaluated as if it has type ",(0,o.kt)("em",null,"root_integer")," (for ",(0,o.kt)("em",null,"universal_integer"),") or ",(0,o.kt)("em",null,"root_real")," (otherwise) unless the context identifies a specific type (in which case that type is used)."),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Ramification: "),"This has no effect for a static expression; its value may be arbitrarily small or large since no specific type is expected for any expression for which this rule specifies one of the root types. The only effect of this rule is to allow Constraint_Error to be raised if the value is outside of the base range of ",(0,o.kt)("em",null,"root_integer")," or ",(0,o.kt)("em",null,"root_real")," when the expression is not static. ")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Reason: "),"This rule means that implementations don't have to support unlimited range math at run time for universal expressions. Note that universal expressions for which the context doesn't specify a specific type are quite rare; attribute prefixes and results are the only known cases. (For operators, 8.6 already specifies that the operator of a root type be used, which provides a specific type.) ")),(0,o.kt)("h4",c({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,o.kt)("p",null,"For the evaluation of a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," that is a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," denoting an object of an unconstrained numeric subtype, if the value of the object is outside the base range of its type, the implementation may either raise Constraint_Error or return the value of the object. "),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Ramification: "),"{",(0,o.kt)("em",null,"AI05-0299-1"),"}"," This means that if extra-range intermediates are used to hold the value of an object of an unconstrained numeric subtype, a Constraint_Error can be raised on a read of the object, rather than only on an assignment to it. Similarly, it means that computing the value of an object of such a subtype can be deferred until the first read of the object (presuming no side effects other than failing an Overflow_Check are possible). This permission is over and above that provided by subclause 11.6, since this allows the Constraint_Error to move to a different handler. ")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Reason: "),"This permission is intended to allow extra-range registers to be used efficiently to hold parameters and local variables, even if they might need to be transferred into smaller registers for performing certain predefined operations. ")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Discussion: "),"There is no need to mention other kinds of ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),'s, since any Constraint_Error to be raised can be "charged" to the evaluation of the particular kind of ',(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),". ")),(0,o.kt)("h4",c({},{id:"examples"}),"Examples"),(0,o.kt)("p",null,(0,o.kt)("em",null,"Examples of primaries:")," "),(0,o.kt)(l.Z,{mdxType:"CodeBlock"},"4.0                --  real literal","\n","Pi                 --  named number","\n","(1 .. 10 =",">"," 0)     --  array aggregate","\n","Sum                --  variable","\n","Integer'Last       --  attribute","\n","Sine(X)            --  function call","\n","Color'(Blue)       --  qualified expression","\n","Real(M*N)          --  conversion","\n","(Line_Count + 10)  --  parenthesized expression ","\n"),(0,o.kt)("p",null,(0,o.kt)("em",null,"Examples of expressions:")," "),(0,o.kt)(l.Z,{mdxType:"CodeBlock"},"{","AI95-00433-01","}"," Volume                      -- primary","\n","not Destroyed               -- factor","\n","2*Line_Count                -- term","\n","-4.0                        -- simple expression","\n","-4.0 + A                    -- simple expression","\n","B**2 - 4.0*A*C              -- simple expression","\n","R*Sin()*Cos()             -- simple expression","\n",'Password(1 .. 3) = "Bwv"    -- relation',"\n","Count in Small_Int          -- relation","\n","Count not in Small_Int      -- relation","\n","Index = 0 or Item_Hit       -- expression","\n","(Cold and Sunny) or Warm    -- expression (parentheses are required)","\n","A**(B**C)                   -- expression (parentheses are required)","\n"),(0,o.kt)("h4",c({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"In Ada 83, ",(0,o.kt)("strong",null,"out")," parameters and their nondiscriminant subcomponents are not allowed as ",(0,o.kt)("code",null,"primaries"),". These restrictions are eliminated in Ada 95.")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"In various contexts throughout the language where Ada 83 syntax rules had ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),", the corresponding Ada 95 syntax rule has ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),' instead. This reflects the inclusion of modular integer types, which makes the logical operators "',(0,o.kt)("strong",null,"and"),'", "',(0,o.kt)("strong",null,"or"),'", and "',(0,o.kt)("strong",null,"xor"),'" more useful in expressions of an integer type. Requiring parentheses to use these operators in such contexts seemed unnecessary and potentially confusing. Note that the bounds of a ',(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-3/AA-3.5#S0037"},"range"))," still have to be specified by ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0138"},"simple_expression")),"s, since otherwise ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),'s involving membership tests might be ambiguous. Essentially, the operation ".." is of higher precedence than the logical operators, and hence uses of logical operators still have to be parenthesized when used in a bound of a range. ')),(0,o.kt)("h4",c({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0003-1"),"}"," Moved ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression"))," from ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary"))," to ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," (see 4.1). This allows the use of ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.7#S0163"},"qualified_expression")),"s in more places.")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0147-1"),"}"," ","{",(0,o.kt)("em",null,"AI05-0176-1"),"}"," Added ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression"))," and ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0153"},"quantified_expression"))," to ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),".")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0158-1"),"}"," Expanded membership test syntax (see 4.5.2). ")),(0,o.kt)("h4",c({},{id:"inconsistencies-with-ada-2012"}),"Inconsistencies With Ada 2012"),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0039-1"),"}"," ",(0,o.kt)("strong",null,"Corrigendum:"),' Revised membership syntax to eliminate ambiguities. In some cases, previously ambiguous membership expressions will now have an unambiguous meaning. If an Ada 2012 implementation chose the "wrong" meaning, the expression could silently change meaning. Virtually all such expressions will become illegal because of type mismatches (and thus be incompatible, not inconsistent). However, if the choices are all of a Boolean type, resolution might succeed. For instance, A ',(0,o.kt)("strong",null,"in")," B | C ",(0,o.kt)("strong",null,"and")," D now always means (A ",(0,o.kt)("strong",null,"in")," B | C) ",(0,o.kt)("strong",null,"and")," D, but the original Ada 2012 syntax would have allowed it to mean A ",(0,o.kt)("strong",null,"in")," B | (C ",(0,o.kt)("strong",null,"and")," D). If a compiler allowed the expression and interpreted it as the latter, the meaning of the expression would silently change. We expect this to be extremely rare as membership operations on Boolean types are unlikely (and this can happen only in code written for Ada 2012). ")),(0,o.kt)("h4",c({},{id:"incompatibilities-with-ada-2012"}),"Incompatibilities With Ada 2012"),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0039-1"),"}"," ",(0,o.kt)("strong",null,"Corrigendum:")," The revised membership syntax will require parentheses in ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0136"},"membership_choice_list")),"s in some cases where the Ada 2012 grammar did not require them. For instance, A ",(0,o.kt)("strong",null,"in")," B ",(0,o.kt)("strong",null,"in")," C | D is now illegal. However, such expressions can be interpreted in multiple ways (either A ",(0,o.kt)("strong",null,"in")," (B ",(0,o.kt)("strong",null,"in")," C) | D or A ",(0,o.kt)("strong",null,"in")," (B ",(0,o.kt)("strong",null,"in")," C | D) for this example), so using such expressions is likely to be dangerous (another compiler might interpret the expression differently). In addition, all such expressions occur only in Ada 2012 syntax; so they should be rare. ")),(0,o.kt)("h4",c({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0227-1"),"}"," ",(0,o.kt)("strong",null,"Correction:")," Added wording so that universal expressions evaluated at run time can raise Constraint_Error if the value is outside of the range of ",(0,o.kt)("em",null,"root_integer")," or ",(0,o.kt)("em",null,"root_real"),". We don't document this as an inconsistency because the rule requires no implementation to change (as Constraint_Error is not required); it just allows implementations that already raise Constraint_Error (which is all of them surveyed) to be considered correct.")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0236-1"),"}"," Added ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0156"},"declare_expression"))," to ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.4#S0141"},"primary")),". It shares the rules about parentheses with ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0148"},"conditional_expression")),"s.")),(0,o.kt)(a.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI12-0317-1"),"}",' Added the definitions of "operative constituent" and "newly constructed" to centralize definitions that are needed for various rules and definitions across the Reference Manual. In particular, ',(0,o.kt)("em",null,"operative constituent")," is often used when we want the semantics or legality to be unchanged by the presence of parens, qualification, or view conversions. Examples are found in 4.3.2, 6.2, and 7.5. ")))}x.isMDXComponent=!0}}]);