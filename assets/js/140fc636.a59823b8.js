"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6157],{76855:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>x,contentTitle:()=>f,default:()=>Z,frontMatter:()=>h,metadata:()=>g,toc:()=>T});var a=n(91716),r=n(21256),o=n(24895),i=n(28090),l=n(48424),s=n(82262),d=Object.defineProperty,u=Object.defineProperties,m=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,k=Object.prototype.hasOwnProperty,y=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?d(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,A=(e,t)=>{for(var n in t||(t={}))k.call(t,n)&&c(e,n,t[n]);if(p)for(var n of p(t))y.call(t,n)&&c(e,n,t[n]);return e};const h={sidebar_position:108},f="13.4 Enumeration Representation Clauses",g={unversionedId:"arm/AA-13/AA-13.4",id:"arm/AA-13/AA-13.4",title:"13.4 Enumeration Representation Clauses",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-13/AA-13.4.mdx",sourceDirName:"arm/AA-13",slug:"/arm/AA-13/AA-13.4",permalink:"/docs/arm/AA-13/AA-13.4",draft:!1,tags:[],version:"current",sidebarPosition:108,frontMatter:{sidebar_position:108},sidebar:"referenceManualSidebar",previous:{title:"13.3 Operational and Representation Attributes",permalink:"/docs/arm/AA-13/AA-13.3"},next:{title:"13.5 Record Layout",permalink:"/docs/arm/AA-13/AA-13.5"}},x={},T=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4}],b={toc:T};function Z(e){var t,n=e,{components:d}=n,c=((e,t)=>{var n={};for(var a in e)k.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&p)for(var a of p(e))t.indexOf(a)<0&&y.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=A(A({},b),c),u(t,m({components:d,mdxType:"MDXLayout"}))),(0,a.kt)("h1",A({},{id:"134-enumeration-representation-clauses"}),"13.4 Enumeration Representation Clauses"),(0,a.kt)("admonition",A({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",A({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"1"),(0,a.kt)("p",null,"[An ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause"))," specifies the internal codes for enumeration literals.] ",(0,a.kt)("br",null)),(0,a.kt)("h4",A({},{id:"syntax"}),"Syntax"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"2"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"enumeration_representation_clause"),(0,a.kt)("a",{id:"S0350"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("br",null),"    ",(0,a.kt)("strong",null,"for")," ",(0,a.kt)("em",null,"first_subtype_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0345"},"local_name"))," ",(0,a.kt)("strong",null,"use")," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0351"},"enumeration_aggregate")),";",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"3"),(0,a.kt)(i.Z,{mdxType:"CodeBlock"},(0,a.kt)("code",null,"enumeration_aggregate"),(0,a.kt)("a",{id:"S0351"}),(0,a.kt)("code",null," ::= "),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0113"},"array_aggregate")),(0,a.kt)("br",null)),(0,a.kt)("h4",A({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"4"),(0,a.kt)("p",null,"The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0351"},"enumeration_aggregate"))," shall be written as a one-dimensional ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0113"},"array_aggregate")),", for which the index subtype is the unconstrained subtype of the enumeration type, and each component expression is expected to be of any integer type. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"4.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,a.kt)("strong",null),"The \u201cfull coverage rules\u201d for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0106"},"aggregate")),"s applies. An ",(0,a.kt)("strong",null,"others")," is not allowed \u2014 there is no applicable index constraint in this context. ",(0,a.kt)("br",null))),(0,a.kt)("h4",A({},{id:"legality-rules"}),"Legality Rules"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"5"),(0,a.kt)("p",null,"The ",(0,a.kt)("em",null,"first_subtype_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0345"},"local_name"))," of an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause"))," shall denote an enumeration subtype. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"5.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,a.kt)("strong",null),"As for all type-related representation items, the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0345"},"local_name"))," is required to denote a first subtype. ",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"6/2"),(0,a.kt)(s.Z,{items:["AI95-00287-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"Each component of the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0113"},"array_aggregate"))," shall be given by an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," rather than a ","<",">",". The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),"s given in the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0113"},"array_aggregate"))," shall be static, and shall specify distinct integer codes for each value of the enumeration type; the associated integer codes shall satisfy the predefined ordering relation of the type. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"6.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",mdxType:"Admonition"},(0,a.kt)("strong",null),"Each value of the enumeration type has to be given an internal code, even if the first subtype of the enumeration type is constrained to only a subrange (this is only possible if the enumeration type is a derived type). This \u201cfull coverage\u201d requirement is important because one may refer to Enum'Base'First and Enum'Base'Last, which need to have defined representations. ",(0,a.kt)("br",null))),(0,a.kt)("h4",A({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"7"),(0,a.kt)("p",null,"An ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause"))," specifies the ",(0,a.kt)("em",null,"coding")," aspect of representation. The coding consists of the ",(0,a.kt)("em",null,"internal code")," for each enumeration literal, that is, the integral value used internally to represent each literal.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"7.a/3"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,a.kt)("strong",null,"Aspect Description for "),(0,a.kt)("strong",null,"Coding: "),"Internal representation of enumeration literals. Specified by an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause")),", not by an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.1#S0346"},"aspect_specification")),".",(0,a.kt)("br",null))),(0,a.kt)("h4",A({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"8"),(0,a.kt)("p",null,"For nonboolean enumeration types, if the coding is not specified for the type, then for each value of the type, the internal code shall be equal to its position number. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"8.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",mdxType:"Admonition"},(0,a.kt)("strong",null),"This default representation is already used by all known Ada compilers for nonboolean enumeration types. Therefore, we make it a requirement so users can depend on it, rather than feeling obliged to supply for every enumeration type an enumeration representation clause that is equivalent to this default rule. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"8.b"),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,a.kt)("strong",null),"For boolean types, it is relatively common to use all ones for True, and all zeros for False, since some hardware supports that directly. Of course, for a one-bit Boolean object (like in a packed array), False is presumably zero and True is presumably one (choosing the reverse would be extremely unfriendly!). ",(0,a.kt)("br",null))),(0,a.kt)("h4",A({},{id:"implementation-advice"}),"Implementation Advice"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"9"),(0,a.kt)("p",null,"The recommended level of support for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause")),"s is: ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10/5"),(0,a.kt)(s.Z,{items:["AI12-0444-1"],mdxType:"MarginInfo"}),(0,a.kt)("ul",null,(0,a.kt)("li",null,"An implementation should support at least the internal codes in the range System.Min_Int .. System.Max_Int. An implementation is not required to  support ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause")),"s for boolean types. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,a.kt)("strong",null),"The implementation may support numbers outside the above range, such as numbers greater than System.Max_Int. See AI83-00564. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.b"),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",mdxType:"Admonition"},(0,a.kt)("strong",null),"The benefits of specifying the internal coding of a boolean type do not outweigh the implementation costs. Consider, for example, the implementation of the logical operators on a packed array of booleans with strange internal codes. It's implementable, but not worth it. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.c/2"),(0,a.kt)(r.Z,{type:"aarm",aarm:"implementation-advice",mdxType:"Admonition"},(0,a.kt)("strong",null),"The recommended level of support for ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause")),"s should be followed.",(0,a.kt)("br",null))),(0,a.kt)("h4",A({},{id:"static-semantics-1"}),"Static Semantics"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.1/5"),(0,a.kt)(s.Z,{items:["AI12-0237-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"For every discrete subtype S, the following attributes are defined: ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.2/5"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"S'Enum_Rep"),(0,a.kt)(s.Z,{items:["AI12-0237-1"],mdxType:"MarginInfo"}),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"S'Enum_Rep denotes a function with the following specification: ",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.3/5"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"function S'Enum_Rep (Arg : S'Base) return universal_integer","\n"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.4/5"),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"This function returns the representation value of the value of Arg, as a value of type ",(0,a.kt)("em",null,"universal_integer"),". The ",(0,a.kt)("em",null,"representation value")," is the internal code specified in an enumeration representation clause, if any, for the type corresponding to the value of Arg, and otherwise is the position number of the value.",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.5/5"),(0,a.kt)("dt",null,(0,a.kt)("br",null),"S'Enum_Val"),(0,a.kt)(s.Z,{items:["AI12-0237-1"],mdxType:"MarginInfo"}),(0,a.kt)("dd",null,"S'Enum_Val denotes a function with the following specification: ",(0,a.kt)("br",null))),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.6/5"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"function S'Enum_Val (Arg : universal_integer) return S'Base","\n"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.7/5"),(0,a.kt)("dl",null,(0,a.kt)("dd",null,"This function returns a value of the type of S whose representation value equals the value of Arg. For the evaluation of a call on S'Enum_Val, if there is no value in the base range of its type with the given representation value, Constraint_Error is raised.",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"10.d/5"),(0,a.kt)(r.Z,{type:"aarm",aarm:"reason",mdxType:"Admonition"},(0,a.kt)("strong",null),"We define these on all discrete types so that they can be used inside of a generic unit on a subtype of a generic formal discrete type. They're not useful on integer types (they have the same effect as S'Pos and S'Val). ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"11/5"),(0,a.kt)(s.Z,{items:["AI95-00137-01","AI05-0299-1","AI12-0237-1","AI12-0442-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE   ","{",(0,a.kt)("em",null,"8652/0009"),"}"," Attribute Enum_Rep  can  be used to query the internal codes used for an enumeration type; attribute Enum_Val can  be used to convert from an internal code to an enumeration value. The other attributes of the type, such as Succ, Pred, and Pos, are unaffected by an  ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause")),". For example, Pos always returns the position number, ",(0,a.kt)("em",null,"not")," an  internal integer code that was  specified in an ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause")),". ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"11.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,a.kt)("strong",null),"Suppose the enumeration type in question is derived: ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"11.b"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"type T1 is (Red, Green, Blue);","\n","subtype S1 is T1 range Red .. Green;","\n","type S2 is new S1;","\n","for S2 use (Red =",">"," 10, Green =",">"," 20, Blue =",">"," 30);","\n")),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"11.c/1"),(0,a.kt)(s.Z,{items:["AI95-00137-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"8652/0009"),"}"," The ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause"))," has to specify values for all enumerals, even ones that are not in S2 (such as Blue). The Base attribute can be used to get at these values. For example: ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"11.d"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"for I in S2'Base loop","\n","    ... -- When I equals Blue, the internal code is 30.","\n","end loop;","\n")),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"11.e"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"We considered allowing or requiring \u201c",(0,a.kt)("strong",null,"for")," S2'Base ",(0,a.kt)("strong",null,"use")," ...\u201d in cases like this, but it didn't seem worth the trouble. ",(0,a.kt)("br",null))),(0,a.kt)("h4",A({},{id:"examples"}),"Examples"),(0,a.kt)(l.Z,{mdxType:"MarginText"},"12/5"),(0,a.kt)(s.Z,{items:["AI12-0312-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,(0,a.kt)("em",null,"Examples  of  enumeration representation clauses :")," ",(0,a.kt)("br",null)),(0,a.kt)(l.Z,{mdxType:"MarginText"},"13"),(0,a.kt)(i.Z,{language:"ada",mdxType:"CodeBlock"},"type Mix_Code is (ADD, SUB, MUL, LDA, STA, STZ);","\n",(0,a.kt)(l.Z,{mdxType:"MarginText"},"14"),"for Mix_Code use","\n","   (ADD =",">"," 1, SUB =",">"," 2, MUL =",">"," 3, LDA =",">"," 8, STA =",">"," 24, STZ =",">","33);","\n",(0,a.kt)(l.Z,{mdxType:"MarginText"},"15/5"),(0,a.kt)(s.Z,{items:["AI12-0312-1"],mdxType:"MarginInfo"}),"-- See ",(0,a.kt)("a",{href:"../AA-3/AA-3.5#Subclause_3.5.2"},"3.5.2"),".","\n","for Roman_Digit use ('I' =",">"," 1,","\n","                     'V' =",">"," 5,","\n","                     'X' =",">"," 10,","\n","                     'L' =",">"," 50,","\n","                     'C' =",">"," 100,","\n","                     'D' =",">"," 500,","\n","                     'M' =",">"," 1000);","\n",(0,a.kt)(l.Z,{mdxType:"MarginText"},"16/5"),(0,a.kt)(s.Z,{items:["AI12-0312-1"],mdxType:"MarginInfo"}),"-- For an example of the use of attribute Enum_Rep, see ",(0,a.kt)("a",{href:"../AA-4/AA-4.2#Subclause_4.2.1"},"4.2.1"),".","\n"),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)("h4",A({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83")),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"16.a"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"As in other similar contexts, Ada 95 allows expressions of any integer type, not just expressions of type ",(0,a.kt)("em",null,"universal_integer"),", for the component expressions in the ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0351"},"enumeration_aggregate")),". The preference rules for the predefined operators of ",(0,a.kt)("em",null,"root_integer")," eliminate any ambiguity.",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"16.b"),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"For portability, we now require that the default coding for an enumeration type be the \u201cobvious\u201d coding using position numbers. This is satisfied by all known implementations. ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)("h4",A({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95")),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"16.c/2"),(0,a.kt)(s.Z,{items:["AI95-00137-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"8652/0009"),"}"," ",(0,a.kt)("strong",null,"Corrigendum:")," Updated to reflect that we no longer have something called ",(0,a.kt)("code",null,"representation_clause"),".",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"16.d/2"),(0,a.kt)(s.Z,{items:["AI95-00287-01"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Added wording to prevent the use of ","<",">"," in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-13/AA-13.4#S0350"},"enumeration_representation_clause")),". (","<",">"," is newly added to ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"../AA-4/AA-4.3#S0113"},"array_aggregate")),"s.) ",(0,a.kt)("br",null))),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)("h4",A({},{id:"extensions-to-ada-2012"}),"Extensions to Ada 2012")),(0,a.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,a.kt)(l.Z,{mdxType:"MarginText"},"16.e/5"),(0,a.kt)(s.Z,{items:["AI12-0237-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Attributes Enum_Rep and Enum_Val are new. ",(0,a.kt)("br",null))))}Z.isMDXComponent=!0}}]);