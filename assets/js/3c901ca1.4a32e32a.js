"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8028],{609:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>k,contentTitle:()=>f,default:()=>b,frontMatter:()=>h,metadata:()=>m,toc:()=>y});var o=n(1716),a=n(7556),r=n(3183),l=Object.defineProperty,i=Object.defineProperties,s=Object.getOwnPropertyDescriptors,d=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,p=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?l(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,A=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&c(e,n,t[n]);if(d)for(var n of d(t))p.call(t,n)&&c(e,n,t[n]);return e};const h={sidebar_position:54},f="6.6 Overloading of Operators",m={unversionedId:"arm/AA-6/AA-6.6",id:"arm/AA-6/AA-6.6",title:"6.6 Overloading of Operators",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-6/AA-6.6.mdx",sourceDirName:"arm/AA-6",slug:"/arm/AA-6/AA-6.6",permalink:"/docs/arm/AA-6/AA-6.6",draft:!1,tags:[],version:"current",sidebarPosition:54,frontMatter:{sidebar_position:54},sidebar:"referenceManualSidebar",previous:{title:"6.5 Return Statements",permalink:"/docs/arm/AA-6/AA-6.5"},next:{title:"6.7 Null Procedures",permalink:"/docs/arm/AA-6/AA-6.7"}},k={},y=[{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}],g={toc:y};function b(e){var t,n=e,{components:l}=n,c=((e,t)=>{var n={};for(var o in e)u.call(e,o)&&t.indexOf(o)<0&&(n[o]=e[o]);if(null!=e&&d)for(var o of d(e))t.indexOf(o)<0&&p.call(e,o)&&(n[o]=e[o]);return n})(n,["components"]);return(0,o.kt)("wrapper",(t=A(A({},g),c),i(t,s({components:l,mdxType:"MDXLayout"}))),(0,o.kt)("h1",A({},{id:"66-overloading-of-operators"}),"6.6 Overloading of Operators"),(0,o.kt)("admonition",A({},{type:"warning"}),(0,o.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,o.kt)("a",A({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,o.kt)("p",null,"An ",(0,o.kt)("em",null,"operator")," is a function whose ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.1#S0199"},"designator"))," is an ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),". [Operators, like other functions, may be overloaded.] "),(0,o.kt)("h4",A({},{id:"name-resolution-rules"}),"Name Resolution Rules"),(0,o.kt)("p",null,"Each use of a unary or binary operator is equivalent to a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call"))," with ",(0,o.kt)("em",null,"function_"),(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0093"},"prefix"))," being the corresponding ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol")),", and with (respectively) one or two positional actual parameters being the operand(s) of the operator (in order). "),(0,o.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"To be honest: "),"{",(0,o.kt)("em",null,"AI05-0299-1"),"}",' We also use the term operator (in Clause 4 and in 6.1) to refer to one of the syntactic categories defined in 4.5, "Operators and Expression Evaluation" whose names end with "_operator:" ',(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0142"},"logical_operator")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0143"},"relational_operator")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0144"},"binary_adding_operator")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0145"},"unary_adding_operator")),", ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0146"},"multiplying_operator")),", and ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.5#S0147"},"highest_precedence_operator")),". ")),(0,o.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Discussion: "),"{",(0,o.kt)("em",null,"AI05-0005-1"),"}"," This equivalence extends to uses of ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call"))," in most other language rules. However, as often happens, the equivalence is not perfect, as operator calls are not a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),", while a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.4#S0218"},"function_call"))," is a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),". Thus, operator calls cannot be used in contexts that require a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name"))," (such as a rename of an object). A direct fix for this problem would be very disruptive, and thus we have not done that. However, qualifying an operator call can be used as a workaround in contexts that require a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-4/AA-4.1#S0091"},"name")),". ")),(0,o.kt)("h4",A({},{id:"legality-rules"}),"Legality Rules"),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0143-1"),"}"," The ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.1#S0196"},"subprogram_specification"))," of a unary or binary operator shall have one or two parameters, respectively. The parameters shall be of mode ",(0,o.kt)("strong",null,"in"),". A generic function instantiation whose ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.1#S0199"},"designator"))," is an ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.1#S0202"},"operator_symbol"))," is only allowed if the specification of the generic function has the corresponding number of parameters, and they are all of mode ",(0,o.kt)("strong",null,"in"),"."),(0,o.kt)("p",null,(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-3/AA-3.7#S0063"},"Default_expression")),"s are not allowed for the parameters of an operator (whether the operator is declared with an explicit ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-6/AA-6.1#S0196"},"subprogram_specification"))," or by a ",(0,o.kt)("code",null,(0,o.kt)("a",{href:"../AA-12/AA-12.3#S0315"},"generic_instantiation")),")."),(0,o.kt)("p",null,'An explicit declaration of "/=" shall not have a result type of the predefined type Boolean. '),(0,o.kt)("h4",A({},{id:"static-semantics"}),"Static Semantics"),(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0128-1"),"}",' An explicit declaration of "=" whose result type is Boolean implicitly declares an operator "/=" that gives the complementary result. '),(0,o.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,(0,o.kt)("strong",null,"Discussion: "),"{",(0,o.kt)("em",null,"AI05-0128-1"),"}",' A "/=" defined by this rule is considered user-defined, which means that it will be inherited by a derived type. "User-defined" means "not language-defined" for the purposes of inheritance, that is anything other than predefined operators. ')),(0,o.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"NOTE 1   ","{",(0,o.kt)("em",null,"AI12-0440-1"),"}",' The operators "+" and "" are both unary and binary operators, and hence can be overloaded with both one- and two-parameter functions. ')),(0,o.kt)("h4",A({},{id:"examples"}),"Examples"),(0,o.kt)("p",null,(0,o.kt)("em",null,"Examples of user-defined operators:")," "),(0,o.kt)(a.Z,{mdxType:"CodeBlock"},'function "+" (Left, Right : Matrix) return Matrix;',"\n",'function "+" (Left, Right : Vector) return Vector;',"\n","\n","--  assuming that A, B, and C are of the type Vector","\n","--  the following two statements are equivalent:","\n","\n","A := B + C;","\n",'A := "+"(B, C);',"\n"),(0,o.kt)("h4",A({},{id:"extensions-to-ada-83"}),"Extensions to Ada 83"),(0,o.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,'Explicit declarations of "=" are now permitted for any combination of parameter and result types.')),(0,o.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,'Explicit declarations of "/=" are now permitted, so long as the result type is not Boolean. ')),(0,o.kt)("h4",A({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,o.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0128-1"),"}"," ",(0,o.kt)("strong",null,"Correction:"),' Corrected the wording so that only explicit declarations of "=" cause an implicit declaration of "/="; otherwise, we could get multiple implicit definitions of "/=" without an obvious way to chose between them.')),(0,o.kt)(r.Z,{type:"note",mdxType:"Admonition"},(0,o.kt)("p",null,"{",(0,o.kt)("em",null,"AI05-0143-1"),"}"," Added wording so that operators only allow parameters of mode ",(0,o.kt)("strong",null,"in"),". This was made necessary by the elimination elsewhere of the restriction that function parameters be only of mode ",(0,o.kt)("strong",null,"in"),". ")))}b.isMDXComponent=!0}}]);