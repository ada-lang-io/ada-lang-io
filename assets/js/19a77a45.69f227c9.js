"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4384],{56051:(e,s,r)=>{r.r(s),r.d(s,{assets:()=>x,contentTitle:()=>t,default:()=>p,frontMatter:()=>l,metadata:()=>A,toc:()=>j});var n=r(74848),i=r(28453),a=r(13842),d=r(91435),c=r(21432),o=r(79162),h=r(34421);const l={sidebar_position:34},t="4.4 Expressions",A={id:"arm/AA-4/AA-4.4",title:"4.4 Expressions",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-4/AA-4.4.mdx",sourceDirName:"arm/AA-4",slug:"/arm/AA-4/AA-4.4",permalink:"/docs/arm/AA-4/AA-4.4",draft:!1,unlisted:!1,tags:[],version:"current",sidebarPosition:34,frontMatter:{sidebar_position:34},sidebar:"referenceManualSidebar",previous:{title:"4.3 Aggregates",permalink:"/docs/arm/AA-4/AA-4.3"},next:{title:"4.5 Operators and Expression Evaluation",permalink:"/docs/arm/AA-4/AA-4.5"}},x={},j=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Inconsistencies With Ada 2012",id:"inconsistencies-with-ada-2012",level:4},{value:"Incompatibilities With Ada 2012",id:"incompatibilities-with-ada-2012",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function m(e){const s={a:"a",admonition:"admonition",h1:"h1",h4:"h4",p:"p",...(0,i.R)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(s.h1,{id:"44-expressions",children:"4.4 Expressions"}),"\n",(0,n.jsx)(s.admonition,{type:"danger",children:(0,n.jsxs)(s.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.jsx)(s.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,n.jsx)(o.A,{children:"1/3"}),"\n",(0,n.jsx)(h.A,{items:["AI05-0147-1","AI05-0158-1","AI05-0176-1"]}),"\n",(0,n.jsxs)("p",{children:["An ",(0,n.jsx)("em",{children:"expression"})," is a formula that defines the computation or retrieval of a value. In this Reference Manual, the term \u201cexpression\u201d refers to a construct of the syntactic category ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," or of any of the following categories: ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0133",children:"choice_expression"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0139",children:"term"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0140",children:"factor"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0148",children:"conditional_expression"})}),", ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0153",children:"quantified_expression"})}),". ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(s.h4,{id:"syntax",children:"Syntax"}),"\n",(0,n.jsx)(o.A,{children:"2"}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("code",{children:"expression"}),(0,n.jsx)("a",{id:"S0132"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("br",{}),"     ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})})," ","{",(0,n.jsx)("strong",{children:"and"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})}),"}"," \t| ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})})," ","{",(0,n.jsx)("strong",{children:"and"})," ",(0,n.jsx)("strong",{children:"then"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})}),"}",(0,n.jsx)("br",{}),"   | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})})," ","{",(0,n.jsx)("strong",{children:"or"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})}),"}"," \t| ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})})," ","{",(0,n.jsx)("strong",{children:"or"})," ",(0,n.jsx)("strong",{children:"else"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})}),"}",(0,n.jsx)("br",{}),"   | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})})," ","{",(0,n.jsx)("strong",{children:"xor"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0135",children:"relation"})}),"}",(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(o.A,{children:"2.1/3"}),"\n",(0,n.jsx)(h.A,{items:["AI05-0158-1"]}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsxs)("code",{children:["choice","_","expression"]}),(0,n.jsx)("a",{id:"S0133"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("br",{}),"     ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})})," ","{",(0,n.jsx)("strong",{children:"and"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})}),"}",(0,n.jsx)("br",{}),"   | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})})," ","{",(0,n.jsx)("strong",{children:"or"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})}),"}",(0,n.jsx)("br",{}),"   | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})})," ","{",(0,n.jsx)("strong",{children:"xor"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})}),"}",(0,n.jsx)("br",{}),"   | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})})," ","{",(0,n.jsx)("strong",{children:"and then"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})}),"}",(0,n.jsx)("br",{}),"   | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})})," ","{",(0,n.jsx)("strong",{children:"or else"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0134",children:"choice_relation"})}),"}",(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(o.A,{children:"2.2/3"}),"\n",(0,n.jsx)(h.A,{items:["AI05-0158-1"]}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsxs)("code",{children:["choice","_","relation"]}),(0,n.jsx)("a",{id:"S0134"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("br",{}),"     ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})})," [",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0143",children:"relational_operator"})})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})}),"]",(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(o.A,{children:"3/4"}),"\n",(0,n.jsx)(h.A,{items:["AI05-0158-1","AI12-0022-1","AI12-0039-1"]}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("code",{children:"relation"}),(0,n.jsx)("a",{id:"S0135"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("br",{}),"     ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})})," [",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0143",children:"relational_operator"})})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})}),"]",(0,n.jsx)("br",{}),"   | ",(0,n.jsxs)("em",{children:["tested","_"]}),(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})})," [",(0,n.jsx)("strong",{children:"not"}),"] ",(0,n.jsx)("strong",{children:"in"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0136",children:"membership_choice_list"})}),(0,n.jsx)("br",{}),"   | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})}),(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(o.A,{children:"3.1/5"}),"\n",(0,n.jsx)(h.A,{items:["AI05-0158-1","AI12-0212-1"]}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsxs)("code",{children:["membership","_","choice","_","list"]}),(0,n.jsx)("a",{id:"S0136"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0137",children:"membership_choice"})})," ","{","'|' ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0137",children:"membership_choice"})}),"}",(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(o.A,{children:"3.2/4"}),"\n",(0,n.jsx)(h.A,{items:["AI05-0158-1","AI12-0039-1"]}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsxs)("code",{children:["membership","_","choice"]}),(0,n.jsx)("a",{id:"S0137"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsxs)("em",{children:["choice","_"]}),(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})})," | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0037",children:"range"})})," | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0028",children:"subtype_mark"})}),(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(o.A,{children:"4"}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsxs)("code",{children:["simple","_","expression"]}),(0,n.jsx)("a",{id:"S0138"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("br",{}),"    [",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0145",children:"unary_adding_operator"})}),"] ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0139",children:"term"})})," ","{",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0144",children:"binary_adding_operator"})})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0139",children:"term"})}),"}",(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(o.A,{children:"5"}),"\n",(0,n.jsxs)(c.A,{children:[(0,n.jsx)("code",{children:"term"}),(0,n.jsx)("a",{id:"S0139"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0140",children:"factor"})}),"{",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0146",children:"multiplying_operator"})}),(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0140",children:"factor"})}),"}",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"6"}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("code",{children:"factor"}),(0,n.jsx)("a",{id:"S0140"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})})," [","*","*"," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})}),"] | ",(0,n.jsx)("strong",{children:"abs"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})})," | ",(0,n.jsx)("strong",{children:"not"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})}),(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(o.A,{children:"7/5"}),"\n",(0,n.jsx)(h.A,{items:["AI05-0003-1","AI05-0147-1","AI05-0176-1","AI12-0236-1"]}),"\n",(0,n.jsx)(c.A,{children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("code",{children:"primary"}),(0,n.jsx)("a",{id:"S0141"}),(0,n.jsx)("code",{children:" ::= "}),(0,n.jsx)("br",{}),"    ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-2/AA-2.4#S0006",children:"numeric_literal"})})," | ",(0,n.jsx)("strong",{children:"null"})," | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-2/AA-2.6#S0016",children:"string_literal"})})," | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0106",children:"aggregate"})}),(0,n.jsx)("br",{}),"  | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," | ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.8#S0164",children:"allocator"})})," | (",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),")",(0,n.jsx)("br",{}),"  | (",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0148",children:"conditional_expression"})}),") | (",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0153",children:"quantified_expression"})}),")",(0,n.jsx)("br",{}),"  | (",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0156",children:"declare_expression"})}),")",(0,n.jsx)("br",{})]})}),"\n",(0,n.jsx)(s.h4,{id:"name-resolution-rules",children:"Name Resolution Rules"}),"\n",(0,n.jsx)(o.A,{children:"8"}),"\n",(0,n.jsxs)("p",{children:["A ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," used as a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})})," shall resolve to denote an object or a value. ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"8.a"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"discussion",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{}),"This replaces RM83-4.4(3). We don't need to mention named numbers explicitly, because the name of a named number denotes a value. We don't need to mention attributes explicitly, because attributes now denote (rather than yield) values in general. Also, the new wording allows attributes that denote objects, which should always have been allowed (in case the implementation chose to have such a thing). ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"8.b"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{}),"It might seem odd that this is an overload resolution rule, but it is relevant during overload resolution. For example, it helps ensure that a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})})," that consists of only the identifier of a parameterless function is interpreted as a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.4#S0218",children:"function_call"})})," rather than directly as a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0092",children:"direct_name"})}),". ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(s.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,n.jsx)(o.A,{children:"9"}),"\n",(0,n.jsxs)("p",{children:["Each expression has a type; it specifies the computation or retrieval of a value of that type.",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"9.1/5"}),"\n",(0,n.jsx)(h.A,{items:["AI12-0317-1"]}),"\n",(0,n.jsxs)("p",{children:["A ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})})," that is an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," surrounded by ( and ) is known as a ",(0,n.jsx)("em",{children:"parenthesized expression"}),".",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"9.2/5"}),"\n",(0,n.jsx)(h.A,{items:["AI12-0317-1"]}),"\n",(0,n.jsxs)("p",{children:["Every ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," or ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," consists of one or more ",(0,n.jsx)("em",{children:"operative constituent"})," ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),"s or ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),"s, only one of which is evaluated as part of evaluating the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," or ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," (the ",(0,n.jsx)("em",{children:"evaluated operative constituent"}),"). The operative constituents are determined as follows, according to the form of the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," (or ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),"):",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"9.3/5"}),"\n",(0,n.jsxs)("ul",{children:[(0,n.jsxs)("li",{children:["if the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," is a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0148",children:"conditional_expression"})}),", the operative constituents of its ",(0,n.jsxs)("em",{children:["dependent","_"]}),(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),"s;",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"9.4/5"}),(0,n.jsxs)("li",{children:["if the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," (or ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),") is a parenthesized expression, a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.7#S0163",children:"qualified_expression"})}),", or a view conversion, the operative constituent(s) of its operand;",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"9.5/5"}),(0,n.jsxs)("li",{children:["if the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," is a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0156",children:"declare_expression"})}),", the operative constituent(s) of its ",(0,n.jsxs)("em",{children:["body","_"]}),(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),";",(0,n.jsx)("br",{})]}),(0,n.jsx)(o.A,{children:"9.6/5"}),(0,n.jsxs)("li",{children:["otherwise, the ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," (or ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),") itself. ",(0,n.jsx)("br",{})]})]}),"\n",(0,n.jsx)(o.A,{children:"9.7/5"}),"\n",(0,n.jsx)(h.A,{items:["AI12-0317-1"]}),"\n",(0,n.jsxs)("p",{children:["In certain contexts, we specify that an operative constituent shall (or shall not) be ",(0,n.jsx)("em",{children:"newly constructed"}),". This means the operative constituent shall (or shall not) be an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#S0106",children:"aggregate"})})," or a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.4#S0218",children:"function_call"})}),"; in either case, a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0309",children:"raise_expression"})})," is permitted.",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"9.a/5"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{children:"To be honest: "}),"If an ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0149",children:"if_expression"})})," does not have an ",(0,n.jsx)("strong",{children:"else"}),' clause, "True" is an operative constituent of the ',(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})})," and it can be the evaluated operative constituent. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(s.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,n.jsx)(o.A,{children:"10"}),"\n",(0,n.jsxs)("p",{children:["The value of a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})})," that is a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," denoting an object is the value of the object.",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"10.1/5"}),"\n",(0,n.jsx)(h.A,{items:["AI12-0227-1"]}),"\n",(0,n.jsxs)("p",{children:["An expression of a numeric universal type is evaluated as if it has type ",(0,n.jsxs)("em",{children:["root","_","integer"]})," (for ",(0,n.jsxs)("em",{children:["universal","_","integer"]}),") or ",(0,n.jsxs)("em",{children:["root","_","real"]})," (otherwise) unless the context identifies a specific type (in which case that type is used).",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"10.a/5"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{}),"This has no effect for a static expression; its value may be arbitrarily small or large since no specific type is expected for any expression for which this rule specifies one of the root types. The only effect of this rule is to allow Constraint","_","Error to be raised if the value is outside of the base range of ",(0,n.jsxs)("em",{children:["root","_","integer"]})," or ",(0,n.jsxs)("em",{children:["root","_","real"]})," when the expression is not static. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"10.b/5"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{}),"This rule means that implementations don't have to support unlimited range math at run time for universal expressions. Note that universal expressions for which the context doesn't specify a specific type are quite rare; attribute prefixes and results are the only known cases. (For operators, ",(0,n.jsx)("a",{href:"/docs/arm/AA-8/AA-8.6",children:"8.6"})," already specifies that the operator of a root type be used, which provides a specific type.) ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(s.h4,{id:"implementation-permissions",children:"Implementation Permissions"}),"\n",(0,n.jsx)(o.A,{children:"11"}),"\n",(0,n.jsxs)("p",{children:["For the evaluation of a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})})," that is a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," denoting an object of an unconstrained numeric subtype, if the value of the object is outside the base range of its type, the implementation may either raise Constraint","_","Error or return the value of the object. ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"11.a/5"}),(0,n.jsx)(h.A,{items:["AI05-0299-1","AI12-0449-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"ramification",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{}),"This means that if extra-range intermediates are used to hold the value of an object of an unconstrained numeric subtype, a Constraint","_","Error can be raised on a read of the object, rather than only on an assignment to it. Similarly, it means that computing the value of an object of such a subtype can be deferred until the first read of the object (presuming no side effects other than failing an Overflow","_","Check are possible). This permission is over and above that provided by ",(0,n.jsx)("a",{href:"/docs/arm/AA-11/AA-11.6",children:"11.6"}),", since this allows the Constraint","_","Error to move to a different handler. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"11.b"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"reason",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{}),"This permission is intended to allow extra-range registers to be used efficiently to hold parameters and local variables, even if they might need to be transferred into smaller registers for performing certain predefined operations. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"11.c"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"discussion",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{}),"There is no need to mention other kinds of ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})}),"s, since any Constraint","_","Error to be raised can be \u201ccharged\u201d to the evaluation of the particular kind of ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})}),". ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(s.h4,{id:"examples",children:"Examples"}),"\n",(0,n.jsx)(o.A,{children:"12"}),"\n",(0,n.jsxs)("p",{children:[(0,n.jsx)("em",{children:"Examples of primaries:"})," ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"13"}),"\n",(0,n.jsx)(c.A,{language:"ada",children:(0,n.jsxs)(s.p,{children:["4.0                -- real literal","\n","Pi                 -- named number","\n","(1 .. 10 =",">"," 0)     -- array aggregate","\n","Sum                -- variable","\n","Integer'Last       -- attribute","\n","Sine(X)            -- function call","\n","Color'(Blue)       -- qualified expression","\n","Real(M","*","N)          -- conversion","\n","(Line","_","Count + 10)  -- parenthesized expression ","\n"]})}),"\n",(0,n.jsx)(o.A,{children:"14"}),"\n",(0,n.jsxs)("p",{children:[(0,n.jsx)("em",{children:"Examples of expressions:"})," ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsx)(o.A,{children:"15/2"}),"\n",(0,n.jsx)(h.A,{items:["AI95-00433-01"]}),"\n",(0,n.jsx)(c.A,{language:"ada",children:(0,n.jsxs)(s.p,{children:["Volume                      -- primary","\n","not Destroyed               -- factor","\n","2","*","Line","_","Count                -- term","\n","-4.0                        -- simple expression","\n","-4.0 + A                    -- simple expression","\n","B","*","*","2 - 4.0","*","A","*","C              -- simple expression","\n","R","*","Sin(\u03b8)","*","Cos(\u03c6)             -- simple expression","\n",'Password(1 .. 3) = "Bwv"    -- relation',"\n","Count in Small","_","Int          -- relation","\n","Count not in Small","_","Int      -- relation","\n","Index = 0 or Item","_","Hit       -- expression","\n","(Cold and Sunny) or Warm    -- expression (parentheses are required)","\n","A","*","*","(B","*","*","C)                   -- expression (parentheses are required)","\n"]})}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(s.h4,{id:"extensions-to-ada-83",children:"Extensions to Ada 83"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.a"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:["In Ada 83, ",(0,n.jsx)("strong",{children:"out"})," parameters and their nondiscriminant subcomponents are not allowed as ",(0,n.jsx)("code",{children:"primaries"}),". These restrictions are eliminated in Ada 95.",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.b"}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:["In various contexts throughout the language where Ada 83 syntax rules had ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})}),", the corresponding Ada 95 syntax rule has ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),' instead. This reflects the inclusion of modular integer types, which makes the logical operators "',(0,n.jsx)("strong",{children:"and"}),'", "',(0,n.jsx)("strong",{children:"or"}),'", and "',(0,n.jsx)("strong",{children:"xor"}),'" more useful in expressions of an integer type. Requiring parentheses to use these operators in such contexts seemed unnecessary and potentially confusing. Note that the bounds of a ',(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-3/AA-3.5#S0037",children:"range"})})," still have to be specified by ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0138",children:"simple_expression"})}),"s, since otherwise ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0132",children:"expression"})}),'s involving membership tests might be ambiguous. Essentially, the operation ".." is of higher precedence than the logical operators, and hence uses of logical operators still have to be parenthesized when used in a bound of a range. ',(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(s.h4,{id:"wording-changes-from-ada-2005",children:"Wording Changes from Ada 2005"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.c/3"}),(0,n.jsx)(h.A,{items:["AI05-0003-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:["Moved ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.7#S0163",children:"qualified_expression"})})," from ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})})," to ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," (see ",(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1",children:"4.1"}),"). This allows the use of ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.7#S0163",children:"qualified_expression"})}),"s in more places.",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.d/3"}),(0,n.jsx)(h.A,{items:["AI05-0147-1","AI05-0176-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:["Added ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0148",children:"conditional_expression"})})," and ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0153",children:"quantified_expression"})})," to ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})}),".",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.e/3"}),(0,n.jsx)(h.A,{items:["AI05-0158-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:["Expanded membership test syntax (see ",(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#Subclause_4.5.2",children:"4.5.2"}),"). ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(s.h4,{id:"inconsistencies-with-ada-2012",children:"Inconsistencies With Ada 2012"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.f/4"}),(0,n.jsx)(h.A,{items:["AI12-0039-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{children:"Corrigendum:"}),' Revised membership syntax to eliminate ambiguities. In some cases, previously ambiguous membership expressions will now have an unambiguous meaning. If an Ada 2012 implementation chose the "wrong" meaning, the expression could silently change meaning. Virtually all such expressions will become illegal because of type mismatches (and thus be incompatible, not inconsistent). However, if the choices are all of a Boolean type, resolution might succeed. For instance, A ',(0,n.jsx)("strong",{children:"in"})," B | C ",(0,n.jsx)("strong",{children:"and"})," D now always means (A ",(0,n.jsx)("strong",{children:"in"})," B | C) ",(0,n.jsx)("strong",{children:"and"})," D, but the original Ada 2012 syntax would have allowed it to mean A ",(0,n.jsx)("strong",{children:"in"})," B | (C ",(0,n.jsx)("strong",{children:"and"})," D). If a compiler allowed the expression and interpreted it as the latter, the meaning of the expression would silently change. We expect this to be extremely rare as membership operations on Boolean types are unlikely (and this can happen only in code written for Ada 2012). ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(s.h4,{id:"incompatibilities-with-ada-2012",children:"Incompatibilities With Ada 2012"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.g/4"}),(0,n.jsx)(h.A,{items:["AI12-0039-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{children:"Corrigendum:"})," The revised membership syntax will require parentheses in ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0136",children:"membership_choice_list"})}),"s in some cases where the Ada 2012 grammar did not require them. For instance, A ",(0,n.jsx)("strong",{children:"in"})," B ",(0,n.jsx)("strong",{children:"in"})," C | D is now illegal. However, such expressions can be interpreted in multiple ways (either A ",(0,n.jsx)("strong",{children:"in"})," (B ",(0,n.jsx)("strong",{children:"in"})," C) | D or A ",(0,n.jsx)("strong",{children:"in"})," (B ",(0,n.jsx)("strong",{children:"in"})," C | D) for this example), so using such expressions is likely to be dangerous (another compiler might interpret the expression differently). In addition, all such expressions occur only in Ada 2012 syntax; so they should be rare. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(d.A,{children:(0,n.jsx)(s.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.h/5"}),(0,n.jsx)(h.A,{items:["AI12-0227-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"correction",children:(0,n.jsxs)(s.p,{children:[(0,n.jsx)("strong",{})," Added wording so that universal expressions evaluated at run time can raise Constraint","_","Error if the value is outside of the range of ",(0,n.jsxs)("em",{children:["root","_","integer"]})," or ",(0,n.jsxs)("em",{children:["root","_","real"]}),". We don't document this as an inconsistency because the rule requires no implementation to change (as Constraint","_","Error is not required); it just allows implementations that already raise Constraint","_","Error (which is all of them surveyed) to be considered correct.",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.i/5"}),(0,n.jsx)(h.A,{items:["AI12-0236-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:["Added ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0156",children:"declare_expression"})})," to ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.4#S0141",children:"primary"})}),". It shares the rules about parentheses with ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.5#S0148",children:"conditional_expression"})}),"s.",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsxs)(d.A,{children:[(0,n.jsx)(o.A,{children:"15.j/5"}),(0,n.jsx)(h.A,{items:["AI12-0317-1"]}),(0,n.jsx)(a.A,{type:"aarm",aarm:"note",children:(0,n.jsxs)(s.p,{children:["Added the definitions of \u201coperative constituent\u201d and \u201cnewly constructed\u201d to centralize definitions that are needed for various rules and definitions across the Reference Manual. In particular, ",(0,n.jsx)("em",{children:"operative constituent"})," is often used when we want the semantics or legality to be unchanged by the presence of parens, qualification, or view conversions. Examples are found in ",(0,n.jsx)("a",{href:"/docs/arm/AA-4/AA-4.3#Subclause_4.3.2",children:"4.3.2"}),", ",(0,n.jsx)("a",{href:"/docs/arm/AA-6/AA-6.2",children:"6.2"}),", and ",(0,n.jsx)("a",{href:"/docs/arm/AA-7/AA-7.5",children:"7.5"}),". ",(0,n.jsx)("br",{})]})})]})]})}function p(e={}){const{wrapper:s}={...(0,i.R)(),...e.components};return s?(0,n.jsx)(s,{...e,children:(0,n.jsx)(m,{...e})}):m(e)}}}]);