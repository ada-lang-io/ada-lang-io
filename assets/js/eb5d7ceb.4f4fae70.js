"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3510],{74681:(e,a,i)=>{i.r(a),i.d(a,{assets:()=>x,contentTitle:()=>A,default:()=>m,frontMatter:()=>h,metadata:()=>n,toc:()=>j});const n=JSON.parse('{"id":"arm/AA-7/AA-7.1","title":"7.1 Package Specifications and Declarations","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-7/AA-7.1.mdx","sourceDirName":"arm/AA-7","slug":"/arm/AA-7/AA-7.1","permalink":"/docs/arm/AA-7/AA-7.1","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":60,"frontMatter":{"sidebar_position":60},"sidebar":"referenceManualSidebar","previous":{"title":"7 Packages","permalink":"/docs/arm/AA-7/"},"next":{"title":"7.2 Package Bodies","permalink":"/docs/arm/AA-7/AA-7.2"}}');var r=i(74848),s=i(28453),c=i(13842),d=i(91435),t=i(21432),o=i(79162),l=i(34421);const h={sidebar_position:60},A="7.1 Package Specifications and Declarations",x={},j=[{value:"Syntax",id:"syntax",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Incompatibilities With Ada 83",id:"incompatibilities-with-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4}];function p(e){const a={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,s.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(a.header,{children:(0,r.jsx)(a.h1,{id:"71-package-specifications-and-declarations",children:"7.1 Package Specifications and Declarations"})}),"\n",(0,r.jsx)(a.admonition,{type:"danger",children:(0,r.jsxs)(a.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,r.jsx)(a.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,r.jsx)(o.A,{children:"1"}),"\n",(0,r.jsxs)("p",{children:["[A package is generally provided in two parts: a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})})," and a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.2#S0231",children:"package_body"})}),". Every package has a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),", but not all packages have a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.2#S0231",children:"package_body"})}),".] ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(a.h4,{id:"syntax",children:"Syntax"}),"\n",(0,r.jsx)(o.A,{children:"2"}),"\n",(0,r.jsx)(t.A,{children:(0,r.jsxs)(a.p,{children:[(0,r.jsxs)("code",{children:["package","_","declaration"]}),(0,r.jsx)("a",{id:"S0229"}),(0,r.jsx)("code",{children:" ::= "}),(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),";",(0,r.jsx)("br",{})]})}),"\n",(0,r.jsx)(o.A,{children:"3/3"}),"\n",(0,r.jsx)(l.A,{items:["AI05-0183-1"]}),"\n",(0,r.jsx)(t.A,{children:(0,r.jsxs)(a.p,{children:[(0,r.jsxs)("code",{children:["package","_","specification"]}),(0,r.jsx)("a",{id:"S0230"}),(0,r.jsx)("code",{children:" ::= "}),(0,r.jsx)("br",{}),"    ",(0,r.jsx)("strong",{children:"package"})," ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0201",children:"defining_program_unit_name"})}),(0,r.jsx)("br",{}),"        [",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})}),"] ",(0,r.jsx)("strong",{children:"is"}),(0,r.jsx)("br",{}),"      ","{",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})}),"}",(0,r.jsx)("br",{}),"   [",(0,r.jsx)("strong",{children:"private"}),(0,r.jsx)("br",{}),"      ","{",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})}),"}","]",(0,r.jsx)("br",{}),"    ",(0,r.jsx)("strong",{children:"end"})," [[",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0291",children:"parent_unit_name"})}),".]",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-2/AA-2.3#S0002",children:"identifier"})}),"]",(0,r.jsx)("br",{})]})}),"\n",(0,r.jsx)(o.A,{children:"4"}),"\n",(0,r.jsxs)("p",{class:"Indented2",children:["If an ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-2/AA-2.3#S0002",children:"identifier"})})," or ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0291",children:"parent_unit_name"})}),".",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-2/AA-2.3#S0002",children:"identifier"})})," appears at the end of a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),", then this sequence of lexical elements shall repeat the ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0201",children:"defining_program_unit_name"})}),". ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(a.h4,{id:"legality-rules",children:"Legality Rules"}),"\n",(0,r.jsx)(o.A,{children:"5/2"}),"\n",(0,r.jsx)(l.A,{items:["AI95-00434-01"]}),"\n",(0,r.jsxs)("p",{children:["A ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0229",children:"package_declaration"})})," or ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0312",children:"generic_package_declaration"})})," requires a completion [(a body)] if it contains any ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})})," that requires a completion, but whose completion is not in its ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),". ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"5.a/3"}),(0,r.jsx)(l.A,{items:["AI05-0229-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:[(0,r.jsx)("strong",{children:"To be honest: "}),"If an implementation supports it, the body of a package or generic package may be imported (using aspect Import, see ",(0,r.jsx)("a",{href:"/docs/arm/AA-B/AA-B.1",children:"B.1"}),"), in which case no explicit body is allowed. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(a.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,r.jsx)(o.A,{children:"6/2"}),"\n",(0,r.jsx)(l.A,{items:["AI95-00420-01","AI95-00434-01"]}),"\n",(0,r.jsxs)("p",{children:["The first list of ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})}),"s of a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})})," of a package other than a generic formal package is called the ",(0,r.jsx)("em",{children:"visible part"})," of the package. [ The optional list of ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})}),"s after the reserved word ",(0,r.jsx)("strong",{children:"private"})," (of any ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),") is called the ",(0,r.jsx)("em",{children:"private part"})," of the package. If the reserved word ",(0,r.jsx)("strong",{children:"private"})," does not appear, the package has an implicit empty private part.] Each list of ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})}),"s of a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})})," forms a ",(0,r.jsx)("em",{children:"declaration list"})," of the package.",(0,r.jsx)("br",{})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"6.a"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"ramification",children:(0,r.jsxs)(a.p,{children:[(0,r.jsx)("strong",{}),"This definition of visible part does not apply to generic formal packages \u2014 ",(0,r.jsx)("a",{href:"/docs/arm/AA-12/AA-12.7",children:"12.7"})," defines the visible part of a generic formal package.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"6.b"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:["The implicit empty private part is important because certain implicit declarations occur there if the package is a child package, and it defines types in its visible part that are derived from, or contain as components, private types declared within the parent package. These implicit declarations are visible in children of the child package. See ",(0,r.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#Subclause_10.1.1",children:"10.1.1"}),". ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(o.A,{children:"7"}),"\n",(0,r.jsxs)("p",{children:["[An entity declared in the private part of a package is visible only within the declarative region of the package itself (including any child units \u2014 see ",(0,r.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#Subclause_10.1.1",children:"10.1.1"}),"). In contrast, expanded names denoting entities declared in the visible part can be used even outside the package; furthermore, direct visibility of such entities can be achieved by means of ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-8/AA-8.4#S0235",children:"use_clause"})}),"s (see ",(0,r.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#Subclause_4.1.3",children:"4.1.3"})," and ",(0,r.jsx)("a",{href:"/docs/arm/AA-8/AA-8.4",children:"8.4"}),").] ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(a.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,r.jsx)(o.A,{children:"8"}),"\n",(0,r.jsxs)("p",{children:["The elaboration of a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0229",children:"package_declaration"})})," consists of the elaboration of its ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-3/AA-3.11#S0088",children:"basic_declarative_item"})}),"s in the given order. ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"9"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:["NOTE 1   The visible part of a package contains all the information that another program unit is able to know about the package.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"10"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:["NOTE 2   If a declaration occurs immediately within the specification of a package, and the declaration has a corresponding completion that is a body, then that body has to occur immediately within the body of the package. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"10.a"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"proof",children:(0,r.jsxs)(a.p,{children:[(0,r.jsx)("strong",{}),"This follows from the fact that the declaration and completion are required to occur immediately within the same declarative region, and the fact that ",(0,r.jsx)("code",{children:"bodies"})," are disallowed (by the Syntax Rules) in ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),"s. This does not apply to instances of generic units, whose bodies can occur in ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),"s. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(a.h4,{id:"examples",children:"Examples"}),"\n",(0,r.jsx)(o.A,{children:"11"}),"\n",(0,r.jsxs)("p",{children:[(0,r.jsx)("em",{children:"Example of a package declaration:"})," ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(o.A,{children:"12"}),"\n",(0,r.jsx)(t.A,{language:"ada",children:(0,r.jsxs)(a.p,{children:["package Rational","_","Numbers is","\n","\n",(0,r.jsx)(o.A,{children:"13"}),"\ntype Rational is","\n","      record","\n","         Numerator   : Integer;","\n","         Denominator : Positive;","\n","      end record;","\n","\n",(0,r.jsx)(o.A,{children:"14"}),'\nfunction "="(X,Y : Rational) return Boolean;',"\n","\n",(0,r.jsx)(o.A,{children:"15"}),'\nfunction "/"  (X,Y : Integer)  return Rational;  --  to construct a rational number',"\n","\n",(0,r.jsx)(o.A,{children:"16"}),'\nfunction "+"  (X,Y : Rational) return Rational;',"\n",'   function "-"  (X,Y : Rational) return Rational;',"\n",'   function "',"*",'"  (X,Y : Rational) return Rational;',"\n",'   function "/"  (X,Y : Rational) return Rational;',"\n","end Rational","_","Numbers;","\n"]})}),"\n",(0,r.jsx)(o.A,{children:"17"}),"\n",(0,r.jsxs)("p",{children:["There are also many examples of package declarations in the predefined language environment (see ",(0,r.jsx)("a",{href:"/docs/arm/AA-A/",children:"Annex A"}),"). ",(0,r.jsx)("br",{})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(a.h4,{id:"incompatibilities-with-ada-83",children:"Incompatibilities With Ada 83"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"17.a/5"}),(0,r.jsx)(l.A,{items:["AI12-0417-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:["In Ada 83, a library package is allowed to have a body even if it doesn't need one. In Ada 95, a library package body is either required or forbidden \u2014 never optional. The workaround is to add aspect Elaborate","_","Body, or something else requiring a body, to each library package that has a body that isn't otherwise required. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(a.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"17.b/3"}),(0,r.jsx)(l.A,{items:["AI05-0299-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:["We have moved the syntax into this subclause and the next subclause from RM83-7.1, \u201cPackage Structure\u201d, which we have removed.",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"17.c"}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:["RM83 was unclear on the rules about when a package requires a body. For example, RM83-7.1(4) and RM83-7.1(8) clearly forgot about the case of an incomplete type declared in a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0229",children:"package_declaration"})})," but completed in the body. In addition, RM83 forgot to make this rule apply to a generic package. We have corrected these rules. Finally, since we now allow a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Import for any explicit declaration, the completion rules need to take this into account as well. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(a.h4,{id:"wording-changes-from-ada-95",children:"Wording Changes from Ada 95"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"17.d/2"}),(0,r.jsx)(l.A,{items:["AI95-00420-01"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:["Defined \u201cdeclaration list\u201d to avoid ambiguity in other rules as to whether packages are included. ",(0,r.jsx)("br",{})]})})]}),"\n",(0,r.jsx)(d.A,{children:(0,r.jsx)(a.h4,{id:"extensions-to-ada-2005",children:"Extensions to Ada 2005"})}),"\n",(0,r.jsxs)(d.A,{children:[(0,r.jsx)(o.A,{children:"17.e/3"}),(0,r.jsx)(l.A,{items:["AI05-0183-1"]}),(0,r.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,r.jsxs)(a.p,{children:["An optional ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})})," can be used in a ",(0,r.jsx)("code",{children:(0,r.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),". This is described in ",(0,r.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#Subclause_13.1.1",children:"13.1.1"}),". ",(0,r.jsx)("br",{})]})})]})]})}function m(e={}){const{wrapper:a}={...(0,s.R)(),...e.components};return a?(0,r.jsx)(a,{...e,children:(0,r.jsx)(p,{...e})}):p(e)}}}]);