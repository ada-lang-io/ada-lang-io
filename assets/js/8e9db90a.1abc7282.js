"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1947],{22731:(e,r,n)=>{n.r(r),n.d(r,{assets:()=>x,contentTitle:()=>A,default:()=>f,frontMatter:()=>h,metadata:()=>a,toc:()=>j});const a=JSON.parse('{"id":"arm/AA-12/AA-12.1","title":"12.1 Generic Declarations","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-12/AA-12.1.mdx","sourceDirName":"arm/AA-12","slug":"/arm/AA-12/AA-12.1","permalink":"/docs/arm/AA-12/AA-12.1","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":96,"frontMatter":{"sidebar_position":96},"sidebar":"referenceManualSidebar","previous":{"title":"12 Generic Units","permalink":"/docs/arm/AA-12/"},"next":{"title":"12.2 Generic Bodies","permalink":"/docs/arm/AA-12/AA-12.2"}}');var s=n(74848),i=n(28453),c=n(13842),d=n(91435),o=n(21432),l=n(79162),t=n(34421);const h={sidebar_position:96},A="12.1 Generic Declarations",x={},j=[{value:"Syntax",id:"syntax",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function m(e){const r={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,i.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(r.header,{children:(0,s.jsx)(r.h1,{id:"121-generic-declarations",children:"12.1 Generic Declarations"})}),"\n",(0,s.jsx)(r.admonition,{type:"danger",children:(0,s.jsxs)(r.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,s.jsx)(r.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,s.jsx)(l.A,{children:"1"}),"\n",(0,s.jsxs)("p",{children:["[A ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0310",children:"generic_declaration"})})," declares a generic unit, which is either a generic subprogram or a generic package. A ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0310",children:"generic_declaration"})})," includes a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})})," declaring any generic formal parameters. A generic formal parameter can be an object; alternatively (unlike a parameter of a subprogram), it can be a type, a subprogram, or a package.] ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(r.h4,{id:"syntax",children:"Syntax"}),"\n",(0,s.jsx)(l.A,{children:"2"}),"\n",(0,s.jsx)(o.A,{children:(0,s.jsxs)(r.p,{children:[(0,s.jsxs)("code",{children:["generic","_","declaration"]}),(0,s.jsx)("a",{id:"S0310"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("br",{}),"    ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0311",children:"generic_subprogram_declaration"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0312",children:"generic_package_declaration"})}),(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(l.A,{children:"3/3"}),"\n",(0,s.jsx)(t.A,{items:["AI05-0183-1"]}),"\n",(0,s.jsx)(o.A,{children:(0,s.jsxs)(r.p,{children:[(0,s.jsxs)("code",{children:["generic","_","subprogram","_","declaration"]}),(0,s.jsx)("a",{id:"S0311"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("br",{}),"    ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})})," ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0196",children:"subprogram_specification"})}),(0,s.jsx)("br",{}),"       [",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})}),"];",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(l.A,{children:"4"}),"\n",(0,s.jsx)(o.A,{children:(0,s.jsxs)(r.p,{children:[(0,s.jsxs)("code",{children:["generic","_","package","_","declaration"]}),(0,s.jsx)("a",{id:"S0312"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("br",{}),"    ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})}),"  ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})}),";",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"4.a/3"}),(0,s.jsx)(t.A,{items:["AI05-0183-1"]}),(0,s.jsx)(c.A,{type:"aarm",aarm:"ramification",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"No syntax change is needed here to allow an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})}),"; a generic package can have an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})})," because a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-7/AA-7.1#S0230",children:"package_specification"})})," allows an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})}),". ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(l.A,{children:"5"}),"\n",(0,s.jsx)(o.A,{children:(0,s.jsxs)(r.p,{children:[(0,s.jsxs)("code",{children:["generic","_","formal","_","part"]}),(0,s.jsx)("a",{id:"S0313"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("br",{}),"    ",(0,s.jsx)("strong",{children:"generic"})," ","{",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0314",children:"generic_formal_parameter_declaration"})})," | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-8/AA-8.4#S0235",children:"use_clause"})}),"}",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(l.A,{children:"6"}),"\n",(0,s.jsx)(o.A,{children:(0,s.jsxs)(r.p,{children:[(0,s.jsxs)("code",{children:["generic","_","formal","_","parameter","_","declaration"]}),(0,s.jsx)("a",{id:"S0314"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("br",{}),"    ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.4#S0319",children:"formal_object_declaration"})}),(0,s.jsx)("br",{}),"  | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.5#S0320",children:"formal_type_declaration"})}),(0,s.jsx)("br",{}),"  | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.6#S0335",children:"formal_subprogram_declaration"})}),(0,s.jsx)("br",{}),"  | ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.7#S0340",children:"formal_package_declaration"})}),(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(l.A,{children:"7"}),"\n",(0,s.jsxs)("p",{class:"Indented2",children:["The only form of ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})})," allowed within a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})})," is a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0028",children:"subtype_mark"})})," [(that is, the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})})," shall not include an explicit ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0029",children:"constraint"})}),")]. The defining name of a generic subprogram shall be an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-2/AA-2.3#S0002",children:"identifier"})})," [(not an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0202",children:"operator_symbol"})}),")]. ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"7.a"}),(0,s.jsx)(c.A,{type:"aarm",aarm:"reason",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"The reason for forbidding ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0029",children:"constraint"})}),"s in ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.2#S0027",children:"subtype_indication"})}),"s is that it simplifies the elaboration of ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0310",children:"generic_declaration"})}),"s (since there is nothing to evaluate), and that it simplifies the matching rules, and makes them more checkable at compile time. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(r.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,s.jsx)(l.A,{children:"8/2"}),"\n",(0,s.jsx)(t.A,{items:["AI95-00434-01"]}),"\n",(0,s.jsxs)("p",{children:["A ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0310",children:"generic_declaration"})})," declares a generic unit \u2014 a generic package, generic procedure, or generic function, as appropriate.",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(l.A,{children:"9"}),"\n",(0,s.jsxs)("p",{children:["An entity is a ",(0,s.jsx)("em",{children:"generic formal"})," entity if it is declared by a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0314",children:"generic_formal_parameter_declaration"})}),". \u201cGeneric formal\u201d, or simply \u201cformal\u201d, is used as a prefix in referring to objects, subtypes (and types), functions, procedures and packages, that are generic formal entities, as well as to their respective declarations. [Examples: \u201cgeneric formal procedure\u201d or a \u201cformal integer type declaration\u201d.]",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(l.A,{children:"9.1/5"}),"\n",(0,s.jsx)(t.A,{items:["AI12-0371-1"]}),"\n",(0,s.jsxs)("p",{children:["The list of ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0314",children:"generic_formal_parameter_declaration"})}),"s of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})})," form a ",(0,s.jsx)("em",{children:"declaration list"})," of the generic unit.",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"9.a/5"}),(0,s.jsx)(c.A,{type:"aarm",aarm:"ramification",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"Aspect specifications (see ",(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#Subclause_13.1.1",children:"13.1.1"}),") given in a generic formal part can only use declarations given in the formal part, and not those in the visible part of the generic unit. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(r.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,s.jsx)(l.A,{children:"10"}),"\n",(0,s.jsxs)("p",{children:["The elaboration of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0310",children:"generic_declaration"})})," has no effect. ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"11"}),(0,s.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["NOTE 1   Outside a generic unit a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," that denotes the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0310",children:"generic_declaration"})})," denotes the generic unit. In contrast, within the declarative region of the generic unit, a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," that denotes the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0310",children:"generic_declaration"})})," denotes the current instance. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"11.a"}),(0,s.jsx)(c.A,{type:"aarm",aarm:"proof",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("strong",{}),"This is stated officially as part of the \u201ccurrent instance\u201d rule in ",(0,s.jsx)("a",{href:"/docs/arm/AA-8/AA-8.6",children:"8.6"}),", \u201c",(0,s.jsx)("a",{href:"/docs/arm/AA-8/AA-8.6",children:"The Context of Overload Resolution"}),"\u201d. See also ",(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.3",children:"12.3"}),", \u201c",(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.3",children:"Generic Instantiation"}),"\u201d. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"12"}),(0,s.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["NOTE 2   Within a generic ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})}),", the name of this program unit acts as the name of a subprogram. Hence this name can be overloaded, and it can appear in a recursive call of the current instance. For the same reason, this name cannot appear after the reserved word ",(0,s.jsx)("strong",{children:"new"})," in a (recursive) ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.3#S0315",children:"generic_instantiation"})}),".",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"13/5"}),(0,s.jsx)(t.A,{items:["AI12-0447-1"]}),(0,s.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["NOTE 3   A ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-3/AA-3.7#S0063",children:"default_expression"})})," or ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.6#S0339",children:"default_name"})})," appearing in a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})})," is not evaluated during elaboration of the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})}),"; instead, it is evaluated when used. However, the usual visibility rules apply to any ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," used in a default, with name resolution performed based on the location of the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," within the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})}),". ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(r.h4,{id:"examples",children:"Examples"}),"\n",(0,s.jsx)(l.A,{children:"14"}),"\n",(0,s.jsxs)("p",{children:[(0,s.jsx)("em",{children:"Examples of generic formal parts:"})," ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(l.A,{children:"15"}),"\n",(0,s.jsx)(o.A,{language:"ada",children:(0,s.jsxs)(r.p,{children:["generic     --  parameterless ","\n","\n",(0,s.jsx)(l.A,{children:"16"}),"\ngeneric","\n","   Size : Natural;  --  formal object ","\n","\n",(0,s.jsx)(l.A,{children:"17"}),"\ngeneric","\n","   Length : Integer := 200;          -- formal object with a default expression","\n","\n",(0,s.jsx)(l.A,{children:"18"}),"\nArea   : Integer := Length","*","Length; -- formal object with a default expression","\n","\n",(0,s.jsx)(l.A,{children:"19"}),"\ngeneric","\n","   type Item  is private;                       -- formal type","\n","   type Index is (","<",">",");                          -- formal type","\n","   type Row   is array(Index range ","<",">",") of Item; -- formal type","\n",'   with function "',"<",'"(X, Y : Item) return Boolean;    -- formal subprogram ',"\n"]})}),"\n",(0,s.jsx)(l.A,{children:"20"}),"\n",(0,s.jsxs)("p",{children:[(0,s.jsx)("em",{children:"Examples of generic declarations declaring generic subprograms Exchange and Squaring:"})," ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(l.A,{children:"21"}),"\n",(0,s.jsxs)(o.A,{language:"ada",children:[(0,s.jsxs)(r.p,{children:["generic","\n","   type Elem is private;","\n","procedure Exchange(U, V : in out Elem);","\n","\n",(0,s.jsx)(l.A,{children:"22/5"})]}),(0,s.jsx)(t.A,{items:["AI12-0178-1"]}),(0,s.jsxs)(r.p,{children:["generic","\n","   type Item (","<",">",") is private;","\n",'   with function "',"*",'"(U, V : Item) return Item is ',"<",">",";","\n","function Squaring(X : Item) return Item;","\n"]})]}),"\n",(0,s.jsx)(l.A,{children:"23"}),"\n",(0,s.jsxs)("p",{children:[(0,s.jsx)("em",{children:"Example of a generic declaration declaring a generic package:"})," ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(l.A,{children:"24"}),"\n",(0,s.jsx)(o.A,{language:"ada",children:(0,s.jsxs)(r.p,{children:["generic","\n","   type Item   is private;","\n","   type Vector is array (Positive range ","<",">",") of Item;","\n","   with function Sum(X, Y : Item) return Item;","\n","package On","_","Vectors is","\n","   function Sum  (A, B : Vector) return Vector;","\n","   function Sigma(A    : Vector) return Item;","\n","   Length","_","Error : exception;","\n","end On","_","Vectors;","\n"]})}),"\n",(0,s.jsx)(d.A,{children:(0,s.jsx)(r.h4,{id:"extensions-to-ada-83",children:"Extensions to Ada 83"})}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"24.a"}),(0,s.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["The syntax rule for ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0314",children:"generic_formal_parameter_declaration"})})," is modified to allow the reserved words ",(0,s.jsx)("strong",{children:"tagged"})," and ",(0,s.jsx)("strong",{children:"abstract"}),", to allow formal derived types, and to allow formal packages.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"24.b"}),(0,s.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:[(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-8/AA-8.4#S0235",children:"Use_clause"})}),"s are allowed in ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0313",children:"generic_formal_part"})}),"s. This is necessary in order to allow a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-8/AA-8.4#S0235",children:"use_clause"})})," within a formal part to provide direct visibility of declarations within a generic formal package. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(d.A,{children:(0,s.jsx)(r.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"24.c/3"}),(0,s.jsx)(t.A,{items:["AI05-0299-1"]}),(0,s.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["The syntax for ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0314",children:"generic_formal_parameter_declaration"})})," and ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.5#S0323",children:"formal_type_definition"})})," is split up into more named categories. The rules for these categories are moved to the appropriate subclauses. The names of the categories are changed to be more intuitive and uniform. For example, we changed ",(0,s.jsxs)("code",{children:["generic","_","parameter","_","declaration"]})," to ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0314",children:"generic_formal_parameter_declaration"})}),", because the thing it declares is a generic formal, not a generic. In the others, we abbreviate \u201cgeneric","_","formal\u201d to just \u201cformal\u201d. We can't do that for ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0314",children:"generic_formal_parameter_declaration"})}),", because of confusion with normal formal parameters of subprograms. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(d.A,{children:(0,s.jsx)(r.h4,{id:"extensions-to-ada-2005",children:"Extensions to Ada 2005"})}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"24.d/3"}),(0,s.jsx)(t.A,{items:["AI05-0183-1"]}),(0,s.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["An optional ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})})," can be used in a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0311",children:"generic_subprogram_declaration"})})," (as well as a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0312",children:"generic_package_declaration"})}),"). This is described in ",(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#Subclause_13.1.1",children:"13.1.1"}),". ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(d.A,{children:(0,s.jsx)(r.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,s.jsxs)(d.A,{children:[(0,s.jsx)(l.A,{children:"24.e/5"}),(0,s.jsx)(t.A,{items:["AI12-0371-1"]}),(0,s.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(r.p,{children:["Defined a formal part as a declaration list, so that the visibility of entities in aspect specifications is properly defined. ",(0,s.jsx)("br",{})]})})]})]})}function f(e={}){const{wrapper:r}={...(0,i.R)(),...e.components};return r?(0,s.jsx)(r,{...e,children:(0,s.jsx)(m,{...e})}):m(e)}}}]);