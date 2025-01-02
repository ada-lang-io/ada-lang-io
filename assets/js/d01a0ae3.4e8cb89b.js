"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1836],{8786:(e,r,n)=>{n.r(r),n.d(r,{assets:()=>x,contentTitle:()=>A,default:()=>m,frontMatter:()=>h,metadata:()=>s,toc:()=>j});const s=JSON.parse('{"id":"arm/AA-6/AA-6.7","title":"6.7 Null Procedures","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-6/AA-6.7.mdx","sourceDirName":"arm/AA-6","slug":"/arm/AA-6/AA-6.7","permalink":"/docs/arm/AA-6/AA-6.7","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":57,"frontMatter":{"sidebar_position":57},"sidebar":"referenceManualSidebar","previous":{"title":"6.6 Overloading of Operators","permalink":"/docs/arm/AA-6/AA-6.6"},"next":{"title":"6.8 Expression Functions","permalink":"/docs/arm/AA-6/AA-6.8"}}');var a=n(4848),i=n(8453),c=n(3842),d=n(1435),l=n(1432),o=n(9162),t=n(4421);const h={sidebar_position:57},A="6.7 Null Procedures",x={},j=[{value:"Syntax",id:"syntax",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function u(e){const r={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(r.header,{children:(0,a.jsx)(r.h1,{id:"67-null-procedures",children:"6.7 Null Procedures"})}),"\n",(0,a.jsx)(r.admonition,{type:"danger",children:(0,a.jsxs)(r.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.jsx)(r.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,a.jsx)(o.A,{children:"1/2"}),"\n",(0,a.jsx)(t.A,{items:["AI95-00348-01"]}),"\n",(0,a.jsxs)("p",{children:["A ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})})," provides a shorthand to declare a procedure with an empty body. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(r.h4,{id:"syntax",children:"Syntax"}),"\n",(0,a.jsx)(o.A,{children:"2/3"}),"\n",(0,a.jsx)(t.A,{items:["AI95-00348-01","AI05-0183-1"]}),"\n",(0,a.jsx)(l.A,{children:(0,a.jsxs)(r.p,{children:[(0,a.jsxs)("code",{children:["null","_","procedure","_","declaration"]}),(0,a.jsx)("a",{id:"S0227"}),(0,a.jsx)("code",{children:" ::= "}),(0,a.jsx)("br",{}),"   [",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-8/AA-8.3#S0234",children:"overriding_indicator"})}),"]",(0,a.jsx)("br",{}),"   ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0197",children:"procedure_specification"})})," ",(0,a.jsx)("strong",{children:"is"})," ",(0,a.jsx)("strong",{children:"null"}),(0,a.jsx)("br",{}),"       [",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})}),"];",(0,a.jsx)("br",{})]})}),"\n",(0,a.jsx)(r.h4,{id:"legality-rules",children:"Legality Rules"}),"\n",(0,a.jsx)(o.A,{children:"2.1/3"}),"\n",(0,a.jsx)(t.A,{items:["AI05-0177-1"]}),"\n",(0,a.jsxs)("p",{children:["If a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})})," is a completion, it shall be the completion of a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0195",children:"subprogram_declaration"})})," or ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-12/AA-12.1#S0311",children:"generic_subprogram_declaration"})}),". The profile of a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})})," that completes a declaration shall conform fully to that of the declaration. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(r.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,a.jsx)(o.A,{children:"3/5"}),"\n",(0,a.jsx)(t.A,{items:["AI95-00348-01","AI05-0177-1","AI05-0264-1","AI12-0408-1"]}),"\n",(0,a.jsxs)("p",{children:["A ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})})," that is not a completion declares a ",(0,a.jsx)("em",{children:"null procedure"}),". A completion is not allowed for a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})}),"; however, a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})})," can complete a previous declaration. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"3.a/2"}),(0,a.jsx)(c.A,{type:"aarm",aarm:"reason",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{}),"There are no null functions because the return value has to be constructed somehow; a function that always raises Program","_","Error doesn't seem very useful or worth the complication. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(r.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,a.jsx)(o.A,{children:"4/5"}),"\n",(0,a.jsx)(t.A,{items:["AI95-00348-01","AI12-0408-1"]}),"\n",(0,a.jsxs)("p",{children:["The execution of a null procedure is invoked by a subprogram call. For the execution of a subprogram call on a null procedure, or on a procedure completed with a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})}),", the execution of the ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.3#S0216",children:"subprogram_body"})})," has no effect. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"4.a/2"}),(0,a.jsx)(c.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(r.p,{children:[(0,a.jsx)("strong",{}),"Thus, a null procedure is equivalent to the body ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"4.b/2"}),(0,a.jsx)(l.A,{language:"ada",children:(0,a.jsxs)(r.p,{children:["begin","\n","   null;","\n","end;","\n"]})})]}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"4.c/2"}),(0,a.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["with the exception that a null procedure can be used in place of a procedure specification. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:"5/3"}),"\n",(0,a.jsx)(t.A,{items:["AI95-00348-01","AI05-0177-1"]}),"\n",(0,a.jsxs)("p",{children:["The elaboration of a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})})," has no other effect than to establish that the null procedure can be called without failing the Elaboration","_","Check. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(r.h4,{id:"examples",children:"Examples"}),"\n",(0,a.jsx)(o.A,{children:"6/5"}),"\n",(0,a.jsx)(t.A,{items:["AI12-0429-1"]}),"\n",(0,a.jsxs)("p",{children:[(0,a.jsx)("em",{children:"Example of the declaration of a null procedure:"})," ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(o.A,{children:"7/5"}),"\n",(0,a.jsx)(t.A,{items:["AI95-00433-01","AI12-0440-1"]}),"\n",(0,a.jsx)(l.A,{language:"ada",children:(0,a.jsxs)(r.p,{children:["procedure Simplify(Expr : in out Expression) is null; -- see ",(0,a.jsx)("a",{href:"/docs/arm/AA-3/AA-3.9",children:"3.9"}),"\n","-- By default, Simplify does nothing, but it can be overridden in extensions of Expression","\n"]})}),"\n",(0,a.jsx)(d.A,{children:(0,a.jsx)(r.h4,{id:"extensions-to-ada-95",children:"Extensions to Ada 95"})}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"7.a/2"}),(0,a.jsx)(t.A,{items:["AI95-00348-01"]}),(0,a.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["Null procedures are new. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(d.A,{children:(0,a.jsx)(r.h4,{id:"extensions-to-ada-2005",children:"Extensions to Ada 2005"})}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"7.b/3"}),(0,a.jsx)(t.A,{items:["AI05-0177-1"]}),(0,a.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["A ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})})," can now be a completion.",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"7.c/3"}),(0,a.jsx)(t.A,{items:["AI05-0183-1"]}),(0,a.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["An optional ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#S0346",children:"aspect_specification"})})," can be used in a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.7#S0227",children:"null_procedure_declaration"})}),". This is described in ",(0,a.jsx)("a",{href:"/docs/arm/AA-13/AA-13.1#Subclause_13.1.1",children:"13.1.1"}),". ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(d.A,{children:(0,a.jsx)(r.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,a.jsxs)(d.A,{children:[(0,a.jsx)(o.A,{children:"7.d/5"}),(0,a.jsx)(t.A,{items:["AI12-0408-1"]}),(0,a.jsx)(c.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(r.p,{children:["Clarified the term \u201cnull procedure\u201d so it matches the meaning expected in ",(0,a.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1",children:"6.1"}),". ",(0,a.jsx)("br",{})]})})]})]})}function m(e={}){const{wrapper:r}={...(0,i.R)(),...e.components};return r?(0,a.jsx)(r,{...e,children:(0,a.jsx)(u,{...e})}):u(e)}}}]);