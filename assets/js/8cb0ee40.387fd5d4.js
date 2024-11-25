"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8344],{4372:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>A,contentTitle:()=>h,default:()=>x,frontMatter:()=>d,metadata:()=>s,toc:()=>m});const s=JSON.parse('{"id":"arm/AA-H/AA-H.5","title":"H.5 Pragma Detect_Blocking","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-H/AA-H.5.mdx","sourceDirName":"arm/AA-H","slug":"/arm/AA-H/AA-H.5","permalink":"/docs/arm/AA-H/AA-H.5","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":190,"frontMatter":{"sidebar_position":190},"sidebar":"referenceManualSidebar","previous":{"title":"H.4 High Integrity Restrictions","permalink":"/docs/arm/AA-H/AA-H.4"},"next":{"title":"H.6 Pragma Partition_Elaboration_Policy","permalink":"/docs/arm/AA-H/AA-H.6"}}');var a=i(74848),t=i(28453),r=i(13842),o=i(91435),l=(i(21432),i(79162)),c=i(34421);const d={sidebar_position:190},h="H.5 Pragma Detect_Blocking",A={},m=[{value:"Syntax",id:"syntax",level:4},{value:"Post-Compilation Rules",id:"post-compilation-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4}];function p(e){const n={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,t.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.header,{children:(0,a.jsx)(n.h1,{id:"h5-pragma-detect_blocking",children:"H.5 Pragma Detect_Blocking"})}),"\n",(0,a.jsx)(n.admonition,{type:"danger",children:(0,a.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,a.jsx)(l.A,{children:"1/5"}),"\n",(0,a.jsx)(c.A,{items:["AI95-00305-01","AI12-0267-1"]}),"\n",(0,a.jsxs)("p",{children:["The following ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," requires an implementation to detect potentially blocking operations during the execution of a protected operation or a parallel construct. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(n.h4,{id:"syntax",children:"Syntax"}),"\n",(0,a.jsx)(l.A,{children:"2/2"}),"\n",(0,a.jsx)(c.A,{items:["AI95-00305-01"]}),"\n",(0,a.jsxs)("p",{class:"Indented2",children:["The form of a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Detect","_","Blocking is as follows: ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(l.A,{children:"3/2"}),"\n",(0,a.jsxs)("p",{class:"Indented2",children:["  ",(0,a.jsx)("strong",{children:"pragma"})," Detect","_","Blocking; ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(n.h4,{id:"post-compilation-rules",children:"Post-Compilation Rules"}),"\n",(0,a.jsx)(l.A,{children:"4/2"}),"\n",(0,a.jsx)(c.A,{items:["AI95-00305-01"]}),"\n",(0,a.jsxs)("p",{children:["A ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Detect","_","Blocking is a configuration pragma. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(n.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,a.jsx)(l.A,{children:"5/5"}),"\n",(0,a.jsx)(c.A,{items:["AI95-00305-01","AI12-0247-1","AI12-0267-1"]}),"\n",(0,a.jsxs)("p",{children:["An implementation is required to detect a potentially blocking operation that occurs during the execution of a protected operation or a parallel construct defined within a compilation unit to which the pragma applies, and to raise Program","_","Error (see ",(0,a.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5",children:"9.5"}),"). ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(n.h4,{id:"implementation-permissions",children:"Implementation Permissions"}),"\n",(0,a.jsx)(l.A,{children:"6/5"}),"\n",(0,a.jsx)(c.A,{items:["AI95-00305-01","AI12-0267-1"]}),"\n",(0,a.jsxs)("p",{children:["An implementation is allowed to reject a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0286",children:"compilation_unit"})})," to which a pragma Detect","_","Blocking applies if a potentially blocking operation is present directly within an ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0260",children:"entry_body"})}),", the body of a protected subprogram, or a parallel construct occurring within the compilation unit. ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(l.A,{children:"7/5"}),(0,a.jsx)(c.A,{items:["AI95-00305-01","AI12-0442-1"]}),(0,a.jsx)(r.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(n.p,{children:["NOTE   An operation that causes a task to be blocked within a foreign language domain is not defined to be potentially blocking, and is unlikely to be detected. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:(0,a.jsx)(n.h4,{id:"extensions-to-ada-95",children:"Extensions to Ada 95"})}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(l.A,{children:"7.a/2"}),(0,a.jsx)(c.A,{items:["AI95-00305-01"]}),(0,a.jsx)(r.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(n.p,{children:["Pragma Detect","_","Blocking is new. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:(0,a.jsx)(n.h4,{id:"extensions-to-ada-2012",children:"Extensions to Ada 2012"})}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(l.A,{children:"7.b/5"}),(0,a.jsx)(c.A,{items:["AI12-0267-1"]}),(0,a.jsx)(r.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(n.p,{children:["Pragma Detect","_","Blocking now applies to parallel constructs as well as protected actions. ",(0,a.jsx)("br",{})]})})]})]})}function x(e={}){const{wrapper:n}={...(0,t.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(p,{...e})}):p(e)}}}]);