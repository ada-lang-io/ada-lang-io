"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1165],{55984:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>A,contentTitle:()=>m,default:()=>x,frontMatter:()=>h,metadata:()=>i,toc:()=>p});const i=JSON.parse('{"id":"arm/AA-J/AA-J.1","title":"J.1 Renamings of Library Units","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-J/AA-J.1.mdx","sourceDirName":"arm/AA-J","slug":"/arm/AA-J/AA-J.1","permalink":"/docs/arm/AA-J/AA-J.1","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":194,"frontMatter":{"sidebar_position":194},"sidebar":"referenceManualSidebar","previous":{"title":"Annex J Obsolescent Features","permalink":"/docs/arm/AA-J/"},"next":{"title":"J.2 Allowed Replacements of Characters","permalink":"/docs/arm/AA-J/AA-J.2"}}');var s=a(74848),r=a(28453),t=a(13842),c=a(91435),d=a(21432),o=a(79162),l=a(34421);const h={sidebar_position:194},m="J.1 Renamings of Library Units",A={},p=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4}];function u(e){const n={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,r.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.header,{children:(0,s.jsx)(n.h1,{id:"j1-renamings-of-library-units",children:"J.1 Renamings of Library Units"})}),"\n",(0,s.jsx)(n.admonition,{type:"danger",children:(0,s.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,s.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,s.jsx)(n.h4,{id:"static-semantics",children:"Static Semantics"}),"\n",(0,s.jsx)(o.A,{children:"1"}),"\n",(0,s.jsxs)("p",{children:["The following ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-10/AA-10.1#S0289",children:"library_unit_renaming_declaration"})}),"s exist: ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(o.A,{children:"2"}),"\n",(0,s.jsx)(d.A,{language:"ada",children:(0,s.jsxs)(n.p,{children:["with Ada.Unchecked","_","Conversion;","\n","generic function Unchecked","_","Conversion renames Ada.Unchecked","_","Conversion;","\n","\n",(0,s.jsx)(o.A,{children:"3"}),"\nwith Ada.Unchecked","_","Deallocation;","\n","generic procedure Unchecked","_","Deallocation renames Ada.Unchecked","_","Deallocation;","\n","\n",(0,s.jsx)(o.A,{children:"4"}),"\nwith Ada.Sequential","_","IO;","\n","generic package Sequential","_","IO renames Ada.Sequential","_","IO;","\n","\n",(0,s.jsx)(o.A,{children:"5"}),"\nwith Ada.Direct","_","IO;","\n","generic package Direct","_","IO renames Ada.Direct","_","IO;","\n","\n",(0,s.jsx)(o.A,{children:"6"}),"\nwith Ada.Text","_","IO;","\n","package Text","_","IO renames Ada.Text","_","IO;","\n","\n",(0,s.jsx)(o.A,{children:"7"}),"\nwith Ada.IO","_","Exceptions;","\n","package IO","_","Exceptions renames Ada.IO","_","Exceptions;","\n","\n",(0,s.jsx)(o.A,{children:"8"}),"\nwith Ada.Calendar;","\n","package Calendar renames Ada.Calendar;","\n","\n",(0,s.jsx)(o.A,{children:"9"}),"\nwith System.Machine","_","Code;","\n","package Machine","_","Code renames System.Machine","_","Code; -- If supported.","\n"]})}),"\n",(0,s.jsxs)(c.A,{children:[(0,s.jsx)(o.A,{children:"9.a/3"}),(0,s.jsx)(l.A,{items:["AI05-0004-1"]}),(0,s.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,s.jsxs)(n.p,{children:[(0,s.jsx)("strong",{}),"These library units correspond to those declared in Ada 83, which did not have the child unit concept or the parent package Ada. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(n.h4,{id:"implementation-requirements",children:"Implementation Requirements"}),"\n",(0,s.jsx)(o.A,{children:"10"}),"\n",(0,s.jsxs)("p",{children:["The implementation shall allow the user to replace these renamings. ",(0,s.jsx)("br",{})]})]})}function x(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(u,{...e})}):u(e)}}}]);