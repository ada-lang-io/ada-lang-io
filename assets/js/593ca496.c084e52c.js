"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[4001],{71736:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>p,contentTitle:()=>h,default:()=>g,frontMatter:()=>c,metadata:()=>s,toc:()=>u});const s=JSON.parse('{"id":"arm/AA-B/AA-B","title":"Annex B Interface to Other Languages","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-B/AA-B.mdx","sourceDirName":"arm/AA-B","slug":"/arm/AA-B/","permalink":"/docs/arm/AA-B/","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":140,"frontMatter":{"sidebar_position":140},"sidebar":"referenceManualSidebar","previous":{"title":"A.19 The Package Locales","permalink":"/docs/arm/AA-A/AA-A.19"},"next":{"title":"B.1 Interfacing Aspects","permalink":"/docs/arm/AA-B/AA-B.1"}}');var i=a(74848),r=a(28453),t=a(13842),o=a(91435),d=(a(21432),a(79162)),l=a(34421);const c={sidebar_position:140},h="Annex B Interface to Other Languages",p={},u=[{value:"Language Design Principles",id:"language-design-principles",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Extensions to Ada 83",id:"extensions-to-ada-83",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}];function m(e){const n={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,r.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.header,{children:(0,i.jsx)(n.h1,{id:"annex-b-interface-to-other-languages",children:"Annex B Interface to Other Languages"})}),"\n",(0,i.jsx)(n.admonition,{type:"danger",children:(0,i.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,i.jsx)(d.A,{children:"1"}),"\n",(0,i.jsxs)("p",{children:["This Annex describes features for writing mixed-language programs. General interface support is presented first; then specific support for C, COBOL, and Fortran is defined, in terms of language interface packages for each of these languages. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(d.A,{children:"1.a"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"This Annex is not a \u201cSpecialized Needs\u201d annex. Every implementation must support all nonoptional features defined here (mainly the package Interfaces). ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(n.h4,{id:"language-design-principles",children:"Language Design Principles"}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(d.A,{children:"1.b"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Ada should have strong support for mixed-language programming. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(n.h4,{id:"implementation-requirements",children:"Implementation Requirements"}),"\n",(0,i.jsx)(d.A,{children:"2/3"}),"\n",(0,i.jsx)(l.A,{items:["AI05-0229-1","AI05-0262-1","AI05-0299-1"]}),"\n",(0,i.jsxs)("p",{children:["Support for interfacing to any foreign language is optional. However, an implementation shall not provide any optional aspect, attribute, library unit, or pragma having the same name as an aspect, attribute, library unit, or pragma (respectively) specified in the subclauses of this Annex unless the provided construct is either as specified in those subclauses or is more limited in capability than that required by those subclauses. A program that attempts to use an unsupported capability of this Annex shall either be identified by the implementation before run time or shall raise an exception at run time. ",(0,i.jsx)("br",{})]}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(d.A,{children:"2.a/3"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"discussion",children:(0,i.jsxs)(n.p,{children:[(0,i.jsx)("strong",{}),"The intent is that the same rules apply for the optional parts of language interfacing as apply for Specialized Needs Annexes. See ",(0,i.jsx)("a",{href:"/docs/arm/AA-1/AA-1.1#Subclause_1.1.3",children:"1.1.3"})," for a discussion of the purpose of these rules. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"extensions-to-ada-83",children:"Extensions to Ada 83"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(d.A,{children:"2.b"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Much of the functionality in this Annex is new to Ada 95. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(d.A,{children:"2.c"}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["This Annex contains what used to be RM83-13.8. ",(0,i.jsx)("br",{})]})})]}),"\n",(0,i.jsx)(o.A,{children:(0,i.jsx)(n.h4,{id:"wording-changes-from-ada-2005",children:"Wording Changes from Ada 2005"})}),"\n",(0,i.jsxs)(o.A,{children:[(0,i.jsx)(d.A,{children:"2.d/3"}),(0,i.jsx)(l.A,{items:["AI05-0262-1"]}),(0,i.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,i.jsxs)(n.p,{children:["Moved the clarification that interfacing to foreign languages is optional and has the same restrictions as a Specialized Needs Annex here. ",(0,i.jsx)("br",{})]})})]})]})}function g(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(m,{...e})}):m(e)}}}]);