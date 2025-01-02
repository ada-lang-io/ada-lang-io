"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1892],{3207:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>x,contentTitle:()=>l,default:()=>A,frontMatter:()=>h,metadata:()=>s,toc:()=>p});const s=JSON.parse('{"id":"arm/AA-11/AA-11","title":"11 Exceptions","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-11/AA-11.mdx","sourceDirName":"arm/AA-11","slug":"/arm/AA-11/","permalink":"/docs/arm/AA-11/","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":88,"frontMatter":{"sidebar_position":88},"sidebar":"referenceManualSidebar","previous":{"title":"10.2 Program Execution","permalink":"/docs/arm/AA-10/AA-10.2"},"next":{"title":"11.1 Exception Declarations","permalink":"/docs/arm/AA-11/AA-11.1"}}');var a=r(4848),i=r(8453),t=r(3842),o=r(1435),c=(r(1432),r(9162)),d=r(4421);const h={sidebar_position:88},l="11 Exceptions",x={},p=[{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}];function m(e){const n={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.header,{children:(0,a.jsx)(n.h1,{id:"11-exceptions",children:"11 Exceptions"})}),"\n",(0,a.jsx)(n.admonition,{type:"danger",children:(0,a.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,a.jsx)(c.A,{children:"1/3"}),"\n",(0,a.jsx)(d.A,{items:["AI05-0299-1"]}),"\n",(0,a.jsxs)("p",{children:["[This clause defines the facilities for dealing with errors or other exceptional situations that arise during program execution.] An ",(0,a.jsx)("em",{children:"exception"})," represents a kind of exceptional situation; an occurrence of such a situation (at run time) is called an ",(0,a.jsx)("em",{children:"exception occurrence"}),". [ To ",(0,a.jsx)("em",{children:"raise"})," an exception is to abandon normal program execution so as to draw attention to the fact that the corresponding situation has arisen. Performing some actions in response to the arising of an exception is called ",(0,a.jsx)("em",{children:"handling"})," the exception. ]",(0,a.jsx)("br",{})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(c.A,{children:"1.a"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(n.p,{children:[(0,a.jsx)("strong",{children:"To be honest: "}),"...or handling the exception occurrence. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(c.A,{children:"1.b"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"ramification",children:(0,a.jsxs)(n.p,{children:[(0,a.jsx)("strong",{}),"For example, an exception End","_","Error might represent error situations in which an attempt is made to read beyond end-of-file. During the execution of a partition, there might be numerous occurrences of this exception. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(c.A,{children:"1.c/5"}),(0,a.jsx)(d.A,{items:["AI12-0426-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(n.p,{children:[(0,a.jsx)("strong",{children:"To be honest: "}),"When the meaning is clear from the context, we sometimes use \u201c",(0,a.jsx)("em",{children:"occurrence"}),"\u201d as a shorthand for \u201cexception occurrence\u201d. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(c.A,{children:"1.d/5"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(n.p,{children:[(0,a.jsx)("strong",{children:"Term entry: "}),(0,a.jsx)("strong",{children:"exception"})," \u2014 kind of exceptional situation",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(c.A,{children:"1.e/5"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(n.p,{children:[(0,a.jsx)("strong",{children:"Term entry: "}),(0,a.jsx)("strong",{children:"exception occurrence"})," \u2014 run-time occurrence of an exceptional situation",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(c.A,{children:"2/3"}),"\n",(0,a.jsx)(d.A,{items:["AI05-0043-1","AI05-0258-1"]}),"\n",(0,a.jsxs)("p",{children:["[An ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-11/AA-11.1#S0303",children:"exception_declaration"})})," declares a name for an exception. An exception can be raised explicitly (for example, by a ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-11/AA-11.3#S0308",children:"raise_statement"})}),") or implicitly (for example, by the failure of a language-defined check). When an exception arises, control can be transferred to a user-provided ",(0,a.jsx)("code",{children:(0,a.jsx)("a",{href:"/docs/arm/AA-11/AA-11.2#S0305",children:"exception_handler"})})," at the end of a ",(0,a.jsx)("code",{children:(0,a.jsxs)("a",{href:"/docs/arm/AA-11/AA-11.2#S0304",children:["handled","_","sequence","_","of","_","statements"]})}),", or it can be propagated to a dynamically enclosing execution.] ",(0,a.jsx)("br",{})]}),"\n",(0,a.jsx)(o.A,{children:(0,a.jsx)(n.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(c.A,{children:"2.a"}),(0,a.jsx)(t.A,{type:"aarm",aarm:"note",children:(0,a.jsxs)(n.p,{children:["We are more explicit about the difference between an exception and an occurrence of an exception. This is necessary because we now have a type (Exception","_","Occurrence) that represents exception occurrences, so the program can manipulate them. Furthermore, we say that when an exception is propagated, it is the same occurrence that is being propagated (as opposed to a new occurrence of the same exception). The same issue applies to a re-raise statement. In order to understand these semantics, we have to make this distinction. ",(0,a.jsx)("br",{})]})})]}),"\n",(0,a.jsx)(o.A,{children:(0,a.jsx)(n.h4,{id:"wording-changes-from-ada-2005",children:"Wording Changes from Ada 2005"})}),"\n",(0,a.jsxs)(o.A,{children:[(0,a.jsx)(c.A,{children:"2.b/3"}),(0,a.jsx)(d.A,{items:["AI05-0043-1"]}),(0,a.jsx)(t.A,{type:"aarm",aarm:"correction",children:(0,a.jsxs)(n.p,{children:[(0,a.jsx)("strong",{})," Generalized the introductory description of how an exception can be raised so that it does not appear to cover all of the cases. ",(0,a.jsx)("br",{})]})})]})]})}function A(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(m,{...e})}):m(e)}}}]);