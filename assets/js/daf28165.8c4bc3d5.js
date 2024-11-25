"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6960],{17546:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>f,contentTitle:()=>l,default:()=>u,frontMatter:()=>c,metadata:()=>a,toc:()=>m});const a=JSON.parse('{"id":"arm/AA-8/AA-8","title":"8 Visibility Rules","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-8/AA-8.mdx","sourceDirName":"arm/AA-8","slug":"/arm/AA-8/","permalink":"/docs/arm/AA-8/","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":66,"frontMatter":{"sidebar_position":66},"sidebar":"referenceManualSidebar","previous":{"title":"7.6 Assignment and Finalization","permalink":"/docs/arm/AA-7/AA-7.6"},"next":{"title":"8.1 Declarative Region","permalink":"/docs/arm/AA-8/AA-8.1"}}');var t=i(74848),s=i(28453),r=i(13842),o=i(91435),d=(i(21432),i(79162)),h=i(34421);const c={sidebar_position:66},l="8 Visibility Rules",f={},m=[{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4}];function A(e){const n={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,s.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.header,{children:(0,t.jsx)(n.h1,{id:"8-visibility-rules",children:"8 Visibility Rules"})}),"\n",(0,t.jsx)(n.admonition,{type:"danger",children:(0,t.jsxs)(n.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,t.jsx)(n.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,t.jsx)(d.A,{children:"1/3"}),"\n",(0,t.jsx)(h.A,{items:["AI05-0299-1"]}),"\n",(0,t.jsxs)("p",{children:["[The rules defining the scope of declarations and the rules defining which ",(0,t.jsx)("code",{children:(0,t.jsx)("a",{href:"/docs/arm/AA-2/AA-2.3#S0002",children:"identifier"})}),"s, ",(0,t.jsx)("code",{children:(0,t.jsx)("a",{href:"/docs/arm/AA-2/AA-2.5#S0015",children:"character_literal"})}),"s, and ",(0,t.jsx)("code",{children:(0,t.jsx)("a",{href:"/docs/arm/AA-6/AA-6.1#S0202",children:"operator_symbol"})}),"s are visible at (or from) various places in the text of the program are described in this clause. The formulation of these rules uses the notion of a declarative region.",(0,t.jsx)("br",{})]}),"\n",(0,t.jsx)(d.A,{children:"2/5"}),"\n",(0,t.jsx)(h.A,{items:["AI12-0439-1"]}),"\n",(0,t.jsxs)("p",{children:["As explained in Clause ",(0,t.jsx)("a",{href:"/docs/arm/AA-3/",children:"3"}),", a declaration declares a view of an entity and associates a defining name with that view. The view comprises an identification of the viewed entity, and possibly additional properties. A usage name denotes a declaration. It also denotes the view declared by that declaration, and denotes the entity of that view. Thus, two different usage names can denote two different views of the same entity; in this case they denote the same entity.] ",(0,t.jsx)("br",{})]}),"\n",(0,t.jsxs)(o.A,{children:[(0,t.jsx)(d.A,{children:"2.a"}),(0,t.jsx)(r.A,{type:"aarm",aarm:"note",children:(0,t.jsxs)(n.p,{children:[(0,t.jsx)("strong",{children:"To be honest: "}),"In some cases, a usage name that denotes a declaration does not denote the view declared by that declaration, nor the entity of that view, but instead denotes a view of the current instance of the entity, and denotes the current instance of the entity. This sometimes happens when the usage name occurs inside the declarative region of the declaration. ",(0,t.jsx)("br",{})]})})]}),"\n",(0,t.jsx)(o.A,{children:(0,t.jsx)(n.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,t.jsxs)(o.A,{children:[(0,t.jsx)(d.A,{children:"2.b"}),(0,t.jsx)(r.A,{type:"aarm",aarm:"note",children:(0,t.jsxs)(n.p,{children:["We no longer define the term \u201cbasic operation;\u201d thus we no longer have to worry about the visibility of them. Since they were essentially always visible in Ada 83, this change has no effect. The reason for this change is that the definition in Ada 83 was confusing, and not quite correct, and we found it difficult to fix. For example, one wonders why an ",(0,t.jsx)("code",{children:(0,t.jsx)("a",{href:"/docs/arm/AA-5/AA-5.3#S0175",children:"if_statement"})})," was not a basic operation of type Boolean. For another example, one wonders what it meant for a basic operation to be \u201cinherent in\u201d something. Finally, this fixes the problem addressed by AI83-00027/07. ",(0,t.jsx)("br",{})]})})]})]})}function u(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(A,{...e})}):A(e)}}}]);