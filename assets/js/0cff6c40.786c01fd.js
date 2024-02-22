"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1165],{80179:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>A,contentTitle:()=>l,default:()=>h,frontMatter:()=>m,metadata:()=>g,toc:()=>p});var t=a(58168),i=(a(96540),a(15680)),r=a(20793),o=a(91435),s=a(21432),d=a(79162),c=a(34421);const m={sidebar_position:194},l="J.1 Renamings of Library Units",g={unversionedId:"arm/AA-J/AA-J.1",id:"arm/AA-J/AA-J.1",title:"J.1 Renamings of Library Units",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-J/AA-J.1.mdx",sourceDirName:"arm/AA-J",slug:"/arm/AA-J/AA-J.1",permalink:"/docs/arm/AA-J/AA-J.1",draft:!1,tags:[],version:"current",sidebarPosition:194,frontMatter:{sidebar_position:194},sidebar:"referenceManualSidebar",previous:{title:"Annex J Obsolescent Features",permalink:"/docs/arm/AA-J/"},next:{title:"J.2 Allowed Replacements of Characters",permalink:"/docs/arm/AA-J/AA-J.2"}},A={},p=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4}],y={toc:p},u="wrapper";function h(e){let{components:n,...a}=e;return(0,i.yg)(u,(0,t.A)({},y,a,{components:n,mdxType:"MDXLayout"}),(0,i.yg)("h1",{id:"j1-renamings-of-library-units"},"J.1 Renamings of Library Units"),(0,i.yg)("admonition",{type:"warning"},(0,i.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,i.yg)("h4",{id:"static-semantics"},"Static Semantics"),(0,i.yg)(d.A,{mdxType:"MarginText"},"1"),(0,i.yg)("p",null,"The following ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-10/AA-10.1#S0289"},"library_unit_renaming_declaration")),"s exist: ",(0,i.yg)("br",null)),(0,i.yg)(d.A,{mdxType:"MarginText"},"2"),(0,i.yg)(s.A,{language:"ada",mdxType:"CodeBlock"},"with Ada.Unchecked_Conversion;","\n","generic function Unchecked_Conversion renames Ada.Unchecked_Conversion;","\n",(0,i.yg)(d.A,{mdxType:"MarginText"},"3"),"with Ada.Unchecked_Deallocation;","\n","generic procedure Unchecked_Deallocation renames Ada.Unchecked_Deallocation;","\n",(0,i.yg)(d.A,{mdxType:"MarginText"},"4"),"with Ada.Sequential_IO;","\n","generic package Sequential_IO renames Ada.Sequential_IO;","\n",(0,i.yg)(d.A,{mdxType:"MarginText"},"5"),"with Ada.Direct_IO;","\n","generic package Direct_IO renames Ada.Direct_IO;","\n",(0,i.yg)(d.A,{mdxType:"MarginText"},"6"),"with Ada.Text_IO;","\n","package Text_IO renames Ada.Text_IO;","\n",(0,i.yg)(d.A,{mdxType:"MarginText"},"7"),"with Ada.IO_Exceptions;","\n","package IO_Exceptions renames Ada.IO_Exceptions;","\n",(0,i.yg)(d.A,{mdxType:"MarginText"},"8"),"with Ada.Calendar;","\n","package Calendar renames Ada.Calendar;","\n",(0,i.yg)(d.A,{mdxType:"MarginText"},"9"),"with System.Machine_Code;","\n","package Machine_Code renames System.Machine_Code; -- If supported.","\n"),(0,i.yg)(o.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(d.A,{mdxType:"MarginText"},"9.a/3"),(0,i.yg)(c.A,{items:["AI05-0004-1"],mdxType:"MarginInfo"}),(0,i.yg)(r.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.yg)("strong",null),"These library units correspond to those declared in Ada 83, which did not have the child unit concept or the parent package Ada. ",(0,i.yg)("br",null))),(0,i.yg)("h4",{id:"implementation-requirements"},"Implementation Requirements"),(0,i.yg)(d.A,{mdxType:"MarginText"},"10"),(0,i.yg)("p",null,"The implementation shall allow the user to replace these renamings. ",(0,i.yg)("br",null)))}h.isMDXComponent=!0}}]);