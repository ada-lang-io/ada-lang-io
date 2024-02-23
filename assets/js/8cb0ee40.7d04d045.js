"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8344],{38599:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>d,contentTitle:()=>y,default:()=>u,frontMatter:()=>g,metadata:()=>m,toc:()=>p});var a=t(58168),i=(t(96540),t(15680)),o=t(20793),r=t(91435),l=(t(21432),t(79162)),s=t(34421);const g={sidebar_position:190},y="H.5 Pragma Detect_Blocking",m={unversionedId:"arm/AA-H/AA-H.5",id:"arm/AA-H/AA-H.5",title:"H.5 Pragma Detect_Blocking",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-H/AA-H.5.mdx",sourceDirName:"arm/AA-H",slug:"/arm/AA-H/AA-H.5",permalink:"/docs/arm/AA-H/AA-H.5",draft:!1,tags:[],version:"current",sidebarPosition:190,frontMatter:{sidebar_position:190},sidebar:"referenceManualSidebar",previous:{title:"H.4 High Integrity Restrictions",permalink:"/docs/arm/AA-H/AA-H.4"},next:{title:"H.6 Pragma Partition_Elaboration_Policy",permalink:"/docs/arm/AA-H/AA-H.6"}},d={},p=[{value:"Syntax",id:"syntax",level:4},{value:"Post-Compilation Rules",id:"post-compilation-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4}],A={toc:p},c="wrapper";function u(e){let{components:n,...t}=e;return(0,i.yg)(c,(0,a.A)({},A,t,{components:n,mdxType:"MDXLayout"}),(0,i.yg)("h1",{id:"h5-pragma-detect_blocking"},"H.5 Pragma Detect_Blocking"),(0,i.yg)("admonition",{type:"warning"},(0,i.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,i.yg)(l.A,{mdxType:"MarginText"},"1/5"),(0,i.yg)(s.A,{items:["AI95-00305-01","AI12-0267-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"The following ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," requires an implementation to detect potentially blocking operations during the execution of a protected operation or a parallel construct. ",(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"syntax"},"Syntax"),(0,i.yg)(l.A,{mdxType:"MarginText"},"2/2"),(0,i.yg)(s.A,{items:["AI95-00305-01"],mdxType:"MarginInfo"}),(0,i.yg)("p",{class:"Indented2"},"The form of a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," Detect","_","Blocking is as follows: ",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"3/2"),(0,i.yg)("p",{class:"Indented2"},"  ",(0,i.yg)("strong",null,"pragma")," Detect","_","Blocking; ",(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"post-compilation-rules"},"Post-Compilation Rules"),(0,i.yg)(l.A,{mdxType:"MarginText"},"4/2"),(0,i.yg)(s.A,{items:["AI95-00305-01"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"A ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," Detect","_","Blocking is a configuration pragma. ",(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"dynamic-semantics"},"Dynamic Semantics"),(0,i.yg)(l.A,{mdxType:"MarginText"},"5/5"),(0,i.yg)(s.A,{items:["AI95-00305-01","AI12-0247-1","AI12-0267-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"An implementation is required to detect a potentially blocking operation that occurs during the execution of a protected operation or a parallel construct defined within a compilation unit to which the pragma applies, and to raise Program","_","Error (see ",(0,i.yg)("a",{href:"../AA-9/AA-9.5"},"9.5"),"). ",(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"implementation-permissions"},"Implementation Permissions"),(0,i.yg)(l.A,{mdxType:"MarginText"},"6/5"),(0,i.yg)(s.A,{items:["AI95-00305-01","AI12-0267-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"An implementation is allowed to reject a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-10/AA-10.1#S0286"},"compilation_unit"))," to which a pragma Detect","_","Blocking applies if a potentially blocking operation is present directly within an ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-9/AA-9.5#S0260"},"entry_body")),", the body of a protected subprogram, or a parallel construct occurring within the compilation unit. ",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(l.A,{mdxType:"MarginText"},"7/5"),(0,i.yg)(s.A,{items:["AI95-00305-01","AI12-0442-1"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE   An operation that causes a task to be blocked within a foreign language domain is not defined to be potentially blocking, and is unlikely to be detected. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)("h4",{id:"extensions-to-ada-95"},"Extensions to Ada 95")),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(l.A,{mdxType:"MarginText"},"7.a/2"),(0,i.yg)(s.A,{items:["AI95-00305-01"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Pragma Detect","_","Blocking is new. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)("h4",{id:"extensions-to-ada-2012"},"Extensions to Ada 2012")),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(l.A,{mdxType:"MarginText"},"7.b/5"),(0,i.yg)(s.A,{items:["AI12-0267-1"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Pragma Detect","_","Blocking now applies to parallel constructs as well as protected actions. ",(0,i.yg)("br",null))))}u.isMDXComponent=!0}}]);