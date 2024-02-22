"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[3672],{49344:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>g,contentTitle:()=>d,default:()=>u,frontMatter:()=>s,metadata:()=>m,toc:()=>p});var a=t(58168),i=(t(96540),t(15680)),o=t(20793),r=t(91435),l=(t(21432),t(79162)),y=t(34421);const s={sidebar_position:191},d="H.6 Pragma Partition_Elaboration_Policy",m={unversionedId:"arm/AA-H/AA-H.6",id:"arm/AA-H/AA-H.6",title:"H.6 Pragma Partition_Elaboration_Policy",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-H/AA-H.6.mdx",sourceDirName:"arm/AA-H",slug:"/arm/AA-H/AA-H.6",permalink:"/docs/arm/AA-H/AA-H.6",draft:!1,tags:[],version:"current",sidebarPosition:191,frontMatter:{sidebar_position:191},sidebar:"referenceManualSidebar",previous:{title:"H.5 Pragma Detect_Blocking",permalink:"/docs/arm/AA-H/AA-H.5"},next:{title:"H.7 Extensions to Global and Global'Class Aspects",permalink:"/docs/arm/AA-H/AA-H.7"}},g={},p=[{value:"Syntax",id:"syntax",level:4},{value:"Post-Compilation Rules",id:"post-compilation-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4}],A={toc:p},c="wrapper";function u(e){let{components:n,...t}=e;return(0,i.yg)(c,(0,a.A)({},A,t,{components:n,mdxType:"MDXLayout"}),(0,i.yg)("h1",{id:"h6-pragma-partition_elaboration_policy"},"H.6 Pragma Partition_Elaboration_Policy"),(0,i.yg)("admonition",{type:"warning"},(0,i.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,i.yg)(l.A,{mdxType:"MarginText"},"1/3"),(0,i.yg)(y.A,{items:["AI95-00265-01","AI05-0299-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"This subclause defines a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," for user control over elaboration policy. ",(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"syntax"},"Syntax"),(0,i.yg)(l.A,{mdxType:"MarginText"},"2/2"),(0,i.yg)(y.A,{items:["AI95-00265-01"],mdxType:"MarginInfo"}),(0,i.yg)("p",{class:"Indented2"},"The form of a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," Partition_Elaboration_Policy is as follows: ",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"3/2"),(0,i.yg)("p",{class:"Indented2"},"  ",(0,i.yg)("strong",null,"pragma")," Partition_Elaboration_Policy (",(0,i.yg)("em",null,"policy_"),(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),");",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"4/2"),(0,i.yg)("p",{class:"Indented2"},"The ",(0,i.yg)("em",null,"policy_"),(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," shall be either Sequential, Concurrent or an implementation-defined identifier. ",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(l.A,{mdxType:"MarginText"},"4.a/2"),(0,i.yg)(o.A,{type:"aarm",aarm:"implementation-defined",mdxType:"Admonition"},(0,i.yg)("strong",null),"Implementation-defined ",(0,i.yg)("em",null,"policy_"),(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier")),"s allowed in a ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," Partition_Elaboration_Policy.",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(l.A,{mdxType:"MarginText"},"4.b/3"),(0,i.yg)(o.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,i.yg)("strong",null),"Note that the Ravenscar profile (see ",(0,i.yg)("a",{href:"../AA-D/AA-D.13"},"D.13"),") has nothing to say about which Partition_Elaboration_Policy is used. This was intentionally omitted from the profile, as there was no agreement as to whether the Sequential policy should be required for Ravenscar programs. As such it was defined separately. ",(0,i.yg)("br",null))),(0,i.yg)("h4",{id:"post-compilation-rules"},"Post-Compilation Rules"),(0,i.yg)(l.A,{mdxType:"MarginText"},"5/2"),(0,i.yg)(y.A,{items:["AI95-00265-01"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"A ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," Partition_Elaboration_Policy is a configuration pragma. It specifies the elaboration policy for a partition. At most one elaboration policy shall be specified for a partition.",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"6/3"),(0,i.yg)(y.A,{items:["AI95-00265-01","AI05-0264-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"If the Sequential policy is specified for a partition, then pragma Restrictions (No_Task_Hierarchy) shall also be specified for the partition. ",(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"dynamic-semantics"},"Dynamic Semantics"),(0,i.yg)(l.A,{mdxType:"MarginText"},"7/2"),(0,i.yg)(y.A,{items:["AI95-00265-01"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"Notwithstanding what this document says elsewhere, this ",(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," allows partition elaboration rules concerning task activation and interrupt attachment to be changed. If the ",(0,i.yg)("em",null,"policy_"),(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.3#S0002"},"identifier"))," is Concurrent, or if there is no pragma Partition_Elaboration_Policy defined for the partition, then the rules defined elsewhere in this Reference Manual apply.",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"8/2"),(0,i.yg)(y.A,{items:["AI95-00265-01","AI95-00421-01"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"If the partition elaboration policy is Sequential, then task activation and interrupt attachment are performed in the following sequence of steps:",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"9/2"),(0,i.yg)("ul",null,(0,i.yg)("li",null,"The activation of all library-level tasks and the attachment of interrupt handlers are deferred until all library units are elaborated.",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"10/2"),(0,i.yg)("li",null,"The interrupt handlers are attached by the environment task.",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"11/2"),(0,i.yg)("li",null,"The environment task is suspended while the library-level tasks are activated.",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"12/2"),(0,i.yg)("li",null,"The environment task executes the main subprogram (if any) concurrently with these executing tasks. ",(0,i.yg)("br",null))),(0,i.yg)(l.A,{mdxType:"MarginText"},"13/2"),(0,i.yg)(y.A,{items:["AI95-00265-01","AI95-00421-01"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"If several dynamic interrupt handler attachments for the same interrupt are deferred, then the most recent call of Attach_Handler or Exchange_Handler determines which handler is attached.",(0,i.yg)("br",null)),(0,i.yg)(l.A,{mdxType:"MarginText"},"14/2"),(0,i.yg)(y.A,{items:["AI95-00265-01","AI95-00421-01"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"If any deferred task activation fails, Tasking_Error is raised at the beginning of the sequence of statements of the body of the environment task prior to calling the main subprogram.",(0,i.yg)("br",null)),(0,i.yg)("h4",{id:"implementation-advice"},"Implementation Advice"),(0,i.yg)(l.A,{mdxType:"MarginText"},"15/3"),(0,i.yg)(y.A,{items:["AI95-00265-01","AI05-0264-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"If the partition elaboration policy is Sequential and the Environment task becomes permanently blocked during elaboration, then the partition is deadlocked and it is recommended that the partition be immediately terminated. ",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(l.A,{mdxType:"MarginText"},"15.a/3"),(0,i.yg)(o.A,{type:"aarm",aarm:"implementation-advice",mdxType:"Admonition"},(0,i.yg)("strong",null),"If the partition elaboration policy is Sequential and the Environment task becomes permanently blocked during elaboration, then the partition should be immediately terminated.",(0,i.yg)("br",null))),(0,i.yg)("h4",{id:"implementation-permissions"},"Implementation Permissions"),(0,i.yg)(l.A,{mdxType:"MarginText"},"16/3"),(0,i.yg)(y.A,{items:["AI95-00265-01","AI05-0264-1"],mdxType:"MarginInfo"}),(0,i.yg)("p",null,"If the partition elaboration policy is Sequential and any task activation fails, then an implementation may immediately terminate the active partition to mitigate the hazard posed by continuing to execute with a subset of the tasks being active. ",(0,i.yg)("br",null)),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(l.A,{mdxType:"MarginText"},"17/5"),(0,i.yg)(y.A,{items:["AI95-00421-01","AI12-0440-1"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE   If any deferred task activation fails, the environment task is unable to handle the Tasking_Error exception and completes immediately. By contrast, if the partition elaboration policy is Concurrent, then this exception can be handled within a library unit. ",(0,i.yg)("br",null))),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)("h4",{id:"extensions-to-ada-95"},"Extensions to Ada 95")),(0,i.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,i.yg)(l.A,{mdxType:"MarginText"},"17.a/2"),(0,i.yg)(y.A,{items:["AI95-00265-01","AI95-00421-01"],mdxType:"MarginInfo"}),(0,i.yg)(o.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.yg)("code",null,(0,i.yg)("a",{href:"../AA-2/AA-2.8#S0019"},"Pragma"))," Partition_Elaboration_Policy is new. ",(0,i.yg)("br",null))))}u.isMDXComponent=!0}}]);