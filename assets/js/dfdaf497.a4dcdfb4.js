"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9627],{3146:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>k,contentTitle:()=>d,default:()=>y,frontMatter:()=>u,metadata:()=>h,toc:()=>f});var a=n(1716),i=Object.defineProperty,l=Object.defineProperties,o=Object.getOwnPropertyDescriptors,r=Object.getOwnPropertySymbols,s=Object.prototype.hasOwnProperty,m=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?i(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,p=(e,t)=>{for(var n in t||(t={}))s.call(t,n)&&c(e,n,t[n]);if(r)for(var n of r(t))m.call(t,n)&&c(e,n,t[n]);return e};const u={sidebar_position:188},d="H.6  Pragma Partition_Elaboration_Policy",h={unversionedId:"arm/AA-H.6",id:"arm/AA-H.6",title:"H.6  Pragma Partition_Elaboration_Policy",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-H.6.mdx",sourceDirName:"arm",slug:"/arm/AA-H.6",permalink:"/docs/arm/AA-H.6",draft:!1,tags:[],version:"current",sidebarPosition:188,frontMatter:{sidebar_position:188},sidebar:"tutorialSidebar",previous:{title:"H.5  Pragma Detect_Blocking",permalink:"/docs/arm/AA-H.5"},next:{title:"H.7  Extensions to Global and Global'Class Aspects",permalink:"/docs/arm/AA-H.7"}},k={},f=[{value:"Syntax",id:"syntax",level:4},{value:"Post-Compilation Rules",id:"post-compilation-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4}],b={toc:f};function y(e){var t,n=e,{components:i}=n,c=((e,t)=>{var n={};for(var a in e)s.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&r)for(var a of r(e))t.indexOf(a)<0&&m.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=p(p({},b),c),l(t,o({components:i,mdxType:"MDXLayout"}))),(0,a.kt)("h1",p({},{id:"h6--pragma-partition_elaboration_policy"}),"H.6  Pragma Partition_Elaboration_Policy"),(0,a.kt)("admonition",p({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",p({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," This subclause defines a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.8#S0019"},"pragma"))," for user control over elaboration policy. "),(0,a.kt)("h4",p({},{id:"syntax"}),"Syntax"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," The form of a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.8#S0019"},"pragma"))," Partition_Elaboration_Policy is as follows: "),(0,a.kt)("p",null,"  ",(0,a.kt)("strong",null,"pragma")," Partition_Elaboration_Policy (",(0,a.kt)("em",null,"policy_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),");"),(0,a.kt)("p",null,"The ",(0,a.kt)("em",null,"policy_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," shall be either Sequential, Concurrent or an implementation-defined identifier. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Implementation defined: "),"Implementation-defined ",(0,a.kt)("em",null,"policy_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.3#S0002"},"identifier")),"s allowed in a ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.8#S0019"},"pragma"))," Partition_Elaboration_Policy."),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Ramification: "),"Note that the Ravenscar profile (see D.13) has nothing to say about which Partition_Elaboration_Policy is used. This was intentionally omitted from the profile, as there was no agreement as to whether the Sequential policy should be required for Ravenscar programs. As such it was defined separately. "),(0,a.kt)("h4",p({},{id:"post-compilation-rules"}),"Post-Compilation Rules"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," A ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.8#S0019"},"pragma"))," Partition_Elaboration_Policy is a configuration pragma. It specifies the elaboration policy for a partition. At most one elaboration policy shall be specified for a partition."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0264-1"),"}"," If the Sequential policy is specified for a partition, then pragma Restrictions (No_Task_Hierarchy) shall also be specified for the partition. "),(0,a.kt)("h4",p({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," Notwithstanding what this document says elsewhere, this ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.8#S0019"},"pragma"))," allows partition elaboration rules concerning task activation and interrupt attachment to be changed. If the ",(0,a.kt)("em",null,"policy_"),(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.3#S0002"},"identifier"))," is Concurrent, or if there is no pragma Partition_Elaboration_Policy defined for the partition, then the rules defined elsewhere in this Reference Manual apply."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00421-01"),"}"," If the partition elaboration policy is Sequential, then task activation and interrupt attachment are performed in the following sequence of steps:"),(0,a.kt)("p",null,"The activation of all library-level tasks and the attachment of interrupt handlers are deferred until all library units are elaborated."),(0,a.kt)("p",null,"The interrupt handlers are attached by the environment task."),(0,a.kt)("p",null,"The environment task is suspended while the library-level tasks are activated."),(0,a.kt)("p",null,"The environment task executes the main subprogram (if any) concurrently with these executing tasks. "),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00421-01"),"}"," If several dynamic interrupt handler attachments for the same interrupt are deferred, then the most recent call of Attach_Handler or Exchange_Handler determines which handler is attached."),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00421-01"),"}"," If any deferred task activation fails, Tasking_Error is raised at the beginning of the sequence of statements of the body of the environment task prior to calling the main subprogram."),(0,a.kt)("h4",p({},{id:"implementation-advice"}),"Implementation Advice"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0264-1"),"}"," If the partition elaboration policy is Sequential and the Environment task becomes permanently blocked during elaboration, then the partition is deadlocked and it is recommended that the partition be immediately terminated. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Implementation Advice: "),"If the partition elaboration policy is Sequential and the Environment task becomes permanently blocked during elaboration, then the partition should be immediately terminated."),(0,a.kt)("h4",p({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," ","{",(0,a.kt)("em",null,"AI05-0264-1"),"}"," If the partition elaboration policy is Sequential and any task activation fails, then an implementation may immediately terminate the active partition to mitigate the hazard posed by continuing to execute with a subset of the tasks being active. "),(0,a.kt)("p",null,"NOTE 1   ","{",(0,a.kt)("em",null,"AI95-00421-01"),"}"," ","{",(0,a.kt)("em",null,"AI12-0440-1"),"}"," If any deferred task activation fails, the environment task is unable to handle the Tasking_Error exception and completes immediately. By contrast, if the partition elaboration policy is Concurrent, then this exception can be handled within a library unit. "),(0,a.kt)("h4",p({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00265-01"),"}"," ","{",(0,a.kt)("em",null,"AI95-00421-01"),"}"," ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-2.8#S0019"},"Pragma"))," Partition_Elaboration_Policy is new. "))}y.isMDXComponent=!0}}]);