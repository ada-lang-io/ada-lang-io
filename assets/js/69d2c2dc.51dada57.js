"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7601],{44193:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>g,contentTitle:()=>f,default:()=>Z,frontMatter:()=>T,metadata:()=>x,toc:()=>A});var i=n(91716),a=n(21256),o=n(24895),r=n(28090),s=n(48424),l=n(82262),d=Object.defineProperty,p=Object.defineProperties,m=Object.getOwnPropertyDescriptors,c=Object.getOwnPropertySymbols,u=Object.prototype.hasOwnProperty,k=Object.prototype.propertyIsEnumerable,h=(e,t,n)=>t in e?d(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,y=(e,t)=>{for(var n in t||(t={}))u.call(t,n)&&h(e,n,t[n]);if(c)for(var n of c(t))k.call(t,n)&&h(e,n,t[n]);return e};const T={sidebar_position:165},f="D.11 Asynchronous Task Control",x={unversionedId:"arm/AA-D/AA-D.11",id:"arm/AA-D/AA-D.11",title:"D.11 Asynchronous Task Control",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-D/AA-D.11.mdx",sourceDirName:"arm/AA-D",slug:"/arm/AA-D/AA-D.11",permalink:"/docs/arm/AA-D/AA-D.11",draft:!1,tags:[],version:"current",sidebarPosition:165,frontMatter:{sidebar_position:165},sidebar:"referenceManualSidebar",previous:{title:"D.10 Synchronous Task Control",permalink:"/docs/arm/AA-D/AA-D.10"},next:{title:"D.12 Other Optimizations and Determinism Rules",permalink:"/docs/arm/AA-D/AA-D.12"}},g={},A=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Erroneous Execution",id:"erroneous-execution",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],b={toc:A};function Z(e){var t,n=e,{components:d}=n,h=((e,t)=>{var n={};for(var i in e)u.call(e,i)&&t.indexOf(i)<0&&(n[i]=e[i]);if(null!=e&&c)for(var i of c(e))t.indexOf(i)<0&&k.call(e,i)&&(n[i]=e[i]);return n})(n,["components"]);return(0,i.kt)("wrapper",(t=y(y({},b),h),p(t,m({components:d,mdxType:"MDXLayout"}))),(0,i.kt)("h1",y({},{id:"d11-asynchronous-task-control"}),"D.11 Asynchronous Task Control"),(0,i.kt)("admonition",y({},{type:"warning"}),(0,i.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.kt)("a",y({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,i.kt)(s.Z,{mdxType:"MarginText"},"1/3"),(0,i.kt)(l.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"[This subclause introduces a language-defined package to do asynchronous suspend/resume on tasks. It uses a conceptual ",(0,i.kt)("em",null,"held priority")," value to represent the task's ",(0,i.kt)("em",null,"held")," state.] ",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"static-semantics"}),"Static Semantics"),(0,i.kt)(s.Z,{mdxType:"MarginText"},"2"),(0,i.kt)("p",null,"The following language-defined library package exists: ",(0,i.kt)("br",null)),(0,i.kt)(s.Z,{mdxType:"MarginText"},"3/5"),(0,i.kt)(l.Z,{items:["AI95-00362-01","AI12-0241-1","AI12-0302-1"],mdxType:"MarginInfo"}),(0,i.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"with Ada.Task_Identification;","\n","package Ada.Asynchronous_Task_Control ","\n","  with  Preelaborate, Nonblocking, Global =",">"," in out synchronized is ","\n","  procedure Hold(T : in Ada.Task_Identification.Task_Id);","\n","  procedure Continue(T : in Ada.Task_Identification.Task_Id);","\n","  function Is_Held(T : Ada.Task_Identification.Task_Id)","\n","   return Boolean;","\n","end Ada.Asynchronous_Task_Control;","\n"),(0,i.kt)("h4",y({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,i.kt)(s.Z,{mdxType:"MarginText"},"4/2"),(0,i.kt)(l.Z,{items:["AI95-00357-01"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"After the Hold operation has been applied to a task, the task becomes ",(0,i.kt)("em",null,"held"),". For each processor there is a conceptual ",(0,i.kt)("em",null,"idle task"),", which is always ready. The base priority of the idle task is below System.Any_Priority'First. The ",(0,i.kt)("em",null,"held priority")," is a constant of the type Integer whose value is below the base priority of the idle task. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"4.a"),(0,i.kt)(a.Z,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.kt)("strong",null),"The held state should not be confused with the blocked state as defined in ",(0,i.kt)("a",{href:"../AA-9/AA-9.2"},"9.2"),"; the task is still ready. ",(0,i.kt)("br",null))),(0,i.kt)(s.Z,{mdxType:"MarginText"},"4.1/2"),(0,i.kt)(l.Z,{items:["AI95-00357-01"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"For any priority below System.Any_Priority'First, the task dispatching policy is FIFO_Within_Priorities. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"4.b/2"),(0,i.kt)(a.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,i.kt)("strong",null,"To be honest: "),"This applies even if a Task_Dispatching_Policy specifies the policy for all of the priorities of the partition. ",(0,i.kt)("br",null))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"4.c/2"),(0,i.kt)(a.Z,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,i.kt)("strong",null),"A task at the held priority never runs, so it is not necessary to implement FIFO_Within_Priorities for systems that have only one policy (such as EDF_Across_Priorities). ",(0,i.kt)("br",null))),(0,i.kt)(s.Z,{mdxType:"MarginText"},"5/2"),(0,i.kt)(l.Z,{items:["AI95-00357-01"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"The Hold operation sets the state of T to held. For a held task, the active priority is reevaluated as if the base priority of the task were the held priority. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"5.a"),(0,i.kt)(a.Z,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,i.kt)("strong",null),"For example, if T is currently inheriting priorities from other sources (e.g. it is executing in a protected action), its active priority does not change, and it continues to execute until it leaves the protected action. ",(0,i.kt)("br",null))),(0,i.kt)(s.Z,{mdxType:"MarginText"},"6/2"),(0,i.kt)(l.Z,{items:["AI95-00357-01"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"The Continue operation resets the state of T to not-held; its active priority is then reevaluated as determined by the task dispatching policy associated with its base priority.",(0,i.kt)("br",null)),(0,i.kt)(s.Z,{mdxType:"MarginText"},"7"),(0,i.kt)("p",null,"The Is_Held function returns True if and only if T is in the held state. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"7.a"),(0,i.kt)(a.Z,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,i.kt)("strong",null),"Note that the state of T can be changed immediately after Is_Held returns. ",(0,i.kt)("br",null))),(0,i.kt)(s.Z,{mdxType:"MarginText"},"8"),(0,i.kt)("p",null,"As part of these operations, a check is made that the task identified by T is not terminated. Tasking_Error is raised if the check fails. Program_Error is raised if the value of T is Null_Task_Id.",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"erroneous-execution"}),"Erroneous Execution"),(0,i.kt)(s.Z,{mdxType:"MarginText"},"9"),(0,i.kt)("p",null,"If any operation in this package is called with a parameter T that specifies a task object that no longer exists, the execution of the program is erroneous. ",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,i.kt)(s.Z,{mdxType:"MarginText"},"10/5"),(0,i.kt)(l.Z,{items:["AI12-0444-1"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"An implementation may omit  support for Asynchronous_Task_Control if it is infeasible to support it in the target environment. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"10.a"),(0,i.kt)(a.Z,{type:"aarm",aarm:"reason",mdxType:"Admonition"},(0,i.kt)("strong",null),"A direct implementation of the Asynchronous_Task_Control semantics using priorities is not necessarily efficient enough. Thus, we envision implementations that use some other mechanism to set the \u201cheld\u201d state. If there is no other such mechanism, support for Asynchronous_Task_Control might be infeasible, because an implementation in terms of priority would require one idle task per processor. On some systems, programs are not supposed to know how many processors are available, so creating enough idle tasks would be problematic. ",(0,i.kt)("br",null))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"11"),(0,i.kt)(a.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 1   It is a consequence of the priority rules that held tasks cannot be dispatched on any processor in a partition (unless they are inheriting priorities) since their priorities are defined to be below the priority of any idle task.",(0,i.kt)("br",null))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"12"),(0,i.kt)(a.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 2   The effect of calling Get_Priority and Set_Priority on a Held task is the same as on any other task.",(0,i.kt)("br",null))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"13"),(0,i.kt)(a.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 3   Calling Hold on a held task or Continue on a non-held task has no effect.",(0,i.kt)("br",null))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"14"),(0,i.kt)(a.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 4   The rules affecting queuing are derived from the above rules, in addition to the normal priority rules: ",(0,i.kt)("br",null))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"15"),(0,i.kt)("ul",null,(0,i.kt)("li",null,"When a held task is on the ready queue, its priority is so low as to never reach the top of the queue as long as there are other tasks on that queue.",(0,i.kt)("br",null)),(0,i.kt)(s.Z,{mdxType:"MarginText"},"16"),(0,i.kt)("li",null,"If a task is executing in a protected action, inside a rendezvous, or is inheriting priorities from other sources (e.g. when activated), it continues to execute until it is no longer executing the corresponding construct.",(0,i.kt)("br",null)),(0,i.kt)(s.Z,{mdxType:"MarginText"},"17"),(0,i.kt)("li",null,"If a task becomes held while waiting (as a caller) for a rendezvous to complete, the active priority of the accepting task is not affected.",(0,i.kt)("br",null)),(0,i.kt)(s.Z,{mdxType:"MarginText"},"18/1"),(0,i.kt)(l.Z,{items:["AI95-00111-01"],mdxType:"MarginInfo"}),(0,i.kt)("li",null,"{",(0,i.kt)("em",null,"8652/0077"),"}"," If a task becomes held while waiting in a ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"../AA-9/AA-9.7#S0270"},"selective_accept")),", and an entry call is issued to one of the open entries, the corresponding ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"../AA-9/AA-9.7#S0273"},"accept_alternative"))," executes. When the rendezvous completes, the active priority of the accepting task is lowered to the held priority (unless it is still inheriting from other sources), and the task does not execute until another Continue.",(0,i.kt)("br",null)),(0,i.kt)(s.Z,{mdxType:"MarginText"},"19"),(0,i.kt)("li",null,"The same holds if the held task is the only task on a protected entry queue whose barrier becomes open. The corresponding entry body executes.",(0,i.kt)("br",null)))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)("h4",y({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95")),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"19.a/2"),(0,i.kt)(l.Z,{items:["AI95-00362-01"],mdxType:"MarginInfo"}),(0,i.kt)(a.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Asynchronous_Task_Control is now Preelaborated, so it can be used in preelaborated units. ",(0,i.kt)("br",null))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)("h4",y({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95")),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"19.b/2"),(0,i.kt)(l.Z,{items:["AI95-00111-01"],mdxType:"MarginInfo"}),(0,i.kt)(a.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"{",(0,i.kt)("em",null,"8652/0077"),"}"," ",(0,i.kt)("strong",null,"Corrigendum:")," Corrected to eliminate the use of the undefined term \u201caccept body\u201d.",(0,i.kt)("br",null))),(0,i.kt)(o.Z,{mdxType:"AnnotatedOnly"},(0,i.kt)(s.Z,{mdxType:"MarginText"},"19.c/2"),(0,i.kt)(l.Z,{items:["AI95-00357-01"],mdxType:"MarginInfo"}),(0,i.kt)(a.Z,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The description of held tasks was changed to reflect that the calculation of active priorities depends on the dispatching policy of the base priority. Thus, the policy of the held priority was specified in order to avoid surprises (especially when using the EDF policy). ",(0,i.kt)("br",null))))}Z.isMDXComponent=!0}}]);