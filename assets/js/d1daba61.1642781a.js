"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[9396],{3597:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>g,contentTitle:()=>y,default:()=>f,frontMatter:()=>T,metadata:()=>b,toc:()=>A});var a=n(1716),i=n(3050),r=n(8604),o=n(7318),l=n(4768),s=Object.defineProperty,d=Object.defineProperties,u=Object.getOwnPropertyDescriptors,m=Object.getOwnPropertySymbols,k=Object.prototype.hasOwnProperty,p=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?s(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,h=(e,t)=>{for(var n in t||(t={}))k.call(t,n)&&c(e,n,t[n]);if(m)for(var n of m(t))p.call(t,n)&&c(e,n,t[n]);return e};const T={sidebar_position:161},y="D.10 Synchronous Task Control",b={unversionedId:"arm/AA-D/AA-D.10",id:"arm/AA-D/AA-D.10",title:"D.10 Synchronous Task Control",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-D/AA-D.10.mdx",sourceDirName:"arm/AA-D",slug:"/arm/AA-D/AA-D.10",permalink:"/docs/arm/AA-D/AA-D.10",draft:!1,tags:[],version:"current",sidebarPosition:161,frontMatter:{sidebar_position:161},sidebar:"referenceManualSidebar",previous:{title:"D.9 Delay Accuracy",permalink:"/docs/arm/AA-D/AA-D.9"},next:{title:"D.11 Asynchronous Task Control",permalink:"/docs/arm/AA-D/AA-D.11"}},g={},A=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Extensions to Ada 95",id:"extensions-to-ada-95",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4},{value:"D.10.1  Synchronous Barriers",id:"d101--synchronous-barriers",level:2},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics-1",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors-1",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005-1",level:4}],x={toc:A};function f(e){var t,n=e,{components:s}=n,c=((e,t)=>{var n={};for(var a in e)k.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&m)for(var a of m(e))t.indexOf(a)<0&&p.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=h(h({},x),c),d(t,u({components:s,mdxType:"MDXLayout"}))),(0,a.kt)("h1",h({},{id:"d10-synchronous-task-control"}),"D.10 Synchronous Task Control"),(0,a.kt)("admonition",h({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,a.kt)("a",h({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1/3"),(0,a.kt)(l.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," ",(0,a.kt)("br",null),"[This subclause describes a language-defined private semaphore (suspension object), which can be used for ",(0,a.kt)("em",null,"two-stage suspend")," operations and as a simple building block for implementing higher-level queues.] ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2"),(0,a.kt)("p",null,"The following language-defined package exists: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"3/5"),(0,a.kt)(l.Z,{items:["AI95-00362-01","AI12-0241-1","AI12-0302-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"--  ","{","AI95-00362-01","}","\n"," --  ","{","AI12-0241-1","}","\n"," --  ","{","AI12-0302-1","}","\n"," package Ada.Synchronous_Task_Control ","\n","  with  Preelaborate, Nonblocking, Global =",">"," in out synchronized is ","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"4/5"),(0,a.kt)(l.Z,{items:["AI12-0241-1"],mdxType:"MarginInfo"}),"--  ","{","AI12-0241-1","}","\n","   type Suspension_Object is limited private;","\n","  procedure Set_True(S : in out Suspension_Object);","\n","  procedure Set_False(S : in out Suspension_Object);","\n","  function Current_State(S : Suspension_Object) return Boolean;","\n","  procedure Suspend_Until_True(S : in out Suspension_Object)","\n","     with Nonblocking =",">"," False;","\n","private","\n","     ... -- not specified by the language","\n","end Ada.Synchronous_Task_Control;","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5"),(0,a.kt)("p",null,"The type Suspension_Object is a by-reference type.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5.a/2"),(0,a.kt)(l.Z,{items:["AI95-00318-02"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"{",(0,a.kt)("em",null,"AI95-00318-02"),"}"," ",(0,a.kt)("br",null),"The implementation can ensure this by, for example, making the full view an explicitly limited record type.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5.1/3"),(0,a.kt)(l.Z,{items:["AI05-0168-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0168-1"),"}"," ",(0,a.kt)("br",null),"The following language-defined package exists: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5.2/5"),(0,a.kt)(l.Z,{items:["AI05-0168-1","AI12-0241-1","AI12-0302-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"--  ","{","AI05-0168-1","}","\n"," --  ","{","AI12-0241-1","}","\n"," --  ","{","AI12-0302-1","}","\n"," with Ada.Real_Time;","\n","package Ada.Synchronous_Task_Control.EDF","\n","   with Nonblocking, Global =",">"," in out synchronized is","\n","   procedure Suspend_Until_True_And_Set_Deadline","\n","      (S  : in out Suspension_Object;","\n","       TS : in     Ada.Real_Time.Time_Span)","\n","      with Nonblocking =",">"," False;","\n","end Ada.Synchronous_Task_Control.EDF;","\n"),(0,a.kt)("h4",h({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6/2"),(0,a.kt)(l.Z,{items:["AI95-00114-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00114-01"),"}"," ",(0,a.kt)("br",null),"An object of the type Suspension_Object has two visible states: True and False. Upon initialization, its value is set to False. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"6.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This object is assumed to be private to the declaring task, i.e. only that task will call Suspend_Until_True on this object, and the count of callers is at most one. Other tasks can, of course, change and query the state of this object. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"7/2"),(0,a.kt)(l.Z,{items:["AI95-00114-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00114-01"),"}"," ",(0,a.kt)("br",null),"The operations Set_True and Set_False are atomic with respect to each other and with respect to Suspend_Until_True; they set the state to True and False respectively.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"8"),(0,a.kt)("p",null,"Current_State returns the current state of the object. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"8.a"),(0,a.kt)(i.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"This state can change immediately after the operation returns. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9/2"),(0,a.kt)(l.Z,{items:["AI95-00114-01"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI95-00114-01"),"}"," ",(0,a.kt)("br",null),"The procedure Suspend_Until_True blocks the calling task until the state of the object S is True; at that point the task becomes ready and the state of the object becomes False.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"10/5"),(0,a.kt)(l.Z,{items:["AI12-0241-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0241-1"),"}","  ",(0,a.kt)("br",null),"Program_Error is raised upon calling Suspend_Until_True if another task is already waiting on that suspension object. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"10.1/5"),(0,a.kt)(l.Z,{items:["AI05-0168-1","AI05-0269-1","AI12-0241-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0168-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0269-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0241-1"),"}"," ",(0,a.kt)("br",null),"The procedure Suspend_Until_True_And_Set_Deadline blocks the calling task until the state of the object S is True; at that point the task becomes ready with a deadline of Ada.Real_Time.Clock + TS, and the state of the object becomes False. Program_Error is raised upon calling Suspend_Until_True_And_Set_Deadline if another task is already waiting on that suspension object.  ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"bounded-run-time-errors"}),"Bounded (Run-Time) Errors"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"10.2/5"),(0,a.kt)(l.Z,{items:["AI12-0171-1","AI12-0439-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0171-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0439-1"),"}"," ",(0,a.kt)("br",null),"It is a bounded error for two or more tasks to call Suspend_Until_True on the same Suspension_Object concurrently. For each task, Program_Error can  be raised, the task can  proceed without suspending, or the task can  suspend, potentially indefinitely. The state of the suspension object can  end up either True or False. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"11"),(0,a.kt)("p",null,"The implementation is required to allow the calling of Set_False and Set_True during any protected action, even one that has its ceiling priority in the Interrupt_Priority range.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12/3"),(0,a.kt)(l.Z,{items:["AI05-0168-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   ","{",(0,a.kt)("em",null,"AI05-0168-1"),"}"," ",(0,a.kt)("br",null),"More complex schemes, such as setting the deadline relative to when Set_True is called, can be programmed using a protected object. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"extensions-to-ada-95"}),"Extensions to Ada 95"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12.a/2"),(0,a.kt)(l.Z,{items:["AI95-00362-01"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI95-00362-01"),"}"," ",(0,a.kt)("br",null),"Synchronous_Task_Control is now Preelaborated, so it can be used in preelaborated units. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"extensions-to-ada-2005"}),"Extensions to Ada 2005"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12.b/3"),(0,a.kt)(l.Z,{items:["AI05-0168-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0168-1"),"}"," ",(0,a.kt)("br",null),"Child package Ada.Synchronous_Task_Control.EDF is new. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12.c/5"),(0,a.kt)(l.Z,{items:["AI12-0171-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"correction",title:"Correction:",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI12-0171-1"),"}"," ",(0,a.kt)("strong",null,(0,a.kt)("br",null))," Clarified that Suspend_Until_True should only be called from a single task, and what happens if that is violated. ",(0,a.kt)("br",null)),(0,a.kt)("a",{id:"Subclause_D.10.1"}),(0,a.kt)("h2",h({},{id:"d101--synchronous-barriers"}),"D.10.1  Synchronous Barriers"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"1/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1","AI05-0299-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ","{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," ",(0,a.kt)("br",null),"This subclause introduces a language-defined package to synchronously release a group of tasks after the number of blocked tasks reaches a specified count value. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"static-semantics-1"}),"Static Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"2/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"The following language-defined library package exists: ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"3/5"),(0,a.kt)(l.Z,{items:["AI12-0241-1","AI12-0302-1"],mdxType:"MarginInfo"}),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"--  ","{","AI12-0241-1","}","\n"," --  ","{","AI12-0302-1","}","\n"," package Ada.Synchronous_Barriers ","\n","   with  Preelaborate, Nonblocking, Global =",">"," in out synchronized is ","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"4/3"),"subtype Barrier_Limit is Positive range 1 .. implementation-defined;","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"4.a.1/3"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"The value of Barrier_Limit'Last in Synchronous_Barriers.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"5/3"),(0,a.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"type Synchronous_Barrier (Release_Threshold : Barrier_Limit) is limited private;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"6/5"),(0,a.kt)(l.Z,{items:["AI12-0241-1"],mdxType:"MarginInfo"}),"--  ","{","AI12-0241-1","}","\n","    procedure Wait_For_Release (The_Barrier : in out Synchronous_Barrier;","\n","                               Notified    :    out Boolean)","\n","      with Nonblocking =",">"," False;","\n",(0,a.kt)(o.Z,{mdxType:"MarginText"},"7/3"),"private","\n","   -- not specified by the language","\n","end Ada.Synchronous_Barriers;","\n"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"8/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"Type Synchronous_Barrier needs finalization (see ",(0,a.kt)("a",{href:"../AA-7/AA-7.6"},"7.6"),"). ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"dynamic-semantics-1"}),"Dynamic Semantics"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"9/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"Each call to Wait_For_Release blocks the calling task until the number of blocked tasks associated with the Synchronous_Barrier object is equal to Release_Threshold, at which time all blocked tasks are released. Notified is set to True for one of the released tasks, and set to False for all other released tasks.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"10/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"The mechanism for determining which task sets Notified to True is implementation defined.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"11/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"Once all tasks have been released, a Synchronous_Barrier object may be reused to block another Release_Threshold number of tasks.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"12/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"As the first step of the finalization of a Synchronous_Barrier, each blocked task is unblocked and Program_Error is raised at the place of the call to Wait_For_Release.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"13/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"It is implementation defined whether an abnormal task which is waiting on a Synchronous_Barrier object is aborted immediately or aborted when the tasks waiting on the object are released. ",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"13.a.1/3"),(0,a.kt)(i.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,a.kt)("strong",null),"When an aborted task that is waiting on a Synchronous_Barrier is aborted.",(0,a.kt)("br",null)),(0,a.kt)(o.Z,{mdxType:"MarginText"},"14/5"),(0,a.kt)(l.Z,{items:["AI05-0174-1","AI12-0241-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,(0,a.kt)("em",null,"This paragraph was deleted."),"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0241-1"),"}","  ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"bounded-run-time-errors-1"}),"Bounded (Run-Time) Errors"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"15/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"It is a bounded error to call Wait_For_Release on a Synchronous_Barrier object after that object is finalized. If the error is detected, Program_Error is raised. Otherwise, the call proceeds normally, which may leave a task blocked forever. ",(0,a.kt)("br",null)),(0,a.kt)("h4",h({},{id:"extensions-to-ada-2005-1"}),"Extensions to Ada 2005"),(0,a.kt)(o.Z,{mdxType:"MarginText"},"15.a/3"),(0,a.kt)(l.Z,{items:["AI05-0174-1"],mdxType:"MarginInfo"}),(0,a.kt)(i.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,a.kt)("em",null,"AI05-0174-1"),"}"," ",(0,a.kt)("br",null),"The package Ada.Synchronous_Barriers is new. ",(0,a.kt)("br",null)))}f.isMDXComponent=!0}}]);