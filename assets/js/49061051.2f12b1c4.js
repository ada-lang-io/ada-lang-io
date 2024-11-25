"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[1793],{13447:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>A,contentTitle:()=>x,default:()=>f,frontMatter:()=>l,metadata:()=>r,toc:()=>m});const r=JSON.parse('{"id":"arm/AA-9/AA-9.8","title":"9.8 Abort of a Task - Abort of a Sequence of Statements","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-9/AA-9.8.mdx","sourceDirName":"arm/AA-9","slug":"/arm/AA-9/AA-9.8","permalink":"/docs/arm/AA-9/AA-9.8","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":81,"frontMatter":{"sidebar_position":81},"sidebar":"referenceManualSidebar","previous":{"title":"9.7 Select Statements","permalink":"/docs/arm/AA-9/AA-9.7"},"next":{"title":"9.9 Task and Entry Attributes","permalink":"/docs/arm/AA-9/AA-9.9"}}');var s=n(74848),a=n(28453),o=n(13842),i=n(91435),c=n(21432),d=n(79162),h=n(34421);const l={sidebar_position:81},x="9.8 Abort of a Task - Abort of a Sequence of Statements",A={},m=[{value:"Syntax",id:"syntax",level:4},{value:"Name Resolution Rules",id:"name-resolution-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Bounded (Run-Time) Errors",id:"bounded-run-time-errors",level:4},{value:"Erroneous Execution",id:"erroneous-execution",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}];function j(e){const t={a:"a",admonition:"admonition",h1:"h1",h4:"h4",header:"header",p:"p",...(0,a.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(t.header,{children:(0,s.jsx)(t.h1,{id:"98-abort-of-a-task---abort-of-a-sequence-of-statements",children:"9.8 Abort of a Task - Abort of a Sequence of Statements"})}),"\n",(0,s.jsx)(t.admonition,{type:"danger",children:(0,s.jsxs)(t.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,s.jsx)(t.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,s.jsx)(d.A,{children:"1"}),"\n",(0,s.jsxs)("p",{children:["[An ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})})," causes one or more tasks to become abnormal, thus preventing any further interaction with such tasks. The completion of the ",(0,s.jsx)("code",{children:(0,s.jsxs)("a",{href:"/docs/arm/AA-9/AA-9.7#S0282",children:["triggering","_","statement"]})})," of an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0280",children:"asynchronous_select"})})," causes a ",(0,s.jsx)("code",{children:(0,s.jsxs)("a",{href:"/docs/arm/AA-5/AA-5.1#S0166",children:["sequence","_","of","_","statements"]})})," to be aborted.] ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(t.h4,{id:"syntax",children:"Syntax"}),"\n",(0,s.jsx)(d.A,{children:"2"}),"\n",(0,s.jsx)(c.A,{children:(0,s.jsxs)(t.p,{children:[(0,s.jsxs)("code",{children:["abort","_","statement"]}),(0,s.jsx)("a",{id:"S0284"}),(0,s.jsx)("code",{children:" ::= "}),(0,s.jsx)("strong",{children:"abort"})," ",(0,s.jsxs)("em",{children:["task","_"]}),(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," ","{",", ",(0,s.jsxs)("em",{children:["task","_"]}),(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),"}",";",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsx)(t.h4,{id:"name-resolution-rules",children:"Name Resolution Rules"}),"\n",(0,s.jsx)(d.A,{children:"3/5"}),"\n",(0,s.jsx)(h.A,{items:["AI12-0444-1"]}),"\n",(0,s.jsxs)("p",{children:["Each ",(0,s.jsxs)("em",{children:["task","_"]}),(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})})," is expected to be of any task type[; each can be of a different task type.]",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(t.h4,{id:"dynamic-semantics",children:"Dynamic Semantics"}),"\n",(0,s.jsx)(d.A,{children:"4"}),"\n",(0,s.jsxs)("p",{children:["For the execution of an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})}),", the given ",(0,s.jsxs)("em",{children:["task","_"]}),(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-4/AA-4.1#S0091",children:"name"})}),"s are evaluated in an arbitrary order. Each named task is then ",(0,s.jsx)("em",{children:"aborted"}),", which consists of making the task ",(0,s.jsx)("em",{children:"abnormal"})," and aborting the execution of the corresponding ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.1#S0248",children:"task_body"})}),", unless it is already completed. ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"4.a/2"}),(0,s.jsx)(h.A,{items:["AI95-00114-01"]}),(0,s.jsx)(o.A,{type:"aarm",aarm:"ramification",children:(0,s.jsxs)(t.p,{children:[(0,s.jsx)("strong",{}),"Note that aborting those tasks is not defined to be an abort-deferred operation. Therefore, if one of the named tasks is the task executing the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})}),", or if the task executing the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})})," depends on one of the named tasks, then it is possible for the execution of the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})})," to be aborted, thus leaving some of the tasks unaborted. This allows the implementation to use either a sequence of calls to an \u201cabort task\u201d run-time system primitive, or a single call to an \u201cabort list of tasks\u201d run-time system primitive. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(d.A,{children:"5"}),"\n",(0,s.jsxs)("p",{children:["When the execution of a construct is ",(0,s.jsx)("em",{children:"aborted"})," (including that of a ",(0,s.jsx)("code",{children:(0,s.jsxs)("a",{href:"/docs/arm/AA-9/AA-9.1#S0248",children:["task","_","body"]})})," or of a ",(0,s.jsx)("code",{children:(0,s.jsxs)("a",{href:"/docs/arm/AA-5/AA-5.1#S0166",children:["sequence","_","of","_","statements"]})}),"), the execution of every construct included within the aborted execution is also aborted, except for executions included within the execution of an ",(0,s.jsx)("em",{children:"abort-deferred"})," operation; the execution of an abort-deferred operation continues to completion without being affected by the abort; the following are the abort-deferred operations: ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(d.A,{children:"6"}),"\n",(0,s.jsxs)("ul",{children:[(0,s.jsxs)("li",{children:["a protected action;",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"7"}),(0,s.jsxs)("li",{children:["waiting for an entry call to complete (after having initiated the attempt to cancel it \u2014 see below);",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"8"}),(0,s.jsxs)("li",{children:["waiting for the termination of dependent tasks;",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"9"}),(0,s.jsxs)("li",{children:["the execution of an Initialize procedure as the last step of the default initialization of a controlled object;",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"10"}),(0,s.jsxs)("li",{children:["the execution of a Finalize procedure as part of the finalization of a controlled object;",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"11"}),(0,s.jsxs)("li",{children:["an assignment operation to an object with a controlled part. ",(0,s.jsx)("br",{})]})]}),"\n",(0,s.jsx)(d.A,{children:"12"}),"\n",(0,s.jsxs)("p",{children:["[The last three of these are discussed further in ",(0,s.jsx)("a",{href:"/docs/arm/AA-7/AA-7.6",children:"7.6"}),".] ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"12.a"}),(0,s.jsx)(o.A,{type:"aarm",aarm:"reason",children:(0,s.jsxs)(t.p,{children:[(0,s.jsx)("strong",{}),"Deferring abort during Initialize and finalization allows, for example, the result of an allocator performed in an Initialize operation to be assigned into an access object without being interrupted in the middle, which would cause storage leaks. For an object with several controlled parts, each individual Initialize is abort-deferred. Note that there is generally no semantic difference between making each Finalize abort-deferred, versus making a group of them abort-deferred, because if the task gets aborted, the first thing it will do is complete any remaining finalizations. Individual objects are finalized prior to an assignment operation (if nonlimited controlled) and as part of Unchecked","_","Deallocation. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"12.b"}),(0,s.jsx)(o.A,{type:"aarm",aarm:"ramification",children:(0,s.jsxs)(t.p,{children:[(0,s.jsx)("strong",{}),'Abort is deferred during the entire assignment operation to an object with a controlled part, even if only some subcomponents are controlled. Note that this says "assignment operation", not "',(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-5/AA-5.2#S0173",children:"assignment_statement"})}),'". Explicit calls to Initialize, Finalize, or Adjust are not abort-deferred. ',(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(d.A,{children:"13"}),"\n",(0,s.jsxs)("p",{children:["When a master is aborted, all tasks that depend on that master are aborted.",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(d.A,{children:"14"}),"\n",(0,s.jsxs)("p",{children:["The order in which tasks become abnormal as the result of an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})})," or the abort of a ",(0,s.jsx)("code",{children:(0,s.jsxs)("a",{href:"/docs/arm/AA-5/AA-5.1#S0166",children:["sequence","_","of","_","statements"]})})," is not specified by the language.",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(d.A,{children:"15"}),"\n",(0,s.jsxs)("p",{children:["If the execution of an entry call is aborted, an immediate attempt is made to cancel the entry call (see ",(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#Subclause_9.5.3",children:"9.5.3"}),"). If the execution of a construct is aborted at a time when the execution is blocked, other than for an entry call, at a point that is outside the execution of an abort-deferred operation, then the execution of the construct completes immediately. For an abort due to an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})}),", these immediate effects occur before the execution of the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})})," completes. Other than for these immediate cases, the execution of a construct that is aborted does not necessarily complete before the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})})," completes. However, the execution of the aborted construct completes no later than its next ",(0,s.jsx)("em",{children:"abort completion point"})," (if any) that occurs outside of an abort-deferred operation; the following are abort completion points for an execution: ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsx)(d.A,{children:"16"}),"\n",(0,s.jsxs)("ul",{children:[(0,s.jsxs)("li",{children:["the point where the execution initiates the activation of another task;",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"17"}),(0,s.jsxs)("li",{children:["the end of the activation of a task;",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"17.1/5"}),(0,s.jsx)(h.A,{items:["AI12-0119-1"]}),(0,s.jsxs)("li",{children:["a point within a parallel construct where a new logical thread of control is created;",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"17.2/5"}),(0,s.jsx)(h.A,{items:["AI12-0119-1"]}),(0,s.jsxs)("li",{children:["the end of a parallel construct;",(0,s.jsx)("br",{})]}),(0,s.jsx)(d.A,{children:"18"}),(0,s.jsxs)("li",{children:["the start or end of the execution of an entry call, ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.5#S0258",children:"accept_statement"})}),", ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.6#S0266",children:"delay_statement"})}),", or ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})}),"; ",(0,s.jsx)("br",{})]})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"18.a"}),(0,s.jsx)(o.A,{type:"aarm",aarm:"ramification",children:(0,s.jsxs)(t.p,{children:[(0,s.jsx)("strong",{}),"Although the abort completion point doesn't occur until the end of the entry call or ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.6#S0266",children:"delay_statement"})}),", these operations might be cut short because an abort attempts to cancel them. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(d.A,{children:"19"}),"\n",(0,s.jsx)("ul",{children:(0,s.jsxs)("li",{children:["the start of the execution of a ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0269",children:"select_statement"})}),", or of the ",(0,s.jsx)("code",{children:(0,s.jsxs)("a",{href:"/docs/arm/AA-5/AA-5.1#S0166",children:["sequence","_","of","_","statements"]})})," of an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-11/AA-11.2#S0305",children:"exception_handler"})}),". ",(0,s.jsx)("br",{})]})}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"19.a"}),(0,s.jsx)(o.A,{type:"aarm",aarm:"reason",children:(0,s.jsxs)(t.p,{children:[(0,s.jsx)("strong",{}),"The start of an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-11/AA-11.2#S0305",children:"exception_handler"})})," is considered an abort completion point simply because it is easy for an implementation to check at such points. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"19.b"}),(0,s.jsx)(o.A,{type:"aarm",aarm:"implementation-note",children:(0,s.jsxs)(t.p,{children:[(0,s.jsx)("strong",{}),"Implementations may of course check for abort more often than at each abort completion point; ideally, a fully preemptive implementation of abort will be provided. If preemptive abort is not supported in a given environment, then supporting the checking for abort as part of subprogram calls and loop iterations might be a useful option. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(t.h4,{id:"bounded-run-time-errors",children:"Bounded (Run-Time) Errors"}),"\n",(0,s.jsx)(d.A,{children:"20/5"}),"\n",(0,s.jsx)(h.A,{items:["AI05-0264-1","AI12-0445-1"]}),"\n",(0,s.jsxs)("p",{children:["An attempt to execute an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0280",children:"asynchronous_select"})})," as part of the execution of an abort-deferred operation is a bounded error. Similarly, an attempt to create a task that depends on a master that is included entirely within the execution of an abort-deferred operation is a bounded error. In both cases, Program","_","Error is raised if the error is detected by the implementation; otherwise, the operations proceed as they would outside an abort-deferred operation, except that an abort of the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0283",children:"abortable_part"})})," or the created task does not necessarily have an effect. ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"20.a"}),(0,s.jsx)(o.A,{type:"aarm",aarm:"reason",children:(0,s.jsxs)(t.p,{children:[(0,s.jsx)("strong",{}),"An ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0280",children:"asynchronous_select"})})," relies on an abort of the ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0283",children:"abortable_part"})})," to effect the asynchronous transfer of control. For an ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.7#S0280",children:"asynchronous_select"})})," within an abort-deferred operation, the abort might have no effect.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"20.b"}),(0,s.jsx)(o.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(t.p,{children:["Creating a task dependent on a master included within an abort-deferred operation is considered an error, because such tasks could be aborted while the abort-deferred operation was still progressing, undermining the purpose of abort-deferral. Alternatively, we could say that such tasks are abort-deferred for their entire execution, but that seems too easy to abuse. Note that task creation is already a bounded error in protected actions, so this additional rule only applies to local task creation as part of Initialize, Finalize, or Adjust. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(t.h4,{id:"erroneous-execution",children:"Erroneous Execution"}),"\n",(0,s.jsx)(d.A,{children:"21"}),"\n",(0,s.jsxs)("p",{children:["If an assignment operation completes prematurely due to an abort, the assignment is said to be ",(0,s.jsx)("em",{children:"disrupted"}),"; the target of the assignment or its parts can become abnormal, and certain subsequent uses of the object can be erroneous, as explained in ",(0,s.jsx)("a",{href:"/docs/arm/AA-13/AA-13.9#Subclause_13.9.1",children:"13.9.1"}),". ",(0,s.jsx)("br",{})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"22/5"}),(0,s.jsx)(h.A,{items:["AI12-0442-1"]}),(0,s.jsx)(o.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(t.p,{children:["NOTE 1   An ",(0,s.jsx)("code",{children:(0,s.jsx)("a",{href:"/docs/arm/AA-9/AA-9.8#S0284",children:"abort_statement"})})," is best used only in situations requiring unconditional termination.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"23"}),(0,s.jsx)(o.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(t.p,{children:["NOTE 2   A task is allowed to abort any task it can name, including itself.",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"24/5"}),(0,s.jsx)(h.A,{items:["AI12-0449-1"]}),(0,s.jsx)(o.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(t.p,{children:["NOTE 3   Additional requirements associated with abort are given in ",(0,s.jsx)("a",{href:"/docs/arm/AA-D/AA-D.6",children:"D.6"}),". ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(i.A,{children:(0,s.jsx)(t.h4,{id:"wording-changes-from-ada-83",children:"Wording Changes from Ada 83"})}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"24.a/3"}),(0,s.jsx)(h.A,{items:["AI05-0299-1"]}),(0,s.jsx)(o.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(t.p,{children:["This subclause has been rewritten to accommodate the concept of aborting the execution of a construct, rather than just of a task. ",(0,s.jsx)("br",{})]})})]}),"\n",(0,s.jsx)(i.A,{children:(0,s.jsx)(t.h4,{id:"wording-changes-from-ada-2012",children:"Wording Changes from Ada 2012"})}),"\n",(0,s.jsxs)(i.A,{children:[(0,s.jsx)(d.A,{children:"24.b/5"}),(0,s.jsx)(h.A,{items:["AI12-0119-1"]}),(0,s.jsx)(o.A,{type:"aarm",aarm:"note",children:(0,s.jsxs)(t.p,{children:["Added points within parallel constructs to the list of abort completion points. ",(0,s.jsx)("br",{})]})})]})]})}function f(e={}){const{wrapper:t}={...(0,a.R)(),...e.components};return t?(0,s.jsx)(t,{...e,children:(0,s.jsx)(j,{...e})}):j(e)}}}]);