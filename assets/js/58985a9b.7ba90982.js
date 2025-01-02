"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8232],{5006:(e,r,s)=>{s.r(r),s.d(r,{assets:()=>A,contentTitle:()=>o,default:()=>m,frontMatter:()=>d,metadata:()=>i,toc:()=>x});const i=JSON.parse('{"id":"arm/AA-M/AA-M.1","title":"M.1 Specific Documentation Requirements","description":"This Reference Manual output has not been verified,","source":"@site/docs/arm/AA-M/AA-M.1.mdx","sourceDirName":"arm/AA-M","slug":"/arm/AA-M/AA-M.1","permalink":"/docs/arm/AA-M/AA-M.1","draft":false,"unlisted":false,"tags":[],"version":"current","sidebarPosition":214,"frontMatter":{"sidebar_position":214},"sidebar":"referenceManualSidebar","previous":{"title":"Annex M Summary of Documentation Requirements","permalink":"/docs/arm/AA-M/"},"next":{"title":"M.2 Implementation-Defined Characteristics","permalink":"/docs/arm/AA-M/AA-M.2"}}');var n=s(4848),a=s(8453),c=s(3842),t=s(1435),l=(s(1432),s(9162)),h=s(4421);const d={sidebar_position:214},o="M.1 Specific Documentation Requirements",A={},x=[];function j(e){const r={a:"a",admonition:"admonition",h1:"h1",header:"header",p:"p",...(0,a.R)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(r.header,{children:(0,n.jsx)(r.h1,{id:"m1-specific-documentation-requirements",children:"M.1 Specific Documentation Requirements"})}),"\n",(0,n.jsx)(r.admonition,{type:"danger",children:(0,n.jsxs)(r.p,{children:["This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.jsx)(r.a,{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20",children:"tracking issue"})]})}),"\n","\n",(0,n.jsx)(l.A,{children:"1/5"}),"\n",(0,n.jsx)(h.A,{items:["AI12-0442-1"]}),"\n",(0,n.jsxs)("p",{children:["In addition to implementation-defined characteristics, each Ada implementation is required to document various properties of the implementation: ",(0,n.jsx)("br",{})]}),"\n",(0,n.jsxs)(t.A,{children:[(0,n.jsx)(l.A,{children:"1.a/2"}),(0,n.jsx)(c.A,{type:"aarm",aarm:"ramification",children:(0,n.jsxs)(r.p,{children:[(0,n.jsx)("strong",{}),"Most of the items in this list require documentation only for implementations that conform to Specialized Needs Annexes. ",(0,n.jsx)("br",{})]})})]}),"\n",(0,n.jsx)(l.A,{children:"2/2"}),"\n",(0,n.jsxs)("ul",{children:[(0,n.jsxs)("li",{children:["The behavior of implementations in implementation-defined situations shall be documented \u2014 see ",(0,n.jsx)("a",{href:"/docs/arm/AA-M/AA-M.2",children:"M.2"})," for a listing. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-1/AA-1.1#Subclause_1.1.3",children:"1.1.3"}),"(19).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"3/2"}),(0,n.jsxs)("li",{children:["The set of values that a user-defined Allocate procedure needs to accept for the Alignment parameter. How the standard storage pool is chosen, and how storage is allocated by standard storage pools. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-13/AA-13.11",children:"13.11"}),"(22).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"4/2"}),(0,n.jsxs)("li",{children:["The algorithm used for random number generation, including a description of its period. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-A/AA-A.5#Subclause_A.5.2",children:"A.5.2"}),"(44).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"5/2"}),(0,n.jsxs)("li",{children:["The minimum time interval between calls to the time-dependent Reset procedure that is guaranteed to initiate different random number sequences. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-A/AA-A.5#Subclause_A.5.2",children:"A.5.2"}),"(45).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"6/2"}),(0,n.jsxs)("li",{children:["The conditions under which Io","_","Exceptions.Name","_","Error, Io","_","Exceptions.Use","_","Error, and Io","_","Exceptions.Device","_","Error are propagated. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-A/AA-A.13",children:"A.13"}),"(15).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"7/2"}),(0,n.jsxs)("li",{children:["The behavior of package Environment","_","Variables when environment variables are changed by external mechanisms. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-A/AA-A.17",children:"A.17"}),"(30/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"8/2"}),(0,n.jsxs)("li",{children:["The overhead of calling machine-code or intrinsic subprograms. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.1",children:"C.1"}),"(6).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"9/2"}),(0,n.jsxs)("li",{children:["The types and attributes used in machine code insertions. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.1",children:"C.1"}),"(7).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"10/2"}),(0,n.jsxs)("li",{children:["The subprogram calling conventions for all supported convention identifiers. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.1",children:"C.1"}),"(8/3).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"11/2"}),(0,n.jsxs)("li",{children:["The mapping between the Link","_","Name or Ada designator and the external link name. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.1",children:"C.1"}),"(9).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"12/2"}),(0,n.jsxs)("li",{children:["The treatment of interrupts. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.3",children:"C.3"}),"(22).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"13/2"}),(0,n.jsxs)("li",{children:["The metrics for interrupt handlers. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.3#Subclause_C.3.1",children:"C.3.1"}),"(16).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"14/3"}),(0,n.jsxs)("li",{children:["If the Ceiling","_","Locking policy is in effect, the default ceiling priority for a protected object that specifies an interrupt handler aspect. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.3#Subclause_C.3.2",children:"C.3.2"}),"(24/5).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"15/2"}),(0,n.jsxs)("li",{children:["Any circumstances when the elaboration of a preelaborated package causes code to be executed. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.4",children:"C.4"}),"(12).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"16/2"}),(0,n.jsxs)("li",{children:["Whether a partition can be restarted without reloading. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.4",children:"C.4"}),"(13).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"17/2"}),(0,n.jsxs)("li",{children:["The effect of calling Current","_","Task from an entry body or interrupt handler. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.7#Subclause_C.7.1",children:"C.7.1"}),"(19).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"18/2"}),(0,n.jsxs)("li",{children:["For package Task","_","Attributes, limits on the number and size of task attributes, and how to configure any limits. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.7#Subclause_C.7.2",children:"C.7.2"}),"(19).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"19/2"}),(0,n.jsxs)("li",{children:["The metrics for the Task","_","Attributes package. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-C/AA-C.7#Subclause_C.7.2",children:"C.7.2"}),"(27).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"20/2"}),(0,n.jsxs)("li",{children:["The details of the configuration used to generate the values of all metrics. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/",children:"D"}),"(2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"21/2"}),(0,n.jsxs)("li",{children:["The maximum priority inversion a user task can experience from the implementation. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.2#Subclause_D.2.3",children:"D.2.3"}),"(12/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"22/2"}),(0,n.jsxs)("li",{children:["The amount of time that a task can be preempted for processing on behalf of lower-priority tasks. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.2#Subclause_D.2.3",children:"D.2.3"}),"(13/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"23/2"}),(0,n.jsxs)("li",{children:["The quantum values supported for round robin dispatching. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.2#Subclause_D.2.5",children:"D.2.5"}),"(16/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"24/2"}),(0,n.jsxs)("li",{children:["The accuracy of the detection of the exhaustion of the budget of a task for round robin dispatching. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.2#Subclause_D.2.5",children:"D.2.5"}),"(17/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"25/2"}),(0,n.jsxs)("li",{children:["Any conditions that cause the completion of the setting of the deadline of a task to be delayed for a multiprocessor. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.2#Subclause_D.2.6",children:"D.2.6"}),"(32/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"26/2"}),(0,n.jsxs)("li",{children:["Any conditions that cause the completion of the setting of the priority of a task to be delayed for a multiprocessor. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.5#Subclause_D.5.1",children:"D.5.1"}),"(12.1/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"27/2"}),(0,n.jsxs)("li",{children:["The metrics for Set","_","Priority. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.5#Subclause_D.5.1",children:"D.5.1"}),"(14).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"28/2"}),(0,n.jsxs)("li",{children:["The metrics for setting the priority of a protected object. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.5#Subclause_D.5.2",children:"D.5.2"}),"(10).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"29/2"}),(0,n.jsxs)("li",{children:["On a multiprocessor, any conditions that cause the completion of an aborted construct to be delayed later than what is specified for a single processor. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.6",children:"D.6"}),"(3).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"30/2"}),(0,n.jsxs)("li",{children:["The metrics for aborts. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.6",children:"D.6"}),"(8).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"31/2"}),(0,n.jsxs)("li",{children:["The values of Time","_","First, Time","_","Last, Time","_","Span","_","First, Time","_","Span","_","Last, Time","_","Span","_","Unit, and Tick for package Real","_","Time. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.8",children:"D.8"}),"(33).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"32/2"}),(0,n.jsxs)("li",{children:["The properties of the underlying time base used in package Real","_","Time. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.8",children:"D.8"}),"(34).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"33/2"}),(0,n.jsxs)("li",{children:["Any synchronization of package Real","_","Time with external time references. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.8",children:"D.8"}),"(35).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"34/5"}),(0,n.jsxs)("li",{children:["Any aspects of the external environment that can interfere with package Real","_","Time. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.8",children:"D.8"}),"(36/5).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"35/2"}),(0,n.jsxs)("li",{children:["The metrics for package Real","_","Time. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.8",children:"D.8"}),"(45).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"36/2"}),(0,n.jsxs)("li",{children:["The minimum value of the delay expression of a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.6#S0268",children:"delay_relative_statement"})})," that causes a task to actually be blocked. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.9",children:"D.9"}),"(7).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"37/2"}),(0,n.jsxs)("li",{children:["The minimum difference between the value of the delay expression of a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-9/AA-9.6#S0267",children:"delay_until_statement"})})," and the value of Real","_","Time.Clock, that causes the task to actually be blocked. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.9",children:"D.9"}),"(8).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"38/2"}),(0,n.jsxs)("li",{children:["The metrics for delay statements. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.9",children:"D.9"}),"(13).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"39/2"}),(0,n.jsxs)("li",{children:["The upper bound on the duration of interrupt blocking caused by the implementation. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.12",children:"D.12"}),"(5).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"40/2"}),(0,n.jsxs)("li",{children:["The metrics for entry-less protected objects. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.12",children:"D.12"}),"(12).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"41/2"}),(0,n.jsxs)("li",{children:["The values of CPU","_","Time","_","First, CPU","_","Time","_","Last, CPU","_","Time","_","Unit, and CPU","_","Tick of package Execution","_","Time. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.14",children:"D.14"}),"(21/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"42/3"}),(0,n.jsxs)("li",{children:["The properties of the mechanism used to implement package Execution","_","Time, including the values of the constants defined in the package. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.14",children:"D.14"}),"(22/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"43/2"}),(0,n.jsxs)("li",{children:["The metrics for execution time. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.14",children:"D.14"}),"(27).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"44/2"}),(0,n.jsxs)("li",{children:["The metrics for timing events. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.15",children:"D.15"}),"(24).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"44.1/3"}),(0,n.jsxs)("li",{children:["The processor(s) on which the clock interrupt is handled; the processors on which each Interrupt","_","Id can be handled. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-D/AA-D.16#Subclause_D.16.1",children:"D.16.1"}),"(32).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"45/2"}),(0,n.jsxs)("li",{children:["Whether the RPC-receiver is invoked from concurrent tasks, and if so, the number of such tasks. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-E/AA-E.5",children:"E.5"}),"(25).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"46/2"}),(0,n.jsxs)("li",{children:["Any techniques used to reduce cancellation errors in Numerics.Generic","_","Real","_","Arrays shall be documented. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-G/AA-G.3#Subclause_G.3.1",children:"G.3.1"}),"(86/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"47/2"}),(0,n.jsxs)("li",{children:["Any techniques used to reduce cancellation errors in Numerics.Generic","_","Complex","_","Arrays shall be documented. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-G/AA-G.3#Subclause_G.3.2",children:"G.3.2"}),"(155/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"48/2"}),(0,n.jsxs)("li",{children:["If a ",(0,n.jsx)("code",{children:(0,n.jsx)("a",{href:"/docs/arm/AA-2/AA-2.8#S0019",children:"pragma"})})," Normalize","_","Scalars applies, the implicit initial values of scalar subtypes shall be documented. Such a value should be an invalid representation when possible; any cases when is it not shall be documented. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-H/AA-H.1",children:"H.1"}),"(5/2).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"49/2"}),(0,n.jsxs)("li",{children:["The range of effects for each bounded error and each unspecified effect. If the effects of a given erroneous construct are constrained, the constraints shall be documented. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-H/AA-H.2",children:"H.2"}),"(1).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"50/2"}),(0,n.jsxs)("li",{children:["For each inspection point, a mapping between each inspectable object and the machine resources where the object's value can be obtained shall be provided. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-H/AA-H.3#Subclause_H.3.2",children:"H.3.2"}),"(8).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"51/2"}),(0,n.jsxs)("li",{children:["If a pragma Restrictions(No","_","Exceptions) is specified, the effects of all constructs where language-defined checks are still performed. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-H/AA-H.4",children:"H.4"}),"(25).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"52/2"}),(0,n.jsxs)("li",{children:["The interrupts to which a task entry may be attached. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-J/AA-J.7#Subclause_J.7.1",children:"J.7.1"}),"(12).",(0,n.jsx)("br",{})]}),(0,n.jsx)(l.A,{children:"53/2"}),(0,n.jsxs)("li",{children:["The type of entry call invoked for an interrupt entry. See ",(0,n.jsx)("a",{href:"/docs/arm/AA-J/AA-J.7#Subclause_J.7.1",children:"J.7.1"}),"(13).",(0,n.jsx)("br",{})]})]})]})}function m(e={}){const{wrapper:r}={...(0,a.R)(),...e.components};return r?(0,n.jsx)(r,{...e,children:(0,n.jsx)(j,{...e})}):j(e)}}}]);