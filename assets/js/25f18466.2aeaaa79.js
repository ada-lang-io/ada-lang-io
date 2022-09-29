"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[8347],{5259:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>T,contentTitle:()=>f,default:()=>_,frontMatter:()=>g,metadata:()=>y,toc:()=>x});var n=i(1716),a=i(3050),s=i(8604),o=i(7318),r=i(4768),l=Object.defineProperty,d=Object.defineProperties,m=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,h=Object.prototype.propertyIsEnumerable,u=(e,t,i)=>t in e?l(e,t,{enumerable:!0,configurable:!0,writable:!0,value:i}):e[t]=i,k=(e,t)=>{for(var i in t||(t={}))c.call(t,i)&&u(e,i,t[i]);if(p)for(var i of p(t))h.call(t,i)&&u(e,i,t[i]);return e};const g={sidebar_position:170},f="D.16 Multiprocessor Implementation",y={unversionedId:"arm/AA-D/AA-D.16",id:"arm/AA-D/AA-D.16",title:"D.16 Multiprocessor Implementation",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-D/AA-D.16.mdx",sourceDirName:"arm/AA-D",slug:"/arm/AA-D/AA-D.16",permalink:"/docs/arm/AA-D/AA-D.16",draft:!1,tags:[],version:"current",sidebarPosition:170,frontMatter:{sidebar_position:170},sidebar:"referenceManualSidebar",previous:{title:"D.15 Timing Events",permalink:"/docs/arm/AA-D/AA-D.15"},next:{title:"Annex E Distributed Systems",permalink:"/docs/arm/AA-E/"}},T={},x=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"Extensions to Ada 2012",id:"extensions-to-ada-2012",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4},{value:"D.16.1  Multiprocessor Dispatching Domains",id:"d161--multiprocessor-dispatching-domains",level:2},{value:"Static Semantics",id:"static-semantics-1",level:4},{value:"Legality Rules",id:"legality-rules-1",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics-1",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Implementation Advice",id:"implementation-advice-1",level:4},{value:"Documentation Requirements",id:"documentation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005-1",level:4},{value:"Inconsistencies With Ada 2012",id:"inconsistencies-with-ada-2012",level:4},{value:"Incompatibilities With Ada 2012",id:"incompatibilities-with-ada-2012",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012-1",level:4}],A={toc:x};function _(e){var t,i=e,{components:l}=i,u=((e,t)=>{var i={};for(var n in e)c.call(e,n)&&t.indexOf(n)<0&&(i[n]=e[n]);if(null!=e&&p)for(var n of p(e))t.indexOf(n)<0&&h.call(e,n)&&(i[n]=e[n]);return i})(i,["components"]);return(0,n.kt)("wrapper",(t=k(k({},A),u),d(t,m({components:l,mdxType:"MDXLayout"}))),(0,n.kt)("h1",k({},{id:"d16-multiprocessor-implementation"}),"D.16 Multiprocessor Implementation"),(0,n.kt)("admonition",k({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,n.kt)("a",k({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1/3"),(0,n.kt)(r.Z,{items:["AI05-0171-1","AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"This subclause allows implementations on multiprocessor platforms to be configured. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"static-semantics"}),"Static Semantics"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"2/3"),(0,n.kt)(r.Z,{items:["AI05-0171-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The following language-defined library package exists: ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"3/5"),(0,n.kt)(r.Z,{items:["AI12-0241-1","AI12-0302-1"],mdxType:"MarginInfo"}),(0,n.kt)(s.Z,{language:"ada",mdxType:"CodeBlock"},"package System.Multiprocessors ","\n","   with  Preelaborate, Nonblocking, Global =",">"," in out synchronized is ","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"4/3"),"type CPU_Range is range 0 .. implementation-defined;","\n","   Not_A_Specific_CPU : constant CPU_Range := 0;","\n","   subtype CPU is CPU_Range range 1 .. CPU_Range'Last;","\n"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"4.a.1/3"),(0,n.kt)(a.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"The value of CPU_Range'Last in System.Multiprocessors.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"5/3"),(0,n.kt)(s.Z,{language:"ada",mdxType:"CodeBlock"},"function Number_Of_CPUs return CPU;","\n","end System.Multiprocessors;","\n"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"6/3"),(0,n.kt)(r.Z,{items:["AI05-0171-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"A call of Number_Of_CPUs returns the number of processors available to the program. Within a given partition, each call on Number_Of_CPUs will return the same value.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"7/5"),(0,n.kt)(r.Z,{items:["AI05-0229-1","AI12-0281-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"For a task type (including the anonymous type of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.1#S0245"},"single_task_declaration")),"), protected type (including the anonymous type of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.4#S0250"},"single_protected_declaration")),"), or subprogram, the following language-defined representation aspect may be specified:",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"8/3"),(0,n.kt)("dt",null,(0,n.kt)("br",null),"CPU"),(0,n.kt)("dl",null,(0,n.kt)("dd",null,"The aspect CPU is an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),", which shall be of type System.Multiprocessors.CPU_Range.",(0,n.kt)("br",null))),(0,n.kt)(o.Z,{mdxType:"MarginText"},"8.a/5"),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Aspect Description for "),(0,n.kt)("strong",null,"CPU: "),"Processor on which a given task, or calling task for a protected operation, should run.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"legality-rules"}),"Legality Rules"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"9/3"),(0,n.kt)(r.Z,{items:["AI05-0171-1","AI05-0229-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"If the CPU aspect is specified for a subprogram, the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," shall be static.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"10/5"),(0,n.kt)(r.Z,{items:["AI05-0229-1","AI12-0281-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The CPU aspect shall not be specified on a task or protected interface type.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"11/5"),(0,n.kt)(r.Z,{items:["AI05-0171-1","AI05-0229-1","AI12-0081-1","AI12-0281-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression"))," specified for the CPU aspect of a task or protected type is evaluated each time an object of the corresponding  type is created (see ",(0,n.kt)("a",{href:"../AA-9/AA-9.1"},"9.1")," and ",(0,n.kt)("a",{href:"../AA-9/AA-9.4"},"9.4"),"). The CPU value is then associated with the  object.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"12/3"),(0,n.kt)(r.Z,{items:["AI05-0171-1","AI05-0229-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The CPU aspect has no effect if it is specified for a subprogram other than the main subprogram; the CPU value is not associated with any task.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"13/3"),(0,n.kt)(r.Z,{items:["AI05-0171-1","AI05-0229-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The CPU value is associated with the environment task if the CPU aspect is specified for the main subprogram. If the CPU aspect is not specified for the main subprogram it is implementation defined on which processor the environment task executes. ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"13.a.1/3"),(0,n.kt)(a.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"The processor on which the environment task executes in the absence of a value for the aspect CPU.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"14/5"),(0,n.kt)(r.Z,{items:["AI05-0171-1","AI05-0264-1","AI12-0281-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"For a task, the  CPU value determines the processor on which the task will activate and execute; the task is said to be assigned to that processor. If the CPU value is Not_A_Specific_CPU, then the task is not assigned to a processor. A task without a CPU aspect specified will activate and execute on the same processor as its activating task if the activating task is assigned a processor. If the CPU value is not in the range of System.Multiprocessors.CPU_Range or is greater than Number_Of_CPUs the task is defined to have failed, and it becomes a completed task (see ",(0,n.kt)("a",{href:"../AA-9/AA-9.2"},"9.2"),").",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"15/5"),(0,n.kt)(r.Z,{items:["AI12-0281-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"For a protected type, the CPU value determines the processor on which calling tasks will execute; the protected object is said to be assigned to that processor. If the CPU value is Not_A_Specific_CPU, then the protected object is not assigned to a processor. A call to a protected object that is assigned to a processor from a task that is not assigned a processor or is assigned a different processor raises Program_Error.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"15.a/5"),(0,n.kt)(r.Z,{items:["AI12-0005-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"When a protected object is assigned to a CPU, only tasks also assigned to that CPU can call it. In contrast, a protected object that is not assigned to a specific CPU can be called by any task on any processor (subject, of course, to visibility and ceiling priority restrictions). As noted below, when the tasks and protected object are necessarily on the same CPU, a simpler implementation can be used. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"implementation-advice"}),"Implementation Advice"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"16/5"),(0,n.kt)(r.Z,{items:["AI12-0281-1","AI12-0323-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"Starting a protected action on a protected object statically assigned to a processor should be implemented without busy-waiting.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"16.a/5"),(0,n.kt)(a.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"Busy-waiting is a form of lock and can be a source of deadlock. Busy-waiting is typically needed for starting protected actions on multiprocessors, but if all tasks calling a protected object execute on the same CPU, this locking isn't needed and the source of deadlock and associated overhead can be eliminated. ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"16.b/5"),(0,n.kt)(a.Z,{type:"aarm",aarm:"implementation-advice",title:"Implementation Advice",mdxType:"Admonition"},(0,n.kt)("strong",null),"Starting a protected action on a protected object statically assigned to a processor should not use busy-waiting.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"extensions-to-ada-2005"}),"Extensions to Ada 2005"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"16.c/3"),(0,n.kt)(r.Z,{items:["AI05-0171-1","AI05-0229-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The package System.Multiprocessors and the CPU aspect are new. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"extensions-to-ada-2012"}),"Extensions to Ada 2012"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"16.d/5"),(0,n.kt)(r.Z,{items:["AI12-0281-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Aspect CPU can now be applied to protected types, in order to avoid the overhead and deadlock potential of multiprocessor execution. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"16.e/4"),(0,n.kt)(r.Z,{items:["AI12-0081-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Corrigendum:")," Clarified when the CPU aspect expression is evaluated. ",(0,n.kt)("br",null)),(0,n.kt)("a",{id:"Subclause_D.16.1"}),(0,n.kt)("h2",k({},{id:"d161--multiprocessor-dispatching-domains"}),"D.16.1  Multiprocessor Dispatching Domains"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"1/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI05-0299-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"This subclause allows implementations on multiprocessor platforms to be partitioned into distinct dispatching domains during program startup.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"static-semantics-1"}),"Static Semantics"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"2/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The following language-defined library package exists: ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"3/5"),(0,n.kt)(r.Z,{items:["AI12-0241-1","AI12-0302-1"],mdxType:"MarginInfo"}),(0,n.kt)(s.Z,{language:"ada",mdxType:"CodeBlock"},"with Ada.Real_Time;","\n","with Ada.Task_Identification;","\n","package System.Multiprocessors.Dispatching_Domains","\n","   with Nonblocking, Global =",">"," in out synchronized is","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"4/3"),"Dispatching_Domain_Error : exception;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"5/3"),"type Dispatching_Domain (","<",">",") is limited private;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"6/3"),"System_Dispatching_Domain : constant Dispatching_Domain;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"7/4"),(0,n.kt)(r.Z,{items:["AI12-0033-1"],mdxType:"MarginInfo"}),"function Create (First : CPU; Last : CPU_Range) return Dispatching_Domain;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"8/3"),"function Get_First_CPU (Domain : Dispatching_Domain) return CPU;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"9/4"),(0,n.kt)(r.Z,{items:["AI12-0033-1"],mdxType:"MarginInfo"}),"function Get_Last_CPU  (Domain : Dispatching_Domain) return CPU_Range;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"9.1/4"),(0,n.kt)(r.Z,{items:["AI12-0033-1"],mdxType:"MarginInfo"}),"type CPU_Set is array(CPU range ","<",">",") of Boolean;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"9.2/4"),(0,n.kt)(r.Z,{items:["AI12-0033-1"],mdxType:"MarginInfo"}),"function Create (Set : CPU_Set) return Dispatching_Domain;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"9.3/4"),(0,n.kt)(r.Z,{items:["AI12-0033-1"],mdxType:"MarginInfo"}),"function Get_CPU_Set (Domain : Dispatching_Domain) return CPU_Set;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"10/3"),"function Get_Dispatching_Domain","\n","      (T   : Ada.Task_Identification.Task_Id :=","\n","                 Ada.Task_Identification.Current_Task)","\n","           return Dispatching_Domain;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"11/3"),"procedure Assign_Task","\n","      (Domain : in out Dispatching_Domain;","\n","       CPU    : in     CPU_Range := Not_A_Specific_CPU;","\n","       T      : in     Ada.Task_Identification.Task_Id :=","\n","                 Ada.Task_Identification.Current_Task);","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"12/3"),"procedure Set_CPU","\n","      (CPU : in CPU_Range;","\n","       T   : in Ada.Task_Identification.Task_Id :=","\n","                 Ada.Task_Identification.Current_Task);","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"13/3"),"function Get_CPU","\n","      (T   : Ada.Task_Identification.Task_Id :=","\n","                 Ada.Task_Identification.Current_Task)","\n","           return CPU_Range;","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"14/3"),"procedure Delay_Until_And_Set_CPU","\n","      (Delay_Until_Time : in Ada.Real_Time.Time; CPU : in CPU_Range);","\n",(0,n.kt)(o.Z,{mdxType:"MarginText"},"15/3"),"private","\n","   ... -- not specified by the language","\n","end System.Multiprocessors.Dispatching_Domains;","\n"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"16/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI12-0082-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"A ",(0,n.kt)("em",null,"dispatching domain")," represents a set of processors on which a task may execute. Each processor is contained within exactly one dispatching domain. An object of type Dispatching_Domain identifies a dispatching domain. System_Dispatching_Domain identifies a domain that contains the processor or processors on which the environment task executes. At program start-up all processors are contained within this domain.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"17/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"For a task type (including the anonymous type of a ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-9/AA-9.1#S0245"},"single_task_declaration")),"), the following language-defined representation aspect may be specified:",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"18/3"),(0,n.kt)("dt",null,(0,n.kt)("br",null),"Dispatching_Domain"),(0,n.kt)("dl",null,(0,n.kt)("dd",null,"The value of aspect Dispatching_Domain is an ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.4#S0132"},"expression")),", which shall be of type Dispatching_Domains.Dispatching_Domain. This aspect is the domain to which the task (or all objects of the task type) are assigned.",(0,n.kt)("br",null))),(0,n.kt)(o.Z,{mdxType:"MarginText"},"18.a/3"),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Aspect Description for "),(0,n.kt)("strong",null,"Dispatching_Domain: "),"Domain (group of processors) on which a given task should run.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"legality-rules-1"}),"Legality Rules"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"19/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The Dispatching_Domain aspect shall not be specified for a task interface.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"dynamic-semantics-1"}),"Dynamic Semantics"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"20/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI12-0033-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The expression specified for the Dispatching_Domain aspect of a task type is evaluated each time an object of the task type is created (see ",(0,n.kt)("a",{href:"../AA-9/AA-9.1"},"9.1"),"). If the identified dispatching domain is empty, then Dispatching_Domain_Error is raised; otherwise the newly created task is assigned to the domain identified by the value of the expression.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"21/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"If a task is not explicitly assigned to any domain, it is assigned to that of the activating task. A task always executes on some CPU in its domain.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"22/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI12-0082-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"If both the dispatching domain and CPU are specified for a task, and the CPU value is not contained within the set of processors for the domain (and is not Not_A_Specific_CPU), the activation of the task is defined to have failed, and it becomes a completed task (see ",(0,n.kt)("a",{href:"../AA-9/AA-9.2"},"9.2"),").",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"23/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI12-0033-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The function Create with First and Last parameters creates and returns a dispatching domain containing all the processors in the range First .. Last. The function Create with a Set parameter creates and returns a dispatching domain containing the processors for which Set(I) is True. These processors are removed from System_Dispatching_Domain. A call of Create will raise Dispatching_Domain_Error if any designated processor is not currently in System_Dispatching_Domain, or if the system cannot support a distinct domain over the processors identified, or if a processor has a task assigned to it, or if the allocation would leave System_Dispatching_Domain empty. A call of Create will raise Dispatching_Domain_Error if the calling task is not the environment task, or if Create is called after the call to the main subprogram.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"24/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI12-0033-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The function Get_First_CPU returns the first CPU in Domain, or CPU'First if Domain is empty; Get_Last_CPU returns the last CPU in Domain, or CPU_Range'First if Domain is empty. The function Get_CPU_Set(D) returns an array whose low bound is Get_First_CPU(D), whose high bound is Get_Last_CPU(D), with True values in the Set corresponding to the CPUs that are in the given Domain.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"25/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI12-0082-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The function Get_Dispatching_Domain returns the dispatching domain on which the task is assigned.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"26/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI05-0278-1","AI12-0033-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"A call of the procedure Assign_Task assigns task T to the CPU within the dispatching domain Domain. Task T can now execute only on CPU, unless CPU designates Not_A_Specific_CPU in which case it can execute on any processor within Domain. The exception Dispatching_Domain_Error is propagated if Domain is empty, T is already assigned to a dispatching domain other than System_Dispatching_Domain, or if CPU is not one of the processors of Domain (and is not Not_A_Specific_CPU). A call of Assign_Task is a task dispatching point for task T unless T is inside of a protected action, in which case the effect on task T is delayed until its next task dispatching point. If T is the Current_Task the effect is immediate if T is not inside a protected action, otherwise the effect is as soon as practical. Assigning a task already assigned to System_Dispatching_Domain to that domain has no effect.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"27/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI05-0278-1","AI12-0082-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"A call of procedure Set_CPU assigns task T to the CPU. Task T can now execute only on CPU, unless CPU designates Not_A_Specific_CPU, in which case it can execute on any processor within its dispatching domain. The exception Dispatching_Domain_Error is propagated if CPU is not one of the processors of the dispatching domain on which T is assigned (and is not Not_A_Specific_CPU). A call of Set_CPU is a task dispatching point for task T unless T is inside of a protected action, in which case the effect on task T is delayed until its next task dispatching point. If T is the Current_Task the effect is immediate if T is not inside a protected action, otherwise the effect is as soon as practical.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"28/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The function Get_CPU returns the processor assigned to task T, or Not_A_Specific_CPU if the task is not assigned to a processor.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"29/4"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI12-0082-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"A call of Delay_Until_And_Set_CPU delays the calling task for the designated time and then assigns the task to the specified processor when the delay expires. The exception Dispatching_Domain_Error is propagated if P is not one of the processors of the calling task's dispatching domain (and is not Not_A_Specific_CPU).",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"30/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The implementation shall perform the operations Assign_Task, Set_CPU, Get_CPU and Delay_Until_And_Set_CPU atomically with respect to any of these operations on the same dispatching_domain, processor or task.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"30.1/4"),(0,n.kt)(r.Z,{items:["AI12-0048-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"Any task that belongs to the system dispatching domain can execute on any CPU within that domain, unless the assignment of the task has been specified.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"30.a/4"),(0,n.kt)(a.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"This ensures that priorities and deadlines are respected within the system dispatching domain. There is no such guarantee between different domains.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"30.b/4"),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"We only need to talk about the system dispatching domain here, because Assign_Task and Set_CPU already have such wording for tasks that are assigned explicitly to a dispatching domain and specify Not_a_Specific_CPU. ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"30.c/4"),(0,n.kt)(a.Z,{type:"aarm",aarm:"ramification",title:"Ramification: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"If no dispatching domains are created, all tasks can execute on all processors. (As always, implementation-defined dispatching policies may have other rules, so a partition that does not specify any language-defined dispatching policy may do anything at all and in particular does not need to follow this rule. ",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"30.d/4"),(0,n.kt)(a.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"A task can be assigned to a specific CPU by specifying the aspect CPU for a task, or by calling a dynamic operation like Set_CPU or Assign_Task. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"implementation-advice-1"}),"Implementation Advice"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"31/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"Each dispatching domain should have separate and disjoint ready queues.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"31.a/3"),(0,n.kt)(a.Z,{type:"aarm",aarm:"implementation-advice",title:"Implementation Advice",mdxType:"Admonition"},(0,n.kt)("strong",null),"Each dispatching domain should have separate and disjoint ready queues.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"31.b/4"),(0,n.kt)(r.Z,{items:["AI12-0048-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"To be honest: "),'\u201cReady queue\u201d here doesn\'t mean the conceptual "ready queue" as defined in ',(0,n.kt)("a",{href:"../AA-D/AA-D.2#Subclause_D.2.1"},"D.2.1")," (one per processor); this rule is talking about the ready queues used by the implementation. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"documentation-requirements"}),"Documentation Requirements"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"32/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The implementation shall document the processor(s) on which the clock interrupt is handled and hence where delay queue and ready queue manipulations occur. For any Interrupt_Id whose handler can execute on more than one processor the implementation shall also document this set of processors.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"32.a/3"),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Documentation Requirement: "),"The processor(s) on which the clock interrupt is handled; the processors on which each Interrupt_Id can be handled.",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"33/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"An implementation may limit the number of dispatching domains that can be created and raise Dispatching_Domain_Error if an attempt is made to exceed this number.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"34/5"),(0,n.kt)(r.Z,{items:["AI12-0119-1"],mdxType:"MarginInfo"}),(0,n.kt)("p",null,"The implementation may defer the effect of a Set_CPU or an Assign_Task operation until the specified task leaves an ongoing parallel construct.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"34.a/5"),(0,n.kt)(r.Z,{items:["AI12-0119-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,n.kt)("strong",null),"These operations can change the set of CPUs that a parallel operation is allowed to use. This could require the ability to move or suspend one or more threads to execute them on a different CPU. However, parallel constructs are primarily intended to improve performance of code, and the overhead needed to support such a rarely used operation could be substantial. Therefore, rather than requiring support we allow the implementation to wait to implement these operations until the parallel construct (and thus the extra threads) have completed. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"extensions-to-ada-2005-1"}),"Extensions to Ada 2005"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"34.b/3"),(0,n.kt)(r.Z,{items:["AI05-0167-1","AI05-0278-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The package System.Multiprocessors.Dispatching_Domains and the aspect Dispatching_Domains are new. ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"inconsistencies-with-ada-2012"}),"Inconsistencies With Ada 2012"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"34.c/4"),(0,n.kt)(r.Z,{items:["AI12-0033-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Corrigendum:")," We now explicitly allow empty dispatching domains, as it would be difficult to avoid declaring them when a system is configured at runtime. Therefore, assigning a task to an empty domain now raises Dispatching_Domain_Error; creating such a domain should not raise Dispatching_Domain_Error. If an implementation does something different in these cases, and a program depends on that difference, the program could malfunction. This seems very unlikely (if no exception is ever raised, the task assigned to the empty domain could never run; if the exception is raised earlier, the program can't do anything useful). ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"incompatibilities-with-ada-2012"}),"Incompatibilities With Ada 2012"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"34.d/5"),(0,n.kt)(r.Z,{items:["AI05-0005-1","AI05-0033-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Corrigendum:")," The subtypes of the parameter or result of several routines were changed to support empty domains. These changes will cause rules requiring subtype conformance to fail on these routines (such as 'Access). We believe such uses are unlikely. In addition, type CPU_Set and function Get_CPU_Set, along with an overloaded Create are newly added to this package. As such, a use clause conflict is possible; see the introduction of ",(0,n.kt)("a",{href:"../AA-A/"},"Annex A")," for more on this topic.  ",(0,n.kt)("br",null)),(0,n.kt)("h4",k({},{id:"wording-changes-from-ada-2012-1"}),"Wording Changes from Ada 2012"),(0,n.kt)(o.Z,{mdxType:"MarginText"},"34.e/4"),(0,n.kt)(r.Z,{items:["AI12-0048-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Corrigendum:")," Added wording to clarify that all tasks can execute on all CPUs of the system dispatching domain by default.",(0,n.kt)("br",null)),(0,n.kt)(o.Z,{mdxType:"MarginText"},"34.f/4"),(0,n.kt)(r.Z,{items:["AI12-0082-1"],mdxType:"MarginInfo"}),(0,n.kt)(a.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,n.kt)("strong",null,"Corrigendum:"),' Added a definition to clarify that a "dispatching domain" is a concept which is identified by an object of type Dispatching_Domain; more than one object might identify the same dispatching domain (for instance, the result of function Get_Dispatching_Domain is a different object but identifies the same dispatching domain). ',(0,n.kt)("br",null)))}_.isMDXComponent=!0}}]);