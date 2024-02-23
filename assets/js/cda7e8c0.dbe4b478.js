"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[7124],{79138:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>A,contentTitle:()=>c,default:()=>u,frontMatter:()=>y,metadata:()=>g,toc:()=>m});var n=a(58168),o=(a(96540),a(15680)),i=a(20793),r=a(91435),s=a(21432),l=a(79162),d=a(34421);const y={sidebar_position:75},c="9.2 Task Execution - Task Activation",g={unversionedId:"arm/AA-9/AA-9.2",id:"arm/AA-9/AA-9.2",title:"9.2 Task Execution - Task Activation",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-9/AA-9.2.mdx",sourceDirName:"arm/AA-9",slug:"/arm/AA-9/AA-9.2",permalink:"/docs/arm/AA-9/AA-9.2",draft:!1,tags:[],version:"current",sidebarPosition:75,frontMatter:{sidebar_position:75},sidebar:"referenceManualSidebar",previous:{title:"9.1 Task Units and Task Objects",permalink:"/docs/arm/AA-9/AA-9.1"},next:{title:"9.3 Task Dependence - Termination of Tasks",permalink:"/docs/arm/AA-9/AA-9.3"}},A={},m=[{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Examples",id:"examples",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4}],h={toc:m},p="wrapper";function u(e){let{components:t,...a}=e;return(0,o.yg)(p,(0,n.A)({},h,a,{components:t,mdxType:"MDXLayout"}),(0,o.yg)("h1",{id:"92-task-execution---task-activation"},"9.2 Task Execution - Task Activation"),(0,o.yg)("admonition",{type:"warning"},(0,o.yg)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,o.yg)("a",{parentName:"p",href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"},"tracking issue"))),(0,o.yg)("h4",{id:"dynamic-semantics"},"Dynamic Semantics"),(0,o.yg)(l.A,{mdxType:"MarginText"},"1"),(0,o.yg)("p",null,"The execution of a task of a given task type consists of the execution of the corresponding ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body")),". The initial part of this execution is called the ",(0,o.yg)("em",null,"activation")," of the task; it consists of the elaboration of the ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-3/AA-3.11#S0086"},"declarative_part"))," of the ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-9/AA-9.1#S0248"},"task_body")),". Should an exception be propagated by the elaboration of its ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-3/AA-3.11#S0086"},"declarative_part")),", the activation of the task is defined to have ",(0,o.yg)("em",null,"failed"),", and it becomes a completed task.",(0,o.yg)("br",null)),(0,o.yg)(l.A,{mdxType:"MarginText"},"2/2"),(0,o.yg)(d.A,{items:["AI95-00416-01"],mdxType:"MarginInfo"}),(0,o.yg)("p",null,"A task object (which represents one task) can be a part of a stand-alone object, of an object created by an ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator")),", or of an anonymous object of a limited type, or a coextension of one of these. All tasks that are part or coextensions of any of the stand-alone objects created by the elaboration of ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-3/AA-3.3#S0032"},"object","_","declaration")),"s (or ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-12/AA-12.3#S0317"},"generic_association")),"s of formal objects of mode ",(0,o.yg)("strong",null,"in"),") of a single declarative region are activated together. All tasks that are part or coextensions of a single object that is not a stand-alone object are activated together. ",(0,o.yg)("br",null)),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"2.a"),(0,o.yg)(i.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,o.yg)("strong",null),"The initialization of an ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-3/AA-3.3#S0032"},"object_declaration"))," or ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator"))," can indirectly include the creation of other objects that contain tasks. For example, the default expression for a subcomponent of an object created by an ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator"))," might call a function that evaluates a completely different ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator")),". Tasks created by the two allocators are ",(0,o.yg)("em",null,"not")," activated together. ",(0,o.yg)("br",null))),(0,o.yg)(l.A,{mdxType:"MarginText"},"3/2"),(0,o.yg)(d.A,{items:["AI95-00416-01"],mdxType:"MarginInfo"}),(0,o.yg)("p",null,"For the tasks of a given declarative region, the activations are initiated within the context of the ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-11/AA-11.2#S0304"},"handled","_","sequence","_","of","_","statements"))," (and its associated ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-11/AA-11.2#S0305"},"exception","_","handler")),"s if any \u2014 see ",(0,o.yg)("a",{href:"../AA-11/AA-11.2"},"11.2"),"), just prior to executing the statements of the ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-11/AA-11.2#S0304"},"handled_sequence_of_statements")),". [For a package without an explicit body or an explicit ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-11/AA-11.2#S0304"},"handled","_","sequence","_","of","_","statements")),", an implicit body or an implicit ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-5/AA-5.1#S0170"},"null","_","statement"))," is assumed, as defined in ",(0,o.yg)("a",{href:"../AA-7/AA-7.2"},"7.2"),".] ",(0,o.yg)("br",null)),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"3.a"),(0,o.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,o.yg)("strong",null),"If Tasking","_","Error is raised, it can be handled by handlers of the ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-11/AA-11.2#S0304"},"handled","_","sequence","_","of","_","statements")),". ",(0,o.yg)("br",null))),(0,o.yg)(l.A,{mdxType:"MarginText"},"4/2"),(0,o.yg)(d.A,{items:["AI95-00416-01"],mdxType:"MarginInfo"}),(0,o.yg)("p",null,"For tasks that are part or coextensions of a single object that is not a stand-alone object, activations are initiated after completing any initialization of the outermost object enclosing these tasks, prior to performing any other operation on the outermost object. In particular, for tasks that are part or coextensions of the object created by the evaluation of an ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator")),", the activations are initiated as the last step of evaluating the ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator")),", prior to returning the new access value. For tasks that are part or coextensions of an object that is the result of a function call, the activations are not initiated until after the function returns.",(0,o.yg)("br",null)),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"4.a/2"),(0,o.yg)(d.A,{items:["AI95-00416-01"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,o.yg)("strong",null),"The intent is that \u201ctemporary\u201d objects with task parts (or coextensions) are treated similarly to an object created by an allocator. The \u201cwhole\u201d object is initialized, and then all of the task parts (including the coextensions) are activated together. Each such \u201cwhole\u201d object has its own task activation sequence, involving the activating task being suspended until all the new tasks complete their activation. ",(0,o.yg)("br",null))),(0,o.yg)(l.A,{mdxType:"MarginText"},"5"),(0,o.yg)("p",null,"The task that created the new tasks and initiated their activations (the ",(0,o.yg)("em",null,"activator"),") is blocked until all of these activations complete (successfully or not). Once all of these activations are complete, if the activation of any of the tasks has failed [(due to the propagation of an exception)], Tasking","_","Error is raised in the activator, at the place at which it initiated the activations. Otherwise, the activator proceeds with its execution normally. Any tasks that are aborted prior to completing their activation are ignored when determining whether to raise Tasking","_","Error. ",(0,o.yg)("br",null)),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"5.a"),(0,o.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,o.yg)("strong",null),"Note that a task created by an ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator"))," does not necessarily depend on its activator; in such a case the activator's termination can precede the termination of the newly created task. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"5.b"),(0,o.yg)(i.A,{type:"aarm",aarm:"discussion",mdxType:"Admonition"},(0,o.yg)("strong",null),"Tasking","_","Error is raised only once, even if two or more of the tasks being activated fail their activation. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"5.c/2"),(0,o.yg)(d.A,{items:["AI95-00265-01"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},(0,o.yg)("strong",null,"To be honest: "),"The pragma Partition","_","Elaboration","_","Policy (see ",(0,o.yg)("a",{href:"../AA-H/AA-H.6"},"H.6"),") can be used to defer task activation to a later point, thus changing many of these rules. ",(0,o.yg)("br",null))),(0,o.yg)(l.A,{mdxType:"MarginText"},"6/3"),(0,o.yg)(d.A,{items:["AI05-0045-1"],mdxType:"MarginInfo"}),(0,o.yg)("p",null,"If the master that directly encloses the point where the activation of a task ",(0,o.yg)("em",null,"T")," would be initiated, completes before the activation of ",(0,o.yg)("em",null,"T")," is initiated, ",(0,o.yg)("em",null,"T")," becomes terminated and is never activated. Furthermore, if a return statement is left such that the return object is not returned to the caller, any task that was created as a part of the return object or one of its coextensions immediately becomes terminated and is never activated. ",(0,o.yg)("br",null)),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"6.a/3"),(0,o.yg)(d.A,{items:["AI05-0045-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"ramification",mdxType:"Admonition"},(0,o.yg)("strong",null),"The first case can only happen if the activation point of T is not reached due to an exception being raised or a task or statement being aborted. Note that this is exclusive; if the master completes normally and starts finalization, we're already past the activation point.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"6.b/3"),(0,o.yg)(d.A,{items:["AI05-0045-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"The second case can happen with an exception being raised in a return statement, by an exit or goto from an ",(0,o.yg)("code",null,(0,o.yg)("a",{href:"../AA-6/AA-6.5#S0225"},"extended_return_statement")),", or by a return statement being aborted. Any tasks created for the return object of such a return statement are never activated. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"7"),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 1   An entry of a task can be called before the task has been activated.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"8/5"),(0,o.yg)(d.A,{items:["AI12-0442-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 2   If several tasks are activated together, the execution of any of these tasks can proceed without waiting until the end of the activation of the other tasks.",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"9"),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"NOTE 3   A task can become completed during its activation either because of an exception or because it is aborted (see ",(0,o.yg)("a",{href:"../AA-9/AA-9.8"},"9.8"),").",(0,o.yg)("br",null))),(0,o.yg)("h4",{id:"examples"},"Examples"),(0,o.yg)(l.A,{mdxType:"MarginText"},"10"),(0,o.yg)("p",null,(0,o.yg)("em",null,"Example of task activation:")," ",(0,o.yg)("br",null)),(0,o.yg)(l.A,{mdxType:"MarginText"},"11"),(0,o.yg)(s.A,{language:"ada",mdxType:"CodeBlock"},"procedure P is","\n","   A, B : Server;    --  elaborate the task objects A, B","\n","   C    : Server;    --  elaborate the task object C","\n","begin","\n","   --  the tasks A, B, C are activated together before the first statement","\n","   ...","\n","end;","\n"),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)("h4",{id:"wording-changes-from-ada-83"},"Wording Changes from Ada 83")),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"11.a"),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"We have replaced the term ",(0,o.yg)("em",null,"suspended")," with ",(0,o.yg)("em",null,"blocked"),', since we didn\'t want to consider a task blocked when it was simply competing for execution resources. "Suspended" is sometimes used more generally to refer to tasks that are not actually running on some processor, due to the lack of resources.',(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"11.b/3"),(0,o.yg)(d.A,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"This subclause has been rewritten in an attempt to improve presentation. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)("h4",{id:"wording-changes-from-ada-95"},"Wording Changes from Ada 95")),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"11.c/2"),(0,o.yg)(d.A,{items:["AI95-00416-01"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"note",mdxType:"Admonition"},"Adjusted the wording for activating tasks to handle the case of anonymous function return objects. This is critical; we don't want to be waiting for the tasks in a return object when we exit the function normally. ",(0,o.yg)("br",null))),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)("h4",{id:"wording-changes-from-ada-2005"},"Wording Changes from Ada 2005")),(0,o.yg)(r.A,{mdxType:"AnnotatedOnly"},(0,o.yg)(l.A,{mdxType:"MarginText"},"11.d/3"),(0,o.yg)(d.A,{items:["AI05-0045-1"],mdxType:"MarginInfo"}),(0,o.yg)(i.A,{type:"aarm",aarm:"correction",mdxType:"Admonition"},(0,o.yg)("strong",null)," Corrected the wording that handles tasks that are never activated to ensure that no lookahead is implied and to make it clear that tasks created by return statements that never return are never activated. ",(0,o.yg)("br",null))))}u.isMDXComponent=!0}}]);