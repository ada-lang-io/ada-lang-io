"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[551],{3436:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>k,contentTitle:()=>m,default:()=>y,frontMatter:()=>h,metadata:()=>p,toc:()=>f});var a=n(1716),o=Object.defineProperty,r=Object.defineProperties,i=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,l=Object.prototype.hasOwnProperty,c=Object.prototype.propertyIsEnumerable,u=(e,t,n)=>t in e?o(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,d=(e,t)=>{for(var n in t||(t={}))l.call(t,n)&&u(e,n,t[n]);if(s)for(var n of s(t))c.call(t,n)&&u(e,n,t[n]);return e};const h={sidebar_position:71},m="9 Tasks and Synchronization",p={unversionedId:"arm/AA-9",id:"arm/AA-9",title:"9 Tasks and Synchronization",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-9.mdx",sourceDirName:"arm",slug:"/arm/AA-9",permalink:"/docs/arm/AA-9",draft:!1,tags:[],version:"current",sidebarPosition:71,frontMatter:{sidebar_position:71},sidebar:"tutorialSidebar",previous:{title:"8.6  The Context of Overload Resolution",permalink:"/docs/arm/AA-8.6"},next:{title:"9.1  Task Units and Task Objects",permalink:"/docs/arm/AA-9.1"}},k={},f=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Wording Changes from Ada 83",id:"wording-changes-from-ada-83",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4}],g={toc:f};function y(e){var t,n=e,{components:o}=n,u=((e,t)=>{var n={};for(var a in e)l.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&s)for(var a of s(e))t.indexOf(a)<0&&c.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=d(d({},g),u),r(t,i({components:o,mdxType:"MDXLayout"}))),(0,a.kt)("h1",d({},{id:"9-tasks-and-synchronization"}),"9 Tasks and Synchronization"),(0,a.kt)("admonition",d({},{type:"warning"}),(0,a.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,a.kt)("a",d({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI05-0299-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0119-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0330-1"),"}"," The execution of an Ada program consists of the execution of one or more ",(0,a.kt)("em",null,"tasks"),". Each task represents a separable activity that proceeds independently and concurrently between the points where it ",(0,a.kt)("em",null,"interacts")," with other tasks. A single task, when within the context of a parallel construct, can represent multiple ",(0,a.kt)("em",null,"logical threads of control")," which can proceed in parallel; in other contexts, each task represents one logical thread of control. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"To be honest: "),"The execution of an Ada program consists of the execution of one or more partitions (see 10.2), each of which in turn consists of the execution of an environment task and zero or more subtasks. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Glossary entry: "),"A logical thread of control is an activity within the execution of a program that can proceed in parallel with other activities of the same task, or of separate tasks."),(0,a.kt)("p",null,"Version=[5],Kind=(AddedNormal),Group=[R],Term=[logical thread of control], Def=[an activity within the execution of a program that can proceed in parallel with other activities of the same task, or of separate tasks] The various forms of task interaction are described in this clause, and include:"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0119-1"),"}"," the activation and termination of a task;"),(0,a.kt)("p",null,"a call on a protected subprogram of a ",(0,a.kt)("em",null,"protected object"),", providing exclusive read-write access, or concurrent read-only access to shared data;"),(0,a.kt)("p",null,"a call on an entry, either of another task, allowing for synchronous communication with that task, or of a protected object, allowing for asynchronous communication with one or more other tasks using that same protected object;"),(0,a.kt)("p",null,"a timed operation, including a simple delay statement, a timed entry call or accept, or a timed asynchronous select statement (see next item);"),(0,a.kt)("p",null,"an asynchronous transfer of control as part of an asynchronous select statement, where a task stops what it is doing and begins execution at a different point in response to the completion of an entry call or the expiration of a delay;"),(0,a.kt)("p",null,"an abort statement, allowing one task to cause the termination of another task. "),(0,a.kt)("p",null,"In addition, tasks can communicate indirectly by reading and updating (unprotected) shared variables, presuming the access is properly synchronized through some other kind of task interaction."),(0,a.kt)("h4",d({},{id:"static-semantics"}),"Static Semantics"),(0,a.kt)("p",null,"The properties of a task are defined by a corresponding task declaration and ",(0,a.kt)("code",null,(0,a.kt)("a",{href:"./AA-9.1#S0248"},"task_body")),", which together define a program unit called a ",(0,a.kt)("em",null,"task unit"),". "),(0,a.kt)("h4",d({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0119-1"),"}"," Over time, tasks proceed through various ",(0,a.kt)("em",null,"states"),". A task is initially ",(0,a.kt)("em",null,"inactive"),"; upon activation, and prior to its ",(0,a.kt)("em",null,"termination")," it is either ",(0,a.kt)("em",null,"blocked")," (as part of some task interaction) or ",(0,a.kt)("em",null,"ready")," to run. While ready, a task competes for the available ",(0,a.kt)("em",null,"execution resources")," that it requires to run. In the context of a parallel construct, a single task can utilize multiple processing resources simultaneously. "),(0,a.kt)("p",null,(0,a.kt)("strong",null,"Discussion: "),"{",(0,a.kt)("em",null,"AI05-0229-1"),"}"," The means for selecting which of the ready tasks to run, given the currently available execution resources, is determined by the ",(0,a.kt)("em",null,"task dispatching policy")," in effect, which is generally implementation defined, but may be controlled by aspects, pragmas, and operations defined in the Real-Time Annex (see D.2 and D.5). "),(0,a.kt)("p",null,"NOTE 1   ","{",(0,a.kt)("em",null,"AI12-0119-1"),"}"," ","{",(0,a.kt)("em",null,"AI12-0440-1"),"}"," Concurrent task execution can be implemented on multicomputers, multiprocessors, or with interleaved execution on a single physical processor. On the other hand, whenever an implementation can determine that the required semantic effects can be achieved when parts of the execution of a single logical thread of control are performed by different physical processors acting in parallel, it can choose to perform them in this way."),(0,a.kt)("h4",d({},{id:"wording-changes-from-ada-83"}),"Wording Changes from Ada 83"),(0,a.kt)("p",null,"The introduction has been rewritten."),(0,a.kt)("p",null,'We use the term "concurrent" rather than "parallel" when talking about logically independent execution of threads of control. The term "parallel" is reserved for referring to the situation where multiple physical processors run simultaneously. '),(0,a.kt)("h4",d({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,a.kt)("p",null,"{",(0,a.kt)("em",null,"AI12-0119-1"),"}"," Rewrote the introduction to emphasize that threads of control can be generated by parallel constructs as well as tasks. "))}y.isMDXComponent=!0}}]);