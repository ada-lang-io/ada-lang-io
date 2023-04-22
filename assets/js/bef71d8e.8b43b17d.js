"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[6723],{52120:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>y,contentTitle:()=>h,default:()=>g,frontMatter:()=>p,metadata:()=>m,toc:()=>f});var a=n(91716),i=Object.defineProperty,o=Object.defineProperties,r=Object.getOwnPropertyDescriptors,s=Object.getOwnPropertySymbols,l=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,c=(e,t,n)=>t in e?i(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n,u=(e,t)=>{for(var n in t||(t={}))l.call(t,n)&&c(e,n,t[n]);if(s)for(var n of s(t))d.call(t,n)&&c(e,n,t[n]);return e};const p={title:"6.1 Concurrency Options"},h=void 0,m={unversionedId:"style-guide/s6/01",id:"style-guide/s6/01",title:"6.1 Concurrency Options",description:"Many problems map naturally to a concurrent programming solution. By",source:"@site/docs/style-guide/s6/01.mdx",sourceDirName:"style-guide/s6",slug:"/style-guide/s6/01",permalink:"/docs/style-guide/s6/01",draft:!1,tags:[],version:"current",frontMatter:{title:"6.1 Concurrency Options"},sidebar:"styleGuideSidebar",previous:{title:"6. Concurrency",permalink:"/docs/style-guide/s6/"},next:{title:"6.2 Communication",permalink:"/docs/style-guide/s6/02"}},y={},f=[{value:"Protected Objects",id:"protected-objects",level:3},{value:"guideline",id:"guideline",level:4},{value:"example",id:"example",level:4},{value:"rationale",id:"rationale",level:4},{value:"exceptions",id:"exceptions",level:4},{value:"Tasks",id:"tasks",level:3},{value:"guideline",id:"guideline-1",level:4},{value:"example",id:"example-1",level:4},{value:"rationale",id:"rationale-1",level:4},{value:"Discriminants",id:"discriminants",level:3},{value:"guideline",id:"guideline-2",level:4},{value:"example",id:"example-2",level:4},{value:"rationale",id:"rationale-2",level:4},{value:"notes",id:"notes",level:4},{value:"Anonymous Task Types and Protected Types",id:"anonymous-task-types-and-protected-types",level:3},{value:"guideline",id:"guideline-3",level:4},{value:"example",id:"example-3",level:4},{value:"rationale",id:"rationale-3",level:4},{value:"notes",id:"notes-1",level:4},{value:"Dynamic Tasks",id:"dynamic-tasks",level:3},{value:"guideline",id:"guideline-4",level:4},{value:"example",id:"example-4",level:4},{value:"rationale",id:"rationale-4",level:4},{value:"Priorities",id:"priorities",level:3},{value:"guideline",id:"guideline-5",level:4},{value:"example",id:"example-5",level:4},{value:"rationale",id:"rationale-5",level:4},{value:"notes",id:"notes-2",level:4},{value:"Delay Statements",id:"delay-statements",level:3},{value:"guideline",id:"guideline-6",level:4},{value:"example",id:"example-6",level:4},{value:"rationale",id:"rationale-6",level:4},{value:"notes",id:"notes-3",level:4},{value:"Extensibility and Concurrent Structures",id:"extensibility-and-concurrent-structures",level:3},{value:"guideline",id:"guideline-7",level:4},{value:"rationale",id:"rationale-7",level:4},{value:"notes",id:"notes-4",level:4}],k={toc:f};function g(e){var t,n=e,{components:i}=n,c=((e,t)=>{var n={};for(var a in e)l.call(e,a)&&t.indexOf(a)<0&&(n[a]=e[a]);if(null!=e&&s)for(var a of s(e))t.indexOf(a)<0&&d.call(e,a)&&(n[a]=e[a]);return n})(n,["components"]);return(0,a.kt)("wrapper",(t=u(u({},k),c),o(t,r({components:i,mdxType:"MDXLayout"}))),(0,a.kt)("p",null,"Many problems map naturally to a concurrent programming solution. By\nunderstanding and correctly using the Ada language concurrency features,\nyou can produce solutions that are largely independent of target\nimplementation. Tasks provide a means, within the Ada language, of\nexpressing concurrent, asynchronous threads of control and relieving\nprogrammers from the problem of explicitly controlling multiple\nconcurrent activities. Protected objects serve as a building block to\nsupport other synchronization paradigms. Tasks cooperate to perform the\nrequired activities of the software. Synchronization and mutual\nexclusion are required between individual tasks. The Ada rendezvous and\nprotected objects provide powerful mechanisms for both synchronization\nand mutual exclusion."),(0,a.kt)("h3",u({},{id:"protected-objects"}),"Protected Objects"),(0,a.kt)("h4",u({},{id:"guideline"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Consider using protected objects to provide mutually exclusive\naccess to data."),(0,a.kt)("li",{parentName:"ul"},"Consider using protected objects to control or synchronize access to\ndata shared by multiple tasks ."),(0,a.kt)("li",{parentName:"ul"},"Consider using protected objects to implement synchronization, such\nas a passive resource monitor."),(0,a.kt)("li",{parentName:"ul"},"Consider encapsulating protected objects in the private part or body\nof a package."),(0,a.kt)("li",{parentName:"ul"},"Consider using a protected procedure to implement an interrupt\nhandler."),(0,a.kt)("li",{parentName:"ul"},"Do not attach a protected procedure handler to a hardware interrupt\nif that interrupt has a maximum priority greater than the ceiling\npriority assigned to the handler."),(0,a.kt)("li",{parentName:"ul"},"Avoid the use of global variables in entry barriers."),(0,a.kt)("li",{parentName:"ul"},"Avoid the use of barrier expressions with side effects.")),(0,a.kt)("h4",u({},{id:"example"}),"example"),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"generic\n   type Item is private;\n   Maximum_Buffer_Size : in Positive;\npackage Bounded_Buffer_Package is\n\n   subtype Buffer_Index is Positive range 1..Maximum_Buffer_Size;\n   subtype Buffer_Count is Natural  range 0..Maximum_Buffer_Size;\n   type    Buffer_Array is array (Buffer_Index) of Item;\n\n   protected type Bounded_Buffer is\n      entry Get (X : out Item);\n      entry Put (X : in Item);\n   private\n      Get_Index : Buffer_Index := 1;\n      Put_Index : Buffer_Index := 1;\n      Count     : Buffer_Count := 0;\n      Data      : Buffer_Array;\n   end Bounded_Buffer;\n\nend Bounded_Buffer_Package;\n\n------------------------------------------------------------------\npackage body Bounded_Buffer_Package is\n\n   protected body Bounded_Buffer is\n\n      entry Get (X : out Item) when Count > 0 is\n      begin\n         X := Data(Get_Index);\n         Get_Index := (Get_Index mod Maximum_Buffer_Size) + 1;\n         Count := Count - 1;\n      end Get;\n\n      entry Put (X : in Item) when Count < Maximum_Buffer_Size is\n      begin\n         Data(Put_Index) := X;\n         Put_Index  := (Put_Index mod Maximum_Buffer_Size) + 1;\n         Count := Count + 1;\n      end Put;\n\n   end Bounded_Buffer;\n\nend Bounded_Buffer_Package;\n")),(0,a.kt)("h4",u({},{id:"rationale"}),"rationale"),(0,a.kt)("p",null,'Protected objects are intended to provide a "lightweight" mechanism for\nmutual exclusion and data synchronization. You should use a task only\nwhen you need to introduce explicitly a new, concurrent thread of\ncontrol (see Guideline 6.1.2).'),(0,a.kt)("p",null,"Protected objects offer a low overhead, efficient means to coordinate\naccess to shared data. A protected type declaration is similar to a\nprogram unit and consists of both a specification and a body. The data\nto be protected must be declared in the specification, as well as the\noperations that can be used to manipulate this data. If some operations\nare only allowed conditionally, entries must be provided. Ada 95 rules\nrequire that entry barriers be evaluated at the end of procedure calls\nand entry calls on protected objects. Entry barriers should avoid\nreferring to global variables so that the underlying assumptions of the\nstate of the protected object are not violated. Protected procedures and\nentries should be used to change the state of a protected object."),(0,a.kt)("p",null,"Most clients of an abstraction do not need to know how it is\nimplemented, whether it is a regular abstraction or a shared\nabstraction. A protected type is inherently a limited type, and you can\nuse protected types to implement a limited private type exported by a\npackage. As pointed out in Guideline 5.3.3, abstractions are best\nimplemented using private types (possibly derived from controlled types)\nor limited private types, providing appropriate operations that overcome\nthe restrictiveness imposed by the use of private types."),(0,a.kt)("p",null,"The Rationale (1995, \xa79.1) describes the interrupt handling features\nthat make the protected procedure the recommended building block:"),(0,a.kt)("p",null,"A protected procedure is very well suited to act as an interrupt handler\nfor a number of reasons; they both typically have a short bounded\nexecution time, do not arbitrarily block, have a limited context and\nfinally they both have to integrate with the priority model. The\nnonblocking critical region matches the needs of an interrupt handler,\nas well as the needs of non-interrupt-level code to synchronize with an\ninterrupt handler. The entry barrier construct allows an interrupt\nhandler to signal a normal task by changing the state of a component of\nthe protected object and thereby making a barrier true."),(0,a.kt)("p",null,"When using protected procedures for interrupt handling, you must ensure\nthat the ceiling priority of the handler is at least as high as the\nmaximum possible priority of the interrupt to be handled. With\npriority-ceiling locking, the delivery of an interrupt with a higher\npriority than the ceiling priority of the handler will result in\nerroneous execution (Ada Reference Manual 1995, \xa7C.3.1)."),(0,a.kt)("p",null,"A global variable could be changed by another task or even by a call of\na protected function. These changes will not be acted upon promptly.\nTherefore, you should not use a global variable in an entry barrier."),(0,a.kt)("p",null,"Side effects in barrier expressions can cause undesirable dependencies.\nTherefore, you should avoid the use of barrier expressions that can\ncause side effects."),(0,a.kt)("p",null,"See also Guideline ."),(0,a.kt)("h4",u({},{id:"exceptions"}),"exceptions"),(0,a.kt)("p",null,"If the client of the abstraction containing the protected object must\nuse a select statement with an entry call, you must expose the protected\nobject on the package interface."),(0,a.kt)("h3",u({},{id:"tasks"}),"Tasks"),(0,a.kt)("h4",u({},{id:"guideline-1"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Use tasks to model selected asynchronous threads of control within\nthe problem domain."),(0,a.kt)("li",{parentName:"ul"},"Consider using tasks to define concurrent algorithms."),(0,a.kt)("li",{parentName:"ul"},"Consider using rendezvous when your application requires synchronous\nunbuffered communication.")),(0,a.kt)("h4",u({},{id:"example-1"}),"example"),(0,a.kt)("p",null,"The naturally concurrent objects within the problem domain can be\nmodeled as Ada tasks."),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"-- The following example of a stock exchange simulation shows how naturally\n-- concurrent objects within the problem domain can be modeled as Ada tasks.\n\n-------------------------------------------------------------------------\n\n-- Protected objects are used for the Display and for the Transaction_Queue\n-- because they only need a mutual exclusion mechanism.\n\nprotected Display is\n   entry Shift_Tape_Left;\n   entry Put_Character_On_Tape (C : in Character);\nend Display;\n\nprotected Transaction_Queue is\n   entry Put (T : in     Transaction);\n   entry Get (T :    out Transaction);\n   function Is_Empty return Boolean;\nend Transaction_Queue;\n\n-------------------------------------------------------------------------\n\n-- A task is needed for the Ticker_Tape because it has independent cyclic\n-- activity.  The Specialist and the Investor are best modeled with tasks\n-- since they perform different actions simultaneously, and should be\n-- asynchronous threads of control.\n\ntask Ticker_Tape;\n\ntask Specialist is\n   entry Buy  (Order : in Order_Type);\n   entry Sell (Order : in Order_Type);\nend Specialist;\n\ntask Investor;\n-------------------------------------------------------------------------\ntask body Ticker_Tape is\n   ...\nbegin\n   loop\n      Display.Shift_Tape_Left;\n\n      if not More_To_Send (Current_Tape_String) and then\n         not Transaction_Queue.Is_Empty\n      then\n         Transaction_Queue.Get (Current_Tape_Transaction);\n         ... -- convert Transaction to string\n      end if;\n\n      if More_To_Send (Current_Tape_String) then\n         Display.Put_Character_On_Tape (Next_Char);\n      end if;\n\n      delay until Time_To_Shift_Tape;\n      Time_To_Shift_Tape := Time_To_Shift_Tape + Shift_Interval;\n   end loop;\nend Ticker_Tape;\n\ntask body Specialist is\n   ...\n\n   loop\n      select\n         accept Buy  (Order : in Order_Type) do\n            ...\n         end Buy;\n         ...\n      or\n         accept Sell (Order : in Order_Type) do\n            ...\n         end Sell;\n         ...\n      else\n         -- match orders\n         ...\n         Transaction_Queue.Put (New_Transaction);\n         ...\n      end select;\n   end loop;\n\nend Specialist;\n\ntask body Investor is\n   ...\nbegin\n\n   loop\n      -- some algorithm that determines whether the investor\n      -- buys or sells, quantity, price, etc\n\n      ...\n\n      if ... then\n         Specialist.Buy (Order);\n      end if;\n\n      if ... then\n         Specialist.Sell (Order);\n      end if;\n   end loop;\n\nend Investor;\n")),(0,a.kt)("p",null,"Multiple tasks that implement the decomposition of a large, matrix\nmultiplication algorithm are an example of an opportunity for real\nconcurrency in a multiprocessor target environment. In a single\nprocessor target environment, this approach may not be justified due to\nthe overhead incurred from context switching and the sharing of system\nresources."),(0,a.kt)("p",null,"A task that updates a radar display every 30 milliseconds is an example\nof a cyclic activity supported by a task."),(0,a.kt)("p",null,"A task that detects an over-temperature condition in a nuclear reactor\nand performs an emergency shutdown of the systems is an example of a\ntask to support a high-priority activity."),(0,a.kt)("h4",u({},{id:"rationale-1"}),"rationale"),(0,a.kt)("p",null,"These guidelines reflect the intended uses of tasks. They all revolve\naround the fact that a task has its own thread of control separate from\nthe main subprogram (or environment task) of a partition. The conceptual\nmodel for a task is a separate program with its own virtual processor.\nThis provides the opportunity to model entities from the problem domain\nin terms more closely resembling those entities and the opportunity to\nhandle physical devices as a separate concern from the main algorithm of\nthe application. Tasks also allow naturally concurrent activities that\ncan be mapped to multiple processors within a partition when available."),(0,a.kt)("p",null,"You should use tasks for separate threads of control. When you\nsynchronize tasks, you should use the rendezvous mechanism only when you\nare trying to synchronize actual processes (e.g., specify a\ntime-sensitive ordering relationship or tightly coupled interprocess\ncommunication). For most synchronization needs, however, you should use\nprotected objects (see Guideline 6.1.1), which are more flexible and can\nminimize unnecessary bottlenecks. Additionally, passive tasks are\nprobably better modeled through protected objects than active tasks."),(0,a.kt)("p",null,"Resources shared between multiple tasks, such as devices, require\ncontrol and synchronization because their operations are not atomic.\nDrawing a circle on a display might require that many low-level\noperations be performed without interruption by another task. A display\nmanager would ensure that no other task accesses the display until all\nthese operations are complete."),(0,a.kt)("h3",u({},{id:"discriminants"}),"Discriminants"),(0,a.kt)("h4",u({},{id:"guideline-2"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Consider using discriminants to minimize the need for an explicit\ninitialization operation (Rationale 1995, \xa79.1)."),(0,a.kt)("li",{parentName:"ul"},"Consider using discriminants to control composite components of the\nprotected objects, including setting the size of an entry family\n(Rationale 1995, \xa79.1)."),(0,a.kt)("li",{parentName:"ul"},"Consider using a discriminant to set the priority of a protected\nobject (Rationale 1995, \xa79.1)."),(0,a.kt)("li",{parentName:"ul"},"Consider using a discriminant to identify an interrupt to a\nprotected object (Rationale 1995, \xa79.1)."),(0,a.kt)("li",{parentName:"ul"},"Consider declaring a task type with a discriminant to indicate\n(Rationale 1995, \xa79.6):",(0,a.kt)("ul",{parentName:"li"},(0,a.kt)("li",{parentName:"ul"},"Priority, storage size, and size of entry families of individual\ntasks of a type"),(0,a.kt)("li",{parentName:"ul"},"Data associated with a task (through an access discriminant)")))),(0,a.kt)("h4",u({},{id:"example-2"}),"example"),(0,a.kt)("p",null,"The following code fragment shows how a task type with discriminant can\nbe used to associate data with a task (Rationale 1995, \xa79.6):"),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"type Task_Data is\n   record\n      ...  -- data for task to work on\n   end record;\ntask type Worker (D : access Task_Data) is\n   ...\nend;\n-- When you declare a task object of type Worker, you explicitly associate this task with\n-- its data through the discriminant D\nData_for_Worker_X : aliased Task_Data := ...;\nX : Worker (Data_for_Worker_X'Access);\n")),(0,a.kt)("p",null,"The following example shows how to use discriminants to associate data\nwith tasks, thus allowing the tasks to be parameterized when they are\ndeclared and eliminating the need for an initial rendezvous with the\ntask:"),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"task type Producer (Channel : Channel_Number; ID : ID_Number);\n\ntask body Producer is\nbegin\n\n   loop\n\n      ... -- generate an item\n\n      Buffer.Put (New_Item);\n\n   end loop;\nend Producer;\n\n...\n\nKeyboard : Producer (Channel => Keyboard_Channel, ID => 1);\nMouse    : Producer (Channel => Mouse_Channel,    ID => 2);\n")),(0,a.kt)("p",null,"The next example shows how an initial rendezvous can be used to\nassociate data with tasks. This is more complicated and more error prone\nthan the previous example. This method is no longer needed in Ada 95 due\nto the availability of discriminants with task types and protected\ntypes:"),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"task type Producer is\n   entry Initialize (Channel : in Channel_Number; ID : in ID_Number);\nend Producer;\n\ntask body Producer is\n   IO_Channel  : Channel_Number;\n   Producer_ID : ID_Number;\nbegin\n\n   accept Initialize (Channel : in Channel_Number; ID : in ID_Number) do\n      IO_Channel  := Channel;\n      Producer_ID := ID;\n   end;\n\n   loop\n\n      ... -- generate an item\n\n      Buffer.Put (New_Item);\n\n   end loop;\nend Producer;\n\n...\n\nKeyboard : Producer;\nMouse    : Producer;\n\n...\n\nbegin\n   ...\n   Keyboard.Initialize (Channel => Keyboard_Channel, ID => 1);\n   Mouse.Initialize    (Channel => Mouse_Channel,    ID => 2);\n   ...\n")),(0,a.kt)("h4",u({},{id:"rationale-2"}),"rationale"),(0,a.kt)("p",null,"Using discriminants to parameterize protected objects provides a\nlow-overhead way of specializing the protected object. You avoid having\nto declare and call special subprograms solely for the purpose of\npassing this information to the protected object."),(0,a.kt)("p",null,"Task discriminants provide a way for you to identify or parameterize a\ntask without the overhead of an initial rendezvous. For example, you can\nuse this discriminant to initialize a task or tell it who it is (from\namong an array of tasks) (Rationale 1995, \xa7II.9). More importantly, you\ncan associate the discriminant with specific data. When you use an\naccess discriminant, you can bind the data securely to the task because\nthe access discriminant is constant and cannot be detached from the task\n(Rationale 1995, \xa79.6). This reduces and might eliminate bottlenecks in\nthe parallel activation of tasks (Rationale 1995, \xa79.6)."),(0,a.kt)("h4",u({},{id:"notes"}),"notes"),(0,a.kt)("p",null,"Using an access discriminant to initialize a task has a potential danger\nin that the data being referenced could change after the rendezvous.\nThis possibility and its effects should be considered and, if necessary,\nappropriate actions taken (e.g., copy the referenced data and not rely\non the data pointed to by the discriminant after initialization)."),(0,a.kt)("h3",u({},{id:"anonymous-task-types-and-protected-types"}),"Anonymous Task Types and Protected Types"),(0,a.kt)("h4",u({},{id:"guideline-3"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Consider using single task declarations to declare unique instances\nof concurrent tasks."),(0,a.kt)("li",{parentName:"ul"},"Consider using single protected declarations to declare unique\ninstances of protected objects.")),(0,a.kt)("h4",u({},{id:"example-3"}),"example"),(0,a.kt)("p",null,"The following example illustrates the syntactic differences between the\nkinds of tasks and protected objects discussed here. Buffer is static,\nbut its type is anonymous. No type name is declared to enable you to\ndeclare further objects of the same type."),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"task      Buffer;\n")),(0,a.kt)("p",null,"Because it is declared explicitly, the task type Buffer_Manager is not\nanonymous. Channel is static and has a name, and its type is not\nanonymous."),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"task type Buffer_Manager;\nChannel : Buffer_Manager;\n")),(0,a.kt)("h4",u({},{id:"rationale-3"}),"rationale"),(0,a.kt)("p",null,"The use of anonymous tasks and protected objects of anonymous type\navoids a proliferation of task and protected types that are only used\nonce, and the practice communicates to maintainers that there are no\nother tasks or protected objects of that type. If the need arises later\nto have additional tasks or protected objects of the same type, then the\nwork required to convert an anonymous task to a task type or an\nanonymous protected object to a protected type is minimal."),(0,a.kt)("p",null,"The consistent and logical use of task and protected types, when\nnecessary, contributes to understandability. Identical tasks can be\ndeclared using a common task type. Identical protected objects can be\ndeclared using a common protected type. Dynamically allocated task or\nprotected structures are necessary when you must create and destroy\ntasks or protected objects dynamically or when you must reference them\nby different names."),(0,a.kt)("h4",u({},{id:"notes-1"}),"notes"),(0,a.kt)("p",null,"Though changing the task or protected object from an anonymous type to a\ndeclared type is trivial, structural changes to the software\narchitecture might not be trivial. Introduction of multiple tasks or\nprotected objects of the declared type might require the scope of the\ntype to change and might change the behavior of the network of\nsynchronizing tasks and protected objects."),(0,a.kt)("h3",u({},{id:"dynamic-tasks"}),"Dynamic Tasks"),(0,a.kt)("h4",u({},{id:"guideline-4"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Minimize dynamic creation of tasks because of the potentially high\nstartup overhead; reuse tasks by having them wait for new work on\nsome appropriate entry queue.")),(0,a.kt)("h4",u({},{id:"example-4"}),"example"),(0,a.kt)("p",null,"The approach used in the following example is not recommended. The\nexample shows why caution is required with dynamically allocated task\nand protected objects. It illustrates how a dynamic task can be\ndisassociated from its name:"),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"task type Radar_Track;\ntype      Radar_Track_Pointer is access Radar_Track;\nCurrent_Track : Radar_Track_Pointer;\n---------------------------------------------------------------------\ntask body Radar_Track is\nbegin\n   loop\n      -- update tracking information\n      ...\n      -- exit when out of range\n      delay 1.0;\n   end loop;\n...\nend Radar_Track;\n---------------------------------------------------------------------\n...\nloop\n   ...\n   -- Radar_Track tasks created in previous passes through the loop\n   -- cannot be accessed from Current_Track after it is updated.\n   -- Unless some code deals with non-null values of Current_Track,\n   -- (such as an array of existing tasks)\n   -- this assignment leaves the existing Radar_Track task running with\n   -- no way to signal it to abort or to instruct the system to\n   -- reclaim its resources.\n\n   Current_Track := new Radar_Track;\n   ...\nend loop;\n")),(0,a.kt)("h4",u({},{id:"rationale-4"}),"rationale"),(0,a.kt)("p",null,"Starting up a task has significant overhead in many implementations. If\nan application has a need for dynamically created tasks, the tasks\nshould be implemented with a top-level loop so that after such a task\ncompletes its given job, it can cycle back and wait for a new job."),(0,a.kt)("p",null,"You can use dynamically allocated tasks and protected objects when you\nneed to allow the number of tasks and protected objects to vary during\nexecution. When you must ensure that tasks are activated in a particular\norder, you should use dynamically allocated tasks because the Ada\nlanguage does not define an activation order for statically allocated\ntask objects. In using dynamically allocated tasks and protected\nobjects, you face the same issues as with any use of the heap."),(0,a.kt)("h3",u({},{id:"priorities"}),"Priorities"),(0,a.kt)("h4",u({},{id:"guideline-5"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Do not rely on pragma Priority unless your compiler supports the\nReal-Time Annex (Ada Reference Manual 1995, Annex D) and priority\nscheduling."),(0,a.kt)("li",{parentName:"ul"},"Minimize risk of priority inversion by use of protected objects and\nceiling priority."),(0,a.kt)("li",{parentName:"ul"},"Do not rely upon task priorities to achieve a particular sequence of\ntask execution.")),(0,a.kt)("h4",u({},{id:"example-5"}),"example"),(0,a.kt)("p",null,"For example, let the tasks have the following priorities:"),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"task T1 is\n   pragma Priority (High);\nend T1;\n\ntask T2 is\n   pragma Priority (Medium);\nend T2;\n\ntask Server is\n   entry Operation (...);\nend Server;\n\n----------------------------\ntask body T1 is\nbegin\n   ...\n   Server.Operation (...);\n   ...\nend T1;\ntask body T2 is\nbegin\n   ...\n   Server.Operation (...);\n   ...\nend T2;\n\ntask body Server is\nbegin\n   ...\n   accept Operation (...);\n   ...\nend Server;\n")),(0,a.kt)("p",null,"At some point in its execution, T1 is blocked. Otherwise, T2 and Server\nmight never execute. If T1 is blocked, it is possible for T2 to reach\nits call to Server's entry (Operation) before T1. Suppose this has\nhappened and that T1 now makes its entry call before Server has a chance\nto accept T2's call."),(0,a.kt)("p",null,"This is the timeline of events so far:"),(0,a.kt)("p",null,"T1 blocks T2 calls Server.Operation T1 unblocks T1 calls\nServer.Operation\u2014Does Server accept the call from T1 or from T2?"),(0,a.kt)("p",null,"You might expect that, due to its higher priority, T1's call would be\naccepted by Server before that of T2. However, entry calls are queued in\nfirst-in-first-out (FIFO) order and not queued in order of priority\n(unless pragma Queueing_Policy is used). Therefore, the synchronization\nbetween T1 and Server is not affected by T1's priority. As a result, the\ncall from T2 is accepted first. This is a form of priority inversion.\n(Annex D can change the default policy of FIFO queues.)"),(0,a.kt)("p",null,"A solution might be to provide an entry for a High priority user and an\nentry for a Medium priority user."),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"---------------------------------------------------------------------\ntask Server is\n   entry Operation_High_Priority;\n   entry Operation_Medium_Priority;\n   ...\nend Server;\n---------------------------------------------------------------------\ntask body Server is\nbegin\n   loop\n      select\n         accept Operation_High_Priority do\n            Operation;\n         end Operation_High_Priority;\n      else  -- accept any priority\n         select\n            accept Operation_High_Priority do\n               Operation;\n            end Operation_High_Priority;\n         or\n            accept Operation_Medium_Priority do\n               Operation;\n            end Operation_Medium_Priority;\n         or\n            terminate;\n         end select;\n      end select;\n   end loop;\n...\nend Server;\n---------------------------------------------------------------------\n")),(0,a.kt)("p",null,"However, in this approach, T1 still waits for one execution of Operation\nwhen T2 has already gained control of the task Server. In addition, the\napproach increases the communication complexity (see Guideline 6.2.6)."),(0,a.kt)("h4",u({},{id:"rationale-5"}),"rationale"),(0,a.kt)("p",null,"The pragma Priority allows relative priorities to be placed on tasks to\naccomplish scheduling. Precision becomes a critical issue with\nhard-deadline scheduling. However, there are certain problems associated\nwith using priorities that warrant caution."),(0,a.kt)("p",null,"Priority inversion occurs when lower priority tasks are given service\nwhile higher priority tasks remain blocked. In the first example, this\noccurred because entry queues are serviced in FIFO order, not by\npriority. There is another situation referred to as a race condition . A\nprogram like the one in the first example might often behave as expected\nas long as T1 calls Server.Operation only when T2 is not already using\nServer.Operation or waiting. You cannot rely on T1 always winning the\nrace because that behavior would be due more to fate than to the\nprogrammed priorities. Race conditions change when either adding code to\nan unrelated task or porting this code to a new target."),(0,a.kt)("p",null,"You should not rely upon task priorities to achieve an exact sequence of\nexecution or rely upon them to achieve mutual exclusion. Although the\nunderlying dispatching model is common to all Ada 95 implementations,\nthere might be differences in dispatching, queuing, and locking policies\nfor tasks and protected objects. All of these factors might lead to\ndifferent sequences of execution. If you need to ensure a sequence of\nexecution, you should make use of Ada's synchronization mechanisms,\ni.e., protected objects or rendezvous."),(0,a.kt)("h4",u({},{id:"notes-2"}),"notes"),(0,a.kt)("p",null,"Work is being done to minimize these problems, including the\nintroduction of a scheduling algorithm known as the priority ceiling\nprotocol (Goodenough and Sha 1988). The priority ceiling protocol\nreduces the blocking time that causes priority inversion to only one\ncritical region (defined by the entries in a task). The protocol also\neliminates deadlock (unless a task recursively tries to access a\ncritical region) by giving a ceiling priority to each task accessing a\nresource that is as high as the priority of any task that ever accesses\nthat resource. This protocol is based on priority inheritance and, thus,\ndeviates from the standard Ada tasking paradigm, which supports priority\nceiling emulation instead of the priority ceiling blocking that occurs\nwith priority inheritance."),(0,a.kt)("p",null,"Priorities are used to control when tasks run relative to one another.\nWhen both tasks are not blocked waiting at an entry, the highest\npriority task is given precedence. However, the most critical tasks in\nan application do not always have the highest priority. For example,\nsupport tasks or tasks with small periods might have higher priorities\nbecause they need to run frequently."),(0,a.kt)("p",null,"All production-quality validated Ada 95 compilers will probably support\npragma Priority. However, you should use caution unless (Annex D is\nspecifically supported."),(0,a.kt)("p",null,"There is currently no universal consensus on how to apply the basic\nprinciples of rate monotonic scheduling (RMS) to the Ada 95 concurrency\nmodel. One basic principle of RMS is to arrange all periodic tasks so\nthat tasks with shorter periods have higher priorities than tasks with\nlonger periods. However, with Ada 95, it might be faster to raise the\npriorities of tasks whose jobs suddenly become critical than to wait for\nan executive task to reschedule them. In this case, priority inversion\ncan be minimized using a protected object with pragma\nLocking_Policy(Ceiling_Locking) as the server instead of a task."),(0,a.kt)("h3",u({},{id:"delay-statements"}),"Delay Statements"),(0,a.kt)("h4",u({},{id:"guideline-6"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Do not depend on a particular delay being achievable (Nissen and\nWallis 1984)."),(0,a.kt)("li",{parentName:"ul"},"Use a delay until not a delay statement to delay until a specific\ntime has been reached."),(0,a.kt)("li",{parentName:"ul"},"Avoid using a busy waiting loop instead of a delay.")),(0,a.kt)("h4",u({},{id:"example-6"}),"example"),(0,a.kt)("p",null,"The phase of a periodic task is the fraction of a complete cycle elapsed\nas measured from a specified reference point. In the following example,\nan inaccurate delay causes the phase of the periodic task to drift over\ntime (i.e., the task starts later and later in the cycle):"),(0,a.kt)("p",null,"Periodic:"),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"   loop\n      delay Interval;\n      ...\n   end loop Periodic;\n")),(0,a.kt)("p",null,"To avoid an inaccurate delay drift, you should use the delay until\nstatement. The following example (Rationale 1995, \xa79.3) shows how to\nsatisfy a periodic requirement with an average period:"),(0,a.kt)("pre",null,(0,a.kt)("code",u({parentName:"pre"},{className:"language-ada"}),"task body Poll_Device is\n   use type Ada.Real_Time.Time;\n   use type Ada.Real_Time.Time_Span;\n\n   Poll_Time :          Ada.Real_Time.Time := ...; -- time to start polling\n   Period    : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);\nbegin\n   loop\n      delay until Poll_Time;\n      ... -- Poll the device\n      Poll_Time := Poll_Time + Period;\n   end loop;\nend Poll_Device;\n")),(0,a.kt)("h4",u({},{id:"rationale-6"}),"rationale"),(0,a.kt)("p",null,"There are two forms of delay statement. The delay will cause a delay for\nat least a specified time interval. The delay until causes a delay until\nan absolute wake-up time. You should choose the form appropriate to your\napplication."),(0,a.kt)("p",null,"The Ada language definition only guarantees that the delay time is a\nminimum. The meaning of a delay or delay until statement is that the\ntask is not scheduled for execution before the interval has expired. In\nother words, a task becomes eligible to resume execution as soon as the\namount of time has passed. However, there is no guarantee of when (or\nif) it is scheduled after that time because the required resources for\nthat task might not be available at the expiration of the delay ."),(0,a.kt)("p",null,"A busy wait can interfere with processing by other tasks. It can consume\nthe very processor resource necessary for completion of the activity for\nwhich it is waiting. Even a loop with a delay can have the impact of\nbusy waiting if the planned wait is significantly longer than the delay\ninterval. If a task has nothing to do, it should be blocked at an accept\nor select statement, an entry call, or an appropriate delay."),(0,a.kt)("p",null,"The expiration time for a relative delay is rounded up to the nearest\nclock tick. If you use the real-time clock features provided by (Annex\nD, however, clock ticks are guaranteed to be no greater than one\nmillisecond (Ada Reference Manual 1995, \xa7D.8)."),(0,a.kt)("h4",u({},{id:"notes-3"}),"notes"),(0,a.kt)("p",null,"You need to ensure the arithmetic precision of the calculation\nPoll_Time := Poll_Time + Period; to avoid drift."),(0,a.kt)("h3",u({},{id:"extensibility-and-concurrent-structures"}),"Extensibility and Concurrent Structures"),(0,a.kt)("h4",u({},{id:"guideline-7"}),"guideline"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"Carefully consider the placement of components of protected types\nwithin a tagged type inheritance hierarchy."),(0,a.kt)("li",{parentName:"ul"},"Consider using generics to provide extensibility of data types\nrequiring the restrictions provided by protected objects.")),(0,a.kt)("h4",u({},{id:"rationale-7"}),"rationale"),(0,a.kt)("p",null,"Once a component of a protected type is added to an inheritance\nhierarchy of an abstract data type, further extensibility of that data\ntype is impaired. When you constrain the concurrent behavior of a type\n(i.e., introduce a protected type component), you lose the ability to\nmodify that behavior in subsequent derivations. Therefore, when the need\narises for a version of an abstract data type to impose the restrictions\nprovided by protected objects, the opportunity for reuse is maximized by\nadding the protected objects at the leaves of the inheritance hierarchy."),(0,a.kt)("p",null,"The reusability of common protected operations (e.g., mutually exclusive\nread/write operations) can be maximized by using generic implementations\nof abstract data types. These generic implementations then provide\ntemplates that can be instantiated with data types specific to\nindividual applications."),(0,a.kt)("h4",u({},{id:"notes-4"}),"notes"),(0,a.kt)("p",null,"You can address synchronization within an inheritance hierarchy in one\nof three ways:"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},"You can declare the root as a limited tagged type with a component\nthat belongs to a protected type and give the tagged type primitive\noperations that work by invoking the protected operations of that\ncomponent."),(0,a.kt)("li",{parentName:"ul"},"Given a tagged type implementing an abstract data type (perhaps\nresulting from several extensions), you can declare a protected type\nwith a component belonging to the tagged type. The body of each\nprotected operation would then invoke the corresponding operation of\nthe abstract data type. The protected operations provide mutual\nexclusion."),(0,a.kt)("li",{parentName:"ul"},"You can use a hybrid approach where you declare a protected type\nwith a component of some tagged type. You then use this protected\ntype to implement a new root tagged type (not a descendant of the\noriginal tagged type).")))}g.isMDXComponent=!0}}]);