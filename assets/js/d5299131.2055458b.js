"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[104],{7272:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>b,contentTitle:()=>k,default:()=>P,frontMatter:()=>g,metadata:()=>f,toc:()=>T});var i=a(1716),n=a(3050),r=a(8604),o=a(7318),s=a(4768),l=Object.defineProperty,m=Object.defineProperties,d=Object.getOwnPropertyDescriptors,p=Object.getOwnPropertySymbols,c=Object.prototype.hasOwnProperty,h=Object.prototype.propertyIsEnumerable,u=(e,t,a)=>t in e?l(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,y=(e,t)=>{for(var a in t||(t={}))c.call(t,a)&&u(e,a,t[a]);if(p)for(var a of p(t))h.call(t,a)&&u(e,a,t[a]);return e};const g={sidebar_position:176},k="E.5 Partition Communication Subsystem",f={unversionedId:"arm/AA-E/AA-E.5",id:"arm/AA-E/AA-E.5",title:"E.5 Partition Communication Subsystem",description:"This Reference Manual output has not been verified,",source:"@site/docs/arm/AA-E/AA-E.5.mdx",sourceDirName:"arm/AA-E",slug:"/arm/AA-E/AA-E.5",permalink:"/docs/arm/AA-E/AA-E.5",draft:!1,tags:[],version:"current",sidebarPosition:176,frontMatter:{sidebar_position:176},sidebar:"referenceManualSidebar",previous:{title:"E.4 Remote Subprogram Calls",permalink:"/docs/arm/AA-E/AA-E.4"},next:{title:"Annex F Information Systems",permalink:"/docs/arm/AA-F/"}},b={},T=[{value:"Static Semantics",id:"static-semantics",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Documentation Requirements",id:"documentation-requirements",level:4},{value:"Implementation Permissions",id:"implementation-permissions",level:4},{value:"Implementation Advice",id:"implementation-advice",level:4},{value:"Incompatibilities With Ada 95",id:"incompatibilities-with-ada-95",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4}],x={toc:T};function P(e){var t,a=e,{components:l}=a,u=((e,t)=>{var a={};for(var i in e)c.call(e,i)&&t.indexOf(i)<0&&(a[i]=e[i]);if(null!=e&&p)for(var i of p(e))t.indexOf(i)<0&&h.call(e,i)&&(a[i]=e[i]);return a})(a,["components"]);return(0,i.kt)("wrapper",(t=y(y({},x),u),m(t,d({components:l,mdxType:"MDXLayout"}))),(0,i.kt)("h1",y({},{id:"e5-partition-communication-subsystem"}),"E.5 Partition Communication Subsystem"),(0,i.kt)("admonition",y({},{type:"warning"}),(0,i.kt)("p",{parentName:"admonition"},"This Reference Manual output has not been verified,\nand may contain omissions or errors.\nReport any problems on the ",(0,i.kt)("a",y({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,i.kt)(o.Z,{mdxType:"MarginText"},"1/2"),(0,i.kt)(s.Z,{items:["AI95-00273-01"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"[The ",(0,i.kt)("em",null,"Partition Communication Subsystem")," (PCS) provides facilities for supporting communication between the active partitions of a distributed program. The package System.RPC is a language-defined interface to the PCS.] ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"1.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"The prefix RPC is used rather than RSC because the term remote procedure call and its acronym are more familiar. ",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"static-semantics"}),"Static Semantics"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"2"),(0,i.kt)("p",null,"The following language-defined library package exists: ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"3/5"),(0,i.kt)(s.Z,{items:["AI12-0241-1","AI12-0302-1"],mdxType:"MarginInfo"}),(0,i.kt)(r.Z,{language:"ada",mdxType:"CodeBlock"},"with Ada.Streams; -- see ",(0,i.kt)("a",{href:"../AA-13/AA-13.13#Subclause_13.13.1"},"13.13.1"),"\n","package System.RPC","\n","   with Nonblocking =",">"," False, Global =",">"," in out synchronized is","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"4"),"type Partition_Id is range 0 .. implementation-defined;","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"5"),"Communication_Error : exception;","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"6"),"type Params_Stream_Type (","\n","      Initial_Size : Ada.Streams.Stream_Element_Count) is new","\n","      Ada.Streams.Root_Stream_Type with private;","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"7"),"procedure Read(","\n","      Stream : in out Params_Stream_Type;","\n","      Item : out Ada.Streams.Stream_Element_Array;","\n","      Last : out Ada.Streams.Stream_Element_Offset);","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"8"),"procedure Write(","\n","      Stream : in out Params_Stream_Type;","\n","      Item : in Ada.Streams.Stream_Element_Array);","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"9"),"-- Synchronous call","\n","   procedure Do_RPC(","\n","      Partition  : in Partition_Id;","\n","      Params     : access Params_Stream_Type;","\n","      Result     : access Params_Stream_Type);","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"10"),"-- Asynchronous call","\n","   procedure Do_APC(","\n","      Partition  : in Partition_Id;","\n","      Params     : access Params_Stream_Type);","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"11"),"-- The handler for incoming RPCs","\n","   type RPC_Receiver is access procedure(","\n","      Params     : access Params_Stream_Type;","\n","      Result     : access Params_Stream_Type);","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"12"),"procedure Establish_RPC_Receiver(","\n","      Partition : in Partition_Id;","\n","      Receiver  : in RPC_Receiver);","\n",(0,i.kt)(o.Z,{mdxType:"MarginText"},"13"),"private","\n","   ... -- not specified by the language","\n","end System.RPC;","\n"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"14"),(0,i.kt)("p",null,"A value of the type Partition_Id is used to identify a partition. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"14.a/2"),(0,i.kt)(n.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"The range of type System.RPC.Partition_Id.",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"15"),(0,i.kt)("p",null,"An object of the type Params_Stream_Type is used for identifying the particular remote subprogram that is being called, as well as marshalling and unmarshalling the parameters or result of a remote subprogram call, as part of sending them between partitions.",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"16"),(0,i.kt)("p",null,"[The Read and Write procedures override the corresponding abstract operations for the type Params_Stream_Type.]",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"17"),(0,i.kt)("p",null,"The Do_RPC and Do_APC procedures send a message to the active partition identified by the Partition parameter. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"17.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"It is assumed that the RPC interface is above the message-passing layer of the network protocol stack and is implemented in terms of it. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"18"),(0,i.kt)("p",null,"After sending the message, Do_RPC blocks the calling task until a reply message comes back from the called partition or some error is detected by the underlying communication system in which case Communication_Error is raised at the point of the call to Do_RPC. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"18.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"Only one exception is defined in System.RPC, although many sources of errors might exist. This is so because it is not always possible to distinguish among these errors. In particular, it is often impossible to tell the difference between a failing communication link and a failing processing node. Additional information might be associated with a particular Exception_Occurrence for a Communication_Error. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"19"),(0,i.kt)("p",null,"Do_APC operates in the same way as Do_RPC except that it is allowed to return immediately after sending the message.",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"20"),(0,i.kt)("p",null,"Upon normal return, the stream designated by the Result parameter of Do_RPC contains the reply message.",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"21"),(0,i.kt)("p",null,"The procedure System.RPC.Establish_RPC_Receiver is called once, immediately after elaborating the library units of an active partition (that is, right after the ",(0,i.kt)("em",null,"elaboration of the partition"),") if the partition includes an RCI library unit, but prior to invoking the main subprogram, if any. The Partition parameter is the Partition_Id of the active partition being elaborated. The Receiver parameter designates an implementation-provided procedure called the ",(0,i.kt)("em",null,"RPC-receiver")," which will handle all RPCs received by the partition from the PCS. Establish_RPC_Receiver saves a reference to the RPC-receiver; when a message is received at the called partition, the RPC-receiver is called with the Params stream containing the message. When the RPC-receiver returns, the contents of the stream designated by Result is placed in a message and sent back to the calling partition. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"21.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"It is defined by the PCS implementation whether one or more threads of control should be available to process incoming messages and to wait for their completion. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"21.b"),(0,i.kt)(n.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"At link-time, the linker provides the RPC-receiver and the necessary tables to support it. A call on Establish_RPC_Receiver is inserted just before the call on the main subprogram. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"21.c"),(0,i.kt)(n.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"The interface between the PCS (the System.RPC package) and the RPC-receiver is defined to be dynamic in order to allow the elaboration sequence to notify the PCS that all packages have been elaborated and that it is safe to call the receiving stubs. It is not guaranteed that the PCS units will be the last to be elaborated, so some other indication that elaboration is complete is needed. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"22"),(0,i.kt)("p",null,"If a call on Do_RPC is aborted, a cancellation message is sent to the called partition, to request that the execution of the remotely called subprogram be aborted. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"22.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,i.kt)("strong",null,"To be honest: "),"The full effects of this message are dependent on the implementation of the PCS. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"23/5"),(0,i.kt)(s.Z,{items:["AI12-0241-1"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,(0,i.kt)("em",null,"This paragraph was deleted.")," ",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"24"),(0,i.kt)("p",null,"The implementation of the RPC-receiver shall be reentrant[, thereby allowing concurrent calls on it from the PCS to service concurrent remote subprogram calls into the partition]. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"24.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"There seems no reason to allow the implementation of RPC-receiver to be nonreentrant, even though we don't require that every implementation of the PCS actually perform concurrent calls on the RPC-receiver. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"24.1/1"),(0,i.kt)(s.Z,{items:["AI95-00082-01"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"8652/0087"),"}"," An implementation shall not restrict the replacement of the body of System.RPC. An implementation shall not restrict children of System.RPC. [The related implementation permissions in the introduction to Annex A do not apply.] ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"24.a.1/1"),(0,i.kt)(n.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"The point of System.RPC is to let the user tailor the communications mechanism without requiring changes to or other cooperation from the compiler. However, implementations can restrict the replacement of language-defined units. This requirement overrides that permission for System.RPC. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"24.2/1"),(0,i.kt)(s.Z,{items:["AI95-00082-01"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"{",(0,i.kt)("em",null,"8652/0087"),"}"," If the implementation of System.RPC is provided by the user, an implementation shall support remote subprogram calls as specified. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"24.b/2"),(0,i.kt)(s.Z,{items:["AI95-00273-01"],mdxType:"MarginInfo"}),(0,i.kt)(n.Z,{type:"aarm",aarm:"discussion",title:"Discussion: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"If the implementation takes advantage of the implementation permission to use a different specification for System.RPC, it still needs to use it for remote subprogram calls, and allow the user to replace the body of System.RPC. It just isn't guaranteed to be portable to do so in Ada 2005 - an advantage which was more theoretical than real anyway. ",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"documentation-requirements"}),"Documentation Requirements"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"25"),(0,i.kt)("p",null,"The implementation of the PCS shall document whether the RPC-receiver is invoked from concurrent tasks. If there is an upper limit on the number of such tasks, this limit shall be documented as well, together with the mechanisms to configure it (if this is supported). ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"25.a/2"),(0,i.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,i.kt)("em",null,"This paragraph was deleted."),(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"25.a.1/2"),(0,i.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},(0,i.kt)("strong",null,"Documentation Requirement: "),"Whether the RPC-receiver is invoked from concurrent tasks, and if so, the number of such tasks.",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"implementation-permissions"}),"Implementation Permissions"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"26"),(0,i.kt)("p",null,"The PCS is allowed to contain implementation-defined interfaces for explicit message passing, broadcasting, etc. Similarly, it is allowed to provide additional interfaces to query the state of some remote partition (given its partition ID) or of the PCS itself, to set timeouts and retry parameters, to get more detailed error status, etc. These additional interfaces should be provided in child packages of System.RPC. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"26.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"implementation-defined",title:"Implementation defined: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"Implementation-defined interfaces in the PCS.",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"27/5"),(0,i.kt)(s.Z,{items:["AI12-0444-1"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"A body for the package System.RPC is not required to  be supplied by the implementation. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"27.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"It is presumed that a body for the package System.RPC might be extremely environment specific. Therefore, we do not require that a body be provided by the (compiler) implementation. The user will have to write a body, or acquire one, appropriate for the target environment. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"27.1/3"),(0,i.kt)(s.Z,{items:["AI95-00273-01","AI05-0299-1"],mdxType:"MarginInfo"}),(0,i.kt)("p",null,"An alternative declaration is allowed for package System.RPC as long as it provides a set of operations that is substantially equivalent to the specification defined in this subclause. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"27.b/2"),(0,i.kt)(n.Z,{type:"aarm",aarm:"reason",title:"Reason: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"Experience has proved that the definition of System.RPC given here is inadequate for interfacing to existing distribution mechanisms (such as CORBA), especially on heterogeneous systems. Rather than mandate a change in the mechanism (which would break existing systems), require implementations to support multiple mechanisms (which is impractical), or prevent the use of Annex E facilities with existing systems (which would be silly), we simply make this facility optional.",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"27.c/2"),(0,i.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"One of the purposes behind System.RPC was that knowledgeable users, rather than compiler vendors, could create this package tailored to their networks. Experience has shown that users get their RPC from vendors anyway; users have not taken advantage of the flexibility provided by this defined interface. Moreover, one could compare this defined interface to requiring Ada compilers to use a defined interface to implement tasking. No one thinks that the latter is a good idea, why should anyone believe that the former is?",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"27.d/3"),(0,i.kt)(s.Z,{items:["AI05-0299-1"],mdxType:"MarginInfo"}),(0,i.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"Therefore, this subclause is made optional. We considered deleting the subclause outright, but we still require that users may replace the package (whatever its interface). Also, it still provides a useful guide to the implementation of this feature. ",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"implementation-advice"}),"Implementation Advice"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"28"),(0,i.kt)("p",null,"Whenever possible, the PCS on the called partition should allow for multiple tasks to call the RPC-receiver with different messages and should allow them to block until the corresponding subprogram body returns. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"28.a/2"),(0,i.kt)(n.Z,{type:"aarm",aarm:"implementation-advice",title:"Implementation Advice",mdxType:"Admonition"},(0,i.kt)("strong",null),"The PCS should allow for multiple tasks to call the RPC-receiver.",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"29"),(0,i.kt)("p",null,"The Write operation on a stream of type Params_Stream_Type should raise Storage_Error if it runs out of space trying to write the Item into the stream. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"29.a.1/2"),(0,i.kt)(n.Z,{type:"aarm",aarm:"implementation-advice",title:"Implementation Advice",mdxType:"Admonition"},(0,i.kt)("strong",null),"The System.RPC.Write operation should raise Storage_Error if it runs out of space when writing an item.",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"29.a"),(0,i.kt)(n.Z,{type:"aarm",aarm:"implementation-note",title:"Implementation Note: ",mdxType:"Admonition"},(0,i.kt)("strong",null),"An implementation could also dynamically allocate more space as needed, only propagating Storage_Error if the ",(0,i.kt)("code",null,(0,i.kt)("a",{href:"../AA-4/AA-4.8#S0164"},"allocator"))," it calls raises Storage_Error. This storage could be managed through a controlled component of the stream object, to ensure that it is reclaimed when the stream object is finalized. ",(0,i.kt)("br",null)),(0,i.kt)(o.Z,{mdxType:"MarginText"},"30"),(0,i.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"NOTE 1   The package System.RPC is not designed for direct calls by user programs. It is instead designed for use in the implementation of remote subprograms calls, being called by the calling stubs generated for a remote call interface library unit to initiate a remote call, and in turn calling back to an RPC-receiver that dispatches to the receiving stubs generated for the body of a remote call interface, to handle a remote call received from elsewhere. ",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"incompatibilities-with-ada-95"}),"Incompatibilities With Ada 95"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"30.a/2"),(0,i.kt)(s.Z,{items:["AI95-00273-01"],mdxType:"MarginInfo"}),(0,i.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"The specification of System.RPC can now be tailored for an implementation. If a program replaces the body of System.RPC with a user-defined body, it might not compile in a given implementation of Ada 2005 (if the specification of System.RPC has been changed). ",(0,i.kt)("br",null)),(0,i.kt)("h4",y({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,i.kt)(o.Z,{mdxType:"MarginText"},"30.b/2"),(0,i.kt)(s.Z,{items:["AI95-00082-01"],mdxType:"MarginInfo"}),(0,i.kt)(n.Z,{type:"aarm",aarm:"note",title:"Note: ",mdxType:"Admonition"},"{",(0,i.kt)("em",null,"8652/0087"),"}"," ",(0,i.kt)("strong",null,"Corrigendum:")," Clarified that the user can replace System.RPC. ",(0,i.kt)("br",null)))}P.isMDXComponent=!0}}]);