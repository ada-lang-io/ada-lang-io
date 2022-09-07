"use strict";(self.webpackChunkada_lang_io=self.webpackChunkada_lang_io||[]).push([[5284],{71:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>f,contentTitle:()=>g,default:()=>A,frontMatter:()=>m,metadata:()=>y,toc:()=>k});var n=a(1716),o=a(7556),i=a(3183),r=Object.defineProperty,s=Object.defineProperties,l=Object.getOwnPropertyDescriptors,c=Object.getOwnPropertySymbols,p=Object.prototype.hasOwnProperty,d=Object.prototype.propertyIsEnumerable,h=(e,t,a)=>t in e?r(e,t,{enumerable:!0,configurable:!0,writable:!0,value:a}):e[t]=a,u=(e,t)=>{for(var a in t||(t={}))p.call(t,a)&&h(e,a,t[a]);if(c)for(var a of c(t))d.call(t,a)&&h(e,a,t[a]);return e};const m={sidebar_position:172},g="E.4 Remote Subprogram Calls",y={unversionedId:"arm/AA-E/AA-E.4",id:"arm/AA-E/AA-E.4",title:"E.4 Remote Subprogram Calls",description:"We're still working on the Reference manual output.  Internal links are broken,",source:"@site/docs/arm/AA-E/AA-E.4.mdx",sourceDirName:"arm/AA-E",slug:"/arm/AA-E/AA-E.4",permalink:"/docs/arm/AA-E/AA-E.4",draft:!1,tags:[],version:"current",sidebarPosition:172,frontMatter:{sidebar_position:172},sidebar:"referenceManualSidebar",previous:{title:"E.3 Consistency of a Distributed System",permalink:"/docs/arm/AA-E/AA-E.3"},next:{title:"E.5 Partition Communication Subsystem",permalink:"/docs/arm/AA-E/AA-E.5"}},f={},k=[{value:"Language Design Principles",id:"language-design-principles",level:4},{value:"Legality Rules",id:"legality-rules",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics",level:4},{value:"Implementation Requirements",id:"implementation-requirements",level:4},{value:"Wording Changes from Ada 95",id:"wording-changes-from-ada-95",level:4},{value:"Wording Changes from Ada 2005",id:"wording-changes-from-ada-2005",level:4},{value:"Wording Changes from Ada 2012",id:"wording-changes-from-ada-2012",level:4},{value:"E.4.1  Asynchronous Remote Calls",id:"e41--asynchronous-remote-calls",level:2},{value:"Static Semantics",id:"static-semantics",level:4},{value:"Legality Rules",id:"legality-rules-1",level:4},{value:"Dynamic Semantics",id:"dynamic-semantics-1",level:4},{value:"Implementation Requirements",id:"implementation-requirements-1",level:4},{value:"Extensions to Ada 2005",id:"extensions-to-ada-2005",level:4},{value:"E.4.2  Example of Use of a Remote Access-to-Class-Wide Type",id:"e42--example-of-use-of-a-remote-access-to-class-wide-type",level:2},{value:"Examples",id:"examples",level:4}],b={toc:k};function A(e){var t,a=e,{components:r}=a,h=((e,t)=>{var a={};for(var n in e)p.call(e,n)&&t.indexOf(n)<0&&(a[n]=e[n]);if(null!=e&&c)for(var n of c(e))t.indexOf(n)<0&&d.call(e,n)&&(a[n]=e[n]);return a})(a,["components"]);return(0,n.kt)("wrapper",(t=u(u({},b),h),s(t,l({components:r,mdxType:"MDXLayout"}))),(0,n.kt)("h1",u({},{id:"e4-remote-subprogram-calls"}),"E.4 Remote Subprogram Calls"),(0,n.kt)("admonition",u({},{type:"warning"}),(0,n.kt)("p",{parentName:"admonition"},"We're still working on the Reference manual output.  Internal links are broken,\nas are a bunch of other things.\nSee the ",(0,n.kt)("a",u({parentName:"p"},{href:"https://github.com/ada-lang-io/ada-lang-io/issues/20"}),"tracking issue"))),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0359-1"),"}"," A ",(0,n.kt)("em",null,"remote subprogram call")," is a subprogram call that invokes the execution of a subprogram in another (active) partition. The partition that originates the remote subprogram call is the ",(0,n.kt)("em",null,"calling partition"),", and the partition that executes the corresponding subprogram body is the ",(0,n.kt)("em",null,"called partition"),". Some remote procedure calls are allowed to return prior to the completion of subprogram execution. These are called ",(0,n.kt)("em",null,"asynchronous remote procedure calls"),"."),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"{",(0,n.kt)("em",null,"AI12-0359-1"),"}"," Remote subprogram calls are always between active partitions; a passive partition has no execution resources of its own and thus cannot execute anything, while a remote subprogram call is always executed by the called partition. ")),(0,n.kt)("p",null,"There are three different ways of performing a remote subprogram call: "),(0,n.kt)("p",null,"As a direct call on a (remote) subprogram explicitly declared in a remote call interface;"),(0,n.kt)("p",null,"As an indirect call through a value of a remote access-to-subprogram type;"),(0,n.kt)("p",null,"As a dispatching call with a controlling operand designated by a value of a remote access-to-class-wide type. "),(0,n.kt)("p",null,"The first way of calling corresponds to a ",(0,n.kt)("em",null,"static")," binding between the calling and the called partition. The latter two ways correspond to a ",(0,n.kt)("em",null,"dynamic")," binding between the calling and the called partition."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0101-1"),"}"," Remote types library units (see E.2.2) and remote call interface library units (see E.2.3) define the remote subprograms or remote access types used for remote subprogram calls. "),(0,n.kt)("h4",u({},{id:"language-design-principles"}),"Language Design Principles"),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"Remote subprogram calls are standardized since the RPC paradigm is widely-used, and establishing an interface to it in the annex will increase the portability and reusability of distributed programs.")),(0,n.kt)("h4",u({},{id:"legality-rules"}),"Legality Rules"),(0,n.kt)("p",null,"In a dispatching call with two or more controlling operands, if one controlling operand is designated by a value of a remote access-to-class-wide type, then all shall be."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0283-1"),"}"," A nonblocking program unit shall not contain, other than within nested units with Nonblocking specified as statically False, a dispatching call with a controlling operand designated by a value of a remote access-to-class-wide type."),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Reason: "),"Such a dispatching call is a potentially blocking call (see below) even if the called subprogram is nonblocking, so we must not assert that no blocking is possible. ")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Ramification: "),"The calls is illegal if the Nonblocking aspect of the containing unit is True, either implicitly by inheritance or by explicit specification. ")),(0,n.kt)("h4",u({},{id:"dynamic-semantics"}),"Dynamic Semantics"),(0,n.kt)("p",null,"For the execution of a remote subprogram call, subprogram parameters (and later the results, if any) are passed using a stream-oriented representation (see 13.13.1) [which is suitable for transmission between partitions]. This action is called ",(0,n.kt)("em",null,"marshalling"),". ",(0,n.kt)("em",null,"Unmarshalling")," is the reverse action of reconstructing the parameters or results from the stream-oriented representation. [Marshalling is performed initially as part of the remote subprogram call in the calling partition; unmarshalling is done in the called partition. After the remote subprogram completes, marshalling is performed in the called partition, and finally unmarshalling is done in the calling partition.]"),(0,n.kt)("p",null,"A ",(0,n.kt)("em",null,"calling stub")," is the sequence of code that replaces the subprogram body of a remotely called subprogram in the calling partition. A ",(0,n.kt)("em",null,"receiving stub"),' is the sequence of code (the "wrapper") that receives a remote subprogram call on the called partition and invokes the appropriate subprogram body. '),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"The use of the term ",(0,n.kt)("em",null,"stub")," in this annex should not be confused with ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-10/AA-10.1#S0297"},"body_stub"))," as defined in 10.1.3. The term ",(0,n.kt)("em",null,"stub")," is used here because it is a commonly understood term when talking about the RPC paradigm. ")),(0,n.kt)("p",null,"Remote subprogram calls are executed at most once, that is, if the subprogram call returns normally, then the called subprogram's body was executed exactly once."),(0,n.kt)("p",null,"The task executing a remote subprogram call blocks until the subprogram in the called partition returns, unless the call is asynchronous. For an asynchronous remote procedure call, the calling task can become ready before the procedure in the called partition returns."),(0,n.kt)("p",null,"If a construct containing a remote call is aborted, the remote subprogram call is ",(0,n.kt)("em",null,"cancelled"),". Whether the execution of the remote subprogram is immediately aborted as a result of the cancellation is implementation defined. "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Implementation defined: "),"Whether the execution of the remote subprogram is immediately aborted as a result of cancellation.")),(0,n.kt)("p",null,"If a remote subprogram call is received by a called partition before the partition has completed its elaboration, the call is kept pending until the called partition completes its elaboration (unless the call is cancelled by the calling partition prior to that)."),(0,n.kt)("p",null,"If an exception is propagated by a remotely called subprogram, and the call is not an asynchronous call, the corresponding exception is reraised at the point of the remote subprogram call. For an asynchronous call, if the remote procedure call returns prior to the completion of the remotely called subprogram, any exception is lost."),(0,n.kt)("p",null,"The exception Communication_Error (see E.5) is raised if a remote call cannot be completed due to difficulties in communicating with the called partition."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0183-1"),"}"," All forms of remote subprogram calls are potentially blocking operations (see 9.5). "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Reason: "),"Asynchronous remote procedure calls are potentially blocking since the implementation may require waiting for the availability of shared resources to initiate the remote call. ")),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"8652/0085"),"}"," ","{",(0,n.kt)("em",null,"AI95-00215-01"),"}"," In a remote subprogram call with a formal parameter of a class-wide type, a check is made that the tag of the actual parameter identifies a tagged type declared in a declared-pure or shared passive library unit, or in the visible part of a remote types or remote call interface library unit. Program_Error is raised if this check fails. In a remote function call which returns a class-wide type, the same check is made on the function result. "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"{",(0,n.kt)("em",null,"8652/0085"),"}"," ","{",(0,n.kt)("em",null,"AI95-00215-01"),"}",' This check makes certain that the specific type passed or returned in an RPC satisfies the rules for a "communicable" type. Normally this is guaranteed by the compile-time restrictions on remote call interfaces. However, with class-wide types, it is possible to pass an object whose tag identifies a type declared outside the "safe" packages.')),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'This is considered an accessibility_check since only the types declared in "safe" packages are considered truly "global" (cross-partition). Other types are local to a single partition. This is analogous to the "accessibility" of global vs. local declarations in a single-partition program.')),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'This rule replaces a rule from an early version of Ada 9X which was given in the subclause on Remote Types Library Units (now E.2.2, "Remote Types Library Units"). That rule tried to prevent "bad" types from being sent by arranging for their tags to mismatch between partitions. However, that interfered with other uses of tags. The new rule allows tags to agree in all partitions, even for those types which are not "safe" to pass in an RPC. ')),(0,n.kt)("p",null,"In a dispatching call with two or more controlling operands that are designated by values of a remote access-to-class-wide type, a check is made [(in addition to the normal Tag_Check - see 11.5)] that all the remote access-to-class-wide values originated from Access ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0100"},"attribute_reference")),"s that were evaluated by tasks of the same active partition. Constraint_Error is raised if this check fails. "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Implementation Note: "),"When a remote access-to-class-wide value is created by an Access ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0100"},"attribute_reference")),", the identity of the active partition that evaluated the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-4/AA-4.1#S0100"},"attribute_reference"))," should be recorded in the representation of the remote access value. ")),(0,n.kt)("h4",u({},{id:"implementation-requirements"}),"Implementation Requirements"),(0,n.kt)("p",null,"The implementation of remote subprogram calls shall conform to the PCS interface as defined by the specification of the language-defined package System.RPC (see E.5). The calling stub shall use the Do_RPC procedure unless the remote procedure call is asynchronous in which case Do_APC shall be used. On the receiving side, the corresponding receiving stub shall be invoked by the RPC-receiver. "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Implementation Note: "),"One possible implementation model is as follows:")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"The code for calls to subprograms declared in an RCI package is generated normally, that is, the call-site is the same as for a local subprogram call. The code for the remotely callable subprogram bodies is also generated normally. Subprogram's prologue and epilogue are the same as for a local call.")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"When compiling the specification of an RCI package, the compiler generates calling stubs for each visible subprogram. Similarly, when compiling the body of an RCI package, the compiler generates receiving stubs for each visible subprogram together with the appropriate tables to allow the RPC-receiver to locate the correct receiving stub.")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"For the statically bound remote calls, the identity of the remote partition is statically determined (it is resolved at configuration/link time).")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"The calling stub operates as follows: ")),(0,n.kt)("p",null,"It allocates (or reuses) a stream of Params_Stream_Type of Initial_Size, and initializes it by repeatedly calling Write operations, first to identify which remote subprogram in the receiving partition is being called, and then to pass the incoming value of each of the ",(0,n.kt)("strong",null,"in")," and ",(0,n.kt)("strong",null,"in out")," parameters of the call."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," It allocates (or reuses) a stream for the Result, unless an aspect Asynchronous is specified as True for the procedure."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," It calls Do_RPC unless an aspect Asynchronous is specified as True for the procedure in which case it calls Do_APC. An access value designating the message stream allocated and initialized above is passed as the Params parameter. An access value designating the Result stream is passed as the Result parameter."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," If the aspect Asynchronous is not specified for the procedure, Do_RPC blocks until a reply message arrives, and then returns to the calling stub. The stub returns after extracting from the Result stream, using Read operations, the ",(0,n.kt)("strong",null,"in out")," and ",(0,n.kt)("strong",null,"out")," parameters or the function result. If the reply message indicates that the execution of the remote subprogram propagated an exception, the exception is propagated from Do_RPC to the calling stub, and thence to the point of the original remote subprogram call. If Do_RPC detects that communication with the remote partition has failed, it propagates Communication_Error."),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"On the receiving side, the RPC-receiver procedure operates as follows: ")),(0,n.kt)("p",null,"It is called from the PCS when a remote-subprogram-call message is received. The call originates in some remote call receiver task executed and managed in the context of the PCS."),(0,n.kt)("p",null,"It extracts information from the stream to identify the appropriate receiving stub."),(0,n.kt)("p",null,"The receiving stub extracts the ",(0,n.kt)("strong",null,"in")," and ",(0,n.kt)("strong",null,"in out")," parameters using Read from the stream designated by the Params parameter."),(0,n.kt)("p",null,"The receiving stub calls the actual subprogram body and, upon completion of the subprogram, uses Write to insert the results into the stream pointed to by the Result parameter. The receiving stub returns to the RPC-receiver procedure which in turn returns to the PCS. If the actual subprogram body propagates an exception, it is propagated by the RPC-receiver to the PCS, which handles the exception, and indicates in the reply message that the execution of the subprogram body propagated an exception. The exception occurrence can be represented in the reply message using the Write attribute of Ada.Exceptions.Exception_Occurrence. "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"For remote access-to-subprogram types:")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"A value of a remote access-to-subprogram type can be represented by the following components: a reference to the remote partition, an index to the package containing the remote subprogram, and an index to the subprogram within the package. The values of these components are determined at run time when the remote access value is created. These three components serve the same purpose when calling Do_APC/RPC, as in the statically bound remote calls; the only difference is that they are evaluated dynamically.")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"For remote access-to-class-wide types:")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"For each remote access-to-class-wide type, a calling stub is generated for each dispatching operation of the designated type. In addition, receiving stubs are generated to perform the remote dispatching operations in the called partition. The appropriate ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-6/AA-6.3#S0216"},"subprogram_body"))," is determined as for a local dispatching call once the receiving stub has been reached.")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"A value of a remote access-to-class-wide type can be represented with the following components: a reference to the remote partition, an index to a table (created one per each such access type) containing addresses of all the dispatching operations of the designated type, and an access value designating the actual remote object.")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,'Alternatively, a remote access-to-class-wide value can be represented as a normal access value, pointing to a "stub" object which in turn contains the information mentioned above. A call on any dispatching operation of such a stub object does the remote call, if necessary, using the information in the stub object to locate the target partition, etc. This approach has the advantage that less special-casing is required in the compiler. All access values can remain just a simple address.')),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"For a call to Do_RPC or Do_APC: The partition ID of all controlling operands are checked for equality (a Constraint_Error is raised if this check fails). The partition ID value is used for the Partition parameter. An index into the ",(0,n.kt)("em",null,"tagged-type-descriptor")," is created. This index points to the receiving stub of the class-wide operation. This index and the index to the table (described above) are written to the stream. Then, the actual parameters are marshalled into the message stream. For a controlling operand, only the access value designating the remote object is required (the other two components are already present in the other parameters).")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"On the called partition (after the RPC-receiver has transferred control to the appropriate receiving stub) the parameters are first unmarshalled. Then, the tags of the controlling operands (obtained by dereferencing the pointer to the object) are checked for equality. If the check fails Constraint_Error is raised and propagated back to the calling partition, unless it is a result of an asynchronous call. Finally, a dispatching call to the specific subprogram (based on the controlling object's tag) is made. Note that since this subprogram is not in an RCI package, no specific stub is generated for it, it is called normally from the ",(0,n.kt)("em",null,"dispatching stub"),".")),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"8652/0086"),"}"," ","{",(0,n.kt)("em",null,"AI95-00159-01"),"}"," With respect to shared variables in shared passive library units, the execution of the corresponding subprogram body of a synchronous remote procedure call is considered to be part of the execution of the calling task. The execution of the corresponding subprogram body of an asynchronous remote procedure call proceeds in parallel with the calling task and does not signal the next action of the calling task (see 9.10). "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"NOTE 1   A given active partition can both make and receive remote subprogram calls. Thus, an active partition can act as both a client and a server.")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"NOTE 2   If a given exception is propagated by a remote subprogram call, but the exception does not exist in the calling partition, the exception can be handled by an ",(0,n.kt)("strong",null,"others")," choice or be propagated to and handled by a third partition. ")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"This situation can happen in a case of dynamically nested remote subprogram calls, where an intermediate call executes in a partition that does not include the library unit that defines the exception. ")),(0,n.kt)("h4",u({},{id:"wording-changes-from-ada-95"}),"Wording Changes from Ada 95"),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"8652/0086"),"}"," ","{",(0,n.kt)("em",null,"AI95-00159-01"),"}"," ",(0,n.kt)("strong",null,"Corrigendum:")," Added rules so that tasks can safely access shared passive objects.")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"8652/0085"),"}"," ","{",(0,n.kt)("em",null,"AI95-00215-01"),"}"," ",(0,n.kt)("strong",null,"Corrigendum:")," Clarified that the check on class-wide types also applies to values returned from remote subprogram call functions. ")),(0,n.kt)("h4",u({},{id:"wording-changes-from-ada-2005"}),"Wording Changes from Ada 2005"),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0101-1"),"}"," ",(0,n.kt)("strong",null,"Correction:")," Corrected the text to note that remote access types can be defined in remote types units. ")),(0,n.kt)("h4",u({},{id:"wording-changes-from-ada-2012"}),"Wording Changes from Ada 2012"),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0283-1"),"}"," Added a rule to ensure that potentially blocking remote calls are not considered nonblocking.")),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0359-1"),"}"," Clarified that remote subprogram calls are always to active partitions. ")),(0,n.kt)("h2",u({},{id:"e41--asynchronous-remote-calls"}),"E.4.1  Asynchronous Remote Calls"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," [This subclause introduces the aspect Asynchronous which can be specified to allow a remote subprogram call to return prior to completion of the execution of the corresponding remote subprogram body.] "),(0,n.kt)("p",null,(0,n.kt)("em",null,"Paragraphs 2 through 7 were deleted.")," "),(0,n.kt)("h4",u({},{id:"static-semantics"}),"Static Semantics"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," For a remote procedure, the following language-defined representation aspect may be specified: "),(0,n.kt)("p",null,"AsynchronousThe type of aspect Asynchronous is Boolean. If directly specified, the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-13/AA-13.1#S0348"},"aspect_definition"))," shall be a static expression. If not specified, the aspect is False."),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Aspect Description for "),(0,n.kt)("strong",null,"Asynchronous: "),"Remote procedure calls are asynchronous; the caller continues without waiting for the call to return.")),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," For a remote access type, the following language-defined representation aspect may be specified: "),(0,n.kt)("p",null,"AsynchronousThe type of aspect Asynchronous is Boolean. If directly specified, the ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-13/AA-13.1#S0348"},"aspect_definition"))," shall be a static expression. If not specified (including by inheritance), the aspect is False. "),(0,n.kt)("h4",u({},{id:"legality-rules-1"}),"Legality Rules"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," If aspect Asynchronous is specified for a remote procedure, the formal parameters of the procedure shall all be of mode ",(0,n.kt)("strong",null,"in"),"."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," If aspect Asynchronous is specified for a remote access type, the type shall be a remote access-to-class-wide type, or the type shall be a remote access-to-procedure type with the formal parameters of the designated profile of the type all of mode ",(0,n.kt)("strong",null,"in"),". "),(0,n.kt)("h4",u({},{id:"dynamic-semantics-1"}),"Dynamic Semantics"),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," A remote call is ",(0,n.kt)("em",null,"asynchronous")," if it is a call to a procedure, or a call through a value of an access-to-procedure type, for which aspect Asynchronous is True. In addition, if aspect Asynchronous is True for a remote access-to-class-wide type, then a dispatching call on a procedure with a controlling operand designated by a value of the type is asynchronous if the formal parameters of the procedure are all of mode ",(0,n.kt)("strong",null,"in"),". "),(0,n.kt)("h4",u({},{id:"implementation-requirements-1"}),"Implementation Requirements"),(0,n.kt)("p",null,"Asynchronous remote procedure calls shall be implemented such that the corresponding body executes at most once as a result of the call. "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"To be honest: "),"It is not clear that this rule can be tested or even defined formally. ")),(0,n.kt)("h4",u({},{id:"extensions-to-ada-2005"}),"Extensions to Ada 2005"),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI05-0229-1"),"}"," Aspect Asynchronous is new; ",(0,n.kt)("code",null,(0,n.kt)("a",{href:"../AA-2/AA-2.8#S0019"},"pragma"))," Asynchronous is now obsolescent. ")),(0,n.kt)("h2",u({},{id:"e42--example-of-use-of-a-remote-access-to-class-wide-type"}),"E.4.2  Example of Use of a Remote Access-to-Class-Wide Type"),(0,n.kt)("h4",u({},{id:"examples"}),"Examples"),(0,n.kt)("p",null,(0,n.kt)("em",null,"Example of using a remote access-to-class-wide type to achieve dynamic binding across active partitions:")," "),(0,n.kt)(o.Z,{mdxType:"CodeBlock"},"{","AI12-0414-1","}"," package Tapes","\n","   with Pure is","\n","   type Tape is abstract tagged limited private;","\n","   -- Primitive dispatching operations where","\n","   -- Tape is controlling operand","\n","   procedure Copy (From, To : access Tape;","\n","                   Num_Recs : in Natural) is abstract;","\n","   procedure Rewind (T : access Tape) is abstract;","\n","   -- More operations","\n","private","\n","   type Tape is ...","\n","end Tapes;","\n"),(0,n.kt)(o.Z,{mdxType:"CodeBlock"},"{","AI12-0414-1","}"," with Tapes;","\n","package Name_Server","\n","   with Remote_Call_Interface is","\n","   -- Dynamic binding to remote operations is achieved","\n","   -- using the access-to-limited-class-wide type Tape_Ptr","\n","   type Tape_Ptr is access all Tapes.Tape'Class;","\n","   -- The following statically bound remote operations","\n","   -- allow for a name-server capability in this example","\n","   function  Find     (Name : String) return Tape_Ptr;","\n","   procedure Register (Name : in String; T : in Tape_Ptr);","\n","   procedure Remove   (T : in Tape_Ptr);","\n","   -- More operations","\n","end Name_Server;","\n"),(0,n.kt)(o.Z,{mdxType:"CodeBlock"},"package Tape_Driver is","\n","  -- Declarations are not shown, they are irrelevant here","\n","end Tape_Driver;","\n"),(0,n.kt)(o.Z,{mdxType:"CodeBlock"},"{","AI12-0347-1","}"," with Tapes, Name_Server;","\n","package body Tape_Driver is","\n","   type New_Tape is new Tapes.Tape with ...","\n","   overriding","\n","   procedure Rewind (T : access New_Tape);","\n","   overriding","\n","   procedure Copy","\n","    (From, To : access New_Tape; Num_Recs: in Natural) is","\n","   begin","\n","     . . .","\n","   end Copy;","\n","   procedure Rewind (T : access New_Tape) is","\n","   begin","\n","      . . .","\n","   end Rewind;","\n","   -- Objects remotely accessible through use","\n","   -- of Name_Server operations","\n","   Tape1, Tape2 : aliased New_Tape;","\n","begin","\n",'   Name_Server.Register ("NINE-TRACK",  Tape1\'Access);',"\n",'   Name_Server.Register ("SEVEN-TRACK", Tape2\'Access);',"\n","end Tape_Driver;","\n"),(0,n.kt)(o.Z,{mdxType:"CodeBlock"},"with Tapes, Name_Server;","\n","-- Tape_Driver is not needed and thus not mentioned in the ",(0,n.kt)("a",{href:"../AA-10/AA-10.1#S0294"},"with_clause"),"\n","procedure Tape_Client is","\n","   T1, T2 : Name_Server.Tape_Ptr;","\n","begin","\n",'   T1 := Name_Server.Find ("NINE-TRACK");',"\n",'   T2 := Name_Server.Find ("SEVEN-TRACK");',"\n","   Tapes.Rewind (T1);","\n","   Tapes.Rewind (T2);","\n","   Tapes.Copy (T1, T2, 3);","\n","end Tape_Client;","\n"),(0,n.kt)("p",null,(0,n.kt)("em",null,"Notes on the example"),": "),(0,n.kt)(i.Z,{type:"note",mdxType:"Admonition"},(0,n.kt)("p",null,(0,n.kt)("strong",null,"Discussion: "),"The example does not show the case where tapes are removed from or added to the system. In the former case, an appropriate exception needs to be defined to instruct the client to use another tape. In the latter, the Name_Server should have a query function visible to the clients to inform them about the availability of the tapes in the system. ")),(0,n.kt)("p",null,(0,n.kt)("em",null,"This paragraph was deleted.")),(0,n.kt)("p",null,"The package Tapes provides the necessary declarations of the type and its primitive operations."),(0,n.kt)("p",null,"Name_Server is a remote call interface package and is elaborated in a separate active partition to provide the necessary naming services (such as Register and Find) to the entire distributed program through remote subprogram calls."),(0,n.kt)("p",null,"Tape_Driver is a normal package that is elaborated in a partition configured on the processing node that is connected to the tape device(s). The abstract operations are overridden to support the locally declared tape devices (Tape1, Tape2). The package is not visible to its clients, but it exports the tape devices (as remote objects) through the services of the Name_Server. This allows for tape devices to be dynamically added, removed or replaced without requiring the modification of the clients' code."),(0,n.kt)("p",null,"{",(0,n.kt)("em",null,"AI12-0442-1"),"}"," The Tape_Client procedure references only declarations in the Tapes and Name_Server packages. Before using a tape for the first time, it will query the Name_Server for a system-wide identity for that tape. From then on, it can use that identity to access the tape device."),(0,n.kt)("p",null,"Values of remote access type Tape_Ptr include the necessary information to complete the remote dispatching operations that result from dereferencing the controlling operands T1 and T2."))}A.isMDXComponent=!0}}]);