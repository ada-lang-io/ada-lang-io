---
sidebar_position:  172
---

# E.4  Remote Subprogram Calls

{AI12-0359-1} A remote subprogram call is a subprogram call that invokes the execution of a subprogram in another (active) partition. The partition that originates the remote subprogram call is the calling partition, and the partition that executes the corresponding subprogram body is the called partition. Some remote procedure calls are allowed to return prior to the completion of subprogram execution. These are called asynchronous remote procedure calls.

Discussion: {AI12-0359-1} Remote subprogram calls are always between active partitions; a passive partition has no execution resources of its own and thus cannot execute anything, while a remote subprogram call is always executed by the called partition. 

There are three different ways of performing a remote subprogram call: 

As a direct call on a (remote) subprogram explicitly declared in a remote call interface;

As an indirect call through a value of a remote access-to-subprogram type;

As a dispatching call with a controlling operand designated by a value of a remote access-to-class-wide type. 

The first way of calling corresponds to a static binding between the calling and the called partition. The latter two ways correspond to a dynamic binding between the calling and the called partition.

{AI05-0101-1} Remote types library units (see E.2.2) and remote call interface library units (see E.2.3) define the remote subprograms or remote access types used for remote subprogram calls. 


#### Language Design Principles

Remote subprogram calls are standardized since the RPC paradigm is widely-used, and establishing an interface to it in the annex will increase the portability and reusability of distributed programs.


#### Legality Rules

In a dispatching call with two or more controlling operands, if one controlling operand is designated by a value of a remote access-to-class-wide type, then all shall be.

{AI12-0283-1} A nonblocking program unit shall not contain, other than within nested units with Nonblocking specified as statically False, a dispatching call with a controlling operand designated by a value of a remote access-to-class-wide type.

Reason: Such a dispatching call is a potentially blocking call (see below) even if the called subprogram is nonblocking, so we must not assert that no blocking is possible. 

Ramification: The calls is illegal if the Nonblocking aspect of the containing unit is True, either implicitly by inheritance or by explicit specification. 


#### Dynamic Semantics

For the execution of a remote subprogram call, subprogram parameters (and later the results, if any) are passed using a stream-oriented representation (see 13.13.1) [which is suitable for transmission between partitions]. This action is called marshalling. Unmarshalling is the reverse action of reconstructing the parameters or results from the stream-oriented representation. [Marshalling is performed initially as part of the remote subprogram call in the calling partition; unmarshalling is done in the called partition. After the remote subprogram completes, marshalling is performed in the called partition, and finally unmarshalling is done in the calling partition.]

A calling stub is the sequence of code that replaces the subprogram body of a remotely called subprogram in the calling partition. A receiving stub is the sequence of code (the "wrapper") that receives a remote subprogram call on the called partition and invokes the appropriate subprogram body. 

Discussion: The use of the term stub in this annex should not be confused with [body_stub](./AA-10.1#S0297) as defined in 10.1.3. The term stub is used here because it is a commonly understood term when talking about the RPC paradigm. 

Remote subprogram calls are executed at most once, that is, if the subprogram call returns normally, then the called subprogram's body was executed exactly once.

The task executing a remote subprogram call blocks until the subprogram in the called partition returns, unless the call is asynchronous. For an asynchronous remote procedure call, the calling task can become ready before the procedure in the called partition returns.

If a construct containing a remote call is aborted, the remote subprogram call is cancelled. Whether the execution of the remote subprogram is immediately aborted as a result of the cancellation is implementation defined. 

Implementation defined: Whether the execution of the remote subprogram is immediately aborted as a result of cancellation.

If a remote subprogram call is received by a called partition before the partition has completed its elaboration, the call is kept pending until the called partition completes its elaboration (unless the call is cancelled by the calling partition prior to that).

If an exception is propagated by a remotely called subprogram, and the call is not an asynchronous call, the corresponding exception is reraised at the point of the remote subprogram call. For an asynchronous call, if the remote procedure call returns prior to the completion of the remotely called subprogram, any exception is lost.

The exception Communication_Error (see E.5) is raised if a remote call cannot be completed due to difficulties in communicating with the called partition.

{AI12-0183-1} All forms of remote subprogram calls are potentially blocking operations (see 9.5). 

Reason: Asynchronous remote procedure calls are potentially blocking since the implementation may require waiting for the availability of shared resources to initiate the remote call. 

{8652/0085} {AI95-00215-01} In a remote subprogram call with a formal parameter of a class-wide type, a check is made that the tag of the actual parameter identifies a tagged type declared in a declared-pure or shared passive library unit, or in the visible part of a remote types or remote call interface library unit. Program_Error is raised if this check fails. In a remote function call which returns a class-wide type, the same check is made on the function result. 

Discussion: {8652/0085} {AI95-00215-01} This check makes certain that the specific type passed or returned in an RPC satisfies the rules for a "communicable" type. Normally this is guaranteed by the compile-time restrictions on remote call interfaces. However, with class-wide types, it is possible to pass an object whose tag identifies a type declared outside the "safe" packages.

This is considered an accessibility_check since only the types declared in "safe" packages are considered truly "global" (cross-partition). Other types are local to a single partition. This is analogous to the "accessibility" of global vs. local declarations in a single-partition program.

This rule replaces a rule from an early version of Ada 9X which was given in the subclause on Remote Types Library Units (now E.2.2, "Remote Types Library Units"). That rule tried to prevent "bad" types from being sent by arranging for their tags to mismatch between partitions. However, that interfered with other uses of tags. The new rule allows tags to agree in all partitions, even for those types which are not "safe" to pass in an RPC. 

In a dispatching call with two or more controlling operands that are designated by values of a remote access-to-class-wide type, a check is made [(in addition to the normal Tag_Check - see 11.5)] that all the remote access-to-class-wide values originated from Access [attribute_reference](./AA-4.1#S0100)s that were evaluated by tasks of the same active partition. Constraint_Error is raised if this check fails. 

Implementation Note: When a remote access-to-class-wide value is created by an Access [attribute_reference](./AA-4.1#S0100), the identity of the active partition that evaluated the [attribute_reference](./AA-4.1#S0100) should be recorded in the representation of the remote access value. 


#### Implementation Requirements

The implementation of remote subprogram calls shall conform to the PCS interface as defined by the specification of the language-defined package System.RPC (see E.5). The calling stub shall use the Do_RPC procedure unless the remote procedure call is asynchronous in which case Do_APC shall be used. On the receiving side, the corresponding receiving stub shall be invoked by the RPC-receiver. 

Implementation Note: One possible implementation model is as follows:

The code for calls to subprograms declared in an RCI package is generated normally, that is, the call-site is the same as for a local subprogram call. The code for the remotely callable subprogram bodies is also generated normally. Subprogram's prologue and epilogue are the same as for a local call.

When compiling the specification of an RCI package, the compiler generates calling stubs for each visible subprogram. Similarly, when compiling the body of an RCI package, the compiler generates receiving stubs for each visible subprogram together with the appropriate tables to allow the RPC-receiver to locate the correct receiving stub.

For the statically bound remote calls, the identity of the remote partition is statically determined (it is resolved at configuration/link time).

The calling stub operates as follows: 

It allocates (or reuses) a stream of Params_Stream_Type of Initial_Size, and initializes it by repeatedly calling Write operations, first to identify which remote subprogram in the receiving partition is being called, and then to pass the incoming value of each of the in and in out parameters of the call.

{AI05-0229-1} It allocates (or reuses) a stream for the Result, unless an aspect Asynchronous is specified as True for the procedure.

{AI05-0229-1} It calls Do_RPC unless an aspect Asynchronous is specified as True for the procedure in which case it calls Do_APC. An access value designating the message stream allocated and initialized above is passed as the Params parameter. An access value designating the Result stream is passed as the Result parameter.

{AI05-0229-1} If the aspect Asynchronous is not specified for the procedure, Do_RPC blocks until a reply message arrives, and then returns to the calling stub. The stub returns after extracting from the Result stream, using Read operations, the in out and out parameters or the function result. If the reply message indicates that the execution of the remote subprogram propagated an exception, the exception is propagated from Do_RPC to the calling stub, and thence to the point of the original remote subprogram call. If Do_RPC detects that communication with the remote partition has failed, it propagates Communication_Error.

On the receiving side, the RPC-receiver procedure operates as follows: 

It is called from the PCS when a remote-subprogram-call message is received. The call originates in some remote call receiver task executed and managed in the context of the PCS.

It extracts information from the stream to identify the appropriate receiving stub.

The receiving stub extracts the in and in out parameters using Read from the stream designated by the Params parameter.

The receiving stub calls the actual subprogram body and, upon completion of the subprogram, uses Write to insert the results into the stream pointed to by the Result parameter. The receiving stub returns to the RPC-receiver procedure which in turn returns to the PCS. If the actual subprogram body propagates an exception, it is propagated by the RPC-receiver to the PCS, which handles the exception, and indicates in the reply message that the execution of the subprogram body propagated an exception. The exception occurrence can be represented in the reply message using the Write attribute of Ada.Exceptions.Exception_Occurrence. 

For remote access-to-subprogram types:

A value of a remote access-to-subprogram type can be represented by the following components: a reference to the remote partition, an index to the package containing the remote subprogram, and an index to the subprogram within the package. The values of these components are determined at run time when the remote access value is created. These three components serve the same purpose when calling Do_APC/RPC, as in the statically bound remote calls; the only difference is that they are evaluated dynamically.

For remote access-to-class-wide types:

For each remote access-to-class-wide type, a calling stub is generated for each dispatching operation of the designated type. In addition, receiving stubs are generated to perform the remote dispatching operations in the called partition. The appropriate [subprogram_body](./AA-6.3#S0216) is determined as for a local dispatching call once the receiving stub has been reached.

A value of a remote access-to-class-wide type can be represented with the following components: a reference to the remote partition, an index to a table (created one per each such access type) containing addresses of all the dispatching operations of the designated type, and an access value designating the actual remote object.

Alternatively, a remote access-to-class-wide value can be represented as a normal access value, pointing to a "stub" object which in turn contains the information mentioned above. A call on any dispatching operation of such a stub object does the remote call, if necessary, using the information in the stub object to locate the target partition, etc. This approach has the advantage that less special-casing is required in the compiler. All access values can remain just a simple address.

For a call to Do_RPC or Do_APC: The partition ID of all controlling operands are checked for equality (a Constraint_Error is raised if this check fails). The partition ID value is used for the Partition parameter. An index into the tagged-type-descriptor is created. This index points to the receiving stub of the class-wide operation. This index and the index to the table (described above) are written to the stream. Then, the actual parameters are marshalled into the message stream. For a controlling operand, only the access value designating the remote object is required (the other two components are already present in the other parameters).

On the called partition (after the RPC-receiver has transferred control to the appropriate receiving stub) the parameters are first unmarshalled. Then, the tags of the controlling operands (obtained by dereferencing the pointer to the object) are checked for equality. If the check fails Constraint_Error is raised and propagated back to the calling partition, unless it is a result of an asynchronous call. Finally, a dispatching call to the specific subprogram (based on the controlling object's tag) is made. Note that since this subprogram is not in an RCI package, no specific stub is generated for it, it is called normally from the dispatching stub.

{8652/0086} {AI95-00159-01} With respect to shared variables in shared passive library units, the execution of the corresponding subprogram body of a synchronous remote procedure call is considered to be part of the execution of the calling task. The execution of the corresponding subprogram body of an asynchronous remote procedure call proceeds in parallel with the calling task and does not signal the next action of the calling task (see 9.10). 

NOTE 1   A given active partition can both make and receive remote subprogram calls. Thus, an active partition can act as both a client and a server.

NOTE 2   If a given exception is propagated by a remote subprogram call, but the exception does not exist in the calling partition, the exception can be handled by an others choice or be propagated to and handled by a third partition. 

Discussion: This situation can happen in a case of dynamically nested remote subprogram calls, where an intermediate call executes in a partition that does not include the library unit that defines the exception. 


#### Wording Changes from Ada 95

{8652/0086} {AI95-00159-01} Corrigendum: Added rules so that tasks can safely access shared passive objects.

{8652/0085} {AI95-00215-01} Corrigendum: Clarified that the check on class-wide types also applies to values returned from remote subprogram call functions. 


#### Wording Changes from Ada 2005

{AI05-0101-1} Correction: Corrected the text to note that remote access types can be defined in remote types units. 


#### Wording Changes from Ada 2012

{AI12-0283-1} Added a rule to ensure that potentially blocking remote calls are not considered nonblocking.

{AI12-0359-1} Clarified that remote subprogram calls are always to active partitions. 


## E.4.1  Asynchronous Remote Calls

{AI05-0229-1} [This subclause introduces the aspect Asynchronous which can be specified to allow a remote subprogram call to return prior to completion of the execution of the corresponding remote subprogram body.] 

Paragraphs 2 through 7 were deleted. 


#### Static Semantics

{AI05-0229-1} For a remote procedure, the following language-defined representation aspect may be specified: 

AsynchronousThe type of aspect Asynchronous is Boolean. If directly specified, the [aspect_definition](./AA-13.1#S0348) shall be a static expression. If not specified, the aspect is False.

Aspect Description for Asynchronous: Remote procedure calls are asynchronous; the caller continues without waiting for the call to return.

{AI05-0229-1} For a remote access type, the following language-defined representation aspect may be specified: 

AsynchronousThe type of aspect Asynchronous is Boolean. If directly specified, the [aspect_definition](./AA-13.1#S0348) shall be a static expression. If not specified (including by inheritance), the aspect is False. 


#### Legality Rules

{AI05-0229-1} If aspect Asynchronous is specified for a remote procedure, the formal parameters of the procedure shall all be of mode in.

{AI05-0229-1} If aspect Asynchronous is specified for a remote access type, the type shall be a remote access-to-class-wide type, or the type shall be a remote access-to-procedure type with the formal parameters of the designated profile of the type all of mode in. 


#### Dynamic Semantics

{AI05-0229-1} A remote call is asynchronous if it is a call to a procedure, or a call through a value of an access-to-procedure type, for which aspect Asynchronous is True. In addition, if aspect Asynchronous is True for a remote access-to-class-wide type, then a dispatching call on a procedure with a controlling operand designated by a value of the type is asynchronous if the formal parameters of the procedure are all of mode in. 


#### Implementation Requirements

Asynchronous remote procedure calls shall be implemented such that the corresponding body executes at most once as a result of the call. 

To be honest: It is not clear that this rule can be tested or even defined formally. 


#### Extensions to Ada 2005

{AI05-0229-1} Aspect Asynchronous is new; [pragma](./AA-2.8#S0019) Asynchronous is now obsolescent. 


## E.4.2  Example of Use of a Remote Access-to-Class-Wide Type


#### Examples

Example of using a remote access-to-class-wide type to achieve dynamic binding across active partitions: 

```ada
{AI12-0414-1} package Tapes
   with Pure is
   type Tape is abstract tagged limited private;
   -- Primitive dispatching operations where
   -- Tape is controlling operand
   procedure Copy (From, To : access Tape;
                   Num_Recs : in Natural) is abstract;
   procedure Rewind (T : access Tape) is abstract;
   -- More operations
private
   type Tape is ...
end Tapes;

```

```ada
{AI12-0414-1} with Tapes;
package Name_Server
   with Remote_Call_Interface is
   -- Dynamic binding to remote operations is achieved
   -- using the access-to-limited-class-wide type Tape_Ptr
   type Tape_Ptr is access all Tapes.Tape'Class;
   -- The following statically bound remote operations
   -- allow for a name-server capability in this example
   function  Find     (Name : String) return Tape_Ptr;
   procedure Register (Name : in String; T : in Tape_Ptr);
   procedure Remove   (T : in Tape_Ptr);
   -- More operations
end Name_Server;

```

```ada
package Tape_Driver is
  -- Declarations are not shown, they are irrelevant here
end Tape_Driver;

```

```ada
{AI12-0347-1} with Tapes, Name_Server;
package body Tape_Driver is
   type New_Tape is new Tapes.Tape with ...
   overriding
   procedure Rewind (T : access New_Tape);
   overriding
   procedure Copy
    (From, To : access New_Tape; Num_Recs: in Natural) is
   begin
     . . .
   end Copy;
   procedure Rewind (T : access New_Tape) is
   begin
      . . .
   end Rewind;
   -- Objects remotely accessible through use
   -- of Name_Server operations
   Tape1, Tape2 : aliased New_Tape;
begin
   Name_Server.Register ("NINE-TRACK",  Tape1'Access);
   Name_Server.Register ("SEVEN-TRACK", Tape2'Access);
end Tape_Driver;

```

```ada
with Tapes, Name_Server;
-- Tape_Driver is not needed and thus not mentioned in the [with_clause](./AA-10.1#S0294)
procedure Tape_Client is
   T1, T2 : Name_Server.Tape_Ptr;
begin
   T1 := Name_Server.Find ("NINE-TRACK");
   T2 := Name_Server.Find ("SEVEN-TRACK");
   Tapes.Rewind (T1);
   Tapes.Rewind (T2);
   Tapes.Copy (T1, T2, 3);
end Tape_Client;

```

Notes on the example: 

Discussion: The example does not show the case where tapes are removed from or added to the system. In the former case, an appropriate exception needs to be defined to instruct the client to use another tape. In the latter, the Name_Server should have a query function visible to the clients to inform them about the availability of the tapes in the system. 

This paragraph was deleted.

The package Tapes provides the necessary declarations of the type and its primitive operations.

Name_Server is a remote call interface package and is elaborated in a separate active partition to provide the necessary naming services (such as Register and Find) to the entire distributed program through remote subprogram calls.

Tape_Driver is a normal package that is elaborated in a partition configured on the processing node that is connected to the tape device(s). The abstract operations are overridden to support the locally declared tape devices (Tape1, Tape2). The package is not visible to its clients, but it exports the tape devices (as remote objects) through the services of the Name_Server. This allows for tape devices to be dynamically added, removed or replaced without requiring the modification of the clients' code.

{AI12-0442-1} The Tape_Client procedure references only declarations in the Tapes and Name_Server packages. Before using a tape for the first time, it will query the Name_Server for a system-wide identity for that tape. From then on, it can use that identity to access the tape device.

Values of remote access type Tape_Ptr include the necessary information to complete the remote dispatching operations that result from dereferencing the controlling operands T1 and T2.

