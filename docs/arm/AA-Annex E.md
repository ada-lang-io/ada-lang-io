---
sidebar_position:  19
---

# Annex E Distributed Systems

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[This Annex defines facilities for supporting the implementation of distributed systems using multiple partitions working cooperatively as part of a single Ada program.] 


#### Extensions to Ada 83

This Annex is new to Ada 95. 


#### Post-Compilation Rules

A distributed system is an interconnection of one or more processing nodes (a system resource that has both computational and storage capabilities), and zero or more storage nodes (a system resource that has only storage capabilities, with the storage addressable by one or more processing nodes).

A distributed program comprises one or more partitions that execute independently (except when they communicate) in a distributed system.

The process of mapping the partitions of a program to the nodes in a distributed system is called configuring the partitions of the program. 


#### Implementation Requirements

The implementation shall provide means for explicitly assigning library units to a partition and for the configuring and execution of a program consisting of multiple partitions on a distributed system; the means are implementation defined. 

Implementation defined: The means for creating and executing distributed programs.


#### Implementation Permissions

An implementation may require that the set of processing nodes of a distributed system be homogeneous.

NOTE 1   The partitions comprising a program may be executed on differently configured distributed systems or on a nondistributed system without requiring recompilation. A distributed program may be partitioned differently from the same set of library units without recompilation. The resulting execution is semantically equivalent.

NOTE 2   A distributed program retains the same type safety as the equivalent single partition program.


## E.1  Partitions

[The partitions of a distributed program are classified as either active or passive.] 


#### Post-Compilation Rules

An active partition is a partition as defined in 10.2. A passive partition is a partition that has no thread of control of its own, whose library units are all preelaborated, and whose data and subprograms are accessible to one or more active partitions. 

Discussion: In most situations, a passive partition does not execute, and does not have a "real" environment task. Any execution involved in its elaboration and initialization occurs before it comes into existence in a distributed program (like most preelaborated entities). Likewise, there is no concrete meaning to passive partition termination. 

A passive partition shall include only [library_item](S0216)s that either are declared pure or are shared passive (see 10.2.1 and E.2.1).

An active partition shall be configured on a processing node. A passive partition shall be configured either on a storage node or on a processing node.

The configuration of the partitions of a program onto a distributed system shall be consistent with the possibility for data references or calls between the partitions implied by their semantic dependences. Any reference to data or call of a subprogram across partitions is called a remote access. 

Discussion: For example, an active partition that includes a unit with a semantic dependence on the declaration of another RCI package of some other active partition has to be connected to that other partition by some sort of a message passing mechanism.

A passive partition that is accessible to an active partition should have its storage addressable to the processor(s) of the active partition. The processor(s) should be able to read and write from/to that storage, as well as to perform "read-modify-write" operations (in order to support entry-less protected objects).


#### Dynamic Semantics

A [library_item](S0216) is elaborated as part of the elaboration of each partition that includes it. If a normal library unit (see E.2) has state, then a separate copy of the state exists in each active partition that elaborates it. [The state evolves independently in each such partition.]

Ramification: Normal library units cannot be included in passive partitions. 

[An active partition terminates when its environment task terminates.] A partition becomes inaccessible if it terminates or if it is aborted. An active partition is aborted when its environment task is aborted. In addition, if a partition fails during its elaboration, it becomes inaccessible to other partitions. Other implementation-defined events can also result in a partition becoming inaccessible. 

Implementation defined: Any events that can result in a partition becoming inaccessible.

For a prefix D that denotes a library-level declaration, excepting a declaration of or within a declared-pure library unit, the following attribute is defined: 

D'Partition_IdDenotes a value of the type universal_integer that identifies the partition in which D was elaborated. If D denotes the declaration of a remote call interface library unit (see E.2.3) the given partition is the one where the body of D was elaborated. 


#### Bounded (Run-Time) Errors

It is a bounded error for there to be cyclic elaboration dependences between the active partitions of a single distributed program. The possible effects are deadlock during elaboration, or the raising of Program_Error in one or all of the active partitions involved. 


#### Implementation Permissions

An implementation may allow multiple active or passive partitions to be configured on a single processing node, and multiple passive partitions to be configured on a single storage node. In these cases, the scheduling policies, treatment of priorities, and management of shared resources between these partitions are implementation defined. 

Implementation defined: The scheduling policies, treatment of priorities, and management of shared resources between partitions in certain cases.

An implementation may allow separate copies of an active partition to be configured on different processing nodes, and to provide appropriate interactions between the copies to present a consistent state of the partition to other active partitions. 

Ramification: The language does not specify the nature of these interactions, nor the actual level of consistency preserved. 

In an implementation, the partitions of a distributed program need not be loaded and elaborated all at the same time; they may be loaded and elaborated one at a time over an extended period of time. An implementation may provide facilities to abort and reload a partition during the execution of a distributed program.

An implementation may allow the state of some of the partitions of a distributed program to persist while other partitions of the program terminate and are later reinvoked. 

NOTE 1   Library units are grouped into partitions after compile time, but before run time. At compile time, only the relevant library unit properties are identified using categorization pragmas.

NOTE 2   The value returned by the Partition_Id attribute can be used as a parameter to implementation-provided subprograms in order to query information about the partition.


## E.2  Categorization of Library Units

[Library units can be categorized according to the role they play in a distributed program. Certain restrictions are associated with each category to ensure that the semantics of a distributed program remain close to the semantics for a nondistributed program.]

A categorization pragma is a library unit pragma (see 10.1.5) that restricts the declarations, child units, or semantic dependences of the library unit to which it applies. A categorized library unit is a library unit to which a categorization pragma applies.

The pragmas Shared_Passive, Remote_Types, and Remote_Call_Interface are categorization pragmas. In addition, for the purposes of this Annex, the pragma Pure (see 10.2.1) is considered a categorization pragma.

[ A library package or generic library package is called a shared passive library unit if a Shared_Passive pragma applies to it.  A library package or generic library package is called a remote types library unit if a Remote_Types pragma applies to it.  A library package or generic library package is called a remote call interface if a Remote_Call_Interface pragma applies to it.] A normal library unit is one to which no categorization pragma applies. 

[The various categories of library units and the associated restrictions are described in this clause and its subclauses. The categories are related hierarchically in that the library units of one category can depend semantically only on library units of that category or an earlier one, except that the body of a remote types or remote call interface library unit is unrestricted.

The overall hierarchy (including declared pure) is as follows: 

Declared Pure Can depend only on other declared pure library units;

Shared Passive Can depend only on other shared passive or declared pure library units;

Remote Types The declaration of the library unit can depend only on other remote types library units, or one of the above library unit categories, or limited views, or preelaborated normal library units that are mentioned only in private with clauses; the body of the library unit is unrestricted;

Remote Call Interface The declaration of the library unit can depend only on other remote call interfaces, or one of the above; the body of the library unit is unrestricted;

Normal Unrestricted. 

Declared pure and shared passive library units are preelaborated. The declaration of a remote types or remote call interface library unit is required to be preelaborable. ]


#### Implementation Requirements

For a given library-level type declared in a preelaborated library unit or in the declaration of a remote types or remote call interface library unit, the implementation shall choose the same representation for the type upon each elaboration of the type's declaration for different partitions of the same program. 


#### Implementation Permissions

Implementations are allowed to define other categorization pragmas.


### E.2.1  Shared Passive Library Units

[A shared passive library unit is used for managing global data shared between active partitions. The restrictions on shared passive library units prevent the data or tasks of one active partition from being accessible to another active partition through references implicit in objects declared in the shared passive library unit.] 


#### Language Design Principles

The restrictions governing a shared passive library unit are designed to ensure that objects and subprograms declared in the package can be used safely from multiple active partitions, even though the active partitions live in different address spaces, and have separate run-time systems. 


#### Syntax

The form of a [pragma](S0016) Shared_Passive is as follows: 

  pragma Shared_Passive[(library_unit_[name](S0084))];


#### Legality Rules

A shared passive library unit is a library unit to which a Shared_Passive pragma applies. The following restrictions apply to such a library unit:

[it shall be preelaborable (see 10.2.1);] 

Ramification: It cannot contain library-level declarations of protected objects with entries, nor of task objects. Task objects are disallowed because passive partitions don't have any threads of control of their own, nor any run-time system of their own. Protected objects with entries are disallowed because an entry queue contains references to calling tasks, and that would require in effect a pointer from a passive partition back to a task in some active partition. 

it shall depend semantically only upon declared pure or shared passive library units; 

Reason: Shared passive packages cannot depend semantically upon remote types packages because the values of an access type declared in a remote types package refer to the local heap of the active partition including the remote types package. 

it shall not contain a library-level declaration of an access type that designates a class-wide type, task type, or protected type with [entry_declaration](S0187)s; if the shared passive library unit is generic, it shall not contain a declaration for such an access type unless the declaration is nested within a body other than a [package_body](S0163).

Reason: These kinds of access types are disallowed because the object designated by an access value of such a type could contain an implicit reference back to the active partition on whose behalf the designated object was created. 

Notwithstanding the definition of accessibility given in 3.10.2, the declaration of a library unit P1 is not accessible from within the declarative region of a shared passive library unit P2, unless the shared passive library unit P2 depends semantically on P1. 

Discussion: We considered a more complex rule, but dropped it. This is the simplest rule that recognizes that a shared passive package may outlive some other library package, unless it depends semantically on that package. In a nondistributed program, all library packages are presumed to have the same lifetime.

Implementations may define additional pragmas that force two library packages to be in the same partition, or to have the same lifetime, which would allow this rule to be relaxed in the presence of such pragmas. 


#### Static Semantics

A shared passive library unit is preelaborated.


#### Post-Compilation Rules

A shared passive library unit shall be assigned to at most one partition within a given program.

Notwithstanding the rule given in 10.2, a compilation unit in a given partition does not need (in the sense of 10.2) the shared passive library units on which it depends semantically to be included in that same partition; they will typically reside in separate passive partitions.


### E.2.2  Remote Types Library Units

[A remote types library unit supports the definition of types intended for use in communication between active partitions.] 


#### Language Design Principles

The restrictions governing a remote types package are similar to those for a declared pure package. However, the restrictions are relaxed deliberately to allow such a package to contain declarations that violate the stateless property of pure packages, though it is presumed that any state-dependent properties are essentially invisible outside the package. 


#### Syntax

The form of a [pragma](S0016) Remote_Types is as follows: 

  pragma Remote_Types[(library_unit_[name](S0084))];


#### Legality Rules

A remote types library unit is a library unit to which the pragma Remote_Types applies. The following restrictions apply to such a library unit:

[it shall be preelaborable;]

it shall depend semantically only on declared pure, shared passive, or other remote types library units;

it shall not contain the declaration of any variable within the visible part of the library unit; 

Reason: This is essentially a "methodological" restriction. A separate copy of a remote types package is included in each partition that references it, just like a normal package. Nevertheless, a remote types package is thought of as an "essentially pure" package for defining types to be used for interpartition communication, and it could be misleading to declare visible objects when no remote data access is actually being provided. 

if the full view of a type declared in the visible part of the library unit has a part that is of a nonremote access type, then that access type, or the type of some part that includes the access type subcomponent, shall have user-specified Read and Write attributes. 

Reason: This is to prevent the use of the predefined Read and Write attributes of an access type as part of the Read and Write attributes of a visible type. 

An access type declared in the visible part of a remote types or remote call interface library unit is called a remote access type. Such a type shall be either an access-to-subprogram type or a general access type that designates a class-wide limited private type.

The following restrictions apply to the use of a remote access-to-subprogram type: 

A value of a remote access-to-subprogram type shall be converted only to another (subtype-conformant) remote access-to-subprogram type;

The [prefix](S0086) of an Access [attribute_reference](S0093) that yields a value of a remote access-to-subprogram type shall statically denote a (subtype-conformant) remote subprogram. 

The following restrictions apply to the use of a remote access-to-class-wide type: 

The primitive subprograms of the corresponding specific limited private type shall only have access parameters if they are controlling formal parameters; the types of all the noncontrolling formal parameters shall have  Read and Write attributes.

A value of a remote access-to-class-wide type shall be explicitly converted only to another remote access-to-class-wide type;

A value of a remote access-to-class-wide type shall be dereferenced (or implicitly converted to an anonymous access type) only as part of a dispatching call where the value designates a controlling operand of the call (see E.4, "Remote Subprogram Calls"); 

The Storage_Pool and Storage_Size attributes are not defined for remote access-to-class-wide types; the expected type for an [allocator](S0122) shall not be a remote access-to-class-wide type; a remote access-to-class-wide type shall not be an actual parameter for a generic formal access type; 

Reason: All three of these restrictions are because there is no storage pool associated with a remote access-to-class-wide type. 

NOTE 1   A remote types library unit need not be pure, and the types it defines may include levels of indirection implemented by using access types. User-specified Read and Write attributes (see 13.13.2) provide for sending values of such a type between active partitions, with Write marshalling the representation, and Read unmarshalling any levels of indirection.


### E.2.3  Remote Call Interface Library Units

[A remote call interface library unit can be used as an interface for remote procedure calls (RPCs) (or remote function calls) between active partitions.] 


#### Language Design Principles

The restrictions governing a remote call interface library unit are intended to ensure that the values of the actual parameters in a remote call can be meaningfully sent between two active partitions. 


#### Syntax

The form of a [pragma](S0016) Remote_Call_Interface is as follows: 

  pragma Remote_Call_Interface[(library_unit_[name](S0084))];

The form of a [pragma](S0016) All_Calls_Remote is as follows: 

  pragma All_Calls_Remote[(library_unit_[name](S0084))];

A [pragma](S0016) All_Calls_Remote is a library unit pragma. 


#### Legality Rules

A remote call interface (RCI) is a library unit to which the pragma Remote_Call_Interface applies. A subprogram declared in the visible part of such a library unit is called a remote subprogram.

The declaration of an RCI library unit shall be preelaborable (see 10.2.1), and shall depend semantically only upon declared pure, shared passive, remote types, or other remote call interface library units.

In addition, the following restrictions apply to the visible part of an RCI library unit: 

it shall not contain the declaration of a variable; 

Reason: Remote call interface packages do not provide remote data access. A shared passive package has to be used for that. 

it shall not contain the declaration of a limited type; 

Reason: We disallow the declaration of task and protected types, since calling an entry or a protected subprogram implicitly passes an object of a limited type (the target task or protected object). We disallow other limited types since we require that such types have user-defined Read and Write attributes, but we certainly don't want the Read and Write attributes themselves to involve remote calls (thereby defeating their purpose of marshalling the value for remote calls). 

it shall not contain a nested [generic_declaration](S0236); 

Reason: This is disallowed because the body of the nested generic would presumably have access to data inside the body of the RCI package, and if instantiated in a different partition, remote data access might result, which is not supported. 

it shall not  contain the declaration of a subprogram to which a pragma Inline applies;

it shall not  contain a subprogram (or access-to-subprogram) declaration whose profile has an access parameter, or a formal parameter of a limited type unless that limited type has user-specified Read and Write attributes;

any public child of the library unit shall be a remote call interface library unit. 

Reason: No restrictions apply to the private part of an RCI package, and since a public child can "see" the private part of its parent, such a child must itself have a Remote_Call_Interface pragma, and be assigned to the same partition (see below). 

Discussion: We considered making the public child of an RCI package implicitly RCI, but it seemed better to require an explicit pragma to avoid any confusion.

Note that there is no need for a private child to be an RCI package, since it can only be seen from the body of its parent or its siblings, all of which are required to be in the same active partition. 

If a pragma All_Calls_Remoteapplies to a library unit, the library unit shall be a remote call interface.

Aspect Description for All_Calls_Remote: 


#### Post-Compilation Rules

A remote call interface library unit shall be assigned to at most one partition of a given program. A remote call interface library unit whose parent is also an RCI library unit shall be assigned only to the same partition as its parent. 

Implementation Note: The declaration of an RCI package, with a calling-stub body, is automatically included in all active partitions with compilation units that depend on it. However the whole RCI library unit, including its (non-stub) body, will only be in one of the active partitions. 

Notwithstanding the rule given in 10.2, a compilation unit in a given partition that semantically depends on the declaration of an RCI library unit, needs (in the sense of 10.2) only the declaration of the RCI library unit, not the body, to be included in that same partition. [Therefore, the body of an RCI library unit is included only in the partition to which the RCI library unit is explicitly assigned.]


#### Implementation Requirements

If a pragma All_Calls_Remote applies to a given RCI library package, then the implementation shall route any call to a subprogram of the RCI package from outside the declarative region of the package through the Partition Communication Subsystem (PCS); see E.5. Calls to such subprograms from within the declarative region of the package are defined to be local and shall not go through the PCS. 

Discussion: Without this pragma, it is presumed that most implementations will make direct calls if the call originates in the same partition as that of the RCI package. With this pragma, all calls from outside the subsystem rooted at the RCI package are treated like calls from outside the partition, ensuring that the PCS is involved in all such calls (for debugging, redundancy, etc.). 

Reason: There is no point to force local calls (or calls from children) to go through the PCS, since on the target system, these calls are always local, and all the units are in the same active partition. 


#### Implementation Permissions

An implementation need not support the Remote_Call_Interface pragma nor the All_Calls_Remote pragma. [Explicit message-based communication between active partitions can be supported as an alternative to RPC.] 

Ramification: Of course, it is pointless to support the All_Calls_Remote pragma if the Remote_Call_Interface pragma (or some approximate equivalent) is not supported. 


## E.3  Consistency of a Distributed System

[This clause defines attributes and rules associated with verifying the consistency of a distributed program.] 


#### Language Design Principles

The rules guarantee that remote call interface and shared passive packages are consistent among all partitions prior to the execution of a distributed program, so that the semantics of the distributed program are well defined.


#### Static Semantics

For a prefix P that statically denotes a program unit, the following attributes are defined: 

P'VersionYields a value of the predefined type String that identifies the version of the compilation unit that contains the declaration of the program unit.

P'Body_VersionYields a value of the predefined type String that identifies the version of the compilation unit that contains the body (but not any subunits) of the program unit. 

The version of a compilation unit changes whenever the version changes for any compilation unit on which it depends semantically. The version also changes whenever the compilation unit itself changes in a semantically significant way. It is implementation defined whether there are other events (such as recompilation) that result in the version of a compilation unit changing. 

This paragraph was deleted.Implementation defined: Events that cause the version of a compilation unit to change.


#### Bounded (Run-Time) Errors

In a distributed program, a library unit is consistent if the same version of its declaration is used throughout. It is a bounded error to elaborate a partition of a distributed program that contains a compilation unit that depends on a different version of the declaration of a shared passive or RCI library unit than that included in the partition to which the shared passive or RCI library unit was assigned. As a result of this error, Program_Error can be raised in one or both partitions during elaboration; in any case, the partitions become inaccessible to one another. 

Ramification: Because a version changes if anything on which it depends undergoes a version change, requiring consistency for shared passive and remote call interface library units is sufficient to ensure consistency for the declared pure and remote types library units that define the types used for the objects and parameters through which interpartition communication takes place.

Note that we do not require matching Body_Versions; it is irrelevant for shared passive and remote call interface packages, since only one copy of their body exists in a distributed program (in the absence of implicit replication), and we allow the bodies to differ for declared pure and remote types packages from partition to partition, presuming that the differences are due to required error corrections that took place during the execution of a long-running distributed program. The Body_Version attribute provides a means for performing stricter consistency checks. 


## E.4  Remote Subprogram Calls

A remote subprogram call is a subprogram call that invokes the execution of a subprogram in another partition. The partition that originates the remote subprogram call is the calling partition, and the partition that executes the corresponding subprogram body is the called partition. Some remote procedure calls are allowed to return prior to the completion of subprogram execution. These are called asynchronous remote procedure calls.

There are three different ways of performing a remote subprogram call: 

As a direct call on a (remote) subprogram explicitly declared in a remote call interface;

As an indirect call through a value of a remote access-to-subprogram type;

As a dispatching call with a controlling operand designated by a value of a remote access-to-class-wide type. 

The first way of calling corresponds to a static binding between the calling and the called partition. The latter two ways correspond to a dynamic binding between the calling and the called partition.

A remote call interface library unit (see E.2.3) defines the remote subprograms or remote access types used for remote subprogram calls. 


#### Language Design Principles

Remote subprogram calls are standardized since the RPC paradigm is widely-used, and establishing an interface to it in the annex will increase the portability and reusability of distributed programs.


#### Legality Rules

In a dispatching call with two or more controlling operands, if one controlling operand is designated by a value of a remote access-to-class-wide type, then all shall be.


#### Dynamic Semantics

For the execution of a remote subprogram call, subprogram parameters (and later the results, if any) are passed using a stream-oriented representation (see 13.13.1) [which is suitable for transmission between partitions]. This action is called marshalling. Unmarshalling is the reverse action of reconstructing the parameters or results from the stream-oriented representation. [Marshalling is performed initially as part of the remote subprogram call in the calling partition; unmarshalling is done in the called partition. After the remote subprogram completes, marshalling is performed in the called partition, and finally unmarshalling is done in the calling partition.]

A calling stub is the sequence of code that replaces the subprogram body of a remotely called subprogram in the calling partition. A receiving stub is the sequence of code (the "wrapper") that receives a remote subprogram call on the called partition and invokes the appropriate subprogram body. 

Discussion: The use of the term stub in this annex should not be confused with [body_stub](S0224) as defined in 10.1.3. The term stub is used here because it is a commonly understood term when talking about the RPC paradigm. 

Remote subprogram calls are executed at most once, that is, if the subprogram call returns normally, then the called subprogram's body was executed exactly once.

The task executing a remote subprogram call blocks until the subprogram in the called partition returns, unless the call is asynchronous. For an asynchronous remote procedure call, the calling task can become ready before the procedure in the called partition returns.

If a construct containing a remote call is aborted, the remote subprogram call is cancelled. Whether the execution of the remote subprogram is immediately aborted as a result of the cancellation is implementation defined. 

Implementation defined: Whether the execution of the remote subprogram is immediately aborted as a result of cancellation.

If a remote subprogram call is received by a called partition before the partition has completed its elaboration, the call is kept pending until the called partition completes its elaboration (unless the call is cancelled by the calling partition prior to that).

If an exception is propagated by a remotely called subprogram, and the call is not an asynchronous call, the corresponding exception is reraised at the point of the remote subprogram call. For an asynchronous call, if the remote procedure call returns prior to the completion of the remotely called subprogram, any exception is lost.

The exception Communication_Error (see E.5) is raised if a remote call cannot be completed due to difficulties in communicating with the called partition.

All forms of remote subprogram calls are potentially blocking operations (see 9.5.1). 

Reason: Asynchronous remote procedure calls are potentially blocking since the implementation may require waiting for the availability of shared resources to initiate the remote call. 

In a remote subprogram call with a formal parameter of a class-wide type, a check is made that the tag of the actual parameter identifies a tagged type declared in a declared-pure or shared passive library unit, or in the visible part of a remote types or remote call interface library unit. Program_Error is raised if this check fails. 

Discussion: This check makes certain that the specific type passed in an RPC satisfies the rules for a "communicable" type. Normally this is guaranteed by the compile-time restrictions on remote call interfaces. However, with class-wide types, it is possible to pass an object whose tag identifies a type declared outside the "safe" packages.

This is considered an accessibility_check since only the types declared in "safe" packages are considered truly "global" (cross-partition). Other types are local to a single partition. This is analogous to the "accessibility" of global vs. local declarations in a single-partition program.

This rule replaces a rule from an early version of Ada 9X which was given in the subclause on Remote Types Library Units (now E.2.2, "Remote Types Library Units"). That rule tried to prevent "bad" types from being sent by arranging for their tags to mismatch between partitions. However, that interfered with other uses of tags. The new rule allows tags to agree in all partitions, even for those types which are not "safe" to pass in an RPC. 

In a dispatching call with two or more controlling operands that are designated by values of a remote access-to-class-wide type, a check is made [(in addition to the normal Tag_Check - see 11.5)] that all the remote access-to-class-wide values originated from Access [attribute_reference](S0093)s that were evaluated by tasks of the same active partition. Constraint_Error is raised if this check fails. 

Implementation Note: When a remote access-to-class-wide value is created by an Access [attribute_reference](S0093), the identity of the active partition that evaluated the [attribute_reference](S0093) should be recorded in the representation of the remote access value. 


#### Implementation Requirements

The implementation of remote subprogram calls shall conform to the PCS interface as defined by the specification of the language-defined package System.RPC (see E.5). The calling stub shall use the Do_RPC procedure unless the remote procedure call is asynchronous in which case Do_APC shall be used. On the receiving side, the corresponding receiving stub shall be invoked by the RPC-receiver. 

Implementation Note: One possible implementation model is as follows:

The code for calls to subprograms declared in an RCI package is generated normally, that is, the call-site is the same as for a local subprogram call. The code for the remotely callable subprogram bodies is also generated normally. Subprogram's prologue and epilogue are the same as for a local call.

When compiling the specification of an RCI package, the compiler generates calling stubs for each visible subprogram. Similarly, when compiling the body of an RCI package, the compiler generates receiving stubs for each visible subprogram together with the appropriate tables to allow the RPC-receiver to locate the correct receiving stub.

For the statically bound remote calls, the identity of the remote partition is statically determined (it is resolved at configuration/link time).

The calling stub operates as follows: 

It allocates (or reuses) a stream of Params_Stream_Type of Initial_Size, and initializes it by repeatedly calling Write operations, first to identify which remote subprogram in the receiving partition is being called, and then to pass the incoming value of each of the in and in out parameters of the call.

It allocates (or reuses) a stream for the Result, unless a pragma Asynchronous is applied to the procedure.

It calls Do_RPC unless a pragma Asynchronous applied to the procedure in which case it calls Do_APC. An access value designating the message stream allocated and initialized above is passed as the Params parameter. An access value designating the Result stream is passed as the Result parameter.

If the pragma Asynchronous is not specified for the procedure, Do_RPC blocks until a reply message arrives, and then returns to the calling stub. The stub returns after extracting from the Result stream, using Read operations, the in out and out parameters or the function result. If the reply message indicates that the execution of the remote subprogram propagated an exception, the exception is propagated from Do_RPC to the calling stub, and thence to the point of the original remote subprogram call. If Do_RPC detects that communication with the remote partition has failed, it propagates Communication_Error.

On the receiving side, the RPC-receiver procedure operates as follows: 

It is called from the PCS when a remote-subprogram-call message is received. The call originates in some remote call receiver task executed and managed in the context of the PCS.

It extracts information from the stream to identify the appropriate receiving stub.

The receiving stub extracts the in and in out parameters using Read from the stream designated by the Params parameter.

The receiving stub calls the actual subprogram body and, upon completion of the subprogram, uses Write to insert the results into the stream pointed to by the Result parameter. The receiving stub returns to the RPC-receiver procedure which in turn returns to the PCS. If the actual subprogram body propagates an exception, it is propagated by the RPC-receiver to the PCS, which handles the exception, and indicates in the reply message that the execution of the subprogram body propagated an exception. The exception occurrence can be represented in the reply message using the Write attribute of Ada.Exceptions.Exception_Occurrence. 

For remote access-to-subprogram types:

A value of a remote access-to-subprogram type can be represented by the following components: a reference to the remote partition, an index to the package containing the remote subprogram, and an index to the subprogram within the package. The values of these components are determined at run time when the remote access value is created. These three components serve the same purpose when calling Do_APC/RPC, as in the statically bound remote calls; the only difference is that they are evaluated dynamically.

For remote access-to-class-wide types:

For each remote access-to-class-wide type, a calling stub is generated for each dispatching operation of the designated type. In addition, receiving stubs are generated to perform the remote dispatching operations in the called partition. The appropriate [subprogram_body](S0154) is determined as for a local dispatching call once the receiving stub has been reached.

A value of a remote access-to-class-wide type can be represented with the following components: a reference to the remote partition, an index to a table (created one per each such access type) containing addresses of all the dispatching operations of the designated type, and an access value designating the actual remote object.

Alternatively, a remote access-to-class-wide value can be represented as a normal access value, pointing to a "stub" object which in turn contains the information mentioned above. A call on any dispatching operation of such a stub object does the remote call, if necessary, using the information in the stub object to locate the target partition, etc. This approach has the advantage that less special-casing is required in the compiler. All access values can remain just a simple address.

For a call to Do_RPC or Do_APC: The partition ID of all controlling operands are checked for equality (a Constraint_Error is raised if this check fails). The partition ID value is used for the Partition parameter. An index into the tagged-type-descriptor is created. This index points to the receiving stub of the class-wide operation. This index and the index to the table (described above) are written to the stream. Then, the actual parameters are marshalled into the message stream. For a controlling operand, only the access value designating the remote object is required (the other two components are already present in the other parameters).

On the called partition (after the RPC-receiver has transferred control to the appropriate receiving stub) the parameters are first unmarshalled. Then, the tags of the controlling operands (obtained by dereferencing the pointer to the object) are checked for equality. If the check fails Constraint_Error is raised and propagated back to the calling partition, unless it is a result of an asynchronous call. Finally, a dispatching call to the specific subprogram (based on the controlling object's tag) is made. Note that since this subprogram is not in an RCI package, no specific stub is generated for it, it is called normally from the dispatching stub.

NOTE 1   A given active partition can both make and receive remote subprogram calls. Thus, an active partition can act as both a client and a server.

NOTE 2   If a given exception is propagated by a remote subprogram call, but the exception does not exist in the calling partition, the exception can be handled by an others choice or be propagated to and handled by a third partition. 

Discussion: This situation can happen in a case of dynamically nested remote subprogram calls, where an intermediate call executes in a partition that does not include the library unit that defines the exception. 


### E.4.1  Pragma Asynchronous

[This subclause introduces the pragma Asynchronous which allows a remote subprogram call to return prior to completion of the execution of the corresponding remote subprogram body.] 


#### Syntax

The form of a [pragma](S0016) Asynchronous is as follows: 

  pragma Asynchronous([local_name](S0264)); 


#### Legality Rules

The [local_name](S0264) of a pragma Asynchronous shall denote either: 

One or more remote procedures; the formal parameters of the procedure(s) shall all be of mode in;

The first subtype of a remote access-to-procedure type; the formal parameters of the designated profile of the type shall all be of mode in;

The first subtype of a remote access-to-class-wide type. 


#### Static Semantics

A pragma Asynchronous is a representation pragma. When applied to a type, it specifies the type-related asynchronous aspect of the type. 


#### Dynamic Semantics

A remote call is asynchronous if it is a call to a procedure, or a call through a value of an access-to-procedure type, to which a pragma Asynchronous applies. In addition, if a pragma Asynchronous applies to a remote access-to-class-wide type, then a dispatching call on a procedure with a controlling operand designated by a value of the type is asynchronous if the formal parameters of the procedure are all of mode in. 


#### Implementation Requirements

Asynchronous remote procedure calls shall be implemented such that the corresponding body executes at most once as a result of the call. 

To be honest: It is not clear that this rule can be tested or even defined formally. 


### E.4.2  Example of Use of a Remote Access-to-Class-Wide Type


#### Examples

Example of using a remote access-to-class-wide type to achieve dynamic binding across active partitions: 

```ada
package Tapes is
    pragma Pure(Tapes);
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
with Tapes;
package Name_Server is
    pragma Remote_Call_Interface;
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
with Tapes, Name_Server;
package body Tape_Driver is
   type New_Tape is new Tapes.Tape with ...
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
-- Tape_Driver is not needed and thus not mentioned in the [with_clause](S0223)
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

 

The package Tapes provides the necessary declarations of the type and its primitive operations.

Name_Server is a remote call interface package and is elaborated in a separate active partition to provide the necessary naming services (such as Register and Find) to the entire distributed program through remote subprogram calls.

Tape_Driver is a normal package that is elaborated in a partition configured on the processing node that is connected to the tape device(s). The abstract operations are overridden to support the locally declared tape devices (Tape1, Tape2). The package is not visible to its clients, but it exports the tape devices (as remote objects) through the services of the Name_Server. This allows for tape devices to be dynamically added, removed or replaced without requiring the modification of the clients' code.

The Tape_Client procedure references only declarations in the Tapes and Name_Server packages. Before using a tape for the first time, it needs to query the Name_Server for a system-wide identity for that tape. From then on, it can use that identity to access the tape device.

Values of remote access type Tape_Ptr include the necessary information to complete the remote dispatching operations that result from dereferencing the controlling operands T1 and T2.


## E.5  Partition Communication Subsystem

[The Partition Communication Subsystem (PCS) provides facilities for supporting communication between the active partitions of a distributed program. The package System.RPC is a language-defined interface to the PCS.] An implementation conforming to this Annex shall use the RPC interface to implement remote subprogram calls. 

Reason: The prefix RPC is used rather than RSC because the term remote procedure call and its acronym are more familiar. 


#### Static Semantics

The following language-defined library package exists: 

```ada
with Ada.Streams; -- see 13.13.1
package System.RPC is

```

```ada
   type Partition_Id is range 0 .. implementation-defined;

```

```ada
   Communication_Error : exception;

```

```ada
   type Params_Stream_Type (
      Initial_Size : Ada.Streams.Stream_Element_Count) is new
      Ada.Streams.Root_Stream_Type with private;

```

```ada
   procedure Read(
      Stream : in out Params_Stream_Type;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

```

```ada
   procedure Write(
      Stream : in out Params_Stream_Type;
      Item : in Ada.Streams.Stream_Element_Array);

```

```ada
   -- Synchronous call
   procedure Do_RPC(
      Partition  : in Partition_Id;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

```

```ada
   -- Asynchronous call
   procedure Do_APC(
      Partition  : in Partition_Id;
      Params     : access Params_Stream_Type);

```

```ada
   -- The handler for incoming RPCs
   type RPC_Receiver is access procedure(
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

```

```ada
   procedure Establish_RPC_Receiver(
      Partition : in Partition_Id;
      Receiver  : in RPC_Receiver);

```

```ada
private
   ... -- not specified by the language
end System.RPC;

```

A value of the type Partition_Id is used to identify a partition. 

An object of the type Params_Stream_Type is used for identifying the particular remote subprogram that is being called, as well as marshalling and unmarshalling the parameters or result of a remote subprogram call, as part of sending them between partitions.

[The Read and Write procedures override the corresponding abstract operations for the type Params_Stream_Type.]


#### Dynamic Semantics

The Do_RPC and Do_APC procedures send a message to the active partition identified by the Partition parameter. 

Implementation Note: It is assumed that the RPC interface is above the message-passing layer of the network protocol stack and is implemented in terms of it. 

After sending the message, Do_RPC blocks the calling task until a reply message comes back from the called partition or some error is detected by the underlying communication system in which case Communication_Error is raised at the point of the call to Do_RPC. 

Reason: Only one exception is defined in System.RPC, although many sources of errors might exist. This is so because it is not always possible to distinguish among these errors. In particular, it is often impossible to tell the difference between a failing communication link and a failing processing node. Additional information might be associated with a particular Exception_Occurrence for a Communication_Error. 

Do_APC operates in the same way as Do_RPC except that it is allowed to return immediately after sending the message.

Upon normal return, the stream designated by the Result parameter of Do_RPC contains the reply message.

The procedure System.RPC.Establish_RPC_Receiver is called once, immediately after elaborating the library units of an active partition (that is, right after the elaboration of the partition) if the partition includes an RCI library unit, but prior to invoking the main subprogram, if any. The Partition parameter is the Partition_Id of the active partition being elaborated. The Receiver parameter designates an implementation-provided procedure called the RPC-receiver which will handle all RPCs received by the partition from the PCS. Establish_RPC_Receiver saves a reference to the RPC-receiver; when a message is received at the called partition, the RPC-receiver is called with the Params stream containing the message. When the RPC-receiver returns, the contents of the stream designated by Result is placed in a message and sent back to the calling partition. 

Implementation Note: It is defined by the PCS implementation whether one or more threads of control should be available to process incoming messages and to wait for their completion. 

Implementation Note: At link-time, the linker provides the RPC-receiver and the necessary tables to support it. A call on Establish_RPC_Receiver is inserted just before the call on the main subprogram. 

Reason: The interface between the PCS (the System.RPC package) and the RPC-receiver is defined to be dynamic in order to allow the elaboration sequence to notify the PCS that all packages have been elaborated and that it is safe to call the receiving stubs. It is not guaranteed that the PCS units will be the last to be elaborated, so some other indication that elaboration is complete is needed. 

If a call on Do_RPC is aborted, a cancellation message is sent to the called partition, to request that the execution of the remotely called subprogram be aborted. 

To be honest: The full effects of this message are dependent on the implementation of the PCS. 

The subprograms declared in System.RPC are potentially blocking operations. 


#### Implementation Requirements

The implementation of the RPC-receiver shall be reentrant[, thereby allowing concurrent calls on it from the PCS to service concurrent remote subprogram calls into the partition]. 

Reason: There seems no reason to allow the implementation of RPC-receiver to be nonreentrant, even though we don't require that every implementation of the PCS actually perform concurrent calls on the RPC-receiver. 


#### Documentation Requirements

The implementation of the PCS shall document whether the RPC-receiver is invoked from concurrent tasks. If there is an upper limit on the number of such tasks, this limit shall be documented as well, together with the mechanisms to configure it (if this is supported). 

This paragraph was deleted.Implementation defined: Implementation-defined aspects of the PCS.


#### Implementation Permissions

The PCS is allowed to contain implementation-defined interfaces for explicit message passing, broadcasting, etc. Similarly, it is allowed to provide additional interfaces to query the state of some remote partition (given its partition ID) or of the PCS itself, to set timeouts and retry parameters, to get more detailed error status, etc. These additional interfaces should be provided in child packages of System.RPC. 

Implementation defined: Implementation-defined interfaces in the PCS.

A body for the package System.RPC need not be supplied by the implementation. 

Reason: It is presumed that a body for the package System.RPC might be extremely environment specific. Therefore, we do not require that a body be provided by the (compiler) implementation. The user will have to write a body, or acquire one, appropriate for the target environment. 


#### Implementation Advice

Whenever possible, the PCS on the called partition should allow for multiple tasks to call the RPC-receiver with different messages and should allow them to block until the corresponding subprogram body returns. 

The Write operation on a stream of type Params_Stream_Type should raise Storage_Error if it runs out of space trying to write the Item into the stream. 

Implementation Note: An implementation could also dynamically allocate more space as needed, only propagating Storage_Error if the [allocator](S0122) it calls raises Storage_Error. This storage could be managed through a controlled component of the stream object, to ensure that it is reclaimed when the stream object is finalized. 

NOTE 1   The package System.RPC is not designed for direct calls by user programs. It is instead designed for use in the implementation of remote subprograms calls, being called by the calling stubs generated for a remote call interface library unit to initiate a remote call, and in turn calling back to an RPC-receiver that dispatches to the receiving stubs generated for the body of a remote call interface, to handle a remote call received from elsewhere. 

