---
sidebar_position:  207
---

# K.1  Language-Defined Aspects

{AI05-0229-1} {AI05-0299-1} This subclause summarizes the definitions given elsewhere of the language-defined aspects. Aspects are properties of entities that can be specified by the Ada program; unless otherwise specified below, aspects can be specified using an [aspect_specification](./AA-13.1#S0346).

AddressMachine address of an entity. See 13.3.

AggregateMechanism to define user-defined aggregates. See 4.3.5.

Alignment (object)Alignment of an object. See 13.3.

Alignment (subtype)Alignment of a subtype. See 13.3.

All_Calls_RemoteAll indirect or dispatching remote subprogram calls, and all direct remote subprogram calls, should use the Partition Communication Subsystem. See E.2.3.

Allows_ExitAn indication of whether a subprogram will operate correctly for arbitrary transfers of control. See 5.5.3.

AsynchronousRemote procedure calls are asynchronous; the caller continues without waiting for the call to return. See E.4.1.

AtomicDeclare that a type, object, or component is atomic. See C.6.

Atomic_ComponentsDeclare that the components of an array type or object are atomic. See C.6.

Attach_HandlerProtected procedure is attached to an interrupt. See C.3.1.

Bit_OrderOrder of bit numbering in a [record_representation_clause](./AA-13.5#S0352). See 13.5.3.

CodingInternal representation of enumeration literals. Specified by an [enumeration_representation_clause](./AA-13.4#S0350), not by an [aspect_specification](./AA-13.1#S0346). See 13.4.

Component_SizeSize in bits of a component of an array type. See 13.3.

Constant_IndexingDefines function(s) to implement user-defined [indexed_component](./AA-4.1#S0096)s. See 4.1.6.

ConventionCalling convention or other convention used for interfacing to other languages. See B.1.

CPUProcessor on which a given task, or calling task for a protected operation, should run. See D.16.

Default_Component_ValueDefault value for the components of an array-of-scalar subtype. See 3.6.

Default_Initial_ConditionA condition that will hold true after the default initialization of an object. See 7.3.3.

Default_IteratorDefault iterator to be used in for loops. See 5.5.1.

Default_Storage_PoolDefault storage pool for a generic instance. See 13.11.3.

Default_ValueDefault value for a scalar subtype. See 3.5.

Discard_NamesRequests a reduction in storage for names associated with an entity. See C.5.

DispatchingGeneric formal parameters used in the implementation of an entity. See H.7.1.

Dispatching_DomainDomain (group of processors) on which a given task should run. See D.16.1.

Dynamic_PredicateCondition that will hold true for objects of a given subtype; the subtype is not static. See 3.2.4.

Elaborate_BodyA given package will have a body, and that body is elaborated immediately after the declaration. See 10.2.1.

Exclusive_FunctionsSpecifies mutual exclusion behavior of protected functions in a protected type. See 9.5.1.

ExportEntity is exported to another language. See B.1.

External_NameName used to identify an imported or exported entity. See B.1.

External_TagUnique identifier for a tagged type in streams. See 13.3.

Full_Access_OnlyDeclare that a volatile type, object, or component is full access. See C.6.

GlobalGlobal object usage contract. See 6.1.2.

Global'ClassGlobal object usage contract inherited on derivation. See 6.1.2.

Implicit_DereferenceMechanism for user-defined implicit .all. See 4.1.5.

ImportEntity is imported from another language. See B.1.

IndependentDeclare that a type, object, or component is independently addressable. See C.6.

Independent_ComponentsDeclare that the components of an array or record type, or an array object, are independently addressable. See C.6.

InlineFor efficiency, Inline calls are requested for a subprogram. See 6.3.2.

InputFunction to read a value from a stream for a given type, including any bounds and discriminants. See 13.13.2.

Input'ClassFunction to read a value from a stream for a the class-wide type associated with a given type, including any bounds and discriminants. See 13.13.2.

Integer_LiteralDefines a function to implement user-defined integer literals. See 4.2.1.

Interrupt_HandlerProtected procedure may be attached to interrupts. See C.3.1.

Interrupt_PriorityPriority of a task object or type, or priority of a protected object or type; the priority is in the interrupt range. See D.1.

Iterator_ElementElement type to be used for user-defined iterators. See 5.5.1.

Iterator_ViewAn alternative type to used for container element iterators. See 5.5.1.

Layout (record)Layout of record components. Specified by a [record_representation_clause](./AA-13.5#S0352), not by an [aspect_specification](./AA-13.1#S0346). See 13.5.1.

Link_NameLinker symbol used to identify an imported or exported entity. See B.1.

Machine_RadixRadix (2 or 10) that is used to represent a decimal fixed point type. See F.1.

Max_Entry_Queue_LengthThe maximum entry queue length for a task type, protected type, or entry. See D.4.

No_Controlled_PartsA specification that a type and its descendants do not have controlled parts. See H.4.1.

No_ReturnA subprogram will not return normally. See 6.5.1.

NonblockingSpecifies that an associated subprogram does not block. See 9.5.

OutputProcedure to write a value to a stream for a given type, including any bounds and discriminants. See 13.13.2.

Output'ClassProcedure to write a value to a stream for a the class-wide type associated with a given type, including any bounds and discriminants. See 13.13.2.

PackMinimize storage when laying out records and arrays. See 13.2.

Parallel_CallsSpecifies whether a given subprogram is expected to be called in parallel. See 9.10.1.

Parallel_IteratorAn indication of whether a subprogram may use multiple threads of control to invoke a loop body procedure. See 5.5.3.

PostPostcondition; a condition that will hold true after a call. See 6.1.1.

Post'ClassPostcondition that applies to corresponding subprograms of descendant types. See 6.1.1.

PrePrecondition; a condition that is expected to hold true before a call. See 6.1.1.

Pre'ClassPrecondition that applies to corresponding subprograms of descendant types. See 6.1.1.

Predicate_FailureAction to be performed when a predicate check fails. See 3.2.4.

Preelaborable_InitializationDeclares that a type has preelaborable initialization. See 10.2.1.

PreelaborateCode execution during elaboration is avoided for a given package. See 10.2.1.

PriorityPriority of a task object or type, or priority of a protected object or type; the priority is not in the interrupt range. See D.1.

PureSide effects are avoided in the subprograms of a given package. See 10.2.1.

Put_ImageProcedure to define the image of a given type. See 4.10.

ReadProcedure to read a value from a stream for a given type. See 13.13.2.

Read'ClassProcedure to read a value from a stream for the class-wide type associated with a given type. See 13.13.2.

Real_LiteralDefines a function or functions to implement user-defined real literals. See 4.2.1.

Record layoutSee Layout. See 13.5.1.

Relative_DeadlineTask or protected type parameter used in Earliest Deadline First Dispatching. See D.2.6.

Remote_Call_InterfaceSubprograms in a given package may be used in remote procedure calls. See E.2.3.

Remote_TypesTypes in a given package may be used in remote procedure calls. See E.2.2.

Shared_PassiveA given package is used to represent shared memory in a distributed system. See E.2.1.

Size (object)Size in bits of an object. See 13.3.

Size (subtype)Size in bits of a subtype. See 13.3.

SmallScale factor for a fixed point type. See 3.5.10.

Stable_PropertiesA list of functions describing characteristics that usually are unchanged by primitive operations of the type or an individual primitive subprogram. See 7.3.4.

Stable_Properties'ClassA list of functions describing characteristics that usually are unchanged by primitive operations of a class of types or a primitive subprogram for such a class. See 7.3.4.

StaticSpecifies that an associated expression function can be used in static expressions. See 6.8.

Static_PredicateCondition that will hold true for objects of a given subtype; the subtype may be static. See 3.2.4.

Storage_PoolPool of memory from which new will allocate for a given access type. See 13.11.

Storage_Size (access)Sets memory size for allocations for an access type. See 13.11.

Storage_Size (task)Size in storage elements reserved for a task type or single task object. See 13.3.

Stream_SizeSize in bits used to represent elementary objects in a stream. See 13.13.2.

String_LiteralDefines a function to implement user-defined string literals. See 4.2.1.

SynchronizationDefines whether a given primitive operation of a synchronized interface will be implemented by an entry or protected procedure. See 9.5.

Type_InvariantA condition that will hold true for all objects of a type. See 7.3.2.

Type_Invariant'ClassA condition that will hold true for all objects in a class of types. See 7.3.2.

Unchecked_UnionType is used to interface to a C union type. See B.3.3.

Use_FormalGeneric formal parameters used in the implementation of an entity. See H.7.1.

Variable_IndexingDefines function(s) to implement user-defined [indexed_component](./AA-4.1#S0096)s. See 4.1.6.

VolatileDeclare that a type, object, or component is volatile. See C.6.

Volatile_ComponentsDeclare that the components of an array type or object are volatile. See C.6.

WriteProcedure to write a value to a stream for a given type. See 13.13.2.

Write'ClassProcedure to write a value to a stream for a the class-wide type associated with a given type. See 13.13.2.

YieldEnsures that a callable entity includes a task dispatching point. See D.2.1.

