---
sidebar_position:  17
---

# Annex C Systems Programming

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[ The Systems Programming Annex specifies additional capabilities provided for low-level programming. These capabilities are also required in many real-time, embedded, distributed, and information systems.] 


#### Extensions to Ada 83

This Annex is new to Ada 95. 


## C.1  Access to Machine Operations

[This clause specifies rules regarding access to machine instructions from within an Ada program.] 

Implementation defined: Support for access to machine instructions.


#### Implementation Requirements

The implementation shall support machine code insertions (see 13.8) or intrinsic subprograms (see 6.3.1) (or both). Implementation-defined attributes shall be provided to allow the use of Ada entities as operands.


#### Implementation Advice

The machine code or intrinsics support should allow access to all operations normally available to assembly language programmers for the target environment, including privileged instructions, if any. 

Ramification: Of course, on a machine with protection, an attempt to execute a privileged instruction in user mode will probably trap. Nonetheless, we want implementations to provide access to them so that Ada can be used to write systems programs that run in privileged mode. 

The interfacing pragmas (see Annex B) should support interface to assembler; the default assembler should be associated with the convention identifier Assembler. 

If an entity is exported to assembly language, then the implementation should allocate it at an addressable location, and should ensure that it is retained by the linking process, even if not otherwise referenced from the Ada code. The implementation should assume that any call to a machine code or assembler subprogram is allowed to read or update every object that is specified as exported. 


#### Documentation Requirements

The implementation shall document the overhead associated with calling machine-code or intrinsic subprograms, as compared to a fully-inlined call, and to a regular out-of-line call. 

The implementation shall document the types of the package System.Machine_Code usable for machine code insertions, and the attributes to be used in machine code insertions for references to Ada entities. 

The implementation shall document the subprogram calling conventions associated with the convention identifiers available for use with the interfacing pragmas (Ada and Assembler, at a minimum), including register saving, exception propagation, parameter passing, and function value returning. 

For exported and imported subprograms, the implementation shall document the mapping between the Link_Name string, if specified, or the Ada designator, if not, and the external link name used for such a subprogram. 

This paragraph was deleted.Implementation defined: Implementation-defined aspects of access to machine operations.


#### Implementation Advice

The implementation should ensure that little or no overhead is associated with calling intrinsic and machine-code subprograms. 

It is recommended that intrinsic subprograms be provided for convenient access to any machine operations that provide special capabilities or efficiency and that are not otherwise available through the language constructs. Examples of such instructions include: 

Atomic read-modify-write operations - e.g., test and set, compare and swap, decrement and test, enqueue/dequeue.

Standard numeric functions - e.g., sin, log.

String manipulation operations - e.g., translate and test.

Vector operations - e.g., compare vector against thresholds.

Direct operations on I/O ports.


## C.2  Required Representation Support

This clause specifies minimal requirements on the implementation's support for representation items and related features. 


#### Implementation Requirements

The implementation shall support at least the functionality defined by the recommended levels of support in Section 13. 


## C.3  Interrupt Support

[This clause specifies the language-defined model for hardware interrupts in addition to mechanisms for handling interrupts.] 


#### Dynamic Semantics

[An interrupt represents a class of events that are detected by the hardware or the system software.] Interrupts are said to occur. An occurrence of an interrupt is separable into generation and delivery. Generation of an interrupt is the event in the underlying hardware or system that makes the interrupt available to the program. Delivery is the action that invokes part of the program as response to the interrupt occurrence. Between generation and delivery, the interrupt occurrence [(or interrupt)] is pending. Some or all interrupts may be blocked. When an interrupt is blocked, all occurrences of that interrupt are prevented from being delivered. Certain interrupts are reserved. The set of reserved interrupts is implementation defined. A reserved interrupt is either an interrupt for which user-defined handlers are not supported, or one which already has an attached handler by some other implementation-defined means. Program units can be connected to nonreserved interrupts. While connected, the program unit is said to be attached to that interrupt. The execution of that program unit, the interrupt handler, is invoked upon delivery of the interrupt occurrence. 

This paragraph was deleted.Implementation defined: Implementation-defined aspects of interrupts.

To be honest: As an obsolescent feature, interrupts may be attached to task entries by an address clause. See J.7.1. 

While a handler is attached to an interrupt, it is called once for each delivered occurrence of that interrupt. While the handler executes, the corresponding interrupt is blocked.

While an interrupt is blocked, all occurrences of that interrupt are prevented from being delivered. Whether such occurrences remain pending or are lost is implementation defined.

Each interrupt has a default treatment which determines the system's response to an occurrence of that interrupt when no user-defined handler is attached. The set of possible default treatments is implementation defined, as is the method (if one exists) for configuring the default treatments for interrupts.

An interrupt is delivered to the handler (or default treatment) that is in effect for that interrupt at the time of delivery.

An exception propagated from a handler that is invoked by an interrupt has no effect.

[If the Ceiling_Locking policy (see D.3) is in effect, the interrupt handler executes with the active priority that is the ceiling priority of the corresponding protected object.]


#### Implementation Requirements

The implementation shall provide a mechanism to determine the minimum stack space that is needed for each interrupt handler and to reserve that space for the execution of the handler. [This space should accommodate nested invocations of the handler where the system permits this.]

If the hardware or the underlying system holds pending interrupt occurrences, the implementation shall provide for later delivery of these occurrences to the program.

If the Ceiling_Locking policy is not in effect, the implementation shall provide means for the application to specify whether interrupts are to be blocked during protected actions.


#### Documentation Requirements

The implementation shall document the following items: 

Discussion: This information may be different for different forms of interrupt handlers. 

a)For each interrupt, which interrupts are blocked from delivery when a handler attached to that interrupt executes (either as a result of an interrupt delivery or of an ordinary call on a procedure of the corresponding protected object).

b)Any interrupts that cannot be blocked, and the effect of attaching handlers to such interrupts, if this is permitted.

c)Which run-time stack an interrupt handler uses when it executes as a result of an interrupt delivery; if this is configurable, what is the mechanism to do so; how to specify how much space to reserve on that stack.

d)Any implementation- or hardware-specific activity that happens before a user-defined interrupt handler gets control (e.g., reading device registers, acknowledging devices).

e)Any timing or other limitations imposed on the execution of interrupt handlers.

f)The state (blocked/unblocked) of the nonreserved interrupts when the program starts; if some interrupts are unblocked, what is the mechanism a program can use to protect itself before it can attach the corresponding handlers.

g)Whether the interrupted task is allowed to resume execution before the interrupt handler returns.

h)The treatment of interrupt occurrences that are generated while the interrupt is blocked; i.e., whether one or more occurrences are held for later delivery, or all are lost.

i)Whether predefined or implementation-defined exceptions are raised as a result of the occurrence of any interrupt, and the mapping between the machine interrupts (or traps) and the predefined exceptions.

j)On a multi-processor, the rules governing the delivery of an interrupt to a particular processor. 


#### Implementation Permissions

If the underlying system or hardware does not allow interrupts to be blocked, then no blocking is required [as part of the execution of subprograms of a protected object whose one of its subprograms is an interrupt handler].

In a multi-processor with more than one interrupt subsystem, it is implementation defined whether (and how) interrupt sources from separate subsystems share the same Interrupt_Id type (see C.3.2). In particular, the meaning of a blocked or pending interrupt may then be applicable to one processor only. 

Discussion: This issue is tightly related to the issue of scheduling on a multi-processor. In a sense, if a particular interrupt source is not available to all processors, the system is not truly homogeneous.

One way to approach this problem is to assign sub-ranges within Interrupt_Id to each interrupt subsystem, such that "similar" interrupt sources (e.g. a timer) in different subsystems get a distinct id. 

Implementations are allowed to impose timing or other limitations on the execution of interrupt handlers. 

Reason: These limitations are often necessary to ensure proper behavior of the implementation. 

Other forms of handlers are allowed to be supported, in which case, the rules of this subclause should be adhered to.

The active priority of the execution of an interrupt handler is allowed to vary from one occurrence of the same interrupt to another.


#### Implementation Advice

If the Ceiling_Locking policy is not in effect, the implementation should provide means for the application to specify which interrupts are to be blocked during protected actions, if the underlying system allows for a finer-grain control of interrupt blocking. 

NOTE 1   The default treatment for an interrupt can be to keep the interrupt pending or to deliver it to an implementation-defined handler. Examples of actions that an implementation-defined handler is allowed to perform include aborting the partition, ignoring (i.e., discarding occurrences of) the interrupt, or queuing one or more occurrences of the interrupt for possible later delivery when a user-defined handler is attached to that interrupt.

NOTE 2   It is a bounded error to call Task_Identification.Current_Task (see C.7.1) from an interrupt handler.

NOTE 3   The rule that an exception propagated from an interrupt handler has no effect is modeled after the rule about exceptions propagated out of task bodies.


### C.3.1  Protected Procedure Handlers


#### Syntax

The form of a pragma Interrupt_Handler is as follows: 

  pragma Interrupt_Handler(handler_name);

The form of a pragma Attach_Handler is as follows: 

  pragma Attach_Handler(handler_name, expression); 


#### Name Resolution Rules

For the Interrupt_Handler and Attach_Handler pragmas, the handler_name shall resolve to denote a protected procedure with a parameterless profile.

For the Attach_Handler pragma, the expected type for the expression is Interrupts.Interrupt_Id (see C.3.2). 


#### Legality Rules

The Attach_Handler pragma is only allowed immediately within the protected_definition where the corresponding subprogram is declared. The corresponding protected_type_declaration or single_protected_declaration shall be a librarylevel declaration.

Discussion: In the case of a protected_type_declaration, an object_declaration of an object of that type need not be at library level.

The Interrupt_Handler pragma is only allowed immediately within a protected_definition. The corresponding protected_type_declaration shall be a librarylevel declaration. In addition, any object_declaration of such a type shall be a library level declaration. 


#### Dynamic Semantics

If the pragma Interrupt_Handler appears in a protected_definition, then the corresponding procedure can be attached dynamically, as a handler, to interrupts (see C.3.2). [Such procedures are allowed to be attached to multiple interrupts.]

The expression in the Attach_Handler pragma [as evaluated at object creation time] specifies an interrupt. As part of the initialization of that object, if the Attach_Handler pragma is specified, the handler procedure is attached to the specified interrupt. A check is made that the corresponding interrupt is not reserved. Program_Error is raised if the check fails, and the existing treatment for the interrupt is not affected.

If the Ceiling_Locking policy (see D.3) is in effect then upon the initialization of a protected object that either an Attach_Handler or  Interrupt_Handler pragma applies to one of its procedures, a check is made that the ceiling priority defined in the protected_definition is in the range of System.Interrupt_Priority. If the check fails, Program_Error is raised.

When a protected object is finalized, for any of its procedures that are attached to interrupts, the handler is detached. If the handler was attached by a procedure in the Interrupts package or if no user handler was previously attached to the interrupt, the default treatment is restored. Otherwise, [that is, if an Attach_Handler pragma was specified], the previous handler is restored. 

Discussion: Since only library-level protected procedures can be attached as handlers using the Interrupts package, the finalization discussed above occurs only as part of the finalization of all library-level packages in a partition. 

When a handler is attached to an interrupt, the interrupt is blocked [(subject to the Implementation Permission in C.3)] during the execution of every protected action on the protected object containing the handler.


#### Erroneous Execution

If the Ceiling_Locking policy (see D.3) is in effect and an interrupt is delivered to a handler, and the interrupt hardware priority is higher than the ceiling priority of the corresponding protected object, the execution of the program is erroneous.


#### Metrics

The following metric shall be documented by the implementation: 

The worst case overhead for an interrupt handler that is a parameterless protected procedure, in clock cycles. This is the execution time not directly attributable to the handler procedure or the interrupted execution. It is estimated as C  (A+B), where A is how long it takes to complete a given sequence of instructions without any interrupt, B is how long it takes to complete a normal call to a given protected procedure, and C is how long it takes to complete the same sequence of instructions when it is interrupted by one execution of the same procedure called via an interrupt. 

Implementation Note: The instruction sequence and interrupt handler used to measure interrupt handling overhead should be chosen so as to maximize the execution time cost due to cache misses. For example, if the processor has cache memory and the activity of an interrupt handler could invalidate the contents of cache memory, the handler should be written such that it invalidates all of the cache memory. 


#### Implementation Permissions

When the pragmas Attach_Handler or Interrupt_Handler apply to a protected procedure, the implementation is allowed to impose implementation-defined restrictions on the corresponding protected_type_declaration and protected_body. 

Ramification: The restrictions may be on the constructs that are allowed within them, and on ordinary calls (i.e. not via interrupts) on protected operations in these protected objects. 

Implementation defined: 

An implementation may use a different mechanism for invoking a protected procedure in response to a hardware interrupt than is used for a call to that protected procedure from a task. 

Discussion: This is despite the fact that the priority of an interrupt handler (see D.1) is modeled after a hardware task calling the handler. 

Notwithstanding what this subclause says elsewhere, the Attach_Handler and Interrupt_Handler pragmas are allowed to be used for other, implementation defined, forms of interrupt handlers. 

Ramification: For example, if an implementation wishes to allow interrupt handlers to have parameters, it is allowed to do so via these pragmas; it need not invent implementation-defined pragmas for the purpose. 

Implementation defined: 


#### Implementation Advice

Whenever possible, the implementation should allow interrupt handlers to be called directly by the hardware. 

Whenever practical, the implementation should detect violations of any implementation-defined restrictions before run time. 

NOTE 1   The Attach_Handler pragma can provide static attachment of handlers to interrupts if the implementation supports preelaboration of protected objects. (See C.4.)

NOTE 2   The ceiling priority of a protected object that one of its procedures is attached to an interruptshould be at least as high as the highest processor priority at which that interrupt will ever be delivered.

NOTE 3   Protected procedures can also be attached dynamically to interrupts via operations declared in the predefined package Interrupts.

NOTE 4   An example of a possible implementation-defined restriction is disallowing the use of the standard storage pools within the body of a protected procedure that is an interrupt handler.


### C.3.2  The Package Interrupts


#### Static Semantics

The following language-defined packages exist: 

```ada
with System;
package Ada.Interruptsis
   type Interrupt_Id is implementation-defined;
   type Parameterless_Handler is
      access protected procedure;

```

```ada
 

```

```ada
   function Is_Reserved (Interrupt : Interrupt_Id)
      return Boolean;

```

```ada
   function Is_Attached (Interrupt : Interrupt_Id)
      return Boolean;

```

```ada
   function Current_Handler (Interrupt : Interrupt_Id)
      return Parameterless_Handler;

```

```ada
   procedure Attach_Handler
      (New_Handler : in Parameterless_Handler;
       Interrupt   : in Interrupt_Id);

```

```ada
   procedure Exchange_Handler
      (Old_Handler : out Parameterless_Handler;
       New_Handler : in Parameterless_Handler;
       Interrupt   : in Interrupt_Id);

```

```ada
   procedure Detach_Handler
      (Interrupt : in Interrupt_Id);

```

```ada
   function Reference (Interrupt : Interrupt_Id)
      return System.Address;

```

```ada
private
   ... -- not specified by the language
end Ada.Interrupts;

```

```ada
package Ada.Interrupts.Names is
   implementation-defined : constant Interrupt_Id :=
     implementation-defined;
      . . .
   implementation-defined : constant Interrupt_Id :=
     implementation-defined;
end Ada.Interrupts.Names;

```


#### Dynamic Semantics

The Interrupt_Id type is an implementation-defined discrete type used to identify interrupts.

The Is_Reserved function returns True if and only if the specified interrupt is reserved.

The Is_Attached function returns True if and only if a user-specified interrupt handler is attached to the interrupt.

The Current_Handler function returns a value that represents the attached handler of the interrupt. If no user-defined handler is attached to the interrupt, Current_Handler returns a value that designates the default treatment; calling Attach_Handler or Exchange_Handler with this value restores the default treatment.

The Attach_Handler procedure attaches the specified handler to the interrupt, overriding any existing treatment (including a user handler) in effect for that interrupt. If New_Handler is null, the default treatment is restored. If New_Handler designates a protected procedure to which the pragma Interrupt_Handler does not apply, Program_Error is raised. In this case, the operation does not modify the existing interrupt treatment.

The Exchange_Handler procedure operates in the same manner as Attach_Handler with the addition that the value returned in Old_Handler designates the previous treatment for the specified interrupt. 

Ramification: Calling Attach_Handler or Exchange_Handler with this value for New_Handler restores the previous handler.

The Detach_Handler procedure restores the default treatment for the specified interrupt.

For all operations defined in this package that take a parameter of type Interrupt_Id, with the exception of Is_Reserved and Reference, a check is made that the specified interrupt is not reserved. Program_Error is raised if this check fails.

If, by using the Attach_Handler, Detach_Handler, or Exchange_Handler procedures, an attempt is made to detach a handler that was attached statically (using the pragma Attach_Handler), the handler is not detached and Program_Error is raised.

The Reference function returns a value of type System.Address that can be used to attach a task entry, via an address clause (see J.7.1) to the interrupt specified by Interrupt. This function raises Program_Error if attaching task entries to interrupts (or to this particular interrupt) is not supported.


#### Implementation Requirements

At no time during attachment or exchange of handlers shall the current handler of the corresponding interrupt be undefined.


#### Documentation Requirements

If the Ceiling_Locking policy (see D.3) is in effect the implementation shall document the default ceiling priority assigned to a protected object that contains either the Attach_Handler or Interrupt_Handler pragmas, but not the Interrupt_Priority pragma. [This default need not be the same for all interrupts.] 

Documentation Requirement: 


#### Implementation Advice

If implementation-defined forms of interrupt handler procedures are supported, such as protected procedures with parameters, then for each such form of a handler, a type analogous to Parameterless_Handler should be specified in a child package of Interrupts, with the same operations as in the predefined package Interrupts.

NOTE 1   The package Interrupts.Names contains implementation-defined names (and constant values) for the interrupts that are supported by the implementation.


#### Examples

Example of interrupt handlers: 

```ada
Device_Priority : constant
  array (1..5) of System.Interrupt_Priority := ( ... );
protected type Device_Interface
  (Int_Id : Ada.Interrupts.Interrupt_Id) is
  procedure Handler;
  pragma Attach_Handler(Handler, Int_Id);
  ...
  pragma Interrupt_Priority(Device_Priority(Int_Id));
end Device_Interface;
  ...
Device_1_Driver : Device_Interface(1);
  ...
Device_5_Driver : Device_Interface(5);
  ...

```


## C.4  Preelaboration Requirements

[This clause specifies additional implementation and documentation requirements for the Preelaborate pragma (see 10.2.1).] 


#### Implementation Requirements

The implementation shall not incur any run-time overhead for the elaboration checks of subprograms and protected_bodies declared in preelaborated library units. 

The implementation shall not execute any memory write operations after load time for the elaboration of constant objects declared immediately within the declarative region of a preelaborated library package, so long as the subtype and initial expression (or default initial expressions if initialized by default) of the object_declaration satisfy the following restrictions. The meaning of load time is implementation defined. 

Discussion: On systems where the image of the partition is initially copied from disk to RAM, or from ROM to RAM, prior to starting execution of the partition, the intention is that "load time" consist of this initial copying step. On other systems, load time and run time might actually be interspersed. 

Any subtype_mark denotes a statically constrained subtype, with statically constrained subcomponents, if any;

any constraint is a static constraint;

any allocator is for an access-to-constant type;

any uses of predefined operators appear only within static expressions;

any primaries that are names, other than attribute_references for the Access or Address attributes, appear only within static expressions; 

Ramification: This cuts out attribute_references that are not static, except for Access and Address. 

any name that is not part of a static expression is an expanded name or direct_name that statically denotes some entity; 

Ramification: This cuts out function_calls and type_conversions that are not static, including calls on attribute functions like 'Image and 'Value. 

any discrete_choice of an array_aggregate is static;

no language-defined check associated with the elaboration of the object_declaration can fail. 

Reason: The intent is that aggregates all of whose scalar subcomponents are static, and all of whose access subcomponents are null, allocators for access-to-constant types, or X'Access, will be supported with no run-time code generated. 


#### Documentation Requirements

The implementation shall document any circumstances under which the elaboration of a preelaborated package causes code to be executed at run time. 

The implementation shall document whether the method used for initialization of preelaborated variables allows a partition to be restarted without reloading. 

This paragraph was deleted.Implementation defined: Implementation-defined aspects of preelaboration.

Discussion: This covers the issue of the RTS itself being restartable, so that need not be a separate Documentation Requirement. 


#### Implementation Advice

It is recommended that preelaborated packages be implemented in such a way that there should be little or no code executed at run time for the elaboration of entities not already covered by the Implementation Requirements.


## C.5  Pragma Discard_Names

[A pragma Discard_Names may be used to request a reduction in storage used for the names of certain entities.] 


#### Syntax

The form of a pragma Discard_Names is as follows: 

  pragma Discard_Names[([On =&gt ] local_name)];

A pragma Discard_Names is allowed only immediately within a declarative_part, immediately within a package_specification, or as a configuration pragma. 


#### Legality Rules

The local_name (if present) shall denote a nonderived enumeration [first] subtype, a tagged [first] subtype, or an exception. The pragma applies to the type or exception. Without a local_name, the pragma applies to all such entities declared after the pragma, within the same declarative region. Alternatively, the pragma can be used as a configuration pragma. If the pragma applies to a type, then it applies also to all descendants of the type.


#### Static Semantics

If a local_name is given, then a pragma Discard_Names is a representation pragma.

If the pragma applies to an enumeration type, then the semantics of the Wide_Image and Wide_Value attributes are implementation defined for that type[; the semantics of Image and Value are still defined in terms of Wide_Image and Wide_Value]. In addition, the semantics of Text_IO.Enumeration_IO are implementation defined. If the pragma applies to a tagged type, then the semantics of the Tags.Expanded_Name function are implementation defined for that type. If the pragma applies to an exception, then the semantics of the Exceptions.Exception_Name function are implementation defined for that exception.

Implementation defined: The semantics of pragma Discard_Names.

Ramification: The Width attribute is still defined in terms of Image.

The semantics of S'Wide_Image and S'Wide_Value are implementation defined for any subtype of an enumeration type to which the pragma applies. (The pragma actually names the first subtype, of course.) 


#### Implementation Advice

If the pragma applies to an entity, then the implementation should reduce the amount of storage used for storing names associated with that entity. 

Implementation Advice: 

Reason: A typical implementation of the Image attribute for enumeration types is to store a table containing the names of all the enumeration literals. Pragma Discard_Names allows the implementation to avoid storing such a table without having to prove that the Image attribute is never used (which can be difficult in the presence of separate compilation).

We did not specify the semantics of the Image attribute in the presence of this pragma because different semantics might be desirable in different situations. In some cases, it might make sense to use the Image attribute to print out a useful value that can be used to identify the entity given information in compiler-generated listings. In other cases, it might make sense to get an error at compile time or at run time. In cases where memory is plentiful, the simplest implementation makes sense: ignore the pragma. Implementations that are capable of avoiding the extra storage in cases where the Image attribute is never used might also wish to ignore the pragma.

The same applies to the Tags.Expanded_Name and Exceptions.Exception_Name functions. 


## C.6  Shared Variable Control

[This clause specifies representation pragmas that control the use of shared variables.] 


#### Syntax

The form for pragmas Atomic, Volatile, Atomic_Components, and Volatile_Components is as follows: 

  pragma Atomic(local_name);

  pragma Volatile(local_name);

  pragma Atomic_Components(array_local_name);

  pragma Volatile_Components(array_local_name);


#### Static Semantics

An atomic type is one to which a pragma Atomic applies. An atomic object (including a component) is one to which a pragma Atomic applies, or a component of an array to which a pragma Atomic_Components applies, or any object of an atomic type. 

A volatile type is one to which a pragma Volatile applies. A volatile object (including a component) is one to which a pragma Volatile applies, or a component of an array to which a pragma Volatile_Components applies, or any object of a volatile type. In addition, every atomic type or object is also defined to be volatile. Finally, if an object is volatile, then so are all of its subcomponents [(the same does not apply to atomic)].


#### Name Resolution Rules

The local_name in an Atomic or Volatile pragma shall resolve to denote either an object_declaration, a noninherited component_declaration, or a full_type_declaration. The array_local_name in an Atomic_Components or Volatile_Components pragma shall resolve to denote the declaration of an array type or an array object of an anonymous type. 


#### Legality Rules

It is illegal to apply either an Atomic or Atomic_Components pragma to an object or type if the implementation cannot support the indivisible reads and updates required by the pragma (see below).

It is illegal to specify the Size attribute of an atomic object, the Component_Size attribute for an array type with atomic components, or the layout attributes of an atomic component, in a way that prevents the implementation from performing the required indivisible reads and updates.

If an atomic object is passed as a parameter, then the type of the formal parameter shall either be atomic or allow pass by copy [(that is, not be a nonatomic by-reference type)]. If an atomic object is used as an actual for a generic formal object of mode in out, then the type of the generic formal object shall be atomic. If the prefix of an attribute_reference for an Access attribute denotes an atomic object [(including a component)], then the designated type of the resulting access type shall be atomic. If an atomic type is used as an actual for a generic formal derived type, then the ancestor of the formal type shall be atomic or allow pass by copy. Corresponding rules apply to volatile objects and types.

If a pragma Volatile, Volatile_Components, Atomic, or Atomic_Components applies to a stand-alone constant object, then a pragma Import shall also apply to it. 

Ramification: Hence, no initialization expression is allowed for such a constant. Note that a constant that is atomic or volatile because of its type is allowed. 

Reason: Stand-alone constants that are explicitly specified as Atomic or Volatile only make sense if they are being manipulated outside the Ada program. From the Ada perspective the object is read-only. Nevertheless, if imported and atomic or volatile, the implementation should presume it might be altered externally. For an imported stand-alone constant that is not atomic or volatile, the implementation can assume that it will not be altered. 


#### Static Semantics

These pragmas are representation pragmas (see 13.1). 


#### Dynamic Semantics

For an atomic object (including an atomic component) all reads and updates of the object as a whole are indivisible.

For a volatile object all reads and updates of the object as a whole are performed directly to memory.

Implementation Note: This precludes any use of register temporaries, caches, and other similar optimizations for that object. 

Two actions are sequential (see 9.10) if each is the read or update of the same atomic object.

If a type is atomic or volatile and it is not a by-copy type, then the type is defined to be a by-reference type. If any subcomponent of a type is atomic or volatile, then the type is defined to be a by-reference type.

If an actual parameter is atomic or volatile, and the corresponding formal parameter is not, then the parameter is passed by copy. 

Implementation Note: Note that in the case where such a parameter is normally passed by reference, a copy of the actual will have to be produced at the call-site, and a pointer to the copy passed to the formal parameter. If the actual is atomic, any copying has to use indivisible read on the way in, and indivisible write on the way out. 

Reason: It has to be known at compile time whether an atomic or a volatile parameter is to be passed by copy or by reference. For some types, it is unspecified whether parameters are passed by copy or by reference. The above rules further specify the parameter passing rules involving atomic and volatile types and objects. 

Implementation Note: } 


#### Implementation Requirements

The external effect of a program (see ) is defined to include each read and update of a volatile or atomic object. The implementation shall not generate any memory reads or updates of atomic or volatile objects other than those specified by the program. 

Discussion: The presumption is that volatile or atomic objects might reside in an "active" part of the address space where each read has a potential side effect, and at the very least might deliver a different value.

The rule above and the definition of external effect are intended to prevent (at least) the following incorrect optimizations, where V is a volatile variable: 

X:= V; Y:=V; cannot be allowed to be translated as Y:=V; X:=V;

Deleting redundant loads: X:= V; X:= V; shall read the value of V from memory twice.

Deleting redundant stores: V:= X; V:= X; shall write into V twice.

Extra stores: V:= X+Y; should not translate to something like V:= X; V:= V+Y;

Extra loads: X:= V; Y:= X+Z; X:=X+B; should not translate to something like Y:= V+Z; X:= V+B;

Reordering of loads from volatile variables: X:= V1; Y:= V2; (whether or not V1 = V2) should not translate to Y:= V2; X:= V1;

Reordering of stores to volatile variables: V1:= X; V2:= X; should not translate to V2:=X; V1:= X; 

If a pragma Pack applies to a type any of whose subcomponents are atomic, the implementation shall not pack the atomic subcomponents more tightly than that for which it can support indivisible reads and updates. 

Implementation Note: A warning might be appropriate if no packing whatsoever can be achieved. 

NOTE 1   An imported volatile or atomic constant behaves as a constant (i.e. read-only) with respect to other parts of the Ada program, but can still be modified by an "external source".


#### Incompatibilities With Ada 83

Pragma Atomic replaces Ada 83's pragma Shared. The name "Shared" was confusing, because the pragma was not used to mark variables as shared. 


## C.7  Task Identification and Attributes

[This clause describes operations and attributes that can be used to obtain the identity of a task. In addition, a package that associates user-defined information with a task is defined.] 


### C.7.1  The Package Task_Identification


#### Static Semantics

The following language-defined library package exists: 

```ada
package Ada.Task_Identification is
   type Task_Id is private;
   Null_Task_Id : constant Task_Id;
   function  "=" (Left, Right : Task_Id) return Boolean;

```

```ada
   function  Image                  (T : Task_Id) return String;
   function  Current_Task     return Task_Id;
   procedure Abort_Task             (T : in out Task_Id);

```

```ada
   function  Is_Terminated          (T : Task_Id) return Boolean;
   function  Is_Callable            (T : Task_Id) return Boolean;
private
   ... -- not specified by the language
end Ada.Task_Identification;

```


#### Dynamic Semantics

A value of the type Task_Id identifies an existent task. The constant Null_Task_Id does not identify any task. Each object of the type Task_Id is default initialized to the value of Null_Task_Id.

The function "=" returns True if and only if Left and Right identify the same task or both have the value Null_Task_Id.

The function Image returns an implementation-defined string that identifies T. If T equals Null_Task_Id, Image returns an empty string. 

Implementation defined: The result of the Task_Identification.Image attribute.

The function Current_Task returns a value that identifies the calling task.

The effect of Abort_Task is the same as the abort_statement for the task identified by T. [In addition, if T identifies the environment task, the entire partition is aborted, See E.1.]

The functions Is_Terminated and Is_Callable return the value of the corresponding attribute of the task identified by T. 

For a prefix T that is of a task type [(after any implicit dereference)], the following attribute is defined: 

T'IdentityYields a value of the type Task_Id that identifies the task denoted by T.

For a prefix E that denotes an entry_declaration, the following attribute is defined: 

E'CallerYields a value of the type Task_Id that identifies the task whose call is now being serviced. Use of this attribute is allowed only inside an entry_body or accept_statementcorresponding to the entry_declaration denoted by E. 

Program_Error is raised if a value of Null_Task_Id is passed as a parameter to Abort_Task, Is_Terminated, and Is_Callable.

Abort_Task is a potentially blocking operation (see 9.5.1). 


#### Bounded (Run-Time) Errors

It is a bounded error to call the Current_Task function from an entry body or an interrupt handler. Program_Error is raised, or an implementation-defined value of the type Task_Id is returned. 

Implementation defined: The value of Current_Task when in a protected entry or interrupt handler.

Implementation Note: This value could be Null_Task_Id, or the ID of some user task, or that of an internal task created by the implementation. 


#### Erroneous Execution

If a value of Task_Id is passed as a parameter to any of the operations declared in this package (or any language-defined child of this package), and the corresponding task object no longer exists, the execution of the program is erroneous. 


#### Documentation Requirements

The implementation shall document the effect of calling Current_Task from an entry body or interrupt handler. 

This paragraph was deleted.Implementation defined: The effect of calling Current_Task from an entry body or interrupt handler.

NOTE 1   This package is intended for use in writing user-defined task scheduling packages and constructing server tasks. Current_Task can be used in conjunction with other operations requiring a task as an argument such as Set_Priority (see D.5).

NOTE 2   The function Current_Task and the attribute Caller can return a Task_Id value that identifies the environment task.


### C.7.2  The Package Task_Attributes


#### Static Semantics

The following language-defined generic library package exists: 

```ada
with Ada.Task_Identification; use Ada.Task_Identification;
generic
   type Attribute is private;
   Initial_Value : in Attribute;
package Ada.Task_Attributes is

```

```ada
   type Attribute_Handle is access all Attribute;

```

```ada
   function Value(T : Task_Id := Current_Task)
     return Attribute;

```

```ada
   function Reference(T : Task_Id := Current_Task)
     return Attribute_Handle;

```

```ada
   procedure Set_Value(Val : in Attribute;
                       T : in Task_Id := Current_Task);
   procedure Reinitialize(T : in Task_Id := Current_Task);

```

```ada
end Ada.Task_Attributes;

```


#### Dynamic Semantics

When an instance of Task_Attributes is elaborated in a given active partition, an object of the actual type corresponding to the formal type Attribute is implicitly created for each task (of that partition) that exists and is not yet terminated. This object acts as a user-defined attribute of the task. A task created previously in the partition and not yet terminated has this attribute from that point on. Each task subsequently created in the partition will have this attribute when created. In all these cases, the initial value of the given attribute is Initial_Value.

The Value operation returns the value of the corresponding attribute of T.

The Reference operation returns an access value that designates the corresponding attribute of T.

The Set_Value operation performs any finalization on the old value of the attribute of T and assigns Val to that attribute (see 5.2 and 7.6).

The effect of the Reinitialize operation is the same as Set_Value where the Val parameter is replaced with Initial_Value. 

Implementation Note: In most cases, the attribute memory can be reclaimed at this point. 

For all the operations declared in this package, Tasking_Error is raised if the task identified by T is terminated. Program_Error is raised if the value of T is Null_Task_Id.


#### Erroneous Execution

It is erroneous to dereference the access value returned by a given call on Reference after a subsequent call on Reinitialize for the same task attribute, or after the associated task terminates. 

Reason: This allows the storage to be reclaimed for the object associated with an attribute upon Reinitialize or task termination. 

If a value of Task_Id is passed as a parameter to any of the operations declared in this package and the corresponding task object no longer exists, the execution of the program is erroneous.


#### Implementation Requirements

The implementation shall perform each of the above operations for a given attribute of a given task atomically with respect to any other of the above operations for the same attribute of the same task. 

Ramification: Hence, other than by dereferencing an access value returned by Reference, an attribute of a given task can be safely read and updated concurrently by multiple tasks. 

When a task terminates, the implementation shall finalize all attributes of the task, and reclaim any other storage associated with the attributes. 


#### Documentation Requirements

The implementation shall document the limit on the number of attributes per task, if any, and the limit on the total storage for attribute values per task, if such a limit exists.

In addition, if these limits can be configured, the implementation shall document how to configure them. 

This paragraph was deleted.Implementation defined: Implementation-defined aspects of Task_Attributes.


#### Metrics

The implementation shall document the following metrics: A task calling the following subprograms shall execute in a sufficiently high priority as to not be preempted during the measurement period. This period shall start just before issuing the call and end just after the call completes. If the attributes of task T are accessed by the measurement tests, no other task shall access attributes of that task during the measurement period. For all measurements described here, the Attribute type shall be a scalar whose size is equal to the size of the predefined integer size. For each measurement, two cases shall be documented: one where the accessed attributes are of the calling task [(that is, the default value for the T parameter is used)], and the other, where T identifies another, nonterminated, task.

The following calls (to subprograms in the Task_Attributes package) shall be measured: 

a call to Value, where the return value is Initial_Value;

a call to Value, where the return value is not equal to Initial_Value;

a call to Reference, where the return value designates a value equal to Initial_Value;

a call to Reference, where the return value designates a value not equal to Initial_Value;

a call to Set_Value where the Val parameter is not equal to Initial_Value and the old attribute value is equal to Initial_Value.

a call to Set_Value where the Val parameter is not equal to Initial_Value and the old attribute value is not equal to Initial_Value.


#### Implementation Permissions

An implementation need not actually create the object corresponding to a task attribute until its value is set to something other than that of Initial_Value, or until Reference is called for the task attribute. Similarly, when the value of the attribute is to be reinitialized to that of Initial_Value, the object may instead be finalized and its storage reclaimed, to be recreated when needed later. While the object does not exist, the function Value may simply return Initial_Value, rather than implicitly creating the object. 

Discussion: The effect of this permission can only be observed if the assignment operation for the corresponding type has side effects. 

Implementation Note: This permission means that even though every task has every attribute, storage need only be allocated for those attributes that have been Reference'd or set to a value other than that of Initial_Value. 

An implementation is allowed to place restrictions on the maximum number of attributes a task may have, the maximum size of each attribute, and the total storage size allocated for all the attributes of a task.


#### Implementation Advice

Some implementations are targeted to domains in which memory use at run time must be completely deterministic. For such implementations, it is recommended that the storage for task attributes will be pre-allocated statically and not from the heap. This can be accomplished by either placing restrictions on the number and the size of the task's attributes, or by using the pre-allocated storage for the first N attribute objects, and the heap for the others. In the latter case, N should be documented.

NOTE 1   An attribute always exists (after instantiation), and has the initial value. It need not occupy memory until the first operation that potentially changes the attribute value. The same holds true after Reinitialize.

NOTE 2   The result of the Reference function should be used with care; it is always safe to use that result in the task body whose attribute is being accessed. However, when the result is being used by another task, the programmer must make sure that the task whose attribute is being accessed is not yet terminated. Failing to do so could make the program execution erroneous.

NOTE 3   As specified in C.7.1, if the parameter T (in a call on a subprogram of an instance of this package) identifies a nonexistent task, the execution of the program is erroneous. 

