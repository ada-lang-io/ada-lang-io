---
sidebar_position:  144
---

# C.1  Access to Machine Operations

{AI05-0299-1} [This subclause specifies rules regarding access to machine instructions from within an Ada program.] 

Implementation defined: Implementation-defined intrinsic subprograms.


#### Implementation Requirements

{AI12-0320-1} The implementation shall support machine code insertions (see 13.8) or intrinsic subprograms (see 6.3.1) (or both). The implementation shall allow the use of Ada entities as operands for such machine code insertions or intrinsic subprograms.

Discussion: {AI12-0320-1} Ada entities could be used as operands in machine code insertions and/or intrinsic subprograms using language-defined attributes (such as address), implementation-defined attributes, or could be directly allowed. 


#### Implementation Advice

The machine code or intrinsics support should allow access to all operations normally available to assembly language programmers for the target environment, including privileged instructions, if any. 

Implementation Advice: The machine code or intrinsics support should allow access to all operations normally available to assembly language programmers for the target environment.

Ramification: Of course, on a machine with protection, an attempt to execute a privileged instruction in user mode will probably trap. Nonetheless, we want implementations to provide access to them so that Ada can be used to write systems programs that run in privileged mode. 

{AI05-0229-1} The support for interfacing aspects (see Annex B) should include interface to assembler; the default assembler should be associated with the convention identifier Assembler. 

Implementation Advice: Interface to assembler should be supported; the default assembler should be associated with the convention identifier Assembler.

If an entity is exported to assembly language, then the implementation should allocate it at an addressable location, and should ensure that it is retained by the linking process, even if not otherwise referenced from the Ada code. The implementation should assume that any call to a machine code or assembler subprogram is allowed to read or update every object that is specified as exported. 

Implementation Advice: If an entity is exported to assembly language, then the implementation should allocate it at an addressable location even if not otherwise referenced from the Ada code. A call to a machine code or assembler subprogram should be treated as if it can read or update every object that is specified as exported.


#### Documentation Requirements

The implementation shall document the overhead associated with calling machine-code or intrinsic subprograms, as compared to a fully-inlined call, and to a regular out-of-line call. 

Documentation Requirement: The overhead of calling machine-code or intrinsic subprograms.

The implementation shall document the types of the package System.Machine_Code usable for machine code insertions, and the attributes to be used in machine code insertions for references to Ada entities. 

Documentation Requirement: The types and attributes used in machine code insertions.

{AI05-0229-1} The implementation shall document the subprogram calling conventions associated with the convention identifiers available for use with the Convention aspect (Ada and Assembler, at a minimum), including register saving, exception propagation, parameter passing, and function value returning. 

Documentation Requirement: The subprogram calling conventions for all supported convention identifiers.

For exported and imported subprograms, the implementation shall document the mapping between the Link_Name string, if specified, or the Ada designator, if not, and the external link name used for such a subprogram. 

This paragraph was deleted.

Documentation Requirement: The mapping between the Link_Name or Ada designator and the external link name.


#### Implementation Advice

The implementation should ensure that little or no overhead is associated with calling intrinsic and machine-code subprograms. 

Implementation Advice: Little or no overhead should be associated with calling intrinsic and machine-code subprograms.

It is recommended that intrinsic subprograms be provided for convenient access to any machine operations that provide special capabilities or efficiency and that are not otherwise available through the language constructs. Examples of such instructions include: 

Atomic read-modify-write operations - e.g., test and set, compare and swap, decrement and test, enqueue/dequeue.

Standard numeric functions - e.g., sin, log.

String manipulation operations - e.g., translate and test.

Vector operations - e.g., compare vector against thresholds.

Direct operations on I/O ports.

Implementation Advice: Intrinsic subprograms should be provided to access any machine operations that provide special capabilities or efficiency not normally available.

