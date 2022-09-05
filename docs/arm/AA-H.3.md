---
sidebar_position:  185
---

# H.3  Reviewable Object Code

Object code review and validation are supported by pragmas Reviewable and Inspection_Point. 


## H.3.1  Pragma Reviewable

This pragma directs the implementation to provide information to facilitate analysis and review of a program's object code, in particular to allow determination of execution time and storage usage and to identify the correspondence between the source and object programs. 

Discussion: Since the purpose of this pragma is to provide information to the user, it is hard to objectively test for conformity. In practice, users want the information in an easily understood and convenient form, but neither of these properties can be easily measured.


#### Syntax

The form of a [pragma](./AA-2.8#S0019) Reviewable is as follows: 

  pragma Reviewable; 


#### Post-Compilation Rules

Pragma Reviewable is a configuration pragma. It applies to all [compilation_unit](./AA-10.1#S0286)s included in a partition. 


#### Implementation Requirements

The implementation shall provide the following information for any compilation unit to which such a pragma applies: 

Discussion: The list of requirements can be checked for, even if issues like intelligibility are not addressed.

Where compiler-generated runtime checks remain; 

Discussion: A constraint check which is implemented via a check on the upper and lower bound should clearly be indicated. If a check is implicit in the form of machine instructions used (such an overflow checking), this should also be covered by the documentation. It is particularly important to cover those checks which are not obvious from the source code, such as that for stack overflow.

An identification of any construct with a language-defined check that is recognized prior to run time as certain to fail if executed (even if the generation of runtime checks has been suppressed); 

Discussion: In this case, if the compiler determines that a check must fail, the user should be informed of this. However, since it is not in general possible to know what the compiler will detect, it is not easy to test for this. In practice, it is thought that compilers claiming conformity to this Annex will perform significant optimizations and therefore will detect such situations. Of course, such events could well indicate a programmer error.

{AI95-00209-01} For each read of a scalar object, an identification of the read as either "known to be initialized", or "possibly uninitialized", independent of whether pragma Normalize_Scalars applies; 

Discussion: This issue again raises the question as to what the compiler has determined. A lazy implementation could clearly mark all scalars as "possibly uninitialized", but this would be very unhelpful to the user. It should be possible to analyze a range of scalar uses and note the percentage in each class. Note that an access marked "known to be initialized" does not imply that the value is in range, since the initialization could be from an (erroneous) call of unchecked conversion, or by means external to the Ada program.

Where run-time support routines are implicitly invoked; 

Discussion: Validators will need to know the calls invoked in order to check for the correct functionality. For instance, for some safety applications, it may be necessary to ensure that certain sections of code can execute in a particular time.

An object code listing, including: 

Machine instructions, with relative offsets; 

Discussion: The machine instructions should be in a format that is easily understood, such as the symbolic format of the assembler. The relative offsets are needed in numeric format, to check any alignment restrictions that the architecture might impose.

Where each data object is stored during its lifetime; 

Discussion: This requirement implies that if the optimizer assigns a variable to a register, this needs to be evident.

Correspondence with the source program, including an identification of the code produced per declaration and per statement. 

Discussion: This correspondence will be quite complex when extensive optimization is performed. In particular, address calculation to access some data structures could be moved from the actual access. However, when all the machine code arising from a statement or declaration is in one basic block, this must be indicated by the implementation.

An identification of each construct for which the implementation detects the possibility of erroneous execution; 

Discussion: This requirement is quite vague. In general, it is hard for compilers to detect erroneous execution and therefore the requirement will be rarely invoked. However, if the pragma Suppress is used and the compiler can show that a predefined exception will be raised, then such an identification would be useful.

For each subprogram, block, task, or other construct implemented by reserving and subsequently freeing an area on a run-time stack, an identification of the length of the fixed-size portion of the area and an indication of whether the non-fixed size portion is reserved on the stack or in a dynamically-managed storage region. 

Discussion: This requirement is vital for those requiring to show that the storage available to a program is sufficient. This is crucial in those cases in which the internal checks for stack overflow are suppressed (perhaps by pragma Restrictions(No_Exceptions)). 

The implementation shall provide the following information for any partition to which the pragma applies: 

An object code listing of the entire partition, including initialization and finalization code as well as run-time system components, and with an identification of those instructions and data that will be relocated at load time; 

Discussion: The object code listing should enable a validator to estimate upper bounds for the time taken by critical parts of a program. Similarly, by an analysis of the entire partition, it should be possible to ensure that the storage requirements are suitably bounded, assuming that the partition was written in an appropriate manner.

A description of the run-time model relevant to the partition. 

Discussion: For example, a description of the storage model is vital, since the Ada language does not explicitly define such a model. 

The implementation shall provide control- and data-flow information, both within each compilation unit and across the compilation units of the partition. 

Discussion: This requirement is quite vague, since it is unclear what control and data flow information the compiler has produced. It is really a plea not to throw away information that could be useful to the validator. Note that the data flow information is relevant to the detection of "possibly uninitialized" objects referred to above. 


#### Implementation Advice

The implementation should provide the above information in both a human-readable and machine-readable form, and should document the latter so as to ease further processing by automated tools. 

Implementation Advice: The information produced by [pragma](./AA-2.8#S0019) Reviewable should be provided in both a human-readable and machine-readable form, and the latter form should be documented.

Object code listings should be provided both in a symbolic format and also in an appropriate numeric format (such as hexadecimal or octal). 

Implementation Advice: Object code listings should be provided both in a symbolic format and in a numeric format.

Reason: This is to enable other tools to perform any analysis that the user needed to aid validation. The format should be in some agreed form.

NOTE 1   The order of elaboration of library units will be documented even in the absence of [pragma](./AA-2.8#S0019) Reviewable (see 10.2). 

Discussion: There might be some interactions between pragma Reviewable and compiler optimizations. For example, an implementation may disable some optimizations when pragma Reviewable is in force if it would be overly complicated to provide the detailed information to allow review of the optimized object code. See also [pragma](./AA-2.8#S0019) Optimize (2.8). 


#### Wording Changes from Ada 95

{AI95-00209-01} The wording was clarified that pragma Reviewable applies to each read of an object, as it makes no sense to talk about the state of an object that will immediately be overwritten. 


## H.3.2  Pragma Inspection_Point

An occurrence of a pragma Inspection_Point identifies a set of objects each of whose values is to be available at the point(s) during program execution corresponding to the position of the pragma in the compilation unit. The purpose of such a pragma is to facilitate code validation. 

Discussion: Inspection points are a high level equivalent of break points used by debuggers.


#### Syntax

The form of a [pragma](./AA-2.8#S0019) Inspection_Point is as follows: 

  pragma Inspection_Point[(object_[name](./AA-4.1#S0091) {, object_[name](./AA-4.1#S0091)})]; 


#### Legality Rules

A pragma Inspection_Point is allowed wherever a [declarative_item](./AA-3.11#S0087) or [statement](./AA-5.1#S0167) is allowed. Each object_name shall statically denote the declaration of an object. 

Discussion: The static denotation is required, since no dynamic evaluation of a name is involved in this pragma.


#### Static Semantics

{8652/0093} {AI95-00207-01} {AI95-00434-01} An inspection point is a point in the object code corresponding to the occurrence of a pragma Inspection_Point in the compilation unit. An object is inspectable at an inspection point if the corresponding pragma Inspection_Point either has an argument denoting that object, or has no arguments and the declaration of the object is visible at the inspection point. 

Ramification: If a pragma Inspection_Point is in an in-lined subprogram, there might be numerous inspection points in the object code corresponding to the single occurrence of the pragma in the source; similar considerations apply if such a pragma is in a generic, or in a loop that has been "unrolled" by an optimizer.

{8652/0093} {AI95-00207-01} The short form of the pragma is a convenient shorthand for listing all objects which could be explicitly made inspectable by the long form of the pragma; thus only visible objects are made inspectable by it. Objects that are not visible at the point of the pragma are not made inspectable by the short form pragma. This is necessary so that implementations need not keep information about (or prevent optimizations on) a unit simply because some other unit might contain a short form Inspection_Point pragma. 

Discussion: {8652/0093} {AI95-00207-01} If the short form of the pragma is used, then all visible objects are inspectable. This implies that global objects from other compilation units are inspectable. A good interactive debugging system could provide information similar to a post-mortem dump at such inspection points. The annex does not require that any inspection facility is provided, merely that the information is available to understand the state of the machine at those points. 


#### Dynamic Semantics

Execution of a pragma Inspection_Point has no effect. 

Discussion: {AI95-00114-01} Although an inspection point has no (semantic) effect, the removal or adding of a new point could change the machine code generated by the compiler.


#### Implementation Requirements

Reaching an inspection point is an external interaction with respect to the values of the inspectable objects at that point (see ). 

Ramification: The compiler is inhibited from moving an assignment to an inspectable variable past an inspection point for that variable. On the other hand, the evaluation of an expression that might raise an exception may be moved past an inspection point (see 11.6).


#### Documentation Requirements

For each inspection point, the implementation shall identify a mapping between each inspectable object and the machine resources (such as memory locations or registers) from which the object's value can be obtained. 

This paragraph was deleted.

Documentation Requirement: For each inspection point, a mapping between each inspectable object and the machine resources where the object's value can be obtained shall be provided.

NOTE 1   {AI95-00209-01} The implementation is not allowed to perform "dead store elimination" on the last assignment to a variable prior to a point where the variable is inspectable. Thus an inspection point has the effect of an implicit read of each of its inspectable objects.

NOTE 2   Inspection points are useful in maintaining a correspondence between the state of the program in source code terms, and the machine state during the program's execution. Assertions about the values of program objects can be tested in machine terms at inspection points. Object code between inspection points can be processed by automated tools to verify programs mechanically. 

Discussion: Although it is not a requirement of the annex, it would be useful if the state of the stack and heap could be interrogated. This would allow users to check that a program did not have a `storage leak'.

NOTE 3   The identification of the mapping from source program objects to machine resources is allowed to be in the form of an annotated object listing, in human-readable or tool-processable form. 

Discussion: In principle, it is easy to check an implementation for this pragma, since one merely needs to check the content of objects against those values known from the source listing. In practice, one needs a tool similar to an interactive debugger to perform the check.


#### Wording Changes from Ada 95

{8652/0093} {AI95-00207-01} Corrigendum: Corrected the definition of the Inspection_Point pragma to apply to only variables visible at the point of the pragma. Otherwise, the compiler would have to assume that some other code somewhere could have a pragma Inspection_Point, preventing many optimizations (such as unused object elimination). 

