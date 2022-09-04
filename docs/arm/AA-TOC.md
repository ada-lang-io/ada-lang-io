---
sidebar_position:  1
---

#  Table of Contents

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
Foreword

Introduction

1. General
    1.1 Scope
        1.1.1 Extent
    1.2 Normative References
    1.3 Definitions

2. Lexical Elements
    2.1 Character Set
    2.2 Lexical Elements, Separators, and Delimiters
    2.3 Identifiers
    2.4 Numeric Literals
        2.4.1 Decimal Literals
        2.4.2 Based Literals
    2.5 Character Literals
    2.6 String Literals
    2.7 Comments
    2.8 Pragmas
    2.9 Reserved Words

3. Declarations and Types
    3.1 Declarations
    3.2 Types and Subtypes
        3.2.1 Type Declarations
        3.2.2 Subtype Declarations
        3.2.3 Classification of Operations
    3.3 Objects and Named Numbers
        3.3.1 Object Declarations
        3.3.2 Number Declarations
    3.4 Derived Types and Classes
        3.4.1 Derivation Classes
    3.5 Scalar Types
        3.5.1 Enumeration Types
        3.5.2 Character Types
        3.5.3 Boolean Types
        3.5.4 Integer Types
        3.5.5 Operations of Discrete Types
        3.5.6 Real Types
        3.5.7 Floating Point Types
        3.5.8 Operations of Floating Point Types
        3.5.9 Fixed Point Types
        3.5.10 Operations of Fixed Point Types
    3.6 Array Types
        3.6.1 Index Constraints and Discrete Ranges
        3.6.2 Operations of Array Types
        3.6.3 String Types
    3.7 Discriminants
        3.7.1 Discriminant Constraints
        3.7.2 Operations of Discriminated Types
    3.8 Record Types
        3.8.1 Variant Parts and Discrete Choices
    3.9 Tagged Types and Type Extensions
        3.9.1 Type Extensions
        3.9.2 Dispatching Operations of Tagged Types
        3.9.3 Abstract Types and Subprograms
    3.10 Access Types
        3.10.1 Incomplete Type Declarations
        3.10.2 Operations of Access Types
    3.11 Declarative Parts
        3.11.1 Completions of Declarations

4. Names and Expressions
    4.1 Names
        4.1.1 Indexed Components
        4.1.2 Slices
        4.1.3 Selected Components
        4.1.4 Attributes
    4.2 Literals
    4.3 Aggregates
        4.3.1 Record Aggregates
        4.3.2 Extension Aggregates
        4.3.3 Array Aggregates
    4.4 Expressions
    4.5 Operators and Expression Evaluation
        4.5.1 Logical Operators and Short-circuit Control Forms
        4.5.2 Relational Operators and Membership Tests
        4.5.3 Binary Adding Operators
        4.5.4 Unary Adding Operators
        4.5.5 Multiplying Operators
        4.5.6 Highest Precedence Operators
    4.6 Type Conversions
    4.7 Qualified Expressions
    4.8 Allocators
    4.9 Static Expressions and Static Subtypes
        4.9.1 Statically Matching Constraints and Subtypes

5. Statements
    5.1 Simple and Compound Statements - Sequences of Statements
    5.2 Assignment Statements
    5.3 If Statements
    5.4 Case Statements
    5.5 Loop Statements
    5.6 Block Statements
    5.7 Exit Statements
    5.8 Goto Statements

6. Subprograms
    6.1 Subprogram Declarations
    6.2 Formal Parameter Modes
    6.3 Subprogram Bodies
        6.3.1 Conformance Rules
        6.3.2 Inline Expansion of Subprograms
    6.4 Subprogram Calls
        6.4.1 Parameter Associations
    6.5 Return Statements
        6.5.1 Pragma No_Return
    6.6 Overloading of Operators

7. Packages
    7.1 Package Specifications and Declarations
    7.2 Package Bodies
    7.3 Private Types and Private Extensions
        7.3.1 Private Operations
    7.4 Deferred Constants
    7.5 Limited Types
    7.6 User-Defined Assignment and Finalization
        7.6.1 Completion and Finalization

8. Visibility Rules
    8.1 Declarative Region
    8.2 Scope of Declarations
    8.3 Visibility
    8.4 Use Clauses
    8.5 Renaming Declarations
        8.5.1 Object Renaming Declarations
        8.5.2 Exception Renaming Declarations
        8.5.3 Package Renaming Declarations
        8.5.4 Subprogram Renaming Declarations
        8.5.5 Generic Renaming Declarations
    8.6 The Context of Overload Resolution

9. Tasks and Synchronization
    9.1 Task Units and Task Objects
    9.2 Task Execution - Task Activation
    9.3 Task Dependence - Termination of Tasks
    9.4 Protected Units and Protected Objects
    9.5 Intertask Communication
        9.5.1 Protected Subprograms and Protected Actions
        9.5.2 Entries and Accept Statements
        9.5.3 Entry Calls
        9.5.4 Requeue Statements
    9.6 Delay Statements, Duration, and Time
    9.7 Select Statements
        9.7.1 Selective Accept
        9.7.2 Timed Entry Calls
        9.7.3 Conditional Entry Calls
        9.7.4 Asynchronous Transfer of Control
    9.8 Abort of a Task - Abort of a Sequence of Statements
    9.9 Task and Entry Attributes
    9.10 Shared Variables
    9.11 Example of Tasking and Synchronization

10. Program Structure and Compilation Issues
    10.1 Separate Compilation
        10.1.1 Compilation Units - Library Units
        10.1.2 Context Clauses - With Clauses
        10.1.3 Subunits of Compilation Units
        10.1.4 The Compilation Process
        10.1.5 Pragmas and Program Units
        10.1.6 Environment-Level Visibility Rules
    10.2 Program Execution
        10.2.1 Elaboration Control

11. Exceptions
    11.1 Exception Declarations
    11.2 Exception Handlers
    11.3 Raise Statements
    11.4 Exception Handling
        11.4.1 The Package Exceptions
        11.4.2 Example of Exception Handling
    11.5 Suppressing Checks
    11.6 Exceptions and Optimization

12. Generic Units
    12.1 Generic Declarations
    12.2 Generic Bodies
    12.3 Generic Instantiation
    12.4 Formal Objects
    12.5 Formal Types
        12.5.1 Formal Private and Derived Types
        12.5.2 Formal Scalar Types
        12.5.3 Formal Array Types
        12.5.4 Formal Access Types
    12.6 Formal Subprograms
    12.7 Formal Packages
    12.8 Example of a Generic Package

13. Representation Issues
    13.1 Representation Items
    13.2 Pragma Pack
    13.3 Representation Attributes
    13.4 Enumeration Representation Clauses
    13.5 Record Layout
        13.5.1 Record Representation Clauses
        13.5.2 Storage Place Attributes
        13.5.3 Bit Ordering
    13.6 Change of Representation
    13.7 The Package System
        13.7.1 The Package System.Storage_Elements
        13.7.2 The Package System.Address_To_Access_Conversions
    13.8 Machine Code Insertions
    13.9 Unchecked Type Conversions
        13.9.1 Data Validity
        13.9.2 The Valid Attribute
    13.10 Unchecked Access Value Creation
    13.11 Storage Management
        13.11.1 The Max_Size_In_Storage_Elements Attribute
        13.11.2 Unchecked Storage Deallocation
        13.11.3 Pragma Controlled
    13.12 Pragma Restrictions
        13.12.1 Language-Defined Restrictions
    13.13 Streams
        13.13.1 The Package Streams
        13.13.2 Stream-Oriented Attributes
    13.14 Freezing Rules

The Standard Libraries

A. Predefined Language Environment
    A.1 The Package Standard
    A.2 The Package Ada
    A.3 Character Handling
        A.3.1 The Package Characters
        A.3.2 The Package Characters.Handling
        A.3.3 The Package Characters.Latin_1
    A.4 String Handling
        A.4.1 The Package Strings
        A.4.2 The Package Strings.Maps
        A.4.3 Fixed-Length String Handling
        A.4.4 Bounded-Length String Handling
        A.4.5 Unbounded-Length String Handling
        A.4.6 String-Handling Sets and Mappings
        A.4.7 Wide_String Handling
    A.5 The Numerics Packages
        A.5.1 Elementary Functions
        A.5.2 Random Number Generation
        A.5.3 Attributes of Floating Point Types
        A.5.4 Attributes of Fixed Point Types
    A.6 Input-Output
    A.7 External Files and File Objects
    A.8 Sequential and Direct Files
        A.8.1 The Generic Package Sequential_IO
        A.8.2 File Management
        A.8.3 Sequential Input-Output Operations
        A.8.4 The Generic Package Direct_IO
        A.8.5 Direct Input-Output Operations
    A.9 The Generic Package Storage_IO
    A.10 Text Input-Output
        A.10.1 The Package Text_IO
        A.10.2 Text File Management
        A.10.3 Default Input, Output, and Error Files
        A.10.4 Specification of Line and Page Lengths
        A.10.5 Operations on Columns, Lines, and Pages
        A.10.6 Get and Put Procedures
        A.10.7 Input-Output of Characters and Strings
        A.10.8 Input-Output for Integer Types
        A.10.9 Input-Output for Real Types
        A.10.10 Input-Output for Enumeration Types
    A.11 Wide Text Input-Output
    A.12 Stream Input-Output
        A.12.1 The Package Streams.Stream_IO
        A.12.2 The Package Text_IO.Text_Streams
        A.12.3 The Package Wide_Text_IO.Text_Streams
    A.13 Exceptions in Input-Output
    A.14 File Sharing
    A.15 The Package Command_Line

B. Interface to Other Languages
    B.1 Interfacing Pragmas
    B.2 The Package Interfaces
    B.3 Interfacing with C
        B.3.1 The Package Interfaces.C.Strings
        B.3.2 The Generic Package Interfaces.C.Pointers
        B.3.3 Pragma Unchecked_Union
    B.4 Interfacing with COBOL
    B.5 Interfacing with Fortran

C. Systems Programming
    C.1 Access to Machine Operations
    C.2 Required Representation Support
    C.3 Interrupt Support
        C.3.1 Protected Procedure Handlers
        C.3.2 The Package Interrupts
    C.4 Preelaboration Requirements
    C.5 Pragma Discard_Names
    C.6 Shared Variable Control
    C.7 Task Identification and Attributes
        C.7.1 The Package Task_Identification
        C.7.2 The Package Task_Attributes

D. Real-Time Systems
    D.1 Task Priorities
    D.2 Priority Scheduling
        D.2.1 The Task Dispatching Model
        D.2.2 The Standard Task Dispatching Policy
    D.3 Priority Ceiling Locking
    D.4 Entry Queuing Policies
    D.5 Dynamic Priorities
    D.6 Preemptive Abort
    D.7 Tasking Restrictions
    D.8 Monotonic Time
    D.9 Delay Accuracy
    D.10 Synchronous Task Control
    D.11 Asynchronous Task Control
    D.12 Other Optimizations and Determinism Rules
    D.13 The Ravenscar Profile

E. Distributed Systems
    E.1 Partitions
    E.2 Categorization of Library Units
        E.2.1 Shared Passive Library Units
        E.2.2 Remote Types Library Units
        E.2.3 Remote Call Interface Library Units
    E.3 Consistency of a Distributed System
    E.4 Remote Subprogram Calls
        E.4.1 Pragma Asynchronous
        E.4.2 Example of Use of a Remote Access-to-Class-Wide Type
    E.5 Partition Communication Subsystem

F. Information Systems
    F.1 Machine_Radix Attribute Definition Clause
    F.2 The Package Decimal
    F.3 Edited Output for Decimal Types
        F.3.1 Picture String Formation
        F.3.2 Edited Output Generation
        F.3.3 The Package Text_IO.Editing
        F.3.4 The Package Wide_Text_IO.Editing

G. Numerics
    G.1 Complex Arithmetic
        G.1.1 Complex Types
        G.1.2 Complex Elementary Functions
        G.1.3 Complex Input-Output
        G.1.4 The Package Wide_Text_IO.Complex_IO
    G.2 Numeric Performance Requirements
        G.2.1 Model of Floating Point Arithmetic
        G.2.2 Model-Oriented Attributes of Floating Point Types
        G.2.3 Model of Fixed Point Arithmetic
        G.2.4 Accuracy Requirements for the Elementary Functions
        G.2.5 Performance Requirements for Random Number Generation
        G.2.6 Accuracy Requirements for Complex Arithmetic

H. Safety and Security
    H.1 Pragma Normalize_Scalars
    H.2 Documentation of Implementation Decisions
    H.3 Reviewable Object Code
        H.3.1 Pragma Reviewable
        H.3.2 Pragma Inspection_Point
    H.4 Safety and Security Restrictions

J. Obsolescent Features
    J.1 Renamings of Ada 83 Library Units
    J.2 Allowed Replacements of Characters
    J.3 Reduced Accuracy Subtypes
    J.4 The Constrained Attribute
    J.5 ASCII
    J.6 Numeric_Error
    J.7 At Clauses
        J.7.1 Interrupt Entries
    J.8 Mod Clauses
    J.9 The Storage_Size Attribute

K. Language-Defined Attributes

L. Language-Defined Pragmas

M. Implementation-Defined Characteristics

N. Glossary

P. Syntax Summary
    P.1 Syntax Rules
    P.2 Syntax Cross Reference

Index

This is the Annotated Ada Reference Manual. 

Discussion: This document is the Annotated Ada Reference Manual (AARM). It contains the entire text of the Ada 95 standard (ISO/IEC 8652:1995), plus various annotations. It is intended primarily for compiler writers, validation test writers, and other language lawyers. The annotations include detailed rationale for individual rules and explanations of some of the more arcane interactions among the rules. 

Other available Ada documents include: 

Rationale for the Ada Programming Language - 1995 edition, which gives an introduction to the new features of Ada, and explains the rationale behind them. Programmers should read this first.

Changes to Ada - 1987 to 1995. This document lists in detail the changes made to the 1987 edition of the standard.

The Ada Reference Manual (RM). This directly corresponds to the International Standard - ISO/IEC 8652:1995.

Design Goals

Ada was originally designed with three overriding concerns: program reliability and maintenance, programming as a human activity, and efficiency. This revision to the language was designed to provide greater flexibility and extensibility, additional control over storage management and synchronization, and standardized packages oriented toward supporting important application areas, while at the same time retaining the original emphasis on reliability, maintainability, and efficiency.

The need for languages that promote reliability and simplify maintenance is well established. Hence emphasis was placed on program readability over ease of writing. For example, the rules of the language require that program variables be explicitly declared and that their type be specified. Since the type of a variable is invariant, compilers can ensure that operations on variables are compatible with the properties intended for objects of the type. Furthermore, error-prone notations have been avoided, and the syntax of the language avoids the use of encoded forms in favor of more English-like constructs. Finally, the language offers support for separate compilation of program units in a way that facilitates program development and maintenance, and which provides the same degree of checking between units as within a unit.

Concern for the human programmer was also stressed during the design. Above all, an attempt was made to keep to a relatively small number of underlying concepts integrated in a consistent and systematic way while continuing to avoid the pitfalls of excessive involution. The design especially aims to provide language constructs that correspond intuitively to the normal expectations of users.

Like many other human activities, the development of programs is becoming ever more decentralized and distributed. Consequently, the ability to assemble a program from independently produced software components continues to be a central idea in the design. The concepts of packages, of private types, and of generic units are directly related to this idea, which has ramifications in many other aspects of the language. An allied concern is the maintenance of programs to match changing requirements; type extension and the hierarchical library enable a program to be modified while minimizing disturbance to existing tested and trusted components.

No language can avoid the problem of efficiency. Languages that require over-elaborate compilers, or that lead to the inefficient use of storage or execution time, force these inefficiencies on all machines and on all programs. Every construct of the language was examined in the light of present implementation techniques. Any proposed construct whose implementation was unclear or that required excessive machine resources was rejected.

Language Summary

An Ada program is composed of one or more program units. Program units may be subprograms (which define executable algorithms), packages (which define collections of entities), task units (which define concurrent computations), protected units (which define operations for the coordinated sharing of data between tasks), or generic units (which define parameterized forms of packages and subprograms). Each program unit normally consists of two parts: a specification, containing the information that must be visible to other units, and a body, containing the implementation details, which need not be visible to other units. Most program units can be compiled separately.

This distinction of the specification and body, and the ability to compile units separately, allows a program to be designed, written, and tested as a set of largely independent software components.

An Ada program will normally make use of a library of program units of general utility. The language provides means whereby individual organizations can construct their own libraries. All libraries are structured in a hierarchical manner; this enables the logical decomposition of a subsystem into individual components. The text of a separately compiled program unit must name the library units it requires.

Program Units

A subprogram is the basic unit for expressing an algorithm. There are two kinds of subprograms: procedures and functions. A procedure is the means of invoking a series of actions. For example, it may read data, update variables, or produce some output. It may have parameters, to provide a controlled means of passing information between the procedure and the point of call. A function is the means of invoking the computation of a value. It is similar to a procedure, but in addition will return a result.

A package is the basic unit for defining a collection of logically related entities. For example, a package can be used to define a set of type declarations and associated operations. Portions of a package can be hidden from the user, thus allowing access only to the logical properties expressed by the package specification.

Subprogram and package units may be compiled separately and arranged in hierarchies of parent and child units giving fine control over visibility of the logical properties and their detailed implementation.

A task unit is the basic unit for defining a task whose sequence of actions may be executed concurrently with those of other tasks. Such tasks may be implemented on multicomputers, multiprocessors, or with interleaved execution on a single processor. A task unit may define either a single executing task or a task type permitting the creation of any number of similar tasks.

A protected unit is the basic unit for defining protected operations for the coordinated use of data shared between tasks. Simple mutual exclusion is provided automatically, and more elaborate sharing protocols can be defined. A protected operation can either be a subprogram or an entry. A protected entry specifies a Boolean expression (an entry barrier) that must be true before the body of the entry is executed. A protected unit may define a single protected object or a protected type permitting the creation of several similar objects.

Declarations and Statements

The body of a program unit generally contains two parts: a declarative part, which defines the logical entities to be used in the program unit, and a sequence of statements, which defines the execution of the program unit.

The declarative part associates names with declared entities. For example, a name may denote a type, a constant, a variable, or an exception. A declarative part also introduces the names and parameters of other nested subprograms, packages, task units, protected units, and generic units to be used in the program unit.

The sequence of statements describes a sequence of actions that are to be performed. The statements are executed in succession (unless a transfer of control causes execution to continue from another place).

An assignment statement changes the value of a variable. A procedure call invokes execution of a procedure after associating any actual parameters provided at the call with the corresponding formal parameters.

Case statements and if statements allow the selection of an enclosed sequence of statements based on the value of an expression or on the value of a condition.

The loop statement provides the basic iterative mechanism in the language. A loop statement specifies that a sequence of statements is to be executed repeatedly as directed by an iteration scheme, or until an exit statement is encountered.

A block statement comprises a sequence of statements preceded by the declaration of local entities used by the statements.

Certain statements are associated with concurrent execution. A delay statement delays the execution of a task for a specified duration or until a specified time. An entry call statement is written as a procedure call statement; it requests an operation on a task or on a protected object, blocking the caller until the operation can be performed. A called task may accept an entry call by executing a corresponding accept statement, which specifies the actions then to be performed as part of the rendezvous with the calling task. An entry call on a protected object is processed when the corresponding entry barrier evaluates to true, whereupon the body of the entry is executed. The requeue statement permits the provision of a service as a number of related activities with preference control. One form of the select statement allows a selective wait for one of several alternative rendezvous. Other forms of the select statement allow conditional or timed entry calls and the asynchronous transfer of control in response to some triggering event.

Execution of a program unit may encounter error situations in which normal program execution cannot continue. For example, an arithmetic computation may exceed the maximum allowed value of a number, or an attempt may be made to access an array component by using an incorrect index value. To deal with such error situations, the statements of a program unit can be textually followed by exception handlers that specify the actions to be taken when the error situation arises. Exceptions can be raised explicitly by a raise statement.

Data Types

Every object in the language has a type, which characterizes a set of values and a set of applicable operations. The main classes of types are elementary types (comprising enumeration, numeric, and access types) and composite types (including array and record types).

An enumeration type defines an ordered set of distinct enumeration literals, for example a list of states or an alphabet of characters. The enumeration types Boolean, Character, and Wide_Character are predefined.

Numeric types provide a means of performing exact or approximate numerical computations. Exact computations use integer types, which denote sets of consecutive integers. Approximate computations use either fixed point types, with absolute bounds on the error, or floating point types, with relative bounds on the error. The numeric types Integer, Float, and Duration are predefined.

Composite types allow definitions of structured objects with related components. The composite types in the language include arrays and records. An array is an object with indexed components of the same type. A record is an object with named components of possibly different types. Task and protected types are also forms of composite types. The array types String and Wide_String are predefined.

Record, task, and protected types may have special components called discriminants which parameterize the type. Variant record structures that depend on the values of discriminants can be defined within a record type.

Access types allow the construction of linked data structures. A value of an access type represents a reference to an object declared as aliased or to an object created by the evaluation of an allocator. Several variables of an access type may designate the same object, and components of one object may designate the same or other objects. Both the elements in such linked data structures and their relation to other elements can be altered during program execution. Access types also permit references to subprograms to be stored, passed as parameters, and ultimately dereferenced as part of an indirect call.

Private types permit restricted views of a type. A private type can be defined in a package so that only the logically necessary properties are made visible to the users of the type. The full structural details that are externally irrelevant are then only available within the package and any child units.

From any type a new type may be defined by derivation. A type, together with its derivatives (both direct and indirect) form a derivation class. Class-wide operations may be defined that accept as a parameter an operand of any type in a derivation class. For record and private types, the derivatives may be extensions of the parent type. Types that support these object-oriented capabilities of class-wide operations and type extension must be tagged, so that the specific type of an operand within a derivation class can be identified at run time. When an operation of a tagged type is applied to an operand whose specific type is not known until run time, implicit dispatching is performed based on the tag of the operand.

The concept of a type is further refined by the concept of a subtype, whereby a user can constrain the set of allowed values of a type. Subtypes can be used to define subranges of scalar types, arrays with a limited set of index values, and records and private types with particular discriminant values.

Other Facilities

Representation clauses can be used to specify the mapping between types and features of an underlying machine. For example, the user can specify that objects of a given type must be represented with a given number of bits, or that the components of a record are to be represented using a given storage layout. Other features allow the controlled use of low level, nonportable, or implementation-dependent aspects, including the direct insertion of machine code.

The predefined environment of the language provides for input-output and other capabilities (such as string manipulation and random number generation) by means of standard library packages. Input-output is supported for values of user-defined as well as of predefined types. Standard means of representing values in display form are also provided. Other standard library packages are defined in annexes of the standard to support systems with specialized requirements.

Finally, the language provides a powerful means of parameterization of program units, called generic program units. The generic parameters can be types and subprograms (as well as objects and packages) and so allow general algorithms and data structures to be defined that are applicable to all types of a given class. 

Version=[5],Language Changes

This International Standard replaces the first edition of 1987. In this edition, the following major language changes have been incorporated: 

Support for standard 8-bit and 16-bit character sets. See Section 2, 3.5.2, 3.6.3, A.1, A.3, and A.4.

Object-oriented programming with run-time polymorphism. See the discussions of classes, derived types, tagged types, record extensions, and private extensions in clauses 3.4, 3.9, and 7.3. See also the new forms of generic formal parameters that are allowed by 12.5.1, "Formal Private and Derived Types" and 12.7, "Formal Packages".

Access types have been extended to allow an access value to designate a subprogram or an object declared by an object declaration (as opposed to just a heap-allocated object). See 3.10.

Efficient data-oriented synchronization is provided via protected types. See Section 9.

The library units of a library may be organized into a hierarchy of parent and child units. See Section 10.

Additional support has been added for interfacing to other languages. See Annex B.

The Specialized Needs Annexes have been added to provide specific support for certain application areas: 

Annex C, "Systems Programming"

Annex D, "Real-Time Systems"

Annex E, "Distributed Systems"

Annex F, "Information Systems"

Annex G, "Numerics"

Annex H, "Safety and Security" 

Instructions for Comment Submission

Informal comments on this Reference Manual may be sent via e-mail to ada-comment@sw-eng.falls-church.va.us. If appropriate, the Project Editor will initiate the defect correction procedure.

Comments should use the following format: 

	!topic Title summarizing comment
	!reference RM95-ss.ss(pp)
	!from Author Name yy-mm-dd
	!keywords keywords related to topic
	!discussion

	text of discussion

where ss.ss is the section, clause or subclause number, pp is the paragraph number where applicable, and yy-mm-dd is the date the comment was sent. The date is optional, as is the !keywords line.

Multiple comments per e-mail message are acceptable. Please use a descriptive "Subject" in your e-mail message.

When correcting typographical errors or making minor wording suggestions, please put the correction directly as the topic of the comment; use square brackets [ ] to indicate text to be omitted and curly braces { } to indicate text to be added, and provide enough context to make the nature of the suggestion self-evident or put additional information in the body of the comment, for example: 

	!topic [c]{C}haracter
	!topic it[']s meaning is not defined

Formal requests for interpretations and for reporting defects in the International Standard may be made in accordance with the ISO/IEC JTC 1 Directives and the ISO/IEC JTC 1/SC 22 policy for interpretations. National Bodies may submit a Defect Report to ISO/IEC JTC 1/SC 22 for resolution under the JTC 1 procedures. A response will be provided and, if appropriate, a Technical Corrigendum will be issued in accordance with the procedures.

Acknowledgements

This Reference Manual was prepared by the Ada 9X Mapping/Revision Team based at Intermetrics, Inc., which has included: W. Carlson, Program Manager; T. Taft, Technical Director; J. Barnes (consultant); B. Brosgol (consultant); R. Duff (Oak Tree Software); M. Edwards; C. Garrity; R. Hilliard; O. Pazy (consultant); D. Rosenfeld; L. Shafer; W. White; M. Woodger.

The following consultants to the Ada 9X Project contributed to the Specialized Needs Annexes: T. Baker (Real-Time/Systems Programming - SEI, FSU); K. Dritz (Numerics - Argonne National Laboratory); A. Gargaro (Distributed Systems - Computer Sciences); J. Goodenough (Real-Time/Systems Programming - SEI); J. McHugh (Secure Systems - consultant); B. Wichmann (Safety-Critical Systems - NPL: UK).

This work was regularly reviewed by the Ada 9X Distinguished Reviewers and the members of the Ada 9X Rapporteur Group (XRG): E. Ploedereder, Chairman of DRs and XRG (University of Stuttgart: Germany); B. Bardin (Hughes); J. Barnes (consultant: UK); B. Brett (DEC); B. Brosgol (consultant); R. Brukardt (RR Software); N. Cohen (IBM); R. Dewar (NYU); G. Dismukes (TeleSoft); A. Evans (consultant); A. Gargaro (Computer Sciences); M. Gerhardt (ESL); J. Goodenough (SEI); S. Heilbrunner (University of Salzburg: Austria); P. Hilfinger (UC/Berkeley); B. Källberg (CelsiusTech: Sweden); M. Kamrad II (Unisys); J. van Katwijk (Delft University of Technology: The Netherlands); V. Kaufman (Russia); P. Kruchten (Rational); R. Landwehr (CCI: Germany); C. Lester (Portsmouth Polytechnic: UK); L. Månsson (TELIA Research: Sweden); S. Michell (Multiprocessor Toolsmiths: Canada); M. Mills (US Air Force); D. Pogge (US Navy); K. Power (Boeing); O. Roubine (Verdix: France); A. Strohmeier (Swiss Fed Inst of Technology: Switzerland); W. Taylor (consultant: UK); J. Tokar (Tartan); E. Vasilescu (Grumman); J. Vladik (Prospeks s.r.o.: Czech Republic); S. Van Vlierberghe (OFFIS: Belgium). 

Other valuable feedback influencing the revision process was provided by the Ada 9X Language Precision Team (Odyssey Research Associates), the Ada 9X User/Implementer Teams (AETECH, Tartan, TeleSoft), the Ada 9X Implementation Analysis Team (New York University) and the Ada community-at-large.

Special thanks go to R. Mathis, Convenor of ISO/IEC JTC 1/SC 22 Working Group 9. 

The Ada 9X Project was sponsored by the Ada Joint Program Office. Christine M. Anderson at the Air Force Phillips Laboratory (Kirtland AFB, NM) was the project manager.

