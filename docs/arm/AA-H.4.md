---
sidebar_position:  186
---

# H.4  High Integrity Restrictions

{AI05-0299-1} This subclause defines restrictions that can be used with pragma Restrictions (see 13.12); these facilitate the demonstration of program correctness by allowing tailored versions of the run-time system. 

Discussion: {AI05-0005-1} Note that the restrictions are absolute. If a partition has 100 library units and just one needs Unchecked_Conversion, then the pragma cannot be used to ensure the other 99 units do not use Unchecked_Conversion. Note also that these are restrictions on all Ada code within a partition, and therefore it might not be evident from the specification of a package whether a restriction can be imposed.


#### Static Semantics

This paragraph was deleted.{AI95-00347-01} {AI95-00394-01} 

{AI95-00394-01} The following restriction_[identifier](./AA-2.3#S0002)s are language defined:

Tasking-related restriction:

No_Protected_Types There are no declarations of protected types or protected objects. 

Memory-management related restrictions:

No_Allocators There are no occurrences of an [allocator](./AA-4.8#S0164).

{8652/0042} {AI95-00130} No_Local_Allocators [Allocator](./AA-4.8#S0164)s are prohibited in subprograms, generic subprograms, tasks, and entry bodies. 

Ramification: Thus [allocator](./AA-4.8#S0164)s are permitted only in expressions whose evaluation can only be performed before the main subprogram is invoked. 

This paragraph was deleted.{8652/0042} {AI95-00130} 

{AI05-0152-1} {AI05-0262-1} No_Anonymous_Allocators There are no [allocator](./AA-4.8#S0164)s of anonymous access types.

{AI05-0190-1} No_Coextensions There are no coextensions. See 3.10.2.

{AI05-0190-1} No_Access_Parameter_Allocators [Allocator](./AA-4.8#S0164)s are not permitted as the actual parameter to an access parameter. See 6.1.

This paragraph was deleted.{AI95-00394-01} 

Immediate_Reclamation Except for storage occupied by objects created by [allocator](./AA-4.8#S0164)s and not deallocated via unchecked deallocation, any storage reserved at run time for an object is immediately reclaimed when the object no longer exists. 

Discussion: Immediate reclamation would apply to storage created by the compiler, such as for a return value from a function whose size is not known at the call site. 

Exception-related restriction:

{AI12-0344-1} No_Exceptions [Raise_statement](./AA-11.3#S0308)s and [exception_handler](./AA-11.2#S0305)s are not allowed. No language-defined runtime checks are generated; however, a runtime check performed automatically by the hardware is permitted. The callable entity associated with a [procedural_iterator](./AA-5.5#S0185) (see 5.5.3) is considered to not allow exit, independent of the value of its Allows_Exit aspect.

Discussion: This restriction mirrors a method of working that is quite common in the safety area. The programmer is required to show that exceptions cannot be raised. Then a simplified run-time system is used without exception handling. However, some hardware checks may still be enforced. If the software check would have failed, or if the hardware check actually fails, then the execution of the program is unpredictable. There are obvious dangers in this approach, but it is similar to programming at the assembler level.

Other restrictions:

No_Floating_Point Uses of predefined floating point types and operations, and declarations of new floating point types, are not allowed. 

Discussion: {AI95-00114-01} The intention is to avoid the use of floating point hardware at run time, but this is expressed in language terms. It is conceivable that floating point is used implicitly in some contexts, say fixed point type conversions of high accuracy. However, the Implementation Requirements below make it clear that the restriction would apply to the "run-time system" and hence not be allowed. This restriction could be used to inform a compiler that a variant of the architecture is being used which does not have floating point instructions.

No_Fixed_Point Uses of predefined fixed point types and operations, and declarations of new fixed point types, are not allowed. 

Discussion: This restriction would have the side effect of prohibiting the [delay_relative_statement](./AA-9.6#S0268). As with the No_Floating_Point restriction, this might be used to avoid any question of rounding errors. Unless an Ada run-time is written in Ada, it seems hard to rule out implicit use of fixed point, since at the machine level, fixed point is virtually the same as integer arithmetic.

This paragraph was deleted.{AI95-00394-01} 

No_Access_Subprograms The declaration of access-to-subprogram types is not allowed. 

Discussion: Most critical applications would require some restrictions or additional validation checks on uses of access-to-subprogram types. If the application does not require the functionality, then this restriction provides a means of ensuring the design requirement has been satisfied. The same applies to several of the following restrictions, and to restriction No_Dependence =&gt Ada.Unchecked_Conversion. 

No_Unchecked_Access The Unchecked_Access attribute is not allowed.

No_Dispatch Occurrences of T'Class are not allowed, for any (tagged) subtype T.

{AI95-00285-01} {AI12-0318-1} No_IO Semantic dependence on any of the library units Sequential_IO, Direct_IO, Text_IO, Wide_Text_IO, Wide_Wide_Text_IO, Stream_IO, or Directories is not allowed. 

Discussion: Excluding the input-output facilities of an implementation may be needed in those environments which cannot support the supplied functionality. A program in such an environment is likely to require some low level facilities or a call on a non-Ada feature.

No_Delay [Delay_Statement](./AA-9.6#S0266)s and semantic dependence on package Calendar are not allowed. 

Ramification: This implies that [delay_alternative](./AA-9.7#S0274)s in a [select_statement](./AA-9.7#S0269) are prohibited.

The purpose of this restriction is to avoid the need for timing facilities within the run-time system.

No_Recursion As part of the execution of a subprogram, the same subprogram is not invoked.

No_Reentrancy During the execution of a subprogram by a task, no other task invokes the same subprogram.

{AI12-0079-3} No_Unspecified_GlobalsNo library-level entity shall have a Global aspect of Unspecified, either explicitly or by default. No library-level entity shall have a Global'Class aspect of Unspecified, explicitly or by default, if it is used as part of a dispatching call.

Ramification: Global'Class need not be specified on an operation if there are no dispatching calls to the operation, or if all of the dispatching calls are covered by [dispatching_operation_specifier](./AA-H.7#S0366)s for operations with such calls (see H.7). 

{AI12-0079-3} {AI12-0380-1} No_Hidden_Indirect_GlobalsWhen within a context where an applicable global aspect is neither Unspecified nor in out all, any execution within such a context does neither of the following:

Update (or return a writable reference to) a variable that is reachable via a sequence of zero or more dereferences of access-to-object values from a parameter of a visibly access-to-constant type, from a part of a non-access-type formal parameter of mode in (after any overriding  see H.7), or from a global that has mode in or is not within the applicable global variable set, unless the initial dereference is of a part of a formal parameter or global that is visibly of an access-to-variable type;

Read (or return a readable reference to) a variable that is reachable via a sequence of zero or more dereferences of access-to-object values from a global that is not within the applicable global variable set, unless the initial dereference is of a part of a formal parameter or global that is visibly of an access-to-object type. 

Ramification: The above two rules specify that any hidden indirect references are covered by the global or formal parameter modes that apply, and are not subject to alternative paths of access (such as aliasing) that could result in conflicts. On the other hand, any visible access-to-object parts are allowed to designate objects that are accessible via other means, and side-effects on such objects are permitted if the value is visibly of an access-to-variable type. Such effects do not need to be covered by the applicable global aspect(s), but are rather for the caller to worry about. 

For the purposes of the above rules:

a part of an object is visibly of an access type if the type of the object is declared immediately within the visible part of a package specification, and at the point of declaration of the type the part is visible and of an access type;

a function returns a writable reference to V if it returns a result with a part that is visibly of an access-to-variable type designating V; similarly, a function returns a readable reference to V if it returns a result with a part that is visibly of an access-to-constant type designating V;

if an applicable global variable set includes a package name, and the collection of some pool-specific access type (see 7.6.1) is implicitly declared in a part of the declarative region of the package included within the global variable set, then all objects allocated from that collection are considered included within the global variable set. 

The consequences of violating the No_Hidden_Indirect_Globals restriction is implementation-defined. Any aspects or other means for identifying such violations prior to or during execution are implementation-defined. 

Implementation defined: The consequences of violating No_Hidden_Indirect_Globals.

Discussion: We do not make violations automatically erroneous, because if the implementation chooses to never fully trust it, there is nothing erroneous that can happen. If an implementation chooses to trust the restriction, and performs some optimization as a result of the restriction, the implementation would define such a violation as erroneous. Such an implementation might also endeavor to detect most violations, perhaps by providing additional aspects, thereby reducing the situations which result in erroneous execution. Implementations might detect some but not all violations of the restrictions. Implementations that completely ignore the restriction should treat the restriction as an unsupported capability of Annex H, "High Integrity Systems". 


#### Dynamic Semantics

{AI12-0020-1} {AI12-0340-1} The following restriction_parameter_[identifier](./AA-2.3#S0002) is language defined:

Max_Image_LengthSpecifies the maximum length for the result of an Image, Wide_Image, or Wide_Wide_Image attribute. Violation of this restriction results in the raising of Program_Error at the point of the invocation of an image attribute. 


#### Implementation Requirements

{AI95-00394-01} An implementation of this Annex shall support: 

the restrictions defined in this subclause; and

{AI05-0189-1} the following restrictions defined in D.7: No_Task_Hierarchy, No_Abort_Statement, No_Implicit_Heap_Allocation, No_Standard_Allocators_After_Elaboration; and

{AI95-00347-01} the pragma Profile(Ravenscar); and 

Discussion: {AI95-00347-01} The reference to pragma Profile(Ravenscar) is intended to show that properly restricted tasking is appropriate for use in high integrity systems. The Ada 95 Annex seemed to suggest that tasking was inappropriate for such systems. 

the following uses of restriction_parameter_[identifier](./AA-2.3#S0002)s defined in D.7[, which are checked prior to program execution]: 

Max_Task_Entries =&gt 0,

Max_Asynchronous_Select_Nesting =&gt 0, and

Max_Tasks =&gt 0. 

{AI12-0020-1} {AI12-0340-1} If a Max_Image_Length restriction applies to any compilation unit in the partition, then for any subtype S, S'Image, S'Wide_Image, and S'Wide_Wide_Image shall be implemented within that partition without any dynamic allocation.

Implementation Note: {AI12-0340-1} {AI12-0384-2} This can be accomplished by using an object of the Text_Buffers.Bounded.Buffer_Type with the maximum characters as specified in the Max_Image_Length restriction, with a raise of Program_Error afterward if Text_Truncated (Buf) is True after the call on Put_Image (Buf, Arg). 

{AI05-0263-1} {AI05-0272-1} {AI12-0308-1} If an implementation supports [pragma](./AA-2.8#S0019) Restrictions for a particular argument, then except for the restrictions No_Access_Subprograms, No_Unchecked_Access, No_Specification_of_Aspect, No_Use_of_Attribute, No_Use_of_Pragma, No_Dependence =&gt Ada.Unchecked_Conversion, and No_Dependence =&gt Ada.Unchecked_Deallocation, the associated restriction applies to the run-time system. 

Reason: Permission is granted for the run-time system to use the specified otherwise-restricted features, since the use of these features may simplify the run-time system by allowing more of it to be written in Ada. 

Discussion: The restrictions that are applied to the partition are also applied to the run-time system. For example, if No_Floating_Point is specified, then an implementation that uses floating point for implementing the delay statement (say) would require that No_Floating_Point is only used in conjunction with No_Delay. It is clearly important that restrictions are effective so that Max_Tasks=0 does imply that tasking is not used, even implicitly (for input-output, say).

An implementation of tasking could be produced based upon a run-time system written in Ada in which the rendezvous was controlled by protected types. In this case, No_Protected_Types could only be used in conjunction with Max_Task_Entries=0. Other implementation dependencies could be envisaged.

If the run-time system is not written in Ada, then the wording needs to be applied in an appropriate fashion.


#### Documentation Requirements

If a pragma Restrictions(No_Exceptions) is specified, the implementation shall document the effects of all constructs where language-defined checks are still performed automatically (for example, an overflow check performed by the processor). 

This paragraph was deleted.

Documentation Requirement: If a pragma Restrictions(No_Exceptions) is specified, the effects of all constructs where language-defined checks are still performed.

Discussion: {AI95-00114-01} The documentation requirements here are quite difficult to satisfy. One method is to review the object code generated and determine the checks that are still present, either explicitly, or implicitly within the architecture. As another example from that of overflow, consider the question of dereferencing a null pointer. This could be undertaken by a memory access trap when checks are performed. When checks are suppressed via the argument No_Exceptions, it would not be necessary to have the memory access trap mechanism enabled.


#### Erroneous Execution

Program execution is erroneous if pragma Restrictions(No_Exceptions) has been specified and the conditions arise under which a generated language-defined runtime check would fail. 

Discussion: The situation here is very similar to the application of pragma Suppress. Since users are removing some of the protection the language provides, they had better be careful!

Program execution is erroneous if pragma Restrictions(No_Recursion) has been specified and a subprogram is invoked as part of its own execution, or if pragma Restrictions(No_Reentrancy) has been specified and during the execution of a subprogram by a task, another task invokes the same subprogram. 

Discussion: {AI05-0005-1} In practice, many implementations might not exploit the absence of recursion or need for reentrancy, in which case the program execution would be unaffected by the use of recursion or reentrancy, even though the program is still formally erroneous.

This paragraph was deleted.

NOTE   {AI95-00394-01} {AI12-0440-1} Uses of restriction_parameter_[identifier](./AA-2.3#S0002) No_Dependence defined in 13.12.1: No_Dependence =&gt Ada.Unchecked_Deallocation and No_Dependence =&gt Ada.Unchecked_Conversion can be appropriate for high-integrity systems. Other uses of No_Dependence can also be appropriate for high-integrity systems. 

Discussion: The specific mention of these two uses is meant to replace the identifiers now banished to J.13, "Dependence Restriction Identifiers".

Restriction No_Dependence =&gt Ada.Unchecked_Deallocation would be useful in those contexts in which heap storage is needed on program start-up, but need not be increased subsequently. The danger of a dangling pointer can therefore be avoided. 


#### Extensions to Ada 95

{8652/0042} {AI95-00130-01} No_Local_Allocators no longer prohibits generic instantiations. 


#### Wording Changes from Ada 95

{AI95-00285-01} Wide_Wide_Text_IO (which is new) is added to the No_IO restriction.

{AI95-00347-01} {AI05-0299-1} The title of this subclause was changed to match the change to the Annex title. Pragma Profile(Ravenscar) is part of this annex.

{AI95-00394-01} Restriction No_Dependence is used instead of special restriction_[identifier](./AA-2.3#S0002)s. The old names are banished to Obsolescent Features (see J.13).

{AI95-00394-01} The bizarre wording "apply in this Annex" (which no one quite can explain the meaning of) is banished. 


#### Extensions to Ada 2005

{AI05-0152-1} {AI05-0190-1} Restrictions No_Anonymous_Allocators, No_Coextensions, and No_Access_Parameter_Allocators are new. 


#### Wording Changes from Ada 2005

{AI05-0189-1} New restriction No_Standard_Allocators_After_Elaboration is added to the list of restrictions that are required by this annex.

{AI05-0263-1} Correction: Ada 2005 restriction No_Dependence is added where needed (this was missed in Ada 2005).

{AI05-0272-1} Restrictions against individual aspects, pragmas, and attributes do not apply to the run-time system, in order that an implementation can use whatever aspects, pragmas, and attributes are needed to do the job. For instance, attempting to write a run-time system for Linux that does not use the Import aspect would be very difficult and probably is not what the user is trying to prevent anyway. 


#### Incompatibilities With Ada 2012

{AI12-0318-1} Correction: Restriction No_IO now excludes use of Ada.Directories. If a program using No_IO used Ada.Directories, it would be legal in Ada 2012 and illegal in Ada 2022. However, given the role of Ada.Directories as a support package for the other packages that are excluded by No_IO, it seems unlikely that any use of the restriction would use this package (and it's possible that implementations wouldn't support its use with No_IO anyway). 


#### Extensions to Ada 2012

{AI12-0020-1} Restriction Max_Image_Length is new.

{AI12-0079-3} Restrictions No_Unspecified_Globals and No_Hidden_Indirect_Globals are new. 


## H.4.1  Aspect No_Controlled_Parts


#### Static Semantics

{AI12-0256-1} {AI12-0403-1} For a type, the following type-related, operational aspect may be specified:

No_Controlled_PartsThe type of this aspect is Boolean. If True, the type and any descendants shall not have any controlled parts. If specified, the value of the expression shall be static. If not specified, the value of this aspect is False.

Aspect Description for No_Controlled_Parts: A specification that a type and its descendants do not have controlled parts.

{AI12-0256-1} The No_Controlled_Parts aspect is nonoverridable (see 13.1.1).

Discussion: {AI12-0407-1} Since this is a Boolean-valued aspect, the blanket restrictions defined by 13.1.1 apply to the specification of Boolean-valued aspects on descendants of types with such aspects. But we still need rules about inheritance from progenitors and about hiding the aspect; it would be too painful to repeat those rules here (and have future maintenance fixes not get applied to this aspect). 


#### Legality Rules

{AI12-0256-1} {AI12-0407-1} If No_Controlled_Parts is True for a type, no component of the type shall have a controlled part nor shall the type itself be controlled. For the purposes of this rule, a type has a controlled part if its full type has a controlled part; this is applied recursively. In addition to the places where Legality Rules normally apply (see 12.3), this rule also applies in the private part of an instance of a generic unit.

Discussion: {AI12-0407-1} This check breaks privacy by looking at the full definition of all of the types involved. This is more like a representation aspect than an operational aspect, but representation aspects are not allowed on partial views and we need this aspect to be visible to clients. 

{AI12-0256-1} {AI12-0407-1} When enforcing the above rule within a generic body G or within the body of a generic unit declared within the declarative region of generic unit G, a generic formal private type of G and a generic formal derived type of G whose ancestor is a tagged type whose No_Controlled_Parts aspect is False are considered to have a controlled part.

Reason: This is a typical generic assume-the-worst rule. 

To be honest: {AI12-0407-1} If the ancestor of the generic derived type is class-wide, the aspect in question belongs to the specific type associated with the class-wide type. 


#### Extensions to Ada 2012

{AI12-0256-1} {AI12-0407-1} Aspect No_Controlled_Parts is new. 

