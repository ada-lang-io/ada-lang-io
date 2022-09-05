---
sidebar_position:  158
---

# D.7  Tasking Restrictions

{AI05-0299-1} [This subclause defines restrictions that can be used with a pragma Restrictions (see 13.12) to facilitate the construction of highly efficient tasking run-time systems.] 


#### Static Semantics

{AI12-0290-1} {AI12-0369-1} A scalar [expression](./AA-4.4#S0132) within a protected unit is said to be pure-barrier-eligible if it is one of the following: 

a static expression;

a [name](./AA-4.1#S0091) that statically names (see 4.9) a scalar subcomponent of the immediately enclosing protected unit;

a Count [attribute_reference](./AA-4.1#S0100) whose [prefix](./AA-4.1#S0093) statically denotes an entry declaration of the immediately enclosing unit;

a call to a predefined relational operator or boolean logical operator (and, or, xor, not), where each operand is pure-barrier-eligible;

a membership test whose tested_[simple_expression](./AA-4.4#S0138) is pure-barrier-eligible, and whose [membership_choice_list](./AA-4.4#S0136) meets the requirements for a static membership test (see 4.9);

a short-circuit control form both of whose operands are pure-barrier-eligible;

a [conditional_expression](./AA-4.5#S0148) all of whose [condition](./AA-4.5#S0150)s, selecting_[expression](./AA-4.4#S0132)s, and dependent_[expression](./AA-4.4#S0132)s are pure-barrier-eligible; or

a pure-barrier-eligible [expression](./AA-4.4#S0132) enclosed in parentheses. 

The following restriction_[identifier](./AA-2.3#S0002)s are language defined: 

{AI05-0013-1} {AI05-0216-1} No_Task_Hierarchy No task depends on a master other than the library-level master.

Ramification: {AI05-0216-1} This is equivalent to saying "no task depends on a master other than the master that is the execution of the body of the environment task of the partition", but it is much easier to understand. This is a post-compilation check, which can be checked at compile-time.

{AI05-0013-1} This disallows any function returning an object with a task part or coextension, even if called at the library level, as such a task would temporarily depend on a nested master (the master of the return statement), which is disallowed by this restriction. 

{8652/0042} {AI95-00130-01} {AI95-00360-01} {AI05-0013-1} No_Nested_Finalization Objects of a type that needs finalization (see 7.6) are declared only at library level. If an access type does not have library-level accessibility, then there are no [allocator](./AA-4.8#S0164)s of the type where the type determined by the [subtype_mark](./AA-3.2#S0028) of the [subtype_indication](./AA-3.2#S0027) or [qualified_expression](./AA-4.7#S0163) needs finalization. 

This paragraph was deleted.{8652/0042} {AI95-00130-01} 

Ramification: {AI05-0013-1} The second sentence prevents the declaration of objects of access types which would require nested finalization. It also prevents the declarations of coextensions that need finalization in a nested scope. The latter cannot be done by preventing the declaration of the objects, as it is not necessarily known if the coextension type needs finalization (it could be a limited view). 

{AI05-0211-1} No_Abort_Statements There are no [abort_statement](./AA-9.8#S0284)s, and there is no use of a [name](./AA-4.1#S0091) denoting Task_Identification.Abort_Task.

No_Terminate_Alternatives There are no [selective_accept](./AA-9.7#S0270)s with [terminate_alternative](./AA-9.7#S0275)s.

No_Task_Allocators There are no [allocator](./AA-4.8#S0164)s for task types or types containing task subcomponents.

{AI05-0224-1} In the case of an initialized [allocator](./AA-4.8#S0164) of an access type whose designated type is class-wide and limited, a check is made that the specific type of the allocated object has no task subcomponents. Program_Error is raised if this check fails. 

No_Implicit_Heap_Allocations There are no operations that implicitly require heap storage allocation to be performed by the implementation. The operations that implicitly require heap storage allocation are implementation defined. 

Implementation defined: Any operations that implicitly require heap storage allocation.

{AI95-00327-01} No_Dynamic_Priorities There are no semantic dependences on the package Dynamic_Priorities, and no occurrences of the attribute Priority. 

{AI95-00305-01} {AI95-00394-01} {AI05-0013-1} {AI05-0211-1} No_Dynamic_Attachment There is no use of a [name](./AA-4.1#S0091) denoting any of the operations defined in package Interrupts (Is_Reserved, Is_Attached, Current_Handler, Attach_Handler, Exchange_Handler, Detach_Handler, and Reference). 

Ramification: {AI05-0013-1} This includes 'Access and 'Address of any of these operations, as well as inherited versions of these operations. 

{AI12-0055-1} No_Dynamic_CPU_Assignment No task has the CPU aspect specified to be a non-static expression. Each task (including the environment task) that has the CPU aspect specified as Not_A_Specific_CPU will be assigned to a particular implementation-defined CPU. The same is true for the environment task when the CPU aspect is not specified. [Any other task without a CPU aspect will activate and execute on the same processor as its activating task.] 

Proof: The processor of a task without a CPU aspect is defined in D.16, and this restriction guarantees that the activator always has a CPU assigned. 

Reason: This restriction prevents any migration of tasks. 

Ramification: If no CPU aspects are specified, then the program will run on a single CPU, as all of the tasks will be activated directly or indirectly by the environment task, and the rules require the same CPU to be used as the activating task. 

Implementation defined: When restriction No_Dynamic_CPU_Assignment applies to a partition, the processor on which a task with a CPU value of a Not_A_Specific_CPU will execute.

{AI95-00305-01} {AI05-0013-1} No_Local_Protected_Objects Protected objects are declared only at library level.

{AI95-00297-01} {AI05-0013-1} No_Local_Timing_Events Timing_Events are declared only at library level.

{AI95-00305-01} No_Protected_Type_Allocators There are no [allocator](./AA-4.8#S0164)s for protected types or types containing protected type subcomponents.

{AI05-0224-1} In the case of an initialized [allocator](./AA-4.8#S0164) of an access type whose designated type is class-wide and limited, a check is made that the specific type of the allocated object has no protected subcomponents. Program_Error is raised if this check fails. 

{AI95-00305-01} {AI05-0211-1} No_Relative_Delay There are no [delay_relative_statement](./AA-9.6#S0268)s, and there is no use of a [name](./AA-4.1#S0091) that denotes the Timing_Events.Set_Handler subprogram that has a Time_Span parameter.

{AI95-00305-01} No_Requeue_Statements There are no [requeue_statement](./AA-9.5#S0265)s.

{AI95-00305-01} No_Select_Statements There are no [select_statement](./AA-9.7#S0269)s.

{AI95-00394-01} {AI05-0211-1} No_Specific_Termination_Handlers There is no use of a [name](./AA-4.1#S0091) denoting the Set_Specific_Handler and Specific_Handler subprograms in Task_Termination.

{AI12-0117-1} No_Tasks_Unassigned_To_CPU The CPU aspect is specified for the environment task. No CPU aspect is specified to be statically equal to Not_A_Specific_CPU. If aspect CPU is specified (dynamically) to the value Not_A_Specific_CPU, then Program_Error is raised. If Set_CPU or Delay_Until_And_Set_CPU are called with the CPU parameter equal to Not_A_Specific_CPU, then Program_Error is raised. 

Ramification: If this restriction is used in a context for which restriction No_Dynamic_CPU_Assignment is in effect, then no runtime check is needed when specifying the CPU aspect. If the restriction is used with the Ravenscar profile, no runtime checks are needed. 

{AI12-0290-1} Pure_Barriers The Boolean expression in each protected entry barrier is pure-barrier-eligible.

{AI95-00305-01} {AI05-0013-1} {AI12-0369-1} Simple_Barriers The Boolean expression in each entry barrier is either a static expression or a [name](./AA-4.1#S0091) that statically names (see 4.9) a subcomponent of the enclosing protected object.

The following restriction_parameter_[identifier](./AA-2.3#S0002)s are language defined: 

Max_Select_Alternatives Specifies the maximum number of alternatives in a [selective_accept](./AA-9.7#S0270).

Max_Task_Entries Specifies the maximum number of entries per task. The bounds of every entry family of a task unit shall be static, or shall be defined by a discriminant of a subtype whose corresponding bound is static. [A value of zero indicates that no rendezvous are possible.]

Max_Protected_Entries Specifies the maximum number of entries per protected type. The bounds of every entry family of a protected unit shall be static, or shall be defined by a discriminant of a subtype whose corresponding bound is static. 


#### Dynamic Semantics

{8652/0076} {AI95-00067-01} {AI95-00305-01} The following restriction_[identifier](./AA-2.3#S0002) is language defined:

{AI95-00305-01} {AI95-00394-01} No_Task_Termination All tasks are nonterminating. It is implementation-defined what happens if a task attempts to terminate. If there is a fall-back handler (see C.7.3) set for the partition it should be called when the first task attempts to terminate. 

Implementation defined: When restriction No_Task_Termination applies to a partition, what happens when a task terminates.

The following restriction_parameter_[identifier](./AA-2.3#S0002)s are language defined: 

{8652/0076} {AI95-00067-01} Max_Storage_At_Blocking Specifies the maximum portion [(in storage elements)] of a task's Storage_Size that can be retained by a blocked task. If an implementation chooses to detect a violation of this restriction, Storage_Error should be raised; otherwise, the behavior is implementation defined. 

Implementation defined: The behavior when restriction Max_Storage_At_Blocking is violated.

{8652/0076} {AI95-00067-01} Max_Asynchronous_Select_Nesting Specifies the maximum dynamic nesting level of [asynchronous_select](./AA-9.7#S0280)s. A value of zero prevents the use of any [asynchronous_select](./AA-9.7#S0280) and, if a program contains an [asynchronous_select](./AA-9.7#S0280), it is illegal. If an implementation chooses to detect a violation of this restriction for values other than zero, Storage_Error should be raised; otherwise, the behavior is implementation defined. 

Implementation defined: The behavior when restriction Max_Asynchronous_Select_Nesting is violated.

{8652/0076} {AI95-00067-01} Max_Tasks Specifies the maximum number of task creations that may be executed over the lifetime of a partition, not counting the creation of the environment task. A value of zero prevents any task creation and, if a program contains a task creation, it is illegal. If an implementation chooses to detect a violation of this restriction, Storage_Error should be raised; otherwise, the behavior is implementation defined. 

Ramification: Note that this is not a limit on the number of tasks active at a given time; it is a limit on the total number of task creations that occur. 

Implementation Note: We envision an implementation approach that places TCBs or pointers to them in a fixed-size table, and never reuses table elements. 

Implementation defined: The behavior when restriction Max_Tasks is violated.

{AI95-00305-01} Max_Entry_Queue_Length Max_Entry_Queue_Length defines the maximum number of calls that are queued on an entry. Violation of this restriction results in the raising of Program_Error at the point of the call or requeue.

{AI05-0189-1} No_Standard_Allocators_After_Elaboration Specifies that an [allocator](./AA-4.8#S0164) using a standard storage pool (see 13.11) shall not occur within a parameterless library subprogram, nor within the [handled_sequence_of_statements](./AA-11.2#S0304) of a task body. For the purposes of this rule, an [allocator](./AA-4.8#S0164) of a type derived from a formal access type does not use a standard storage pool.

{AI05-0189-1} {AI05-0262-1} At run time, Storage_Error is raised if an [allocator](./AA-4.8#S0164) using a standard storage pool is evaluated after the elaboration of the [library_item](./AA-10.1#S0287)s of the partition has completed. 

It is implementation defined whether the use of pragma Restrictions results in a reduction in executable program size, storage requirements, or execution time. If possible, the implementation should provide quantitative descriptions of such effects for each restriction. 

Implementation defined: Whether the use of pragma Restrictions results in a reduction in program code or data size or execution time.


#### Implementation Advice

When feasible, the implementation should take advantage of the specified restrictions to produce a more efficient implementation.

Implementation Advice: When feasible, specified restrictions should be used to produce a more efficient implementation.

NOTE 1   The above Storage_Checks can be suppressed with pragma Suppress. 


#### Incompatibilities With Ada 95

{AI95-00360-01} Amendment Correction: The No_Nested_Finalization is now defined in terms of types that need finalization. These types include a variety of language-defined types that might be implemented with a controlled type. If the restriction No_Nested_Finalization (see D.7) applies to the partition, and one of these language-defined types does not have a controlled part, it will not be allowed in local objects in Ada 2005 whereas it would be allowed in original Ada 95. Such code is not portable, as other Ada compilers may have had a controlled part, and thus would be illegal under the restriction. 


#### Extensions to Ada 95

{AI95-00297-01} {AI95-00305-01} {AI95-00394-01} Restrictions No_Dynamic_Attachment, No_Local_Protected_Objects, No_Protected_Type_Allocators, No_Local_Timing_Events, No_Relative_Delay, No_Requeue_Statement, No_Select_Statements, No_Specific_Termination_Handlers, No_Task_Termination, Max_Entry_Queue_Length, and Simple_Barriers are newly added to Ada. 


#### Wording Changes from Ada 95

{8652/0042} {AI95-00130-01} Corrigendum: Clarified that No_Nested_Finalization covered task and protected parts as well.

{8652/0076} {AI95-00067-01} Corrigendum: Changed the description of Max_Tasks and Max_Asynchronous_Select_Nested to eliminate conflicts with the High Integrity Annex (see H.4).

{AI95-00327-01} Added using of the new Priority attribute to the restriction No_Dynamic_Priorities.

{AI95-00394-01} Restriction No_Asynchronous_Control is now obsolescent. 


#### Incompatibilities With Ada 2005

{AI05-0013-1} Correction: Changed so that coextensions of types that require nested finalization are also prohibited; this is done by prohibiting [allocator](./AA-4.8#S0164)s rather than objects of specific access types. It seems unlikely that any program depending on this restriction would violate it in this blatant manner, so it is expected that very few programs will be affected by this change.

{AI05-0211-1} Correction: The restriction No_Relative_Delay was changed to include the Timing_Events routine that uses a relative delay. This means that a program that uses that routine and this restriction will now be rejected. However, such a program violates the spirit and intent of the restriction and as such the program should never have been allowed. Moreover, it is unlikely that any program depending on this restriction would violate it in such an obvious manner, so it is expected that very few programs will be affected by this change.

{AI05-0211-1} Correction: A number of restrictions were changed from "no calls" on some subprogram to "no use of a [name](./AA-4.1#S0091) that denotes" that subprogram. This closes a hole where renames, uses as the prefix of 'Access, and the like, would not be rejected by the restriction, possibly allowing backdoor access to the prohibited subprogram. A program that uses one of these restrictions and using such backdoor access will now be rejected; however, it is extremely unlikely that any program that relies on these restrictions would also use an end-run around the restriction, so it is expected that very few programs will be affected by this change. 


#### Extensions to Ada 2005

{AI05-0189-1} Restriction No_Standard_Allocators_After_Elaboration is newly added to Ada. 


#### Wording Changes from Ada 2005

{AI05-0013-1} {AI05-0216-1} Correction: Improved the wording of various restrictions to make it clearer that they prohibit things that would otherwise be legal, and to word them as definitions, not Legality Rules;.

{AI05-0192-1} Correction: Added wording to explain how No_Task_Allocators and No_Protected_Type_Allocators are checked for class-wide types. This might be an extension if the compiler assumed the worst in the past (it is now a runtime check). 


#### Extensions to Ada 2012

{AI12-0055-1} Corrigendum: Restriction No_Dynamic_CPU_Assignment is newly added to Ada, for use as part of the Ravenscar profile (see D.13).

{AI12-0117-1} Corrigendum: Restriction No_Tasks_Unassigned_To_CPU is newly added to Ada; it ensures that no task is running on an implementation-defined CPU so that task scheduling can be analyzed.

{AI12-0290-1} {AI12-0369-1} Restriction Pure_Barriers is newly added to Ada; it allows the Jorvik profile to use more expressive barriers than the Ravenscar profile without having to worry about exceptions or side-effects.

{AI12-0369-1} Restriction Simple_Barriers (which is part of the Ravenscar profile) is extended to allow statically named record and array subcomponents; this allows common static analysis patterns to be used with Ravenscar. 

