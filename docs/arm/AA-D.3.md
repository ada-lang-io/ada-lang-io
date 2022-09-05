---
sidebar_position:  154
---

# D.3  Priority Ceiling Locking

{AI05-0299-1} [This subclause specifies the interactions between priority task scheduling and protected object ceilings. This interaction is based on the concept of the ceiling priority of a protected object.] 


#### Syntax

The form of a [pragma](./AA-2.8#S0019) Locking_Policy is as follows: 

  pragma Locking_Policy(policy_[identifier](./AA-2.3#S0002)); 


#### Legality Rules

The policy_[identifier](./AA-2.3#S0002) shall either be Ceiling_Locking or an implementation-defined [identifier](./AA-2.3#S0002). 

Implementation defined: Implementation-defined policy_[identifier](./AA-2.3#S0002)s allowed in a [pragma](./AA-2.8#S0019) Locking_Policy.


#### Post-Compilation Rules

A Locking_Policy pragma is a configuration pragma.


#### Dynamic Semantics

{8652/0073} {AI95-00091-01} {AI95-00327-01} [A locking policy specifies the details of protected object locking. All protected objects have a priority. The locking policy specifies the meaning of the priority of a protected object, and the relationships between these priorities and task priorities. In addition, the policy specifies the state of a task when it executes a protected action, and how its active priority is affected by the locking.] The locking policy is specified by a Locking_Policy pragma. For implementation-defined locking policies, the meaning of the priority of a protected object is implementation defined. If no Locking_Policy pragma applies to any of the program units comprising a partition, the locking policy for that partition, as well as the meaning of the priority of a protected object, are implementation defined. 

Implementation defined: The locking policy if no Locking_Policy pragma applies to any unit of a partition.

{AI95-00327-01} {AI05-0229-1} The [expression](./AA-4.4#S0132) specified for the Priority or Interrupt_Priority aspect (see D.1) is evaluated as part of the creation of the corresponding protected object and converted to the subtype System.Any_Priority or System.Interrupt_Priority, respectively. The value of the expression is the initial priority of the corresponding protected object. If no Priority or Interrupt_Priority aspect is specified for a protected object, the initial priority is specified by the locking policy. 

There is one predefined locking policy, Ceiling_Locking; this policy is defined as follows: 

{AI95-00327-01} {AI05-0229-1} Every protected object has a ceiling priority, which is determined by either a Priority or Interrupt_Priority aspect as defined in D.1, or by assignment to the Priority attribute as described in D.5.2. The ceiling priority of a protected object (or ceiling, for short) is an upper bound on the active priority a task can have when it calls protected operations of that protected object.

{AI95-00327-01} The initial ceiling priority of a protected object is equal to the initial priority for that object.

{AI95-00327-01} {AI05-0229-1} {AI12-0051-1} If an Interrupt_Handler or Attach_Handler aspect (see C.3.1) is specified for a protected subprogram of a protected type that does not have either the Priority or Interrupt_Priority aspect specified, the initial priority of protected objects of that type is implementation defined, but in the range of the subtype System.Interrupt_Priority. 

Implementation defined: Default ceiling priorities.

{AI95-00327-01} {AI05-0229-1} If neither aspect Priority nor Interrupt_Priority is specified for a protected type, and no protected subprogram of the type has aspect Interrupt_Handler or Attach_Handler specified, then the initial priority of the corresponding protected object is System.Priority'Last.

While a task executes a protected action, it inherits the ceiling priority of the corresponding protected object.

When a task calls a protected operation, a check is made that its active priority is not higher than the ceiling of the corresponding protected object; Program_Error is raised if this check fails.

{AI12-0230-1} {AI12-0404-1} If the task dispatching policy specified for the ceiling priority of a protected object is EDF_Within_Priorities, the following additional rules apply:

Every protected object has a relative deadline, which is determined by a Relative_Deadline aspect as defined in D.2.6, or by assignment to the Relative_Deadline attribute as described in D.5.2. The relative deadline of a protected object represents a lower bound on the relative deadline a task may have when it calls a protected operation of that protected object.

If aspect Relative_Deadline is not specified for a protected type then the initial relative deadline of the corresponding protected object is Ada.Real_Time.Time_Span_Zero.

While a task executes a protected action on a protected object P, it inherits the relative deadline of P. In this case, let DF be 'now' ('now' is obtained via a call on Ada.Real_Time.Clock at the start of the action) plus the deadline floor of P. If the active deadline of the task is later than DF, its active deadline is reduced to DF[; the active deadline is unchanged otherwise].

When a task calls a protected operation, a check is made that its active deadline minus its last release time is not less than the relative deadline of the corresponding protected object; Program_Error is raised if this check fails. 


#### Bounded (Run-Time) Errors

{AI95-00327-01} {AI12-0230-1} Following any change of priority, it is a bounded error for the active priority of any task with a call queued on an entry of a protected object to be higher than the ceiling priority of the protected object. In this case one of the following applies:

{AI12-0404-1} at any time prior to executing the entry body, Program_Error is raised in the calling task;

{AI12-0404-1} when the entry is open,  the entry body is executed at the ceiling priority of the protected object;

{AI12-0404-1} when the entry is open,  the entry body is executed at the ceiling priority of the protected object and then Program_Error is raised in the calling task; or

{AI12-0404-1} when the entry is open, the entry body is executed at the ceiling priority of the protected object that was in effect when the entry call was queued. 

Ramification: Note that the error is "blamed" on the task that did the entry call, not the task that changed the priority of the task or protected object. This seems to make sense for the case of changing the priority of a task blocked on a call, since if the Set_Priority had happened a little bit sooner, before the task queued a call, the entry-calling task would get the error. Similarly, there is no reason not to raise the priority of a task that is executing in an [abortable_part](./AA-9.7#S0283), so long as its priority is lowered before it gets to the end and needs to cancel the call. The priority might need to be lowered to allow it to remove the call from the entry queue, in order to avoid violating the ceiling. This seems relatively harmless, since there is an error, and the task is about to start raising an exception anyway. 


#### Implementation Permissions

The implementation is allowed to round all ceilings in a certain subrange of System.Priority or System.Interrupt_Priority up to the top of that subrange, uniformly. 

Discussion: For example, an implementation might use Priority'Last for all ceilings in Priority, and Interrupt_Priority'Last for all ceilings in Interrupt_Priority. This would be equivalent to having two ceiling priorities for protected objects, "nonpreemptible" and "noninterruptible", and is an allowed behavior.

Note that the implementation cannot choose a subrange that crosses the boundary between normal and interrupt priorities. 

{AI95-00256-01} {AI12-0444-1} Implementations are allowed to define other locking policies, but are not required to support specifying more than one locking policy per partition.

[Since implementations are allowed to place restrictions on code that runs at an interrupt-level active priority (see C.3.1 and D.2.1), the implementation may implement a language feature in terms of a protected object with an implementation-defined ceiling, but the ceiling shall be no less than Priority'Last.] 

Implementation defined: The ceiling of any protected object used internally by the implementation.

Proof: This permission follows from the fact that the implementation can place restrictions on interrupt handlers and on any other code that runs at an interrupt-level active priority.

The implementation might protect a storage pool with a protected object whose ceiling is Priority'Last, which would cause [allocator](./AA-4.8#S0164)s to fail when evaluated at interrupt priority. Note that the ceiling of such an object has to be at least Priority'Last, since there is no permission for [allocator](./AA-4.8#S0164)s to fail when evaluated at a noninterrupt priority. 


#### Implementation Advice

The implementation should use names that end with "_Locking" for implementation-defined locking policies.

Implementation Advice: Names that end with "_Locking" should be used for implementation-defined locking policies.

NOTE 1   While a task executes in a protected action, it can be preempted only by tasks whose active priorities are higher than the ceiling priority of the protected object.

NOTE 2   If a protected object has a ceiling priority in the range of Interrupt_Priority, certain interrupts are blocked while protected actions of that object execute. In the extreme, if the ceiling is Interrupt_Priority'Last, all blockable interrupts are blocked during that time.

NOTE 3   The ceiling priority of a protected object has to be in the Interrupt_Priority range if one of its procedures is to be used as an interrupt handler (see C.3).

NOTE 4   {AI12-0442-1} When specifying the ceiling of a protected object, a correct value is one that is at least as high as the highest active priority at which tasks can be executing when they call protected operations of that object. In determining this value the following factors, which can affect active priority, are relevant: the effect of Set_Priority, nested protected operations, entry calls, task activation, and other implementation-defined factors.

NOTE 5   Attaching a protected procedure whose ceiling is below the interrupt hardware priority to an interrupt causes the execution of the program to be erroneous (see C.3.1).

NOTE 6   On a single processor implementation, the ceiling priority rules guarantee that there is no possibility of deadlock involving only protected subprograms (excluding the case where a protected operation calls another protected operation on the same protected object).


#### Extensions to Ada 95

{AI95-00327-01} All protected objects now have a priority, which is the value of the Priority attribute of D.5.2. How this value is interpreted depends on the locking policy; for instance, the ceiling priority is derived from this value when the locking policy is Ceiling_Locking. 


#### Wording Changes from Ada 95

{8652/0073} {AI95-00091-01} Corrigendum: Corrected the wording to reflect that pragma Locking_Policy cannot be inside of a program unit.

{AI95-00256-01} Clarified that an implementation need support only one locking policy (of any kind, language-defined or otherwise) per partition.

{AI95-00327-01} The bounded error for the priority of a task being higher than the ceiling of an object it is currently in was moved here from D.5, so that it applies no matter how the situation arises. 


#### Wording Changes from Ada 2005

{AI05-0229-1} Revised to use aspects Priority and Interrupt_Priority as [pragma](./AA-2.8#S0019)s Priority and Interrupt_Priority are now obsolescent. 


#### Extensions to Ada 2012

{AI12-0230-1} All protected objects now have a relative deadline, which is the value of the Relative_Deadline attribute of D.5.2. How this value is interpreted depends on the locking policy. 


#### Wording Changes from Ada 2012

{AI12-0051-1} Corrigendum: Clarified that the Priority aspect can be used to set the initial ceiling priority of a protected object that contains an interrupt handler. 

