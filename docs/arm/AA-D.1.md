---
sidebar_position:  152
---

# D.1  Task Priorities

{AI05-0299-1} [This subclause specifies the priority model for real-time systems. In addition, the methods for specifying priorities are defined.] 

Paragraphs 2 through 6 were moved to Annex J, "Obsolescent Features". 


#### Static Semantics

{AI05-0229-1} For a task type (including the anonymous type of a [single_task_declaration](./AA-9.1#S0245)), protected type (including the anonymous type of a [single_protected_declaration](./AA-9.4#S0250)), or subprogram, the following language-defined representation aspects may be specified:

PriorityThe aspect Priority is an [expression](./AA-4.4#S0132), which shall be of type Integer.

Aspect Description for Priority: Priority of a task object or type, or priority of a protected object or type; the priority is not in the interrupt range.

Interrupt_PriorityThe aspect Interrupt_Priority is an [expression](./AA-4.4#S0132), which shall be of type Integer.

Aspect Description for Interrupt_Priority: Priority of a task object or type, or priority of a protected object or type; the priority is in the interrupt range.


#### Legality Rules

This paragraph was deleted.{AI05-0229-1} 

{AI05-0229-1} If the Priority aspect is specified for a subprogram, the [expression](./AA-4.4#S0132) shall be static, and its value shall be in the range of System.Priority. 

Reason: This value is needed before it gets elaborated, when the environment task starts executing. 

{AI05-0229-1} At most one of the Priority and Interrupt_Priority aspects may be specified for a given entity.

Ramification: This includes specifying via pragmas (see J.15.11). Note that 13.1 prevents multiple specifications of a single representation aspect by any means. 

{AI05-0229-1} Neither of the Priority or Interrupt_Priority aspects shall be specified for a synchronized interface type.


#### Static Semantics

The following declarations exist in package System: 

```ada
subtype Any_Priority is Integer range implementation-defined;
subtype Priority is Any_Priority
   range Any_Priority'First .. implementation-defined;
subtype Interrupt_Priority is Any_Priority
   range Priority'Last+1 .. Any_Priority'Last;

```

```ada
Default_Priority : constant Priority := (Priority'First + Priority'Last)/2;

```

Implementation defined: The declarations of Any_Priority and Priority.

The full range of priority values supported by an implementation is specified by the subtype Any_Priority. The subrange of priority values that are high enough to require the blocking of one or more interrupts is specified by the subtype Interrupt_Priority. [The subrange of priority values below System.Interrupt_Priority'First is specified by the subtype System.Priority.]

This paragraph was deleted.{AI05-0229-1} 


#### Dynamic Semantics

{AI05-0229-1} The Priority aspect has no effect if it is specified for a subprogram other than the main subprogram; the Priority value is not associated with any task.

{AI12-0404-1} A task priority is an integer value that indicates a degree of urgency and is the basis for resolving competing demands of tasks for resources. Unless otherwise specified, whenever tasks compete for processors or other implementation-defined resources, the resources are allocated to the task with the highest priority value. The base priority of a task is the priority with which it was created, or to which it was later set by Dynamic_Priorities.Set_Priority (see D.5). At all times, a task also has an active priority, which generally is its base priority unless it inherits a priority from other sources. Priority inheritance is the process by which the priority of a task or other entity (for example, a protected object; see D.3) is used in the evaluation of another task's active priority. 

Implementation defined: Implementation-defined execution resources.

{AI05-0229-1} The effect of specifying a Priority or Interrupt_Priority aspect for a protected type or [single_protected_declaration](./AA-9.4#S0250) is discussed in D.3.

{AI05-0229-1} {AI12-0081-1} The [expression](./AA-4.4#S0132) specified for the Priority or Interrupt_Priority aspect of a task type is evaluated each time an object of the task type is created (see 9.1). For the Priority aspect, the value of the [expression](./AA-4.4#S0132) is converted to the subtype Priority; for the Interrupt_Priority aspect, this value is converted to the subtype Any_Priority. The priority value is then associated with the task object. 

{AI05-0229-1} Likewise, the priority value is associated with the environment task if the aspect is specified for the main subprogram.

{AI05-0229-1} The initial value of a task's base priority is specified by default or by means of a Priority or Interrupt_Priority aspect. [After a task is created, its base priority can be changed only by a call to Dynamic_Priorities.Set_Priority (see D.5).] The initial base priority of a task in the absence of an aspect is the base priority of the task that creates it at the time of creation (see 9.1). If the aspect Priority is not specified for the main subprogram, the initial base priority of the environment task is System.Default_Priority. [The task's active priority is used when the task competes for processors. Similarly, the task's active priority is used to determine the task's position in any queue when Priority_Queuing is specified (see D.4).]

{AI95-00357-01} At any time, the active priority of a task is the maximum of all the priorities the task is inheriting at that instant. For a task that is not held (see D.11), its base priority is a source of priority inheritance unless otherwise specified for a particular task dispatching policy. Other sources of priority inheritance are specified under the following conditions: 

Discussion: Other parts of the annex, e.g. D.11, define other sources of priority inheritance. 

{8652/0072} {AI95-00092-01} During activation, a task being activated inherits the active priority that its activator (see 9.2) had at the time the activation was initiated.

{8652/0072} {AI95-00092-01} During rendezvous, the task accepting the entry call inherits the priority of the entry call (see 9.5.3 and D.4).

{AI12-0276-1} While starting a protected action on a protected object when the FIFO_Spinning admission policy is in effect, a task inherits the ceiling priority of the protected object (see 9.5, D.3, and D.4.1).

Reason: {AI12-0005-1} {AI12-0276-1} Priority inheritance is needed for FIFO_Spinning to ensure that lower priority tasks that initiate spin waiting earlier than other higher priority tasks continue to spin to ensure that they can be granted the resource when it becomes available in order to support FIFO ordering. Note that this rule only matters when tasks that can initiate a protected action on an object P can be on a different processor than P. In particular, this rule does not matter on a monoprocessor. 

{AI12-0404-1} While a task executes a protected action on a protected object, the task inherits the ceiling priority of the protected object (see 9.5 and D.3).

In all of these cases, the priority ceases to be inherited as soon as the condition calling for the inheritance no longer exists.


#### Implementation Requirements

The range of System.Interrupt_Priority shall include at least one value.

The range of System.Priority shall include at least 30 values.

NOTE 1   The priority expression can include references to discriminants of the enclosing type.

NOTE 2   It is a consequence of the active priority rules that at the point when a task stops inheriting a priority from another source, its active priority is re-evaluated. This is in addition to other instances described in this Annex for such re-evaluation.

NOTE 3   {AI05-0248-1} {AI12-0440-1} An implementation can provide a nonstandard mode in which tasks inherit priorities under conditions other than those specified above. 

Ramification: {AI05-0229-1} The use of a Priority or Interrupt_Priority aspect does not require the package System to be named in a [with_clause](./AA-10.1#S0294) for the enclosing [compilation_unit](./AA-10.1#S0286). 


#### Extensions to Ada 83

The priority of a task is per-object and not per-type.

Priorities need not be static anymore (except for the main subprogram).


#### Wording Changes from Ada 83

The description of the Priority pragma has been moved to this annex.


#### Wording Changes from Ada 95

{8652/0072} {AI95-00092-01} Corrigendum: Clarified that dynamic priority changes are not transitive - that is, they don't apply to tasks that are being activated by or in rendezvous with the task that had its priority changed.

{AI95-00357-01} Generalized the definition of priority inheritance to take into account the differences between the existing and new dispatching policies. 


#### Extensions to Ada 2005

{AI05-0229-1} Aspects Priority and Interrupt_Priority are new; [pragma](./AA-2.8#S0019)s Priority and Interrupt_Priority are now obsolescent. 


#### Wording Changes from Ada 2012

{AI12-0081-1} Corrigendum: Clarified when the Priority and Interrupt_Priority aspect expressions are evaluated.

{AI12-0276-1} Added an additional case of priority inheritance when the new admission policy FIFO_Spinning is in effect. 

