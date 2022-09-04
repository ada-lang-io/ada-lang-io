---
sidebar_position:  18
---

# Annex D Real-Time Systems

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
This Annex specifies additional characteristics of Ada implementations intended for real-time systems software. To conform to this Annex, an implementation shall also conform to the Systems Programming Annex. 


#### Metrics

The metrics are documentation requirements; an implementation shall document the values of the language-defined metrics for at least one configuration [of hardware or an underlying system] supported by the implementation, and shall document the details of that configuration. 

This paragraph was deleted.Implementation defined: Values of all Metrics.

Reason: The actual values of the metrics are likely to depend on hardware configuration details that are variable and generally outside the control of a compiler vendor. 

The metrics do not necessarily yield a simple number. [For some, a range is more suitable, for others a formula dependent on some parameter is appropriate, and for others, it may be more suitable to break the metric into several cases.] Unless specified otherwise, the metrics in this annex are expressed in processor clock cycles. For metrics that require documentation of an upper bound, if there is no upper bound, the implementation shall report that the metric is unbounded. 

Discussion: There are several good reasons to specify metrics in seconds; there are however equally good reasons to specify them in processor clock cycles. In defining the metrics, we have tried to strike a balance on a case-by-case basis.

It has been suggested that all metrics should be given names, so that "data-sheets" could be formulated and published by vendors. However the paragraph number can serve that purpose. 

NOTE 1   The specification of the metrics makes a distinction between upper bounds and simple execution times. Where something is just specified as "the execution time of" a piece of code, this leaves one the freedom to choose a nonpathological case. This kind of metric is of the form "there exists a program such that the value of the metric is V". Conversely, the meaning of upper bounds is "there is no program such that the value of the metric is greater than V". This kind of metric can only be partially tested, by finding the value of V for one or more test programs.

NOTE 2   The metrics do not cover the whole language; they are limited to features that are specified in Annex C, "Systems Programming" and in this Annex. The metrics are intended to provide guidance to potential users as to whether a particular implementation of such a feature is going to be adequate for a particular real-time application. As such, the metrics are aimed at known implementation choices that can result in significant performance differences.

NOTE 3   The purpose of the metrics is not necessarily to provide fine-grained quantitative results or to serve as a comparison between different implementations on the same or different platforms. Instead, their goal is rather qualitative; to define a standard set of approximate values that can be measured and used to estimate the general suitability of an implementation, or to evaluate the comparative utility of certain features of an implementation for a particular real-time application.


#### Extensions to Ada 83

This Annex is new to Ada 95. 


## D.1  Task Priorities

[This clause specifies the priority model for real-time systems. In addition, the methods for specifying priorities are defined.] 


#### Syntax

The form of a pragma Priority is as follows: 

  pragma Priority(expression);

The form of a pragma Interrupt_Priority is as follows: 

  pragma Interrupt_Priority[(expression);] 


#### Name Resolution Rules

The expected type for the expression in a Priority or Interrupt_Priority pragma is Integer. 


#### Legality Rules

A Priority pragma is allowed only immediately within a task_definition, a protected_definition, or the declarative_part of a subprogram_body. An Interrupt_Priority pragma is allowed only immediately within a task_definition or a protected_definition. At most one such pragma shall appear within a given construct.

For a Priority pragma that appears in the declarative_part of a subprogram_body, the expression shall be static, and its value shall be in the range of System.Priority. 

Reason: This value is needed before it gets elaborated, when the environment task starts executing. 


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

The priority specified by a Priority or Interrupt_Priority pragma is the value of the expression in the pragma, if any. If there is no expression in an Interrupt_Priority pragma, the priority value is Interrupt_Priority'Last.


#### Dynamic Semantics

A Priority pragma has no effect if it it occurs in the declarative_part of the subprogram_body of a subprogram other than the main subprogram.

A task priority is an integer value that indicates a degree of urgency and is the basis for resolving competing demands of tasks for resources. Unless otherwise specified, whenever tasks compete for processors or other implementation-defined resources, the resources are allocated to the task with the highest priority value. The base priority of a task is the priority with which it was created, or to which it was later set by Dynamic_Priorities.Set_Priority (see D.5). At all times, a task also has an active priority, which generally reflects its base priority as well as any priority it inherits from other sources. Priority inheritance is the process by which the priority of a task or other entity (e.g. a protected object; see D.3) is used in the evaluation of another task's active priority. 

Implementation defined: Implementation-defined execution resources.

The effect of specifying such a pragma in a protected_definition is discussed in D.3.

The expression in a Priority or Interrupt_Priority pragma that appears in a task_definition is evaluated for each task object (see 9.1). For a Priority pragma, the value of the expression is converted to the subtype Priority; for an Interrupt_Priority pragma, this value is converted to the subtype Any_Priority. The priority value is then associated with the task objectwhose task_definition contains the pragma. 

Likewise, the priority value is associated with the environment task if the pragma appears in the declarative_part of the main subprogram.

The initial value of a task's base priority is specified by default or by means of a Priority or Interrupt_Priority pragma. [After a task is created, its base priority can be changed only by a call to Dynamic_Priorities.Set_Priority (see D.5).] The initial base priority of a task in the absence of a pragma is the base priority of the task that creates it at the time of creation (see 9.1). If a pragma Priority does not apply to the main subprogram, the initial base priority of the environment task is System.Default_Priority. [The task's active priority is used when the task competes for processors. Similarly, the task's active priority is used to determine the task's position in any queue when Priority_Queuing is specified (see D.4).]

At any time, the active priority of a task is the maximum of all the priorities the task is inheriting at that instant. For a task that is not held (see D.11), its base priority is always a source of priority inheritance . Other sources of priority inheritance are specified under the following conditions: 

Discussion: Other parts of the annex, e.g. D.11, define other sources of priority inheritance. 

During activation, a task being activated inherits the active priority of the its activator (see 9.2).

During rendezvous, the task accepting the entry call inherits the active priority of the caller (see 9.5.3).

During a protected action on a protected object, a task inherits the ceiling priority of the protected object (see 9.5 and D.3).

In all of these cases, the priority ceases to be inherited as soon as the condition calling for the inheritance no longer exists.


#### Implementation Requirements

The range of System.Interrupt_Priority shall include at least one value.

The range of System.Priority shall include at least 30 values.

NOTE 1   The priority expression can include references to discriminants of the enclosing type.

NOTE 2   It is a consequence of the active priority rules that at the point when a task stops inheriting a priority from another source, its active priority is re-evaluated. This is in addition to other instances described in this Annex for such re-evaluation.

NOTE 3   An implementation may provide a non-standard mode in which tasks inherit priorities under conditions other than those specified above. 

Ramification: The use of a Priority or Interrupt_Priority pragma does not require the package System to be named in a with_clause for the enclosing compilation_unit. 


#### Extensions to Ada 83

The priority of a task is per-object and not per-type.

Priorities need not be static anymore (except for the main subprogram).


#### Wording Changes from Ada 83

The description of the Priority pragma has been moved to this annex.


## D.2  Priority Scheduling

[This clause describes the rules that determine which task is selected for execution when more than one task is ready (see 9.2). The rules have two parts: the task dispatching model (see D.2.1), and a specific task dispatching policy (see D.2.2).] 


### D.2.1  The Task Dispatching Model

[The task dispatching model specifies preemptive scheduling, based on conceptual priority-ordered ready queues.] 


#### Dynamic Semantics

A task runs (that is, it becomes a running task) only when it is ready (see 9.2) and the execution resources required by that task are available. Processors are allocated to tasks based on each task's active priority.

It is implementation defined whether, on a multiprocessor, a task that is waiting for access to a protected object keeps its processor busy. 

Implementation defined: Whether, on a multiprocessor, a task that is waiting for access to a protected object keeps its processor busy.

Task dispatching is the process by which one ready task is selected for execution on a processor. This selection is done at certain points during the execution of a task called task dispatching points. A task reaches a task dispatching point whenever it becomes blocked, and whenever it becomes ready. In addition, the completion of an accept_statement (see 9.5.2), and task termination are task dispatching points for the executing task. [Other task dispatching points are defined throughout this Annex.] 

Ramification: On multiprocessor systems, more than one task can be chosen, at the same time, for execution on more than one processor, as explained below. 

Task dispatching policies are specified in terms of conceptual ready queues, task states, and task preemption. A ready queue is an ordered list of ready tasks. The first position in a queue is called the head of the queue, and the last position is called the tail of the queue. A task is ready if it is in a ready queue, or if it is running. Each processor has one ready queue for each priority value. At any instant, each ready queue of a processor contains exactly the set of tasks of that priority that are ready for execution on that processor, but are not running on any processor; that is, those tasks that are ready, are not running on any processor, and can be executed using that processor and other available resources. A task can be on the ready queues of more than one processor. 

Discussion: The core language defines a ready task as one that is not blocked. Here we refine this definition and talk about ready queues. 

Each processor also has one running task, which is the task currently being executed by that processor. Whenever a task running on a processor reaches a task dispatching point, one task is selected to run on that processor. The task selected is the one at the head of the highest priority nonempty ready queue; this task is then removed from all ready queues to which it belongs. 

Discussion: There is always at least one task to run, if we count the idle task. 

A preemptible resource is a resource that while allocated to one task can be allocated (temporarily) to another instead. Processors are preemptible resources. Access to a protected object (see 9.5.1) is a nonpreemptible resource. When a higher-priority task is dispatched to the processor, and the previously running task is placed on the appropriate ready queue, the latter task is said to be preempted. 

Reason: A processor that is executing a task is available to execute tasks of higher priority, within the set of tasks that that processor is able to execute. Write access to a protected object, on the other hand, cannot be granted to a new task before the old task has released it. 

A new running task is also selected whenever there is a nonempty ready queue with a higher priority than the priority of the running task, or when the task dispatching policy requires a running task to go back to a ready queue. [These are also task dispatching points.] 

Ramification: Thus, when a task becomes ready, this is a task dispatching point for all running tasks of lower priority. 


#### Implementation Permissions

An implementation is allowed to define additional resources as execution resources, and to define the corresponding allocation policies for them. Such resources may have an implementation defined effect on task dispatching (see D.2.2). 

Implementation defined: The affect of implementation defined execution resources on task dispatching.

An implementation may place implementation-defined restrictions on tasks whose active priority is in the Interrupt_Priority range. 

Ramification: For example, on some operating systems, it might be necessary to disallow them altogether. This permission applies to tasks whose priority is set to interrupt level for any reason: via a pragma, via a call to Dynamic_Priorities.Set_Priority, or via priority inheritance. 

NOTE 1   Section 9 specifies under which circumstances a task becomes ready. The ready state is affected by the rules for task activation and termination, delay statements, and entry calls. When a task is not ready, it is said to be blocked.

NOTE 2   An example of a possible implementation-defined execution resource is a page of physical memory, which needs to be loaded with a particular page of virtual memory before a task can continue execution.

NOTE 3   The ready queues are purely conceptual; there is no requirement that such lists physically exist in an implementation.

NOTE 4   While a task is running, it is not on any ready queue. Any time the task that is running on a processor is added to a ready queue, a new running task is selected for that processor.

NOTE 5   In a multiprocessor system, a task can be on the ready queues of more than one processor. At the extreme, if several processors share the same set of ready tasks, the contents of their ready queues is identical, and so they can be viewed as sharing one ready queue, and can be implemented that way. [Thus, the dispatching model covers multiprocessors where dispatching is implemented using a single ready queue, as well as those with separate dispatching domains.]

NOTE 6   The priority of a task is determined by rules specified in this subclause, and under D.1, "Task Priorities", D.3, "Priority Ceiling Locking", and D.5, "Dynamic Priorities".


### D.2.2  The Standard Task Dispatching Policy


#### Syntax

The form of a pragma Task_Dispatching_Policy is as follows: 

  pragma Task_Dispatching_Policy(policy_identifier);




#### Legality Rules

The policy_identifier shall either be FIFO_Within_Priorities or an implementation-defined identifier. 

This paragraph was deleted.Implementation defined: Implementation-defined policy_identifiers allowed in a pragma Task_Dispatching_Policy.


#### Post-Compilation Rules

A Task_Dispatching_Policy pragma is a configuration pragma.

If the FIFO_Within_Priorities policy is specified for a partition, then the Ceiling_Locking policy (see D.3) shall also be specified for the partition.


#### Dynamic Semantics

[A task dispatching policy specifies the details of task dispatching that are not covered by the basic task dispatching model. These rules govern when tasks are inserted into and deleted from the ready queues, and whether a task is inserted at the head or the tail of the queue for its active priority.] The task dispatching policy is specified by a Task_Dispatching_Policy configuration pragma. If no such pragma appears in any of the program units comprising a partition, the task dispatching policy for that partition is unspecified.

The language defines only one task dispatching policy, FIFO_Within_Priorities; when this policy is in effect, modifications to the ready queues occur only as follows:

When a blocked task becomes ready, it is added at the tail of the ready queue for its active priority.

When the active priority of a ready task that is not running changes, or the setting of its base priority takes effect, the task is removed from the ready queue for its old active priority and is added at the tail of the ready queue for its new active priority, except in the case where the active priority is lowered due to the loss of inherited priority, in which case the task is added at the head of the ready queue for its new active priority.

When the setting of the base priority of a running task takes effect, the task is added to the tail of the ready queue for its active priority.

When a task executes a delay_statement that does not result in blocking, it is added to the tail of the ready queue for its active priority. 

Ramification: If the delay does result in blocking, the task moves to the "delay queue", not to the ready queue. 

Each of the events specified above is a task dispatching point (see D.2.1).

In addition, when a task is preempted, it is added at the head of the ready queue for its active priority.


#### Documentation Requirements

Priority inversion is the duration for which a task remains at the head of the highest priority ready queue while the processor executes a lower priority task. The implementation shall document: 

The maximum priority inversion a user task can experience due to activity of the implementation (on behalf of lower priority tasks), and

whether execution of a task can be preempted by the implementation processing of delay expirations for lower priority tasks, and if so, for how long. 

This paragraph was deleted.Implementation defined: Implementation-defined aspects of priority inversion.


#### Implementation Permissions

Implementations are allowed to define other task dispatching policies, but need not support more than one such policy per partition.

[For optimization purposes,] an implementation may alter the points at which task dispatching occurs, in an implementation defined manner. However, a delay_statement always corresponds to at least one task dispatching point.

Implementation defined: Implementation defined task dispatching.

NOTE 1   If the active priority of a running task is lowered due to loss of inherited priority (as it is on completion of a protected operation) and there is a ready task of the same active priority that is not running, the running task continues to run (provided that there is no higher priority task).

NOTE 2   The setting of a task's base priority as a result of a call to Set_Priority does not always take effect immediately when Set_Priority is called. The effect of setting the task's base priority is deferred while the affected task performs a protected action.

NOTE 3   Setting the base priority of a ready task causes the task to move to the end of the queue for its active priority, regardless of whether the active priority of the task actually changes.


#### Syntax

  


#### Static Semantics




## D.3  Priority Ceiling Locking

[This clause specifies the interactions between priority task scheduling and protected object ceilings. This interaction is based on the concept of the ceiling priority of a protected object.] 


#### Syntax

The form of a pragma Locking_Policy is as follows: 

  pragma Locking_Policy(policy_identifier); 


#### Legality Rules

The policy_identifier shall either be Ceiling_Locking or an implementation-defined identifier. 

Implementation defined: Implementation-defined policy_identifiers allowed in a pragma Locking_Policy.


#### Post-Compilation Rules

A Locking_Policy pragma is a configuration pragma.


#### Dynamic Semantics

[A locking policy specifies the details of protected object locking. These rules specify whether or not protected objects have priorities, and the relationships between these priorities and task priorities. In addition, the policy specifies the state of a task when it executes a protected action, and how its active priority is affected by the locking.] The locking policy is specified by a Locking_Policy pragma. For implementation-defined locking policies, the effect of a Priority or Interrupt_Priority pragma on a protected object is implementation defined. If no Locking_Policy pragma appears in any of the program units comprising a partition, the locking policy for that partition, as well as the effect of specifying either a Priority or Interrupt_Priority pragma for a protected object, are implementation defined. 

There is one predefined locking policy, Ceiling_Locking; this policy is defined as follows: 

Every protected object has a ceiling priority, which is determined by either a Priority or Interrupt_Priority pragma as defined in D.1. The ceiling priority of a protected object (or ceiling, for short) is an upper bound on the active priority a task can have when it calls protected operations of that protected object.

The expression of a Priority or Interrupt_Priority pragma is evaluated as part of the creation of the corresponding protected object and converted to the subtype System.Any_Priority or System.Interrupt_Priority, respectively. The value of the expression is the ceiling priority of the corresponding protected object. 

If an Interrupt_Handler or Attach_Handler pragma (see C.3.1) appears in a protected_definition without an Interrupt_Priority pragma, the ceiling priority of protected objects of that type is implementation defined, but in the range of the subtype System.Interrupt_Priority. 

Implementation defined: Default ceiling priorities.

If no pragma Priority, Interrupt_Priority, Interrupt_Handler, or Attach_Handler is specified in the protected_definition, then the ceiling priority of the corresponding protected object is System.Priority'Last.

While a task executes a protected action, it inherits the ceiling priority of the corresponding protected object.

When a task calls a protected operation, a check is made that its active priority is not higher than the ceiling of the corresponding protected object; Program_Error is raised if this check fails.


#### Implementation Permissions

The implementation is allowed to round all ceilings in a certain subrange of System.Priority or System.Interrupt_Priority up to the top of that subrange, uniformly. 

Discussion: For example, an implementation might use Priority'Last for all ceilings in Priority, and Interrupt_Priority'Last for all ceilings in Interrupt_Priority. This would be equivalent to having two ceiling priorities for protected objects, "nonpreemptible" and "noninterruptible", and is an allowed behavior.

Note that the implementation cannot choose a subrange that crosses the boundary between normal and interrupt priorities. 

Implementations are allowed to define other locking policies, but need not support more than one such policy per partition.

[Since implementations are allowed to place restrictions on code that runs at an interrupt-level active priority (see C.3.1 and D.2.1), the implementation may implement a language feature in terms of a protected object with an implementation-defined ceiling, but the ceiling shall be no less than Priority'Last.] 

Implementation defined: The ceiling of any protected object used internally by the implementation.

Proof: This permission follows from the fact that the implementation can place restrictions on interrupt handlers and on any other code that runs at an interrupt-level active priority.

The implementation might protect a storage pool with a protected object whose ceiling is Priority'Last, which would cause allocators to fail when evaluated at interrupt priority. Note that the ceiling of such an object has to be at least Priority'Last, since there is no permission for allocators to fail when evaluated at a noninterrupt priority. 


#### Implementation Advice

The implementation should use names that end with "_Locking" for implementation-defined locking policies.

NOTE 1   While a task executes in a protected action, it can be preempted only by tasks whose active priorities are higher than the ceiling priority of the protected object.

NOTE 2   If a protected object has a ceiling priority in the range of Interrupt_Priority, certain interrupts are blocked while protected actions of that object execute. In the extreme, if the ceiling is Interrupt_Priority'Last, all blockable interrupts are blocked during that time.

NOTE 3   The ceiling priority of a protected object has to be in the Interrupt_Priority range if one of its procedures is to be used as an interrupt handler (see C.3).

NOTE 4   When specifying the ceiling of a protected object, one should choose a value that is at least as high as the highest active priority at which tasks can be executing when they call protected operations of that object. In determining this value the following factors, which can affect active priority, should be considered: the effect of Set_Priority, nested protected operations, entry calls, task activation, and other implementation-defined factors.

NOTE 5   Attaching a protected procedure whose ceiling is below the interrupt hardware priority to an interrupt causes the execution of the program to be erroneous (see C.3.1).

NOTE 6   On a single processor implementation, the ceiling priority rules guarantee that there is no possibility of deadlock involving only protected subprograms (excluding the case where a protected operation calls another protected operation on the same protected object).


## D.4  Entry Queuing Policies

[ This clause specifies a mechanism for a user to choose an entry queuing policy. It also defines one such policy. Other policies are implementation defined.] 

Implementation defined: Implementation-defined queuing policies.


#### Syntax

The form of a pragma Queuing_Policy is as follows: 

  pragma Queuing_Policy(policy_identifier); 


#### Legality Rules

The policy_identifier shall be either FIFO_Queuing, Priority_Queuing or an implementation-defined identifier.


#### Post-Compilation Rules

A Queuing_Policy pragma is a configuration pragma.


#### Dynamic Semantics

[A queuing policy governs the order in which tasks are queued for entry service, and the order in which different entry queues are considered for service.] The queuing policy is specified by a Queuing_Policy pragma. 

Ramification: The queuing policy includes entry queuing order, the choice among open alternatives of a selective_accept, and the choice among queued entry calls of a protected object when more than one entry_barrier condition is True. 

Two queuing policies, FIFO_Queuing and Priority_Queuing, are language defined. If no Queuing_Policy pragma appears in any of the program units comprising the partition, the queuing policy for that partition is FIFO_Queuing. The rules for this policy are specified in 9.5.3 and 9.7.1.

The Priority_Queuing policy is defined as follows:

The calls to an entry [(including a member of an entry family)] are queued in an order consistent with the priorities of the calls. The priority of an entry call is initialized from the active priority of the calling task at the time the call is made, but can change later. Within the same priority, the order is consistent with the calling (or requeuing, or priority setting) time (that is, a FIFO order).

After a call is first queued, changes to the active priority of a task do not affect the priority of the call, unless the base priority of the task is set.

When the base priority of a task is set (see D.5), if the task is blocked on an entry call, and the call is queued, the priority of the call is updated to the new active priority of the calling task. This causes the call to be removed from and then reinserted in the queue at the new active priority. 

Reason: A task is blocked on an entry call if the entry call is simple, conditional, or timed. If the call came from the triggering_statement of an asynchronous_select, or a requeue thereof, then the task is not blocked on that call; such calls do not have their priority updated. Thus, there can exist many queued calls from a given task (caused by many nested ATC's), but a task can be blocked on only one call at a time.

A previous version of Ada 9X required queue reordering in the asynchronous_select case as well. If the call corresponds to a "synchronous" entry call, then the task is blocked while queued, and it makes good sense to move it up in the queue if its priority is raised.

However, if the entry call is "asynchronous", that is, it is due to an asynchronous_select whose triggering_statement is an entry call, then the task is not waiting for this entry call, so the placement of the entry call on the queue is irrelevant to the rate at which the task proceeds.

Furthermore, when an entry is used for asynchronous_selects, it is almost certain to be a "broadcast" entry or have only one caller at a time. For example, if the entry is used to notify tasks of a mode switch, then all tasks on the entry queue would be signaled when the mode changes. Similarly, if it is indicating some interrupting event such as a control-C, all tasks sensitive to the interrupt will want to be informed that the event occurred. Hence, the order on such a queue is essentially irrelevant.

Given the above, it seems an unnecessary semantic and implementation complexity to specify that asynchronous queued calls are moved in response to dynamic priority changes. Furthermore, it is somewhat inconsistent, since the call was originally queued based on the active priority of the task, but dynamic priority changes are changing the base priority of the task, and only indirectly the active priority. We say explicitly that asynchronous queued calls are not affected by normal changes in active priority during the execution of an abortable_part. Saying that, if a change in the base priority affects the active priority, then we do want the calls reordered, would be inconsistent. It would also require the implementation to maintain a readily accessible list of all queued calls which would not otherwise be necessary.

Several rules were removed or simplified when we changed the rules so that calls due to asynchronous_selects are never moved due to intervening changes in active priority, be they due to protected actions, some other priority inheritance, or changes in the base priority. 

When more than one condition of an entry_barrier of a protected object becomes True, and more than one of the respective queues is nonempty, the call with the highest priority is selected. If more than one such call has the same priority, the call that is queued on the entry whose declaration is first in textual order in the protected_definition is selected. For members of the same entry family, the one with the lower family index is selected.

If the expiration time of two or more open delay_alternatives is the same and no other accept_alternatives are open, the sequence_of_statements of the delay_alternative that is first in textual order in the selective_accept is executed.

When more than one alternative of a selective_accept is open and has queued calls, an alternative whose queue has the highest-priority call at its head is selected. If two or more open alternatives have equal-priority queued calls, then a call on the entry in the accept_alternative that is first in textual order in the selective_accept is selected.


#### Implementation Permissions

Implementations are allowed to define other queuing policies, but need not support more than one such policy per partition. 


#### Implementation Advice

The implementation should use names that end with "_Queuing" for implementation-defined queuing policies.


#### Syntax




## D.5  Dynamic Priorities

[This clause describes how the base priority of a task can be modified or queried at run time.] 


#### Static Semantics

The following language-defined library package exists: 

```ada
with System;
with Ada.Task_Identification; -- See C.7.1
package Ada.Dynamic_Priorities is

```

```ada
    procedure Set_Priority(Priority : in System.Any_Priority;
                           T : in Ada.Task_Identification.Task_Id :=
                           Ada.Task_Identification.Current_Task);

```

```ada
    function Get_Priority (T : Ada.Task_Identification.Task_Id :=
                           Ada.Task_Identification.Current_Task)
                           return System.Any_Priority;

```

```ada
end Ada.Dynamic_Priorities;

```


#### Dynamic Semantics

The procedure Set_Priority sets the base priority of the specified task to the specified Priority value. Set_Priority has no effect if the task is terminated.

The function Get_Priority returns T's current base priority. Tasking_Error is raised if the task is terminated. 

Reason: There is no harm in setting the priority of a terminated task. A previous version of Ada 9X made this a run-time error. However, there is little difference between setting the priority of a terminated task, and setting the priority of a task that is about to terminate very soon; neither case should be an error. Furthermore, the runtime check is not necessarily feasible to implement on all systems, since priority changes might be deferred due to inter-processor communication overhead, so the error might not be detected until after Set_Priority has returned.

However, we wish to allow implementations to avoid storing "extra" information about terminated tasks. Therefore, we make Get_Priority of a terminated task raise an exception; the implementation need not continue to store the priority of a task that has terminated. 

Program_Error is raised by Set_Priority and Get_Priority if T is equal to Null_Task_Id.

Setting the task's base priority to the new value takes place as soon as is practical but not while the task is performing a protected action. This setting occurs no later then the next abort completion point of the task T (see 9.8).

Implementation Note: When Set_Priority is called by a task T1 to set the priority of T2, if T2 is blocked, waiting on an entry call queued on a protected object, the entry queue needs to be reordered. Since T1 might have a priority that is higher than the ceiling of the protected object, T1 cannot, in general, do the reordering. One way to implement this is to wake T2 up and have T2 do the work. This is similar to the disentangling of queues that needs to happen when a high-priority task aborts a lower-priority task, which might have a call queued on a protected object with a low ceiling. 

Reason: A previous version of Ada 9X made it a run-time error for a high-priority task to set the priority of a lower-priority task that has a queued call on a protected object with a low ceiling. This was changed because: 

The check was not feasible to implement on all systems, since priority changes might be deferred due to inter-processor communication overhead. The calling task would continue to execute without finding out whether the operation succeeded or not.

The runtime check would tend to cause intermittent system failures - how is the caller supposed to know whether the other task happens to have a queued call at any given time? Consider for example an interrupt that needs to trigger a priority change in some task. The interrupt handler could not safely call Set_Priority without knowing exactly what the other task is doing, or without severely restricting the ceilings used in the system. If the interrupt handler wants to hand the job off to a third task whose job is to call Set_Priority, this won't help, because one would normally want the third task to have high priority. 


#### Bounded (Run-Time) Errors

If a task is blocked on a protected entry call, and the call is queued, it is a bounded error to raise its base priority above the ceiling priority of the corresponding protected object. When an entry call is cancelled, it is a bounded error if the priority of the calling task is higher than the ceiling priority of the corresponding protected object. In either of these cases, either Program_Error is raised in the task that called the entry, or its priority is temporarily lowered, or both, or neither. 

Ramification: Note that the error is "blamed" on the task that did the entry call, not the task that called Set_Priority. This seems to make sense for the case of a task blocked on a call, since if the Set_Priority had happened a little bit sooner, before the task queued a call, the entry-calling task would get the error. In the other case, there is no reason not to raise the priority of a task that is executing in an abortable_part, so long as its priority is lowered before it gets to the end and needs to cancel the call. The priority might need to be lowered to allow it to remove the call from the entry queue, in order to avoid violating the ceiling. This seems relatively harmless, since there is an error, and the task is about to start raising an exception anyway. 


#### Erroneous Execution

If any subprogram in this package is called with a parameter T that specifies a task object that no longer exists, the execution of the program is erroneous. 

Ramification: Note that this rule overrides the above rule saying that Program_Error is raised on Get_Priority of a terminated task. If the task object still exists, and the task is terminated, Get_Priority raises Program_Error. However, if the task object no longer exists, calling Get_Priority causes erroneous execution. 


#### Metrics

The implementation shall document the following metric: 

The execution time of a call to Set_Priority, for the nonpreempting case, in processor clock cycles. This is measured for a call that modifies the priority of a ready task that is not running (which cannot be the calling one), where the new base priority of the affected task is lower than the active priority of the calling task, and the affected task is not on any entry queue and is not executing a protected operation. 

NOTE 1   Setting a task's base priority affects task dispatching. First, it can change the task's active priority. Second, under the standard task dispatching policy it always causes the task to move to the tail of the ready queue corresponding to its active priority, even if the new base priority is unchanged.

NOTE 2   Under the priority queuing policy, setting a task's base priority has an effect on a queued entry call if the task is blocked waiting for the call. That is, setting the base priority of a task causes the priority of a queued entry call from that task to be updated and the call to be removed and then reinserted in the entry queue at the new priority (see D.4), unless the call originated from the triggering_statement of an asynchronous_select.

NOTE 3   The effect of two or more Set_Priority calls executed in parallel on the same task is defined as executing these calls in some serial order.

Proof: This follows from the general reentrancy requirements stated near the beginning of Annex A, "Predefined Language Environment". 

NOTE 4   The rule for when Tasking_Error is raised for Set_Priority or Get_Priority is different from the rule for when Tasking_Error is raised on an entry call (see 9.5.3). In particular, setting or querying the priority of a completed or an abnormal task is allowed, so long as the task is not yet terminated.

NOTE 5   Changing the priorities of a set of tasks can be performed by a series of calls to Set_Priority for each task separately. For this to work reliably, it should be done within a protected operation that has high enough ceiling priority to guarantee that the operation completes without being preempted by any of the affected tasks.


#### Static Semantics






## D.6  Preemptive Abort

[This clause specifies requirements on the immediacy with which an aborted construct is completed.] 


#### Dynamic Semantics

On a system with a single processor, an aborted construct is completed immediately at the first point that is outside the execution of an abort-deferred operation.


#### Documentation Requirements

On a multiprocessor, the implementation shall document any conditions that cause the completion of an aborted construct to be delayed later than what is specified for a single processor. 

This paragraph was deleted.Implementation defined: On a multiprocessor, any conditions that cause the completion of an aborted construct to be delayed later than what is specified for a single processor.


#### Metrics

The implementation shall document the following metrics: 

The execution time, in processor clock cycles, that it takes for an abort_statement to cause the completion of the aborted task. This is measured in a situation where a task T2 preempts task T1 and aborts T1. T1 does not have any finalization code. T2 shall verify that T1 has terminated, by means of the Terminated attribute.

On a multiprocessor, an upper bound in seconds, on the time that the completion of an aborted task can be delayed beyond the point that it is required for a single processor.

An upper bound on the execution time of an asynchronous_select, in processor clock cycles. This is measured between a point immediately before a task T1 executes a protected operation Pr.Set that makes the condition of an entry_barrier Pr.Wait true, and the point where task T2 resumes execution immediately after an entry call to Pr.Wait in an asynchronous_select. T1 preempts T2 while T2 is executing the abortable part, and then blocks itself so that T2 can execute. The execution time of T1 is measured separately, and subtracted.

An upper bound on the execution time of an asynchronous_select, in the case that no asynchronous transfer of control takes place. This is measured between a point immediately before a task executes the asynchronous_select with a nonnull abortable part, and the point where the task continues execution immediately after it. The execution time of the abortable part is subtracted. 


#### Implementation Advice

Even though the abort_statement is included in the list of potentially blocking operations (see 9.5.1), it is recommended that this statement be implemented in a way that never requires the task executing the abort_statement to block.

On a multi-processor, the delay associated with aborting a task on another processor should be bounded; the implementation should use periodic polling, if necessary, to achieve this.

NOTE 1   Abortion does not change the active or base priority of the aborted task.

NOTE 2   Abortion cannot be more immediate than is allowed by the rules for deferral of abortion during finalization and in protected actions.


## D.7  Tasking Restrictions

[This clause defines restrictions that can be used with a pragma Restrictions (see 13.12) to facilitate the construction of highly efficient tasking run-time systems.] 


#### Static Semantics

The following restriction_identifiers are language defined: 

No_Task_Hierarchy All (nonenvironment) tasks depend directly on the environment task of the partition.

No_Nested_Finalization Objects with controlled parts and access types that designate such objects shall be declared only at library level. 

Ramification: Note that protected types with entries and interrupt-handling protected types have nontrivial finalization actions. However, this restriction does not restrict those things.

No_Abort_Statements There are no abort_statements, and there are no calls on Task_Identification.Abort_Task.

No_Terminate_Alternatives There are no selective_accepts with terminate_alternatives.

No_Task_Allocators There are no allocators for task types or types containing task subcomponents.

No_Implicit_Heap_Allocations There are no operations that implicitly require heap storage allocation to be performed by the implementation. The operations that implicitly require heap storage allocation are implementation defined. 

Implementation defined: Any operations that implicitly require heap storage allocation.

No_Dynamic_Priorities There are no semantic dependences on the package Dynamic_Priorities. 

No_Asynchronous_Control There are no semantic dependences on the package Asynchronous_Task_Control. 

The following restriction_parameter_identifiers are language defined: 

Max_Select_Alternatives Specifies the maximum number of alternatives in a selective_accept.

Max_Task_Entries Specifies the maximum number of entries per task. The bounds of every entry family of a task unit shall be static, or shall be defined by a discriminant of a subtype whose corresponding bound is static. [A value of zero indicates that no rendezvous are possible.]

Max_Protected_Entries Specifies the maximum number of entries per protected type. The bounds of every entry family of a protected unit shall be static, or shall be defined by a discriminant of a subtype whose corresponding bound is static. 


#### Dynamic Semantics

If the following restrictions are violated, the behavior is implementation defined. If an implementation chooses to detect such a violation, Storage_Error should be raised.

The following restriction_parameter_identifiers are language defined: 

Max_Storage_At_Blocking Specifies the maximum portion [(in storage elements)] of a task's Storage_Size that can be retained by a blocked task. 

Max_Asynchronous_Select_Nesting Specifies the maximum dynamic nesting level of asynchronous_selects. A value of zero prevents the use of any asynchronous_select. 

Max_Tasks Specifies the maximum number of task creations that may be executed over the lifetime of a partition, not counting the creation of the environment task. 

Ramification: Note that this is not a limit on the number of tasks active at a given time; it is a limit on the total number of task creations that occur. 

Implementation Note: We envision an implementation approach that places TCBs or pointers to them in a fixed-size table, and never reuses table elements. 

It is implementation defined whether the use of pragma Restrictions results in a reduction in executable program size, storage requirements, or execution time. If possible, the implementation should provide quantitative descriptions of such effects for each restriction. 

Implementation defined: Implementation-defined aspects of pragma Restrictions.


#### Implementation Advice

When feasible, the implementation should take advantage of the specified restrictions to produce a more efficient implementation.

NOTE 1   The above Storage_Checks can be suppressed with pragma Suppress. 


## D.8  Monotonic Time

[This clause specifies a high-resolution, monotonic clock package.] 


#### Static Semantics

The following language-defined library package exists: 

```ada
package Ada.Real_Time is

```

```ada
  type Time is private;
  Time_First : constant Time;
  Time_Last : constant Time;
  Time_Unit : constant := implementation-defined-real-number;

```

```ada
  type Time_Span is private;
  Time_Span_First : constant Time_Span;
  Time_Span_Last : constant Time_Span;
  Time_Span_Zero : constant Time_Span;
  Time_Span_Unit : constant Time_Span;

```

```ada
  Tick : constant Time_Span;
  function Clock return Time;

```

```ada
  function "+" (Left : Time; Right : Time_Span) return Time;
  function "+" (Left : Time_Span; Right : Time) return Time;
  function "-" (Left : Time; Right : Time_Span) return Time;
  function "-" (Left : Time; Right : Time) return Time_Span;

```

```ada
  function "&lt" (Left, Right : Time) return Boolean;
  function "&lt="(Left, Right : Time) return Boolean;
  function "&gt" (Left, Right : Time) return Boolean;
  function "&gt="(Left, Right : Time) return Boolean;

```

```ada
  function "+" (Left, Right : Time_Span) return Time_Span;
  function "-" (Left, Right : Time_Span) return Time_Span;
  function "-" (Right : Time_Span) return Time_Span;
  function "*" (Left : Time_Span; Right : Integer) return Time_Span;
  function "*" (Left : Integer; Right : Time_Span) return Time_Span;
  function "/" (Left, Right : Time_Span) return Integer;
  function "/" (Left : Time_Span; Right : Integer) return Time_Span;

```

```ada
  function "abs"(Right : Time_Span) return Time_Span;

```

```ada
 

```

```ada
  function "&lt" (Left, Right : Time_Span) return Boolean;
  function "&lt="(Left, Right : Time_Span) return Boolean;
  function "&gt" (Left, Right : Time_Span) return Boolean;
  function "&gt="(Left, Right : Time_Span) return Boolean;

```

```ada
  function To_Duration (TS : Time_Span) return Duration;
  function To_Time_Span (D : Duration) return Time_Span;

```

```ada
  function Nanoseconds  (NS : Integer) return Time_Span;
  function Microseconds (US : Integer) return Time_Span;
  function Milliseconds (MS : Integer) return Time_Span;

```

```ada
  type Seconds_Count is range implementation-defined;

```

```ada
  procedure Split(T : in Time; SC : out Seconds_Count; TS : out Time_Span);
  function Time_Of(SC : Seconds_Count; TS : Time_Span) return Time;

```

```ada
private
   ... -- not specified by the language
end Ada.Real_Time;

```

This paragraph was deleted.Implementation defined: Implementation-defined aspects of package Real_Time.

In this Annex, real time is defined to be the physical time as observed in the external environment. The type Time is a time type as defined by 9.6; [values of this type may be used in a delay_until_statement.] Values of this type represent segments of an ideal time line. The set of values of the type Time corresponds one-to-one with an implementation-defined range of mathematical integers. 

Discussion: Informally, real time is defined to be the International Atomic Time (TAI) which is monotonic and nondecreasing. We use it here for the purpose of discussing rate of change and monotonic behavior only. It does not imply anything about the absolute value of Real_Time.Clock, or about Real_Time.Time being synchronized with TAI. It is also used for real time in the metrics, for comparison purposes. 

Implementation Note: The specification of TAI as "real time" does not preclude the use of a simulated TAI clock for simulated execution environments. 

The Time value I represents the half-open real time interval that starts with E+I*Time_Unit and is limited by E+(I+1)*Time_Unit, where Time_Unit is an implementation-defined real number and E is an unspecified origin point, the epoch, that is the same for all values of the type Time. It is not specified by the language whether the time values are synchronized with any standard time reference. [For example, E can correspond to the time of system initialization or it can correspond to the epoch of some time standard.] 

Discussion: E itself does not have to be a proper time value.

This half-open interval I consists of all real numbers R such that E+I*Time_Unit &lt= R &lt E+(I+1)*Time_Unit. 

Values of the type Time_Span represent length of real time duration. The set of values of this type corresponds one-to-one with an implementation-defined range of mathematical integers. The Time_Span value corresponding to the integer I represents the real-time duration I*Time_Unit. 

Reason: The purpose of this type is similar to Standard.Duration; the idea is to have a type with a higher resolution. 

Discussion: We looked at many possible names for this type: Real_Time.Duration, Fine_Duration, Interval, Time_Interval_Length, Time_Measure, and more. Each of these names had some problems, and we've finally settled for Time_Span. 

Time_First and Time_Last are the smallest and largest values of the Time type, respectively. Similarly, Time_Span_First and Time_Span_Last are the smallest and largest values of the Time_Span type, respectively.

A value of type Seconds_Count represents an elapsed time, measured in seconds, since the epoch.


#### Dynamic Semantics

Time_Unit is the smallest amount of real time representable by the Time type; it is expressed in seconds. Time_Span_Unit is the difference between two successive values of the Time type. It is also the smallest positive value of type Time_Span. Time_Unit and Time_Span_Unit represent the same real time duration. A clock tick is a real time interval during which the clock value (as observed by calling the Clock function) remains constant. Tick is the average length of such intervals.

The function To_Duration converts the value TS to a value of type Duration. Similarly, the function To_Time_Span converts the value D to a value of type Time_Span. For both operations, the result is rounded to the nearest exactly representable value (away from zero if exactly halfway between two exactly representable values).

To_Duration(Time_Span_Zero) returns 0.0, and To_Time_Span(0.0) returns Time_Span_Zero.

The functions Nanoseconds, Microseconds, and Milliseconds convert the input parameter to a value of the type Time_Span. NS, US, and MS are interpreted as a number of nanoseconds, microseconds, and milliseconds respectively.The result is rounded to the nearest exactly representable value (away from zero if exactly halfway between two exactly representable values). 

Discussion: The above does not imply that the Time_Span type will have to accommodate Integer'Last of milliseconds; Constraint_Error is allowed to be raised. 

The effects of the operators on Time and Time_Span are as for the operators defined for integer types. 

Implementation Note: Though time values are modeled by integers, the types Time and Time_Span need not be implemented as integers. 

The function Clock returns the amount of time since the epoch.

The effects of the Split and Time_Of operations are defined as follows, treating values of type Time, Time_Span, and Seconds_Count as mathematical integers. The effect of Split(T,SC,TS) is to set SC and TS to values such that T*Time_Unit = SC*1.0 + TS*Time_Unit, and 0.0 &lt= TS*Time_Unit &lt 1.0. The value returned by Time_Of(SC,TS) is the value T such that T*Time_Unit = SC*1.0 + TS*Time_Unit. 


#### Implementation Requirements

The range of Time values shall be sufficient to uniquely represent the range of real times from program start-up to 50 years later. Tick shall be no greater than 1 millisecond. Time_Unit shall be less than or equal to 20 microseconds. 

Implementation Note: The required range and accuracy of Time are such that 32-bits worth of seconds and 32-bits worth of ticks in a second could be used as the representation. 

Time_Span_First shall be no greater than 3600 seconds, and Time_Span_Last shall be no less than 3600 seconds. 

Reason: This is equivalent to ± one hour and there is still room for a two-microsecond resolution. 

A clock jump is the difference between two successive distinct values of the clock (as observed by calling the Clock function). There shall be no backward clock jumps.


#### Documentation Requirements

The implementation shall document the values of Time_First, Time_Last, Time_Span_First, Time_Span_Last, Time_Span_Unit, and Tick. 

The implementation shall document the properties of the underlying time base used for the clock and for type Time, such as the range of values supported and any relevant aspects of the underlying hardware or operating system facilities used. 

Discussion: If there is an underlying operating system, this might include information about which system call is used to implement the clock. Otherwise, it might include information about which hardware clock is used. 

The implementation shall document whether or not there is any synchronization with external time references, and if such synchronization exists, the sources of synchronization information, the frequency of synchronization, and the synchronization method applied. 

The implementation shall document any aspects of the the external environment that could interfere with the clock behavior as defined in this clause. 

Discussion: For example, the implementation is allowed to rely on the time services of an underlying operating system, and this operating system clock can implement time zones or allow the clock to be reset by an operator. This dependence has to be documented. 


#### Metrics

For the purpose of the metrics defined in this clause, real time is defined to be the International Atomic Time (TAI).

The implementation shall document the following metrics: 

An upper bound on the real-time duration of a clock tick. This is a value D such that if t1 and t2 are any real times such that t1 &lt t2 and Clockt1 = Clockt2 then t2  t1 &lt= D.

An upper bound on the size of a clock jump.

An upper bound on the drift rate of Clock with respect to real time. This is a real number D such that 

E*(1D) &lt= (Clockt+E  Clockt) &lt= E*(1+D)
        provided that: Clockt + E*(1+D) &lt= Time_Last.

where Clockt is the value of Clock at time t, and E is a real time duration not less than 24 hours. The value of E used for this metric shall be reported. 

Reason: This metric is intended to provide a measurement of the long term (cumulative) deviation; therefore, 24 hours is the lower bound on the measurement period. On some implementations, this is also the maximum period, since the language does not require that the range of the type Duration be more than 24 hours. On those implementations that support longer-range Duration, longer measurements should be performed. 

An upper bound on the execution time of a call to the Clock function, in processor clock cycles.

Upper bounds on the execution times of the operators of the types Time and Time_Span, in processor clock cycles. 

Implementation Note: A fast implementation of the Clock function involves repeated reading until you get the same value twice. It is highly improbable that more than three reads will be necessary. Arithmetic on time values should not be significantly slower than 64-bit arithmetic in the underlying machine instruction set. 


#### Implementation Permissions

Implementations targeted to machines with word size smaller than 32 bits need not support the full range and granularity of the Time and Time_Span types. 

Discussion: These requirements are based on machines with a word size of 32 bits.

Since the range and granularity are implementation defined, the supported values need to be documented. 


#### Implementation Advice

When appropriate, implementations should provide configuration mechanisms to change the value of Tick. 

Reason: This is often needed when the compilation system was originally targeted to a particular processor with a particular interval timer, but the customer uses the same processor with a different interval timer. 

Discussion: Tick is a deferred constant and not a named number specifically for this purpose. 

Implementation Note: This can be achieved either by pre-run-time configuration tools, or by having Tick be initialized (in the package private part) by a function call residing in a board specific module. 

It is recommended that Calendar.Clock and Real_Time.Clock be implemented as transformations of the same time base. 

It is recommended that the "best" time base which exists in the underlying system be available to the application through Clock. "Best" may mean highest accuracy or largest range. 

NOTE 1   The rules in this clause do not imply that the implementation can protect the user from operator or installation errors which could result in the clock being set incorrectly.

NOTE 2   Time_Unit is the granularity of the Time type. In contrast, Tick represents the granularity of Real_Time.Clock. There is no requirement that these be the same.


## D.9  Delay Accuracy

[This clause specifies performance requirements for the delay_statement. The rules apply both to delay_relative_statement and to delay_until_statement. Similarly, they apply equally to a simple delay_statement and to one which appears in a delay_alternative.] 


#### Dynamic Semantics

The effect of the delay_statement for Real_Time.Time is defined in terms of Real_Time.Clock: 

If C1 is a value of Clock read before a task executes a delay_relative_statement with duration D, and C2 is a value of Clock read after the task resumes execution following that delay_statement, then C2  C1 &gt= D.

If C is a value of Clock read after a task resumes execution following a delay_until_statement with Real_Time.Time value T, then C &gt= T. 

A simple delay_statement with a negative or zero value for the expiration time does not cause the calling task to be blocked; it is nevertheless a potentially blocking operation (see 9.5.1).

When a delay_statement appears in a delay_alternative of a timed_entry_call the selection of the entry call is attempted, regardless of the specified expiration time. When a delay_statement appears in a selective_accept_alternative, and a call is queued on one of the open entries, the selection of that entry call proceeds, regardless of the value of the delay expression. 

Ramification: The effect of these requirements is that one has to always attempt a rendezvous, regardless of the value of the delay expression. This can be tested by issuing a timed_entry_call with an expiration time of zero, to an open entry. 


#### Documentation Requirements

The implementation shall document the minimum value of the delay expression of a delay_relative_statement that causes the task to actually be blocked. 

The implementation shall document the minimum difference between the value of the delay expression of a delay_until_statement and the value of Real_Time.Clock, that causes the task to actually be blocked. 

This paragraph was deleted.Implementation defined: Implementation-defined aspects of delay_statements.


#### Metrics

The implementation shall document the following metrics: 

An upper bound on the execution time, in processor clock cycles, of a delay_relative_statement whose requested value of the delay expression is less than or equal to zero.

An upper bound on the execution time, in processor clock cycles, of a delay_until_statement whose requested value of the delay expression is less than or equal to the value of Real_Time.Clock at the time of executing the statement. Similarly, for Calendar.Clock.

An upper bound on the lateness of a delay_relative_statement, for a positive value of the delay expression, in a situation where the task has sufficient priority to preempt the processor as soon as it becomes ready, and does not need to wait for any other execution resources. The upper bound is expressed as a function of the value of the delay expression. The lateness is obtained by subtracting the value of the delay expression from the actual duration. The actual duration is measured from a point immediately before a task executes the delay_statement to a point immediately after the task resumes execution following this statement.

An upper bound on the lateness of a delay_until_statement, in a situation where the value of the requested expiration time is after the time the task begins executing the statement, the task has sufficient priority to preempt the processor as soon as it becomes ready, and it does not need to wait for any other execution resources. The upper bound is expressed as a function of the difference between the requested expiration time and the clock value at the time the statement begins execution. The lateness of a delay_until_statement is obtained by subtracting the requested expiration time from the real time that the task resumes execution following this statement. 

NOTE 1   The execution time of a delay_statement that does not cause the task to be blocked (e.g. "delay 0.0;" ) is of interest in situations where delays are used to achieve voluntary round-robin task dispatching among equal-priority tasks.


#### Wording Changes from Ada 83

The rules regarding a timed_entry_call with a very small positive Duration value, have been tightened to always require the check whether the rendezvous is immediately possible.


## D.10  Synchronous Task Control

[This clause describes a language-defined private semaphore (suspension object), which can be used for two-stage suspend operations and as a simple building block for implementing higher-level queues.] 


#### Static Semantics

The following language-defined package exists: 

```ada
package Ada.Synchronous_Task_Control is

```

```ada
  type Suspension_Object is limited private;
  procedure Set_True(S : in out Suspension_Object);
  procedure Set_False(S : in out Suspension_Object);
  function Current_State(S : Suspension_Object) return Boolean;
  procedure Suspend_Until_True(S : in out Suspension_Object);
private
     ... -- not specified by the language
end Ada.Synchronous_Task_Control;

```

The type Suspension_Object is a by-reference type.

Implementation Note: The implementation can ensure this by, for example, making the full view a limited record type.


#### Dynamic Semantics

An object of the type Suspension_Object has two visible states: true and false. Upon initialization, its value is set to false. 

Discussion: This object is assumed to be private to the declaring task, i.e. only that task will call Suspend_Until_True on this object, and the count of callers is at most one. Other tasks can, of course, change and query the state of this object. 

The operations Set_True and Set_False are atomic with respect to each other and with respect to Suspend_Until_True; they set the state to true and false respectively.

Current_State returns the current state of the object. 

Discussion: This state can change immediately after the operation returns. 

The procedure Suspend_Until_True blocks the calling task until the state of the object S is true; at that point the task becomes ready and the state of the object becomes false.

Program_Error is raised upon calling Suspend_Until_True if another task is already waiting on that suspension object. Suspend_Until_True is a potentially blocking operation (see 9.5.1).


#### Implementation Requirements

The implementation is required to allow the calling of Set_False and Set_True during any protected action, even one that has its ceiling priority in the Interrupt_Priority range.


## D.11  Asynchronous Task Control

[This clause introduces a language-defined package to do asynchronous suspend/resume on tasks. It uses a conceptual held priority value to represent the task's held state.] 


#### Static Semantics

The following language-defined library package exists: 

```ada
with Ada.Task_Identification;
package Ada.Asynchronous_Task_Control is
  procedure Hold(T : in Ada.Task_Identification.Task_Id);
  procedure Continue(T : in Ada.Task_Identification.Task_Id);
  function Is_Held(T : Ada.Task_Identification.Task_Id)
   return Boolean;
end Ada.Asynchronous_Task_Control;

```


#### Dynamic Semantics

After the Hold operation has been applied to a task, the task becomes held. For each processor there is a conceptual idle task, which is always ready. The base priority of the idle task is below System.Any_Priority'First. The held priority is a constant of the type integer whose value is below the base priority of the idle task. 

Discussion: The held state should not be confused with the blocked state as defined in 9.2; the task is still ready. 

The Hold operation sets the state of T to held. For a held task: the task's own base priority does not constitute an inheritance source (see D.1), and the value of the held priority is defined to be such a source instead. 

Ramification: For example, if T is currently inheriting priorities from other sources (e.g. it is executing in a protected action), its active priority does not change, and it continues to execute until it leaves the protected action. 

The Continue operation resets the state of T to not-held; T's active priority is then reevaluated as described in D.1. [This time, T's base priority is taken into account.]

The Is_Held function returns True if and only if T is in the held state. 

Discussion: Note that the state of T can be changed immediately after Is_Held returns. 

As part of these operations, a check is made that the task identified by T is not terminated. Tasking_Error is raised if the check fails. Program_Error is raised if the value of T is Null_Task_Id.


#### Erroneous Execution

If any operation in this package is called with a parameter T that specifies a task object that no longer exists, the execution of the program is erroneous. 


#### Implementation Permissions

An implementation need not support Asynchronous_Task_Control if it is infeasible to support it in the target environment. 

Reason: A direct implementation of the Asynchronous_Task_Control semantics using priorities is not necessarily efficient enough. Thus, we envision implementations that use some other mechanism to set the "held" state. If there is no other such mechanism, support for Asynchronous_Task_Control might be infeasible, because an implementation in terms of priority would require one idle task per processor. On some systems, programs are not supposed to know how many processors are available, so creating enough idle tasks would be problematic. 

NOTE 1   It is a consequence of the priority rules that held tasks cannot be dispatched on any processor in a partition (unless they are inheriting priorities) since their priorities are defined to be below the priority of any idle task.

NOTE 2   The effect of calling Get_Priority and Set_Priority on a Held task is the same as on any other task.

NOTE 3   Calling Hold on a held task or Continue on a non-held task has no effect.

NOTE 4   The rules affecting queuing are derived from the above rules, in addition to the normal priority rules: 

When a held task is on the ready queue, its priority is so low as to never reach the top of the queue as long as there are other tasks on that queue.

If a task is executing in a protected action, inside a rendezvous, or is inheriting priorities from other sources (e.g. when activated), it continues to execute until it is no longer executing the corresponding construct.

If a task becomes held while waiting (as a caller) for a rendezvous to complete, the active priority of the accepting task is not affected.

If a task becomes held while waiting in a selective_accept, and a entry call is issued to one of the open entries, the corresponding accept body executes. When the rendezvous completes, the active priority of the accepting task is lowered to the held priority (unless it is still inheriting from other sources), and the task does not execute until another Continue.

The same holds if the held task is the only task on a protected entry queue whose barrier becomes open. The corresponding entry body executes.


## D.12  Other Optimizations and Determinism Rules

[This clause describes various requirements for improving the response and determinism in a real-time system.] 


#### Implementation Requirements

If the implementation blocks interrupts (see C.3) not as a result of direct user action (e.g. an execution of a protected action) there shall be an upper bound on the duration of this blocking. 

Ramification: The implementation shall not allow itself to be interrupted when it is in a state where it is unable to support all the language-defined operations permitted in the execution of interrupt handlers. (see 9.5.1). 

The implementation shall recognize entry-less protected types. The overhead of acquiring the execution resource of an object of such a type (see 9.5.1) shall be minimized. In particular, there should not be any overhead due to evaluating entry_barrier conditions. 

Implementation Note: Ideally the overhead should just be a spin-lock. 

Unchecked_Deallocation shall be supported for terminated tasks that are designated by access types, and shall have the effect of releasing all the storage associated with the task. This includes any run-time system or heap storage that has been implicitly allocated for the task by the implementation.


#### Documentation Requirements

The implementation shall document the upper bound on the duration of interrupt blocking caused by the implementation. If this is different for different interrupts or interrupt priority levels, it should be documented for each case. 

This paragraph was deleted.Implementation defined: The upper bound on the duration of interrupt blocking caused by the implementation.


#### Metrics

The implementation shall document the following metric: 

The overhead associated with obtaining a mutual-exclusive access to an entry-less protected object. This shall be measured in the following way:

For a protected object of the form: 

```ada
protected Lock is
   procedure Set;
   function Read return Boolean;
private
   Flag : Boolean := False;
end Lock;

```

```ada
protected body Lock is
   procedure Set is
   begin
      Flag := True;
   end Set;
   function Read return Boolean
   begin
      return Flag;
   end Read;
end Lock;

```

The execution time, in processor clock cycles, of a call to Set. This shall be measured between the point just before issuing the call, and the point just after the call completes. The function Read shall be called later to verify that Set was indeed called (and not optimized away). The calling task shall have sufficiently high priority as to not be preempted during the measurement period. The protected object shall have sufficiently high ceiling priority to allow the task to call Set.

For a multiprocessor, if supported, the metric shall be reported for the case where no contention (on the execution resource) exists [from tasks executing on other processors]. 


#### Syntax

  

Documentation Requirement: 

Aspect Description for CPU: 

