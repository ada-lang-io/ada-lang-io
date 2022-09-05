---
sidebar_position:  153
---

# D.2  Priority Scheduling

{AI95-00321-01} {AI05-0299-1} [This subclause describes the rules that determine which task is selected for execution when more than one task is ready (see 9).] 


#### Wording Changes from Ada 95

{AI95-00321-01} {AI05-0299-1} This introduction is simplified in order to reflect the rearrangement and expansion of this subclause. 


## D.2.1  The Task Dispatching Model

{AI95-00321-01} [The task dispatching model specifies task scheduling, based on conceptual priority-ordered ready queues.] 


#### Static Semantics

{AI95-00355-01} The following language-defined library package exists: 

```ada
{AI05-0166-1} {AI12-0241-1} {AI12-0302-1} package Ada.Dispatching
  with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI05-0166-1} {AI12-0241-1}   procedure Yield
   with Nonblocking =&gt False;

```

```ada
{AI05-0166-1}   Dispatching_Policy_Error : exception;
end Ada.Dispatching;

```

Dispatching serves as the parent of other language-defined library units concerned with task dispatching.

{AI12-0279-1} For a noninstance subprogram (including a generic formal subprogram), a generic subprogram, or an entry, the following language-defined aspect may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1):

YieldThe type of aspect Yield is Boolean.

Aspect Description for Yield: Ensures that a callable entity includes a task dispatching point.

If directly specified, the [aspect_definition](./AA-13.1#S0348) shall be a static expression. If not specified (including by inheritance), the aspect is False.

{AI12-0279-1} {AI12-0294-1} If a Yield aspect is specified True for a primitive subprogram S of a type T, then the aspect is inherited by the corresponding primitive subprogram of each descendant of T. 


#### Legality Rules

{AI12-0279-1} {AI12-0294-1} If the Yield aspect is specified for a dispatching subprogram that inherits the aspect, the specified value shall be confirming.

{AI12-0279-1} {AI12-0294-1} If the Nonblocking aspect (see 9.5) of the associated callable entity is statically True, the Yield aspect shall not be specified as True. For a callable entity that is declared within a generic body, this rule is checked assuming that any nonstatic Nonblocking attributes in the expression of the Nonblocking aspect of the entity are statically True.

Reason: {AI12-0294-1} The second sentence here is an assume-the-worst rule. The only Nonblocking attributes that are nonstatic are those that depend, directly or indirectly, on the nonblocking aspect of a generic formal parameter. We have to assume these might in fact have the value True if given an appropriate actual entity. 

{AI12-0294-1} In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit. 


#### Dynamic Semantics

{AI95-00321-01} A task can become a running task only if it is ready (see 9) and the execution resources required by that task are available. Processors are allocated to tasks based on each task's active priority.

It is implementation defined whether, on a multiprocessor, a task that is waiting for access to a protected object keeps its processor busy. 

Implementation defined: Whether, on a multiprocessor, a task that is waiting for access to a protected object keeps its processor busy.

{AI95-00321-01} {AI12-0119-1} Task dispatching is the process by which a logical thread of control associated with a ready task is selected for execution on a processor. This selection is done during the execution of such a logical thread of control, at certain points called task dispatching points. Such a logical thread of control reaches a task dispatching point whenever it becomes blocked, and when its associated task terminates. [Other task dispatching points are defined throughout this Annex for specific policies.] Below we talk in terms of tasks, but in the context of a parallel construct, a single task can be represented by multiple logical threads of control, each of which can appear separately on a ready queue. 

Ramification: On multiprocessor systems, more than one task can be chosen, at the same time, for execution on more than one processor, as explained below. 

{AI95-00321-01} Task dispatching policies are specified in terms of conceptual ready queues and task states. A ready queue is an ordered list of ready tasks. The first position in a queue is called the head of the queue, and the last position is called the tail of the queue. A task is ready if it is in a ready queue, or if it is running. Each processor has one ready queue for each priority value. At any instant, each ready queue of a processor contains exactly the set of tasks of that priority that are ready for execution on that processor, but are not running on any processor; that is, those tasks that are ready, are not running on any processor, and can be executed using that processor and other available resources. A task can be on the ready queues of more than one processor. 

Discussion: The core language defines a ready task as one that is not blocked. Here we refine this definition and talk about ready queues. 

{AI95-00321-01} Each processor also has one running task, which is the task currently being executed by that processor. Whenever a task running on a processor reaches a task dispatching point it goes back to one or more ready queues; a task (possibly the same task) is then selected to run on that processor. The task selected is the one at the head of the highest priority nonempty ready queue; this task is then removed from all ready queues to which it belongs. 

Discussion: There is always at least one task to run, if we count the idle task. 

{AI95-00321-01} {AI05-0166-1} {AI12-0241-1} A call of Yield and a [delay_statement](./AA-9.6#S0266) are task dispatching points for all language-defined policies.

This paragraph was deleted.

{AI95-00321-01} {AI12-0279-1} If the Yield aspect has the value True, then a call to procedure Yield is included within the body of the associated callable entity, and invoked immediately prior to returning from the body if and only if no other task dispatching points were encountered during the execution of the body. 

This paragraph was deleted.


#### Implementation Permissions

{AI95-00321-01} An implementation is allowed to define additional resources as execution resources, and to define the corresponding allocation policies for them. Such resources may have an implementation-defined effect on task dispatching. 

Implementation defined: The effect of implementation-defined execution resources on task dispatching.

An implementation may place implementation-defined restrictions on tasks whose active priority is in the Interrupt_Priority range. 

Ramification: {AI05-0229-1} For example, on some operating systems, it might be necessary to disallow them altogether. This permission applies to tasks whose priority is set to interrupt level for any reason: via an aspect, via a call to Dynamic_Priorities.Set_Priority, or via priority inheritance. 

{AI95-00321-01} {AI12-0299-1} Unless otherwise specified for a task dispatching policy, an implementation may add additional points at which task dispatching may occur, in an implementation-defined manner.

Reason: {AI12-0299-1} This permission is intended to allow the implementation of Ada tasks in terms of target system threads, which may have additional conditions that cause task dispatching. For instance, for Linux threads, page faults are task dispatching points. 

Discussion: {AI12-0299-1} The Non_Preemptive_FIFO_Within_Priorities task dispatching policy (see D.2.4) does not allow additional task dispatching points. 

NOTE 1   {AI05-0299-1} Clause 9 specifies under which circumstances a task becomes ready. The ready state is affected by the rules for task activation and termination, delay statements, and entry calls. When a task is not ready, it is said to be blocked.

NOTE 2   {AI12-0442-1} An example of a possible implementation-defined execution resource is a page of physical memory, which must be loaded with a particular page of virtual memory before a task can continue execution.

NOTE 3   The ready queues are purely conceptual; there is no requirement that such lists physically exist in an implementation.

NOTE 4   While a task is running, it is not on any ready queue. Any time the task that is running on a processor is added to a ready queue, a new running task is selected for that processor.

NOTE 5   In a multiprocessor system, a task can be on the ready queues of more than one processor. At the extreme, if several processors share the same set of ready tasks, the contents of their ready queues is identical, and so they can be viewed as sharing one ready queue, and can be implemented that way. [Thus, the dispatching model covers multiprocessors where dispatching is implemented using a single ready queue, as well as those with separate dispatching domains.]

NOTE 6   The priority of a task is determined by rules specified in this subclause, and under D.1, "Task Priorities", D.3, "Priority Ceiling Locking", and D.5, "Dynamic Priorities".

NOTE 7   {AI95-00321-01} The setting of a task's base priority as a result of a call to Set_Priority does not always take effect immediately when Set_Priority is called. The effect of setting the task's base priority is deferred while the affected task performs a protected action.


#### Wording Changes from Ada 95

{AI95-00321-01} {AI05-0005-1} This description is simplified to describe only the parts of the dispatching model common to all policies. In particular, rules about preemption are moved elsewhere. This makes it easier to add other policies (which might not involve preemption). 


#### Incompatibilities With Ada 2005

{AI05-0166-1} Procedure Yield is added to Dispatching. If Dispatching is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) of Yield is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur.

{AI05-0166-1} {AI12-0005-1} Package Dispatching was a Pure package, but now is Preelaborated with the addition of Yield. This is incompatible as Dispatching can no longer be depended upon from a Pure package. This should happen rarely in practice as the only contents was the exception Dispatching_Policy_Error and none of the child packages that could raise that exception are pure. 


#### Inconsistencies With Ada 2012

{AI12-0299-1} Correction: Substantially reduced the Implementation Permission that used to allow "altering" task dispatching points to only allow adding task dispatching points. If an implementation was using this permission to remove task dispatching points, and a program depended on that behavior to work, it could fail when used with Ada 2022. We are not aware of any such implementations, and such behavior was never portable to other implementations, so we do not expect this to matter in practice. 


#### Extensions to Ada 2012

{AI12-0279-1} {AI12-0294-1} Aspect Yield is new. 


#### Wording Changes from Ada 2012

{AI12-0119-1} Redid the description of task dispatching to include the separate threads of control that can be started by a parallel construct. 


## D.2.2  Task Dispatching Pragmas

{AI95-00355-01} {AI05-0299-1} [This subclause allows a single task dispatching policy to be defined for all priorities, or the range of priorities to be split into subranges that are assigned individual dispatching policies.] 


#### Syntax

The form of a [pragma](./AA-2.8#S0019) Task_Dispatching_Policy is as follows: 

  pragma Task_Dispatching_Policy(policy_[identifier](./AA-2.3#S0002));

{AI95-00355-01} The form of a [pragma](./AA-2.8#S0019) Priority_Specific_Dispatching is as follows: 

  pragma Priority_Specific_Dispatching (
     policy_[identifier](./AA-2.3#S0002), first_priority_[expression](./AA-4.4#S0132), last_priority_[expression](./AA-4.4#S0132));


#### Name Resolution Rules

{AI95-00355-01} The expected type for first_priority_[expression](./AA-4.4#S0132) and last_priority_[expression](./AA-4.4#S0132) is Integer. 


#### Legality Rules

{AI95-00321-01} {AI95-00355-01} The policy_[identifier](./AA-2.3#S0002) used in a [pragma](./AA-2.8#S0019) Task_Dispatching_Policy shall be the name of a task dispatching policy. 

This paragraph was deleted.

{AI95-00355-01} The policy_[identifier](./AA-2.3#S0002) used in a [pragma](./AA-2.8#S0019) Priority_Specific_Dispatching shall be the name of a task dispatching policy.

{AI95-00355-01} Both first_priority_[expression](./AA-4.4#S0132) and last_priority_[expression](./AA-4.4#S0132) shall be static expressions in the range of System.Any_Priority; last_priority_[expression](./AA-4.4#S0132) shall have a value greater than or equal to first_priority_[expression](./AA-4.4#S0132).


#### Static Semantics

{AI95-00355-01} [Pragma](./AA-2.8#S0019) Task_Dispatching_Policy specifies the single task dispatching policy.

{AI95-00355-01} [Pragma](./AA-2.8#S0019) Priority_Specific_Dispatching specifies the task dispatching policy for the specified range of priorities. Tasks with base priorities within the range of priorities specified in a Priority_Specific_Dispatching pragma have their active priorities determined according to the specified dispatching policy. Tasks with active priorities within the range of priorities specified in a Priority_Specific_Dispatching pragma are dispatched according to the specified dispatching policy. 

Reason: {AI95-00355-01} Each ready queue is managed by exactly one policy. Anything else would be chaos. The ready queue is determined by the active priority. However, how the active priority is calculated is determined by the policy; in order to break out of this circle, we have to say that the active priority is calculated by the method determined by the policy of the base priority. 

{AI95-00355-01} {AI05-0262-1} If a partition contains one or more Priority_Specific_Dispatching pragmas, the dispatching policy for priorities not covered by any Priority_Specific_Dispatching pragmas is FIFO_Within_Priorities.


#### Post-Compilation Rules

{AI95-00355-01} A Task_Dispatching_Policy pragma is a configuration pragma. A Priority_Specific_Dispatching pragma is a configuration pragma. 

{AI95-00355-01} The priority ranges specified in more than one Priority_Specific_Dispatching pragma within the same partition shall not be overlapping.

{AI95-00355-01} If a partition contains one or more Priority_Specific_Dispatching pragmas it shall not contain a Task_Dispatching_Policy pragma.

This paragraph was deleted.{AI95-00333-01} 


#### Dynamic Semantics

{AI95-00355-01} [A task dispatching policy specifies the details of task dispatching that are not covered by the basic task dispatching model. These rules govern when tasks are inserted into and deleted from the ready queues.] A single task dispatching policy is specified by a Task_Dispatching_Policy pragma. Pragma Priority_Specific_Dispatching assigns distinct dispatching policies to subranges of System.Any_Priority.

{AI95-00355-01} If neither [pragma](./AA-2.8#S0019) applies to any of the program units comprising a partition, the task dispatching policy for that partition is unspecified.

{AI95-00355-01} {AI05-0262-1} If a partition contains one or more Priority_Specific_Dispatching pragmas, a task dispatching point occurs for the currently running task of a processor whenever there is a nonempty ready queue for that processor with a higher priority than the priority of the running task.

Discussion: {AI05-0005-1} If we have priority specific dispatching then we want preemption across the entire range of priorities. That prevents higher priority tasks from being blocked by lower priority tasks that have a different policy. On the other hand, if we have a single policy for the entire partition, we want the characteristics of that policy to apply for preemption; specifically, we might not require any preemption. Note that policy Non_Preemptive_FIFO_Within_Priorities is not allowed in a priority specific dispatching pragma. 

{AI95-00355-01} A task that has its base priority changed may move from one dispatching policy to another. It is immediately subject to the new dispatching policy.

Ramification: Once subject to the new dispatching policy, it may be immediately preempted or dispatched, according the rules of the new policy. 

Paragraphs 7 through 13 were moved to D.2.3. 


#### Implementation Requirements

{AI95-00333-01} {AI95-00355-01} An implementation shall allow, for a single partition, both the locking policy (see D.3) to be specified as Ceiling_Locking and also one or more Priority_Specific_Dispatching pragmas to be given. 


#### Documentation Requirements

Paragraphs 14 through 16 were moved to D.2.3. 

This paragraph was deleted.


#### Implementation Permissions

{AI95-00256-01} {AI12-0444-1} Implementations are allowed to define other task dispatching policies, but are not required to support specifying more than one task dispatching policy per partition.

{AI95-00355-01} {AI12-0444-1} An implementation is not required to support [pragma](./AA-2.8#S0019) Priority_Specific_Dispatching if it is infeasible to support it in the target environment.

Implementation defined: Implementation defined task dispatching policies.

Paragraphs 19 through 21 were deleted. 


#### Extensions to Ada 95

{AI95-00333-01} Amendment Correction: It is no longer required to specify Ceiling_Locking with the language-defined task dispatching policies; we only require that implementations allow them to be used together.

{AI95-00355-01} {AI05-0005-1} Pragma Priority_Specific_Dispatching is new; it allows the specification of different policies for different priorities. 


#### Wording Changes from Ada 95

{AI95-00256-01} Clarified that an implementation need support only one task dispatching policy (of any kind, language-defined or otherwise) per partition.

{AI95-00321-01} {AI05-0005-1} This description is simplified to describe only the rules for the Task_Dispatching_Policy pragma that are common to all policies. In particular, rules about preemption are moved elsewhere. This makes it easier to add other policies (which might not involve preemption). 


## D.2.3  Preemptive Dispatching

{AI95-00321-01} {AI05-0299-1} [This subclause defines a preemptive task dispatching policy.] 


#### Static Semantics

{AI95-00355-01} The policy_[identifier](./AA-2.3#S0002) FIFO_Within_Priorities is a task dispatching policy.


#### Dynamic Semantics

{AI95-00321-01} When FIFO_Within_Priorities is in effect, modifications to the ready queues occur only as follows:

{AI95-00321-01} When a blocked task becomes ready, it is added at the tail of the ready queue for its active priority.

When the active priority of a ready task that is not running changes, or the setting of its base priority takes effect, the task is removed from the ready queue for its old active priority and is added at the tail of the ready queue for its new active priority, except in the case where the active priority is lowered due to the loss of inherited priority, in which case the task is added at the head of the ready queue for its new active priority.

When the setting of the base priority of a running task takes effect, the task is added to the tail of the ready queue for its active priority.

When a task executes a [delay_statement](./AA-9.6#S0266) that does not result in blocking, it is added to the tail of the ready queue for its active priority. 

Ramification: If the delay does result in blocking, the task moves to the "delay queue", not to the ready queue. 

{AI95-00321-01} Each of the events specified above is a task dispatching point (see D.2.1).

{AI95-00321-01} A task dispatching point occurs for the currently running task of a processor whenever there is a nonempty ready queue for that processor with a higher priority than the priority of the running task. The currently running task is said to be preempted and it is added at the head of the ready queue for its active priority.


#### Implementation Requirements

{AI95-00333-01} An implementation shall allow, for a single partition, both the task dispatching policy to be specified as FIFO_Within_Priorities and also the locking policy (see D.3) to be specified as Ceiling_Locking. 

Reason: This is the preferred combination of the FIFO_Within_Priorities policy with a locking policy, and we want that combination to be portable. 


#### Documentation Requirements

{AI95-00321-01} Priority inversion is the duration for which a task remains at the head of the highest priority nonempty ready queue while the processor executes a lower priority task. The implementation shall document:

The maximum priority inversion a user task can experience due to activity of the implementation (on behalf of lower priority tasks), and 

Documentation Requirement: The maximum priority inversion a user task can experience from the implementation.

whether execution of a task can be preempted by the implementation processing of delay expirations for lower priority tasks, and if so, for how long. 

Documentation Requirement: The amount of time that a task can be preempted for processing on behalf of lower-priority tasks.

NOTE 1   {AI95-00321-01} If the active priority of a running task is lowered due to loss of inherited priority (as it is on completion of a protected operation) and there is a ready task of the same active priority that is not running, the running task continues to run (provided that there is no higher priority task).

NOTE 2   {AI95-00321-01} Setting the base priority of a ready task causes the task to move to the tail of the queue for its active priority, regardless of whether the active priority of the task actually changes.


#### Wording Changes from Ada 95

{AI95-00321-01} This subclause is new; it mainly consists of text that was found in D.2.1 and D.2.2 in Ada 95. This was separated out so the definition of additional policies was easier.

{AI95-00333-01} We require that implementations allow this policy and Ceiling_Locking together.

{AI95-00355-01} We explicitly defined FIFO_Within_Priorities to be a task dispatching policy. 


## D.2.4  Non-Preemptive Dispatching

{AI95-00298-01} {AI05-0299-1} [This subclause defines a non-preemptive task dispatching policy.] 


#### Static Semantics

{AI95-00298-01} {AI95-00355-01} The policy_[identifier](./AA-2.3#S0002) Non_Preemptive_FIFO_Within_Priorities is a task dispatching policy.

{AI05-0166-1} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} package Ada.Dispatching.Non_Preemptive
  with Preelaborate, Nonblocking, Global =&gt in out synchronized is
  procedure Yield_To_Higher;
  procedure Yield_To_Same_Or_Higher renames Yield;
end Ada.Dispatching.Non_Preemptive;

```

{AI05-0166-1} {AI05-0264-1} A call of Yield_To_Higher is a task dispatching point for this policy. If the task at the head of the highest priority ready queue has a higher active priority than the calling task, then the calling task is preempted.

Ramification: For language-defined policies other than Non_Preemptive_FIFO_Within_Priorities, a higher priority task should never be on a ready queue while a lower priority task is executed. Thus, for such policies, Yield_To_Higher does nothing.

Yield_To_Higher is not a potentially blocking operation; it can be used during a protected operation. That is allowed, as under the predefined Ceiling_Locking policy any task with a higher priority than the protected operation cannot call the operation (that would violate the locking policy). An implementation-defined locking policy may need to define the semantics of Yield_To_Higher differently. 


#### Legality Rules

{AI95-00355-01} Non_Preemptive_FIFO_Within_Priorities shall not be specified as the policy_[identifier](./AA-2.3#S0002) of [pragma](./AA-2.8#S0019) Priority_Specific_Dispatching (see D.2.2).

Reason: The non-preemptive nature of this policy could cause the policies of higher priority tasks to malfunction, missing deadlines and having unlimited priority inversion. That would render the use of such policies impotent and misleading. As such, this policy only makes sense for a complete system. 


#### Dynamic Semantics

{AI95-00298-01} When Non_Preemptive_FIFO_Within_Priorities is in effect, modifications to the ready queues occur only as follows:

{AI95-00298-01} When a blocked task becomes ready, it is added at the tail of the ready queue for its active priority.

When the active priority of a ready task that is not running changes, or the setting of its base priority takes effect, the task is removed from the ready queue for its old active priority and is added at the tail of the ready queue for its new active priority.

When the setting of the base priority of a running task takes effect, the task is added to the tail of the ready queue for its active priority.

When a task executes a [delay_statement](./AA-9.6#S0266) that does not result in blocking, it is added to the tail of the ready queue for its active priority. 

Ramification: If the delay does result in blocking, the task moves to the "delay queue", not to the ready queue. 

{AI05-0166-1} For this policy, blocking or termination of a task, a [delay_statement](./AA-9.6#S0266), a call to Yield_To_Higher, and a call to Yield_To_Same_Or_Higher or Yield are the only task dispatching points (see D.2.1). 

Ramification: {AI05-0166-1} A [delay_statement](./AA-9.6#S0266) is always a task dispatching point even if it is not blocking. Similarly, a call to Yield_To_Higher is never blocking, but it is a task dispatching point In each of these cases, they can cause the current task to stop running (it is still ready). Otherwise, the running task continues to run until it is blocked.

{AI12-0299-1} This rule supersedes the Implementation Permission of D.2.1; an implementation that adds additional task dispatching points to this policy is incorrect. 


#### Implementation Requirements

{AI95-00333-01} An implementation shall allow, for a single partition, both the task dispatching policy to be specified as Non_Preemptive_FIFO_Within_Priorities and also the locking policy (see D.3) to be specified as Ceiling_Locking. 

Reason: This is the preferred combination of the Non_Preemptive_FIFO_Within_Priorities policy with a locking policy, and we want that combination to be portable. 


#### Implementation Permissions

{AI95-00298-01} {AI05-0229-1} {AI05-0269-1} Since implementations are allowed to round all ceiling priorities in subrange System.Priority to System.Priority'Last (see D.3), an implementation may allow a task of a partition using the Non_Premptive_FIFO_Within_Priorities policy to execute within a protected object without raising its active priority provided the associated protected unit does not contain any subprograms with aspects Interrupt_Handler or Attach_Handler specified, nor does the unit have aspect Interrupt_Priority  specified. When the locking policy (see D.3) is Ceiling_Locking, an implementation taking advantage of this permission shall ensure that a call to Yield_to_Higher that occurs within a protected action uses the ceiling priority of the protected object (rather than the active priority of the task) when determining whether to preempt the task. 

Reason: {AI05-0269-1} We explicitly require that the ceiling priority be used in calls to Yield_to_Higher in order to prevent a risk of priority inversion and consequent loss of mutual exclusion when Yield_to_Higher is used in a protected object. This requirement might lessen the value of the permission (as the current Ceiling_Priority will have to be maintained in the TCB), but loss of mutual exclusion cannot be tolerated. The primary benefit of the permission (eliminating the need for preemption at the end of a protected action) is still available. As noted above, an implementation-defined locking policy will need to specify the semantics of Yield_to_Higher, including this case. 


#### Extensions to Ada 95

{AI95-00298-01} {AI95-00355-01} Policy Non_Preemptive_FIFO_Within_Priorities is new. 


#### Extensions to Ada 2005

{AI05-0166-1} Package Dispatching.Non_Preemptive is new. 


## D.2.5  Round Robin Dispatching

{AI95-00355-01} {AI05-0299-1} [This subclause defines the task dispatching policy Round_Robin_Within_Priorities and the package Round_Robin.] 


#### Static Semantics

{AI95-00355-01} The policy_[identifier](./AA-2.3#S0002) Round_Robin_Within_Priorities is a task dispatching policy.

{AI95-00355-01} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} with System;
with Ada.Real_Time;
package Ada.Dispatching.Round_Robin
  with Nonblocking, Global =&gt in out synchronized is
  Default_Quantum : constant Ada.Real_Time.Time_Span :=
             implementation-defined;
  procedure Set_Quantum (Pri     : in System.Priority;
                         Quantum : in Ada.Real_Time.Time_Span);
  procedure Set_Quantum (Low, High : in System.Priority;
                         Quantum   : in Ada.Real_Time.Time_Span);
  function Actual_Quantum (Pri : System.Priority)
             return Ada.Real_Time.Time_Span;
  function Is_Round_Robin (Pri : System.Priority) return Boolean;
end Ada.Dispatching.Round_Robin;

```

Implementation defined: The value of Default_Quantum in Dispatching.Round_Robin.

{AI95-00355-01} When task dispatching policy Round_Robin_Within_Priorities is the single policy in effect for a partition, each task with priority in the range of System.Interrupt_Priority is dispatched according to policy FIFO_Within_Priorities.


#### Dynamic Semantics

{AI95-00355-01} The procedures Set_Quantum set the required Quantum value for a single priority level Pri or a range of priority levels Low .. High. If no quantum is set for a Round Robin priority level, Default_Quantum is used.

{AI95-00355-01} The function Actual_Quantum returns the actual quantum used by the implementation for the priority level Pri.

{AI95-00355-01} {AI05-0264-1} The function Is_Round_Robin returns True if priority Pri is covered by task dispatching policy Round_Robin_Within_Priorities; otherwise, it returns False.

{AI95-00355-01} A call of Actual_Quantum or Set_Quantum raises exception Dispatching.Dispatching_Policy_Error if a predefined policy other than Round_Robin_Within_Priorities applies to the specified priority or any of the priorities in the specified range.

{AI95-00355-01} For Round_Robin_Within_Priorities, the dispatching rules for FIFO_Within_Priorities apply with the following additional rules:

When a task is added or moved to the tail of the ready queue for its base priority, it has an execution time budget equal to the quantum for that priority level. This will also occur when a blocked task becomes executable again.

When a task is preempted (by a higher priority task) and is added to the head of the ready queue for its priority level, it retains its remaining budget.

While a task is executing, its budget is decreased by the amount of execution time it uses. The accuracy of this accounting is the same as that for execution time clocks (see D.14). 

Ramification: Note that this happens even when the task is executing at a higher, inherited priority, and even if that higher priority is dispatched by a different policy than round robin. 

When a task has exhausted its budget and is without an inherited priority (and is not executing within a protected operation), it is moved to the tail of the ready queue for its priority level. This is a task dispatching point.

Ramification: In this case, it will be given a budget as described in the first bullet.

The rules for FIFO_Within_Priority (to which these bullets are added) say that a task that has its base priority set to a Round Robin priority is moved to the tail of the ready queue for its new priority level, and then will be given a budget as described in the first bullet. That happens whether or not the task's original base priority was a Round Robin priority. 


#### Implementation Requirements

{AI95-00333-01} {AI95-00355-01} An implementation shall allow, for a single partition, both the task dispatching policy to be specified as Round_Robin_Within_Priorities and also the locking policy (see D.3) to be specified as Ceiling_Locking. 

Reason: This is the preferred combination of the Round_Robin_Within_Priorities policy with a locking policy, and we want that combination to be portable. 


#### Documentation Requirements

{AI95-00355-01} An implementation shall document the quantum values supported. 

Documentation Requirement: The quantum values supported for round robin dispatching.

{AI95-00355-01} An implementation shall document the accuracy with which it detects the exhaustion of the budget of a task. 

Documentation Requirement: The accuracy of the detection of the exhaustion of the budget of a task for round robin dispatching.

NOTE 1   {AI95-00355-01} {AI12-0442-1} Due to implementation constraints, the quantum value returned by Actual_Quantum can differ from that set with Set_Quantum.

NOTE 2   {AI95-00355-01} A task that executes continuously with an inherited priority will not be subject to round robin dispatching.


#### Extensions to Ada 95

{AI95-00355-01} Policy Round_Robin_Within_Priorities and package Dispatching.Round_Robin are new. 


## D.2.6  Earliest Deadline First Dispatching

{AI95-00357-01} {AI12-0439-1} The deadline of a task is an indication of the urgency of the task; it represents a point on an ideal physical time line. The deadline can affect how resources are allocated to the task.

{AI95-00357-01} {AI05-0229-1} {AI05-0299-1} {AI12-0230-1} [This subclause presents Dispatching.EDF, a package for representing the deadline of a task and a dispatching policy that defines Earliest Deadline First (EDF) dispatching. The Relative_Deadline aspect is provided to assign an initial deadline to a task. A configuration pragma Generate_Deadlines is provided to specify that a task's deadline is recomputed whenever it is made ready.]

This paragraph was deleted.{AI05-0229-1} {AI12-0230-1} 


#### Language Design Principles

This paragraph was deleted.{AI95-00357-01} {AI05-0299-1} {AI12-0230-1} 

Paragraphs 3 through 6 were moved to Annex J, "Obsolescent Features". 


#### Static Semantics

{AI95-00357-01} {AI12-0230-1} The policy_[identifier](./AA-2.3#S0002) EDF_Within_Priorities is a task dispatching policy.

{AI95-00357-01} The following language-defined library package exists: 

```ada
{AI12-0230-1} {AI12-0241-1} {AI12-0302-1} with Ada.Real_Time;
with Ada.Task_Identification;
package Ada.Dispatching.EDF
  with Nonblocking, Global =&gt in out synchronized is
  subtype Deadline is Ada.Real_Time.Time;
  subtype Relative_Deadline is Ada.Real_Time.Time_Span;
  Default_Deadline : constant Deadline :=
              Ada.Real_Time.Time_Last;
  Default_Relative_Deadline : constant Relative_Deadline :=
              Ada.Real_Time.Time_Span_Last;
  procedure Set_Deadline
     (D : in Deadline;
      T : in Ada.Task_Identification.Task_Id :=
      Ada.Task_Identification.Current_Task);
  function Get_Deadline
     (T : Ada.Task_Identification.Task_Id :=
      Ada.Task_Identification.Current_Task) return Deadline;
  procedure Set_Relative_Deadline
     (D : in Relative_Deadline;
      T : in Ada.Task_Identification.Task_Id :=
      Ada.Task_Identification.Current_Task);
  function Get_Relative_Deadline
     (T : Ada.Task_Identification.Task_Id :=
      Ada.Task_Identification.Current_Task)
      return Relative_Deadline;
  procedure Delay_Until_And_Set_Deadline
     (Delay_Until_Time : in Ada.Real_Time.Time;
      Deadline_Offset : in Ada.Real_Time.Time_Span)
     with Nonblocking =&gt False;
  function Get_Last_Release_Time
     (T : Ada.Task_Identification.Task_Id :=
      Ada.Task_Identification.Current_Task)
      return Ada.Real_Time.Time;
end Ada.Dispatching.EDF;

```

{AI05-0229-1} {AI12-0230-1} For a subprogram, a task type (including the anonymous type of a [single_task_declaration](./AA-9.1#S0245)), or a protected type (including the anonymous type of a [single_protected_declaration](./AA-9.4#S0250)), the following language-defined representation aspect may be specified:

Relative_DeadlineThe aspect Relative_Deadline is an [expression](./AA-4.4#S0132), which shall be of type Real_Time.Time_Span.

Aspect Description for Relative_Deadline: Task or protected type parameter used in Earliest Deadline First Dispatching.

{AI12-0230-1} The form of [pragma](./AA-2.8#S0019) Generate_Deadlines is as follows:

  pragma Generate_Deadlines;

{AI12-0230-1} The Generate_Deadlines [pragma](./AA-2.8#S0019) is a configuration pragma.


#### Legality Rules

{AI05-0229-1} {AI12-0230-1} The Relative_Deadline aspect shall not be specified on a task or protected interface type. If the Relative_Deadline aspect is specified for a subprogram, the [aspect_definition](./AA-13.1#S0348) shall be a static expression.


#### Post-Compilation Rules

{AI95-00357-01} {AI12-0230-1} If the EDF_Within_Priorities policy is specified for a partition, then the Ceiling_Locking policy (see D.3) shall also be specified for the partition.

{AI95-00357-01} {AI12-0230-1} If the EDF_Within_Priorities policy appears in a Priority_Specific_Dispatching pragma (see D.2.2) in a partition, then the Ceiling_Locking policy (see D.3) shall also be specified for the partition.

Reason: Unlike the other language-defined dispatching policies, the semantic description of EDF_Within_Priorities assumes Ceiling_Locking (and a ceiling priority) in order to make the mapping between deadlines and priorities work. Thus, we require both policies to be specified if EDF is used in the partition. 


#### Dynamic Semantics

{AI95-00357-01} {AI05-0229-1} The Relative_Deadline aspect has no effect if it is specified for a subprogram other than the main subprogram.

{AI12-0230-1} If pragma Generate_Deadlines is in effect, the deadline of a task is recomputed each time it becomes ready. The new deadline is the value of Real_Time.Clock at the time the task is added to a ready queue plus the value returned by Get_Relative_Deadline.

{AI95-00357-01} {AI05-0229-1} {AI12-0230-1} The initial absolute deadline for a task with a specified Relative_Deadline is the result of adding the value returned by a call of Real_Time.Clock to the value of the [expression](./AA-4.4#S0132) specified as the Relative_Deadline aspect, where this entire computation, including the call of Real_Time.Clock, is performed between task creation and the start of its activation. If the aspect Relative_Deadline is not specified, then the initial absolute deadline of a task is the value of Default_Deadline (Ada.Real_Time.Time_Last). The environment task is also given an initial deadline by this rule, using the value of the Relative_Deadline aspect of the main subprogram (if any).

Proof: The environment task is a normal task by 10.2, so of course this rule applies to it. 

{AI12-0230-1} The effect of specifying a Relative_Deadline aspect for a protected type or [single_protected_declaration](./AA-9.4#S0250) is discussed in D.3.

{AI95-00357-01} {AI12-0230-1} A task has both an active and a base absolute deadline. These are the same except when the task is inheriting a relative deadline during activation or a rendezvous (see below) or within a protected action (see D.3). The procedure Set_Deadline changes the (base) absolute deadline of the task to D. The function Get_Deadline returns the (base) absolute deadline of the task.

{AI12-0230-1} The procedure Set_Relative_Deadline changes the relative deadline of the task to D. The function Get_Relative_Deadline returns the relative deadline of the task.

{AI12-0230-1} The function Get_Last_Release_Time returns the time, as provided by Real_Time.Clock, when the task was last made ready (that is, was added to a ready queue).

{AI95-00357-01} {AI12-0230-1} The procedure Delay_Until_And_Set_Deadline delays the calling task until time Delay_Until_Time. When the task becomes ready again it will have deadline Delay_Until_Time + Deadline_Offset.

{AI95-00357-01} {AI12-0230-1} On a system with a single processor, the setting of the deadline of a task to the new value occurs immediately at the first point that is outside the execution of a protected action. If the task is currently on a ready queue it is removed and re-entered onto the ready queue determined by the rules defined below.

{AI95-00357-01} {AI12-0230-1} When EDF_Within_Priorities is specified for a priority, the ready queue for that priority is ordered by deadline. The task at the head of a queue is the one with the earliest deadline.

{AI95-00357-01} {AI12-0230-1} A task dispatching point occurs for the currently running task T to which policy EDF_Within_Priorities applies:

{AI12-0230-1} when a change to the base (absolute) deadline of T occurs;

This paragraph was deleted.{AI12-0230-1} 

{AI12-0230-1} there is a nonempty ready queue for that processor with a higher priority than the active priority of the running task;

{AI12-0230-1} there is a ready task with the same priority as T but with an earlier absolute deadline.

{AI12-0230-1} In these cases, the currently running task is said to be preempted and is returned to the ready queue for its active priority, at a position determined by its active (absolute) deadline.

Paragraphs 23 through 27 were deleted. 

{AI95-00357-01} {AI12-0230-1} When the setting of the base priority of a ready task takes effect and the new priority is specified as EDF_Within_Priorities, the task is added to the ready queue, at a position determined by its active deadline.

{AI95-00357-01} For all the operations defined in Dispatching.EDF, Tasking_Error is raised if the task identified by T has terminated. Program_Error is raised if the value of T is Null_Task_Id.

{AI12-0230-1} If two tasks with priority designated as EDF_Within_Priorities rendezvous then the deadline for the execution of the accept statement is the earlier of the deadlines of the two tasks.

{AI12-0230-1} During activation, a task being activated inherits the deadline that its activator (see 9.2) had at the time the activation was initiated.

Paragraph 30 was deleted. 


#### Erroneous Execution

{AI95-00357-01} If a value of Task_Id is passed as a parameter to any of the subprograms of this package and the corresponding task object no longer exists, the execution of the program is erroneous.


#### Documentation Requirements

{AI95-00357-01} On a multiprocessor, the implementation shall document any conditions that cause the completion of the setting of the deadline of a task to be delayed later than what is specified for a single processor. 

Documentation Requirement: Any conditions that cause the completion of the setting of the deadline of a task to be delayed for a multiprocessor.

NOTE   {AI95-00357-01} {AI05-0264-1} {AI12-0230-1} If two distinct priorities are specified to have policy EDF_Within_Priorities, then tasks from the higher priority always run before tasks of the lower priority, regardless of deadlines.

This paragraph was deleted.{AI95-00357-01} {AI12-0230-1} 

Implementation Note: {AI95-00357-01} An implementation may support additional dispatching policies by replacing absolute deadline with an alternative measure of urgency. 


#### Extensions to Ada 95

{AI95-00357-01} Policy EDF_Across_Priorities and package Dispatching.EDF are new. 


#### Extensions to Ada 2005

{AI05-0229-1} Aspect Relative_Deadline is new; [pragma](./AA-2.8#S0019) Relative_Deadline is now obsolescent. 


#### Wording Changes from Ada 2005

{AI05-0055-1} Correction: Corrected definition of active priority to avoid deadline inversion in an unusual case. 


#### Incompatibilities With Ada 2012

{AI12-0230-1} The policy EDF_Across_Priorities was replaced by EDF_Within_Priorities. A program using EDF_Across_Priorities could fail to compile. However, we not are aware of any implementations of EDF_Across_Priorities, so it seems unlikely that any such programs exist outside of books and papers. 

