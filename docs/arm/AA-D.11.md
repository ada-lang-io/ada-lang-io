---
sidebar_position:  162
---

# D.11  Asynchronous Task Control

{AI05-0299-1} [This subclause introduces a language-defined package to do asynchronous suspend/resume on tasks. It uses a conceptual held priority value to represent the task's held state.] 


#### Static Semantics

The following language-defined library package exists: 

```ada
{AI95-00362-01} {AI12-0241-1} {AI12-0302-1} with Ada.Task_Identification;
package Ada.Asynchronous_Task_Control
  with Preelaborate, Nonblocking, Global =&gt in out synchronized is
  procedure Hold(T : in Ada.Task_Identification.Task_Id);
  procedure Continue(T : in Ada.Task_Identification.Task_Id);
  function Is_Held(T : Ada.Task_Identification.Task_Id)
   return Boolean;
end Ada.Asynchronous_Task_Control;

```


#### Dynamic Semantics

{AI95-00357-01} After the Hold operation has been applied to a task, the task becomes held. For each processor there is a conceptual idle task, which is always ready. The base priority of the idle task is below System.Any_Priority'First. The held priority is a constant of the type Integer whose value is below the base priority of the idle task. 

Discussion: The held state should not be confused with the blocked state as defined in 9.2; the task is still ready. 

{AI95-00357-01} For any priority below System.Any_Priority'First, the task dispatching policy is FIFO_Within_Priorities. 

To be honest: This applies even if a Task_Dispatching_Policy specifies the policy for all of the priorities of the partition. 

Ramification: A task at the held priority never runs, so it is not necessary to implement FIFO_Within_Priorities for systems that have only one policy (such as EDF_Across_Priorities). 

{AI95-00357-01} The Hold operation sets the state of T to held. For a held task, the active priority is reevaluated as if the base priority of the task were the held priority. 

Ramification: For example, if T is currently inheriting priorities from other sources (e.g. it is executing in a protected action), its active priority does not change, and it continues to execute until it leaves the protected action. 

{AI95-00357-01} The Continue operation resets the state of T to not-held; its active priority is then reevaluated as determined by the task dispatching policy associated with its base priority.

The Is_Held function returns True if and only if T is in the held state. 

Discussion: Note that the state of T can be changed immediately after Is_Held returns. 

As part of these operations, a check is made that the task identified by T is not terminated. Tasking_Error is raised if the check fails. Program_Error is raised if the value of T is Null_Task_Id.


#### Erroneous Execution

If any operation in this package is called with a parameter T that specifies a task object that no longer exists, the execution of the program is erroneous. 


#### Implementation Permissions

{AI12-0444-1} An implementation may omit support for Asynchronous_Task_Control if it is infeasible to support it in the target environment. 

Reason: A direct implementation of the Asynchronous_Task_Control semantics using priorities is not necessarily efficient enough. Thus, we envision implementations that use some other mechanism to set the "held" state. If there is no other such mechanism, support for Asynchronous_Task_Control might be infeasible, because an implementation in terms of priority would require one idle task per processor. On some systems, programs are not supposed to know how many processors are available, so creating enough idle tasks would be problematic. 

NOTE 1   It is a consequence of the priority rules that held tasks cannot be dispatched on any processor in a partition (unless they are inheriting priorities) since their priorities are defined to be below the priority of any idle task.

NOTE 2   The effect of calling Get_Priority and Set_Priority on a Held task is the same as on any other task.

NOTE 3   Calling Hold on a held task or Continue on a non-held task has no effect.

NOTE 4   The rules affecting queuing are derived from the above rules, in addition to the normal priority rules: 

When a held task is on the ready queue, its priority is so low as to never reach the top of the queue as long as there are other tasks on that queue.

If a task is executing in a protected action, inside a rendezvous, or is inheriting priorities from other sources (e.g. when activated), it continues to execute until it is no longer executing the corresponding construct.

If a task becomes held while waiting (as a caller) for a rendezvous to complete, the active priority of the accepting task is not affected.

{8652/0077} {AI95-00111-01} If a task becomes held while waiting in a [selective_accept](./AA-9.7#S0270), and an entry call is issued to one of the open entries, the corresponding [accept_alternative](./AA-9.7#S0273) executes. When the rendezvous completes, the active priority of the accepting task is lowered to the held priority (unless it is still inheriting from other sources), and the task does not execute until another Continue.

The same holds if the held task is the only task on a protected entry queue whose barrier becomes open. The corresponding entry body executes.


#### Extensions to Ada 95

{AI95-00362-01} Asynchronous_Task_Control is now Preelaborated, so it can be used in preelaborated units. 


#### Wording Changes from Ada 95

{8652/0077} {AI95-00111-01} Corrigendum: Corrected to eliminate the use of the undefined term "accept body".

{AI95-00357-01} The description of held tasks was changed to reflect that the calculation of active priorities depends on the dispatching policy of the base priority. Thus, the policy of the held priority was specified in order to avoid surprises (especially when using the EDF policy). 

