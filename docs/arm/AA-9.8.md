---
sidebar_position:  79
---

# 9.8  Abort of a Task - Abort of a Sequence of Statements

[An [abort_statement](./AA-9.8#S0284) causes one or more tasks to become abnormal, thus preventing any further interaction with such tasks. The completion of the [triggering_statement](./AA-9.7#S0282) of an [asynchronous_select](./AA-9.7#S0280) causes a [sequence_of_statements](./AA-5.1#S0166) to be aborted.] 


#### Syntax

abort_statement<a id="S0284"></a> ::= abort task_[name](./AA-4.1#S0091) {, task_[name](./AA-4.1#S0091)};


#### Name Resolution Rules

{AI12-0444-1} Each task_[name](./AA-4.1#S0091) is expected to be of any task type[; each can be of a different task type.]


#### Dynamic Semantics

For the execution of an [abort_statement](./AA-9.8#S0284), the given task_[name](./AA-4.1#S0091)s are evaluated in an arbitrary order. Each named task is then aborted, which consists of making the task abnormal and aborting the execution of the corresponding [task_body](./AA-9.1#S0248), unless it is already completed. 

Ramification: {AI95-00114-01} Note that aborting those tasks is not defined to be an abort-deferred operation. Therefore, if one of the named tasks is the task executing the [abort_statement](./AA-9.8#S0284), or if the task executing the [abort_statement](./AA-9.8#S0284) depends on one of the named tasks, then it is possible for the execution of the [abort_statement](./AA-9.8#S0284) to be aborted, thus leaving some of the tasks unaborted. This allows the implementation to use either a sequence of calls to an "abort task" run-time system primitive, or a single call to an "abort list of tasks" run-time system primitive. 

When the execution of a construct is aborted (including that of a [task_body](./AA-9.1#S0248) or of a [sequence_of_statements](./AA-5.1#S0166)), the execution of every construct included within the aborted execution is also aborted, except for executions included within the execution of an abort-deferred operation; the execution of an abort-deferred operation continues to completion without being affected by the abort; the following are the abort-deferred operations: 

a protected action;

waiting for an entry call to complete (after having initiated the attempt to cancel it - see below);

waiting for the termination of dependent tasks;

the execution of an Initialize procedure as the last step of the default initialization of a controlled object;

the execution of a Finalize procedure as part of the finalization of a controlled object;

an assignment operation to an object with a controlled part. 

[The last three of these are discussed further in 7.6.] 

Reason: Deferring abort during Initialize and finalization allows, for example, the result of an allocator performed in an Initialize operation to be assigned into an access object without being interrupted in the middle, which would cause storage leaks. For an object with several controlled parts, each individual Initialize is abort-deferred. Note that there is generally no semantic difference between making each Finalize abort-deferred, versus making a group of them abort-deferred, because if the task gets aborted, the first thing it will do is complete any remaining finalizations. Individual objects are finalized prior to an assignment operation (if nonlimited controlled) and as part of Unchecked_Deallocation. 

Ramification: Abort is deferred during the entire assignment operation to an object with a controlled part, even if only some subcomponents are controlled. Note that this says "assignment operation", not "[assignment_statement](./AA-5.2#S0173)". Explicit calls to Initialize, Finalize, or Adjust are not abort-deferred. 

When a master is aborted, all tasks that depend on that master are aborted.

The order in which tasks become abnormal as the result of an [abort_statement](./AA-9.8#S0284) or the abort of a [sequence_of_statements](./AA-5.1#S0166) is not specified by the language.

If the execution of an entry call is aborted, an immediate attempt is made to cancel the entry call (see 9.5.3). If the execution of a construct is aborted at a time when the execution is blocked, other than for an entry call, at a point that is outside the execution of an abort-deferred operation, then the execution of the construct completes immediately. For an abort due to an [abort_statement](./AA-9.8#S0284), these immediate effects occur before the execution of the [abort_statement](./AA-9.8#S0284) completes. Other than for these immediate cases, the execution of a construct that is aborted does not necessarily complete before the [abort_statement](./AA-9.8#S0284) completes. However, the execution of the aborted construct completes no later than its next abort completion point (if any) that occurs outside of an abort-deferred operation; the following are abort completion points for an execution: 

the point where the execution initiates the activation of another task;

the end of the activation of a task;

{AI12-0119-1} a point within a parallel construct where a new logical thread of control is created;

{AI12-0119-1} the end of a parallel construct;

the start or end of the execution of an entry call, [accept_statement](./AA-9.5#S0258), [delay_statement](./AA-9.6#S0266), or [abort_statement](./AA-9.8#S0284); 

Ramification: Although the abort completion point doesn't occur until the end of the entry call or [delay_statement](./AA-9.6#S0266), these operations might be cut short because an abort attempts to cancel them. 

the start of the execution of a [select_statement](./AA-9.7#S0269), or of the [sequence_of_statements](./AA-5.1#S0166) of an [exception_handler](./AA-11.2#S0305). 

Reason: The start of an [exception_handler](./AA-11.2#S0305) is considered an abort completion point simply because it is easy for an implementation to check at such points. 

Implementation Note: Implementations may of course check for abort more often than at each abort completion point; ideally, a fully preemptive implementation of abort will be provided. If preemptive abort is not supported in a given environment, then supporting the checking for abort as part of subprogram calls and loop iterations might be a useful option. 


#### Bounded (Run-Time) Errors

{AI05-0264-1} {AI12-0445-1} An attempt to execute an [asynchronous_select](./AA-9.7#S0280) as part of the execution of an abort-deferred operation is a bounded error. Similarly, an attempt to create a task that depends on a master that is included entirely within the execution of an abort-deferred operation is a bounded error. In both cases, Program_Error is raised if the error is detected by the implementation; otherwise, the operations proceed as they would outside an abort-deferred operation, except that an abort of the [abortable_part](./AA-9.7#S0283) or the created task does not necessarily have an effect. 

Reason: An [asynchronous_select](./AA-9.7#S0280) relies on an abort of the [abortable_part](./AA-9.7#S0283) to effect the asynchronous transfer of control. For an [asynchronous_select](./AA-9.7#S0280) within an abort-deferred operation, the abort might have no effect.

Creating a task dependent on a master included within an abort-deferred operation is considered an error, because such tasks could be aborted while the abort-deferred operation was still progressing, undermining the purpose of abort-deferral. Alternatively, we could say that such tasks are abort-deferred for their entire execution, but that seems too easy to abuse. Note that task creation is already a bounded error in protected actions, so this additional rule only applies to local task creation as part of Initialize, Finalize, or Adjust. 


#### Erroneous Execution

If an assignment operation completes prematurely due to an abort, the assignment is said to be disrupted; the target of the assignment or its parts can become abnormal, and certain subsequent uses of the object can be erroneous, as explained in 13.9.1. 

NOTE 1   {AI12-0442-1} An [abort_statement](./AA-9.8#S0284) is best used only in situations requiring unconditional termination.

NOTE 2   A task is allowed to abort any task it can name, including itself.

NOTE 3   Additional requirements associated with abort are given in D.6, "Preemptive Abort". 


#### Wording Changes from Ada 83

{AI05-0299-1} This subclause has been rewritten to accommodate the concept of aborting the execution of a construct, rather than just of a task. 


#### Wording Changes from Ada 2012

{AI12-0119-1} Added points within parallel constructs to the list of abort completion points. 

