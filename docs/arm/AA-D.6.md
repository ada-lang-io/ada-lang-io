---
sidebar_position:  157
---

# D.6  Preemptive Abort

{AI05-0299-1} [This subclause specifies requirements on the immediacy with which an aborted construct is completed.] 


#### Dynamic Semantics

On a system with a single processor, an aborted construct is completed immediately at the first point that is outside the execution of an abort-deferred operation.


#### Documentation Requirements

On a multiprocessor, the implementation shall document any conditions that cause the completion of an aborted construct to be delayed later than what is specified for a single processor. 

This paragraph was deleted.

Documentation Requirement: On a multiprocessor, any conditions that cause the completion of an aborted construct to be delayed later than what is specified for a single processor.


#### Metrics

The implementation shall document the following metrics: 

The execution time, in processor clock cycles, that it takes for an [abort_statement](./AA-9.8#S0284) to cause the completion of the aborted task. This is measured in a situation where a task T2 preempts task T1 and aborts T1. T1 does not have any finalization code. T2 shall verify that T1 has terminated, by means of the Terminated attribute.

On a multiprocessor, an upper bound in seconds, on the time that the completion of an aborted task can be delayed beyond the point that it is required for a single processor.

{AI95-00114-01} An upper bound on the execution time of an [asynchronous_select](./AA-9.7#S0280), in processor clock cycles. This is measured between a point immediately before a task T1 executes a protected operation Pr.Set that makes the [condition](./AA-4.5#S0150) of an [entry_barrier](./AA-9.5#S0262) Pr.Wait True, and the point where task T2 resumes execution immediately after an entry call to Pr.Wait in an [asynchronous_select](./AA-9.7#S0280). T1 preempts T2 while T2 is executing the abortable part, and then blocks itself so that T2 can execute. The execution time of T1 is measured separately, and subtracted.

An upper bound on the execution time of an [asynchronous_select](./AA-9.7#S0280), in the case that no asynchronous transfer of control takes place. This is measured between a point immediately before a task executes the [asynchronous_select](./AA-9.7#S0280) with a nonnull abortable part, and the point where the task continues execution immediately after it. The execution time of the abortable part is subtracted. 

Documentation Requirement: The metrics for aborts.


#### Implementation Advice

Even though the [abort_statement](./AA-9.8#S0284) is included in the list of potentially blocking operations (see 9.5.1), it is recommended that this statement be implemented in a way that never requires the task executing the [abort_statement](./AA-9.8#S0284) to block.

Implementation Advice: The [abort_statement](./AA-9.8#S0284) should not require the task executing the statement to block.

On a multi-processor, the delay associated with aborting a task on another processor should be bounded; the implementation should use periodic polling, if necessary, to achieve this.

Implementation Advice: On a multi-processor, the delay associated with aborting a task on another processor should be bounded.

NOTE 1   Abortion does not change the active or base priority of the aborted task.

NOTE 2   Abortion cannot be more immediate than is allowed by the rules for deferral of abortion during finalization and in protected actions.

