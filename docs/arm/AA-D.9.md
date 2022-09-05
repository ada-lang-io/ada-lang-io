---
sidebar_position:  160
---

# D.9  Delay Accuracy

{AI05-0299-1} [This subclause specifies performance requirements for the [delay_statement](./AA-9.6#S0266). The rules apply both to [delay_relative_statement](./AA-9.6#S0268) and to [delay_until_statement](./AA-9.6#S0267). Similarly, they apply equally to a simple [delay_statement](./AA-9.6#S0266) and to one which appears in a [delay_alternative](./AA-9.7#S0274).] 


#### Dynamic Semantics

The effect of the [delay_statement](./AA-9.6#S0266) for Real_Time.Time is defined in terms of Real_Time.Clock: 

If C1 is a value of Clock read before a task executes a [delay_relative_statement](./AA-9.6#S0268) with duration D, and C2 is a value of Clock read after the task resumes execution following that [delay_statement](./AA-9.6#S0266), then C2  C1 &gt= D.

If C is a value of Clock read after a task resumes execution following a [delay_until_statement](./AA-9.6#S0267) with Real_Time.Time value T, then C &gt= T. 

A simple [delay_statement](./AA-9.6#S0266) with a negative or zero value for the expiration time does not cause the calling task to be blocked; it is nevertheless a potentially blocking operation (see 9.5.1).

{AI05-0004-1} When a [delay_statement](./AA-9.6#S0266) appears in a [delay_alternative](./AA-9.7#S0274) of a [timed_entry_call](./AA-9.7#S0276) the selection of the entry call is attempted, regardless of the specified expiration time. When a [delay_statement](./AA-9.6#S0266) appears in a [select_alternative](./AA-9.7#S0272), and a call is queued on one of the open entries, the selection of that entry call proceeds, regardless of the value of the delay expression. 

Ramification: The effect of these requirements is that one has to always attempt a rendezvous, regardless of the value of the delay expression. This can be tested by issuing a [timed_entry_call](./AA-9.7#S0276) with an expiration time of zero, to an open entry. 


#### Documentation Requirements

The implementation shall document the minimum value of the delay expression of a [delay_relative_statement](./AA-9.6#S0268) that causes the task to actually be blocked. 

Documentation Requirement: The minimum value of the delay expression of a [delay_relative_statement](./AA-9.6#S0268) that causes a task to actually be blocked.

The implementation shall document the minimum difference between the value of the delay expression of a [delay_until_statement](./AA-9.6#S0267) and the value of Real_Time.Clock, that causes the task to actually be blocked. 

This paragraph was deleted.

Documentation Requirement: The minimum difference between the value of the delay expression of a [delay_until_statement](./AA-9.6#S0267) and the value of Real_Time.Clock, that causes the task to actually be blocked.


#### Metrics

The implementation shall document the following metrics: 

An upper bound on the execution time, in processor clock cycles, of a [delay_relative_statement](./AA-9.6#S0268) whose requested value of the delay expression is less than or equal to zero.

An upper bound on the execution time, in processor clock cycles, of a [delay_until_statement](./AA-9.6#S0267) whose requested value of the delay expression is less than or equal to the value of Real_Time.Clock at the time of executing the statement. Similarly, for Calendar.Clock.

{AI12-0445-1} An upper bound on the lateness of a [delay_relative_statement](./AA-9.6#S0268), for a positive value of the delay expression, in a situation where the task has sufficient priority to preempt the processor as soon as it becomes ready, and can proceed without waiting for any other execution resources. The upper bound is expressed as a function of the value of the delay expression. The lateness is obtained by subtracting the value of the delay expression from the actual duration. The actual duration is measured from a point immediately before a task executes the [delay_statement](./AA-9.6#S0266) to a point immediately after the task resumes execution following this statement.

{AI12-0445-1} An upper bound on the lateness of a [delay_until_statement](./AA-9.6#S0267), in a situation where the value of the requested expiration time is after the time the task begins executing the statement, the task has sufficient priority to preempt the processor as soon as it becomes ready, and it can proceed without waiting for any other execution resources. The upper bound is expressed as a function of the difference between the requested expiration time and the clock value at the time the statement begins execution. The lateness of a [delay_until_statement](./AA-9.6#S0267) is obtained by subtracting the requested expiration time from the real time that the task resumes execution following this statement. 

Documentation Requirement: The metrics for delay statements.


#### Wording Changes from Ada 83

The rules regarding a [timed_entry_call](./AA-9.7#S0276) with a very small positive Duration value, have been tightened to always require the check whether the rendezvous is immediately possible.


#### Wording Changes from Ada 95

{AI95-00355-01} The note about "voluntary round-robin', while still true, has been deleted as potentially confusing as it is describing a different kind of round-robin than is defined by the round-robin dispatching policy.

