---
sidebar_position:  155
---

# D.4  Entry Queuing Policies

{8652/0074} {AI95-00068-01} {AI05-0299-1} {AI12-0324-1} [ This subclause specifies a mechanism for a user to choose an entry queuing policy. It also defines three such policies. Other policies are implementation defined.] 

Implementation defined: Implementation-defined queuing policies.


#### Syntax

The form of a [pragma](./AA-2.8#S0019) Queuing_Policy is as follows: 

  pragma Queuing_Policy(policy_[identifier](./AA-2.3#S0002)); 


#### Legality Rules

{AI12-0183-1} The policy_[identifier](./AA-2.3#S0002) shall be either FIFO_Queuing, Ordered_FIFO_Queuing, Priority_Queuing or an implementation-defined [identifier](./AA-2.3#S0002).


#### Post-Compilation Rules

A Queuing_Policy pragma is a configuration pragma.


#### Dynamic Semantics

[A queuing policy governs the order in which tasks are queued for entry service, and the order in which different entry queues are considered for service.] The queuing policy is specified by a Queuing_Policy pragma. 

Ramification: The queuing policy includes entry queuing order, the choice among open alternatives of a [selective_accept](./AA-9.7#S0270), and the choice among queued entry calls of a protected object when more than one [entry_barrier](./AA-9.5#S0262) [condition](./AA-4.5#S0150) is True. 

{AI95-00355-01} {AI12-0163-1} {AI12-0183-1} Three queuing policies, FIFO_Queuing, Ordered_FIFO_Queuing, and Priority_Queuing, are language defined. If no Queuing_Policy pragma applies to any of the program units comprising the partition, the queuing policy for that partition is FIFO_Queuing. The rules for the FIFO_Queuing policy are specified in 9.5.3 and 9.7.1.

{AI12-0163-1} The Ordered_FIFO_Queuing policy is defined as follows:

Calls are selected on a given entry queue in order of arrival.

When more than one condition of an [entry_barrier](./AA-9.5#S0262) of a protected object becomes True, and more than one of the respective queues is nonempty, the call that arrived first is selected.

If the expiration time of two or more open [delay_alternative](./AA-9.7#S0274)s is the same and no other [accept_alternative](./AA-9.7#S0273)s are open, the [sequence_of_statements](./AA-5.1#S0166) of the [delay_alternative](./AA-9.7#S0274) that is first in textual order in the [selective_accept](./AA-9.7#S0270) is executed.

When more than one alternative of a [selective_accept](./AA-9.7#S0270) is open and has queued calls, the alternative whose queue has the call that arrived first is selected. 

Implementation Note: A possible implementation for this policy would be to assign a sequence number to each queued entry call, where the sequence number is incremented globally across all queues associated with the protected object or [selective_accept](./AA-9.7#S0270). 

Reason: It would have been marginally easier to use textual order for the case when multiple queues are selectable. But textual order can lead to unfair queue servicing, since the queues in earlier textual order will end up starving the later ones if calls arrive fast enough. Applying arrival first as the selector provides fairness for this policy. 

The Priority_Queuing policy is defined as follows:

The calls to an entry [(including a member of an entry family)] are queued in an order consistent with the priorities of the calls. The priority of an entry call is initialized from the active priority of the calling task at the time the call is made, but can change later. Within the same priority, the order is consistent with the calling (or requeuing, or priority setting) time (that is, a FIFO order).

{8652/0075} {AI95-00205-01} After a call is first queued, changes to the active priority of a task do not affect the priority of the call, unless the base priority of the task is set while the task is blocked on an entry call.

When the base priority of a task is set (see D.5), if the task is blocked on an entry call, and the call is queued, the priority of the call is updated to the new active priority of the calling task. This causes the call to be removed from and then reinserted in the queue at the new active priority. 

Reason: A task is blocked on an entry call if the entry call is simple, conditional, or timed. If the call came from the [triggering_statement](./AA-9.7#S0282) of an [asynchronous_select](./AA-9.7#S0280), or a requeue thereof, then the task is not blocked on that call; such calls do not have their priority updated. Thus, there can exist many queued calls from a given task (caused by many nested ATC's), but a task can be blocked on only one call at a time.

A previous version of Ada 9X required queue reordering in the [asynchronous_select](./AA-9.7#S0280) case as well. If the call corresponds to a "synchronous" entry call, then the task is blocked while queued, and it makes good sense to move it up in the queue if its priority is raised.

However, if the entry call is "asynchronous", that is, it is due to an [asynchronous_select](./AA-9.7#S0280) whose [triggering_statement](./AA-9.7#S0282) is an entry call, then the task is not waiting for this entry call, so the placement of the entry call on the queue is irrelevant to the rate at which the task proceeds.

Furthermore, when an entry is used for [asynchronous_select](./AA-9.7#S0280)s, it is almost certain to be a "broadcast" entry or have only one caller at a time. For example, if the entry is used to notify tasks of a mode switch, then all tasks on the entry queue would be signaled when the mode changes. Similarly, if it is indicating some interrupting event such as a control-C, all tasks sensitive to the interrupt will want to be informed that the event occurred. Hence, the order on such a queue is essentially irrelevant.

Given the above, it seems an unnecessary semantic and implementation complexity to specify that asynchronous queued calls are moved in response to dynamic priority changes. Furthermore, it is somewhat inconsistent, since the call was originally queued based on the active priority of the task, but dynamic priority changes are changing the base priority of the task, and only indirectly the active priority. We say explicitly that asynchronous queued calls are not affected by normal changes in active priority during the execution of an [abortable_part](./AA-9.7#S0283). Saying that, if a change in the base priority affects the active priority, then we do want the calls reordered, would be inconsistent. It would also require the implementation to maintain a readily accessible list of all queued calls which would not otherwise be necessary.

Several rules were removed or simplified when we changed the rules so that calls due to [asynchronous_select](./AA-9.7#S0280)s are never moved due to intervening changes in active priority, be they due to protected actions, some other priority inheritance, or changes in the base priority. 

When more than one [condition](./AA-4.5#S0150) of an [entry_barrier](./AA-9.5#S0262) of a protected object becomes True, and more than one of the respective queues is nonempty, the call with the highest priority is selected. If more than one such call has the same priority, the call that is queued on the entry whose declaration is first in textual order in the [protected_definition](./AA-9.4#S0251) is selected. For members of the same entry family, the one with the lower family index is selected.

If the expiration time of two or more open [delay_alternative](./AA-9.7#S0274)s is the same and no other [accept_alternative](./AA-9.7#S0273)s are open, the [sequence_of_statements](./AA-5.1#S0166) of the [delay_alternative](./AA-9.7#S0274) that is first in textual order in the [selective_accept](./AA-9.7#S0270) is executed.

When more than one alternative of a [selective_accept](./AA-9.7#S0270) is open and has queued calls, an alternative whose queue has the highest-priority call at its head is selected. If two or more open alternatives have equal-priority queued calls, then a call on the entry in the [accept_alternative](./AA-9.7#S0273) that is first in textual order in the [selective_accept](./AA-9.7#S0270) is selected.


#### Implementation Permissions

{AI95-00256-01} {AI12-0444-1} Implementations are allowed to define other queuing policies, but are not required to support specifying more than one queuing policy per partition. 

Discussion: {8652/0116} {AI95-00069-01} {AI95-00256-01} This rule is really redundant, as 10.1.5 allows an implementation to limit the use of configuration pragmas to an empty environment. In that case, there would be no way to have multiple policies in a partition. 

{AI95-00188-02} Implementations are allowed to defer the reordering of entry queues following a change of base priority of a task blocked on the entry call if it is not practical to reorder the queue immediately. 

Reason: Priority change is immediate, but the effect of the change on entry queues can be deferred. That is necessary in order to implement priority changes on top of a non-Ada kernel. 

Discussion: The reordering should occur as soon as the blocked task can itself perform the reinsertion into the entry queue. 


#### Implementation Advice

The implementation should use names that end with "_Queuing" for implementation-defined queuing policies.

Implementation Advice: Names that end with "_Queuing" should be used for implementation-defined queuing policies.


#### Static Semantics

{AI12-0164-1} For a task type (including the anonymous type of a [single_task_declaration](./AA-9.1#S0245)), protected type (including the anonymous type of a [single_protected_declaration](./AA-9.4#S0250)), or an [entry_declaration](./AA-9.5#S0257), the following language-defined representation aspect may be specified:

Max_Entry_Queue_LengthThe type of aspect Max_Entry_Queue_Length is Integer.

Aspect Description for Max_Entry_Queue_Length: The maximum entry queue length for a task type, protected type, or entry.

If directly specified, the aspect_definition shall be a static expression no less than -1. If not specified, the aspect has value -1 (representing no additional restriction on queue length). 


#### Legality Rules

{AI12-0164-1} {AI12-0388-1} If the Max_Entry_Queue_Length aspect for a type has a nonnegative value, the Max_Entry_Queue_Length aspect for every individual entry of that type shall not be greater than the value of the aspect for the type. The Max_Entry_Queue_Length aspect of a type is nonoverridable (see 13.1.1).

Ramification: Aspect Max_Entry_Queue_Length can specify less than the partition-wide or type-wide default, but it can't expand the length of a queue. 


#### Post-Compilation Rules

{AI12-0164-1} If a restriction Max_Entry_Queue_Length applies to a partition, any value specified for the Max_Entry_Queue_Length aspect specified for the declaration of a type or entry in the partition shall not be greater than the value of the restriction.

Ramification: 13.12(6) says that the restriction value has to be static, so this is statically checkable. But the restriction does not have to be in the same compilation as the aspect, so the check cannot, in general, be done until link time. 


#### Dynamic Semantics

{AI12-0164-1} If a nonconfirming value is specified for Max_Entry_Queue_Length for a type, and an entry call or requeue would cause the queue for any entry of the type to become longer than the specified value, then Program_Error is raised at the point of the call or requeue.

{AI12-0164-1} If a nonconfirming value is specified for Max_Entry_Queue_Length for an entry, and an entry call or requeue would cause the queue for an entry to become longer than the specified value, then Program_Error is raised at the point of the call or requeue. 


#### Wording Changes from Ada 95

{8652/0074} {AI95-00068-01} Corrigendum: Corrected the number of queuing policies defined.

{8652/0075} {AI95-00205-01} Corrigendum: Corrected so that a call of Set_Priority in an abortable part does not change the priority of the triggering entry call.

{AI95-00188-02} Added a permission to defer queue reordering when the base priority of a task is changed. This is a counterpart to stronger requirements on the implementation of priority change.

{AI95-00256-01} Clarified that an implementation need support only one queuing policy (of any kind, language-defined or otherwise) per partition.

{AI95-00355-01} Fixed wording to make clear that [pragma](./AA-2.8#S0019) never appears inside of a unit; rather it "applies to" the unit. 


#### Extensions to Ada 2012

{AI12-0163-1} Defined the new queuing policy Ordered_FIFO_Queuing.

{AI12-0164-1} Defined the new aspect Max_Enty_Queue_Length. 


## D.4.1  Admission Policies

{AI12-0276-1} [This subclause specifies a mechanism for a user to choose an admission policy. It also defines one such policy. Other policies are implementation defined.]

Implementation defined: Implementation-defined admission policies.


#### Syntax

{AI12-0276-1} The form of a [pragma](./AA-2.8#S0019) Admission_Policy is as follows:

  pragma Admission_Policy (policy_[identifier](./AA-2.3#S0002)); 


#### Legality Rules

{AI12-0276-1} The policy_[identifier](./AA-2.3#S0002) shall be either FIFO_Spinning or an implementation-defined identifier. 


#### Post-Compilation Rules

{AI12-0276-1} An Admission_Policy pragma is a configuration pragma.


#### Dynamic Semantics

{AI12-0276-1} An admission policy governs the order in which competing tasks are evaluated for acquiring the execution resource associated with a protected object. The admission policy is specified by an Admission_Policy pragma.

{AI12-0276-1} One admission policy, FIFO_Spinning, is language defined. If FIFO_Spinning is in effect, and starting a protected action on a protected object involves busy-waiting, then calls are selected for acquiring the execution resource of the protected object in the order in which the busy-wait was initiated; otherwise the FIFO_Spinning policy has no effect. If no Admission_Policy pragma applies to any of the program units in the partition, the admission policy for that partition is implementation defined.

Discussion: {AI12-0005-1} Busy-waiting might be used for protected objects that can be called from tasks running on other processors than the one the protected object is on. It is unnecessary if all of the tasks that can call a protected object are on the same processor as the object; in particular, it would not be used on a monoprocessor. Aspect CPU (see D.16) can be used to ensure that busy-waiting is not needed. 

Implementation Note: A possible implementation for this policy would be to apply the abstraction of a ticketing system by assigning two sequence number values to each protected object. One sequence number represents the next available ticket number, and the other sequence number represents the ticket number currently being serviced by the protected object. The next available ticket number is incremented and assigned to a task when the task initiates a busy-wait for acquiring the execution resource associated with the protected object. The ticket number currently being serviced is incremented when a task releases this execution resource. As part of acquiring the execution resource, a task busy-waits until its assigned ticket number equals the protected object's value for the ticket number currently being serviced. While a task busy-waits, it monitors the active priority of the protected object in order to inherit any modifications to the protected object's active priority. 


#### Implementation Permissions

{AI12-0276-1} {AI12-0444-1} Implementations are allowed to define other admission policies, but are not required to support specifying more than one admission policy per partition.

Discussion: This rule is in fact redundant, as 10.1.5 allows an implementation to limit the use of configuration pragmas to an empty environment. In that case, there would be no way to have multiple policies in a partition. 


#### Extensions to Ada 2012

{AI12-0276-1} Admission Policies and the specific policy FIFO_Spinning are new. 

