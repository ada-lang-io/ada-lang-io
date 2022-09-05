---
sidebar_position:  188
---

# H.6  Pragma Partition_Elaboration_Policy

{AI95-00265-01} {AI05-0299-1} This subclause defines a [pragma](./AA-2.8#S0019) for user control over elaboration policy. 


#### Syntax

{AI95-00265-01} The form of a [pragma](./AA-2.8#S0019) Partition_Elaboration_Policy is as follows: 

  pragma Partition_Elaboration_Policy (policy_[identifier](./AA-2.3#S0002));

The policy_[identifier](./AA-2.3#S0002) shall be either Sequential, Concurrent or an implementation-defined identifier. 

Implementation defined: Implementation-defined policy_[identifier](./AA-2.3#S0002)s allowed in a [pragma](./AA-2.8#S0019) Partition_Elaboration_Policy.

Ramification: Note that the Ravenscar profile (see D.13) has nothing to say about which Partition_Elaboration_Policy is used. This was intentionally omitted from the profile, as there was no agreement as to whether the Sequential policy should be required for Ravenscar programs. As such it was defined separately. 


#### Post-Compilation Rules

{AI95-00265-01} A [pragma](./AA-2.8#S0019) Partition_Elaboration_Policy is a configuration pragma. It specifies the elaboration policy for a partition. At most one elaboration policy shall be specified for a partition.

{AI95-00265-01} {AI05-0264-1} If the Sequential policy is specified for a partition, then pragma Restrictions (No_Task_Hierarchy) shall also be specified for the partition. 


#### Dynamic Semantics

{AI95-00265-01} Notwithstanding what this document says elsewhere, this [pragma](./AA-2.8#S0019) allows partition elaboration rules concerning task activation and interrupt attachment to be changed. If the policy_[identifier](./AA-2.3#S0002) is Concurrent, or if there is no pragma Partition_Elaboration_Policy defined for the partition, then the rules defined elsewhere in this Reference Manual apply.

{AI95-00265-01} {AI95-00421-01} If the partition elaboration policy is Sequential, then task activation and interrupt attachment are performed in the following sequence of steps:

The activation of all library-level tasks and the attachment of interrupt handlers are deferred until all library units are elaborated.

The interrupt handlers are attached by the environment task.

The environment task is suspended while the library-level tasks are activated.

The environment task executes the main subprogram (if any) concurrently with these executing tasks. 

{AI95-00265-01} {AI95-00421-01} If several dynamic interrupt handler attachments for the same interrupt are deferred, then the most recent call of Attach_Handler or Exchange_Handler determines which handler is attached.

{AI95-00265-01} {AI95-00421-01} If any deferred task activation fails, Tasking_Error is raised at the beginning of the sequence of statements of the body of the environment task prior to calling the main subprogram.


#### Implementation Advice

{AI95-00265-01} {AI05-0264-1} If the partition elaboration policy is Sequential and the Environment task becomes permanently blocked during elaboration, then the partition is deadlocked and it is recommended that the partition be immediately terminated. 

Implementation Advice: If the partition elaboration policy is Sequential and the Environment task becomes permanently blocked during elaboration, then the partition should be immediately terminated.


#### Implementation Permissions

{AI95-00265-01} {AI05-0264-1} If the partition elaboration policy is Sequential and any task activation fails, then an implementation may immediately terminate the active partition to mitigate the hazard posed by continuing to execute with a subset of the tasks being active. 

NOTE 1   {AI95-00421-01} {AI12-0440-1} If any deferred task activation fails, the environment task is unable to handle the Tasking_Error exception and completes immediately. By contrast, if the partition elaboration policy is Concurrent, then this exception can be handled within a library unit. 


#### Extensions to Ada 95

{AI95-00265-01} {AI95-00421-01} [Pragma](./AA-2.8#S0019) Partition_Elaboration_Policy is new. 

