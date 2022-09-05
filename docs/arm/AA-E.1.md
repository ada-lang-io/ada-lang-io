---
sidebar_position:  169
---

# E.1  Partitions

[The partitions of a distributed program are classified as either active or passive.] 


#### Post-Compilation Rules

An active partition is a partition as defined in 10.2. A passive partition is a partition that has no thread of control of its own, whose library units are all preelaborated, and whose data and subprograms are accessible to one or more active partitions. 

Discussion: In most situations, a passive partition does not execute, and does not have a "real" environment task. Any execution involved in its elaboration and initialization occurs before it comes into existence in a distributed program (like most preelaborated entities). Likewise, there is no concrete meaning to passive partition termination. 

A passive partition shall include only [library_item](./AA-10.1#S0287)s that either are declared pure or are shared passive (see 10.2.1 and E.2.1).

An active partition shall be configured on a processing node. A passive partition shall be configured either on a storage node or on a processing node.

The configuration of the partitions of a program onto a distributed system shall be consistent with the possibility for data references or calls between the partitions implied by their semantic dependences. Any reference to data or call of a subprogram across partitions is called a remote access. 

Discussion: For example, an active partition that includes a unit with a semantic dependence on the declaration of another RCI package of some other active partition has to be connected to that other partition by some sort of a message passing mechanism.

A passive partition that is accessible to an active partition should have its storage addressable to the processor(s) of the active partition. The processor(s) should be able to read and write from/to that storage, as well as to perform "read-modify-write" operations (in order to support entry-less protected objects).

Ramification: {AI12-0359-1} A passive partition has no execution resources of its own, so while a call of a subprogram in a passive partition is a remote access to that subprogram, it is not a remote subprogram call (see E.4). The calling active partition executes the body of the subprogram of the passive partition. 


#### Dynamic Semantics

A [library_item](./AA-10.1#S0287) is elaborated as part of the elaboration of each partition that includes it. If a normal library unit (see E.2) has state, then a separate copy of the state exists in each active partition that elaborates it. [The state evolves independently in each such partition.]

Ramification: Normal library units cannot be included in passive partitions. 

[An active partition terminates when its environment task terminates.] A partition becomes inaccessible if it terminates or if it is aborted. An active partition is aborted when its environment task is aborted. In addition, if a partition fails during its elaboration, it becomes inaccessible to other partitions. Other implementation-defined events can also result in a partition becoming inaccessible. 

Implementation defined: Any events that can result in a partition becoming inaccessible.

For a [prefix](./AA-4.1#S0093) D that denotes a library-level declaration, excepting a declaration of or within a declared-pure library unit, the following attribute is defined: 

D'Partition_IdDenotes a value of the type universal_integer that identifies the partition in which D was elaborated. If D denotes the declaration of a remote call interface library unit (see E.2.3) the given partition is the one where the body of D was elaborated. 


#### Bounded (Run-Time) Errors

{AI95-00226-01} It is a bounded error for there to be cyclic elaboration dependences between the active partitions of a single distributed program. The possible effects, in each of the partitions involved, are deadlock during elaboration, or the raising of Communication_Error or Program_Error. 


#### Implementation Permissions

An implementation may allow multiple active or passive partitions to be configured on a single processing node, and multiple passive partitions to be configured on a single storage node. In these cases, the scheduling policies, treatment of priorities, and management of shared resources between these partitions are implementation defined. 

Implementation defined: The scheduling policies, treatment of priorities, and management of shared resources between partitions in certain cases.

An implementation may allow separate copies of an active partition to be configured on different processing nodes, and to provide appropriate interactions between the copies to present a consistent state of the partition to other active partitions. 

Ramification: The language does not specify the nature of these interactions, nor the actual level of consistency preserved. 

{AI12-0444-1} In an implementation, the partitions of a distributed program may be loaded and elaborated at different times; they may be loaded and elaborated one at a time over an extended period of time. An implementation may provide facilities to abort and reload a partition during the execution of a distributed program.

An implementation may allow the state of some of the partitions of a distributed program to persist while other partitions of the program terminate and are later reinvoked. 

NOTE 1   {AI12-0417-1} Library units are grouped into partitions after compile time, but before run time. At compile time, only the relevant library unit properties are identified using categorization aspects.

NOTE 2   The value returned by the Partition_Id attribute can be used as a parameter to implementation-provided subprograms in order to query information about the partition.


#### Wording Changes from Ada 95

{AI95-00226-01} Corrected wording so that a partition that has an elaboration problem will either deadlock or raise an exception. While an Ada 95 implementation could allow some partitions to continue to execute, they could be accessing unelaborated data, which is very bad (and erroneous in a practical sense). Therefore, this isn't listed as an inconsistency. 

