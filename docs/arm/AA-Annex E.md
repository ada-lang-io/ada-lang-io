---
sidebar_position:  168
---

# Annex E Distributed Systems

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[This Annex defines facilities for supporting the implementation of distributed systems using multiple partitions working cooperatively as part of a single Ada program.] 


#### Extensions to Ada 83

This Annex is new to Ada 95. 


#### Post-Compilation Rules

A distributed system is an interconnection of one or more processing nodes (a system resource that has both computational and storage capabilities), and zero or more storage nodes (a system resource that has only storage capabilities, with the storage addressable by one or more processing nodes).

A distributed program comprises one or more partitions that execute independently (except when they communicate) in a distributed system.

The process of mapping the partitions of a program to the nodes in a distributed system is called configuring the partitions of the program. 


#### Implementation Requirements

The implementation shall provide means for explicitly assigning library units to a partition and for the configuring and execution of a program consisting of multiple partitions on a distributed system; the means are implementation defined. 

Implementation defined: The means for creating and executing distributed programs.


#### Implementation Permissions

An implementation may require that the set of processing nodes of a distributed system be homogeneous.

NOTE 1   {AI12-0440-1} The partitions comprising a program can be executed on differently configured distributed systems or on a nondistributed system without requiring recompilation. A distributed program can be partitioned differently from the same set of library units without recompilation. The resulting execution is semantically equivalent.

NOTE 2   A distributed program retains the same type safety as the equivalent single partition program.

