---
sidebar_position:  184
---

# H.2  Documentation of Implementation Decisions


#### Documentation Requirements

{AI12-0439-1} The implementation shall document the range of effects for each situation that the language rules identify as either a bounded error or as having an unspecified effect. If the implementation can constrain the effects of erroneous execution for a given construct, then it shall document such constraints. [The documentation may be provided either independently of any compilation unit or partition, or as part of an annotated listing for a given unit or partition. See also , and .] 

This paragraph was deleted.

Documentation Requirement: The range of effects for each bounded error and each unspecified effect. If the effects of a given erroneous construct are constrained, the constraints shall be documented.

NOTE   Among the situations to be documented are the conventions chosen for parameter passing, the methods used for the management of run-time storage, and the method used to evaluate numeric expressions if this involves extended range or extra precision.

Discussion: Look up "unspecified" and "erroneous execution" in the index for a list of the cases.

The management of run-time storage is particularly important. For safety applications, it is often necessary to show that a program cannot raise Storage_Error, and for security applications that information cannot leak via the run-time system. Users are likely to prefer a simple storage model that can be easily validated.

The documentation could helpfully take into account that users may well adopt a subset to avoid some forms of erroneous execution, for instance, not using the abort statement, so that the effects of a partly completed [assignment_statement](./AA-5.2#S0173) do not have to be considered in the validation of a program (see 9.8). For this reason documentation linked to an actual compilation may be most useful. Similarly, an implementation may be able to take into account use of the Restrictions pragma. 

