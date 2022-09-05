---
sidebar_position:  194
---

# J.4  The Constrained Attribute


#### Static Semantics

For every private subtype S, the following attribute is defined: 

Discussion: This includes generic formal private subtypes. 

S'ConstrainedYields the value False if S denotes an unconstrained nonformal private subtype with discriminants; also yields the value False if S denotes a generic formal private subtype, and the associated actual subtype is either an unconstrained subtype with discriminants or an unconstrained array subtype; yields the value True otherwise. The value of this attribute is of the predefined subtype Boolean. 

Reason: Because Ada 95 has [unknown_discriminant_part](./AA-3.7#S0060)s, the Constrained attribute of private subtypes is obsolete. This is fortunate, since its Ada 83 definition was confusing, as explained below. Because this attribute is obsolete, we do not bother to extend its definition to private extensions.

The Constrained attribute of an object is not obsolete.

Note well: S'Constrained matches the Ada 95 definition of "constrained" only for composite subtypes. For elementary subtypes, S'Constrained is always true, whether or not S is constrained. (The Constrained attribute of an object does not have this problem, as it is only defined for objects of a discriminated type.) So one should think of its designator as being 'Constrained_Or_Elementary. 

