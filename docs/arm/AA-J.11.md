---
sidebar_position:  201
---

# J.11  The Class Attribute of Untagged Incomplete Types


#### Static Semantics

{AI95-00326-01} For the first subtype S of a type T declared by an [incomplete_type_declaration](./AA-3.10#S0085) that is not tagged, the following attribute is defined: 

{AI95-00326-01} S'Class Denotes the first subtype of the incomplete class-wide type rooted at T. The completion of T shall declare a tagged type. Such an attribute reference shall occur in the same library unit as the [incomplete_type_declaration](./AA-3.10#S0085). 

Reason: {AI95-00326-01} This must occur in the same unit to prevent children from imposing requirements on their ancestor library units for deferred incomplete types. 


#### Wording Changes from Ada 95

{AI95-00326-01} {AI05-0299-1} This subclause is new. This feature was moved here because the tagged incomplete type provides a better way to provide this capability (it doesn't put requirements on the completion based on uses that could be anywhere). Pity we didn't think of it in 1994. 

