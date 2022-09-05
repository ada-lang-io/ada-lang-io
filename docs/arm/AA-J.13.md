---
sidebar_position:  203
---

# J.13  Dependence Restriction Identifiers

{AI95-00394-01} The following restrictions involve dependence on specific language-defined units. The more general restriction No_Dependence (see 13.12.1) should be used for this purpose. 


#### Static Semantics

{AI95-00394-01} The following restriction_[identifier](./AA-2.3#S0002)s exist:

{AI95-00394-01} No_Asynchronous_Control Semantic dependence on the predefined package Asynchronous_Task_Control is not allowed.

{AI95-00394-01} No_Unchecked_Conversion Semantic dependence on the predefined generic function Unchecked_Conversion is not allowed.

{AI95-00394-01} No_Unchecked_Deallocation Semantic dependence on the predefined generic procedure Unchecked_Deallocation is not allowed.


#### Wording Changes from Ada 95

{AI95-00394-01} {AI05-0299-1} This subclause is new. These restrictions are replaced by the more general No_Dependence (see 13.12.1). 

