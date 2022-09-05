---
sidebar_position:  199
---

# J.9  The Storage_Size Attribute


#### Static Semantics

For any task subtype T, the following attribute is defined: 

T'Storage_Size Denotes an implementation-defined value of type universal_integer representing the number of storage elements reserved for a task of the subtype T. 

To be honest: {AI05-0229-1} T'Storage_Size cannot be particularly meaningful in the presence of the specification of the aspect Storage_Size, especially when the expression is dynamic, or depends on a discriminant of the task, because the Storage_Size will be different for different objects of the type. Even without such a specification, the Storage_Size can be different for different objects of the type, and in any case, the value is implementation defined. Hence, it is always implementation defined. 

{AI95-00345-01} {AI05-0229-1} Storage_Size may be specified for a task first subtype that is not an interface via an [attribute_definition_clause](./AA-13.3#S0349). When the attribute is specified, the Storage_Size aspect is specified to be the value of the given [expression](./AA-4.4#S0132). 

Ramification: {AI05-0229-1} When this attribute is specified with an [attribute_definition_clause](./AA-13.3#S0349), the associated aspect is set to the value of the [expression](./AA-4.4#S0132) given in the [attribute_definition_clause](./AA-13.3#S0349), rather than the [expression](./AA-4.4#S0132) itself. This value is therefore the same for all objects of the type; in particular, it is not re-evaluated when objects are created. This is different than when the aspect is specified with an [aspect_specification](./AA-13.1#S0346) (see 13.3). 


#### Wording Changes from Ada 95

{AI95-00345-01} We don't allow specifying Storage_Size on task interfaces. We don't need to mention class-wide task types, because these cannot be a first subtype. 

