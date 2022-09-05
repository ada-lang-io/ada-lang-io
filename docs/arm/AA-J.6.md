---
sidebar_position:  196
---

# J.6  Numeric_Error


#### Static Semantics

The following declaration exists in the declaration of package Standard: 

```ada
Numeric_Error : exception renames Constraint_Error;

```

Discussion: This is true even though it is not shown in A.1. 

Reason: In Ada 83, it was unclear which situations should raise Numeric_Error, and which should raise Constraint_Error. The permissions of RM83-11.6 could often be used to allow the implementation to raise Constraint_Error in a situation where one would normally expect Numeric_Error. To avoid this confusion, all situations that raise Numeric_Error in Ada 83 are changed to raise Constraint_Error in Ada 95. Numeric_Error is changed to be a renaming of Constraint_Error to avoid most of the upward compatibilities associated with this change.

In new code, Constraint_Error should be used instead of Numeric_Error. 

