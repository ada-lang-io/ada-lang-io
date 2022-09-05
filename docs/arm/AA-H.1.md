---
sidebar_position:  183
---

# H.1  Pragma Normalize_Scalars

This pragma ensures that an otherwise uninitialized scalar object is set to a predictable value, but out of range if possible. 

Discussion: The goal of the pragma is to reduce the impact of a bounded error that results from a reference to an uninitialized scalar object, by having such a reference violate a range check and thus raise Constraint_Error. 


#### Syntax

The form of a [pragma](./AA-2.8#S0019) Normalize_Scalars is as follows: 

  pragma Normalize_Scalars; 


#### Post-Compilation Rules

Pragma Normalize_Scalars is a configuration pragma. It applies to all [compilation_unit](./AA-10.1#S0286)s included in a partition. 


#### Documentation Requirements

{AI95-00434-01} If a [pragma](./AA-2.8#S0019) Normalize_Scalars applies, the implementation shall document the implicit initial values for scalar subtypes, and shall identify each case in which such a value is used and is not an invalid representation. 

Documentation Requirement: If a [pragma](./AA-2.8#S0019) Normalize_Scalars applies, the implicit initial values of scalar subtypes shall be documented. Such a value should be an invalid representation when possible; any cases when is it not shall be documented.

To be honest: It's slightly inaccurate to say that the value is a representation, but the point should be clear anyway. 

Discussion: By providing a type with a size specification so that spare bits are present, it is possible to force an implementation of Normalize_Scalars to use an out of range value. This can be tested for by ensuring that Constraint_Error is raised. Similarly, for an unconstrained integer type, in which no spare bit is surely present, one can check that the initialization takes place to the value specified in the documentation of the implementation. For a floating point type, spare bits might not be available, but a range constraint can provide the ability to use an out of range value.

If it is difficult to document the general rule for the implicit initial value, the implementation might choose instead to record the value on the object code listing or similar output produced during compilation.


#### Implementation Advice

{AI95-00434-01} Whenever possible, the implicit initial values for a scalar subtype should be an invalid representation (see 13.9.1). 

Discussion: When an out of range value is used for the initialization, it is likely that constraint checks will detect it. In addition, it can be detected by the Valid attribute.

This rule is included in the documentation requirements, and thus does not need a separate summary item.

NOTE 1   The initialization requirement applies to uninitialized scalar objects that are subcomponents of composite objects, to allocated objects, and to stand-alone objects. It also applies to scalar out parameters. Scalar subcomponents of composite out parameters are initialized to the corresponding part of the actual, by virtue of 6.4.1.

NOTE 2   The initialization requirement does not apply to a scalar for which pragma Import has been specified, since initialization of an imported object is performed solely by the foreign language environment (see B.1).

NOTE 3   {AI12-0440-1} The use of pragma Normalize_Scalars in conjunction with Pragma Restrictions(No_Exceptions) can result in erroneous execution (see H.4). 

Discussion: Since the effect of an access to an out of range value will often be to raise Constraint_Error, it is clear that suppressing the exception mechanism could result in erroneous execution. In particular, the assignment to an array, with the array index out of range, will result in a write to an arbitrary store location, having unpredictable effects.

