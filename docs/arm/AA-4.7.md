---
sidebar_position:  35
---

# 4.7  Qualified Expressions

[A [qualified_expression](./AA-4.7#S0163) is used to state explicitly the type, and to verify the subtype, of an operand that is either an [expression](./AA-4.4#S0132) or an [aggregate](./AA-4.3#S0106). ]


#### Syntax

qualified_expression<a id="S0163"></a> ::= 
   [subtype_mark](./AA-3.2#S0028)'([expression](./AA-4.4#S0132)) | [subtype_mark](./AA-3.2#S0028)'[aggregate](./AA-4.3#S0106)


#### Name Resolution Rules

{AI12-0325-1} The expected type for the operand (the [expression](./AA-4.4#S0132) or [aggregate](./AA-4.3#S0106)) is determined by the [subtype_mark](./AA-3.2#S0028). Furthermore, the operand shall resolve to be either the specified expected type or a universal type that covers it. 

Reason: {AI12-0325-1} The first sentence defines the expected type for rules that assume one is defined. The second sentence prevents the use of the various implicit conversions that are usually allowed for expected types (except the one for numeric literals). The intent is that a qualified expression is similar to an assertion about the subtype of the operand, and thus implicit conversions would interfere with that intent. 


#### Static Semantics

{AI05-0003-1} [If the operand of a [qualified_expression](./AA-4.7#S0163) denotes an object, the [qualified_expression](./AA-4.7#S0163) denotes a constant view of that object.] The nominal subtype of a [qualified_expression](./AA-4.7#S0163) is the subtype denoted by the [subtype_mark](./AA-3.2#S0028). 

Proof: {AI05-0003-1} This is stated in 3.3. 


#### Dynamic Semantics

{AI12-0100-1} The evaluation of a [qualified_expression](./AA-4.7#S0163) evaluates the operand (and if of a universal type, converts it to the type determined by the [subtype_mark](./AA-3.2#S0028)) and checks that its value belongs to the subtype denoted by the [subtype_mark](./AA-3.2#S0028). The exception Constraint_Error is raised if this check fails. Furthermore, if predicate checks are enabled for the subtype denoted by the [subtype_mark](./AA-3.2#S0028), a check is performed as defined in subclause 3.2.4, "Subtype Predicates" that the value satifies the predicates of the subtype. 

Ramification: This is one of the few contexts in Ada 95 where implicit subtype conversion is not performed prior to a constraint check, and hence no "sliding" of array bounds is provided.

{AI12-0100-1} The effect of a failed predicate check is as defined in 3.2.4; such a check could raise any exception, not just Constraint_Error or Assertion_Error. 

Reason: Implicit subtype conversion is not provided because a [qualified_expression](./AA-4.7#S0163) with a constrained target subtype is essentially an assertion about the subtype of the operand, rather than a request for conversion. An explicit [type_conversion](./AA-4.6#S0162) can be used rather than a [qualified_expression](./AA-4.7#S0163) if subtype conversion is desired.

{AI12-0100-1} We do a predicate check here so that a [qualified_expression](./AA-4.7#S0163) never allows something that the equivalent [type_conversion](./AA-4.6#S0162) would not allow. 

NOTE 1   {AI12-0440-1} When a given context does not uniquely identify an expected type, a [qualified_expression](./AA-4.7#S0163) can be used to do so. In particular, if an overloaded [name](./AA-4.1#S0091) or [aggregate](./AA-4.3#S0106) is passed to an overloaded subprogram, it can be necessary to qualify the operand to resolve its type. 


#### Examples

Examples of disambiguating expressions using qualification: 

```ada
type Mask is (Fix, Dec, Exp, Signif);
type Code is (Fix, Cla, Dec, Tnz, Sub);

```

```ada
Print (Mask'(Dec));  --  Dec is of type Mask
Print (Code'(Dec));  --  Dec is of type Code 

```

```ada
{AI12-0442-1} for J in Code'(Fix) .. Code'(Dec) loop ... -- qualification is necessary for either Fix or Dec
for J in Code range Fix .. Dec loop ...    -- qualification unnecessary
for J in Code'(Fix) .. Dec loop ...        -- qualification unnecessary for Dec

```

```ada
Dozen'(1 | 3 | 5 | 7 =&gt 2, others =&gt 0) -- see 4.6 

```


#### Wording Changes from Ada 2005

{AI05-0003-1} Added a definition of the nominal subtype of a [qualified_expression](./AA-4.7#S0163). 


#### Inconsistencies With Ada 2012

{AI12-0100-1} {AI12-0005-1} Corrigendum: A [qualified_expression](./AA-4.7#S0163) now performs a predicate check for the named subtype (if it is enabled). Original Ada 2012 did not include that check (an omission). While this is formally inconsistent (an exception could be raised when none would be raised by original Ada 2012), cases where this scenario arises are likely to be rare (the qualified expression would have to have a stricter subtype than the following usage) and the check is more likely to detect bugs than be unexpected. 


#### Wording Changes from Ada 2012

{AI12-0325-1} Reworded the resolution rule so that the operand of a [qualified_expression](./AA-4.7#S0163) has an expected type. This eliminates an annoying inconsistency in the language definition. 

