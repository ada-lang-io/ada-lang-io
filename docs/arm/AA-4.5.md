---
sidebar_position:  33
---

# 4.5  Operators and Expression Evaluation

[ The language defines the following six categories of operators (given in order of increasing precedence). The corresponding [operator_symbol](./AA-6.1#S0202)s, and only those, can be used as [designator](./AA-6.1#S0199)s in declarations of functions for user-defined operators. See 6.6, "Overloading of Operators".] 


#### Syntax

logical_operator<a id="S0142"></a> ::= 	 and | or  | xor

relational_operator<a id="S0143"></a> ::= 	 =   | /=  | &lt   | &lt= | &gt | &gt=

binary_adding_operator<a id="S0144"></a> ::= 	 +   |    | &

unary_adding_operator<a id="S0145"></a> ::= 	 +   | 

multiplying_operator<a id="S0146"></a> ::= 	 *   | /   | mod | rem

highest_precedence_operator<a id="S0147"></a> ::= 	 **  | abs | not

Discussion: Some of the above syntactic categories are not used in other syntax rules. They are just used for classification. The others are used for both classification and parsing. 


#### Static Semantics

For a sequence of operators of the same precedence level, the operators are associated with their operands in textual order from left to right. Parentheses can be used to impose specific associations. 

Discussion: The left-associativity is not directly inherent in the grammar of 4.4, though in the definition of the metasymbols {} implies left associativity. So this could be seen as redundant, depending on how literally one interprets the definition of the {} metasymbols.

See the Implementation Permissions below regarding flexibility in reassociating operators of the same precedence. 

For each form of type definition, certain of the above operators are predefined; that is, they are implicitly declared immediately after the type definition. For each such implicit operator declaration, the parameters are called Left and Right for binary operators; the single parameter is called Right for unary operators. [An expression of the form X op Y, where op is a binary operator, is equivalent to a [function_call](./AA-6.4#S0218) of the form "op"(X, Y). An expression of the form op Y, where op is a unary operator, is equivalent to a [function_call](./AA-6.4#S0218) of the form "op"(Y). The predefined operators and their effects are described in subclauses 4.5.1 through 4.5.6.] 


#### Dynamic Semantics

[ The predefined operations on integer types either yield the mathematically correct result or raise the exception Constraint_Error. For implementations that support the Numerics Annex, the predefined operations on real types yield results whose accuracy is defined in Annex G, or raise the exception Constraint_Error. ]

To be honest: Predefined operations on real types can "silently" give wrong results when the Machine_Overflows attribute is false, and the computation overflows. 

Proof: For integer types, this is normatively stated in the Dynamic Semantics of 3.5.4. For floating point types, this is normatively stated at the end of the Implementation Requirements of G.2.1. For fixed point types, this is normatively stated at the end of the Implementation Requirements of G.2.3. 


#### Implementation Requirements

The implementation of a predefined operator that delivers a result of an integer or fixed point type may raise Constraint_Error only if the result is outside the base range of the result type.

The implementation of a predefined operator that delivers a result of a floating point type may raise Constraint_Error only if the result is outside the safe range of the result type. 

To be honest: An exception is made for exponentiation by a negative exponent in 4.5.6.


#### Implementation Permissions

For a sequence of predefined operators of the same precedence level (and in the absence of parentheses imposing a specific association), an implementation may impose any association of the operators with operands so long as the result produced is an allowed result for the left-to-right association, but ignoring the potential for failure of language-defined checks in either the left-to-right or chosen order of association. 

Discussion: Note that the permission to reassociate the operands in any way subject to producing a result allowed for the left-to-right association is not much help for most floating point operators, since reassociation may introduce significantly different round-off errors, delivering a result that is outside the model interval for the left-to-right association. Similar problems arise for division with integer or fixed point operands.

Note that this permission does not apply to user-defined operators. 

NOTE 1   The two operands of an expression of the form X op Y, where op is a binary operator, are evaluated in an arbitrary order, as for any [function_call](./AA-6.4#S0218) (see 6.4).


#### Examples

Examples of precedence: 

```ada
not Sunny or Warm    --  same as (not Sunny) or Warm
X &gt 4.0 and Y &gt 0.0  --  same as (X &gt 4.0) and (Y &gt 0.0)

```

```ada
-4.0*A**2            --  same as (4.0 * (A**2))
abs(1 + A) + B       --  same as (abs (1 + A)) + B
Y**(-3)              --  parentheses are necessary
A / B * C            --  same as (A/B)*C
A + (B + C)          --  evaluate B + C before adding it to A 

```


#### Wording Changes from Ada 83

We don't give a detailed definition of precedence, since it is all implicit in the syntax rules anyway.

The permission to reassociate is moved here from RM83-11.6(5), so it is closer to the rules defining operator association. 


## 4.5.1  Logical Operators and Short-circuit Control Forms


#### Name Resolution Rules

An [expression](./AA-4.4#S0132) consisting of two [relation](./AA-4.4#S0135)s connected by and then or or else (a short-circuit control form) shall resolve to be of some boolean type; the expected type for both [relation](./AA-4.4#S0135)s is that same boolean type. 

Reason: This rule is written this way so that overload resolution treats the two operands symmetrically; the resolution of overloading present in either one can benefit from the resolution of the other. Furthermore, the type expected by context can help. 


#### Static Semantics

The following logical operators are predefined for every boolean type T, for every modular type T, and for every one-dimensional array type T whose component type is a boolean type: 

```ada
function "and"(Left, Right : T) return T
function "or" (Left, Right : T) return T
function "xor"(Left, Right : T) return T

```

This paragraph was deleted.{AI95-00145-01} 

Ramification: {AI95-00145-01} For these operators, we are talking about the type without any (interesting) subtype, and not some subtype with a constraint or exclusion. Since it's possible that there is no name for the "uninteresting" subtype, we denote the type with an italicized T. This applies to the italicized T in many other predefined operators and attributes as well.

{AI95-00145-01} In many cases, there is a subtype with the correct properties available. The italicized T means: 

T'Base, for scalars;

the first subtype of T, for tagged types;

a subtype of the type T without any constraint or null exclusion, in other cases. 

Note that "without a constraint" is not the same as unconstrained. For instance, a record type with no discriminant part is considered constrained; no subtype of it has a constraint, but the subtype is still constrained.

Thus, the last case often is the same as the first subtype of T, but that isn't the case for constrained array types (where the correct subtype is unconstrained) and for access types with a [null_exclusion](./AA-3.10#S0083) (where the correct subtype does not exclude null).

This italicized T is used for defining operators and attributes of the language. The meaning is intended to be as described here. 

For boolean types, the predefined logical operators and, or, and xor perform the conventional operations of conjunction, inclusive disjunction, and exclusive disjunction, respectively.

For modular types, the predefined logical operators are defined on a bit-by-bit basis, using the binary representation of the value of the operands to yield a binary representation for the result, where zero represents False and one represents True. If this result is outside the base range of the type, a final subtraction by the modulus is performed to bring the result into the base range of the type.

The logical operators on arrays are performed on a component-by-component basis on matching components (as for equality - see 4.5.2), using the predefined logical operator for the component type. The bounds of the resulting array are those of the left operand.


#### Dynamic Semantics

The short-circuit control forms and then and or else deliver the same result as the corresponding predefined and and or operators for boolean types, except that the left operand is always evaluated first, and the right operand is not evaluated if the value of the left operand determines the result.

For the logical operators on arrays, a check is made that for each component of the left operand there is a matching component of the right operand, and vice versa. Also, a check is made that each component of the result belongs to the component subtype. The exception Constraint_Error is raised if either of the above checks fails. 

Discussion: The check against the component subtype is per AI83-00535. 

NOTE   The conventional meaning of the logical operators is given by the following truth table: 

	  A	  B	(A and B)	(A or B)	(A xor B)

	True  	True  	True  	True  	False
	True  	False 	False 	True  	True
	False 	True  	False 	True  	True
	False 	False 	False 	False 	False


#### Examples

Examples of logical operators: 

```ada
Sunny or Warm
Filter(1 .. 10) and Filter(15 .. 24)   --   see 3.6.1 

```

Examples of short-circuit control forms: 

```ada
Next_Car.Owner /= null and then Next_Car.Owner.Age &gt 25   --   see 3.10.1
N = 0 or else A(N) = Hit_Value

```


## 4.5.2  Relational Operators and Membership Tests

[ The equality operators = (equals) and /= (not equals) are predefined for nonlimited types. The other [relational_operator](./AA-4.5#S0143)s are the ordering operators &lt (less than), &lt= (less than or equal), &gt (greater than), and &gt= (greater than or equal). The ordering operators are predefined for scalar types, and for discrete array types, that is, one-dimensional array types whose components are of a discrete type. 

Ramification: The equality operators are not defined for every nonlimited type - see below for the exact rule. 

{AI05-0262-1} {AI05-0269-1} A membership test, using in or not in, determines whether or not a value belongs to any given subtype or range, is equal to any given value, has a tag that identifies a type that is covered by a given type, or is convertible to and has an accessibility level appropriate for a given access type. Membership tests are allowed for all types.]


#### Name Resolution Rules

{AI95-00251-01} {AI05-0158-1} The tested type of a membership test is determined by the [membership_choice](./AA-4.4#S0137)s of the [membership_choice_list](./AA-4.4#S0136). Either all [membership_choice](./AA-4.4#S0137)s of the [membership_choice_list](./AA-4.4#S0136) shall resolve to the same type, which is the tested type; or each [membership_choice](./AA-4.4#S0137) shall be of an elementary type, and the tested type shall be covered by each of these elementary types.

{AI05-0158-1} {AI12-0039-1} {AI12-0418-1} If the tested type is tagged, then the tested_[simple_expression](./AA-4.4#S0138) shall resolve to be of a type that is convertible (see 4.6) to the tested type; if untagged, the expected type of the tested_[simple_expression](./AA-4.4#S0138) is the tested type. The expected type of a choice_[simple_expression](./AA-4.4#S0138) in a [membership_choice](./AA-4.4#S0137), and of a [simple_expression](./AA-4.4#S0138) of a [range](./AA-3.5#S0037) in a [membership_choice](./AA-4.4#S0137), is the tested type of the membership operation.

Reason: {AI95-00230-01} The part of the rule for untagged types is stated in a way that ensures that operands like a string literal are still legal as operands of a membership test.

{AI95-00251-01} {AI12-0039-1} The significance of "is convertible to" is that we allow the tested_[simple_expression](./AA-4.4#S0138) to be of any class-wide type that could be converted to the tested type, not just the one rooted at the tested type. This includes any class-wide type that covers the tested type, along with class-wide interfaces in some cases.

{AI05-0158-1} The special rule for determining the tested type for elementary types is to allow numeric literals in [membership_choice_list](./AA-4.4#S0136)s. Without the rule, A in B | 1 would be illegal as B and 1 would have different types (the literal having type universal integer). 


#### Legality Rules

{AI12-0039-1} For a membership test, if the tested_[simple_expression](./AA-4.4#S0138) is of a tagged class-wide type, then the tested type shall be (visibly) tagged. 

Ramification: Untagged types covered by the tagged class-wide type are not permitted. Such types can exist if they are descendants of a private type whose full type is tagged. This rule is intended to avoid confusion since such derivatives don't have their "own" tag, and hence are indistinguishable from one another at run time once converted to a covering class-wide type. 

{AI05-0158-1} {AI12-0039-1} {AI12-0328-1} If a membership test includes one or more choice_[simple_expression](./AA-4.4#S0138)s and the tested type of the membership test is limited, then the tested type of the membership test shall have a visible primitive equality operator; if the tested type of the membership test is nonlimited with a user-defined primitive equality operator that is defined at a point where the type is limited, the tested type shall be a record type or record extension. 

Reason: {AI05-0158-1} A visible equality operator is required in order to avoid breaking privacy; that is, we don't want to depend on a hidden equality operator.

{AI12-0328-1} We make the membership test on the nonlimited view of a type illegal if it would use a different equality operator than what would be used for a limited view of the same type (and such a limited view is known to exist). 


#### Static Semantics

The result type of a membership test is the predefined type Boolean.

The equality operators are predefined for every specific type T that is not limited, and not an anonymous access type, with the following specifications: 

```ada
function "=" (Left, Right : T) return Boolean
function "/="(Left, Right : T) return Boolean

```

{AI95-00230-01} The following additional equality operators for the universal_access type are declared in package Standard for use with anonymous access types: 

```ada
function "=" (Left, Right : universal_access) return Boolean
function "/="(Left, Right : universal_access) return Boolean

```

The ordering operators are predefined for every specific scalar type T, and for every discrete array type T, with the following specifications: 

```ada
function "&lt" (Left, Right : T) return Boolean
function "&lt="(Left, Right : T) return Boolean
function "&gt" (Left, Right : T) return Boolean
function "&gt="(Left, Right : T) return Boolean

```


#### Name Resolution Rules

{AI95-00230-01} {AI95-00420-01} At least one of the operands of an equality operator for universal_access shall be of a specific anonymous access type. Unless the predefined equality operator is identified using an expanded name with [prefix](./AA-4.1#S0093) denoting the package Standard, neither operand shall be of an access-to-object type whose designated type is D or D'Class, where D has a user-defined primitive equality operator such that: 

its result type is Boolean;

{AI05-0020-1} it is declared immediately within the same declaration list as D or any partial or incomplete view of D; and

at least one of its operands is an access parameter with designated type D. 

Reason: The first sentence prevents compatibility problems by ensuring that these operators are not used for named access types. Also, universal access types do not count for the purposes of this rule. Otherwise, equality expressions like (X = null) would be ambiguous for normal access types.

The rest of the rule makes it possible to call (including a dispatching call) user-defined "=" operators for anonymous access-to-object types (they'd be hidden otherwise), and to write user-defined "=" operations for anonymous access types (by making it possible to see the universal operator using the Standard prefix). 

Ramification: We don't need a similar rule for anonymous access-to-subprogram types because they can't be primitive for any type. Note that any nonprimitive user-defined equality operators still are hidden by the universal operators; they'll have to be called with a package prefix, but they are likely to be very uncommon. 


#### Legality Rules

{AI95-00230-01} At least one of the operands of the equality operators for universal_access shall be of type universal_access, or both shall be of access-to-object types, or both shall be of access-to-subprogram types. Further: 

When both are of access-to-object types, the designated types shall be the same or one shall cover the other, and if the designated types are elementary or array types, then the designated subtypes shall statically match;

When both are of access-to-subprogram types, the designated profiles shall be subtype conformant. 

Reason: We don't want to allow completely arbitrary comparisons, as we don't want to insist that all access types are represented in ways that are convertible to one another. For instance, a compiler could use completely separate address spaces or incompatible representations. Instead, we allow compares if there exists an access parameter to which both operands could be converted. Since the user could write such an subprogram, and any reasonable meaning for "=" would allow using it in such a subprogram, this doesn't impose any further restrictions on Ada implementations. 

{AI05-0123-1} {AI12-0101-1} {AI12-0352-1} If the profile of an explicitly declared primitive equality operator of an untagged record type is type conformant with that of the corresponding predefined equality operator, the declaration shall occur before the type is frozen. In addition, no type shall have been derived from the untagged record type before the declaration of the primitive equality operator. In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit. 


#### Dynamic Semantics

For discrete types, the predefined relational operators are defined in terms of corresponding mathematical operations on the position numbers of the values of the operands.

For real types, the predefined relational operators are defined in terms of the corresponding mathematical operations on the values of the operands, subject to the accuracy of the type. 

Ramification: For floating point types, the results of comparing nearly equal values depends on the accuracy of the implementation (see G.2.1, "Model of Floating Point Arithmetic" for implementations that support the Numerics Annex). 

Implementation Note: On a machine with signed zeros, if the generated code generates both plus zero and minus zero, plus and minus zero must be equal by the predefined equality operators. 

Two access-to-object values are equal if they designate the same object, or if both are equal to the null value of the access type.

Two access-to-subprogram values are equal if they are the result of the same evaluation of an Access [attribute_reference](./AA-4.1#S0100), or if both are equal to the null value of the access type. Two access-to-subprogram values are unequal if they designate different subprograms. [It is unspecified whether two access values that designate the same subprogram but are the result of distinct evaluations of Access [attribute_reference](./AA-4.1#S0100)s are equal or unequal.] 

Reason: This allows each Access [attribute_reference](./AA-4.1#S0100) for a subprogram to designate a distinct "wrapper" subprogram if necessary to support an indirect call. 

{AI05-0123-1} For a type extension, predefined equality is defined in terms of the primitive [(possibly user-defined)] equals operator for the parent type and for any components that have a record type in the extension part, and predefined equality for any other components not inherited from the parent type. 

Ramification: Two values of a type extension are not equal if there is a [variant_part](./AA-3.8#S0071) in the extension part and the two values have different [variant](./AA-3.8#S0072)s present. This is a ramification of the requirement that a discriminant governing such a [variant_part](./AA-3.8#S0071) has to be a "new" discriminant, and so has to be equal in the two values for the values to be equal. Note that [variant_part](./AA-3.8#S0071)s in the parent part need not match if the primitive equals operator for the parent type considers them equal.

{AI95-00349-01} The full type extension's operation is used for a private extension. This follows as only full types have parent types; the type specified in a private extension is an ancestor, but not necessarily the parent type. For instance, in: 

```ada
with Pak1;
package Pak2 is
   type Typ3 is new Pak1.Typ1 with private;
private
   type Typ3 is new Pak1.Typ2 with null record;
end Pak2;
  

```

the parent type is Pak1.Typ2, not Pak1.Typ1, and the equality operator of Pak1.Typ2 is used to create predefined equality for Typ3. 

{AI05-0123-1} {AI12-0328-1} For a private type, if its full type is a record type or a record extension, predefined equality is defined in terms of the primitive equals operator of the full type; otherwise, predefined equality for the private type is that of its full type.

For other composite types, the predefined equality operators [(and certain other predefined operations on composite types - see 4.5.1 and 4.6)] are defined in terms of the corresponding operation on matching components, defined as follows: 

For two composite objects or values of the same non-array type, matching components are those that correspond to the same [component_declaration](./AA-3.8#S0070) or [discriminant_specification](./AA-3.7#S0062);

For two one-dimensional arrays of the same type, matching components are those (if any) whose index values match in the following sense: the lower bounds of the index ranges are defined to match, and the successors of matching indices are defined to match;

For two multidimensional arrays of the same type, matching components are those whose index values match in successive index positions. 

The analogous definitions apply if the types of the two objects or values are convertible, rather than being the same. 

Discussion: Ada 83 seems to omit this part of the definition, though it is used in array type conversions. See 4.6. 

Given the above definition of matching components, the result of the predefined equals operator for composite types (other than for those composite types covered earlier) is defined as follows: 

If there are no components, the result is defined to be True;

If there are unmatched components, the result is defined to be False;

{AI05-0123-1} Otherwise, the result is defined in terms of the primitive equals operator for any matching components that are records, and the predefined equals for any other matching components. 

Reason: {AI05-0123-1} This asymmetry between components with and without a record type is necessary to preserve most upward compatibility and corresponds with the corresponding situation with generics, where the predefined operations "reemerge" in a generic for non-record types, but do not for record types. Also, only tagged types support user-defined assignment (see 7.6), so only tagged types can fully handle levels of indirection in the implementation of the type. For untagged types, one reason for a user-defined equals operator might be to allow values with different bounds or discriminants to compare equal in certain cases. When such values are matching components, the bounds or discriminants will necessarily match anyway if the discriminants of the enclosing values match. 

Ramification: Two null arrays of the same type are always equal; two null records of the same type are always equal.

{AI05-0123-1} Note that if a composite object has a component of a floating point type, and the floating point type has both a plus and minus zero, which are considered equal by the predefined equality, then a block compare cannot be used for the predefined composite equality. Of course, with user-defined equals operators for components that are records, a block compare breaks down anyway, so this is not the only special case that requires component-by-component comparisons. On a one's complement machine, a similar situation might occur for integer types, since one's complement machines typically have both a plus and minus (integer) zero. 

To be honest: {AI95-00230-01} For a component with an anonymous access type, "predefined equality" is that defined for the universal_access type (anonymous access types have no equality operators of their own).

{AI05-0123-1} For a component with a record type T, "the primitive equals operator" is the one with two parameters of T which returns Boolean. We're not talking about some random other primitive function named "=". 

{AI05-0123-1} {AI12-0413-1} If the primitive equals operator for an untagged record type is abstract, then Program_Error is raised at the point of any call to that abstract subprogram[, implicitly as part of an equality operation on an enclosing composite object, or in an instance of a generic with a formal private type where the actual type is a record type with an abstract "="]. 

Reason: {AI12-0413-1} An explicit call to an abstract subprogram is generally illegal. This rule is needed in order to define the effect of a call in an instance of a generic body, or an implicit call such as a call that is part of the predefined equality operation for an enclosing composite type that has a component of an untagged record type that has an abstract primitive equals operator. For tagged types, an abstract primitive equals operator is only allowed for an abstract type, and abstract types cannot be components, so this case does not occur. 

{8652/0016} {AI95-00123-01} For any composite type, the order in which "=" is called for components is unspecified. Furthermore, if the result can be determined before calling "=" on some components, it is unspecified whether "=" is called on those components.

The predefined "/=" operator gives the complementary result to the predefined "=" operator. 

Ramification: Furthermore, if the user defines an "=" operator that returns Boolean, then a "/=" operator is implicitly declared in terms of the user-defined "=" operator so as to give the complementary result. See 6.6. 

{AI05-0264-1} For a discrete array type, the predefined ordering operators correspond to lexicographic order using the predefined order relation of the component type: A null array is lexicographically less than any array having at least one component. In the case of nonnull arrays, the left operand is lexicographically less than the right operand if the first component of the left operand is less than that of the right; otherwise, the left operand is lexicographically less than the right operand only if their first components are equal and the tail of the left operand is lexicographically less than that of the right (the tail consists of the remaining components beyond the first and can be null).

{AI05-0269-1} An individual membership test is the membership test of a single [membership_choice](./AA-4.4#S0137).

{AI05-0158-1} {AI12-0039-1} For the evaluation of a membership test using in whose [membership_choice_list](./AA-4.4#S0136) has a single [membership_choice](./AA-4.4#S0137), the tested_[simple_expression](./AA-4.4#S0138) and the [membership_choice](./AA-4.4#S0137) are evaluated in an arbitrary order; the result is the result of the individual membership test for the [membership_choice](./AA-4.4#S0137).

{AI05-0158-1} {AI12-0039-1} For the evaluation of a membership test using in whose [membership_choice_list](./AA-4.4#S0136) has more than one [membership_choice](./AA-4.4#S0137), the tested_[simple_expression](./AA-4.4#S0138) of the membership test is evaluated first and the result of the operation is equivalent to that of a sequence consisting of an individual membership test on each [membership_choice](./AA-4.4#S0137) combined with the short-circuit control form or else.

Ramification: {AI05-0158-1} This equivalence includes the evaluation of the [membership_choice](./AA-4.4#S0137)s; evaluation stops as soon as an individual choice evaluates to True. 

{AI05-0158-1} {AI05-0269-1} An individual membership test yields the result True if: 

{AI05-0158-1} {AI05-0264-1} {AI12-0039-1} {AI12-0328-1} The [membership_choice](./AA-4.4#S0137) is a choice_[simple_expression](./AA-4.4#S0138), and the tested_[simple_expression](./AA-4.4#S0138) is equal to the value of the [membership_choice](./AA-4.4#S0137). If the tested type is a record type or a record extension, or is limited at the point where the membership test occurs, the test uses the primitive equality for the type; otherwise, the test uses predefined equality.

Reason: We use the predefined equality operator if the membership test occurs where the type is nonlimited if the type is not a record type or record extension. However, to avoid confusion, cases where a membership test could use different equality operators based on the view are illegal. 

{AI05-0153-3} {AI05-0158-1} {AI12-0039-1} The [membership_choice](./AA-4.4#S0137) is a [range](./AA-3.5#S0037) and the value of the tested_[simple_expression](./AA-4.4#S0138) belongs to the given [range](./AA-3.5#S0037).

{AI05-0153-3} {AI05-0158-1} {AI12-0039-1} {AI12-0071-1} The [membership_choice](./AA-4.4#S0137) is a [subtype_mark](./AA-3.2#S0028), the tested type is scalar, the value of the tested_[simple_expression](./AA-4.4#S0138) belongs to the range of the named subtype, and the value satisfies the predicates of the named subtype. 

Ramification: {AI05-0153-3} The scalar membership test only does a range check and a predicate check. It does not perform any other check, such as whether a value falls in a "hole" of a "holey" enumeration type. The Pos attribute function can be used for that purpose.

Even though Standard.Float is an unconstrained subtype, the test "X in Float" will still return False (presuming the evaluation of X does not raise Constraint_Error) when X is outside Float'Range. 

{AI95-00231-01} {AI05-0153-3} {AI05-0158-1} {AI12-0039-1} {AI12-0071-1} The [membership_choice](./AA-4.4#S0137) is a [subtype_mark](./AA-3.2#S0028), the tested type is not scalar, the value of the tested_[simple_expression](./AA-4.4#S0138) satisfies any constraints of the named subtype, the value satisfies the predicates of the named subtype, and: 

{AI95-00231-01} {AI12-0039-1} if the type of the tested_[simple_expression](./AA-4.4#S0138) is class-wide, the value has a tag that identifies a type covered by the tested type; 

Ramification: {AI12-0039-1} Note that the tag is not checked if the tested_[simple_expression](./AA-4.4#S0138) is of a specific type. 

{AI95-00231-01} {AI05-0149-1} {AI12-0039-1} if the tested type is an access type and the named subtype excludes null, the value of the tested_[simple_expression](./AA-4.4#S0138) is not null;

{AI05-0149-1} {AI12-0039-1} if the tested type is a general access-to-object type, the type of the tested_[simple_expression](./AA-4.4#S0138) is convertible to the tested type and its accessibility level is no deeper than that of the tested type; further, if the designated type of the tested type is tagged and the tested_[simple_expression](./AA-4.4#S0138) is nonnull, the tag of the object designated by the value of the tested_[simple_expression](./AA-4.4#S0138) is covered by the designated type of the tested type. 

To be honest: {AI12-0005-1} In some of these cases, the bulleted checks need to pass before any predicate check is executed. Otherwise, a predicate check could be performed on an object of the wrong type. Consider:

```ada
type Root is tagged null record;
type Ext is new Root with record Data : Integer; end record;
function Is_Even (Param : Ext) return Boolean is
   (Param.Data mod 2 = 0);
subtype Even_Ext is Ext
  with Dynamic_Predicate =&gt Is_Even (Even_Ext);
function F (X : Root'Class) return Boolean is
   (X in Even_Ext);
Flag : Boolean := F (Root'(null record));

```

If the predicate check is performed before the tag check or regardless of the result of that check, Is_Even would be called on an object that does not have a Data component. Similar cases can be constructed for general access types. 

{AI05-0264-1} Otherwise, the test yields the result False.

A membership test using not in gives the complementary result to the corresponding membership test using in.

To be honest: {AI05-0158-1} {AI12-0039-1} X not in A | B | C is intended to be exactly equivalent to not (X in A | B | C), including the order of evaluation of the tested_[simple_expression](./AA-4.4#S0138) and [membership_choice](./AA-4.4#S0137)s. 


#### Implementation Requirements

{8652/0016} {AI95-00123-01} For all nonlimited types declared in language-defined packages, the "=" and "/=" operators of the type shall behave as if they were the predefined equality operators for the purposes of the equality of composite types and generic formal types. 

Ramification: {AI95-00123-01} {AI05-0123-1} If any language-defined types are implemented with a user-defined "=" operator, then either the full type must be a record type, or the compiler must use "magic" to implement equality for this type. A normal user-defined "=" operator for a non-record type does not meet this requirement. 

This paragraph was deleted.{AI95-00230-01} 

NOTE   If a composite type has components that depend on discriminants, two values of this type have matching components if and only if their discriminants are equal. Two nonnull arrays have matching components if and only if the length of each dimension is the same for both. 


#### Examples

Examples of expressions involving relational operators and membership tests: 

```ada
X /= Y

```

```ada
{AI12-0178-1} {AI12-0425-1} A_String = "A"                        -- True (see 3.3.1)
"" &lt A_String and A_String &lt "Aa"     -- True
A_String &lt "Bb" and A_String &lt "A  "  -- True

```

```ada
{AI05-0264-1} My_Car = null               -- True if My_Car has been set to null (see 3.10.1)
My_Car = Your_Car           -- True if we both share the same car
My_Car.all = Your_Car.all   -- True if the two cars are identical

```

```ada
{AI05-0158-1} N not in 1 .. 10            -- range membership test
Today in Mon .. Fri         -- range membership test
Today in Weekday            -- subtype membership test (see 3.5.1)
Card in Clubs | Spades      -- list membership test (see 3.5.1)
Archive in Disk_Unit        -- subtype membership test (see 3.8.1)
Tree.all in Addition'Class  -- class membership test (see 3.9.1)

```


#### Extensions to Ada 83

Membership tests can be used to test the tag of a class-wide value.

Predefined equality for a composite type is defined in terms of the primitive equals operator for tagged components or the parent part. 


#### Wording Changes from Ada 83

The term "membership test" refers to the [relation](./AA-4.4#S0135) "X in S" rather to simply the reserved word in or not in.

We use the term "equality operator" to refer to both the = (equals) and /= (not equals) operators. Ada 83 referred to = as the equality operator, and /= as the inequality operator. The new wording is more consistent with the ISO 10646 name for "=" (equals sign) and provides a category similar to "ordering operator" to refer to both = and /=.

We have changed the term "catenate" to "concatenate". 


#### Extensions to Ada 95

{AI95-00230-01} {AI95-00420-01} The universal_access equality operators are new. They provide equality operations (most importantly, testing against null) for anonymous access types. 


#### Wording Changes from Ada 95

{8652/0016} {AI95-00123-01} Corrigendum: Wording was added to clarify that the order of calls (and whether the calls are made at all) on "=" for components is unspecified. Also clarified that "=" must compose properly for language-defined types.

{AI95-00251-01} Memberships were adjusted to allow interfaces which don't cover the tested type, in order to be consistent with type conversions. 


#### Inconsistencies With Ada 2005

{AI05-0123-1} User-defined untagged record equality is now defined to compose and be used in generics. Any code which assumes that the predefined equality reemerges in generics and in predefined equals for composite types could fail. However, it is much more likely that this change will fix bugs, as the behavior that would be expected (the user-defined "=" is used) will be true in more cases.

{AI05-0123-1} If a composite type contains a component of an untagged record type with an abstract equality operation, calling "=" on the composite type will raise Program_Error, while in the past a result will be returned using the predefined equality. This is quite possible in ASIS programs; it will detect a bug in such programs but of course the programs will need to be fixed before they will work. 


#### Incompatibilities With Ada 2005

{AI05-0123-1} Late and hidden overriding of equality for untagged record types is now prohibited. This is necessary to make composition of equality predictable. It should always be possible to move the overriding to an earlier spot where it will be legal. 


#### Extensions to Ada 2005

{AI05-0149-1} Membership tests for valid accessibility levels and tag coverage by the designated type for general access types are new.

{AI05-0153-3} Membership tests now include a predicate check.

{AI05-0158-1} Membership tests now allow multiple choices. 


#### Wording Changes from Ada 2005

{AI05-0020-1} Correction: Wording was added to clarify that universal_access "=" does not apply if an appropriate operator is declared for a partial or incomplete view of the designated type. Otherwise, adding a partial or incomplete view could make some "=" operators ambiguous. 


#### Inconsistencies With Ada 2012

{AI12-0101-1} Corrigendum: Removed the incompatible rule preventing the declaration of "=" in the private part of a package specification for an untagged record type that completes a private type. Any code that calls the predefined "=" on the private type will now execute the body for the redefined "=" instead for the predefined "=". Eliminating the rule eliminates an unnecessary incompatibility (especially for programs that never call the predefined "="). Moreover, (like the composition of untagged record "=" in Ada 2012) this is more likely to fix bugs than cause them (who defines an "=" with a presumably different result and does not want clients to use it?). 


#### Incompatibilities With Ada 2012

{AI12-0328-1} A membership test is now illegal if all of the following are True: 

The membership test has one or more choice_[simple_expression](./AA-4.4#S0138)s;

The membership test occurs in a place where the tested type is nonlimited;

The tested type has a limited view with a primitive "=" operator;

The full type of the tested type is not a record type or a record extension. 

In such a case, the limited and nonlimited views would use different equality operators, which would be confusing and would cause various semantic difficulties. We believe such cases to be quite rare, especially as such membership tests are new to Ada 2012. The workaround is to replace such memberships with equality tests (assuming that the primitive "=" is intended; the predefined "=" is hidden in such cases and an extra type is needed to make it accessible). 


#### Wording Changes from Ada 2012

{AI12-0039-1} Corrigendum: Reworded membership tests to use the syntax items tested_[simple_expression](./AA-4.4#S0138) and choice_[simple_expression](./AA-4.4#S0138). This was necessary to eliminate wording ambiguities introduced when the grammar was corrected to eliminate syntax ambiguities. (Both of the above are now [simple_expression](./AA-4.4#S0138)s, so merely talking about a [simple_expression](./AA-4.4#S0138) is insufficient.)

{AI12-0071-1} Corrigendum: Updated wording of the membership tests to use the new term "satisfies the predicates" (see 3.2.4).

{AI12-0413-1} Correction: Clarified that a call to an abstract equality in an instance body raises Program_Error. 


## 4.5.3  Binary Adding Operators


#### Static Semantics

The binary adding operators + (addition) and  (subtraction) are predefined for every specific numeric type T with their conventional meaning. They have the following specifications: 

```ada
function "+"(Left, Right : T) return T
function "-"(Left, Right : T) return T

```

The concatenation operators & are predefined for every nonlimited, one-dimensional array type T with component type C. They have the following specifications: 

```ada
function "&"(Left : T; Right : T) return T
function "&"(Left : T; Right : C) return T
function "&"(Left : C; Right : T) return T
function "&"(Left : C; Right : C) return T

```


#### Dynamic Semantics

For the evaluation of a concatenation with result type T, if both operands are of type T, the result of the concatenation is a one-dimensional array whose length is the sum of the lengths of its operands, and whose components comprise the components of the left operand followed by the components of the right operand. If the left operand is a null array, the result of the concatenation is the right operand. Otherwise, the lower bound of the result is determined as follows: 

If the ultimate ancestor of the array type was defined by a [constrained_array_definition](./AA-3.6#S0054), then the lower bound of the result is that of the index subtype; 

Reason: This rule avoids Constraint_Error when using concatenation on an array type whose first subtype is constrained. 

If the ultimate ancestor of the array type was defined by an [unconstrained_array_definition](./AA-3.6#S0052), then the lower bound of the result is that of the left operand. 

[The upper bound is determined by the lower bound and the length.] A check is made that the upper bound of the result of the concatenation belongs to the range of the index subtype, unless the result is a null array. Constraint_Error is raised if this check fails.

If either operand is of the component type C, the result of the concatenation is given by the above rules, using in place of such an operand an array having this operand as its only component (converted to the component subtype) and having the lower bound of the index subtype of the array type as its lower bound. 

Ramification: The conversion might raise Constraint_Error. The conversion provides "sliding" for the component in the case of an array-of-arrays, consistent with the normal Ada 95 rules that allow sliding during parameter passing. 

The result of a concatenation is defined in terms of an assignment to an anonymous object, as for any function call (see 6.5). 

Ramification: This implies that value adjustment is performed as appropriate - see 7.6. We don't bother saying this for other predefined operators, even though they are all function calls, because this is the only one where it matters. It is the only one that can return a value having controlled parts. 

NOTE   As for all predefined operators on modular types, the binary adding operators + and  on modular types include a final reduction modulo the modulus if the result is outside the base range of the type. 

Implementation Note: A full "modulus" operation need not be performed after addition or subtraction of modular types. For binary moduli, a simple mask is sufficient. For nonbinary moduli, a check after addition to see if the value is greater than the high bound of the base range can be followed by a conditional subtraction of the modulus. Conversely, a check after subtraction to see if a "borrow" was performed can be followed by a conditional addition of the modulus. 


#### Examples

Examples of expressions involving binary adding operators: 

```ada
Z + 0.1      --  Z has to be of a real type 

```

```ada
"A" & "BCD"  --  concatenation of two string literals
'A' & "BCD"  --  concatenation of a character literal and a string literal
'A' & 'A'    --  concatenation of two character literals 

```


#### Inconsistencies With Ada 83

The lower bound of the result of concatenation, for a type whose first subtype is constrained, is now that of the index subtype. This is inconsistent with Ada 83, but generally only for Ada 83 programs that raise Constraint_Error. For example, the concatenation operator in 

```ada
X : array(1..10) of Integer;
begin
X := X(6..10) & X(1..5);

```

would raise Constraint_Error in Ada 83 (because the bounds of the result of the concatenation would be 6..15, which is outside of 1..10), but would succeed and swap the halves of X (as expected) in Ada 95. 


#### Extensions to Ada 83

Concatenation is now useful for array types whose first subtype is constrained. When the result type of a concatenation is such an array type, Constraint_Error is avoided by effectively first sliding the left operand (if nonnull) so that its lower bound is that of the index subtype. 


## 4.5.4  Unary Adding Operators


#### Static Semantics

The unary adding operators + (identity) and  (negation) are predefined for every specific numeric type T with their conventional meaning. They have the following specifications: 

```ada
function "+"(Right : T) return T
function "-"(Right : T) return T

```

NOTE 1   For modular integer types, the unary adding operator , when given a nonzero operand, returns the result of subtracting the value of the operand from the modulus; for a zero operand, the result is zero. 


## 4.5.5  Multiplying Operators


#### Static Semantics

The multiplying operators * (multiplication), / (division), mod (modulus), and rem (remainder) are predefined for every specific integer type T: 

```ada
function "*"  (Left, Right : T) return T
function "/"  (Left, Right : T) return T
function "mod"(Left, Right : T) return T
function "rem"(Left, Right : T) return T

```

Signed integer multiplication has its conventional meaning.

Signed integer division and remainder are defined by the relation: 

```ada
A = (A/B)*B + (A rem B)

```

where (A rem B) has the sign of A and an absolute value less than the absolute value of B. Signed integer division satisfies the identity: 

```ada
(-A)/B = -(A/B) = A/(-B)

```

{AI05-0260-1} The signed integer modulus operator is defined such that the result of A mod B is either zero, or has the sign of B and an absolute value less than the absolute value of B; in addition, for some signed integer value N, this result satisfies the relation: 

```ada
A = B*N + (A mod B)

```

The multiplying operators on modular types are defined in terms of the corresponding signed integer operators[, followed by a reduction modulo the modulus if the result is outside the base range of the type] [(which is only possible for the "*" operator)]. 

Ramification: The above identity satisfied by signed integer division is not satisfied by modular division because of the difference in effect of negation. 

Multiplication and division operators are predefined for every specific floating point type T: 

```ada
function "*"(Left, Right : T) return T
function "/"(Left, Right : T) return T

```

The following multiplication and division operators, with an operand of the predefined type Integer, are predefined for every specific fixed point type T: 

```ada
function "*"(Left : T; Right : Integer) return T
function "*"(Left : Integer; Right : T) return T
function "/"(Left : T; Right : Integer) return T

```

[All of the above multiplying operators are usable with an operand of an appropriate universal numeric type.] The following additional multiplying operators for root_real are predefined[, and are usable when both operands are of an appropriate universal or root numeric type, and the result is allowed to be of type root_real, as in a [number_declaration](./AA-3.3#S0034)]: 

Ramification: These operators are analogous to the multiplying operators involving fixed or floating point types where root_real substitutes for the fixed or floating point type, and root_integer substitutes for Integer. Only values of the corresponding universal numeric types are implicitly convertible to these root numeric types, so these operators are really restricted to use with operands of a universal type, or the specified root numeric types. 

```ada
function "*"(Left, Right : root_real) return root_real
function "/"(Left, Right : root_real) return root_real

```

```ada
function "*"(Left : root_real; Right : root_integer) return root_real
function "*"(Left : root_integer; Right : root_real) return root_real
function "/"(Left : root_real; Right : root_integer) return root_real

```

Multiplication and division between any two fixed point types are provided by the following two predefined operators: 

Ramification: {AI12-0005-1} Universal_fixed is the universal type for the class of fixed point types, meaning that these operators take operands of any fixed point type (not necessarily the same) and return a result that is implicitly (or explicitly) convertible to any fixed point type. 

```ada
function "*"(Left, Right : universal_fixed) return universal_fixed
function "/"(Left, Right : universal_fixed) return universal_fixed

```


#### Name Resolution Rules

{AI95-00364-01} {AI95-00420-01} The above two fixed-fixed multiplying operators shall not be used in a context where the expected type for the result is itself universal_fixed [- the context has to identify some other numeric type to which the result is to be converted, either explicitly or implicitly]. Unless the predefined universal operator is identified using an expanded name with [prefix](./AA-4.1#S0093) denoting the package Standard, an explicit conversion is required on the result when using the above fixed-fixed multiplication operator if either operand is of a type having a user-defined primitive multiplication operator such that: 

{AI05-0020-1} {AI05-0209-1} it is declared immediately within the same declaration list as the type or any partial or incomplete view thereof; and

both of its formal parameters are of a fixed-point type. 

{AI95-00364-01} {AI95-00420-01} A corresponding requirement applies to the universal fixed-fixed division operator.

Discussion: The small of universal_fixed is infinitesimal; no loss of precision is permitted. However, fixed-fixed division is impractical to implement when an exact result is required, and multiplication will sometimes result in unanticipated overflows in such circumstances, so we require an explicit conversion to be inserted in expressions like A * B * C if A, B, and C are each of some fixed point type.

On the other hand, X := A * B; is permitted by this rule, even if X, A, and B are all of different fixed point types, since the expected type for the result of the multiplication is the type of X, which is necessarily not universal_fixed.

{AI95-00364-01} {AI95-00420-01} We have made these into Name Resolution rules to ensure that user-defined primitive fixed-fixed operators are not made unusable due to the presence of these universal fixed-fixed operators. But we do allow these operators to be used if prefixed by package Standard, so that they can be used in the definitions of user-defined operators. 

Paragraph 20 was deleted. 


#### Dynamic Semantics

The multiplication and division operators for real types have their conventional meaning. [For floating point types, the accuracy of the result is determined by the precision of the result type. For decimal fixed point types, the result is truncated toward zero if the mathematical result is between two multiples of the small of the specific result type (possibly determined by context); for ordinary fixed point types, if the mathematical result is between two multiples of the small, it is unspecified which of the two is the result. ]

The exception Constraint_Error is raised by integer division, rem, and mod if the right operand is zero. [Similarly, for a real type T with T'Machine_Overflows True, division by zero raises Constraint_Error.] 

NOTE 1   For positive A and B, A/B is the quotient and A rem B is the remainder when A is divided by B. The following relations are satisfied by the rem operator: 

```ada
     A  rem (-B) =   A rem B
   (-A) rem   B  = -(A rem B)

```

NOTE 2   For any signed integer K, the following identity holds: 

```ada
   A mod B   =   (A + K*B) mod B

```

The relations between signed integer division, remainder, and modulus are illustrated by the following table: 

```ada
   A      B   A/B   A rem B  A mod B     A     B    A/B   A rem B   A mod B

```

```ada
   10     5    2       0        0       -10    5    -2       0         0
   11     5    2       1        1       -11    5    -2      -1         4
   12     5    2       2        2       -12    5    -2      -2         3
   13     5    2       3        3       -13    5    -2      -3         2
   14     5    2       4        4       -14    5    -2      -4         1

```

```ada
   A      B   A/B   A rem B  A mod B     A     B    A/B   A rem B   A mod B

   10    -5   -2       0        0       -10   -5     2       0         0
   11    -5   -2       1       -4       -11   -5     2      -1        -1
   12    -5   -2       2       -3       -12   -5     2      -2        -2
   13    -5   -2       3       -2       -13   -5     2      -3        -3
   14    -5   -2       4       -1       -14   -5     2      -4        -4

```


#### Examples

Examples of expressions involving multiplying operators: 

```ada
I : Integer := 1;
J : Integer := 2;
K : Integer := 3;

```

```ada
X : Real := 1.0;                      --     see 3.5.7
Y : Real := 2.0;

```

```ada
F : Fraction := 0.25;                 --     see 3.5.9
G : Fraction := 0.5;

```

```ada
Expression  	Value  	Result Type

I*J            	2      	same as I and J, that is, Integer
K/J            	1      	same as K and J, that is, Integer
K mod J  	1      	same as K and J, that is, Integer

X/Y            	0.5    	same as X and Y, that is, Real
F/2            	0.125  	same as F, that is, Fraction

3*F            	0.75   	same as F, that is, Fraction
0.75*G         	0.375  	universal_fixed, implicitly convertible
               	       	to any fixed point type
Fraction(F*G)  	0.125  	Fraction, as stated by the conversion
Real(J)*Y      	4.0    	Real, the type of both operands after
               	       	conversion of J

```


#### Incompatibilities With Ada 83

{AI95-00364-01} {AI95-00420-01} The universal fixed-fixed multiplying operators are now directly available (see below). Any attempt to use user-defined fixed-fixed multiplying operators will be ambiguous with the universal ones. The only way to use the user-defined operators is to fully qualify them in a prefix call. This problem was not documented during the design of Ada 95, and has been mitigated by Ada 2005. 


#### Extensions to Ada 83

Explicit conversion of the result of multiplying or dividing two fixed point numbers is no longer required, provided the context uniquely determines some specific fixed point result type. This is to improve support for decimal fixed point, where requiring explicit conversion on every fixed-fixed multiply or divide was felt to be inappropriate.

The type universal_fixed is covered by universal_real, so real literals and fixed point operands may be multiplied or divided directly, without any explicit conversions required. 


#### Wording Changes from Ada 83

We have used the normal syntax for function definition rather than a tabular format. 


#### Incompatibilities With Ada 95

{AI95-00364-01} We have changed the resolution rules for the universal fixed-fixed multiplying operators to remove the incompatibility with Ada 83 discussed above. The solution is to hide the universal operators in some circumstances. As a result, some legal Ada 95 programs will require the insertion of an explicit conversion around a fixed-fixed multiply operator. This change is likely to catch as many bugs as it causes, since it is unlikely that the user wanted to use predefined operators when they had defined user-defined versions. 


#### Wording Changes from Ada 2005

{AI05-0020-1} {AI05-0209-1} Correction: Wording was added to clarify that universal_fixed "*" and "/" does not apply if an appropriate operator is declared for a partial (or incomplete) view of the designated type. Otherwise, adding a partial (or incomplete) view could make some "*" and "/" operators ambiguous.

{AI05-0260-1} Correction: The wording for the mod operator was corrected so that a result of 0 does not have to have "the sign of B" (which is impossible if B is negative). 


## 4.5.6  Highest Precedence Operators


#### Static Semantics

The highest precedence unary operator abs (absolute value) is predefined for every specific numeric type T, with the following specification: 

```ada
function "abs"(Right : T) return T

```

The highest precedence unary operator not (logical negation) is predefined for every boolean type T, every modular type T, and for every one-dimensional array type T whose components are of a boolean type, with the following specification: 

```ada
function "not"(Right : T) return T

```

The result of the operator not for a modular type is defined as the difference between the high bound of the base range of the type and the value of the operand. [For a binary modulus, this corresponds to a bit-wise complement of the binary representation of the value of the operand.]

The operator not that applies to a one-dimensional array of boolean components yields a one-dimensional boolean array with the same bounds; each component of the result is obtained by logical negation of the corresponding component of the operand (that is, the component that has the same index value). A check is made that each component of the result belongs to the component subtype; the exception Constraint_Error is raised if this check fails. 

Discussion: The check against the component subtype is per AI83-00535. 

The highest precedence exponentiation operator ** is predefined for every specific integer type T with the following specification: 

```ada
function "**"(Left : T; Right : Natural) return T

```

Exponentiation is also predefined for every specific floating point type as well as root_real, with the following specification (where T is root_real or the floating point type): 

```ada
function "**"(Left : T; Right : Integer'Base) return T

```

{AI05-0088-1} The right operand of an exponentiation is the exponent. The value of X**N with the value of the exponent N positive is the same as the value of X*X*...X (with N1 multiplications) except that the multiplications are associated in an arbitrary order. With N equal to zero, the result is one. With the value of N negative [(only defined for a floating point operand)], the result is the reciprocal of the result using the absolute value of N as the exponent. 

Ramification: The language does not specify the order of association of the multiplications inherent in an exponentiation. For a floating point type, the accuracy of the result might depend on the particular association order chosen. 


#### Implementation Permissions

The implementation of exponentiation for the case of a negative exponent is allowed to raise Constraint_Error if the intermediate result of the repeated multiplications is outside the safe range of the type, even though the final result (after taking the reciprocal) would not be. (The best machine approximation to the final result in this case would generally be 0.0.) 

NOTE 1   As implied by the specification given above for exponentiation of an integer type, a check is made that the exponent is not negative. Constraint_Error is raised if this check fails. 


#### Inconsistencies With Ada 83

{8652/0100} {AI95-00018-01} The definition of "**" allows arbitrary association of the multiplications which make up the result. Ada 83 required left-to-right associations (confirmed by AI83-00137). Thus it is possible that "**" would provide a slightly different (and more potentially accurate) answer in Ada 95 than in the same Ada 83 program. 


#### Wording Changes from Ada 83

We now show the specification for "**" for integer types with a parameter subtype of Natural rather than Integer for the exponent. This reflects the fact that Constraint_Error is raised if a negative value is provided for the exponent. 


#### Wording Changes from Ada 2005

{AI05-0088-1} Correction: The equivalence definition for "**" was corrected so that it does not imply that the operands are evaluated multiple times. 


## 4.5.7  Conditional Expressions

{AI05-0147-1} {AI05-0188-1} {AI05-0262-1} A [conditional_expression](./AA-4.5#S0148) selects for evaluation at most one of the enclosed dependent_[expression](./AA-4.4#S0132)s, depending on a decision among the alternatives. One kind of [conditional_expression](./AA-4.5#S0148) is the [if_expression](./AA-4.5#S0149), which selects for evaluation a dependent_[expression](./AA-4.4#S0132) depending on the value of one or more corresponding conditions. The other kind of [conditional_expression](./AA-4.5#S0148) is the [case_expression](./AA-4.5#S0151), which selects for evaluation one of a number of alternative dependent_[expression](./AA-4.4#S0132)s; the chosen alternative is determined by the value of a selecting_[expression](./AA-4.4#S0132). 


#### Language Design Principles

{AI05-0188-1} As previously noted, there are two kinds of [conditional_expression](./AA-4.5#S0148), [if_expression](./AA-4.5#S0149)s and [case_expression](./AA-4.5#S0151)s. Whenever possible, we have written the rules in terms of [conditional_expression](./AA-4.5#S0148)s to avoid duplication.

{AI05-0147-1} The rules for [conditional_expression](./AA-4.5#S0148)s have been designed as much as possible to work similarly to a parenthesized expression. The intent is that as much as possible, wherever a parenthesized expression would be allowed, a [conditional_expression](./AA-4.5#S0148) would be allowed, and it should work the same way. 


#### Syntax

{AI05-0188-1} conditional_expression<a id="S0148"></a> ::= [if_expression](./AA-4.5#S0149) | [case_expression](./AA-4.5#S0151)

{AI05-0147-1} {AI05-0188-1} if_expression<a id="S0149"></a> ::= 
   if [condition](./AA-4.5#S0150) then dependent_[expression](./AA-4.4#S0132)
   {elsif [condition](./AA-4.5#S0150) then dependent_[expression](./AA-4.4#S0132)}
   [else dependent_[expression](./AA-4.4#S0132)]

{AI05-0147-1} condition<a id="S0150"></a> ::= boolean_[expression](./AA-4.4#S0132)

{AI05-0188-1} case_expression<a id="S0151"></a> ::= 
    case selecting_[expression](./AA-4.4#S0132) is
    [case_expression_alternative](./AA-4.5#S0152) {,
    [case_expression_alternative](./AA-4.5#S0152)}

{AI05-0188-1} case_expression_alternative<a id="S0152"></a> ::= 
    when [discrete_choice_list](./AA-3.8#S0073) =&gt
        dependent_[expression](./AA-4.4#S0132)

{AI05-0147-1} Wherever the Syntax Rules allow an [expression](./AA-4.4#S0132), a [conditional_expression](./AA-4.5#S0148) may be used in place of the [expression](./AA-4.4#S0132), so long as it is immediately surrounded by parentheses.

Discussion: {AI05-0147-1} The syntactic category [conditional_expression](./AA-4.5#S0148) appears only as a primary that is parenthesized. The above rule allows it to additionally be used in other contexts where it would be directly surrounded by parentheses.

The grammar makes the following directly legal:

```ada
A := (if X then Y else Z); -- parentheses required
A := B + (if X then Y else Z) + C; -- parentheses required

```

The following procedure calls are syntactically legal; the first uses the above rule to eliminate the redundant parentheses found in the second:

```ada
P(if X then Y else Z);
P((if X then Y else Z)); -- redundant parentheses

```

```ada
P((if X then Y else Z), Some_Other_Param);
P(Some_Other_Param, (if X then Y else Z));
P(Formal =&gt (if X then Y else Z));

```

whereas the following are illegal:

```ada
P(if X then Y else Z, Some_Other_Param);
P(Some_Other_Param, if X then Y else Z);
P(Formal =&gt if X then Y else Z);

```

because in these latter cases, the [conditional_expression](./AA-4.5#S0148) is not immediately surrounded by parentheses (which means on both sides!).

The English-language rule applies in all places that could surround an expression with parentheses, including pragma arguments, type conversion and qualified expression operands, and array index expressions.

This English-language rule could have been implemented instead by adding a nonterminal expression_within_parentheses, which would consist of [expression](./AA-4.4#S0132)s and [conditional_expression](./AA-4.5#S0148)s. Then, that could be used in all of the syntax which could consist of parens directly around an [expression](./AA-4.4#S0132). We did not do that because of the large amount of change required. A complete grammar is given in AI05-0147-1. 

Implementation Note: {AI05-0147-1} Implementers are cautioned to consider error detection when implementing the syntax for [conditional_expression](./AA-4.5#S0148)s. An [if_expression](./AA-4.5#S0149) and an [if_statement](./AA-5.3#S0175) are very similar syntactically, (as are a [case_expression](./AA-4.5#S0151) and a [case_statement](./AA-5.4#S0176)) and simple mistakes can appear to change one into the other, potentially causing errors to be moved far away from their actual location. The absence of end if to terminate an [if_expression](./AA-4.5#S0149) (and end case for a [case_expression](./AA-4.5#S0151)) also may make error handling harder. 


#### Name Resolution Rules

{AI05-0147-1} If a [conditional_expression](./AA-4.5#S0148) is expected to be of a type T, then each dependent_[expression](./AA-4.4#S0132) of the [conditional_expression](./AA-4.5#S0148) is expected to be of type T. Similarly, if a [conditional_expression](./AA-4.5#S0148) is expected to be of some class of types, then each dependent_[expression](./AA-4.4#S0132) of the [conditional_expression](./AA-4.5#S0148) is subject to the same expectation. If a [conditional_expression](./AA-4.5#S0148) shall resolve to be of a type T, then each dependent_[expression](./AA-4.4#S0132) shall resolve to be of type T.

{AI05-0147-1} The possible types of a [conditional_expression](./AA-4.5#S0148) are further determined as follows:

If the [conditional_expression](./AA-4.5#S0148) is the operand of a type conversion, the type of the [conditional_expression](./AA-4.5#S0148) is the target type of the conversion; otherwise,

Reason: This rule distributes an enclosing type conversion to the dependent_[expression](./AA-4.4#S0132)s. This means that 

```ada
T(if C then A else B)

```

has the same semantics as 

```ada
(if C then T(A) else T(B))

```

If all of the dependent_[expression](./AA-4.4#S0132)s are of the same type, the type of the [conditional_expression](./AA-4.5#S0148) is that type; otherwise,

If a dependent_[expression](./AA-4.4#S0132) is of an elementary type, the type of the [conditional_expression](./AA-4.5#S0148) shall be covered by that type; otherwise,

Reason: This rule supports the use of numeric literals and universal expressions within a [conditional_expression](./AA-4.5#S0148). 

If the [conditional_expression](./AA-4.5#S0148) is expected to be of type T or shall resolve to type T, then the [conditional_expression](./AA-4.5#S0148) is of type T.

Ramification: If the type of the [conditional_expression](./AA-4.5#S0148) cannot be determined by one of these rules, then Name Resolution has failed for that expression, even if the dependent_[expression](./AA-4.4#S0132)s would resolve individually. 

{AI05-0147-1} A [condition](./AA-4.5#S0150) is expected to be of any boolean type. 

{AI05-0188-1} The expected type for the selecting_[expression](./AA-4.4#S0132) and the [discrete_choice](./AA-3.8#S0074)s are as for case statements (see 5.4). 


#### Legality Rules

{AI05-0147-1} {AI05-0188-1} All of the dependent_[expression](./AA-4.4#S0132)s shall be convertible (see 4.6) to the type of the [conditional_expression](./AA-4.5#S0148).

{AI05-0147-1} {AI05-0188-1} {AI05-0269-1} If the expected type of a [conditional_expression](./AA-4.5#S0148) is a specific tagged type, all of the dependent_[expression](./AA-4.4#S0132)s of the [conditional_expression](./AA-4.5#S0148) shall be dynamically tagged, or none shall be dynamically tagged. In this case, the [conditional_expression](./AA-4.5#S0148) is dynamically tagged if all of the dependent_[expression](./AA-4.4#S0132)s are dynamically tagged, is tag-indeterminate if all of the dependent_[expression](./AA-4.4#S0132)s are tag-indeterminate, and is statically tagged otherwise.

{AI05-0147-1} {AI05-0262-1} If there is no else dependent_[expression](./AA-4.4#S0132), the [if_expression](./AA-4.5#S0149) shall be of a boolean type.

{AI05-0188-1} {AI05-0269-1} All Legality Rules that apply to the [discrete_choice](./AA-3.8#S0074)s of a [case_statement](./AA-5.4#S0176) (see 5.4) also apply to the [discrete_choice](./AA-3.8#S0074)s of a [case_expression](./AA-4.5#S0151) except within an instance of a generic unit. 

Reason: The exemption for a case expression that occurs in an instance allows the following example: 

```ada
generic
   with function Int_Func return Integer;
package G is
   X : Float := (case Int_Func is
                  when Integer'First .. -1 =&gt -1.0,
                  when 0 =&gt 0.0,
                  when Positive =&gt 1.0);
end G;

```

```ada
function Nat_Func return Natural is (123);

```

```ada
package I is new G (Int_Func =&gt Nat_Func); -- Legal

```

Note that the Legality Rules still apply in the generic unit itself; they are just not enforced in an instance of the unit. 


#### Dynamic Semantics

{AI05-0147-1} {AI05-0188-1} For the evaluation of an [if_expression](./AA-4.5#S0149), the [condition](./AA-4.5#S0150) specified after if, and any [condition](./AA-4.5#S0150)s specified after elsif, are evaluated in succession (treating a final else as elsif True then), until one evaluates to True or all [condition](./AA-4.5#S0150)s are evaluated and yield False. If a [condition](./AA-4.5#S0150) evaluates to True, the associated dependent_[expression](./AA-4.4#S0132) is evaluated, converted to the type of the [if_expression](./AA-4.5#S0149), and the resulting value is the value of the [if_expression](./AA-4.5#S0149). Otherwise (when there is no else clause), the value of the [if_expression](./AA-4.5#S0149) is True.

Ramification: Else is required unless the [if_expression](./AA-4.5#S0149) has a boolean type, so the last sentence can only apply to [if_expression](./AA-4.5#S0149)s with a boolean type. 

{AI05-0188-1} For the evaluation of a [case_expression](./AA-4.5#S0151), the selecting_[expression](./AA-4.4#S0132) is first evaluated. If the value of the selecting_[expression](./AA-4.4#S0132) is covered by the [discrete_choice_list](./AA-3.8#S0073) of some [case_expression_alternative](./AA-4.5#S0152), then the dependent_[expression](./AA-4.4#S0132) of the [case_expression_alternative](./AA-4.5#S0152) is evaluated, converted to the type of the [case_expression](./AA-4.5#S0151), and the resulting value is the value of the [case_expression](./AA-4.5#S0151). Otherwise (the value is not covered by any [discrete_choice_list](./AA-3.8#S0073), perhaps due to being outside the base range), Constraint_Error is raised. 


#### Examples

{AI12-0429-1} Example of use of an [if_expression](./AA-4.5#S0149):

```ada
{AI12-0312-1} Put_Line ("Casey is " &
      (if Casey.Sex = M then "Male" else "Female")); -- see 3.10.1

```

{AI12-0429-1} Example of use of a [case_expression](./AA-4.5#S0151):

```ada
{AI12-0312-1} function Card_Color (Card : Suit) return Color is -- see 3.5.1
  (case Card is
      when Clubs  | Spades   =&gt Black,
      when Hearts | Diamonds =&gt Red);

```


#### Extensions to Ada 2005

{AI05-0147-1} If expressions and case expressions are new. 


## 4.5.8  Quantified Expressions

{AI12-0158-1} Quantified expressions provide a way to write universally and existentially quantified predicates over containers and arrays. 


#### Syntax

{AI05-0176-1} quantified_expression<a id="S0153"></a> ::= for [quantifier](./AA-4.5#S0154) [loop_parameter_specification](./AA-5.5#S0181) =&gt [predicate](./AA-4.5#S0155)
  | for [quantifier](./AA-4.5#S0154) [iterator_specification](./AA-5.5#S0183) =&gt [predicate](./AA-4.5#S0155)

quantifier<a id="S0154"></a> ::= all | some

predicate<a id="S0155"></a> ::= boolean_[expression](./AA-4.4#S0132)

{AI05-0176-1} Wherever the Syntax Rules allow an [expression](./AA-4.4#S0132), a [quantified_expression](./AA-4.5#S0153) may be used in place of the [expression](./AA-4.4#S0132), so long as it is immediately surrounded by parentheses.

Discussion: The syntactic category [quantified_expression](./AA-4.5#S0153) appears only as a [primary](./AA-4.4#S0141) that is parenthesized. The above rule allows it to additionally be used in other contexts where it would be directly surrounded by parentheses. This is the same rule that is used for [conditional_expression](./AA-4.5#S0148)s; see 4.5.7 for a detailed discussion of the meaning and effects of this rule. 


#### Name Resolution Rules

{AI05-0176-1} The expected type of a [quantified_expression](./AA-4.5#S0153) is any Boolean type. The [predicate](./AA-4.5#S0155) in a [quantified_expression](./AA-4.5#S0153) is expected to be of the same type. 


#### Dynamic Semantics

{AI05-0176-1} {AI12-0158-1} {AI12-0327-1} For the evaluation of a [quantified_expression](./AA-4.5#S0153), the [loop_parameter_specification](./AA-5.5#S0181) or [iterator_specification](./AA-5.5#S0183) is first elaborated. The evaluation of a [quantified_expression](./AA-4.5#S0153) then performs an iteration, and evaluates the [predicate](./AA-4.5#S0155) for each value conditionally produced by the iteration (see 5.5 and 5.5.2).

Ramification: {AI12-0327-1} The order of evaluation of the predicates is that in which the values are produced, as specified in 5.5 or 5.5.2. 

{AI05-0176-1} The value of the [quantified_expression](./AA-4.5#S0153) is determined as follows:

{AI12-0158-1} If the [quantifier](./AA-4.5#S0154) is all, the expression is False if the evaluation of any [predicate](./AA-4.5#S0155) yields False; evaluation of the [quantified_expression](./AA-4.5#S0153) stops at that point. Otherwise (every predicate has been evaluated and yielded True), the expression is True. Any exception raised by evaluation of the [predicate](./AA-4.5#S0155) is propagated.

Ramification: The [expression](./AA-4.4#S0132) is True if the domain contains no values. 

{AI12-0158-1} If the [quantifier](./AA-4.5#S0154) is some, the expression is True if the evaluation of any [predicate](./AA-4.5#S0155) yields True; evaluation of the [quantified_expression](./AA-4.5#S0153) stops at that point. Otherwise (every predicate has been evaluated and yielded False), the expression is False. Any exception raised by evaluation of the [predicate](./AA-4.5#S0155) is propagated.

Ramification: The [expression](./AA-4.4#S0132) is False if the domain contains no values. 


#### Examples

{AI05-0176-1} {AI12-0429-1} Example of a quantified expression as a postcondition for a sorting routine on an array A with an index subtype T:

```ada
Post =&gt (A'Length &lt 2 or else
   (for all I in A'First .. T'Pred(A'Last) =&gt A (I) &lt= A (T'Succ (I))))

```

{AI05-0176-1} {AI12-0429-1} {AI12-0430-1} Example of use of a quantified expression as an assertion that a positive number N is composite (as opposed to prime):

```ada
{AI12-0312-1} pragma Assert (for some X in 2 .. N when X * X &lt= N =&gt N mod X = 0);
   -- see [iterator_filter](./AA-5.5#S0182) in 5.5

```


#### Extensions to Ada 2005

{AI05-0176-1} Quantified expressions are new. 


#### Wording Changes from Ada 2012

{AI12-0158-1} Corrigendum: Revised the wording to make it clear that the semantics is short-circuited, and what the result is when there are no values for the loop parameter. 


## 4.5.9  Declare Expressions

{AI12-0236-1} Declare expressions provide a way to declare local constants and object renamings in an expression context. 


#### Syntax

{AI12-0236-1} declare_expression<a id="S0156"></a> ::= 
    declare {[declare_item](./AA-4.5#S0157)}
    begin body_[expression](./AA-4.4#S0132)

{AI12-0236-1} declare_item<a id="S0157"></a> ::= [object_declaration](./AA-3.3#S0032) | [object_renaming_declaration](./AA-8.5#S0239)

Reason: We allow (declare begin [expression](./AA-4.4#S0132)) with no [declare_item](./AA-4.5#S0157)s, for uniformity with block statements, which also allow a pointless declare. 

{AI12-0236-1} Wherever the Syntax Rules allow an [expression](./AA-4.4#S0132), a [declare_expression](./AA-4.5#S0156) may be used in place of the [expression](./AA-4.4#S0132), so long as it is immediately surrounded by parentheses.

Discussion: The syntactic category [declare_expression](./AA-4.5#S0156) appears only as a [primary](./AA-4.4#S0141) that is parenthesized. The above rule allows it to additionally be used in other contexts where it would be directly surrounded by parentheses. This is the same rule that is used for [conditional_expression](./AA-4.5#S0148)s; see 4.5.7 for a detailed discussion of the meaning and effects of this rule. 


#### Legality Rules

{AI12-0236-1} A [declare_item](./AA-4.5#S0157) that is an [object_declaration](./AA-3.3#S0032) shall declare a constant of a nonlimited type.

Reason: We disallow limited objects to avoid the horror of task waiting in the middle of an expression. The solution used for controlled objects (wait until the entire expression is finished) isn't appropriate for tasks. This restriction also eliminates build-in-place complexities and the need to activate tasks found in a [declare_item](./AA-4.5#S0157). 

{AI12-0236-1} {AI12-0317-1} A [declare_item](./AA-4.5#S0157) that is an [object_renaming_declaration](./AA-8.5#S0239) (see 8.5.1) shall not rename an object of a limited type if any operative constituent of the object_[name](./AA-4.1#S0091) is a value conversion or is newly constructed (see 4.4).

Reason: We disallow renaming limited temporary objects (like the result of a function call) for the same reasons as for stand-alone objects. We do allow renaming existing limited objects because these don't cause any problems. Note that one cannot directly rename an [aggregate](./AA-4.3#S0106), parenthesized expression, [conditional_expression](./AA-4.5#S0148), or [declare_expression](./AA-4.5#S0156) (these are not [name](./AA-4.1#S0091)s), but any of these can appear inside of a [qualified_expression](./AA-4.7#S0163) or [type_conversion](./AA-4.6#S0162) (which are [name](./AA-4.1#S0091)s and thus can be renamed). 

{AI12-0236-1} The following are not allowed within a [declare_expression](./AA-4.5#S0156): a declaration containing the reserved word aliased; the [attribute_designator](./AA-4.1#S0101) Access or Unchecked_Access; or an anonymous access type.

Reason: We do not want to define accessibility rules for [declare_item](./AA-4.5#S0157)s, as nested [declare_expression](./AA-4.5#S0156)s cause complexities or usage warts. We want to keep this feature simple to use, understand, and implement. Thus, we do not allow any of the features that would require accessibility rules. 


#### Name Resolution Rules

{AI12-0236-1} If a [declare_expression](./AA-4.5#S0156) is expected to be of a type T, then the body_[expression](./AA-4.4#S0132) is expected to be of type T. Similarly, if a [declare_expression](./AA-4.5#S0156) is expected to be of some class of types, then the body_[expression](./AA-4.4#S0132) is subject to the same expectation. If a [declare_expression](./AA-4.5#S0156) shall resolve to be of a type T, then the body_[expression](./AA-4.4#S0132) shall resolve to be of type T.

{AI12-0236-1} The type of a [declare_expression](./AA-4.5#S0156) is the type of the body_[expression](./AA-4.4#S0132). 


#### Dynamic Semantics

{AI12-0236-1} For the evaluation of a [declare_expression](./AA-4.5#S0156), the [declare_item](./AA-4.5#S0157)s are elaborated in order, and then the body_[expression](./AA-4.4#S0132) is evaluated. The value of the [declare_expression](./AA-4.5#S0156) is that of the body_[expression](./AA-4.4#S0132). 


#### Examples

{AI12-0236-1} {AI12-0429-1} Example of use of a declare expression as a replacement postcondition for Ada.Containers.Vectors."&" (see A.18.2):

```ada
{AI12-0236-1} with Post =&gt
   (declare
      Result renames Vectors."&"'Result;
      Length : constant Count_Type := Left.Length + Right.Length;
    begin
      Result.Length = Length and then
      not Tampering_With_Elements_Prohibited (Result) and then
      not Tampering_With_Cursors_Prohibited (Result) and then
      Result.Capacity &gt= Length)

```


#### Extensions to Ada 2012

{AI12-0236-1} Declare expressions are new. 


## 4.5.10  Reduction Expressions

{AI12-0242-1} {AI12-0262-1} Reduction expressions provide a way to map or transform a collection of values into a new set of values, and then summarize the values produced by applying an operation to reduce the set to a single value result. A reduction expression is represented as an [attribute_reference](./AA-4.1#S0100) of the reduction attributes Reduce or Parallel_Reduce.

Glossary entry: A reduction expression is an expression that defines how to map or transform a collection of values into a new set of values, and then summarize the values by applying an operation to reduce the set to a single value.

Version=[5],Kind=(AddedNormal),Group=[C],Term=[reduction expression], Def=[an expression that defines how to map or transform a collection of values into a new set of values, and then summarize the values by applying an operation to reduce the set to a single value] 


#### Syntax

{AI12-0242-1} {AI12-0262-1} reduction_attribute_reference<a id="S0158"></a> ::= 
    [value_sequence](./AA-4.5#S0159)'[reduction_attribute_designator](./AA-4.5#S0160)
  | [prefix](./AA-4.1#S0093)'[reduction_attribute_designator](./AA-4.5#S0160)

{AI12-0262-1} {AI12-0355-2} value_sequence<a id="S0159"></a> ::= 
    '[' [parallel[([chunk_specification](./AA-5.5#S0180))] [[aspect_specification](./AA-13.1#S0346)]]
        [iterated_element_association](./AA-4.3#S0131) ']'

{AI12-0262-1} reduction_attribute_designator<a id="S0160"></a> ::= reduction_[identifier](./AA-2.3#S0002)([reduction_specification](./AA-4.5#S0161))

{AI12-0262-1} {AI12-0348-1} reduction_specification<a id="S0161"></a> ::= reducer_[name](./AA-4.1#S0091), initial_value_[expression](./AA-4.4#S0132)

{AI12-0250-1} {AI12-0262-1} The [iterated_element_association](./AA-4.3#S0131) of a [value_sequence](./AA-4.5#S0159) shall not have a key_[expression](./AA-4.4#S0132), nor shall it have a [loop_parameter_specification](./AA-5.5#S0181) that has the reserved word reverse.

Reason: The intent is that the syntax matches as closely as possible array or container aggregate notation. Syntax that matches a [loop_parameter_specification](./AA-5.5#S0181) with the reverse reserved word would not be permitted in an array aggregate, so we disallow that here. 

{AI12-0262-1} The [chunk_specification](./AA-5.5#S0180), if any, of a [value_sequence](./AA-4.5#S0159) shall be an integer_[simple_expression](./AA-4.4#S0138).


#### Name Resolution Rules

{AI12-0262-1} The expected type for a [reduction_attribute_reference](./AA-4.5#S0158) shall be a single nonlimited type.

{AI12-0262-1} In the remainder of this subclause, we will refer to nonlimited subtypes Value_Type and Accum_Type of a [reduction_attribute_reference](./AA-4.5#S0158). These subtypes and interpretations of the [name](./AA-4.1#S0091)s and [expression](./AA-4.4#S0132)s of a [reduction_attribute_reference](./AA-4.5#S0158) are determined by the following rules:

Discussion: Accum_Type represents the result of the reduction (the accumulator type), and Value_Type represents the type of the input values to the reduction. 

{AI12-0262-1} Accum_Type is a subtype of the expected type of the [reduction_attribute_reference](./AA-4.5#S0158).

{AI12-0262-1} A reducer subprogram is subtype conformant with one of the following specifications:

```ada
   function Reducer(Accumulator : Accum_Type;
                    Value : Value_Type) return Accum_Type;

```

```ada
   procedure Reducer(Accumulator : in out Accum_Type;
                     Value : in Value_Type);

```

{AI12-0262-1} The reducer_[name](./AA-4.1#S0091) of a [reduction_specification](./AA-4.5#S0161) denotes a reducer subprogram.

{AI12-0262-1} The expected type of an initial_value_[expression](./AA-4.4#S0132) of a [reduction_specification](./AA-4.5#S0161) is that of subtype Accum_Type.

{AI12-0250-1} {AI12-0262-1} The expected type of the [expression](./AA-4.4#S0132) of the [iterated_element_association](./AA-4.3#S0131) of a [value_sequence](./AA-4.5#S0159) is that of subtype Value_Type. 


#### Legality Rules

{AI12-0262-1} {AI12-0348-1} If the [reduction_attribute_reference](./AA-4.5#S0158) has a [value_sequence](./AA-4.5#S0159) with the reserved word parallel, the subtypes Accum_Type and Value_Type shall statically match.

{AI12-0262-1} {AI12-0348-1} If the [identifier](./AA-2.3#S0002) of a [reduction_attribute_designator](./AA-4.5#S0160) is Parallel_Reduce, the subtypes Accum_Type and Value_Type shall statically match.

Reason: For a [reduction_attribute_reference](./AA-4.5#S0158) with a [value_sequence](./AA-4.5#S0159) that does not have the reserved word parallel or has a [prefix](./AA-4.1#S0093) and the [identifier](./AA-2.3#S0002) of the [reduction_attribute_designator](./AA-4.5#S0160) is Reduce, the subtypes Accum_Type and Value_Type can be different because only one logical thread of control is presumed so there is no need to combine multiple results. 


#### Static Semantics

{AI12-0262-1} A [reduction_attribute_reference](./AA-4.5#S0158) denotes a value, with its nominal subtype being the subtype of the first parameter of the subprogram denoted by the reducer_[name](./AA-4.1#S0091). 


#### Dynamic Semantics

{AI12-0250-1} {AI12-0262-1} {AI12-0327-1} {AI12-0355-2} For the evaluation of a [value_sequence](./AA-4.5#S0159), the [iterated_element_association](./AA-4.3#S0131), the [chunk_specification](./AA-5.5#S0180), and the [aspect_specification](./AA-13.1#S0346), if any, are elaborated in an arbitrary order. Next an iteration is performed, and for each value conditionally produced by the iteration (see 5.5 and 5.5.2), the associated [expression](./AA-4.4#S0132) is evaluated with the loop parameter having this value, which produces a result that is converted to Value_Type and is used to define the next value in the sequence.

{AI12-0262-1} If the [value_sequence](./AA-4.5#S0159) does not have the reserved word parallel, it is produced as a single sequence of values by a single logical thread of control. If the reserved word parallel is present in the [value_sequence](./AA-4.5#S0159), the enclosing [reduction_attribute_reference](./AA-4.5#S0158) is a parallel construct, and the sequence of values is generated by a parallel iteration (as defined in 5.5, 5.5.1, and 5.5.2), as a set of non-empty, non-overlapping contiguous chunks (subsequences) with one logical thread of control (see clause 9) associated with each subsequence. If there is a [chunk_specification](./AA-5.5#S0180), it determines the maximum number of chunks, as defined in 5.5; otherwise the maximum number of chunks is implementation defined.

Implementation defined: The maximum number of chunks for a parallel reduction expression without a [chunk_specification](./AA-5.5#S0180).

{AI12-0262-1} For a [value_sequence](./AA-4.5#S0159) V, the following attribute is defined:

V'Reduce(Reducer, Initial_Value){AI12-0262-1} {AI12-0348-1} This attribute represents a reduction expression, and is in the form of a [reduction_attribute_reference](./AA-4.5#S0158). 

{AI12-0262-1} {AI12-0348-1} The evaluation of a use of this attribute begins by evaluating the parts of the [reduction_attribute_designator](./AA-4.5#S0160) (the reducer_[name](./AA-4.1#S0091) Reducer and the initial_value_[expression](./AA-4.4#S0132) Initial_Value), in an arbitrary order. It then initializes the accumulator of the reduction expression to the value of the initial_value_[expression](./AA-4.4#S0132) (the initial value).  The [value_sequence](./AA-4.5#S0159) V is then evaluated.

{AI12-0262-1} {AI12-0348-1} If the [value_sequence](./AA-4.5#S0159) does not have the reserved word parallel, each value of the [value_sequence](./AA-4.5#S0159) is passed, in order, as the second (Value) parameter to a call on Reducer, with the first (Accumulator) parameter being the prior value of the accumulator, saving the result as the new value of the accumulator. The reduction expression yields the final value of the accumulator.

{AI12-0262-1} If the reserved word parallel is present in a [value_sequence](./AA-4.5#S0159), then the (parallel) reduction expression is a parallel construct and the sequence has been partitioned into one or more subsequences (see above) each with its own separate logical thread of control.

{AI12-0262-1} {AI12-0348-1} Each logical thread of control creates a local accumulator for processing its subsequence. The accumulator for a subsequence is initialized to the first value of the subsequence, and calls on Reducer start with the second value of the subsequence (if any). The result for the subsequence is the final value of its local accumulator.

{AI12-0262-1} {AI12-0348-1} After all logical threads of control of a parallel reduction expression have completed, Reducer is called for each subsequence, in the original sequence order, passing the local accumulator for that subsequence as the second (Value) parameter, and the overall accumulator [(initialized above to the initial value)] as the first (Accumulator) parameter, with the result saved back in the overall accumulator. The parallel reduction expression yields the final value of the overall accumulator.

{AI12-0262-1} If the evaluation of the [value_sequence](./AA-4.5#S0159) yields an empty sequence of values, the reduction expression yields the initial value.

{AI12-0262-1} {AI12-0348-1} If an exception is propagated by one of the calls on Reducer, that exception is propagated from the reduction expression. If different exceptions are propagated in different logical threads of control, one is chosen arbitrarily to be propagated from the reduction expression as a whole.

Implementation Note: {AI12-0262-1} For a [reduction_attribute_reference](./AA-4.5#S0158) that has a [value_sequence](./AA-4.5#S0159) without the reserved word parallel or a [prefix](./AA-4.1#S0093) where the [identifier](./AA-2.3#S0002) of the [reduction_attribute_designator](./AA-4.5#S0160) is Reduce (see below), generally the compiler can still choose to execute the reduction in parallel, presuming doing so would not change the results. However sequential execution is necessary if the subtypes of the parameters of Reducer do not statically match, since there is no subprogram identified in the construct that could be used for combining the results in parallel. 

Discussion: {AI12-0262-1} We say the calls to Reducer that combine the results of parallel execution are sequentially ordered in increasing order because certain reductions, such as vector concatenation, can be non-commutative (but still associative) operations. In order to return a deterministic result for parallel execution that is consistent with sequential execution, we need to specify an order for the iteration, and for the combination of results from the logical threads of control. It is also necessary that combining calls to Reducer are issued sequentially with respect to each other, which may require extra synchronization if the calls to Reducer are being executed by different logical threads of control. 

{AI12-0242-1} For a [prefix](./AA-4.1#S0093) X of an array type[ (after any implicit dereference)], or that denotes an iterable container object (see 5.5.1), the following attributes are defined:

X'Reduce(Reducer, Initial_Value){AI12-0242-1} {AI12-0348-1} X'Reduce is a reduction expression that yields a result equivalent to replacing the [prefix](./AA-4.1#S0093) of the attribute with the [value_sequence](./AA-4.5#S0159): 

```ada
[for Item of X =&gt Item]

```

X'Parallel_Reduce(Reducer, Initial_Value){AI12-0242-1} {AI12-0348-1} X'Parallel_Reduce is a reduction expression that yields a result equivalent to replacing the attribute [identifier](./AA-2.3#S0002) with Reduce and the [prefix](./AA-4.1#S0093) of the attribute with the [value_sequence](./AA-4.5#S0159): 

```ada
[parallel for Item of X =&gt Item]

```


#### Bounded (Run-Time) Errors

{AI12-0262-1} {AI12-0348-1} For a parallel reduction expression, it is a bounded error if the reducer subprogram is not associative. That is, for any arbitrary values of subtype Value_Type A, B, C and a reducer function R, the result of R (A, R (B, C)) should produce a result equal to R (R (A, B), C)); it is a bounded error if R does not. The possible consequences are Program_Error, or a result that does not match the equivalent sequential reduction expression due to the order of calls on the reducer subprogram being unspecified in the overall reduction. Analogous rules apply in the case of a reduction procedure.

Reason: In a sequential reduction expression, the reducer subprogram is called in a left-to-right order, whereas in a parallel reduction expression, the reducer subprogram is called in an order that depends on the number of logical threads of control that execute the reduction and on the elements/components given to each chunk. If the reducer is associative, this order does not matter, but in other cases, very different results are possible. While one can specify the maximum number of chunks, the actual number of chunks is unspecified. Similarly, the split of elements has only weak requirements. Thus, to get a consistent and portable result, an associative reducer is required for a parallel reduction. We define this as a Bounded (Run-Time) Errors to provide a stern warning about the required nature of the reducer subprogram and to let compilers detect the problem when possible. 

To be honest: In this rule, "equal" means semantically equal. We don't care if the bit patterns differ but that the results mean the same thing. In particular, if the primitive equal is user-defined, that equality would be the one used to determine if this rule is violated. 


#### Examples

{AI12-0262-1} {AI12-0429-1} Example of an expression function that returns its result as a reduction expression:

```ada
function Factorial(N : Natural) return Natural is
   ([for J in 1..N =&gt J]'Reduce("*", 1));

```

{AI12-0262-1} {AI12-0429-1} Example of a reduction expression that computes the Sine of X using a Taylor expansion:

```ada
function Sine (X : Float; Num_Terms : Positive := 5) return Float is
   ([for I in 1..Num_Terms =&gt
      (-1.0)**(I-1) * X**(2*I-1)/Float(Factorial(2*I-1))]'Reduce("+", 0.0));

```

{AI12-0262-1} {AI12-0379-1} {AI12-0429-1} Example of a reduction expression that outputs the sum of squares:

```ada
Put_Line ("Sum of Squares is" &
          Integer'Image([for I in 1 .. 10 =&gt I**2]'Reduce("+", 0)));

```

{AI12-0262-1} {AI12-0379-1} {AI12-0429-1} Example of a reduction expression used to compute the value of Pi:

```ada
--  See 3.5.7.
function Pi (Number_Of_Steps : Natural := 10_000) return Real is
  (1.0 / Real (Number_Of_Steps) *
    [for I in 1 .. Number_Of_Steps =&gt
        (4.0 / (1.0 + ((Real (I) - 0.5) *
           (1.0 / Real (Number_Of_Steps)))**2))]
              'Reduce("+", 0.0));

```

{AI12-0242-1} {AI12-0429-1} Example of a reduction expression used to calculate the sum of elements of an array of integers:

```ada
A'Reduce("+",0)  -- See 4.3.3.

```

{AI12-0242-1} {AI12-0429-1} Example of a reduction expression used to determine if all elements in a two-dimensional array of booleans are set to true:

```ada
Grid'Reduce("and", True)  -- See 3.6.

```

{AI12-0242-1} {AI12-0429-1} Example of a reduction expression used to calculate the minimum value of an array of integers in parallel:

```ada
A'Parallel_Reduce(Integer'Min, Integer'Last)

```

{AI12-0312-1} {AI12-0429-1} Example of a parallel reduction expression used to calculate the mean of the elements of a two-dimensional array of subtype Matrix (see 3.6) that are greater than 100.0:

```ada
type Accumulator is record
   Sum   : Real; -- See 3.5.7.
   Count : Integer;
end record;

```

```ada
function Accumulate (L, R : Accumulator) return Accumulator is
  (Sum   =&gt L.Sum   + R.Sum,
   Count =&gt L.Count + R.Count);

```

```ada
function Average_of_Values_Greater_Than_100 (M : Matrix) return Real is
   (declare
       Acc : constant Accumulator :=
          [parallel for Val of M when Val &gt 100.0 =&gt (Val, 1)]
             'Reduce(Accumulate, (Sum =&gt 0, Count =&gt 0));
    begin
       Acc.Sum / Real(Acc.Count));

```


#### Extensions to Ada 2012

{AI12-0242-1} {AI12-0262-1} {AI12-0348-1} Reduction expressions attributes are new. 

