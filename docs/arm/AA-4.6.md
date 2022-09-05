---
sidebar_position:  34
---

# 4.6  Type Conversions

{AI05-0299-1} [Explicit type conversions, both value conversions and view conversions, are allowed between closely related types as defined below. This subclause also defines rules for value and view conversions to a particular subtype of a type, both explicit ones and those implicit in other constructs. ]


#### Syntax

type_conversion<a id="S0162"></a> ::= 
    [subtype_mark](./AA-3.2#S0028)([expression](./AA-4.4#S0132))
  | [subtype_mark](./AA-3.2#S0028)([name](./AA-4.1#S0091))

The target subtype of a [type_conversion](./AA-4.6#S0162) is the subtype denoted by the [subtype_mark](./AA-3.2#S0028). The operand of a [type_conversion](./AA-4.6#S0162) is the [expression](./AA-4.4#S0132) or [name](./AA-4.1#S0091) within the parentheses; its type is the operand type.

{AI05-0299-1} One type is convertible to a second type if a [type_conversion](./AA-4.6#S0162) with the first type as operand type and the second type as target type is legal according to the rules of this subclause. Two types are convertible if each is convertible to the other. 

Ramification: Note that "convertible" is defined in terms of legality of the conversion. Whether the conversion would raise an exception at run time is irrelevant to this definition. 

{8652/0017} {AI95-00184-01} {AI95-00330-01} {AI12-0392-1} A [type_conversion](./AA-4.6#S0162) is called a view conversion if both its target type and operand type are tagged, or if it appears in a call as an actual parameter of mode out or in out; other [type_conversion](./AA-4.6#S0162)s are called value conversions. 

Ramification: A view conversion to a tagged type can appear in any context that requires an object [name](./AA-4.1#S0091), including in an object renaming, the [prefix](./AA-4.1#S0093) of a [selected_component](./AA-4.1#S0098), and if the operand is a variable, on the left side of an [assignment_statement](./AA-5.2#S0173). View conversions to other types only occur as actual parameters. Allowing view conversions of untagged types in all contexts seemed to incur an undue implementation burden.

{AI95-00330-01} A type conversion appearing as an in out parameter in a generic instantiation is not a view conversion; the second part of the rule only applies to subprogram calls, not instantiations. 


#### Name Resolution Rules

The operand of a [type_conversion](./AA-4.6#S0162) is expected to be of any type. 

Discussion: This replaces the "must be determinable" wording of Ada 83. This is equivalent to (but hopefully more intuitive than) saying that the operand of a [type_conversion](./AA-4.6#S0162) is a "complete context". 

The operand of a view conversion is interpreted only as a [name](./AA-4.1#S0091); the operand of a value conversion is interpreted as an [expression](./AA-4.4#S0132). 

Reason: {AI12-0005-1} This formally resolves the syntactic ambiguity between the two forms of [type_conversion](./AA-4.6#S0162). This matters because an [expression](./AA-4.4#S0132) that is a [name](./AA-4.1#S0091) is evaluated and represents a value while a [name](./AA-4.1#S0091) by itself can be an object; we want a view conversion to be an object. 

Ramification: {AI12-0005-1} This wording uses "interpreted as" rather than "shall be" so that this rule is not used to resolve overloading; it is solely about evaluation as described above. 


#### Legality Rules

{AI95-00251-01} In a view conversion for an untagged type, the target type shall be convertible (back) to the operand type. 

Reason: Untagged view conversions appear only as [in] out parameters. Hence, the reverse conversion must be legal as well. The forward conversion must be legal even for an out parameter, because (for example) actual parameters of an access type are always copied in anyway. 

Paragraphs 9 through 20 were reorganized and moved below. 

Discussion: {AI95-00251-01} The entire Legality Rules section has been reorganized to eliminate an unintentional incompatibility with Ada 83. In rare cases, a type conversion between two types related by derivation is not allowed by Ada 95, while it is allowed in Ada 83. The reorganization fixes this. Much of the wording of the legality section is unchanged, but it is reordered and reformatted. Because of the limitations of our tools, we had to delete and replace nearly the entire section. The text of Ada 95 paragraphs 8 through 12, 14, 15, 17, 19, 20, and 24 are unchanged (just moved); these are now 24.1 through 24.5, 24.12, 24.13, 24.17, 24.19, 24.20, and 8. 

{AI95-00251-01} {AI05-0115-1} If there is a type (other than a root numeric type) that is an ancestor of both the target type and the operand type, or both types are class-wide types, then at least one of the following rules shall apply: 

{AI95-00251-01} The target type shall be untagged; or

The operand type shall be covered by or descended from the target type; or 

Ramification: This is a conversion toward the root, which is always safe. 

{AI95-00251-01} The operand type shall be a class-wide type that covers the target type; or 

Ramification: This is a conversion of a class-wide type toward the leaves, which requires a tag check. See Dynamic Semantics.

{AI95-00251-01} These two rules imply that a conversion from an ancestor type to a type extension is not permitted, as this would require specifying the values for additional components, in general, and changing the tag. An [extension_aggregate](./AA-4.3#S0111) has to be used instead, constructing a new value, rather than converting an existing value. However, a conversion from the class-wide type rooted at an ancestor type is permitted; such a conversion just verifies that the operand's tag is a descendant of the target. 

{AI95-00251-01} The operand and target types shall both be class-wide types and the specific type associated with at least one of them shall be an interface type. 

Ramification: We allow converting any class-wide type T'Class to or from a class-wide interface type even if the specific type T does not have an appropriate interface ancestor, because some extension of T might have the needed ancestor. This is similar to a conversion of a class-wide type toward the leaves of the tree, and we need to be consistent. Of course, there is a runtime check that the actual object has the needed interface. 

{AI95-00251-01} {AI05-0115-1} If there is no type (other than a root numeric type) that is the ancestor of both the target type and the operand type, and they are not both class-wide types, one of the following rules shall apply: 

{AI95-00251-01} If the target type is a numeric type, then the operand type shall be a numeric type.

{AI95-00251-01} If the target type is an array type, then the operand type shall be an array type. Further:

{AI95-00251-01} The types shall have the same dimensionality;

{AI95-00251-01} Corresponding index types shall be convertible; 

{AI95-00251-01} The component subtypes shall statically match; 

{AI95-00392-01} If the component types are anonymous access types, then the accessibility level of the operand type shall not be statically deeper than that of the target type; 

Reason: For unrelated array types, the component types could have different accessibility, and we had better not allow a conversion of a local type into a global type, in case the local type points at local objects. We don't need a check for other types of components; such components necessarily are for related types, and either have the same accessibility or (for access discriminants) cannot be changed so the discriminant check will prevent problems. 

{AI95-00246-01} Neither the target type nor the operand type shall be limited; 

Reason: We cannot allow conversions between unrelated limited types, as they may have different representations, and (since the types are limited), a copy cannot be made to reconcile the representations. 

{AI95-00251-01} {AI95-00363-01} If the target type of a view conversion has aliased components, then so shall the operand type; and 

Reason: {AI95-00363-01} We cannot allow a view conversion from an object with unaliased components to an object with aliased components, because that would effectively allow pointers to unaliased components. This rule was missing from Ada 95. 

{AI95-00246-01} {AI95-00251-01} The operand type of a view conversion shall not have a tagged, private, or volatile subcomponent. 

Reason: {AI95-00246-01} We cannot allow view conversions between unrelated might-be-by-reference types, as they may have different representations, and a copy cannot be made to reconcile the representations. 

Ramification: These rules only apply to unrelated array conversions; different (weaker) rules apply to conversions between related types. 

{AI95-00230-01} If the target type is universal_access, then the operand type shall be an access type.

Discussion: Such a conversion cannot be written explicitly, of course, but it can be implicit (see below). 

{AI95-00230-01} {AI95-00251-01} If the target type is a general access-to-object type, then the operand type shall be universal_access or an access-to-object type. Further, if the operand type is not universal_access:

Discussion: The Legality Rules and Dynamic Semantics are worded so that a [type_conversion](./AA-4.6#S0162) T(X) (where T is an access type) is (almost) equivalent to the [attribute_reference](./AA-4.1#S0100) X.all'Access, where the result is of type T. The only difference is that the [type_conversion](./AA-4.6#S0162) accepts a null value, whereas the [attribute_reference](./AA-4.1#S0100) would raise Constraint_Error. 

{AI95-00251-01} If the target type is an access-to-variable type, then the operand type shall be an access-to-variable type; 

Ramification: If the target type is an access-to-constant type, then the operand type can be access-to-constant or access-to-variable. 

{AI95-00251-01} If the target designated type is tagged, then the operand designated type shall be convertible to the target designated type; 

{AI95-00251-01} {AI95-00363-01} If the target designated type is not tagged, then the designated types shall be the same, and either:

{AI95-00363-01} the designated subtypes shall statically match; or

{AI95-00363-01} {AI95-00384-01} the designated type shall be discriminated in its full view and unconstrained in any partial view, and one of the designated subtypes shall be unconstrained;

Ramification: {AI95-00363-01} This does not require that types have a partial view in order to allow the conversion, simply that any partial view that does exist is unconstrained.

{AI95-00384-01} This allows conversions both ways (either subtype can be unconstrained); while Ada 95 only allowed the conversion if the target subtype is unconstrained. We generally want type conversions to be symmetric; which type is the target shouldn't matter for legality. 

Reason: {AI95-00363-01} If the visible partial view is constrained, we do not allow conversion between unconstrained and constrained subtypes. This means that whether the full type had discriminants is not visible to clients of the partial view. 

Discussion: {AI12-0095-1} {AI12-0005-1} We assume the worst in a generic body about whether or not a formal subtype has a constrained partial view; specifically, in a generic body a discriminated subtype is considered to have a constrained partial view if it is a descendant of an untagged generic formal private or derived type (see 12.5.1 for the formal definition of this rule). 

Reason: These rules are designed to ensure that aliased array objects only need "dope" if their nominal subtype is unconstrained, but they can always have dope if required by the run-time model (since no sliding is permitted as part of access type conversion). By contrast, aliased discriminated objects will always need their discriminants stored with them, even if nominally constrained. (Here, we are assuming an implementation that represents an access value as a single pointer.) 

{AI95-00251-01} {AI05-0148-1} {AI05-0248-1} {AI12-0027-1} The accessibility level of the operand type shall not be statically deeper than that of the target type, unless the target type is an anonymous access type of a stand-alone object. If the target type is that of such a stand-alone object, the accessibility level of the operand type shall not be statically deeper than that of the declaration of the stand-alone object. 

Ramification: {AI05-0148-1} The access parameter case is handled by a run-time check. runtime checks are also done in instance bodies, and for stand-alone objects of anonymous access types. 

Reason: We prohibit storing accesses to objects deeper than a stand-alone object of an anonymous access-to-object (even while we allow storing all other accesses) in order to prevent dangling accesses. 

{AI95-00230-01} If the target type is a pool-specific access-to-object type, then the operand type shall be universal_access. 

Reason: This allows null to be converted to pool-specific types. Without it, null could be converted to general access types but not pool-specific ones, which would be too inconsistent. Remember that these rules only apply to unrelated types, so we don't have to talk about conversions to derived or other related types. 

{AI95-00230-01} {AI95-00251-01} If the target type is an access-to-subprogram type, then the operand type shall be universal_access or an access-to-subprogram type. Further, if the operand type is not universal_access:

{AI95-00251-01} {AI05-0239-1} The designated profiles shall be subtype conformant. 

{AI95-00251-01} {AI12-0027-1} The accessibility level of the operand type shall not be statically deeper than that of the target type. If the operand type is declared within a generic body, the target type shall be declared within the generic body.

Reason: The reason it is illegal to convert from an access-to-subprogram type declared in a generic body to one declared outside that body is that in an implementation that shares generic bodies, subprograms declared inside the generic need to have a different calling convention - they need an extra parameter pointing to the data declared in the current instance. For subprograms declared in the spec, that's OK, because the compiler can know about them at compile time of the instantiation. 

{AI12-0380-1} If the target type has a Global aspect other than in out all or Unspecified, then each mode of the Global aspect of the operand type shall identify a subset of the variables identified by the corresponding mode of the target type Global aspect, or by the in out mode of the target type Global aspect.

{AI12-0064-2} If the target type is nonblocking, the operand type shall be nonblocking. 

{AI12-0027-1} In addition to the places where Legality Rules normally apply (see 12.3), these rules apply also in the private part of an instance of a generic unit. 

Discussion: {AI12-0005-1} This applies to all of the Legality Rules in this section. It won't matter for the majority of these rules, but in any case that it does, we want to apply the same rules in the private part. (Ada got the default wrong for these, as there is only one known case where we don't want to recheck in the private part, see derivations without record extensions in 3.4.) 


#### Static Semantics

A [type_conversion](./AA-4.6#S0162) that is a value conversion denotes the value that is the result of converting the value of the operand to the target subtype.

{AI05-0264-1} A [type_conversion](./AA-4.6#S0162) that is a view conversion denotes a view of the object denoted by the operand. This view is a variable of the target type if the operand denotes a variable; otherwise, it is a constant of the target type.

The nominal subtype of a [type_conversion](./AA-4.6#S0162) is its target subtype. 


#### Dynamic Semantics

For the evaluation of a [type_conversion](./AA-4.6#S0162) that is a value conversion, the operand is evaluated, and then the value of the operand is converted to a corresponding value of the target type, if any. If there is no value of the target type that corresponds to the operand value, Constraint_Error is raised[; this can only happen on conversion to a modular type, and only when the operand value is outside the base range of the modular type.] Additional rules follow: 

Numeric Type Conversion 

If the target and the operand types are both integer types, then the result is the value of the target type that corresponds to the same mathematical integer as the operand.

If the target type is a decimal fixed point type, then the result is truncated (toward 0) if the value of the operand is not a multiple of the small of the target type.

If the target type is some other real type, then the result is within the accuracy of the target type (see G.2, "Numeric Performance Requirements", for implementations that support the Numerics Annex). 

Discussion: An integer type might have more bits of precision than a real type, so on conversion (of a large integer), some precision might be lost. 

If the target type is an integer type and the operand type is real, the result is rounded to the nearest integer (away from zero if exactly halfway between two integers). 

Discussion: {AI95-00267-01} This was implementation defined in Ada 83. There seems no reason to preserve the nonportability in Ada 95. Round-away-from-zero is the conventional definition of rounding, and standard Fortran and COBOL both specify rounding away from zero, so for interoperability, it seems important to pick this. This is also the most easily "undone" by hand. Round-to-nearest-even is an alternative, but that is quite complicated if not supported by the hardware. In any case, this operation is not usually part of an inner loop, so predictability and portability are judged most important. A floating point attribute function Unbiased_Rounding is provided (see A.5.3) for those applications that require round-to-nearest-even, and a floating point attribute function Machine_Rounding (also see A.5.3) is provided for those applications that require the highest possible performance. "Deterministic" rounding is required for static conversions to integer as well. See 4.9. 

Enumeration Type Conversion 

The result is the value of the target type with the same position number as that of the operand value. 

Array Type Conversion 

If the target subtype is a constrained array subtype, then a check is made that the length of each dimension of the value of the operand equals the length of the corresponding dimension of the target subtype. The bounds of the result are those of the target subtype.

If the target subtype is an unconstrained array subtype, then the bounds of the result are obtained by converting each bound of the value of the operand to the corresponding index type of the target type. For each nonnull index range, a check is made that the bounds of the range belong to the corresponding index subtype. 

Discussion: Only nonnull index ranges are checked, per AI83-00313. 

In either array case, the value of each component of the result is that of the matching component of the operand value (see 4.5.2). 

Ramification: This applies whether or not the component is initialized. 

{AI95-00392-01} If the component types of the array types are anonymous access types, then a check is made that the accessibility level of the operand type is not deeper than that of the target type. 

Reason: {AI12-0005-1} This check is needed for operands that are in instance bodies. Other cases are handled by the legality rule given previously. 

Composite (Non-Array) Type Conversion 

The value of each nondiscriminant component of the result is that of the matching component of the operand value. 

Ramification: This applies whether or not the component is initialized. 

[The tag of the result is that of the operand.] If the operand type is class-wide, a check is made that the tag of the operand identifies a (specific) type that is covered by or descended from the target type. 

Ramification: This check is certain to succeed if the operand type is itself covered by or descended from the target type. 

Proof: The fact that a [type_conversion](./AA-4.6#S0162) preserves the tag is stated officially in 3.9, "Tagged Types and Type Extensions" 

For each discriminant of the target type that corresponds to a discriminant of the operand type, its value is that of the corresponding discriminant of the operand value; if it corresponds to more than one discriminant of the operand type, a check is made that all these discriminants are equal in the operand value.

For each discriminant of the target type that corresponds to a discriminant that is specified by the [derived_type_definition](./AA-3.4#S0035) for some ancestor of the operand type (or if class-wide, some ancestor of the specific type identified by the tag of the operand), its value in the result is that specified by the [derived_type_definition](./AA-3.4#S0035). 

Ramification: It is a ramification of the rules for the discriminants of derived types that each discriminant of the result is covered either by this paragraph or the previous one. See 3.7. 

For each discriminant of the operand type that corresponds to a discriminant that is specified by the [derived_type_definition](./AA-3.4#S0035) for some ancestor of the target type, a check is made that in the operand value it equals the value specified for it.

For each discriminant of the result, a check is made that its value belongs to its subtype. 

Access Type Conversion 

{AI05-0148-1} {AI05-0248-1} For an access-to-object type, a check is made that the accessibility level of the operand type is not deeper than that of the target type, unless the target type is an anonymous access type of a stand-alone object. If the target type is that of such a stand-alone object, a check is made that the accessibility level of the operand type is not deeper than that of the declaration of the stand-alone object[; then if the check succeeds, the accessibility level of the target type becomes that of the operand type]. 

Ramification: {AI05-0148-1} This check is needed for operands that are access parameters, for stand-alone anonymous access objects, and in instance bodies.

Note that this check can never fail for the implicit conversion to the anonymous type of an access parameter that is done when calling a subprogram with an access parameter. 

{AI95-00230-01} {AI95-00231-01} If the operand value is null, the result of the conversion is the null value of the target type. 

Ramification: A conversion to an anonymous access type happens implicitly as part of initializing or assigning to an anonymous access object. 

If the operand value is not null, then the result designates the same object (or subprogram) as is designated by the operand value, but viewed as being of the target designated subtype (or profile); any checks associated with evaluating a conversion to the target designated subtype are performed. 

Ramification: The checks are certain to succeed if the target and operand designated subtypes statically match. 

{AI95-00231-01} {AI05-0153-3} {AI05-0290-1} {AI12-0071-1} {AI12-0333-1} After conversion of the value to the target type, if the target subtype is constrained, a check is performed that the value satisfies this constraint. If the target subtype excludes null, then a check is made that the value is not null. If predicate checks are enabled for the target subtype (see 3.2.4), a check is performed that the value satisfies the predicates of the target subtype, unless the conversion is:. 

Ramification: {AI95-00231-01} The first check above is a Range_Check for scalar subtypes, a Discriminant_Check or Index_Check for access subtypes, and a Discriminant_Check for discriminated subtypes. The Length_Check for an array conversion is performed as part of the conversion to the target type. The check for exclusion of null is an Access_Check. 

{AI12-0333-1} {AI12-0432-1} a view conversion that is the target of an assignment statement and is not referenced with a [target_name](./AA-5.2#S0174), or an actual parameter of mode out; or

{AI12-0333-1} an implicit subtype conversion of an actual parameter of mode out to the nominal subtype of its formal parameter. 

Ramification: {AI12-0333-1} The reverse conversion applied to by-copy out parameters is not a view conversion and it is to the nominal subtype of the actual parameter, therefore any enabled predicate checks are performed. 

For the evaluation of a view conversion, the operand [name](./AA-4.1#S0091) is evaluated, and a new view of the object denoted by the operand is created, whose type is the target type; if the target type is composite, checks are performed as above for a value conversion.

The properties of this new view are as follows: 

{8652/0017} {AI95-00184-01} If the target type is composite, the bounds or discriminants (if any) of the view are as defined above for a value conversion; each nondiscriminant component of the view denotes the matching component of the operand object; the subtype of the view is constrained if either the target subtype or the operand object is constrained, or if the target subtype is indefinite, or if the operand type is a descendant of the target type and has discriminants that were not inherited from the target type;

{AI12-0439-1} If the target type is tagged, then an assignment to the view assigns to the corresponding part of the object denoted by the operand; otherwise, an assignment to the view assigns to the object, after converting the assigned value to the subtype of the object (which can raise Constraint_Error); 

{AI12-0074-1} {AI12-0439-1} Reading the value of the view yields the result of converting the value of the operand object to the target subtype (which can raise Constraint_Error), except if the object is of an elementary type and the view conversion is passed as an out parameter; in this latter case, the value of the operand object may be used to initialize the formal parameter without checking against any constraint of the target subtype (as described more precisely in 6.4.1). 

Reason: {AI12-0074-1} This ensures that even an out parameter of an elementary type is initialized reasonably. 

{AI05-0290-1} {AI12-0096-1} If an Accessibility_Check fails, Program_Error is raised. If a predicate check fails, the effect is as defined in subclause 3.2.4, "Subtype Predicates". Any other check associated with a conversion raises Constraint_Error if it fails.

Conversion to a type is the same as conversion to an unconstrained subtype of the type. 

Reason: This definition is needed because the semantics of various constructs involves converting to a type, whereas an explicit [type_conversion](./AA-4.6#S0162) actually converts to a subtype. For example, the evaluation of a [range](./AA-3.5#S0037) is defined to convert the values of the expressions to the type of the range. 

Ramification: A conversion to a scalar type, or, equivalently, to an unconstrained scalar subtype, can raise Constraint_Error if the value is outside the base range of the type. 

{AI12-0027-1} {AI12-0226-1} Evaluation of a value conversion of an object either creates a new anonymous object[ (similar to the object created by the evaluation of an [aggregate](./AA-4.3#S0106) or a function call)] or yields a new view of the operand object without creating a new object:

If the target type is a by-reference type and there is a type that is an ancestor of both the target type and the operand type then no new object is created;

If the target type is an array type having aliased components and the operand type is an array type having unaliased components, then a new object is created;

{AI12-0226-1} If the target type is an elementary type, then a new object is created;

Otherwise, it is unspecified whether a new object is created. 

{AI12-0027-1} If a new object is created, then the initialization of that object is an assignment operation.

Reason: This makes a difference in the case of converting from an array type with unaliased components to one with aliased components if the element type has a controlled part.

{AI12-0226-1} For an elementary type, the representation might change so we require a new object to avoid problems. 

Implementation Note: {AI12-0226-1} The temporary object need not be materialized in most cases; it should be handled like the return object of a predefined operator. Generally, whether the object exists can only be detected if it is renamed (unless a part of the type is controlled). 

Discussion: {AI12-0226-1} This set of rules does not apply in those cases where the operand is not an object (such as a value conversion of a named number); in such cases, the result isn't an object, so it isn't necessary to describe what that means. The rules cover all value conversions of composite types (since there aren't any values of composite types separate from objects). 

NOTE 1   In addition to explicit [type_conversion](./AA-4.6#S0162)s, type conversions are performed implicitly in situations where the expected type and the actual type of a construct differ, as is permitted by the type resolution rules (see 8.6). For example, an integer literal is of the type universal_integer, and is implicitly converted when assigned to a target of some specific integer type. Similarly, an actual parameter of a specific tagged type is implicitly converted when the corresponding formal parameter is of a class-wide type.

Even when the expected and actual types are the same, implicit subtype conversions are performed to adjust the array bounds (if any) of an operand to match the desired target subtype, or to raise Constraint_Error if the (possibly adjusted) value does not satisfy the constraints of the target subtype.

NOTE 2   {AI95-00230-01} A ramification of the overload resolution rules is that the operand of an (explicit) [type_conversion](./AA-4.6#S0162) cannot be an [allocator](./AA-4.8#S0164), an [aggregate](./AA-4.3#S0106), a [string_literal](./AA-2.6#S0016), a [character_literal](./AA-2.5#S0015), or an [attribute_reference](./AA-4.1#S0100) for an Access or Unchecked_Access attribute. Similarly, such an [expression](./AA-4.4#S0132) enclosed by parentheses is not allowed. A [qualified_expression](./AA-4.7#S0163) (see 4.7) can be used instead of such a [type_conversion](./AA-4.6#S0162).

NOTE 3   The constraint of the target subtype has no effect for a [type_conversion](./AA-4.6#S0162) of an elementary type passed as an out parameter. Hence, it is recommended that the first subtype be specified as the target to minimize confusion (a similar recommendation applies to renaming and generic formal in out objects). 


#### Examples

Examples of numeric type conversion: 

```ada
Real(2*J)      --  value is converted to floating point
Integer(1.6)   --  value is 2
Integer(-0.4)  --  value is 0

```

Example of conversion between derived types: 

```ada
type A_Form is new B_Form;

```

```ada
X : A_Form;
Y : B_Form;

```

```ada
X := A_Form(Y);
Y := B_Form(X);  --  the reverse conversion 

```

Examples of conversions between array types: 

```ada
type Sequence is array (Integer range &lt&gt) of Integer;
subtype Dozen is Sequence(1 .. 12);
Ledger : array(1 .. 100) of Integer;

```

```ada
Sequence(Ledger)            --  bounds are those of Ledger
Sequence(Ledger(31 .. 42))  --  bounds are 31 and 42
Dozen(Ledger(31 .. 42))     --  bounds are those of Dozen 

```


#### Incompatibilities With Ada 83

A [character_literal](./AA-2.5#S0015) is not allowed as the operand of a [type_conversion](./AA-4.6#S0162), since there are now two character types in package Standard.

The component subtypes have to statically match in an array conversion, rather than being checked for matching constraints at run time.

Because sliding of array bounds is now provided for operations where it was not in Ada 83, programs that used to raise Constraint_Error might now continue executing and produce a reasonable result. This is likely to fix more bugs than it creates. 


#### Extensions to Ada 83

A [type_conversion](./AA-4.6#S0162) is considered the name of an object in certain circumstances (such a [type_conversion](./AA-4.6#S0162) is called a view conversion). In particular, as in Ada 83, a [type_conversion](./AA-4.6#S0162) can appear as an in out or out actual parameter. In addition, if the target type is tagged and the operand is the [name](./AA-4.1#S0091) of an object, then so is the [type_conversion](./AA-4.6#S0162), and it can be used as the [prefix](./AA-4.1#S0093) to a [selected_component](./AA-4.1#S0098), in an [object_renaming_declaration](./AA-8.5#S0239), etc.

We no longer require type-mark conformance between a parameter of the form of a type conversion, and the corresponding formal parameter. This had caused some problems for inherited subprograms (since there isn't really a type-mark for converted formals), as well as for renamings, formal subprograms, etc. See AI83-00245, AI83-00318, AI83-00547.

We now specify "deterministic" rounding from real to integer types when the value of the operand is exactly between two integers (rounding is away from zero in this case).

"Sliding" of array bounds (which is part of conversion to an array subtype) is performed in more cases in Ada 95 than in Ada 83. Sliding is not performed on the operand of a membership test, nor on the operand of a [qualified_expression](./AA-4.7#S0163). It wouldn't make sense on a membership test, and we wish to retain a connection between subtype membership and subtype qualification. In general, a subtype membership test returns True if and only if a corresponding subtype qualification succeeds without raising an exception. Other operations that take arrays perform sliding. 


#### Wording Changes from Ada 83

We no longer explicitly list the kinds of things that are not allowed as the operand of a [type_conversion](./AA-4.6#S0162), except in a NOTE.

{AI05-0299-1} The rules in this subclause subsume the rules for "parameters of the form of a type conversion", and have been generalized to cover the use of a type conversion as a [name](./AA-4.1#S0091). 


#### Incompatibilities With Ada 95

{AI95-00246-01} Amendment Correction: Conversions between unrelated array types that are limited or (for view conversions) might be by-reference types are now illegal. The representations of two such arrays may differ, making the conversions impossible. We make the check here, because legality should not be based on representation properties. Such conversions are likely to be rare, anyway. There is a potential that this change would make a working program illegal (if the types have the same representation).

{AI95-00363-01} If a discriminated full type has a partial view (private type) that is constrained, we do not allow conversion between access-to-unconstrained and access-to-constrained subtypes designating the type. Ada 95 allowed this conversion and the declaration of various access subtypes, requiring that the designated object be constrained and thus making details of the implementation of the private type visible to the client of the private type. See 4.8 for more on this topic. 


#### Extensions to Ada 95

{AI95-00230-01} Conversion rules for universal_access were defined. These allow the use of anonymous access values in equality tests (see 4.5.2), and also allow the use of null in type conversions and other contexts that do not provide a single expected type.

{AI95-00384-01} A type conversion from an access-to-discriminated and unconstrained object to an access-to-discriminated and constrained one is allowed. Ada 95 only allowed the reverse conversion, which was weird and asymmetric. Of course, a constraint check will be performed for this conversion. 


#### Wording Changes from Ada 95

{8652/0017} {AI95-00184-01} Corrigendum: Wording was added to ensure that view conversions are constrained, and that a tagged view conversion has a tagged object. Both rules are needed to avoid having a way to change the discriminants of a constrained object.

{8652/0008} {AI95-00168-01} Corrigendum: Wording was added to ensure that the aliased status of array components cannot change in a view conversion. This rule was needed to avoid having a way to change the discriminants of an aliased object. This rule was repealed later, as Ada 2005 allows changing the discriminants of an aliased object.

{AI95-00231-01} Wording was added to check subtypes that exclude null (see 3.10).

{AI95-00251-01} The organization of the legality rules was changed, both to make it clearer, and to eliminate an unintentional incompatibility with Ada 83. The old organization prevented type conversions between some types that were related by derivation (which Ada 83 always allowed).

{AI95-00330-01} {AI05-0005-1} Clarified that an untagged type conversion appearing as a generic actual parameter for a generic in out formal parameter is not a view conversion (and thus is illegal). This confirms the ACATS tests, so all implementations already follow this interpretation.

{AI95-00363-01} Rules added by the Corrigendum to eliminate problems with discriminants of aliased components changing were removed, as we now generally allow discriminants of aliased components to be changed.

{AI95-00392-01} Accessibility checks on conversions involving types with anonymous access components were added. These components have the level of the type, and conversions can be between types at different levels, which could cause dangling access values in the absence of such checks. 


#### Inconsistencies With Ada 2005

{AI05-0148-1} A stand-alone object of an anonymous access-to-object type now has dynamic accessibility. Normally, this will make programs legal that were illegal in Ada 2005. However, it is possible that a program that previously raised Program_Error now will not. It is very unlikely that an existing program intentionally depends on the exception being raised; the change is more likely to fix bugs than introduce them. 


#### Wording Changes from Ada 2005

{AI05-0115-1} Correction: Clarified that a root numeric type is not considered a common ancestor for a conversion.

{AI05-0153-3} {AI05-0290-1} Added rules so that predicate aspects (see 3.2.4) are enforced on subtype conversion. 


#### Inconsistencies With Ada 2012

{AI12-0333-1} {AI12-0432-1} Correction: Predicate checks are no longer made on any out parameters before a call (they're still made when the call returns). This was already true for elementary out parameters. If a program depends on a predicate check failing on an inbound out composite parameter, it will get an incorrect result. Similarly, predicate checks are no longer made on the view conversion of a target of an [assignment_statement](./AA-5.2#S0173). Both of these cases seem quite unlikely, as programs (outside of ACATS tests) that depend on the failure of checks are very rare, and the predicate might be checking uninitialized components (making check failure unreliable). 


#### Incompatibilities With Ada 2012

{AI12-0095-1} {AI12-0005-1} Corrigendum: Because of a rule added in 12.5.1, the checks for the legality of an access type conversion in a generic body were strengthened to use an assume-the-worst rule. This case is rather unlikely because a formal private or derived type with discriminants is required along with a conversion between two access types whose designated types don't statically match, and any such programs were at risk having objects disappear while valid access values still pointed at them. 


#### Wording Changes from Ada 2012

{AI12-0027-1} Corrigendum: Moved the generic boilerplate so that it covers all Legality Rules in this subclause. This was always intended, but it is not expected to change anything other than conversions between unrelated arrays.

{AI12-0027-1} Corrigendum: Added a formal definition of the copy potentially created by a value conversion of a composite type, so properties like finalization and accessibility are properly defined. This model was always intended and expected (else 13.6 would not work), but it was not previously formally defined.

{AI12-0071-1} Corrigendum: Updated wording of type conversions to use the new term "satisfies the predicates" (see 3.2.4).

{AI12-0074-1} Corrigendum: Clarified the wording describing the effect of view conversions of out parameters such that it is clear that the detailed effect is defined in 6.4.1, not here.

{AI12-0096-1} Corrigendum: Updated wording of type conversions so that the exception raise or other effect of a failed predicate check is as defined in 3.2.4; we don't want to repeat those rules here. This doesn't change the behavior for predicate checks possible in original Ada 2012, only ones using the new aspect Predicate_Failure.

{AI12-0064-2} {AI12-0380-1} Required Global (see 6.1.2) and Nonblocking (see 9.5) aspect matching for access-to-subprogram conversions.

{AI12-0226-1} Described the objects associated with value conversions of elementary types. This is necessary to support an extension documented in 3.3.

{AI12-0392-1} Correction: Eliminated the requirement that the operand of a view conversion be a simple [name](./AA-4.1#S0091). The requirement could cause unintended consequences in the case where the operand of a type conversion represents an object but is more complex than a simple [name](./AA-4.1#S0091) (such as a [qualified_expression](./AA-4.7#S0163) or [conditional_expression](./AA-4.5#S0148)). 

