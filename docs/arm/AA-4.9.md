---
sidebar_position:  37
---

# 4.9  Static Expressions and Static Subtypes

Certain expressions of a scalar or string type are defined to be static. Similarly, certain discrete ranges are defined to be static, and certain scalar and string subtypes are defined to be static subtypes. [ Static means determinable at compile time, using the declared properties or values of the program entities.] 

Discussion: As opposed to more elaborate data flow analysis, etc. 


#### Language Design Principles

For an expression to be static, it has to be calculable at compile time.

Only scalar and string expressions are static.

To be static, an expression cannot have any nonscalar, nonstring subexpressions (though it can have nonscalar constituent [name](./AA-4.1#S0091)s). A static scalar expression cannot have any nonscalar subexpressions. There is one exception - a membership test for a string subtype can be static, and the result is scalar, even though a subexpression is nonscalar.

The rules for evaluating static expressions are designed to maximize portability of static calculations.

Reason: {AI12-0201-1} We support static string expressions so that, for example, the [aspect_definition](./AA-13.1#S0348) for a Link_Name aspect can contain a concatenation. We don't support static aggregates (even for string types) or non-string static nonscalar types; we're trying to keep it cheap and simple (from the implementer's viewpoint). 


#### Static Semantics

A static expression is [a scalar or string expression that is] one of the following:

{AI12-0249-1} a [numeric_literal](./AA-2.4#S0006) of a numeric type; 

Ramification: {AI12-0249-1} A [numeric_literal](./AA-2.4#S0006) of a numeric type is always a static expression, even if its expected type is not that of a static subtype. However, if its value is explicitly converted to, or qualified by, a nonstatic subtype, the resulting expression is nonstatic. Non-numeric types can have numeric literals if aspect Integer_Literal or Real_Literal is used; these are never static. 

a [string_literal](./AA-2.6#S0016) of a static string subtype; 

Ramification: That is, the constrained subtype defined by the index range of the string is static. Note that elementary values don't generally have subtypes, while composite values do (since the bounds or discriminants are inherent in the value). 

{AI12-0394-1} a [name](./AA-4.1#S0091) that denotes the declaration of a static constant;

{AI12-0394-1} a [name](./AA-4.1#S0091) that denotes a named number, and that is interpreted as a value of a numeric type;

To be honest: {AI12-0394-1} This is referring the resolution of the named number and not the Static Semantics (for which all named numbers are values of a universal numeric type). The word "interpreted" is intended to make the distinction. 

Ramification: Note that enumeration literals are covered by the [function_call](./AA-6.4#S0218) case. 

a [function_call](./AA-6.4#S0218) whose function_[name](./AA-4.1#S0091) or function_[prefix](./AA-4.1#S0093) statically denotes a static function, and whose actual parameters, if any (whether given explicitly or by default), are all static expressions; 

Ramification: This includes uses of operators that are equivalent to [function_call](./AA-6.4#S0218)s. 

an [attribute_reference](./AA-4.1#S0100) that denotes a scalar value, and whose [prefix](./AA-4.1#S0093) denotes a static scalar subtype;

Ramification: Note that this does not include the case of an attribute that is a function; a reference to such an attribute is not even an expression. See above for function calls.

An implementation may define the staticness and other properties of implementation-defined attributes. 

{AI12-0368-1} an [attribute_reference](./AA-4.1#S0100) whose [prefix](./AA-4.1#S0093) statically names a statically constrained array object or array subtype, and whose [attribute_designator](./AA-4.1#S0101) is First, Last, or Length, with an optional dimension;

{AI12-0064-2} an [attribute_reference](./AA-4.1#S0100) whose [prefix](./AA-4.1#S0093) denotes a non-generic entity that is not declared in a generic unit, and whose [attribute_designator](./AA-4.1#S0101) is Nonblocking;

{AI12-0201-1} a [type_conversion](./AA-4.6#S0162) whose [subtype_mark](./AA-3.2#S0028) denotes a static [(scalar or string)] subtype, and whose operand is a static expression;

a [qualified_expression](./AA-4.7#S0163) whose [subtype_mark](./AA-3.2#S0028) denotes a static [(scalar or string)] subtype, and whose operand is a static expression; 

Ramification: This rules out the [subtype_mark](./AA-3.2#S0028)'[aggregate](./AA-4.3#S0106) case. 

Reason: Adding qualification to an expression shouldn't make it nonstatic, even for strings. 

{AI05-0158-1} {AI05-0269-1} {AI12-0039-1} a membership test whose tested_[simple_expression](./AA-4.4#S0138) is a static expression, and whose [membership_choice_list](./AA-4.4#S0136) consists only of [membership_choice](./AA-4.4#S0137)s that are either static choice_[simple_expression](./AA-4.4#S0138)s, static [range](./AA-3.5#S0037)s, or [subtype_mark](./AA-3.2#S0028)s that denote a static [(scalar or string)] subtype; 

Reason: Clearly, we should allow membership tests in exactly the same cases where we allow [qualified_expression](./AA-4.7#S0163)s. 

a short-circuit control form both of whose [relation](./AA-4.4#S0135)s are static expressions;

{AI05-0147-1} {AI05-0188-1} a [conditional_expression](./AA-4.5#S0148) all of whose [condition](./AA-4.5#S0150)s, selecting_[expression](./AA-4.4#S0132)s, and dependent_[expression](./AA-4.4#S0132)s are static expressions;

{AI12-0368-1} a [declare_expression](./AA-4.5#S0156) whose body_[expression](./AA-4.4#S0132) is static and each of whose declarations, if any, is either the declaration of a static constant or is an [object_renaming_declaration](./AA-8.5#S0239) with an object_[name](./AA-4.1#S0091) that statically names the renamed object;

a static expression enclosed in parentheses. 

Discussion: Informally, we talk about a static value. When we do, we mean a value specified by a static expression. 

Ramification: {AI12-0005-1} The language requires a static expression in a [number_declaration](./AA-3.3#S0034), a numeric type definition, certain representation items, and a number of other contexts. 

A [name](./AA-4.1#S0091) statically denotes an entity if it denotes the entity and: 

It is a [direct_name](./AA-4.1#S0092), expanded name, or [character_literal](./AA-2.5#S0015), and it denotes a declaration other than a [renaming_declaration](./AA-8.5#S0238); or

It is an [attribute_reference](./AA-4.1#S0100) whose [prefix](./AA-4.1#S0093) statically denotes some entity; or

{AI12-0322-1} It is a [target_name](./AA-5.2#S0174) (see 5.2.1) in an [assignment_statement](./AA-5.2#S0173) whose variable_[name](./AA-4.1#S0091) statically denotes some entity; or

It denotes a [renaming_declaration](./AA-8.5#S0238) with a [name](./AA-4.1#S0091) that statically denotes the renamed entity. 

Ramification: [Selected_component](./AA-4.1#S0098)s that are not expanded names and [indexed_component](./AA-4.1#S0096)s do not statically denote things. 

{AI12-0368-1} A [name](./AA-4.1#S0091) statically names an object if it: 

{AI12-0368-1} statically denotes the declaration of an object [(possibly through one or more renames)]; 

Proof: Follows from the definition of statically denotes. 

{AI12-0368-1} {AI12-0373-1} is a [selected_component](./AA-4.1#S0098) whose prefix statically names an object, there is no implicit dereference of the prefix, and the [selector_name](./AA-4.1#S0099) does not denote a [component_declaration](./AA-3.8#S0070) occurring within a [variant_part](./AA-3.8#S0071); or 

Reason: We disallow components in a [variant_part](./AA-3.8#S0071) so that no discriminant checks are needed to evaluate the [selected_component](./AA-4.1#S0098). Note that other kinds of discriminant-dependent components do not need any checks on access (only when they are changed). 

{AI12-0368-1} is an [indexed_component](./AA-4.1#S0096) whose prefix statically names an object, there is no implicit dereference of the prefix, the object is statically constrained, and the index expressions of the object are static and have values that are within the range of the index constraint. 

{AI12-0368-1} For an entity other than an object, a [name](./AA-4.1#S0091) statically names an entity if the [name](./AA-4.1#S0091) statically denotes the entity.

A static function is one of the following: 

Ramification: These are the functions whose calls can be static expressions. 

a predefined operator whose parameter and result types are all scalar types none of which are descendants of formal scalar types;

{AI12-0201-1} a predefined relational operator whose parameters are of a string type that is not a descendant of a formal array type;

{AI12-0201-1} a predefined concatenation operator whose result type is a string type that is not a descendant of a formal array type;

{AI12-0385-1} a shifting or rotating function associated with a modular type declared in package Interfaces (see B.2);

an enumeration literal;

{AI12-0075-1} a static expression function (see 6.8);

a language-defined attribute that is a function, if the [prefix](./AA-4.1#S0093) denotes a static scalar subtype, and if the parameter and result types are scalar. 

In any case, a generic formal subprogram is not a static function.

{AI12-0201-1} {AI12-0393-1} A static constant is a constant view declared by a full constant declaration or an [object_renaming_declaration](./AA-8.5#S0239) with a static nominal subtype, having a value defined by a static scalar expression or by a static string expression, and which satisfies any constraint or predicate that applies to the nominal subtype. 

Ramification: A deferred constant is not static; the view introduced by the corresponding full constant declaration can be static. 

This paragraph was deleted.{AI05-0229-1} {AI12-0201-1} 

This paragraph was deleted.{AI12-0201-1} 

A static range is a [range](./AA-3.5#S0037) whose bounds are static expressions, [or a [range_attribute_reference](./AA-4.1#S0102) that is equivalent to such a [range](./AA-3.5#S0037).] A static [discrete_range](./AA-3.6#S0058) is one that is a static range or is a [subtype_indication](./AA-3.2#S0027) that defines a static scalar subtype. The base range of a scalar type is a static range, unless the type is a descendant of a formal scalar type.

{AI95-00263-01} {AI05-0153-3} A static subtype is either a static scalar subtype or a static string subtype. A static scalar subtype is an unconstrained scalar subtype whose type is not a descendant of a formal type, or a constrained scalar subtype formed by imposing a compatible static constraint on a static scalar subtype. A static string subtype is an unconstrained string subtype whose index subtype and component subtype are static, or a constrained string subtype formed by imposing a compatible static constraint on a static string subtype. In any case, the subtype of a generic formal object of mode in out, and the result subtype of a generic formal function, are not static. Also, a subtype is not static if any Dynamic_Predicate specifications apply to it. 

Ramification: String subtypes are the only composite subtypes that can be static. 

Reason: The part about generic formal objects of mode in out is necessary because the subtype of the formal is not required to have anything to do with the subtype of the actual. For example: 

```ada
subtype Int10 is Integer range 1..10;

```

```ada
generic
    F : in out Int10;
procedure G;

```

```ada
procedure G is
begin
    case F is
        when 1..10 =&gt null;
        -- Illegal!
    end case;
end G;

```

```ada
X : Integer range 1..20;
procedure I is new G(F =&gt X); -- OK.

```

The [case_statement](./AA-5.4#S0176) is illegal, because the subtype of F is not static, so the choices have to cover all values of Integer, not just those in the range 1..10. A similar issue arises for generic formal functions, now that function calls are object names. 

The different kinds of static constraint are defined as follows: 

A null constraint is always static;

A scalar constraint is static if it has no [range_constraint](./AA-3.5#S0036), or one with a static range;

An index constraint is static if each [discrete_range](./AA-3.6#S0058) is static, and each index subtype of the corresponding array type is static;

A discriminant constraint is static if each [expression](./AA-4.4#S0132) of the constraint is static, and the subtype of each discriminant is static. 

{AI95-00311-01} In any case, the constraint of the first subtype of a scalar formal type is neither static nor null.

A subtype is statically constrained if it is constrained, and its constraint is static. An object is statically constrained if its nominal subtype is statically constrained, or if it is a static string constant. 


#### Legality Rules

{AI05-0147-1} An expression is statically unevaluated if it is part of:

{AI05-0147-1} the right operand of a static short-circuit control form whose value is determined by its left operand; or

{AI05-0147-1} {AI05-0188-1} a dependent_[expression](./AA-4.4#S0132) of an [if_expression](./AA-4.5#S0149) whose associated [condition](./AA-4.5#S0150) is static and equals False; or

{AI05-0147-1} {AI05-0188-1} a [condition](./AA-4.5#S0150) or dependent_[expression](./AA-4.4#S0132) of an [if_expression](./AA-4.5#S0149) where the [condition](./AA-4.5#S0150) corresponding to at least one preceding dependent_[expression](./AA-4.4#S0132) of the [if_expression](./AA-4.5#S0149) is static and equals True; or

Reason: We need this bullet so that only a single dependent_[expression](./AA-4.4#S0132) is evaluated in a static [if_expression](./AA-4.5#S0149) if there is more than one [condition](./AA-4.5#S0150) that evaluates to True. The part about [condition](./AA-4.5#S0150)s makes 

```ada
(if N = 0 then Min elsif 10_000/N &gt Min then 10_000/N else Min)

```

legal if N and Min are static and N = 0. 

Discussion: {AI05-0147-1} {AI05-0188-1} We need the "of the [if_expression](./AA-4.5#S0149)" here so there is no confusion for nested [if_expression](./AA-4.5#S0149)s; this rule only applies to the [condition](./AA-4.5#S0150)s and dependent_[expression](./AA-4.4#S0132)s of a single [if_expression](./AA-4.5#S0149). Similar reasoning applies to the "of a [case_expression](./AA-4.5#S0151)" of the last bullet. 

{AI05-0188-1} {AI05-0269-1} a dependent_[expression](./AA-4.4#S0132) of a [case_expression](./AA-4.5#S0151) whose selecting_[expression](./AA-4.4#S0132) is static and whose value is not covered by the corresponding [discrete_choice_list](./AA-3.8#S0073); or

{AI05-0158-1} {AI12-0039-1} a choice_[simple_expression](./AA-4.4#S0138) (or a [simple_expression](./AA-4.4#S0138) of a [range](./AA-3.5#S0037) that occurs as a [membership_choice](./AA-4.4#S0137) of a [membership_choice_list](./AA-4.4#S0136)) of a static membership test that is preceded in the enclosing [membership_choice_list](./AA-4.4#S0136) by another item whose individual membership test (see 4.5.2) statically yields True. 

{AI05-0147-1} A static expression is evaluated at compile time except when it is statically unevaluated. The compile-time evaluation of a static expression is performed exactly, without performing Overflow_Checks. For a static expression that is evaluated: 

{AI05-0262-1} The expression is illegal if its evaluation fails a language-defined check other than Overflow_Check. For the purposes of this evaluation, the assertion policy is assumed to be Check.

Reason: {AI05-0262-1} Assertion policies can control whether checks are made, but we don't want assertion policies to affect legality. For Ada 2012, subtype predicates are the only checks controlled by the assertion policy that can appear in static expressions. 

{AI95-00269-01} If the expression is not part of a larger static expression and the expression is expected to be of a single specific type, then its value shall be within the base range of its expected type. Otherwise, the value may be arbitrarily large or small. 

Ramification: {AI95-00269-01} If the expression is expected to be of a universal type, or of "any integer type", there are no limits on the value of the expression. 

{AI95-00269-01} If the expression is of type universal_real and its expected type is a decimal fixed point type, then its value shall be a multiple of the small of the decimal type. This restriction does not apply if the expected type is a descendant of a formal scalar type (or a corresponding actual type in an instance). 

Ramification: This means that a [numeric_literal](./AA-2.4#S0006) for a decimal type cannot have "extra" significant digits. 

Reason: {AI95-00269-01} The small is not known for a generic formal type, so we have to exclude formal types from this check. 

{AI95-00269-01} In addition to the places where Legality Rules normally apply (see 12.3), the above restrictions also apply in the private part of an instance of a generic unit.

Discussion: Values outside the base range are not permitted when crossing from the "static" domain to the "dynamic" domain. This rule is designed to enhance portability of programs containing static expressions. Note that this rule applies to the exact value, not the value after any rounding or truncation. (See below for the rounding and truncation requirements.)

Short-circuit control forms are a special case: 

```ada
N: constant := 0.0;
X: constant Boolean := (N = 0.0) or else (1.0/N &gt 0.5); -- Static.

```

The declaration of X is legal, since the divide-by-zero part of the expression is not evaluated. X is a static constant equal to True.

{AI12-0075-1} The preceding "statically unevaluated" rule allows

```ada
X : constant := (if True then 37 else (1 / 0));

```

but does not allow

```ada
function If_Then_Else (Flag : Boolean; X, Y : Integer) return Integer is
   (if Flag then X else Y) with Static; -- see 6.8
X : constant := If_Then_Else (True, 37, 1 / 0);

```

because evaluation of a function call includes evaluation of all of its actual parameters. 


#### Implementation Requirements

{AI95-00268-01} {AI95-00269-01} For a real static expression that is not part of a larger static expression, and whose expected type is not a descendant of a formal type, the implementation shall round or truncate the value (according to the Machine_Rounds attribute of the expected type) to the nearest machine number of the expected type; if the value is exactly half-way between two machine numbers, the rounding performed is implementation-defined. If the expected type is a descendant of a formal type, or if the static expression appears in the body of an instance of a generic unit and the corresponding expression is nonstatic in the corresponding generic body, then no special rounding or truncating is required - normal accuracy rules apply (see Annex G). 

Implementation defined: Rounding of real static expressions which are exactly half-way between two machine numbers.

Reason: {AI95-00268-01} Discarding extended precision enhances portability by ensuring that the value of a static constant of a real type is always a machine number of the type. 

When the expected type is a descendant of a formal floating point type, extended precision (beyond that of the machine numbers) can be retained when evaluating a static expression, to ease code sharing for generic instantiations. For similar reasons, normal (nondeterministic) rounding or truncating rules apply for descendants of a formal fixed point type.

{AI95-00269-01} There is no requirement for exact evaluation or special rounding in an instance body (unless the expression is static in the generic body). This eliminates a potential contract issue where the exact value of a static expression depends on the actual parameters (which could then affect the legality of other code). 

Implementation Note: Note that the implementation of static expressions has to keep track of plus and minus zero for a type whose Signed_Zeros attribute is True.

{AI95-00100-01} Note that the only machine numbers of a fixed point type are the multiples of the small, so a static conversion to a fixed-point type, or division by an integer, must do truncation to a multiple of small. It is not correct for the implementation to do all static calculations in infinite precision.


#### Implementation Advice

{AI95-00268-01} For a real static expression that is not part of a larger static expression, and whose expected type is not a descendant of a formal type, the rounding should be the same as the default rounding for the target system. 

Implementation Advice: A real static expression with a nonformal type that is not part of a larger static expression should be rounded the same as the target system.

NOTE 1   An expression can be static even if it occurs in a context where staticness is not required. 

Ramification: For example: 

```ada
X : Float := Float'(1.0E+400) + 1.0 - Float'(1.0E+400);

```

The expression is static, which means that the value of X must be exactly 1.0, independent of the accuracy or range of the run-time floating point implementation.

The following kinds of expressions are never static: [explicit_dereference](./AA-4.1#S0094), [indexed_component](./AA-4.1#S0096), [slice](./AA-4.1#S0097), null, [aggregate](./AA-4.3#S0106), [allocator](./AA-4.8#S0164). 

NOTE 2   A static (or run-time) [type_conversion](./AA-4.6#S0162) from a real type to an integer type performs rounding. If the operand value is exactly half-way between two integers, the rounding is performed away from zero. 

Reason: We specify this for portability. The reason for not choosing round-to-nearest-even, for example, is that this method is easier to undo. 

Ramification: The attribute Truncation (see A.5.3) can be used to perform a (static) truncation prior to conversion, to prevent rounding. 

Implementation Note: The value of the literal 0E999999999999999999999999999999999999999999999 is zero. The implementation must take care to evaluate such literals properly.


#### Examples

Examples of static expressions: 

```ada
1 + 1       -- 2
abs(-10)*3  -- 30

```

```ada
Kilo : constant := 1000;
Mega : constant := Kilo*Kilo;   -- 1_000_000
Long : constant := Float'Digits*2;

```

```ada
Half_Pi    : constant := Pi/2;           -- see 3.3.2
Deg_To_Rad : constant := Half_Pi/90;
Rad_To_Deg : constant := 1.0/Deg_To_Rad; -- equivalent to 1.0/((3.14159_26536/2)/90)

```


#### Extensions to Ada 83

The rules for static expressions and static subtypes are generalized to allow more kinds of compile-time-known expressions to be used where compile-time-known values are required, as follows: 

Membership tests and short-circuit control forms may appear in a static expression.

The bounds and length of statically constrained array objects or subtypes are static.

The Range attribute of a statically constrained array subtype or object gives a static range.

A [type_conversion](./AA-4.6#S0162) is static if the [subtype_mark](./AA-3.2#S0028) denotes a static scalar subtype and the operand is a static expression.

All numeric literals are now static, even if the expected type is a formal scalar type. This is useful in [case_statement](./AA-5.4#S0176)s and [variant_part](./AA-3.8#S0071)s, which both now allow a value of a formal scalar type to control the selection, to ease conversion of a package into a generic package. Similarly, named array aggregates are also permitted for array types with an index type that is a formal scalar type. 

The rules for the evaluation of static expressions are revised to require exact evaluation at compile time, and force a machine number result when crossing from the static realm to the dynamic realm, to enhance portability and predictability. Exact evaluation is not required for descendants of a formal scalar type, to simplify generic code sharing and to avoid generic contract model problems.

Static expressions are legal even if an intermediate in the expression goes outside the base range of the type. Therefore, the following will succeed in Ada 95, whereas it might raise an exception in Ada 83: 

```ada
type Short_Int is range -32_768 .. 32_767;
I : Short_Int := -32_768;

```

This might raise an exception in Ada 83 because "32_768" is out of range, even though "32_768" is not. In Ada 95, this will always succeed.

Certain expressions involving string operations (in particular concatenation and membership tests) are considered static in Ada 95.

The reason for this change is to simplify the rule requiring compile-time-known string expressions as the link name in an interfacing pragma, and to simplify the preelaborability rules. 


#### Incompatibilities With Ada 83

An Ada 83 program that uses an out-of-range static value is illegal in Ada 95, unless the expression is part of a larger static expression, or the expression is not evaluated due to being on the right-hand side of a short-circuit control form. 


#### Wording Changes from Ada 83

{AI05-0299-1} This subclause (and 4.5.5, "Multiplying Operators") subsumes the RM83 section on Universal Expressions.

The existence of static string expressions necessitated changing the definition of static subtype to include string subtypes. Most occurrences of "static subtype" have been changed to "static scalar subtype", in order to preserve the effect of the Ada 83 rules. This has the added benefit of clarifying the difference between "static subtype" and "statically constrained subtype", which has been a source of confusion. In cases where we allow static string subtypes, we explicitly use phrases like "static string subtype" or "static (scalar or string) subtype", in order to clarify the meaning for those who have gotten used to the Ada 83 terminology.

In Ada 83, an expression was considered nonstatic if it raised an exception. Thus, for example: 

```ada
Bad: constant := 1/0; -- Illegal!

```

was illegal because 1/0 was not static. In Ada 95, the above example is still illegal, but for a different reason: 1/0 is static, but there's a separate rule forbidding the exception raising.


#### Inconsistencies With Ada 95

{AI95-00268-01} Amendment Correction: Rounding of static real expressions is implementation-defined in Ada 2005, while it was specified as away from zero in (original) Ada 95. This could make subtle differences in programs. However, the original Ada 95 rule required rounding that (probably) differed from the target processor, thus creating anomalies where the value of a static expression was required to be different than the same expression evaluated at run time. 


#### Wording Changes from Ada 95

{AI95-00263-01} {AI95-00268-01} The Ada 95 wording that defined static subtypes unintentionally failed to exclude formal derived types that happen to be scalar (these aren't formal scalar types); and had a parenthetical remark excluding formal string types - but that was neither necessary nor parenthetical (it didn't follow from other wording). This issue also applies to the rounding rules for real static expressions.

{AI95-00269-01} Ada 95 didn't clearly define the bounds of a value of a static expression for universal types and for "any integer/float/fixed type". We also make it clear that we do not intend exact evaluation of static expressions in an instance body if the expressions aren't static in the generic body.

{AI95-00311-01} We clarify that the first subtype of a scalar formal type has a nonstatic, nonnull constraint. 


#### Wording Changes from Ada 2005

{AI05-0147-1} {AI05-0188-1} Added wording to define staticness and the lack of evaluation for [if_expression](./AA-4.5#S0149)s and [case_expression](./AA-4.5#S0151)s. These are new and defined elsewhere.

{AI05-0153-3} Added wording to prevent subtypes that have dynamic predicates (see 3.2.4) from being static.

{AI05-0158-1} Revised wording for membership tests to allow for the new possibilities allowed by the [membership_choice_list](./AA-4.4#S0136). 


#### Incompatibilities With Ada 2012

{AI12-0201-1} Added a missing exclusion for concatenations of a string type descended from a formal array type. This could potentially make some expression non-static; but as that could only matter in a context where a static string is required (such as the Link_Name aspect), it is quite unlikely.

{AI12-0385-1} Shifting and rotating functions declared in package Interfaces are now static. This could potentially make some expression illegal that is legal if nonstatic (as in Ada 2012). While this can happen especially in conditional code that is not in use, it is quite unlikely given typical uses of shifting or rotating functions. 


#### Extensions to Ada 2012

{AI12-0201-1} Expressions involving string relational operators or string type conversions now can be static. Additionally, the length limit on static string constants was removed as being a hazard without much help to implementations. 


#### Wording Changes from Ada 2012

{AI12-0064-2} Defined the staticness of the Nonblocking attribute (see 9.5).

{AI12-0075-1} Expression functions can be static if declared correctly; this is documented as an extension in 6.8.

{AI12-0249-1} {AI12-0394-1} A [numeric_literal](./AA-2.4#S0006) or named number can be non-static if they interpreted using an Integer_Literal or Real_Literal aspect (see 4.2.1).

{AI12-0322-1} Clarified that a target name symbol can statically denote an entity if the associated variable_[name](./AA-4.1#S0091) statically denotes an entity. This is necessary so that target names participate in the anti-order-dependence checks of 6.4.1.

{AI12-0368-1} {AI12-0373-1} Added wording to define staticness for [declare_expression](./AA-4.5#S0156)s. Also moved "statically names" definition here and used it in array attribute prefix wording.

{AI12-0393-1} Correction: Clarified that constants whose values do not belong to their nominal subtype are not static. This change potentially would be incompatible, but this case is considered pathological and will not be checked by the ACATS. 


## 4.9.1  Statically Matching Constraints and Subtypes


#### Static Semantics

{AI95-00311-01} A constraint statically matches another constraint if: 

both are null constraints;

Discussion: A null constraint has nothing to do with null exclusions! Unconstrained array subtypes, subtypes with unknown discriminants, and subtypes with no explicit constraint have null constraints (see 3.2). This terminology became confusing when null exclusions were introduced in the 2007 Amendment. 

both are static and have equal corresponding bounds or discriminant values;

both are nonstatic and result from the same elaboration of a [constraint](./AA-3.2#S0029) of a [subtype_indication](./AA-3.2#S0027) or the same evaluation of a [range](./AA-3.5#S0037) of a [discrete_subtype_definition](./AA-3.6#S0055); or

{AI95-00311-01} both are nonstatic and come from the same [formal_type_declaration](./AA-12.5#S0320). 

{AI12-0374-2} {AI12-0427-1} The Global or Global'Class aspects (see 6.1.2) of two entities statically match if both consist of a single [global_aspect_definition](./AA-6.1#S0209) where each is the reserved word null, or each is of the form "[global_mode](./AA-6.1#S0211) [global_designator](./AA-6.1#S0214)" with each [global_mode](./AA-6.1#S0211) being the same sequence of reserved words and each [global_designator](./AA-6.1#S0214) being the same reserved word, or each being a [global_name](./AA-6.1#S0215) that statically names the same entity.

{AI95-00231-01} {AI95-00254-01} {AI05-0153-3} {AI12-0059-1} {AI12-0374-2} A subtype statically matches another subtype of the same type if they have statically matching constraints, all predicate specifications that apply to them come from the same declarations, Nonblocking aspects have the same value, global aspects statically match, Object_Size (see 13.3) has been specified to have a nonconfirming value for either both or neither, and the nonconfirming values, if any, are the same, and, for access subtypes, either both or neither exclude null. Two anonymous access-to-object subtypes statically match if their designated subtypes statically match, and either both or neither exclude null, and either both or neither are access-to-constant. Two anonymous access-to-subprogram subtypes statically match if their designated profiles are subtype conformant, and either both or neither exclude null. 

Ramification: Statically matching constraints and subtypes are the basis for subtype conformance of profiles (see 6.3.1). 

Reason: Even though anonymous access types always represent different types, they can statically match. That's important so that they can be used widely. For instance, if this wasn't true, access parameters and access discriminants could never conform, so they couldn't be used in separate specifications. 

Ramification: {AI12-0059-1} If one of the subtypes is not yet frozen, an implementation may have to repeat the check when the subtypes are both frozen (as it is impossible to check the Object_Size part before the subtypes are frozen). This recheck can only make a previously statically matching subtype fail to match; it cannot make a match legal. 

Discussion: {AI12-0059-1} We exclude the case where both Object_Sizes are confirming so that we don't introduce an incompatibility for existing Ada code. But practically the implementation can simply check that the Object_Size values are the same, as we have a rule in 13.1 that the subtype-specific aspects (such as Object_Size) are always the same for statically matching subtypes. We wrote the rules this way to avoid having wording that appeared to require predicting the future ("would statically match if ..."). 

Two ranges of the same type statically match if both result from the same evaluation of a [range](./AA-3.5#S0037), or if both are static and have equal corresponding bounds. 

Ramification: The notion of static matching of ranges is used in 12.5.3, "Formal Array Types"; the index ranges of formal and actual constrained array subtypes have to statically match. 

{AI05-0086-1} {AI05-0153-3} A constraint is statically compatible with a scalar subtype if it statically matches the constraint of the subtype, or if both are static and the constraint is compatible with the subtype. A constraint is statically compatible with an access or composite subtype if it statically matches the constraint of the subtype, or if the subtype is unconstrained. 

Discussion: Static compatibility is required when constraining a parent subtype with a discriminant from a new [discriminant_part](./AA-3.7#S0059). See 3.7. Static compatibility is also used in matching generic formal derived types.

Note that statically compatible with a subtype does not imply compatible with a type. It is OK since the terms are used in different contexts. 

{AI05-0153-3} Two statically matching subtypes are statically compatible with each other. In addition, a subtype S1 is statically compatible with a subtype S2 if: 

the constraint of S1 is statically compatible with S2, and

{AI05-0086-1} if S2 excludes null, so does S1, and

either: 

all predicate specifications that apply to S2 apply also to S1, or

{AI05-0290-1} {AI12-0071-1} both subtypes are static, every value that satisfies the predicates of S1 also satisfies the predicates of S2, and it is not the case that both types each have at least one applicable predicate specification, predicate checks are enabled (see 11.4.2) for S2, and predicate checks are not enabled for S1. 


#### Wording Changes from Ada 83

This subclause is new to Ada 95. 


#### Wording Changes from Ada 95

{AI95-00231-01} {AI95-00254-01} Added static matching rules for null exclusions and anonymous access-to-subprogram types; both of these are new.

{AI95-00311-01} We clarify that the constraint of the first subtype of a scalar formal type statically matches itself. 


#### Incompatibilities With Ada 2005

{AI05-0086-1} Correction: Updated the statically compatible rules to take null exclusions into account. This is technically incompatible, as it could cause a legal Ada 2005 program to be rejected; however, such a program violates the intent of the rules (for instance, 3.7(15)) and this probably will simply detect bugs. 


#### Wording Changes from Ada 2005

{AI05-0153-3} {AI05-0290-1} Modified static matching and static compatibility to take predicate aspects (see 3.2.4) into account. 


#### Wording Changes from Ada 2012

{AI12-0071-1} Corrigendum: Updated wording of static compatibility to use the new term "satisfies the predicates" (see 3.2.4).

{AI12-0059-1} Updated wording to take nonconfirming values of Object_Size into account.

{AI12-0374-2} Static matching now includes the effects of Global aspects and the Nonblocking aspect. 

