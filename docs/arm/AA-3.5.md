---
sidebar_position:  21
---

# 3.5  Scalar Types

Scalar types comprise enumeration types, integer types, and real types. Enumeration types and integer types are called discrete types; each value of a discrete type has a position number which is an integer value. Integer types and real types are called numeric types. [All scalar types are ordered, that is, all relational operators are predefined for their values.]


#### Syntax

range_constraint<a id="S0036"></a> ::=  range [range](./AA-3.5#S0037)

range<a id="S0037"></a> ::=  [range_attribute_reference](./AA-4.1#S0102)
   | [simple_expression](./AA-4.4#S0138) .. [simple_expression](./AA-4.4#S0138)

Discussion: These need to be [simple_expression](./AA-4.4#S0138)s rather than more general [expression](./AA-4.4#S0132)s because ranges appear in membership tests and other contexts where [expression](./AA-4.4#S0132) .. [expression](./AA-4.4#S0132) would be ambiguous. 

A range has a lower bound and an upper bound and specifies a subset of the values of some scalar type (the type of the range). A range with lower bound L and upper bound R is described by "L .. R". If R is less than L, then the range is a null range, and specifies an empty set of values. Otherwise, the range specifies the values of the type from the lower bound to the upper bound, inclusive. A value belongs to a range if it is of the type of the range, and is in the subset of values specified by the range. A value satisfies a range constraint if it belongs to the associated range. One range is included in another if all values that belong to the first range also belong to the second. 


#### Name Resolution Rules

For a [subtype_indication](./AA-3.2#S0027) containing a [range_constraint](./AA-3.5#S0036), either directly or as part of some other [scalar_constraint](./AA-3.2#S0030), the type of the [range](./AA-3.5#S0037) shall resolve to that of the type determined by the [subtype_mark](./AA-3.2#S0028) of the [subtype_indication](./AA-3.2#S0027). For a [range](./AA-3.5#S0037) of a given type, the [simple_expression](./AA-4.4#S0138)s of the [range](./AA-3.5#S0037) (likewise, the [simple_expression](./AA-4.4#S0138)s of the equivalent [range](./AA-3.5#S0037) for a [range_attribute_reference](./AA-4.1#S0102)) are expected to be of the type of the [range](./AA-3.5#S0037). 

Discussion: In Ada 95, [constraint](./AA-3.2#S0029)s only appear within [subtype_indication](./AA-3.2#S0027)s; things that look like constraints that appear in type declarations are called something else like [real_range_specification](./AA-3.5#S0046)s.

{AI05-0299-1} We say "the expected type is ..." or "the type is expected to be ..." depending on which reads better. They are fundamentally equivalent, and both feed into the type resolution rules of subclause 8.6.

In some cases, it doesn't work to use expected types. For example, in the above rule, we say that the "type of the [range](./AA-3.5#S0037) shall resolve to ..." rather than "the expected type for the [range](./AA-3.5#S0037) is ..." We then use "expected type" for the bounds. If we used "expected" at both points, there would be an ambiguity, since one could apply the rules of 8.6 either on determining the type of the range, or on determining the types of the individual bounds. It is clearly important to allow one bound to be of a universal type, and the other of a specific type, so we need to use "expected type" for the bounds. Hence, we used "shall resolve to" for the type of the range as a whole. There are other situations where "expected type" is not quite right, and we use "shall resolve to" instead. 


#### Static Semantics

The base range of a scalar type is the range of finite values of the type that can be represented in every unconstrained object of the type; it is also the range supported at a minimum for intermediate values during the evaluation of expressions involving predefined operators of the type. 

Implementation Note: Note that in some machine architectures intermediates in an expression (particularly if static), and register-resident variables might accommodate a wider range. The base range does not include the values of this wider range that are not assignable without overflow to memory-resident objects.

Ramification: The base range of an enumeration type is the range of values of the enumeration type. 

Reason: If the representation supports infinities, the base range is nevertheless restricted to include only the representable finite values, so that 'Base'First and 'Base'Last are always guaranteed to be finite.

To be honest: By a "value that can be assigned without overflow" we don't mean to restrict ourselves to values that can be represented exactly. Values between machine representable values can be assigned, but on subsequent reading, a slightly different value might be retrieved, as (partially) determined by the number of digits of precision of the type. 

[A constrained scalar subtype is one to which a range constraint applies.] The range of a constrained scalar subtype is the range associated with the range constraint of the subtype. The range of an unconstrained scalar subtype is the base range of its type. 


#### Dynamic Semantics

A range is compatible with a scalar subtype if and only if it is either a null range or each bound of the range belongs to the range of the subtype. A [range_constraint](./AA-3.5#S0036) is compatible with a scalar subtype if and only if its range is compatible with the subtype. 

Ramification: Only [range_constraint](./AA-3.5#S0036)s (explicit or implicit) impose conditions on the values of a scalar subtype. The other [scalar_constraint](./AA-3.2#S0030)s, [digits_constraint](./AA-3.5#S0050)s and [delta_constraint](./AA-J.3#S0367)s impose conditions on the subtype denoted by the [subtype_mark](./AA-3.2#S0028) in a [subtype_indication](./AA-3.2#S0027), but don't impose a condition on the values of the subtype being defined. Therefore, a scalar subtype is not called constrained if all that applies to it is a [digits_constraint](./AA-3.5#S0050). Decimal subtypes are subtle, because a [digits_constraint](./AA-3.5#S0050) without a [range_constraint](./AA-3.5#S0036) nevertheless includes an implicit [range_constraint](./AA-3.5#S0036). 

The elaboration of a [range_constraint](./AA-3.5#S0036) consists of the evaluation of the [range](./AA-3.5#S0037). The evaluation of a [range](./AA-3.5#S0037) determines a lower bound and an upper bound. If [simple_expression](./AA-4.4#S0138)s are given to specify bounds, the evaluation of the [range](./AA-3.5#S0037) evaluates these [simple_expression](./AA-4.4#S0138)s in an arbitrary order, and converts them to the type of the [range](./AA-3.5#S0037). If a [range_attribute_reference](./AA-4.1#S0102) is given, the evaluation of the [range](./AA-3.5#S0037) consists of the evaluation of the [range_attribute_reference](./AA-4.1#S0102).

Attributes

For every scalar subtype S, the following attributes are defined: 

S'FirstS'First denotes the lower bound of the range of S. The value of this attribute is of the type of S. 

Ramification: Evaluating S'First never raises Constraint_Error.

S'LastS'Last denotes the upper bound of the range of S. The value of this attribute is of the type of S. 

Ramification: Evaluating S'Last never raises Constraint_Error.

S'RangeS'Range is equivalent to the [range](./AA-3.5#S0037) S'First .. S'Last.

S'BaseS'Base denotes an unconstrained subtype of the type of S. This unconstrained subtype is called the base subtype of the type. 

S'MinS'Min denotes a function with the following specification: 

```ada
function S'Min(Left, Right : S'Base)
  return S'Base

```

The function returns the lesser of the values of the two parameters. 

Discussion: The formal parameter names are italicized because they cannot be used in calls - see 6.4. Such a specification cannot be written by the user because an [attribute_reference](./AA-4.1#S0100) is not permitted as the designator of a user-defined function, nor can its formal parameters be anonymous. 

S'MaxS'Max denotes a function with the following specification: 

```ada
function S'Max(Left, Right : S'Base)
  return S'Base

```

The function returns the greater of the values of the two parameters.

S'SuccS'Succ denotes a function with the following specification: 

```ada
function S'Succ(Arg : S'Base)
  return S'Base

```

For an enumeration type, the function returns the value whose position number is one more than that of the value of Arg; Constraint_Error is raised if there is no such value of the type. For an integer type, the function returns the result of adding one to the value of Arg. For a fixed point type, the function returns the result of adding small to the value of Arg. For a floating point type, the function returns the machine number (as defined in 3.5.7) immediately above the value of Arg; Constraint_Error is raised if there is no such machine number. 

Ramification: S'Succ for a modular integer subtype wraps around if the value of Arg is S'Base'Last. S'Succ for a signed integer subtype might raise Constraint_Error if the value of Arg is S'Base'Last, or it might return the out-of-base-range value S'Base'Last+1, as is permitted for all predefined numeric operations. 

S'PredS'Pred denotes a function with the following specification: 

```ada
function S'Pred(Arg : S'Base)
  return S'Base

```

For an enumeration type, the function returns the value whose position number is one less than that of the value of Arg; Constraint_Error is raised if there is no such value of the type. For an integer type, the function returns the result of subtracting one from the value of Arg. For a fixed point type, the function returns the result of subtracting small from the value of Arg. For a floating point type, the function returns the machine number (as defined in 3.5.7) immediately below the value of Arg; Constraint_Error is raised if there is no such machine number. 

Ramification: S'Pred for a modular integer subtype wraps around if the value of Arg is S'Base'First. S'Pred for a signed integer subtype might raise Constraint_Error if the value of Arg is S'Base'First, or it might return the out-of-base-range value S'Base'First1, as is permitted for all predefined numeric operations. 

Paragraphs 28 through 37 were moved to 4.10, "Image Attributes". 

S'Wide_Wide_Width{AI95-00285-01} {AI12-0020-1} S'Wide_Wide_Width denotes the maximum length of a Wide_Wide_String returned by S'Wide_Wide_Image over all values of the subtype S, assuming a default implementation of S'Put_Image. It denotes zero for a subtype that has a null range. Its type is universal_integer.

S'Wide_Width{AI12-0020-1} S'Wide_Width denotes the maximum length of a Wide_String returned by S'Wide_Image over all values of the subtype S, assuming a default implementation of S'Put_Image. It denotes zero for a subtype that has a null range. Its type is universal_integer.

S'Width{AI12-0020-1} S'Width denotes the maximum length of a String returned by S'Image over all values of the subtype S, assuming a default implementation of S'Put_Image. It denotes zero for a subtype that has a null range. Its type is universal_integer.

S'Wide_Wide_Value{AI95-00285-01} S'Wide_Wide_Value denotes a function with the following specification: 

```ada
function S'Wide_Wide_Value(Arg : Wide_Wide_String)
  return S'Base

```

This function returns a value given an image of the value as a Wide_Wide_String, ignoring any leading or trailing spaces.

{AI05-0264-1} For the evaluation of a call on S'Wide_Wide_Value for an enumeration subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of an enumeration literal and if it corresponds to a literal of the type of S (or corresponds to the result of S'Wide_Wide_Image for a nongraphic character of the type), the result is the corresponding enumeration value; otherwise, Constraint_Error is raised. 

Discussion: It's not crystal clear that Range_Check is appropriate here, but it doesn't seem worthwhile to invent a whole new check name just for this weird case, so we decided to lump it in with Range_Check. 

To be honest: {8652/0096} {AI95-00053-01} A sequence of characters corresponds to the result of S'Wide_Wide_Image if it is the same ignoring case. Thus, the case of an image of a nongraphic character does not matter. For example, Character'Wide_Wide_Value("nul") does not raise Constraint_Error, even though Character'Wide_Wide_Image returns "NUL" for the nul character. 

{AI05-0264-1} For the evaluation of a call on S'Wide_Wide_Value for an integer subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of an integer literal, with an optional leading sign character (plus or minus for a signed type; only plus for a modular type), and the corresponding numeric value belongs to the base range of the type of S, then that value is the result; otherwise, Constraint_Error is raised.

Discussion: We considered allowing 'Value to return a representable but out-of-range value without a Constraint_Error. However, we currently require (see 4.9) in an [assignment_statement](./AA-5.2#S0173) like "X := &ltnumeric_literal&gt;" that the value of the numeric-literal be in X's base range (at compile time), so it seems unfriendly and confusing to have a different range allowed for 'Value. Furthermore, for modular types, without the requirement for being in the base range, 'Value would have to handle arbitrarily long literals (since overflow never occurs for modular types). 

For the evaluation of a call on S'Wide_Wide_Value for a real subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of one of the following: 

[numeric_literal](./AA-2.4#S0006)

[numeral](./AA-2.4#S0008).[[exponent](./AA-2.4#S0009)]

.[numeral](./AA-2.4#S0008)[[exponent](./AA-2.4#S0009)]

[base](./AA-2.4#S0012)#[based_numeral](./AA-2.4#S0013).#[[exponent](./AA-2.4#S0009)]

[base](./AA-2.4#S0012)#.[based_numeral](./AA-2.4#S0013)#[[exponent](./AA-2.4#S0009)] 

{AI05-0264-1} with an optional leading sign character (plus or minus), and if the corresponding numeric value belongs to the base range of the type of S, then that value is the result; otherwise, Constraint_Error is raised. The sign of a zero value is preserved (positive if none has been specified) if S'Signed_Zeros is True.

S'Wide_ValueS'Wide_Value denotes a function with the following specification: 

```ada
function S'Wide_Value(Arg : Wide_String)
  return S'Base

```

This function returns a value given an image of the value as a Wide_String, ignoring any leading or trailing spaces.

{AI95-00285-01} {AI05-0264-1} {AI12-0020-1} For the evaluation of a call on S'Wide_Value for an enumeration subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of an enumeration literal and if it corresponds to a literal of the type of S (or corresponds to the result of S'Wide_Image for a value of the type, assuming a default implementation of S'Put_Image), the result is the corresponding enumeration value; otherwise, Constraint_Error is raised. For a numeric subtype S, the evaluation of a call on S'Wide_Value with Arg of type Wide_String is equivalent to a call on S'Wide_Wide_Value for a corresponding Arg of type Wide_Wide_String. 

This paragraph was deleted.

This paragraph was deleted.{8652/0096} {AI95-00053-01} 

Reason: S'Wide_Value is subtly different from S'Wide_Wide_Value for enumeration subtypes since S'Wide_Image might produce a different sequence of characters than S'Wide_Wide_Image if the enumeration literal uses characters outside of the predefined type Wide_Character. That is why we don't just define S'Wide_Value in terms of S'Wide_Wide_Value for enumeration subtypes. S'Wide_Value and S'Wide_Wide_Value for numeric subtypes yield the same result given the same sequence of characters. 

Paragraphs 44 through 51 were moved to Wide_Wide_Value. 

S'ValueS'Value denotes a function with the following specification: 

```ada
function S'Value(Arg : String)
  return S'Base

```

This function returns a value given an image of the value as a String, ignoring any leading or trailing spaces.

{AI95-00285-01} {AI05-0264-1} {AI12-0020-1} For the evaluation of a call on S'Value for an enumeration subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of an enumeration literal and if it corresponds to a literal of the type of S (or corresponds to the result of S'Image for a value of the type, assuming a default implementation of S'Put_Image), the result is the corresponding enumeration value; otherwise, Constraint_Error is raised. For a numeric subtype S, the evaluation of a call on S'Value with Arg of type String is equivalent to a call on S'Wide_Wide_Value for a corresponding Arg of type Wide_Wide_String. 

Reason: {AI95-00285-01} S'Value is subtly different from S'Wide_Wide_Value for enumeration subtypes; see the discussion under S'Wide_Value. 


#### Implementation Permissions

{AI95-00285-01} An implementation may extend the Wide_Wide_Value, [Wide_Value, Value, Wide_Wide_Image, Wide_Image, and Image] attributes of a floating point type to support special values such as infinities and NaNs.

Proof: {AI95-00285-01} The permission is really only necessary for Wide_Wide_Value, because Value and Wide_Value are defined in terms of Wide_Wide_Value, and because the behavior of Wide_Wide_Image, Wide_Image, and Image is already unspecified for things like infinities and NaNs. 

Reason: This is to allow implementations to define full support for IEEE arithmetic. See also the similar permission for Get in A.10.9. 

{AI05-0182-1} {AI05-0262-1} {AI05-0269-1} An implementation may extend the Wide_Wide_Value, Wide_Value, and Value attributes of a character type to accept strings of the form "Hex_hhhhhhhh" (ignoring case) for any character (not just the ones for which Wide_Wide_Image would produce that form - see 3.5.2), as well as three-character strings of the form "'X'", where X is any character, including nongraphic characters. 


#### Static Semantics

{AI05-0228-1} For a scalar type, the following language-defined representation aspect may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1): 

Default_ValueThis aspect shall be specified by a static expression, and that expression shall be explicit, even if the aspect has a boolean type. Default_Value shall be specified only on a [full_type_declaration](./AA-3.2#S0024). 

Reason: The part about requiring an explicit expression is to disallow omitting the value for this aspect, which would otherwise be allowed by the rules of 13.1.1.

This is a representation aspect in order to disallow specifying it on a derived type that has inherited primitive subprograms; that is necessary as the sizes of out parameters could be different whether or not a Default_Value is specified (see 6.4.1). 

Aspect Description for Default_Value: Default value for a scalar subtype.

{AI05-0228-1} {AI12-0427-1} If a derived type inherits a boolean Default_Value aspect, the aspect may be specified to have any value for the derived type. If a derived type T does not inherit a Default_Value aspect, it shall not specify such an aspect if it inherits a primitive subprogram that has a parameter of type T of mode out. 

Reason: This overrides the 13.1.1 rule that says that a boolean aspect with a value True cannot be changed.

{AI12-0427-1} The second sentence is to avoid violating the rules specified in 6.4.1 about view conversions of out parameters with a specified Default_Value aspect. 


#### Name Resolution Rules

{AI05-0228-1} The expected type for the [expression](./AA-4.4#S0132) specified for the Default_Value aspect is the type defined by the [full_type_declaration](./AA-3.2#S0024) on which it appears. 

NOTE 1   The evaluation of S'First or S'Last never raises an exception. If a scalar subtype S has a nonnull range, S'First and S'Last belong to this range. These values can, for example, always be assigned to a variable of subtype S. 

Discussion: This paragraph addresses an issue that came up with Ada 83, where for fixed point types, the end points of the range specified in the type definition were not necessarily within the base range of the type. However, it was later clarified (and we reconfirm it in 3.5.9, "Fixed Point Types") that the First and Last attributes reflect the true bounds chosen for the type, not the bounds specified in the type definition (which might be outside the ultimately chosen base range). 

NOTE 2   {AI12-0442-1} For a subtype of a scalar type, the result delivered by the attributes Succ, Pred, and Value can be outside to the subtype; similarly, the actual parameters of the attributes Succ, Pred, and Image can also be outside the subtype.

NOTE 3   {AI95-00285-01} {AI12-0020-1} For any value V (including any nongraphic character) of an enumeration subtype S without a specified Put_Image (see 4.10), S'Value(S'Image(V)) equals V, as do S'Wide_Value(S'Wide_Image(V)) and S'Wide_Wide_Value(S'Wide_Wide_Image(V)). None of these expressions ever raise Constraint_Error. 


#### Examples

Examples of ranges: 

```ada
-10 .. 10
X .. X + 1
0.0 .. 2.0*Pi
Red .. Green     -- see 3.5.1
1 .. 0           -- a null range
Table'Range      -- a range attribute reference (see 3.6)

```

Examples of range constraints: 

```ada
range -999.0 .. +999.0
range S'First+1 .. S'Last-1

```


#### Incompatibilities With Ada 83

S'Base is no longer defined for nonscalar types. One conceivable existing use of S'Base for nonscalar types is S'Base'Size where S is a generic formal private type. However, that is not generally useful because the actual subtype corresponding to S might be a constrained array or discriminated type, which would mean that S'Base'Size might very well overflow (for example, S'Base'Size where S is a constrained subtype of String will generally be 8 * (Integer'Last + 1)). For derived discriminated types that are packed, S'Base'Size might not even be well defined if the first subtype is constrained, thereby allowing some amount of normally required "dope" to have been squeezed out in the packing. Hence our conclusion is that S'Base'Size is not generally useful in a generic, and does not justify keeping the attribute Base for nonscalar types just so it can be used as a [prefix](./AA-4.1#S0093).


#### Extensions to Ada 83

The attribute S'Base for a scalar subtype is now permitted anywhere a [subtype_mark](./AA-3.2#S0028) is permitted. S'Base'First .. S'Base'Last is the base range of the type. Using an [attribute_definition_clause](./AA-13.3#S0349), one cannot specify any subtype-specific attributes for the subtype denoted by S'Base (the base subtype).

The attribute S'Range is now allowed for scalar subtypes.

The attributes S'Min and S'Max are now defined, and made available for all scalar types.

The attributes S'Succ, S'Pred, S'Image, S'Value, and S'Width are now defined for real types as well as discrete types.

Wide_String versions of S'Image and S'Value are defined. These are called S'Wide_Image and S'Wide_Value to avoid introducing ambiguities involving uses of these attributes with string literals. 


#### Wording Changes from Ada 83

We now use the syntactic category [range_attribute_reference](./AA-4.1#S0102) since it is now syntactically distinguished from other attribute references.

The definition of S'Base has been moved here from 3.3.3 since it now applies only to scalar types.

More explicit rules are provided for nongraphic characters. 


#### Extensions to Ada 95

{AI95-00285-01} The attributes Wide_Wide_Image, Wide_Wide_Value, and Wide_Wide_Width are new. Note that Wide_Image and Wide_Value are now defined in terms of Wide_Wide_Image and Wide_Wide_Value, but the image of types other than characters have not changed. 


#### Wording Changes from Ada 95

{AI95-00285-01} The Wide_Image and Wide_Value attributes are now defined in terms of Wide_Wide_Image and Wide_Wide_Value, but the images of numeric types have not changed. 


#### Inconsistencies With Ada 2005

{AI05-0181-1} Correction: Soft hyphen (code point 173) is nongraphic in ISO/IEC 10646:2011 (and also in the 2003 version of that standard). Thus, we have given it the language-defined name soft_hyphen. This changes the result of Character'Image (and all of the related types and Image attributes) for this character, and changes the behavior of Character'Value (and all of the related types and Value attributes) for this character, and (in unusual circumstances), changes the result for Character'Width (and all of the related types and Width attributes). The vast majority of programs won't see any difference, as they are already prepared to handle nongraphic characters.

{AI05-0182-1} Correction: Added an Implementation Permissions to let Wide_Wide_Value, Wide_Value, and Value accept strings in the form of literals containing nongraphic characters and "Hex_hhhhhhhh" for Latin-1 and graphic characters. These were required to raise Constraint_Error in Ada 2005. Since these attributes aren't very useful, implementations were inconsistent as to whether these were accepted, and since code that would care why the attribute failed seems unlikely, this should not be a problem in practice. 


#### Extensions to Ada 2005

{AI05-0228-1} The new aspect Default_Value allows defining implicit initial values (see 3.3.1) for scalar types. 


#### Extensions to Ada 2012

{AI12-0124-1} Corrigendum: An object can be now used as the prefix of the Image attribute (as well as Wide_Image and Wide_Wide_Image), a convenience feature already present in some implementations.

{AI12-0020-1} All of the Image-family attributes have been moved to 4.10. 


## 3.5.1  Enumeration Types

[ An [enumeration_type_definition](./AA-3.5#S0038) defines an enumeration type.] 


#### Syntax

enumeration_type_definition<a id="S0038"></a> ::= 
   ([enumeration_literal_specification](./AA-3.5#S0039) {, [enumeration_literal_specification](./AA-3.5#S0039)})

enumeration_literal_specification<a id="S0039"></a> ::=  [defining_identifier](./AA-3.1#S0022) | [defining_character_literal](./AA-3.5#S0040)

defining_character_literal<a id="S0040"></a> ::= [character_literal](./AA-2.5#S0015)


#### Legality Rules

{AI05-0227-1} {AI05-0299-1} The [defining_identifier](./AA-3.1#S0022)s in upper case [and the [defining_character_literal](./AA-3.5#S0040)s] listed in an [enumeration_type_definition](./AA-3.5#S0038) shall be distinct. 

Proof: {AI05-0227-1} For character literals, this is a ramification of the normal disallowance of homographs explicitly declared immediately in the same declarative region. 

Reason: {AI05-0227-1} To ease implementation of the attribute Wide_Wide_Value, we require that all enumeration literals have distinct images. 


#### Static Semantics

{AI05-0006-1} Each [enumeration_literal_specification](./AA-3.5#S0039) is the explicit declaration of the corresponding enumeration literal: it declares a parameterless function, whose defining name is the [defining_identifier](./AA-3.1#S0022) or [defining_character_literal](./AA-3.5#S0040), and whose result subtype is the base subtype of the enumeration type. 

Reason: This rule defines the profile of the enumeration literal, which is used in the various types of conformance. 

Ramification: The parameterless function associated with an enumeration literal is fully defined by the [enumeration_type_definition](./AA-3.5#S0038); a body is not permitted for it, and it never fails the Elaboration_Check when called. 

Discussion: {AI05-0006-1} The result subtype is primarily a concern when an enumeration literal is used as the [expression](./AA-4.4#S0132) of a case statement, due to the full coverage requirement based on the nominal subtype. 

Each enumeration literal corresponds to a distinct value of the enumeration type, and to a distinct position number. The position number of the value of the first listed enumeration literal is zero; the position number of the value of each subsequent enumeration literal is one more than that of its predecessor in the list.

[The predefined order relations between values of the enumeration type follow the order of corresponding position numbers.]

[ If the same [defining_identifier](./AA-3.1#S0022) or [defining_character_literal](./AA-3.5#S0040) is specified in more than one [enumeration_type_definition](./AA-3.5#S0038), the corresponding enumeration literals are said to be overloaded. At any place where an overloaded enumeration literal occurs in the text of a program, the type of the enumeration literal has to be determinable from the context (see 8.6).] 


#### Dynamic Semantics

The elaboration of an [enumeration_type_definition](./AA-3.5#S0038) creates the enumeration type and its first subtype, which is constrained to the base range of the type. 

Ramification: The first subtype of a discrete type is always constrained, except in the case of a derived type whose parent subtype is Whatever'Base. 

When called, the parameterless function associated with an enumeration literal returns the corresponding value of the enumeration type. 

NOTE 1   If an enumeration literal occurs in a context that does not otherwise suffice to determine the type of the literal, then qualification by the name of the enumeration type is one way to resolve the ambiguity (see 4.7). 


#### Examples

Examples of enumeration types and subtypes: 

```ada
{AI12-0386-1} type Day        is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
type Month_Name is (January, February, March, April, May, June, July,
                    August, September, October, November, December);
type Suit       is (Clubs, Diamonds, Hearts, Spades);
type Gender     is (M, F);
type Level      is (Low, Medium, Urgent);
type Color      is (White, Red, Yellow, Green, Blue, Brown, Black);
type Light      is (Red, Amber, Green); -- Red and Green are overloaded

```

```ada
type Hexa       is ('A', 'B', 'C', 'D', 'E', 'F');
type Mixed      is ('A', 'B', '*', B, None, '?', '%');

```

```ada
subtype Weekday is Day   range Mon .. Fri;
subtype Major   is Suit  range Hearts .. Spades;
subtype Rainbow is Color range Red .. Blue;  --  the Color Red, not the Light

```


#### Wording Changes from Ada 83

The syntax rule for [defining_character_literal](./AA-3.5#S0040) is new. It is used for the defining occurrence of a [character_literal](./AA-2.5#S0015), analogously to [defining_identifier](./AA-3.1#S0022). Usage occurrences use the [name](./AA-4.1#S0091) or [selector_name](./AA-4.1#S0099) syntactic categories.

We emphasize the fact that an enumeration literal denotes a function, which is called to produce a value. 


#### Incompatibilities With Ada 2005

{AI05-0227-1} Correction: Required that all enumeration literals in a type have distinct images; this might not be the case since upper case conversion can map distinct characters to the same upper case character. This can only happen for identifiers using Unicode characters first allowed by Ada 2005; moreover, the original definition of Ada 2005 was confused and appeared to require inconsistent results from the Image attribute, so implementations that allowed problematic cases are rare; the problematic cases are very rare; so it is expected that this change would only affect test programs. 


#### Wording Changes from Ada 2005

{AI05-0006-1} Correction: Defined the result subtype of an enumeration literal to close a minor language hole. 


## 3.5.2  Character Types


#### Static Semantics

An enumeration type is said to be a character type if at least one of its enumeration literals is a [character_literal](./AA-2.5#S0015).

{AI95-00285-01} {AI05-0181-1} {AI05-0262-1} {AI05-0266-1} {AI12-0263-1} The predefined type Character is a character type whose values correspond to the 256 code points of Row 00 (also known as Latin-1) of the ISO/IEC 10646:2017 Basic Multilingual Plane (BMP). Each of the graphic characters of Row 00 of the BMP has a corresponding [character_literal](./AA-2.5#S0015) in Character. Each of the nongraphic characters of Row 00 has a corresponding language-defined name, which is not usable as an enumeration literal, but which is usable with the attributes Image, Wide_Image, Wide_Wide_Image, Value, Wide_Value, and Wide_Wide_Value; these names are given in the definition of type Character in A.1, "The Package Standard", but are set in italics. 

Discussion: {AI05-0262-1} {AI12-0263-1} Code point is defined in ISO/IEC 10646:2017. 

{AI95-00285-01} {AI05-0262-1} {AI12-0263-1} The predefined type Wide_Character is a character type whose values correspond to the 65536 code points of the ISO/IEC 10646:2017 Basic Multilingual Plane (BMP). Each of the graphic characters of the BMP has a corresponding [character_literal](./AA-2.5#S0015) in Wide_Character. The first 256 values of Wide_Character have the same [character_literal](./AA-2.5#S0015) or language-defined name as defined for Character. Each of the graphic_characters has a corresponding [character_literal](./AA-2.5#S0015).

{AI95-00285-01} {AI05-0262-1} {AI12-0263-1} The predefined type Wide_Wide_Character is a character type whose values correspond to the 2147483648 code points of the ISO/IEC 10646:2017 character set. Each of the graphic_characters has a corresponding [character_literal](./AA-2.5#S0015) in Wide_Wide_Character. The first 65536 values of Wide_Wide_Character have the same [character_literal](./AA-2.5#S0015) or language-defined name as defined for Wide_Character.

{AI95-00285-01} {AI05-0262-1} The characters whose code point is larger than 16#FF# and which are not graphic_characters have language-defined names which are formed by appending to the string "Hex_" the representation of their code point in hexadecimal as eight extended digits. As with other language-defined names, these names are usable only with the attributes (Wide_)Wide_Image and (Wide_)Wide_Value; they are not usable as enumeration literals.

Reason: {AI95-00285-01} The language-defined names are not usable as enumeration literals to avoid "polluting" the name space. Since Wide_Character and Wide_Wide_Character are defined in Standard, if the language-defined names were usable as enumeration literals, they would hide other nonoverloadable declarations with the same names in use-d packages.

Original Paragraphs 4 and 5 were deleted. 

NOTE 1   The language-defined library package Characters.Latin_1 (see A.3.3) includes the declaration of constants denoting control characters, lower case characters, and special characters of the predefined type Character. 

To be honest: The package ASCII does the same, but only for the first 128 characters of Character. Hence, it is an obsolescent package, and we no longer mention it here. 

NOTE 2   {AI05-0299-1} A conventional character set such as EBCDIC can be declared as a character type; the internal codes of the characters can be specified by an [enumeration_representation_clause](./AA-13.4#S0350) as explained in subclause 13.4. 


#### Examples

Example of a character type: 

```ada
type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');

```


#### Inconsistencies With Ada 83

The declaration of Wide_Character in package Standard hides use-visible declarations with the same defining identifier. In the unlikely event that an Ada 83 program had depended on such a use-visible declaration, and the program remains legal after the substitution of Standard.Wide_Character, the meaning of the program will be different. 


#### Incompatibilities With Ada 83

The presence of Wide_Character in package Standard means that an expression such as 

```ada
'a' = 'b'

```

is ambiguous in Ada 95, whereas in Ada 83 both literals could be resolved to be of type Character.

The change in visibility rules (see 4.2) for character literals means that additional qualification might be necessary to resolve expressions involving overloaded subprograms and character literals. 


#### Extensions to Ada 83

The type Character has been extended to have 256 positions, and the type Wide_Character has been added. Note that this change was already approved by the ARG for Ada 83 conforming compilers.

The rules for referencing character literals are changed (see 4.2), so that the declaration of the character type need not be directly visible to use its literals, similar to null and string literals. Context is used to resolve their type. 


#### Inconsistencies With Ada 95

{AI95-00285-01} Ada 95 defined most characters in Wide_Character to be graphic characters, while Ada 2005 uses the categorizations from ISO-10646:2003. It also provides language-defined names for all nongraphic characters. That means that in Ada 2005, Wide_Character'Wide_Value will raise Constraint_Error for a string representing a [character_literal](./AA-2.5#S0015) of a nongraphic character, while Ada 95 would have accepted it. Similarly, the result of Wide_Character'Wide_Image will change for such nongraphic characters.

{AI95-00395-01} {AI05-0005-1} {AI05-0262-1} The language-defined names FFFE and FFFF were replaced by a consistent set of language-defined names for all nongraphic characters with code points greater than 16#FF#. That means that in Ada 2005, Wide_Character'Wide_Value("FFFE") will raise Constraint_Error while Ada 95 would have accepted it. Similarly, the result of Wide_Character'Wide_Image will change for the position numbers 16#FFFE# and 16#FFFF#. It is very unlikely that this will matter in practice, as these names do not represent usable characters.

{AI95-00285-01} {AI95-00395-01} Because of the previously mentioned changes to the Wide_Character'Wide_Image of various character values, the value of attribute Wide_Width will change for some subtypes of Wide_Character. However, the new language-defined names were chosen so that the value of Wide_Character'Wide_Width itself does not change.

{AI95-00285-01} The declaration of Wide_Wide_Character in package Standard hides use-visible declarations with the same defining identifier. In the (very) unlikely event that an Ada 95 program had depended on such a use-visible declaration, and the program remains legal after the substitution of Standard.Wide_Wide_Character, the meaning of the program will be different. 


#### Extensions to Ada 95

{AI95-00285-01} The type Wide_Wide_Character is new. 


#### Wording Changes from Ada 95

{AI95-00285-01} Characters are now defined in terms of the entire ISO/IEC 10646:2003 character set.

{AI95-00285-01} {AI05-0248-1} We dropped the Implementation Advice for nonstandard interpretation of character sets; an implementation can do what it wants in a nonstandard mode, so there isn't much point to any advice. 


#### Wording Changes from Ada 2005

{AI05-0181-1} Correction: Removed the position numbers of nongraphic characters from the text, as it is wrong and thus misleading.

{AI05-0262-1} Changed "code position" to "code point" consistently throughout the standard, as ISO/IEC 10646:2011 prefers "code point" and we are referring to the definition in that Standard. This change also reduces confusion between "code point" and "position number"; while these have the same values for the predefined character types, there is no required relationship for other character types. 


## 3.5.3  Boolean Types


#### Static Semantics

There is a predefined enumeration type named Boolean, [declared in the visible part of package Standard]. It has the two enumeration literals False and True ordered with the relation False &lt True. Any descendant of the predefined type Boolean is called a boolean type. 

Implementation Note: An implementation is not required to support enumeration representation clauses on boolean types that impose an unacceptable implementation burden. See 13.4, "Enumeration Representation Clauses". However, it is generally straightforward to support representations where False is zero and True is 2**n  1 for some n. 


## 3.5.4  Integer Types

An [integer_type_definition](./AA-3.5#S0041) defines an integer type; it defines either a signed integer type, or a modular integer type. The base range of a signed integer type includes at least the values of the specified range. A modular type is an integer type with all arithmetic modulo a specified positive modulus; such a type corresponds to an unsigned type with wrap-around semantics. 


#### Syntax

integer_type_definition<a id="S0041"></a> ::= [signed_integer_type_definition](./AA-3.5#S0042) | [modular_type_definition](./AA-3.5#S0043)

signed_integer_type_definition<a id="S0042"></a> ::= range static_[simple_expression](./AA-4.4#S0138) .. static_[simple_expression](./AA-4.4#S0138)

Discussion: We don't call this a [range_constraint](./AA-3.5#S0036), because it is rather different - not only is it required to be static, but the associated overload resolution rules are different than for normal range constraints. A similar comment applies to [real_range_specification](./AA-3.5#S0046). This used to be integer_range_specification but when we added support for modular types, it seemed overkill to have three levels of syntax rules, and just calling these signed_integer_range_specification and modular_range_specification loses the fact that they are defining different classes of types, which is important for the generic type matching rules. 

modular_type_definition<a id="S0043"></a> ::= mod static_[expression](./AA-4.4#S0132)


#### Name Resolution Rules

{AI12-0444-1} Each [simple_expression](./AA-4.4#S0138) in a [signed_integer_type_definition](./AA-3.5#S0042) is expected to be of any integer type; they can be of different integer types. The [expression](./AA-4.4#S0132) in a [modular_type_definition](./AA-3.5#S0043) is likewise expected to be of any integer type. 


#### Legality Rules

The [simple_expression](./AA-4.4#S0138)s of a [signed_integer_type_definition](./AA-3.5#S0042) shall be static, and their values shall be in the range System.Min_Int .. System.Max_Int.

The [expression](./AA-4.4#S0132) of a [modular_type_definition](./AA-3.5#S0043) shall be static, and its value (the modulus) shall be positive, and shall be no greater than System.Max_Binary_Modulus if a power of 2, or no greater than System.Max_Nonbinary_Modulus if not. 

Reason: For a 2's-complement machine, supporting nonbinary moduli greater than System.Max_Int can be quite difficult, whereas essentially any binary moduli are straightforward to support, up to 2*System.Max_Int+2, so this justifies having two separate limits. 


#### Static Semantics

The set of values for a signed integer type is the (infinite) set of mathematical integers[, though only values of the base range of the type are fully supported for run-time operations]. The set of values for a modular integer type are the values from 0 to one less than the modulus, inclusive.

A [signed_integer_type_definition](./AA-3.5#S0042) defines an integer type whose base range includes at least the values of the [simple_expression](./AA-4.4#S0138)s and is symmetric about zero, excepting possibly an extra negative value. A [signed_integer_type_definition](./AA-3.5#S0042) also defines a constrained first subtype of the type, with a range whose bounds are given by the values of the [simple_expression](./AA-4.4#S0138)s, converted to the type being defined. 

Implementation Note: {AI95-00114-01} The base range of a signed integer type might be much larger than is necessary to satisfy the above requirements. 

To be honest: The conversion mentioned above is not an implicit subtype conversion (which is something that happens at overload resolution, see 4.6), although it happens implicitly. Therefore, the freezing rules are not invoked on the type (which is important so that representation items can be given for the type). 

A [modular_type_definition](./AA-3.5#S0043) defines a modular type whose base range is from zero to one less than the given modulus. A [modular_type_definition](./AA-3.5#S0043) also defines a constrained first subtype of the type with a range that is the same as the base range of the type.

There is a predefined signed integer subtype named Integer[, declared in the visible part of package Standard]. It is constrained to the base range of its type. 

Reason: Integer is a constrained subtype, rather than an unconstrained subtype. This means that on assignment to an object of subtype Integer, a range check is required. On the other hand, an object of subtype Integer'Base is unconstrained, and no range check (only overflow check) is required on assignment. For example, if the object is held in an extended-length register, its value might be outside of Integer'First .. Integer'Last. All parameter and result subtypes of the predefined integer operators are of such unconstrained subtypes, allowing extended-length registers to be used as operands or for the result. In an earlier version of Ada 95, Integer was unconstrained. However, the fact that certain Constraint_Errors might be omitted or appear elsewhere was felt to be an undesirable upward inconsistency in this case. Note that for Float, the opposite conclusion was reached, partly because of the high cost of performing range checks when not actually necessary. Objects of subtype Float are unconstrained, and no range checks, only overflow checks, are performed for them. 

Integer has two predefined subtypes, [declared in the visible part of package Standard:] 

```ada
subtype Natural  is Integer range 0 .. Integer'Last;
subtype Positive is Integer range 1 .. Integer'Last;

```

A type defined by an [integer_type_definition](./AA-3.5#S0041) is implicitly derived from root_integer, an anonymous predefined (specific) integer type, whose base range is System.Min_Int .. System.Max_Int. However, the base range of the new type is not inherited from root_integer, but is instead determined by the range or modulus specified by the [integer_type_definition](./AA-3.5#S0041). [Integer literals are all of the type universal_integer, the universal type (see 3.4.1) for the class rooted at root_integer, allowing their use with the operations of any integer type.] 

Discussion: This implicit derivation is not considered exactly equivalent to explicit derivation via a [derived_type_definition](./AA-3.4#S0035). In particular, integer types defined via a [derived_type_definition](./AA-3.4#S0035) inherit their base range from their parent type. A type defined by an [integer_type_definition](./AA-3.5#S0041) does not necessarily inherit its base range from root_integer. It is not specified whether the implicit derivation from root_integer is direct or indirect, not that it really matters. All we want is for all integer types to be descendants of root_integer.

{8652/0099} {AI95-00152-01} Note that this derivation does not imply any inheritance of subprograms. Subprograms are inherited only for types derived by a [derived_type_definition](./AA-3.4#S0035) (see 3.4), or a [private_extension_declaration](./AA-7.3#S0233) (see 7.3, 7.3.1, and 12.5.1). 

Implementation Note: It is the intent that even nonstandard integer types (see below) will be descendants of root_integer, even though they might have a base range that exceeds that of root_integer. This causes no problem for static calculations, which are performed without range restrictions (see 4.9). However for run-time calculations, it is possible that Constraint_Error might be raised when using an operator of root_integer on the result of 'Val applied to a value of a nonstandard integer type. 

The position number of an integer value is equal to the value.

{AI95-00340-01} For every modular subtype S, the following attributes are defined: 

S'Mod{AI95-00340-01} S'Mod denotes a function with the following specification:

```ada
function S'Mod (Arg : universal_integer)
  return S'Base

```

This function returns Arg mod S'Modulus, as a value of the type of S.

S'ModulusS'Modulus yields the modulus of the type of S, as a value of the type universal_integer. 


#### Dynamic Semantics

The elaboration of an [integer_type_definition](./AA-3.5#S0041) creates the integer type and its first subtype.

For a modular type, if the result of the execution of a predefined operator (see 4.5) is outside the base range of the type, the result is reduced modulo the modulus of the type to a value that is within the base range of the type.

For a signed integer type, the exception Constraint_Error is raised by the execution of an operation that cannot deliver the correct result because it is outside the base range of the type. [ For any integer type, Constraint_Error is raised by the operators "/", "rem", and "mod" if the right operand is zero.]


#### Implementation Requirements

In an implementation, the range of Integer shall include the range 2**15+1 .. +2**151.

If Long_Integer is predefined for an implementation, then its range shall include the range 2**31+1 .. +2**311.

System.Max_Binary_Modulus shall be at least 2**16. 


#### Implementation Permissions

{AI12-0444-1} For the execution of a predefined operation of a signed integer type, it is optional to raise Constraint_Error if the result is outside the base range of the type, so long as the correct result is produced. 

Discussion: Constraint_Error is never raised for operations on modular types, except for divide-by-zero (and rem/mod-by-zero). 

An implementation may provide additional predefined signed integer types[, declared in the visible part of Standard], whose first subtypes have names of the form Short_Integer, Long_Integer, Short_Short_Integer, Long_Long_Integer, etc. Different predefined integer types are allowed to have the same base range. However, the range of Integer should be no wider than that of Long_Integer. Similarly, the range of Short_Integer (if provided) should be no wider than Integer. Corresponding recommendations apply to any other predefined integer types. An implementation may support base ranges for which there is no corresponding named integer type. The range of each first subtype should be the base range of its type. 

{AI12-0444-1} Implementation defined: The predefined integer types declared in Standard.

{AI12-0444-1} An implementation may provide nonstandard integer types, descendants of root_integer that are declared outside of the specification of package Standard, which may have different characteristics than a type defined by an [integer_type_definition](./AA-3.5#S0041). For example, a nonstandard integer type can have an asymmetric base range or it can be disallowed as an array or loop index (a very long integer). Any type descended from a nonstandard integer type is also nonstandard. An implementation may place arbitrary restrictions on the use of such types; it is implementation defined whether operators that are predefined for "any integer type" are defined for a particular nonstandard integer type. [In any case, such types are not permitted as [explicit_generic_actual_parameter](./AA-12.3#S0318)s for formal scalar types - see 12.5.2.] 

Implementation defined: Any nonstandard integer types and the operators defined for them.

For a one's complement machine, the high bound of the base range of a modular type whose modulus is one less than a power of 2 may be equal to the modulus, rather than one less than the modulus. It is implementation defined for which powers of 2, if any, this permission is exercised.

{8652/0003} {AI95-00095-01} For a one's complement machine, implementations may support nonbinary modulus values greater than System.Max_Nonbinary_Modulus. It is implementation defined which specific values greater than System.Max_Nonbinary_Modulus, if any, are supported. 

Reason: On a one's complement machine, the natural full word type would have a modulus of 2**Word_Size1. However, we would want to allow the all-ones bit pattern (which represents negative zero as a number) in logical operations. These permissions are intended to allow that and the natural modulus value without burdening implementations with supporting expensive modulus values. 


#### Implementation Advice

An implementation should support Long_Integer in addition to Integer if the target machine supports 32-bit (or longer) arithmetic. No other named integer subtypes are recommended for package Standard. Instead, appropriate named integer subtypes should be provided in the library package Interfaces (see B.2). 

Implementation Advice: Long_Integer should be declared in Standard if the target supports 32-bit arithmetic. No other named integer subtypes should be declared in Standard.

Implementation Note: To promote portability, implementations should explicitly declare the integer (sub)types Integer and Long_Integer in Standard, and leave other predefined integer types anonymous. For implementations that already support Byte_Integer, etc., upward compatibility argues for keeping such declarations in Standard during the transition period, but perhaps generating a warning on use. A separate package Interfaces in the predefined environment is available for pre-declaring types such as Integer_8, Integer_16, etc. See B.2. In any case, if the user declares a subtype (first or not) whose range fits in, for example, a byte, the implementation can store variables of the subtype in a single byte, even if the base range of the type is wider. 

An implementation for a two's complement machine should support modular types with a binary modulus up to System.Max_Int*2+2. An implementation should support a nonbinary modulus up to Integer'Last. 

Implementation Advice: For a two's complement target, modular types with a binary modulus up to System.Max_Int*2+2 should be supported. A nonbinary modulus up to Integer'Last should be supported.

Reason: Modular types provide bit-wise "and", "or", "xor", and "not" operations. It is important for systems programming that these be available for all integer types of the target hardware. 

Ramification: Note that on a one's complement machine, the largest supported modular type would normally have a nonbinary modulus. On a two's complement machine, the largest supported modular type would normally have a binary modulus. 

Implementation Note: Supporting a nonbinary modulus greater than Integer'Last can impose an undesirable implementation burden on some machines. 

NOTE 1   Integer literals are of the anonymous predefined integer type universal_integer. Other integer types have no literals. However, the overload resolution rules (see 8.6, "The Context of Overload Resolution") allow expressions of the type universal_integer whenever an integer type is expected.

NOTE 2   The same arithmetic operators are predefined for all signed integer types defined by a [signed_integer_type_definition](./AA-3.5#S0042) (see 4.5, "Operators and Expression Evaluation"). For modular types, these same operators are predefined, plus bit-wise logical operators (and, or, xor, and not). In addition, for the unsigned types declared in the language-defined package Interfaces (see B.2), functions are defined that provide bit-wise shifting and rotating.

NOTE 3   Modular types match a [generic_formal_parameter_declaration](./AA-12.1#S0314) of the form "type T is mod &lt&gt;"; signed integer types match "type T is range &lt&gt;" (see 12.5.2). 


#### Examples

Examples of integer types and subtypes: 

```ada
type Page_Num  is range 1 .. 2_000;
type Line_Size is range 1 .. Max_Line_Size;

```

```ada
subtype Small_Int   is Integer   range -10 .. 10;
subtype Column_Ptr  is Line_Size range 1 .. 10;
subtype Buffer_Size is Integer   range 0 .. Max;

```

```ada
type Byte        is mod 256; -- an unsigned byte
type Hash_Index  is mod 97;  -- modulus is prime

```


#### Extensions to Ada 83

An implementation is allowed to support any number of distinct base ranges for integer types, even if fewer integer types are explicitly declared in Standard.

Modular (unsigned, wrap-around) types are new. 


#### Wording Changes from Ada 83

Ada 83's integer types are now called "signed" integer types, to contrast them with "modular" integer types.

Standard.Integer, Standard.Long_Integer, etc., denote constrained subtypes of predefined integer types, consistent with the Ada 95 model that only subtypes have names.

We now impose minimum requirements on the base range of Integer and Long_Integer.

We no longer explain integer type definition in terms of an equivalence to a normal type derivation, except to say that all integer types are by definition implicitly derived from root_integer. This is for various reasons.

First of all, the equivalence with a type derivation and a subtype declaration was not perfect, and was the source of various AIs (for example, is the conversion of the bounds static? Is a numeric type a derived type with respect to other rules of the language?)

Secondly, we don't want to require that every integer size supported shall have a corresponding named type in Standard. Adding named types to Standard creates nonportabilities.

Thirdly, we don't want the set of types that match a formal derived type "type T is new Integer;" to depend on the particular underlying integer representation chosen to implement a given user-defined integer type. Hence, we would have needed anonymous integer types as parent types for the implicit derivation anyway. We have simply chosen to identify only one anonymous integer type - root_integer, and stated that every integer type is derived from it.

Finally, the "fiction" that there were distinct preexisting predefined types for every supported representation breaks down for fixed point with arbitrary smalls, and was never exploited for enumeration types, array types, etc. Hence, there seems little benefit to pushing an explicit equivalence between integer type definition and normal type derivation. 


#### Extensions to Ada 95

{AI95-00340-01} The Mod attribute is new. It eases mixing of signed and unsigned values in an expression, which can be difficult as there may be no type which can contain all of the values of both of the types involved. 


#### Wording Changes from Ada 95

{8652/0003} {AI95-00095-01} Corrigendum: Added additional permissions for modular types on one's complement machines. 


## 3.5.5  Operations of Discrete Types


#### Static Semantics

For every discrete subtype S, the following attributes are defined: 

S'PosS'Pos denotes a function with the following specification: 

```ada
function S'Pos(Arg : S'Base)
  return universal_integer

```

This function returns the position number of the value of Arg, as a value of type universal_integer.

S'ValS'Val denotes a function with the following specification: 

```ada
function S'Val(Arg : universal_integer)
  return S'Base

```

This function returns a value of the type of S whose position number equals the value of Arg. For the evaluation of a call on S'Val, if there is no value in the base range of its type with the given position number, Constraint_Error is raised. 

Ramification: By the overload resolution rules, a formal parameter of type universal_integer allows an actual parameter of any integer type.

Reason: We considered allowing S'Val for a signed integer subtype S to return an out-of-range value, but since checks were required for enumeration and modular types anyway, the allowance didn't seem worth the complexity of the rule.

{AI05-0297-1} {AI12-0071-1} For every static discrete subtype S for which there exists at least one value belonging to S that satisfies the predicates of S, the following attributes are defined:

S'First_Valid{AI05-0297-1} {AI12-0071-1} S'First_Valid denotes the smallest value that belongs to S and satisfies the predicates of S. The value of this attribute is of the type of S.

S'Last_Valid{AI05-0297-1} {AI12-0071-1} S'Last_Valid denotes the largest value that belongs to S and satisfies the predicates of S. The value of this attribute is of the type of S. 

{AI05-0297-1} [First_Valid and Last_Valid [attribute_reference](./AA-4.1#S0100)s are always static expressions. Any explicit predicate of S can only have been specified by a Static_Predicate aspect.]

Proof: An [attribute_reference](./AA-4.1#S0100) is static if the prefix is a static subtype (see 4.9), (true by definition) and any arguments are static (there are none). Similarly, a dynamic predicate always makes a subtype nonstatic. QED. 

Reason: We require there to be at least one value so that these are always values of the subtype. (This sidesteps the question of what to return for a subtype with no values.) 

Discussion: These attributes are intended primarily for use in the case where the Static_Predicate aspect of S has been specified; First and Last are equivalent if these are allowed and there is no predicate. 


#### Implementation Advice

For the evaluation of a call on S'Pos for an enumeration subtype, if the value of the operand does not correspond to the internal code for any enumeration literal of its type [(perhaps due to an uninitialized variable)], then the implementation should raise Program_Error. This is particularly important for enumeration types with noncontiguous internal codes specified by an [enumeration_representation_clause](./AA-13.4#S0350). 

Implementation Advice: Program_Error should be raised for the evaluation of S'Pos for an enumeration type, if the value of the operand does not correspond to the internal code for any enumeration literal of the type.

Reason: We say Program_Error here, rather than Constraint_Error, because the main reason for such values is uninitialized variables, and the normal way to indicate such a use (if detected) is to raise Program_Error. (Other reasons would involve the misuse of low-level features such as Unchecked_Conversion.) 

NOTE 1   Indexing and loop iteration use values of discrete types.

NOTE 2   {AI05-0299-1} The predefined operations of a discrete type include the assignment operation, qualification, the membership tests, and the relational operators; for a boolean type they include the short-circuit control forms and the logical operators; for an integer type they include type conversion to and from other numeric types, as well as the binary and unary adding operators  and +, the multiplying operators, the unary operator abs, and the exponentiation operator. The assignment operation is described in 5.2. The other predefined operations are described in Clause 4.

NOTE 3   As for all types, objects of a discrete type have Size and Address attributes (see 13.3).

NOTE 4   {AI12-0442-1} For a subtype of a discrete type, the result delivered by the attribute Val can be outside the subtype; similarly, the actual parameter of the attribute Pos can also be outside the subtype. The following relations are satisfied (in the absence of an exception) by these attributes: 

```ada
   S'Val(S'Pos(X)) = X
   S'Pos(S'Val(N)) = N

```


#### Examples

Examples of attributes of discrete subtypes: 

```ada
--  For the types and subtypes declared in subclause 3.5.1 the following hold: 

```

```ada
--  Color'First   = White,   Color'Last   = Black
--  Rainbow'First = Red,     Rainbow'Last = Blue

```

```ada
--  Color'Succ(Blue) = Rainbow'Succ(Blue) = Brown
--  Color'Pos(Blue)  = Rainbow'Pos(Blue)  = 4
--  Color'Val(0)     = Rainbow'Val(0)     = White

```


#### Extensions to Ada 83

The attributes S'Succ, S'Pred, S'Width, S'Image, and S'Value have been generalized to apply to real types as well (see 3.5, "Scalar Types"). 


#### Extensions to Ada 2005

{AI05-0297-1} The attributes S'First_Valid and S'Last_Valid are new. 


#### Wording Changes from Ada 2012

{AI12-0071-1} Corrigendum: Updated wording of the attributes S'First_Valid and S'Last_Valid to use the new term "satisfies the predicates" (see 3.2.4). 


## 3.5.6  Real Types

Real types provide approximations to the real numbers, with relative bounds on errors for floating point types, and with absolute bounds for fixed point types. 


#### Syntax

real_type_definition<a id="S0044"></a> ::= 
   [floating_point_definition](./AA-3.5#S0045) | [fixed_point_definition](./AA-3.5#S0047)


#### Static Semantics

A type defined by a [real_type_definition](./AA-3.5#S0044) is implicitly derived from root_real, an anonymous predefined (specific) real type. [Hence, all real types, whether floating point or fixed point, are in the derivation class rooted at root_real.] 

Ramification: It is not specified whether the derivation from root_real is direct or indirect, not that it really matters. All we want is for all real types to be descendants of root_real.

{8652/0099} {AI95-00152-01} Note that this derivation does not imply any inheritance of subprograms. Subprograms are inherited only for types derived by a [derived_type_definition](./AA-3.4#S0035) (see 3.4), or a [private_extension_declaration](./AA-7.3#S0233) (see 7.3, 7.3.1, and 12.5.1).

[ Real literals are all of the type universal_real, the universal type (see 3.4.1) for the class rooted at root_real, allowing their use with the operations of any real type. Certain multiplying operators have a result type of universal_fixed (see 4.5.5), the universal type for the class of fixed point types, allowing the result of the multiplication or division to be used where any specific fixed point type is expected.] 


#### Dynamic Semantics

The elaboration of a [real_type_definition](./AA-3.5#S0044) consists of the elaboration of the [floating_point_definition](./AA-3.5#S0045) or the [fixed_point_definition](./AA-3.5#S0047). 


#### Implementation Requirements

An implementation shall perform the run-time evaluation of a use of a predefined operator of root_real with an accuracy at least as great as that of any floating point type definable by a [floating_point_definition](./AA-3.5#S0045). 

Ramification: Static calculations using the operators of root_real are exact, as for all static calculations. See 4.9. 

Implementation Note: The Digits attribute of the type used to represent root_real at run time is at least as great as that of any other floating point type defined by a [floating_point_definition](./AA-3.5#S0045), and its safe range includes that of any such floating point type with the same Digits attribute. On some machines, there might be real types with less accuracy but a wider range, and hence run-time calculations with root_real might not be able to accommodate all values that can be represented at run time in such floating point or fixed point types. 


#### Implementation Permissions

{AI95-00114-01} {AI12-0426-1} {AI12-0444-1} [For the execution of a predefined operation of a real type, it is optional to raise Constraint_Error if the result is outside the base range of the type, so long as the correct result is produced, or the Machine_Overflows attribute of the type is False (see G.2.1).]

{AI12-0426-1} {AI12-0444-1} An implementation may provide nonstandard real types, descendants of root_real that are declared outside of the specification of package Standard, which may have different characteristics than a type defined by a [real_type_definition](./AA-3.5#S0044). For example, a nonstandard real type can have an asymmetric or unsigned base range, or its predefined operations can wrap around or "saturate" rather than overflow (modular or saturating arithmetic), or it can have a different accuracy model than is standard (see G.2.1). Any type descended from a nonstandard real type is also nonstandard. An implementation may place arbitrary restrictions on the use of such types; it is implementation defined whether operators that are predefined for "any real type" are defined for a particular nonstandard real type. [In any case, such types are not permitted as [explicit_generic_actual_parameter](./AA-12.3#S0318)s for formal scalar types - see 12.5.2.] 

Implementation defined: Any nonstandard real types and the operators defined for them.

NOTE 1   As stated, real literals are of the anonymous predefined real type universal_real. Other real types have no literals. However, the overload resolution rules (see 8.6) allow expressions of the type universal_real whenever a real type is expected.


#### Wording Changes from Ada 83

The syntax rule for [real_type_definition](./AA-3.5#S0044) is modified to use the new syntactic categories [floating_point_definition](./AA-3.5#S0045) and [fixed_point_definition](./AA-3.5#S0047), instead of floating_point_constraint and fixed_point_constraint, because the semantics of a type definition are significantly different than the semantics of a constraint.

All discussion of model numbers, safe ranges, and machine numbers is moved to 3.5.7, 3.5.8, and G.2. Values of a fixed point type are now described as being multiples of the small of the fixed point type, and we have no need for model numbers, safe ranges, etc. for fixed point types.


## 3.5.7  Floating Point Types

For floating point types, the error bound is specified as a relative precision by giving the required minimum number of significant decimal digits. 


#### Syntax

floating_point_definition<a id="S0045"></a> ::= 
  digits static_[expression](./AA-4.4#S0132) [[real_range_specification](./AA-3.5#S0046)]

real_range_specification<a id="S0046"></a> ::= 
  range static_[simple_expression](./AA-4.4#S0138) .. static_[simple_expression](./AA-4.4#S0138)


#### Name Resolution Rules

The requested decimal precision, which is the minimum number of significant decimal digits required for the floating point type, is specified by the value of the [expression](./AA-4.4#S0132) given after the reserved word digits. This [expression](./AA-4.4#S0132) is expected to be of any integer type.

{AI12-0444-1} Each [simple_expression](./AA-4.4#S0138) of a [real_range_specification](./AA-3.5#S0046) is expected to be of any real type[; the types can be different]. 


#### Legality Rules

The requested decimal precision shall be specified by a static [expression](./AA-4.4#S0132) whose value is positive and no greater than System.Max_Base_Digits. Each [simple_expression](./AA-4.4#S0138) of a [real_range_specification](./AA-3.5#S0046) shall also be static. If the [real_range_specification](./AA-3.5#S0046) is omitted, the requested decimal precision shall be no greater than System.Max_Digits. 

Reason: We have added Max_Base_Digits to package System. It corresponds to the requested decimal precision of root_real. System.Max_Digits corresponds to the maximum value for Digits that may be specified in the absence of a [real_range_specification](./AA-3.5#S0046), for upward compatibility. These might not be the same if root_real has a base range that does not include ± 10.0**(4*Max_Base_Digits). 

A [floating_point_definition](./AA-3.5#S0045) is illegal if the implementation does not support a floating point type that satisfies the requested decimal precision and range. 

Implementation defined: What combinations of requested decimal precision and range are supported for floating point types.


#### Static Semantics

The set of values for a floating point type is the (infinite) set of rational numbers. The machine numbers of a floating point type are the values of the type that can be represented exactly in every unconstrained variable of the type. The base range (see 3.5) of a floating point type is symmetric around zero, except that it can include some extra negative values in some implementations.

Implementation Note: For example, if a 2's complement representation is used for the mantissa rather than a sign-mantissa or 1's complement representation, then there is usually one extra negative machine number.

To be honest: If the Signed_Zeros attribute is True, then minus zero could in a sense be considered a value of the type. However, for most purposes, minus zero behaves the same as plus zero.

The base decimal precision of a floating point type is the number of decimal digits of precision representable in objects of the type. The safe range of a floating point type is that part of its base range for which the accuracy corresponding to the base decimal precision is preserved by all predefined operations. 

Implementation Note: In most cases, the safe range and base range are the same. However, for some hardware, values near the boundaries of the base range might result in excessive inaccuracies or spurious overflows when used with certain predefined operations. For such hardware, the safe range would omit such values.

{AI12-0439-1} A [floating_point_definition](./AA-3.5#S0045) defines a floating point type whose base decimal precision is no less than the requested decimal precision. If a [real_range_specification](./AA-3.5#S0046) is given, the safe range of the floating point type (and hence, also its base range) includes at least the values of the simple expressions given in the [real_range_specification](./AA-3.5#S0046). If a [real_range_specification](./AA-3.5#S0046) is not given, the safe (and base) range of the type includes at least the values of the range 10.0**(4*D) .. +10.0**(4*D) where D is the requested decimal precision. [The safe range can include other values as well. The attributes Safe_First and Safe_Last give the actual bounds of the safe range.]

A [floating_point_definition](./AA-3.5#S0045) also defines a first subtype of the type. If a [real_range_specification](./AA-3.5#S0046) is given, then the subtype is constrained to a range whose bounds are given by a conversion of the values of the [simple_expression](./AA-4.4#S0138)s of the [real_range_specification](./AA-3.5#S0046) to the type being defined. Otherwise, the subtype is unconstrained.

To be honest: The conversion mentioned above is not an implicit subtype conversion (which is something that happens at overload resolution, see 4.6), although it happens implicitly. Therefore, the freezing rules are not invoked on the type (which is important so that representation items can be given for the type). 

There is a predefined, unconstrained, floating point subtype named Float[, declared in the visible part of package Standard]. 


#### Dynamic Semantics

[The elaboration of a [floating_point_definition](./AA-3.5#S0045) creates the floating point type and its first subtype.] 


#### Implementation Requirements

In an implementation that supports floating point types with 6 or more digits of precision, the requested decimal precision for Float shall be at least 6.

If Long_Float is predefined for an implementation, then its requested decimal precision shall be at least 11. 


#### Implementation Permissions

{AI12-0444-1} An implementation is allowed to provide additional predefined floating point types[, declared in the visible part of Standard], whose (unconstrained) first subtypes have names of the form Short_Float, Long_Float, Short_Short_Float, Long_Long_Float, etc. Different predefined floating point types are allowed to have the same base decimal precision. However, the precision of Float should be no greater than that of Long_Float. Similarly, the precision of Short_Float (if provided) should be no greater than Float. Corresponding recommendations apply to any other predefined floating point types. An implementation may support base decimal precisions for which there is no corresponding named floating point type. 

Implementation defined: The predefined floating point types declared in Standard.


#### Implementation Advice

An implementation should support Long_Float in addition to Float if the target machine supports 11 or more digits of precision. No other named floating point subtypes are recommended for package Standard. Instead, appropriate named floating point subtypes should be provided in the library package Interfaces (see B.2). 

Implementation Advice: Long_Float should be declared in Standard if the target supports 11 or more digits of precision. No other named float subtypes should be declared in Standard.

Implementation Note: To promote portability, implementations should explicitly declare the floating point (sub)types Float and Long_Float in Standard, and leave other predefined float types anonymous. For implementations that already support Short_Float, etc., upward compatibility argues for keeping such declarations in Standard during the transition period, but perhaps generating a warning on use. A separate package Interfaces in the predefined environment is available for pre-declaring types such as Float_32, IEEE_Float_64, etc. See B.2. 

NOTE 1   If a floating point subtype is unconstrained, then assignments to variables of the subtype involve only Overflow_Checks, never Range_Checks. 


#### Examples

Examples of floating point types and subtypes: 

```ada
type Coefficient is digits 10 range -1.0 .. 1.0;

```

```ada
type Real is digits 8;
type Mass is digits 7 range 0.0 .. 1.0E35;

```

```ada
subtype Probability is Real range 0.0 .. 1.0;   --   a subtype with a smaller range

```


#### Inconsistencies With Ada 83

No Range_Checks, only Overflow_Checks, are performed on variables (or parameters) of an unconstrained floating point subtype. This is upward compatible for programs that do not raise Constraint_Error. For those that do raise Constraint_Error, it is possible that the exception will be raised at a later point, or not at all, if extended range floating point registers are used to hold the value of the variable (or parameter). 

Reason: This change was felt to be justified by the possibility of improved performance on machines with extended-range floating point registers. An implementation need not take advantage of this relaxation in the range checking; it can hide completely the use of extended range registers if desired, presumably at some run-time expense. 


#### Wording Changes from Ada 83

The syntax rules for floating_point_constraint and floating_accuracy_definition are removed. The syntax rules for [floating_point_definition](./AA-3.5#S0045) and [real_range_specification](./AA-3.5#S0046) are new.

A syntax rule for [digits_constraint](./AA-3.5#S0050) is given in 3.5.9, "Fixed Point Types". In J.3 we indicate that a [digits_constraint](./AA-3.5#S0050) may be applied to a floating point [subtype_mark](./AA-3.2#S0028) as well (to be compatible with Ada 83's floating_point_constraint).

Discussion of model numbers is postponed to 3.5.8 and G.2. The concept of safe numbers has been replaced by the concept of the safe range of values. The bounds of the safe range are given by T'Safe_First .. T'Safe_Last, rather than -T'Safe_Large .. T'Safe_Large, since on some machines the safe range is not perfectly symmetric. The concept of machine numbers is new, and is relevant to the definition of Succ and Pred for floating point numbers. 


## 3.5.8  Operations of Floating Point Types


#### Static Semantics

The following attribute is defined for every floating point subtype S:

S'Digits{8652/0004} {AI95-00203-01} S'Digits denotes the requested decimal precision for the subtype S. The value of this attribute is of the type universal_integer. The requested decimal precision of the base subtype of a floating point type T is defined to be the largest value of d for which 
ceiling(d * log(10) / log(T'Machine_Radix)) + g &lt= T'Model_Mantissa
where g is 0 if Machine_Radix is a positive power of 10 and 1 otherwise. 

NOTE 1   The predefined operations of a floating point type include the assignment operation, qualification, the membership tests, and explicit conversion to and from other numeric types. They also include the relational operators and the following predefined arithmetic operators: the binary and unary adding operators  and +, certain multiplying operators, the unary operator abs, and the exponentiation operator.

NOTE 2   As for all types, objects of a floating point type have Size and Address attributes (see 13.3). Other attributes of floating point types are defined in A.5.3. 


#### Wording Changes from Ada 95

{8652/0004} {AI95-00203-01} Corrigendum: Corrected the formula for Digits when the Machine_Radix is 10. 


## 3.5.9  Fixed Point Types

A fixed point type is either an ordinary fixed point type, or a decimal fixed point type. The error bound of a fixed point type is specified as an absolute value, called the delta of the fixed point type. 


#### Syntax

fixed_point_definition<a id="S0047"></a> ::= [ordinary_fixed_point_definition](./AA-3.5#S0048) | [decimal_fixed_point_definition](./AA-3.5#S0049)

ordinary_fixed_point_definition<a id="S0048"></a> ::= 
   delta static_[expression](./AA-4.4#S0132)  [real_range_specification](./AA-3.5#S0046)

decimal_fixed_point_definition<a id="S0049"></a> ::= 
   delta static_[expression](./AA-4.4#S0132) digits static_[expression](./AA-4.4#S0132) [[real_range_specification](./AA-3.5#S0046)]

{AI12-0152-1} digits_constraint<a id="S0050"></a> ::= 
   digits static_[simple_expression](./AA-4.4#S0138) [[range_constraint](./AA-3.5#S0036)]


#### Name Resolution Rules

For a type defined by a [fixed_point_definition](./AA-3.5#S0047), the delta of the type is specified by the value of the [expression](./AA-4.4#S0132) given after the reserved word delta; this [expression](./AA-4.4#S0132) is expected to be of any real type. For a type defined by a [decimal_fixed_point_definition](./AA-3.5#S0049) (a decimal fixed point type), the number of significant decimal digits for its first subtype (the digits of the first subtype) is specified by the [expression](./AA-4.4#S0132) given after the reserved word digits; this [expression](./AA-4.4#S0132) is expected to be of any integer type.

{AI12-0159-1} The [simple_expression](./AA-4.4#S0138) of a [digits_constraint](./AA-3.5#S0050) is expected to be of any integer type. 


#### Legality Rules

In a [fixed_point_definition](./AA-3.5#S0047) or [digits_constraint](./AA-3.5#S0050), the [expression](./AA-4.4#S0132)s given after the reserved words delta and digits shall be static; their values shall be positive.

{AI95-00100-01} The set of values of a fixed point type comprise the integral multiples of a number called the small of the type. The machine numbers of a fixed point type are the values of the type that can be represented exactly in every unconstrained variable of the type. For a type defined by an [ordinary_fixed_point_definition](./AA-3.5#S0048) (an ordinary fixed point type), the small may be specified by an [attribute_definition_clause](./AA-13.3#S0349) (see 13.3); if so specified, it shall be no greater than the delta of the type. If not specified, the small of an ordinary fixed point type is an implementation-defined power of two less than or equal to the delta. 

Implementation defined: The small of an ordinary fixed point type.

For a decimal fixed point type, the small equals the delta; the delta shall be a power of 10. If a [real_range_specification](./AA-3.5#S0046) is given, both bounds of the range shall be in the range (10**digits1)*delta .. +(10**digits1)*delta.

A [fixed_point_definition](./AA-3.5#S0047) is illegal if the implementation does not support a fixed point type with the given small and specified range or digits. 

Implementation defined: What combinations of small, range, and digits are supported for fixed point types.

For a [subtype_indication](./AA-3.2#S0027) with a [digits_constraint](./AA-3.5#S0050), the [subtype_mark](./AA-3.2#S0028) shall denote a decimal fixed point subtype. 

To be honest: Or, as an obsolescent feature, a floating point subtype is permitted - see J.3. 


#### Static Semantics

The base range (see 3.5) of a fixed point type is symmetric around zero, except possibly for an extra negative value in some implementations.

An [ordinary_fixed_point_definition](./AA-3.5#S0048) defines an ordinary fixed point type whose base range includes at least all multiples of small that are between the bounds specified in the [real_range_specification](./AA-3.5#S0046). The base range of the type does not necessarily include the specified bounds themselves. An [ordinary_fixed_point_definition](./AA-3.5#S0048) also defines a constrained first subtype of the type, with each bound of its range given by the closer to zero of: 

the value of the conversion to the fixed point type of the corresponding [expression](./AA-4.4#S0132) of the [real_range_specification](./AA-3.5#S0046); 

To be honest: The conversion mentioned above is not an implicit subtype conversion (which is something that happens at overload resolution, see 4.6), although it happens implicitly. Therefore, the freezing rules are not invoked on the type (which is important so that representation items can be given for the type). 

the corresponding bound of the base range. 

A [decimal_fixed_point_definition](./AA-3.5#S0049) defines a decimal fixed point type whose base range includes at least the range (10**digits1)*delta .. +(10**digits1)*delta. A [decimal_fixed_point_definition](./AA-3.5#S0049) also defines a constrained first subtype of the type. If a [real_range_specification](./AA-3.5#S0046) is given, the bounds of the first subtype are given by a conversion of the values of the [expression](./AA-4.4#S0132)s of the [real_range_specification](./AA-3.5#S0046). Otherwise, the range of the first subtype is (10**digits1)*delta .. +(10**digits1)*delta.

To be honest: The conversion mentioned above is not an implicit subtype conversion (which is something that happens at overload resolution, see 4.6), although it happens implicitly. Therefore, the freezing rules are not invoked on the type (which is important so that representation items can be given for the type). 


#### Dynamic Semantics

The elaboration of a [fixed_point_definition](./AA-3.5#S0047) creates the fixed point type and its first subtype.

{AI12-0152-1} For a [digits_constraint](./AA-3.5#S0050) on a decimal fixed point subtype with a given delta, if it does not have a [range_constraint](./AA-3.5#S0036), then it specifies an implicit range (10**D1)*delta .. +(10**D1)*delta, where D is the value of the [simple_expression](./AA-4.4#S0138). A [digits_constraint](./AA-3.5#S0050) is compatible with a decimal fixed point subtype if the value of the [simple_expression](./AA-4.4#S0138) is no greater than the digits of the subtype, and if it specifies (explicitly or implicitly) a range that is compatible with the subtype. 

Discussion: Except for the requirement that the digits specified be no greater than the digits of the subtype being constrained, a [digits_constraint](./AA-3.5#S0050) is essentially equivalent to a [range_constraint](./AA-3.5#S0036).

Consider the following example: 

```ada
type D is delta 0.01 digits 7 range -0.00 .. 9999.99;

```

The compatibility rule implies that the [digits_constraint](./AA-3.5#S0050) "digits 6" specifies an implicit range of "9999.99 .. 9999.99". Thus, "digits 6" is not compatible with the constraint of D, but "digits 6 range 0.00 .. 9999.99" is compatible.

{AI95-00114-01} A value of a scalar type belongs to a constrained subtype of the type if it belongs to the range of the subtype. Attributes like Digits and Delta have no effect on this fundamental rule. So the obsolescent forms of [digits_constraint](./AA-3.5#S0050)s and [delta_constraint](./AA-J.3#S0367)s that are called "accuracy constraints" in RM83 don't really represent constraints on the values of the subtype, but rather primarily affect compatibility of the "constraint" with the subtype being "constrained". In this sense, they might better be called "subtype assertions" rather than "constraints".

Note that the [digits_constraint](./AA-3.5#S0050) on a decimal fixed point subtype is a combination of an assertion about the digits of the subtype being further constrained, and a constraint on the range of the subtype being defined, either explicit or implicit. 

{AI12-0152-1} The elaboration of a [digits_constraint](./AA-3.5#S0050) consists of the elaboration of the [range_constraint](./AA-3.5#S0036), if any. If a [range_constraint](./AA-3.5#S0036) is given, a check is made that the bounds of the range are both in the range (10**D1)*delta .. +(10**D1)*delta, where D is the value of the (static) [simple_expression](./AA-4.4#S0138) given after the reserved word digits. If this check fails, Constraint_Error is raised. 


#### Implementation Requirements

The implementation shall support at least 24 bits of precision (including the sign bit) for fixed point types. 

Reason: This is sufficient to represent Standard.Duration with a small no more than 50 milliseconds. 


#### Implementation Permissions

Implementations are permitted to support only smalls that are a power of two. In particular, all decimal fixed point type declarations can be disallowed. Note however that conformance with the Information Systems Annex requires support for decimal smalls, and decimal fixed point type declarations with digits up to at least 18. 

Implementation Note: The accuracy requirements for multiplication, division, and conversion (see G.2.1, "Model of Floating Point Arithmetic") are such that support for arbitrary smalls should be practical without undue implementation effort. Therefore, implementations should support fixed point types with arbitrary values for small (within reason). One reasonable limitation would be to limit support to fixed point types that can be converted to the most precise floating point type without loss of precision (so that Fixed_IO is implementable in terms of Float_IO). 

NOTE   {AI12-0442-1} The specified bounds themselves can be outside the base range of an ordinary fixed point type so that the range specification can be given in a natural way, such as: 

```ada
   type Fraction is delta 2.0**(-15) range -1.0 .. 1.0;
  

```

{AI12-0442-1} With 2's complement hardware, such a type would typically have a signed 16-bit representation, using 1 bit for the sign and 15 bits for fraction, resulting in a base range of 1.0 .. 1.02.0**(15). 


#### Examples

Examples of fixed point types and subtypes: 

```ada
type Volt is delta 0.125 range 0.0 .. 255.0;

```

```ada
  -- A pure fraction which requires all the available
  -- space in a word can be declared as the type Fraction:
type Fraction is delta System.Fine_Delta range -1.0 .. 1.0;
  -- Fraction'Last = 1.0  System.Fine_Delta

```

```ada
type Money is delta 0.01 digits 15;  -- decimal fixed point
subtype Salary is Money digits 10;
  -- Money'Last = 10.0**13  0.01, Salary'Last = 10.0**8  0.01

```


#### Inconsistencies With Ada 83

In Ada 95, S'Small always equals S'Base'Small, so if an implementation chooses a small for a fixed point type smaller than required by the delta, the value of S'Small in Ada 95 might not be the same as it was in Ada 83. 


#### Extensions to Ada 83

{AI05-0005-1} Decimal fixed point types are new, though their capabilities are essentially similar to that available in Ada 83 with a fixed point type whose small equals its delta and both are a power of 10. However, in the Information Systems Annex, additional requirements are placed on the support of decimal fixed point types (e.g. a minimum of 18 digits of precision). 


#### Wording Changes from Ada 83

The syntax rules for fixed_point_constraint and fixed_accuracy_definition are removed. The syntax rule for [fixed_point_definition](./AA-3.5#S0047) is new. A syntax rule for [delta_constraint](./AA-J.3#S0367) is included in the Obsolescent features (to be compatible with Ada 83's fixed_point_constraint). 


#### Wording Changes from Ada 95

{AI95-00100-01} Added wording to define the machine numbers of fixed point types; this is needed by the static evaluation rules. 


#### Incompatibilities With Ada 2012

{AI12-0152-1} Corrigendum: Changed the syntax so that the value following digits in a [digits_constraint](./AA-3.5#S0050) is a [simple_expression](./AA-4.4#S0138). This is compatible with one very unlikely exception: if the digits expression is a static expression of a modular type using an unparenthesized logical operator (like and or or). Parenthesizing the expression will make it legal in that case. The change is necessary to eliminate syntax ambguities in [derived_type_definition](./AA-3.4#S0035)s. 


#### Wording Changes from Ada 2012

{AI12-0159-1} Corrigendum:Added wording to define the expected type for a [digits_constraint](./AA-3.5#S0050). This was missing since Ada 95, but as it is obvious and unchanged from Ada 83, we don't consider it an incompatibility. 


## 3.5.10  Operations of Fixed Point Types


#### Static Semantics

The following attributes are defined for every fixed point subtype S: 

S'Small{8652/0005} {AI95-00054-01} {AI12-0367-1} S'Small denotes the small of the type of S. The value of this attribute is of the type universal_real. Small may be specified for nonderived ordinary fixed point types via an [attribute_definition_clause](./AA-13.3#S0349) (see 13.3); the expression of such a clause shall be static and positive.

Aspect Description for Small: Scale factor for a fixed point type.

S'DeltaS'Delta denotes the delta of the fixed point subtype S. The value of this attribute is of the type universal_real. 

Reason: The delta is associated with the subtype as opposed to the type, because of the possibility of an (obsolescent) [delta_constraint](./AA-J.3#S0367).

S'ForeS'Fore yields the minimum number of characters needed before the decimal point for the decimal representation of any value of the subtype S, assuming that the representation does not include an exponent, but includes a one-character prefix that is either a minus sign or a space. (This minimum number does not include superfluous zeros or underlines, and is at least 2.) The value of this attribute is of the type universal_integer.

S'AftS'Aft yields the number of decimal digits needed after the decimal point to accommodate the delta of the subtype S, unless the delta of the subtype S is greater than 0.1, in which case the attribute yields the value one. [(S'Aft is the smallest positive integer N for which (10**N)*S'Delta is greater than or equal to one.)] The value of this attribute is of the type universal_integer. 

The following additional attributes are defined for every decimal fixed point subtype S: 

S'DigitsS'Digits denotes the digits of the decimal fixed point subtype S, which corresponds to the number of decimal digits that are representable in objects of the subtype. The value of this attribute is of the type universal_integer. Its value is determined as follows: 

For a first subtype or a subtype defined by a [subtype_indication](./AA-3.2#S0027) with a [digits_constraint](./AA-3.5#S0050), the digits is the value of the expression given after the reserved word digits;

{AI12-0426-1} For a subtype defined by a [subtype_indication](./AA-3.2#S0027) without a [digits_constraint](./AA-3.5#S0050), the digits of the subtype is the same as that of the subtype denoted by the [subtype_mark](./AA-3.2#S0028) in the [subtype_indication](./AA-3.2#S0027); 

Implementation Note: Although a decimal subtype can be both range-constrained and digits-constrained, the digits constraint is intended to control the Size attribute of the subtype. For decimal types, Size can be important because input/output of decimal types is so common. 

The digits of a base subtype is the largest integer D such that the range (10**D1)*delta .. +(10**D1)*delta is included in the base range of the type.

S'ScaleS'Scale denotes the scale of the subtype S, defined as the value N such that S'Delta = 10.0**(N). [The scale indicates the position of the point relative to the rightmost significant digits of values of subtype S.] The value of this attribute is of the type universal_integer. 

Ramification: S'Scale is negative if S'Delta is greater than one. By contrast, S'Aft is always positive. 

S'RoundS'Round denotes a function with the following specification: 

```ada
function S'Round(X : universal_real)
  return S'Base

```

The function returns the value obtained by rounding X (away from 0, if X is midway between two values of the type of S). 

NOTE 1   All subtypes of a fixed point type will have the same value for the Delta attribute, in the absence of [delta_constraint](./AA-J.3#S0367)s (see J.3).

NOTE 2   S'Scale is not always the same as S'Aft for a decimal subtype; for example, if S'Delta = 1.0 then S'Aft is 1 while S'Scale is 0.

NOTE 3   The predefined operations of a fixed point type include the assignment operation, qualification, the membership tests, and explicit conversion to and from other numeric types. They also include the relational operators and the following predefined arithmetic operators: the binary and unary adding operators  and +, multiplying operators, and the unary operator abs.

NOTE 4   As for all types, objects of a fixed point type have Size and Address attributes (see 13.3). Other attributes of fixed point types are defined in A.5.4. 


#### Wording Changes from Ada 95

{8652/0005} {AI95-00054-01} Corrigendum: Clarified that small may be specified only for ordinary fixed point types. 


#### Wording Changes from Ada 2012

{AI12-0367-1} Correction: Clarified that small may be specified only with positive values. 

