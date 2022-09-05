---
sidebar_position:  30
---

# 4.2  Literals

[ A literal represents a value literally, that is, by means of notation suited to its kind.] A literal is either a [numeric_literal](./AA-2.4#S0006), a [character_literal](./AA-2.5#S0015), the literal null, or a [string_literal](./AA-2.6#S0016). 

Discussion: An enumeration literal that is an [identifier](./AA-2.3#S0002) rather than a [character_literal](./AA-2.5#S0015) is not considered a literal in the above sense, because it involves no special notation "suited to its kind". It might more properly be called an enumeration_identifier, except for historical reasons. 


#### Name Resolution Rules

This paragraph was deleted.{AI95-00230-01} 

For a [name](./AA-4.1#S0091) that consists of a [character_literal](./AA-2.5#S0015), either its expected type shall be a single character type, in which case it is interpreted as a parameterless [function_call](./AA-6.4#S0218) that yields the corresponding value of the character type, or its expected profile shall correspond to a parameterless function with a character result type, in which case it is interpreted as the name of the corresponding parameterless function declared as part of the character type's definition (see 3.5.1). In either case, the [character_literal](./AA-2.5#S0015) denotes the [enumeration_literal_specification](./AA-3.5#S0039). 

Discussion: See 4.1.3 for the resolution rules for a [selector_name](./AA-4.1#S0099) that is a [character_literal](./AA-2.5#S0015). 

{AI12-0325-1} {AI12-0373-1} The expected type for a [primary](./AA-4.4#S0141) that is a [string_literal](./AA-2.6#S0016) shall be a single string type or a type with a specified String_Literal aspect (see 4.2.1). In either case, the [string_literal](./AA-2.6#S0016) is interpreted to be of its expected type. If the expected type of an integer literal is a type with a specified Integer_Literal aspect (see 4.2.1), the literal is interpreted to be of its expected type; otherwise it is interpreted to be of type universal_integer. If the expected type of a real literal is a type with a specified Real_Literal aspect (see 4.2.1), it is interpreted to be of its expected type; otherwise, it is interpreted to be of type universal_real. 


#### Legality Rules

A [character_literal](./AA-2.5#S0015) that is a [name](./AA-4.1#S0091) shall correspond to a [defining_character_literal](./AA-3.5#S0040) of the expected type, or of the result type of the expected profile.

{AI12-0295-1} {AI12-0325-1} If the expected type for a string_literal is a string type, then for each character of the [string_literal](./AA-2.6#S0016) there shall be a corresponding [defining_character_literal](./AA-3.5#S0040) of the component type of the expected string type.

This paragraph was deleted.{AI95-00230-01} {AI95-00231-01} 


#### Static Semantics

{AI95-00230-01} {AI12-0373-1} The literal null is of type universal_access. 


#### Dynamic Semantics

{AI12-0249-1} If its expected type is a numeric type, the evaluation of a numeric literal yields the represented value. [In other cases, the effect of evaluating a numeric literal is determined by the Integer_Literal or Real_Literal aspect that applies (see 4.2.1).]

{AI12-0249-1} The evaluation of the literal null yields the null value of the expected type.

{AI12-0295-1} {AI12-0325-1} The evaluation of a [string_literal](./AA-2.6#S0016) that is a [primary](./AA-4.4#S0141) and has an expected type that is a string type, yields an array value containing the value of each character of the sequence of characters of the [string_literal](./AA-2.6#S0016), as defined in 2.6. The bounds of this array value are determined according to the rules for [positional_array_aggregate](./AA-4.3#S0114)s (see 4.3.3), except that for a null string literal, the upper bound is the predecessor of the lower bound. [In other cases, the effect of evaluating a [string_literal](./AA-2.6#S0016) is determined by the String_Literal aspect that applies (see 4.2.1).]

{AI12-0295-1} {AI12-0325-1} For the evaluation of a [string_literal](./AA-2.6#S0016) of a string type T, a check is made that the value of each character of the [string_literal](./AA-2.6#S0016) belongs to the component subtype of T. For the evaluation of a null string literal of a string type, a check is made that its lower bound is greater than the lower bound of the base range of the index type. The exception Constraint_Error is raised if either of these checks fails. 

Ramification: {AI12-0005-1} If no predicates apply to the component subtype, the checks on the characters need not involve more than two checks altogether, since one need only check the characters of the string with the lowest and highest position numbers against the range of the component subtype. 

NOTE 1   Enumeration literals that are [identifier](./AA-2.3#S0002)s rather than [character_literal](./AA-2.5#S0015)s follow the normal rules for [identifier](./AA-2.3#S0002)s when used in a [name](./AA-4.1#S0091) (see 4.1 and 4.1.3). [Character_literal](./AA-2.5#S0015)s used as [selector_name](./AA-4.1#S0099)s follow the normal rules for expanded names (see 4.1.3). 


#### Examples

Examples of literals: 

```ada
3.14159_26536 	--  a real literal
1_345 	--  an integer literal
'A' 	--  a character literal
"Some Text" 	--  a string literal 

```


#### Incompatibilities With Ada 83

Because [character_literal](./AA-2.5#S0015)s are now treated like other literals, in that they are resolved using context rather than depending on direct visibility, additional qualification might be necessary when passing a [character_literal](./AA-2.5#S0015) to an overloaded subprogram. 


#### Extensions to Ada 83

[Character_literal](./AA-2.5#S0015)s are now treated analogously to null and [string_literal](./AA-2.6#S0016)s, in that they are resolved using context, rather than their content; the declaration of the corresponding [defining_character_literal](./AA-3.5#S0040) need not be directly visible. 


#### Wording Changes from Ada 83

Name Resolution rules for enumeration literals that are not [character_literal](./AA-2.5#S0015)s are not included anymore, since they are neither syntactically nor semantically "literals" but are rather names of parameterless functions. 


#### Extensions to Ada 95

{AI95-00230-01} {AI95-00231-01} Null now has type universal_access, which is similar to other literals. Null can be used with anonymous access types. 


#### Wording Changes from Ada 2012

{AI12-0249-1} {AI12-0295-1} The rules in this subclause are adjusted to allow for the possibility of user-defined literals. These are fully documented in the next subclause. 


## 4.2.1  User-Defined Literals

{AI12-0249-1} Using one or more of the aspects defined below, a type may be specified to allow the use of one or more kinds of literals as values of the type. 


#### Static Semantics

{AI12-0249-1} {AI12-0342-1} The following type-related operational aspects (collectively known as user-defined literal aspects) may be specified for a type T:

{AI12-0249-1} {AI12-0342-1} {AI12-0373-1} Integer_LiteralThis aspect is specified by a function_[name](./AA-4.1#S0091) that statically denotes a function with a result type of T and one in parameter that is of type String and is not explicitly aliased.

Aspect Description for Integer_Literal: Defines a function to implement user-defined integer literals.

{AI12-0249-1} {AI12-0342-1} {AI12-0373-1} {AI12-0394-1} Real_LiteralThis aspect is specified by a function_[name](./AA-4.1#S0091) that statically denotes a function with a result type of T and one in parameter that is of type String and is not explicitly aliased, and optionally a second function [(overloading the first) ]with a result type of T and two in parameters of type String that are not explicitly aliased.

Aspect Description for Real_Literal: Defines a function or functions to implement user-defined real literals.

{AI12-0295-1} {AI12-0342-1} {AI12-0373-1} String_LiteralThis aspect is specified by a function_[name](./AA-4.1#S0091) that statically denotes a function with a result type of T and one in parameter that is of type Wide_Wide_String and is not explicitly aliased.

Aspect Description for String_Literal: Defines a function to implement user-defined string literals.

Ramification: {AI12-0342-1} The following example is legal because the resolution of an [aspect_definition](./AA-13.1#S0348) for an aspect that is defined to be a subprogram is based on the profile required for that aspect (see 13.1.1): 

```ada
package Pkg1 is
   type T is record X, Y : Integer; end record
     with Integer_Literal =&gt Int_Lit;
   function Int_Lit (X, Y : T) return Duration;    -- Wrong profile.
   function Int_Lit (Lit_Image : String) return T; -- Right profile.
end Pkg1;

```

{AI12-0342-1} {AI12-0419-1} User-defined literal aspects are nonoverridable (see 13.1.1). 

Discussion: This means that in this example

```ada
package Pkg2 is
   type T1 is record
      X, Y : Integer;
   end record with Integer_Literal =&gt I_L;

```

```ada
   function I_L (S : String) return T1 is ((0, 0));

```

```ada
   type T2 is new T1;
   function I_L (S : String) return T2 is ((1, 1));
   X : T2 := 123;
end Pkg2;

```

{AI12-0005-1} {AI12-0342-1} {AI12-0419-1} the initial value of Pkg2.X is (1,1), not (0,0). 

{AI12-0342-1} {AI12-0427-1} When a numeric literal is interpreted as a value of a non-numeric type T or a [string_literal](./AA-2.6#S0016) is interpreted as a value of a type T that is not a string type (see 4.2), it is equivalent to a call to the subprogram denoted by the corresponding aspect of T: the Integer_Literal aspect for an integer literal, the Real_Literal aspect for a real literal, and the String_Literal aspect for a [string_literal](./AA-2.6#S0016). The actual parameter of this notional call is a [string_literal](./AA-2.6#S0016) representing a sequence of characters that is the same as the sequence of characters in the original numeric literal, or the sequence represented by the original string literal.

Discussion: This equivalence defines, for example, the nominal type, the nominal subtype, and the accessibility level of a user-defined literal. It also has the consequence that a user-defined literal shall not be of an abstract type (because that would be equivalent to a nondispatching call to an abstract function). This equivalence also defines the Dynamic Semantics of evaluating a user-defined literal.

The (sub)type of the actual parameter to this call is determined by the profile of the appropriate aspect, and the bounds of the [string_literal](./AA-2.6#S0016) are defined by the usual rules for the bounds of a [string_literal](./AA-2.6#S0016). 

{AI12-0342-1} Such a literal is said to be a user-defined literal.

{AI12-0394-1} When a named number that denotes a value of type universal_integer is interpreted as a value of a non-numeric type T, it is equivalent to a call to the function denoted by the Integer_Literal aspect of T. The actual parameter of this notional call is a String having a textual representation of a decimal integer literal optionally preceded by a minus sign, representing the same value as the named number.

{AI12-0394-1} When a named number that denotes a value of type universal_real is interpreted as a value of a non-numeric type T, it is equivalent to a call to the two-parameter function denoted by the Real_Literal aspect of T, if any. The actual parameters of this notional call are each a String with the textual representation of a decimal integer literal, with the first optionally preceded by a minus sign, where the first String represents the same value as the numerator, and the second the same value as the denominator, of the named number when represented as a rational number in lowest terms, with a positive denominator.


#### Legality Rules

{AI12-0249-1} {AI12-0295-1} {AI12-0325-1} {AI12-0342-1} The Integer_Literal or Real_Literal aspect shall not be specified for a type T if the full view of T is a numeric type. The String_Literal aspect shall not be specified for a type T if the full view of T is a string type.

{AI12-0342-1} For a nonabstract type, the function directly specified for a user-defined literal aspect shall not be abstract.

{AI12-0342-1} For a tagged type with a partial view, a user-defined literal aspect shall not be directly specified on the full type.

{AI12-0342-1} {AI12-0394-1} If a nonabstract tagged type inherits any user-defined literal aspect, then each inherited aspect shall be directly specified as a nonabstract function for the type unless the inherited aspect denotes a nonabstract function, or functions, and the type is a null extension.

{AI12-0394-1} If a named number that denotes a value of type universal_integer is interpreted as a value of a non-numeric type T, T shall have an Integer_Literal aspect. If a named number that denotes a value of type universal_real is interpreted as a value of a non-numeric type T, T shall have a Real_Literal aspect, and the aspect shall denote a function that has two in parameters, both of type String, with result of type T.

{AI12-0249-1} {AI12-0342-1} In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit. 


#### Bounded (Run-Time) Errors

{AI12-0249-1} {AI12-0325-1} {AI12-0342-1} {AI12-0394-1} It is a bounded error if the evaluation of a literal or named number that has an expected type with a specified user-defined literal aspect propagates an exception. Either Program_Error or the exception propagated by the evaluation is raised at the point of use of the value of the literal or named number. If it is recognized prior to run time that evaluation of such a literal or named number will inevitably (if executed) result in such a bounded error, then this may be reported as an error prior to run time.

Implementation Note: {AI12-0249-1} As always, an implementation may apply "as-if" optimizations (those that result in the same external effects in the absence of erroneous execution) to the function calls associated with user-defined literals. In particular, if the function associated with a user-defined literal aspect has a Global aspect that indicates no references to global variables, then a number of optimizations are available to the implementation: 

The implementation can evaluate a user-defined literal function at compile-time if it has access to the body of the function (for example, if it is inlined), and that body doesn't reference anything evaluated at runtime. If the compile-time evaluation results in an exception, this bounded error allows the compilation unit to be rejected.

Implementations can use existing permissions (see 6.1.2) to avoid evaluating the function associated with a user-defined literal more than once for a particular literal value. This evaluation can be "hoisted" (done once per compilation unit during the elaboration of the unit) if the compiler can prove that the function doesn't depend on any constants or locals with a runtime value not yet elaborated.

If the literal value is not needed by the execution of the program, the function call can be omitted even if it might have side-effects (again, see 6.1.2). 


#### Examples

{AI12-0429-1} Examples of the specification and use of user-defined literals:

```ada
{AI12-0312-1} subtype Roman_Character is Wide_Wide_Character
   with Static_Predicate =&gt
      Roman_Character in 'I' | 'V' | 'X' | 'L' | 'C' | 'D' | 'M';

```

```ada
{AI12-0312-1} Max_Roman_Number : constant := 3_999;  -- MMMCMXCIX

```

```ada
{AI12-0312-1} type Roman_Number is range 1 .. Max_Roman_Number
   with String_Literal =&gt To_Roman_Number;

```

```ada
{AI12-0312-1} function To_Roman_Number (S : Wide_Wide_String) return Roman_Number
   with Pre =&gt S'Length &gt 0 and then
      (for all Char of S =&gt Char in Roman_Character);

```

```ada
{AI12-0312-1} {AI12-0386-1} function To_Roman_Number (S : Wide_Wide_String) return Roman_Number is
   (declare
      R : constant array (Integer range &lt&gt) of Roman_Number :=
         (for D in S'Range =&gt Roman_Digit'Enum_Rep
             (Roman_Digit'Wide_Wide_Value (''' & S(D) & ''')));
                     -- See 3.5.2 and 13.4
    begin
      [for I in R'Range =&gt
         (if I &lt R'Last and then R(I) &lt R(I + 1) then -1 else 1) * R(I)]
            'Reduce("+", 0)
   );

```

```ada
{AI12-0312-1} X : Roman_Number := "III" * "IV" * "XII"; -- 144 (that is, CXLIV)

```


#### Extensions to Ada 2012

{AI12-0249-1} {AI12-0295-1} {AI12-0325-1} {AI12-0342-1} The user-defined literal aspects Integer_Literal, Real_Literal, and String_Literal are new. 

