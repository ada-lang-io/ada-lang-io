---
sidebar_position:  32
---

# 4.4  Expressions

{AI05-0147-1} {AI05-0158-1} {AI05-0176-1} An expression is a formula that defines the computation or retrieval of a value. In this Reference Manual, the term "expression" refers to a construct of the syntactic category [expression](./AA-4.4#S0132) or of any of the following categories: [choice_expression](./AA-4.4#S0133), [choice_relation](./AA-4.4#S0134), [relation](./AA-4.4#S0135), [simple_expression](./AA-4.4#S0138), [term](./AA-4.4#S0139), [factor](./AA-4.4#S0140), [primary](./AA-4.4#S0141), [conditional_expression](./AA-4.5#S0148), [quantified_expression](./AA-4.5#S0153). 


#### Syntax

expression<a id="S0132"></a> ::= 
     [relation](./AA-4.4#S0135) {and [relation](./AA-4.4#S0135)} 	| [relation](./AA-4.4#S0135) {and then [relation](./AA-4.4#S0135)}
   | [relation](./AA-4.4#S0135) {or [relation](./AA-4.4#S0135)} 	| [relation](./AA-4.4#S0135) {or else [relation](./AA-4.4#S0135)}
   | [relation](./AA-4.4#S0135) {xor [relation](./AA-4.4#S0135)}

{AI05-0158-1} choice_expression<a id="S0133"></a> ::= 
     [choice_relation](./AA-4.4#S0134) {and [choice_relation](./AA-4.4#S0134)}
   | [choice_relation](./AA-4.4#S0134) {or [choice_relation](./AA-4.4#S0134)}
   | [choice_relation](./AA-4.4#S0134) {xor [choice_relation](./AA-4.4#S0134)}
   | [choice_relation](./AA-4.4#S0134) {and then [choice_relation](./AA-4.4#S0134)}
   | [choice_relation](./AA-4.4#S0134) {or else [choice_relation](./AA-4.4#S0134)}

{AI05-0158-1} choice_relation<a id="S0134"></a> ::= 
     [simple_expression](./AA-4.4#S0138) [[relational_operator](./AA-4.5#S0143) [simple_expression](./AA-4.4#S0138)]

{AI05-0158-1} {AI12-0022-1} {AI12-0039-1} relation<a id="S0135"></a> ::= 
     [simple_expression](./AA-4.4#S0138) [[relational_operator](./AA-4.5#S0143) [simple_expression](./AA-4.4#S0138)]
   | tested_[simple_expression](./AA-4.4#S0138) [not] in [membership_choice_list](./AA-4.4#S0136)
   | [raise_expression](./AA-11.3#S0309)

{AI05-0158-1} {AI12-0212-1} membership_choice_list<a id="S0136"></a> ::= [membership_choice](./AA-4.4#S0137) {'|' [membership_choice](./AA-4.4#S0137)}

{AI05-0158-1} {AI12-0039-1} membership_choice<a id="S0137"></a> ::= choice_[simple_expression](./AA-4.4#S0138) | [range](./AA-3.5#S0037) | [subtype_mark](./AA-3.2#S0028)

simple_expression<a id="S0138"></a> ::= [[unary_adding_operator](./AA-4.5#S0145)] [term](./AA-4.4#S0139) {[binary_adding_operator](./AA-4.5#S0144) [term](./AA-4.4#S0139)}

term<a id="S0139"></a> ::= [factor](./AA-4.4#S0140) {[multiplying_operator](./AA-4.5#S0146) [factor](./AA-4.4#S0140)}

factor<a id="S0140"></a> ::= [primary](./AA-4.4#S0141) [** [primary](./AA-4.4#S0141)] | abs [primary](./AA-4.4#S0141) | not [primary](./AA-4.4#S0141)

{AI05-0003-1} {AI05-0147-1} {AI05-0176-1} {AI12-0236-1} primary<a id="S0141"></a> ::= 
    [numeric_literal](./AA-2.4#S0006) | null | [string_literal](./AA-2.6#S0016) | [aggregate](./AA-4.3#S0106)
  | [name](./AA-4.1#S0091) | [allocator](./AA-4.8#S0164) | ([expression](./AA-4.4#S0132))
  | ([conditional_expression](./AA-4.5#S0148)) | ([quantified_expression](./AA-4.5#S0153))
  | ([declare_expression](./AA-4.5#S0156))


#### Name Resolution Rules

A [name](./AA-4.1#S0091) used as a [primary](./AA-4.4#S0141) shall resolve to denote an object or a value. 

Discussion: This replaces RM83-4.4(3). We don't need to mention named numbers explicitly, because the name of a named number denotes a value. We don't need to mention attributes explicitly, because attributes now denote (rather than yield) values in general. Also, the new wording allows attributes that denote objects, which should always have been allowed (in case the implementation chose to have such a thing). 

Reason: It might seem odd that this is an overload resolution rule, but it is relevant during overload resolution. For example, it helps ensure that a [primary](./AA-4.4#S0141) that consists of only the identifier of a parameterless function is interpreted as a [function_call](./AA-6.4#S0218) rather than directly as a [direct_name](./AA-4.1#S0092). 


#### Static Semantics

Each expression has a type; it specifies the computation or retrieval of a value of that type.

{AI12-0317-1} A [primary](./AA-4.4#S0141) that is an [expression](./AA-4.4#S0132) surrounded by ( and ) is known as a parenthesized expression.

{AI12-0317-1} Every [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132) consists of one or more operative constituent [name](./AA-4.1#S0091)s or [expression](./AA-4.4#S0132)s, only one of which is evaluated as part of evaluating the [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132) (the evaluated operative constituent). The operative constituents are determined as follows, according to the form of the [expression](./AA-4.4#S0132) (or [name](./AA-4.1#S0091)):

if the [expression](./AA-4.4#S0132) is a [conditional_expression](./AA-4.5#S0148), the operative constituents of its dependent_[expression](./AA-4.4#S0132)s;

if the [expression](./AA-4.4#S0132) (or [name](./AA-4.1#S0091)) is a parenthesized expression, a [qualified_expression](./AA-4.7#S0163), or a view conversion, the operative constituent(s) of its operand;

if the [expression](./AA-4.4#S0132) is a [declare_expression](./AA-4.5#S0156), the operative constituent(s) of its body_[expression](./AA-4.4#S0132);

otherwise, the [expression](./AA-4.4#S0132) (or [name](./AA-4.1#S0091)) itself. 

{AI12-0317-1} In certain contexts, we specify that an operative constituent shall (or shall not) be newly constructed. This means the operative constituent shall (or shall not) be an [aggregate](./AA-4.3#S0106) or a [function_call](./AA-6.4#S0218); in either case, a [raise_expression](./AA-11.3#S0309) is permitted.

To be honest: If an [if_expression](./AA-4.5#S0149) does not have an else clause, "True" is an operative constituent of the [expression](./AA-4.4#S0132) and it can be the evaluated operative constituent. 


#### Dynamic Semantics

The value of a [primary](./AA-4.4#S0141) that is a [name](./AA-4.1#S0091) denoting an object is the value of the object.

{AI12-0227-1} An expression of a numeric universal type is evaluated as if it has type root_integer (for universal_integer) or root_real (otherwise) unless the context identifies a specific type (in which case that type is used).

Ramification: This has no effect for a static expression; its value may be arbitrarily small or large since no specific type is expected for any expression for which this rule specifies one of the root types. The only effect of this rule is to allow Constraint_Error to be raised if the value is outside of the base range of root_integer or root_real when the expression is not static. 

Reason: This rule means that implementations don't have to support unlimited range math at run time for universal expressions. Note that universal expressions for which the context doesn't specify a specific type are quite rare; attribute prefixes and results are the only known cases. (For operators, 8.6 already specifies that the operator of a root type be used, which provides a specific type.) 


#### Implementation Permissions

For the evaluation of a [primary](./AA-4.4#S0141) that is a [name](./AA-4.1#S0091) denoting an object of an unconstrained numeric subtype, if the value of the object is outside the base range of its type, the implementation may either raise Constraint_Error or return the value of the object. 

Ramification: {AI05-0299-1} This means that if extra-range intermediates are used to hold the value of an object of an unconstrained numeric subtype, a Constraint_Error can be raised on a read of the object, rather than only on an assignment to it. Similarly, it means that computing the value of an object of such a subtype can be deferred until the first read of the object (presuming no side effects other than failing an Overflow_Check are possible). This permission is over and above that provided by subclause 11.6, since this allows the Constraint_Error to move to a different handler. 

Reason: This permission is intended to allow extra-range registers to be used efficiently to hold parameters and local variables, even if they might need to be transferred into smaller registers for performing certain predefined operations. 

Discussion: There is no need to mention other kinds of [primary](./AA-4.4#S0141)s, since any Constraint_Error to be raised can be "charged" to the evaluation of the particular kind of [primary](./AA-4.4#S0141). 


#### Examples

Examples of primaries: 

```ada
4.0                --  real literal
Pi                 --  named number
(1 .. 10 =&gt 0)     --  array aggregate
Sum                --  variable
Integer'Last       --  attribute
Sine(X)            --  function call
Color'(Blue)       --  qualified expression
Real(M*N)          --  conversion
(Line_Count + 10)  --  parenthesized expression 

```

Examples of expressions: 

```ada
{AI95-00433-01} Volume                      -- primary
not Destroyed               -- factor
2*Line_Count                -- term
-4.0                        -- simple expression
-4.0 + A                    -- simple expression
B**2 - 4.0*A*C              -- simple expression
R*Sin()*Cos()             -- simple expression
Password(1 .. 3) = "Bwv"    -- relation
Count in Small_Int          -- relation
Count not in Small_Int      -- relation
Index = 0 or Item_Hit       -- expression
(Cold and Sunny) or Warm    -- expression (parentheses are required)
A**(B**C)                   -- expression (parentheses are required)

```


#### Extensions to Ada 83

In Ada 83, out parameters and their nondiscriminant subcomponents are not allowed as primaries. These restrictions are eliminated in Ada 95.

In various contexts throughout the language where Ada 83 syntax rules had [simple_expression](./AA-4.4#S0138), the corresponding Ada 95 syntax rule has [expression](./AA-4.4#S0132) instead. This reflects the inclusion of modular integer types, which makes the logical operators "and", "or", and "xor" more useful in expressions of an integer type. Requiring parentheses to use these operators in such contexts seemed unnecessary and potentially confusing. Note that the bounds of a [range](./AA-3.5#S0037) still have to be specified by [simple_expression](./AA-4.4#S0138)s, since otherwise [expression](./AA-4.4#S0132)s involving membership tests might be ambiguous. Essentially, the operation ".." is of higher precedence than the logical operators, and hence uses of logical operators still have to be parenthesized when used in a bound of a range. 


#### Wording Changes from Ada 2005

{AI05-0003-1} Moved [qualified_expression](./AA-4.7#S0163) from [primary](./AA-4.4#S0141) to [name](./AA-4.1#S0091) (see 4.1). This allows the use of [qualified_expression](./AA-4.7#S0163)s in more places.

{AI05-0147-1} {AI05-0176-1} Added [conditional_expression](./AA-4.5#S0148) and [quantified_expression](./AA-4.5#S0153) to [primary](./AA-4.4#S0141).

{AI05-0158-1} Expanded membership test syntax (see 4.5.2). 


#### Inconsistencies With Ada 2012

{AI12-0039-1} Corrigendum: Revised membership syntax to eliminate ambiguities. In some cases, previously ambiguous membership expressions will now have an unambiguous meaning. If an Ada 2012 implementation chose the "wrong" meaning, the expression could silently change meaning. Virtually all such expressions will become illegal because of type mismatches (and thus be incompatible, not inconsistent). However, if the choices are all of a Boolean type, resolution might succeed. For instance, A in B | C and D now always means (A in B | C) and D, but the original Ada 2012 syntax would have allowed it to mean A in B | (C and D). If a compiler allowed the expression and interpreted it as the latter, the meaning of the expression would silently change. We expect this to be extremely rare as membership operations on Boolean types are unlikely (and this can happen only in code written for Ada 2012). 


#### Incompatibilities With Ada 2012

{AI12-0039-1} Corrigendum: The revised membership syntax will require parentheses in [membership_choice_list](./AA-4.4#S0136)s in some cases where the Ada 2012 grammar did not require them. For instance, A in B in C | D is now illegal. However, such expressions can be interpreted in multiple ways (either A in (B in C) | D or A in (B in C | D) for this example), so using such expressions is likely to be dangerous (another compiler might interpret the expression differently). In addition, all such expressions occur only in Ada 2012 syntax; so they should be rare. 


#### Wording Changes from Ada 2012

{AI12-0227-1} Correction: Added wording so that universal expressions evaluated at run time can raise Constraint_Error if the value is outside of the range of root_integer or root_real. We don't document this as an inconsistency because the rule requires no implementation to change (as Constraint_Error is not required); it just allows implementations that already raise Constraint_Error (which is all of them surveyed) to be considered correct.

{AI12-0236-1} Added [declare_expression](./AA-4.5#S0156) to [primary](./AA-4.4#S0141). It shares the rules about parentheses with [conditional_expression](./AA-4.5#S0148)s.

{AI12-0317-1} Added the definitions of "operative constituent" and "newly constructed" to centralize definitions that are needed for various rules and definitions across the Reference Manual. In particular, operative constituent is often used when we want the semantics or legality to be unchanged by the presence of parens, qualification, or view conversions. Examples are found in 4.3.2, 6.2, and 7.5. 

