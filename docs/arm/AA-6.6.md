---
sidebar_position:  54
---

# 6.6  Overloading of Operators

An operator is a function whose [designator](./AA-6.1#S0199) is an [operator_symbol](./AA-6.1#S0202). [Operators, like other functions, may be overloaded.] 


#### Name Resolution Rules

Each use of a unary or binary operator is equivalent to a [function_call](./AA-6.4#S0218) with function_[prefix](./AA-4.1#S0093) being the corresponding [operator_symbol](./AA-6.1#S0202), and with (respectively) one or two positional actual parameters being the operand(s) of the operator (in order). 

To be honest: {AI05-0299-1} We also use the term operator (in Clause 4 and in 6.1) to refer to one of the syntactic categories defined in 4.5, "Operators and Expression Evaluation" whose names end with "_operator:" [logical_operator](./AA-4.5#S0142), [relational_operator](./AA-4.5#S0143), [binary_adding_operator](./AA-4.5#S0144), [unary_adding_operator](./AA-4.5#S0145), [multiplying_operator](./AA-4.5#S0146), and [highest_precedence_operator](./AA-4.5#S0147). 

Discussion: {AI05-0005-1} This equivalence extends to uses of [function_call](./AA-6.4#S0218) in most other language rules. However, as often happens, the equivalence is not perfect, as operator calls are not a [name](./AA-4.1#S0091), while a [function_call](./AA-6.4#S0218) is a [name](./AA-4.1#S0091). Thus, operator calls cannot be used in contexts that require a [name](./AA-4.1#S0091) (such as a rename of an object). A direct fix for this problem would be very disruptive, and thus we have not done that. However, qualifying an operator call can be used as a workaround in contexts that require a [name](./AA-4.1#S0091). 


#### Legality Rules

{AI05-0143-1} The [subprogram_specification](./AA-6.1#S0196) of a unary or binary operator shall have one or two parameters, respectively. The parameters shall be of mode in. A generic function instantiation whose [designator](./AA-6.1#S0199) is an [operator_symbol](./AA-6.1#S0202) is only allowed if the specification of the generic function has the corresponding number of parameters, and they are all of mode in.

[Default_expression](./AA-3.7#S0063)s are not allowed for the parameters of an operator (whether the operator is declared with an explicit [subprogram_specification](./AA-6.1#S0196) or by a [generic_instantiation](./AA-12.3#S0315)).

An explicit declaration of "/=" shall not have a result type of the predefined type Boolean. 


#### Static Semantics

{AI05-0128-1} An explicit declaration of "=" whose result type is Boolean implicitly declares an operator "/=" that gives the complementary result. 

Discussion: {AI05-0128-1} A "/=" defined by this rule is considered user-defined, which means that it will be inherited by a derived type. "User-defined" means "not language-defined" for the purposes of inheritance, that is anything other than predefined operators. 

NOTE 1   {AI12-0440-1} The operators "+" and "" are both unary and binary operators, and hence can be overloaded with both one- and two-parameter functions. 


#### Examples

Examples of user-defined operators: 

```ada
function "+" (Left, Right : Matrix) return Matrix;
function "+" (Left, Right : Vector) return Vector;

--  assuming that A, B, and C are of the type Vector
--  the following two statements are equivalent:

A := B + C;
A := "+"(B, C);

```


#### Extensions to Ada 83

Explicit declarations of "=" are now permitted for any combination of parameter and result types.

Explicit declarations of "/=" are now permitted, so long as the result type is not Boolean. 


#### Wording Changes from Ada 2005

{AI05-0128-1} Correction: Corrected the wording so that only explicit declarations of "=" cause an implicit declaration of "/="; otherwise, we could get multiple implicit definitions of "/=" without an obvious way to chose between them.

{AI05-0143-1} Added wording so that operators only allow parameters of mode in. This was made necessary by the elimination elsewhere of the restriction that function parameters be only of mode in. 

