---
sidebar_position:  43
---

# 5.4  Case Statements

[A [case_statement](./AA-5.4#S0176) selects for execution one of a number of alternative sequences_of_statements; the chosen alternative is defined by the value of an expression.] 


#### Syntax

{AI05-0188-1} case_statement<a id="S0176"></a> ::= 
   case selecting_[expression](./AA-4.4#S0132) is
       [case_statement_alternative](./AA-5.4#S0177)
      {[case_statement_alternative](./AA-5.4#S0177)}
   end case;

case_statement_alternative<a id="S0177"></a> ::= 
   when [discrete_choice_list](./AA-3.8#S0073) =&gt
      [sequence_of_statements](./AA-5.1#S0166)


#### Name Resolution Rules

{AI05-0188-1} The selecting_[expression](./AA-4.4#S0132) is expected to be of any discrete type. The expected type for each [discrete_choice](./AA-3.8#S0074) is the type of the selecting_[expression](./AA-4.4#S0132). 


#### Legality Rules

{AI05-0153-3} The [choice_expression](./AA-4.4#S0133)s, [subtype_indication](./AA-3.2#S0027)s, and [range](./AA-3.5#S0037)s given as [discrete_choice](./AA-3.8#S0074)s of a [case_statement](./AA-5.4#S0176) shall be static. [A [discrete_choice](./AA-3.8#S0074) others, if present, shall appear alone and in the last [discrete_choice_list](./AA-3.8#S0073).]

{AI05-0188-1} {AI05-0240-1} The possible values of the selecting_[expression](./AA-4.4#S0132) shall be covered (see 3.8.1) as follows: 

Discussion: {AI05-0240-1} The meaning of "covered" here and in the following rules is that of the term "cover a value" that is defined in 3.8.1. 

{AI05-0003-1} {AI05-0153-3} {AI05-0188-1} {AI05-0262-1} {AI12-0071-1} If the selecting_[expression](./AA-4.4#S0132) is a [name](./AA-4.1#S0091) [(including a [type_conversion](./AA-4.6#S0162), [qualified_expression](./AA-4.7#S0163), or [function_call](./AA-6.4#S0218))] having a static and constrained nominal subtype, then each non-others [discrete_choice](./AA-3.8#S0074) shall cover only values in that subtype that satisfy its predicates (see 3.2.4), and each value of that subtype that satisfies its predicates shall be covered by some [discrete_choice](./AA-3.8#S0074) [(either explicitly or by others)]. 

Ramification: Although not official [name](./AA-4.1#S0091)s of objects, a value conversion still has a defined nominal subtype, namely its target subtype. See 4.6. 

{AI05-0188-1} If the type of the selecting_[expression](./AA-4.4#S0132) is root_integer, universal_integer, or a descendant of a formal scalar type, then the [case_statement](./AA-5.4#S0176) shall have an others [discrete_choice](./AA-3.8#S0074). 

Reason: This is because the base range is implementation defined for root_integer and universal_integer, and not known statically in the case of a formal scalar type. 

{AI05-0188-1} Otherwise, each value of the base range of the type of the selecting_[expression](./AA-4.4#S0132) shall be covered [(either explicitly or by others)]. 

Two distinct [discrete_choice](./AA-3.8#S0074)s of a [case_statement](./AA-5.4#S0176) shall not cover the same value. 

Ramification: {AI05-0188-1} The goal of these coverage rules is that any possible value of the selecting_[expression](./AA-4.4#S0132) of a [case_statement](./AA-5.4#S0176) should be covered by exactly one [discrete_choice](./AA-3.8#S0074) of the [case_statement](./AA-5.4#S0176), and that this should be checked at compile time. The goal is achieved in most cases, but there are two minor loopholes: 

If the expression reads an object with an invalid representation (e.g. an uninitialized object), then the value can be outside the covered range. This can happen for static constrained subtypes, as well as nonstatic or unconstrained subtypes. It cannot, however, happen if the [case_statement](./AA-5.4#S0176) has the [discrete_choice](./AA-3.8#S0074) others, because others covers all values, even those outside the subtype.

{AI95-00114-01} {AI05-0188-1} If the compiler chooses to represent the value of an expression of an unconstrained subtype in a way that includes values outside the bounds of the subtype, then those values can be outside the covered range. For example, if X: Integer := Integer'Last;, and the case selecting_[expression](./AA-4.4#S0132) is X+1, then the implementation might choose to produce the correct value, which is outside the bounds of Integer. (It might raise Constraint_Error instead.) This case can only happen for nongeneric subtypes that are either unconstrained or nonstatic (or both). It can only happen if there is no others [discrete_choice](./AA-3.8#S0074). 

In the uninitialized variable case, the value might be anything; hence, any alternative can be chosen, or Constraint_Error can be raised. (We intend to prevent, however, jumping to random memory locations and the like.) In the out-of-range case, the behavior is more sensible: if there is an others, then the implementation may choose to raise Constraint_Error on the evaluation of the [expression](./AA-4.4#S0132) (as usual), or it may choose to correctly evaluate the [expression](./AA-4.4#S0132) and therefore choose the others alternative. Otherwise (no others), Constraint_Error is raised either way - on the [expression](./AA-4.4#S0132) evaluation, or for the [case_statement](./AA-5.4#S0176) itself.

For an enumeration type with a discontiguous set of internal codes (see 13.4), the only way to get values in between the proper values is via an object with an invalid representation; there is no "out-of-range" situation that can produce them. 


#### Dynamic Semantics

{AI05-0188-1} For the execution of a [case_statement](./AA-5.4#S0176), the selecting_[expression](./AA-4.4#S0132) is first evaluated.

{AI05-0188-1} If the value of the selecting_[expression](./AA-4.4#S0132) is covered by the [discrete_choice_list](./AA-3.8#S0073) of some [case_statement_alternative](./AA-5.4#S0177), then the [sequence_of_statements](./AA-5.1#S0166) of the _alternative is executed.

Otherwise (the value is not covered by any [discrete_choice_list](./AA-3.8#S0073), perhaps due to being outside the base range), Constraint_Error is raised. 

Ramification: {AI12-0005-1} In this case, the value fails to satisfy its (static) predicate (possible when the predicate is disabled), is outside the base range of its type, or is an invalid representation.

NOTE 1   {AI12-0440-1} The execution of a [case_statement](./AA-5.4#S0176) chooses one and only one alternative. Qualification of the expression of a [case_statement](./AA-5.4#S0176) by a static subtype can often be used to limit the number of choices that can be given explicitly. 


#### Examples

Examples of case statements: 

```ada
case Sensor is
   when Elevation	=&gt Record_Elevation(Sensor_Value);
   when Azimuth	=&gt Record_Azimuth  (Sensor_Value);
   when Distance	=&gt Record_Distance (Sensor_Value);
   when others	=&gt null;
end case;

```

```ada
case Today is
   when Mon	=&gt Compute_Initial_Balance;
   when Fri	=&gt Compute_Closing_Balance;
   when Tue .. Thu	=&gt Generate_Report(Today);
   when Sat .. Sun	=&gt null;
end case;

```

```ada
case Bin_Number(Count) is
   when 1	=&gt Update_Bin(1);
   when 2	=&gt Update_Bin(2);
   when 3 | 4	=&gt
      Empty_Bin(1);
      Empty_Bin(2);
   when others	=&gt raise Error;
end case;

```


#### Incompatibilities With Ada 83

In Ada 95, [function_call](./AA-6.4#S0218)s and [type_conversion](./AA-4.6#S0162)s are [name](./AA-4.1#S0091)s, whereas in Ada 83, they were [expression](./AA-4.4#S0132)s. Therefore, if the [expression](./AA-4.4#S0132) of a [case_statement](./AA-5.4#S0176) is a [function_call](./AA-6.4#S0218) or [type_conversion](./AA-4.6#S0162), and the result subtype is static, it is illegal to specify a choice outside the bounds of the subtype. For this case in Ada 83 choices only are required to be in the base range of the type.

In addition, the rule about which choices must be covered is unchanged in Ada 95. Therefore, for a [case_statement](./AA-5.4#S0176) whose [expression](./AA-4.4#S0132) is a [function_call](./AA-6.4#S0218) or [type_conversion](./AA-4.6#S0162), Ada 83 required covering all choices in the base range, while Ada 95 only requires covering choices in the bounds of the subtype. If the [case_statement](./AA-5.4#S0176) does not include an others [discrete_choice](./AA-3.8#S0074), then a legal Ada 83 [case_statement](./AA-5.4#S0176) will be illegal in Ada 95 if the bounds of the subtype are different than the bounds of the base type. 


#### Extensions to Ada 83

In Ada 83, the [expression](./AA-4.4#S0132) in a [case_statement](./AA-5.4#S0176) is not allowed to be of a generic formal type. This restriction is removed in Ada 95; an others [discrete_choice](./AA-3.8#S0074) is required instead.

In Ada 95, a function call is the name of an object; this was not true in Ada 83 (see 4.1, "Names"). This change makes the following [case_statement](./AA-5.4#S0176) legal: 

```ada
subtype S is Integer range 1..2;
function F return S;
case F is
   when 1 =&gt ...;
   when 2 =&gt ...;
   -- No others needed.
end case;

```

{AI05-0005-1} Note that the result subtype given in a function [renaming_declaration](./AA-8.5#S0238) is ignored; for a [case_statement](./AA-5.4#S0176) whose expression calls a such a function, the full coverage rules are checked using the result subtype of the original function. Note that predefined operators such as "+" have an unconstrained result subtype (see 4.5.1). Note that generic formal functions do not have static result subtypes. Note that the result subtype of an inherited subprogram need not correspond to any nameable subtype; there is still a perfectly good result subtype, though. 


#### Wording Changes from Ada 83

Ada 83 forgot to say what happens for "legally" out-of-bounds values.

We take advantage of rules and terms (e.g. cover a value) defined for [discrete_choice](./AA-3.8#S0074)s and [discrete_choice_list](./AA-3.8#S0073)s in 3.8.1, "Variant Parts and Discrete Choices".

In the Name Resolution Rule for the case expression, we no longer need RM83-5.4(3)'s "which must be determinable independently of the context in which the expression occurs, but using the fact that the expression must be of a discrete type", because the [expression](./AA-4.4#S0132) is now a complete context. See 8.6, "The Context of Overload Resolution".

Since [type_conversion](./AA-4.6#S0162)s are now defined as [name](./AA-4.1#S0091)s, their coverage rule is now covered under the general rule for [name](./AA-4.1#S0091)s, rather than being separated out along with [qualified_expression](./AA-4.7#S0163)s. 


#### Wording Changes from Ada 2005

{AI05-0003-1} Rewording to reflect that a [qualified_expression](./AA-4.7#S0163) is now a [name](./AA-4.1#S0091).

{AI05-0153-3} Revised for changes to [discrete_choice](./AA-3.8#S0074)s made to allow static predicates (see 3.2.4) as case choices (see 3.8.1).

{AI05-0188-1} Added the selecting_ prefix to make this wording consistent with [case_expression](./AA-4.5#S0151), and to clarify which [expression](./AA-4.4#S0132) is being talked about in the wording. 


#### Wording Changes from Ada 2012

{AI12-0071-1} Corrigendum: Updated wording of case coverage to use the new term "satisfies the predicates" (see 3.2.4). 

