---
sidebar_position:  56
---

# 6.8  Expression Functions

{AI05-0177-1} An [expression_function_declaration](./AA-6.8#S0228) provides a shorthand to declare a function whose body consists of a single return statement. 


#### Syntax

{AI95-0177-1} {AI12-0157-1} expression_function_declaration<a id="S0228"></a> ::= 
   [[overriding_indicator](./AA-8.3#S0234)]
   [function_specification](./AA-6.1#S0198) is
       ([expression](./AA-4.4#S0132))
       [[aspect_specification](./AA-13.1#S0346)];
 | [[overriding_indicator](./AA-8.3#S0234)]
   [function_specification](./AA-6.1#S0198) is
       [aggregate](./AA-4.3#S0106)
       [[aspect_specification](./AA-13.1#S0346)];


#### Name Resolution Rules

{AI05-0177-1} {AI12-0157-1} The expected type for the [expression](./AA-4.4#S0132) or [aggregate](./AA-4.3#S0106) of an [expression_function_declaration](./AA-6.8#S0228) is the result type (see 6.5) of the function. 


#### Static Semantics

{AI05-0177-1} {AI05-0264-1} {AI12-0075-1} {AI12-0157-1} {AI12-0408-1} An [expression_function_declaration](./AA-6.8#S0228) that is not a completion declares an expression function. The return expression of an expression function is the [expression](./AA-4.4#S0132) or [aggregate](./AA-4.3#S0106) of the [expression_function_declaration](./AA-6.8#S0228). A completion is not allowed for an [expression_function_declaration](./AA-6.8#S0228); however, an [expression_function_declaration](./AA-6.8#S0228) can complete a previous declaration.

{AI12-0075-1} A potentially static expression is defined in the same way as a static expression except that

a name denoting a formal parameter of an expression function is a potentially static expression; and

each use of "static expression" in the definition of "static expression" is replaced with a corresponding use of "potentially static expression" in the definition of "potentially static expression". 

Discussion: These uses occur in the definition of "static expression" in the cases of function calls, type conversions, qualified expressions, membership tests, short circuit control forms, conditional expressions, and parenthesized expressions. 

{AI12-0075-1} The following language-defined representation aspect may be specified for an expression function:

StaticThe type of aspect Static is Boolean. When aspect Static is True for an expression function, the function is a static expression function. If directly specified, the [aspect_definition](./AA-13.1#S0348) shall be a static expression.

Aspect Description for Static: Specifies that an associated expression function can be used in static expressions.

The Static value for an inherited function is True if some corresponding primitive function of the parent or progenitor type is a static expression function; otherwise, if not directly specified, the aspect is False. 

{AI12-0075-1} [A static expression function is a static function; see 4.9.] 


#### Legality Rules

{AI05-0177-1} If an [expression_function_declaration](./AA-6.8#S0228) is a completion, it shall be the completion of a [subprogram_declaration](./AA-6.1#S0195) or [generic_subprogram_declaration](./AA-12.1#S0311). The profile of an [expression_function_declaration](./AA-6.8#S0228) that completes a declaration shall conform fully to that of the declaration.

{AI05-0177-1} {AI12-0157-1} If the result subtype has one or more unconstrained access discriminants, the accessibility level of the anonymous access type of each access discriminant, as determined by the [expression](./AA-4.4#S0132) or [aggregate](./AA-4.3#S0106) of the [expression_function_declaration](./AA-6.8#S0228), shall not be statically deeper than that of the master that elaborated the [expression_function_declaration](./AA-6.8#S0228).

Ramification: This can only fail if the discriminant is an access to a part of a nonaliased parameter, as there can be no local declarations here. 

Discussion: {AI12-0005-1} We don't need to repeat any of the other Legality Rules for return statements since none of them can fail here: the implicit return statement has to apply to this function (and isn't nested in something), there clearly is a return statement in this function, and the static class-wide accessibility check cannot fail as a tagged type cannot be declared locally in an expression function. 

{AI12-0075-1} Aspect Static shall be specified to have the value True only if the associated [expression_function_declaration](./AA-6.8#S0228):

is not a completion;

has an [expression](./AA-4.4#S0132) that is a potentially static expression;

contains no calls to itself;

each parameter (if any) is of mode in and is of a static subtype;

has a result subtype that is a static subtype;

has no applicable precondition or postcondition expression; and

{AI12-0075-1} {AI12-0191-1} for result type R, if the function is a boundary entity for type R (see 7.3.2), no type invariant applies to type R; if R has a component type C, a similar rule applies to C. 

Ramification: Since a string subtype can be static, this allows an expression function of a string type to be static. 

Paragraph 6 was deleted. 


#### Dynamic Semantics

{AI05-0177-1} {AI05-0262-1} {AI12-0157-1} {AI12-0408-1} The execution of an expression function is invoked by a subprogram call. For the execution of a subprogram call on an expression function, or on a function completed with a [expression_function_declaration](./AA-6.8#S0228), the execution of the [subprogram_body](./AA-6.3#S0216) executes an implicit function body containing only a [simple_return_statement](./AA-6.5#S0222) whose [expression](./AA-4.4#S0132) is the return expression of the expression function.

Discussion: The last sentence effectively means that all of the dynamic wording in 6.5 applies as needed, and we don't have to repeat it here. 

{AI05-0177-1} The elaboration of an [expression_function_declaration](./AA-6.8#S0228) has no other effect than to establish that the expression function can be called without failing the Elaboration_Check. 


#### Examples

{AI12-0429-1} Example of an expression function: 

```ada
{AI05-0177-1} function Is_Origin (P : in Point) return Boolean is -- see 3.9
   (P.X = 0.0 and P.Y = 0.0);

```


#### Extensions to Ada 2005

{AI05-0177-1} Expression functions are new in Ada 2012. 


#### Extensions to Ada 2012

{AI12-0157-1} Corrigendum: A [aggregate](./AA-4.3#S0106) can directly be the return expression of an expression function. This eliminates the double parentheses that otherwise would be necessary.

{AI12-0075-1} Aspect Static is new; it allows using suitable expression functions in static expressions. 


#### Wording Changes from Ada 2012

{AI12-0408-1} Clarified the term "expression function" so it matches the meaning expected in 6.1. 

