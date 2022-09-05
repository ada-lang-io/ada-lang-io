---
sidebar_position:  55
---

# 6.7  Null Procedures

{AI95-00348-01} A [null_procedure_declaration](./AA-6.7#S0227) provides a shorthand to declare a procedure with an empty body. 


#### Syntax

{AI95-00348-01} {AI05-0183-1} null_procedure_declaration<a id="S0227"></a> ::= 
   [[overriding_indicator](./AA-8.3#S0234)]
   [procedure_specification](./AA-6.1#S0197) is null
       [[aspect_specification](./AA-13.1#S0346)];


#### Legality Rules

{AI05-0177-1} If a [null_procedure_declaration](./AA-6.7#S0227) is a completion, it shall be the completion of a [subprogram_declaration](./AA-6.1#S0195) or [generic_subprogram_declaration](./AA-12.1#S0311). The profile of a [null_procedure_declaration](./AA-6.7#S0227) that completes a declaration shall conform fully to that of the declaration. 


#### Static Semantics

{AI95-00348-01} {AI05-0177-1} {AI05-0264-1} {AI12-0408-1} A [null_procedure_declaration](./AA-6.7#S0227) that is not a completion declares a null procedure. A completion is not allowed for a [null_procedure_declaration](./AA-6.7#S0227); however, a [null_procedure_declaration](./AA-6.7#S0227) can complete a previous declaration. 

Reason: There are no null functions because the return value has to be constructed somehow; a function that always raises Program_Error doesn't seem very useful or worth the complication. 


#### Dynamic Semantics

{AI95-00348-01} {AI12-0408-1} The execution of a null procedure is invoked by a subprogram call. For the execution of a subprogram call on a null procedure, or on a procedure completed with a [null_procedure_declaration](./AA-6.7#S0227), the execution of the [subprogram_body](./AA-6.3#S0216) has no effect. 

Ramification: Thus, a null procedure is equivalent to the body 

```ada
begin
   null;
end;

```

with the exception that a null procedure can be used in place of a procedure specification. 

{AI95-00348-01} {AI05-0177-1} The elaboration of a [null_procedure_declaration](./AA-6.7#S0227) has no other effect than to establish that the null procedure can be called without failing the Elaboration_Check. 


#### Examples

{AI12-0429-1} Example of the declaration of a null procedure: 

```ada
{AI95-00433-01} {AI12-0440-1} procedure Simplify(Expr : in out Expression) is null; -- see 3.9
-- By default, Simplify does nothing, but it can be overridden in extensions of Expression

```


#### Extensions to Ada 95

{AI95-00348-01} Null procedures are new. 


#### Extensions to Ada 2005

{AI05-0177-1} A [null_procedure_declaration](./AA-6.7#S0227) can now be a completion.

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [null_procedure_declaration](./AA-6.7#S0227). This is described in 13.1.1. 


#### Wording Changes from Ada 2012

{AI12-0408-1} Clarified the term "null procedure" so it matches the meaning expected in 6.1. 

