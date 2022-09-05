---
sidebar_position:  175
---

# F.1  Machine_Radix Attribute Definition Clause


#### Static Semantics

{AI12-0272-1} The representation attribute Machine_Radix may be specified for a decimal first subtype (see 3.5.9) via an [attribute_definition_clause](./AA-13.3#S0349); the expression of such a clause shall be static, and its value shall be 2 or 10. A value of 2 implies a binary base range; a value of 10 implies a decimal base range. 

Ramification: In the absence of a Machine_Radix clause, the choice of 2 versus 10 for S'Machine_Radix is not specified. 

Aspect Description for Machine_Radix: Radix (2 or 10) that is used to represent a decimal fixed point type.


#### Implementation Advice

Packed decimal should be used as the internal representation for objects of subtype S when S'Machine_Radix = 10. 

Implementation Advice: Packed decimal should be used as the internal representation for objects of subtype S when S'Machine_Radix = 10.

Discussion: {AI05-0229-1} The intent of a decimal Machine_Radix attribute definition clause is to allow the programmer to declare an Ada decimal data object whose representation matches a particular COBOL implementation's representation of packed decimal items. The Ada object may then be passed to an interfaced COBOL program that takes a packed decimal data item as a parameter, assuming that convention COBOL has been specified for the Ada object's type with an aspect Convention.

Additionally, the Ada compiler may choose to generate arithmetic instructions that exploit the packed decimal representation.


#### Examples

Example of Machine_Radix attribute definition clause: 

```ada
type Money is delta 0.01 digits 15;
for Money'Machine_Radix use 10;

```


#### Wording Changes from Ada 2012

{AI12-0272-1} Clarified that Machine_Radix is a representation aspect. 

