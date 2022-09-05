---
sidebar_position:  198
---

# J.8  Mod Clauses


#### Syntax

mod_clause<a id="S0369"></a> ::= at mod static_[expression](./AA-4.4#S0132);


#### Static Semantics

A [record_representation_clause](./AA-13.5#S0352) of the form: 

```ada
{AI05-0092-1} for r use
    record at mod a;
        ...
    end record;

```

is equivalent to: 

```ada
for r'Alignment use a;
for r use
    record
        ...
    end record;

```

Reason: The preferred syntax for specifying the alignment of an entity is an [attribute_definition_clause](./AA-13.3#S0349) specifying the Alignment attribute. Therefore, the special-purpose [mod_clause](./AA-J.8#S0369) syntax is now obsolete.

The above equivalence implies, for example, that it is illegal to give both a [mod_clause](./AA-J.8#S0369) and an [attribute_definition_clause](./AA-13.3#S0349) specifying the Alignment attribute for the same type. 


#### Wording Changes from Ada 83

Ada 83's alignment_clause is now called a [mod_clause](./AA-J.8#S0369) to avoid confusion with the new term "Alignment clause" (that is, an [attribute_definition_clause](./AA-13.3#S0349) for the Alignment attribute). 

