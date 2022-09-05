---
sidebar_position:  11
---

# 2.5  Character Literals

[A [character_literal](./AA-2.5#S0015) is formed by enclosing a graphic character between two apostrophe characters.] 


#### Syntax

character_literal<a id="S0015"></a> ::= 'graphic_character'

NOTE 1   A [character_literal](./AA-2.5#S0015) is an enumeration literal of a character type. See 3.5.2. 


#### Examples

Examples of character literals: 

```ada
{AI95-00433-01} 'A'     '*'     '''     ' '
'L'     ''     ''    -- Various els.
''     ''            -- Big numbers - infinity and aleph.

```


#### Wording Changes from Ada 83

{AI05-0299-1} The definitions of the values of literals are in Clauses 3 and 4, rather than here, since it requires knowledge of types. 

