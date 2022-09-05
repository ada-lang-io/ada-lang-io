---
sidebar_position:  13
---

# 2.7  Comments

A [comment](./AA-2.7#S0018) starts with two adjacent hyphens and extends up to the end of the line. 


#### Syntax

comment<a id="S0018"></a> ::= --{non_end_of_line_character}

A [comment](./AA-2.7#S0018) may appear on any line of a program. 


#### Static Semantics

The presence or absence of [comment](./AA-2.7#S0018)s has no influence on whether a program is legal or illegal. Furthermore, [comment](./AA-2.7#S0018)s do not influence the meaning of a program; their sole purpose is the enlightenment of the human reader. 


#### Examples

Examples of comments: 

```ada
{AI12-0440-1} --  the last sentence above echoes the Algol 68 report 

end;  --  processing of Line is complete 

--  a long comment can be split onto
--  two or more consecutive lines   

----------------  the first two hyphens start the comment  

```

