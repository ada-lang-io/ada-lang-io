---
sidebar_position:  47
---

# 5.8  Goto Statements

[A [goto_statement](./AA-5.8#S0194) specifies an explicit transfer of control from this [statement](./AA-5.1#S0167) to a target statement with a given label.] 


#### Syntax

goto_statement<a id="S0194"></a> ::= goto label_[name](./AA-4.1#S0091);


#### Name Resolution Rules

The label_[name](./AA-4.1#S0091) shall resolve to denote a [label](./AA-5.1#S0171); the [statement](./AA-5.1#S0167) with that [label](./AA-5.1#S0171) is the target statement. 


#### Legality Rules

The innermost [sequence_of_statements](./AA-5.1#S0166) that encloses the target statement shall also enclose the [goto_statement](./AA-5.8#S0194). Furthermore, if a [goto_statement](./AA-5.8#S0194) is enclosed by an [accept_statement](./AA-9.5#S0258) or a body, then the target statement shall not be outside this enclosing construct. 

Ramification: The [goto_statement](./AA-5.8#S0194) can be a [statement](./AA-5.1#S0167) of an inner sequence_.

It follows from the second rule that if the target [statement](./AA-5.1#S0167) is enclosed by such a construct, then the [goto_statement](./AA-5.8#S0194) cannot be outside. 


#### Dynamic Semantics

The execution of a [goto_statement](./AA-5.8#S0194) transfers control to the target statement, completing the execution of any [compound_statement](./AA-5.1#S0169) that encloses the [goto_statement](./AA-5.8#S0194) but does not enclose the target. 

NOTE 1   The above rules allow transfer of control to a [statement](./AA-5.1#S0167) of an enclosing [sequence_of_statements](./AA-5.1#S0166) but not the reverse. Similarly, they prohibit transfers of control such as between alternatives of a [case_statement](./AA-5.4#S0176), [if_statement](./AA-5.3#S0175), or [select_statement](./AA-9.7#S0269); between [exception_handler](./AA-11.2#S0305)s; or from an [exception_handler](./AA-11.2#S0305) of a [handled_sequence_of_statements](./AA-11.2#S0304) back to its [sequence_of_statements](./AA-5.1#S0166). 


#### Examples

Example of a loop containing a goto statement: 

```ada
&lt&ltSort&gt&gt
for I in 1 .. N-1 loop
   if A(I) &gt A(I+1) then
      Exchange(A(I), A(I+1));
      goto Sort;
   end if;
end loop;

```

