---
sidebar_position:  46
---

# 5.7  Exit Statements

[An [exit_statement](./AA-5.7#S0193) is used to complete the execution of an enclosing [loop_statement](./AA-5.5#S0178); the completion is conditional if the [exit_statement](./AA-5.7#S0193) includes a [condition](./AA-4.5#S0150).] 


#### Syntax

exit_statement<a id="S0193"></a> ::= 
   exit [loop_[name](./AA-4.1#S0091)] [when [condition](./AA-4.5#S0150)];


#### Name Resolution Rules

The loop_[name](./AA-4.1#S0091), if any, in an [exit_statement](./AA-5.7#S0193) shall resolve to denote a [loop_statement](./AA-5.5#S0178). 


#### Legality Rules

Each [exit_statement](./AA-5.7#S0193) applies to a [loop_statement](./AA-5.5#S0178); this is the [loop_statement](./AA-5.5#S0178) being exited. An [exit_statement](./AA-5.7#S0193) with a [name](./AA-4.1#S0091) is only allowed within the [loop_statement](./AA-5.5#S0178) denoted by the [name](./AA-4.1#S0091), and applies to that [loop_statement](./AA-5.5#S0178). An [exit_statement](./AA-5.7#S0193) without a [name](./AA-4.1#S0091) is only allowed within a [loop_statement](./AA-5.5#S0178), and applies to the innermost enclosing one. An [exit_statement](./AA-5.7#S0193) that applies to a given [loop_statement](./AA-5.5#S0178) shall not appear within a body or [accept_statement](./AA-9.5#S0258), if this construct is itself enclosed by the given [loop_statement](./AA-5.5#S0178). 


#### Dynamic Semantics

For the execution of an [exit_statement](./AA-5.7#S0193), the [condition](./AA-4.5#S0150), if present, is first evaluated. If the value of the [condition](./AA-4.5#S0150) is True, or if there is no [condition](./AA-4.5#S0150), a transfer of control is done to complete the [loop_statement](./AA-5.5#S0178). If the value of the [condition](./AA-4.5#S0150) is False, no transfer of control takes place. 

NOTE 1   Several nested loops can be exited by an [exit_statement](./AA-5.7#S0193) that names the outer loop. 


#### Examples

Examples of loops with exit statements: 

```ada
for N in 1 .. Max_Num_Items loop
   Get_New_Item(New_Item);
   Merge_Item(New_Item, Storage_File);
   exit when New_Item = Terminal_Item;
end loop;

```

```ada
Main_Cycle:
   loop
      --  initial statements
      exit Main_Cycle when Found;
      --  final statements
   end loop Main_Cycle;

```

