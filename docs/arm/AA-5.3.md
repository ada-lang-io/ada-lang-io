---
sidebar_position:  42
---

# 5.3  If Statements

[An [if_statement](./AA-5.3#S0175) selects for execution at most one of the enclosed sequences_of_statements, depending on the (truth) value of one or more corresponding [condition](./AA-4.5#S0150)s.] 


#### Syntax

if_statement<a id="S0175"></a> ::= 
    if [condition](./AA-4.5#S0150) then
      [sequence_of_statements](./AA-5.1#S0166)
   {elsif [condition](./AA-4.5#S0150) then
      [sequence_of_statements](./AA-5.1#S0166)}
   [else
      [sequence_of_statements](./AA-5.1#S0166)]
    end if;

Paragraphs 3 and 4 were deleted. 


#### Dynamic Semantics

{AI05-0264-1} For the execution of an [if_statement](./AA-5.3#S0175), the [condition](./AA-4.5#S0150) specified after if, and any [condition](./AA-4.5#S0150)s specified after elsif, are evaluated in succession (treating a final else as elsif True then), until one evaluates to True or all [condition](./AA-4.5#S0150)s are evaluated and yield False. If a [condition](./AA-4.5#S0150) evaluates to True, then the corresponding [sequence_of_statements](./AA-5.1#S0166) is executed; otherwise, none of them is executed. 

Ramification: The part about all evaluating to False can't happen if there is an else, since that is herein considered equivalent to elsif True then. 


#### Examples

Examples of if statements: 

```ada
if Month = December and Day = 31 then
   Month := January;
   Day   := 1;
   Year  := Year + 1;
end if;

```

```ada
if Line_Too_Short then
   raise Layout_Error;
elsif Line_Full then
   New_Line;
   Put(Item);
else
   Put(Item);
end if;

```

```ada
if My_Car.Owner.Vehicle /= My_Car then            --  see 3.10.1
   Report ("Incorrect data");
end if;

```


#### Wording Changes from Ada 2005

{AI05-0147-1} Moved definition of [condition](./AA-4.5#S0150) to 4.5.7 in order to eliminate a forward reference. 

