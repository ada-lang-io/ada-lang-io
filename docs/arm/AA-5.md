---
sidebar_position:  6
---

# 5 Statements

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[A statement defines an action to be performed upon its execution.]

[This section describes the general rules applicable to all statements. Some statements are discussed in later sections: Procedure_call_statements and return_statements are described in 6, "Subprograms". Entry_call_statements, requeue_statements, delay_statements, accept_statements, select_statements, and abort_statements are described in 9, "Tasks and Synchronization". Raise_statements are described in 11, "Exceptions", and code_statements in 13. The remaining forms of statements are presented in this section.] 


#### Wording Changes from Ada 83

The description of return_statements has been moved to 6.5, "Return Statements", so that it is closer to the description of subprograms. 


## 5.1  Simple and Compound Statements - Sequences of Statements

[A statement is either simple or compound. A simple_statement encloses no other statement. A compound_statement can enclose simple_statements and other compound_statements.]

Version=[5],Kind=(AddedNormal),Group=[C],Term=[parallel construct], Def=[an executable construct that defines multiple activities of a single task that can proceed in parallel, via the execution of multiple logical threads of control] 


#### Syntax

sequence_of_statements ::= statement {statement}

statement ::= 
   {label} simple_statement | {label} compound_statement

simple_statement ::= null_statement
   | assignment_statement	| exit_statement
   | goto_statement	| procedure_call_statement
   | return_statement	| entry_call_statement
   | requeue_statement	| delay_statement
   | abort_statement	| raise_statement
   | code_statement

compound_statement ::= 
     if_statement	| case_statement
   | loop_statement	| block_statement
   | accept_statement	| select_statement

null_statement ::= null;

label ::= &lt&ltlabel_statement_identifier&gt&gt

statement_identifier ::= direct_name

The direct_name of a statement_identifier shall be an identifier (not an operator_symbol). 


#### Name Resolution Rules

The direct_name of a statement_identifier shall resolve to denote its corresponding implicit declaration (see below). 


#### Legality Rules

Distinct identifiers shall be used for all statement_identifiers that appear in the same body, including inner block_statements but excluding inner program units. 


#### Static Semantics

For each statement_identifier, there is an implicit declaration (with the specified identifier) at the end of the declarative_part of the innermost block_statement or body that encloses the statement_identifier. The implicit declarations occur in the same order as the statement_identifiers occur in the source text. If a usage name denotes such an implicit declaration, the entity it denotes is the label, loop_statement, or block_statement with the given statement_identifier. 

Reason: We talk in terms of individual statement_identifiers here rather than in terms of the corresponding statements, since a given statement may have multiple statement_identifiers.

A block_statement that has no explicit declarative_part has an implicit empty declarative_part, so this rule can safely refer to the declarative_part of a block_statement.

The scope of a declaration starts at the place of the declaration itself (see 8.2). In the case of a label, loop, or block name, it follows from this rule that the scope of the implicit declaration starts before the first explicit occurrence of the corresponding name, since this occurrence is either in a statement label, a loop_statement, a block_statement, or a goto_statement. An implicit declaration in a block_statement may hide a declaration given in an outer program unit or block_statement (according to the usual rules of hiding explained in 8.3).

The syntax rule for label uses statement_identifier which is a direct_name (not a defining_identifier), because labels are implicitly declared. The same applies to loop and block names. In other words, the label itself is not the defining occurrence; the implicit declaration is.

We cannot consider the label to be a defining occurrence. An example that can tell the difference is this: 

```ada
declare
    -- Label Foo is implicitly declared here.
begin
    for Foo in ... loop
        ...
        &lt&ltFoo&gt&gt -- Illegal.
        ...
    end loop;
end;
  

```

The label in this example is hidden from itself by the loop parameter with the same name; the example is illegal. We considered creating a new syntactic category name, separate from direct_name and selector_name, for use in the case of statement labels. However, that would confuse the rules in Section 8, so we didn't do it. 


#### Dynamic Semantics

The execution of a null_statement has no effect.

A transfer of control is the run-time action of an exit_statement, return_statement, goto_statement, or requeue_statement, selection of a terminate_alternative, raising of an exception, or an abort, which causes the next action performed to be one other than what would normally be expected from the other rules of the language. [As explained in 7.6.1, a transfer of control can cause the execution of constructs to be completed and then left, which may trigger finalization.]

The execution of a sequence_of_statements consists of the execution of the individual statements in succession until the sequence_ is completed. 

Ramification: It could be completed by reaching the end of it, or by a transfer of control. 

NOTE   A statement_identifier that appears immediately within the declarative region of a named loop_statement or an accept_statement is nevertheless implicitly declared immediately within the declarative region of the innermost enclosing body or block_statement; in other words, the expanded name for a named statement is not affected by whether the statement occurs inside or outside a named loop or an accept_statement - only nesting within block_statements is relevant to the form of its expanded name. 

Discussion: Each comment in the following example gives the expanded name associated with an entity declared in the task body: 

```ada
task body Compute is
   Sum : Integer := 0;                       -- Compute.Sum
begin
 Outer:                                      -- Compute.Outer
   for I in 1..10 loop     -- Compute.Outer.I
    Blk:                                     -- Compute.Blk
      declare
         Sum : Integer := 0;                 -- Compute.Blk.Sum
      begin
         accept Ent(I : out Integer; J : in Integer) do
                                             -- Compute.Ent.I, Compute.Ent.J
            Compute.Ent.I := Compute.Outer.I;
          Inner:                             -- Compute.Blk.Inner
            for J in 1..10 loop
                                             -- Compute.Blk.Inner.J
               Sum := Sum + Compute.Blk.Inner.J * Compute.Ent.J;
            end loop Inner;
         end Ent;
         Compute.Sum := Compute.Sum + Compute.Blk.Sum;
      end Blk;
   end loop Outer;
   Record_Result(Sum);
end Compute;

```


#### Examples

Examples of labeled statements: 

```ada
&lt&ltHere&gt&gt &lt&ltIci&gt&gt &lt&ltAqui&gt&gt &lt&ltHier&gt&gt null;

```

```ada
&lt&ltAfter&gt&gt X := 1;

```


#### Extensions to Ada 83

The requeue_statement is new. 


#### Wording Changes from Ada 83

We define the syntactic category statement_identifier to simplify the description. It is used for labels, loop names, and block names. We define the entity associated with the implicit declarations of statement names.

Completion includes completion caused by a transfer of control, although RM83-5.1(6) did not take this view. 


## 5.2  Assignment Statements

[An assignment_statement replaces the current value of a variable with the result of evaluating an expression.] 


#### Syntax

assignment_statement ::= 
   variable_name := expression;

The execution of an assignment_statement includes the evaluation of the expression and the assignment of the value of the expression into the target. [An assignment operation (as opposed to an assignment_statement) is performed in other contexts as well, including object initialization and by-copy parameter passing.] The target of an assignment operation is the view of the object to which a value is being assigned; the target of an assignment_statement is the variable denoted by the variable_name. 

Discussion: Don't confuse this notion of the "target" of an assignment with the notion of the "target object" of an entry call or requeue.

Don't confuse the term "assignment operation" with the assignment_statement. The assignment operation is just one part of the execution of an assignment_statement. The assignment operation is also a part of the execution of various other constructs; see 7.6.1, "Completion and Finalization" for a complete list. Note that when we say, "such-and-such is assigned to so-and-so", we mean that the assignment operation is being applied, and that so-and-so is the target of the assignment operation. 


#### Name Resolution Rules

The variable_name of an assignment_statement is expected to be of any nonlimited type. The expected type for the expression is the type of the target. 

Implementation Note: An assignment_statement as a whole is a "complete context", so if the variable_name of an assignment_statement is overloaded, the expression can be used to help disambiguate it. For example: 

```ada
  type P1 is access R1;
  type P2 is access R2;

```

```ada
  function F return P1;
  function F return P2;

```

```ada
  X : R1;
begin
  F.all := X;  -- Right hand side helps resolve left hand side

```


#### Legality Rules

The target [denoted by the variable_name] shall be a variable.

If the target is of a tagged class-wide type T'Class, then the expression shall either be dynamically tagged, or of type T and tag-indeterminate (see 3.9.2). 

Reason: This is consistent with the general rule that a single dispatching operation shall not have both dynamically tagged and statically tagged operands. Note that for an object initialization (as opposed to the assignment_statement), a statically tagged initialization expression is permitted, since there is no chance for confusion (or Tag_Check failure). Also, in an object initialization, tag-indeterminate expressions of any type covered by T'Class would be allowed, but with an assignment_statement, that might not work if the tag of the target was for a type that didn't have one of the dispatching operations in the tag-indeterminate expression. 


#### Dynamic Semantics

For the execution of an assignment_statement, the variable_name and the expression are first evaluated in an arbitrary order. 

Ramification: Other rules of the language may require that the bounds of the variable be determined prior to evaluating the expression, but that does not necessarily require evaluation of the variable_name, as pointed out by the ACID. 

When the type of the target is class-wide: 

If the expression is tag-indeterminate (see 3.9.2), then the controlling tag value for the expression is the tag of the target; 

Ramification: See 3.9.2, "Dispatching Operations of Tagged Types". 

Otherwise [(the expression is dynamically tagged)], a check is made that the tag of the value of the expression is the same as that of the target; if this check fails, Constraint_Error is raised. 

The value of the expression is converted to the subtype of the target. [The conversion might raise an exception (see 4.6).] 

Ramification: 4.6, "Type Conversions" defines what actions and checks are associated with subtype conversion. For non-array subtypes, it is just a constraint check presuming the types match. For array subtypes, it checks the lengths and slides if the target is constrained. "Sliding" means the array doesn't have to have the same bounds, so long as it is the same length. 

In cases involving controlled types, the target is finalized, and an anonymous object might be used as an intermediate in the assignment, as described in 7.6.1, "Completion and Finalization". In any case, the converted value of the expression is then assigned to the target, which consists of the following two steps: 

To be honest: 7.6.1 actually says that finalization happens always, but unless controlled types are involved, this finalization during an assignment_statement does nothing. 

The value of the target becomes the converted value.

If any part of the target is controlled, its value is adjusted as explained in clause 7.6. 

Ramification: If any parts of the object are controlled, abort is deferred during the assignment operation itself, but not during the rest of the execution of an assignment_statement. 

NOTE   The tag of an object never changes; in particular, an assignment_statement does not change the tag of the target.

NOTE   The values of the discriminants of an object designated by an access value cannot be changed (not even by assigning a complete value to the object itself) since such objects are always constrained; however, subcomponents of such objects may be unconstrained. 

Ramification: The implicit subtype conversion described above for assignment_statements is performed only for the value of the right-hand side expression as a whole; it is not performed for subcomponents of the value.

The determination of the type of the variable of an assignment_statement may require consideration of the expression if the variable name can be interpreted as the name of a variable designated by the access value returned by a function call, and similarly, as a component or slice of such a variable (see 8.6, "The Context of Overload Resolution"). 


#### Examples

Examples of assignment statements: 

```ada
Value := Max_Value - 1;
Shade := Blue;

```

```ada
Next_Frame(F)(M, N) := 2.5;        --  see 4.1.1
U := Dot_Product(V, W);            --  see 6.3

```

```ada
Writer := (Status =&gt Open, Unit =&gt Printer, Line_Count =&gt 60);  -- see 3.8.1
Next_Car.all := (72074, null);    --  see 3.10.1

```

Examples involving scalar subtype conversions: 

```ada
I, J : Integer range 1 .. 10 := 5;
K    : Integer range 1 .. 20 := 15;
 ...

```

```ada
I := J;  --  identical ranges
K := J;  --  compatible ranges
J := K;  --  will raise Constraint_Error if K &gt 10

```

Examples involving array subtype conversions: 

```ada
A : String(1 .. 31);
B : String(3 .. 33);
 ...

```

```ada
A := B;  --  same number of components

```

```ada
A(1 .. 9)  := "tar sauce";
A(4 .. 12) := A(1 .. 9);  --  A(1 .. 12) = "tartar sauce"

```

NOTE 1   Notes on the examples: Assignment_statements are allowed even in the case of overlapping slices of the same array, because the variable_name and expression are both evaluated before copying the value into the variable. In the above example, an implementation yielding A(1 .. 12) = "tartartartar" would be incorrect. 


#### Extensions to Ada 83

We now allow user-defined finalization and value adjustment actions as part of assignment_statements (see 7.6, "User-Defined Assignment and Finalization"). 


#### Wording Changes from Ada 83

The special case of array assignment is subsumed by the concept of a subtype conversion, which is applied for all kinds of types, not just arrays. For arrays it provides "sliding". For numeric types it provides conversion of a value of a universal type to the specific type of the target. For other types, it generally has no run-time effect, other than a constraint check.

We now cover in a general way in 3.7.2 the erroneous execution possible due to changing the value of a discriminant when the variable in an assignment_statement is a subcomponent that depends on discriminants. 


## 5.3  If Statements

[An if_statement selects for execution at most one of the enclosed sequences_of_statements, depending on the (truth) value of one or more corresponding conditions.] 


#### Syntax

if_statement ::= 
    if condition then
      sequence_of_statements
   {elsif condition then
      sequence_of_statements}
   [else
      sequence_of_statements]
    end if;

condition ::= boolean_expression


#### Name Resolution Rules

A condition is expected to be of any boolean type. 


#### Dynamic Semantics

For the execution of an if_statement, the condition specified after if, and any conditions specified after elsif, are evaluated in succession (treating a final else as elsif True then), until one evaluates to True or all conditions are evaluated and yield False. If a condition evaluates to True, then the corresponding sequence_of_statements is executed; otherwise none of them is executed. 

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


## 5.4  Case Statements

[A case_statement selects for execution one of a number of alternative sequences_of_statements; the chosen alternative is defined by the value of an expression.] 


#### Syntax

case_statement ::= 
   case expression is
       case_statement_alternative
      {case_statement_alternative}
   end case;

case_statement_alternative ::= 
   when discrete_choice_list =&gt
      sequence_of_statements


#### Name Resolution Rules

The expression is expected to be of any discrete type. The expected type for each discrete_choice is the type of the expression. 


#### Legality Rules

The expressions and discrete_ranges given as discrete_choices of a case_statement shall be static. [A discrete_choice others, if present, shall appear alone and in the last discrete_choice_list.]

The possible values of the expression shall be covered as follows: 

If the expression is a name [(including a type_conversion or a function_call)] having a static and constrained nominal subtype, or is a qualified_expression whose subtype_mark denotes a static and constrained scalar subtype, then each non-others discrete_choice shall cover only values in that subtype, and each value of that subtype shall be covered by some discrete_choice [(either explicitly or by others)]. 

Ramification: Although not official names of objects, a value conversion still has a defined nominal subtype, namely its target subtype. See 4.6. 

If the type of the expression is root_integer, universal_integer, or a descendant of a formal scalar type, then the case_statement shall have an others discrete_choice. 

Reason: This is because the base range is implementation defined for root_integer and universal_integer, and not known statically in the case of a formal scalar type. 

Otherwise, each value of the base range of the type of the expression shall be covered [(either explicitly or by others)]. 

Two distinct discrete_choices of a case_statement shall not cover the same value. 

Ramification: The goal of these coverage rules is that any possible value of the expression of a case_statement should be covered by exactly one discrete_choice of the case_statement, and that this should be checked at compile time. The goal is achieved in most cases, but there are two minor loopholes: 

If the expression reads an object with an invalid representation (e.g. an uninitialized object), then the value can be outside the covered range. This can happen for static constrained subtypes, as well as nonstatic or unconstrained subtypes. It cannot, however, happen if the case_statement has the discrete_choice others, because others covers all values, even those outside the subtype.

If the compiler chooses to represent the value of an expression of an unconstrained subtype in a way that includes values outside the bounds of the subtype, then those values can be outside the covered range. For example, if X: Integer := Integer'Last;, and the case expression is X+1, then the implementation might choose to produce the correct value, which is outside the bounds of Integer. (It might raise Constraint_Error instead.) This case can only happen for nongeneric subtypes that are either unconstrained or non-static (or both). It can only happen if there is no others discrete_choice. 

In the uninitialized variable case, the value might be anything; hence, any alternative can be chosen, or Constraint_Error can be raised. (We intend to prevent, however, jumping to random memory locations and the like.) In the out-of-range case, the behavior is more sensible: if there is an others, then the implementation may choose to raise Constraint_Error on the evaluation of the expression (as usual), or it may choose to correctly evaluate the expression and therefore choose the others alternative. Otherwise (no others), Constraint_Error is raised either way - on the expression evaluation, or for the case_statement itself.

For an enumeration type with a discontiguous set of internal codes (see 13.4), the only way to get values in between the proper values is via an object with an invalid representation; there is no "out-of-range" situation that can produce them. 


#### Dynamic Semantics

For the execution of a case_statement the expression is first evaluated.

If the value of the expression is covered by the discrete_choice_list of some case_statement_alternative, then the sequence_of_statements of the _alternative is executed.

Otherwise (the value is not covered by any discrete_choice_list, perhaps due to being outside the base range), Constraint_Error is raised. 

Ramification: In this case, the value is outside the base range of its type, or is an invalid representation.

NOTE 1   The execution of a case_statement chooses one and only one alternative. Qualification of the expression of a case_statement by a static subtype can often be used to limit the number of choices that need be given explicitly. 


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


#### Extensions to Ada 83

In Ada 83, the expression in a case_statement is not allowed to be of a generic formal type. This restriction is removed in Ada 95; an others discrete_choice is required instead.

In Ada 95, a function call is the name of an object; this was not true in Ada 83 (see 4.1, "Names"). This change makes the following case_statement legal: 

```ada
subtype S is Integer range 1..2;
function F return S;
case F is
   when 1 =&gt ...;
   when 2 =&gt ...;
   -- No others needed.
end case;

```

Note that the result subtype given in a function renaming_declaration is ignored; for a case_statement whose expression calls a such a function, the full coverage rules are checked using the result subtype of the original function. Note that predefined operators such as "+" have an unconstrained result subtype (see 4.5.1). Note that generic formal functions do not have static result subtypes. Note that the result subtype of an inherited subprogram need not correspond to any namable subtype; there is still a perfectly good result subtype, though. 


#### Wording Changes from Ada 83

Ada 83 forgot to say what happens for "legally" out-of-bounds values.

We take advantage of rules and terms (e.g. cover a value) defined for discrete_choices and discrete_choice_lists in 3.8.1, "Variant Parts and Discrete Choices".

In the Name Resolution Rule for the case expression, we no longer need RM83-5.4(3)'s "which must be determinable independently of the context in which the expression occurs, but using the fact that the expression must be of a discrete type", because the expression is now a complete context. See 8.6, "The Context of Overload Resolution".

Since type_conversions are now defined as names, their coverage rule is now covered under the general rule for names, rather than being separated out along with qualified_expressions. 


## 5.5  Loop Statements

[A loop_statement includes a sequence_of_statements that is to be executed repeatedly, zero or more times.] 


#### Syntax

loop_statement ::= 
   [loop_statement_identifier:]
      [iteration_scheme] loop
         sequence_of_statements
       end loop [loop_identifier];

iteration_scheme ::= while condition
   | for loop_parameter_specification

loop_parameter_specification ::= 
   defining_identifier in [reverse] discrete_subtype_definition

If a loop_statement has a loop_statement_identifier, then the identifier shall be repeated after the end loop; otherwise, there shall not be an identifier after the end loop.


#### Static Semantics

A loop_parameter_specification declares a loop parameter, which is an object whose subtype is that defined by the discrete_subtype_definition. 


#### Dynamic Semantics

Version=[5],Kind=(AddedNormal),Group=[C],Term=[iterator filter], Def=[a construct that is used to restrict the elements produced by an iteration to those for which a boolean condition evaluates to True]

For the execution of a loop_statement, the sequence_of_statements is executed repeatedly, zero or more times, until the loop_statement is complete. The loop_statement is complete when a transfer of control occurs that transfers control out of the loop, or, in the case of an iteration_scheme, as specified below.

For the execution of a loop_statement with a while iteration_scheme, the condition is evaluated before each execution of the sequence_of_statements; if the value of the condition is True, the sequence_of_statements is executed; if False, the execution of the loop_statement is complete.

For the execution of a loop_statement with a for iteration_scheme, the loop_parameter_specification is first elaborated. This elaboration creates the loop parameter and elaborates the discrete_subtype_definition. If the discrete_subtype_definition defines a subtype with a null range, the execution of the loop_statement is complete. Otherwise, the sequence_of_statements is executed once for each value of the discrete subtype defined by the discrete_subtype_definition (or until the loop is left as a consequence of a transfer of control). Prior to each such iteration, the corresponding value of the discrete subtype is assigned to the loop parameter. These values are assigned in increasing order unless the reserved word reverse is present, in which case the values are assigned in decreasing order. 

Ramification: The order of creating the loop parameter and evaluating the discrete_subtype_definition doesn't matter, since the creation of the loop parameter has no side effects (other than possibly raising Storage_Error, but anything can do that).

NOTE 1   A loop parameter is a constant; it cannot be updated within the sequence_of_statements of the loop (see 3.3).

NOTE 2   An object_declaration should not be given for a loop parameter, since the loop parameter is automatically declared by the loop_parameter_specification. The scope of a loop parameter extends from the loop_parameter_specification to the end of the loop_statement, and the visibility rules are such that a loop parameter is only visible within the sequence_of_statements of the loop. 

Implementation Note: An implementation could give a warning if a variable is hidden by a loop_parameter_specification. 

NOTE 3   The discrete_subtype_definition of a for loop is elaborated just once. Use of the reserved word reverse does not alter the discrete subtype defined, so that the following iteration_schemes are not equivalent; the first has a null range. 

```ada
for J in reverse 1 .. 0
for J in 0 .. 1

```

Ramification: If a loop_parameter_specification has a static discrete range, the subtype of the loop parameter is static. 


#### Examples

Example of a loop statement without an iteration scheme: 

```ada
loop
   Get(Current_Character);
   exit when Current_Character = '*';
end loop;

```

Example of a loop statement with a while iteration scheme: 

```ada
while Bid(N).Price &lt Cut_Off.Price loop
   Record_Bid(Bid(N).Price);
   N := N + 1;
end loop;

```

Example of a loop statement with a for iteration scheme: 

```ada
for J in Buffer'Range loop     --  works even with a null range
   if Buffer(J) /= Space then
      Put(Buffer(J));
   end if;
end loop;

```

Example of a loop statement with a name: 

```ada
Summation:
   while Next /= Head loop       -- see 3.10.1
      Sum  := Sum + Next.Value;
      Next := Next.Succ;
   end loop Summation;

```


#### Wording Changes from Ada 83

The constant-ness of loop parameters is specified in 3.3, "Objects and Named Numbers". 


#### Static Semantics

Version=[5],Kind=(AddedNormal),Group=[T],Term=[iterable container type], Def=[a type that has user-defined behavior for iteration, via the Default_Iterator and Iterator_Element aspects]


#### Legality Rules

Version=[5],Kind=(AddedNormal),Group=[C],Term=[iterator], Def=[a construct that is used to loop over the elements of an array or container], Note1=[Iterators can be user defined, and can perform arbitrary computations to access elements from a container.]


## 5.6  Block Statements

[A block_statement encloses a handled_sequence_of_statements optionally preceded by a declarative_part.] 


#### Syntax

block_statement ::= 
   [block_statement_identifier:]
       [declare
            declarative_part]
        begin
            handled_sequence_of_statements
        end [block_identifier];

If a block_statement has a block_statement_identifier, then the identifier shall be repeated after the end; otherwise, there shall not be an identifier after the end. 


#### Static Semantics

A block_statement that has no explicit declarative_part has an implicit empty declarative_part. 

Ramification: Thus, other rules can always refer to the declarative_part of a block_statement. 


#### Dynamic Semantics

The execution of a block_statement consists of the elaboration of its declarative_part followed by the execution of its handled_sequence_of_statements. 


#### Examples

Example of a block statement with a local variable: 

```ada
Swap:
   declare
      Temp : Integer;
   begin
      Temp := V; V := U; U := Temp;
   end Swap;

```

Ramification: If task objects are declared within a block_statement whose execution is completed, the block_statement is not left until all its dependent tasks are terminated (see 7.6). This rule applies to completion caused by a transfer of control.

Within a block_statement, the block name can be used in expanded names denoting local entities such as Swap.Temp in the above example (see 4.1.3). 


#### Wording Changes from Ada 83

The syntax rule for block_statement now uses the syntactic category handled_sequence_of_statements. 


## 5.7  Exit Statements

[An exit_statement is used to complete the execution of an enclosing loop_statement; the completion is conditional if the exit_statement includes a condition.] 


#### Syntax

exit_statement ::= 
   exit [loop_name] [when condition];


#### Name Resolution Rules

The loop_name, if any, in an exit_statement shall resolve to denote a loop_statement. 


#### Legality Rules

Each exit_statement applies to a loop_statement; this is the loop_statement being exited. An exit_statement with a name is only allowed within the loop_statement denoted by the name, and applies to that loop_statement. An exit_statement without a name is only allowed within a loop_statement, and applies to the innermost enclosing one. An exit_statement that applies to a given loop_statement shall not appear within a body or accept_statement, if this construct is itself enclosed by the given loop_statement. 


#### Dynamic Semantics

For the execution of an exit_statement, the condition, if present, is first evaluated. If the value of the condition is True, or if there is no condition, a transfer of control is done to complete the loop_statement. If the value of the condition is False, no transfer of control takes place. 

NOTE 1   Several nested loops can be exited by an exit_statement that names the outer loop. 


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


## 5.8  Goto Statements

[A goto_statement specifies an explicit transfer of control from this statement to a target statement with a given label.] 


#### Syntax

goto_statement ::= goto label_name;


#### Name Resolution Rules

The label_name shall resolve to denote a label; the statement with that label is the target statement. 


#### Legality Rules

The innermost sequence_of_statements that encloses the target statement shall also enclose the goto_statement. Furthermore, if a goto_statement is enclosed by an accept_statement or a body, then the target statement shall not be outside this enclosing construct. 

Ramification: The goto_statement can be a statement of an inner sequence_.

It follows from the second rule that if the target statement is enclosed by such a construct, then the goto_statement cannot be outside. 


#### Dynamic Semantics

The execution of a goto_statement transfers control to the target statement, completing the execution of any compound_statement that encloses the goto_statement but does not enclose the target. 

NOTE 1   The above rules allow transfer of control to a statement of an enclosing sequence_of_statements but not the reverse. Similarly, they prohibit transfers of control such as between alternatives of a case_statement, if_statement, or select_statement; between exception_handlers; or from an exception_handler of a handled_sequence_of_statements back to its sequence_of_statements. 


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

