---
sidebar_position:  45
---

# 5.6  Block Statements

[A [block_statement](./AA-5.6#S0191) encloses a [handled_sequence_of_statements](./AA-11.2#S0304) optionally preceded by a [declarative_part](./AA-3.11#S0086).] 


#### Syntax

block_statement<a id="S0191"></a> ::= 
   [block_[statement_identifier](./AA-5.1#S0172):]
       [declare
            [declarative_part](./AA-3.11#S0086)]
        begin
            [handled_sequence_of_statements](./AA-11.2#S0304)
        end [block_[identifier](./AA-2.3#S0002)];

If a [block_statement](./AA-5.6#S0191) has a block_[statement_identifier](./AA-5.1#S0172), then the [identifier](./AA-2.3#S0002) shall be repeated after the end; otherwise, there shall not be an [identifier](./AA-2.3#S0002) after the end. 


#### Static Semantics

A [block_statement](./AA-5.6#S0191) that has no explicit [declarative_part](./AA-3.11#S0086) has an implicit empty [declarative_part](./AA-3.11#S0086). 

Ramification: Thus, other rules can always refer to the [declarative_part](./AA-3.11#S0086) of a [block_statement](./AA-5.6#S0191). 


#### Dynamic Semantics

The execution of a [block_statement](./AA-5.6#S0191) consists of the elaboration of its [declarative_part](./AA-3.11#S0086) followed by the execution of its [handled_sequence_of_statements](./AA-11.2#S0304). 


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

Ramification: If task objects are declared within a [block_statement](./AA-5.6#S0191) whose execution is completed, the [block_statement](./AA-5.6#S0191) is not left until all its dependent tasks are terminated (see 7.6). This rule applies to completion caused by a transfer of control.

Within a [block_statement](./AA-5.6#S0191), the block name can be used in expanded names denoting local entities such as Swap.Temp in the above example (see 4.1.3). 


#### Wording Changes from Ada 83

The syntax rule for [block_statement](./AA-5.6#S0191) now uses the syntactic category [handled_sequence_of_statements](./AA-11.2#S0304). 


## 5.6.1  Parallel Block Statements

{AI12-0119-1} {AI12-0436-1} [A [parallel_block_statement](./AA-5.6#S0192) comprises two or more [sequence_of_statements](./AA-5.1#S0166) separated by and where each represents an independent activity that is intended to proceed concurrently with the others.]


#### Syntax

{AI12-0119-1} {AI12-0427-1} {AI12-0436-1} parallel_block_statement<a id="S0192"></a> ::= 
    parallel [([chunk_specification](./AA-5.5#S0180))] [[aspect_specification](./AA-13.1#S0346)] do
       [sequence_of_statements](./AA-5.1#S0166)
    and
       [sequence_of_statements](./AA-5.1#S0166)
   {and
       [sequence_of_statements](./AA-5.1#S0166)}
    end do;

{AI12-0427-1} The [chunk_specification](./AA-5.5#S0180), if any, of a [parallel_block_statement](./AA-5.6#S0192) shall be an integer_[simple_expression](./AA-4.4#S0138). 


#### Dynamic Semantics

{AI12-0119-1} {AI12-0427-1} For the execution of a [parallel_block_statement](./AA-5.6#S0192), the [chunk_specification](./AA-5.5#S0180) and the [aspect_specification](./AA-13.1#S0346), if any, are elaborated in an arbitrary order. After elaborating the [chunk_specification](./AA-5.5#S0180), if any, a check is made that the determined maximum number of chunks is greater than zero. If this check fails, Program_Error is raised. 

{AI12-0119-1} {AI12-0427-1} {AI12-0436-1} Then, the various [sequence_of_statements](./AA-5.1#S0166) are grouped into one or more chunks, each with its own logical thread of control (see clause 9), up to the maximum number of chunks specified by the [chunk_specification](./AA-5.5#S0180), if any. Within each chunk every [sequence_of_statements](./AA-5.1#S0166) of the chunk is executed in turn, in an arbitrary order. The [parallel_block_statement](./AA-5.6#S0192) is complete once every one of the [sequence_of_statements](./AA-5.1#S0166) has completed, either by reaching the end of its execution, or due to a transfer of control out of the construct by one of the [sequence_of_statements](./AA-5.1#S0166) (see 5.1).

Implementation Note: {AI12-0119-1} {AI12-0436-1} Although each [sequence_of_statements](./AA-5.1#S0166) of a parallel block represents a separate logical thread of control, the implementation may choose to combine two or more such logical threads of control into a single physical thread of control to reduce the cost of creating numerous physical threads of control. 


#### Examples

{AI12-0429-1} Example of a parallel block used to walk a binary tree in parallel:

```ada
{AI12-0119-1} {AI12-0404-1} procedure Traverse (T : Expr_Ptr) is -- see 3.9.1
begin
   if T /= null and then
      T.all in Binary_Operation'Class -- see 3.9.1
   then -- recurse down the binary tree
      parallel do
         Traverse (T.Left);
      and
         Traverse (T.Right);
      and
         Ada.Text_IO.Put_Line
            ("Processing " & Ada.Tags.Expanded_Name (T'Tag));
      end do;
   end if;
end Traverse;

```

{AI12-0429-1} Example of a parallel block used to search two halves of a string in parallel:

```ada
{AI12-0119-1} function Search (S : String; Char : Character) return Boolean is
begin
   if S'Length &lt= 1000 then
       -- Sequential scan
       return (for some C of S =&gt C = Char);
   else
       -- Parallel divide and conquer
       declare
          Mid : constant Positive := S'First + S'Length/2 - 1;
       begin
          parallel do
             for C of S(S'First .. Mid) loop
                if C = Char then
                   return True;  -- Terminates enclosing do
                end if;
             end loop;
          and
             for C of S(Mid + 1 .. S'Last) loop
                if C = Char then
                   return True;  -- Terminates enclosing do
                end if;
             end loop;
          end do;
          -- Not found
          return False;
       end;
   end if;
end Search;

```


#### Extensions to Ada 2012

{AI12-0119-1} {AI12-0427-1} {AI12-0436-1} The [parallel_block_statement](./AA-5.6#S0192) is new. 

