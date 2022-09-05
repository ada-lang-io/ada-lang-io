---
sidebar_position:  41
---

# 5.2  Assignment Statements

[An [assignment_statement](./AA-5.2#S0173) replaces the current value of a variable with the result of evaluating an [expression](./AA-4.4#S0132).] 


#### Syntax

assignment_statement<a id="S0173"></a> ::= 
   variable_[name](./AA-4.1#S0091) := [expression](./AA-4.4#S0132);

The execution of an [assignment_statement](./AA-5.2#S0173) includes the evaluation of the [expression](./AA-4.4#S0132) and the assignment of the value of the [expression](./AA-4.4#S0132) into the target. [An assignment operation (as opposed to an [assignment_statement](./AA-5.2#S0173)) is performed in other contexts as well, including object initialization and by-copy parameter passing.] The target of an assignment operation is the view of the object to which a value is being assigned; the target of an [assignment_statement](./AA-5.2#S0173) is the variable denoted by the variable_[name](./AA-4.1#S0091). 

Discussion: Don't confuse this notion of the "target" of an assignment with the notion of the "target object" of an entry call or requeue.

Don't confuse the term "assignment operation" with the [assignment_statement](./AA-5.2#S0173). The assignment operation is just one part of the execution of an [assignment_statement](./AA-5.2#S0173). The assignment operation is also a part of the execution of various other constructs; see 7.6.1, "Completion and Finalization" for a complete list. Note that when we say, "such-and-such is assigned to so-and-so", we mean that the assignment operation is being applied, and that so-and-so is the target of the assignment operation. 


#### Name Resolution Rules

{AI95-00287-01} The variable_[name](./AA-4.1#S0091) of an [assignment_statement](./AA-5.2#S0173) is expected to be of any type. The expected type for the [expression](./AA-4.4#S0132) is the type of the target. 

Implementation Note: An [assignment_statement](./AA-5.2#S0173) as a whole is a "complete context", so if the variable_[name](./AA-4.1#S0091) of an [assignment_statement](./AA-5.2#S0173) is overloaded, the [expression](./AA-4.4#S0132) can be used to help disambiguate it. For example: 

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

{AI95-00287-01} The target [denoted by the variable_[name](./AA-4.1#S0091)] shall be a variable of a nonlimited type.

If the target is of a tagged class-wide type T'Class, then the [expression](./AA-4.4#S0132) shall either be dynamically tagged, or of type T and tag-indeterminate (see 3.9.2). 

Reason: This is consistent with the general rule that a single dispatching operation shall not have both dynamically tagged and statically tagged operands. Note that for an object initialization (as opposed to the [assignment_statement](./AA-5.2#S0173)), a statically tagged initialization expression is permitted, since there is no chance for confusion (or Tag_Check failure). Also, in an object initialization, tag-indeterminate expressions of any type covered by T'Class would be allowed, but with an [assignment_statement](./AA-5.2#S0173), that might not work if the tag of the target was for a type that didn't have one of the dispatching operations in the tag-indeterminate expression. 


#### Dynamic Semantics

For the execution of an [assignment_statement](./AA-5.2#S0173), the variable_[name](./AA-4.1#S0091) and the [expression](./AA-4.4#S0132) are first evaluated in an arbitrary order. 

Ramification: Other rules of the language may require that the bounds of the variable be determined prior to evaluating the [expression](./AA-4.4#S0132), but that does not necessarily require evaluation of the variable_[name](./AA-4.1#S0091), as pointed out by the ACID. 

When the type of the target is class-wide: 

If the [expression](./AA-4.4#S0132) is tag-indeterminate (see 3.9.2), then the controlling tag value for the [expression](./AA-4.4#S0132) is the tag of the target; 

Ramification: See 3.9.2, "Dispatching Operations of Tagged Types". 

Otherwise [(the [expression](./AA-4.4#S0132) is dynamically tagged)], a check is made that the tag of the value of the [expression](./AA-4.4#S0132) is the same as that of the target; if this check fails, Constraint_Error is raised. 

{AI12-0439-1} The value of the [expression](./AA-4.4#S0132) is converted to the subtype of the target. [The conversion can raise an exception (see 4.6).] 

Ramification: 4.6, "Type Conversions" defines what actions and checks are associated with subtype conversion. For non-array subtypes, it is just a constraint check presuming the types match. For array subtypes, it checks the lengths and slides if the target is constrained. "Sliding" means the array doesn't have to have the same bounds, so long as it is the same length. 

{AI12-0439-1} In cases involving controlled types, the target is finalized, and an anonymous object can be used as an intermediate in the assignment, as described in 7.6.1, "Completion and Finalization". In any case, the converted value of the [expression](./AA-4.4#S0132) is then assigned to the target, which consists of the following two steps: 

To be honest: 7.6.1 actually says that finalization happens always, but unless controlled types are involved, this finalization during an [assignment_statement](./AA-5.2#S0173) does nothing. 

The value of the target becomes the converted value.

{AI05-0299-1} If any part of the target is controlled, its value is adjusted as explained in subclause 7.6. 

Ramification: If any parts of the object are controlled, abort is deferred during the assignment operation itself, but not during the rest of the execution of an [assignment_statement](./AA-5.2#S0173). 

NOTE   The tag of an object never changes; in particular, an [assignment_statement](./AA-5.2#S0173) does not change the tag of the target.

This paragraph was deleted.{AI95-00363-01} 

Ramification: The implicit subtype conversion described above for [assignment_statement](./AA-5.2#S0173)s is performed only for the value of the right-hand side expression as a whole; it is not performed for subcomponents of the value.

The determination of the type of the variable of an [assignment_statement](./AA-5.2#S0173) may require consideration of the expression if the variable name can be interpreted as the name of a variable designated by the access value returned by a function call, and similarly, as a component or slice of such a variable (see 8.6, "The Context of Overload Resolution"). 


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
{AI12-0056-1} Writer := (Status =&gt Open, Unit =&gt Printer, Line_Count =&gt 60);  -- see 3.8.1
Next.all := (72074, null, Head);   --  see 3.10.1

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

NOTE 1   Notes on the examples: [Assignment_statement](./AA-5.2#S0173)s are allowed even in the case of overlapping slices of the same array, because the variable_[name](./AA-4.1#S0091) and [expression](./AA-4.4#S0132) are both evaluated before copying the value into the variable. In the above example, an implementation yielding A(1 .. 12) = "tartartartar" would be incorrect. 


#### Extensions to Ada 83

We now allow user-defined finalization and value adjustment actions as part of [assignment_statement](./AA-5.2#S0173)s (see 7.6, "Assignment and Finalization"). 


#### Wording Changes from Ada 83

The special case of array assignment is subsumed by the concept of a subtype conversion, which is applied for all kinds of types, not just arrays. For arrays it provides "sliding". For numeric types it provides conversion of a value of a universal type to the specific type of the target. For other types, it generally has no run-time effect, other than a constraint check.

We now cover in a general way in 3.7.2 the erroneous execution possible due to changing the value of a discriminant when the variable in an [assignment_statement](./AA-5.2#S0173) is a subcomponent that depends on discriminants. 


#### Incompatibilities With Ada 95

{AI95-00287-01} The change of the limited check from a resolution rule to a legality rule is not quite upward compatible. For example 

```ada
type AccNonLim is access NonLim;
function Foo (Arg : in Integer) return AccNonLim;
type AccLim is access Lim;
function Foo (Arg : in Integer) return AccLim;
Foo(2).all := Foo(1).all;

```

where NonLim is a nonlimited type and Lim is a limited type. The assignment is legal in Ada 95 (only the first Foo would be considered), and is ambiguous in Ada 2005. We made the change because we want limited types to be as similar to nonlimited types as possible. Limited expressions are now allowed in all other contexts (with a similar incompatibility), and it would be odd if assignments had different resolution rules (which would eliminate ambiguities in some cases). Moreover, examples like this one are rare, as they depend on assigning into overloaded function calls. 


## 5.2.1  Target Name Symbols

{AI12-0125-3} @, known as the target name of an assignment statement, provides an abbreviation to avoid repetition of potentially long names in assignment statements. 


#### Syntax

{AI12-0125-3} target_name<a id="S0174"></a> ::= @


#### Name Resolution Rules

{AI12-0125-3} [If a [target_name](./AA-5.2#S0174) occurs in an [assignment_statement](./AA-5.2#S0173) A, the variable_[name](./AA-4.1#S0091) V of A is a complete context. The target name is a constant view of V, having the nominal subtype of V.]

Proof: {AI12-0125-3} {AI12-0322-1} The complete context rule is formally given in 8.6. The constant view rule is formally given in 3.3; the nominal subtype is a property taken from the target object as described below in Dynamic Semantics. 


#### Legality Rules

{AI12-0125-3} {AI12-0322-1} A [target_name](./AA-5.2#S0174) shall appear only in the [expression](./AA-4.4#S0132) of an [assignment_statement](./AA-5.2#S0173). 


#### Dynamic Semantics

{AI12-0125-3} {AI12-0322-1} For the execution of an [assignment_statement](./AA-5.2#S0173) with one or more [target_name](./AA-5.2#S0174)s appearing in its [expression](./AA-4.4#S0132), the variable_[name](./AA-4.1#S0091) V of the [assignment_statement](./AA-5.2#S0173) is evaluated first to determine the object denoted by V, and then the [expression](./AA-4.4#S0132) of the [assignment_statement](./AA-5.2#S0173) is evaluated with the evaluation of each [target_name](./AA-5.2#S0174) yielding a constant view of the the target whose properties are otherwise identical to those of the view provided by V. The remainder of the execution of the [assignment_statement](./AA-5.2#S0173) is as given in subclause 5.2.

To be honest: The properties here include static properties like whether the [target_name](./AA-5.2#S0174) is aliased and the nominal subtype of the [target_name](./AA-5.2#S0174). It was too weird to give separate rules for static and dynamic properties that said almost the same thing. 

Ramification: {AI12-0322-1} Use of a [target_name](./AA-5.2#S0174) can be erroneous if the variable_[name](./AA-4.1#S0091) V is a discriminant-dependent component, and some other constituent of the [expression](./AA-4.4#S0132) modifies the discriminant governing the component V. The assignment probably would be erroneous anyway, but the use of a [target_name](./AA-5.2#S0174) eliminates the possibility that a later evaluation of V raises an exception before any erroneous execution occurs. See 3.7.2. 


#### Examples

{AI12-0429-1} Examples of the use of target name symbols:

```ada
{AI12-0125-3} {AI12-0379-1} {AI12-0442-1} Board(1, 1) := @ + 1.0;  -- An abbreviation for Board(1, 1) := Board(1, 1) + 1.0;
                       -- (Board is declared in 3.6.1).

```

```ada
{AI12-0125-3} My_Complex_Array : array (1 .. Max) of Complex; -- See 3.3.2, 3.8.
...
-- Square the element in the Count (see 3.3.1) position:
My_Complex_Array (Count) := (Re =&gt @.Re**2 - @.Im**2,
                             Im =&gt 2.0 * @.Re * @.Im);
   -- A target_name can be used multiple times and as a prefix if desired.

```


#### Extensions to Ada 2012

{AI12-0125-3} {AI12-0322-1} The target name symbol @ is new. 

