---
sidebar_position:  44
---

# 5.5  Loop Statements

{AI12-0119-1} [A [loop_statement](./AA-5.5#S0178) includes a [sequence_of_statements](./AA-5.1#S0166) that is to be executed repeatedly, zero or more times with the iterations running sequentially or concurrently with one another.] 


#### Syntax

loop_statement<a id="S0178"></a> ::= 
   [loop_[statement_identifier](./AA-5.1#S0172):]
      [[iteration_scheme](./AA-5.5#S0179)] loop
         [sequence_of_statements](./AA-5.1#S0166)
       end loop [loop_[identifier](./AA-2.3#S0002)];

{AI05-0139-2} {AI12-0119-1} {AI12-0189-1} {AI12-0251-1} {AI12-0266-1} {AI12-0326-2} {AI12-0355-2} iteration_scheme<a id="S0179"></a> ::= while [condition](./AA-4.5#S0150)
   | for [loop_parameter_specification](./AA-5.5#S0181)
   | for [iterator_specification](./AA-5.5#S0183)
   | [parallel [[aspect_specification](./AA-13.1#S0346)]]
     for [procedural_iterator](./AA-5.5#S0185)
   | parallel [([chunk_specification](./AA-5.5#S0180))] [[aspect_specification](./AA-13.1#S0346)]
     for [loop_parameter_specification](./AA-5.5#S0181)
   | parallel [([chunk_specification](./AA-5.5#S0180))] [[aspect_specification](./AA-13.1#S0346)]
     for [iterator_specification](./AA-5.5#S0183)

{AI12-0251-1} chunk_specification<a id="S0180"></a> ::= 
     integer_[simple_expression](./AA-4.4#S0138)
   | [defining_identifier](./AA-3.1#S0022) in [discrete_subtype_definition](./AA-3.6#S0055)

{AI12-0250-1} loop_parameter_specification<a id="S0181"></a> ::= 
   [defining_identifier](./AA-3.1#S0022) in [reverse] [discrete_subtype_definition](./AA-3.6#S0055)
     [[iterator_filter](./AA-5.5#S0182)]

{AI12-0250-1} iterator_filter<a id="S0182"></a> ::= when [condition](./AA-4.5#S0150)

If a [loop_statement](./AA-5.5#S0178) has a loop_[statement_identifier](./AA-5.1#S0172), then the [identifier](./AA-2.3#S0002) shall be repeated after the end loop; otherwise, there shall not be an [identifier](./AA-2.3#S0002) after the end loop.

{AI12-0119-1} An [iteration_scheme](./AA-5.5#S0179) that begins with the reserved word parallel shall not have the reserved word reverse in its [loop_parameter_specification](./AA-5.5#S0181). 


#### Name Resolution Rules

{AI12-0251-1} In a [chunk_specification](./AA-5.5#S0180) that is an integer_[simple_expression](./AA-4.4#S0138), the integer_[simple_expression](./AA-4.4#S0138) is expected to be of any integer type. 


#### Static Semantics

{AI12-0061-1} A [loop_parameter_specification](./AA-5.5#S0181) declares a loop parameter, which is an object whose subtype (and nominal subtype) is that defined by the [discrete_subtype_definition](./AA-3.6#S0055). 

{AI12-0251-1} In a [chunk_specification](./AA-5.5#S0180) that has a [discrete_subtype_definition](./AA-3.6#S0055), the [chunk_specification](./AA-5.5#S0180) declares a chunk parameter object whose subtype (and nominal subtype) is that defined by the [discrete_subtype_definition](./AA-3.6#S0055). 


#### Dynamic Semantics

{AI12-0250-1} {AI12-0327-1} The filter of an iterator construct  (a [loop_parameter_specification](./AA-5.5#S0181), [iterator_specification](./AA-5.5#S0183), or [procedural_iterator](./AA-5.5#S0185)) is defined to be satisfied when there is no [iterator_filter](./AA-5.5#S0182) for the iterator construct, or when the [condition](./AA-4.5#S0150) of the [iterator_filter](./AA-5.5#S0182) evaluates to True for a given iteration of the iterator construct.

Glossary entry: An iterator filter is a construct that is used to restrict the elements produced by an iteration to those for which a boolean condition evaluates to True.

Version=[5],Kind=(AddedNormal),Group=[C],Term=[iterator filter], Def=[a construct that is used to restrict the elements produced by an iteration to those for which a boolean condition evaluates to True]

{AI12-0250-1} {AI12-0327-1} If a [sequence_of_statements](./AA-5.1#S0166) of a [loop_statement](./AA-5.5#S0178) with an iterator construct is said to be conditionally executed, then the [statement](./AA-5.1#S0167)s are executed only when the filter of the iterator construct is satisfied.

{AI12-0250-1} {AI12-0327-1} The loop iterators [loop_parameter_specification](./AA-5.5#S0181) and [iterator_specification](./AA-5.5#S0183) can also be used in contexts other than [loop_statement](./AA-5.5#S0178)s (for example, see 4.3.5 and 4.5.8). In such a context, the iterator conditionally produces values in the order specified for the associated construct below or in 5.5.2. The values produced are the values given to the loop parameter when the filter of the iterator construct is satisfied for that value. [No value is produced when the [condition](./AA-4.5#S0150) of an [iterator_filter](./AA-5.5#S0182) evaluates to False.]

{AI12-0119-1} For the execution of a [loop_statement](./AA-5.5#S0178), the [sequence_of_statements](./AA-5.1#S0166) is executed zero or more times, until the [loop_statement](./AA-5.5#S0178) is complete. The [loop_statement](./AA-5.5#S0178) is complete when a transfer of control occurs that transfers control out of the loop, or, in the case of an [iteration_scheme](./AA-5.5#S0179), as specified below.

For the execution of a [loop_statement](./AA-5.5#S0178) with a while [iteration_scheme](./AA-5.5#S0179), the condition is evaluated before each execution of the [sequence_of_statements](./AA-5.1#S0166); if the value of the [condition](./AA-4.5#S0150) is True, the [sequence_of_statements](./AA-5.1#S0166) is executed; if False, the execution of the [loop_statement](./AA-5.5#S0178) is complete.

{AI12-0251-1} {AI12-0294-1} If the reserved word parallel is present in the [iteration_scheme](./AA-5.5#S0179) of a [loop_statement](./AA-5.5#S0178) (a parallel loop), the iterations are partitioned into one or more chunks, each with its own separate logical thread of control (see clause 9). If a [chunk_specification](./AA-5.5#S0180) is present in a parallel loop, it is elaborated first, and the result of the elaboration determines the maximum number of chunks used for the parallel loop. If the [chunk_specification](./AA-5.5#S0180) is an integer_[simple_expression](./AA-4.4#S0138), the elaboration evaluates the expression, and the value of the expression determines the maximum number of chunks. If a [discrete_subtype_definition](./AA-3.6#S0055) is present, the elaboration elaborates the [discrete_subtype_definition](./AA-3.6#S0055), which defines the subtype of the chunk parameter, and the number of values in this subtype determines the maximum number of chunks. After elaborating the [chunk_specification](./AA-5.5#S0180), a check is made that the determined maximum number of chunks is greater than zero. If this check fails, Program_Error is raised. 

{AI05-0139-2} {AI05-0262-1} {AI12-0071-1} {AI12-0119-1} {AI12-0250-1} {AI12-0251-1} {AI12-0294-1} {AI12-0355-2} {AI12-0416-1} For the execution of a [loop_statement](./AA-5.5#S0178) that has an [iteration_scheme](./AA-5.5#S0179) including a [loop_parameter_specification](./AA-5.5#S0181), after elaborating the [chunk_specification](./AA-5.5#S0180) and [aspect_specification](./AA-13.1#S0346), if any, the [loop_parameter_specification](./AA-5.5#S0181) is elaborated. This elaborates the [discrete_subtype_definition](./AA-3.6#S0055), which defines the subtype of the loop parameter. If the [discrete_subtype_definition](./AA-3.6#S0055) defines a subtype with a null range, the execution of the [loop_statement](./AA-5.5#S0178) is complete. Otherwise, the [sequence_of_statements](./AA-5.1#S0166) is conditionally executed once for each value of the discrete subtype defined by the [discrete_subtype_definition](./AA-3.6#S0055) that satisfies the predicates of the subtype (or until the loop is left as a consequence of a transfer of control). Prior to each such iteration, the corresponding value of the discrete subtype is assigned to the loop parameter associated with the given iteration. If the loop is a parallel loop, each chunk has its own logical thread of control with its own copy of the loop parameter; otherwise (a sequential loop), a single logical thread of control performs the loop, and there is a single copy of the loop parameter. Each logical thread of control handles a distinct subrange of the values of the subtype of the loop parameter such that all values are covered with no overlaps. Within each logical thread of control, the values are assigned to the loop parameter in increasing order unless the reserved word reverse is present, in which case the values are assigned in decreasing order. In the absence of a transfer of control, the associated parallel construct of a [loop_parameter_specification](./AA-5.5#S0181) is complete when all of its logical threads of control are complete. 

To be honest: {AI12-0294-1} This wording does not describe when the loop parameter object(s) are created. That creation has no side-effects (other than possibly raising Storage_Error, but anything can do that), so we simplified the wording by leaving it out. Each object has to be created before any iteration that depends on it starts, but we do not (for instance) require that the objects are all created at once at the start of the loop, nor that the objects are created after the elaboration of the [discrete_subtype_definition](./AA-3.6#S0055). 

This paragraph was deleted.{AI12-0294-1} 

Ramification: {AI05-0262-1} The predicate (if any) necessarily has to be a static predicate as a dynamic predicate is explicitly disallowed - see 3.2.4. 

Reason: {AI05-0262-1} If there is a predicate, the loop still visits the values in the order of the underlying base type; the order of the values in the predicate is irrelevant. This is the case so that the following loops have the same sequence of calls and parameters on procedure Call for any subtype S: 

```ada
for I in S loop
   Call (I);
end loop;

```

```ada
for I in S'Base loop
   if I in S then
      Call (I);
   end if;
end loop;

```

Discussion: {AI12-0416-1} The rules for completing a parallel construct when there is a transfer of control are given in 5.1. 

{AI12-0251-1} {AI12-0294-1} If a [chunk_specification](./AA-5.5#S0180) with a [discrete_subtype_definition](./AA-3.6#S0055) is present, then the logical thread of control associated with a given chunk has its own copy of the chunk parameter initialized with a distinct value from the discrete subtype defined by the [discrete_subtype_definition](./AA-3.6#S0055). The values of the chunk parameters are assigned such that they increase with increasing values of the ranges covered by the corresponding loop parameters.

{AI12-0251-1} Whether or not a [chunk_specification](./AA-5.5#S0180) is present in a parallel loop, the total number of iterations of the loop represents an upper bound on the number of logical threads of control devoted to the loop.

{AI05-0262-1} {AI12-0250-1} {AI12-0266-1} [For details about the execution of a [loop_statement](./AA-5.5#S0178) with the [iteration_scheme](./AA-5.5#S0179) including an [iterator_specification](./AA-5.5#S0183), see 5.5.2. For details relating to a [procedural_iterator](./AA-5.5#S0185), see 5.5.3.]

NOTE 1   {AI12-0250-1} A loop parameter declared by a [loop_parameter_specification](./AA-5.5#S0181) is a constant; it cannot be updated within the [sequence_of_statements](./AA-5.1#S0166) of the loop (see 3.3).

NOTE 2   {AI12-0442-1} No separate [object_declaration](./AA-3.3#S0032) is expected for a loop parameter, since the loop parameter is automatically declared by the [loop_parameter_specification](./AA-5.5#S0181). The scope of a loop parameter extends from the [loop_parameter_specification](./AA-5.5#S0181) to the end of the [loop_statement](./AA-5.5#S0178), and the visibility rules are such that a loop parameter is only visible within the [sequence_of_statements](./AA-5.1#S0166) of the loop. 

Implementation Note: An implementation could give a warning if a variable is hidden by a [loop_parameter_specification](./AA-5.5#S0181). 

NOTE 3   The [discrete_subtype_definition](./AA-3.6#S0055) of a for loop is elaborated just once. Use of the reserved word reverse does not alter the discrete subtype defined, so that the following [iteration_scheme](./AA-5.5#S0179)s are not equivalent; the first has a null range. 

```ada
for J in reverse 1 .. 0
for J in 0 .. 1

```

Ramification: If a [loop_parameter_specification](./AA-5.5#S0181) has a static discrete range, the subtype of the loop parameter is static. 


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

{AI12-0119-1} {AI12-0312-1} Example of a simple parallel loop: 

```ada
-- see 3.6
parallel
for I in Grid'Range(1) loop
   Grid(I, 1) := (for all J in Grid'Range(2) =&gt Grid(I,J) = True);
end loop;

```

{AI12-0312-1} Example of a parallel loop with a chunk specification: 

```ada
declare
   subtype Chunk_Number is Natural range 1 .. 8;

```

```ada
   Partial_Sum,
   Partial_Max : array (Chunk_Number) of Natural := (others =&gt 0);
   Partial_Min : array (Chunk_Number) of Natural :=
                       (others =&gt Natural'Last);

```

```ada
begin
   parallel (Chunk in Chunk_Number)
   for I in Grid'Range(1) loop
      declare
         True_Count : constant Natural :=
           [for J in Grid'Range(2) =&gt
              (if Grid (I, J) then 1 else 0)]'Reduce("+",0);
      begin
         Partial_Sum (Chunk) := @ + True_Count;
         Partial_Min (Chunk) := Natural'Min(@, True_Count);
         Partial_Max (Chunk) := Natural'Max(@, True_Count);
      end;
   end loop;

```

```ada
{AI12-0386-1}    Put_Line
     ("Total=" & Partial_Sum'Reduce("+", 0)'Image &
      ", Min=" & Partial_Min'Reduce(Natural'Min, Natural'Last)'Image &
      ", Max=" & Partial_Max'Reduce(Natural'Max, 0)'Image);
end;

```

{AI12-0312-1} For an example of an [iterator_filter](./AA-5.5#S0182), see 4.5.8. 


#### Wording Changes from Ada 83

The constant-ness of loop parameters is specified in 3.3, "Objects and Named Numbers". 


#### Wording Changes from Ada 2005

{AI05-0139-2} {AI05-0262-1} {AI05-0299-1} Generalized [iterator_specification](./AA-5.5#S0183)s are allowed in for loops; these are documented as an extension in the appropriate subclause. 


#### Extensions to Ada 2012

{AI12-0119-1} {AI12-0251-1} {AI12-0266-1} {AI12-0294-1} Parallel loops are new.

{AI12-0250-1} An [iterator_filter](./AA-5.5#S0182) is now allowed on [loop_parameter_specification](./AA-5.5#S0181)s. This is mainly for consistency with aggregate and reduction iterators, where it eliminates the need for temporary objects. 


#### Wording Changes from Ada 2012

{AI12-0071-1} Corrigendum: Updated wording of loop execution to use the new term "satisfies the predicates" (see 3.2.4).

{AI12-0061-1} Added text so that the nominal subtype of a loop parameter is clearly defined. 


## 5.5.1  User-Defined Iterator Types


#### Static Semantics

{AI05-0139-2} The following language-defined generic library package exists: 

```ada
{AI12-0241-1} generic
   type Cursor;
   with function Has_Element (Position : Cursor) return Boolean;
package Ada.Iterator_Interfaces
   with Pure, Nonblocking =&gt False is

```

```ada
   type Forward_Iterator is limited interface;
   function First (Object : Forward_Iterator) return Cursor is abstract;
   function Next (Object : Forward_Iterator; Position : Cursor)
      return Cursor is abstract;

```

```ada
   type Reversible_Iterator is limited interface and Forward_Iterator;
   function Last (Object : Reversible_Iterator) return Cursor is abstract;
   function Previous (Object : Reversible_Iterator; Position : Cursor)
      return Cursor is abstract;

```

```ada
{AI12-0266-1}    type Parallel_Iterator is limited interface and Forward_Iterator;

```

```ada
{AI12-0266-1}    subtype Chunk_Index is Positive;

```

```ada
{AI12-0266-1}    function Is_Split (Object : Parallel_Iterator)
      return Boolean is abstract;

```

```ada
{AI12-0266-1}    procedure Split_Into_Chunks (Object     : in out Parallel_Iterator;
                                Max_Chunks : in     Chunk_Index) is abstract
      with Pre'Class   =&gt not Object.Is_Split or else raise Program_Error,
           Post'Class  =&gt Object.Is_Split and then
                          Object.Chunk_Count &lt= Max_Chunks;

```

```ada
{AI12-0266-1}    function Chunk_Count (Object : Parallel_Iterator)
      return Chunk_Index is abstract
      with Pre'Class   =&gt Object.Is_Split or else raise Program_Error;

```

```ada
{AI12-0266-1}    function First (Object : Parallel_Iterator;
                   Chunk  : Chunk_Index) return Cursor is abstract
      with Pre'Class   =&gt (Object.Is_Split and then
                              Chunk &lt= Object.Chunk_Count)
                           or else raise Program_Error;

```

```ada
{AI12-0266-1}    function Next (Object   : Parallel_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) return Cursor is abstract
      with Pre'Class   =&gt (Object.Is_Split and then
                              Chunk &lt= Object.Chunk_Count)
                           or else raise Program_Error;

```

```ada
{AI12-0266-1}    type Parallel_Reversible_Iterator is limited interface
      and Parallel_Iterator and Reversible_Iterator;

```

```ada
end Ada.Iterator_Interfaces;

```

Reason: {AI12-0241-1} This package must allow blocking (Nonblocking =&gt False) for compatibility. The purpose of this package is to provide a template for overriding user-defined routines; and such routines can only allow blocking if the root type does so. Users can still declare their overridding routines nonblocking if they wish. 

{AI05-0139-2} {AI12-0266-1} An iterator type is a type descended from the Forward_Iterator interface from some instance of Ada.Iterator_Interfaces. A reversible iterator type is a type descended from the Reversible_Iterator interface from some instance of Ada.Iterator_Interfaces. A parallel iterator type is a type descended from the Parallel_Iterator interface from some instance of Ada.Iterator_Interfaces. A type descended from the Parallel_Reversible_Iterator interface from some instance of Ada.Iterator_Interfaces is both a parallel iterator type and a reversible iterator type. An iterator object is an object of an iterator type. A reversible iterator object is an object of a reversible iterator type. A parallel iterator object is an object of a parallel iterator type. The formal subtype Cursor from the associated instance of Ada.Iterator_Interfaces is the iteration cursor subtype for the iterator type.

{AI05-0139-2} {AI05-0292-1} The following type-related operational aspects may be specified for an indexable container type T (see 4.1.6):

{AI12-0111-1} {AI12-0428-1} Default_IteratorThis aspect is specified by a [name](./AA-4.1#S0091) that denotes exactly one function declared immediately within the same declaration list in which T, or the declaration completed by T, is declared, whose first parameter is of type T or T'Class or an access parameter whose designated type is type T or T'Class, whose other parameters, if any, have default expressions, and whose result type is an iterator type. This function is the default iterator function for T. Its result subtype is the default iterator subtype for T. The iteration cursor subtype for the default iterator subtype is the default cursor subtype for T. This aspect is inherited by descendants of type T (including T'Class).

Aspect Description for Default_Iterator: Default iterator to be used in for loops.

{AI12-0111-1} Iterator_ElementThis aspect is specified by a [name](./AA-4.1#S0091) that denotes a subtype. This is the default element subtype for T. This aspect is inherited by descendants of type T (including T'Class).

Aspect Description for Iterator_Element: Element type to be used for user-defined iterators.

{AI12-0111-1} Iterator_ViewThis aspect is specified by a [name](./AA-4.1#S0091) that denotes a type T2 with the following properties:

T2 is declared in the same compilation unit as T;

T2 is an iterable container type;

T2 has a single discriminant which is an access discriminant designating T; and

The default iterator subtypes for T and T2 statically match. 

This aspect is never inherited[, even by T'Class].

Reason: Iterator_View allows specifying an alternative type to be automatically used by container element iterators; see 5.5.2. This allows setting state for an iteration only once rather than for each individual reference. 

Ramification: Since Iterator_View is not inherited, it does not apply to T'Class. Otherwise, the type of the iterator object would not be known at compile-time (since it necessarily has to be different for each descendant). 

Aspect Description for Iterator_View: An alternative type to used for container element iterators.

This paragraph was deleted.{AI12-0111-1} 

{AI05-0139-2} {AI05-0292-1} {AI12-0266-1} An iterable container type is an indexable container type with specified Default_Iterator and Iterator_Element aspects. A reversible iterable container type is an iterable container type with the default iterator type being a reversible iterator type. A parallel iterable container type is an iterable container type with the default iterator type being a parallel iterator type. An iterable container object is an object of an iterable container type. A reversible iterable container object is an object of a reversible iterable container type. A parallel iterable container object is an object of a parallel iterable container type.

Glossary entry: An iterable container type is one that has user-defined behavior for iteration, via the Default_Iterator and Iterator_Element aspects.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[iterable container type], Def=[a type that has user-defined behavior for iteration, via the Default_Iterator and Iterator_Element aspects]

{AI12-0138-1} The Default_Iterator and Iterator_Element aspects are nonoverridable (see 13.1.1). 

Reason: This ensures that all descendants of an iterable container type have aspects with the same properties. This prevents generic contract problems with formal derived types. 


#### Legality Rules

{AI05-0139-2} {AI05-0292-1} The Constant_Indexing aspect (if any) of an iterable container type T shall denote exactly one function with the following properties:

the result type of the function is covered by the default element type of T or is a reference type (see 4.1.5) with an access discriminant designating a type covered by the default element type of T;

the type of the second parameter of the function covers the default cursor type for T;

if there are more than two parameters, the additional parameters all have default expressions.

This function (if any) is the default constant indexing function for T.

Ramification: This does not mean that Constant_Indexing has to designate only one subprogram, only that there is only one routine that meets all of these properties. There can be other routines designated by Constant_Indexing, but they cannot have the profile described above. For instance, map containers have a version of Constant_Indexing that takes a key instead of a cursor; this is allowed. 

{AI05-0139-2} {AI05-0292-1} The Variable_Indexing aspect (if any) of an iterable container type T shall denote exactly one function with the following properties:

the result type of the function is a reference type (see 4.1.5) with an access discriminant designating a type covered by the default element type of T;

the type of the second parameter of the function covers the default cursor type for T;

if there are more than two parameters, the additional parameters all have default expressions.

This function (if any) is the default variable indexing function for T.


#### Erroneous Execution

{AI12-0354-1} A call on the First or Next operation on a given Parallel_Iterator object with a given Chunk value, which does not propagate an exception, should return a Cursor value that either yields False when passed to Has_Element, or that identifies an element distinct from any Cursor value returned by a call on a First or Next operation on the same Parallel_Iterator object with a different Chunk value. If the First or Next operations with a Chunk parameter behave in any other manner, execution is erroneous. 

Reason: This describes the expectations from a user-written parallel iterator. If the expectations are not met, execution is erroneous so that implementations do not need to go to heroic efforts to avoid problems caused by bad iterators. This is similar to the handling of storage pools, see 13.11. 


#### Extensions to Ada 2005

{AI05-0139-2} User-defined iterator types are new in Ada 2012. 


#### Incompatibilities With Ada 2012

{AI12-0138-1} Corrigendum: Defined Default_Iterator and Iterator_Element to be nonoveridable, which makes redefinitions and hiding of these aspects illegal. It's possible that some program could violate one of these new restrictions, but in most cases this can easily be worked around by using overriding rather than redefinition.

{AI12-0266-1} Various new types and subprograms are newly added to Ada.Iterator_Interfaces. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0111-1} Aspect Iterator_View is new; it allows container element iterators to set the tampering state once rather than for each use of the element.

{AI12-0266-1} {AI12-0354-1} Parallel iterator interfaces are new; they allow user-defined parallel loops to be defined. 


## 5.5.2  Generalized Loop Iteration

{AI05-0139-2} Generalized forms of loop iteration are provided by an [iterator_specification](./AA-5.5#S0183). 


#### Syntax

{AI05-0139-2} {AI05-0292-1} {AI12-0156-1} {AI12-0250-1} iterator_specification<a id="S0183"></a> ::= 
    [defining_identifier](./AA-3.1#S0022) [: [loop_parameter_subtype_indication](./AA-5.5#S0184)] in [reverse] iterator_[name](./AA-4.1#S0091)
      [[iterator_filter](./AA-5.5#S0182)]
  | [defining_identifier](./AA-3.1#S0022) [: [loop_parameter_subtype_indication](./AA-5.5#S0184)] of [reverse] iterable_[name](./AA-4.1#S0091)
      [[iterator_filter](./AA-5.5#S0182)]

{AI12-0156-1} loop_parameter_subtype_indication<a id="S0184"></a> ::= [subtype_indication](./AA-3.2#S0027) | [access_definition](./AA-3.10#S0084)

{AI12-0266-1} If an [iterator_specification](./AA-5.5#S0183) is for a parallel construct, the reserved word reverse shall not appear in the [iterator_specification](./AA-5.5#S0183). 


#### Name Resolution Rules

{AI05-0139-2} {AI05-0292-1} For the first form of [iterator_specification](./AA-5.5#S0183), called a generalized iterator, the expected type for the iterator_[name](./AA-4.1#S0091) is any iterator type. For the second form of [iterator_specification](./AA-5.5#S0183), the expected type for the iterable_[name](./AA-4.1#S0091) is any array or iterable container type. If the iterable_[name](./AA-4.1#S0091) denotes an array object, the [iterator_specification](./AA-5.5#S0183) is called an array component iterator; otherwise it is called a container element iterator. 

Glossary entry: An iterator is a construct that is used to loop over the elements of an array or container. Iterators may be user defined, and may perform arbitrary computations to access elements from a container.


#### Legality Rules

Version=[5],Kind=(AddedNormal),Group=[C],Term=[iterator], Def=[a construct that is used to loop over the elements of an array or container], Note1=[Iterators can be user defined, and can perform arbitrary computations to access elements from a container.]

{AI05-0139-2} {AI12-0266-1} If the reserved word reverse appears, the [iterator_specification](./AA-5.5#S0183) is a reverse iterator. If the [iterator_specification](./AA-5.5#S0183) is for a parallel construct, the [iterator_specification](./AA-5.5#S0183) is a parallel iterator. Otherwise, it is a forward iterator. Forward and reverse iterators are collectively called sequential iterators. In a reverse generalized iterator, the iterator_[name](./AA-4.1#S0091) shall be of a reversible iterator type. In a parallel generalized iterator, the iterator_[name](./AA-4.1#S0091) shall be of a parallel iterator type. In a reverse container element iterator, the default iterator type for the type of the iterable_[name](./AA-4.1#S0091) shall be a reversible iterator type. In a parallel container element iterator, the default iterator type for the type of the iterable_[name](./AA-4.1#S0091) shall be of a parallel iterator type.

{AI05-0139-2} {AI12-0151-1} {AI12-0156-1} {AI12-0183-1} The subtype defined by the [loop_parameter_subtype_indication](./AA-5.5#S0184), if any, of a generalized iterator shall statically match the iteration cursor subtype. The subtype defined by the [loop_parameter_subtype_indication](./AA-5.5#S0184), if any, of an array component iterator shall statically match the component subtype of the type of the iterable_[name](./AA-4.1#S0091). The subtype defined by the [loop_parameter_subtype_indication](./AA-5.5#S0184), if any, of a container element iterator shall statically match the default element subtype for the type of the iterable_[name](./AA-4.1#S0091).

{AI05-0139-2} In a container element iterator whose iterable_[name](./AA-4.1#S0091) has type T, if the iterable_[name](./AA-4.1#S0091) denotes a constant or the Variable_Indexing aspect is not specified for T, then the Constant_Indexing aspect shall be specified for T.

{AI12-0047-1} The iterator_[name](./AA-4.1#S0091) or iterable_[name](./AA-4.1#S0091) of an [iterator_specification](./AA-5.5#S0183) shall not denote a subcomponent that depends on discriminants of an object whose nominal subtype is unconstrained, unless the object is known to be constrained.

Reason: This is the same rule that applies to renames; it serves the same purpose of preventing the object from disappearing while the iterator is still using it. 

{AI12-0120-1} A container element iterator is illegal if the call of the default iterator function that creates the loop iterator (see below) is illegal.

Ramification: This can happen if the parameter to the default iterator function is in out and the iterable_[name](./AA-4.1#S0091) is a constant. The wording applies to any reason that the call would be illegal, as it's possible that one of the default parameters would be illegal, or that some accessibility check would fail. 

{AI12-0120-1} A generalized iterator is illegal if the iteration cursor subtype of the iterator_[name](./AA-4.1#S0091) is a limited type at the point of the generalized iterator. A container element iterator is illegal if the default cursor subtype of the type of the iterable_[name](./AA-4.1#S0091) is a limited type at the point of the container element iterator.

Reason: If the cursor type is limited, the assignment to the loop parameter for a generalized iterator would be illegal. The same is true for a container element iterator. We have to say "at the point of the iterator" as the limitedness of a type can change due to visibility. 


#### Static Semantics

{AI05-0139-2} {AI05-0269-1} {AI05-0292-1} {AI12-0156-1} An [iterator_specification](./AA-5.5#S0183) declares a loop parameter. In a generalized iterator, an array component iterator, or a container element iterator, if a [loop_parameter_subtype_indication](./AA-5.5#S0184) is present, it determines the nominal subtype of the loop parameter. In a generalized iterator, if a [loop_parameter_subtype_indication](./AA-5.5#S0184) is not present, the nominal subtype of the loop parameter is the iteration cursor subtype. In an array component iterator, if a [loop_parameter_subtype_indication](./AA-5.5#S0184) is not present, the nominal subtype of the loop parameter is the component subtype of the type of the iterable_[name](./AA-4.1#S0091). In a container element iterator, if a [loop_parameter_subtype_indication](./AA-5.5#S0184) is not present, the nominal subtype of the loop parameter is the default element subtype for the type of the iterable_[name](./AA-4.1#S0091).

{AI05-0139-2} {AI05-0292-1} In a generalized iterator, the loop parameter is a constant. In an array component iterator, the loop parameter is a constant if the iterable_[name](./AA-4.1#S0091) denotes a constant; otherwise it denotes a variable. In a container element iterator, the loop parameter is a constant if the iterable_[name](./AA-4.1#S0091) denotes a constant, or if the Variable_Indexing aspect is not specified for the type of the iterable_[name](./AA-4.1#S0091); otherwise it is a variable.

Ramification: {AI12-0093-1} {AI12-0156-1} The loop parameter of a generalized iterator has the same accessibility as the loop statement. This means that the loop parameter object is finalized when the loop statement is left. (It also may be finalized as part of assigning a new value to the loop parameter.) For array component iterators, the loop parameter directly denotes an element of the array and has the accessibility of the associated array. For container element iterators, the loop parameter denotes the result of the indexing function call (in the case of a constant indexing) or a generalized reference thereof (in the case of a variable indexing). Roughly speaking, the loop parameter has the accessibility level of a single iteration of the loop. More precisely, the function result (or the generalized reference thereof) is considered to be renamed in the declarative part of a notional block statement which immediately encloses the loop's [sequence_of_statements](./AA-5.1#S0166); the accessibility of the loop parameter is that of the block statement. 


#### Dynamic Semantics

{AI05-0139-2} For the execution of a [loop_statement](./AA-5.5#S0178) with an [iterator_specification](./AA-5.5#S0183), the [iterator_specification](./AA-5.5#S0183) is first elaborated. This elaboration elaborates the [subtype_indication](./AA-3.2#S0027), if any.

{AI05-0139-2} {AI12-0250-1} {AI12-0266-1} For a sequential generalized iterator, the loop parameter is created, the iterator_[name](./AA-4.1#S0091) is evaluated, and the denoted iterator object becomes the loop iterator. In a forward generalized iterator, the operation First of the iterator type is called on the loop iterator, to produce the initial value for the loop parameter. If the result of calling Has_Element on the initial value is False, then the execution of the [loop_statement](./AA-5.5#S0178) is complete. Otherwise, the [sequence_of_statements](./AA-5.1#S0166) is conditionally executed and then the Next operation of the iterator type is called with the loop iterator and the current value of the loop parameter to produce the next value to be assigned to the loop parameter. This repeats until the result of calling Has_Element on the loop parameter is False, or the loop is left as a consequence of a transfer of control. For a reverse generalized iterator, the operations Last and Previous are called rather than First and Next.

Ramification: {AI12-0093-1} The loop parameter of a generalized iterator is a variable of which the user only has a constant view. It follows the normal rules for a variable of its nominal subtype. In particular, if the nominal subtype is indefinite, the variable is constrained by its initial value. Similarly, if the nominal subtype is class-wide, the variable (like all variables) has the tag of the initial value. Constraint_Error may be raised by a subsequent iteration if Next or Previous return an object with a different tag or constraint. 

{AI12-0266-1} For a parallel generalized iterator, the [chunk_specification](./AA-5.5#S0180), if any, of the associated parallel construct, is first elaborated, to determine the maximum number of chunks (see 5.5), and then the operation Split_Into_Chunks of the iterator type is called, with the determined maximum passed as the Max_Chunks parameter, specifying the upper bound for the number of loop parameter objects (and the number of logical threads of control) to be associated with the iterator. In the absence of a [chunk_specification](./AA-5.5#S0180), the maximum number of chunks is determined in an implementation-defined manner.

Implementation defined: The maximum number of chunks for a parallel generalized iterator without a [chunk_specification](./AA-5.5#S0180).

Discussion: The Max_Chunks parameter of the Split_Into_Chunks procedure is an upper bound for the number of chunks to be associated with a loop. A container implementation may opt for a lower value for the number of chunks if a more optimal split can be determined. For instance, a tree-based container might create the split based on the number of branches at the top levels of the tree. 

{AI12-0250-1} {AI12-0266-1} {AI12-0418-1} Upon return from Split_Into_Chunks, the actual number of chunks for the loop is determined by calling the Chunk_Count operation of the iterator, at which point one logical thread of control is initiated for each chunk, with an associated chunk index in the range from one to the actual number of chunks.

{AI12-0250-1} {AI12-0266-1} {AI12-0418-1} Within each logical thread of control, a loop parameter is created. If a [chunk_specification](./AA-5.5#S0180) with a [discrete_subtype_definition](./AA-3.6#S0055) is present in the associated parallel construct, then a chunk parameter is created and initialized with a value from the discrete subtype defined by the [discrete_subtype_definition](./AA-3.6#S0055), so that the order of the chosen chunk parameter values correspond to the order of the chunk indices associated with the logical threads of control. The operation First of the iterator type that has a Chunk parameter is called on the loop iterator, with Chunk initialized from the corresponding chunk index, to produce the initial value for the loop parameter. If the result of calling Has_Element on this initial value is False, then the execution of the logical thread of control is complete. Otherwise, the [sequence_of_statements](./AA-5.1#S0166) is conditionally executed, and then the Next operation of the iterator type that has a Chunk parameter is called with the loop iterator, the current value of the loop parameter, and the corresponding chunk index, to produce the next value to be assigned to the loop parameter. This repeats until the result of calling Has_Element on the loop parameter is False, or the associated parallel construct is left as a consequence of a transfer of control.

{AI12-0250-1} {AI12-0266-1} {AI12-0418-1} In the absence of a transfer of control, the associated parallel construct of a parallel generalized iterator is complete when all of its logical threads of control are complete.

{AI05-0139-2} {AI05-0292-1} {AI12-0250-1} {AI12-0266-1} For an array component iterator, the [chunk_specification](./AA-5.5#S0180) of the associated parallel construct, if any, is first elaborated to determine the maximum number of chunks (see 5.5), and then the iterable_[name](./AA-4.1#S0091) is evaluated and the denoted array object becomes the array for the loop. If the array for the loop is a null array, then the execution of the [loop_statement](./AA-5.5#S0178) is complete. Otherwise, the [sequence_of_statements](./AA-5.1#S0166) is conditionally executed with the loop parameter denoting each component of the array for the loop, using a canonical order of components, which is last dimension varying fastest (unless the array has convention Fortran, in which case it is first dimension varying fastest). For a forward array component iterator, the iteration starts with the component whose index values are each the first in their index range, and continues in the canonical order. For a reverse array component iterator, the iteration starts with the component whose index values are each the last in their index range, and continues in the reverse of the canonical order. For a parallel array component iterator, the iteration is broken up into contiguous chunks of the canonical order, such that all components are covered with no overlaps; each chunk has its own logical thread of control with its own loop parameter and iteration within each chunk is in the canonical order. The number of chunks is implementation defined, but is limited in the presence of a [chunk_specification](./AA-5.5#S0180) to the determined maximum. The loop iteration proceeds until the [sequence_of_statements](./AA-5.1#S0166) has been conditionally executed for each component of the array for the loop, or until the loop is left as a consequence of a transfer of control.

Implementation defined: The number of chunks for an array component iterator.

{AI12-0266-1} If a [chunk_specification](./AA-5.5#S0180) with a [discrete_subtype_definition](./AA-3.6#S0055) is present in the associated parallel construct, then the logical thread of control associated with a given chunk has a chunk parameter initialized with a distinct value from the discrete subtype defined by the [discrete_subtype_definition](./AA-3.6#S0055). The values of the chunk parameters are assigned such that they increase in the canonical order of the starting array components for the chunks.

{AI05-0139-2} {AI05-0292-1} {AI12-0111-1} {AI12-0266-1} For a container element iterator, the [chunk_specification](./AA-5.5#S0180) of the associated parallel construct, if any, is first elaborated to determine the maximum number of chunks (see 5.5), and then the iterable_[name](./AA-4.1#S0091) is evaluated. If the container type has Iterator_View specified, an object of the Iterator_View type is created with the discriminant referencing the iterable container object denoted by the iterable_[name](./AA-4.1#S0091). This is the iterable container object for the loop. Otherwise, the iterable container object denoted by the iterable_[name](./AA-4.1#S0091) becomes the iterable container object for the loop. The default iterator function for the type of the iterable container object for the loop is called on the iterable container object and the result is the loop iterator. For a sequential container element iterator, an object of the default cursor subtype is created (the loop cursor). For a parallel container element iterator, each chunk of iterations will have its own loop cursor, again of the default cursor subtype.

Reason: {AI12-0111-1} If Iterator_View is specified, we add an extra object and use that object for this iteration. This allows these iterators to automatically use the stable view (defined in each of the language-defined containers) to do the iteration. That eliminates the need to set and clear the tampering with elements indication each time Reference is called; that eliminates substantial overhead as finalization is typically used to implement the tampering reset. 

{AI05-0139-2} {AI05-0292-1} {AI12-0266-1} A container element iterator then proceeds as described above for a generalized iterator, except that each reference to a loop parameter is replaced by a reference to the corresponding loop cursor. For a container element iterator, the loop parameter for each iteration instead denotes an indexing (see 4.1.6) into the iterable container object for the loop, with the only parameter to the indexing being the value of the loop cursor for the given iteration. If the loop parameter is a constant (see above), then the indexing uses the default constant indexing function for the type of the iterable container object for the loop; otherwise it uses the default variable indexing function.

{AI12-0120-1} Any exception propagated by the execution of a generalized iterator or container element iterator is propagated by the immediately enclosing loop statement.

Ramification: This text covers exceptions raised by called functions that make up the execution of the iterator as well as exceptions raised by the assignment to the loop parameter or cursor. 


#### Examples

{AI12-0429-1} Example of a parallel generalized loop over an array:

```ada
{AI05-0269-1} {AI12-0266-1} {AI12-0429-1} parallel
for Element of Board loop  -- See 3.6.1.
   Element := Element * 2.0; -- Double each element of Board, a two-dimensional array.
end loop;

```

{AI05-0268-1} {AI12-0429-1} For examples of use of generalized iterators, see A.18.33 and the corresponding container packages in A.18.2 and A.18.3.


#### Extensions to Ada 2005

{AI05-0139-2} Generalized forms of loop iteration are new. 


#### Incompatibilities With Ada 2012

{AI12-0047-1} Corrigendum: Added a rule to ensure that the object being iterated cannot be a component that could disappear before the loop completes. This could be incompatible by making a loop that was legal (and worked correctly, so long as the enclosing object is not modified during the loop) from the original Ada 2012 illegal in corrected Ada 2012. Such loops should be pretty rare, especially as these iterator forms are new to Ada 2012.

{AI12-0120-1} Corrigendum: Added rules to reject loops if the call to the default iterator function for a container element iterator is illegal, or if the cursor type of an iterator is limited. These are formally incompatible with original Ada 2012, but as it's unlikely that any Ada 2012 compiler ever allowed the illegal usages in an expansion of a loop (it's much more likely that they would have just caused an internal error in the compiler), this should have no effect in practice.

{AI12-0151-1} Corrigendum: Added a requirement that the given subtype statically match the subtype of the element or component for a component element iterator or array component iterator. Original Ada 2012 text allowed any type that covered the subtype of the element or component, but that led to questions of what the meaning was if they are different. In this case, the element is essentially a renaming of the container element, and it doesn't make sense for the constraints to be different. Ignoring explicitly defined constraints in renames is a mistake that we don't want to continue, thus we require static matching. This means that some programs might be illegal, but those programs were misleading at best, and potentially would raise unexpected exceptions because the element values might have been invalid or abnormal with respect to the declared constraint. 


#### Extensions to Ada 2012

{AI12-0156-1} For consistency, we now allow a [subtype_indication](./AA-3.2#S0027) on a generalized iterator, and anonymous access types on all forms of iterator. We introduced a new syntax non-terminal, [loop_parameter_subtype_indication](./AA-5.5#S0184) to simplfy the wording.

{AI12-0250-1} An [iterator_filter](./AA-5.5#S0182) is now allowed on [iterator_specification](./AA-5.5#S0183)s. This is mainly for consistency with aggregate and reduction iterators, where it eliminates the need for temporary objects. 


#### Wording Changes from Ada 2012

{AI12-0120-1} Corrigendum: Added wording to specify that a loop propagates any exceptions propagated by the execution of an iterator. Since that's what naturally would happen from a macro-style expansion of the parts of an iterator, and no other interpretation makes sense given the way the rest of Ada works, we consider it so unlikely that any Ada 2012 implementation ever did anything else that we don't document this as a possible inconsistency.

{AI12-0111-1} Added wording to include the use of the iterator view in a container element iterator.

{AI12-0266-1} Added wording to describe the execution of parallel iterators. 


## 5.5.3  Procedural Iterators

{AI12-0189-1} A [procedural_iterator](./AA-5.5#S0185) invokes a user-defined procedure, passing in the body of the enclosing [loop_statement](./AA-5.5#S0178) as a parameter of an anonymous access-to-procedure type, to allow the loop body to be executed repeatedly as part of the invocation of the user-defined procedure. 


#### Syntax

{AI12-0189-1} {AI12-0250-1} procedural_iterator<a id="S0185"></a> ::= 
     [iterator_parameter_specification](./AA-5.5#S0186) of [iterator_procedure_call](./AA-5.5#S0187)
       [[iterator_filter](./AA-5.5#S0182)]

{AI12-0189-1} {AI12-0308-1} iterator_parameter_specification<a id="S0186"></a> ::= 
     [formal_part](./AA-6.1#S0206)
   | ([defining_identifier](./AA-3.1#S0022){, [defining_identifier](./AA-3.1#S0022)})

{AI12-0189-1} iterator_procedure_call<a id="S0187"></a> ::= 
     procedure_[name](./AA-4.1#S0091)
   | procedure_[prefix](./AA-4.1#S0093) [iterator_actual_parameter_part](./AA-5.5#S0188)

{AI12-0189-1} iterator_actual_parameter_part<a id="S0188"></a> ::= 
     ([iterator_parameter_association](./AA-5.5#S0189) {, [iterator_parameter_association](./AA-5.5#S0189)})

{AI12-0189-1} iterator_parameter_association<a id="S0189"></a> ::= 
     [parameter_association](./AA-6.4#S0220)
   | [parameter_association_with_box](./AA-5.5#S0190)

{AI12-0189-1} parameter_association_with_box<a id="S0190"></a> ::= 
   [ formal_parameter_[selector_name](./AA-4.1#S0099) =&gt ] &lt&gt

{AI12-0189-1} At most one [iterator_parameter_association](./AA-5.5#S0189) within an [iterator_actual_parameter_part](./AA-5.5#S0188) shall be a [parameter_association_with_box](./AA-5.5#S0190). 


#### Name Resolution Rules

{AI12-0189-1} {AI12-0292-1} {AI12-0326-2} The [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) given in an [iterator_procedure_call](./AA-5.5#S0187) shall resolve to denote a callable entity C (the iterating procedure) that is a procedure, or an entry renamed as (viewed as) a procedure. [When there is an [iterator_actual_parameter_part](./AA-5.5#S0188), the [prefix](./AA-4.1#S0093) can be an [implicit_dereference](./AA-4.1#S0095) of an access-to-subprogram value.]

{AI12-0189-1} An [iterator_procedure_call](./AA-5.5#S0187) without a [parameter_association_with_box](./AA-5.5#S0190) is equivalent to one with an [iterator_actual_parameter_part](./AA-5.5#S0188) with an additional [parameter_association_with_box](./AA-5.5#S0190) at the end, with the formal_parameter_[selector_name](./AA-4.1#S0099) identifying the last formal parameter of the callable entity denoted by the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093).

{AI12-0189-1} {AI12-0320-1} An [iterator_procedure_call](./AA-5.5#S0187) shall contain at most one [iterator_parameter_association](./AA-5.5#S0189) for each formal parameter of the callable entity C. Each formal parameter without an [iterator_parameter_association](./AA-5.5#S0189) shall have a [default_expression](./AA-3.7#S0063) (in the profile of the view of C denoted by the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093)).

{AI12-0189-1} The formal parameter of the callable entity C associated with the [parameter_association_with_box](./AA-5.5#S0190) shall be of an anonymous access-to-procedure type A. 


#### Legality Rules

{AI12-0189-1} {AI12-0308-1} The anonymous access-to-procedure type A shall have at least one formal parameter in its parameter profile. If the [iterator_parameter_specification](./AA-5.5#S0186) is a [formal_part](./AA-6.1#S0206), then this [formal_part](./AA-6.1#S0206) shall be mode conformant with that of A. If the [iterator_parameter_specification](./AA-5.5#S0186) is a list of [defining_identifier](./AA-3.1#S0022)s, the number of formal parameters of A shall be the same as the length of this list.

{AI12-0189-1} {AI12-0292-1} [If the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) given in an [iterator_procedure_call](./AA-5.5#S0187) denotes an abstract subprogram, the subprogram shall be a dispatching subprogram.]

Proof: {AI12-0320-1} This is stated normatively in 3.9.3. 


#### Static Semantics

{AI12-0189-1} {AI12-0250-1} {AI12-0308-1} {AI12-0326-2} A [loop_statement](./AA-5.5#S0178) with an [iteration_scheme](./AA-5.5#S0179) that has a [procedural_iterator](./AA-5.5#S0185) is equivalent to a local declaration of a procedure P followed by a [procedure_call_statement](./AA-6.4#S0217) that is formed from the [iterator_procedure_call](./AA-5.5#S0187) by replacing the &lt&gt of the [parameter_association_with_box](./AA-5.5#S0190) with P'Access. The [formal_part](./AA-6.1#S0206) of the locally declared procedure P is formed from the [formal_part](./AA-6.1#S0206) of the anonymous access-to-procedure type A, by replacing the [identifier](./AA-2.3#S0002) of each formal parameter of this [formal_part](./AA-6.1#S0206) with the [identifier](./AA-2.3#S0002) of the corresponding formal parameter or element of the list of [defining_identifier](./AA-3.1#S0022)s given in the [iterator_parameter_specification](./AA-5.5#S0186). The body of P consists of the conditionally executed [sequence_of_statements](./AA-5.1#S0166). The procedure P is called the loop body procedure.

Implementation Note: {AI12-0250-1} For a [procedural_iterator](./AA-5.5#S0185) with an [iterator_filter](./AA-5.5#S0182), the body of the routine would be something like:

```ada
procedure P ... is
begin
   if [iterator_filter](./AA-5.5#S0182) then
      [sequence_of_statements](./AA-5.1#S0166)
   end if;
end P;

```

{AI12-0344-1} In a procedural iterator, the Parallel_Calls aspect (see 9.10.1) of the loop body procedure is True if the reserved word parallel occurs in the corresponding loop statement, and False otherwise.

{AI12-0189-1} {AI12-0326-2} {AI12-0344-1} The following aspects may be specified for a callable entity S that has exactly one formal parameter of an anonymous access-to-subprogram type:

Allows_ExitThe Allows_Exit aspect is of type Boolean. The specified value shall be static. The Allows_Exit aspect of an inherited primitive subprogram is True if Allows_Exit is True either for the corresponding subprogram of the progenitor type or for any other inherited subprogram that it overrides. If not specified or inherited as True, the Allows_Exit aspect of a callable entity is False. For an entry, only a confirming specification of False is permitted for the Allows_Exit aspect.

Reason: An entry does not allow exit, because implementing a transfer of control out of a task or protected entry creates unnecessarily complex dynamic semantics. 

Specifying the Allows_Exit aspect to be True for a subprogram indicates that the subprogram allows exit, meaning that it is prepared to be completed by arbitrary transfers of control from the loop body procedure[, including propagation of exceptions. A subprogram for which Allows_Exit is True should use finalization as appropriate rather than exception handling to recover resources and make any necessary final updates to data structures].

Aspect Description for Allows_Exit: An indication of whether a subprogram will operate correctly for arbitrary transfers of control.

Ramification: A subprogram that does not need cleanup satisfies the requirements, and thus can specify Allows_Exit as True. If a subprogram S allows exit, it cannot expect to get control other than via finalization if the loop body procedure initiates a transfer of control as part of a [procedural_iterator](./AA-5.5#S0185). In particular, exception handlers in S, even when others handlers, will not be executed when a transfer of control occurs. The mechanism that the implementation uses to implement such transfers of control needs to avoid triggering exception handlers. 

Parallel_IteratorThe Parallel_Iterator aspect is of type Boolean. The specified value shall be static. The Parallel_Iterator aspect of an inherited primitive subprogram is True if Parallel_Iterator is True either for the corresponding subprogram of the progenitor type or for any other inherited subprogram that it overrides. If not specified or inherited as True, the Parallel_Iterator aspect of a callable entity is False.

{AI12-0189-1} {AI12-0442-1} Specifying the Parallel_Iterator aspect to be True for a callable entity indicates that the entity is allowed to invoke the loop body procedure from multiple distinct logical threads of control. The Parallel_Iterator aspect for a subprogram shall be statically False if the subprogram allows exit.

Aspect Description for Parallel_Iterator: An indication of whether a subprogram may use multiple threads of control to invoke a loop body procedure.

Reason: Permitting exit from a parallel procedural iterator introduces additional semantic and implementation complexity. 


#### Legality Rules

{AI12-0189-1} {AI12-0326-2} If a callable entity overrides an inherited dispatching subprogram that allows exit, the overriding callable entity also shall allow exit. If a callable entity overrides an inherited dispatching subprogram that has a True Parallel_Iterator aspect, the overriding callable entity also shall have a True Parallel_Iterator aspect.

Ramification: Since an entry never allows exit, attempting to implement an allows exit subprogram with a task or protected entry is always illegal. However, the Parallel_Iterator aspect can be applied to an entry, so a subprogram with the Parallel_Iterator aspect True can be implemented by an entry. 

{AI12-0326-2} A [loop_statement](./AA-5.5#S0178) with a [procedural_iterator](./AA-5.5#S0185) as its [iteration_scheme](./AA-5.5#S0179) shall begin with the reserved word parallel if and only if the callable entity identified in the [iterator_procedure_call](./AA-5.5#S0187) has a Parallel_iterator aspect of True.

{AI12-0326-2} If the actual parameter of an anonymous access-to-subprogram type, passed in an explicit call of a subprogram for which the Parallel_Iterator aspect is True, is of the form P'Access, the designated subprogram P shall have a Parallel_Calls aspect True (see 9.10.1).

{AI12-0189-1} {AI12-0326-2} The [sequence_of_statements](./AA-5.1#S0166) of a [loop_statement](./AA-5.5#S0178) with a [procedural_iterator](./AA-5.5#S0185) as its [iteration_scheme](./AA-5.5#S0179) shall contain an [exit_statement](./AA-5.7#S0193), return statement, [goto_statement](./AA-5.8#S0194), or [requeue_statement](./AA-9.5#S0265) that leaves the loop only if the callable entity associated with the [procedural_iterator](./AA-5.5#S0185) allows exit.

{AI12-0294-1} The [sequence_of_statements](./AA-5.1#S0166) of a [loop_statement](./AA-5.5#S0178) with a [procedural_iterator](./AA-5.5#S0185) as its [iteration_scheme](./AA-5.5#S0179) shall not contain an [accept_statement](./AA-9.5#S0258) whose [entry_declaration](./AA-9.5#S0257) occurs outside the [loop_statement](./AA-5.5#S0178).

Reason: An [accept_statement](./AA-9.5#S0258) is not allowed in a procedure (see 9.5.2), it has to be directly in a [task_body](./AA-9.1#S0248). Since the loop body here is implemented as  a procedure, we can't allow [accept_statement](./AA-9.5#S0258)s there, either, even if the loop itself is directly in a [task_body](./AA-9.1#S0248). 

Ramification: This includes cases where the [accept_statement](./AA-9.5#S0258) is part of another construct, for instance, a [select_statement](./AA-9.7#S0269). 


#### Dynamic Semantics

{AI12-0326-2} {AI12-0355-2} {AI12-0445-1} [For the execution of a [loop_statement](./AA-5.5#S0178) with an [iteration_scheme](./AA-5.5#S0179) that has a [procedural_iterator](./AA-5.5#S0185), the procedure denoted by the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) of the [iterator_procedure_call](./AA-5.5#S0187) (the iterating procedure) is invoked, passing an access value designating the loop body procedure as a parameter. The iterating procedure then calls the loop body procedure zero or more times and returns, whereupon the [loop_statement](./AA-5.5#S0178) is complete. If the parallel reserved word is present, the iterating procedure is allowed to invoke the loop body procedure from multiple distinct logical threads of control.] The [aspect_specification](./AA-13.1#S0346), if any, is elaborated prior to the invocation of the iterating procedure.

Proof: The stated dynamic semantics are implied by the static semantics given above and the bounded errors given below. 


#### Bounded (Run-Time) Errors

{AI12-0326-2} {AI12-0445-1} If the callable entity identified in the [iterator_procedure_call](./AA-5.5#S0187) allows exit, then it is a bounded error for a call of the loop body procedure to be performed from within an abort-deferred operation (see 9.8), unless the entire [loop_statement](./AA-5.5#S0178) was within the same abort-deferred operation. If detected, Program_Error is raised at the point of the call; otherwise, a transfer of control from the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) will not necessarily terminate the [loop_statement](./AA-5.5#S0178), and the loop body procedure can be called again.

{AI12-0326-2} {AI12-0445-1} If a [loop_statement](./AA-5.5#S0178) with the [procedural_iterator](./AA-5.5#S0185) as its [iteration_scheme](./AA-5.5#S0179) (see 5.5) does not begin with the reserved word parallel, it is a bounded error if the loop body procedure is invoked from a different logical thread of control than the one that initiates the [loop_statement](./AA-5.5#S0178). If detected, Program_Error is raised; otherwise, conflicts associated with concurrent executions of the loop body procedure can occur without being detected by the applicable conflict check policy (see 9.10.1). Furthermore, propagating an exception or making an attempt to exit in the presence of multiple threads of control will not necessarily terminate the [loop_statement](./AA-5.5#S0178), deadlock can occur, or the loop body procedure can be called again.

Discussion: {AI12-0326-2} Other Ada rules are still in effect for the allows exit subprogram A, of course. For instance, if a transfer of control causes finalization which raises an exception, Program_Error will be propagated by A (rather than the transfer of control). In such a case, the bounded error above would still apply. Another example is the case where an unrelated task is waiting on the normal completion of the loop body procedure call in A. Such a task might end up waiting forever if a transfer of control happens (this is a deadlock situation). This case does not require additional wording, as the same thing would happen if an exception is propagated from the loop body procedure or if A executed a transfer of control (such as a return statement). 


#### Examples

{AI12-0189-1} {AI12-0379-1} {AI12-0429-1} Example of iterating over a map from My_Key_Type to My_Element_Type (see A.18.4):

```ada
for (C : Cursor) of My_Map.Iterate loop
   Put_Line (My_Key_Type'Image (Key (C)) & " =&gt " &
      My_Element_Type'Image (Element (C)));
end loop;

```

```ada
-- The above is equivalent to:

```

```ada
declare
   procedure P (C : Cursor) is
   begin
      Put_Line (My_Key_Type'Image (Key (c)) & " =&gt " &
         My_Element_Type'Image (Element (C)));
   end P;
begin
   My_Map.Iterate (P'Access);
end;

```

{AI12-0189-1} {AI12-0429-1} Example of iterating over the environment variables (see A.17):

```ada
for (Name, Val) of Ada.Environment_Variables.Iterate(&lt&gt) loop
   --  "(&lt&gt)" is optional because it is the last parameter
   Put_Line (Name & " =&gt " & Val);
end loop;

```

```ada
-- The above is equivalent to:

```

```ada
declare
   procedure P (Name : String; Val : String) is
   begin
      Put_Line (Name & " =&gt " & Val);
   end P;
begin
   Ada.Environment_Variables.Iterate (P'Access);
end;

```


#### Extensions to Ada 2012

{AI12-0189-1} {AI12-0292-1} {AI12-0294-1} {AI12-0326-2} {AI12-0344-1} Procedural iterators, and the Allows_Exit and Parallel_Iterator aspects are new in Ada 2022. 

