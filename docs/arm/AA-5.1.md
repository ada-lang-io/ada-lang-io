---
sidebar_position:  40
---

# 5.1  Simple and Compound Statements - Sequences of Statements

{AI12-0119-1} [A [statement](./AA-5.1#S0167) is either simple or compound. A [simple_statement](./AA-5.1#S0168) encloses no other [statement](./AA-5.1#S0167). A [compound_statement](./AA-5.1#S0169) can enclose [simple_statement](./AA-5.1#S0168)s and other [compound_statement](./AA-5.1#S0169)s.] A parallel construct is a construct that introduces additional logical threads of control (see clause 9) without creating a new task. Parallel loops (see 5.5) and [parallel_block_statement](./AA-5.6#S0192)s (see 5.6.1) are parallel constructs. 

Glossary entry: A parallel construct is an executable construct that defines multiple activities of a single task that can proceed in parallel, via the execution of multiple logical threads of control.

Version=[5],Kind=(AddedNormal),Group=[C],Term=[parallel construct], Def=[an executable construct that defines multiple activities of a single task that can proceed in parallel, via the execution of multiple logical threads of control] 


#### Syntax

{AI05-0179-1} sequence_of_statements<a id="S0166"></a> ::= [statement](./AA-5.1#S0167) {[statement](./AA-5.1#S0167)} {[label](./AA-5.1#S0171)}

statement<a id="S0167"></a> ::= 
   {[label](./AA-5.1#S0171)} [simple_statement](./AA-5.1#S0168) | {[label](./AA-5.1#S0171)} [compound_statement](./AA-5.1#S0169)

{AI95-00318-02} simple_statement<a id="S0168"></a> ::= [null_statement](./AA-5.1#S0170)
   | [assignment_statement](./AA-5.2#S0173)	| [exit_statement](./AA-5.7#S0193)
   | [goto_statement](./AA-5.8#S0194)	| [procedure_call_statement](./AA-6.4#S0217)
   | [simple_return_statement](./AA-6.5#S0222)	| [entry_call_statement](./AA-9.5#S0264)
   | [requeue_statement](./AA-9.5#S0265)	| [delay_statement](./AA-9.6#S0266)
   | [abort_statement](./AA-9.8#S0284)	| [raise_statement](./AA-11.3#S0308)
   | [code_statement](./AA-13.8#S0357)

{AI95-00318-02} {AI12-0119-1} compound_statement<a id="S0169"></a> ::= 
     [if_statement](./AA-5.3#S0175)	| [case_statement](./AA-5.4#S0176)
   | [loop_statement](./AA-5.5#S0178)	| [block_statement](./AA-5.6#S0191)
   | [extended_return_statement](./AA-6.5#S0225)
   | [parallel_block_statement](./AA-5.6#S0192)
   | [accept_statement](./AA-9.5#S0258)	| [select_statement](./AA-9.7#S0269)

null_statement<a id="S0170"></a> ::= null;

label<a id="S0171"></a> ::= &lt&ltlabel_[statement_identifier](./AA-5.1#S0172)&gt&gt

statement_identifier<a id="S0172"></a> ::= [direct_name](./AA-4.1#S0092)

The [direct_name](./AA-4.1#S0092) of a [statement_identifier](./AA-5.1#S0172) shall be an [identifier](./AA-2.3#S0002) (not an [operator_symbol](./AA-6.1#S0202)). 


#### Name Resolution Rules

The [direct_name](./AA-4.1#S0092) of a [statement_identifier](./AA-5.1#S0172) shall resolve to denote its corresponding implicit declaration (see below). 


#### Legality Rules

Distinct [identifier](./AA-2.3#S0002)s shall be used for all [statement_identifier](./AA-5.1#S0172)s that appear in the same body, including inner [block_statement](./AA-5.6#S0191)s but excluding inner program units. 


#### Static Semantics

For each [statement_identifier](./AA-5.1#S0172), there is an implicit declaration (with the specified [identifier](./AA-2.3#S0002)) at the end of the [declarative_part](./AA-3.11#S0086) of the innermost [block_statement](./AA-5.6#S0191) or body that encloses the [statement_identifier](./AA-5.1#S0172). The implicit declarations occur in the same order as the [statement_identifier](./AA-5.1#S0172)s occur in the source text. If a usage name denotes such an implicit declaration, the entity it denotes is the [label](./AA-5.1#S0171), [loop_statement](./AA-5.5#S0178), or [block_statement](./AA-5.6#S0191) with the given [statement_identifier](./AA-5.1#S0172). 

Reason: We talk in terms of individual [statement_identifier](./AA-5.1#S0172)s here rather than in terms of the corresponding statements, since a given [statement](./AA-5.1#S0167) may have multiple [statement_identifier](./AA-5.1#S0172)s.

A [block_statement](./AA-5.6#S0191) that has no explicit [declarative_part](./AA-3.11#S0086) has an implicit empty [declarative_part](./AA-3.11#S0086), so this rule can safely refer to the [declarative_part](./AA-3.11#S0086) of a [block_statement](./AA-5.6#S0191).

The scope of a declaration starts at the place of the declaration itself (see 8.2). In the case of a label, loop, or block name, it follows from this rule that the scope of the implicit declaration starts before the first explicit occurrence of the corresponding name, since this occurrence is either in a statement label, a [loop_statement](./AA-5.5#S0178), a [block_statement](./AA-5.6#S0191), or a [goto_statement](./AA-5.8#S0194). An implicit declaration in a [block_statement](./AA-5.6#S0191) may hide a declaration given in an outer program unit or [block_statement](./AA-5.6#S0191) (according to the usual rules of hiding explained in 8.3).

The syntax rule for [label](./AA-5.1#S0171) uses [statement_identifier](./AA-5.1#S0172) which is a [direct_name](./AA-4.1#S0092) (not a [defining_identifier](./AA-3.1#S0022)), because labels are implicitly declared. The same applies to loop and block names. In other words, the [label](./AA-5.1#S0171) itself is not the defining occurrence; the implicit declaration is.

We cannot consider the [label](./AA-5.1#S0171) to be a defining occurrence. An example that can tell the difference is this: 

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

{AI05-0299-1} The label in this example is hidden from itself by the loop parameter with the same name; the example is illegal. We considered creating a new syntactic category name, separate from [direct_name](./AA-4.1#S0092) and [selector_name](./AA-4.1#S0099), for use in the case of statement labels. However, that would confuse the rules in Clause 8, so we didn't do it. 

{AI05-0179-1} If one or more [label](./AA-5.1#S0171)s end a [sequence_of_statements](./AA-5.1#S0166), an implicit [null_statement](./AA-5.1#S0170) follows the [label](./AA-5.1#S0171)s before any following constructs.

Reason: The semantics of a [goto_statement](./AA-5.8#S0194) is defined in terms of the statement having (following) that label. Thus we ensure that every label has a following statement, which might be implicit. 


#### Dynamic Semantics

The execution of a [null_statement](./AA-5.1#S0170) has no effect.

{AI95-00318-02} A transfer of control is the run-time action of an [exit_statement](./AA-5.7#S0193), return statement, [goto_statement](./AA-5.8#S0194), or [requeue_statement](./AA-9.5#S0265), selection of a [terminate_alternative](./AA-9.7#S0275), raising of an exception, or an abort, which causes the next action performed to be one other than what would normally be expected from the other rules of the language. [As explained in 7.6.1, a transfer of control can cause the execution of constructs to be completed and then left, which may trigger finalization.]

The execution of a [sequence_of_statements](./AA-5.1#S0166) consists of the execution of the individual [statement](./AA-5.1#S0167)s in succession until the sequence_ is completed. 

Ramification: It could be completed by reaching the end of it, or by a transfer of control. 

{AI12-0119-1} Within a parallel construct, if a transfer of control out of the construct is initiated by one of the logical threads of control, an attempt is made to cancel all other logical threads of control initiated by the parallel construct. Once all other logical threads of control of the construct either complete or are canceled, the transfer of control occurs. If two or more logical threads of control of the same construct initiate such a transfer of control concurrently, one of them is chosen arbitrarily and the others are canceled.

{AI12-0119-1} When a logical thread of control is canceled, the cancellation causes it to complete as though it had performed a transfer of control to the point where it would have finished its execution. Such a cancellation is deferred while the logical thread of control is executing within an abort-deferred operation (see 9.8), and may be deferred further, but not past a point where the logical thread initiates a new nested parallel construct or reaches an exception handler that is outside such an abort-deferred operation. 


#### Bounded (Run-Time) Errors

{AI12-0119-1} {AI12-0442-1} During the execution of a parallel construct, it is a bounded error to invoke an operation that is potentially blocking (see 9.5). Program_Error is raised if the error is detected by the implementation; otherwise, the execution of the potentially blocking operation can either proceed normally, or it can result in the indefinite blocking of some or all of the logical threads of control making up the current task. 

NOTE   A [statement_identifier](./AA-5.1#S0172) that appears immediately within the declarative region of a named [loop_statement](./AA-5.5#S0178) or an [accept_statement](./AA-9.5#S0258) is nevertheless implicitly declared immediately within the declarative region of the innermost enclosing body or [block_statement](./AA-5.6#S0191); in other words, the expanded name for a named statement is not affected by whether the statement occurs inside or outside a named loop or an [accept_statement](./AA-9.5#S0258) - only nesting within [block_statement](./AA-5.6#S0191)s is relevant to the form of its expanded name. 

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

The [requeue_statement](./AA-9.5#S0265) is new. 


#### Wording Changes from Ada 83

We define the syntactic category [statement_identifier](./AA-5.1#S0172) to simplify the description. It is used for labels, loop names, and block names. We define the entity associated with the implicit declarations of statement names.

Completion includes completion caused by a transfer of control, although RM83-5.1(6) did not take this view. 


#### Extensions to Ada 95

{AI95-00318-02} The [extended_return_statement](./AA-6.5#S0225) is new ([simple_return_statement](./AA-6.5#S0222) is merely renamed). 


#### Extensions to Ada 2005

{AI05-0179-1} A [label](./AA-5.1#S0171) can end a [sequence_of_statements](./AA-5.1#S0166), eliminating the requirement for having an explicit null; statement after an ending label (a common use). 


#### Extensions to Ada 2012

{AI12-0119-1} The definition of "parallel constructs" and the [parallel_block_statement](./AA-5.6#S0192) are new. 

