---
sidebar_position:  14
---

# 2.8  Pragmas

A pragma is a compiler directive. There are language-defined pragmas that give instructions for optimization, listing control, etc. An implementation may support additional (implementation-defined) pragmas. Version=[5],Kind=(AddedNormal),Group=[C],Term=[pragma], Def=[a compiler directive to provide control over and above that provided by the other syntactic constructs of the language], Note1=[There are language-defined pragmas that give instructions for optimization, listing control, etc. An implementation can support additional (implementation-defined) pragmas.] 


#### Language Design Principles

{AI05-0100-1} {AI05-0163-1} In general, if all [pragma](./AA-2.8#S0019)s are treated as unrecognized [pragma](./AA-2.8#S0019)s, the program should remain both syntactically and semantically legal. There are a few exceptions to this general principle (for example, [pragma](./AA-2.8#S0019) Import can eliminate the need for a completion), but the principle remains, and is strictly true at the syntactic level. Certainly any implementation-defined [pragma](./AA-2.8#S0019)s should obey this principle both syntactically and semantically, so that if the [pragma](./AA-2.8#S0019)s are not recognized by some other implementation, the program will remain legal. 


#### Syntax

pragma<a id="S0019"></a> ::= 
   pragma [identifier](./AA-2.3#S0002) [([pragma_argument_association](./AA-2.8#S0020) {, [pragma_argument_association](./AA-2.8#S0020)})];

{AI05-0290-1} pragma_argument_association<a id="S0020"></a> ::= 
     [pragma_argument_[identifier](./AA-2.3#S0002) =&gt] [name](./AA-4.1#S0091)
   | [pragma_argument_[identifier](./AA-2.3#S0002) =&gt] [expression](./AA-4.4#S0132)
   | pragma_argument_[aspect_mark](./AA-13.1#S0347) =&gt  [name](./AA-4.1#S0091)
   | pragma_argument_[aspect_mark](./AA-13.1#S0347) =&gt  [expression](./AA-4.4#S0132)

{AI05-0290-1} In a [pragma](./AA-2.8#S0019), any [pragma_argument_association](./AA-2.8#S0020)s without a pragma_argument_[identifier](./AA-2.3#S0002) or pragma_argument_[aspect_mark](./AA-13.1#S0347) shall precede any associations with a pragma_argument_[identifier](./AA-2.3#S0002) or pragma_argument_[aspect_mark](./AA-13.1#S0347).

[Pragma](./AA-2.8#S0019)s are only allowed at the following places in a program: 

{AI12-0236-1} After a semicolon delimiter, but not within a [formal_part](./AA-6.1#S0206), [discriminant_part](./AA-3.7#S0059), or [declare_expression](./AA-4.5#S0156).

{AI05-0100-1} {AI05-0163-1} At any place where the syntax rules allow a construct defined by a syntactic category whose name ends with "declaration", "item", "statement", "clause", or "alternative", or one of the syntactic categories [variant](./AA-3.8#S0072) or [exception_handler](./AA-11.2#S0305); but not in place of such a construct if the construct is required, or is part of a list that is required to have at least one such construct.

{AI05-0163-1} In place of a [statement](./AA-5.1#S0167) in a [sequence_of_statements](./AA-5.1#S0166).

{AI05-0100-1} At any place where a [compilation_unit](./AA-10.1#S0286) is allowed. 

Additional syntax rules and placement restrictions exist for specific pragmas. 

Discussion: The above rule is written in text, rather than in BNF; the syntactic category [pragma](./AA-2.8#S0019) is not used in any BNF syntax rule. 

Ramification: A [pragma](./AA-2.8#S0019) is allowed where a [generic_formal_parameter_declaration](./AA-12.1#S0314) is allowed. 

The name of a [pragma](./AA-2.8#S0019) is the identifier following the reserved word pragma. The [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132) of a [pragma_argument_association](./AA-2.8#S0020) is a pragma argument.

To be honest: {AI95-00284-02} For compatibility with Ada 83, the name of a [pragma](./AA-2.8#S0019) may also be "interface", which is not an identifier (because it is a reserved word). See J.12. 

{AI05-0272-1} An identifier specific to a pragma is an identifier or reserved word that is used in a pragma argument with special meaning for that pragma. 

To be honest: Whenever the syntax rules for a given pragma allow "[identifier](./AA-2.3#S0002)" as an argument of the [pragma](./AA-2.8#S0019), that [identifier](./AA-2.3#S0002) is an identifier specific to that pragma.

{AI05-0272-1} In a few cases, a reserved word is allowed as "an identifier specific to a pragma". Even in these cases, the syntax still is written as [identifier](./AA-2.3#S0002) (the reserved word(s) are not shown). For example, the restriction No_Use_Of_Attribute (see 13.12.1) allows the reserved words which can be attribute designators, but the syntax for a restriction does not include these reserved words. 


#### Static Semantics

If an implementation does not recognize the name of a [pragma](./AA-2.8#S0019), then it has no effect on the semantics of the program. Inside such a [pragma](./AA-2.8#S0019), the only rules that apply are the Syntax Rules. 

To be honest: This rule takes precedence over any other rules that imply otherwise. 

Ramification: Note well: this rule applies only to [pragma](./AA-2.8#S0019)s whose name is not recognized. If anything else is wrong with a [pragma](./AA-2.8#S0019) (at compile time), the [pragma](./AA-2.8#S0019) is illegal. This is true whether the [pragma](./AA-2.8#S0019) is language defined or implementation defined.

For example, an expression in an unrecognized [pragma](./AA-2.8#S0019) does not cause freezing, even though the rules in 13.14, "Freezing Rules" say it does; the above rule overrules those other rules. On the other hand, an expression in a recognized [pragma](./AA-2.8#S0019) causes freezing, even if this makes something illegal.

For another example, an expression that would be ambiguous is not illegal if it is inside an unrecognized [pragma](./AA-2.8#S0019).

Note, however, that implementations have to recognize pragma Inline(Foo) and freeze things accordingly, even if they choose to never do inlining.

Obviously, the contradiction needs to be resolved one way or the other. The reasons for resolving it this way are: The implementation is simple - the compiler can just ignore the [pragma](./AA-2.8#S0019) altogether. The interpretation of constructs appearing inside implementation-defined [pragma](./AA-2.8#S0019)s is implementation defined. For example: "pragma Mumble(X);". If the current implementation has never heard of Mumble, then it doesn't know whether X is a name, an expression, or an identifier specific to the pragma Mumble. 

To be honest: The syntax of individual pragmas overrides the general syntax for [pragma](./AA-2.8#S0019). 

Ramification: Thus, an identifier specific to a [pragma](./AA-2.8#S0019) is not a [name](./AA-4.1#S0091), syntactically; if it were, the visibility rules would be invoked, which is not what we want.

{AI05-0229-1} This also implies that named associations do not allow one to give the arguments in an arbitrary order - the order given in the syntax rule for each individual pragma must be obeyed. However, it is generally possible to leave out earlier arguments when later ones are given; for example, this is allowed by the syntax rule for pragma Import (see J.15.5, "Interfacing Pragmas"). As for subprogram calls, positional notation precedes named notation.

Note that Ada 83 had no pragmas for which the order of named associations mattered, since there was never more than one argument that allowed named associations. 

To be honest: The interpretation of the arguments of implementation-defined pragmas is implementation defined. However, the syntax rules have to be obeyed. 


#### Dynamic Semantics

Any [pragma](./AA-2.8#S0019) that appears at the place of an executable construct is executed. Unless otherwise specified for a particular pragma, this execution consists of the evaluation of each evaluable pragma argument in an arbitrary order. 

Ramification: For a [pragma](./AA-2.8#S0019) that appears at the place of an elaborable construct, execution is elaboration.

An identifier specific to a pragma is neither a [name](./AA-4.1#S0091) nor an [expression](./AA-4.4#S0132) - such identifiers are not evaluated (unless an implementation defines them to be evaluated in the case of an implementation-defined [pragma](./AA-2.8#S0019)).

The "unless otherwise specified" part allows us (and implementations) to make exceptions, so a [pragma](./AA-2.8#S0019) can contain an expression that is not evaluated. Note that [pragma](./AA-2.8#S0019)s in [type_definition](./AA-3.2#S0025)s may contain expressions that depend on discriminants.

When we wish to define a pragma with some run-time effect, we usually make sure that it appears in an executable context; otherwise, special rules are needed to define the run-time effect and when it happens. 


#### Implementation Requirements

The implementation shall give a warning message for an unrecognized pragma name. 

Ramification: An implementation is also allowed to have modes in which a warning message is suppressed, or in which the presence of an unrecognized [pragma](./AA-2.8#S0019) is a compile-time error. 


#### Implementation Permissions

An implementation may provide implementation-defined pragmas; the name of an implementation-defined pragma shall differ from those of the language-defined pragmas. 

Implementation defined: Implementation-defined pragmas.

Ramification: The semantics of implementation-defined pragmas, and any associated rules (such as restrictions on their placement or arguments), are, of course, implementation defined. Implementation-defined pragmas may have run-time effects. 

An implementation may ignore an unrecognized pragma even if it violates some of the Syntax Rules, if detecting the syntax error is too complex. 

Reason: Many compilers use extra post-parsing checks to enforce the syntax rules, since the Ada syntax rules are not LR(k) (for any k). (The grammar is ambiguous, in fact.) This paragraph allows them to ignore an unrecognized pragma, without having to perform such post-parsing checks. 


#### Implementation Advice

{AI05-0163-1} Normally, implementation-defined pragmas should have no semantic effect for error-free programs; that is, if the implementation-defined pragmas in a working program are replaced with unrecognized pragmas, the program should still be legal, and should still have the same semantics. 

Implementation Advice: Implementation-defined pragmas should have no semantic effect for error-free programs.

Ramification: Note that "semantics" is not the same as "effect;" as explained in , the semantics defines a set of possible effects.

Note that adding a [pragma](./AA-2.8#S0019) to a program might cause an error (either at compile time or at run time). On the other hand, if the language-specified semantics for a feature are in part implementation defined, it makes sense to support pragmas that control the feature, and that have real semantics; thus, this paragraph is merely a recommendation. 

Normally, an implementation should not define pragmas that can make an illegal program legal, except as follows: 

{AI05-0229-1} A [pragma](./AA-2.8#S0019) used to complete a declaration;

Discussion: {AI05-0229-1} There are no language-defined pragmas which can be completions; [pragma](./AA-2.8#S0019) Import was defined this way in Ada 95 and Ada 2005, but in Ada 2012 [pragma](./AA-2.8#S0019) Import just sets aspect Import which disallows having any completion. 

A [pragma](./AA-2.8#S0019) used to configure the environment by adding, removing, or replacing [library_item](./AA-10.1#S0287)s. 

Implementation Advice: Implementation-defined pragmas should not make an illegal program legal, unless they complete a declaration or configure the [library_item](./AA-10.1#S0287)s in an environment.

Ramification: For example, it is OK to support Interface, System_Name, Storage_Unit, and Memory_Size [pragma](./AA-2.8#S0019)s for upward compatibility reasons, even though all of these [pragma](./AA-2.8#S0019)s can make an illegal program legal. (The latter three can affect legality in a rather subtle way: They affect the value of named numbers in System, and can therefore affect the legality in cases where static expressions are required.)

On the other hand, adding implementation-defined pragmas to a legal program can make it illegal. For example, a common kind of implementation-defined pragma is one that promises some property that allows more efficient code to be generated. If the promise is a lie, it is best if the user gets an error message. 


#### Incompatibilities With Ada 83

In Ada 83, "bad" [pragma](./AA-2.8#S0019)s are ignored. In Ada 95, they are illegal, except in the case where the name of the [pragma](./AA-2.8#S0019) itself is not recognized by the implementation. 


#### Extensions to Ada 83

Implementation-defined [pragma](./AA-2.8#S0019)s may affect the legality of a program. 


#### Wording Changes from Ada 83

Implementation-defined [pragma](./AA-2.8#S0019)s may affect the run-time semantics of the program. This was always true in Ada 83 (since it was not explicitly forbidden by RM83), but it was not clear, because there was no definition of "executing" or "elaborating" a [pragma](./AA-2.8#S0019). 


#### Extensions to Ada 2005

{AI05-0163-1} Correction: Allow [pragma](./AA-2.8#S0019)s in place of a [statement](./AA-5.1#S0167), even if there are no other [statement](./AA-5.1#S0167)s in a [sequence_of_statements](./AA-5.1#S0166).

{AI05-0272-1} Identifiers specific to a pragma can be reserved words.

{AI05-0290-1} Pragma arguments can be identified with [aspect_mark](./AA-13.1#S0347)s; this allows [identifier](./AA-2.3#S0002)'Class in this context. As usual, this is only allowed if specifically allowed by a particular pragma. 


#### Wording Changes from Ada 2005

{AI05-0100-1} Correction: Clarified where [pragma](./AA-2.8#S0019)s are (and are not) allowed. 


#### Wording Changes from Ada 2012

{AI12-0236-1} Added wording to ensure that [pragma](./AA-2.8#S0019)s are not allowed in [declare_expression](./AA-4.5#S0156)s. We don't allow this as the definition of most pragmas assume that they're given between entities. We also don't want to answer questions about conformance of [pragma](./AA-2.8#S0019)s, especially unrecognized [pragma](./AA-2.8#S0019)s and [pragma](./AA-2.8#S0019)s whose state can change between the specification and body. 


#### Syntax

The forms of List, Page, and Optimize [pragma](./AA-2.8#S0019)s are as follows:

  pragma List([identifier](./AA-2.3#S0002));

  pragma Page;

  pragma Optimize([identifier](./AA-2.3#S0002));

[Other pragmas are defined throughout this Reference Manual, and are summarized in Annex L.] 

Ramification: The language-defined pragmas are supported by every implementation, although "supporting" some of them (for example, Inline) requires nothing more than checking the arguments, since they act only as advice to the implementation. 


#### Static Semantics

A [pragma](./AA-2.8#S0019) List takes one of the [identifier](./AA-2.3#S0002)s On or Off as the single argument. This pragma is allowed anywhere a [pragma](./AA-2.8#S0019) is allowed. It specifies that listing of the compilation is to be continued or suspended until a List [pragma](./AA-2.8#S0019) with the opposite argument is given within the same compilation. The [pragma](./AA-2.8#S0019) itself is always listed if the compiler is producing a listing.

A [pragma](./AA-2.8#S0019) Page is allowed anywhere a [pragma](./AA-2.8#S0019) is allowed. It specifies that the program text which follows the [pragma](./AA-2.8#S0019) should start on a new page (if the compiler is currently producing a listing).

A [pragma](./AA-2.8#S0019) Optimize takes one of the [identifier](./AA-2.3#S0002)s Time, Space, or Off as the single argument. This [pragma](./AA-2.8#S0019) is allowed anywhere a [pragma](./AA-2.8#S0019) is allowed, and it applies until the end of the immediately enclosing declarative region, or for a [pragma](./AA-2.8#S0019) at the place of a [compilation_unit](./AA-10.1#S0286), to the end of the [compilation](./AA-10.1#S0285). It gives advice to the implementation as to whether time or space is the primary optimization criterion, or that optional optimizations should be turned off. [It is implementation defined how this advice is followed.] 

Implementation defined: Effect of pragma Optimize.

Discussion: For example, a compiler might use Time vs. Space to control whether generic instantiations are implemented with a macro-expansion model, versus a shared-generic-body model.

We don't define what constitutes an "optimization" - in fact, it cannot be formally defined in the context of Ada. One compiler might call something an optional optimization, whereas another compiler might consider that same thing to be a normal part of code generation. Thus, the programmer cannot rely on this pragma having any particular portable effect on the generated code. Some compilers might even ignore the pragma altogether. 


#### Examples

Examples of pragmas: 

```ada
{AI95-00433-01} {AI05-0229-1} {AI12-0417-1} pragma List(Off); -- turn off listing generation
pragma Optimize(Off); -- turn off optional optimizations
pragma Assertion_Policy(Check); -- check assertions
pragma Assert(Exists(File_Name),
              Message =&gt "Nonexistent file"); -- assert file exists

```


#### Extensions to Ada 83

The Optimize [pragma](./AA-2.8#S0019) now allows the identifier Off to request that normal optimization be turned off.

An Optimize [pragma](./AA-2.8#S0019) may appear anywhere pragmas are allowed. 


#### Wording Changes from Ada 83

We now describe the pragmas Page, List, and Optimize here, to act as examples, and to remove the normative material from Annex L, "Language-Defined Pragmas", so it can be entirely an informative annex. 


#### Wording Changes from Ada 95

{AI95-00433-01} Updated the example of named pragma parameters, because the second parameter of [pragma](./AA-2.8#S0019) Suppress is obsolescent. 


#### Wording Changes from Ada 2005

{AI05-0229-1} Updated the example of pragmas, because both [pragma](./AA-2.8#S0019)s Inline and Import are obsolescent. 

