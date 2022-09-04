---
sidebar_position:  2
---

# 1 General

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
Ada is a programming language designed to support the construction of long-lived, highly reliable software systems. The language includes facilities to define packages of related types, objects, and operations. The packages may be parameterized and the types may be extended to support the construction of libraries of reusable, adaptable software components. The operations may be implemented as subprograms using conventional sequential control structures, or as entries that include synchronization of concurrent threads of control as part of their invocation. The language treats modularity in the physical sense as well, with a facility to support separate compilation.

The language includes a complete facility for the support of real-time, concurrent programming. Errors can be signaled as exceptions and handled explicitly. The language also covers systems programming; this requires precise control over the representation of data and access to system-dependent properties. Finally, a predefined environment of standard packages is provided, including facilities for, among others, input-output, string manipulation, numeric elementary functions, and random number generation. 

Discussion: This Annotated Ada Reference Manual (AARM) contains the entire text of the Ada Reference Manual (RM95), plus certain annotations. The annotations give a more in-depth analysis of the language. They describe the reason for each nonobvious rule, and point out interesting ramifications of the rules and interactions among the rules (interesting to language lawyers, that is). Differences between Ada 83 and Ada 95 are listed. (The text you are reading now is an annotation.)

The AARM stresses detailed correctness and uniformity over readability and understandability. We're not trying to make the language "appear" simple here; on the contrary, we're trying to expose hidden complexities, so we can more easily detect language bugs. The RM95, on the other hand, is intended to be a more readable document for programmers.

The annotations in the AARM are as follows: 

Text that is logically redundant is shown [in square brackets, like this]. Technically, such text could be written as a Note in the RM95, since it is really a theorem that can be proven from the nonredundant rules of the language. We use the square brackets instead when it seems to make the RM95 more readable.

The rules of the language (and some AARM-only text) are categorized, and placed under certain sub-headings that indicate the category. For example, the distinction between Name Resolution Rules and Legality Rules is particularly important, as explained in 8.6.

Text under the following sub-headings appears in both documents: 

The unlabeled text at the beginning of each clause or subclause,

Syntax,

Name Resolution Rules,

Legality Rules,

Static Semantics,

Post-Compilation Rules,

Dynamic Semantics,

Bounded (Run-Time) Errors,

Erroneous Execution,

Implementation Requirements,

Documentation Requirements,

Metrics,

Implementation Permissions,

Implementation Advice,

NOTES,

Examples. 

Text under the following sub-headings does not appear in the RM95: 

Language Design Principles,

Inconsistencies With Ada 83,

Incompatibilities With Ada 83,

Extensions to Ada 83,

Wording Changes from Ada 83.

The AARM also includes the following kinds of annotations. These do not necessarily annotate the immediately preceding rule, although they often do. 

Reason: An explanation of why a certain rule is necessary, or why it is worded in a certain way. 

Ramification: An obscure ramification of the rules that is of interest only to language lawyers. (If a ramification of the rules is of interest to programmers, then it appears under NOTES.) 

Proof: An informal proof explaining how a given Note or [marked-as-redundant] piece of text follows from the other rules of the language. 

Implementation Note: A hint about how to implement a feature, or a particular potential pitfall that an implementer needs to be aware of.

Change: Change annotations are not used in this version. Changes from previous versions have been removed. Changes in this version are marked with versioned paragraph numbers, as explained in the "Corrigendum Changes" clause of the "Introduction". 

Discussion: Other annotations not covered by the above. 

To be honest: A rule that is considered logically necessary to the definition of the language, but which is so obscure or pedantic that only a language lawyer would care. These are the only annotations that could be considered part of the language definition. 

Glossary entry: The text of a Glossary entry - this text will also appear in Annex N, "Glossary". 

Discussion: In general, RM95 text appears in the normal font, whereas AARM-only text appears in a smaller font. Notes also appear in the smaller font, as recommended by ISO/IEC style guidelines. Ada examples are also usually printed in a smaller font.

If you have trouble finding things, be sure to use the index. Each defined term appears there, and also in italics, like this. Syntactic categories defined in BNF are also indexed.

A definition marked "[distributed]" is the main definition for a term whose complete definition is given in pieces distributed throughout the document. The pieces are marked "[partial]" or with a phrase explaining what cases the partial definition applies to. 


## 1.1  Scope

This Reference Manual specifies the form and meaning of programs written in Ada. Its purpose is to promote the portability of Ada programs to a variety of data processing systems.


### 1.1.1  Extent

This Reference Manual specifies: 

The form of a program written in Ada;

The effect of translating and executing such a program;

The manner in which program units may be combined to form Ada programs;

The language-defined library units that a conforming implementation is required to supply;

The permissible variations within the standard, and the manner in which they are to be documented;

Those violations of the standard that a conforming implementation is required to detect, and the effect of attempting to translate or execute a program containing such violations;

Those violations of the standard that a conforming implementation is not required to detect. 

This Reference Manual does not specify: 

The means whereby a program written in Ada is transformed into object code executable by a processor;

The means whereby translation or execution of programs is invoked and the executing units are controlled;

The size or speed of the object code, or the relative execution speed of different language constructs;

The form or contents of any listings produced by implementations; in particular, the form or contents of error or warning messages;

The effect of unspecified execution.

The size of a program or program unit that will exceed the capacity of a particular conforming implementation. 

Version=[0],New=[Structure],Old=[Structure]

This document contains thirteen sections, fourteen annexes, and an index. 

The core of the Ada language consists of: 

Sections 1 through 13 

Annex A, "Predefined Language Environment"

Annex B, "Interface to Other Languages"

Annex J, "Obsolescent Features" 

Version=[5],Kind=(AddedNormal),Group=[C],Term=[core language], Def=[a clause or annex in which are defined language constructs or capabilities that are provided by all conforming implementations], Note1=[A construct is said to be part of the core language if it is defined in a core language clause or annex.]

The following Specialized Needs Annexes define features that are needed by certain application areas: 

Annex C, "Systems Programming"

Annex D, "Real-Time Systems"

Annex E, "Distributed Systems"

Annex F, "Information Systems"

Annex G, "Numerics"

Annex H, "Safety and Security" 

Version=[5],Kind=(AddedNormal),Group=[C],Term=[specialized needs annex], Def=[an annex in which are defined language constructs or capabilities that are not necessarily provided by all conforming implementations]

The core language and the Specialized Needs Annexes are normative, except that the material in each of the items listed below is informative: 

Text under a NOTES or Examples heading.

Each clause or subclause whose title starts with the word "Example" or "Examples". 

All implementations shall conform to the core language. In addition, an implementation may conform separately to one or more Specialized Needs Annexes.

The following Annexes are informative: 

Annex K, "Language-Defined Attributes"

Annex L, "Language-Defined Pragmas"

Annex N, "Glossary" 

Annex P, "Syntax Summary"

Discussion: The idea of the Specialized Needs Annexes is that implementations can choose to target certain application areas. For example, an implementation specifically targeted to embedded machines might support the application-specific features for Real-time Systems, but not the application-specific features for Information Systems.

The Specialized Needs Annexes extend the core language only in ways that users, implementations, and standards bodies are allowed to extend the language; for example, via additional library units, attributes, representation items (see 13.1), [pragma](S0016)s, and constraints on semantic details that are left unspecified by the core language. Many implementations already provide much of the functionality defined by Specialized Needs Annexes; our goal is to increase uniformity among implementations by defining standard ways of providing the functionality.

We recommend that the validation procedures allow implementations to validate the core language, plus any set of the Specialized Needs Annexes. We recommend that implementations not be allowed to validate a portion of one of the Specialized Needs Annexes, although implementations can, of course, provide unvalidated support for such portions. We have designed the Specialized Needs Annexes assuming that this recommendation is followed. Thus, our decisions about what to include and what not to include in those annexes are based on the assumption that each annex is validated in an "all-or-nothing" manner.

An implementation may, of course, support extensions that are different from (but possibly related to) those defined by one of the Specialized Needs Annexes. We recommend that, where appropriate, implementations do this by adding library units that are children of existing language-defined library packages.

An implementation should not provide extensions that conflict with those defined in the Specialized Needs Annexes, in the following sense: Suppose an implementation supports a certain error-free program that uses only functionality defined in the core and in the Specialized Needs Annexes. The implementation should ensure that that program will still be error free in some possible full implementation of all of the Specialized Needs Annexes, and that the semantics of the program will not change. For example, an implementation should not provide a package with the same name as one defined in one of the Specialized Needs Annexes, but that behaves differently, even if that implementation does not claim conformance to that Annex.

Note that the Specialized Needs Annexes do not conflict with each other; it is the intent that a single implementation can conform to all of them. 

Each section is divided into clauses and subclauses that have a common structure. Each section, clause, and subclause first introduces its subject. After the introductory text, text is labeled with the following headings: 


#### Language Design Principles

These are not rules of the language, but guiding principles or goals used in defining the rules of the language. In some cases, the goal is only partially met; such cases are explained.

This is not part of the definition of the language, and does not appear in the RM95. 


#### Syntax

Syntax rules (indented). 


#### Name Resolution Rules

Compile-time rules that are used in name resolution, including overload resolution. 

Discussion: These rules are observed at compile time. (We say "observed" rather than "checked", because these rules are not individually checked. They are really just part of the Legality Rules in Section 8 that require exactly one interpretation of each constituent of a complete context.) The only rules used in overload resolution are the Syntax Rules and the Name Resolution Rules.

When dealing with nonoverloadable declarations it sometimes makes no semantic difference whether a given rule is a Name Resolution Rule or a Legality Rule, and it is sometimes difficult to decide which it should be. We generally make a given rule a Name Resolution Rule only if it has to be. For example, "The [name](S0084), if any, in a [raise_statement](S0235) shall be the [name](S0084) of an exception." is under "Legality Rules." 


#### Legality Rules

Rules that are enforced at compile time. A construct is legal if it obeys all of the Legality Rules. 

Discussion: These rules are not used in overload resolution.

Note that run-time errors are always attached to exceptions; for example, it is not "illegal" to divide by zero, it just raises an exception. 


#### Static Semantics

A definition of the compile-time effect of each construct. 

Discussion: The most important compile-time effects represent the effects on the symbol table associated with declarations (implicit or explicit). In addition, we use this heading as a bit of a grab bag for equivalences, package specifications, etc. For example, this is where we put statements like so-and-so is equivalent to such-and-such. (We ought to try to really mean it when we say such things!) Similarly, statements about magically-generated implicit declarations go here. These rules are generally written as statements of fact about the semantics, rather than as a you-shall-do-such-and-such sort of thing. 


#### Post-Compilation Rules

Rules that are enforced before running a partition. A partition is legal if its compilation units are legal and it obeys all of the Post-Compilation Rules. 

Discussion: It is not specified exactly when these rules are checked, so long as they are checked for any given partition before that partition starts running. An implementation may choose to check some such rules at compile time, and reject [compilation_unit](S0215)s accordingly. Alternatively, an implementation may check such rules when the partition is created (usually known as "link time"), or when the partition is mapped to a particular piece of hardware (but before the partition starts running). 


#### Dynamic Semantics

A definition of the run-time effect of each construct. 

Discussion: This heading describes what happens at run time. runtime checks, which raise exceptions upon failure, are described here. Each item that involves a runtime check is marked with the name of the check - these are the same check names that are used in a [pragma](S0016) Suppress. Principle: Every check should have a name, usable in a [pragma](S0016) Suppress. 


#### Bounded (Run-Time) Errors

Situations that result in bounded (run-time) errors (see ). 

Discussion: The "bounds" of each such error are described here - that is, we characterize the set of all possible behaviors that can result from a bounded error occurring at run time. 


#### Erroneous Execution

Situations that result in erroneous execution (see ). 


#### Implementation Requirements

Additional requirements for conforming implementations. 

Discussion: ...as opposed to rules imposed on the programmer. An example might be, "The smallest representable duration, Duration'Small, shall not be greater than twenty milliseconds".

It's really just an issue of how the rule is worded. We could write the same rule as "The smallest representable duration is an implementation-defined value less than or equal to 20 milliseconds" and then it would be under "Static Semantics". 


#### Documentation Requirements

Documentation requirements for conforming implementations. 

Discussion: These requirements are beyond those that are implicitly specified by the phrase "implementation defined". The latter require documentation as well, but we don't repeat these cases under this heading. Usually this heading is used for when the description of the documentation requirement is longer and does not correspond directly to one, narrow normative sentence. 


#### Metrics

Metrics that are specified for the time/space properties of the execution of certain language constructs. 


#### Implementation Permissions

Additional permissions given to the implementer. 

Discussion: For example, "The implementation is allowed to impose further restrictions on the record aggregates allowed in code statements." When there are restrictions on the permission, those restrictions are given here also. For example, "An implementation is allowed to restrict the kinds of subprograms that are allowed to be main subprograms. However, it shall support at least parameterless procedures." - we don't split this up between here and "Implementation Requirements". 


#### Implementation Advice

Optional advice given to the implementer. The word "should" is used to indicate that the advice is a recommendation, not a requirement. It is implementation defined whether or not a given recommendation is obeyed. 

Implementation defined: Whether or not each recommendation given in Implementation Advice is followed.

Discussion: The advice generally shows the intended implementation, but the implementer is free to ignore it. The implementer is the sole arbiter of whether or not the advice has been obeyed, if not, whether the reason is a good one, and whether the required documentation is sufficient. It would be wrong for the ACVC to enforce any of this advice.

For example, "Whenever possible, the implementation should choose a value no greater than fifty microseconds for the smallest representable duration, Duration'Small."

We use this heading, for example, when the rule is so low level or implementation-oriented as to be untestable. We also use this heading when we wish to encourage implementations to behave in a certain way in most cases, but we do not wish to burden implementations by requiring the behavior. 

NOTE 1   Notes emphasize consequences of the rules described in the (sub)clause or elsewhere. This material is informative. 


#### Examples

Examples illustrate the possible forms of the constructs described. This material is informative. 

Discussion:  
 
The next three headings list all language changes between Ada 83 and Ada 95. Language changes are any change that changes the set of text strings that are legal Ada programs, or changes the meaning of any legal program. Wording changes, such as changes in terminology, are not language changes. Each language change falls into one of the following three categories: 


#### Inconsistencies With Ada 83

This heading lists all of the upward inconsistencies between Ada 83 and Ada 95. Upward inconsistencies are situations in which a legal Ada 83 program is a legal Ada 95 program with different semantics. This type of upward incompatibility is the worst type for users, so we only tolerate it in rare situations.

(Note that the semantics of a program is not the same thing as the behavior of the program. Because of Ada's indeterminacy, the "semantics" of a given feature describes a set of behaviors that can be exhibited by that feature. The set can contain more than one allowed behavior. Thus, when we ask whether the semantics changes, we are asking whether the set of behaviors changes.)

This is not part of the definition of the language, and does not appear in the RM95. 


#### Incompatibilities With Ada 83

This heading lists all of the upward incompatibilities between Ada 83 and Ada 95, except for the ones listed under "Inconsistencies With Ada 83" above. These are the situations in which a legal Ada 83 program is illegal in Ada 95. We do not generally consider a change that turns erroneous execution into an exception, or into an illegality, to be upwardly incompatible.

This is not part of the definition of the language, and does not appear in the RM95. 


#### Extensions to Ada 83

This heading is used to list all upward compatible language changes; that is, language extensions. These are the situations in which a legal Ada 95 program is not a legal Ada 83 program. The vast majority of language changes fall into this category.

This is not part of the definition of the language, and does not appear in the RM95.

 
 
As explained above, the next heading does not represent any language change: 


#### Wording Changes from Ada 83

This heading lists some of the nonsemantic changes between RM83 and the RM95. It is incomplete; we have not attempted to list all wording changes, but only the "interesting" ones.

This is not part of the definition of the language, and does not appear in the RM95. 

Version=[5],New=[Conformity of an Implementation],Old=[Conformity of an Implementation with the Standard]


#### Implementation Requirements

A conforming implementation shall: 

Discussion: The implementation is the software and hardware that implements the language. This includes compiler, linker, operating system, hardware, etc.

We first define what it means to "conform" in general - basically, the implementation has to properly implement the normative rules given throughout the Reference Manual. Then we define what it means to conform to a Specialized Needs Annex - the implementation must support the core features plus the features of that Annex. Finally, we define what it means to "conform to the Reference Manual" - this requires support for the core language, and allows partial (but not conflicting) support for the Specialized Needs Annexes. 

Translate and correctly execute legal programs written in Ada, provided that they are not so large as to exceed the capacity of the implementation;

Identify all programs or program units that are so large as to exceed the capacity of the implementation (or raise an appropriate exception at run time); 

Implementation defined: Capacity limitations of the implementation.

Identify all programs or program units that contain errors whose detection is required by this Reference Manual; 

Discussion: Note that we no longer use the term "rejection" of programs or program units. We require that programs or program units with errors or that exceed some capacity limit be "identified". The way in which errors or capacity problems are reported is not specified.

An implementation is allowed to use standard error-recovery techniques. We do not disallow such techniques from being used across [compilation_unit](S0215) or [compilation](S0214) boundaries.

See also the Implementation Requirements of 10.2, which disallow the execution of illegal partitions.

Supply all language-defined library units required by this Reference Manual; 

Implementation Note: An implementation cannot add to or modify the visible part of a language-defined library unit, except where such permission is explicitly granted, unless such modifications are semantically neutral with respect to the client compilation units of the library unit. An implementation defines the contents of the private part and body of language-defined library units.

An implementation can add [with_clause](S0223)s and [use_clause](S0166)s, since these modifications are semantically neutral to clients. (The implementation might need [with_clause](S0223)s in order to implement the private part, for example.) Similarly, an implementation can add a private part even in cases where a private part is not shown in the Reference Manual. Explicit declarations can be provided implicitly or by renaming, provided the changes are semantically neutral.

Wherever in the Reference Manual the text of a language-defined library unit contains an italicized phrase starting with "implementation-defined", the implementation's version will replace that phrase with some implementation-defined text that is syntactically legal at that place, and follows any other applicable rules.

Note that modifications are permitted, even if there are other tools in the environment that can detect the changes (such as a program library browser), so long as the modifications make no difference with respect to the static or dynamic semantics of the resulting programs, as defined by the Reference Manual. 

Contain no variations except those explicitly permitted by this Reference Manual, or those that are impossible or impractical to avoid given the implementation's execution environment; 

Implementation defined: Variations from the standard that are impractical to avoid given the implementation's execution environment.

Reason: The "impossible or impractical" wording comes from AI-325. It takes some judgement and common sense to interpret this. Restricting compilation units to less than 4 lines is probably unreasonable, whereas restricting them to less than 4 billion lines is probably reasonable (at least given today's technology). We do not know exactly where to draw the line, so we have to make the rule vague. 

Specify all such variations in the manner prescribed by this Reference Manual. 

The external effect of the execution of an Ada program is defined in terms of its interactions with its external environment. The following are defined as external interactions: 

Any interaction with an external file (see A.7);

The execution of certain [code_statement](S0273)s (see 13.8); which [code_statement](S0273)s cause external interactions is implementation defined. 

Implementation defined: Which [code_statement](S0273)s cause external interactions.

Any call on an imported subprogram (see Annex B), including any parameters passed to it;

Any result returned or exception propagated from a main subprogram (see 10.2) or an exported subprogram (see Annex B) to an external caller; 

Discussion: By "result returned" we mean to include function results and values returned in [in] out parameters.

[Any read or update of an atomic or volatile object (see C.6);]

The values of imported and exported objects (see Annex B) at the time of any other interaction with the external environment. 

To be honest: Also other uses of imported and exported entities, as defined by the implementation, if the implementation supports such [pragma](S0016)s. 

A conforming implementation of this Reference Manual shall produce for the execution of a given Ada program a set of interactions with the external environment whose order and timing are consistent with the definitions and requirements of this Reference Manual for the semantics of the given program. 

Ramification: There is no need to produce any of the "internal effects" defined for the semantics of the program - all of these can be optimized away - so long as an appropriate sequence of external interactions is produced. 

Discussion: See also 11.6 which specifies various liberties associated with optimizations in the presence of language-defined checks, that could change the external effects that might be produced. These alternative external effects are still consistent with the standard, since 11.6 is part of the standard.

Note also that we only require "an appropriate sequence of external interactions" rather than "the same sequence..." An optimizer may cause a different sequence of external interactions to be produced than would be produced without the optimizer, so long as the new sequence still satisfies the requirements of the standard. For example, optimization might affect the relative rate of progress of two concurrent tasks, thereby altering the order in which two external interactions occur.

Note that RM83 explicitly mentions the case of an "exact effect" of a program, but since so few programs have their effects defined that exactly, we don't even mention this "special" case. In particular, almost any program that uses floating point or tasking has to have some level of inexactness in the specification of its effects. And if one includes aspects of the timing of the external interactions in the external effect of the program (as is appropriate for a real-time language), no "exact effect" can be specified. For example, if two external interactions initiated by a single task are separated by a "delay 1.0;" then the language rules imply that the two external interactions have to be separated in time by at least one second, as defined by the clock associated with the [delay_relative_statement](S0198). This in turn implies that the time at which an external interaction occurs is part of the characterization of the external interaction, at least in some cases, again making the specification of the required "exact effect" impractical. 

An implementation that conforms to this Reference Manual shall support each capability required by the core language as specified. In addition, an implementation that conforms to this Reference Manual may conform to one or more Specialized Needs Annexes (or to none). Conformance to a Specialized Needs Annex means that each capability required by the Annex is provided as specified. 

Discussion: The last sentence defines what it means to say that an implementation conforms to a Specialized Needs Annex, namely, only by supporting all capabilities required by the Annex. 

An implementation conforming to this Reference Manual may provide additional attributes, library units, and pragmas. However, it shall not provide any attribute, library unit, or pragma having the same name as an attribute, library unit, or pragma (respectively) specified in a Specialized Needs Annex unless the provided construct is either as specified in the Specialized Needs Annex or is more limited in capability than that required by the Annex. A program that attempts to use an unsupported capability of an Annex shall either be identified by the implementation before run time or shall raise an exception at run time. 

Discussion: The last sentence of the preceding paragraph defines what an implementation is allowed to do when it does not "conform" to a Specialized Needs Annex. In particular, the sentence forbids implementations from providing a construct with the same name as a corresponding construct in a Specialized Needs Annex but with a different syntax (e.g., an extended syntax) or quite different semantics. The phrase concerning "more limited in capability" is intended to give permission to provide a partial implementation, such as not implementing a subprogram in a package or having a restriction not permitted by an implementation that conforms to the Annex. For example, a partial implementation of the package Ada.Decimal might have Decimal.Max_Decimal_Digits as 15 (rather than the required 18). This allows a partial implementation to grow to a fully conforming implementation.

A restricted implementation might be restricted by not providing some subprograms specified in one of the packages defined by an Annex. In this case, a program that tries to use the missing subprogram will usually fail to compile. Alternatively, the implementation might declare the subprogram as abstract, so it cannot be called. Alternatively, a subprogram body might be implemented just to raise Program_Error. The advantage of this approach is that a program to be run under a fully conforming Annex implementation can be checked syntactically and semantically under an implementation that only partially supports the Annex. Finally, an implementation might provide a package declaration without the corresponding body, so that programs can be compiled, but partitions cannot be built and executed.

To ensure against wrong answers being delivered by a partial implementation, implementers are required to raise an exception when a program attempts to use an unsupported capability and this can be detected only at run time. For example, a partial implementation of Ada.Decimal might require the length of the Currency string to be 1, and hence, an exception would be raised if a subprogram were called in the package Edited_Output with a length greater than 1. 


#### Documentation Requirements

Certain aspects of the semantics are defined to be either implementation defined or unspecified. In such cases, the set of possible effects is specified, and the implementation may choose any effect in the set. Implementations shall document their behavior in implementation-defined situations, but documentation is not required for unspecified situations. The implementation-defined characteristics are summarized in . 

Discussion: We used to use the term "implementation dependent" instead of "unspecified". However, that sounded too much like "implementation defined". Furthermore, the term "unspecified" is used in the ANSI C and POSIX standards for this purpose, so that is another advantage. We also use "not specified" and "not specified by the language" as synonyms for "unspecified". The documentation requirement is the only difference between implementation defined and unspecified.

Note that the "set of possible effects" can be "all imaginable effects", as is the case with erroneous execution. 

The implementation may choose to document implementation-defined behavior either by documenting what happens in general, or by providing some mechanism for the user to determine what happens in a particular case. 

Discussion: For example, if the Reference Manual says that library unit elaboration order is implementation defined, the implementation might describe (in its user's manual) the algorithm it uses to determine the elaboration order. On the other hand, the implementation might provide a command that produces a description of the elaboration order for a partition upon request from the user. It is also acceptable to provide cross references to existing documentation (for example, a hardware manual), where appropriate.

Note that dependence of a program on implementation-defined or unspecified functionality is not defined to be an error; it might cause the program to be less portable, however. 


#### Implementation Advice

If an implementation detects the use of an unsupported Specialized Needs Annex feature at run time, it should raise Program_Error if feasible. 

Reason: The reason we don't require Program_Error is that there are situations where other exceptions might make sense. For example, if the Real Time Systems Annex requires that the range of System.Priority include at least 30 values, an implementation could conform to the Reference Manual (but not to the Annex) if it supported only 12 values. Since the rules of the language require Constraint_Error to be raised for out-of-range values, we cannot require Program_Error to be raised instead. 

If an implementation wishes to provide implementation-defined extensions to the functionality of a language-defined library unit, it should normally do so by adding children to the library unit. 

Implementation Note: If an implementation has support code ("run-time system code") that is needed for the execution of user-defined code, it can put that support code in child packages of System. Otherwise, it has to use some trick to avoid polluting the user's namespace. It is important that such tricks not be available to user-defined code (not in the standard mode, at least) - that would defeat the purpose. 

NOTE   The above requirements imply that an implementation conforming to this Reference Manual may support some of the capabilities required by a Specialized Needs Annex without supporting all required capabilities. 

Discussion: A conforming implementation can partially support a Specialized Needs Annex. Such an implementation does not conform to the Annex, but it does conform to the Reference Manual. 

Version=[0],New=[Method of Description and Syntax Notation],Old=[Method of Description and Syntax Notation]

The form of an Ada program is described by means of a context-free syntax together with context-dependent requirements expressed by narrative rules.

The meaning of Ada programs is described by means of narrative rules defining both the effects of each construct and the composition rules for constructs.

The context-free syntax of the language is described using a simple variant of Backus-Naur Form. In particular: 

Lower case words in a sans-serif font, some containing embedded underlines, are used to denote syntactic categories, for example: 

[case_statement](S0133)

Boldface words are used to denote reserved words, for example: 

array

Square brackets enclose optional items. Thus the two following rules are equivalent. 

[return_statement](S0160) ::= return [[expression](S0108)];
[return_statement](S0160) ::= return; | return [expression](S0108);

Curly brackets enclose a repeated item. The item may appear zero or more times; the repetitions occur from left to right as with an equivalent left-recursive rule. Thus the two following rules are equivalent. 

[term](S0111) ::= [factor](S0112) {[multiplying_operator](S0118) [factor](S0112)}
[term](S0111) ::= [factor](S0112) | [term](S0111) [multiplying_operator](S0118) [factor](S0112)

A vertical line separates alternative items unless it occurs immediately after an opening curly bracket, in which case it stands for itself: 

[constraint](S0026) ::= [scalar_constraint](S0027) | [composite_constraint](S0028)
[discrete_choice_list](S0070) ::= [discrete_choice](S0071) {| [discrete_choice](S0071)}

If the name of any syntactic category starts with an italicized part, it is equivalent to the category name without the italicized part. The italicized part is intended to convey some semantic information. For example subtype_[name](S0084) and task_[name](S0084) are both equivalent to [name](S0084) alone. 

Discussion: The grammar given in the RM95 is not LR(1). In fact, it is ambiguous; the ambiguities are resolved by the overload resolution rules (see 8.6).

We often use "if" to mean "if and only if" in definitions. For example, if we define "photogenic" by saying, "A type is photogenic if it has the following properties...", we mean that a type is photogenic if and only if it has those properties. It is usually clear from the context, and adding the "and only if" seems too cumbersome.

When we say, for example, "a [declarative_item](S0080) of a [declarative_part](S0079)", we are talking about a [declarative_item](S0080) immediately within that [declarative_part](S0079). When we say "a [declarative_item](S0080) in, or within, a [declarative_part](S0079)", we are talking about a [declarative_item](S0080) anywhere in the [declarative_part](S0079), possibly deeply nested within other [declarative_part](S0079)s. (This notation doesn't work very well for [name](S0084)s, since the name "of" something also has another meaning.)

When we refer to the name of a language-defined entity (for example, Duration), we mean the language-defined entity even in programs where the declaration of the language-defined entity is hidden by another declaration. For example, when we say that the expected type for the [expression](S0108) of a [delay_relative_statement](S0198) is Duration, we mean the language-defined type Duration that is declared in Standard, not some type Duration the user might have declared. 

A syntactic category is a nonterminal in the grammar defined in BNF under "Syntax". Names of syntactic categories are set in a different font, like_this. 

A construct is a piece of text (explicit or implicit) that is an instance of a syntactic category defined under "Syntax". Version=[5],Kind=(AddedNormal),Group=[C],Term=[construct], Def=[a piece of text (explicit or implicit) that is an instance of a syntactic category defined under Syntax] 

Ramification: For example, an [expression](S0108) is a construct. A declaration is a construct, whereas the thing declared by a declaration is an "entity". 

Discussion: "Explicit" and "implicit" don't mean exactly what you might think they mean: The text of an instance of a generic is considered explicit, even though it does not appear explicitly (in the nontechnical sense) in the program text, and even though its meaning is not defined entirely in terms of that text. 

A constituent of a construct is the construct itself, or any construct appearing within it.

Whenever the run-time semantics defines certain actions to happen in an arbitrary order, this means that the implementation shall arrange for these actions to occur in a way that is equivalent to some sequential order, following the rules that result from that sequential order. When evaluations are defined to happen in an arbitrary order, with conversion of the results to some subtypes, or with some runtime checks, the evaluations, conversions, and checks may be arbitrarily interspersed, so long as each expression is evaluated before converting or checking its value. [Note that the effect of a program can depend on the order chosen by the implementation. This can happen, for example, if two actual parameters of a given call have side effects.] 

Discussion: Programs will be more portable if their external effect does not depend on the particular order chosen by an implementation. 

Ramification: Additional reordering permissions are given in 11.6, "Exceptions and Optimization".

There is no requirement that the implementation always choose the same order in a given kind of situation. In fact, the implementation is allowed to choose a different order for two different executions of the same construct. However, we expect most implementations will behave in a relatively predictable manner in most situations. 

Reason: The "sequential order" wording is intended to allow the programmer to rely on "benign" side effects. For example, if F is a function that returns a unique integer by incrementing some global and returning the result, a call such as P(F, F) is OK if the programmer cares only that the two results of F are unique; the two calls of F cannot be executed in parallel, unless the compiler can prove that parallel execution is equivalent to some sequential order. 

NOTE 2   The syntax rules describing structured constructs are presented in a form that corresponds to the recommended paragraphing. For example, an [if_statement](S0131) is defined as: 

```ada
[if_statement](S0131) ::=
    if [condition](S0132) then
      [sequence_of_statements](S0123)
   {elsif [condition](S0132) then
      [sequence_of_statements](S0123)}
   [else
      [sequence_of_statements](S0123)]
    end if;

```

NOTE 3   The line breaks and indentation in the syntax rules indicate the recommended line breaks and indentation in the corresponding constructs. The preferred places for other line breaks are after semicolons. 

Version=[0],New=[Classification of Errors],Old=[Classification of Errors]


#### Implementation Requirements

The language definition classifies errors into several different categories: 

Errors that are required to be detected prior to run time by every Ada implementation;

These errors correspond to any violation of a rule given in this document, other than those listed below. In particular, violation of any rule that uses the terms shall, allowed, permitted, legal, or illegal belongs to this category. Any program that contains such an error is not a legal Ada program; on the other hand, the fact that a program is legal does not mean, per se, that the program is free from other forms of error.

The rules are further classified as either compile time rules, or post compilation rules, depending on whether a violation has to be detected at the time a compilation unit is submitted to the compiler, or may be postponed until the time a compilation unit is incorporated into a partition of a program. 

Ramification: See, for example, 10.1.3, "Subunits of Compilation Units", for some errors that are detected only after compilation. Implementations are allowed, but not required, to detect post compilation rules at compile time when possible. 

Errors that are required to be detected at run time by the execution of an Ada program;

The corresponding error situations are associated with the names of the predefined exceptions. Every Ada compiler is required to generate code that raises the corresponding exception if such an error situation arises during program execution. [If such an error situation is certain to arise in every execution of a construct, then an implementation is allowed (although not required) to report this fact at compilation time.]

Bounded errors;

The language rules define certain kinds of errors that need not be detected either prior to or during run time, but if not detected, the range of possible effects shall be bounded. The errors of this category are called bounded errors. The possible effects of a given bounded error are specified for each such error, but in any case one possible effect of a bounded error is the raising of the exception Program_Error. 

Erroneous execution.

In addition to bounded errors, the language rules define certain kinds of errors as leading to erroneous execution. Like bounded errors, the implementation need not detect such errors either prior to or during run time. Unlike bounded errors, there is no language-specified bound on the possible effect of erroneous execution; the effect is in general not predictable. 

Ramification: Executions are erroneous, not programs or parts of programs. Once something erroneous happens, the execution of the entire program is erroneous from that point on, and potentially before given possible reorderings permitted by 11.6 and elsewhere. We cannot limit it to just one partition, since partitions are not required to live in separate address spaces. (But implementations are encouraged to limit it as much as possible.)

Suppose a program contains a pair of things that will be executed "in an arbitrary order". It is possible that one order will result in something sensible, whereas the other order will result in erroneous execution. If the implementation happens to choose the first order, then the execution is not erroneous. This may seem odd, but it is not harmful.

Saying that something is erroneous is semantically equivalent to saying that the behavior is unspecified. However, "erroneous" has a slightly more disapproving flavor. 


#### Implementation Permissions

[ An implementation may provide nonstandard modes of operation. Typically these modes would be selected by a [pragma](S0016) or by a command line switch when the compiler is invoked. When operating in a nonstandard mode, the implementation may reject [compilation_unit](S0215)s that do not conform to additional requirements associated with the mode, such as an excessive number of warnings or violation of coding style guidelines. Similarly, in a nonstandard mode, the implementation may apply special optimizations or alternative algorithms that are only meaningful for programs that satisfy certain criteria specified by the implementation. In any case, an implementation shall support a standard mode that conforms to the requirements of this Reference Manual; in particular, in the standard mode, all legal [compilation_unit](S0215)s shall be accepted.] 

Discussion: These permissions are designed to authorize explicitly the support for alternative modes. Of course, nothing we say can prevent them anyway, but this (redundant) paragraph is designed to indicate that such alternative modes are in some sense "approved" and even encouraged where they serve the specialized needs of a given user community, so long as the standard mode, designed to foster maximum portability, is always available. 


#### Implementation Advice

If an implementation detects a bounded error or erroneous execution, it should raise Program_Error. 


#### Wording Changes from Ada 83

Some situations that are erroneous in Ada 83 are no longer errors at all. For example, depending on the parameter passing mechanism when unspecified is possibly nonportable, but not erroneous.

Other situations that are erroneous in Ada 83 are changed to be bounded errors. In particular, evaluating an uninitialized scalar variable is a bounded error. The possible results are to raise Program_Error (as always), or to produce a machine-representable value (which might not be in the subtype of the variable). Violating a Range_Check or Overflow_Check raises Constraint_Error, even if the value came from an uninitialized variable. This means that optimizers can no longer "assume" that all variables are initialized within their subtype's range. Violating a check that is suppressed remains erroneous.

The "incorrect order dependences" category of errors is removed. All such situations are simply considered potential nonportabilities. This category was removed due to the difficulty of defining what it means for two executions to have a "different effect". For example, if a function with a side effect is called twice in a single expression, it is not in principle possible for the compiler to decide whether the correctness of the resulting program depends on the order of execution of the two function calls. A compile time warning might be appropriate, but raising of Program_Error at run time would not be. 


## 1.2  Normative References

The following standards contain provisions which, through reference in this text, constitute provisions of this International Standard. At the time of publication, the editions indicated were valid. All standards are subject to revision, and parties to agreements based on this International Standard are encouraged to investigate the possibility of applying the most recent editions of the standards indicated below. Members of IEC and ISO maintain registers of currently valid International Standards. 


## 1.3  Definitions

Terms are defined throughout this document, indicated by italic type. Terms explicitly defined in this document are not to be presumed to refer implicitly to similar terms defined elsewhere. Terms not defined in this document are to be interpreted according to the Webster's Third New International Dictionary of the English Language. Informal descriptions of some terms are also given in Annex N, "Glossary". 

Discussion: The index contains an entry for every defined term.

Glossary entry: Each term defined in Annex N is marked like this. 

Discussion: Here are some AARM-only definitions: The Ada Rapporteur Group (ARG) interprets the RM83. An Ada Issue (AI) is a numbered ruling from the ARG. The Ada Commentary Integration Document (ACID) is an edition of RM83 in which clearly marked insertions and deletions indicate the effect of integrating the approved AIs. The Uniformity Rapporteur Group (URG) issues recommendations intended to increase uniformity across Ada implementations. A Uniformity Issue (UI) is a numbered recommendation from the URG. 

Version=[5],Group=[T]

Version=[5],Group=[S]

Version=[5],Group=[C]

Version=[5],Group=[R]

Version=[5],Group=[E]

