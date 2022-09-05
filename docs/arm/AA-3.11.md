---
sidebar_position:  27
---

# 3.11  Declarative Parts

[A [declarative_part](./AA-3.11#S0086) contains [declarative_item](./AA-3.11#S0087)s (possibly none).] 


#### Syntax

declarative_part<a id="S0086"></a> ::= {[declarative_item](./AA-3.11#S0087)}

declarative_item<a id="S0087"></a> ::= 
    [basic_declarative_item](./AA-3.11#S0088) | [body](./AA-3.11#S0089)

{8652/0009} {AI95-00137-01} basic_declarative_item<a id="S0088"></a> ::= 
    [basic_declaration](./AA-3.1#S0021) | [aspect_clause](./AA-13.1#S0343) | [use_clause](./AA-8.4#S0235)

body<a id="S0089"></a> ::= [proper_body](./AA-3.11#S0090) | [body_stub](./AA-10.1#S0297)

proper_body<a id="S0090"></a> ::= 
    [subprogram_body](./AA-6.3#S0216) | [package_body](./AA-7.2#S0231) | [task_body](./AA-9.1#S0248) | [protected_body](./AA-9.4#S0254)


#### Static Semantics

{AI95-00420-01} The list of [declarative_item](./AA-3.11#S0087)s of a [declarative_part](./AA-3.11#S0086) is called the declaration list of the [declarative_part](./AA-3.11#S0086). 


#### Dynamic Semantics

The elaboration of a [declarative_part](./AA-3.11#S0086) consists of the elaboration of the [declarative_item](./AA-3.11#S0087)s, if any, in the order in which they are given in the [declarative_part](./AA-3.11#S0086).

An elaborable construct is in the elaborated state after the normal completion of its elaboration. Prior to that, it is not yet elaborated. 

Ramification: The elaborated state is only important for bodies; certain uses of a body raise an exception if the body is not yet elaborated.

Note that "prior" implies before the start of elaboration, as well as during elaboration.

The use of the term "normal completion" implies that if the elaboration propagates an exception or is aborted, the declaration is not elaborated. RM83 missed the aborted case. 

For a construct that attempts to use a body, a check (Elaboration_Check) is performed, as follows: 

{8652/0014} {AI95-00064-01} For a call to a (non-protected) subprogram that has an explicit body, a check is made that the body is already elaborated. This check and the evaluations of any actual parameters of the call are done in an arbitrary order. 

Discussion: AI83-00180 specifies that there is no elaboration check for a subprogram defined by a [pragma](./AA-2.8#S0019) Interface (or equivalently, [pragma](./AA-2.8#S0019) Import). AI83-00430 specifies that there is no elaboration check for an enumeration literal. AI83-00406 specifies that the evaluation of parameters and the elaboration check occur in an arbitrary order. AI83-00406 applies to generic instantiation as well (see below).

{8652/0014} {AI95-00064-01} {AI05-0177-1} A subprogram can be completed by a renaming-as-body, a [null_procedure_declaration](./AA-6.7#S0227), or an [expression_function_declaration](./AA-6.8#S0228), and we need to make an elaboration check on such a body, so we use "body" rather than [subprogram_body](./AA-6.3#S0216) above. 

{AI05-0229-1} For a call to a protected operation of a protected type (that has a body - no check is performed if  the protected type is imported - see B.1), a check is made that the [protected_body](./AA-9.4#S0254) is already elaborated. This check and the evaluations of any actual parameters of the call are done in an arbitrary order. 

Discussion: A protected type has only one elaboration "bit", rather than one for each operation, because one call may result in evaluating the barriers of other entries, and because there are no elaborable declarations between the bodies of the operations. In fact, the elaboration of a [protected_body](./AA-9.4#S0254) does not elaborate the enclosed bodies, since they are not considered independently elaborable.

Note that there is no elaboration check when calling a task entry. Task entry calls are permitted even before the associated [task_body](./AA-9.1#S0248) has been seen. Such calls are simply queued until the task is activated and reaches a corresponding [accept_statement](./AA-9.5#S0258). We considered a similar rule for protected entries - simply queuing all calls until the [protected_body](./AA-9.4#S0254) was seen, but felt it was not worth the possible implementation overhead, particularly given that there might be multiple instances of the protected type. 

For the activation of a task, a check is made by the activator that the [task_body](./AA-9.1#S0248) is already elaborated. If two or more tasks are being activated together (see 9.2), as the result of the elaboration of a [declarative_part](./AA-3.11#S0086) or the initialization for the object created by an allocator, this check is done for all of them before activating any of them. 

Reason: As specified by AI83-00149, the check is done by the activator, rather than by the task itself. If it were done by the task itself, it would be turned into a Tasking_Error in the activator, and the other tasks would still be activated. 

For the instantiation of a generic unit that has a body, a check is made that this body is already elaborated. This check and the evaluation of any [explicit_generic_actual_parameter](./AA-12.3#S0318)s of the instantiation are done in an arbitrary order. 

The exception Program_Error is raised if any of these checks fails. 


#### Extensions to Ada 83

{AI95-00114-01} The syntax for [declarative_part](./AA-3.11#S0086) is modified to remove the ordering restrictions of Ada 83; that is, the distinction between [basic_declarative_item](./AA-3.11#S0088)s and later_declarative_items within [declarative_part](./AA-3.11#S0086)s is removed. This means that things like [use_clause](./AA-8.4#S0235)s and [object_declaration](./AA-3.3#S0032)s can be freely intermixed with things like bodies.

The syntax rule for [proper_body](./AA-3.11#S0090) now allows a [protected_body](./AA-9.4#S0254), and the rules for elaboration checks now cover calls on protected operations. 


#### Wording Changes from Ada 83

The syntax rule for later_declarative_item is removed; the syntax rule for [declarative_item](./AA-3.11#S0087) is new.

RM83 defines "elaborated" and "not yet elaborated" for [declarative_item](./AA-3.11#S0087)s here, and for other things in 3.1, "Declarations". That's no longer necessary, since these terms are fully defined in 3.1.

In RM83, all uses of [declarative_part](./AA-3.11#S0086) are optional (except for the one in [block_statement](./AA-5.6#S0191) with a declare) which is sort of strange, since a [declarative_part](./AA-3.11#S0086) can be empty, according to the syntax. That is, [declarative_part](./AA-3.11#S0086)s are sort of "doubly optional". In Ada 95, these [declarative_part](./AA-3.11#S0086)s are always required (but can still be empty). To simplify description, we go further and say (see 5.6, "Block Statements") that a [block_statement](./AA-5.6#S0191) without an explicit [declarative_part](./AA-3.11#S0086) is equivalent to one with an empty one. 


#### Wording Changes from Ada 95

{8652/0009} {AI95-00137-01} Corrigendum: Changed representation clauses to aspect clauses to reflect that they are used for more than just representation.

{8652/0014} {AI95-00064-01} Corrigendum: Clarified that the elaboration check applies to all kinds of subprogram bodies.

{AI95-00420-01} Defined "declaration list" to avoid confusion for various rules. Other kinds of declaration list are defined elsewhere. 


## 3.11.1  Completions of Declarations

{8652/0014} {AI95-00064-01} {AI05-0177-1} Declarations sometimes come in two parts. A declaration that requires a second part is said to require completion. The second part is called the completion of the declaration (and of the entity declared), and is either another declaration, a body, or a [pragma](./AA-2.8#S0019). A body is a [body](./AA-3.11#S0089), an [entry_body](./AA-9.5#S0260), a [null_procedure_declaration](./AA-6.7#S0227) or an [expression_function_declaration](./AA-6.8#S0228) that completes another declaration, or a renaming-as-body (see 8.5.4). 

Discussion: Throughout the RM95, there are rules about completions that define the following: 

Which declarations require a corresponding completion.

Which constructs can only serve as the completion of a declaration.

Where the completion of a declaration is allowed to be.

What kinds of completions are allowed to correspond to each kind of declaration that allows one. 

Don't confuse this compile-time concept with the run-time concept of completion defined in 7.6.1.

Note that the declaration of a private type (if limited) can be completed with the declaration of a task type, which is then completed with a body. Thus, a declaration can actually come in three parts.

{AI95-00217-06} {AI05-0162-1} An incomplete type (whether declared in the limited view of a package or not) may be completed by a private type declaration, so we can in fact have four parts.

{AI05-0229-1} In Ada 2012, there are no language-defined pragmas that act as completions. Pragma Import (which is obsolescent) has the effect of setting aspect Import to True; such an aspect makes giving a completion illegal. The wording that allows pragmas as completions was left as it is harmless and appears in many places in this Reference Manual. 


#### Name Resolution Rules

A construct that can be a completion is interpreted as the completion of a prior declaration only if: 

The declaration and the completion occur immediately within the same declarative region;

The defining name or [defining_program_unit_name](./AA-6.1#S0201) in the completion is the same as in the declaration, or in the case of a [pragma](./AA-2.8#S0019), the [pragma](./AA-2.8#S0019) applies to the declaration;

If the declaration is overloadable, then the completion either has a type-conformant profile, or is a [pragma](./AA-2.8#S0019). 


#### Legality Rules

{AI05-0229-1} An implicit declaration shall not have a completion. For any explicit declaration that is specified to require completion, there shall be a corresponding explicit completion, unless the declared entity is imported (see B.1). 

To be honest: {AI95-00217-06} The implicit declarations occurring in a limited view do have a completion (the explicit declaration occurring in the full view) but that's a special case, since the implicit declarations are actually built from the explicit ones. So they do not require a completion, they have one by fiat. 

Discussion: {AI05-0299-1} The implicit declarations of predefined operators are not allowed to have a completion. Enumeration literals, although they are subprograms, are not allowed to have a corresponding [subprogram_body](./AA-6.3#S0216). That's because the completion rules are described in terms of constructs ([subprogram_declaration](./AA-6.1#S0195)s) and not entities (subprograms). When a completion is required, it has to be explicit; the implicit null [package_body](./AA-7.2#S0231) that Clause 7 talks about cannot serve as the completion of a [package_declaration](./AA-7.1#S0229) if a completion is required. 

At most one completion is allowed for a given declaration. Additional requirements on completions appear where each kind of completion is defined. 

Ramification: A subunit is not a completion; the stub is.

If the completion of a declaration is also a declaration, then that declaration might have a completion, too. For example, a limited private type can be completed with a task type, which can then be completed with a task body. This is not a violation of the "at most one completion" rule. 

A type is completely defined at a place that is after its full type definition (if it has one) and after all of its subcomponent types are completely defined. A type shall be completely defined before it is frozen (see 13.14 and 7.3). 

Reason: Index types are always completely defined - no need to mention them. There is no way for a completely defined type to depend on the value of a (still) deferred constant. 

NOTE 1   {AI05-0229-1} Completions are in principle allowed for any kind of explicit declaration. However, for some kinds of declaration, the only allowed completion is an implementation-defined pragma, and implementations are not required to have any such pragmas. 

This paragraph was deleted.{AI05-0229-1} 

NOTE 2   There are rules that prevent premature uses of declarations that have a corresponding completion. The Elaboration_Checks of 3.11 prevent such uses at run time for subprograms, protected operations, tasks, and generic units. The rules of 13.14, "Freezing Rules" prevent, at compile time, premature uses of other entities such as private types and deferred constants. 


#### Wording Changes from Ada 83

This subclause is new. It is intended to cover all kinds of completions of declarations, be they a body for a spec, a full type for an incomplete or private type, a full constant declaration for a deferred constant declaration, or a [pragma](./AA-2.8#S0019) Import for any kind of entity. 


#### Wording Changes from Ada 95

{8652/0014} {AI95-00064-01} Corrigendum: Added a definition of body, which is different than [body](./AA-3.11#S0089) or body. 


#### Wording Changes from Ada 2005

{AI95-0177-1} Added null procedures and expression functions that are completions to the definition of body. 

