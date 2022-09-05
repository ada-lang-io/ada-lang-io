---
sidebar_position:  17
---

# 3.1  Declarations

{AI12-0373-1} The language defines several kinds of named entities that are declared by declarations. The entity's name is defined by the declaration, usually by a [defining_identifier](./AA-3.1#S0022), but sometimes by a [defining_character_literal](./AA-3.5#S0040) or [defining_operator_symbol](./AA-6.1#S0203). There are also entities that are not directly declared; some of these are elements of other entities, or are allocated dynamically. Such entities can be denoted using [indexed_component](./AA-4.1#S0096), [selected_component](./AA-4.1#S0098), or dereference [name](./AA-4.1#S0091)s (see 4.1).

Discussion: {AI12-0373-1} Some entities are always anonymous. For instance, a type is never named (the name represents the first subtype). We don't mention those here as this paragraph is about named entities. 

There are several forms of declaration. A [basic_declaration](./AA-3.1#S0021) is a form of declaration defined as follows. 


#### Syntax

{AI95-00348-01} {AI05-0177-1} basic_declaration<a id="S0021"></a> ::= 
     [type_declaration](./AA-3.2#S0023)	| [subtype_declaration](./AA-3.2#S0026)
   | [object_declaration](./AA-3.3#S0032)	| [number_declaration](./AA-3.3#S0034)
   | [subprogram_declaration](./AA-6.1#S0195)	| [abstract_subprogram_declaration](./AA-3.9#S0076)
   | [null_procedure_declaration](./AA-6.7#S0227)	| [expression_function_declaration](./AA-6.8#S0228)
   | [package_declaration](./AA-7.1#S0229)	| [renaming_declaration](./AA-8.5#S0238)
   | [exception_declaration](./AA-11.1#S0303)	| [generic_declaration](./AA-12.1#S0310)
   | [generic_instantiation](./AA-12.3#S0315)

defining_identifier<a id="S0022"></a> ::= [identifier](./AA-2.3#S0002)


#### Static Semantics

A declaration is a language construct that associates a name with (a view of) an entity. A declaration may appear explicitly in the program text (an explicit declaration), or may be supposed to occur at a given place in the text as a consequence of the semantics of another construct (an implicit declaration). 

Discussion: An implicit declaration generally declares a predefined or inherited operation associated with the definition of a type. This term is used primarily when allowing explicit declarations to override implicit declarations, as part of a type declaration. 

Version=[5],Kind=(AddedNormal),Group=[C],Term=[declaration], Def=[a language construct that associates a name with (a view of) an entity], Note1=[A declaration can appear explicitly in the program text (an explicit declaration), or can be supposed to occur at a given place in the text as a consequence of the semantics of another construct (an implicit declaration).]

{AI95-00318-02} {AI05-0255-1} {AI05-0277-1} {AI12-0061-1} {AI12-0308-1} Each of the following is defined to be a declaration: any [basic_declaration](./AA-3.1#S0021); an [enumeration_literal_specification](./AA-3.5#S0039); a [discriminant_specification](./AA-3.7#S0062); a [component_declaration](./AA-3.8#S0070); a [defining_identifier](./AA-3.1#S0022) of an [iterated_component_association](./AA-4.3#S0119); a [loop_parameter_specification](./AA-5.5#S0181); a [defining_identifier](./AA-3.1#S0022) of a [chunk_specification](./AA-5.5#S0180); an [iterator_specification](./AA-5.5#S0183); a [defining_identifier](./AA-3.1#S0022) of an [iterator_parameter_specification](./AA-5.5#S0186); a [parameter_specification](./AA-6.1#S0207); a [subprogram_body](./AA-6.3#S0216); an [extended_return_object_declaration](./AA-6.5#S0224); an [entry_declaration](./AA-9.5#S0257); an [entry_index_specification](./AA-9.5#S0263); a [choice_parameter_specification](./AA-11.2#S0306); a [generic_formal_parameter_declaration](./AA-12.1#S0314). 

Discussion: This list (when [basic_declaration](./AA-3.1#S0021) is expanded out) contains all syntactic categories that end in "_declaration" or "_specification", except for program unit _specifications. Moreover, it contains [subprogram_body](./AA-6.3#S0216). A [subprogram_body](./AA-6.3#S0216) is a declaration, whether or not it completes a previous declaration. This is a bit strange, [subprogram_body](./AA-6.3#S0216) is not part of the syntax of [basic_declaration](./AA-3.1#S0021) or [library_unit_declaration](./AA-10.1#S0288). A renaming-as-body is considered a declaration. An [accept_statement](./AA-9.5#S0258) is not considered a declaration. Completions are sometimes declarations, and sometimes not. 

All declarations contain a definition for a view of an entity. A view consists of an identification of the entity (the entity of the view), plus view-specific characteristics that affect the use of the entity through that view (such as mode of access to an object, formal parameter names and defaults for a subprogram, or visibility to components of a type). In most cases, a declaration also contains the definition for the entity itself (a [renaming_declaration](./AA-8.5#S0238) is an example of a declaration that does not define a new entity, but instead defines a view of an existing entity (see 8.5)).

Glossary entry: A view of an entity reveals some or all of the properties of the entity. A single entity may have multiple views.

Version=[5],Kind=(Added),Group=[T],Term=[view of an entity], Def=[a representation of an entity that reveals some or all of the properties of the entity], Note1=[A single entity can have multiple views.]

Discussion: Most declarations define a view (of some entity) whose view-specific characteristics are unchanging for the life of the view. However, subtypes are somewhat unusual in that they inherit characteristics from whatever view of their type is currently visible. Hence, a subtype is not a view of a type; it is more of an indirect reference. By contrast, a private type provides a single, unchanging (partial) view of its full type. 

{AI05-0080-1} When it is clear from context, the term object is used in place of view of an object. Similarly, the terms type and subtype are used in place of view of a type and view of a subtype, respectively.

Discussion: Rules interpreted at compile time generally refer to views of entities, rather than the entities themselves. This is necessary to preserve privacy; characteristics that are not visible should not be used in compile-time rules. Thus, Static Semantics and Legality Rules generally implicitly have "view of". Legality Rules that need to look into the private part are the exception to this interpretation.

On the other hand, run-time rules can work either way, so "view of" should not be assumed in Dynamic Semantics rules.

{AI12-0191-1} For example, a reference to the components of an object in a rule that is interpreted at compile time would not apply to components that are not visible. On the other hand, a reference to the components of an object in a dynamic semantics rule would apply to all components of the object, visible or not, including (for tagged objects) components which are not components of the nominal type of the object (see 3.9.1). Other terms, such as "subcomponent" and "part", are interpreted analogously. 

For each declaration, the language rules define a certain region of text called the scope of the declaration (see 8.2). Most declarations associate an [identifier](./AA-2.3#S0002) with a declared entity. Within its scope, and only there, there are places where it is possible to use the [identifier](./AA-2.3#S0002) to refer to the declaration, the view it defines, and the associated entity; these places are defined by the visibility rules (see 8.3). At such places the [identifier](./AA-2.3#S0002) is said to be a name of the entity (the [direct_name](./AA-4.1#S0092) or [selector_name](./AA-4.1#S0099)); the name is said to denote the declaration, the view, and the associated entity (see 8.6). The declaration is said to declare the name, the view, and in most cases, the entity itself.

As an alternative to an [identifier](./AA-2.3#S0002), an enumeration literal can be declared with a [character_literal](./AA-2.5#S0015) as its name (see 3.5.1), and a function can be declared with an [operator_symbol](./AA-6.1#S0202) as its name (see 6.1).

The syntax rules use the terms [defining_identifier](./AA-3.1#S0022), [defining_character_literal](./AA-3.5#S0040), and [defining_operator_symbol](./AA-6.1#S0203) for the defining occurrence of a name; these are collectively called defining names. The terms [direct_name](./AA-4.1#S0092) and [selector_name](./AA-4.1#S0099) are used for usage occurrences of [identifier](./AA-2.3#S0002)s, [character_literal](./AA-2.5#S0015)s, and [operator_symbol](./AA-6.1#S0202)s. These are collectively called usage names. 

To be honest: The terms [identifier](./AA-2.3#S0002), [character_literal](./AA-2.5#S0015), and [operator_symbol](./AA-6.1#S0202) are used directly in contexts where the normal visibility rules do not apply (such as the [identifier](./AA-2.3#S0002) that appears after the end of a [task_body](./AA-9.1#S0248)). Analogous conventions apply to the use of [designator](./AA-6.1#S0199), which is the collective term for [identifier](./AA-2.3#S0002) and [operator_symbol](./AA-6.1#S0202). 


#### Dynamic Semantics

The process by which a construct achieves its run-time effect is called execution. This process is also called elaboration for declarations and evaluation for expressions. One of the terms execution, elaboration, or evaluation is defined by this Reference Manual for each construct that has a run-time effect. 

Glossary entry: The process by which a construct achieves its run-time effect is called execution. Execution of a declaration is also called elaboration. Execution of an expression is also called evaluation.

Version=[5],Kind=(Added),Group=[R],Term=[execution], Def=[the process by which a construct achieves its run-time effect], Note1=[Execution of a declaration is also called elaboration. Execution of an expression is also called evaluation.] 

To be honest: The term elaboration is also used for the execution of certain constructs that are not declarations, and the term evaluation is used for the execution of certain constructs that are not expressions. For example, [subtype_indication](./AA-3.2#S0027)s are elaborated, and [range](./AA-3.5#S0037)s are evaluated.

For bodies, execution and elaboration are both explicitly defined. When we refer specifically to the execution of a body, we mean the explicit definition of execution for that kind of body, not its elaboration. 

Discussion: Technically, "the execution of a declaration" and "the elaboration of a declaration" are synonymous. We use the term "elaboration" of a construct when we know the construct is elaborable. When we are talking about more arbitrary constructs, we use the term "execution". For example, we use the term "erroneous execution", to refer to any erroneous execution, including erroneous elaboration or evaluation.

When we explicitly define evaluation or elaboration for a construct, we are implicitly defining execution of that construct.

We also use the term "execution" for things like [statement](./AA-5.1#S0167)s, which are executable, but neither elaborable nor evaluable. We considered using the term "execution" only for nonelaborable, nonevaluable constructs, and defining the term "action" to mean what we have defined "execution" to mean. We rejected this idea because we thought three terms that mean the same thing was enough - four would be overkill. Thus, the term "action" is used only informally in the standard (except where it is defined as part of a larger term, such as "protected action"). 

Glossary entry: The process by which a declaration achieves its run-time effect is called elaboration. Elaboration is one of the forms of execution.

Glossary entry: The process by which an expression achieves its run-time effect is called evaluation. Evaluation is one of the forms of execution.

Version=[5],Kind=(Added),Group=[R],Term=[elaboration], Def=[the process by which a declaration achieves its run-time effect], Note1=[Elaboration is one of the forms of execution.] Version=[5],Kind=(Added),Group=[R],Term=[evaluation], Def=[the process by which an expression achieves its run-time effect], Note1=[Evaluation is one of the forms of execution.] 

To be honest: A construct is elaborable if elaboration is defined for it. A construct is evaluable if evaluation is defined for it. A construct is executable if execution is defined for it. 

Discussion: Don't confuse "elaborable" with "preelaborable" (defined in 10.2.1).

{AI95-00114-01} Evaluation of an evaluable construct produces a result that is either a value, a denotation, or a range. The following are evaluable: expression; [name](./AA-4.1#S0091) [prefix](./AA-4.1#S0093); [range](./AA-3.5#S0037); [entry_index_specification](./AA-9.5#S0263); and possibly [discrete_range](./AA-3.6#S0058). The last one is curious - RM83 uses the term "evaluation of a [discrete_range](./AA-3.6#S0058)", but never defines it. One might presume that the evaluation of a [discrete_range](./AA-3.6#S0058) consists of the evaluation of the [range](./AA-3.5#S0037) or the [subtype_indication](./AA-3.2#S0027), depending on what it is. But [subtype_indication](./AA-3.2#S0027)s are not evaluated; they are elaborated.

Intuitively, an executable construct is one that has a defined run-time effect (which may be null). Since execution includes elaboration and evaluation as special cases, all elaborable and all evaluable constructs are also executable. Hence, most constructs in Ada are executable. An important exception is that the constructs inside a generic unit are not executable directly, but rather are used as a template for (generally) executable constructs in instances of the generic. 

NOTE   At compile time, the declaration of an entity declares the entity. At run time, the elaboration of the declaration creates the entity. 

Ramification: Syntactic categories for declarations are named either entity_declaration (if they include a trailing semicolon) or entity_specification (if not).

The various kinds of named entities that can be declared are as follows: an object (including components and parameters), a named number, a type (the name always refers to its first subtype), a subtype, a subprogram (including enumeration literals and operators), a single entry, an entry family, a package, a protected or task unit (which corresponds to either a type or a single object), an exception, a generic unit, a label, and the name of a statement.

Identifiers are also associated with names of pragmas, arguments to pragmas, and with attributes, but these are not user-definable. 


#### Wording Changes from Ada 83

The syntax rule for [defining_identifier](./AA-3.1#S0022) is new. It is used for the defining occurrence of an [identifier](./AA-2.3#S0002). Usage occurrences use the [direct_name](./AA-4.1#S0092) or [selector_name](./AA-4.1#S0099) syntactic categories. Each occurrence of an [identifier](./AA-2.3#S0002) (or simple_name), [character_literal](./AA-2.5#S0015), or [operator_symbol](./AA-6.1#S0202) in the Ada 83 syntax rules is handled as follows in Ada 95: 

It becomes a [defining_identifier](./AA-3.1#S0022), [defining_character_literal](./AA-3.5#S0040), or [defining_operator_symbol](./AA-6.1#S0203) (or some syntactic category composed of these), to indicate a defining occurrence;

{AI05-0299-1} It becomes a [direct_name](./AA-4.1#S0092), in usage occurrences where the usage is required (in Clause 8) to be directly visible;

{AI05-0299-1} It becomes a [selector_name](./AA-4.1#S0099), in usage occurrences where the usage is required (in Clause 8) to be visible but not necessarily directly visible;

It remains an [identifier](./AA-2.3#S0002), [character_literal](./AA-2.5#S0015), or [operator_symbol](./AA-6.1#S0202), in cases where the visibility rules do not apply (such as the [designator](./AA-6.1#S0199) that appears after the end of a [subprogram_body](./AA-6.3#S0216)). 

For declarations that come in "two parts" (program unit declaration plus body, private or incomplete type plus full type, deferred constant plus full constant), we consider both to be defining occurrences. Thus, for example, the syntax for [package_body](./AA-7.2#S0231) uses [defining_identifier](./AA-3.1#S0022) after the reserved word body, as opposed to [direct_name](./AA-4.1#S0092).

The defining occurrence of a statement name is in its implicit declaration, not where it appears in the program text. Considering the statement name itself to be the defining occurrence would complicate the visibility rules.

The phrase "visible by selection" is not used in Ada 95. It is subsumed by simply "visible" and the Name Resolution Rules for [selector_name](./AA-4.1#S0099)s.

{AI05-0299-1} (Note that in Ada 95, a declaration is visible at all places where one could have used a [selector_name](./AA-4.1#S0099), not just at places where a [selector_name](./AA-4.1#S0099) was actually used. Thus, the places where a declaration is directly visible are a subset of the places where it is visible. See Clause 8 for details.)

We use the term "declaration" to cover _specifications that declare (views of) objects, such as [parameter_specification](./AA-6.1#S0207)s. In Ada 83, these are referred to as a "form of declaration", but it is not entirely clear that they are considered simply "declarations".

{AI05-0299-1} RM83 contains an incomplete definition of "elaborated" in this subclause: it defines "elaborated" for declarations, [declarative_part](./AA-3.11#S0086)s, [declarative_item](./AA-3.11#S0087)s and [compilation_unit](./AA-10.1#S0286)s, but "elaboration" is defined elsewhere for various other constructs. To make matters worse, Ada 95 has a different set of elaborable constructs. Instead of correcting the list, it is more maintainable to refer to the term "elaborable," which is defined in a distributed manner.

RM83 uses the term "has no other effect" to describe an elaboration that doesn't do anything except change the state from not-yet-elaborated to elaborated. This was a confusing wording, because the answer to "other than what?" was to be found many pages away. In Ada 95, we change this wording to "has no effect" (for things that truly do nothing at run time), and "has no effect other than to establish that so-and-so can happen without failing the Elaboration_Check" (for things where it matters).

We make it clearer that the term "execution" covers elaboration and evaluation as special cases. This was implied in RM83. For example, "erroneous execution" can include any execution, and RM83-9.4(3) has, "The task designated by any other task object depends on the master whose execution creates the task object;" the elaboration of the master's [declarative_part](./AA-3.11#S0086) is doing the task creation. 


#### Wording Changes from Ada 95

{AI95-00318-02} Added [extended_return_statement](./AA-6.5#S0225) to the list of declarations.

{AI95-00348-01} Added null procedures (see 6.7) to the syntax. 


#### Wording Changes from Ada 2005

{AI05-0177-1} Added expression functions (see 6.8) to the syntax. 

