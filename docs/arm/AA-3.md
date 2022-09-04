---
sidebar_position:  4
---

# 3 Declarations and Types

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
This section describes the types in the language and the rules for declaring constants, variables, and named numbers. 


## 3.1  Declarations

The language defines several kinds of named entities that are declared by declarations. The entity's name is defined by the declaration, usually by a [defining_identifier](S0019), but sometimes by a [defining_character_literal](S0037) or [defining_operator_symbol](S0148).

There are several forms of declaration. A [basic_declaration](S0018) is a form of declaration defined as follows. 


#### Syntax

basic_declaration ::= 
     [type_declaration](S0020)	| [subtype_declaration](S0023)
   | [object_declaration](S0029)	| [number_declaration](S0031)
   | [subprogram_declaration](S0141)	| [abstract_subprogram_declaration](S0142)
   | [package_declaration](S0161)	| [renaming_declaration](S0169)
   | [exception_declaration](S0230)	| [generic_declaration](S0236)
   | [generic_instantiation](S0241)

defining_identifier ::= [identifier](S0002)


#### Static Semantics

A declaration is a language construct that associates a name with (a view of) an entity. A declaration may appear explicitly in the program text (an explicit declaration), or may be supposed to occur at a given place in the text as a consequence of the semantics of another construct (an implicit declaration). 

Discussion: An implicit declaration generally declares a predefined or inherited operation associated with the definition of a type. This term is used primarily when allowing explicit declarations to override implicit declarations, as part of a type declaration. 

Version=[5],Kind=(AddedNormal),Group=[C],Term=[declaration], Def=[a language construct that associates a name with (a view of) an entity], Note1=[A declaration can appear explicitly in the program text (an explicit declaration), or can be supposed to occur at a given place in the text as a consequence of the semantics of another construct (an implicit declaration).]

Each of the following is defined to be a declaration: any [basic_declaration](S0018); an [enumeration_literal_specification](S0036); a [discriminant_specification](S0059); a [component_declaration](S0067); a [loop_parameter_specification](S0137); a [parameter_specification](S0152); a [subprogram_body](S0154); an [entry_declaration](S0187); an [entry_index_specification](S0193); a [choice_parameter_specification](S0233); a [generic_formal_parameter_declaration](S0240). 

Discussion: This list (when [basic_declaration](S0018) is expanded out) contains all syntactic categories that end in "_declaration" or "_specification", except for program unit _specifications. Moreover, it contains [subprogram_body](S0154). A [subprogram_body](S0154) is a declaration, whether or not it completes a previous declaration. This is a bit strange, [subprogram_body](S0154) is not part of the syntax of [basic_declaration](S0018) or [library_unit_declaration](S0217). A renaming-as-body is considered a declaration. An [accept_statement](S0188) is not considered a declaration. Completions are sometimes declarations, and sometimes not. 

All declarations contain a definition for a view of an entity. A view consists of an identification of the entity (the entity of the view), plus view-specific characteristics that affect the use of the entity through that view (such as mode of access to an object, formal parameter names and defaults for a subprogram, or visibility to components of a type). In most cases, a declaration also contains the definition for the entity itself (a [renaming_declaration](S0169) is an example of a declaration that does not define a new entity, but instead defines a view of an existing entity (see 8.5)).

Glossary entry: (See Definition.)

Version=[5],Kind=(Added),Group=[T],Term=[view of an entity], Def=[a representation of an entity that reveals some or all of the properties of the entity], Note1=[A single entity can have multiple views.]

Discussion: Most declarations define a view (of some entity) whose view-specific characteristics are unchanging for the life of the view. However, subtypes are somewhat unusual in that they inherit characteristics from whatever view of their type is currently visible. Hence, a subtype is not a view of a type; it is more of an indirect reference. By contrast, a private type provides a single, unchanging (partial) view of its full type. 

This paragraph was deleted.All declarations contain a definition for a view of an entity. A view consists of an identification of the entity (the entity of the view), plus view-specific characteristics that affect the use of the entity through that view (such as mode of access to an object, formal parameter names and defaults for a subprogram, or visibility to components of a type). In most cases, a declaration also contains the definition for the entity itself (a [renaming_declaration](S0169) is an example of a declaration that does not define a new entity, but instead defines a view of an existing entity (see 8.5)).

For each declaration, the language rules define a certain region of text called the scope of the declaration (see 8.2). Most declarations associate an [identifier](S0002) with a declared entity. Within its scope, and only there, there are places where it is possible to use the [identifier](S0002) to refer to the declaration, the view it defines, and the associated entity; these places are defined by the visibility rules (see 8.3). At such places the [identifier](S0002) is said to be a name of the entity (the [direct_name](S0085) or [selector_name](S0092)); the name is said to denote the declaration, the view, and the associated entity (see 8.6). The declaration is said to declare the name, the view, and in most cases, the entity itself.

As an alternative to an [identifier](S0002), an enumeration literal can be declared with a [character_literal](S0012) as its name (see 3.5.1), and a function can be declared with an [operator_symbol](S0147) as its name (see 6.1).

The syntax rules use the terms [defining_identifier](S0019), [defining_character_literal](S0037), and [defining_operator_symbol](S0148) for the defining occurrence of a name; these are collectively called defining names. The terms [direct_name](S0085) and [selector_name](S0092) are used for usage occurrences of [identifier](S0002)s, [character_literal](S0012)s, and [operator_symbol](S0147)s. These are collectively called usage names. 

To be honest: The terms [identifier](S0002), [character_literal](S0012), and [operator_symbol](S0147) are used directly in contexts where the normal visibility rules do not apply (such as the [identifier](S0002) that appears after the end of a [task_body](S0179)). Analogous conventions apply to the use of [designator](S0144), which is the collective term for [identifier](S0002) and [operator_symbol](S0147). 


#### Dynamic Semantics

The process by which a construct achieves its run-time effect is called execution. This process is also called elaboration for declarations and evaluation for expressions. One of the terms execution, elaboration, or evaluation is defined by this Reference Manual for each construct that has a run-time effect. 

Glossary entry: The process by which a construct achieves its run-time effect is called execution. Execution of a declaration is also called elaboration. Execution of an expression is also called evaluation.

Version=[5],Kind=(Added),Group=[R],Term=[execution], Def=[the process by which a construct achieves its run-time effect], Note1=[Execution of a declaration is also called elaboration. Execution of an expression is also called evaluation.] 

To be honest: The term elaboration is also used for the execution of certain constructs that are not declarations, and the term evaluation is used for the execution of certain constructs that are not expressions. For example, [subtype_indication](S0024)s are elaborated, and [range](S0034)s are evaluated.

For bodies, execution and elaboration are both explicitly defined. When we refer specifically to the execution of a body, we mean the explicit definition of execution for that kind of body, not its elaboration. 

Discussion: Technically, "the execution of a declaration" and "the elaboration of a declaration" are synonymous. We use the term "elaboration" of a construct when we know the construct is elaborable. When we are talking about more arbitrary constructs, we use the term "execution". For example, we use the term "erroneous execution", to refer to any erroneous execution, including erroneous elaboration or evaluation.

When we explicitly define evaluation or elaboration for a construct, we are implicitly defining execution of that construct.

We also use the term "execution" for things like [statement](S0124)s, which are executable, but neither elaborable nor evaluable. We considered using the term "execution" only for nonelaborable, nonevaluable constructs, and defining the term "action" to mean what we have defined "execution" to mean. We rejected this idea because we thought three terms that mean the same thing was enough - four would be overkill. Thus, the term "action" is used only informally in the standard (except where it is defined as part of a larger term, such as "protected action"). 

Version=[5],Kind=(Added),Group=[R],Term=[elaboration], Def=[the process by which a declaration achieves its run-time effect], Note1=[Elaboration is one of the forms of execution.] Version=[5],Kind=(Added),Group=[R],Term=[evaluation], Def=[the process by which an expression achieves its run-time effect], Note1=[Evaluation is one of the forms of execution.] 

To be honest: A construct is elaborable if elaboration is defined for it. A construct is evaluable if evaluation is defined for it. A construct is executable if execution is defined for it. 

Discussion: Don't confuse "elaborable" with "preelaborable" (defined in 10.2.1).

Evaluation of an evaluable construct produces a result that is either a value, a denotation, or a range. The following are evaluable: expression; [name](S0084) [prefix](S0086); [range](S0034); entry_list_iterator; and possibly [discrete_range](S0055). The last one is curious - RM83 uses the term "evaluation of a [discrete_range](S0055)", but never defines it. One might presume that the evaluation of a [discrete_range](S0055) consists of the evaluation of the [range](S0034) or the [subtype_indication](S0024), depending on what it is. But [subtype_indication](S0024)s are not evaluated; they are elaborated.

Intuitively, an executable construct is one that has a defined run-time effect (which may be null). Since execution includes elaboration and evaluation as special cases, all elaborable and all evaluable constructs are also executable. Hence, most constructs in Ada are executable. An important exception is that the constructs inside a generic unit are not executable directly, but rather are used as a template for (generally) executable constructs in instances of the generic. 

NOTE   At compile time, the declaration of an entity declares the entity. At run time, the elaboration of the declaration creates the entity. 

Ramification: Syntactic categories for declarations are named either entity_declaration (if they include a trailing semicolon) or entity_specification (if not).

The various kinds of named entities that can be declared are as follows: an object (including components and parameters), a named number, a type (the name always refers to its first subtype), a subtype, a subprogram (including enumeration literals and operators), a single entry, an entry family, a package, a protected or task unit (which corresponds to either a type or a single object), an exception, a generic unit, a label, and the name of a statement.

Identifiers are also associated with names of pragmas, arguments to pragmas, and with attributes, but these are not user-definable. 


#### Wording Changes from Ada 83

The syntax rule for [defining_identifier](S0019) is new. It is used for the defining occurrence of an [identifier](S0002). Usage occurrences use the [direct_name](S0085) or [selector_name](S0092) syntactic categories. Each occurrence of an [identifier](S0002) (or simple_name), [character_literal](S0012), or [operator_symbol](S0147) in the Ada 83 syntax rules is handled as follows in Ada 95: 

It becomes a [defining_identifier](S0019), [defining_character_literal](S0037), or [defining_operator_symbol](S0148) (or some syntactic category composed of these), to indicate a defining occurrence;

It becomes a [direct_name](S0085), in usage occurrences where the usage is required (in Section 8) to be directly visible;

It becomes a [selector_name](S0092), in usage occurrences where the usage is required (in Section 8) to be visible but not necessarily directly visible;

It remains an [identifier](S0002), [character_literal](S0012), or [operator_symbol](S0147), in cases where the visibility rules do not apply (such as the [designator](S0144) that appears after the end of a [subprogram_body](S0154)). 

For declarations that come in "two parts" (program unit declaration plus body, private or incomplete type plus full type, deferred constant plus full constant), we consider both to be defining occurrences. Thus, for example, the syntax for [package_body](S0163) uses [defining_identifier](S0019) after the reserved word body, as opposed to [direct_name](S0085).

The defining occurrence of a statement name is in its implicit declaration, not where it appears in the program text. Considering the statement name itself to be the defining occurrence would complicate the visibility rules.

The phrase "visible by selection" is not used in Ada 95. It is subsumed by simply "visible" and the Name Resolution Rules for [selector_name](S0092)s.

(Note that in Ada 95, a declaration is visible at all places where one could have used a [selector_name](S0092), not just at places where a [selector_name](S0092) was actually used. Thus, the places where a declaration is directly visible are a subset of the places where it is visible. See Section 8 for details.)

We use the term "declaration" to cover _specifications that declare (views of) objects, such as [parameter_specification](S0152)s. In Ada 83, these are referred to as a "form of declaration", but it is not entirely clear that they are considered simply "declarations".

RM83 contains an incomplete definition of "elaborated" in this clause: it defines "elaborated" for declarations, [declarative_part](S0079)s, [declarative_item](S0080)s and [compilation_unit](S0215)s, but "elaboration" is defined elsewhere for various other constructs. To make matters worse, Ada 95 has a different set of elaborable constructs. Instead of correcting the list, it is more maintainable to refer to the term "elaborable," which is defined in a distributed manner.

RM83 uses the term "has no other effect" to describe an elaboration that doesn't do anything except change the state from not-yet-elaborated to elaborated. This was a confusing wording, because the answer to "other than what?" was to be found many pages away. In Ada 95, we change this wording to "has no effect" (for things that truly do nothing at run time), and "has no effect other than to establish that so-and-so can happen without failing the Elaboration_Check" (for things where it matters).

We make it clearer that the term "execution" covers elaboration and evaluation as special cases. This was implied in RM83. For example, "erroneous execution" can include any execution, and RM83-9.4(3) has, "The task designated by any other task object depends on the master whose execution creates the task object;" the elaboration of the master's [declarative_part](S0079) is doing the task creation. 


## 3.2  Types and Subtypes


#### Static Semantics

A type is characterized by a set of values, and a set of primitive operations which implement the fundamental aspects of its semantics. An object of a given type is a run-time entity that contains (has) a value of the type. 

Glossary entry: Each object has a type. A type has an associated set of values, and a set of primitive operations which implement the fundamental aspects of its semantics. Types are grouped into classes. The types of a given class share a set of primitive operations.  Classes are closed under derivation; that is, if a type is in a class, then all of its derivatives are in that class.

Glossary entry: A subtype is a type together with a constraint, which constrains the values of the subtype to satisfy a certain condition. The values of a subtype are a subset of the values of its type.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[type], Def=[a defining characteristic of each object and expression of the language, with an associated set of values, and a set of primitive operations that implement the fundamental aspects of its semantics], Note1=[Types are grouped into categories. Most language-defined categories of types are also classes of types.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[subtype], Def=[a type together with optional constraints, null exclusions, and predicates, which constrain the values of the type to the subset that satisfies the implied conditions]

Types are grouped into classes of types, reflecting the similarity of their values and primitive operations. There exist several language-defined classes of types (see NOTES below). Elementary types are those whose values are logically indivisible; composite types are those whose values are composed of component values. 

Glossary entry: A class is a set of types that is closed under derivation, which means that if a given type is in the class, then all types derived from that type are also in the class. The set of types of a class share common properties, such as their primitive operations.

Glossary entry: An elementary type does not have components.

Glossary entry: A composite type has components.

Glossary entry: A scalar type is either a discrete type or a real type.

Glossary entry: An access type has values that designate aliased objects. Access types correspond to "pointer types" or "reference types" in some other languages.

Glossary entry: A discrete type is either an integer type or an enumeration type. Discrete types may be used, for example, in [case_statement](S0133)s and as array indices.

Glossary entry: A real type has values that are approximations of the real numbers. Floating point and fixed point types are real types.

Glossary entry: Integer types comprise the signed integer types and the modular types. A signed integer type has a base range that includes both positive and negative numbers, and has operations that may raise an exception when the result is outside the base range. A modular type has a base range whose lower bound is zero, and has operations with "wraparound" semantics. Modular types subsume what are called "unsigned types" in some other languages.

Glossary entry: An enumeration type is defined by an enumeration of its values, which may be named by identifiers or character literals.

Glossary entry: A character type is an enumeration type whose values include characters.

Glossary entry: A record type is a composite type consisting of zero or more named components, possibly of different types.

Glossary entry: A record extension is a type that extends another type by adding additional components.

Glossary entry: An array type is a composite type whose components are all of the same type. Components are selected by indexing.

Glossary entry: A task type is a composite type whose values are tasks, which are active entities that may execute concurrently with other tasks. The top-level task of a partition is called the environment task.

Glossary entry: A protected type is a composite type whose components are protected from concurrent access by multiple tasks.

Glossary entry: A private type is a partial view of a type whose full view is hidden from its clients.

Glossary entry: A private extension is like a record extension, except that the components of the extension part are hidden from its clients.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[class of types], Def=[a set of types that is closed under derivation, which means that if a given type is in the class, then all types derived from that type are also in the class], Note1=[The set of types of a class share common properties, such as their primitive operations.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[category of types], Def=[a set of types with one or more common properties, such as primitive operations], Note1=[A category of types that is closed under derivation is also known as a class.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[elementary type], Def=[a type that does not have components] Version=[5],Kind=(AddedNormal),Group=[T],Term=[composite type], Def=[a type with components, such as an array or record] Version=[5],Kind=(AddedNormal),Group=[T],Term=[scalar type], Def=[either a discrete type or a real type] Version=[5],Kind=(AddedNormal),Group=[T],Term=[access type], Def=[a type that has values that designate aliased objects], Note1=[Access types correspond to "pointer types" or "reference types" in some other languages.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[discrete type], Def=[a type that is either an integer type or an enumeration type] Version=[5],Kind=(AddedNormal),Group=[T],Term=[real type], Def=[a type that has values that are approximations of the real numbers], Note1=[Floating point and fixed point types are real types.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[integer type], Def=[a type that represents signed or modular integers], Note1=[A signed integer type has a base range that includes both positive and negative numbers, and has operations that can raise an exception when the result is outside the base range. A modular type has a base range whose lower bound is zero, and has operations with "wraparound" semantics. Modular types subsume what are called "unsigned types" in some other languages.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[enumeration type], Def=[a type defined by an enumeration of its values, which can be denoted by identifiers or character literals] Version=[5],Kind=(AddedNormal),Group=[T],Term=[character type], Def=[an enumeration type whose values include characters] Version=[5],Kind=(AddedNormal),Group=[T],Term=[record type], Def=[a composite type consisting of zero or more named components, possibly of different types] Version=[5],Kind=(AddedNormal),Group=[T],Term=[record extension], Def=[a type that extends another type optionally with additional components] Version=[5],Kind=(AddedNormal),Group=[T],Term=[array type], Def=[a composite type whose components are all of the same type] Version=[5],Kind=(AddedNormal),Group=[T],Term=[task type], Def=[a composite type used to represent active entities which execute concurrently and that can communicate via queued task entries], Note1=[The top-level task of a partition is called the environment task.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[protected type], Def=[a composite type whose components are accessible only through one of its protected operations, which synchronize concurrent access by multiple tasks] Version=[5],Kind=(AddedNormal),Group=[T],Term=[private type], Def=[a view of a type that reveals only some of its properties], Note1=[The remaining properties are provided by the full view given elsewhere. Private types can be used for defining abstractions that hide unnecessary details from their clients.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[private extension], Def=[a type that extends another type, with the additional properties hidden from its clients] Version=[5],Kind=(AddedNormal),Group=[T],Term=[incomplete type], Def=[a view of a type that reveals only a few of its properties], Note1=[The remaining properties are provided by the full view given elsewhere.], Note2=[Incomplete types can be used for defining recursive data structures.]

The elementary types are the scalar types (discrete and real) and the access types (whose values provide access to objects or subprograms). Discrete types are either integer types or are defined by enumeration of their values (enumeration types). Real types are either floating point types or fixed point types.

The composite types are the record types, record extensions, array types, task types, and protected types. A private type or private extension represents a partial view (see 7.3) of a type, providing support for data abstraction. A partial view is a composite type. 

To be honest: The set of all record types do not form a class (because tagged record types can have private extensions), though the set of untagged record types do. In any case, what record types had in common in Ada 83 (component selection) is now a property of the composite class, since all composite types (other than array types) can have discriminants. Similarly, the set of all private types do not form a class (because tagged private types can have record extensions), though the set of untagged private types do. Nevertheless, the set of untagged private types is not particularly "interesting" - more interesting is the set of all nonlimited types, since that is what a generic formal (nonlimited) private type matches. 

Certain composite types (and partial views thereof) have special components called discriminants whose values affect the presence, constraints, or initialization of other components. Discriminants can be thought of as parameters of the type.

The term subcomponent is used in this Reference Manual in place of the term component to indicate either a component, or a component of another subcomponent. Where other subcomponents are excluded, the term component is used instead. Similarly, a part of an object or value is used to mean the whole object or value, or any set of its subcomponents. 

Discussion: The definition of "part" here is designed to simplify rules elsewhere. By design, the intuitive meaning of "part" will convey the correct result to the casual reader, while this formalistic definition will answer the concern of the compiler-writer.

We use the term "part" when talking about the parent part, ancestor part, or extension part of a type extension. In contexts such as these, the part might represent an empty set of subcomponents (e.g. in a null record extension, or a nonnull extension of a null record). We also use "part" when specifying rules such as those that apply to an object with a "controlled part" meaning that it applies if the object as a whole is controlled, or any subcomponent is. 

The set of possible values for an object of a given type can be subjected to a condition that is called a constraint (the case of a null constraint that specifies no restriction is also included)[; the rules for which values satisfy a given kind of constraint are given in 3.5 for [range_constraint](S0033)s, 3.6.1 for [index_constraint](S0054)s, and 3.7.1 for [discriminant_constraint](S0061)s].

A subtype of a given type is a combination of the type, a constraint on values of the type, and certain attributes specific to the subtype. The given type is called the type of the subtype. Similarly, the associated constraint is called the constraint of the subtype. The set of values of a subtype consists of the values of its type that satisfy its constraint. Such values belong to the subtype. 

Discussion: We make a strong distinction between a type and its subtypes. In particular, a type is not a subtype of itself. There is no constraint associated with a type (not even a null one), and type-related attributes are distinct from subtype-specific attributes. 

Discussion: We no longer use the term "base type." All types were "base types" anyway in Ada 83, so the term was redundant, and occasionally confusing. In the RM95 we say simply "the type of the subtype" instead of "the base type of the subtype." 

Ramification: The value subset for a subtype might be empty, and need not be a proper subset. 

To be honest: Any name of a class of types (such as "discrete" or "real"), or other category of types (such as "limited" or "incomplete") is also used to qualify its subtypes, as well as its objects, values, declarations, and definitions, such as an "integer type declaration" or an "integer value". In addition, if a term such as "parent subtype" or "index subtype" is defined, then the corresponding term for the type of the subtype is "parent type" or "index type". 

Discussion: We use these corresponding terms without explicitly defining them, when the meaning is obvious. 

A subtype is called an unconstrained subtype if its type has unknown discriminants, or if its type allows range, index, or discriminant constraints, but the subtype does not impose such a constraint; otherwise, the subtype is called a constrained subtype (since it has no unconstrained characteristics). 

Discussion: In an earlier version of Ada 9X, "constrained" meant "has a nonnull constraint." However, we changed to this definition since we kept having to special case composite non-array/nondiscriminated types. It also corresponds better to the (now obsolescent) attribute 'Constrained.

For scalar types, "constrained" means "has a nonnull constraint". For composite types, in implementation terms, "constrained" means that the size of all objects of the subtype is the same, assuming a typical implementation model.

Class-wide subtypes are always unconstrained. 

NOTE   Any set of types that is closed under derivation (see 3.4) can be called a "class" of types. However, only certain classes are used in the description of the rules of the language - generally those that have their own particular set of primitive operations (see 3.2.3), or that correspond to a set of types that are matched by a given kind of generic formal type (see 12.5). The following are examples of "interesting" language-defined classes: elementary, scalar, discrete, enumeration, character, boolean, integer, signed integer, modular, real, floating point, fixed point, ordinary fixed point, decimal fixed point, numeric, access, access-to-object, access-to-subprogram, composite, array, string, (untagged) record, tagged, task, protected, nonlimited. Special syntax is provided to define types in each of these classes. 

Discussion: A value is a run-time entity with a given type which can be assigned to an object of an appropriate subtype of the type. An operation is a program entity that operates on zero or more operands to produce an effect, or yield a result, or both. 

Ramification: Note that a type's class depends on the place of the reference - a private type is composite outside and possibly elementary inside. It's really the view that is elementary or composite. Note that although private types are composite, there are some properties that depend on the corresponding full view - for example, parameter passing modes, and the constraint checks that apply in various places.

Not every property of types represents a class. For example, the set of all abstract types does not form a class, because this set is not closed under derivation.

The set of limited types forms a class in the sense that it is closed under derivation, but the more interesting class, from the point of generic formal type matching, is the set of all types, limited and nonlimited, since that is what matches a generic formal "limited" private type. Note also that a limited type can "become nonlimited" under certain circumstances, which makes "limited" somewhat problematic as a class of types.

These language-defined classes are organized like this: 

all types
	elementary
		scalar
			discrete
				enumeration
					character
					boolean
					other enumeration
				integer
					signed integer
					modular integer
			real
				floating point
				fixed point
					ordinary fixed point
					decimal fixed point
		access
			access-to-object
			access-to-subprogram
	composite
		array
			string
			other array
		untagged record
		tagged
		task
		protected

The classes "numeric" and "nonlimited" represent other classification dimensions and do not fit into the above strictly hierarchical picture. 


#### Wording Changes from Ada 83

This clause and its subclauses now precede the clause and subclauses on objects and named numbers, to cut down on the number of forward references.

We have dropped the term "base type" in favor of simply "type" (all types in Ada 83 were "base types" so it wasn't clear when it was appropriate/necessary to say "base type"). Given a subtype S of a type T, we call T the "type of the subtype S." 


### 3.2.1  Type Declarations

A [type_declaration](S0020) declares a type and its first subtype. 


#### Syntax

type_declaration ::=  [full_type_declaration](S0021)
   | [incomplete_type_declaration](S0078)
   | [private_type_declaration](S0164)
   | [private_extension_declaration](S0165)

full_type_declaration ::= 
     type [defining_identifier](S0019) [[known_discriminant_part](S0058)] is [type_definition](S0022);
   | [task_type_declaration](S0175)
   | [protected_type_declaration](S0180)

type_definition ::= 
     [enumeration_type_definition](S0035)	| [integer_type_definition](S0038)
   | [real_type_definition](S0041)	| [array_type_definition](S0048)
   | [record_type_definition](S0063)	| [access_type_definition](S0073)
   | [derived_type_definition](S0032)


#### Legality Rules

A given type shall not have a subcomponent whose type is the given type itself. 


#### Static Semantics

The [defining_identifier](S0019) of a [type_declaration](S0020) denotes the first subtype of the type. The [known_discriminant_part](S0058), if any, defines the discriminants of the type (see 3.7, "Discriminants"). The remainder of the [type_declaration](S0020) defines the remaining characteristics of (the view of) the type.

A type defined by a [type_declaration](S0020) is a named type; such a type has one or more nameable subtypes. Certain other forms of declaration also include type definitions as part of the declaration for an object (including a parameter or a discriminant). The type defined by such a declaration is anonymous - it has no nameable subtypes. For explanatory purposes, this document sometimes refers to an anonymous type by a pseudo-name, written in italics, and uses such pseudo-names at places where the syntax normally requires an [identifier](S0002). For a named type whose first subtype is T, this document sometimes refers to the type of T as simply "the type T". 

Ramification: The only user-defined types that can be anonymous in the above sense are array, access, task, and protected types. An anonymous array, task, or protected type can be defined as part of an [object_declaration](S0029). An anonymous access type can be defined as part of a parameter or discriminant specification. 

A named type that is declared by a [full_type_declaration](S0021), or an anonymous type that is defined as part of declaring an object of the type, is called a full type. The [type_definition](S0022), [task_definition](S0177), [protected_definition](S0182), or [access_definition](S0077) that defines a full type is called a full type definition. [Types declared by other forms of [type_declaration](S0020) are not separate types; they are partial or incomplete views of some full type.] 

To be honest: Class-wide, universal, and root numeric types are full types. 

The definition of a type implicitly declares certain predefined operators that operate on the type, according to what classes the type belongs, as specified in 4.5, "Operators and Expression Evaluation". 

Discussion: We no longer talk about the implicit declaration of basic operations. These are treated like an [if_statement](S0131) - they don't need to be declared, but are still applicable to only certain classes of types.

The predefined types [(for example the types Boolean, Wide_Character, Integer, root_integer, and universal_integer)] are the types that are defined in [a predefined library package called] Standard[; this package also includes the [(implicit)] declarations of their predefined operators]. [The package Standard is described in A.1.] 

Ramification: We use the term "predefined" to refer to entities declared in the visible part of Standard, to implicitly declared operators of a type whose semantics are defined by the language, to Standard itself, and to the "predefined environment". We do not use this term to refer to library packages other than Standard. For example Text_IO is a language-defined package, not a predefined package, and Text_IO.Put_Line is not a predefined operation. 


#### Dynamic Semantics

The elaboration of a [full_type_declaration](S0021) consists of the elaboration of the full type definition. Each elaboration of a full type definition creates a distinct type and its first subtype. 

Reason: The creation is associated with the type definition, rather than the type declaration, because there are types that are created by full type definitions that are not immediately contained within a type declaration (e.g. an array object declaration, a singleton task declaration, etc.). 

Ramification: Any implicit declarations that occur immediately following the full type definition are elaborated where they (implicitly) occur. 


#### Examples

Examples of type definitions: 

```ada
(White, Red, Yellow, Green, Blue, Brown, Black)
range 1 .. 72
array(1 .. 10) of Integer

```

Examples of type declarations: 

```ada
type Color  is (White, Red, Yellow, Green, Blue, Brown, Black);
type Column is range 1 .. 72;
type Table  is array(1 .. 10) of Integer;

```

NOTE 1   Each of the above examples declares a named type. The identifier given denotes the first subtype of the type. Other named subtypes of the type can be declared with [subtype_declaration](S0023)s (see 3.2.2). Although names do not directly denote types, a phrase like "the type Column" is sometimes used in this document to refer to the type of Column, where Column denotes the first subtype of the type. For an example of the definition of an anonymous type, see the declaration of the array Color_Table in 3.3.1; its type is anonymous - it has no nameable subtypes. 


#### Wording Changes from Ada 83

The syntactic category [full_type_declaration](S0021) now includes task and protected type declarations.

We have generalized the concept of first-named subtype (now called simply "first subtype") to cover all kinds of types, for uniformity of description elsewhere. RM83 defined first-named subtype in Section 13. We define first subtype here, because it is now a more fundamental concept. We renamed the term, because in Ada 95 some first subtypes have no name.

We no longer elaborate [discriminant_part](S0056)s, because there is nothing to do, and it was complex to say that you only wanted to elaborate it once for a private or incomplete type. This is also consistent with the fact that subprogram specifications are not elaborated (neither in Ada 83 nor in Ada 95). Note, however, that an [access_definition](S0077) appearing in a [discriminant_part](S0056) is elaborated when an object with such a discriminant is created. 


### 3.2.2  Subtype Declarations

A [subtype_declaration](S0023) declares a subtype of some previously declared type, as defined by a [subtype_indication](S0024). 


#### Syntax

subtype_declaration ::= 
   subtype [defining_identifier](S0019) is [subtype_indication](S0024);

subtype_indication ::=  [subtype_mark](S0025) [[constraint](S0026)]

subtype_mark ::= subtype_[name](S0084)

Ramification: Note that [name](S0084) includes [attribute_reference](S0093); thus, S'Base can be used as a [subtype_mark](S0025). 

Reason: We considered changing [subtype_mark](S0025) to subtype_name. However, existing users are used to the word "mark," so we're keeping it. 

constraint ::= [scalar_constraint](S0027) | [composite_constraint](S0028)

scalar_constraint ::= 
     [range_constraint](S0033) | [digits_constraint](S0047) | [delta_constraint](S0275)

composite_constraint ::= 
     [index_constraint](S0054) | [discriminant_constraint](S0061)


#### Name Resolution Rules

A [subtype_mark](S0025) shall resolve to denote a subtype. The type determined by a [subtype_mark](S0025) is the type of the subtype denoted by the [subtype_mark](S0025). 

Ramification: Types are never directly named; all [subtype_mark](S0025)s denote subtypes - possibly an unconstrained (base) subtype, but never the type. When we use the term anonymous type we really mean a type with no namable subtypes. 


#### Dynamic Semantics

The elaboration of a [subtype_declaration](S0023) consists of the elaboration of the [subtype_indication](S0024). The elaboration of a [subtype_indication](S0024) creates a new subtype. If the [subtype_indication](S0024) does not include a [constraint](S0026), the new subtype has the same (possibly null) constraint as that denoted by the [subtype_mark](S0025). The elaboration of a [subtype_indication](S0024) that includes a [constraint](S0026) proceeds as follows: 

The [constraint](S0026) is first elaborated.

A check is then made that the [constraint](S0026) is compatible with the subtype denoted by the [subtype_mark](S0025). 

Ramification: The checks associated with constraint compatibility are all Range_Checks. Discriminant_Checks and Index_Checks are associated only with checks that a value satisfies a constraint. 

The condition imposed by a [constraint](S0026) is the condition obtained after elaboration of the [constraint](S0026). The rules defining compatibility are given for each form of [constraint](S0026) in the appropriate subclause. These rules are such that if a [constraint](S0026) is compatible with a subtype, then the condition imposed by the [constraint](S0026) cannot contradict any condition already imposed by the subtype on its values. The exception Constraint_Error is raised if any check of compatibility fails. 

To be honest: The condition imposed by a [constraint](S0026) is named after it - a [range_constraint](S0033) imposes a range constraint, etc. 

Ramification: A [range_constraint](S0033) causes freezing of its type. Other [constraint](S0026)s do not. 

NOTE 1   A [scalar_constraint](S0027) may be applied to a subtype of an appropriate scalar type (see 3.5, 3.5.9, and J.3), even if the subtype is already constrained. On the other hand, a [composite_constraint](S0028) may be applied to a composite subtype (or an access-to-composite subtype) only if the composite subtype is unconstrained (see 3.6.1 and 3.7.1). 


#### Examples

Examples of subtype declarations: 

```ada
subtype Rainbow   is Color range Red .. Blue;        --  see 3.2.1
subtype Red_Blue  is Rainbow;
subtype Int       is Integer;
subtype Small_Int is Integer range -10 .. 10;
subtype Up_To_K   is Column range 1 .. K;            --  see 3.2.1
subtype Square    is Matrix(1 .. 10, 1 .. 10);       --  see 3.6
subtype Male      is Person(Sex =&gt M);               --  see 3.10.1

```


#### Incompatibilities With Ada 83

In Ada 95, all [range_constraint](S0033)s cause freezing of their type. Hence, a type-related representation item for a scalar type has to precede any [range_constraint](S0033)s whose type is the scalar type. 


#### Wording Changes from Ada 83

[Subtype_mark](S0025)s allow only subtype names now, since types are never directly named. There is no need for RM83-3.3.2(3), which says a [subtype_mark](S0025) can denote both the type and the subtype; in Ada 95, you denote an unconstrained (base) subtype if you want, but never the type.

The syntactic category type_mark is now called [subtype_mark](S0025), since it always denotes a subtype. 


### 3.2.3  Classification of Operations


#### Static Semantics

An operation operates on a type T if it yields a value of type T, if it has an operand whose expected type (see 8.6) is T, or if it has an access parameter (see 6.1) designating T. A predefined operator, or other language-defined operation such as assignment or a membership test, that operates on a type, is called a predefined operation of the type. The primitive operations of a type are the predefined operations of the type, plus any user-defined primitive subprograms. 

Glossary entry: The primitive operations of a type are the operations (such as subprograms) declared together with the type declaration. They are inherited by other types in the same class of types. For a tagged type, the primitive subprograms are dispatching subprograms, providing run-time polymorphism. A dispatching subprogram may be called with statically tagged operands, in which case the subprogram body invoked is determined at compile time. Alternatively, a dispatching subprogram may be called using a dispatching call, in which case the subprogram body invoked is determined at run time.

Version=[5],Kind=(Added),Group=[T],Term=[primitive operations of a type], Def=[the operations (such as subprograms) declared together with the type declarations], Note1=[Primitive operations are inherited by other types in the same derivation class of types.]

To be honest: Protected subprograms are not considered to be "primitive subprograms", even though they are subprograms, and they are inherited by derived types. 

Discussion: We use the term "primitive subprogram" in most of the rest of the manual. The term "primitive operation" is used mostly in conceptual discussions. 

The primitive subprograms of a specific type are defined as follows: 

The predefined operators of the type (see 4.5);

For a derived type, the inherited (see 3.4) user-defined subprograms;

For an enumeration type, the enumeration literals (which are considered parameterless functions - see 3.5.1);

For a specific type declared immediately within a [package_specification](S0162), any subprograms (in addition to the enumeration literals) that are explicitly declared immediately within the same [package_specification](S0162) and that operate on the type;

Any subprograms not covered above [that are explicitly declared immediately within the same declarative region as the type] and that override (see 8.3) other implicitly declared primitive subprograms of the type. 

Discussion: In Ada 83, only subprograms declared in the visible part were "primitive" (i.e. derivable). In Ada 95, mostly because of child library units, we include all operations declared in the private part as well, and all operations that override implicit declarations. 

Ramification: It is possible for a subprogram to be primitive for more than one type, though it is illegal for a subprogram to be primitive for more than one tagged type. See 3.9. 

Discussion: The order of the implicit declarations when there are both predefined operators and inherited subprograms is described in 3.4, "Derived Types and Classes". 

A primitive subprogram whose designator is an [operator_symbol](S0147) is called a primitive operator.


#### Incompatibilities With Ada 83

The attribute S'Base is no longer defined for nonscalar subtypes. Since this was only permitted as the prefix of another attribute, and there are no interesting nonscalar attributes defined for an unconstrained composite or access subtype, this should not affect any existing programs. 


#### Extensions to Ada 83

The primitive subprograms (derivable subprograms) include subprograms declared in the private part of a package specification as well, and those that override implicitly declared subprograms, even if declared in a body. 


#### Wording Changes from Ada 83

We have dropped the confusing term operation of a type in favor of the more useful primitive operation of a type and the phrase operates on a type.

The description of S'Base has been moved to 3.5, "Scalar Types" because it is now defined only for scalar types. 


## 3.3  Objects and Named Numbers

[Objects are created at run time and contain a value of a given type. An object can be created and initialized as part of elaborating a declaration, evaluating an [allocator](S0122), [aggregate](S0097), or [function_call](S0156), or passing a parameter by copy. Prior to reclaiming the storage for an object, it is finalized if necessary (see 7.6.1).] 


#### Static Semantics

All of the following are objects: 

Glossary entry: An object is either a constant or a variable. An object contains a value. An object is created by an [object_declaration](S0029) or by an [allocator](S0122). A formal parameter is (a view of) an object. A subcomponent of an object is an object.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[object], Def=[an entity that contains a value, and is either a constant or a variable], Note1=[An object is created by an [object_declaration](S0029) or by an [allocator](S0122). A formal parameter is (a view of) an object. A subcomponent of an object is an object.]

the entity declared by an [object_declaration](S0029);

a formal parameter of a subprogram, entry, or generic subprogram;

a generic formal object;

a loop parameter;

a choice parameter of an [exception_handler](S0232);

an entry index of an [entry_body](S0190);

the result of dereferencing an access-to-object value (see 4.1);

the result of evaluating a [function_call](S0156) (or the equivalent operator invocation - see 6.6);

the result of evaluating an [aggregate](S0097);

a component, slice, or view conversion of another object. 

An object is either a constant object or a variable object. The value of a constant object cannot be changed between its initialization and its finalization, whereas the value of a variable object can be changed. Similarly, a view of an object is either a constant or a variable. All views of a constant object are constant. A constant view of a variable object cannot be used to modify the value of the variable. The terms constant and variable by themselves refer to constant and variable views of objects.

The value of an object is read when the value of any part of the object is evaluated, or when the value of an enclosing object is evaluated. The value of a variable is updated when an assignment is performed to any part of the variable, or when an assignment is performed to an enclosing object. 

Ramification: Reading and updating are intended to include read/write references of any kind, even if they are not associated with the evaluation of a particular construct. Consider, for example, the expression "X.all(F)", where X is an access-to-array object, and F is a function. The implementation is allowed to first evaluate "X.all" and then F. Finally, a read is performed to get the value of the F'th component of the array. Note that the array is not necessarily read as part of the evaluation of "X.all". This is important, because if F were to free X using Unchecked_Deallocation, we want the execution of the final read to be erroneous. 

Whether a view of an object is constant or variable is determined by the definition of the view. The following (and no others) represent constants: 

an object declared by an [object_declaration](S0029) with the reserved word constant;

a formal parameter or generic formal object of mode in;

a discriminant;

a loop parameter, choice parameter, or entry index;

the dereference of an access-to-constant value;

the result of evaluating a [function_call](S0156) or an [aggregate](S0097);

a [selected_component](S0091), [indexed_component](S0089), [slice](S0090), or view conversion of a constant.

To be honest: A noninvertible view conversion to a general access type is also defined to be a constant - see 4.6. 

At the place where a view of an object is defined, a nominal subtype is associated with the view. The object's actual subtype (that is, its subtype) can be more restrictive than the nominal subtype of the view; it always is if the nominal subtype is an indefinite subtype. A subtype is an indefinite subtype if it is an unconstrained array subtype, or if it has unknown discriminants or unconstrained discriminants without defaults (see 3.7); otherwise the subtype is a definite subtype [(all elementary subtypes are definite subtypes)]. [A class-wide subtype is defined to have unknown discriminants, and is therefore an indefinite subtype. An indefinite subtype does not by itself provide enough information to create an object; an additional [constraint](S0026) or explicit initialization [expression](S0108) is necessary (see 3.3.1). A component cannot have an indefinite nominal subtype.]

Version=[5],Kind=(AddedNormal),Group=[T], Term=[nominal subtype of a view of an object], Def=[the subtype specified when the view is defined]

A named number provides a name for a numeric value known at compile time. It is declared by a [number_declaration](S0031). 

NOTE 1   A constant cannot be the target of an assignment operation, nor be passed as an in out or out parameter, between its initialization and finalization, if any.

NOTE 2   The nominal and actual subtypes of an elementary object are always the same. For a discriminated or array object, if the nominal subtype is constrained then so is the actual subtype. 


#### Extensions to Ada 83

There are additional kinds of objects (choice parameters and entry indices of entry bodies).

The result of a function and of evaluating an aggregate are considered (constant) objects. This is necessary to explain the action of finalization on such things. Because a [function_call](S0156) is also syntactically a [name](S0084) (see 4.1), the result of a [function_call](S0156) can be renamed, thereby allowing repeated use of the result without calling the function again. 


#### Wording Changes from Ada 83

This clause and its subclauses now follow the clause and subclauses on types and subtypes, to cut down on the number of forward references.

The term nominal subtype is new. It is used to distinguish what is known at compile time about an object's constraint, versus what its "true" run-time constraint is.

The terms definite and indefinite (which apply to subtypes) are new. They are used to aid in the description of generic formal type matching, and to specify when an explicit initial value is required in an [object_declaration](S0029).

We have moved the syntax for [object_declaration](S0029) and [number_declaration](S0031) down into their respective subclauses, to keep the syntax close to the description of the associated semantics.

We talk about variables and constants here, since the discussion is not specific to [object_declaration](S0029)s, and it seems better to have the list of the kinds of constants juxtaposed with the kinds of objects.

We no longer talk about indirect updating due to parameter passing. Parameter passing is handled in 6.2 and 6.4.1 in a way that there is no need to mention it here in the definition of read and update. Reading and updating now includes the case of evaluating or assigning to an enclosing object. 


### 3.3.1  Object Declarations

An [object_declaration](S0029) declares a stand-alone object with a given nominal subtype and, optionally, an explicit initial value given by an initialization expression. For an array, task, or protected object, the [object_declaration](S0029) may include the definition of the (anonymous) type of the object. 


#### Syntax

object_declaration ::= 
    [defining_identifier_list](S0030) : [aliased] [constant] [subtype_indication](S0024) [:= [expression](S0108)];
  | [defining_identifier_list](S0030) : [aliased] [constant] [array_type_definition](S0048) [:= [expression](S0108)];
  | [single_task_declaration](S0176)
  | [single_protected_declaration](S0181)

defining_identifier_list ::= 
  [defining_identifier](S0019) {, [defining_identifier](S0019)}


#### Name Resolution Rules

For an [object_declaration](S0029) with an [expression](S0108) following the compound delimiter :=, the type expected for the [expression](S0108) is that of the object. This [expression](S0108) is called the initialization expression. 


#### Legality Rules

An [object_declaration](S0029) without the reserved word constant declares a variable object. If it has a [subtype_indication](S0024) or an [array_type_definition](S0048) that defines an indefinite subtype, then there shall be an initialization expression. An initialization expression shall not be given if the object is of a limited type. 


#### Static Semantics

An [object_declaration](S0029) with the reserved word constant declares a constant object. If it has an initialization expression, then it is called a full constant declaration. Otherwise it is called a deferred constant declaration. The rules for deferred constant declarations are given in clause 7.4. The rules for full constant declarations are given in this subclause.

Any declaration that includes a [defining_identifier_list](S0030) with more than one [defining_identifier](S0019) is equivalent to a series of declarations each containing one [defining_identifier](S0019) from the list, with the rest of the text of the declaration copied for each declaration in the series, in the same order as the list. The remainder of this Reference Manual relies on this equivalence; explanations are given for declarations with a single [defining_identifier](S0019).

The [subtype_indication](S0024) or full type definition of an [object_declaration](S0029) defines the nominal subtype of the object. The [object_declaration](S0029) declares an object of the type of the nominal subtype. 

Discussion: The phrase "full type definition" here includes the case of an anonymous array, task, or protected type. 


#### Dynamic Semantics

If a composite object declared by an [object_declaration](S0029) has an unconstrained nominal subtype, then if this subtype is indefinite or the object is constant or aliased (see 3.10) the actual subtype of this object is constrained. The constraint is determined by the bounds or discriminants (if any) of its initial value; the object is said to be constrained by its initial value. [In the case of an aliased object, this initial value may be either explicit or implicit; in the other cases, an explicit initial value is required.] When not constrained by its initial value, the actual and nominal subtypes of the object are the same. If its actual subtype is constrained, the object is called a constrained object.

For an [object_declaration](S0029) without an initialization expression, any initial values for the object or its subcomponents are determined by the implicit initial values defined for its nominal subtype, as follows: 

The implicit initial value for an access subtype is the null value of the access type.

The implicit initial (and only) value for each discriminant of a constrained discriminated subtype is defined by the subtype.

For a (definite) composite subtype, the implicit initial value of each component with a [default_expression](S0060) is obtained by evaluation of this expression and conversion to the component's nominal subtype (which might raise Constraint_Error - see 4.6, "Type Conversions"), unless the component is a discriminant of a constrained subtype (the previous case), or is in an excluded [variant](S0069) (see 3.8.1). For each component that does not have a [default_expression](S0060), any implicit initial values are those determined by the component's nominal subtype.

For a protected or task subtype, there is an implicit component (an entry queue) corresponding to each entry, with its implicit initial value being an empty queue. 

Implementation Note: The implementation may add implicit components for its own use, which might have implicit initial values. For a task subtype, such components might represent the state of the associated thread of control. For a type with dynamic-sized components, such implicit components might be used to hold the offset to some explicit component. 

The elaboration of an [object_declaration](S0029) proceeds in the following sequence of steps: 

a)The [subtype_indication](S0024), [array_type_definition](S0048), [single_task_declaration](S0176), or [single_protected_declaration](S0181) is first elaborated. This creates the nominal subtype (and the anonymous type in the latter three cases).

b)If the [object_declaration](S0029) includes an initialization expression, the (explicit) initial value is obtained by evaluating the expression and converting it to the nominal subtype (which might raise Constraint_Error - see 4.6). 

c)The object is created, and, if there is not an initialization expression, any per-object expressions (see 3.8) are evaluated and any implicit initial values for the object or for its subcomponents are obtained as determined by the nominal subtype. 

Discussion: For a per-object constraint that contains some per-object expressions and some non-per-object expressions, the values used for the constraint consist of the values of the non-per-object expressions evaluated at the point of the [type_declaration](S0020), and the values of the per-object expressions evaluated at the point of the creation of the object.

The elaboration of per-object constraints was presumably performed as part of the dependent compatibility check in Ada 83. If the object is of a limited type with an access discriminant, the [access_definition](S0077) is elaborated at this time (see 3.7). 

Reason: The reason we say that evaluating an explicit initialization expression happens before creating the object is that in some cases it is impossible to know the size of the object being created until its initial value is known, as in "X: String := Func_Call(...);". The implementation can create the object early in the common case where the size can be known early, since this optimization is semantically neutral. 

d)Any initial values (whether explicit or implicit) are assigned to the object or to the corresponding subcomponents. As described in 5.2 and 7.6, Initialize and Adjust procedures can be called. 

Ramification: Since the initial values have already been converted to the appropriate nominal subtype, the only Constraint_Errors that might occur as part of these assignments are for values outside their base range that are used to initialize unconstrained numeric subcomponents. See 3.5. 

For the third step above, the object creation and any elaborations and evaluations are performed in an arbitrary order, except that if the [default_expression](S0060) for a discriminant is evaluated to obtain its initial value, then this evaluation is performed before that of the [default_expression](S0060) for any component that depends on the discriminant, and also before that of any [default_expression](S0060) that includes the name of the discriminant. The evaluations of the third step and the assignments of the fourth step are performed in an arbitrary order, except that each evaluation is performed before the resulting value is assigned. 

Reason: For example: 

```ada
type R(D : Integer := F) is
    record
        S : String(1..D) := (others =&gt G);
    end record;

```

```ada
X : R;

```

For the elaboration of the declaration of X, it is important that F be evaluated before the aggregate. 

[There is no implicit initial value defined for a scalar subtype.] In the absence of an explicit initialization, a newly created scalar object might have a value that does not belong to its subtype (see 13.9.1 and H.1). 

To be honest: It could even be represented by a bit pattern that doesn't actually represent any value of the type at all, such as an invalid internal code for an enumeration type, or a NaN for a floating point type. It is a generally a bounded error to reference scalar objects with such "invalid representations", as explained in 13.9.1, "Data Validity". 

Ramification: There is no requirement that two objects of the same scalar subtype have the same implicit initial "value" (or representation). It might even be the case that two elaborations of the same [object_declaration](S0029) produce two different initial values. However, any particular uninitialized object is default-initialized to a single value (or invalid representation). Thus, multiple reads of such an uninitialized object will produce the same value each time (if the implementation chooses not to detect the error). 

NOTE 1   Implicit initial values are not defined for an indefinite subtype, because if an object's nominal subtype is indefinite, an explicit initial value is required.

NOTE 2   As indicated above, a stand-alone object is an object declared by an [object_declaration](S0029). Similar definitions apply to "stand-alone constant" and "stand-alone variable". A subcomponent of an object is not a stand-alone object, nor is an object that is created by an [allocator](S0122). An object declared by a [loop_parameter_specification](S0137), [parameter_specification](S0152), [entry_index_specification](S0193), [choice_parameter_specification](S0233), or a [formal_object_declaration](S0245) is not called a stand-alone object.

NOTE 3   The type of a stand-alone object cannot be abstract (see 3.9.3). 


#### Examples

Example of a multiple object declaration: 

```ada
--  the multiple object declaration 

```

```ada
John, Paul : Person_Name := new Person(Sex =&gt M);  --  see 3.10.1

```

```ada
--  is equivalent to the two single object declarations in the order given

```

```ada
John : Person_Name := new Person(Sex =&gt M);
Paul : Person_Name := new Person(Sex =&gt M);

```

Examples of variable declarations: 

```ada
Count, Sum  : Integer;
Size        : Integer range 0 .. 10_000 := 0;
Sorted      : Boolean := False;
Color_Table : array(1 .. Max) of Color;
Option      : Bit_Vector(1 .. 10) := (others =&gt True);
Hello       : constant String := "Hi, world.";

```

Examples of constant declarations: 

```ada
Limit     : constant Integer := 10_000;
Low_Limit : constant Integer := Limit/10;
Tolerance : constant Real := Dispersion(1.15);

```


#### Extensions to Ada 83

The syntax rule for [object_declaration](S0029) is modified to allow the aliased reserved word.

A variable declared by an [object_declaration](S0029) can be constrained by its initial value; that is, a variable of a nominally unconstrained array subtype, or discriminated type without defaults, can be declared so long as it has an explicit initial value. In Ada 83, this was permitted for constants, and for variables created by allocators, but not for variables declared by [object_declaration](S0029)s. This is particularly important for tagged class-wide types, since there is no way to constrain them explicitly, and so an initial value is the only way to provide a constraint. It is also important for generic formal private types with unknown discriminants.

We now allow an [unconstrained_array_definition](S0049) in an [object_declaration](S0029). This allows an object of an anonymous array type to have its bounds determined by its initial value. This is for uniformity: If one can write "X: constant array(Integer range 1..10) of Integer := ...;" then it makes sense to also allow "X: constant array(Integer range &lt&gt) of Integer := ...;". (Note that if anonymous array types are ever sensible, a common situation is for a table implemented as an array. Tables are often constant, and for constants, there's usually no point in forcing the user to count the number of elements in the value.) 


#### Wording Changes from Ada 83

We have moved the syntax for [object_declaration](S0029)s into this subclause.

Deferred constants no longer have a separate syntax rule, but rather are incorporated in [object_declaration](S0029) as constants declared without an initialization expression. 


### 3.3.2  Number Declarations

A [number_declaration](S0031) declares a named number. 

Discussion: If a value or other property of a construct is required to be static that means it is required to be determined prior to execution. A static expression is an expression whose value is computed at compile time and is usable in contexts where the actual value might affect the legality of the construct. This is fully defined in clause 4.9. 


#### Syntax

number_declaration ::= 
     [defining_identifier_list](S0030) : constant := static_[expression](S0108);


#### Name Resolution Rules

The static_[expression](S0108) given for a [number_declaration](S0031) is expected to be of any numeric type.


#### Legality Rules

The static_[expression](S0108) given for a number declaration shall be a static expression, as defined by clause 4.9. 


#### Static Semantics

The named number denotes a value of type universal_integer if the type of the static_[expression](S0108) is an integer type. The named number denotes a value of type universal_real if the type of the static_[expression](S0108) is a real type.

The value denoted by the named number is the value of the static_[expression](S0108), converted to the corresponding universal type. 


#### Dynamic Semantics

The elaboration of a [number_declaration](S0031) has no effect. 

Proof: Since the static_[expression](S0108) was evaluated at compile time. 


#### Examples

Examples of number declarations: 

```ada
Two_Pi        : constant := 2.0*Ada.Numerics.Pi;   -- a real number (see A.5)

```

```ada
Max           : constant := 500;                   -- an integer number
Max_Line_Size : constant := Max/6                  -- the integer 83
Power_16      : constant := 2**16;                 -- the integer 65_536
One, Un, Eins : constant := 1;                     -- three different names for 1

```


#### Extensions to Ada 83

We now allow a static expression of any numeric type to initialize a named number. For integer types, it was possible in Ada 83 to use 'Pos to define a named number, but there was no way to use a static expression of some nonuniversal real type to define a named number. This change is upward compatible because of the preference rule for the operators of the root numeric types. 


#### Wording Changes from Ada 83

We have moved the syntax rule into this subclause.

AI83-00263 describes the elaboration of a number declaration in words similar to that of an [object_declaration](S0029). However, since there is no expression to be evaluated and no object to be created, it seems simpler to say that the elaboration has no effect. 


## 3.4  Derived Types and Classes

A [derived_type_definition](S0032) defines a new type (and its first subtype) whose characteristics are derived from those of a parent type. 

Glossary entry: A derived type is a type defined in terms of another type, which is the parent type of the derived type. Each class containing the parent type also contains the derived type. The derived type inherits properties such as components and primitive operations from the parent. A type together with the types derived from it (directly or indirectly) form a derivation class.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[derived type], Def=[a type defined in terms of a parent type and zero or more progenitor types given in a derived type definition], Note1=[A derived type inherits properties such as components and primitive operations from its parent and progenitors.], Note2=[A type together with the types derived from it (directly or indirectly) form a derivation class.]


#### Syntax

derived_type_definition ::= [abstract] new parent_[subtype_indication](S0024) [[record_extension_part](S0072)]


#### Legality Rules

The parent_[subtype_indication](S0024) defines the parent subtype; its type is the parent type. 

Version=[5],Kind=(AddedNormal),Group=[T],Term=[parent of a derived type], Def=[the first ancestor type given in the definition of the derived type], Note1=[The parent can be almost any kind of type, including an interface type.]

A type shall be completely defined (see 3.11.1) prior to being specified as the parent type in a [derived_type_definition](S0032) - [the [full_type_declaration](S0021)s for the parent type and any of its subcomponents have to precede the [derived_type_definition](S0032).] 

Discussion: This restriction does not apply to the ancestor type of a private extension - see 7.3; such a type need not be completely defined prior to the [private_extension_declaration](S0165). However, the restriction does apply to record extensions, so the ancestor type will have to be completely defined prior to the [full_type_declaration](S0021) corresponding to the [private_extension_declaration](S0165). 

Reason: We originally hoped we could relax this restriction. However, we found it too complex to specify the rules for a type derived from an incompletely defined limited type that subsequently became nonlimited. 

If there is a [record_extension_part](S0072), the derived type is called a record extension of the parent type. A [record_extension_part](S0072) shall be provided if and only if the parent type is a tagged type. 

Implementation Note: We allow a record extension to inherit discriminants; an early version of Ada 9X did not. If the parent subtype is unconstrained, it can be implemented as though its discriminants were repeated in a new [known_discriminant_part](S0058) and then used to constrain the old ones one-for-one. However, in an extension aggregate, the discriminants in this case do not appear in the component association list. 

Ramification: This rule needs to be rechecked in the visible part of an instance of a generic unit 


#### Static Semantics

The first subtype of the derived type is unconstrained if a [known_discriminant_part](S0058) is provided in the declaration of the derived type, or if the parent subtype is unconstrained. Otherwise, the constraint of the first subtype corresponds to that of the parent subtype in the following sense: it is the same as that of the parent subtype except that for a range constraint (implicit or explicit), the value of each bound of its range is replaced by the corresponding value of the derived type. 

Discussion: A [digits_constraint](S0047) in a [subtype_indication](S0024) for a decimal fixed point subtype always imposes a range constraint, implicitly if there is no explicit one given. See 3.5.9, "Fixed Point Types". 

The characteristics of the derived type are defined as follows: 

Each class of types that includes the parent type also includes the derived type. 

Discussion: This is inherent in our notion of a "class" of types. It is not mentioned in the initial definition of "class" since at that point type derivation has not been defined. In any case, this rule ensures that every class of types is closed under derivation. 

If the parent type is an elementary type or an array type, then the set of possible values of the derived type is a copy of the set of possible values of the parent type. For a scalar type, the base range of the derived type is the same as that of the parent type. 

Discussion: The base range of a type defined by an [integer_type_definition](S0038) or a [real_type_definition](S0041) is determined by the _definition, and is not necessarily the same as that of the corresponding root numeric type from which the newly defined type is implicitly derived. Treating numerics types as implicitly derived from one of the two root numeric types is simply to link them into a type hierarchy; such an implicit derivation does not follow all the rules given here for an explicit [derived_type_definition](S0032). 

If the parent type is a composite type other than an array type, then the components, protected subprograms, and entries that are declared for the derived type are as follows: 

The discriminants specified by a new [known_discriminant_part](S0058), if there is one; otherwise, each discriminant of the parent type (implicitly declared in the same order with the same specifications) - in the latter case, the discriminants are said to be inherited, or if unknown in the parent, are also unknown in the derived type;

Each nondiscriminant component, entry, and protected subprogram of the parent type, implicitly declared in the same order with the same declarations; these components, entries, and protected subprograms are said to be inherited; 

Ramification: The profiles of entries and protected subprograms do not change upon type derivation, although the type of the "implicit" parameter identified by the [prefix](S0086) of the [name](S0084) in a call does.

To be honest: Any name in the parent [type_declaration](S0020) that denotes the current instance of the type is replaced with a name denoting the current instance of the derived type, converted to the parent type.

Each component declared in a [record_extension_part](S0072), if any. 

Declarations of components, protected subprograms, and entries, whether implicit or explicit, occur immediately within the declarative region of the type, in the order indicated above, following the parent [subtype_indication](S0024). 

Discussion: The order of declarations within the region matters for [record_aggregate](S0098)s and [extension_aggregate](S0102)s. 

Ramification: In most cases, these things are implicitly declared immediately following the parent [subtype_indication](S0024). However, 7.3.1, "Private Operations" defines some cases in which they are implicitly declared later, and some cases in which the are not declared at all. 

Discussion: The place of the implicit declarations of inherited components matters for visibility - they are not visible in the [known_discriminant_part](S0058) nor in the parent [subtype_indication](S0024), but are usually visible within the [record_extension_part](S0072), if any (although there are restrictions on their use). Note that a discriminant specified in a new [known_discriminant_part](S0058) is not considered "inherited" even if it has the same name and subtype as a discriminant of the parent type. 

The derived type is limited if and only if the parent type is limited. 

To be honest: The derived type can become nonlimited if the derivation takes place in the visible part of a child package, and the parent type is nonlimited as viewed from the private part of the child package - see 7.5. 

[For each predefined operator of the parent type, there is a corresponding predefined operator of the derived type.] 

Proof: This is a ramification of the fact that each class that includes the parent type also includes the derived type, and the fact that the set of predefined operators that is defined for a type, as described in 4.5, is determined by the classes to which it belongs. 

Reason: Predefined operators are handled separately because they follow a slightly different rule than user-defined primitive subprograms. In particular the systematic replacement described below does not apply fully to the relational operators for Boolean and the exponentiation operator for Integer. The relational operators for a type derived from Boolean still return Standard.Boolean. The exponentiation operator for a type derived from Integer still expects Standard.Integer for the right operand. In addition, predefined operators "reemerge" when a type is the actual type corresponding to a generic formal type, so they need to be well defined even if hidden by user-defined primitive subprograms. 

For each user-defined primitive subprogram (other than a user-defined equality operator - see below) of the parent type that already exists at the place of the [derived_type_definition](S0032), there exists a corresponding inherited primitive subprogram of the derived type with the same defining name. Primitive user-defined equality operators of the parent type are also inherited by the derived type, except when the derived type is a nonlimited record extension, and the inherited operator would have a profile that is type conformant with the profile of the corresponding predefined equality operator; in this case, the user-defined equality operator is not inherited, but is rather incorporated into the implementation of the predefined equality operator of the record extension (see 4.5.2). 

Ramification: We say "...already exists..." rather than "is visible" or "has been declared" because there are certain operations that are declared later, but still exist at the place of the [derived_type_definition](S0032), and there are operations that are never declared, but still exist. These cases are explained in 7.3.1.

Note that nonprivate extensions can appear only after the last primitive subprogram of the parent - the freezing rules ensure this. 

Reason: A special case is made for the equality operators on nonlimited record extensions because their predefined equality operators are already defined in terms of the primitive equality operator of their parent type (and of the tagged components of the extension part). Inheriting the parent's equality operator as is would be undesirable, because it would ignore any components of the extension part. On the other hand, if the parent type is limited, then any user-defined equality operator is inherited as is, since there is no predefined equality operator to take its place. 

Ramification: Because user-defined equality operators are not inherited by record extensions, the formal parameter names of = and /= revert to Left and Right, even if different formal parameter names were used in the user-defined equality operators of the parent type. 

The profile of an inherited subprogram (including an inherited enumeration literal) is obtained from the profile of the corresponding (user-defined) primitive subprogram of the parent type, after systematic replacement of each subtype of its profile (see 6.1) that is of the parent type with a corresponding subtype of the derived type. For a given subtype of the parent type, the corresponding subtype of the derived type is defined as follows: 

If the declaration of the derived type has neither a [known_discriminant_part](S0058) nor a [record_extension_part](S0072), then the corresponding subtype has a constraint that corresponds (as defined above for the first subtype of the derived type) to that of the given subtype.

If the derived type is a record extension, then the corresponding subtype is the first subtype of the derived type.

If the derived type has a new [known_discriminant_part](S0058) but is not a record extension, then the corresponding subtype is constrained to those values that when converted to the parent type belong to the given subtype (see 4.6). 

Reason: An inherited subprogram of an untagged type has an Intrinsic calling convention, which precludes the use of the Access attribute. We preclude 'Access because correctly performing all required constraint checks on an indirect call to such an inherited subprogram was felt to impose an undesirable implementation burden.

The same formal parameters have [default_expression](S0060)s in the profile of the inherited subprogram. [Any type mismatch due to the systematic replacement of the parent type by the derived type is handled as part of the normal type conversion associated with parameter passing - see 6.4.1.] 

Reason: We don't introduce the type conversion explicitly here since conversions to record extensions or on access parameters are not generally legal. Furthermore, any type conversion would just be "undone" since the parent's subprogram is ultimately being called anyway. 

If a primitive subprogram of the parent type is visible at the place of the [derived_type_definition](S0032), then the corresponding inherited subprogram is implicitly declared immediately after the [derived_type_definition](S0032). Otherwise, the inherited subprogram is implicitly declared later or not at all, as explained in 7.3.1.

A derived type can also be defined by a [private_extension_declaration](S0165) (see 7.3) or a [formal_derived_type_definition](S0249) (see 12.5.1). Such a derived type is a partial view of the corresponding full or actual type.

All numeric types are derived types, in that they are implicitly derived from a corresponding root numeric type (see 3.5.4 and 3.5.6).


#### Dynamic Semantics

The elaboration of a [derived_type_definition](S0032) creates the derived type and its first subtype, and consists of the elaboration of the [subtype_indication](S0024) and the [record_extension_part](S0072), if any. If the [subtype_indication](S0024) depends on a discriminant, then only those expressions that do not depend on a discriminant are evaluated. 

For the execution of a call on an inherited subprogram, a call on the corresponding primitive subprogram of the parent type is performed; the normal conversion of each actual parameter to the subtype of the corresponding formal parameter (see 6.4.1) performs any necessary type conversion as well. If the result type of the inherited subprogram is the derived type, the result of calling the parent's subprogram is converted to the derived type. 

Discussion: If an inherited function returns the derived type, and the type is a record extension, then the inherited function is abstract, and (unless overridden) cannot be called except via a dispatching call. See 3.9.3. 

NOTE 1   Classes are closed under derivation - any class that contains a type also contains its derivatives. Operations available for a given class of types are available for the derived types in that class.

NOTE 2   Evaluating an inherited enumeration literal is equivalent to evaluating the corresponding enumeration literal of the parent type, and then converting the result to the derived type. This follows from their equivalence to parameterless functions. 

NOTE 3   A generic subprogram is not a subprogram, and hence cannot be a primitive subprogram and cannot be inherited by a derived type. On the other hand, an instance of a generic subprogram can be a primitive subprogram, and hence can be inherited.

NOTE 4   If the parent type is an access type, then the parent and the derived type share the same storage pool; there is a null access value for the derived type and it is the implicit initial value for the type. See 3.10.

NOTE 5   If the parent type is a boolean type, the predefined relational operators of the derived type deliver a result of the predefined type Boolean (see 4.5.2). If the parent type is an integer type, the right operand of the predefined exponentiation operator is of the predefined type Integer (see 4.5.6).

NOTE 6   Any discriminants of the parent type are either all inherited, or completely replaced with a new set of discriminants.

NOTE 7   For an inherited subprogram, the subtype of a formal parameter of the derived type need not have any value in common with the first subtype of the derived type. 

Proof: This happens when the parent subtype is constrained to a range that does not overlap with the range of a subtype of the parent type that appears in the profile of some primitive subprogram of the parent type. For example: 

```ada
type T1 is range 1..100;
subtype S1 is T1 range 1..10;
procedure P(X : in S1);  -- P is a primitive subprogram
type T2 is new T1 range 11..20;
-- implicitly declared:
-- procedure P(X : in T2'Base range 1..10);
--      X cannot be in T2'First .. T2'Last

```

NOTE 8   If the reserved word abstract is given in the declaration of a type, the type is abstract (see 3.9.3).


#### Examples

Examples of derived type declarations: 

```ada
type Local_Coordinate is new Coordinate;   --  two different types
type Midweek is new Day range Tue .. Thu;  --  see 3.5.1
type Counter is new Positive;              --  same range as Positive 

```

```ada
type Special_Key is new Key_Manager.Key;   --  see 7.3.1
  -- the inherited subprograms have the following specifications: 
  --         procedure Get_Key(K : out Special_Key);
  --         function "&lt"(X,Y : Special_Key) return Boolean;

```


#### Inconsistencies With Ada 83

When deriving from a (nonprivate, nonderived) type in the same visible part in which it is defined, if a predefined operator had been overridden prior to the derivation, the derived type will inherit the user-defined operator rather than the predefined operator. The work-around (if the new behavior is not the desired behavior) is to move the definition of the derived type prior to the overriding of any predefined operators.


#### Incompatibilities With Ada 83

When deriving from a (nonprivate, nonderived) type in the same visible part in which it is defined, a primitive subprogram of the parent type declared before the derived type will be inherited by the derived type. This can cause upward incompatibilities in cases like this: 

```ada
   package P is
      type T is (A, B, C, D);
      function F( X : T := A ) return Integer;
      type NT is new T;
      -- inherits F as
      -- function F( X : NT := A ) return Integer;
      -- in Ada 95 only
      ...
   end P;
   ...
   use P;  -- Only one declaration of F from P is use-visible in
           -- Ada 83;  two declarations of F are use-visible in
           -- Ada 95.
begin
   ...
   if F &gt 1 then ... -- legal in Ada 83, ambiguous in Ada 95

```


#### Extensions to Ada 83

The syntax for a [derived_type_definition](S0032) is amended to include an optional [record_extension_part](S0072) (see 3.9.1).

A derived type may override the discriminants of the parent by giving a new [discriminant_part](S0056).

The parent type in a [derived_type_definition](S0032) may be a derived type defined in the same visible part.

When deriving from a type in the same visible part in which it is defined, the primitive subprograms declared prior to the derivation are inherited as primitive subprograms of the derived type. See 3.2.3. 


#### Wording Changes from Ada 83

We now talk about the classes to which a type belongs, rather than a single class.

As explained in Section 13, the concept of "storage pool" replaces the Ada 83 concept of "collection." These concepts are similar, but not the same. 


### 3.4.1  Derivation Classes

In addition to the various language-defined classes of types, types can be grouped into derivation classes. 


#### Static Semantics

A derived type is derived from its parent type directly; it is derived indirectly from any type from which its parent type is derived. The derivation class of types for a type T (also called the class rooted at T) is the set consisting of T (the root type of the class) and all types derived from T (directly or indirectly) plus any associated universal or class-wide types (defined below). 

Discussion: Note that the definition of "derived from" is a recursive definition. We don't define a root type for all interesting language-defined classes, though presumably we could. 

To be honest: By the class-wide type "associated" with a type T, we mean the type T'Class. Similarly, the universal type associated with root_integer, root_real, and root_fixed are universal_integer, universal_real, and universal_fixed, respectively. 

Every type is either a specific type, a class-wide type, or a universal type. A specific type is one defined by a [type_declaration](S0020), a [formal_type_declaration](S0246), or a full type definition embedded in a declaration for an object. Class-wide and universal types are implicitly defined, to act as representatives for an entire class of types, as follows: 

To be honest: The root types root_integer, root_real, and root_fixed are also specific types. They are declared in the specification of package Standard. 

Class-wide types Class-wide types are defined for [(and belong to)] each derivation class rooted at a tagged type (see 3.9). Given a subtype S of a tagged type T, S'Class is the [subtype_mark](S0025) for a corresponding subtype of the tagged class-wide type T'Class. Such types are called "class-wide" because when a formal parameter is defined to be of a class-wide type T'Class, an actual parameter of any type in the derivation class rooted at T is acceptable (see 8.6).

The set of values for a class-wide type T'Class is the discriminated union of the set of values of each specific type in the derivation class rooted at T (the tag acts as the implicit discriminant - see 3.9). Class-wide types have no primitive subprograms of their own. However, as explained in 3.9.2, operands of a class-wide type T'Class can be used as part of a dispatching call on a primitive subprogram of the type T. The only components [(including discriminants)] of T'Class that are visible are those of T. If S is a first subtype, then S'Class is a first subtype. 

Reason: We want S'Class to be a first subtype when S is, so that an [attribute_definition_clause](S0265) like "for S'Class'Output use ...;" will be legal. 

Universal types Universal types are defined for [(and belong to)] the integer, real, and fixed point classes, and are referred to in this standard as respectively, universal_integer, universal_real, and universal_fixed. These are analogous to class-wide types for these language-defined numeric classes. As with class-wide types, if a formal parameter is of a universal type, then an actual parameter of any type in the corresponding class is acceptable. In addition, a value of a universal type (including an integer or real [numeric_literal](S0004)) is "universal" in that it is acceptable where some particular type in the class is expected (see 8.6).

The set of values of a universal type is the undiscriminated union of the set of values possible for any definable type in the associated class. Like class-wide types, universal types have no primitive subprograms of their own. However, their "universality" allows them to be used as operands with the primitive subprograms of any type in the corresponding class. 

Discussion: A class-wide type is only class-wide in one direction, from specific to class-wide, whereas a universal type is class-wide (universal) in both directions, from specific to universal and back.

We considered defining class-wide or perhaps universal types for all derivation classes, not just tagged classes and these three numeric classes. However, this was felt to overly weaken the strong-typing model in some situations. Tagged types preserve strong type distinctions thanks to the run-time tag. Class-wide or universal types for untagged types would weaken the compile-time type distinctions without providing a compensating run-time-checkable distinction.

We considered defining standard names for the universal numeric types so they could be used in formal parameter specifications. However, this was felt to impose an undue implementation burden for some implementations. 

To be honest: Formally, the set of values of a universal type is actually a copy of the undiscriminated union of the values of the types in its class. This is because we want each value to have exactly one type, with explicit or implicit conversion needed to go between types. An alternative, consistent model would be to associate a class, rather than a particular type, with a value, even though any given expression would have a particular type. In that case, implicit type conversions would not generally need to change the value, although an associated subtype conversion might need to. 

The integer and real numeric classes each have a specific root type in addition to their universal type, named respectively root_integer and root_real.

A class-wide or universal type is said to cover all of the types in its class. A specific type covers only itself.

A specific type T2 is defined to be a descendant of a type T1 if T2 is the same as T1, or if T2 is derived (directly or indirectly) from T1. A class-wide type T2'Class is defined to be a descendant of type T1 if T2 is a descendant of T1. Similarly, the universal types are defined to be descendants of the root types of their classes. If a type T2 is a descendant of a type T1, then T1 is called an ancestor of T2. The ultimate ancestor of a type is the ancestor of the type that is not a descendant of any other type. 

Ramification: A specific type is a descendant of itself. Class-wide types are considered descendants of the corresponding specific type, and do not have any descendants of their own.

A specific type is an ancestor of itself. The root of a derivation class is an ancestor of all types in the class, including any class-wide types in the class. 

Discussion: The terms root, parent, ancestor, and ultimate ancestor are all related. For example: 

Each type has at most one parent, and one or more ancestor types; each type has exactly one ultimate ancestor. In Ada 83, the term "parent type" was sometimes used more generally to include any ancestor type (e.g. RM83-9.4(14)). In Ada 95, we restrict parent to mean the immediate ancestor.

A class of types has at most one root type; a derivation class has exactly one root type.

The root of a class is an ancestor of all of the types in the class (including itself).

The type root_integer is the root of the integer class, and is the ultimate ancestor of all integer types. A similar statement applies to root_real. 

Version=[5],Kind=(AddedNormal),Group=[T],Term=[ancestor of a type], Def=[the type itself or, in the case of a type derived from other types, its parent type or one of its progenitor types or one of their ancestors], Note1=[Ancestor and descendant are inverse relationships.]

Version=[5],Kind=(AddedNormal),Group=[T],Term=[descendant of a type], Def=[the type itself or a type derived (directly or indirectly) from it], Note1=[Descendant and ancestor are inverse relationships.]

An inherited component [(including an inherited discriminant)] of a derived type is inherited from a given ancestor of the type if the corresponding component was inherited by each derived type in the chain of derivations going back to the given ancestor.

NOTE   Because operands of a universal type are acceptable to the predefined operators of any type in their class, ambiguity can result. For universal_integer and universal_real, this potential ambiguity is resolved by giving a preference (see 8.6) to the predefined operators of the corresponding root types (root_integer and root_real, respectively). Hence, in an apparently ambiguous expression like 

1 + 4 &lt 7

where each of the literals is of type universal_integer, the predefined operators of root_integer will be preferred over those of other specific integer types, thereby resolving the ambiguity. 

Ramification: Except for this preference, a root numeric type is essentially like any other specific type in the associated numeric class. In particular, the result of a predefined operator of a root numeric type is not "universal" (implicitly convertible) even if both operands were. 


## 3.5  Scalar Types

Scalar types comprise enumeration types, integer types, and real types. Enumeration types and integer types are called discrete types; each value of a discrete type has a position number which is an integer value. Integer types and real types are called numeric types. [All scalar types are ordered, that is, all relational operators are predefined for their values.]


#### Syntax

range_constraint ::=  range [range](S0034)

range ::=  [range_attribute_reference](S0095)
   | [simple_expression](S0110) .. [simple_expression](S0110)

Discussion: These need to be [simple_expression](S0110)s rather than more general [expression](S0108)s because ranges appear in membership tests and other contexts where [expression](S0108) .. [expression](S0108) would be ambiguous. 

A range has a lower bound and an upper bound and specifies a subset of the values of some scalar type (the type of the range). A range with lower bound L and upper bound R is described by "L .. R". If R is less than L, then the range is a null range, and specifies an empty set of values. Otherwise, the range specifies the values of the type from the lower bound to the upper bound, inclusive. A value belongs to a range if it is of the type of the range, and is in the subset of values specified by the range. A value satisfies a range constraint if it belongs to the associated range. One range is included in another if all values that belong to the first range also belong to the second. 


#### Name Resolution Rules

For a [subtype_indication](S0024) containing a [range_constraint](S0033), either directly or as part of some other [scalar_constraint](S0027), the type of the [range](S0034) shall resolve to that of the type determined by the [subtype_mark](S0025) of the [subtype_indication](S0024). For a [range](S0034) of a given type, the [simple_expression](S0110)s of the [range](S0034) (likewise, the [simple_expression](S0110)s of the equivalent [range](S0034) for a [range_attribute_reference](S0095)) are expected to be of the type of the [range](S0034). 

Discussion: In Ada 95, [constraint](S0026)s only appear within [subtype_indication](S0024)s; things that look like constraints that appear in type declarations are called something else like [real_range_specification](S0043)s.

We say "the expected type is ..." or "the type is expected to be ..." depending on which reads better. They are fundamentally equivalent, and both feed into the type resolution rules of clause 8.6.

In some cases, it doesn't work to use expected types. For example, in the above rule, we say that the "type of the [range](S0034) shall resolve to ..." rather than "the expected type for the [range](S0034) is ..." We then use "expected type" for the bounds. If we used "expected" at both points, there would be an ambiguity, since one could apply the rules of 8.6 either on determining the type of the range, or on determining the types of the individual bounds. It is clearly important to allow one bound to be of a universal type, and the other of a specific type, so we need to use "expected type" for the bounds. Hence, we used "shall resolve to" for the type of the range as a whole. There are other situations where "expected type" is not quite right, and we use "shall resolve to" instead. 


#### Static Semantics

The base range of a scalar type is the range of finite values of the type that can be represented in every unconstrained object of the type; it is also the range supported at a minimum for intermediate values during the evaluation of expressions involving predefined operators of the type. 

Implementation Note: Note that in some machine architectures intermediates in an expression (particularly if static), and register-resident variables might accommodate a wider range. The base range does not include the values of this wider range that are not assignable without overflow to memory-resident objects.

Ramification: The base range of an enumeration type is the range of values of the enumeration type. 

Reason: If the representation supports infinities, the base range is nevertheless restricted to include only the representable finite values, so that 'Base'First and 'Base'Last are always guaranteed to be finite.

To be honest: By a "value that can be assigned without overflow" we don't mean to restrict ourselves to values that can be represented exactly. Values between machine representable values can be assigned, but on subsequent reading, a slightly different value might be retrieved, as (partially) determined by the number of digits of precision of the type. 

[A constrained scalar subtype is one to which a range constraint applies.] The range of a constrained scalar subtype is the range associated with the range constraint of the subtype. The range of an unconstrained scalar subtype is the base range of its type. 


#### Dynamic Semantics

A range is compatible with a scalar subtype if and only if it is either a null range or each bound of the range belongs to the range of the subtype. A [range_constraint](S0033) is compatible with a scalar subtype if and only if its range is compatible with the subtype. 

Ramification: Only [range_constraint](S0033)s (explicit or implicit) impose conditions on the values of a scalar subtype. The other [scalar_constraint](S0027)s, [digits_constraint](S0047)s and [delta_constraint](S0275)s impose conditions on the subtype denoted by the [subtype_mark](S0025) in a [subtype_indication](S0024), but don't impose a condition on the values of the subtype being defined. Therefore, a scalar subtype is not called constrained if all that applies to it is a [digits_constraint](S0047). Decimal subtypes are subtle, because a [digits_constraint](S0047) without a [range_constraint](S0033) nevertheless includes an implicit [range_constraint](S0033). 

The elaboration of a [range_constraint](S0033) consists of the evaluation of the [range](S0034). The evaluation of a [range](S0034) determines a lower bound and an upper bound. If [simple_expression](S0110)s are given to specify bounds, the evaluation of the [range](S0034) evaluates these [simple_expression](S0110)s in an arbitrary order, and converts them to the type of the [range](S0034). If a [range_attribute_reference](S0095) is given, the evaluation of the [range](S0034) consists of the evaluation of the [range_attribute_reference](S0095).

Attributes

For every scalar subtype S, the following attributes are defined: 

S'FirstS'First denotes the lower bound of the range of S. The value of this attribute is of the type of S. 

Ramification: Evaluating S'First never raises Constraint_Error.

S'LastS'Last denotes the upper bound of the range of S. The value of this attribute is of the type of S. 

Ramification: Evaluating S'Last never raises Constraint_Error.

S'RangeS'Range is equivalent to the [range](S0034) S'First .. S'Last.

S'BaseS'Base denotes an unconstrained subtype of the type of S. This unconstrained subtype is called the base subtype of the type. 

S'MinS'Min denotes a function with the following specification: 

```ada
function S'Min(Left, Right : S'Base)
  return S'Base

```

The function returns the lesser of the values of the two parameters. 

Discussion: The formal parameter names are italicized because they cannot be used in calls - see 6.4. Such a specification cannot be written by the user because an [attribute_reference](S0093) is not permitted as the designator of a user-defined function, nor can its formal parameters be anonymous. 

S'MaxS'Max denotes a function with the following specification: 

```ada
function S'Max(Left, Right : S'Base)
  return S'Base

```

The function returns the greater of the values of the two parameters.

S'SuccS'Succ denotes a function with the following specification: 

```ada
function S'Succ(Arg : S'Base)
  return S'Base

```

For an enumeration type, the function returns the value whose position number is one more than that of the value of Arg; Constraint_Error is raised if there is no such value of the type. For an integer type, the function returns the result of adding one to the value of Arg. For a fixed point type, the function returns the result of adding small to the value of Arg. For a floating point type, the function returns the machine number (as defined in 3.5.7) immediately above the value of Arg; Constraint_Error is raised if there is no such machine number. 

Ramification: S'Succ for a modular integer subtype wraps around if the value of Arg is S'Base'Last. S'Succ for a signed integer subtype might raise Constraint_Error if the value of Arg is S'Base'Last, or it might return the out-of-base-range value S'Base'Last+1, as is permitted for all predefined numeric operations. 

S'PredS'Pred denotes a function with the following specification: 

```ada
function S'Pred(Arg : S'Base)
  return S'Base

```

For an enumeration type, the function returns the value whose position number is one less than that of the value of Arg; Constraint_Error is raised if there is no such value of the type. For an integer type, the function returns the result of subtracting one from the value of Arg. For a fixed point type, the function returns the result of subtracting small from the value of Arg. For a floating point type, the function returns the machine number (as defined in 3.5.7) immediately below the value of Arg; Constraint_Error is raised if there is no such machine number. 

Ramification: S'Pred for a modular integer subtype wraps around if the value of Arg is S'Base'First. S'Pred for a signed integer subtype might raise Constraint_Error if the value of Arg is S'Base'First, or it might return the out-of-base-range value S'Base'First1, as is permitted for all predefined numeric operations. 



S'Wide_ImageS'Wide_Image denotes a function with the following specification:

```ada
function S'Wide_Image(Arg : S'Base)
  return Wide_String

```

The function returns an image of the value of Arg, that is, a sequence of characters representing the value in display form. The lower bound of the result is one.

Implementation defined: 

The image of an integer value is the corresponding decimal literal, without underlines, leading zeros, exponent, or trailing spaces, but with a single leading character that is either a minus sign or a space. 

Implementation Note: If the machine supports negative zeros for signed integer types, it is not specified whether "0" or " 0" should be returned for negative zero. We don't have enough experience with such machines to know what is appropriate, and what other languages do. In any case, the implementation should be consistent. 

The image of an enumeration value is either the corresponding identifier in upper case or the corresponding character literal (including the two apostrophes); neither leading nor trailing spaces are included. For a nongraphic character (a value of a character type that has no enumeration literal associated with it), the result is a corresponding language-defined or implementation-defined name in upper case (for example, the image of the nongraphic character identified as nul is "NUL" - the quotes are not part of the image). 

Implementation Note: For an enumeration type T that has "holes" (caused by an [enumeration_representation_clause](S0266)), T'Wide_Image should raise Program_Error if the value is one of the holes (which is a bounded error anyway, since holes can be generated only via uninitialized variables and similar things). 

The image of a floating point value is a decimal real literal best approximating the value (rounded away from zero if halfway between) with a single leading character that is either a minus sign or a space, a single digit (that is nonzero unless the value is zero), a decimal point, S'Digits1 (see 3.5.8) digits after the decimal point (but one if S'Digits is one), an upper case E, the sign of the exponent (either + or ), and two or more digits (with leading zeros if necessary) representing the exponent. If S'Signed_Zeros is True, then the leading character is a minus sign for a negatively signed zero. 

To be honest: Leading zeros are present in the exponent only if necessary to make the exponent at least two digits. 

Reason: This image is intended to conform to that produced by Text_IO.Float_IO.Put in its default format. 

Implementation Note: The rounding direction is specified here to ensure portability of output results. 

The image of a fixed point value is a decimal real literal best approximating the value (rounded away from zero if halfway between) with a single leading character that is either a minus sign or a space, one or more digits before the decimal point (with no redundant leading zeros), a decimal point, and S'Aft (see 3.5.10) digits after the decimal point. 

Reason: This image is intended to conform to that produced by Text_IO.Fixed_IO.Put. 

Implementation Note: The rounding direction is specified here to ensure portability of output results. 

Implementation Note: For a machine that supports negative zeros, it is not specified whether "0.000" or " 0.000" is returned. See corresponding comment above about integer types with signed zeros. 

S'ImageS'Image denotes a function with the following specification:

```ada
function S'Image(Arg : S'Base)
  return String

```

The function returns an image of the value of Arg as a String. The lower bound of the result is one. The image has the same sequence of graphic characters as that defined for S'Wide_Image if all the graphic characters are defined in Character; otherwise the sequence of characters is implementation defined (but no shorter than that of S'Wide_Image for the same value of Arg).

Implementation defined: The sequence of characters of the value returned by S'Image when some of the graphic characters of S'Wide_Image are not defined in Character.



S'Wide_WidthS'Wide_Width denotes the maximum length of a Wide_String returned by S'Wide_Image over all values of the subtype S. It denotes zero for a subtype that has a null range. Its type is universal_integer.

S'WidthS'Width denotes the maximum length of a String returned by S'Image over all values of the subtype S. It denotes zero for a subtype that has a null range. Its type is universal_integer.



S'Wide_ValueS'Wide_Value denotes a function with the following specification: 

```ada
function S'Wide_Value(Arg : Wide_String)
  return S'Base

```

This function returns a value given an image of the value as a Wide_String, ignoring any leading or trailing spaces.

For the evaluation of a call on S'Wide_Value for an enumeration subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of an enumeration literal and if it corresponds to a literal of the type of S (or corresponds to the result of S'Wide_Image for a nongraphic character of the type), the result is the corresponding enumeration value; otherwise Constraint_Error is raised. 

Discussion: It's not crystal clear that Range_Check is appropriate here, but it doesn't seem worthwhile to invent a whole new check name just for this weird case, so we decided to lump it in with Range_Check. 

For the evaluation of a call on S'Wide_Value (or S'Value) for an integer subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of an integer literal, with an optional leading sign character (plus or minus for a signed type; only plus for a modular type), and the corresponding numeric value belongs to the base range of the type of S, then that value is the result; otherwise Constraint_Error is raised.

Discussion: We considered allowing 'Value to return a representable but out-of-range value without a Constraint_Error. However, we currently require (see 4.9) in an [assignment_statement](S0130) like "X := &ltnumeric_literal&gt;" that the value of the numeric-literal be in X's base range (at compile time), so it seems unfriendly and confusing to have a different range allowed for 'Value. Furthermore, for modular types, without the requirement for being in the base range, 'Value would have to handle arbitrarily long literals (since overflow never occurs for modular types). 

For the evaluation of a call on S'Wide_Value (or S'Value) for a real subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of one of the following: 

[numeric_literal](S0004)

[numeral](S0006).[[exponent](S0007)]

.[numeral](S0006)[[exponent](S0007)]

[base](S0009)#[based_numeral](S0010).#[[exponent](S0007)]

[base](S0009)#.[based_numeral](S0010)#[[exponent](S0007)] 

with an optional leading sign character (plus or minus), and if the corresponding numeric value belongs to the base range of the type of S, then that value is the result; otherwise Constraint_Error is raised. The sign of a zero value is preserved (positive if none has been specified) if S'Signed_Zeros is True.

S'ValueS'Value denotes a function with the following specification: 

```ada
function S'Value(Arg : String)
  return S'Base

```

This function returns a value given an image of the value as a String, ignoring any leading or trailing spaces.

For the evaluation of a call on S'Value for an enumeration subtype S, if the sequence of characters of the parameter (ignoring leading and trailing spaces) has the syntax of an enumeration literal and if it corresponds to a literal of the type of S (or corresponds to the result of S'Image for a value of the type), the result is the corresponding enumeration value; otherwise Constraint_Error is raised. For a numeric subtype S, the evaluation of a call on S'Value with Arg of type String is equivalent to a call on S'Wide_Value for a corresponding Arg of type Wide_String. 

Reason: S'Value is subtly different from S'Wide_Value for enumeration subtypessince S'Image might produce a different sequence of characters than S'Wide_Image if the enumeration literal uses characters outside of the predefined type Character. That is why we don't just define S'Value in terms of S'Wide_Value for enumeration subtypes. S'Value and S'Wide_Value for numeric subtypes yield the same result given the same sequence of characters. 








#### Implementation Permissions

An implementation may extend the Wide_Value, [Value, Wide_Image, and Image] attributes of a floating point type to support special values such as infinities and NaNs.

Proof: The permission is really only necessary for Wide_Value, because Value is defined in terms of Wide_Value, and because the behavior of Wide_Image and Image is already unspecified for things like infinities and NaNs. 

Reason: This is to allow implementations to define full support for IEEE arithmetic. See also the similar permission for Get in A.10.9. 

NOTE 1   The evaluation of S'First or S'Last never raises an exception. If a scalar subtype S has a nonnull range, S'First and S'Last belong to this range. These values can, for example, always be assigned to a variable of subtype S. 

Discussion: This paragraph addresses an issue that came up with Ada 83, where for fixed point types, the end points of the range specified in the type definition were not necessarily within the base range of the type. However, it was later clarified (and we reconfirm it in 3.5.9, "Fixed Point Types") that the First and Last attributes reflect the true bounds chosen for the type, not the bounds specified in the type definition (which might be outside the ultimately chosen base range). 

NOTE 2   For a subtype of a scalar type, the result delivered by the attributes Succ, Pred, and Value might not belong to the subtype; similarly, the actual parameters of the attributes Succ, Pred, and Image need not belong to the subtype.

NOTE 3   For any value V (including any nongraphic character) of an enumeration subtype S, S'Value(S'Image(V)) equals V, as does S'Wide_Value(S'Wide_Image(V)). Neither expression ever raises Constraint_Error. 


#### Examples

Examples of ranges: 

```ada
-10 .. 10
X .. X + 1
0.0 .. 2.0*Pi
Red .. Green     -- see 3.5.1
1 .. 0           -- a null range
Table'Range      -- a range attribute reference (see 3.6)

```

Examples of range constraints: 

```ada
range -999.0 .. +999.0
range S'First+1 .. S'Last-1

```


#### Incompatibilities With Ada 83

S'Base is no longer defined for nonscalar types. One conceivable existing use of S'Base for nonscalar types is S'Base'Size where S is a generic formal private type. However, that is not generally useful because the actual subtype corresponding to S might be a constrained array or discriminated type, which would mean that S'Base'Size might very well overflow (for example, S'Base'Size where S is a constrained subtype of String will generally be 8 * (Integer'Last + 1)). For derived discriminated types that are packed, S'Base'Size might not even be well defined if the first subtype is constrained, thereby allowing some amount of normally required "dope" to have been squeezed out in the packing. Hence our conclusion is that S'Base'Size is not generally useful in a generic, and does not justify keeping the attribute Base for nonscalar types just so it can be used as a prefix.


#### Extensions to Ada 83

The attribute S'Base for a scalar subtype is now permitted anywhere a [subtype_mark](S0025) is permitted. S'Base'First .. S'Base'Last is the base range of the type. Using an [attribute_definition_clause](S0265), one cannot specify any subtype-specific attributes for the subtype denoted by S'Base (the base subtype).

The attribute S'Range is now allowed for scalar subtypes.

The attributes S'Min and S'Max are now defined, and made available for all scalar types.

The attributes S'Succ, S'Pred, S'Image, S'Value, and S'Width are now defined for real types as well as discrete types.

Wide_String versions of S'Image and S'Value are defined. These are called S'Wide_Image and S'Wide_Value to avoid introducing ambiguities involving uses of these attributes with string literals. 


#### Wording Changes from Ada 83

We now use the syntactic category [range_attribute_reference](S0095) since it is now syntactically distinguished from other attribute references.

The definition of S'Base has been moved here from 3.3.3 since it now applies only to scalar types.

More explicit rules are provided for nongraphic characters. 


### 3.5.1  Enumeration Types

[ An [enumeration_type_definition](S0035) defines an enumeration type.] 


#### Syntax

enumeration_type_definition ::= 
   ([enumeration_literal_specification](S0036) {, [enumeration_literal_specification](S0036)})

enumeration_literal_specification ::=  [defining_identifier](S0019) | [defining_character_literal](S0037)

defining_character_literal ::= [character_literal](S0012)


#### Legality Rules

The [defining_identifier](S0019)s [and [defining_character_literal](S0037)s] listed in an [enumeration_type_definition](S0035) shall be distinct. 

Proof: This is a ramification of the normal disallowance of homographs explicitly declared immediately in the same declarative region. 


#### Static Semantics

Each [enumeration_literal_specification](S0036) is the explicit declaration of the corresponding enumeration literal: it declares a parameterless function, whose defining name is the [defining_identifier](S0019) or [defining_character_literal](S0037), and whose result type is the enumeration type. 

Reason: This rule defines the profile of the enumeration literal, which is used in the various types of conformance. 

Ramification: The parameterless function associated with an enumeration literal is fully defined by the [enumeration_type_definition](S0035); a body is not permitted for it, and it never fails the Elaboration_Check when called. 

Each enumeration literal corresponds to a distinct value of the enumeration type, and to a distinct position number. The position number of the value of the first listed enumeration literal is zero; the position number of the value of each subsequent enumeration literal is one more than that of its predecessor in the list.

[The predefined order relations between values of the enumeration type follow the order of corresponding position numbers.]

[ If the same [defining_identifier](S0019) or [defining_character_literal](S0037) is specified in more than one [enumeration_type_definition](S0035), the corresponding enumeration literals are said to be overloaded. At any place where an overloaded enumeration literal occurs in the text of a program, the type of the enumeration literal has to be determinable from the context (see 8.6).] 


#### Dynamic Semantics

The elaboration of an [enumeration_type_definition](S0035) creates the enumeration type and its first subtype, which is constrained to the base range of the type. 

Ramification: The first subtype of a discrete type is always constrained, except in the case of a derived type whose parent subtype is Whatever'Base. 

When called, the parameterless function associated with an enumeration literal returns the corresponding value of the enumeration type. 

NOTE 1   If an enumeration literal occurs in a context that does not otherwise suffice to determine the type of the literal, then qualification by the name of the enumeration type is one way to resolve the ambiguity (see 4.7). 


#### Examples

Examples of enumeration types and subtypes: 

```ada
type Day        is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
type Suit       is (Clubs, Diamonds, Hearts, Spades);
type Gender     is (M, F);
type Level      is (Low, Medium, Urgent);
type Color      is (White, Red, Yellow, Green, Blue, Brown, Black);
type Light      is (Red, Amber, Green); -- Red and Green are overloaded

```

```ada
type Hexa       is ('A', 'B', 'C', 'D', 'E', 'F');
type Mixed      is ('A', 'B', '*', B, None, '?', '%');

```

```ada
subtype Weekday is Day   range Mon .. Fri;
subtype Major   is Suit  range Hearts .. Spades;
subtype Rainbow is Color range Red .. Blue;  --  the Color Red, not the Light

```


#### Wording Changes from Ada 83

The syntax rule for [defining_character_literal](S0037) is new. It is used for the defining occurrence of a [character_literal](S0012), analogously to [defining_identifier](S0019). Usage occurrences use the [name](S0084) or [selector_name](S0092) syntactic categories.

We emphasize the fact that an enumeration literal denotes a function, which is called to produce a value. 


### 3.5.2  Character Types


#### Static Semantics

An enumeration type is said to be a character type if at least one of its enumeration literals is a [character_literal](S0012).

The predefined type Character is a character type whose values correspond to the 256 code positions of Row 00 (also known as Latin-1) of the ISO 10646 Basic Multilingual Plane (BMP). Each of the graphic characters of Row 00 of the BMP has a corresponding [character_literal](S0012) in Character. Each of the nongraphic positions of Row 00 (0000-001F and 007F-009F) has a corresponding language-defined name, which is not usable as an enumeration literal, but which is usable with the attributes (Wide_)Image and (Wide_)Value; these names are given in the definition of type Character in A.1, "The Package Standard", but are set in italics. 

The predefined type Wide_Character is a character type whose values correspond to the 65536 code positions of the ISO 10646 Basic Multilingual Plane (BMP). Each of the graphic characters of the BMP has a corresponding [character_literal](S0012) in Wide_Character. The first 256 values of Wide_Character have the same [character_literal](S0012) or language-defined name as defined for Character. The last 2 values of Wide_Character correspond to the nongraphic positions FFFE and FFFF of the BMP, and are assigned the language-defined names FFFE and FFFF. As with the other language-defined names for nongraphic characters, the names FFFE and FFFF are usable only with the attributes (Wide_)Image and (Wide_)Value; they are not usable as enumeration literals. All other values of Wide_Character are considered graphic characters, and have a corresponding [character_literal](S0012).

Reason: The language-defined names are not usable as enumeration literals to avoid "polluting" the name space. Since Wide_Character are defined in Standard, if the names FFFE and FFFF were usable as enumeration literals, they would hide other nonoverloadable declarations with the same names in use-d packages.

ISO 10646 has not defined the meaning of all of the code positions from 0100 through FFFD, but they are all considered graphic characters by Ada to simplify the implementation, and to allow for revisions to ISO 10646. In ISO 10646, FFFE and FFFF are special, and will never be associated with graphic characters in any revision. 


#### Implementation Permissions

In a nonstandard mode, an implementation may provide other interpretations for the predefined types Character and Wide_Character[, to conform to local conventions]. 


#### Implementation Advice

If an implementation supports a mode with alternative interpretations for Character and Wide_Character, the set of graphic characters of Character should nevertheless remain a proper subset of the set of graphic characters of Wide_Character. Any character set "localizations" should be reflected in the results of the subprograms defined in the language-defined package Characters.Handling (see A.3) available in such a mode. In a mode with an alternative interpretation of Character, the implementation should also support a corresponding change in what is a legal identifier_letter. 

NOTE 1   The language-defined library package Characters.Latin_1 (see A.3.3) includes the declaration of constants denoting control characters, lower case characters, and special characters of the predefined type Character. 

To be honest: The package ASCII does the same, but only for the first 128 characters of Character. Hence, it is an obsolescent package, and we no longer mention it here. 

NOTE 2   A conventional character set such as EBCDIC can be declared as a character type; the internal codes of the characters can be specified by an [enumeration_representation_clause](S0266) as explained in clause 13.4. 


#### Examples

Example of a character type: 

```ada
type Roman_Digit is ('I', 'V', 'X', 'L', 'C', 'D', 'M');

```


#### Inconsistencies With Ada 83

The declaration of Wide_Character in package Standard hides use-visible declarations with the same defining identifier. In the unlikely event that an Ada 83 program had depended on such a use-visible declaration, and the program remains legal after the substitution of Standard.Wide_Character, the meaning of the program will be different. 


#### Incompatibilities With Ada 83

The presence of Wide_Character in package Standard means that an expression such as 

```ada
'a' = 'b'

```

is ambiguous in Ada 95, whereas in Ada 83 both literals could be resolved to be of type Character.

The change in visibility rules (see 4.2) for character literals means that additional qualification might be necessary to resolve expressions involving overloaded subprograms and character literals. 


#### Extensions to Ada 83

The type Character has been extended to have 256 positions, and the type Wide_Character has been added. Note that this change was already approved by the ARG for Ada 83 conforming compilers.

The rules for referencing character literals are changed (see 4.2), so that the declaration of the character type need not be directly visible to use its literals, similar to null and string literals. Context is used to resolve their type. 


### 3.5.3  Boolean Types


#### Static Semantics

There is a predefined enumeration type named Boolean, [declared in the visible part of package Standard]. It has the two enumeration literals False and True ordered with the relation False &lt True. Any descendant of the predefined type Boolean is called a boolean type. 

Implementation Note: An implementation is not required to support enumeration representation clauses on boolean types that impose an unacceptable implementation burden. See 13.4, "Enumeration Representation Clauses". However, it is generally straightforward to support representations where False is zero and True is 2**n  1 for some n. 


### 3.5.4  Integer Types

An [integer_type_definition](S0038) defines an integer type; it defines either a signed integer type, or a modular integer type. The base range of a signed integer type includes at least the values of the specified range. A modular type is an integer type with all arithmetic modulo a specified positive modulus; such a type corresponds to an unsigned type with wrap-around semantics. 


#### Syntax

integer_type_definition ::= [signed_integer_type_definition](S0039) | [modular_type_definition](S0040)

signed_integer_type_definition ::= range static_[simple_expression](S0110) .. static_[simple_expression](S0110)

Discussion: We don't call this a [range_constraint](S0033), because it is rather different - not only is it required to be static, but the associated overload resolution rules are different than for normal range constraints. A similar comment applies to [real_range_specification](S0043). This used to be integer_range_specification but when we added support for modular types, it seemed overkill to have three levels of syntax rules, and just calling these signed_integer_range_specification and modular_range_specification loses the fact that they are defining different classes of types, which is important for the generic type matching rules. 

modular_type_definition ::= mod static_[expression](S0108)


#### Name Resolution Rules

Each [simple_expression](S0110) in a [signed_integer_type_definition](S0039) is expected to be of any integer type; they need not be of the same type. The [expression](S0108) in a [modular_type_definition](S0040) is likewise expected to be of any integer type. 


#### Legality Rules

The [simple_expression](S0110)s of a [signed_integer_type_definition](S0039) shall be static, and their values shall be in the range System.Min_Int .. System.Max_Int.

The [expression](S0108) of a [modular_type_definition](S0040) shall be static, and its value (the modulus) shall be positive, and shall be no greater than System.Max_Binary_Modulus if a power of 2, or no greater than System.Max_Nonbinary_Modulus if not. 

Reason: For a 2's-complement machine, supporting nonbinary moduli greater than System.Max_Int can be quite difficult, whereas essentially any binary moduli are straightforward to support, up to 2*System.Max_Int+2, so this justifies having two separate limits. 


#### Static Semantics

The set of values for a signed integer type is the (infinite) set of mathematical integers[, though only values of the base range of the type are fully supported for run-time operations]. The set of values for a modular integer type are the values from 0 to one less than the modulus, inclusive.

A [signed_integer_type_definition](S0039) defines an integer type whose base range includes at least the values of the [simple_expression](S0110)s and is symmetric about zero, excepting possibly an extra negative value. A [signed_integer_type_definition](S0039) also defines a constrained first subtype of the type, with a range whose bounds are given by the values of the [simple_expression](S0110)s, converted to the type being defined. 

Implementation Note: The base range of a signed integer type might be much larger than is necessary to satisfy the aboved requirements. 

A [modular_type_definition](S0040) defines a modular type whose base range is from zero to one less than the given modulus. A [modular_type_definition](S0040) also defines a constrained first subtype of the type with a range that is the same as the base range of the type.

There is a predefined signed integer subtype named Integer[, declared in the visible part of package Standard]. It is constrained to the base range of its type. 

Reason: Integer is a constrained subtype, rather than an unconstrained subtype. This means that on assignment to an object of subtype Integer, a range check is required. On the other hand, an object of subtype Integer'Base is unconstrained, and no range check (only overflow check) is required on assignment. For example, if the object is held in an extended-length register, its value might be outside of Integer'First .. Integer'Last. All parameter and result subtypes of the predefined integer operators are of such unconstrained subtypes, allowing extended-length registers to be used as operands or for the result. In an earlier version of Ada 95, Integer was unconstrained. However, the fact that certain Constraint_Errors might be omitted or appear elsewhere was felt to be an undesirable upward inconsistency in this case. Note that for Float, the opposite conclusion was reached, partly because of the high cost of performing range checks when not actually necessary. Objects of subtype Float are unconstrained, and no range checks, only overflow checks, are performed for them. 

Integer has two predefined subtypes, [declared in the visible part of package Standard:] 

```ada
subtype Natural  is Integer range 0 .. Integer'Last;
subtype Positive is Integer range 1 .. Integer'Last;

```

A type defined by an [integer_type_definition](S0038) is implicitly derived from root_integer, an anonymous predefined (specific) integer type, whose base range is System.Min_Int .. System.Max_Int. However, the base range of the new type is not inherited from root_integer, but is instead determined by the range or modulus specified by the [integer_type_definition](S0038). [Integer literals are all of the type universal_integer, the universal type (see 3.4.1) for the class rooted at root_integer, allowing their use with the operations of any integer type.] 

Discussion: This implicit derivation is not considered exactly equivalent to explicit derivation via a [derived_type_definition](S0032). In particular, integer types defined via a [derived_type_definition](S0032) inherit their base range from their parent type. A type defined by an [integer_type_definition](S0038) does not necessarily inherit its base range from root_integer. It is not specified whether the implicit derivation from root_integer is direct or indirect, not that it really matters. All we want is for all integer types to be descendants of root_integer.

Implementation Note: It is the intent that even nonstandard integer types (see below) will be descendants of root_integer, even though they might have a base range that exceeds that of root_integer. This causes no problem for static calculations, which are performed without range restrictions (see 4.9). However for run-time calculations, it is possible that Constraint_Error might be raised when using an operator of root_integer on the result of 'Val applied to a value of a nonstandard integer type. 

The position number of an integer value is equal to the value.

For every modular subtype S, the following attribute is defined: 



S'ModulusS'Modulus yields the modulus of the type of S, as a value of the type universal_integer. 


#### Dynamic Semantics

The elaboration of an [integer_type_definition](S0038) creates the integer type and its first subtype.

For a modular type, if the result of the execution of a predefined operator (see 4.5) is outside the base range of the type, the result is reduced modulo the modulus of the type to a value that is within the base range of the type.

For a signed integer type, the exception Constraint_Error is raised by the execution of an operation that cannot deliver the correct result because it is outside the base range of the type. [ For any integer type, Constraint_Error is raised by the operators "/", "rem", and "mod" if the right operand is zero.]


#### Implementation Requirements

In an implementation, the range of Integer shall include the range 2**15+1 .. +2**151.

If Long_Integer is predefined for an implementation, then its range shall include the range 2**31+1 .. +2**311.

System.Max_Binary_Modulus shall be at least 2**16. 


#### Implementation Permissions

For the execution of a predefined operation of a signed integer type, the implementation need not raise Constraint_Error if the result is outside the base range of the type, so long as the correct result is produced. 

Discussion: Constraint_Error is never raised for operations on modular types, except for divide-by-zero (and rem/mod-by-zero). 

An implementation may provide additional predefined signed integer types[, declared in the visible part of Standard], whose first subtypes have names of the form Short_Integer, Long_Integer, Short_Short_Integer, Long_Long_Integer, etc. Different predefined integer types are allowed to have the same base range. However, the range of Integer should be no wider than that of Long_Integer. Similarly, the range of Short_Integer (if provided) should be no wider than Integer. Corresponding recommendations apply to any other predefined integer types. There need not be a named integer type corresponding to each distinct base range supported by an implementation. The range of each first subtype should be the base range of its type. 

Implementation defined: The predefined integer types declared in Standard.

An implementation may provide nonstandard integer types, descendants of root_integer that are declared outside of the specification of package Standard, which need not have all the standard characteristics of a type defined by an [integer_type_definition](S0038). For example, a nonstandard integer type might have an asymmetric base range or it might not be allowed as an array or loop index (a very long integer). Any type descended from a nonstandard integer type is also nonstandard. An implementation may place arbitrary restrictions on the use of such types; it is implementation defined whether operators that are predefined for "any integer type" are defined for a particular nonstandard integer type. [In any case, such types are not permitted as [explicit_generic_actual_parameter](S0244)s for formal scalar types - see 12.5.2.] 

Implementation defined: Any nonstandard integer types and the operators defined for them.

For a one's complement machine, the high bound of the base range of a modular type whose modulus is one less than a power of 2 may be equal to the modulus, rather than one less than the modulus. It is implementation defined for which powers of 2, if any, this permission is exercised.


#### Implementation Advice

An implementation should support Long_Integer in addition to Integer if the target machine supports 32-bit (or longer) arithmetic. No other named integer subtypes are recommended for package Standard. Instead, appropriate named integer subtypes should be provided in the library package Interfaces (see B.2). 

Implementation Note: To promote portability, implementations should explicitly declare the integer (sub)types Integer and Long_Integer in Standard, and leave other predefined integer types anonymous. For implementations that already support Byte_Integer, etc., upward compatibility argues for keeping such declarations in Standard during the transition period, but perhaps generating a warning on use. A separate package Interfaces in the predefined environment is available for pre-declaring types such as Integer_8, Integer_16, etc. See B.2. In any case, if the user declares a subtype (first or not) whose range fits in, for example, a byte, the implementation can store variables of the subtype in a single byte, even if the base range of the type is wider. 

An implementation for a two's complement machine should support modular types with a binary modulus up to System.Max_Int*2+2. An implementation should support a nonbinary modulus up to Integer'Last. 

Reason: Modular types provide bit-wise "and", "or", "xor", and "not" operations. It is important for systems programming that these be available for all integer types of the target hardware. 

Ramification: Note that on a one's complement machine, the largest supported modular type would normally have a nonbinary modulus. On a two's complement machine, the largest supported modular type would normally have a binary modulus. 

Implementation Note: Supporting a nonbinary modulus greater than Integer'Last can impose an undesirable implementation burden on some machines. 

NOTE 1   Integer literals are of the anonymous predefined integer type universal_integer. Other integer types have no literals. However, the overload resolution rules (see 8.6, "The Context of Overload Resolution") allow expressions of the type universal_integer whenever an integer type is expected.

NOTE 2   The same arithmetic operators are predefined for all signed integer types defined by a [signed_integer_type_definition](S0039) (see 4.5, "Operators and Expression Evaluation"). For modular types, these same operators are predefined, plus bit-wise logical operators (and, or, xor, and not). In addition, for the unsigned types declared in the language-defined package Interfaces (see B.2), functions are defined that provide bit-wise shifting and rotating.

NOTE 3   Modular types match a [generic_formal_parameter_declaration](S0240) of the form "type T is mod &lt&gt;"; signed integer types match "type T is range &lt&gt;" (see 12.5.2). 


#### Examples

Examples of integer types and subtypes: 

```ada
type Page_Num  is range 1 .. 2_000;
type Line_Size is range 1 .. Max_Line_Size;

```

```ada
subtype Small_Int   is Integer   range -10 .. 10;
subtype Column_Ptr  is Line_Size range 1 .. 10;
subtype Buffer_Size is Integer   range 0 .. Max;

```

```ada
type Byte        is mod 256; -- an unsigned byte
type Hash_Index  is mod 97;  -- modulus is prime

```


#### Extensions to Ada 83

An implementation is allowed to support any number of distinct base ranges for integer types, even if fewer integer types are explicitly declared in Standard.

Modular (unsigned, wrap-around) types are new. 


#### Wording Changes from Ada 83

Ada 83's integer types are now called "signed" integer types, to contrast them with "modular" integer types.

Standard.Integer, Standard.Long_Integer, etc., denote constrained subtypes of predefined integer types, consistent with the Ada 95 model that only subtypes have names.

We now impose minimum requirements on the base range of Integer and Long_Integer.

We no longer explain integer type definition in terms of an equivalence to a normal type derivation, except to say that all integer types are by definition implicitly derived from root_integer. This is for various reasons.

First of all, the equivalence with a type derivation and a subtype declaration was not perfect, and was the source of various AIs (for example, is the conversion of the bounds static? Is a numeric type a derived type with respect to other rules of the language?)

Secondly, we don't want to require that every integer size supported shall have a corresponding named type in Standard. Adding named types to Standard creates nonportabilities.

Thirdly, we don't want the set of types that match a formal derived type "type T is new Integer;" to depend on the particular underlying integer representation chosen to implement a given user-defined integer type. Hence, we would have needed anonymous integer types as parent types for the implicit derivation anyway. We have simply chosen to identify only one anonymous integer type - root_integer, and stated that every integer type is derived from it.

Finally, the "fiction" that there were distinct preexisting predefined types for every supported representation breaks down for fixed point with arbitrary smalls, and was never exploited for enumeration types, array types, etc. Hence, there seems little benefit to pushing an explicit equivalence between integer type definition and normal type derivation. 


### 3.5.5  Operations of Discrete Types


#### Static Semantics

For every discrete subtype S, the following attributes are defined: 

S'PosS'Pos denotes a function with the following specification: 

```ada
function S'Pos(Arg : S'Base)
  return universal_integer

```

This function returns the position number of the value of Arg, as a value of type universal_integer.

S'ValS'Val denotes a function with the following specification: 

```ada
function S'Val(Arg : universal_integer)
  return S'Base

```

This function returns a value of the type of S whose position number equals the value of Arg. For the evaluation of a call on S'Val, if there is no value in the base range of its type with the given position number, Constraint_Error is raised. 

Ramification: By the overload resolution rules, a formal parameter of type universal_integer allows an actual parameter of any integer type.

Reason: We considered allowing S'Val for a signed integer subtype S to return an out-of-range value, but since checks were required for enumeration and modular types anyway, the allowance didn't seem worth the complexity of the rule.






#### Implementation Advice

For the evaluation of a call on S'Pos for an enumeration subtype, if the value of the operand does not correspond to the internal code for any enumeration literal of its type [(perhaps due to an uninitialized variable)], then the implementation should raise Program_Error. This is particularly important for enumeration types with noncontiguous internal codes specified by an [enumeration_representation_clause](S0266). 

Reason: We say Program_Error here, rather than Constraint_Error, because the main reason for such values is uninitialized variables, and the normal way to indicate such a use (if detected) is to raise Program_Error. (Other reasons would involve the misuse of low-level features such as Unchecked_Conversion.) 

NOTE 1   Indexing and loop iteration use values of discrete types.

NOTE 2   The predefined operations of a discrete type include the assignment operation, qualification, the membership tests, and the relational operators; for a boolean type they include the short-circuit control forms and the logical operators; for an integer type they include type conversion to and from other numeric types, as well as the binary and unary adding operators  and +, the multiplying operators, the unary operator abs, and the exponentiation operator. The assignment operation is described in 5.2. The other predefined operations are described in Section 4.

NOTE 3   As for all types, objects of a discrete type have Size and Address attributes (see 13.3).

NOTE 4   For a subtype of a discrete type, the result delivered by the attribute Val might not belong to the subtype; similarly, the actual parameter of the attribute Pos need not belong to the subtype. The following relations are satisfied (in the absence of an exception) by these attributes: 

```ada
   S'Val(S'Pos(X)) = X
   S'Pos(S'Val(N)) = N

```


#### Examples

Examples of attributes of discrete subtypes: 

```ada
--  For the types and subtypes declared in subclause 3.5.1 the following hold: 

```

```ada
--  Color'First   = White,   Color'Last   = Black
--  Rainbow'First = Red,     Rainbow'Last = Blue

```

```ada
--  Color'Succ(Blue) = Rainbow'Succ(Blue) = Brown
--  Color'Pos(Blue)  = Rainbow'Pos(Blue)  = 4
--  Color'Val(0)     = Rainbow'Val(0)     = White

```


#### Extensions to Ada 83

The attributes S'Succ, S'Pred, S'Width, S'Image, and S'Value have been generalized to apply to real types as well (see 3.5, "Scalar Types"). 


### 3.5.6  Real Types

Real types provide approximations to the real numbers, with relative bounds on errors for floating point types, and with absolute bounds for fixed point types. 


#### Syntax

real_type_definition ::= 
   [floating_point_definition](S0042) | [fixed_point_definition](S0044)


#### Static Semantics

A type defined by a [real_type_definition](S0041) is implicitly derived from root_real, an anonymous predefined (specific) real type. [Hence, all real types, whether floating point or fixed point, are in the derivation class rooted at root_real.] 

Ramification: It is not specified whether the derivation from root_real is direct or indirect, not that it really matters. All we want is for all real types to be descendants of root_real.

[ Real literals are all of the type universal_real, the universal type (see 3.4.1) for the class rooted at root_real, allowing their use with the operations of any real type. Certain multiplying operators have a result type of universal_fixed (see 4.5.5), the universal type for the class of fixed point types, allowing the result of the multiplication or division to be used where any specific fixed point type is expected.] 


#### Dynamic Semantics

The elaboration of a [real_type_definition](S0041) consists of the elaboration of the [floating_point_definition](S0042) or the [fixed_point_definition](S0044). 


#### Implementation Requirements

An implementation shall perform the run-time evaluation of a use of a predefined operator of root_real with an accuracy at least as great as that of any floating point type definable by a [floating_point_definition](S0042). 

Ramification: Static calculations using the operators of root_real are exact, as for all static calculations. See 4.9. 

Implementation Note: The Digits attribute of the type used to represent root_real at run time is at least as great as that of any other floating point type defined by a [floating_point_definition](S0042), and its safe range includes that of any such floating point type with the same Digits attribute. On some machines, there might be real types with less accuracy but a wider range, and hence run-time calculations with root_real might not be able to accommodate all values that can be represented at run time in such floating point or fixed point types. 


#### Implementation Permissions

[For the execution of a predefined operation of a real type, the implementation need not raise Constraint_Error if the result is outside the base range of the type, so long as the correct result is produced, or the Machine_Overflows attribute of the type is false (see G.2).]

An implementation may provide nonstandard real types, descendants of root_real that are declared outside of the specification of package Standard, which need not have all the standard characteristics of a type defined by a [real_type_definition](S0041). For example, a nonstandard real type might have an asymmetric or unsigned base range, or its predefined operations might wrap around or "saturate" rather than overflow (modular or saturating arithmetic), or it might not conform to the accuracy model (see G.2). Any type descended from a nonstandard real type is also nonstandard. An implementation may place arbitrary restrictions on the use of such types; it is implementation defined whether operators that are predefined for "any real type" are defined for a particular nonstandard real type. [In any case, such types are not permitted as [explicit_generic_actual_parameter](S0244)s for formal scalar types - see 12.5.2.] 

Implementation defined: Any nonstandard real types and the operators defined for them.

NOTE 1   As stated, real literals are of the anonymous predefined real type universal_real. Other real types have no literals. However, the overload resolution rules (see 8.6) allow expressions of the type universal_real whenever a real type is expected.


#### Wording Changes from Ada 83

The syntax rule for [real_type_definition](S0041) is modified to use the new syntactic categories [floating_point_definition](S0042) and [fixed_point_definition](S0044), instead of floating_point_constraint and fixed_point_constraint, because the semantics of a type definition are significantly different than the semantics of a constraint.

All discussion of model numbers, safe ranges, and machine numbers is moved to 3.5.7, 3.5.8, and G.2. Values of a fixed point type are now described as being multiples of the small of the fixed point type, and we have no need for model numbers, safe ranges, etc. for fixed point types.


### 3.5.7  Floating Point Types

For floating point types, the error bound is specified as a relative precision by giving the required minimum number of significant decimal digits. 


#### Syntax

floating_point_definition ::= 
  digits static_[expression](S0108) [[real_range_specification](S0043)]

real_range_specification ::= 
  range static_[simple_expression](S0110) .. static_[simple_expression](S0110)


#### Name Resolution Rules

The requested decimal precision, which is the minimum number of significant decimal digits required for the floating point type, is specified by the value of the [expression](S0108) given after the reserved word digits. This [expression](S0108) is expected to be of any integer type.

Each [simple_expression](S0110) of a [real_range_specification](S0043) is expected to be of any real type[; the types need not be the same]. 


#### Legality Rules

The requested decimal precision shall be specified by a static [expression](S0108) whose value is positive and no greater than System.Max_Base_Digits. Each [simple_expression](S0110) of a [real_range_specification](S0043) shall also be static. If the [real_range_specification](S0043) is omitted, the requested decimal precision shall be no greater than System.Max_Digits. 

Reason: We have added Max_Base_Digits to package System. It corresponds to the requested decimal precision of root_real. System.Max_Digits corresponds to the maximum value for Digits that may be specified in the absence of a [real_range_specification](S0043), for upward compatibility. These might not be the same if root_real has a base range that does not include ± 10.0**(4*Max_Base_Digits). 

A [floating_point_definition](S0042) is illegal if the implementation does not support a floating point type that satisfies the requested decimal precision and range. 

Implementation defined: What combinations of requested decimal precision and range are supported for floating point types.


#### Static Semantics

The set of values for a floating point type is the (infinite) set of rational numbers. The machine numbers of a floating point type are the values of the type that can be represented exactly in every unconstrained variable of the type. The base range (see 3.5) of a floating point type is symmetric around zero, except that it can include some extra negative values in some implementations.

Implementation Note: For example, if a 2's complement representation is used for the mantissa rather than a sign-mantissa or 1's complement representation, then there is usually one extra negative machine number.

To be honest: If the Signed_Zeros attribute is True, then minus zero could in a sense be considered a value of the type. However, for most purposes, minus zero behaves the same as plus zero.

The base decimal precision of a floating point type is the number of decimal digits of precision representable in objects of the type. The safe range of a floating point type is that part of its base range for which the accuracy corresponding to the base decimal precision is preserved by all predefined operations. 

Implementation Note: In most cases, the safe range and base range are the same. However, for some hardware, values near the boundaries of the base range might result in excessive inaccuracies or spurious overflows when used with certain predefined operations. For such hardware, the safe range would omit such values.

A [floating_point_definition](S0042) defines a floating point type whose base decimal precision is no less than the requested decimal precision. If a [real_range_specification](S0043) is given, the safe range of the floating point type (and hence, also its base range) includes at least the values of the simple expressions given in the [real_range_specification](S0043). If a [real_range_specification](S0043) is not given, the safe (and base) range of the type includes at least the values of the range 10.0**(4*D) .. +10.0**(4*D) where D is the requested decimal precision. [The safe range might include other values as well. The attributes Safe_First and Safe_Last give the actual bounds of the safe range.]

A [floating_point_definition](S0042) also defines a first subtype of the type. If a [real_range_specification](S0043) is given, then the subtype is constrained to a range whose bounds are given by a conversion of the values of the [simple_expression](S0110)s of the [real_range_specification](S0043) to the type being defined. Otherwise, the subtype is unconstrained.

There is a predefined, unconstrained, floating point subtype named Float[, declared in the visible part of package Standard]. 


#### Dynamic Semantics

[The elaboration of a [floating_point_definition](S0042) creates the floating point type and its first subtype.] 


#### Implementation Requirements

In an implementation that supports floating point types with 6 or more digits of precision, the requested decimal precision for Float shall be at least 6.

If Long_Float is predefined for an implementation, then its requested decimal precision shall be at least 11. 


#### Implementation Permissions

An implementation is allowed to provide additional predefined floating point types[, declared in the visible part of Standard], whose (unconstrained) first subtypes have names of the form Short_Float, Long_Float, Short_Short_Float, Long_Long_Float, etc. Different predefined floating point types are allowed to have the same base decimal precision. However, the precision of Float should be no greater than that of Long_Float. Similarly, the precision of Short_Float (if provided) should be no greater than Float. Corresponding recommendations apply to any other predefined floating point types. There need not be a named floating point type corresponding to each distinct base decimal precision supported by an implementation. 

Implementation defined: The predefined floating point types declared in Standard.


#### Implementation Advice

An implementation should support Long_Float in addition to Float if the target machine supports 11 or more digits of precision. No other named floating point subtypes are recommended for package Standard. Instead, appropriate named floating point subtypes should be provided in the library package Interfaces (see B.2). 

Implementation Note: To promote portability, implementations should explicitly declare the floating point (sub)types Float and Long_Float in Standard, and leave other predefined float types anonymous. For implementations that already support Short_Float, etc., upward compatibility argues for keeping such declarations in Standard during the transition period, but perhaps generating a warning on use. A separate package Interfaces in the predefined environment is available for pre-declaring types such as Float_32, IEEE_Float_64, etc. See B.2. 

NOTE 1   If a floating point subtype is unconstrained, then assignments to variables of the subtype involve only Overflow_Checks, never Range_Checks. 


#### Examples

Examples of floating point types and subtypes: 

```ada
type Coefficient is digits 10 range -1.0 .. 1.0;

```

```ada
type Real is digits 8;
type Mass is digits 7 range 0.0 .. 1.0E35;

```

```ada
subtype Probability is Real range 0.0 .. 1.0;   --   a subtype with a smaller range

```


#### Inconsistencies With Ada 83

No Range_Checks, only Overflow_Checks, are performed on variables (or parameters) of an unconstrained floating point subtype. This is upward compatible for programs that do not raise Constraint_Error. For those that do raise Constraint_Error, it is possible that the exception will be raised at a later point, or not at all, if extended range floating point registers are used to hold the value of the variable (or parameter). 

Reason: This change was felt to be justified by the possibility of improved performance on machines with extended-range floating point registers. An implementation need not take advantage of this relaxation in the range checking; it can hide completely the use of extended range registers if desired, presumably at some run-time expense. 


#### Wording Changes from Ada 83

The syntax rules for floating_point_constraint and floating_accuracy_definition are removed. The syntax rules for [floating_point_definition](S0042) and [real_range_specification](S0043) are new.

A syntax rule for [digits_constraint](S0047) is given in 3.5.9, "Fixed Point Types". In J.3 we indicate that a [digits_constraint](S0047) may be applied to a floating point [subtype_mark](S0025) as well (to be compatible with Ada 83's floating_point_constraint).

Discussion of model numbers is postponed to 3.5.8 and G.2. The concept of safe numbers has been replaced by the concept of the safe range of values. The bounds of the safe range are given by T'Safe_First .. T'Safe_Last, rather than -T'Safe_Large .. T'Safe_Large, since on some machines the safe range is not perfectly symmetric. The concept of machine numbers is new, and is relevant to the definition of Succ and Pred for floating point numbers. 


### 3.5.8  Operations of Floating Point Types


#### Static Semantics

The following attribute is defined for every floating point subtype S:

The requested decimal precision of the base subtype of a floating point type T is defined to be the largest value of d for which ceiling(d * log(10) / log(T'Machine_Radix)) + 1 &lt= T'Model_Mantissa. 

NOTE 1   The predefined operations of a floating point type include the assignment operation, qualification, the membership tests, and explicit conversion to and from other numeric types. They also include the relational operators and the following predefined arithmetic operators: the binary and unary adding operators  and +, certain multiplying operators, the unary operator abs, and the exponentiation operator.

NOTE 2   As for all types, objects of a floating point type have Size and Address attributes (see 13.3). Other attributes of floating point types are defined in A.5.3. 


### 3.5.9  Fixed Point Types

A fixed point type is either an ordinary fixed point type, or a decimal fixed point type. The error bound of a fixed point type is specified as an absolute value, called the delta of the fixed point type. 


#### Syntax

fixed_point_definition ::= [ordinary_fixed_point_definition](S0045) | [decimal_fixed_point_definition](S0046)

ordinary_fixed_point_definition ::= 
   delta static_[expression](S0108)  [real_range_specification](S0043)

decimal_fixed_point_definition ::= 
   delta static_[expression](S0108) digits static_[expression](S0108) [[real_range_specification](S0043)]

digits_constraint ::= 
   digits static_[expression](S0108) [[range_constraint](S0033)]


#### Name Resolution Rules

For a type defined by a [fixed_point_definition](S0044), the delta of the type is specified by the value of the [expression](S0108) given after the reserved word delta; this [expression](S0108) is expected to be of any real type. For a type defined by a [decimal_fixed_point_definition](S0046) (a decimal fixed point type), the number of significant decimal digits for its first subtype (the digits of the first subtype) is specified by the [expression](S0108) given after the reserved word digits; this [expression](S0108) is expected to be of any integer type.


#### Legality Rules

In a [fixed_point_definition](S0044) or [digits_constraint](S0047), the [expression](S0108)s given after the reserved words delta and digits shall be static; their values shall be positive.

The set of values of a fixed point type comprise the integral multiples of a number called the small of the type. For a type defined by an [ordinary_fixed_point_definition](S0045) (an ordinary fixed point type), the small may be specified by an [attribute_definition_clause](S0265) (see 13.3); if so specified, it shall be no greater than the delta of the type. If not specified, the small of an ordinary fixed point type is an implementation-defined power of two less than or equal to the delta. 

Implementation defined: The small of an ordinary fixed point type.

For a decimal fixed point type, the small equals the delta; the delta shall be a power of 10. If a [real_range_specification](S0043) is given, both bounds of the range shall be in the range (10**digits1)*delta .. +(10**digits1)*delta.

A [fixed_point_definition](S0044) is illegal if the implementation does not support a fixed point type with the given small and specified range or digits. 

Implementation defined: What combinations of small, range, and digits are supported for fixed point types.

For a [subtype_indication](S0024) with a [digits_constraint](S0047), the [subtype_mark](S0025) shall denote a decimal fixed point subtype. 

To be honest: Or, as an obsolescent feature, a floating point subtype is permitted - see J.3. 


#### Static Semantics

The base range (see 3.5) of a fixed point type is symmetric around zero, except possibly for an extra negative value in some implementations.

An [ordinary_fixed_point_definition](S0045) defines an ordinary fixed point type whose base range includes at least all multiples of small that are between the bounds specified in the [real_range_specification](S0043). The base range of the type does not necessarily include the specified bounds themselves. An [ordinary_fixed_point_definition](S0045) also defines a constrained first subtype of the type, with each bound of its range given by the closer to zero of: 

the value of the conversion to the fixed point type of the corresponding [expression](S0108) of the [real_range_specification](S0043); 

the corresponding bound of the base range. 

A [decimal_fixed_point_definition](S0046) defines a decimal fixed point type whose base range includes at least the range (10**digits1)*delta .. +(10**digits1)*delta. A [decimal_fixed_point_definition](S0046) also defines a constrained first subtype of the type. If a [real_range_specification](S0043) is given, the bounds of the first subtype are given by a conversion of the values of the [expression](S0108)s of the [real_range_specification](S0043). Otherwise, the range of the first subtype is (10**digits1)*delta .. +(10**digits1)*delta.


#### Dynamic Semantics

The elaboration of a [fixed_point_definition](S0044) creates the fixed point type and its first subtype.

For a [digits_constraint](S0047) on a decimal fixed point subtype with a given delta, if it does not have a [range_constraint](S0033), then it specifies an implicit range (10**D1)*delta .. +(10**D1)*delta, where D is the value of the [expression](S0108). A [digits_constraint](S0047) is compatible with a decimal fixed point subtype if the value of the [expression](S0108) is no greater than the digits of the subtype, and if it specifies (explicitly or implicitly) a range that is compatible with the subtype. 

Discussion: Except for the requirement that the digits specified be no greater than the digits of the subtype being constrained, a [digits_constraint](S0047) is essentially equivalent to a [range_constraint](S0033).

Consider the following example: 

```ada
type D is delta 0.01 digits 7 range -0.00 .. 9999.99;

```

The compatibility rule implies that the [digits_constraint](S0047) "digits 6" specifies an implicit range of "99.9999 .. 99.9999". Thus, "digits 6" is not compatible with the constraint of D, but "digits 6 range 0.00 .. 9999.99" is compatible.

A value of a scalar type belongs to a constrained subtype of the type if it belongs to the range of the subtype. Attributes like Digits and Delta have no affect on this fundamental rule. So the obsolescent forms of [digits_constraint](S0047)s and [delta_constraint](S0275)s that are called "accuracy constraints" in RM83 don't really represent constraints on the values of the subtype, but rather primarily affect compatibility of the "constraint" with the subtype being "constrained". In this sense, they might better be called "subtype assertions" rather than "constraints".

Note that the [digits_constraint](S0047) on a decimal fixed point subtype is a combination of an assertion about the digits of the subtype being further constrained, and a constraint on the range of the subtype being defined, either explicit or implicit. 

The elaboration of a [digits_constraint](S0047) consists of the elaboration of the [range_constraint](S0033), if any. If a [range_constraint](S0033) is given, a check is made that the bounds of the range are both in the range (10**D1)*delta .. +(10**D1)*delta, where D is the value of the (static) [expression](S0108) given after the reserved word digits. If this check fails, Constraint_Error is raised. 


#### Implementation Requirements

The implementation shall support at least 24 bits of precision (including the sign bit) for fixed point types. 

Reason: This is sufficient to represent Standard.Duration with a small no more than 50 milliseconds. 


#### Implementation Permissions

Implementations are permitted to support only smalls that are a power of two. In particular, all decimal fixed point type declarations can be disallowed. Note however that conformance with the Information Systems Annex requires support for decimal smalls, and decimal fixed point type declarations with digits up to at least 18. 

Implementation Note: The accuracy requirements for multiplication, division, and conversion (see G.2.1, "Model of Floating Point Arithmetic") are such that support for arbitrary smalls should be practical without undue implementation effort. Therefore, implementations should support fixed point types with arbitrary values for small (within reason). One reasonable limitation would be to limit support to fixed point types that can be converted to the most precise floating point type without loss of precision (so that Fixed_IO is implementable in terms of Float_IO). 

NOTE   The base range of an ordinary fixed point type need not include the specified bounds themselves so that the range specification can be given in a natural way, such as: 

```ada
   type Fraction is delta 2.0**(-15) range -1.0 .. 1.0;
  

```

With 2's complement hardware, such a type could have a signed 16-bit representation, using 1 bit for the sign and 15 bits for fraction, resulting in a base range of 1.0 .. 1.02.0**(15). 


#### Examples

Examples of fixed point types and subtypes: 

```ada
type Volt is delta 0.125 range 0.0 .. 255.0;

```

```ada
  -- A pure fraction which requires all the available
  -- space in a word can be declared as the type Fraction:
type Fraction is delta System.Fine_Delta range -1.0 .. 1.0;
  -- Fraction'Last = 1.0  System.Fine_Delta

```

```ada
type Money is delta 0.01 digits 15;  -- decimal fixed point
subtype Salary is Money digits 10;
  -- Money'Last = 10.0**13  0.01, Salary'Last = 10.0**8  0.01

```


#### Inconsistencies With Ada 83

In Ada 95, S'Small always equals S'Base'Small, so if an implementation chooses a small for a fixed point type smaller than required by the delta, the value of S'Small in Ada 95 might not be the same as it was in Ada 83. 


#### Extensions to Ada 83

Decimal fixed point types are new, though their capabilities are essentially similar to that available in Ada 83 with a fixed point type whose small equals its delta equals a power of 10. However, in the Information Systems Annex, additional requirements are placed on the support of decimal fixed point types (e.g. a minimum of 18 digits of precision). 


#### Wording Changes from Ada 83

The syntax rules for fixed_point_constraint and fixed_accuracy_definition are removed. The syntax rule for [fixed_point_definition](S0044) is new. A syntax rule for [delta_constraint](S0275) is included in the Obsolescent features (to be compatible with Ada 83's fixed_point_constraint). 


### 3.5.10  Operations of Fixed Point Types


#### Static Semantics

The following attributes are defined for every fixed point subtype S: 

Small may be specified for nonderived fixed point types via an [attribute_definition_clause](S0265) (see 13.3); the expression of such a clause shall be static.

S'DeltaS'Delta denotes the delta of the fixed point subtype S. The value of this attribute is of the type universal_real. 

Reason: The delta is associated with the subtype as opposed to the type, because of the possibility of an (obsolescent) [delta_constraint](S0275).

S'ForeS'Fore yields the minimum number of characters needed before the decimal point for the decimal representation of any value of the subtype S, assuming that the representation does not include an exponent, but includes a one-character prefix that is either a minus sign or a space. (This minimum number does not include superfluous zeros or underlines, and is at least 2.) The value of this attribute is of the type universal_integer.

S'AftS'Aft yields the number of decimal digits needed after the decimal point to accommodate the delta of the subtype S, unless the delta of the subtype S is greater than 0.1, in which case the attribute yields the value one. [(S'Aft is the smallest positive integer N for which (10**N)*S'Delta is greater than or equal to one.)] The value of this attribute is of the type universal_integer. 

The following additional attributes are defined for every decimal fixed point subtype S: 

S'DigitsS'Digits denotes the digits of the decimal fixed point subtype S, which corresponds to the number of decimal digits that are representable in objects of the subtype. The value of this attribute is of the type universal_integer. Its value is determined as follows: 

For a first subtype or a subtype defined by a [subtype_indication](S0024) with a [digits_constraint](S0047), the digits is the value of the expression given after the reserved word digits;

For a subtype defined by a [subtype_indication](S0024) without a [digits_constraint](S0047), the digits of the subtype is the same as that of the subtype denoted by the [subtype_mark](S0025) in the [subtype_indication](S0024). 

Implementation Note: Although a decimal subtype can be both range-constrained and digits-constrained, the digits constraint is intended to control the Size attribute of the subtype. For decimal types, Size can be important because input/output of decimal types is so common. 

The digits of a base subtype is the largest integer D such that the range (10**D1)*delta .. +(10**D1)*delta is included in the base range of the type.

S'ScaleS'Scale denotes the scale of the subtype S, defined as the value N such that S'Delta = 10.0**(N). [The scale indicates the position of the point relative to the rightmost significant digits of values of subtype S.] The value of this attribute is of the type universal_integer. 

Ramification: S'Scale is negative if S'Delta is greater than one. By contrast, S'Aft is always positive. 

S'RoundS'Round denotes a function with the following specification: 

```ada
function S'Round(X : universal_real)
  return S'Base

```

The function returns the value obtained by rounding X (away from 0, if X is midway between two values of the type of S). 

NOTE 1   All subtypes of a fixed point type will have the same value for the Delta attribute, in the absence of [delta_constraint](S0275)s (see J.3).

NOTE 2   S'Scale is not always the same as S'Aft for a decimal subtype; for example, if S'Delta = 1.0 then S'Aft is 1 while S'Scale is 0.

NOTE 3   The predefined operations of a fixed point type include the assignment operation, qualification, the membership tests, and explicit conversion to and from other numeric types. They also include the relational operators and the following predefined arithmetic operators: the binary and unary adding operators  and +, multiplying operators, and the unary operator abs.

NOTE 4   As for all types, objects of a fixed point type have Size and Address attributes (see 13.3). Other attributes of fixed point types are defined in A.5.4. 


## 3.6  Array Types

An array object is a composite object consisting of components which all have the same subtype. The name for a component of an array uses one or more index values belonging to specified discrete types. The value of an array object is a composite value consisting of the values of the components. 


#### Syntax

array_type_definition ::= 
   [unconstrained_array_definition](S0049) | [constrained_array_definition](S0051)

unconstrained_array_definition ::= 
   array([index_subtype_definition](S0050) {, [index_subtype_definition](S0050)}) of [component_definition](S0053)

index_subtype_definition ::= [subtype_mark](S0025) range &lt&gt

constrained_array_definition ::= 
   array ([discrete_subtype_definition](S0052) {, [discrete_subtype_definition](S0052)}) of [component_definition](S0053)

discrete_subtype_definition ::= discrete_[subtype_indication](S0024) | [range](S0034)

component_definition ::= [aliased] [subtype_indication](S0024)


#### Name Resolution Rules

For a [discrete_subtype_definition](S0052) that is a [range](S0034), the [range](S0034) shall resolve to be of some specific discrete type[; which discrete type shall be determined without using any context other than the bounds of the [range](S0034) itself (plus the preference for root_integer - see 8.6).] 


#### Legality Rules

Each [index_subtype_definition](S0050) or [discrete_subtype_definition](S0052) in an [array_type_definition](S0048) defines an index subtype; its type (the index type) shall be discrete. 

Discussion: An index is a discrete quantity used to select along a given dimension of an array. A component is selected by specifying corresponding values for each of the indices. 

The subtype defined by the [subtype_indication](S0024) of a [component_definition](S0053) (the component subtype) shall be a definite subtype. 

Ramification: This applies to all uses of [component_definition](S0053), including in [record_type_definition](S0063)s and [protected_definition](S0182)s.

Within the definition of a nonlimited composite type (or a limited composite type that later in its immediate scope becomes nonlimited - see 7.3.1 and 7.5), if a [component_definition](S0053) contains the reserved word aliased and the type of the component is discriminated, then the nominal subtype of the component shall be constrained. 

Reason: If we allowed the subtype to be unconstrained, then the discriminants might change because of an assignment to the containing (nonlimited) object, thus causing a potential violation of an access subtype constraint of an access value designating the aliased component.

Note that the rule elsewhere defining all aliased discriminated objects to be constrained does not help - that rule prevents assignments to the component itself from doing any harm, but not assignments to the containing object.

We allow this for components within limited types since assignment to the enclosing object is not a problem. Furthermore, it is important to be able to use a default expression for a discriminant in arrays of limited components, since that is the only way to give the components different values for their discriminants. For example: 

```ada
protected type Counter_Type(Initial_Value : Integer := 1) is
   procedure Get_Next(Next_Value : out Integer);
     -- Returns the next value on each call, bumping Count
     -- before returning.
private
   Count : Integer := Initial_Value;
end Counter_Type;
protected body Counter_Type is ...

```

```ada
function Next_Id(Counter : access Counter_Type) return Integer is
    Result : Integer;
begin
    Counter.Get_Next(Result);
    return Result;
end Next_Id;

```

```ada
C : aliased Counter_Type;
task type T(Who_Am_I : Integer := Next_Id(C'Access));
task body T is ...

```

```ada
Task_Array : array(1..100) of aliased T;
  -- Array of task elements, each with its own unique ID.
  -- We specify "aliased" so we can use Task_Array(I)'Access.
  -- This is safe because Task_Array is of a limited type,
  -- so there is no way an assignment to it could change
  -- the discriminants of one of its components.

```

Ramification: Note that this rule applies to array components and record components, but not to protected type components (since they are always limited). 


#### Static Semantics

An array is characterized by the number of indices (the dimensionality of the array), the type and position of each index, the lower and upper bounds for each index, and the subtype of the components. The order of the indices is significant.

A one-dimensional array has a distinct component for each possible index value. A multidimensional array has a distinct component for each possible sequence of index values that can be formed by selecting one value for each index position (in the given order). The possible values for a given index are all the values between the lower and upper bounds, inclusive; this range of values is called the index range. The bounds of an array are the bounds of its index ranges. The length of a dimension of an array is the number of values of the index range of the dimension (zero for a null range). The length of a one-dimensional array is the length of its only dimension.

An [array_type_definition](S0048) defines an array type and its first subtype. For each object of this array type, the number of indices, the type and position of each index, and the subtype of the components are as in the type definition[; the values of the lower and upper bounds for each index belong to the corresponding index subtype of its type, except for null arrays (see 3.6.1)].

An [unconstrained_array_definition](S0049) defines an array type with an unconstrained first subtype. Each [index_subtype_definition](S0050) defines the corresponding index subtype to be the subtype denoted by the [subtype_mark](S0025). [ The compound delimiter &lt&gt (called a box) of an [index_subtype_definition](S0050) stands for an undefined range (different objects of the type need not have the same bounds).]

A [constrained_array_definition](S0051) defines an array type with a constrained first subtype. Each [discrete_subtype_definition](S0052) defines the corresponding index subtype, as well as the corresponding index range for the constrained first subtype. The constraint of the first subtype consists of the bounds of the index ranges. 

Discussion: Although there is no namable unconstrained array subtype in this case, the predefined slicing and concatenation operations can operate on and yield values that do not necessarily belong to the first array subtype. This is also true for Ada 83. 

The discrete subtype defined by a [discrete_subtype_definition](S0052) is either that defined by the [subtype_indication](S0024), or a subtype determined by the [range](S0034) as follows: 

If the type of the [range](S0034) resolves to root_integer, then the [discrete_subtype_definition](S0052) defines a subtype of the predefined type Integer with bounds given by a conversion to Integer of the bounds of the [range](S0034); 

Reason: This ensures that indexing over the discrete subtype can be performed with regular Integers, rather than only universal_integers. 

Discussion: We considered doing this by simply creating a "preference" for Integer when resolving the [range](S0034). However, this can introduce Beaujolais effects when the [simple_expression](S0110)s involve calls on functions visible due to use clauses. 

Otherwise, the [discrete_subtype_definition](S0052) defines a subtype of the type of the [range](S0034), with the bounds given by the [range](S0034). 

The [component_definition](S0053) of an [array_type_definition](S0048) defines the nominal subtype of the components. If the reserved word aliased appears in the [component_definition](S0053), then each component of the array is aliased (see 3.10). 

Ramification: In this case, the nominal subtype cannot be an unconstrained discriminated subtype. See 3.8. 


#### Dynamic Semantics

The elaboration of an [array_type_definition](S0048) creates the array type and its first subtype, and consists of the elaboration of any [discrete_subtype_definition](S0052)s and the [component_definition](S0053).

The elaboration of a [discrete_subtype_definition](S0052) creates the discrete subtype, and consists of the elaboration of the [subtype_indication](S0024) or the evaluation of the [range](S0034). The elaboration of a [component_definition](S0053) in an [array_type_definition](S0048) consists of the elaboration of the [subtype_indication](S0024). The elaboration of any [discrete_subtype_definition](S0052)s and the elaboration of the [component_definition](S0053) are performed in an arbitrary order. 

NOTE 1   All components of an array have the same subtype. In particular, for an array of components that are one-dimensional arrays, this means that all components have the same bounds and hence the same length.

NOTE 2   Each elaboration of an [array_type_definition](S0048) creates a distinct array type. A consequence of this is that each object whose [object_declaration](S0029) contains an [array_type_definition](S0048) is of its own unique type. 


#### Examples

Examples of type declarations with unconstrained array definitions: 

```ada
type Vector     is array(Integer  range &lt&gt) of Real;
type Matrix     is array(Integer  range &lt&gt, Integer range &lt&gt) of Real;
type Bit_Vector is array(Integer  range &lt&gt) of Boolean;
type Roman      is array(Positive range &lt&gt) of Roman_Digit; -- see 3.5.2

```

Examples of type declarations with constrained array definitions: 

```ada
type Table    is array(1 .. 10) of Integer;
type Schedule is array(Day) of Boolean;
type Line     is array(1 .. Max_Line_Size) of Character;

```

Examples of object declarations with array type definitions: 

```ada
Grid : array(1 .. 80, 1 .. 100) of Boolean;
Mix  : array(Color range Red .. Green) of Boolean;
Page : array(Positive range &lt&gt) of Line :=  --  an array of arrays
  (1 | 50  =&gt Line'(1 | Line'Last =&gt '+', others =&gt '-'),  -- see 4.3.3
   2 .. 49 =&gt Line'(1 | Line'Last =&gt '|', others =&gt ' '));
    -- Page is constrained by its initial value to (1..50)

```


#### Extensions to Ada 83

The syntax rule for [component_definition](S0053) is modified to allow the reserved word aliased.

The syntax rules for [unconstrained_array_definition](S0049) and [constrained_array_definition](S0051) are modified to use [component_definition](S0053) (instead of component_[subtype_indication](S0024)). The effect of this change is to allow the reserved word aliased before the component [subtype_indication](S0024).

A [range](S0034) in a [discrete_subtype_definition](S0052) may use arbitrary universal expressions for each bound (e.g. 1 .. 3+5), rather than strictly "implicitly convertible" operands. The subtype defined will still be a subtype of Integer. 


#### Wording Changes from Ada 83

We introduce a new syntactic category, [discrete_subtype_definition](S0052), as distinct from [discrete_range](S0055). These two constructs have the same syntax, but their semantics are quite different (one defines a subtype, with a preference for Integer subtypes, while the other just selects a subrange of an existing subtype). We use this new syntactic category in for loops and entry families.

The syntax for [index_constraint](S0054) and [discrete_range](S0055) have been moved to their own subclause, since they are no longer used here.

The syntax rule for [component_definition](S0053) (formerly component_subtype_definition) is moved here from RM83-3.7. 


### 3.6.1  Index Constraints and Discrete Ranges

An [index_constraint](S0054) determines the range of possible values for every index of an array subtype, and thereby the corresponding array bounds. 


#### Syntax

index_constraint ::=  ([discrete_range](S0055) {, [discrete_range](S0055)})

discrete_range ::= discrete_[subtype_indication](S0024) | [range](S0034)


#### Name Resolution Rules

The type of a [discrete_range](S0055) is the type of the subtype defined by the [subtype_indication](S0024), or the type of the [range](S0034). For an [index_constraint](S0054), each [discrete_range](S0055) shall resolve to be of the type of the corresponding index. 

Discussion: In Ada 95, [index_constraint](S0054)s only appear in a [subtype_indication](S0024); they no longer appear in [constrained_array_definition](S0051)s. 


#### Legality Rules

An [index_constraint](S0054) shall appear only in a [subtype_indication](S0024) whose [subtype_mark](S0025) denotes either an unconstrained array subtype, or an unconstrained access subtype whose designated subtype is an unconstrained array subtype; in either case, the [index_constraint](S0054) shall provide a [discrete_range](S0055) for each index of the array type. 


#### Static Semantics

A [discrete_range](S0055) defines a range whose bounds are given by the [range](S0034), or by the range of the subtype defined by the [subtype_indication](S0024). 


#### Dynamic Semantics

An [index_constraint](S0054) is compatible with an unconstrained array subtype if and only if the index range defined by each [discrete_range](S0055) is compatible (see 3.5) with the corresponding index subtype. If any of the [discrete_range](S0055)s defines a null range, any array thus constrained is a null array, having no components. An array value satisfies an [index_constraint](S0054) if at each index position the array value and the [index_constraint](S0054) have the same index bounds. 

Ramification: There is no need to define compatibility with a constrained array subtype, because one is not allowed to constrain it again.

The elaboration of an [index_constraint](S0054) consists of the evaluation of the [discrete_range](S0055)(s), in an arbitrary order. The evaluation of a [discrete_range](S0055) consists of the elaboration of the [subtype_indication](S0024) or the evaluation of the [range](S0034). 

NOTE 1   The elaboration of a [subtype_indication](S0024) consisting of a [subtype_mark](S0025) followed by an [index_constraint](S0054) checks the compatibility of the [index_constraint](S0054) with the [subtype_mark](S0025) (see 3.2.2).

NOTE 2   Even if an array value does not satisfy the index constraint of an array subtype, Constraint_Error is not raised on conversion to the array subtype, so long as the length of each dimension of the array value and the array subtype match. See 4.6. 


#### Examples

Examples of array declarations including an index constraint: 

```ada
Board     : Matrix(1 .. 8,  1 .. 8);  --  see 3.6
Rectangle : Matrix(1 .. 20, 1 .. 30);
Inverse   : Matrix(1 .. N,  1 .. N);  --  N need not be static

```

```ada
Filter    : Bit_Vector(0 .. 31);      --  see 3.6

```

Example of array declaration with a constrained array subtype: 

```ada
My_Schedule : Schedule;  --  all arrays of type Schedule have the same bounds

```

Example of record type with a component that is an array: 

```ada
type Var_Line(Length : Natural) is
   record
      Image : String(1 .. Length);
   end record;

```

```ada
Null_Line : Var_Line(0);  --  Null_Line.Image is a null array

```


#### Extensions to Ada 83

We allow the declaration of a variable with a nominally unconstrained array subtype, so long as it has an initialization expression to determine its bounds. 


#### Wording Changes from Ada 83

We have moved the syntax for [index_constraint](S0054) and [discrete_range](S0055) here since they are no longer used in [constrained_array_definition](S0051)s. We therefore also no longer have to describe the (special) semantics of [index_constraint](S0054)s and [discrete_range](S0055)s that appear in [constrained_array_definition](S0051)s.

The rules given in RM83-3.6.1(5,7-10), which define the bounds of an array object, are redundant with rules given elsewhere, and so are not repeated here. RM83-3.6.1(6), which requires that the (nominal) subtype of an array variable be constrained, no longer applies, so long as the variable is explicitly initialized. 


### 3.6.2  Operations of Array Types


#### Legality Rules

[The argument N used in the [attribute_designator](S0094)s for the N-th dimension of an array shall be a static [expression](S0108) of some integer type.] The value of N shall be positive (nonzero) and no greater than the dimensionality of the array. 


#### Static Semantics

The following attributes are defined for a prefix A that is of an array type [(after any implicit dereference)], or denotes a constrained array subtype: 

Ramification: These attributes are not defined if A is a subtype-mark for an access-to-array subtype. They are defined (by implicit dereference) for access-to-array values.

A'FirstA'First denotes the lower bound of the first index range; its type is the corresponding index type.

A'First(N)A'First(N) denotes the lower bound of the N-th index range; its type is the corresponding index type.

A'LastA'Last denotes the upper bound of the first index range; its type is the corresponding index type.

A'Last(N)A'Last(N) denotes the upper bound of the N-th index range; its type is the corresponding index type.

A'RangeA'Range is equivalent to the range A'First .. A'Last, except that the [prefix](S0086) A is only evaluated once.

A'Range(N)A'Range(N) is equivalent to the range A'First(N) .. A'Last(N), except that the [prefix](S0086) A is only evaluated once.

A'LengthA'Length denotes the number of values of the first index range (zero for a null range); its type is universal_integer.

A'Length(N)A'Length(N) denotes the number of values of the N-th index range (zero for a null range); its type is universal_integer. 


#### Implementation Advice

An implementation should normally represent multidimensional arrays in row-major order, consistent with the notation used for multidimensional array aggregates (see 4.3.3). However, if a pragma Convention(Fortran, ...) applies to a multidimensional array type, then column-major order should be used instead (see B.5, "Interfacing with Fortran"). 

NOTE 1   The [attribute_reference](S0093)s A'First and A'First(1) denote the same value. A similar relation exists for the [attribute_reference](S0093)s A'Last, A'Range, and A'Length. The following relation is satisfied (except for a null array) by the above attributes if the index type is an integer type: 

```ada
   A'Length(N) = A'Last(N) - A'First(N) + 1

```

NOTE 2   An array type is limited if its component type is limited (see 7.5).

NOTE 3   The predefined operations of an array type include the membership tests, qualification, and explicit conversion. If the array type is not limited, they also include assignment and the predefined equality operators. For a one-dimensional array type, they include the predefined concatenation operators (if nonlimited) and, if the component type is discrete, the predefined relational operators; if the component type is boolean, the predefined logical operators are also included.

NOTE 4   A component of an array can be named with an [indexed_component](S0089). A value of an array type can be specified with an [array_aggregate](S0104), unless the array type is limited. For a one-dimensional array type, a slice of the array can be named; also, string literals are defined if the component type is a character type. 


#### Examples

Examples (using arrays declared in the examples of subclause 3.6.1): 

```ada
--  Filter'First      =   0   Filter'Last       =  31   Filter'Length =  32
--  Rectangle'Last(1) =  20   Rectangle'Last(2) =  30

```


### 3.6.3  String Types


#### Static Semantics

A one-dimensional array type whose component type is a character type is called a string type.

[There are two predefined string types, String and Wide_String, each indexed by values of the predefined subtype Positive; these are declared in the visible part of package Standard:] 

```ada
[subtype Positive is Integer range 1 .. Integer'Last;

```

```ada
type String is array(Positive range &lt&gt) of Character;
type Wide_String is array(Positive range &lt&gt) of Wide_Character;

]

```

NOTE 1   String literals (see 2.6 and 4.2) are defined for all string types. The concatenation operator & is predefined for string types, as for all nonlimited one-dimensional array types. The ordering operators &lt, &lt=, &gt, and &gt= are predefined for string types, as for all one-dimensional discrete array types; these ordering operators correspond to lexicographic order (see 4.5.2).


#### Examples

Examples of string objects: 

```ada
Stars      : String(1 .. 120) := (1 .. 120 =&gt '*' );
Question   : constant String  := "How many characters?";
	-- Question'First = 1, Question'Last = 20
	-- Question'Length = 20 (the number of characters)

```

```ada
Ask_Twice  : String  := Question & Question;	-- constrained to (1..40)
Ninety_Six : constant Roman   := "XCVI";	-- see 3.5.2 and 3.6

```


#### Inconsistencies With Ada 83

The declaration of Wide_String in Standard hides a use-visible declaration with the same [defining_identifier](S0019). In rare cases, this might result in an inconsistency between Ada 83 and Ada 95. 


#### Incompatibilities With Ada 83

Because both String and Wide_String are always directly visible, an expression like 

```ada
"a" &lt "bc"

```

is now ambiguous, whereas in Ada 83 both string literals could be resolved to type String. 


#### Extensions to Ada 83

The type Wide_String is new (though it was approved by ARG for Ada 83 compilers as well). 


#### Wording Changes from Ada 83

We define the term string type as a natural analogy to the term character type. 


## 3.7  Discriminants

[ A composite type (other than an array type) can have discriminants, which parameterize the type. A [known_discriminant_part](S0058) specifies the discriminants of a composite type. A discriminant of an object is a component of the object, and is either of a discrete type or an access type. An [unknown_discriminant_part](S0057) in the declaration of a partial view of a type specifies that the discriminants of the type are unknown for the given view; all subtypes of such a partial view are indefinite subtypes.] 

Glossary entry: A discriminant is a parameter of a composite type. It can control, for example, the bounds of a component of the type if that type is an array type. A discriminant of a task type can be used to pass data to a task of the type upon creation.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[discriminant], Def=[a parameter for a composite type, which can control, for example, the bounds of a component that is an array], Note1=[A discriminant for a task type can be used to pass data to a task of the type upon its creation.] 

Discussion: A type, and all of its subtypes, have unknown discriminants when the number or names of the discriminants, if any, are unknown at the point of the type declaration. A [discriminant_part](S0056) of (&lt&gt) is used to indicate unknown discriminants. 


#### Syntax

discriminant_part ::= [unknown_discriminant_part](S0057) | [known_discriminant_part](S0058)

unknown_discriminant_part ::= (&lt&gt)

known_discriminant_part ::= 
   ([discriminant_specification](S0059) {; [discriminant_specification](S0059)})

discriminant_specification ::= 
   [defining_identifier_list](S0030) : [subtype_mark](S0025) [:= [default_expression](S0060)]
 | [defining_identifier_list](S0030) : [access_definition](S0077) [:= [default_expression](S0060)]

default_expression ::= [expression](S0108)


#### Name Resolution Rules

The expected type for the [default_expression](S0060) of a [discriminant_specification](S0059) is that of the corresponding discriminant. 


#### Legality Rules

A [known_discriminant_part](S0058) is only permitted in a declaration for a composite type that is not an array type [(this includes generic formal types)]; a type declared with a [known_discriminant_part](S0058) is called a discriminated type, as is a type that inherits (known) discriminants. 

Implementation Note: Discriminants on array types were considered, but were omitted to ease (existing) implementations. 

Discussion: Note that the above definition for "discriminated type" does not include types declared with an [unknown_discriminant_part](S0057). This seems consistent with Ada 83, where such types (in a generic formal part) would not be considered discriminated types. Furthermore, the full type for a type with unknown discriminants need not even be composite, much less have any discriminants.

The subtype of a discriminant may be defined by a [subtype_mark](S0025), in which case the [subtype_mark](S0025) shall denote a discrete or access subtype, or it may be defined by an [access_definition](S0077) [(in which case the [subtype_mark](S0025) of the [access_definition](S0077) may denote any kind of subtype)]. A discriminant that is defined by an [access_definition](S0077) is called an access discriminant and is of an anonymous general access-to-variable type whose designated subtype is denoted by the [subtype_mark](S0025) of the [access_definition](S0077). 

Reason: In an early version of Ada 9X, we allowed access discriminants on nonlimited types, but this created unpleasant complexities. It turned out to be simpler and more uniform to allow discriminants of a named access type on any discriminated type, and keep access discriminants just for limited types.

Note that discriminants of a named access type are not considered "access discriminants". Similarly, "access parameter" only refers to a formal parameter defined by an [access_definition](S0077). 

A [discriminant_specification](S0059) for an access discriminant shall appear only in the declaration for a task or protected type, or for a type with the reserved word limited in its [(full)] definition or in that of one of its ancestors. In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit. 

Discussion: This rule implies that a type can have an access discriminant if the type is limited, but not if the only reason it's limited is because of a limited component. Compare with the definition of limited type in 7.5. 

Ramification: It is a consequence of this rule that only a return-by-reference type can have an access discriminant (see 6.5). This is important to avoid dangling references to local variables. 

Reason: We also considered the following rules: 

If a type has an access discriminant, this automatically makes it limited, just like having a limited component automatically makes a type limited. This was rejected because it decreases program readability, and because it seemed error prone (two bugs in a previous version of the RM9X were attributable to this rule).

A type with an access discriminant shall be limited. This is equivalent to the rule we actually chose, except that it allows a type to have an access discriminant if it is limited just because of a limited component. For example, any record containing a task would be allowed to have an access discriminant, whereas the actual rule requires "limited record". This rule was also rejected due to readability concerns, and because would interact badly with the rules for limited types that "become nonlimited".

[Default_expression](S0060)s shall be provided either for all or for none of the discriminants of a [known_discriminant_part](S0058). No [default_expression](S0060)s are permitted in a [known_discriminant_part](S0058) in a declaration of a tagged type [or a generic formal type]. 

Reason: The all-or-none rule is related to the rule that a discriminant constraint shall specify values for all discriminants. One could imagine a different rule that allowed a constraint to specify only some of the discriminants, with the others provided by default. Having defaults for discriminants has a special significance - it allows objects of the type to be unconstrained, with the discriminants alterable as part of assigning to the object.

Defaults for discriminants of tagged types are disallowed so that every object of a tagged type is constrained, either by an explicit constraint, or by its initial discriminant values. This substantially simplifies the semantic rules and the implementation of inherited dispatching operations. For generic formal types, the restriction simplifies the type matching rules. If one simply wants a "default" value for the discriminants, a constrained subtype can be declared for future use. 

For a type defined by a [derived_type_definition](S0032), if a [known_discriminant_part](S0058) is provided in its declaration, then: 

The parent subtype shall be constrained;

If the parent type is not a tagged type, then each discriminant of the derived type shall be used in the constraint defining the parent subtype;

Implementation Note: This ensures that the new discriminant can share storage with an existing discriminant.

If a discriminant is used in the constraint defining the parent subtype, the subtype of the discriminant shall be statically compatible (see 4.9.1) with the subtype of the corresponding parent discriminant. 

Reason: This ensures that on conversion (or extension via an extension aggregate) to a distantly related type, if the discriminants satisfy the target type's requirements they satisfy all the intermediate types' requirements as well. 

Ramification: There is no requirement that the new discriminant have the same (or any) [default_expression](S0060) as the parent's discriminant. 

The type of the [default_expression](S0060), if any, for an access discriminant shall be convertible to the anonymous access type of the discriminant (see 4.6). 

Ramification: This requires convertibility of the designated subtypes. 


#### Static Semantics

A [discriminant_specification](S0059) declares a discriminant; the [subtype_mark](S0025) denotes its subtype unless it is an access discriminant, in which case the discriminant's subtype is the anonymous access-to-variable subtype defined by the [access_definition](S0077).

[For a type defined by a [derived_type_definition](S0032), each discriminant of the parent type is either inherited, constrained to equal some new discriminant of the derived type, or constrained to the value of an expression.] When inherited or constrained to equal some new discriminant, the parent discriminant and the discriminant of the derived type are said to correspond. Two discriminants also correspond if there is some common discriminant to which they both correspond. A discriminant corresponds to itself as well. If a discriminant of a parent type is constrained to a specific value by a [derived_type_definition](S0032), then that discriminant is said to be specified by that [derived_type_definition](S0032). 

Ramification: The correspondence relationship is transitive, symmetric, and reflexive. That is, if A corresponds to B, and B corresponds to C, then A, B, and C each corresponds to A, B, and C in all combinations.

A [constraint](S0026) that appears within the definition of a discriminated type depends on a discriminant of the type if it names the discriminant as a bound or discriminant value. A [component_definition](S0053) depends on a discriminant if its [constraint](S0026) depends on the discriminant, or on a discriminant that corresponds to it. 

Ramification: A [constraint](S0026) in a [task_body](S0179) is not considered to depend on a discriminant of the task type, even if it names it. It is only the [constraint](S0026)s in the type definition itself that are considered dependents. Similarly for protected types. 

A component depends on a discriminant if: 

Its [component_definition](S0053) depends on the discriminant; or 

Ramification: A component does not depend on a discriminant just because its [default_expression](S0060) refers to the discriminant.

It is declared in a [variant_part](S0068) that is governed by the discriminant; or

It is a component inherited as part of a [derived_type_definition](S0032), and the [constraint](S0026) of the parent_[subtype_indication](S0024) depends on the discriminant; or 

Reason: When the parent subtype depends on a discriminant, the parent part of the derived type is treated like a discriminant-dependent component. 

Ramification: Because of this rule, we don't really need to worry about "corresponding" discriminants, since all the inherited components will be discriminant-dependent if there is a new [known_discriminant_part](S0058) whose discriminants are used to constrain the old discriminants. 

It is a subcomponent of a component that depends on the discriminant. 

Reason: The concept of discriminant-dependent (sub)components is primarily used in various rules that disallow renaming or 'Access, or specify that certain discriminant-changing assignments are erroneous. The goal is to allow implementations to move around or change the size of discriminant-dependent subcomponents upon a discriminant-changing assignment to an enclosing object. The above definition specifies that all subcomponents of a discriminant-dependent component or parent part are themselves discriminant-dependent, even though their presence or size does not in fact depend on a discriminant. This is because it is likely that they will move in a discriminant-changing assignment if they are a component of one of several discriminant-dependent parts of the same record. 

Each value of a discriminated type includes a value for each component of the type that does not depend on a discriminant[; this includes the discriminants themselves]. The values of discriminants determine which other component values are present in the value of the discriminated type. 

To be honest: Which values are present might depend on discriminants of some ancestor type that are constrained in an intervening [derived_type_definition](S0032). That's why we say "values of discriminants" instead of "values of the discriminants" - a subtle point.

A type declared with a [known_discriminant_part](S0058) is said to have known discriminants; its first subtype is unconstrained. A type declared with an [unknown_discriminant_part](S0057) is said to have unknown discriminants. A type declared without a [discriminant_part](S0056) has no discriminants, unless it is a derived type; if derived, such a type has the same sort of discriminants (known, unknown, or none) as its parent (or ancestor) type. A tagged class-wide type also has unknown discriminants. [Any subtype of a type with unknown discriminants is an unconstrained and indefinite subtype (see 3.2 and 3.3).] 

Discussion: An [unknown_discriminant_part](S0057) "(&lt&gt)" is only permitted in the declaration of a (generic or nongeneric) private type, private extension, or formal derived type. Hence, only such types, descendants thereof, and class-wide types can have unknown discriminants. An [unknown_discriminant_part](S0057) is used to indicate that the corresponding actual or full type might have discriminants without defaults, or be an unconstrained array subtype. Tagged class-wide types are also considered to have unknown discriminants because discriminants can be added by type extensions, so the total number of discriminants of any given value of a tagged class-wide type is not known at compile time.

A subtype with unknown discriminants is indefinite, and hence an object of such a subtype needs explicit initialization. If the subtype is limited, no (stand-alone) objects can be declared since initialization is not permitted (though formal parameters are permitted, and objects of the actual/full type will generally be declarable). A limited private type with unknown discriminants is "extremely" limited; such a type  is useful for keeping complete control over object creation within the package declaring the type.

A partial view of a type might have unknown discriminants, while the full view of the same type might have known, unknown, or no discriminants, 


#### Dynamic Semantics

An [access_definition](S0077) is elaborated when the value of a corresponding access discriminant is defined, either by evaluation of its [default_expression](S0060) or by elaboration of a [discriminant_constraint](S0061). [The elaboration of an [access_definition](S0077) creates the anonymous access type. When the expression defining the access discriminant is evaluated, it is converted to this anonymous access type (see 4.6).] 

Ramification: This conversion raises Constraint_Error if the initial value is null, or, for an object created by an allocator of an access type T, if the initial value is an access parameter that designates a view whose accessibility level is deeper than that of T. 

NOTE 1   If a discriminated type has [default_expression](S0060)s for its discriminants, then unconstrained variables of the type are permitted, and the values of the discriminants can be changed by an assignment to such a variable. If defaults are not provided for the discriminants, then all variables of the type are constrained, either by explicit constraint or by their initial value; the values of the discriminants of such a variable cannot be changed after initialization. 

Discussion: This connection between discriminant defaults and unconstrained variables can be a source of confusion. For Ada 95, we considered various ways to break the connection between defaults and unconstrainedness, but ultimately gave up for lack of a sufficiently simple and intuitive alternative.

An unconstrained discriminated subtype with defaults is called a mutable subtype, and a variable of such a subtype is called a mutable variable, because the discriminants of such a variable can change. There are no mutable arrays (that is, the bounds of an array object can never change), because there is no way in the language to define default values for the bounds. Similarly, there are no mutable class-wide subtypes, because there is no way to define the default tag, and defaults for discriminants are not allowed in the tagged case. Mutable tags would also require a way for the maximum possible size of such a class-wide subtype to be known. (In some implementations, all mutable variables are allocated with the maximum possible size. This approach is appropriate for real-time applications where implicit use of the heap is inappropriate.)

NOTE 2   The [default_expression](S0060) for a discriminant of a type is evaluated when an object of an unconstrained subtype of the type is created.

NOTE 3   Assignment to a discriminant of an object (after its initialization) is not allowed, since the name of a discriminant is a constant; neither [assignment_statement](S0130)s nor assignments inherent in passing as an in out or out parameter are allowed. Note however that the value of a discriminant can be changed by assigning to the enclosing object, presuming it is an unconstrained variable. 

Discussion: An [unknown_discriminant_part](S0057) is permitted only in the declaration of a private type (including generic formal private), private extension, or generic formal derived type. These are the things that will have a corresponding completion or generic actual, which will either define the discriminants, or say there are none. The (&lt&gt) indicates that the actual/full subtype might be an indefinite subtype. An [unknown_discriminant_part](S0057) is not permitted in a normal untagged derived type declaration, because there is no separate full type declaration for such a type. Note that (&lt&gt) allows unconstrained array bounds; those are somewhat like undefaulted discriminants.

For a derived type, either the discriminants are inherited as is, or completely respecified in a new [discriminant_part](S0056). In this latter case, each discriminant of the parent type shall be constrained, either to a specific value, or to equal one of the new discriminants. Constraining a parent type's discriminant to equal one of the new discriminants is like a renaming of the discriminant, except that the subtype of the new discriminant can be more restrictive than that of the parent's one. In any case, the new discriminant can share storage with the parent's discriminant. 

NOTE 4   A discriminant that is of a named access type is not called an access discriminant; that term is used only for discriminants defined by an [access_definition](S0077). 


#### Examples

Examples of discriminated types: 

```ada
type Buffer(Size : Buffer_Size := 100)  is        -- see 3.5.4
   record
      Pos   : Buffer_Size := 0;
      Value : String(1 .. Size);
   end record;

```

```ada
type Matrix_Rec(Rows, Columns : Integer) is
   record
      Mat : Matrix(1 .. Rows, 1 .. Columns);       -- see 3.6
   end record;

```

```ada
type Square(Side : Integer) is new
   Matrix_Rec(Rows =&gt Side, Columns =&gt Side);

```

```ada
type Double_Square(Number : Integer) is
   record
      Left  : Square(Number);
      Right : Square(Number);
   end record;

```

```ada
type Item(Number : Positive) is
   record
      Content : Integer;
      --  no component depends on the discriminant
   end record;

```


#### Extensions to Ada 83

The syntax for a [discriminant_specification](S0059) is modified to allow an access discriminant, with a type specified by an [access_definition](S0077) (see 3.10).

Discriminants are allowed on all composite types other than array types.

Discriminants may be of an access type. 


#### Wording Changes from Ada 83

[Discriminant_part](S0056)s are not elaborated, though an [access_definition](S0077) is elaborated when the discriminant is initialized.


### 3.7.1  Discriminant Constraints

A [discriminant_constraint](S0061) specifies the values of the discriminants for a given discriminated type. 


#### Language Design Principles

The rules in this clause are intentionally parallel to those given in 4.3.1, "Record Aggregates". 


#### Syntax

discriminant_constraint ::= 
   ([discriminant_association](S0062) {, [discriminant_association](S0062)})

discriminant_association ::= 
   [discriminant_[selector_name](S0092) {| discriminant_[selector_name](S0092)} =&gt] [expression](S0108)

A [discriminant_association](S0062) is said to be named if it has one or more discriminant_[selector_name](S0092)s; it is otherwise said to be positional. In a [discriminant_constraint](S0061), any positional associations shall precede any named associations. 


#### Name Resolution Rules

Each [selector_name](S0092) of a named [discriminant_association](S0062) shall resolve to denote a discriminant of the subtype being constrained; the discriminants so named are the associated discriminants of the named association. For a positional association, the associated discriminant is the one whose [discriminant_specification](S0059) occurred in the corresponding position in the [known_discriminant_part](S0058) that defined the discriminants of the subtype being constrained.

The expected type for the [expression](S0108) in a [discriminant_association](S0062) is that of the associated discriminant(s). 


#### Legality Rules

A [discriminant_constraint](S0061) is only allowed in a [subtype_indication](S0024) whose [subtype_mark](S0025) denotes either an unconstrained discriminated subtype, or an unconstrained access subtype whose designated subtype is an unconstrained discriminated subtype. there is a place within the immediate scope of the designated subtype where the designated subtype's view is constrained. 

A named [discriminant_association](S0062) with more than one [selector_name](S0092) is allowed only if the named discriminants are all of the same type. A [discriminant_constraint](S0061) shall provide exactly one value for each discriminant of the subtype being constrained.

The [expression](S0108) associated with an access discriminant shall be of a type convertible to the anonymous access type. 

Ramification: This implies both convertibility of designated types, and static accessibility. This implies that if an object of type T with an access discriminant is created by an allocator for an access type A, then it requires that the type of the [expression](S0108) associated with the access discriminant have an accessibility level that is not statically deeper than that of A. This is to avoid dangling references.


#### Dynamic Semantics

A [discriminant_constraint](S0061) is compatible with an unconstrained discriminated subtype if each discriminant value belongs to the subtype of the corresponding discriminant. 

Ramification: The "dependent compatibility check" has been eliminated in Ada 95. Any checking on subcomponents is performed when (and if) an object is created.

Discussion: There is no need to define compatibility with a constrained discriminated subtype, because one is not allowed to constrain it again.

A composite value satisfies a discriminant constraint if and only if each discriminant of the composite value has the value imposed by the discriminant constraint.

For the elaboration of a [discriminant_constraint](S0061), the [expression](S0108)s in the [discriminant_association](S0062)s are evaluated in an arbitrary order and converted to the type of the associated discriminant (which might raise Constraint_Error - see 4.6); the [expression](S0108) of a named association is evaluated (and converted) once for each associated discriminant. The result of each evaluation and conversion is the value imposed by the constraint for the associated discriminant. 

Reason: We convert to the type, not the subtype, so that the definition of compatibility of discriminant constraints is not vacuous.

NOTE   The rules of the language ensure that a discriminant of an object always has a value, either from explicit or implicit initialization. 

Discussion: Although it is illegal to constrain a class-wide tagged subtype, it is possible to have a partially constrained class-wide subtype: If the subtype S is defined by T(A =&gt B), then S'Class is partially constrained in the sense that objects of subtype S'Class have to have discriminants corresponding to A equal to B, but there can be other discriminants defined in extensions that are not constrained to any particular value. 


#### Examples

Examples (using types declared above in clause 3.7): 

```ada
Large   : Buffer(200);  --  constrained, always 200 characters
                        --   (explicit discriminant value)
Message : Buffer;       --  unconstrained, initially 100 characters
                        --   (default discriminant value)
Basis   : Square(5);    --  constrained, always 5 by 5
Illegal : Square;       --  illegal, a Square has to be constrained

```


#### Inconsistencies With Ada 83

Dependent compatibility checks are no longer performed on subtype declaration. Instead they are deferred until object creation (see 3.3.1). This is upward compatible for a program that does not raise Constraint_Error. 


#### Wording Changes from Ada 83

Everything in RM83-3.7.2(7-12), which specifies the initial values for discriminants, is now redundant with 3.3.1, 6.4.1, 8.5.1, and 12.4. Therefore, we don't repeat it here. Since the material is largely intuitive, but nevertheless complicated to state formally, it doesn't seem worth putting it in a "NOTE". 


### 3.7.2  Operations of Discriminated Types

[If a discriminated type has [default_expression](S0060)s for its discriminants, then unconstrained variables of the type are permitted, and the discriminants of such a variable can be changed by assignment to the variable. For a formal parameter of such a type, an attribute is provided to determine whether the corresponding actual parameter is constrained or unconstrained.] 


#### Static Semantics

For a [prefix](S0086) A that is of a discriminated type [(after any implicit dereference)], the following attribute is defined: 

A'ConstrainedYields the value True if A denotes a constant, a value, or a constrained variable, and False otherwise. 

Implementation Note: This attribute is primarily used on parameters, to determine whether the discriminants can be changed as part of an assignment. The Constrained attribute is statically True for in parameters. For in out and out parameters of a discriminated type, the value of this attribute needs to be passed as an implicit parameter, in general. However, if the type does not have defaults for its discriminants, the attribute is statically True, so no implicit parameter is needed. Parameters of a limited type with defaulted discriminants need this implicit parameter, unless there are no nonlimited views, because they might be passed to a subprogram whose body has visibility on a nonlimited view of the type, and hence might be able to assign to the object and change its discriminants. 


#### Erroneous Execution

The execution of a construct is erroneous if the construct has a constituent that is a [name](S0084) denoting a subcomponent that depends on discriminants, and the value of any of these discriminants is changed by this execution between evaluating the [name](S0084) and the last use (within this execution) of the subcomponent denoted by the [name](S0084). 

Ramification: This rule applies to [assignment_statement](S0130)s, calls (except when the discriminant-dependent subcomponent is an in parameter passed by copy), [indexed_component](S0089)s, and [slice](S0090)s. Ada 83 only covered the first two cases. AI83-00585 pointed out the situation with the last two cases. The cases of [object_renaming_declaration](S0170)s and generic formal in out objects are handled differently, by disallowing the situation at compile time. 


#### Extensions to Ada 83

For consistency with other attributes, we are allowing the prefix of Constrained to be a value as well as an object of a discriminated type, and also an implicit dereference. These extensions are not important capabilities, but there seems no reason to make this attribute different from other similar attributes. We are curious what most Ada 83 compilers do with F(1).X'Constrained.

We now handle in a general way the cases of erroneousness identified by AI83-00585, where the [prefix](S0086) of an [indexed_component](S0089) or [slice](S0090) is discriminant-dependent, and the evaluation of the index or discrete range changes the value of a discriminant. 


#### Wording Changes from Ada 83

We have moved all discussion of erroneous use of [name](S0084)s that denote discriminant-dependent subcomponents to this subclause. In Ada 83, it used to appear separately under [assignment_statement](S0130)s and subprogram calls. 


## 3.8  Record Types

A record object is a composite object consisting of named components. The value of a record object is a composite value consisting of the values of the components. 


#### Syntax

record_type_definition ::= [[abstract] tagged] [limited] [record_definition](S0064)

record_definition ::= 
    record
       [component_list](S0065)
    end record
  | null record

component_list ::= 
      [component_item](S0066) {[component_item](S0066)}
   | {[component_item](S0066)} [variant_part](S0068)
   |  null;

component_item ::= [component_declaration](S0067) | [representation_clause](S0263)

component_declaration ::= 
   [defining_identifier_list](S0030) : [component_definition](S0053) [:= [default_expression](S0060)];


#### Name Resolution Rules

The expected type for the [default_expression](S0060), if any, in a [component_declaration](S0067) is the type of the component. 


#### Legality Rules

A [default_expression](S0060) is not permitted if the component is of a limited type.

Each [component_declaration](S0067) declares a component of the record type. Besides components declared by [component_declaration](S0067)s, the components of a record type include any components declared by [discriminant_specification](S0059)s of the record type declaration. [The identifiers of all components of a record type shall be distinct.] 

Proof: The identifiers of all components of a record type have to be distinct because they are all declared immediately within the same declarative region. See Section 8. 

Within a [type_declaration](S0020), a [name](S0084) that denotes a component, protected subprogram, or entry of the type is allowed only in the following cases:

A [name](S0084) that denotes any component, protected subprogram, or entry is allowed within a representation item that occurs within the declaration of the composite type.

A [name](S0084) that denotes a noninherited discriminant is allowed within the declaration of the type, but not within the [discriminant_part](S0056). If the discriminant is used to define the constraint of a component, the bounds of an entry family, or the constraint of the parent subtype in a [derived_type_definition](S0032) then its name shall appear alone as a [direct_name](S0085) (not as part of a larger expression or expanded name). A discriminant shall not be used to define the constraint of a scalar component. 

Reason: The penultimate restriction simplifies implementation, and allows the outer discriminant and the inner discriminant or bound to possibly share storage. 

Ramification: Other rules prevent such a discriminant from being an inherited one. 

Reason: The last restriction is inherited from Ada 83. The restriction is not really necessary from a language design point of view, but we did not remove it, in order to avoid unnecessary changes to existing compilers. 

Discussion: Note that a discriminant can be used to define the constraint for a component that is of an access-to-composite type. 

Reason: The above rules, and a similar one in 6.1 for formal parameters, are intended to allow initializations of components or parameters to occur in an arbitrary order - whatever order is most efficient, since one [default_expression](S0060) cannot depend on the value of another one. It also prevent circularities. 

Ramification: Inherited discriminants are not allowed to be denoted, except within representation items. However, the discriminant_[selector_name](S0092) of the parent [subtype_indication](S0024) is allowed to denote a discriminant of the parent. 

If the name of the current instance of a type (see 8.6) is used to define the constraint of a component, then it shall appear as a [direct_name](S0085) that is the [prefix](S0086) of an [attribute_reference](S0093) whose result is of an access type, and the [attribute_reference](S0093) shall appear alone. 

Reason: This rule allows T'Access or T'Unchecked_Access, but disallows, for example, a range constraint (1..T'Size). Allowing things like (1..T'Size) would mean that a per-object constraint could affect the size of the object, which would be bad. 


#### Static Semantics

The [component_definition](S0053) of a [component_declaration](S0067) defines the (nominal) subtype of the component. If the reserved word aliased appears in the [component_definition](S0053), then the component is aliased (see 3.10). 

Ramification: In this case, the nominal subtype cannot be an unconstrained discriminated subtype. See 3.6. 

If the [component_list](S0065) of a record type is defined by the reserved word null and there are no discriminants, then the record type has no components and all records of the type are null records. A [record_definition](S0064) of null record is equivalent to record null; end record. 

Ramification: This short-hand is available both for declaring a record type and a record extension - see 3.9.1. 


#### Dynamic Semantics

The elaboration of a [record_type_definition](S0063) creates the record type and its first subtype, and consists of the elaboration of the [record_definition](S0064). The elaboration of a [record_definition](S0064) consists of the elaboration of its [component_list](S0065), if any.

The elaboration of a [component_list](S0065) consists of the elaboration of the [component_item](S0066)s and [variant_part](S0068), if any, in the order in which they appear. The elaboration of a [component_declaration](S0067) consists of the elaboration of the [component_definition](S0053). 

Discussion: If the [defining_identifier_list](S0030) has more than one [defining_identifier](S0019), we presume here that the transformation explained in 3.3.1 has already taken place. Alternatively, we could say that the [component_definition](S0053) is elaborated once for each [defining_identifier](S0019) in the list. 

Within the definition of a composite type, if a [component_definition](S0053) or [discrete_subtype_definition](S0052) (see 9.5.2) includes a [name](S0084) that denotes a discriminant of the type, or that is an [attribute_reference](S0093) whose [prefix](S0086) denotes the current instance of the type, the expression containing the [name](S0084) is called a per-object expression, and the constraint being defined is called a per-object constraint. For the elaboration of a [component_definition](S0053) of a [component_declaration](S0067), if the [constraint](S0026)  of the [subtype_indication](S0024)  is not a per-object constraint, then the [subtype_indication](S0024) is elaborated. On the other hand, if the [constraint](S0026)  is a per-object constraint, then the elaboration consists of the evaluation of any included expression that is not part of a per-object expression. 

Discussion: The evaluation of other expressions that appear in [component_definition](S0053)s and [discrete_subtype_definition](S0052)s is performed when the type definition is elaborated. The evaluation of expressions that appear as [default_expression](S0060)s is postponed until an object is created. Expressions in representation items that appear within a composite type definition are evaluated according to the rules of the particular representation item. 

NOTE 1   A [component_declaration](S0067) with several identifiers is equivalent to a sequence of single [component_declaration](S0067)s, as explained in 3.3.1.

NOTE 2   The [default_expression](S0060) of a record component is only evaluated upon the creation of a default-initialized object of the record type (presuming the object has the component, if it is in a [variant_part](S0068) - see 3.3.1).

NOTE 3   The subtype defined by a [component_definition](S0053) (see 3.6) has to be a definite subtype.

NOTE 4   If a record type does not have a [variant_part](S0068), then the same components are present in all values of the type.

NOTE 5   A record type is limited if it has the reserved word limited in its definition, or if any of its components are limited (see 7.5).

NOTE 6   The predefined operations of a record type include membership tests, qualification, and explicit conversion. If the record type is nonlimited, they also include assignment and the predefined equality operators.

NOTE 7   A component of a record can be named with a [selected_component](S0091). A value of a record can be specified with a [record_aggregate](S0098), unless the record type is limited.


#### Examples

Examples of record type declarations: 

```ada
type Date is
   record
      Day   : Integer range 1 .. 31;
      Month : Month_Name;
      Year  : Integer range 0 .. 4000;
   end record;

```

```ada
type Complex is
   record
      Re : Real := 0.0;
      Im : Real := 0.0;
   end record;

```

Examples of record variables: 

```ada
Tomorrow, Yesterday : Date;
A, B, C : Complex;

```

```ada
-- both components of A, B, and C are implicitly initialized to zero 

```


#### Extensions to Ada 83

The syntax rule for [component_declaration](S0067) is modified to use [component_definition](S0053) (instead of component_subtype_definition). The effect of this change is to allow the reserved word aliased before the component_subtype_definition.

A short-hand is provided for defining a null record type (and a null record extension), as these will be more common for abstract root types (and derived types without additional components).

The syntax rule for [record_type_definition](S0063) is modified to allow the reserved words tagged and limited. Tagging is new. Limitedness is now orthogonal to privateness. In Ada 83 the syntax implied that limited private was sort of more private than private. However, limitedness really has nothing to do with privateness; limitedness simply indicates the lack of assignment capabilities, and makes perfect sense for nonprivate types such as record types. 


#### Wording Changes from Ada 83

The syntax rules now allow [representation_clause](S0263)s to appear in a [record_definition](S0064). This is not a language extension, because Legality Rules prevent all language-defined representation clauses from appearing there. However, an implementation-defined [attribute_definition_clause](S0265) could appear there. The reason for this change is to allow the rules for [representation_clause](S0263)s and representation pragmas to be as similar as possible. 


### 3.8.1  Variant Parts and Discrete Choices

A record type with a [variant_part](S0068) specifies alternative lists of components. Each [variant](S0069) defines the components for the value or values of the discriminant covered by its [discrete_choice_list](S0070). 

Discussion: [Discrete_choice_list](S0070)s and [discrete_choice](S0071)s are said to cover values as defined below; which [discrete_choice_list](S0070) covers a value determines which of various alternatives is chosen. These are used in [variant_part](S0068)s, [array_aggregate](S0104)s, and [case_statement](S0133)s. 


#### Language Design Principles

The definition of "cover" in this subclause and the rules about discrete choices are designed so that they are also appropriate for array aggregates and case statements.

The rules of this subclause intentionally parallel those for case statements. 


#### Syntax

variant_part ::= 
   case discriminant_[direct_name](S0085) is
       [variant](S0069)
      {[variant](S0069)}
   end case;

variant ::= 
   when [discrete_choice_list](S0070) =&gt
      [component_list](S0065)

discrete_choice_list ::= [discrete_choice](S0071) {| [discrete_choice](S0071)}

discrete_choice ::= [expression](S0108) | [discrete_range](S0055) | others


#### Name Resolution Rules

The discriminant_[direct_name](S0085) shall resolve to denote a discriminant (called the discriminant of the [variant_part](S0068)) specified in the [known_discriminant_part](S0058) of the [full_type_declaration](S0021) that contains the [variant_part](S0068). The expected type for each [discrete_choice](S0071) in a [variant](S0069) is the type of the discriminant of the [variant_part](S0068). 

Ramification: A [full_type_declaration](S0021) with a [variant_part](S0068) has to have a (new) [known_discriminant_part](S0058); the discriminant of the [variant_part](S0068) cannot be an inherited discriminant. 


#### Legality Rules

The discriminant of the [variant_part](S0068) shall be of a discrete type. 

Ramification: It shall not be of an access type, named or anonymous.

The [expression](S0108)s and [discrete_range](S0055)s given as [discrete_choice](S0071)s in a [variant_part](S0068) shall be static. The [discrete_choice](S0071) others shall appear alone in a [discrete_choice_list](S0070), and such a [discrete_choice_list](S0070), if it appears, shall be the last one in the enclosing construct.

A [discrete_choice](S0071) is defined to cover a value in the following cases: 

A [discrete_choice](S0071) that is an [expression](S0108) covers a value if the value equals the value of the [expression](S0108) converted to the expected type.

A [discrete_choice](S0071) that is a [discrete_range](S0055) covers all values (possibly none) that belong to the range.

The [discrete_choice](S0071) others covers all values of its expected type that are not covered by previous [discrete_choice_list](S0070)s of the same construct. 

Ramification: For [case_statement](S0133)s, this includes values outside the range of the static subtype (if any) to be covered by the choices. It even includes values outside the base range of the case expression's type, since values of numeric types (and undefined values of any scalar type?) can be outside their base range. 

A [discrete_choice_list](S0070) covers a value if one of its [discrete_choice](S0071)s covers the value.

The possible values of the discriminant of a [variant_part](S0068) shall be covered as follows: 

If the discriminant is of a static constrained scalar subtype, then each non-others [discrete_choice](S0071) shall cover only values in that subtype, and each value of that subtype shall be covered by some [discrete_choice](S0071) [(either explicitly or by others)];

If the type of the discriminant is a descendant of a generic formal scalar type then the [variant_part](S0068) shall have an others [discrete_choice](S0071); 

Reason: The base range is not known statically in this case. 

Otherwise, each value of the base range of the type of the discriminant shall be covered [(either explicitly or by others)]. 

Two distinct [discrete_choice](S0071)s of a [variant_part](S0068) shall not cover the same value.


#### Static Semantics

If the [component_list](S0065) of a [variant](S0069) is specified by null, the variant has no components.

The discriminant of a [variant_part](S0068) is said to govern the [variant_part](S0068) and its [variant](S0069)s. In addition, the discriminant of a derived type governs a [variant_part](S0068) and its [variant](S0069)s if it corresponds (see 3.7) to the discriminant of the [variant_part](S0068).


#### Dynamic Semantics

A record value contains the values of the components of a particular [variant](S0069) only if the value of the discriminant governing the [variant](S0069) is covered by the [discrete_choice_list](S0070) of the [variant](S0069). This rule applies in turn to any further [variant](S0069) that is, itself, included in the [component_list](S0065) of the given [variant](S0069).

The elaboration of a [variant_part](S0068) consists of the elaboration of the [component_list](S0065) of each [variant](S0069) in the order in which they appear. 


#### Examples

Example of record type with a variant part: 

```ada
type Device is (Printer, Disk, Drum);
type State  is (Open, Closed);

```

```ada
type Peripheral(Unit : Device := Disk) is
   record
      Status : State;
      case Unit is
         when Printer =&gt
            Line_Count : Integer range 1 .. Page_Size;
         when others =&gt
            Cylinder   : Cylinder_Index;
            Track      : Track_Number;
      end case;
   end record;

```

Examples of record subtypes: 

```ada
subtype Drum_Unit is Peripheral(Drum);
subtype Disk_Unit is Peripheral(Disk);

```

Examples of constrained record variables: 

```ada
Writer   : Peripheral(Unit  =&gt Printer);
Archive  : Disk_Unit;

```


#### Extensions to Ada 83

In Ada 83, the discriminant of a [variant_part](S0068) is not allowed to be of a generic formal type. This restriction is removed in Ada 95; an others [discrete_choice](S0071) is required in this case. 


#### Wording Changes from Ada 83

The syntactic category choice is removed. The syntax rules for [variant](S0069), [array_aggregate](S0104), and [case_statement](S0133) now use [discrete_choice_list](S0070) or [discrete_choice](S0071) instead. The syntax rule for [record_aggregate](S0098) now defines its own syntax for named associations.

We have added the term Discrete Choice to the title since this is where they are talked about. This is analogous to the name of the subclause "Index Constraints and Discrete Ranges" in the clause on Array Types.

The rule requiring that the discriminant denote a discriminant of the type being defined seems to have been left implicit in RM83. 


## 3.9  Tagged Types and Type Extensions

[ Tagged types and type extensions support object-oriented programming, based on inheritance with extension and run-time polymorphism via dispatching operations. ]


#### Language Design Principles

The intended implementation model is for a tag to be represented as a pointer to a statically allocated and link-time initialized type descriptor. The type descriptor contains the address of the code for each primitive operation of the type. It probably also contains other information, such as might make membership tests convenient and efficient.

The primitive operations of a tagged type are known at its first freezing point; the type descriptor is laid out at that point. It contains linker symbols for each primitive operation; the linker fills in the actual addresses.

Other implementation models are possible.

The rules ensure that "dangling dispatching" is impossible; that is, when a dispatching call is made, there is always a body to execute. This is different from some other object-oriented languages, such as Smalltalk, where it is possible to get a run-time error from a missing method.

Dispatching calls should be efficient, and should have a bounded worst-case execution time. This is important in a language intended for real-time applications. In the intended implementation model, a dispatching call involves calling indirect through the appropriate slot in the dispatch table. No complicated "method lookup" is involved.

The programmer should have the choice at each call site of a dispatching operation whether to do a dispatching call or a statically determined call (i.e. whether the body executed should be determined at run time or at compile time).

The same body should be executed for a call where the tag is statically determined to be T'Tag as for a dispatching call where the tag is found at run time to be T'Tag. This allows one to test a given tagged type with statically determined calls, with some confidence that run-time dispatching will produce the same behavior.

All views of a type should share the same type descriptor and the same tag.

The visibility rules determine what is legal at compile time; they have nothing to do with what bodies can be executed at run time. Thus, it is possible to dispatch to a subprogram whose declaration is not visible at the call site. In fact, this is one of the primary facts that gives object-oriented programming its power. The subprogram that ends up being dispatched to by a given call might even be designed long after the call site has been coded and compiled.

Given that Ada has overloading, determining whether a given subprogram overrides another is based both on the names and the type profiles of the operations.

When a type extension is declared, if there is any place within its immediate scope where a certain subprogram of the parent is visible, then a matching subprogram should override. If there is no such place, then a matching subprogram should be totally unrelated, and occupy a different slot in the type descriptor. This is important to preserve the privacy of private parts; when an operation declared in a private part is inherited, the inherited version can be overridden only in that private part, in the package body, and in any children of the package.

If an implementation shares code for instances of generic bodies, it should be allowed to share type descriptors of tagged types declared in the generic body, so long as they are not extensions of types declared in the specification of the generic unit. 


#### Static Semantics

A record type or private type that has the reserved word tagged in its declaration is called a tagged type. [When deriving from a tagged type, additional components may be defined. As for any derived type, additional primitive subprograms may be defined, and inherited primitive subprograms may be overridden.] The derived type is called an extension of the ancestor type, or simply a type extension. Every type extension is also a tagged type, and is either a record extension or a private extension of some other tagged type. A record extension is defined by a [derived_type_definition](S0032) with a [record_extension_part](S0072). A private extension, which is a partial view of a record extension, can be declared in the visible part of a package (see 7.3) or in a generic formal part (see 12.5.1).

Glossary entry: The objects of a tagged type have a run-time type tag, which indicates the specific type with which the object was originally created. An operand of a class-wide tagged type can be used in a dispatching call; the tag indicates which subprogram body to invoke. Nondispatching calls, in which the subprogram body to invoke is determined at compile time, are also allowed. Tagged types may be extended with additional components.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[tagged type], Def=[a type whose objects each have a run-time type tag, which indicates the specific type for which the object was originally created], Note1=[Tagged types can be extended with additional components.] 

Ramification: If a tagged type is declared other than in a [package_specification](S0162), it is impossible to add new primitive subprograms for that type, although it can inherit primitive subprograms, and those can be overridden. If the user incorrectly thinks a certain subprogram is primitive when it is not, and tries to call it with a dispatching call, an error message will be given at the call site.

Note that the accessibility rules imply that a tagged type declared in a library [package_specification](S0162) cannot be extended in a nested subprogram or task body. 

An object of a tagged type has an associated (run-time) tag that identifies the specific tagged type used to create the object originally. [ The tag of an operand of a class-wide tagged type T'Class controls which subprogram body is to be executed when a primitive subprogram of type T is applied to the operand (see 3.9.2); using a tag to control which body to execute is called dispatching.] 

The tag of a specific tagged type identifies the [full_type_declaration](S0021) of the type. If a declaration for a tagged type occurs within a [generic_package_declaration](S0238), then the corresponding type declarations in distinct instances of the generic package are associated with distinct tags. For a tagged type that is local to a generic package body, the language does not specify whether repeated instantiations of the generic body result in distinct tags. 

Reason: This eases generic code sharing. 

Implementation Note: The language does not specify whether repeated elaborations of the same [full_type_declaration](S0021) correspond to distinct tags. In most cases, we expect that all elaborations will correspond to the same tag, since the tag will frequently be the address (or index) of a statically allocated type descriptor. However, with shared generics, the type descriptor might have to be allocated on a per-instance basis, which in some implementation models implies per-elaboration of the instantiation. 

The following language-defined library package exists: 

```ada
package Ada.Tags  is
    type Tag is private;

```

```ada
    function Expanded_Name(T : Tag) return String;
    function External_Tag(T : Tag) return String;
    function Internal_Tag(External : String) return Tag;

```

```ada
    Tag_Error : exception;

```

```ada
private
   ... -- not specified by the language
end Ada.Tags;

```

Reason: Tag is a nonlimited, definite subtype, because it needs the equality operators, so that tag checking makes sense. Also, equality, assignment, and object declaration are all useful capabilities for this subtype.

For an object X and a type T, "X'Tag = T'Tag" is not needed, because a membership test can be used. However, comparing the tags of two objects cannot be done via membership. This is one reason to allow equality for type Tag. 

The function Expanded_Name returns the full expanded name of the first subtype of the specific type identified by the tag, in upper case, starting with a root library unit. The result is implementation defined if the type is declared within an unnamed [block_statement](S0138). 

To be honest: This name, as well as each [prefix](S0086) of it, does not denote a [renaming_declaration](S0169). 

Implementation defined: The result of Tags.Expanded_Name for types declared within an unnamed [block_statement](S0138).

The function External_Tag returns a string to be used in an external representation for the given tag. The call External_Tag(S'Tag) is equivalent to the [attribute_reference](S0093) S'External_Tag (see 13.3). 

Reason: It might seem redundant to provide both the function External_Tag and the attribute External_Tag. The function is needed because the attribute can't be applied to values of type Tag. The attribute is needed so that it can be specifiable via an [attribute_definition_clause](S0265). 

The function Internal_Tag returns the tag that corresponds to the given external tag, or raises Tag_Error if the given string is not the external tag for any specific type of the partition.



For every subtype S of a tagged type T (specific or class-wide), the following attributes are defined: 

S'ClassS'Class denotes a subtype of the class-wide type (called T'Class in this document) for the class rooted at T (or if S already denotes a class-wide subtype, then S'Class is the same as S).

S'Class is unconstrained. However, if S is constrained, then the values of S'Class are only those that when converted to the type T belong to S. 

Ramification: This attribute is defined for both specific and class-wide subtypes. The definition is such that S'Class'Class is the same as S'Class.

Note that if S is constrained, S'Class is only partially constrained, since there might be additional discriminants added in descendants of T which are not constrained. 

Reason: The Class attribute is not defined for untagged subtypes (except for incomplete types and private types whose full view is tagged - see 3.10.1 and 7.3.1) so as to preclude implicit conversion in the absence of run-time type information. If it were defined for untagged subtypes, it would correspond to the concept of universal types provided for the predefined numeric classes. 

S'TagS'Tag denotes the tag of the type T (or if T is class-wide, the tag of the root type of the corresponding class). The value of this attribute is of type Tag. 

Reason: S'Class'Tag equals S'Tag, to avoid generic contract model problems when S'Class is the actual type associated with a generic formal derived type.

Given a [prefix](S0086) X that is of a class-wide tagged type [(after any implicit dereference)], the following attribute is defined: 

X'TagX'Tag denotes the tag of X. The value of this attribute is of type Tag. 

Reason: X'Tag is not defined if X is of a specific type. This is primarily to avoid confusion that might result about whether the Tag attribute should reflect the tag of the type of X, or the tag of X. No such confusion is possible if X is of a class-wide type. 


#### Dynamic Semantics

The tag associated with an object of a tagged type is determined as follows: 

The tag of a stand-alone object, a component, or an [aggregate](S0097) of a specific tagged type T identifies T. 

Discussion: The tag of a formal parameter of type T is not necessarily the tag of T, if, for example, the actual was a type conversion. 

The tag of an object created by an allocator for an access type with a specific designated tagged type T, identifies T. 

Discussion: The tag of an object designated by a value of such an access type might not be T, if, for example, the access value is the result of a type conversion.

The tag of an object of a class-wide tagged type is that of its initialization expression. 

Ramification: The tag of an object (even a class-wide one) cannot be changed after it is initialized, since a "class-wide" [assignment_statement](S0130) raises Constraint_Error if the tags don't match, and a "specific" [assignment_statement](S0130) does not affect the tag. 

The tag of the result returned by a function whose result type is a specific tagged type T identifies T. 

Implementation Note: This requires a runtime check for limited tagged types, since they are returned "by-reference". For a nonlimited type, a new anonymous object with the appropriate tag is created as part of the function return, and then assigned the value of the return expression. See 6.5, "Return Statements". 

The tag of the result returned by a function with a class-wide result type is that of the return expression. 

The tag is preserved by type conversion and by parameter passing. The tag of a value is the tag of the associated object (see 6.2).


#### Implementation Permissions

The implementation of the functions in Ada.Tags may raise Tag_Error if no specific type corresponding to the tag passed as a parameter exists in the partition at the time the function is called. 

Reason: In most implementations, repeated elaborations of the same [type_declaration](S0020) will all produce the same tag. In such an implementation, Tag_Error will be raised in cases where the internal or external tag was passed from a different partition. However, some implementations might create a new tag value at run time for each elaboration of a [type_declaration](S0020). In that case, Tag_Error could also be raised if the created type no longer exists because the subprogram containing it has returned, for example. We don't require the latter behavior; hence the word "may" in this rule. 

Implementation Advice: 

NOTE 1   A type declared with the reserved word tagged should normally be declared in a [package_specification](S0162), so that new primitive subprograms can be declared for it.

NOTE 2   Once an object has been created, its tag never changes.

NOTE 3   Class-wide types are defined to have unknown discriminants (see 3.7). This means that objects of a class-wide type have to be explicitly initialized (whether created by an [object_declaration](S0029) or an [allocator](S0122)), and that [aggregate](S0097)s have to be explicitly qualified with a specific type when their expected type is class-wide.

NOTE 4   If S denotes an untagged private type whose full type is tagged, then S'Class is also allowed before the full type definition, but only in the private part of the package in which the type is declared (see 7.3.1). Similarly, the Class attribute is defined for incomplete types whose full type is tagged, but only within the library unit in which the incomplete type is declared (see 3.10.1). 


#### Examples

Examples of tagged record types: 

```ada
type Point is tagged
  record
    X, Y : Real := 0.0;
  end record;

```

```ada
type Expression is tagged null record;
  -- Components will be added by each extension

```


#### Extensions to Ada 83

Tagged types are a new concept. 


### 3.9.1  Type Extensions

[ Every type extension is a tagged type, and is either a record extension or a private extension of some other tagged type.] 


#### Language Design Principles

We want to make sure that we can extend a generic formal tagged type, without knowing its discriminants.

We don't want to allow components in an extension aggregate to depend on discriminants inherited from the parent value, since such dependence requires staticness in aggregates, at least for variants. 


#### Syntax

record_extension_part ::= with [record_definition](S0064)


#### Legality Rules

The parent type of a record extension shall not be a class-wide type. If the parent type is nonlimited, then each of the components of the [record_extension_part](S0072) shall be nonlimited. The accessibility level (see 3.10.2) of a record extension shall not be statically deeper than that of its parent type. In addition to the places where Legality Rules normally apply (see 12.3), these rules apply also in the private part of an instance of a generic unit. 

Reason: If the parent is a limited formal type, then the actual might be nonlimited.

A similar accessibility rule is not needed for private extensions, because in a package, the rule will apply to the [full_type_declaration](S0021), and for a generic formal private extension, the actual is all that matters.

A type extension shall not be declared in a generic body if the parent type is declared outside that body.

Reason: This paragraph ensures that a dispatching call will never attempt to execute an inaccessible subprogram body.

The part about generic bodies is necessary in order to preserve the contract model.

Since a generic unit can be instantiated at a deeper accessibility level than the generic unit, it is necessary to prevent type extensions whose parent is declared outside the generic unit. The same is true if the parent is a formal of the generic unit . If the parent is declared in the [generic_declaration](S0236) (but is not a formal), we don't run afoul of the accessibility rules, because we know that the instance declaration and body will be at the same accessibility level. However, we still have a problem in that case, because it might have an unknown number of abstract subprograms, as in the following example: 

```ada
package P is
    type T is tagged null record;
    function F return T; -- Inherited versions will be abstract.
end P;

```

```ada
generic
    type TT is tagged private;
package Gp is
    type NT is abstract new TT with null record;
    procedure Q(X : in NT) is abstract;
end Gp;

```

```ada
package body Gp is
    type NT2 is new NT with null record; -- Illegal!
    procedure Q(X : in NT2) is begin null; end Q;
    -- Is this legal or not? Can't decide because
    -- we don't know whether TT had any functions that go abstract
    -- on extension.
end Gp;

```

```ada
package I is new Gp(TT =&gt P.T);

```

I.NT is an abstract type with two abstract subprograms: F (inherited as abstract) and Q (explicitly declared as abstract). But the generic body doesn't know about F, so we don't know that it needs to be overridden to make a nonabstract extension of NT. Furthermore, a formal tagged limited private type can be extended with limited components, but the actual might not be limited, which would allow assignment of limited types, which is bad. Hence, we have to disallow this case as well.

If TT were declared as abstract, then we could have the same problem with abstract procedures.

We considered disallowing all tagged types in a generic body, for simplicity. We decided not to go that far, in order to avoid unnecessary restrictions.

We also considered trying make the accessibility level part of the contract; i.e. invent some way of saying (in the [generic_declaration](S0236)) "all instances of this generic unit will have the same accessibility level as the [generic_declaration](S0236)". Unfortunately, that doesn't solve the part of the problem having to do with abstract types.

Children of generic units obviate the need for extension in the body somewhat. 


#### Dynamic Semantics

The elaboration of a [record_extension_part](S0072) consists of the elaboration of the [record_definition](S0064). 

NOTE 1   The term "type extension" refers to a type as a whole. The term "extension part" refers to the piece of text that defines the additional components (if any) the type extension has relative to its specified ancestor type. 

Discussion: We considered other terminology, such as "extended type". However, the terms "private extended type" and "record extended type" did not convey the proper meaning. Hence, we have chosen to uniformly use the term "extension" as the type resulting from extending a type, with "private extension" being one produced by privately extending the type, and "record extension" being one produced by extending the type with an additional record-like set of components. Note also that the term "type extension" refers to the result of extending a type in the language Oberon as well (though there the term "extended type" is also used, interchangeably, perhaps because Oberon doesn't have the concept of a "private extension"). 

NOTE 2   The accessibility rules imply that a tagged type declared in a library [package_specification](S0162) can be extended only at library level or as a generic formal. When the extension is declared immediately within a [package_body](S0163), primitive subprograms are inherited and are overridable, but new primitive subprograms cannot be added.

NOTE 3   A [name](S0084) that denotes a component (including a discriminant) of the parent type is not allowed within the [record_extension_part](S0072). Similarly, a [name](S0084) that denotes a component defined within the [record_extension_part](S0072) is not allowed within the [record_extension_part](S0072). It is permissible to use a [name](S0084) that denotes a discriminant of the record extension, providing there is a new [known_discriminant_part](S0058) in the enclosing type declaration. (The full rule is given in 3.8.) 

Reason: The restriction against depending on discriminants of the parent is to simplify the definition of extension aggregates. The restriction against using parent components in other ways is methodological; it presumably simplifies implementation as well. 

NOTE 4   Each visible component of a record extension has to have a unique name, whether the component is (visibly) inherited from the parent type or declared in the [record_extension_part](S0072) (see 8.3). 


#### Examples

Examples of record extensions (of types defined above in 3.9): 

```ada
type Painted_Point is new Point with
  record
    Paint : Color := White;
  end record;
    -- Components X and Y are inherited

```

```ada
Origin : constant Painted_Point := (X | Y =&gt 0.0, Paint =&gt Black);

```

```ada
type Literal is new Expression with
  record                 -- a leaf in an Expression tree
    Value : Real;
  end record;

```

```ada
type Expr_Ptr is access all Expression'Class;
                               -- see 3.10

```

```ada
type Binary_Operation is new Expression with
  record                 -- an internal node in an Expression tree
    Left, Right : Expr_Ptr;
  end record;

```

```ada
type Addition is new Binary_Operation with null record;
type Subtraction is new Binary_Operation with null record;
  -- No additional components needed for these extensions

```

```ada
Tree : Expr_Ptr :=         -- A tree representation of "5.0 + (13.07.0)"
   new Addition'(
      Left  =&gt new Literal'(Value =&gt 5.0),
      Right =&gt new Subtraction'(
         Left  =&gt new Literal'(Value =&gt 13.0),
         Right =&gt new Literal'(Value =&gt 7.0)));

```


#### Extensions to Ada 83

Type extension is a new concept. 


### 3.9.2  Dispatching Operations of Tagged Types

The primitive subprograms of a tagged type are called dispatching operations. [A dispatching operation can be called using a statically determined controlling tag, in which case the body to be executed is determined at compile time. Alternatively, the controlling tag can be dynamically determined, in which case the call dispatches to a body that is determined at run time;] such a call is termed a dispatching call. [As explained below, the properties of the operands and the context of a particular call on a dispatching operation determine how the controlling tag is determined, and hence whether or not the call is a dispatching call. Run-time polymorphism is achieved when a dispatching operation is called by a dispatching call.] 


#### Language Design Principles

The controlling tag determination rules are analogous to the overload resolution rules, except they deal with run-time type identification (tags) rather than compile-time type resolution. As with overload resolution, controlling tag determination may depend on operands or result context. 


#### Static Semantics

A call on a dispatching operation is a call whose [name](S0084) or [prefix](S0086) denotes the declaration of a primitive subprogram of a tagged type, that is, a dispatching operation. A controlling operand in a call on a dispatching operation of a tagged type T is one whose corresponding formal parameter is of type T or is of an anonymous access type with designated type T; the corresponding formal parameter is called a controlling formal parameter. If the controlling formal parameter is an access parameter, the controlling operand is the object designated by the actual parameter, rather than the actual parameter itself. If the call is to a (primitive) function with result type T, then the call has a controlling result - the context of the call can control the dispatching. 

Ramification: This definition implies that a call through the dereference of an access-to-subprogram value is never considered a call on a dispatching operation. Note also that if the [prefix](S0086) denotes a [renaming_declaration](S0169), the place where the renaming occurs determines whether it is primitive; the thing being renamed is irrelevant. 

A [name](S0084) or expression of a tagged type is either statically tagged, dynamically tagged, or tag indeterminate, according to whether, when used as a controlling operand, the tag that controls dispatching is determined statically by the operand's (specific) type, dynamically by its tag at run time, or from context. A [qualified_expression](S0121) or parenthesized expression is statically, dynamically, or indeterminately tagged according to its operand. For other kinds of [name](S0084)s and expressions, this is determined as follows: 

The [name](S0084) or expression is statically tagged if it is of a specific tagged type and, if it is a call with a controlling result, it has at least one statically tagged controlling operand; 

Discussion: It is illegal to have both statically tagged and dynamically tagged controlling operands in the same call -- see below. 

The [name](S0084) or expression is dynamically tagged if it is of a class-wide type, or it is a call with a controlling result and at least one dynamically tagged controlling operand;

The [name](S0084) or expression is tag indeterminate if it is a call with a controlling result, all of whose controlling operands (if any) are tag indeterminate. 

[A [type_conversion](S0120) is statically or dynamically tagged according to whether the type determined by the [subtype_mark](S0025) is specific or class-wide, respectively.] For a controlling operand that is designated by an actual parameter, the controlling operand is statically or dynamically tagged according to whether the designated type of the actual parameter is specific or class-wide, respectively. 

Ramification: A [type_conversion](S0120) is never tag indeterminate, even if its operand is. A designated object is never tag indeterminate.


#### Legality Rules

A call on a dispatching operation shall not have both dynamically tagged and statically tagged controlling operands. 

Reason: This restriction is intended to minimize confusion between whether the dynamically tagged operands are implicitly converted to, or tag checked against the specific type of the statically tagged operand(s). 

If the expected type for an expression or [name](S0084) is some specific tagged type, then the expression or [name](S0084) shall not be dynamically tagged unless it is a controlling operand in a call on a dispatching operation. Similarly, if the expected type for an expression is an anonymous access-to-specific tagged type, then the expression shall not be of an access-to-class-wide type unless it designates a controlling operand in a call on a dispatching operation. 

Reason: This prevents implicit "truncation" of a dynamically-tagged value to the specific type of the target object/formal. An explicit conversion is required to request this truncation. 

Ramification: This rule applies to all expressions or [name](S0084)s with a specific expected type, not just those that are actual parameters to a dispatching call. This rule does not apply to a membership test whose [expression](S0108) is class-wide, since any type that covers the tested type is explicitly allowed. See 4.5.2. 

In the declaration of a dispatching operation of a tagged type, everywhere a subtype of the tagged type appears as a subtype of the profile (see 6.1), it shall statically match the first subtype of the tagged type. If the dispatching operation overrides an inherited subprogram, it shall be subtype conformant with the inherited subprogram. A dispatching operation shall not be of convention Intrinsic. If a dispatching operation overrides the predefined equals operator, then it shall be of convention Ada [(either explicitly or by default - see 6.3.1)]. 

Reason: These rules ensure that constraint checks can be performed by the caller in a dispatching call, and parameter passing conventions match up properly. A special rule on aggregates prevents values of a tagged type from being created that are outside of its first subtype. 

The [default_expression](S0060) for a controlling formal parameter of a dispatching operation shall be tag indeterminate. A controlling formal parameter that is an access parameter shall not have a [default_expression](S0060). 

Reason: The first part ensures that the [default_expression](S0060) always produces the "correct" tag when called with or without dispatching, or when inherited by a descendant. If it were statically tagged, the default would be useless for a dispatching call; if it were dynamically tagged, the default would be useless for a nondispatching call.

The second part is consistent with the first part, since designated objects are never tag-indeterminate. 

A given subprogram shall not be a dispatching operation of two or more distinct tagged types. 

Reason: This restriction minimizes confusion since multiple dispatching is not provided. The normal solution is to replace all but one of the tagged types with their class-wide types. 

The explicit declaration of a primitive subprogram of a tagged type shall occur before the type is frozen (see 13.14). [For example, new dispatching operations cannot be added after objects or values of the type exist, nor after deriving a record extension from it, nor after a body.]

Reason: This rule is needed because (1) we don't want people dispatching to things that haven't been declared yet, and (2) we want to allow tagged type descriptors to be static (allocated statically, and initialized to link-time-known symbols). Suppose T2 inherits primitive P from T1, and then overrides P. Suppose P is called before the declaration of the overriding P. What should it dispatch to? If the answer is the new P, we've violated the first principle above. If the answer is the old P, we've violated the second principle. (A call to the new one necessarily raises Program_Error, but that's beside the point.)

Note that a call upon a dispatching operation of type T will freeze T.

We considered applying this rule to all derived types, for uniformity. However, that would be upward incompatible, so we rejected the idea. As in Ada 83, for an untagged type, the above call upon P will call the old P (which is arguably confusing). 

Implementation Note: Because of this rule, the type descriptor can be created (presumably containing linker symbols pointing at the not-yet-compiled bodies) at the first freezing point of the type. It also prevents, for a tagged type declared in a [package_specification](S0162), overriding in the body or by a child subprogram. 

Ramification: A consequence is that for a derived_type_declaration in a [declarative_part](S0079), only the first primitive subprogram can be declared by a [subprogram_body](S0154). 


#### Dynamic Semantics

For the execution of a call on a dispatching operation of a type T, the controlling tag value determines which subprogram body is executed. The controlling tag value is defined as follows: 

If one or more controlling operands are statically tagged, then the controlling tag value is statically determined to be the tag of T.

If one or more controlling operands are dynamically tagged, then the controlling tag value is not statically determined, but is rather determined by the tags of the controlling operands. If there is more than one dynamically tagged controlling operand, a check is made that they all have the same tag. If this check fails, Constraint_Error is raised unless the call is a [function_call](S0156) whose [name](S0084) denotes the declaration of an equality operator (predefined or user defined) that returns Boolean, in which case the result of the call is defined to indicate inequality, and no [subprogram_body](S0154) is executed. This check is performed prior to evaluating any tag-indeterminate controlling operands. 

Reason: Tag mismatch is considered an error (except for "=" and "/=") since the corresponding primitive subprograms in each specific type expect all controlling operands to be of the same type. For tag mismatch with an equality operator, rather than raising an exception, "=" returns False and "/=" returns True. No equality operator is actually invoked, since there is no common tag value to control the dispatch. Equality is a special case to be consistent with the existing Ada 83 principle that equality comparisons, even between objects with different constraints, never raise Constraint_Error. 

If all of the controlling operands are tag-indeterminate, then: 

If the call has a controlling result and is itself a (possibly parenthesized or qualified) controlling operand of an enclosing call on a dispatching operation of type T, then its controlling tag value is determined by the controlling tag value of this enclosing call;

Otherwise, the controlling tag value is statically determined to be the tag of type T. 

Ramification: This includes the cases of a tag-indeterminate procedure call, and a tag-indeterminate [function_call](S0156) that is used to initialize a class-wide formal parameter or class-wide object. 

For the execution of a call on a dispatching operation, the body executed is the one for the corresponding primitive subprogram of the specific type identified by the controlling tag value. The body for an explicitly declared dispatching operation is the corresponding explicit body for the subprogram. The body for an implicitly declared dispatching operation that is overridden is the body for the overriding subprogram, [even if the overriding occurs in a private part.] The body for an inherited dispatching operation that is not overridden is the body of the corresponding subprogram of the parent or ancestor type.

To be honest: In the unusual case in which a dispatching subprogram is explicitly declared (overridden) by a body (with no preceding [subprogram_declaration](S0141)), the body for that dispatching subprogram is that body; that is, the "corresponding explicit body" in the above rule is the body itself. 

Reason: The wording of the above rule is intended to ensure that the same body is executed for a given tag, whether that tag is determined statically or dynamically. For a type declared in a package, it doesn't matter whether a given subprogram is overridden in the visible part or the private part, and it doesn't matter whether the call is inside or outside the package. For example: 

```ada
package P1 is
    type T1 is tagged null record;
    procedure Op_A(Arg : in T1);
    procedure Op_B(Arg : in T1);
end P1;

```

```ada
with P1; use P1;
package P2 is
    type T2 is new T1 with null record;
    procedure Op_A(Param : in T2);
private
    procedure Op_B(Param : in T2);
end P2;

```

```ada
with P1; with P2;
procedure Main is
    X : T2;
    Y : T1'Class := X;
begin
    P2.Op_A(Param =&gt X); -- Nondispatching call.
    P1.Op_A(Arg =&gt Y); -- Dispatching call.
    P2.Op_B(Arg =&gt X); -- Nondispatching call.
    P1.Op_B(Arg =&gt Y); -- Dispatching call.
end Main;

```

The two calls to Op_A both execute the body of Op_A that has to occur in the body of package P2. Similarly, the two calls to Op_B both execute the body of Op_B that has to occur in the body of package P2, even though Op_B is overridden in the private part of P2. Note, however, that the formal parameter names are different for P2.Op_A versus P2.Op_B. The overriding declaration for P2.Op_B is not visible in Main, so the name in the call actually denotes the implicit declaration of Op_B inherited from T1.

If a call occurs in the program text before an overriding, which can happen only if the call is part of a default expression, the overriding will still take effect for that call.

Implementation Note: Even when a tag is not statically determined, a compiler might still be able to figure it out and thereby avoid the overhead of run-time dispatching. 

NOTE 1   The body to be executed for a call on a dispatching operation is determined by the tag; it does not matter whether that tag is determined statically or dynamically, and it does not matter whether the subprogram's declaration is visible at the place of the call.

NOTE 2   This subclause covers calls on primitive subprograms of a tagged type. Rules for tagged type membership tests are described in 4.5.2. Controlling tag determination for an [assignment_statement](S0130) is described in 5.2.

NOTE 3   A dispatching call can dispatch to a body whose declaration is not visible at the place of the call.

NOTE 4   A call through an access-to-subprogram value is never a dispatching call, even if the access value designates a dispatching operation. Similarly a call whose [prefix](S0086) denotes a [subprogram_renaming_declaration](S0173) cannot be a dispatching call unless the renaming itself is the declaration of a primitive subprogram. 


#### Extensions to Ada 83

The concept of dispatching operations is new. 


### 3.9.3  Abstract Types and Subprograms

[ An abstract type is a tagged type intended for use as a parent type for type extensions, but which is not allowed to have objects of its own. An abstract subprogram is a subprogram that has no body, but is intended to be overridden at some point when inherited. Because objects of an abstract type cannot be created, a dispatching call to an abstract subprogram always dispatches to some overriding body.] Version=[5],Kind=(Added),Group=[T],Term=[abstract type], Def=[a tagged type intended for use as an ancestor of other types, but which is not allowed to have objects of its own] 


#### Language Design Principles

An abstract subprogram has no body, so the rules in this clause are designed to ensure (at compile time) that the body will never be invoked. We do so primarily by disallowing the creation of values of the abstract type. Therefore, since type conversion and parameter passing don't change the tag, we know we will never get a class-wide value with a tag identifying an abstract type. This means that we only have to disallow nondispatching calls on abstract subprograms (dispatching calls will never reach them). 


#### Legality Rules

An abstract type is a specific type that has the reserved word abstract in its declaration.Only a tagged type is allowed to be declared abstract. 

Ramification: Untagged types are never abstract, even though they can have primitive abstract subprograms. Such subprograms cannot be called, unless they also happen to be dispatching operations of some tagged type, and then only via a dispatching call.

Class-wide types are never abstract. If T is abstract, then it is illegal to declare a stand-alone object of type T, but it is OK to declare a stand-alone object of type T'Class; the latter will get a tag from its initial value, and this tag will necessarily be different from T'Tag. 

A subprogram declared by an [abstract_subprogram_declaration](S0142) (see 6.1) is an abstract subprogram. If it is a primitive subprogram of a tagged type, then the tagged type shall be abstract. 

Ramification: Note that for a private type, this applies to both views. The following is illegal: 

```ada
package P is
    type T is abstract tagged private;
    function Foo (X : T) return Boolean is abstract; -- Illegal!
private
    type T is tagged null record; -- Illegal!
    X : T;
    Y : Boolean := Foo (T'Class (X));
end P;

```

The full view of T is not abstract, but has an abstract operation Foo, which is illegal. The two lines marked "-- Illegal!" are illegal when taken together. 

Reason: We considered disallowing untagged types from having abstract primitive subprograms. However, we rejected that plan, because it introduced some silly anomalies, and because such subprograms are harmless (if not terribly useful). For example: 

```ada
package P is
   type Field_Size is range 0..100;
   type T is abstract tagged null record;
   procedure Print(X : in T; F : in Field_Size := 0) is abstract;
  . . .
package Q is
   type My_Field_Size is new Field_Size;
   -- implicit declaration of Print(X : T; F : My_Field_Size := 0) is abstract;
end Q;

```

It seemed silly to make the derivative of My_Field_Size illegal, just because there was an implicitly declared abstract subprogram that was not primitive on some tagged type. Other rules could be formulated to solve this problem, but the current ones seem like the simplest.

For a derived type, if the parent or ancestor type has an abstract primitive subprogram, or a primitive function with a controlling result, then: 

If the derived type is abstract or untagged, the inherited subprogram is abstract. 

Ramification: Note that it is possible to override a concrete subprogram with an abstract one. 

Otherwise, the subprogram shall be overridden with a nonabstract subprogram[; for a type declared in the visible part of a package, the overriding may be either in the visible or the private part]. However, if the type is a generic formal type, the subprogram need not be overridden for the formal type itself; [a nonabstract version will necessarily be provided by the actual type.] 

Reason: A function that returns the parent type becomes abstract for an abstract type extension (if not overridden) because conversion from a parent type to a type extension is not defined, and function return semantics is defined in terms of conversion. (Note that parameters of mode in out or out do not have this problem, because the tag of the actual is not changed.)

Note that the overriding required above can be in the private part, which allows the following: 

```ada
package Pack1 is
    type Ancestor is abstract ...;
    procedure Do_Something(X : in Ancestor) is abstract;
end Pack1;

```

```ada
with Pack1; use Pack1;
package Pack2 is
    type T1 is new Ancestor with record ...;
        -- A concrete type.
    procedure Do_Something(X : in T1); -- Have to override.
end Pack2;

```

```ada
with Pack1; use Pack1;
with Pack2; use Pack2;
package Pack3 is
    type T2 is new Ancestor with private;
        -- A concrete type.
private
    type T2 is new T1 with -- Parent different from ancestor.
      record ... end record;
    -- Here, we inherit Pack2.Do_Something.
end Pack3;
    

```

T2 inherits an abstract Do_Something, but T is not abstract, so Do_Something has to be overridden. However, it is OK to override it in the private part. In this case, we override it by inheriting a concrete version from a different type. Nondispatching calls to Pack3.Do_Something are allowed both inside and outside package Pack3.

A call on an abstract subprogram shall be a dispatching call; nondispatching calls to an abstract subprogram are not allowed. 

Ramification: If an abstract subprogram is not a dispatching operation of some tagged type, then it cannot be called at all. 

The type of an [aggregate](S0097), or of an object created by an [object_declaration](S0029) or an [allocator](S0122), or a generic formal object of mode in, shall not be abstract. The type of the target of an assignment operation (see 5.2) shall not be abstract. The type of a component shall not be abstract. If the result type of a function is abstract, then the function shall be abstract. 

Reason: This ensures that values of an abstract type cannot be created, which ensures that a dispatching call to an abstract subprogram will not try to execute the nonexistent body.

Generic formal objects of mode in are like constants; therefore they should be forbidden for abstract types. Generic formal objects of mode in out are like renamings; therefore, abstract types are OK for them, though probably not terribly useful.

If a partial view is not abstract, the corresponding full view shall not be abstract. If a generic formal type is abstract, then for each primitive subprogram of the formal that is not abstract, the corresponding primitive subprogram of the actual shall not be abstract. 

Discussion: By contrast, we allow the actual type to be nonabstract even if the formal type is declared abstract. Hence, the most general formal tagged type possible is "type T(&lt&gt) is abstract tagged limited private;".

For an abstract private extension declared in the visible part of a package, it is only possible for the full type to be nonabstract if the private extension has no abstract dispatching operations. 

For an abstract type declared in a visible part, an abstract primitive subprogram shall not be declared in the private part, unless it is overriding an abstract subprogram implicitly declared in the visible part. For a tagged type declared in a visible part, a primitive function with a controlling result shall not be declared in the private part, unless it is overriding a function implicitly declared in the visible part. 

Reason: The "visible part" could be that of a package or a generic package. This rule is needed because a nonabstract type extension declared outside the package would not know about any abstract primitive subprograms or primitive functions with controlling results declared in the private part, and wouldn't know that they need to be overridden with nonabstract subprograms. The rule applies to a tagged record type or record extension declared in a visible part, just as to a tagged private type or private extension. The rule applies to explicitly and implicitly declared abstract subprograms: 

```ada
package Pack is
    type T is abstract new T1 with private;
private
    type T is abstract new T2 with record ... end record;
    ...
end Pack;

```

The above example would be illegal if T1 has a nonabstract primitive procedure P, but T2 overrides P with an abstract one; the private part should override P with a nonabstract version. On the other hand, if the P were abstract for both T1 and T2, the example would be legal as is. 

A generic actual subprogram shall not be an abstract subprogram . The [prefix](S0086) of an [attribute_reference](S0093) for the Access, Unchecked_Access, or Address attributes shall not denote an abstract subprogram. 

Ramification: An [abstract_subprogram_declaration](S0142) is not syntactically a [subprogram_declaration](S0141). Nonetheless, an abstract subprogram is a subprogram, and an [abstract_subprogram_declaration](S0142) is a declaration of a subprogram.

The part about generic actual subprograms includes those given by default. 

NOTE 1   Abstractness is not inherited; to declare an abstract type, the reserved word abstract has to be used in the declaration of the type extension. 

Ramification: A derived type can be abstract even if its parent is not. Similarly, an inherited concrete subprogram can be overridden with an abstract subprogram. 

NOTE 2   A class-wide type is never abstract. Even if a class is rooted at an abstract type, the class-wide type for the class is not abstract, and an object of the class-wide type can be created; the tag of such an object will identify some nonabstract type in the class. 


#### Examples

Example of an abstract type representing a set of natural numbers: 

```ada
package Sets is
    subtype Element_Type is Natural;
    type Set is abstract tagged null record;
    function Empty return Set is abstract;
    function Union(Left, Right : Set) return Set is abstract;
    function Intersection(Left, Right : Set) return Set is abstract;
    function Unit_Set(Element : Element_Type) return Set is abstract;
    procedure Take(Element : out Element_Type;
                   From : in out Set) is abstract;
end Sets;

```

NOTE 3   Notes on the example: Given the above abstract type, one could then derive various (nonabstract) extensions of the type, representing alternative implementations of a set. One might use a bit vector, but impose an upper bound on the largest element representable, while another might use a hash table, trading off space for flexibility. 

Discussion: One way to export a type from a package with some components visible and some components private is as follows: 

```ada
package P is
    type Public_Part is abstract tagged
        record
            ...
        end record;
    type T is new Public_Part with private;
    ...
private
    type T is new Public_Part with
        record
            ...
        end record;
end P;

```

The fact that Public_Part is abstract tells clients they have to create objects of type T instead of Public_Part. Note that the public part has to come first; it would be illegal to declare a private type Private_Part, and then a record extension T of it, unless T were in the private part after the full declaration of Private_Part, but then clients of the package would not have visibility to T. 

Version=[5],Kind=(AddedNormal),Group=[T],Term=[interface type], Def=[an abstract tagged type that has no components or concrete operations except possibly null procedures], Note1=[Interface types are used for composing other interfaces and tagged types and thereby provide multiple inheritance. Only an interface type can be used as a progenitor of another type.] 


#### Syntax

 ::= 


#### Static Semantics

Version=[5],Kind=(AddedNormal),Group=[T],Term=[synchronized entity], Def=[an entity that can be safely operated on by multiple tasks concurrently], Note1=[A synchronized interface can be an ancestor of a task or a protected type. Such a task or protected type is called a synchronized tagged type.]

Version=[5],Kind=(AddedNormal),Group=[T],Term=[progenitor of a derived type], Def=[one of the types given in the definition of the derived type other than the first], Note1=[A progenitor is always an interface type. Interfaces, tasks, and protected types can also have progenitors.] 


## 3.10  Access Types

A value of an access type (an access value) provides indirect access to the object or subprogram it designates. Depending on its type, an access value can designate either subprograms, objects created by allocators (see 4.8), or more generally aliased objects of an appropriate type. 

Discussion: A [name](S0084) denotes an entity; an access value designates an entity. The "dereference" of an access value X, written "X.all", is a [name](S0084) that denotes the entity designated by X. 


#### Language Design Principles

Access values should always be well defined (barring uses of certain unchecked features of Section 13). In particular, uninitialized access variables should be prevented by compile-time rules. 


#### Syntax

access_type_definition ::= 
    [access_to_object_definition](S0074)
  | [access_to_subprogram_definition](S0076)

access_to_object_definition ::= 
    access [[general_access_modifier](S0075)] [subtype_indication](S0024)

general_access_modifier ::= all | constant

access_to_subprogram_definition ::= 
    access [protected] procedure [parameter_profile](S0149)
  | access [protected] function  [parameter_and_result_profile](S0150)

access_definition ::= access [subtype_mark](S0025)


#### Static Semantics

There are two kinds of access types, access-to-object types, whose values designate objects, and access-to-subprogram types, whose values designate subprograms. Associated with an access-to-object type is a storage pool; several access types may share the same storage pool. A storage pool is an area of storage used to hold dynamically allocated objects (called pool elements) created by allocators[; storage pools are described further in 13.11, "Storage Management"].

Access-to-object types are further subdivided into pool-specific access types, whose values can designate only the elements of their associated storage pool, and general access types, whose values can designate the elements of any storage pool, as well as aliased objects created by declarations rather than allocators, and aliased subcomponents of other objects. 

Implementation Note: The value of an access type will typically be a machine address. However, a value of a pool-specific access type can be represented as an offset (or index) relative to its storage pool, since it can point only to the elements of that pool. 

A view of an object is defined to be aliased if it is defined by an [object_declaration](S0029) or [component_definition](S0053) with the reserved word aliased, or by a renaming of an aliased view. In addition, the dereference of an access-to-object value denotes an aliased view, as does a view conversion (see 4.6) of an aliased view. Finally, the current instance of a limited type, and a formal parameter or generic formal object of a tagged type are defined to be aliased. [Aliased views are the ones that can be designated by an access value.] If the view defined by an [object_declaration](S0029) is aliased, and the type of the object has discriminants, then the object is constrained; if its nominal subtype is unconstrained, then the object is constrained by its initial value. [Similarly, if the object created by an [allocator](S0122) has discriminants, the object is constrained, either by the designated subtype, or by its initial value.] 

Glossary entry: An aliased view of an object is one that can be designated by an access value. Objects allocated by allocators are aliased. Objects can also be explicitly declared as aliased with the reserved word aliased. The Access attribute can be used to create an access value designating an aliased object.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[aliased view], Def=[a view of an object that can be designated by an access value], Note1=[Objects allocated by allocators are aliased. Objects can also be explicitly declared as aliased with the reserved word aliased. The Access attribute can be used to create an access value designating an aliased object.] 

Ramification: The current instance of a nonlimited type is not aliased.

The object created by an allocator is aliased, but not its subcomponents, except of course for those that themselves have aliased in their [component_definition](S0053).

The renaming of an aliased object is aliased.

Slices are never aliased. See 4.1.2 for more discussion. 

Reason: The current instance of a limited type is defined to be aliased so that an access discriminant of a component can be initialized with T'Access inside the definition of T.

A formal parameter of a tagged type is defined to be aliased so that a (tagged) parameter X may be passed to an access parameter P by using P =&gt X'Access. Access parameters are most important for tagged types because of dispatching-on-access-parameters (see 3.9.2). By restricting this to formal parameters, we minimize problems associated with allowing components that are not declared aliased to be pointed-to from within the same record.

A view conversion of an aliased view is aliased so that the type of an access parameter can be changed without first converting to a named access type. For example: 

```ada
type T1 is tagged ...;
procedure P(X : access T1);

```

```ada
type T2 is new T1 with ...;
procedure P(X : access T2) is
begin
    P(T1(X.all)'Access);  -- hand off to T1's P
    . . .     -- now do extra T2-specific processing
end P;

```

The rule about objects with discriminants is necessary because values of a constrained access subtype can designate an object whose nominal subtype is unconstrained; without this rule, a check on every use of such values would be required to ensure that the discriminants of the object had not changed. With this rule (among others), we ensure that if there might exist aliased views of a discriminated object, then the object is necessarily constrained. Note that this rule is necessary only for untagged types, since a discriminant of a tagged type can't have a default, so all tagged discriminated objects are always constrained anyway. 

We considered making more kinds of objects aliased by default. In particular, any object of a by-reference type will pretty much have to be allocated at an addressable location, so it can be passed by reference without using bit-field pointers. Therefore, one might wish to allow the Access and and Unchecked_Access attributes for such objects. However, private parts are transparent to the definition of "by-reference type", so if we made all objects of a by-reference type aliased, we would be violating the privacy of private parts. Instead, we would have to define a concept of "visibly by-reference" and base the rule on that. This seemed to complicate the rules more than it was worth, especially since there is no way to declare an untagged limited private type to be by-reference, since the full type might by nonlimited. 

Discussion: Note that we do not use the term "aliased" to refer to formal parameters that are referenced through multiple access paths (see 6.2). 

An [access_to_object_definition](S0074) defines an access-to-object type and its first subtype; the [subtype_indication](S0024) defines the designated subtype of the access type. If a [general_access_modifier](S0075) appears, then the access type is a general access type. If the modifier is the reserved word constant, then the type is an access-to-constant type[; a designated object cannot be updated through a value of such a type]. If the modifier is the reserved word all, then the type is an access-to-variable type[; a designated object can be both read and updated through a value of such a type]. If no [general_access_modifier](S0075) appears in the [access_to_object_definition](S0074), the access type is a pool-specific access-to-variable type. 

To be honest: The type of the designated subtype is called the designated type. 

Reason: The modifier all was picked to suggest that values of a general access type could point into "all" storage pools, as well as to objects declared aliased, and that "all" access (both read and update) to the designated object was provided. We couldn't think of any use for pool-specific access-to-constant types, so any access type defined with the modifier constant is considered a general access type, and can point into any storage pool or at other (appropriate) aliased objects. 

Implementation Note: The predefined generic Unchecked_Deallocation can be instantiated for any named access-to-variable type. There is no (language-defined) support for deallocating objects designated by a value of an access-to-constant type. Because of this, an allocator for an access-to-constant type can allocate out of a storage pool with no support for deallocation. Frequently, the allocation can be done at link-time, if the size and initial value are known then. 

Discussion: For the purpose of generic formal type matching, the relevant subclasses of access types are access-to-subprogram types, access-to-constant types, and (named) access-to-variable types, with its subclass (named) general access-to-variable types. Pool-specific access-to-variable types are not a separately matchable subclass of types, since they don't have any "extra" operations relative to all (named) access-to-variable types. 

An [access_to_subprogram_definition](S0076) defines an access-to-subprogram type and its first subtype; the [parameter_profile](S0149) or [parameter_and_result_profile](S0150) defines the designated profile of the access type. There is a calling convention associated with the designated profile[; only subprograms with this calling convention can be designated by values of the access type.] By default, the calling convention is "protected" if the reserved word protected appears, and "Ada" otherwise. [See Annex B for how to override this default.] 

Ramification: The calling convention protected is in italics to emphasize that it cannot be specified explicitly by the user. This is a consequence of it being a reserved word. 

Implementation Note: For an access-to-subprogram type, the representation of an access value might include implementation-defined information needed to support up-level references - for example, a static link. The accessibility rules (see 3.10.2) ensure that in a "global-display-based" implementation model (as opposed to a static-link-based model), an access-to-(unprotected)-subprogram value need consist only of the address of the subprogram. The global display is guaranteed to be properly set up any time the designated subprogram is called. Even in a static-link-based model, the only time a static link is definitely required is for an access-to-subprogram type declared in a scope nested at least two levels deep within subprogram or task bodies, since values of such a type might designate subprograms nested a smaller number of levels. For the normal case of an access-to-subprogram type declared at the outermost (library) level, a code address by itself should be sufficient to represent the access value in many implementations.

For access-to-protected-subprogram, the access values will necessarily include both an address (or other identification) of the code of the subprogram, as well as the address of the associated protected object. This could be thought of as a static link, but it will be needed even for global-display-based implementation models. It corresponds to the value of the "implicit parameter" that is passed into every call of a protected operation, to identify the current instance of the protected type on which they are to operate.

Any Elaboration_Check is performed when a call is made through an access value, rather than when the access value is first "created" via a 'Access. For implementation models that normally put that check at the call-site, an access value will have to point to a separate entry point that does the check. Alternatively, the access value could point to a "subprogram descriptor" that consisted of two words (or perhaps more), the first being the address of the code, the second being the elaboration bit. Or perhaps more efficiently, just the address of the code, but using the trick that the descriptor is initialized to point to a Raise-Program-Error routine initially, and then set to point to the "real" code when the body is elaborated.

For implementations that share code between generic instantiations, the extra level of indirection suggested above to support Elaboration_Checks could also be used to provide a pointer to the per-instance data area normally required when calling shared code. The trick would be to put a pointer to the per-instance data area into the subprogram descriptor, and then make sure that the address of the subprogram descriptor is loaded into a "known" register whenever an indirect call is performed. Once inside the shared code, the address of the per-instance data area can be retrieved out of the subprogram descriptor, by indexing off the "known" register.

Essentially the same implementation issues arise for calls on dispatching operations of tagged types, except that the static link is always known "statically".

Note that access parameters of an anonymous access-to-subprogram type are not permitted. If there were such parameters, full "downward" closureswould be required, meaning that in an implementation that uses a per-task (global) display, the display would have to be passed as a hidden parameter, and reconstructed at the point of call. This was felt to be an undue implementation burden, given that an equivalent (actually, more general) capability is available via formal subprogram parameters to a generic. 

An [access_definition](S0077) defines an anonymous general access-to-variable type; the [subtype_mark](S0025) denotes its designated subtype. [An [access_definition](S0077) is used in the specification of an access discriminant (see 3.7) or an access parameter (see 6.1).]

For each (named) access type, there is a literal null which has a null access value designating no entity at all. [The null value of a named access type is the default initial value of the type.] Other values of an access type are obtained by evaluating an [attribute_reference](S0093) for the Access or Unchecked_Access attribute of an aliased view of an object or nonintrinsic subprogram, or, in the case of a named access-to-object type, an [allocator](S0122)[, which returns an access value designating a newly created object (see 3.10.2)].

Ramification: A value of an anonymous access type (that is, the value of an access parameter or access discriminant) cannot be null. 

Reason: Access parameters allow dispatching on the tag of the object designated by the actual parameter (which gets converted to the anonymous access type as part of the call). In order for dispatching to work properly, there had better be such an object. Hence, the type conversion will raise Constraint_Error if the value of the actual parameter is null. 

[All subtypes of an access-to-subprogram type are constrained.] The first subtype of a type defined by an [access_type_definition](S0073) or an [access_to_object_definition](S0074) is unconstrained if the designated subtype is an unconstrained array or discriminated type; otherwise it is constrained. 

Proof: The Legality Rules on [range_constraint](S0033)s (see 3.5) do not permit the [subtype_mark](S0025) of the [subtype_indication](S0024) to denote an access-to-scalar type, only a scalar type. The Legality Rules on [index_constraint](S0054)s (see 3.6.1) and [discriminant_constraint](S0061)s (see 3.7.1) both permit access-to-composite types in a [subtype_indication](S0024) with such _[constraint](S0026)s. Note that an access-to-access-to-composite is never permitted in a [subtype_indication](S0024) with a [constraint](S0026). 

Reason: Only [composite_constraint](S0028)s are permitted for an access type, and only on access-to-composite types. A constraint on an access-to-scalar or access-to-access type might be violated due to assignments via other access paths that were not so constrained. By contrast, if the designated subtype is an array or discriminated type, the constraint could not be violated by unconstrained assignments, since array objects are always constrained, and aliased discriminated objects are also constrained (by fiat, see Static Semantics). 


#### Dynamic Semantics

A [composite_constraint](S0028) is compatible with an unconstrained access subtype if it is compatible with the designated subtype. An access value satisfies a [composite_constraint](S0028) of an access subtype if it equals the null value of its type or if it designates an object whose value satisfies the constraint.

The elaboration of an [access_type_definition](S0073) creates the access type and its first subtype. For an access-to-object type, this elaboration includes the elaboration of the [subtype_indication](S0024), which creates the designated subtype.

The elaboration of an [access_definition](S0077) creates an anonymous general access-to-variable type [(this happens as part of the initialization of an access parameter or access discriminant)]. 

NOTE 1   Access values are called "pointers" or "references" in some other languages.

NOTE 2   Each access-to-object type has an associated storage pool; several access types can share the same pool. An object can be created in the storage pool of an access type by an [allocator](S0122) (see 4.8) for the access type. A storage pool (roughly) corresponds to what some other languages call a "heap". See 13.11 for a discussion of pools.

NOTE 3   Only [index_constraint](S0054)s and [discriminant_constraint](S0061)s can be applied to access types (see 3.6.1 and 3.7.1). 


#### Examples

Examples of access-to-object types: 

```ada
type Peripheral_Ref is access Peripheral;  --  see 3.8.1
type Binop_Ptr is access all Binary_Operation'Class;
                                           -- general access-to-class-wide, see 3.9.1

```

Example of an access subtype: 

```ada
subtype Drum_Ref is Peripheral_Ref(Drum);  --  see 3.8.1

```

Example of an access-to-subprogram type: 

```ada
type Message_Procedure is access procedure (M : in String := "Error!");
procedure Default_Message_Procedure(M : in String);
Give_Message : Message_Procedure := Default_Message_Procedure'Access;
...
procedure Other_Procedure(M : in String);
...
Give_Message := Other_Procedure'Access;
...
Give_Message("File not found.");  -- call with parameter (.all is optional)
Give_Message.all;                 -- call with no parameters

```


#### Extensions to Ada 83

The syntax for [access_type_definition](S0073) is changed to support general access types (including access-to-constants) and access-to-subprograms. The syntax rules for [general_access_modifier](S0075) and [access_definition](S0077) are new. 


#### Wording Changes from Ada 83

We use the term "storage pool" to talk about the data area from which allocation takes place. The term "collection" is no longer used. ("Collection" and "storage pool" are not the same thing because multiple unrelated access types can share the same storage pool; see 13.11 for more discussion.) 


### 3.10.1  Incomplete Type Declarations

There are no particular limitations on the designated type of an access type. In particular, the type of a component of the designated type can be another access type, or even the same access type. This permits mutually dependent and recursive access types. An [incomplete_type_declaration](S0078) can be used to introduce a type to be used as a designated type, while deferring its full definition to a subsequent [full_type_declaration](S0021). 


#### Syntax

incomplete_type_declaration ::= type [defining_identifier](S0019) [[discriminant_part](S0056)];


#### Legality Rules

An [incomplete_type_declaration](S0078) requires a completion, which shall be a [full_type_declaration](S0021). [If the [incomplete_type_declaration](S0078) occurs immediately within either the visible part of a [package_specification](S0162) or a [declarative_part](S0079), then the [full_type_declaration](S0021) shall occur later and immediately within this visible part or [declarative_part](S0079). If the [incomplete_type_declaration](S0078) occurs immediately within the private part of a given [package_specification](S0162), then the [full_type_declaration](S0021) shall occur later and immediately within either the private part itself, or the [declarative_part](S0079) of the corresponding [package_body](S0163).] 

Proof: This is implied by the next AARM-only rule, plus the rules in 3.11.1, "Completions of Declarations" which require a completion to appear later and immediately within the same declarative region. 

To be honest: If the [incomplete_type_declaration](S0078) occurs immediately within the visible part of a [package_specification](S0162), then the [full_type_declaration](S0021) shall occur immediately within this visible part. 

To be honest: If the implementation supports it, an [incomplete_type_declaration](S0078) can be completed by a [pragma](S0016) Import. 

If an [incomplete_type_declaration](S0078) has a [known_discriminant_part](S0058), then a [full_type_declaration](S0021) that completes it shall have a fully conforming (explicit) [known_discriminant_part](S0058) (see 6.3.1). [If an [incomplete_type_declaration](S0078) has no [discriminant_part](S0056) (or an [unknown_discriminant_part](S0057)), then a corresponding [full_type_declaration](S0021) is nevertheless allowed to have discriminants, either explicitly, or inherited via derivation.]

The only allowed uses of a [name](S0084) that denotes an [incomplete_type_declaration](S0078) are as follows: 

Discussion: No need to say "prior to the end of the [full_type_declaration](S0021)" since the name would not denote the [incomplete_type_declaration](S0078) after the end of the [full_type_declaration](S0021). Also, with child library units, it would not be well defined whether they come before or after the [full_type_declaration](S0021) for deferred incomplete types. 

as the [subtype_mark](S0025) in the [subtype_indication](S0024) of an [access_to_object_definition](S0074); [the only form of [constraint](S0026) allowed in this [subtype_indication](S0024) is a [discriminant_constraint](S0061);] 

Implementation Note: We now allow [discriminant_constraint](S0061)s even if the full type is deferred to the package body. However, there is no particular implementation burden because we have dropped the concept of the dependent compatibility check. In other words, we have effectively repealed AI83-00007. 

as the [subtype_mark](S0025) defining the subtype of a parameter or result of an [access_to_subprogram_definition](S0076); 

Reason: This allows, for example, a record to have a component designating a subprogram that takes that same record type as a parameter. 

as the [subtype_mark](S0025) in an [access_definition](S0077); 

as the [prefix](S0086) of an [attribute_reference](S0093) whose [attribute_designator](S0094) is Class; such an [attribute_reference](S0093) is similarly restricted to the uses allowed here; when used in this way, the corresponding [full_type_declaration](S0021) shall declare a tagged type, and the [attribute_reference](S0093) shall occur in the same library unit as the [incomplete_type_declaration](S0078). 

Reason: This is to prevent children from imposing requirements on their ancestor library units for deferred incomplete types. 

A dereference (whether implicit or explicit - see 4.1) shall not be of an incomplete type. 


#### Static Semantics

An [incomplete_type_declaration](S0078) declares an incomplete type and its first subtype; the first subtype is unconstrained if a [known_discriminant_part](S0058) appears. 

Reason: If an [unknown_discriminant_part](S0057) or no [discriminant_part](S0056) appears, then the constrainedness of the first subtype doesn't matter for any other rules or semantics, so we don't bother defining it. The case with a [known_discriminant_part](S0058) is the only case in which a constraint could later be given in a [subtype_indication](S0024) naming the incomplete type. 


#### Dynamic Semantics

The elaboration of an [incomplete_type_declaration](S0078) has no effect. 

Reason: An incomplete type has no real existence, so it doesn't need to be "created" in the usual sense we do for other types. It is roughly equivalent to a "forward;" declaration in Pascal. Private types are different, because they have a different set of characteristics from their full type. 

NOTE 1   Within a [declarative_part](S0079), an [incomplete_type_declaration](S0078) and a corresponding [full_type_declaration](S0021) cannot be separated by an intervening body. This is because a type has to be completely defined before it is frozen, and a body freezes all types declared prior to it in the same [declarative_part](S0079) (see 13.14).


#### Examples

Example of a recursive type: 

```ada
type Cell;  --  incomplete type declaration
type Link is access Cell;

```

```ada
type Cell is
   record
      Value  : Integer;
      Succ   : Link;
      Pred   : Link;
   end record;

```

```ada
Head   : Link  := new Cell'(0, null, null);
Next   : Link  := Head.Succ;

```

Examples of mutually dependent access types: 

```ada
type Person(&lt&gt);    -- incomplete type declaration
type Car;           -- incomplete type declaration

```

```ada
type Person_Name is access Person;
type Car_Name    is access all Car;

```

```ada
type Car is
   record
      Number  : Integer;
      Owner   : Person_Name;
   end record;

```

```ada
type Person(Sex : Gender) is
   record
      Name     : String(1 .. 20);
      Birth    : Date;
      Age      : Integer range 0 .. 130;
      Vehicle  : Car_Name;
      case Sex is
         when M =&gt Wife           : Person_Name(Sex =&gt F);
         when F =&gt Husband        : Person_Name(Sex =&gt M);
      end case;
   end record;

```

```ada
My_Car, Your_Car, Next_Car : Car_Name := new Car;  -- see 4.8
George : Person_Name := new Person(M);
   ...
George.Vehicle := Your_Car;

```


#### Extensions to Ada 83

The [full_type_declaration](S0021) that completes an [incomplete_type_declaration](S0078) may have a [known_discriminant_part](S0058) even if the [incomplete_type_declaration](S0078) does not.

A [discriminant_constraint](S0061) may be applied to an incomplete type, even if it its completion is deferred to the package body, because there is no "dependent compatibility check" required any more. Of course, the constraint can be specified only if a [known_discriminant_part](S0058) was given in the [incomplete_type_declaration](S0078). As mentioned in the previous paragraph, that is no longer required even when the full type has discriminants. 


#### Wording Changes from Ada 83

Dereferences producing incomplete types were not explicitly disallowed in RM83, though AI83-00039 indicated that it was not strictly necessary since troublesome cases would result in Constraint_Error at run time, since the access value would necessarily be null. However, this introduces an undesirable implementation burden, as illustrated by Example 4 of AI83-00039: 

```ada
package Pack is
    type Pri is private;
private
    type Sep;
    type Pri is access Sep;
    X : Pri;
end Pack;

```

```ada
package body Pack is -- Could be separately compiled!
    type Sep is ...;
    X := new Sep;
end Pack;

```

```ada
pragma Elaborate(Pack);
private package Pack.Child is
    I : Integer := X.all'Size; -- Legal, by AI-00039.
end Pack.Child;

```

Generating code for the above example could be a serious implementation burden, since it would require all aliased objects to store size dope, and for that dope to be in the same format for all kinds of types (or some other equivalently inefficient implementation). On the contrary, most implementations allocate dope differently (or not at all) for different designated subtypes. 


### 3.10.2  Operations of Access Types

[The attribute Access is used to create access values designating aliased objects and nonintrinsic subprograms. The "accessibility" rules prevent dangling references (in the absence of uses of certain unchecked features - see Section 13).] 


#### Language Design Principles

It should be possible for an access value to designate an object declared by an object declaration, or a subcomponent thereof. In implementation terms, this means pointing at stack-allocated and statically allocated data structures. However, dangling references should be prevented, primarily via compile-time rules, so long as features like Unchecked_Access and Unchecked_Deallocation are not used.

In order to create such access values, we require that the access type be a general access type, that the designated object be aliased, and that the accessibility rules be obeyed. 


#### Name Resolution Rules

For an [attribute_reference](S0093) with [attribute_designator](S0094) Access (or Unchecked_Access - see 13.10), the expected type shall be a single access type[; the [prefix](S0086) of such an [attribute_reference](S0093) is never interpreted as an [implicit_dereference](S0088)]. If the expected type is an access-to-subprogram type, then the expected profile of the [prefix](S0086) is the designated profile of the access type. 

Discussion: Saying that the expected type shall be a "single access type" is our "new" way of saying that the type has to be determinable from context using only the fact that it is an access type. See 4.2 and 8.6. Specifying the expected profile only implies type conformance. The more stringent subtype conformance is required by a Legality Rule. This is the only Resolution Rule that applies to the [name](S0084) in a [prefix](S0086) of an [attribute_reference](S0093). In all other cases, the [name](S0084) has to be resolved without using context. See 4.1.4.


#### Static Semantics

[The accessibility rules, which prevent dangling references, are written in terms of accessibility levels, which reflect the run-time nesting of masters. As explained in 7.6.1, a master is the execution of a ,[task_body](S0179), a [block_statement](S0138), a [subprogram_body](S0154), an [entry_body](S0190), or an [accept_statement](S0188). An accessibility level is deeper than another if it is more deeply nested at run time. For example, an object declared local to a called subprogram has a deeper accessibility level than an object declared local to the calling subprogram. The accessibility rules for access types require that the accessibility level of an object designated by an access value be no deeper than that of the access type. This ensures that the object will live at least as long as the access type, which in turn ensures that the access value cannot later designate an object that no longer exists. The Unchecked_Access attribute may be used to circumvent the accessibility rules.]

Version=[5],Kind=(Added),Group=[T],Term=[accessibility level], Def=[a representation of the lifetime of an entity in terms of the level of dynamic nesting within which the entity is known to exist] [A given accessibility level is said to be statically deeper than another if the given level is known at compile time (as defined below) to be deeper than the other for all possible executions. In most cases, accessibility is enforced at compile time by Legality Rules. Run-time accessibility checks are also used, since the Legality Rules do not cover certain cases involving access parameters and generic packages.]

Each master, and each entity and view created by it, has an accessibility level: 

The accessibility level of a given master is deeper than that of each dynamically enclosing master, and deeper than that of each master upon which the task executing the given master directly depends (see 9.3).

An entity or view created by a declaration has the same accessibility level as the innermost enclosing master except in the cases of renaming and derived access types described below. A parameter of a master has the same accessibility level as the master. 

The accessibility level of a view of an object or subprogram defined by a [renaming_declaration](S0169) is the same as that of the renamed view.

The accessibility level of a view conversion is the same as that of the operand.

For a function whose result type is a return-by-reference type, the accessibility level of the result object is the same as that of the master that elaborated the function body. For any other function, the accessibility level of the result object is that of the execution of the called function.

The accessibility level of a derived access type is the same as that of its ultimate ancestor.

The accessibility level of the anonymous access type of an access discriminant is the same as that of the containing object or associated constrained subtype.

The accessibility level of the anonymous access type of an access parameter is the same as that of the view designated by the actual. If the actual is an [allocator](S0122), this is the accessibility level of the execution of the called subprogram. 

The accessibility level of an object created by an [allocator](S0122) is the same as that of the access type.

The accessibility level of a view of an object or subprogram denoted by a dereference of an access value is the same as that of the access type. 

The accessibility level of a component, protected subprogram, or entry of (a view of) a composite object is the same as that of (the view of) the composite object. 

One accessibility level is defined to be statically deeper than another in the following cases: 

For a master that is statically nested within another master, the accessibility level of the inner master is statically deeper than that of the outer master. 

To be honest: Strictly speaking, this should talk about the constructs (such as subprogram_bodies) being statically nested within one another; the masters are really the executions of those constructs. 

To be honest: If a given accessibility level is statically deeper than another, then each level defined to be the same as the given level is statically deeper than each level defined to be the same as the other level. 

The statically deeper relationship does not apply to the accessibility level of the anonymous type of an access parameter; that is, such an accessibility level is not considered to be statically deeper, nor statically shallower, than any other.

[For determining whether one level is statically deeper than another when within a generic package body, the generic package is presumed to be instantiated at the same level as where it was declared; runtime checks are needed in the case of more deeply nested instantiations.] 

For determining whether one level is statically deeper than another when within the declarative region of a [type_declaration](S0020), the current instance of the type is presumed to be an object created at a deeper level than that of the type. 

Ramification: In other words, the rules are checked at compile time of the [type_declaration](S0020), in an assume-the-worst manner. 

The accessibility level of all library units is called the library level; a library-level declaration or entity is one whose accessibility level is the library level. 

Ramification: [Library_unit_declaration](S0217)s are library level. Nested declarations are library level if they are nested only within packages (possibly more than one), and not within subprograms, tasks, etc. 

To be honest: The definition of the accessibility level of the anonymous type of an access parameter cheats a bit, since it refers to the view designated by the actual, but access values designate objects, not views of objects. What we really mean is the view that "would be" denoted by an expression "X.all", where X is the actual, even though such an expression is a figment of our imagination. The definition is intended to be equivalent to the following more verbose version: The accessibility level of the anonymous type of an access parameter is as follows: 

if the actual is an expression of a named access type - the accessibility level of that type;

if the actual is an [allocator](S0122) - the accessibility level of the execution of the called subprogram;

if the actual is a reference to the Access attribute - the accessibility level of the view denoted by the prefix;

if the actual is a reference to the Unchecked_Access attribute - library accessibility level;

if the actual is an access parameter - the accessibility level of its type. 

Note that the [allocator](S0122) case is explicitly mentioned in the RM95, because otherwise the definition would be circular: the level of the anonymous type is that of the view designated by the actual, which is that of the access type. 

Discussion: A deeper accessibility level implies a shorter maximum lifetime. Hence, when a rule requires X to have a level that is "not deeper than" Y's level, this requires that X has a lifetime at least as long as Y. (We say "maximum lifetime" here, because the accessibility level really represents an upper bound on the lifetime; an object created by an [allocator](S0122) can have its lifetime prematurely ended by an instance of Unchecked_Deallocation.)

Package elaborations are not masters, and are therefore invisible to the accessibility rules: an object declared immediately within a package has the same accessibility level as an object declared immediately within the declarative region containing the package. This is true even in the body of a package; it jibes with the fact that objects declared in a [package_body](S0163) live as long as objects declared outside the package, even though the body objects are not visible outside the package.

Note that the level of the view denoted by X.all can be different from the level of the object denoted by X.all. The former is determined by the type of X; the latter is determined either by the type of the [allocator](S0122), or by the master in which the object was declared. The former is used in several Legality Rules and runtime checks; the latter is used to define when X.all gets finalized. The level of a view reflects what we can conservatively "know" about the object of that view; for example, due to [type_conversion](S0120)s, an access value might designate an object that was allocated by an [allocator](S0122) for a different access type.

Similarly, the level of the view denoted by X.all.Comp can be different from the level of the object denoted by X.all.Comp.

If Y is statically deeper than X, this implies that Y will be (dynamically) deeper than X in all possible executions.

Most accessibility checking is done at compile time; the rules are stated in terms of "statically deeper than". The exceptions are: 

Checks involving access parameters. The fact that "statically deeper than" is not defined for the anonymous access type of an access parameter implies that any rule saying "shall not be statically deeper than" does not apply to such a type, nor to anything defined to have "the same" level as such a type.

Checks involving entities and views within generic packages. This is because an instantiation can be at a level that is more deeply nested than the generic package itself. In implementations that use a macro-expansion model of generics, these violations can be detected at macro-expansion time. For implementations that share generics, run-time code is needed to detect the error.

Checks during function return. 

Note that runtime checks are not required for access discriminants, because their accessibility is determined statically by the accessibility level of the enclosing object.

This The accessibility level of the result object of a function reflects the time when that object will be finalized; we don't allow pointers to the object to survive beyond that time.

We sometimes use the terms "accessible" and "inaccessible" to mean that something has an accessibility level that is not deeper, or deeper, respectively, than something else. 

Implementation Note: If an accessibility Legality Rule is satisfied, then the corresponding runtime check (if any) cannot fail (and a reasonable implementation will not generate any checking code) unless access parameters or shared generic bodies are involved.

Accessibility levels are defined in terms of the relations "the same as" and "deeper than". To make the discussion more concrete, we can assign actual numbers to each level. Here, we assume that library-level accessibility is level 0, and each level defined as "deeper than" is one level deeper. Thus, a subprogram directly called from the environment task (such as the main subprogram) would be at level 1, and so on.

Accessibility is not enforced at compile time for access parameters. The "obvious" implementation of the runtime checks would be inefficient, and would involve distributed overhead; therefore, an efficient method is given below. The "obvious" implementation would be to pass the level of the caller at each subprogram call, task creation, etc. This level would be incremented by 1 for each dynamically nested master. An Accessibility_Check would be implemented as a simple comparison - checking that X is not deeper than Y would involve checking that X &lt= Y.

A more efficient method is based on passing static nesting levels (within constructs that correspond at run time to masters - packages don't count). Whenever an access parameter is passed, an implicit extra parameter is passed with it. The extra parameter represents (in an indirect way) the accessibility level of the anonymous access type, and, therefore, the level of the view denoted by a dereference of the access parameter. This is analogous to the implicit "Constrained" bit associated with certain formal parameters of an unconstrained but definite composite subtype. In this method, we avoid distributed overhead: it is not necessary to pass any extra information to subprograms that have no access parameters. For anything other than an access parameter and its anonymous type, the static nesting level is known at compile time, and is defined analogously to the RM95 definition of accessibility level (e.g. derived access types get their nesting level from their parent). Checking "not deeper than" is a "&lt=" test on the levels.

For each access parameter, the static depth passed depends on the actual, as follows: 

If the actual is an expression of a named access type, pass the static nesting level of that type.

If the actual is an [allocator](S0122), pass the static nesting level of the caller, plus one.

If the actual is a reference to the Access attribute, pass the level of the view denoted by the prefix.

If the actual is a reference to the Unchecked_Access attribute, pass 0 (the library accessibility level).

If the actual is an access parameter, usually just pass along the level passed in. However, if the static nesting level of the formal (access) parameter is greater than the static nesting level of the actual (access) parameter, the level to be passed is the minimum of the static nesting level of the access parameter and the actual level passed in. 

For the Accessibility_Check associated with a [type_conversion](S0120) of an access parameter of a given subprogram to a named access type, if the target type is statically nested within the subprogram, do nothing; the check can't fail in this case. Otherwise, check that the value passed in is &lt= the static nesting depth of the target type. The other Accessibility_Checks are handled in a similar manner.

This method, using statically known values most of the time, is efficient, and, more importantly, avoids distributed overhead.

Discussion: Examples of accessibility: 

```ada
package body Lib_Unit is
    type T is tagged ...;
    type A0 is access all T;
    Global: A0 := ...;
    procedure P(X: T) is
        Y: aliased T;
        type A1 is access all T;
        Ptr0: A0 := Global; -- OK.
        Ptr1: A1 := X'Access; -- OK.
    begin
        Ptr1 := Y'Access; -- OK;
        Ptr0 := A0(Ptr1); -- Illegal type conversion!
        Ptr0 := X'Access; -- Illegal reference to Access attribute!
        Ptr0 := Y'Access; -- Illegal reference to Access attribute!
        Global := Ptr0; -- OK.
    end P;
end Lib_Unit;

```

The above illegal statements are illegal because the accessibility level of X and Y are statically deeper than the accessibility level of A0. In every possible execution of any program including this library unit, if P is called, the accessibility level of X will be (dynamically) deeper than that of A0. Note that the accessibility levels of X and Y are the same.

Here's an example involving access parameters: 

```ada
procedure Main is
    type Level_1_Type is access all Integer;

```

```ada
    procedure P(X: access Integer) is
        type Nested_Type is access all Integer;
    begin
        ... Nested_Type(X) ... -- (1)
        ... Level_1_Type(X) ... -- (2)
    end P;

```

```ada
    procedure Q(X: access Integer) is
        procedure Nested(X: access Integer) is
        begin
            P(X);
        end Nested;
    begin
        Nested(X);
    end Q;

```

```ada
    procedure R is
        Level_2: aliased Integer;
    begin
        Q(Level_2'Access); -- (3)
    end R;

```

```ada
    Level_1: aliased Integer;
begin
    Q(Level_1'Access); -- (4)
    R;
end Main;

```

The run-time Accessibility_Check at (1) can never fail, and no code should be generated to check it. The check at (2) will fail when called from (3), but not when called from (4).

Within a [type_declaration](S0020), the rules are checked in an assume-the-worst manner. For example: 

```ada
package P is
    type Int_Ptr is access all Integer;
    type Rec(D: access Integer) is limited private;
private
    type Rec_Ptr is access all Rec;
    function F(X: Rec_Ptr) return Boolean;
    function G(X: access Rec) return Boolean;
    type Rec(D: access Integer) is
        record
            C1: Int_Ptr := Int_Ptr(D); -- Illegal!
            C2: Rec_Ptr := Rec'Access; -- Illegal!
            C3: Boolean := F(Rec'Access); -- Illegal!
            C4: Boolean := G(Rec'Access);
        end record;
end P;

```

C1, C2, and C3 are all illegal, because one might declare an object of type Rec at a more deeply nested place than the declaration of the type. C4 is legal, but the accessibility level of the object will be passed to function G, and constraint checks within G will prevent it from doing any evil deeds.

Note that we cannot defer the checks on C1, C2, and C3 until compile-time of the object creation, because that would cause violation of the privacy of private parts. Furthermore, the problems might occur within a task or protected body, which the compiler can't see while compiling an object creation. 

The following attribute is defined for a [prefix](S0086) X that denotes an aliased view of an object: 

X shall denote an aliased view of an object[, including possibly the current instance (see 8.6) of a limited type within its definition, or a formal parameter or generic formal object of a tagged type]. The view denoted by the [prefix](S0086) X shall satisfy the following additional requirements, presuming the expected type for X'Access is the general access type A: 

If A is an access-to-variable type, then the view shall be a variable; [on the other hand, if A is an access-to-constant type, the view may be either a constant or a variable.] 

Discussion: The current instance of a limited type is considered a variable. 

The view shall not be a subcomponent that depends on discriminants of a variable whose nominal subtype is unconstrained, unless this subtype is indefinite, or the variable is aliased.

Discussion: This restriction is intended to be similar to the restriction on renaming discriminant-dependent subcomponents. 

Reason: This prevents references to subcomponents that might disappear or move or change constraints after creating the reference. 

Implementation Note: There was some thought to making this restriction more stringent, roughly: "X shall not denote a subcomponent of a variable with discriminant-dependent subcomponents, if the nominal subtype of the variable is an unconstrained definite subtype." This was because in some implementations, it is not just the discriminant-dependent subcomponents that might move as the result of an assignment that changed the discriminants of the enclosing object. However, it was decided not to make this change because a reasonable implementation strategy was identified to avoid such problems, as follows: 

Place nondiscriminant-dependent components with any aliased parts at offsets preceding any discriminant-dependent components in a discriminated record type with defaulted discriminants.

Preallocate the maximum space for unconstrained discriminated variables with aliased subcomponents, rather than allocating the initial size and moving them to a larger (heap-resident) place if they grow as the result of an assignment. 

Note that for objects of a by-reference type, it is not an error for a programmer to take advantage of the fact that such objects are passed by reference. Therefore, the above approach is also necessary for discriminated record types with components of a by-reference type.

To make the above strategy work, it is important that a component of a derived type is defined to be discriminant-dependent if it is inherited and the parent subtype constraint is defined in terms of a discriminant of the derived type (see 3.7). 

If the designated type of A is tagged, then the type of the view shall be covered by the designated type; if A's designated type is not tagged, then the type of the view shall be the same, and either A's designated subtype shall statically match the nominal subtype of the view, or the designated subtype shall be discriminated and unconstrained; 

Implementation Note: This ensures that the dope for an aliased array object can always be stored contiguous with it, but need not be if its nominal subtype is constrained. 

The accessibility level of the view shall not be statically deeper than that of the access type A. In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit. 

Ramification: In an instance body, a runtime check applies.

If A is an anonymousaccess type, then the view can never have a deeper accessibility level than A, except when X'Access is used to initialize an access discriminant of an object created by an [allocator](S0122). The latter case is illegal if the accessibility level of X is statically deeper than that of the access type of the [allocator](S0122); a runtime check is needed in the case where the initial value comes from an access parameter. 

A check is made that the accessibility level of X is not deeper than that of the access type A. If this check fails, Program_Error is raised. 

Ramification: The check is needed for access parameters  and in instance bodies.

Implementation Note: This check requires that some indication of lifetime is passed as an implicit parameter along with access parameters.No such requirement applies to access discriminants, since the checks associated with them are all compile-time checks. 

If the nominal subtype of X does not statically match the designated subtype of A, a view conversion of X to the designated subtype is evaluated (which might raise Constraint_Error - see 4.6) and the value of X'Access designates that view. 

The following attribute is defined for a [prefix](S0086) P that denotes a subprogram: 

The accessibility level of P shall not be statically deeper than that of S. In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit. The profile of P shall be subtype-conformant with the designated profile of S, and shall not be Intrinsic. If the subprogram denoted by P is declared within a generic body, S shall be declared within the generic body.

Discussion: The part about generic bodies is worded in terms of the denoted subprogram, not the denoted view; this implies that renaming is invisible to this part of the rule. This rule is partly to prevent contract model problems with respect to the accessibility rules, and partly to ease shared-generic-body implementations, in which a subprogram declared in an instance needs to have a different calling convention from other subprograms with the same profile.

Overload resolution ensures only that the profile is type-conformant. This rule specifies that subtype conformance is required (which also requires matching calling conventions). P cannot denote an entry because access-to-subprogram types never have the entry calling convention. P cannot denote an enumeration literal or an attribute function because these have intrinsic calling conventions. 

NOTE 1   The Unchecked_Access attribute yields the same result as the Access attribute for objects, but has fewer restrictions (see 13.10). There are other predefined operations that yield access values: an [allocator](S0122) can be used to create an object, and return an access value that designates it (see 4.8); evaluating the literal null yields a null access value that designates no entity at all (see 4.2).

NOTE 2   The predefined operations of an access type also include the assignment operation, qualification, and membership tests. Explicit conversion is allowed between general access types with matching designated subtypes; explicit conversion is allowed between access-to-subprogram types with subtype conformant profiles (see 4.6). Named access types have predefined equality operators; anonymous access types do not(see 4.5.2). 

Reason: By not having equality operators for anonymous access types, we eliminate the need to specify exactly where the predefined operators for anonymous access types would be defined, as well as the need for an implementer to insert an implicit declaration for "=", etc. at the appropriate place in their symbol table. Note that 'Access and ".all" are defined, and ":=" is defined though useless since all instances are constant. The literal null is also defined for the purposes of overload resolution, but is disallowed by a Legality Rules of this subclause. 

NOTE 3   The object or subprogram designated by an access value can be named with a dereference, either an [explicit_dereference](S0087) or an [implicit_dereference](S0088). See 4.1.

NOTE 4   A call through the dereference of an access-to-subprogram value is never a dispatching call. 

Proof: See 3.9.2. 

NOTE 5   The accessibility rules imply that it is not possible to use the Access attribute to implement "downward closures" - that is, to pass a more-nested subprogram as a parameter to a less-nested subprogram, as might be desired for example for an iterator abstraction. Instead, downward closures can be implemented using generic formal subprograms (see 12.6). Note that Unchecked_Access is not allowed for subprograms.

NOTE 6   Note that using an access-to-class-wide tagged type with a dispatching operation is a potentially more structured alternative to using an access-to-subprogram type.

NOTE 7   An implementation may consider two access-to-subprogram values to be unequal, even though they designate the same subprogram. This might be because one points directly to the subprogram, while the other points to a special prologue that performs an Elaboration_Check and then jumps to the subprogram. See 4.5.2. 

Ramification: If equality of access-to-subprogram values is important to the logic of a program, a reference to the Access attribute of a subprogram should be evaluated only once and stored in a global constant for subsequent use and equality comparison. 


#### Examples

Example of use of the Access attribute: 

```ada
Martha : Person_Name := new Person(F);       -- see 3.10.1
Cars   : array (1..2) of aliased Car;
   ...
Martha.Vehicle := Cars(1)'Access;
George.Vehicle := Cars(2)'Access;

```


#### Extensions to Ada 83

We no longer make things like 'Last and ".component" (basic) operations of an access type that need to be "declared" somewhere. Instead, implicit dereference in a [prefix](S0086) takes care of them all. This means that there should never be a case when X.all'Last is legal while X'Last is not. See AI83-00154. 


## 3.11  Declarative Parts

[A [declarative_part](S0079) contains [declarative_item](S0080)s (possibly none).] 


#### Syntax

declarative_part ::= {[declarative_item](S0080)}

declarative_item ::= 
    [basic_declarative_item](S0081) | [body](S0082)

basic_declarative_item ::= 
    [basic_declaration](S0018) | [representation_clause](S0263) | [use_clause](S0166)

body ::= [proper_body](S0083) | [body_stub](S0224)

proper_body ::= 
    [subprogram_body](S0154) | [package_body](S0163) | [task_body](S0179) | [protected_body](S0185)


#### Dynamic Semantics

The elaboration of a [declarative_part](S0079) consists of the elaboration of the [declarative_item](S0080)s, if any, in the order in which they are given in the [declarative_part](S0079).

An elaborable construct is in the elaborated state after the normal completion of its elaboration. Prior to that, it is not yet elaborated. 

Ramification: The elaborated state is only important for bodies; certain uses of a body raise an exception if the body is not yet elaborated.

Note that "prior" implies before the start of elaboration, as well as during elaboration.

The use of the term "normal completion" implies that if the elaboration propagates an exception or is aborted, the declaration is not elaborated. RM83 missed the aborted case. 

For a construct that attempts to use a body, a check (Elaboration_Check) is performed, as follows: 

For a call to a (non-protected) subprogram that has an explicit body, a check is made that the [subprogram_body](S0154) is already elaborated. This check and the evaluations of any actual parameters of the call are done in an arbitrary order. 

Discussion: AI83-00180 specifies that there is no elaboration check for a subprogram defined by a [pragma](S0016) Interface (or equivalently, [pragma](S0016) Import). AI83-00430 specifies that there is no elaboration check for an enumeration literal. AI83-00406 specifies that the evaluation of parameters and the elaboration check occur in an arbitrary order. AI83-00406 applies to generic instantiation as well (see below).

For a call to a protected operation of a protected type (that has a body - no check is performed if a [pragma](S0016) Import applies to the protected type), a check is made that the [protected_body](S0185) is already elaborated. This check and the evaluations of any actual parameters of the call are done in an arbitrary order. 

Discussion: A protected type has only one elaboration "bit", rather than one for each operation, because one call may result in evaluating the barriers of other entries, and because there are no elaborable declarations between the bodies of the operations. In fact, the elaboration of a [protected_body](S0185) does not elaborate the enclosed bodies, since they are not considered independently elaborable.

Note that there is no elaboration check when calling a task entry. Task entry calls are permitted even before the associated [task_body](S0179) has been seen. Such calls are simply queued until the task is activated and reaches a corresponding [accept_statement](S0188). We considered a similar rule for protected entries - simply queuing all calls until the [protected_body](S0185) was seen, but felt it was not worth the possible implementation overhead, particularly given that there might be multiple instances of the protected type. 

For the activation of a task, a check is made by the activator that the [task_body](S0179) is already elaborated. If two or more tasks are being activated together (see 9.2), as the result of the elaboration of a [declarative_part](S0079) or the initialization for the object created by an allocator, this check is done for all of them before activating any of them. 

Reason: As specified by AI83-00149, the check is done by the activator, rather than by the task itself. If it were done by the task itself, it would be turned into a Tasking_Error in the activator, and the other tasks would still be activated. 

For the instantiation of a generic unit that has a body, a check is made that this body is already elaborated. This check and the evaluation of any [explicit_generic_actual_parameter](S0244)s of the instantiation are done in an arbitrary order. 

The exception Program_Error is raised if any of these checks fails. 


#### Extensions to Ada 83

The syntax for [declarative_part](S0079) is modified to remove the ordering restrictions of Ada 83; that is, the distinction between [basic_declarative_item](S0081)s and later_declarative_items within [declarative_part](S0079)s is removed. This means that things like [use_clause](S0166)s and variable_declarations can be freely intermixed with things like bodies.

The syntax rule for [proper_body](S0083) now allows a [protected_body](S0185), and the rules for elaboration checks now cover calls on protected operations. 


#### Wording Changes from Ada 83

The syntax rule for later_declarative_item is removed; the syntax rule for [declarative_item](S0080) is new.

RM83 defines "elaborated" and "not yet elaborated" for [declarative_item](S0080)s here, and for other things in 3.1, "Declarations". That's no longer necessary, since these terms are fully defined in 3.1.

In RM83, all uses of [declarative_part](S0079) are optional (except for the one in [block_statement](S0138) with a declare) which is sort of strange, since a [declarative_part](S0079) can be empty, according to the syntax. That is, [declarative_part](S0079)s are sort of "doubly optional". In Ada 95, these [declarative_part](S0079)s are always required (but can still be empty). To simplify description, we go further and say (see 5.6, "Block Statements") that a [block_statement](S0138) without an explicit [declarative_part](S0079) is equivalent to one with an empty one. 


### 3.11.1  Completions of Declarations

Declarations sometimes come in two parts. A declaration that requires a second part is said to require completion. The second part is called the completion of the declaration (and of the entity declared), and is either another declaration, a body, or a [pragma](S0016). 

Discussion: Throughout the RM95, there are rules about completions that define the following: 

Which declarations require a corresponding completion.

Which constructs can only serve as the completion of a declaration.

Where the completion of a declaration is allowed to be.

What kinds of completions are allowed to correspond to each kind of declaration that allows one. 

Don't confuse this compile-time concept with the run-time concept of completion defined in 7.6.1.

Note that the declaration of a private type (if limited) can be completed with the declaration of a task type, which is then completed with a body. Thus, a declaration can actually come in three parts.


#### Name Resolution Rules

A construct that can be a completion is interpreted as the completion of a prior declaration only if: 

The declaration and the completion occur immediately within the same declarative region;

The defining name or [defining_program_unit_name](S0146) in the completion is the same as in the declaration, or in the case of a [pragma](S0016), the [pragma](S0016) applies to the declaration;

If the declaration is overloadable, then the completion either has a type-conformant profile, or is a [pragma](S0016). 


#### Legality Rules

An implicit declaration shall not have a completion. For any explicit declaration that is specified to require completion, there shall be a corresponding explicit completion. 

Discussion: The implicit declarations of predefined operators are not allowed to have a completion. Enumeration literals, although they are subprograms, are not allowed to have a corresponding [subprogram_body](S0154). That's because the completion rules are described in terms of constructs ([subprogram_declaration](S0141)s) and not entities (subprograms). When a completion is required, it has to be explicit; the implicit null [package_body](S0163) that Section 7 talks about cannot serve as the completion of a [package_declaration](S0161) if a completion is required. 

At most one completion is allowed for a given declaration. Additional requirements on completions appear where each kind of completion is defined. 

Ramification: A subunit is not a completion; the stub is.

If the completion of a declaration is also a declaration, then that declaration might have a completion, too. For example, a limited private type can be completed with a task type, which can then be completed with a task body. This is not a violation of the "at most one completion" rule. 

A type is completely defined at a place that is after its full type definition (if it has one) and after all of its subcomponent types are completely defined. A type shall be completely defined before it is frozen (see 13.14 and 7.3). 

Reason: Index types are always completely defined - no need to mention them. There is no way for a completely defined type to depend on the value of a (still) deferred constant. 

NOTE 1   Completions are in principle allowed for any kind of explicit declaration. However, for some kinds of declaration, the only allowed completion is a [pragma](S0016) Import, and implementations are not required to support [pragma](S0016) Import for every kind of entity. 

Discussion: In fact, we expect that implementations will not support pragma Import of things like types - it's hard to even define the semantics of what it would mean. Therefore, in practice, not every explicit declaration can have a completion. In any case, if an implementation chooses to support pragma Import for, say, types, it can place whatever restrictions on the feature it wants to. For example, it might want the [pragma](S0016) to be a freezing point for the type. 

NOTE 2   There are rules that prevent premature uses of declarations that have a corresponding completion. The Elaboration_Checks of 3.11 prevent such uses at run time for subprograms, protected operations, tasks, and generic units. The rules of 13.14, "Freezing Rules" prevent, at compile time, premature uses of other entities such as private types and deferred constants. 


#### Wording Changes from Ada 83

This subclause is new. It is intended to cover all kinds of completions of declarations, be they a body for a spec, a full type for an incomplete or private type, a full constant declaration for a deferred constant declaration, or a [pragma](S0016) Import for any kind of entity. 

