---
sidebar_position:  18
---

# 3.2  Types and Subtypes


#### Static Semantics

A type is characterized by a set of values, and a set of primitive operations which implement the fundamental aspects of its semantics. An object of a given type is a run-time entity that contains (has) a value of the type. 

Glossary entry: Each object has a type. A type has an associated set of values, and a set of primitive operations which implement the fundamental aspects of its semantics. Types are grouped into categories. Most language-defined categories of types are also classes of types.

Glossary entry: A subtype is a type together with optional constraints, null exclusions, and predicates, which constrain the values of the subtype to satisfy certain conditions. The values of a subtype are a subset of the values of its type.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[type], Def=[a defining characteristic of each object and expression of the language, with an associated set of values, and a set of primitive operations that implement the fundamental aspects of its semantics], Note1=[Types are grouped into categories. Most language-defined categories of types are also classes of types.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[subtype], Def=[a type together with optional constraints, null exclusions, and predicates, which constrain the values of the type to the subset that satisfies the implied conditions]

{AI95-00442-01} Types are grouped into categories of types. There exist several language-defined categories of types (see NOTES below), reflecting the similarity of their values and primitive operations. [Most categories of types form classes of types.] Elementary types are those whose values are logically indivisible; composite types are those whose values are composed of component values. 

Proof: {AI95-00442-01} The formal definition of category and class is found in 3.4. 

Glossary entry: A class is a set of types that is closed under derivation, which means that if a given type is in the class, then all types derived from that type are also in the class. The set of types of a class share common properties, such as their primitive operations.

Glossary entry: A category of types is a set of types with one or more common properties, such as primitive operations. A category of types that is closed under derivation is also known as a class.

Glossary entry: An elementary type is a type that does not have components.

Glossary entry: A composite type may have components.

Glossary entry: A scalar type is either a discrete type or a real type.

Glossary entry: An access type has values that designate aliased objects. Access types correspond to "pointer types" or "reference types" in some other languages.

Glossary entry: A discrete type is either an integer type or an enumeration type. Discrete types may be used, for example, in [case_statement](./AA-5.4#S0176)s and as array indices.

Glossary entry: A real type has values that are approximations of the real numbers. Floating point and fixed point types are real types.

Glossary entry: Integer types comprise the signed integer types and the modular types. A signed integer type has a base range that includes both positive and negative numbers, and has operations that may raise an exception when the result is outside the base range. A modular type has a base range whose lower bound is zero, and has operations with "wraparound" semantics. Modular types subsume what are called "unsigned types" in some other languages.

Glossary entry: An enumeration type is defined by an enumeration of its values, which may be named by identifiers or character literals.

Glossary entry: A character type is an enumeration type whose values include characters.

Glossary entry: A record type is a composite type consisting of zero or more named components, possibly of different types.

Glossary entry: A record extension is a type that extends another type by adding additional components.

Glossary entry: An array type is a composite type whose components are all of the same type. Components are selected by indexing.

Glossary entry: A task type is a composite type used to represent active entities which execute concurrently and which can communicate via queued task entries. The top-level task of a partition is called the environment task.

Glossary entry: A protected type is a composite type whose components are accessible only through one of its protected operations which synchronize concurrent access by multiple tasks.

Glossary entry: A private type gives a view of a type that reveals only some of its properties. The remaining properties are provided by the full view given elsewhere. Private types can be used for defining abstractions that hide unnecessary details from their clients.

Glossary entry: A private extension is a type that extends another type, with the additional properties hidden from its clients.

Glossary entry: An incomplete type gives a view of a type that reveals only some of its properties. The remaining properties are provided by the full view given elsewhere. Incomplete types can be used for defining recursive data structures.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[class of types], Def=[a set of types that is closed under derivation, which means that if a given type is in the class, then all types derived from that type are also in the class], Note1=[The set of types of a class share common properties, such as their primitive operations.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[category of types], Def=[a set of types with one or more common properties, such as primitive operations], Note1=[A category of types that is closed under derivation is also known as a class.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[elementary type], Def=[a type that does not have components] Version=[5],Kind=(AddedNormal),Group=[T],Term=[composite type], Def=[a type with components, such as an array or record] Version=[5],Kind=(AddedNormal),Group=[T],Term=[scalar type], Def=[either a discrete type or a real type] Version=[5],Kind=(AddedNormal),Group=[T],Term=[access type], Def=[a type that has values that designate aliased objects], Note1=[Access types correspond to "pointer types" or "reference types" in some other languages.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[discrete type], Def=[a type that is either an integer type or an enumeration type] Version=[5],Kind=(AddedNormal),Group=[T],Term=[real type], Def=[a type that has values that are approximations of the real numbers], Note1=[Floating point and fixed point types are real types.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[integer type], Def=[a type that represents signed or modular integers], Note1=[A signed integer type has a base range that includes both positive and negative numbers, and has operations that can raise an exception when the result is outside the base range. A modular type has a base range whose lower bound is zero, and has operations with "wraparound" semantics. Modular types subsume what are called "unsigned types" in some other languages.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[enumeration type], Def=[a type defined by an enumeration of its values, which can be denoted by identifiers or character literals] Version=[5],Kind=(AddedNormal),Group=[T],Term=[character type], Def=[an enumeration type whose values include characters] Version=[5],Kind=(AddedNormal),Group=[T],Term=[record type], Def=[a composite type consisting of zero or more named components, possibly of different types] Version=[5],Kind=(AddedNormal),Group=[T],Term=[record extension], Def=[a type that extends another type optionally with additional components] Version=[5],Kind=(AddedNormal),Group=[T],Term=[array type], Def=[a composite type whose components are all of the same type] Version=[5],Kind=(AddedNormal),Group=[T],Term=[task type], Def=[a composite type used to represent active entities which execute concurrently and that can communicate via queued task entries], Note1=[The top-level task of a partition is called the environment task.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[protected type], Def=[a composite type whose components are accessible only through one of its protected operations, which synchronize concurrent access by multiple tasks] Version=[5],Kind=(AddedNormal),Group=[T],Term=[private type], Def=[a view of a type that reveals only some of its properties], Note1=[The remaining properties are provided by the full view given elsewhere. Private types can be used for defining abstractions that hide unnecessary details from their clients.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[private extension], Def=[a type that extends another type, with the additional properties hidden from its clients] Version=[5],Kind=(AddedNormal),Group=[T],Term=[incomplete type], Def=[a view of a type that reveals only a few of its properties], Note1=[The remaining properties are provided by the full view given elsewhere.], Note2=[Incomplete types can be used for defining recursive data structures.]

The elementary types are the scalar types (discrete and real) and the access types (whose values provide access to objects or subprograms). Discrete types are either integer types or are defined by enumeration of their values (enumeration types). Real types are either floating point types or fixed point types.

{AI95-00251-01} {AI95-00326-01} The composite types are the record types, record extensions, array types, interface types, task types, and protected types. 

This paragraph was deleted.{AI95-00442-01} 

{AI95-00326-01} There can be multiple views of a type with varying sets of operations. [An incomplete type represents an incomplete view (see 3.10.1) of a type with a very restricted usage, providing support for recursive data structures. A private type or private extension represents a partial view (see 7.3) of a type, providing support for data abstraction. The full view (see 3.2.1) of a type represents its complete definition.] An incomplete or partial view is considered a composite type[, even if the full view is not]. 

Proof: {AI05-0299-1} The real definitions of the views are in the referenced subclauses. 

{AI95-00326-01} Certain composite types (and views thereof) have special components called discriminants whose values affect the presence, constraints, or initialization of other components. Discriminants can be thought of as parameters of the type.

{AI95-00366-01} The term subcomponent is used in this Reference Manual in place of the term component to indicate either a component, or a component of another subcomponent. Where other subcomponents are excluded, the term component is used instead. Similarly, a part of an object or value is used to mean the whole object or value, or any set of its subcomponents. The terms component, subcomponent, and part are also applied to a type meaning the component, subcomponent, or part of objects and values of the type. 

Discussion: The definition of "part" here is designed to simplify rules elsewhere. By design, the intuitive meaning of "part" will convey the correct result to the casual reader, while this formalistic definition will answer the concern of the compiler-writer.

We use the term "part" when talking about the parent part, ancestor part, or extension part of a type extension. In contexts such as these, the part might represent an empty set of subcomponents (e.g. in a null record extension, or a nonnull extension of a null record). We also use "part" when specifying rules such as those that apply to an object with a "controlled part" meaning that it applies if the object as a whole is controlled, or any subcomponent is. 

{AI95-00231-01} The set of possible values for an object of a given type can be subjected to a condition that is called a constraint (the case of a null constraint that specifies no restriction is also included)[; the rules for which values satisfy a given kind of constraint are given in 3.5 for [range_constraint](./AA-3.5#S0036)s, 3.6.1 for [index_constraint](./AA-3.6#S0057)s, and 3.7.1 for [discriminant_constraint](./AA-3.7#S0064)s]. The set of possible values for an object of an access type can also be subjected to a condition that excludes the null value (see 3.10).

Ramification: {AI12-0140-1} "Null constraint" includes the cases of no explicit constraint, as well as unknown discriminants and unconstrained array type declarations (which are explicit ways to declare no constraint). 

{AI95-00231-01} {AI95-00415-01} {AI12-0445-1} A subtype of a given type is a combination of the type, a constraint on values of the type, and certain attributes specific to the subtype. The given type is called the type of the subtype. Similarly, the associated constraint is called the constraint of the subtype.  The set of values of a subtype consists of the values of its type that satisfy its constraint and any exclusion of the null value. Such values belong to the subtype. The other values of the type are outside the subtype. 

Discussion: We make a strong distinction between a type and its subtypes. In particular, a type is not a subtype of itself. There is no constraint associated with a type (not even a null one), and type-related attributes are distinct from subtype-specific attributes. 

Discussion: We no longer use the term "base type." All types were "base types" anyway in Ada 83, so the term was redundant, and occasionally confusing. In the RM95 we say simply "the type of the subtype" instead of "the base type of the subtype." 

Ramification: The value subset for a subtype might be empty, and need not be a proper subset. 

To be honest: {AI95-00442-01} Any name of a category of types (such as "discrete", "real", or "limited") is also used to qualify its subtypes, as well as its objects, values, declarations, and definitions, such as an "integer type declaration" or an "integer value". In addition, if a term such as "parent subtype" or "index subtype" is defined, then the corresponding term for the type of the subtype is "parent type" or "index type". 

Discussion: We use these corresponding terms without explicitly defining them, when the meaning is obvious. 

A subtype is called an unconstrained subtype if its type has unknown discriminants, or if its type allows range, index, or discriminant constraints, but the subtype does not impose such a constraint; otherwise, the subtype is called a constrained subtype (since it has no unconstrained characteristics). 

Discussion: In an earlier version of Ada 9X, "constrained" meant "has a nonnull constraint." However, we changed to this definition since we kept having to special case composite non-array/nondiscriminated types. It also corresponds better to the (now obsolescent) attribute 'Constrained.

For scalar types, "constrained" means "has a nonnull constraint". For composite types, in implementation terms, "constrained" means that the size of all objects of the subtype is the same, assuming a typical implementation model.

Class-wide subtypes are always unconstrained. 

NOTE   {AI95-00442-01} Any set of types can be called a "category" of types, and any set of types that is closed under derivation (see 3.4) can be called a "class" of types. However, only certain categories and classes are used in the description of the rules of the language - generally those that have their own particular set of primitive operations (see 3.2.3), or that correspond to a set of types that are matched by a given kind of generic formal type (see 12.5). The following are examples of "interesting" language-defined classes: elementary, scalar, discrete, enumeration, character, boolean, integer, signed integer, modular, real, floating point, fixed point, ordinary fixed point, decimal fixed point, numeric, access, access-to-object, access-to-subprogram, composite, array, string, (untagged) record, tagged, task, protected, nonlimited. Special syntax is provided to define types in each of these classes. In addition to these classes, the following are examples of "interesting" language-defined categories: abstract, incomplete, interface, limited, private, record. 

Discussion: A value is a run-time entity with a given type which can be assigned to an object of an appropriate subtype of the type. An operation is a program entity that operates on zero or more operands to produce an effect, or yield a result, or both. 

Ramification: {AI95-00442-01} Note that a type's category (and class) depends on the place of the reference - a private type is composite outside and possibly elementary inside. It's really the view that is elementary or composite. Note that although private types are composite, there are some properties that depend on the corresponding full view - for example, parameter passing modes, and the constraint checks that apply in various places.

{AI95-00345-01} {AI95-00442-01} Every property of types forms a category, but not every property of types represents a class. For example, the set of all abstract types does not form a class, because this set is not closed under derivation. Similarly, the set of all interface types does not form a class.

{AI95-00442-01} The set of limited types does not form a class (since nonlimited types can inherit from limited interfaces), but the set of nonlimited types does. The set of tagged record types and the set of tagged private types do not form a class (because each of them can be extended to create a type of the other category); that implies that the set of record types and the set of private types also do not form a class (even though untagged record types and untagged private types do form a class). In all of these cases, we can talk about the category of the type; for instance, we can talk about the "category of limited types".

{AI95-00442-01} Normatively, the language-defined classes are those that are defined to be inherited on derivation by 3.4; other properties either aren't interesting or form categories, not classes. 

{AI95-00442-01} These language-defined categories are organized like this: 

{AI95-00345-01} all types
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
		untagged
			array
				string
				other array
			record
			task
			protected
		tagged (including interfaces)
			nonlimited tagged record
			limited tagged
				limited tagged record
				synchronized tagged
					tagged task
					tagged protected

{AI95-00345-01} {AI95-00442-01} There are other categories, such as "numeric" and "discriminated", which represent other categorization dimensions, but do not fit into the above strictly hierarchical picture. 

Discussion: {AI95-00345-01} {AI95-00442-01} Note that this is also true for some categories mentioned in the chart. The category "task" includes both untagged tasks and tagged tasks. Similarly for "protected", "limited", and "nonlimited" (note that limited and nonlimited are not shown for untagged composite types). 


#### Wording Changes from Ada 83

{AI05-0299-1} This subclause now precedes the subclauses on objects and named numbers, to cut down on the number of forward references.

We have dropped the term "base type" in favor of simply "type" (all types in Ada 83 were "base types" so it wasn't clear when it was appropriate/necessary to say "base type"). Given a subtype S of a type T, we call T the "type of the subtype S." 


#### Wording Changes from Ada 95

{AI95-00231-01} Added a mention of null exclusions when we're talking about constraints (these are not constraints, but they are similar).

{AI95-00251-01} Defined an interface type to be a composite type.

{AI95-00326-01} Revised the wording so that it is clear that an incomplete view is similar to a partial view in terms of the language.

{AI95-00366-01} Added a definition of component of a type, subcomponent of a type, and part of a type. These are commonly used in the standard, but they were not previously defined.

{AI95-00442-01} {AI05-0299-1} Reworded most of this subclause to use category rather than class, since so many interesting properties are not, strictly speaking, classes. Moreover, there was no normative description of exactly which properties formed classes, and which did not. The real definition of class, along with a list of properties, is now in 3.4. 


## 3.2.1  Type Declarations

A [type_declaration](./AA-3.2#S0023) declares a type and its first subtype. 


#### Syntax

type_declaration<a id="S0023"></a> ::=  [full_type_declaration](./AA-3.2#S0024)
   | [incomplete_type_declaration](./AA-3.10#S0085)
   | [private_type_declaration](./AA-7.3#S0232)
   | [private_extension_declaration](./AA-7.3#S0233)

{AI05-0183-1} full_type_declaration<a id="S0024"></a> ::= 
     type [defining_identifier](./AA-3.1#S0022) [[known_discriminant_part](./AA-3.7#S0061)] is [type_definition](./AA-3.2#S0025)
        [[aspect_specification](./AA-13.1#S0346)];
   | [task_type_declaration](./AA-9.1#S0244)
   | [protected_type_declaration](./AA-9.4#S0249)

{AI95-00251-01} type_definition<a id="S0025"></a> ::= 
     [enumeration_type_definition](./AA-3.5#S0038)	| [integer_type_definition](./AA-3.5#S0041)
   | [real_type_definition](./AA-3.5#S0044)	| [array_type_definition](./AA-3.6#S0051)
   | [record_type_definition](./AA-3.8#S0066)	| [access_type_definition](./AA-3.10#S0079)
   | [derived_type_definition](./AA-3.4#S0035)	| [interface_type_definition](./AA-3.9#S0077)


#### Legality Rules

A given type shall not have a subcomponent whose type is the given type itself. 


#### Static Semantics

The [defining_identifier](./AA-3.1#S0022) of a [type_declaration](./AA-3.2#S0023) denotes the first subtype of the type. The [known_discriminant_part](./AA-3.7#S0061), if any, defines the discriminants of the type (see 3.7, "Discriminants"). The remainder of the [type_declaration](./AA-3.2#S0023) defines the remaining characteristics of (the view of) the type.

{AI95-00230-01} A type defined by a [type_declaration](./AA-3.2#S0023) is a named type; such a type has one or more nameable subtypes. Certain other forms of declaration also include type definitions as part of the declaration for an object. The type defined by such a declaration is anonymous - it has no nameable subtypes. For explanatory purposes, this document sometimes refers to an anonymous type by a pseudo-name, written in italics, and uses such pseudo-names at places where the syntax normally requires an [identifier](./AA-2.3#S0002). For a named type whose first subtype is T, this document sometimes refers to the type of T as simply "the type T". 

Ramification: {AI95-00230-01} The only user-defined types that can be anonymous in the above sense are array, access, task, and protected types. An anonymous array, task, or protected type can be defined as part of an [object_declaration](./AA-3.3#S0032). An anonymous access type can be defined as part of numerous other constructs. 

{AI95-00230-01} {AI95-00326-01} A named type that is declared by a [full_type_declaration](./AA-3.2#S0024), or an anonymous type that is defined by an [access_definition](./AA-3.10#S0084) or as part of declaring an object of the type, is called a full type. The declaration of a full type also declares the full view of the type. The [type_definition](./AA-3.2#S0025), [task_definition](./AA-9.1#S0246), [protected_definition](./AA-9.4#S0251), or [access_definition](./AA-3.10#S0084) that defines a full type is called a full type definition. [Types declared by other forms of [type_declaration](./AA-3.2#S0023) are not separate types; they are partial or incomplete views of some full type.] 

To be honest: Class-wide, universal, and root numeric types are full types. 

Reason: {AI95-00230-01} We need to mention [access_definition](./AA-3.10#S0084) separately, as it may occur in renames, which do not declare objects. 

The definition of a type implicitly declares certain predefined operators that operate on the type, according to what classes the type belongs, as specified in 4.5, "Operators and Expression Evaluation". 

Discussion: We no longer talk about the implicit declaration of basic operations. These are treated like an [if_statement](./AA-5.3#S0175) - they don't need to be declared, but are still applicable to only certain classes of types.

The predefined types [(for example the types Boolean, Wide_Character, Integer, root_integer, and universal_integer)] are the types that are defined in [a predefined library package called] Standard[; this package also includes the [(implicit)] declarations of their predefined operators]. [The package Standard is described in A.1.] 

Ramification: We use the term "predefined" to refer to entities declared in the visible part of Standard, to implicitly declared operators of a type whose semantics are defined by the language, to Standard itself, and to the "predefined environment". We do not use this term to refer to library packages other than Standard. For example Text_IO is a language-defined package, not a predefined package, and Text_IO.Put_Line is not a predefined operation. 


#### Dynamic Semantics

The elaboration of a [full_type_declaration](./AA-3.2#S0024) consists of the elaboration of the full type definition. Each elaboration of a full type definition creates a distinct type and its first subtype. 

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

NOTE 1   Each of the above examples declares a named type. The identifier given denotes the first subtype of the type. Other named subtypes of the type can be declared with [subtype_declaration](./AA-3.2#S0026)s (see 3.2.2). Although names do not directly denote types, a phrase like "the type Column" is sometimes used in this document to refer to the type of Column, where Column denotes the first subtype of the type. For an example of the definition of an anonymous type, see the declaration of the array Color_Table in 3.3.1; its type is anonymous - it has no nameable subtypes. 


#### Wording Changes from Ada 83

The syntactic category [full_type_declaration](./AA-3.2#S0024) now includes task and protected type declarations.

We have generalized the concept of first-named subtype (now called simply "first subtype") to cover all kinds of types, for uniformity of description elsewhere. RM83 defined first-named subtype in Section 13. We define first subtype here, because it is now a more fundamental concept. We renamed the term, because in Ada 95 some first subtypes have no name.

{AI95-00230-01} We no longer elaborate [discriminant_part](./AA-3.7#S0059)s, because there is nothing to do, and it was complex to say that you only wanted to elaborate it once for a private or incomplete type. This is also consistent with the fact that subprogram specifications are not elaborated (neither in Ada 83 nor in Ada 95). Note, however, that an [access_definition](./AA-3.10#S0084) appearing in a [discriminant_part](./AA-3.7#S0059) is elaborated at the [full_type_declaration](./AA-3.2#S0024) (for a nonlimited type) or when an object with such a discriminant is created (for a limited type). 


#### Wording Changes from Ada 95

{AI95-00230-01} Added wording so that anonymous access types are always full types, even if they appear in renames.

{AI95-00251-01} Added interface types (see 3.9.4) to the syntax.

{AI95-00326-01} Added a definition of full view, so that all types have a well-defined full view. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [full_type_declaration](./AA-3.2#S0024). This is described in 13.1.1. 


## 3.2.2  Subtype Declarations

A [subtype_declaration](./AA-3.2#S0026) declares a subtype of some previously declared type, as defined by a [subtype_indication](./AA-3.2#S0027). 


#### Syntax

{AI05-0183-1} subtype_declaration<a id="S0026"></a> ::= 
   subtype [defining_identifier](./AA-3.1#S0022) is [subtype_indication](./AA-3.2#S0027)
        [[aspect_specification](./AA-13.1#S0346)];

{AI95-00231-01} subtype_indication<a id="S0027"></a> ::=  [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028) [[constraint](./AA-3.2#S0029)]

subtype_mark<a id="S0028"></a> ::= subtype_[name](./AA-4.1#S0091)

Ramification: Note that [name](./AA-4.1#S0091) includes [attribute_reference](./AA-4.1#S0100); thus, S'Base can be used as a [subtype_mark](./AA-3.2#S0028). 

Reason: We considered changing [subtype_mark](./AA-3.2#S0028) to subtype_name. However, existing users are used to the word "mark," so we're keeping it. 

constraint<a id="S0029"></a> ::= [scalar_constraint](./AA-3.2#S0030) | [composite_constraint](./AA-3.2#S0031)

scalar_constraint<a id="S0030"></a> ::= 
     [range_constraint](./AA-3.5#S0036) | [digits_constraint](./AA-3.5#S0050) | [delta_constraint](./AA-J.3#S0367)

composite_constraint<a id="S0031"></a> ::= 
     [index_constraint](./AA-3.6#S0057) | [discriminant_constraint](./AA-3.7#S0064)


#### Name Resolution Rules

A [subtype_mark](./AA-3.2#S0028) shall resolve to denote a subtype. The type determined by a [subtype_mark](./AA-3.2#S0028) is the type of the subtype denoted by the [subtype_mark](./AA-3.2#S0028). 

Ramification: {AI05-0005-1} Types are never directly named; all [subtype_mark](./AA-3.2#S0028)s denote subtypes - possibly an unconstrained (base) subtype, but never the type. When we use the term anonymous type we really mean a type with no nameable subtypes. 


#### Dynamic Semantics

The elaboration of a [subtype_declaration](./AA-3.2#S0026) consists of the elaboration of the [subtype_indication](./AA-3.2#S0027). The elaboration of a [subtype_indication](./AA-3.2#S0027) creates a new subtype. If the [subtype_indication](./AA-3.2#S0027) does not include a [constraint](./AA-3.2#S0029), the new subtype has the same (possibly null) constraint as that denoted by the [subtype_mark](./AA-3.2#S0028). The elaboration of a [subtype_indication](./AA-3.2#S0027) that includes a [constraint](./AA-3.2#S0029) proceeds as follows: 

The [constraint](./AA-3.2#S0029) is first elaborated.

A check is then made that the [constraint](./AA-3.2#S0029) is compatible with the subtype denoted by the [subtype_mark](./AA-3.2#S0028). 

Ramification: The checks associated with constraint compatibility are all Range_Checks. Discriminant_Checks and Index_Checks are associated only with checks that a value satisfies a constraint. 

The condition imposed by a [constraint](./AA-3.2#S0029) is the condition obtained after elaboration of the [constraint](./AA-3.2#S0029). The rules defining compatibility are given for each form of [constraint](./AA-3.2#S0029) in the appropriate subclause. These rules are such that if a [constraint](./AA-3.2#S0029) is compatible with a subtype, then the condition imposed by the [constraint](./AA-3.2#S0029) cannot contradict any condition already imposed by the subtype on its values. The exception Constraint_Error is raised if any check of compatibility fails. 

To be honest: The condition imposed by a [constraint](./AA-3.2#S0029) is named after it - a [range_constraint](./AA-3.5#S0036) imposes a range constraint, etc. 

Ramification: A [range_constraint](./AA-3.5#S0036) causes freezing of its type. Other [constraint](./AA-3.2#S0029)s do not. 

NOTE 1   {AI12-0440-1} A [scalar_constraint](./AA-3.2#S0030) can be applied to a subtype of an appropriate scalar type (see 3.5, 3.5.9, and J.3), even if the subtype is already constrained. On the other hand, a [composite_constraint](./AA-3.2#S0031) can be applied to a composite subtype (or an access-to-composite subtype) only if the composite subtype is unconstrained (see 3.6.1 and 3.7.1). 


#### Examples

Examples of subtype declarations: 

```ada
{AI95-00433-01} subtype Rainbow   is Color range Red .. Blue;        --  see 3.2.1
subtype Red_Blue  is Rainbow;
subtype Int       is Integer;
subtype Small_Int is Integer range -10 .. 10;
subtype Up_To_K   is Column range 1 .. K;            --  see 3.2.1
subtype Square    is Matrix(1 .. 10, 1 .. 10);       --  see 3.6
subtype Male      is Person(Sex =&gt M);               --  see 3.10.1
subtype Binop_Ref is not null Binop_Ptr;             --  see 3.10

```


#### Incompatibilities With Ada 83

In Ada 95, all [range_constraint](./AA-3.5#S0036)s cause freezing of their type. Hence, a type-related representation item for a scalar type has to precede any [range_constraint](./AA-3.5#S0036)s whose type is the scalar type. 


#### Wording Changes from Ada 83

[Subtype_mark](./AA-3.2#S0028)s allow only subtype names now, since types are never directly named. There is no need for RM83-3.3.2(3), which says a [subtype_mark](./AA-3.2#S0028) can denote both the type and the subtype; in Ada 95, you denote an unconstrained (base) subtype if you want, but never the type.

The syntactic category type_mark is now called [subtype_mark](./AA-3.2#S0028), since it always denotes a subtype. 


#### Extensions to Ada 95

{AI95-00231-01} An optional [null_exclusion](./AA-3.10#S0083) can be used in a [subtype_indication](./AA-3.2#S0027). This is described in 3.10. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [subtype_declaration](./AA-3.2#S0026). This is described in 13.1.1. 


## 3.2.3  Classification of Operations


#### Static Semantics

{AI95-00416-01} An operation operates on a type T if it yields a value of type T, if it has an operand whose expected type (see 8.6) is T, or if it has an access parameter or access result type (see 6.1) designating T. A predefined operator, or other language-defined operation such as assignment or a membership test, that operates on a type, is called a predefined operation of the type. The primitive operations of a type are the predefined operations of the type, plus any user-defined primitive subprograms. 

Glossary entry: The primitive operations of a type are the operations (such as subprograms) declared together with the type declaration. They are inherited by other types in the same class of types. For a tagged type, the primitive subprograms are dispatching subprograms, providing run-time polymorphism. A dispatching subprogram may be called with statically tagged operands, in which case the subprogram body invoked is determined at compile time. Alternatively, a dispatching subprogram may be called using a dispatching call, in which case the subprogram body invoked is determined at run time.

Version=[5],Kind=(Added),Group=[T],Term=[primitive operations of a type], Def=[the operations (such as subprograms) declared together with the type declarations], Note1=[Primitive operations are inherited by other types in the same derivation class of types.]

To be honest: Protected subprograms are not considered to be "primitive subprograms", even though they are subprograms, and they are inherited by derived types. 

Discussion: We use the term "primitive subprogram" in most of the rest of the manual. The term "primitive operation" is used mostly in conceptual discussions. 

The primitive subprograms of a specific type are defined as follows: 

The predefined operators of the type (see 4.5);

For a derived type, the inherited (see 3.4) user-defined subprograms;

For an enumeration type, the enumeration literals (which are considered parameterless functions - see 3.5.1);

For a specific type declared immediately within a [package_specification](./AA-7.1#S0230), any subprograms (in addition to the enumeration literals) that are explicitly declared immediately within the same [package_specification](./AA-7.1#S0230) and that operate on the type;

{AI05-0128-1} For a specific type with an explicitly declared primitive "=" operator whose result type is Boolean, the corresponding "/=" operator (see 6.6);

{AI95-00200-01} For a nonformal type, any subprograms not covered above [that are explicitly declared immediately within the same declarative region as the type] and that override (see 8.3) other implicitly declared primitive subprograms of the type. 

Discussion: In Ada 83, only subprograms declared in the visible part were "primitive" (i.e. derivable). In Ada 95, mostly because of child library units, we include all operations declared in the private part as well, and all operations that override implicit declarations. 

Ramification: It is possible for a subprogram to be primitive for more than one type, though it is illegal for a subprogram to be primitive for more than one tagged type. See 3.9. 

Discussion: The order of the implicit declarations when there are both predefined operators and inherited subprograms is described in 3.4, "Derived Types and Classes". 

Ramification: {AI95-00200-01} Subprograms declared in a generic package specification are never primitive for a formal type, even if they happen to override an operation of the formal type. This includes formal subprograms, which are never primitive operations (that's true even for an abstract formal subprogram). 

A primitive subprogram whose designator is an [operator_symbol](./AA-6.1#S0202) is called a primitive operator.


#### Incompatibilities With Ada 83

The attribute S'Base is no longer defined for nonscalar subtypes. Since this was only permitted as the prefix of another attribute, and there are no interesting nonscalar attributes defined for an unconstrained composite or access subtype, this should not affect any existing programs. 


#### Extensions to Ada 83

The primitive subprograms (derivable subprograms) include subprograms declared in the private part of a package specification as well, and those that override implicitly declared subprograms, even if declared in a body. 


#### Wording Changes from Ada 83

We have dropped the confusing term operation of a type in favor of the more useful primitive operation of a type and the phrase operates on a type.

The description of S'Base has been moved to 3.5, "Scalar Types" because it is now defined only for scalar types. 


#### Wording Changes from Ada 95

{AI95-00200-01} Clarified that a formal subprogram that happens to override a primitive operation of a formal type is not a primitive operation (and thus not a dispatching operation) of the formal type.

{AI95-00416-01} Added wording to include access result types in the kinds of operations that operate on a type T. 


#### Wording Changes from Ada 2005

{AI05-0128-1} Correction: The implicitly declared "/=" for a primitive "=" operator is also primitive; this makes it eligible to be made visible by a use type clause. 


## 3.2.4  Subtype Predicates

{AI05-0153-3} {AI05-0269-1} {AI05-0299-1} {AI12-0396-1} {AI12-0419-1} The language-defined predicate aspects Static_Predicate and Dynamic_Predicate may be used to define properties of subtypes. A predicate specification is an [aspect_specification](./AA-13.1#S0346) for one of the two predicate aspects. General rules for aspects and [aspect_specification](./AA-13.1#S0346)s are found in Clause 13 (13.1 and 13.1.1 respectively). The predicate aspects are assertion aspects (see 11.4.2). [The predicate aspects are not inherited, but their effects are additive, as defined below.] 

Aspect Description for Static_Predicate: Condition that will hold true for objects of a given subtype; the subtype may be static.

Aspect Description for Dynamic_Predicate: Condition that will hold true for objects of a given subtype; the subtype is not static.


#### Name Resolution Rules

{AI05-0153-3} The expected type for a predicate aspect [expression](./AA-4.4#S0132) is any boolean type.


#### Static Semantics

{AI05-0153-3} A predicate specification may be given on a [type_declaration](./AA-3.2#S0023) or a [subtype_declaration](./AA-3.2#S0026), and applies to the declared subtype. In addition, predicate specifications apply to certain other subtypes: 

{AI12-0071-1} {AI12-0099-1} For a (first) subtype defined by a type declaration, any predicates of parent or progenitor subtypes apply.

For a subtype created by a [subtype_indication](./AA-3.2#S0027), the predicate of the subtype denoted by the [subtype_mark](./AA-3.2#S0028) applies. 

This paragraph was deleted.{AI05-0153-3} {AI12-0071-1} 

{AI05-0290-1} Predicate checks are defined to be enabled or disabled for a given subtype as follows:

If a subtype is declared by a [type_declaration](./AA-3.2#S0023) or [subtype_declaration](./AA-3.2#S0026) that includes a predicate specification, then: 

if performing checks is required by the Static_Predicate assertion policy (see 11.4.2) and the declaration includes a Static_Predicate specification, then predicate checks are enabled for the subtype;

if performing checks is required by the Dynamic_Predicate assertion policy (see 11.4.2) and the declaration includes a Dynamic_Predicate specification, then predicate checks are enabled for the subtype;

otherwise, predicate checks are disabled for the subtype[, regardless of whether predicate checking is enabled for any other subtypes mentioned in the declaration]; 

{AI12-0099-1} If a subtype is defined by a type declaration that does not include a predicate specification, then predicate checks are enabled for the subtype if and only if any predicate checks are enabled for parent or progenitor subtypes;

If a subtype is created by a [subtype_indication](./AA-3.2#S0027) other than in one of the previous cases, then predicate checks are enabled for the subtype if and only if predicate checks are enabled for the subtype denoted by the [subtype_mark](./AA-3.2#S0028);

Otherwise, predicate checks are disabled for the given subtype.

Discussion: In this case, no predicate specifications can apply to the subtype and so it doesn't typically matter whether predicate checks are enabled. This rule does make a difference, however, when determining whether predicate checks are enabled for another type when this type is one of multiple progenitors. See the "derived type declaration" wording above.

{AI12-0071-1} Even when predicate checks are disabled, a predicate can affect various Legality Rules, the results of membership tests, the items in a for loop, and the result of the Valid attribute. 

{AI12-0054-2} For a subtype with a directly-specified predicate aspect, the following additional language-defined aspect may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1):

Predicate_FailureThis aspect shall be specified by an [expression](./AA-4.4#S0132), which determines the action to be performed when a predicate check fails because a directly-specified predicate aspect of the subtype evaluates to False, as explained below. 

Aspect Description for Predicate_Failure: Action to be performed when a predicate check fails.


#### Name Resolution Rules

{AI12-0054-2} The expected type for the Predicate_Failure [expression](./AA-4.4#S0132) is String. 


#### Legality Rules

{AI05-0153-3} {AI05-0269-1} The [expression](./AA-4.4#S0132) of a Static_Predicate specification shall be predicate-static; that is, one of the following: 

a static expression;

{AI12-0039-1} a membership test whose tested_[simple_expression](./AA-4.4#S0138) is the current instance, and whose [membership_choice_list](./AA-4.4#S0136) meets the requirements for a static membership test (see 4.9);

a [case_expression](./AA-4.5#S0151) whose selecting_[expression](./AA-4.4#S0132) is the current instance, and whose dependent_[expression](./AA-4.4#S0132)s are static expressions;

a call to a predefined equality or ordering operator, where one operand is the current instance, and the other is a static expression;

{AI05-0262-1} {AI12-0099-1} a call to a predefined boolean operator and, or, xor, or not, where each operand is predicate-static;

{AI05-0269-1} a short-circuit control form where both operands are predicate-static; or

a parenthesized predicate-static [expression](./AA-4.4#S0132). 

{AI05-0262-1} A predicate shall not be specified for an incomplete subtype.

Reason: The expression of such a predicate could not depend on the properties of the value of the type (since it doesn't have any), so it is useless and we don't want to require the added complexity needed to support it. 

{AI05-0287-1} If a predicate applies to a subtype, then that predicate shall not mention any other subtype to which the same predicate applies.

Reason: This is intended to prevent recursive predicates, which cause definitional problems for static predicates. Inside of the predicate, the subtype name refers to the current instance of the subtype, which is an object, so a direct use of the subtype name cannot be recursive. But other subtypes naming the same type might: 

```ada
   type Really_Ugly is private;
private
   subtype Ugly is Really_Ugly;
   type Really_Ugly is new Integer
      with Static_Predicate =&gt Really_Ugly not in Ugly; -- Illegal!

```

{AI05-0153-3} An index subtype, [discrete_range](./AA-3.6#S0058) of an [index_constraint](./AA-3.6#S0057) or [slice](./AA-4.1#S0097), or a [discrete_subtype_definition](./AA-3.6#S0055) of a [constrained_array_definition](./AA-3.6#S0054), [entry_declaration](./AA-9.5#S0257), or [entry_index_specification](./AA-9.5#S0263) shall not denote a subtype to which predicate specifications apply.

{AI05-0153-3} The [prefix](./AA-4.1#S0093) of an [attribute_reference](./AA-4.1#S0100) whose [attribute_designator](./AA-4.1#S0101) is First, Last, or Range shall not denote a scalar subtype to which predicate specifications apply.

Reason: {AI05-0297-1} This is to prevent confusion about whether the First value is the lowest value of the subtype (which does not depend on the predicate) or the lowest value of the subtype which meets the predicate. (For a dynamic predicate, determining this latter value is expensive as it would usually require a loop.) For a static subtype that has a static predicate, the First_Valid and Last_Valid attributes (see 3.5.5) can be used instead. 

{AI05-0153-3} {AI05-0262-1} {AI05-0287-1} The [discrete_subtype_definition](./AA-3.6#S0055) of a [loop_parameter_specification](./AA-5.5#S0181) shall not denote a nonstatic subtype to which predicate specifications apply or any subtype to which Dynamic_Predicate specifications apply.

{AI05-0153-3} {AI05-0262-1} The [discrete_choice](./AA-3.8#S0074) of a [named_array_aggregate](./AA-4.3#S0116) shall not denote a nonstatic subtype to which predicate specifications apply.

Reason: {AI05-0262-1} This rule prevents noncontiguous dynamically bounded array aggregates, which could be expensive to check for. (Array aggregates have rules to prevent problems with static subtypes.) We define this rule here so that the runtime generic body check applies. 

{AI05-0262-1} In addition to the places where Legality Rules normally apply (see 12.3), these rules apply also in the private part of an instance of a generic unit. 


#### Dynamic Semantics

{AI12-0071-1} If any of the above Legality Rules is violated in an instance of a generic unit, Program_Error is raised at the point of the violation.

Discussion: This is the usual way around the contract model; this applies even in instance bodies. Note that errors in instance specifications will be detected at compile time by the "recheck" of the specification; only errors in the body should raise Program_Error. 

{AI12-0071-1} To determine whether a value satisfies the predicates of a subtype S, the following tests are performed in the following order, until one of the tests fails, in which case the predicates are not satisfied and no further tests are performed, or all of the tests succeed, in which case the predicates are satisfied:

the value is first tested to determine whether it satisfies any constraints or any null exclusion of S;

then:

{AI12-0419-1} if S is a first subtype, the value is tested to determine whether it satisfies the predicates of the parent and progenitor subtypes (if any) of S (in an arbitrary order), after a (view) conversion of the value to the corresponding parent or progenitor type;

Ramification: This rule has an effect for derived types (which have a parent subtype and may have progenitors) and for task and protected types (which may have progentitors). Other kinds of type declarations can have neither, and no test is required for other first subtypes. 

if S is defined by a [subtype_indication](./AA-3.2#S0027), the value is tested to determine whether it satisfies the predicates of the subtype denoted by the [subtype_mark](./AA-3.2#S0028) of the [subtype_indication](./AA-3.2#S0027); 

finally, if S is defined by a declaration to which one or more predicate specifications apply, the predicates are evaluated (in an arbitrary order) to test that all of them yield True for the given value. 

Discussion: It is important to stop on the first of the above steps that fails, as later steps might presume that the earlier steps had succeeded. 

{AI05-0153-3} {AI05-0290-1} If predicate checks are enabled for a given subtype, then: 

{AI12-0054-2} {AI12-0071-1} {AI12-0301-1} {AI12-0333-1} {AI12-0432-1} [On a subtype conversion, a check is performed that the operand satisfies the predicates of the target subtype, except for certain view conversions (see 4.6).] In addition, after normal completion and leaving of a subprogram, for each in out or out parameter that is passed by reference, a check is performed that the value of the parameter satisfies the predicates of the subtype of the actual. For an object created by an [object_declaration](./AA-3.3#S0032) with no explicit initialization [expression](./AA-4.4#S0132), or by an uninitialized [allocator](./AA-4.8#S0164), if the types of any parts have specified Default_Value or Default_Component_Value aspects, or any subcomponents have [default_expression](./AA-3.7#S0063)s, a check is performed that the value of the created object satisfies the predicates of the nominal subtype.

Ramification: {AI12-0333-1} Most parameter passing is covered by the subtype conversion rule: all inbound in and in out parameters are converted to the formal subtype, and the copy-back for by-copy out and in out parameters includes a conversion to the actual subtype. The remaining parameter-passing cases are covered by special rules: by-reference out and in out parameters by the rule given above, and we don't want any predicate checks on inbound out parameters, accomplished in part by a special rule in 4.6. 

{AI12-0054-2} If any of the predicate checks fail, Assertion_Error is raised, unless the subtype whose directly-specified predicate aspect evaluated to False also has a directly-specified Predicate_Failure aspect. In that case, the specified Predicate_Failure [expression](./AA-4.4#S0132) is evaluated; if the evaluation of the Predicate_Failure [expression](./AA-4.4#S0132) propagates an exception occurrence, then this occurrence is propagated for the failure of the predicate check; otherwise, Assertion_Error is raised, with an associated message string defined by the value of the Predicate_Failure [expression](./AA-4.4#S0132). In the absence of such a Predicate_Failure aspect, an implementation-defined message string is associated with the Assertion_Error exception.

Ramification: Predicates are not evaluated at the point of the (sub)type declaration. 

Implementation Note: Static_Predicate checks can be removed even in the presence of potentially invalid values, just as constraint checks can be removed. 

Implementation defined: The message string associated with the Assertion_Error exception raised by the failure of a predicate check if there is no applicable Predicate_Failure aspect.

Paragraphs 32 and 33 were moved above 

This paragraph was deleted.

NOTE 1   {AI05-0153-3} A predicate specification does not cause a subtype to be considered constrained.

NOTE 2   {AI05-0153-3} A Static_Predicate, like a constraint, always remains True for all objects of the subtype, except in the case of uninitialized variables and other invalid values. A Dynamic_Predicate, on the other hand, is checked as specified above, but can become False at other times. For example, the predicate of a record subtype is not checked when a subcomponent is modified.

NOTE 3   {AI12-0071-1} No predicates apply to the base subtype of a scalar type; every value of a scalar type T is considered to satisfy the predicates of T'Base.

NOTE 4   {AI12-0054-2} Predicate_Failure [expression](./AA-4.4#S0132)s are never evaluated during the evaluation of a membership test (see 4.5.2) or Valid attribute (see 13.9.2).

NOTE 5   {AI12-0054-2} A Predicate_Failure [expression](./AA-4.4#S0132) can be a [raise_expression](./AA-11.3#S0309) (see 11.3). 


#### Examples

{AI12-0429-1} Examples of predicates applied to scalar types:

```ada
{AI12-0054-2} subtype Basic_Letter is Character -- See A.3.2 for "basic letter".
   with Static_Predicate =&gt Basic_Letter in 'A'..'Z' | 'a'..'z' | 'Æ' | 
                                'æ' | 'Ð' | 'ð' | 'Þ' | 'þ' | 'ß';

```

```ada
{AI12-0054-2} subtype Even_Integer is Integer
   with Dynamic_Predicate =&gt Even_Integer mod 2 = 0,
        Predicate_Failure =&gt "Even_Integer must be a multiple of 2";

```

{AI12-0054-2} Text_IO (see A.10.1) could have used predicates to describe some common exceptional conditions as follows:

```ada
with Ada.IO_Exceptions;
package Ada.Text_IO is

```

```ada
   type File_Type is limited private;

```

```ada
   subtype Open_File_Type is File_Type
      with Dynamic_Predicate =&gt Is_Open (Open_File_Type),
           Predicate_Failure =&gt raise Status_Error with "File not open";
   subtype Input_File_Type is Open_File_Type
      with Dynamic_Predicate =&gt Mode (Input_File_Type) = In_File,
           Predicate_Failure =&gt raise Mode_Error with "Cannot read file: "
              & Name (Input_File_Type);
   subtype Output_File_Type is Open_File_Type
      with Dynamic_Predicate =&gt Mode (Output_File_Type) /= In_File,
           Predicate_Failure =&gt raise Mode_Error with "Cannot write file: "
              & Name (Output_File_Type);

```

```ada
   ...

```

```ada
   function Mode (File : in Open_File_Type) return File_Mode;
   function Name (File : in Open_File_Type) return String;
   function Form (File : in Open_File_Type) return String;

```

```ada
   ...

```

```ada
   procedure Get (File : in Input_File_Type; Item : out Character);

```

```ada
   procedure Put (File : in Output_File_Type; Item : in Character);

```

```ada
   ...

```

```ada
   -- Similarly for all of the other input and output subprograms.

```

Discussion: We didn't change the language-defined Text_IO this way for Ada 2022 as it would be incompatible in marginal cases: these subprogram specifications would not be subtype conformant with existing access-to-subprogram types, so Put_Line'Access (for instance) would become illegal in existing code. The gain would not be worth the disruption. 


#### Extensions to Ada 2005

{AI05-0153-3} {AI05-0262-1} {AI05-0276-1} {AI05-0290-1} Predicate aspects are new in Ada 2012. 


#### Inconsistencies With Ada 2012

{AI12-0301-1} Correction: Predicate checks are now performed on default-initialized objects with parts that have Default_Value or Default_Component_Value specified. This is consistent with the handling of constraint checks for such objects; it is thought that the omission was unintended. However, a program that declares such an object and depends on there not being a predicate check in original Ada 2012 will fail in Ada 2022. As these attributes were new in Ada 2012, their use is uncommon, so we believe that this inconsistency will be rare and more likely to catch a bug than create one. 


#### Extensions to Ada 2012

{AI12-0054-2} Corrigendum: The Predicate_Failure aspect is new. We can consider this a correction as it is always possible for implementers to add implementation-defined aspects, so the same is true for language-defined aspects. 


#### Wording Changes from Ada 2012

{AI12-0071-1} Corrigendum: Specified the order of evaluation of most predicates, by defining the new term "satisfies the predicates of the subtype". This is not inconsistent, as the order previously was unspecified, so any code depending on the order was incorrect. The change is necessary so that the Predicate_Failure aspect has consistent results in cases where multiple predicates and aspects apply; see the Ada.Text_IO example above for such a case.

{AI12-0099-1} Corrigendum: Revised wording to ensure all kinds of types are covered, including the anonymous task associated with a [single_task_declaration](./AA-9.1#S0245), and generalized it.

{AI12-0099-1} Corrigendum: Revised wording to list the boolean operators that can be predicate-static, to eliminate confusion about whether not is included.

{AI12-0333-1} {AI12-0432-1} Correction: Predicate checks are no longer made for any inbound out parameters nor for the target of an [assignment_statement](./AA-5.2#S0173) when it is a view conversion. The rule change for this is found in 4.6, so the inconsistency is documented there. 

