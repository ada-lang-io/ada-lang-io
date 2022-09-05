---
sidebar_position:  148
---

# C.5  Aspect Discard_Names

{AI12-0072-1} [Specifying the aspect Discard_Names can be used to request a reduction in storage used for the names of entities with runtime name text.] 


#### Static Semantics

{AI12-0072-1} An entity with runtime name text is a nonderived enumeration first subtype, a tagged first subtype, or an exception.

{AI12-0072-1} For an entity with runtime name text, the following language-defined representation aspect may be specified:

Discard_NamesThe type of aspect Discard_Names is Boolean. If directly specified, the [aspect_definition](./AA-13.1#S0348) shall be a static expression. If not specified (including by inheritance), the aspect is False.

Aspect Description for Discard_Names: Requests a reduction in storage for names associated with an entity.


#### Syntax

The form of a [pragma](./AA-2.8#S0019) Discard_Names is as follows: 

  pragma Discard_Names[([On =&gt ] [local_name](./AA-13.1#S0345))];

A [pragma](./AA-2.8#S0019) Discard_Names is allowed only immediately within a [declarative_part](./AA-3.11#S0086), immediately within a [package_specification](./AA-7.1#S0230), or as a configuration pragma. 


#### Legality Rules

{AI12-0072-1} The [local_name](./AA-13.1#S0345) (if present) shall denote an entity with runtime name text. The pragma specifies that the aspect Discard_Names for the type or exception has the value True. Without a [local_name](./AA-13.1#S0345), the pragma specifies that all entities with runtime name text declared after the pragma, within the same declarative region have the value True for aspect Discard_Names. Alternatively, the pragma can be used as a configuration pragma. If the configuration pragma Discard_Names applies to a compilation unit, all entities with runtime name text declared in the compilation unit have the value True for the aspect Discard_Names.

Ramification: {AI12-0072-1} If the aspect is specified for a type, then it is inherited by all descendants of the type. The aspect cannot be specified as False on a derived type (because specifying the aspect is not allowed on derived enumeration types, and by rule applying to all aspects for other types (see 13.1.1)). 


#### Static Semantics

If a [local_name](./AA-13.1#S0345) is given, then a [pragma](./AA-2.8#S0019) Discard_Names is a representation pragma.

This paragraph was deleted.{AI05-0229-1} {AI12-0072-1} 

{AI95-00285-01} {AI95-00400-01} {AI12-0072-1} If the aspect Discard_Names is True for an enumeration type, then the semantics of the Wide_Wide_Image and Wide_Wide_Value attributes are implementation defined for that type[; the semantics of Image, Wide_Image, Value, and Wide_Value are still defined in terms of Wide_Wide_Image and Wide_Wide_Value]. In addition, the semantics of Text_IO.Enumeration_IO are implementation defined. If the aspect Discard_Names is True for a tagged type, then the semantics of the Tags.Wide_Wide_Expanded_Name function are implementation defined for that type[; the semantics of Tags.Expanded_Name and Tags.Wide_Expanded_Name are still defined in terms of Tags.Wide_Wide_Expanded_Name]. If the aspect Discard_Names is True for an exception, then the semantics of the Exceptions.Wide_Wide_Exception_Name function are implementation defined for that exception[; the semantics of Exceptions.Exception_Name and Exceptions.Wide_Exception_Name are still defined in terms of Exceptions.Wide_Wide_Exception_Name].

Implementation defined: The semantics of some attributes and functions of an entity for which aspect Discard_Names is True.

Ramification: The Width attribute is still defined in terms of Image.

{AI95-00285-01} {AI12-0072-1} The semantics of S'Wide_Wide_Image and S'Wide_Wide_Value are implementation defined for any subtype of an enumeration type for which the aspect is True. (The pragma, if used, actually names the first subtype, of course.) 


#### Implementation Advice

{AI12-0072-1} If the aspect Discard_Names is True for an entity, then the implementation should reduce the amount of storage used for storing names associated with that entity. 

Implementation Advice: If aspect Discard_Names is True for an entity, then the amount of storage used for storing names associated with that entity should be reduced.

Reason: A typical implementation of the Image attribute for enumeration types is to store a table containing the names of all the enumeration literals. Aspect Discard_Names allows the implementation to avoid storing such a table without having to prove that the Image attribute is never used (which can be difficult in the presence of separate compilation).

We did not specify the semantics of the Image attribute when aspect Discard_Names is True because different semantics might be desirable in different situations. In some cases, it might make sense to use the Image attribute to print out a useful value that can be used to identify the entity given information in compiler-generated listings. In other cases, it might make sense to get an error at compile time or at run time. In cases where memory is plentiful, the simplest implementation makes sense: ignore the aspect. Implementations that are capable of avoiding the extra storage in cases where the Image attribute is never used might also wish to ignore the aspect.

The same applies to the Tags.Expanded_Name and Exceptions.Exception_Name functions. 


#### Wording Changes from Ada 95

{AI95-00285-01} {AI95-00400-01} Updated the wording to reflect that the double wide image and value functions are now the master versions that the others are defined from. 


#### Wording Changes from Ada 2012

{AI12-0072-1} Corrigendum: Defined the pragma in terms of the aspect Discard_Names, and added a missing definition of the meaning of the configuration pragma. This is not intended to make any semantic change (Ada 2012 has an aspect Discard_Names defined via blanket rules for representation pragmas in 13.1 and 13.1.1), just to clarify the meaning. 

