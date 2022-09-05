---
sidebar_position:  29
---

# 4.1  Names

[[Name](./AA-4.1#S0091)s can denote declared entities, whether declared explicitly or implicitly (see 3.1). [Name](./AA-4.1#S0091)s can also denote objects or subprograms designated by access values; the results of [type_conversion](./AA-4.6#S0162)s or [function_call](./AA-6.4#S0218)s; subcomponents and slices of objects and values; protected subprograms, single entries, entry families, and entries in families of entries. Finally, [name](./AA-4.1#S0091)s can denote attributes of any of the foregoing.] 


#### Syntax

{AI05-0003-1} {AI05-0139-2} {AI12-0125-3} name<a id="S0091"></a> ::= 
     [direct_name](./AA-4.1#S0092)	| [explicit_dereference](./AA-4.1#S0094)
   | [indexed_component](./AA-4.1#S0096)	| [slice](./AA-4.1#S0097)
   | [selected_component](./AA-4.1#S0098)	| [attribute_reference](./AA-4.1#S0100)
   | [type_conversion](./AA-4.6#S0162)	| [function_call](./AA-6.4#S0218)
   | [character_literal](./AA-2.5#S0015)	| [qualified_expression](./AA-4.7#S0163)
   | [generalized_reference](./AA-4.1#S0104)	| [generalized_indexing](./AA-4.1#S0105)
   | [target_name](./AA-5.2#S0174)

direct_name<a id="S0092"></a> ::= [identifier](./AA-2.3#S0002) | [operator_symbol](./AA-6.1#S0202)

Discussion: {AI95-00114-01} [character_literal](./AA-2.5#S0015) is no longer a [direct_name](./AA-4.1#S0092). [character_literal](./AA-2.5#S0015)s are usable even when the corresponding enumeration type declaration is not visible. See 4.2. 

prefix<a id="S0093"></a> ::= [name](./AA-4.1#S0091) | [implicit_dereference](./AA-4.1#S0095)

explicit_dereference<a id="S0094"></a> ::= [name](./AA-4.1#S0091).all

implicit_dereference<a id="S0095"></a> ::= [name](./AA-4.1#S0091)

{AI05-0004-1} [Certain forms of [name](./AA-4.1#S0091) ([indexed_component](./AA-4.1#S0096)s, [selected_component](./AA-4.1#S0098)s, [slice](./AA-4.1#S0097)s, and [attribute_reference](./AA-4.1#S0100)s) include a [prefix](./AA-4.1#S0093) that is either itself a [name](./AA-4.1#S0091) that denotes some related entity, or an [implicit_dereference](./AA-4.1#S0095) of an access value that designates some related entity.] 


#### Name Resolution Rules

The [name](./AA-4.1#S0091) in a dereference (either an [implicit_dereference](./AA-4.1#S0095) or an [explicit_dereference](./AA-4.1#S0094)) is expected to be of any access type. 


#### Static Semantics

{AI05-0008-1} If the type of the [name](./AA-4.1#S0091) in a dereference is some access-to-object type T, then the dereference denotes a view of an object, the nominal subtype of the view being the designated subtype of T. If the designated subtype has unconstrained discriminants, the (actual) subtype of the view is constrained by the values of the discriminants of the designated object, except when there is a partial view of the type of the designated subtype that does not have discriminants, in which case the dereference is not constrained by its discriminant values. 

Ramification: If the value of the [name](./AA-4.1#S0091) is the result of an access type conversion, the dereference denotes a view created as part of the conversion. The nominal subtype of the view is not necessarily the same as that used to create the designated object. See 4.6. 

To be honest: We sometimes refer to the nominal subtype of a particular kind of [name](./AA-4.1#S0091) rather than the nominal subtype of the view denoted by the [name](./AA-4.1#S0091) (presuming the [name](./AA-4.1#S0091) denotes a view of an object). These two uses of nominal subtype are intended to mean the same thing. 

Reason: {AI05-0008-1} The last sentence was not present in Ada 95; it is necessary in Ada 2005 because general access types can designate unconstrained objects, which was not possible in Ada 95. Thus, the rules that had this effect in Ada 95 (the object being constrained by its initial value) don't work in Ada 2005 and we have to say this explicitly.

{AI05-0008-1} The "except" part of the last sentence prevents privacy "breaking", so that if a private type has discriminants only in the full view, they don't interfere with freely interassigning values between objects of the type, even when the objects live in the heap. 

Implementation Note: {AI05-0008-1} Since we don't depend on whether the designated object is constrained, it is not necessary to include a constrained bit in every object that could be designated by a general access type. 

If the type of the [name](./AA-4.1#S0091) in a dereference is some access-to-subprogram type S, then the dereference denotes a view of a subprogram, the profile of the view being the designated profile of S. 

Ramification: This means that the formal parameter names and default expressions to be used in a call whose [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) is a dereference are those of the designated profile, which need not be the same as those of the subprogram designated by the access value, since 'Access requires only subtype conformance, not full conformance. 


#### Dynamic Semantics

{AI95-00415-01} The evaluation of a [name](./AA-4.1#S0091) determines the entity denoted by the [name](./AA-4.1#S0091). This evaluation has no other effect for a [name](./AA-4.1#S0091) that is a [direct_name](./AA-4.1#S0092) or a [character_literal](./AA-2.5#S0015).

[The evaluation of a [name](./AA-4.1#S0091) that has a [prefix](./AA-4.1#S0093) includes the evaluation of the [prefix](./AA-4.1#S0093).] The evaluation of a [prefix](./AA-4.1#S0093) consists of the evaluation of the [name](./AA-4.1#S0091) or the [implicit_dereference](./AA-4.1#S0095). The [prefix](./AA-4.1#S0093) denotes the entity denoted by the [name](./AA-4.1#S0091) or the [implicit_dereference](./AA-4.1#S0095).

The evaluation of a dereference consists of the evaluation of the [name](./AA-4.1#S0091) and the determination of the object or subprogram that is designated by the value of the [name](./AA-4.1#S0091). A check is made that the value of the [name](./AA-4.1#S0091) is not the null access value. Constraint_Error is raised if this check fails. The dereference denotes the object or subprogram designated by the value of the [name](./AA-4.1#S0091). 


#### Examples

Examples of direct names: 

```ada
Pi 	-- the direct name of a number 	(see 3.3.2)
Limit 	-- the direct name of a constant 	(see 3.3.1)
Count 	-- the direct name of a scalar variable 	(see 3.3.1)
Board 	-- the direct name of an array variable 	(see 3.6.1)
Matrix 	-- the direct name of a type 	(see 3.6)
Random 	-- the direct name of a function 	(see 6.1)
Error 	-- the direct name of an exception 	(see 11.1)

```

Examples of dereferences: 

```ada
Next_Car.all	--  explicit dereference denoting the object designated by
               	--  the access variable Next_Car (see 3.10.1)
Next_Car.Owner 	--  selected component with implicit dereference;
               	--  same as Next_Car.all.Owner

```


#### Extensions to Ada 83

Type conversions and function calls are now considered names that denote the result of the operation. In the case of a type conversion used as an actual parameter or that is of a tagged type, the type conversion is considered a variable if the operand is a variable. This simplifies the description of "parameters of the form of a type conversion" as well as better supporting an important OOP paradigm that requires the combination of a conversion from a class-wide type to some specific type followed immediately by component selection. Function calls are considered names so that a type conversion of a function call and the function call itself are treated equivalently in the grammar. A function call is considered the name of a constant, and can be used anywhere such a name is permitted. See 6.5.

Type conversions of a tagged type are permitted anywhere their operand is permitted. That is, if the operand is a variable, then the type conversion can appear on the left-hand side of an [assignment_statement](./AA-5.2#S0173). If the operand is an object, then the type conversion can appear in an object renaming or as a [prefix](./AA-4.1#S0093). See 4.6. 


#### Wording Changes from Ada 83

{AI95-00114-01} Everything of the general syntactic form [name](./AA-4.1#S0091)(...) is now syntactically a [name](./AA-4.1#S0091). In any realistic parser, this would be a necessity since distinguishing among the various [name](./AA-4.1#S0091)(...) constructs inevitably requires name resolution. In cases where the construct yields a value rather than an object, the name denotes a value rather than an object. Names already denote values in Ada 83 with named numbers, components of the result of a function call, etc. This is partly just a wording change, and partly an extension of functionality (see Extensions heading above).

The syntax rule for [direct_name](./AA-4.1#S0092) is new. It is used in places where direct visibility is required. It's kind of like Ada 83's simple_name, but simple_name applied to both direct visibility and visibility by selection, and furthermore, it didn't work right for [operator_symbol](./AA-6.1#S0202)s. The syntax rule for simple_name is removed, since its use is covered by a combination of [direct_name](./AA-4.1#S0092) and [selector_name](./AA-4.1#S0099). The syntactic categories [direct_name](./AA-4.1#S0092) and [selector_name](./AA-4.1#S0099) are similar; it's mainly the visibility rules that distinguish the two. The introduction of [direct_name](./AA-4.1#S0092) requires the insertion of one new explicit textual rule: to forbid [statement_identifier](./AA-5.1#S0172)s from being [operator_symbol](./AA-6.1#S0202)s. This is the only case where the explicit rule is needed, because this is the only case where the declaration of the entity is implicit. For example, there is no need to syntactically forbid (say) "X: "Rem";", because it is impossible to declare a type whose name is an [operator_symbol](./AA-6.1#S0202) in the first place.

The syntax rules for [explicit_dereference](./AA-4.1#S0094) and [implicit_dereference](./AA-4.1#S0095) are new; this makes other rules simpler, since dereferencing an access value has substantially different semantics from [selected_component](./AA-4.1#S0098)s. We also use [name](./AA-4.1#S0091) instead of [prefix](./AA-4.1#S0093) in the [explicit_dereference](./AA-4.1#S0094) rule since that seems clearer. Note that these rules rely on the fact that function calls are now names, so we don't need to use prefix to allow functions calls in front of .all. 

Discussion: Actually, it would be reasonable to allow any [primary](./AA-4.4#S0141) in front of .all, since only the value is needed, but that would be a bit radical. 

We no longer use the term appropriate for a type since we now describe the semantics of a prefix in terms of implicit dereference. 


#### Extensions to Ada 2005

{AI05-0003-1} A [qualified_expression](./AA-4.7#S0163) is now a [name](./AA-4.1#S0091) denoting a constant view; this allows them to be used as a prefix and to be renamed as an object. They are often used to remove ambiguity from function calls, and there may be no other way to do that. Interestingly, a [type_conversion](./AA-4.6#S0162) of a [qualified_expression](./AA-4.7#S0163) is already legal in these contexts, so this change mainly reduces clutter by eliminating an otherwise unneeded [type_conversion](./AA-4.6#S0162) from some expressions. 


#### Wording Changes from Ada 2005

{AI05-0008-1} Correction: Added a missing rule so that most dereferences are assumed constrained (without determining whether the designated object is). This is just confirming the Ada 95 rules; Ada 2005 failed to ensure that this property was unchanged.

{AI05-0139-2} {AI05-0299-1} Added [generalized_reference](./AA-4.1#S0104) and [generalized_indexing](./AA-4.1#S0105) as types of [name](./AA-4.1#S0091); these are documented as extensions in the appropriate subclauses. 


#### Wording Changes from Ada 2012

{AI12-0125-3} Added a [target_name](./AA-5.2#S0174) (see 5.2.1) to the syntax of [name](./AA-4.1#S0091). 


## 4.1.1  Indexed Components

[An [indexed_component](./AA-4.1#S0096) denotes either a component of an array or an entry in a family of entries. ]


#### Syntax

indexed_component<a id="S0096"></a> ::= [prefix](./AA-4.1#S0093)([expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)})


#### Name Resolution Rules

The [prefix](./AA-4.1#S0093) of an [indexed_component](./AA-4.1#S0096) with a given number of [expression](./AA-4.4#S0132)s shall resolve to denote an array (after any implicit dereference) with the corresponding number of index positions, or shall resolve to denote an entry family of a task or protected object (in which case there shall be only one [expression](./AA-4.4#S0132)).

The expected type for each [expression](./AA-4.4#S0132) is the corresponding index type.


#### Static Semantics

When the [prefix](./AA-4.1#S0093) denotes an array, the [indexed_component](./AA-4.1#S0096) denotes the component of the array with the specified index value(s). The nominal subtype of the [indexed_component](./AA-4.1#S0096) is the component subtype of the array type. 

When the [prefix](./AA-4.1#S0093) denotes an entry family, the [indexed_component](./AA-4.1#S0096) denotes the individual entry of the entry family with the specified index value.


#### Dynamic Semantics

For the evaluation of an [indexed_component](./AA-4.1#S0096), the [prefix](./AA-4.1#S0093) and the [expression](./AA-4.4#S0132)s are evaluated in an arbitrary order. The value of each [expression](./AA-4.4#S0132) is converted to the corresponding index type. A check is made that each index value belongs to the corresponding index range of the array or entry family denoted by the [prefix](./AA-4.1#S0093). Constraint_Error is raised if this check fails.


#### Examples

Examples of indexed components: 

```ada
 My_Schedule(Sat)     --  a component of a one-dimensional array 	(see 3.6.1)
 Page(10)             --  a component of a one-dimensional array 	(see 3.6)
 Board(M, J + 1)      --  a component of a two-dimensional array 	(see 3.6.1)
 Page(10)(20)         --  a component of a component 	(see 3.6)
 Request(Medium)      --  an entry in a family of entries 	(see 9.1)
 Next_Frame(L)(M, N)  --  a component of a function call 	(see 6.1)

```

NOTE 1   Notes on the examples: Distinct notations are used for components of multidimensional arrays (such as Board) and arrays of arrays (such as Page). The components of an array of arrays are arrays and can therefore be indexed. Thus Page(10)(20) denotes the 20th component of Page(10). In the last example Next_Frame(L) is a function call returning an access value that designates a two-dimensional array.


## 4.1.2  Slices

[ A [slice](./AA-4.1#S0097) denotes a one-dimensional array formed by a sequence of consecutive components of a one-dimensional array. A [slice](./AA-4.1#S0097) of a variable is a variable; a [slice](./AA-4.1#S0097) of a constant is a constant;] a [slice](./AA-4.1#S0097) of a value is a value. 


#### Syntax

slice<a id="S0097"></a> ::= [prefix](./AA-4.1#S0093)([discrete_range](./AA-3.6#S0058))


#### Name Resolution Rules

The [prefix](./AA-4.1#S0093) of a [slice](./AA-4.1#S0097) shall resolve to denote a one-dimensional array (after any implicit dereference).

The expected type for the [discrete_range](./AA-3.6#S0058) of a [slice](./AA-4.1#S0097) is the index type of the array type. 


#### Static Semantics

A [slice](./AA-4.1#S0097) denotes a one-dimensional array formed by the sequence of consecutive components of the array denoted by the [prefix](./AA-4.1#S0093), corresponding to the range of values of the index given by the [discrete_range](./AA-3.6#S0058).

The type of the [slice](./AA-4.1#S0097) is that of the [prefix](./AA-4.1#S0093). Its bounds are those defined by the [discrete_range](./AA-3.6#S0058).


#### Dynamic Semantics

For the evaluation of a [slice](./AA-4.1#S0097), the [prefix](./AA-4.1#S0093) and the [discrete_range](./AA-3.6#S0058) are evaluated in an arbitrary order. If the [slice](./AA-4.1#S0097) is not a null slice (a [slice](./AA-4.1#S0097) where the [discrete_range](./AA-3.6#S0058) is a null range), then a check is made that the bounds of the [discrete_range](./AA-3.6#S0058) belong to the index range of the array denoted by the [prefix](./AA-4.1#S0093). Constraint_Error is raised if this check fails.

NOTE 1   A [slice](./AA-4.1#S0097) is not permitted as the [prefix](./AA-4.1#S0093) of an Access [attribute_reference](./AA-4.1#S0100), even if the components or the array as a whole are aliased. See 3.10.2. 

Proof: Slices are not aliased, by 3.10, "Access Types". 

Reason: This is to ease implementation of general-access-to-array. If slices were aliased, implementations would need to store array dope with the access values, which is not always desirable given access-to-incomplete types completed in a package body. 

NOTE 2   For a one-dimensional array A, the [slice](./AA-4.1#S0097) A(N .. N) denotes an array that has only one component; its type is the type of A. On the other hand, A(N) denotes a component of the array A and has the corresponding component type. 


#### Examples

Examples of slices: 

```ada
  Stars(1 .. 15)        --  a slice of 15 characters 	(see 3.6.3)
  Page(10 .. 10 + Size) --  a slice of 1 + Size components 	(see 3.6)
  Page(L)(A .. B)       --  a slice of the array Page(L) 	(see 3.6)
  Stars(1 .. 0)         --  a null slice 	(see 3.6.3)
  My_Schedule(Weekday)  --  bounds given by subtype 	(see 3.6.1 and 3.5.1)
  Stars(5 .. 15)(K)     --  same as Stars(K) 	(see 3.6.3)
                        --  provided that K is in 5 .. 15

```


## 4.1.3  Selected Components

[[Selected_component](./AA-4.1#S0098)s are used to denote components (including discriminants), entries, entry families, and protected subprograms; they are also used as expanded names as described below. ]


#### Syntax

selected_component<a id="S0098"></a> ::= [prefix](./AA-4.1#S0093) . [selector_name](./AA-4.1#S0099)

selector_name<a id="S0099"></a> ::= [identifier](./AA-2.3#S0002) | [character_literal](./AA-2.5#S0015) | [operator_symbol](./AA-6.1#S0202)


#### Name Resolution Rules

A [selected_component](./AA-4.1#S0098) is called an expanded name if, according to the visibility rules, at least one possible interpretation of its [prefix](./AA-4.1#S0093) denotes a package or an enclosing named construct (directly, not through a [subprogram_renaming_declaration](./AA-8.5#S0242) or [generic_renaming_declaration](./AA-8.5#S0243)). 

Discussion: See AI83-00187. 

A [selected_component](./AA-4.1#S0098) that is not an expanded name shall resolve to denote one of the following: 

Ramification: If the [prefix](./AA-4.1#S0093) of a [selected_component](./AA-4.1#S0098) denotes an enclosing named construct, then the [selected_component](./AA-4.1#S0098) is interpreted only as an expanded name, even if the named construct is a function that could be called without parameters. 

A component [(including a discriminant)]:

The [prefix](./AA-4.1#S0093) shall resolve to denote an object or value of some non-array composite type (after any implicit dereference). The [selector_name](./AA-4.1#S0099) shall resolve to denote a [discriminant_specification](./AA-3.7#S0062) of the type, or, unless the type is a protected type, a [component_declaration](./AA-3.8#S0070) of the type. The [selected_component](./AA-4.1#S0098) denotes the corresponding component of the object or value. 

Reason: {AI05-0005-1} The components of a protected object cannot be named except by an expanded name, even from within the corresponding protected body. The protected body cannot reference the private components of some arbitrary object of the protected type; the protected body may reference components of the current instance only (by an expanded name or a [direct_name](./AA-4.1#S0092)). 

Ramification: Only the discriminants and components visible at the place of the [selected_component](./AA-4.1#S0098) can be selected, since a [selector_name](./AA-4.1#S0099) can only denote declarations that are visible (see 8.3). 

A single entry, an entry family, or a protected subprogram:

The [prefix](./AA-4.1#S0093) shall resolve to denote an object or value of some task or protected type (after any implicit dereference). The [selector_name](./AA-4.1#S0099) shall resolve to denote an [entry_declaration](./AA-9.5#S0257) or [subprogram_declaration](./AA-6.1#S0195) occurring (implicitly or explicitly) within the visible part of that type. The [selected_component](./AA-4.1#S0098) denotes the corresponding entry, entry family, or protected subprogram. 

Reason: This explicitly says "visible part" because even though the body has visibility on the private part, it cannot call the private operations of some arbitrary object of the task or protected type, only those of the current instance (and expanded name notation has to be used for that). 

{AI95-00252-01} {AI95-00407-01} A view of a subprogram whose first formal parameter is of a tagged type or is an access parameter whose designated type is tagged:

{AI95-00252-01} {AI95-00407-01} {AI05-0090-1} The [prefix](./AA-4.1#S0093) (after any implicit dereference) shall resolve to denote an object or value of a specific tagged type T or class-wide type T'Class. The [selector_name](./AA-4.1#S0099) shall resolve to denote a view of a subprogram declared immediately within the declarative region in which an ancestor of the type T is declared. The first formal parameter of the subprogram shall be of type T, or a class-wide type that covers T, or an access parameter designating one of these types. The designator of the subprogram shall not be the same as that of a component of the tagged type visible at the point of the [selected_component](./AA-4.1#S0098). The subprogram shall not be an implicitly declared primitive operation of type T that overrides an inherited subprogram implemented by an entry or protected subprogram visible at the point of the [selected_component](./AA-4.1#S0098). The [selected_component](./AA-4.1#S0098) denotes a view of this subprogram that omits the first formal parameter. This view is called a prefixed view of the subprogram, and the [prefix](./AA-4.1#S0093) of the [selected_component](./AA-4.1#S0098) (after any implicit dereference) is called the prefix of the prefixed view. 

Discussion: {AI05-0090-1} The part of the rule that excludes a primitive overriding subprogram as a selector applies only to the wrapper subprogram that is implicitly declared to override a subprogram inherited from a synchronized interface that is implemented by an operation of a task or protected type (see 9.1 and 9.4). We don't want calls that use a prefixed view to be ambiguous between the wrapper subprogram and the implementing entry or protected operation. Note that it is illegal to declare an explicit primitive that has a prefixed view that is homographic with one of the type's operations, so in normal cases it isn't possible to have an ambiguity in a prefix call. However, a class-wide operation of an ancestor type that is declared in the same declaration list with the ancestor type is also considered, and that can still make a call ambiguous. 

An expanded name shall resolve to denote a declaration that occurs immediately within a named declarative region, as follows: 

The [prefix](./AA-4.1#S0093) shall resolve to denote either a package [(including the current instance of a generic package, or a rename of a package)], or an enclosing named construct.

The [selector_name](./AA-4.1#S0099) shall resolve to denote a declaration that occurs immediately within the declarative region of the package or enclosing construct [(the declaration shall be visible at the place of the expanded name - see 8.3)]. The expanded name denotes that declaration. 

Ramification: Hence, a library unit or subunit can use an expanded name to refer to the declarations within the private part of its parent unit, as well as to other children that have been mentioned in [with_clause](./AA-10.1#S0294)s. 

If the [prefix](./AA-4.1#S0093) does not denote a package, then it shall be a [direct_name](./AA-4.1#S0092) or an expanded name, and it shall resolve to denote a program unit (other than a package), the current instance of a type, a [block_statement](./AA-5.6#S0191), a [loop_statement](./AA-5.5#S0178), or an [accept_statement](./AA-9.5#S0258) (in the case of an [accept_statement](./AA-9.5#S0258) or [entry_body](./AA-9.5#S0260), no family index is allowed); the expanded name shall occur within the declarative region of this construct. Further, if this construct is a callable construct and the [prefix](./AA-4.1#S0093) denotes more than one such enclosing callable construct, then the expanded name is ambiguous, independently of the [selector_name](./AA-4.1#S0099).


#### Legality Rules

{AI95-00252-01} {AI95-00407-01} {AI12-0204-1} {AI12-0427-1} For a prefixed view of a subprogram whose first formal parameter is an access parameter, the prefix shall be legal as the [prefix](./AA-4.1#S0093) of an [attribute_reference](./AA-4.1#S0100) with [attribute_designator](./AA-4.1#S0101) Access appearing as the first actual parameter in a call on the unprefixed view of the subprogram.

Ramification: {AI12-0204-1} This rule prevents, for instance, using a nonaliased prefix in such a prefixed view. It also prevents using discriminant-dependent components as the prefix of such a prefixed view if those components would not be allowed to be renamed. 

{AI95-00407-01} For a subprogram whose first parameter is of mode in out or out, or of an anonymous access-to-variable type, the prefix of any prefixed view shall denote a variable.

Reason: We want calls through a prefixed view and through a normal view to have the same legality. Thus, the implicit 'Access in this new notation needs the same legality check that an explicit 'Access would have. Similarly, we need to prohibit the object from being constant if the first parameter of the subprogram is in out, because that is (obviously) prohibited for passing a normal parameter. 


#### Dynamic Semantics

The evaluation of a [selected_component](./AA-4.1#S0098) includes the evaluation of the [prefix](./AA-4.1#S0093).

For a [selected_component](./AA-4.1#S0098) that denotes a component of a [variant](./AA-3.8#S0072), a check is made that the values of the discriminants are such that the value or object denoted by the [prefix](./AA-4.1#S0093) has this component. The exception Constraint_Error is raised if this check fails.


#### Examples

Examples of selected components: 

```ada
{AI95-00252-01} {AI95-00407-01} {AI12-0178}   Tomorrow.Month     --  a record component 	(see 3.8)
  Next_Car.Owner     --  a record component 	(see 3.10.1)
  Next_Car.Owner.Age --  a record component 	(see 3.10.1)
                     --  the previous two lines involve implicit dereferences
  Writer.Unit        --  a record component (a discriminant) 	(see 3.8.1)
  Min_Cell(H).Value  --  a record component of the result 	(see 6.1)
                     --  of the function call Min_Cell(H)
  Cashier.Append     --  a prefixed view of a procedure 	(see 3.9.4)
  Control.Seize      --  an entry of a protected object 	(see 9.4)
  Pool(K).Write      --  an entry of the task Pool(K) 	(see 9.1)

```

Examples of expanded names: 

```ada
  Key_Manager."&lt"      --  an operator of the visible part of a package 	(see 7.3.1)
  Dot_Product.Sum      --  a variable declared in a function body 	(see 6.1)
  Buffer.Pool          --  a variable declared in a protected unit 	(see 9.11)
  Buffer.Read          --  an entry of a protected unit 	(see 9.11)
  Swap.Temp            --  a variable declared in a block statement 	(see 5.6)
  Standard.Boolean     --  the name of a predefined type 	(see A.1)

```


#### Extensions to Ada 83

We now allow an expanded name to use a prefix that denotes a rename of a package, even if the selector is for an entity local to the body or private part of the package, so long as the entity is visible at the place of the reference. This eliminates a preexisting anomaly where references in a package body may refer to declarations of its visible part but not those of its private part or body when the prefix is a rename of the package. 


#### Wording Changes from Ada 83

The syntax rule for [selector_name](./AA-4.1#S0099) is new. It is used in places where visibility, but not necessarily direct visibility, is required. See 4.1, "Names" for more information.

The description of dereferencing an access type has been moved to 4.1, "Names"; [name](./AA-4.1#S0091).all is no longer considered a [selected_component](./AA-4.1#S0098).

The rules have been restated to be consistent with our new terminology, to accommodate class-wide types, etc. 


#### Extensions to Ada 95

{AI95-00252-01} The prefixed view notation for tagged objects is new. This provides a similar notation to that used in other popular languages, and also reduces the need for [use_clause](./AA-8.4#S0235)s. This is sometimes known as "distinguished receiver notation". 

Given the following definitions for a tagged type T: 

```ada
procedure Do_Something (Obj : in out T; Count : in Natural);
procedure Do_Something_Else (Obj : access T; Flag : in Boolean);
My_Object : aliased T;

```

the following calls are equivalent: 

```ada
Do_Something (My_Object, Count =&gt 10);
My_Object.Do_Something (Count =&gt 10);

```

as are the following calls: 

```ada
Do_Something_Else (My_Object'Access, Flag =&gt True);
My_Object.Do_Something_Else (Flag =&gt True);

```


#### Wording Changes from Ada 2005

{AI05-0090-1} Correction: Corrected the definition of a prefixed view to ignore the implicit subprograms declared for "implemented by" entries and protected subprograms. 


#### Incompatibilities With Ada 2012

{AI12-0204-1} Correction: Added a rule to ensure that all reasons that the prefix of an Access attribute can be illegal are covered by the rule for the implicit Access attribute of a prefixed view. If the object is a subcomponent that depends on discriminants or fails a static accessibility check, Ada 2012 would have allowed the prefix while Ada 2022 would not. This violated the principle that a prefixed view and a normal call have the same semantics; practically, the code may not have worked anyway if a compiler implemented generalized indexing by code expansion into the canonical form. Thus, such code wasn't practically portable. 


## 4.1.4  Attributes

[An attribute is a characteristic of an entity that can be queried via an [attribute_reference](./AA-4.1#S0100) or a [range_attribute_reference](./AA-4.1#S0102).] 

Glossary entry: An attribute is a characteristic or property of an entity that can be queried, and in some cases specified.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[attribute], Def=[a characteristic or property of an entity that can be queried, and in some cases specified] 


#### Syntax

{AI12-0262-1} attribute_reference<a id="S0100"></a> ::= 
    [prefix](./AA-4.1#S0093)'[attribute_designator](./AA-4.1#S0101)
  | [reduction_attribute_reference](./AA-4.5#S0158)

{AI05-0004-1} attribute_designator<a id="S0101"></a> ::= 
    [identifier](./AA-2.3#S0002)[(static_[expression](./AA-4.4#S0132))]
  | Access | Delta | Digits | Mod

range_attribute_reference<a id="S0102"></a> ::= [prefix](./AA-4.1#S0093)'[range_attribute_designator](./AA-4.1#S0103)

range_attribute_designator<a id="S0103"></a> ::= Range[(static_[expression](./AA-4.4#S0132))]


#### Name Resolution Rules

{AI12-0242-1} {AI12-0262-1} In an [attribute_reference](./AA-4.1#S0100) that is not a [reduction_attribute_reference](./AA-4.5#S0158), if the [attribute_designator](./AA-4.1#S0101) is for an attribute defined for (at least some) objects of an access type, then the [prefix](./AA-4.1#S0093) is never interpreted as an [implicit_dereference](./AA-4.1#S0095); otherwise (and for all [range_attribute_reference](./AA-4.1#S0102)s and [reduction_attribute_reference](./AA-4.5#S0158)s), if there is a [prefix](./AA-4.1#S0093) and the type of the [name](./AA-4.1#S0091) within the [prefix](./AA-4.1#S0093) is of an access type, the [prefix](./AA-4.1#S0093) is interpreted as an [implicit_dereference](./AA-4.1#S0095). Similarly, if the [attribute_designator](./AA-4.1#S0101) is for an attribute defined for (at least some) functions, then the [prefix](./AA-4.1#S0093) is never interpreted as a parameterless [function_call](./AA-6.4#S0218); otherwise (and for all [range_attribute_reference](./AA-4.1#S0102)s and [reduction_attribute_reference](./AA-4.5#S0158)s), if there is a [prefix](./AA-4.1#S0093) and the [prefix](./AA-4.1#S0093) consists of a [name](./AA-4.1#S0091) that denotes a function, it is interpreted as a parameterless [function_call](./AA-6.4#S0218).

Discussion: The first part of this rule is essentially a "preference" against implicit dereference, so that it is possible to ask for, say, 'Size of an access object, without automatically getting the size of the object designated by the access object. This rule applies to 'Access, 'Unchecked_Access, 'Size, and 'Address, and any other attributes that are defined for at least some access objects.

The second part of this rule implies that, for a parameterless function F, F'Address is the address of F, whereas F'Size is the size of the anonymous constant returned by F.

We normally talk in terms of expected type or profile for name resolution rules, but we don't do this for attributes because certain attributes are legal independent of the type or the profile of the [prefix](./AA-4.1#S0093).

{AI95-00114-01} Other than the rules given above, the Name Resolution Rules for the [prefix](./AA-4.1#S0093) of each attribute are defined as Name Resolution Rules for that attribute. If no such rules are defined, then no context at all should be used when resolving the [prefix](./AA-4.1#S0093). In particular, any knowledge about the kind of entities required must not be used for resolution unless that is required by Name Resolution Rules. This matters in obscure cases; for instance, given the following declarations: 

```ada
  function Get_It return Integer is ... -- (1)
  function Get_It return Some_Record_Type is ... -- (2)

```

the following [attribute_reference](./AA-4.1#S0100) cannot be resolved and is illegal: 

```ada
  if Get_It'Valid then

```

{AI05-0005-1} even though the Valid attribute is only defined for objects of scalar types, and thus cannot be applied to the result of function (2). That information cannot be used to resolve the [prefix](./AA-4.1#S0093). The same would be true if (2) had been a procedure; even though the procedure does not denote an object, the [attribute_reference](./AA-4.1#S0100) is still illegal. 

The [expression](./AA-4.4#S0132), if any, in an [attribute_designator](./AA-4.1#S0101) or [range_attribute_designator](./AA-4.1#S0103) is expected to be of any integer type. 


#### Legality Rules

The [expression](./AA-4.4#S0132), if any, in an [attribute_designator](./AA-4.1#S0101) or [range_attribute_designator](./AA-4.1#S0103) shall be static. 


#### Static Semantics

{AI05-0006-1} {AI12-0032-1} {AI12-0159-1} An [attribute_reference](./AA-4.1#S0100) denotes a value, an object, a subprogram, or some other kind of program entity. Unless explicitly specified otherwise, for an [attribute_reference](./AA-4.1#S0100) that denotes a value or an object, if its type is scalar, then its nominal subtype is the base subtype of the type; if its type is tagged, its nominal subtype is the first subtype of the type; otherwise, its nominal subtype is a subtype of the type without any constraint, [null_exclusion](./AA-3.10#S0083), or predicate. Similarly, unless explicitly specified otherwise, for an [attribute_reference](./AA-4.1#S0100) that denotes a function, when its result type is scalar, its result subtype is the base subtype of the type, when its result type is tagged, the result subtype is the first subtype of the type, and when the result type is some other type, the result subtype is a subtype of the type without any constraint, [null_exclusion](./AA-3.10#S0083), or predicate. 

Ramification: The attributes defined by the language are summarized in K.2. Implementations can define additional attributes. 

Discussion: {AI05-0006-1} The nominal subtype is primarily a concern when an [attribute_reference](./AA-4.1#S0100), or a call on an [attribute_reference](./AA-4.1#S0100), is used as the [expression](./AA-4.4#S0132) of a case statement, due to the full coverage requirement based on the nominal subtype. For nondiscrete cases, we define the nominal subtype mainly for completeness. Implementations may specify otherwise for implementation-defined attribute functions.

The rule is written to match the meaning of the italicized T in the definition of attributes such as Input; see 4.5.1. 

To be honest: {AI05-0006-1} We don't worry about the fact that "base subtype" is not explicitly defined for the universal types. Since it is not possible to constrain a universal numeric type, all subtypes are unconstrained, and hence can be considered base subtypes. The wording above could be altered to bypass this issue, but it doesn't seem necessary, since universal integer is handled specially in the rules for case expression full coverage, and we don't allow user-defined functions for attribute functions whose result type is universal. 

[A [range_attribute_reference](./AA-4.1#S0102) X'Range(N) is equivalent to the [range](./AA-3.5#S0037) X'First(N) .. X'Last(N), except that the [prefix](./AA-4.1#S0093) is only evaluated once. Similarly, X'Range is equivalent to X'First .. X'Last, except that the [prefix](./AA-4.1#S0093) is only evaluated once.]


#### Dynamic Semantics

{AI12-0262-1} The evaluation of a [range_attribute_reference](./AA-4.1#S0102) or an [attribute_reference](./AA-4.1#S0100) that is not a [reduction_attribute_reference](./AA-4.5#S0158) consists of the evaluation of the [prefix](./AA-4.1#S0093).[ The evaluation of a [reduction_attribute_reference](./AA-4.5#S0158) is defined in 4.5.10.] 


#### Implementation Permissions

{8652/0015} {AI95-00093-01} {AI12-0362-2} An implementation may provide implementation-defined attributes; the [identifier](./AA-2.3#S0002) for such an implementation-defined attribute shall differ from those of the language-defined attributes. 

Implementation defined: Implementation-defined attributes.

Ramification: They cannot be reserved words because reserved words are not legal identifiers.

The semantics of implementation-defined attributes, and any associated rules, are, of course, implementation defined. For example, the implementation defines whether a given implementation-defined attribute can be used in a static expression. 

{AI12-0362-2} An implementation may extend the definition of a language-defined attribute by accepting uses of that attribute that would otherwise be illegal in the following cases: 

in order to support compatibility with a previous edition of of this Reference Manual; or

Ramification: {8652/0015} {AI95-00093-01} Implementations are allowed to support the Small attribute for floating types, as this was defined in Ada 83, even though the name would conflict with a language-defined attribute. 

in the case of a language-defined attribute whose [prefix](./AA-4.1#S0093) is required by this document to be a floating point subtype, an implementation may accept an [attribute_reference](./AA-4.1#S0100) whose [prefix](./AA-4.1#S0093) is a fixed point subtype[; the semantics of such an [attribute_reference](./AA-4.1#S0100) are implementation defined.] 

NOTE 1   Attributes are defined throughout this document, and are summarized in K.2.

NOTE 2   {AI95-00235} In general, the [name](./AA-4.1#S0091) in a [prefix](./AA-4.1#S0093) of an [attribute_reference](./AA-4.1#S0100) (or a [range_attribute_reference](./AA-4.1#S0102)) has to be resolved without using any context. However, in the case of the Access attribute, the expected type for the [attribute_reference](./AA-4.1#S0100) has to be a single access type, and the resolution of the [name](./AA-4.1#S0091) can use the fact that the type of the object or the profile of the callable entity denoted by the [prefix](./AA-4.1#S0093) has to match the designated type or be type conformant with the designated profile of the access type. 

Proof: {AI95-00235} In the general case, there is no "expected type" for the [prefix](./AA-4.1#S0093) of an [attribute_reference](./AA-4.1#S0100). In the special case of 'Access, there is an "expected type" or "expected profile" for the [prefix](./AA-4.1#S0093). 

Reason: 'Access is a special case, because without it, it would be very difficult to take 'Access of an overloaded subprogram. 


#### Examples

Examples of attributes: 

```ada
Color'First        -- minimum value of the enumeration type Color 	(see 3.5.1)
Rainbow'Base'First -- same as Color'First 	(see 3.5.1)
Real'Digits        -- precision of the type Real 	(see 3.5.7)
Board'Last(2)      -- upper bound of the second dimension of Board 	(see 3.6.1)
Board'Range(1)     -- index range of the first dimension of Board 	(see 3.6.1)
Pool(K)'Terminated -- True if task Pool(K) is terminated 	(see 9.1)
Date'Size          -- number of bits for records of type Date 	(see 3.8)
Message'Address    -- address of the record variable Message 	(see 3.7.1)

```


#### Extensions to Ada 83

We now uniformly treat X'Range as X'First..X'Last, allowing its use with scalar subtypes.

We allow any integer type in the static_[expression](./AA-4.4#S0132) of an attribute designator, not just a value of universal_integer. The preference rules ensure upward compatibility. 


#### Wording Changes from Ada 83

We use the syntactic category [attribute_reference](./AA-4.1#S0100) rather than simply "attribute" to avoid confusing the name of something with the thing itself.

The syntax rule for [attribute_reference](./AA-4.1#S0100) now uses [identifier](./AA-2.3#S0002) instead of simple_name, because attribute [identifier](./AA-2.3#S0002)s are not required to follow the normal visibility rules.

We now separate [attribute_reference](./AA-4.1#S0100) from [range_attribute_reference](./AA-4.1#S0102), and enumerate the reserved words that are legal attribute or range attribute designators. We do this because [identifier](./AA-2.3#S0002) no longer includes reserved words.

The Ada 95 name resolution rules are a bit more explicit than in Ada 83. The Ada 83 rule said that the "meaning of the prefix of an attribute must be determinable independently of the attribute designator and independently of the fact that it is the prefix of an attribute".  That isn't quite right since the meaning even in Ada 83 embodies whether or not the prefix is interpreted as a parameterless function call, and in Ada 95, it also embodies whether or not the prefix is interpreted as an implicit_dereference. So the attribute designator does make a difference - just not much.

Note however that if the attribute designator is Access, it makes a big difference in the interpretation of the prefix (see 3.10.2). 


#### Wording Changes from Ada 95

{8652/0015} {AI95-00093-01} Corrigendum: The wording was changed to allow implementations to continue to implement the Ada 83 Small attribute. This was always intended to be allowed.

{AI95-00235-01} The note about resolving prefixes of attributes was updated to reflect that the prefix of an Access attribute now has an expected type (see 3.10.2). 


#### Wording Changes from Ada 2005

{AI05-0006-1} Correction: Defined the nominal subtype of an [attribute_reference](./AA-4.1#S0100) to close a minor language hole. 


#### Wording Changes from Ada 2012

{AI12-0032-1} Corrigendum: Allowed overriding the nominal subtype of an [attribute_reference](./AA-4.1#S0100) for an object; that is used elsewhere in this standard.

{AI12-0159-1} Corrigendum: Added wording so it is clear that predicates don't apply to the result of an attribute.

{AI12-0242-1} {AI12-0262-1} Added [reduction_attribute_reference](./AA-4.5#S0158) and cleaned up the rules here to avoid trampling the definition of those in 4.5.10.

{AI12-0362-2} Added a permission to support fixed point versions of float attributes, such as the rounding attributes found in A.5.3. 


## 4.1.5  User-Defined References


#### Static Semantics

{AI05-0139-2} Given a discriminated type T, the following type-related operational aspect may be specified:

Implicit_DereferenceThis aspect is specified by a [name](./AA-4.1#S0091) that denotes an access discriminant declared for the type T.

Aspect Description for Implicit_Dereference: Mechanism for user-defined implicit .all.

{AI05-0139-2} A (view of a) type with a specified Implicit_Dereference aspect is a reference type. A reference object is an object of a reference type. The discriminant named by the Implicit_Dereference aspect is the reference discriminant of the reference type or reference object. [A [generalized_reference](./AA-4.1#S0104) is a [name](./AA-4.1#S0091) that identifies a reference object, and denotes the object or subprogram designated by the reference discriminant of the reference object.]

Glossary entry: A reference type is one that has user-defined behavior for ".all", defined by the Implicit_Dereference aspect.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[reference type], Def=[a type that has user-defined behavior for ".all", defined by the Implicit_Dereference aspect] 


#### Syntax

{AI05-0139-2} generalized_reference<a id="S0104"></a> ::= reference_object_[name](./AA-4.1#S0091)


#### Name Resolution Rules

{AI05-0139-2} {AI05-0269-1} The expected type for the reference_object_[name](./AA-4.1#S0091) in a [generalized_reference](./AA-4.1#S0104) is any reference type. 


#### Static Semantics

{AI12-0138-1} The Implicit_Dereference aspect is nonoverridable (see 13.1.1).

Reason: This ensures that all descendants of a reference type have the same reference discriminant. This prevents generic contract problems with formal derived types. 

{AI05-0139-2} A [generalized_reference](./AA-4.1#S0104) denotes a view equivalent to that of a dereference of the reference discriminant of the reference object.

{AI05-0139-2} {AI12-0203-1} Given a reference type T, the Implicit_Dereference aspect is inherited by descendants of type T if not overridden [(which is only permitted if confirming)]. If a descendant type constrains the value of the reference discriminant of T by a new discriminant, that new discriminant is the reference discriminant of the descendant. [If the descendant type constrains the value of the reference discriminant of T by an [expression](./AA-4.4#S0132) other than the [name](./AA-4.1#S0091) of a new discriminant, a [generalized_reference](./AA-4.1#S0104) that identifies an object of the descendant type denotes the object or subprogram designated by the value of this constraining expression.]


#### Dynamic Semantics

{AI05-0139-2} The evaluation of a [generalized_reference](./AA-4.1#S0104) consists of the evaluation of the reference_object_[name](./AA-4.1#S0091) and a determination of the object or subprogram designated by the reference discriminant of the named reference object. A check is made that the value of the reference discriminant is not the null access value. Constraint_Error is raised if this check fails. The [generalized_reference](./AA-4.1#S0104) denotes the object or subprogram designated by the value of the reference discriminant of the named reference object.


#### Examples

{AI12-0429-1} Examples of the specification and use of generalized references:

```ada
{AI05-0268-1} type Barrel is tagged ...  -- holds objects of type Element

```

```ada
{AI05-0139-2} {AI05-0299-1} type Ref_Element(Data : access Element) is limited private
   with Implicit_Dereference =&gt Data;
      -- This Ref_Element type is a "reference" type.
      -- "Data" is its reference discriminant.

```

```ada
{AI05-0139-2} {AI05-0268-1} function Find (B : aliased in out Barrel; Key : String) return Ref_Element;
   -- Returns a reference to an element of a barrel.

```

```ada
{AI05-0268-1} {AI05-0299-1} B: aliased Barrel;

```

```ada
{AI05-0139-2} ...

```

```ada
{AI05-0139-2} {AI05-0268-1} Find (B, "grape") := Element'(...);  -- Assign through a reference.

```

```ada
{AI05-0139-2} {AI05-0268-1} -- This is equivalent to:
Find (B, "grape").Data.all := Element'(...);

```


#### Extensions to Ada 2005

{AI05-0139-2} The aspect Implicit_Dereference and the [generalized_reference](./AA-4.1#S0104) are new. 


#### Incompatibilities With Ada 2012

{AI12-0138-1} Corrigendum: Defined Implicit_Dereference to be nonoveridable, which makes redefinitions and hiding of the aspect illegal. It's possible that some program could violate one of these new restrictions, but this is not very likely as reference types are not likely to be used in a hierarchy. 


## 4.1.6  User-Defined Indexing


#### Static Semantics

{AI05-0139-2} Given a tagged type T, the following type-related, operational aspects may be specified:

{AI12-0428-1} Constant_IndexingThis aspect shall be specified by a [name](./AA-4.1#S0091) that denotes one or more functions declared immediately within the same declaration list in which T, or the declaration completed by T, is declared. All such functions shall have at least two parameters, the first of which is of type T or T'Class, or is an access-to-constant parameter with designated type T or T'Class.

Aspect Description for Constant_Indexing: Defines function(s) to implement user-defined [indexed_component](./AA-4.1#S0096)s.

{AI12-0428-1} Variable_IndexingThis aspect shall be specified by a [name](./AA-4.1#S0091) that denotes one or more functions declared immediately within the same declaration list in which T, or the declaration completed by T, is declared. All such functions shall have at least two parameters, the first of which is of type T or T'Class, or is an access parameter with designated type T or T'Class. All such functions shall have a return type that is a reference type (see 4.1.5), whose reference discriminant is of an access-to-variable type. 

Reason: We require these functions to return a reference type so that the object returned from the function can act like a variable. We need no similar rule for Constant_Indexing, since all functions return constant objects. 

Aspect Description for Variable_Indexing: Defines function(s) to implement user-defined [indexed_component](./AA-4.1#S0096)s.

{AI12-0104-1} These aspects are inherited by descendants of T (including the class-wide type T'Class).

Ramification: Indexing can be provided for multiple index types by overloading routines with different parameter profiles. For instance, the map containers provide indexing on both cursors and keys by providing pairs of overloaded routines to the Constant_Indexing and Variable_Indexing aspects. 

{AI05-0139-2} {AI05-0292-1} An indexable container type is (a view of) a tagged type with at least one of the aspects Constant_Indexing or Variable_Indexing specified. An indexable container object is an object of an indexable container type. [A [generalized_indexing](./AA-4.1#S0105) is a [name](./AA-4.1#S0091) that denotes the result of calling a function named by a Constant_Indexing or Variable_Indexing aspect.]

Glossary entry: An indexable container type is one that has user-defined behavior for indexing, via the Constant_Indexing or Variable_Indexing aspects.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[indexable container type], Def=[a type that has user-defined behavior for indexing, via the Constant_Indexing or Variable_Indexing aspects]

{AI12-0138-1} The Constant_Indexing and Variable_Indexing aspects are nonoverridable (see 13.1.1). 

Reason: {AI12-0160-1} This (and the following Legality Rules) ensures that all descendants of an indexable container type have aspects with the same properties. This prevents generic contract problems with formal derived types.

{AI12-0104-1} {AI12-0138-1} A nonoverridable aspect allows the replacement of the implementation of an indexing function and the addition of a new indexing function for a derived type, but not the removal of an indexing function. This is necessary so that indexing can be used on objects of T'Class. So long as the tag of O is that of its nominal subtype, we do not want T'Class(O)(I) to mean something different than O(I). Thus we cannot allow a change in the function identified. As T'Class(O)(I) expands into a dispatching call, we need to ensure that there is a body for each such function -- but it is OK for that body to be changed from the original body (that's just normal dispatching). 


#### Legality Rules

{AI12-0160-1} If an ancestor of a type T is an indexable container type, then any explicit specification of the Constant_Indexing or Variable_Indexing aspects shall be confirming; that is, the specified [name](./AA-4.1#S0091) shall match the inherited aspect (see 13.1.1). 

Paragraphs 7 through 8 were deleted. 

{AI12-0160-1} In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit.

{AI12-0204-1} A [generalized_indexing](./AA-4.1#S0105) is illegal if the equivalent prefixed view (see below) is illegal. 


#### Syntax

{AI05-0139-2} {AI05-0292-1} generalized_indexing<a id="S0105"></a> ::= indexable_container_object_[prefix](./AA-4.1#S0093) [actual_parameter_part](./AA-6.4#S0219)


#### Name Resolution Rules

{AI05-0139-2} {AI05-0292-1} The expected type for the indexable_container_object_[prefix](./AA-4.1#S0093) of a [generalized_indexing](./AA-4.1#S0105) is any indexable container type.

Ramification: A [prefix](./AA-4.1#S0093) can be an [implicit_dereference](./AA-4.1#S0095) (see 4.1), so an access-to-indexable_container_object can be the prefix (English meaning!) of a [generalized_indexing](./AA-4.1#S0105). 

{AI05-0139-2} {AI05-0292-1} If the Constant_Indexing aspect is specified for the type of the indexable_container_object_[prefix](./AA-4.1#S0093) of a [generalized_indexing](./AA-4.1#S0105), then the [generalized_indexing](./AA-4.1#S0105) is interpreted as a constant indexing under the following circumstances:

when the Variable_Indexing aspect is not specified for the type of the indexable_container_object_[prefix](./AA-4.1#S0093);

when the indexable_container_object_[prefix](./AA-4.1#S0093) denotes a constant;

when the [generalized_indexing](./AA-4.1#S0105) is used within a [primary](./AA-4.4#S0141) where a [name](./AA-4.1#S0091) denoting a constant is permitted.

Ramification: This means it is not interpreted as a constant indexing for the variable_[name](./AA-4.1#S0091) in the LHS of an assignment (not inside a [primary](./AA-4.4#S0141)), nor for the [name](./AA-4.1#S0091) used for an out or in out parameter (not allowed to be a constant), nor for the [name](./AA-4.1#S0091) in an object renaming (not inside a primary), unless there is no Variable_Indexing aspect defined. 

Otherwise, the [generalized_indexing](./AA-4.1#S0105) is interpreted as a variable indexing.

When a [generalized_indexing](./AA-4.1#S0105) is interpreted as a constant (or variable) indexing, it is equivalent to a call on a prefixed view of one of the functions named by the Constant_Indexing (or Variable_Indexing) aspect of the type of the indexable_container_object_[prefix](./AA-4.1#S0093) with the given [actual_parameter_part](./AA-6.4#S0219), and with the indexable_container_object_[prefix](./AA-4.1#S0093) as the [prefix](./AA-4.1#S0093) of the prefixed view.

Ramification: In other words, the [generalized_indexing](./AA-4.1#S0105) is equivalent to:

```ada
indexable_container_object_[prefix](./AA-4.1#S0093).Indexing [actual_parameter_part](./AA-6.4#S0219)

```

{AI12-0005-1} where Indexing is the [name](./AA-4.1#S0091) specified for the Constant_Indexing or Variable_Indexing aspect. This equivalence is then resolved in the normal way; the aspect specifies a [name](./AA-4.1#S0091), it does not denote declarations. 

NOTE 1   {AI12-0104-1} The Constant_Indexing and Variable_Indexing aspects cannot be redefined when inherited for a derived type, but the functions that they denote can be modified by overriding or overloading. 


#### Examples

{AI12-0429-1} Examples of the specification and use of generalized indexing:

```ada
{AI05-0268-1} {AI05-0292-1} type Indexed_Barrel is tagged ...
  with Variable_Indexing =&gt Find;
  -- Indexed_Barrel is an indexable container type,
  -- Find is the generalized indexing operation.

```

```ada
{AI05-0268-1} function Find (B : aliased in out Indexed_Barrel; Key : String) return Ref_Element;
   -- Return a reference to an element of a barrel (see 4.1.5).

```

```ada
{AI05-0268-1} IB: aliased Indexed_Barrel;

```

```ada
{AI05-0268-1} -- All of the following calls are then equivalent:
Find (IB,"pear").Data.all := Element'(...); -- Traditional call
IB.Find ("pear").Data.all := Element'(...); -- Call of prefixed view
IB.Find ("pear")          := Element'(...); -- Implicit dereference (see 4.1.5)
IB      ("pear")          := Element'(...); -- Implicit indexing and dereference
IB      ("pear").Data.all := Element'(...); -- Implicit indexing only

```


#### Extensions to Ada 2005

{AI05-0139-2} Aspects Constant_Indexing and Variable_Indexing, and the [generalized_indexing](./AA-4.1#S0105) syntax are new. 


#### Incompatibilities With Ada 2012

{AI12-0160-1} Correction: Prevented a derived type from specifying Constant_Indexing if the ancestor specified Variable_Indexing (and vice versa). This is necessary to preserve the intent that for an object Obj whose tag is that of its nominal subtype, T'Class(Obj)(I) always has the same meaning as Obj(I). Situations like this should be rare in practice; most types will either define both aspects or neither.

{AI12-0204-1} Correction: Added a rule that a generalized indexing is illegal if the equivalent prefixed view would be illegal. If the prefixed view would be illegal for any reason, Ada 2012 would have allowed the generalized indexing while Ada 2022 does not. This violated the principle that a generalized indexing and the equivalent prefixed view have the same semantics; practically, the code may not have worked anyway if a compiler implemented generalized indexing by code expansion into the canonical form. Thus, such code wasn't practically portable. 


#### Wording Changes from Ada 2012

{AI12-0104-1} Corrigendum: Converted confusing and unnecessary normative wording about "overriding an aspect" into a note.

{AI12-0138-1} Corrigendum: Defined Constant_Indexing and Variable_Indexing to be nonoveridable. This is merely a new description for Legality Rules which already applied to these aspects.

{AI12-0428-1} Correction: Allowed the completion of a private type to use declarations from either the visible part or the private part. 

