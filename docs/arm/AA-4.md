---
sidebar_position:  5
---

# 4 Names and Expressions

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[The rules applicable to the different forms of name and expression, and to their evaluation, are given in this section.] 


## 4.1  Names

[Names can denote declared entities, whether declared explicitly or implicitly (see 3.1). Names can also denote objects or subprograms designated by access values; the results of type_conversions or function_calls; subcomponents and slices of objects and values; protected subprograms, single entries, entry families, and entries in families of entries. Finally, names can denote attributes of any of the foregoing.] 


#### Syntax

name ::= 
     direct_name	| explicit_dereference
   | indexed_component	| slice
   | selected_component	| attribute_reference
   | type_conversion	| function_call
   | character_literal

direct_name ::= identifier | operator_symbol

Discussion: character_literal is no longer a direct_name. character_literals are usable even when the corresponding enumeration_type_declaration is not visible. See 4.2. 

prefix ::= name | implicit_dereference

explicit_dereference ::= name.all

implicit_dereference ::= name

[Certain forms of name (indexed_components, selected_components, slices, and attributes) include a prefix that is either itself a name that denotes some related entity, or an implicit_dereference of an access value that designates some related entity.] 


#### Name Resolution Rules

The name in a dereference (either an implicit_dereference or an explicit_dereference) is expected to be of any access type. 


#### Static Semantics

If the type of the name in a dereference is some access-to-object type T, then the dereference denotes a view of an object, the nominal subtype of the view being the designated subtype of T. 

Ramification: If the value of the name is the result of an access type conversion, the dereference denotes a view created as part of the conversion. The nominal subtype of the view is not necessarily the same as that used to create the designated object. See 4.6. 

To be honest: We sometimes refer to the nominal subtype of a particular kind of name rather than the nominal subtype of the view denoted by the name (presuming the name denotes a view of an object). These two uses of nominal subtype are intended to mean the same thing. 

If the type of the name in a dereference is some access-to-subprogram type S, then the dereference denotes a view of a subprogram, the profile of the view being the designated profile of S. 

Ramification: This means that the formal parameter names and default expressions to be used in a call whose name or prefix is a dereference are those of the designated profile, which need not be the same as those of the subprogram designated by the access value, since 'Access requires only subtype conformance, not full conformance. 


#### Dynamic Semantics

The evaluation of a name determines the entity denoted by the name. This evaluation has no other effect for a name that is a direct_name or a character_literal.

[The evaluation of a name that has a prefix includes the evaluation of the prefix.] The evaluation of a prefix consists of the evaluation of the name or the implicit_dereference. The prefix denotes the entity denoted by the name or the implicit_dereference.

The evaluation of a dereference consists of the evaluation of the name and the determination of the object or subprogram that is designated by the value of the name. A check is made that the value of the name is not the null access value. Constraint_Error is raised if this check fails. The dereference denotes the object or subprogram designated by the value of the name. 


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

Type conversions of a tagged type are permitted anywhere their operand is permitted. That is, if the operand is a variable, then the type conversion can appear on the left-hand side of an assignment_statement. If the operand is an object, then the type conversion can appear in an object renaming or as a prefix. See 4.6. 


#### Wording Changes from Ada 83

Everything of the general syntactic form name(...) is now syntactically a name. In any realistic parser, this would be a necessity since distinguishing among the various name(...) constructs inevitably requires name resolution. In cases where the construct yields a value rather than an object, the name denotes the value rather than an object. Names already denote values in Ada 83 with named numbers, components of the result of a function call, etc. This is partly just a wording change, and partly an extension of functionality (see Extensions heading above).

The syntax rule for direct_name is new. It is used in places where direct visibility is required. It's kind of like Ada 83's simple_name, but simple_name applied to both direct visibility and visibility by selection, and furthermore, it didn't work right for operator_symbols. The syntax rule for simple_name is removed, since its use is covered by a combination of direct_name and selector_name. The syntactic categories direct_name and selector_name are similar; it's mainly the visibility rules that distinguish the two. The introduction of direct_name requires the insertion of one new explicit textual rule: to forbid statement_identifiers from being operator_symbols. This is the only case where the explicit rule is needed, because this is the only case where the declaration of the entity is implicit. For example, there is no need to syntactically forbid (say) "X: "Rem";", because it is impossible to declare a type whose name is an operator_symbol in the first place.

The syntax rules for explicit_dereference and implicit_dereference are new; this makes other rules simpler, since dereferencing an access value has substantially different semantics from selected_components. We also use name instead of prefix in the explicit_dereference rule since that seems clearer. Note that these rules rely on the fact that function calls are now names, so we don't need to use prefix to allow functions calls in front of .all. 

Discussion: Actually, it would be reasonable to allow any primary in front of .all, since only the value is needed, but that would be a bit radical. 

We no longer use the term appropriate for a type since we now describe the semantics of a prefix in terms of implicit dereference. 


### 4.1.1  Indexed Components

[An indexed_component denotes either a component of an array or an entry in a family of entries. ]


#### Syntax

indexed_component ::= prefix(expression {, expression})


#### Name Resolution Rules

The prefix of an indexed_component with a given number of expressions shall resolve to denote an array (after any implicit dereference) with the corresponding number of index positions, or shall resolve to denote an entry family of a task or protected object (in which case there shall be only one expression).

The expected type for each expression is the corresponding index type.


#### Static Semantics

When the prefix denotes an array, the indexed_component denotes the component of the array with the specified index value(s). The nominal subtype of the indexed_component is the component subtype of the array type. 

Ramification: In the case of an array whose components are aliased, and of an unconstrained discriminated subtype, the components are constrained even though their nominal subtype is unconstrained. (This is because all aliased discriminated objects are constrained. See 3.10.2.) In all other cases, an array component is constrained if and only if its nominal subtype is constrained. 

When the prefix denotes an entry family, the indexed_component denotes the individual entry of the entry family with the specified index value.


#### Dynamic Semantics

For the evaluation of an indexed_component, the prefix and the expressions are evaluated in an arbitrary order. The value of each expression is converted to the corresponding index type. A check is made that each index value belongs to the corresponding index range of the array or entry family denoted by the prefix. Constraint_Error is raised if this check fails.


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


### 4.1.2  Slices

[ A slice denotes a one-dimensional array formed by a sequence of consecutive components of a one-dimensional array. A slice of a variable is a variable; a slice of a constant is a constant;] a slice of a value is a value. 


#### Syntax

slice ::= prefix(discrete_range)


#### Name Resolution Rules

The prefix of a slice shall resolve to denote a one-dimensional array (after any implicit dereference).

The expected type for the discrete_range of a slice is the index type of the array type. 


#### Static Semantics

A slice denotes a one-dimensional array formed by the sequence of consecutive components of the array denoted by the prefix, corresponding to the range of values of the index given by the discrete_range.

The type of the slice is that of the prefix. Its bounds are those defined by the discrete_range.


#### Dynamic Semantics

For the evaluation of a slice, the prefix and the discrete_range are evaluated in an arbitrary order. If the slice is not a null slice (a slice where the discrete_range is a null range), then a check is made that the bounds of the discrete_range belong to the index range of the array denoted by the prefix. Constraint_Error is raised if this check fails.

NOTE 1   A slice is not permitted as the prefix of an Access attribute_reference, even if the components or the array as a whole are aliased. See 3.10.2. 

Proof: Slices are not aliased, by 3.10, "Access Types". 

Reason: This is to ease implementation of general-access-to-array. If slices were aliased, implementations would need to store array dope with the access values, which is not always desirable given access-to-incomplete types completed in a package body. 

NOTE 2   For a one-dimensional array A, the slice A(N .. N) denotes an array that has only one component; its type is the type of A. On the other hand, A(N) denotes a component of the array A and has the corresponding component type. 


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


### 4.1.3  Selected Components

[Selected_components are used to denote components (including discriminants), entries, entry families, and protected subprograms; they are also used as expanded names as described below. ]


#### Syntax

selected_component ::= prefix . selector_name

selector_name ::= identifier | character_literal | operator_symbol


#### Name Resolution Rules

A selected_component is called an expanded name if, according to the visibility rules, at least one possible interpretation of its prefix denotes a package or an enclosing named construct (directly, not through a subprogram_renaming_declaration or generic_renaming_declaration). 

Discussion: See AI83-00187. 

A selected_component that is not an expanded name shall resolve to denote one of the following: 

Ramification: If the prefix of a selected_component denotes an enclosing named construct, then the selected_component is interpreted only as an expanded name, even if the named construct is a function that could be called without parameters. 

A component [(including a discriminant)]:

The prefix shall resolve to denote an object or value of some non-array composite type (after any implicit dereference). The selector_name shall resolve to denote a discriminant_specification of the type, or, unless the type is a protected type, a component_declaration of the type. The selected_component denotes the corresponding component of the object or value. 

Reason: The components of a protected object cannot be named except by an expanded name, even from within the corresponding protected body. The protected body may not reference the the private components of some arbitrary object of the protected type; the protected body may reference components of the current instance only (by an expanded name or a direct_name). 

Ramification: Only the discriminants and components visible at the place of the selected_component can be selected, since a selector_name can only denote declarations that are visible (see 8.3). 

A single entry, an entry family, or a protected subprogram:

The prefix shall resolve to denote an object or value of some task or protected type (after any implicit dereference). The selector_name shall resolve to denote an entry_declaration or subprogram_declaration occurring (implicitly or explicitly) within the visible part of that type. The selected_component denotes the corresponding entry, entry family, or protected subprogram. 

Reason: This explicitly says "visible part" because even though the body has visibility on the private part, it cannot call the private operations of some arbitrary object of the task or protected type, only those of the current instance (and expanded name notation has to be used for that). 

An expanded name shall resolve to denote a declaration that occurs immediately within a named declarative region, as follows: 

The prefix shall resolve to denote either a package [(including the current instance of a generic package, or a rename of a package)], or an enclosing named construct.

The selector_name shall resolve to denote a declaration that occurs immediately within the declarative region of the package or enclosing construct [(the declaration shall be visible at the place of the expanded name - see 8.3)]. The expanded name denotes that declaration. 

Ramification: Hence, a library unit or subunit can use an expanded name to refer to the declarations within the private part of its parent unit, as well as to other children that have been mentioned in with_clauses. 

If the prefix does not denote a package, then it shall be a direct_name or an expanded name, and it shall resolve to denote a program unit (other than a package), the current instance of a type, a block_statement, a loop_statement, or an accept_statement (in the case of an accept_statement or entry_body, no family index is allowed); the expanded name shall occur within the declarative region of this construct. Further, if this construct is a callable construct and the prefix denotes more than one such enclosing callable construct, then the expanded name is ambiguous, independently of the selector_name.


#### Dynamic Semantics

The evaluation of a selected_component includes the evaluation of the prefix.

For a selected_component that denotes a component of a variant, a check is made that the values of the discriminants are such that the value or object denoted by the prefix has this component. The exception Constraint_Error is raised if this check fails.


#### Examples

Examples of selected components: 

```ada
  Tomorrow.Month     --  a record component 	(see 3.8)
  Next_Car.Owner     --  a record component 	(see 3.10.1)
  Next_Car.Owner.Age --  a record component 	(see 3.10.1)
                     --  the previous two lines involve implicit dereferences
  Writer.Unit        --  a record component (a discriminant) 	(see 3.8.1)
  Min_Cell(H).Value  --  a record component of the result 	(see 6.1)
                     --  of the function call Min_Cell(H)
  Control.Seize      --  an entry of a protected object 	(see 9.4)
  Pool(K).Write      --  an entry of the task Pool(K) 	(see 9.4)

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

The syntax rule for selector_name is new. It is used in places where visibility, but not necessarily direct visibility, is required. See 4.1, "Names" for more information.

The description of dereferencing an access type has been moved to 4.1, "Names"; name.all is no longer considered a selected_component.

The rules have been restated to be consistent with our new terminology, to accommodate class-wide types, etc. 


### 4.1.4  Attributes

[An attribute is a characteristic of an entity that can be queried via an attribute_reference or a range_attribute_reference.] Version=[5],Kind=(AddedNormal),Group=[T],Term=[attribute], Def=[a characteristic or property of an entity that can be queried, and in some cases specified] 


#### Syntax

attribute_reference ::= prefix'attribute_designator

attribute_designator ::= 
    identifier[(static_expression)]
  | Access | Delta | Digits

range_attribute_reference ::= prefix'range_attribute_designator

range_attribute_designator ::= Range[(static_expression)]


#### Name Resolution Rules

In an attribute_reference, if the attribute_designator is for an attribute defined for (at least some) objects of an access type, then the prefix is never interpreted as an implicit_dereference; otherwise (and for all range_attribute_references), if the type of the name within the prefix is of an access type, the prefix is interpreted as an implicit_dereference. Similarly, if the attribute_designator is for an attribute defined for (at least some) functions, then the prefix is never interpreted as a parameterless function_call; otherwise (and for all range_attribute_references), if the prefix consists of a name that denotes a function, it is interpreted as a parameterless function_call.

Discussion: The first part of this rule is essentially a "preference" against implicit dereference, so that it is possible to ask for, say, 'Size of an access object, without automatically getting the size of the object designated by the access object. This rule applies to 'Access, 'Unchecked_Access, 'Size, and 'Address, and any other attributes that are defined for at least some access objects.

The second part of this rule implies that, for a parameterless function F, F'Address is the address of F, whereas F'Size is the size of the anonymous constant returned by F.

We normally talk in terms of expected type or profile for name resolution rules, but we don't do this for attributes because certain attributes are legal independent of the type or the profile of the prefix.

The expression, if any, in an attribute_designator or range_attribute_designator is expected to be of any integer type. 


#### Legality Rules

The expression, if any, in an attribute_designator or range_attribute_designator shall be static. 


#### Static Semantics

An attribute_reference denotes a value, an object, a subprogram, or some other kind of program entity. 

Ramification: The attributes defined by the language are summarized in . Implementations can define additional attributes. 

[A range_attribute_reference X'Range(N) is equivalent to the range X'First(N) .. X'Last(N), except that the prefix is only evaluated once. Similarly, X'Range is equivalent to X'First .. X'Last, except that the prefix is only evaluated once.]


#### Dynamic Semantics

The evaluation of an attribute_reference (or range_attribute_reference) consists of the evaluation of the prefix.[] 


#### Implementation Permissions

An implementation may provide implementation-defined attributes; the identifier for an implementation-defined attribute shall differ from those of the language-defined attributes. 

Implementation defined: Implementation-defined attributes.

Ramification: They cannot be reserved words because reserved words are not legal identifiers.

The semantics of implementation-defined attributes, and any associated rules, are, of course, implementation defined. For example, the implementation defines whether a given implementation-defined attribute can be used in a static expression. 

NOTE 1   Attributes are defined throughout this document, and are summarized in .

NOTE 2   In general, the name in a prefix of an attribute_reference (or a range_attribute_reference) has to be resolved without using any context. However, in the case of the Access attribute, the expected type for the prefix has to be a single access type, and if it is an access-to-subprogram type (see 3.10.2) then the resolution of the name can use the fact that the profile of the callable entity denoted by the prefix has to be type conformant with the designated profile of the access type. 

Proof: In the general case, there is no "expected type" for the prefix of an attribute_reference. In the special case of 'Access, there is an "expected profile" for the prefix. 

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

We allow any integer type in the static_expression of an attribute designator, not just a value of universal_integer. The preference rules ensure upward compatibility. 


#### Wording Changes from Ada 83

We use the syntactic category attribute_reference rather than simply "attribute" to avoid confusing the name of something with the thing itself.

The syntax rule for attribute_reference now uses identifier instead of simple_name, because attribute identifiers are not required to follow the normal visibility rules.

We now separate attribute_reference from range_attribute_reference, and enumerate the reserved words that are legal attribute or range attribute designators. We do this because identifier no longer includes reserved words.

The Ada 95 name resolution rules are a bit more explicit than in Ada 83. The Ada 83 rule said that the "meaning of the prefix of an attribute must be determinable independently of the attribute designator and independently of the fact that it is the prefix of an attribute".  That isn't quite right since the meaning even in Ada 83 embodies whether or not the prefix is interpreted as a parameterless function call, and in Ada 95, it also embodies whether or not the prefix is interpreted as an implicit_dereference. So the attribute designator does make a difference - just not much.

Note however that if the attribute designator is Access, it makes a big difference in the interpretation of the prefix (see 3.10.2). 


#### Static Semantics

Version=[5],Kind=(AddedNormal),Group=[T],Term=[reference type], Def=[a type that has user-defined behavior for ".all", defined by the Implicit_Dereference aspect] 


#### Static Semantics

Version=[5],Kind=(AddedNormal),Group=[T],Term=[indexable container type], Def=[a type that has user-defined behavior for indexing, via the Constant_Indexing or Variable_Indexing aspects]


## 4.2  Literals

[ A literal represents a value literally, that is, by means of notation suited to its kind.] A literal is either a numeric_literal, a character_literal, the literal null, or a string_literal. 

Discussion: An enumeration literal that is an identifier rather than a character_literal is not considered a literal in the above sense, because it involves no special notation "suited to its kind". It might more properly be called an enumeration_identifier, except for historical reasons. 


#### Name Resolution Rules

The expected type for a literal null shall be a single access type. 

Discussion: This new wording ("expected type ... shall be a single ... type") replaces the old "shall be determinable" stuff. It reflects an attempt to simplify and unify the description of the rules for resolving aggregates, literals, type conversions, etc. See 8.6, "The Context of Overload Resolution" for the details. 

For a name that consists of a character_literal, either its expected type shall be a single character type, in which case it is interpreted as a parameterless function_call that yields the corresponding value of the character type, or its expected profile shall correspond to a parameterless function with a character result type, in which case it is interpreted as the name of the corresponding parameterless function declared as part of the character type's definition (see 3.5.1). In either case, the character_literal denotes the enumeration_literal_specification. 

Discussion: See 4.1.3 for the resolution rules for a selector_name that is a character_literal. 

The expected type for a primary that is a string_literal shall be a single string type. 


#### Legality Rules

A character_literal that is a name shall correspond to a defining_character_literal of the expected type, or of the result type of the expected profile.

For each character of a string_literal with a given expected string type, there shall be a corresponding defining_character_literal of the component type of the expected string type.

A literal null shall not be of an anonymous access type[, since such types do not have a null value (see 3.10)]. 

Reason: This is a legality rule rather than an overloading rule, to simplify implementations. 


#### Static Semantics

An integer literal is of type universal_integer. A real literal is of type universal_real. 


#### Dynamic Semantics

The evaluation of a numeric literal, or the literal null, yields the represented value.

The evaluation of a string_literal that is a primary yields an array value containing the value of each character of the sequence of characters of the string_literal, as defined in 2.6. The bounds of this array value are determined according to the rules for positional_array_aggregates (see 4.3.3), except that for a null string literal, the upper bound is the predecessor of the lower bound.

For the evaluation of a string_literal of type T, a check is made that the value of each character of the string_literal belongs to the component subtype of T. For the evaluation of a null string literal, a check is made that its lower bound is greater than the lower bound of the base range of the index type. The exception Constraint_Error is raised if either of these checks fails. 

Ramification: The checks on the characters need not involve more than two checks altogether, since one need only check the characters of the string with the lowest and highest position numbers against the range of the component subtype. 

NOTE 1   Enumeration literals that are identifiers rather than character_literals follow the normal rules for identifiers when used in a name (see 4.1 and 4.1.3). Character_literals used as selector_names follow the normal rules for expanded names (see 4.1.3). 


#### Examples

Examples of literals: 

```ada
3.14159_26536 	--  a real literal
1_345 	--  an integer literal
'A' 	--  a character literal
"Some Text" 	--  a string literal 

```


#### Incompatibilities With Ada 83

Because character_literals are now treated like other literals, in that they are resolved using context rather than depending on direct visibility, additional qualification might be necessary when passing a character_literal to an overloaded subprogram. 


#### Extensions to Ada 83

Character_literals are now treated analogously to null and string_literals, in that they are resolved using context, rather than their content; the declaration of the corresponding defining_character_literal need not be directly visible. 


#### Wording Changes from Ada 83

Name Resolution rules for enumeration literals that are not character_literals are not included anymore, since they are neither syntactically nor semantically "literals" but are rather names of parameterless functions. 


## 4.3  Aggregates

[ An aggregate combines component values into a composite value of an array type, record type, or record extension.] 

Version=[5],Kind=(AddedNormal),Group=[C],Term=[aggregate], Def=[a construct used to define a value of a composite type by specifying the values of the components of the type]


#### Syntax

aggregate ::= record_aggregate | extension_aggregate | array_aggregate


#### Name Resolution Rules

The expected type for an aggregate shall be a single nonlimited array type, record type, or record extension. 

Discussion: See 8.6, "The Context of Overload Resolution" for the meaning of "shall be a single ... type". 


#### Legality Rules

An aggregate shall not be of a class-wide type. 

Ramification: When the expected type in some context is class-wide, an aggregate has to be explicitly qualified by the specific type of value to be created, so that the expected type for the aggregate itself is specific. 

Discussion: We used to disallow aggregates of a type with unknown discriminants. However, that was unnecessarily restrictive in the case of an extension aggregate, and irrelevant to a record aggregate (since a type that is legal for a record aggregate could not possibly have unknown discriminants) and to an array aggregate (the only specific types that can have unknown discriminants are private types, private extensions, and types derived from them). 


#### Dynamic Semantics

For the evaluation of an aggregate, an anonymous object is created and values for the components or ancestor part are obtained (as described in the subsequent subclause for each kind of the aggregate) and assigned into the corresponding components or ancestor part of the anonymous object. Obtaining the values and the assignments occur in an arbitrary order. The value of the aggregate is the value of this object. 

Discussion: The ancestor part is the set of components inherited from the ancestor type. The syntactic category ancestor_part is the expression or subtype_mark that specifies how the ancestor part of the anonymous object should be initialized. 

Ramification: The assignment operations do the necessary value adjustment, as described in 7.6. Note that the value as a whole is not adjusted - just the subcomponents (and ancestor part, if any). 7.6 also describes when this anonymous object is finalized.

If the ancestor_part is a subtype_mark the Initialize procedure for the ancestor type is applied to the ancestor part after default-initializing it, unless the procedure is abstract, as described in 7.6. The Adjust procedure for the ancestor type is not called in this case, since there is no assignment to the ancestor part as a whole. 

If an aggregate is of a tagged type, a check is made that its value belongs to the first subtype of the type. Constraint_Error is raised if this check fails. 

Ramification: This check ensures that no values of a tagged type are ever outside the first subtype, as required for inherited dispatching operations to work properly (see 3.4). This check will always succeed if the first subtype is unconstrained. This check is not extended to untagged types to preserve upward compatibility. 


#### Extensions to Ada 83

We now allow extension_aggregates. 


#### Wording Changes from Ada 83

We have adopted new wording for expressing the rule that the type of an aggregate shall be determinable from the outside, though using the fact that it is nonlimited record (extension) or array.

An aggregate now creates an anonymous object. This is necessary so that controlled types will work (see 7.6). 


### 4.3.1  Record Aggregates

[In a record_aggregate, a value is specified for each component of the record or record extension value, using either a named or a positional association.] 


#### Syntax

record_aggregate ::= (record_component_association_list)

record_component_association_list ::= 
    record_component_association {, record_component_association}
  | null record

record_component_association ::= 
    [component_choice_list =&gt] expression

component_choice_list ::= 
     component_selector_name {| component_selector_name}
   | others

A record_component_association is a named component association if it has a component_choice_list; otherwise, it is a positional component association. Any positional component associations shall precede any named component associations. If there is a named association with a component_choice_list of others, it shall come last. 

Discussion: These rules were implied by the BNF in an early version of the RM9X, but it made the grammar harder to read, and was inconsistent with how we handle discriminant constraints. Note that for array aggregates we still express some of the rules in the grammar, but array aggregates are significantly different because an array aggregate is either all positional (with a possible others at the end), or all named. 

In the record_component_association_list for a record_aggregate, if there is only one association, it shall be a named association. 

Reason: Otherwise the construct would be interpreted as a parenthesized expression. This is considered a syntax rule, since it is relevant to overload resolution. We choose not to express it with BNF so we can share the definition of record_component_association_list in both record_aggregate and extension_aggregate. 

Ramification: The record_component_association_list of an extension_aggregate does not have such a restriction. 


#### Name Resolution Rules

The expected type for a record_aggregate shall be a single nonlimited record type or record extension. 

Ramification: This rule is used to resolve whether an aggregate is an array_aggregate or a record_aggregate. The presence of a with is used to resolve between a record_aggregate and an extension_aggregate. 

For the record_component_association_list of a record_aggregate, all components of the composite value defined by the aggregate are needed[; for the association list of an extension_aggregate, only those components not determined by the ancestor expression or subtype are needed (see 4.3.2).] Each selector_name in a record_component_association shall denote a needed component [(including possibly a discriminant)].

Ramification: For the association list of a record_aggregate, "needed components" includes every component of the composite value, but does not include those in unchosen variants (see AI83-309). If there are variants, then the value specified for the discriminant that governs them determines which variant is chosen, and hence which components are needed.

If an extension defines a new known_discriminant_part, then all of its discriminants are needed in the component association list of an extension aggregate for that type, even if the discriminants have the same names and types as discriminants of the type of the ancestor expression. This is necessary to ensure that the positions in the record_component_association_list are well defined, and that discriminants that govern variant_parts can be given by static expressions. 

Version=[5],Kind=(AddedNormal),Group=[T],Term=[needed component], Def=[a component of a record type or record extension that is required to have its value specified within a given aggregate]

The expected type for the expression of a record_component_association is the type of the associated component(s); the associated component(s) are as follows: 

For a positional association, the component [(including possibly a discriminant)] in the corresponding relative position (in the declarative region of the type), counting only the needed components; 

Ramification: This means that for an association list of an extension_aggregate, only noninherited components are counted to determine the position.

For a named association with one or more component_selector_names, the named component(s);

For a named association with the reserved word others, all needed components that are not associated with some previous association. 


#### Legality Rules

If the type of a record_aggregate is a record extension, then it shall be a descendant of a record type, through one or more record extensions (and no private extensions).

If there are no components needed in a given record_component_association_list, then the reserved words null record shall appear rather than a list of record_component_associations.

Ramification: For example, "(null record)" is a record_aggregate for a null record type. Similarly, "(T'(A) with null record)" is an extension_aggregate for a type defined as a null record extension of T.

Each record_component_association shall have at least one associated component, and each needed component shall be associated with exactly one record_component_association. If a record_component_association has two or more associated components, all of them shall be of the same type.

Ramification: These rules apply to an association with an others choice. 

Reason: Without these rules, there would be no way to know what was the expected type for the expression of the association. 

Discussion: AI83-00244 also requires that the expression shall be legal for each associated component. This is because even though two components have the same type, they might have different subtypes. Therefore, the legality of the expression, particularly if it is an array aggregate, might differ depending on the associated component's subtype. However, we have relaxed the rules on array aggregates slightly for Ada 95, so the staticness of an applicable index constraint has no effect on the legality of the array aggregate to which it applies. See 4.3.3. This was the only case (that we know of) where a subtype provided by context affected the legality of an expression. 

Ramification: The rule that requires at least one associated component for each record_component_association implies that there can be no extra associations for components that don't exist in the composite value, or that are already determined by the ancestor expression or subtype of an extension_aggregate.

The second part of the first sentence ensures that no needed components are left out, nor specified twice. 

If the components of a variant_part are needed, then the value of a discriminant that governs the variant_part shall be given by a static expression. 

Ramification: This expression might either be given within the aggregate itself, or in a constraint on the parent subtype in a derived_type_definition for some ancestor of the type of the aggregate.


#### Dynamic Semantics

The evaluation of a record_aggregate consists of the evaluation of the record_component_association_list.

For the evaluation of a record_component_association_list, any per-object constraints (see 3.8) for components specified in the association list are elaborated and any expressions are evaluated and converted to the subtype of the associated component. Any constraint elaborations and expression evaluations (and conversions) occur in an arbitrary order, except that the expression for a discriminant is evaluated (and converted) prior to the elaboration of any per-object constraint that depends on it, which in turn occurs prior to the evaluation and conversion of the expression for the component with the per-object constraint.

Ramification: The conversion in the first rule might raise Constraint_Error. 

Discussion: This check in the first rule presumably happened as part of the dependent compatibility check in Ada 83. 

The expression of a record_component_association is evaluated (and converted) once for each associated component.

NOTE 1   For a record_aggregate with positional associations, expressions specifying discriminant values appear first since the known_discriminant_part is given first in the declaration of the type; they have to be in the same order as in the known_discriminant_part. 


#### Examples

Example of a record aggregate with positional associations: 

```ada
(4, July, 1776)                                       --  see 3.8 

```

Examples of record aggregates with named associations: 

```ada
(Day =&gt 4, Month =&gt July, Year =&gt 1776)
(Month =&gt July, Day =&gt 4, Year =&gt 1776)

```

```ada
(Disk, Closed, Track =&gt 5, Cylinder =&gt 12)            --  see 3.8.1
(Unit =&gt Disk, Status =&gt Closed, Cylinder =&gt 9, Track =&gt 1)

```

Example of component association with several choices: 

```ada
(Value =&gt 0, Succ|Pred =&gt new Cell'(0, null, null)) 	--  see 3.10.1

```

```ada
 --  The allocator is evaluated twice: Succ and Pred designate different cells

```

Examples of record aggregates for tagged types (see 3.9 and 3.9.1): 

```ada
Expression'(null record)
Literal'(Value =&gt 0.0)
Painted_Point'(0.0, Pi/2.0, Paint =&gt Red)

```


#### Extensions to Ada 83

Null record aggregates may now be specified, via "(null record)". However, this syntax is more useful for null record extensions in extension aggregates. 


#### Wording Changes from Ada 83

Various AIs have been incorporated (AI83-00189, AI83-00244, and AI83-00309). In particular, Ada 83 did not explicitly disallow extra values in a record aggregate. Now we do. 


### 4.3.2  Extension Aggregates

[An extension_aggregate specifies a value for a type that is a record extension by specifying a value or subtype for an ancestor of the type, followed by associations for any components not determined by the ancestor_part.] 


#### Language Design Principles

The model underlying this syntax is that a record extension can also be viewed as a regular record type with an ancestor "prefix". The record_component_association_list corresponds to exactly what would be needed if there were no ancestor/prefix type. The ancestor_part determines the value of the ancestor/prefix. 


#### Syntax

extension_aggregate ::= 
    (ancestor_part with record_component_association_list)

ancestor_part ::= expression | subtype_mark


#### Name Resolution Rules

The expected type for an extension_aggregate shall be a single nonlimited type that is a record extension. If the ancestor_part is an expression, it is expected to be of any nonlimited tagged type. 

Reason: We could have made the expected type T'Class where T is the ultimate ancestor of the type of the aggregate, or we could have made it even more specific than that. However, if the overload resolution rules get too complicated, the implementation gets more difficult and it becomes harder to produce good error messages. 


#### Legality Rules

If the ancestor_part is a subtype_mark, it shall denote a specific tagged subtype. The type of the extension_aggregate shall be derived from the type of the ancestor_part, through one or more record extensions (and no private extensions). 


#### Static Semantics

For the record_component_association_list of an extension_aggregate, the only components needed are those of the composite value defined by the aggregate that are not inherited from the type of the ancestor_part, plus any inherited discriminants if the ancestor_part is a subtype_mark that denotes an unconstrained subtype. 


#### Dynamic Semantics

For the evaluation of an extension_aggregate, the record_component_association_list is evaluated. If the ancestor_part is an expression, it is also evaluated; if the ancestor_part is a subtype_mark, the components of the value of the aggregate not given by the record_component_association_list are initialized by default as for an object of the ancestor type. Any implicit initializations or evaluations are performed in an arbitrary order, except that the expression for a discriminant is evaluated prior to any other evaluation or initialization that depends on it.

If the type of the ancestor_part has discriminants that are not inherited by the type of the extension_aggregate, then, unless the ancestor_part is a subtype_mark that denotes an unconstrained subtype, a check is made that each discriminant of the ancestor has the value specified for a corresponding discriminant, either in the record_component_association_list, or in the derived_type_definition for some ancestor of the type of the extension_aggregate. Constraint_Error is raised if this check fails. 

Ramification: Corresponding and specified discriminants are defined in 3.7. The rules requiring static compatibility between new discriminants of a derived type and the parent discriminant(s) they constrain ensure that at most one check is required per discriminant of the ancestor expression.

NOTE 1   If all components of the value of the extension_aggregate are determined by the ancestor_part, then the record_component_association_list is required to be simply null record.

NOTE 2   If the ancestor_part is a subtype_mark, then its type can be abstract. If its type is controlled, then as the last step of evaluating the aggregate, the Initialize procedure of the ancestor type is called, unless the Initialize procedure is abstract (see 7.6). 


#### Examples

Examples of extension aggregates (for types defined in 3.9.1): 

```ada
Painted_Point'(Point with Red)
(Point'(P) with Paint =&gt Black)

```

```ada
(Expression with Left =&gt 1.2, Right =&gt 3.4)
Addition'(Binop with null record)
             -- presuming Binop is of type Binary_Operation

```


#### Extensions to Ada 83

The extension aggregate syntax is new. 


### 4.3.3  Array Aggregates

[In an array_aggregate, a value is specified for each component of an array, either positionally or by its index.] For a positional_array_aggregate, the components are given in increasing-index order, with a final others, if any, representing any remaining components. For a named_array_aggregate, the components are identified by the values covered by the discrete_choices.


#### Language Design Principles

The rules in this subclause are based on terms and rules for discrete_choice_lists defined in 3.8.1, "Variant Parts and Discrete Choices". 


#### Syntax

array_aggregate ::= 
    positional_array_aggregate | named_array_aggregate

positional_array_aggregate ::= 
    (expression, expression {, expression})
  | (expression {, expression}, others =&gt expression)

'[' ']'",Old=[]"}

named_array_aggregate ::= 
    array_component_association {, array_component_association})

array_component_association ::= 
    discrete_choice_list =&gt expression

An n-dimensional array_aggregate is one that is written as n levels of nested array_aggregates (or at the bottom level, equivalent string_literals). For the multidimensional case (n &gt= 2) the array_aggregates (or equivalent string_literals) at the n1 lower levels are called subaggregates of the enclosing n-dimensional array_aggregate. The expressions of the bottom level subaggregates (or of the array_aggregate itself if one-dimensional) are called the array component expressions of the enclosing n-dimensional array_aggregate. 

Ramification: Subaggregates do not have a type. They correspond to part of an array. For example, with a matrix, a subaggregate would correspond to a single row of the matrix. The definition of "n-dimensional" array_aggregate applies to subaggregates as well as aggregates that have a type. 

To be honest: An others choice is the reserved word others as it appears in a positional_array_aggregate or as the discrete_choice of the discrete_choice_list in an array_component_association. 


#### Name Resolution Rules

The expected type for an array_aggregate (that is not a subaggregate) shall be a single nonlimited array type. The component type of this array type is the expected type for each array component expression of the array_aggregate. 

Ramification: We already require a single array or record type or record extension for an aggregate. The above rule requiring a single nonlimited array type (and similar ones for record and extension aggregates) resolves which kind of aggregate you have. 

The expected type for each discrete_choice in any discrete_choice_list of a named_array_aggregate is the type of the corresponding index; the corresponding index for an array_aggregate that is not a subaggregate is the first index of its type; for an (nm)-dimensional subaggregate within an array_aggregate of an n-dimensional type, the corresponding index is the index in position m+1. 


#### Legality Rules

An array_aggregate of an n-dimensional array type shall be written as an n-dimensional array_aggregate. 

Ramification: In an m-dimensional array_aggregate [(including a subaggregate)], where m &gt= 2, each of the expressions has to be an (m1)-dimensional subaggregate. 

An others choice is allowed for an array_aggregate only if an applicable index constraint applies to the array_aggregate. [An applicable index constraint is a constraint provided by certain contexts where an array_aggregate is permitted that can be used to determine the bounds of the array value specified by the aggregate.] Each of the following contexts (and none other) defines an applicable index constraint: 

For an explicit_actual_parameter, an explicit_generic_actual_parameter, the expression of a return_statement, the initialization expression in an object_declaration, or a default_expression [(for a parameter or a component)], when the nominal subtype of the corresponding formal parameter, generic formal parameter, function result, object, or component is a constrained array subtype, the applicable index constraint is the constraint of the subtype;

For the expression of an assignment_statement where the name denotes an array variable, the applicable index constraint is the constraint of the array variable; 

Reason: This case is broken out because the constraint comes from the actual subtype of the variable (which is always constrained) rather than its nominal subtype (which might be unconstrained). 

For the operand of a qualified_expression whose subtype_mark denotes a constrained array subtype, the applicable index constraint is the constraint of the subtype;

For a component expression in an aggregate, if the component's nominal subtype is a constrained array subtype, the applicable index constraint is the constraint of the subtype; 

Discussion: Here, the array_aggregate with others is being used within a larger aggregate. 

For a parenthesized expression, the applicable index constraint is that, if any, defined for the expression. 

Discussion: RM83 omitted this case, presumably as an oversight. We want to minimize situations where an expression becomes illegal if parenthesized. 

The applicable index constraint applies to an array_aggregate that appears in such a context, as well as to any subaggregates thereof. In the case of an explicit_actual_parameter (or default_expression) for a call on a generic formal subprogram, no applicable index constraint is defined. 

Reason: This avoids generic contract model problems, because only mode conformance is required when matching actual subprograms with generic formal subprograms. 

The discrete_choice_list of an array_component_association is allowed to have a discrete_choice that is a nonstatic expression or that is a discrete_range that defines a nonstatic or null range, only if it is the single discrete_choice of its discrete_choice_list, and there is only one array_component_association in the array_aggregate.

Discussion: We now allow a nonstatic others choice even if there are other array component expressions as well. 

In a named_array_aggregate with more than one discrete_choice, no two discrete_choices are allowed to cover the same value (see 3.8.1); if there is no others choice, the discrete_choices taken together shall exactly cover a contiguous sequence of values of the corresponding index type. 

Ramification: This implies that each component must be specified exactly once. See AI83-309. 

A bottom level subaggregate of a multidimensional array_aggregate of a given array type is allowed to be a string_literal only if the component type of the array type is a character type; each character of such a string_literal shall correspond to a defining_character_literal of the component type. 


#### Static Semantics

A subaggregate that is a string_literal is equivalent to one that is a positional_array_aggregate of the same length, with each expression being the character_literal for the corresponding character of the string_literal.


#### Dynamic Semantics

The evaluation of an array_aggregate of a given array type proceeds in two steps: 

a)Any discrete_choices of this aggregate and of its subaggregates are evaluated in an arbitrary order, and converted to the corresponding index type; 

b)The array component expressions of the aggregate are evaluated in an arbitrary order and their values are converted to the component subtype of the array type; an array component expression is evaluated once for each associated component. 

Ramification: Subaggregates are not separately evaluated. The conversion of the value of the component expressions to the component subtype might raise Constraint_Error.

The bounds of the index range of an array_aggregate [(including a subaggregate)] are determined as follows: 

For an array_aggregate with an others choice, the bounds are those of the corresponding index range from the applicable index constraint;

For a positional_array_aggregate [(or equivalent string_literal)] without an others choice, the lower bound is that of the corresponding index range in the applicable index constraint, if defined, or that of the corresponding index subtype, if not; in either case, the upper bound is determined from the lower bound and the number of expressions [(or the length of the string_literal)];

For a named_array_aggregate without an others choice, the bounds are determined by the smallest and largest index values covered by any discrete_choice_list. 

Reason: We don't need to say that each index value has to be covered exactly once, since that is a ramification of the general rule on aggregates that each component's value has to be specified exactly once. 

For an array_aggregate, a check is made that the index range defined by its bounds is compatible with the corresponding index subtype. 

Discussion: In RM83, this was phrased more explicitly, but once we define "compatibility" between a range and a subtype, it seems to make sense to take advantage of that definition. 

Ramification: The definition of compatibility handles the special case of a null range, which is always compatible with a subtype. See AI83-00313. 

For an array_aggregate with an others choice, a check is made that no expression is specified for an index value outside the bounds determined by the applicable index constraint. 

Discussion: RM83 omitted this case, apparently through an oversight. AI83-00309 defines this as a dynamic check, even though other Ada 83 rules ensured that this check could be performed statically. We now allow an others choice to be dynamic, even if it is not the only choice, so this check now needs to be dynamic, in some cases. Also, within a generic unit, this would be a nonstatic check in some cases. 

For a multidimensional array_aggregate, a check is made that all subaggregates that correspond to the same index have the same bounds. 

Ramification: No array bounds "sliding" is performed on subaggregates. 

Reason: If sliding were performed, it would not be obvious which subaggregate would determine the bounds of the corresponding index. 

The exception Constraint_Error is raised if any of the above checks fail. 

NOTE 1   In an array_aggregate, positional notation may only be used with two or more expressions; a single expression in parentheses is interpreted as a parenthesized_expression. A named_array_aggregate, such as (1 =&gt X), may be used to specify an array with a single component.


#### Examples

Examples of array aggregates with positional associations: 

```ada
(7, 9, 5, 1, 3, 2, 4, 8, 6, 0)
Table'(5, 8, 4, 1, others =&gt 0)  --  see 3.6 

```

Examples of array aggregates with named associations: 

```ada
(1 .. 5 =&gt (1 .. 8 =&gt 0.0))      --  two-dimensional
(1 .. N =&gt new Cell)             --  N new cells, in particular for N = 0

```

```ada
Table'(2 | 4 | 10 =&gt 1, others =&gt 0)
Schedule'(Mon .. Fri =&gt True,  others =&gt False)  --  see 3.6
Schedule'(Wed | Sun  =&gt False, others =&gt True)
Vector'(1 =&gt 2.5)                                --  single-component vector

```

Examples of two-dimensional array aggregates: 

```ada
-- Three aggregates for the same value of subtype Matrix(1..2,1..3) (see 3.6):

```

```ada
((1.1, 1.2, 1.3), (2.1, 2.2, 2.3))
(1 =&gt (1.1, 1.2, 1.3), 2 =&gt (2.1, 2.2, 2.3))
(1 =&gt (1 =&gt 1.1, 2 =&gt 1.2, 3 =&gt 1.3), 2 =&gt (1 =&gt 2.1, 2 =&gt 2.2, 3 =&gt 2.3))

```

Examples of aggregates as initial values: 

```ada
A : Table := (7, 9, 5, 1, 3, 2, 4, 8, 6, 0);        -- A(1)=7, A(10)=0
B : Table := (2 | 4 | 10 =&gt 1, others =&gt 0);        -- B(1)=0, B(10)=1
C : constant Matrix := (1 .. 5 =&gt (1 .. 8 =&gt 0.0)); -- C'Last(1)=5, C'Last(2)=8

```

```ada
D : Bit_Vector(M .. N) := (M .. N =&gt True);         -- see 3.6
E : Bit_Vector(M .. N) := (others =&gt True);
F : String(1 .. 1) := (1 =&gt 'F');  -- a one component aggregate: same as "F"

```


#### Extensions to Ada 83

We now allow "named with others" aggregates in all contexts where there is an applicable index constraint, effectively eliminating what was RM83-4.3.2(6). Sliding never occurs on an aggregate with others, because its bounds come from the applicable index constraint, and therefore already match the bounds of the target.

The legality of an others choice is no longer affected by the staticness of the applicable index constraint. This substantially simplifies several rules, while being slightly more flexible for the user. It obviates the rulings of AI83-00244 and AI83-00310, while taking advantage of the dynamic nature of the "extra values" check required by AI83-00309.

Named array aggregates are permitted even if the index type is descended from a formal scalar type. See 4.9 and AI83-00190. 


#### Wording Changes from Ada 83

We now separate named and positional array aggregate syntax, since, unlike other aggregates, named and positional associations cannot be mixed in array aggregates (except that an others choice is allowed in a positional array aggregate).

We have also reorganized the presentation to handle multidimensional and one-dimensional aggregates more uniformly, and to incorporate the rulings of AI83-00019, AI83-00309, etc. 

Version=[5],Kind=(AddedNormal),Group=[C],Term=[container aggregate], Def=[a construct used to define a value of a type that represents a collection of elements, by explicitly specifying the elements in the collection]


#### Syntax

key_choice_list =&gt expression | key_choice_list =&gt &lt&gt | iterated_element_association",Old=&lt&gt"}

key_choice {'|' key_choice}",Old=&lt&gt"&gt

key_expression | discrete_range",Old=&lt}"&gt

for loop_parameter_specification[ use key_expression] =&gt expression | for iterator_specification[ use key_expression] =&gt expression",Old=&lt&gt"} 


## 4.4  Expressions

An expression is a formula that defines the computation or retrieval of a value. In this Reference Manual, the term "expression" refers to a construct of the syntactic category expression or of any of the other five syntactic categories defined below. 


#### Syntax

expression ::= 
     relation {and relation} 	| relation {and then relation}
   | relation {or relation} 	| relation {or else relation}
   | relation {xor relation}

relation ::= 
     simple_expression [relational_operator simple_expression]
   | simple_expression [not] in range
   | simple_expression [not] in subtype_mark

simple_expression ::= [unary_adding_operator] term {binary_adding_operator term}

term ::= factor {multiplying_operator factor}

factor ::= primary [** primary] | abs primary | not primary

primary ::= 
    numeric_literal | null | string_literal | aggregate
  | name | qualified_expression | allocator | (expression)


#### Name Resolution Rules

A name used as a primary shall resolve to denote an object or a value. 

Discussion: This replaces RM83-4.4(3). We don't need to mention named numbers explicitly, because the name of a named number denotes a value. We don't need to mention attributes explicitly, because attributes now denote (rather than yield) values in general. Also, the new wording allows attributes that denote objects, which should always have been allowed (in case the implementation chose to have such a thing). 

Reason: It might seem odd that this is an overload resolution rule, but it is relevant during overload resolution. For example, it helps ensure that a primary that consists of only the identifier of a parameterless function is interpreted as a function_call rather than directly as a direct_name. 


#### Static Semantics

Each expression has a type; it specifies the computation or retrieval of a value of that type.


#### Dynamic Semantics

The value of a primary that is a name denoting an object is the value of the object.


#### Implementation Permissions

For the evaluation of a primary that is a name denoting an object of an unconstrained numeric subtype, if the value of the object is outside the base range of its type, the implementation may either raise Constraint_Error or return the value of the object. 

Ramification: This means that if extra-range intermediates are used to hold the value of an object of an unconstrained numeric subtype, a Constraint_Error can be raised on a read of the object, rather than only on an assignment to it. Similarly, it means that computing the value of an object of such a subtype can be deferred until the first read of the object (presuming no side effects other than failing an Overflow_Check are possible). This permission is over and above that provided by clause 11.6, since this allows the Constraint_Error to move to a different handler. 

Reason: This permission is intended to allow extra-range registers to be used efficiently to hold parameters and local variables, even if they might need to be transferred into smaller registers for performing certain predefined operations. 

Discussion: There is no need to mention other kinds of primarys, since any Constraint_Error to be raised can be "charged" to the evaluation of the particular kind of primary. 


#### Examples

Examples of primaries: 

```ada
4.0                --  real literal
Pi                 --  named number
(1 .. 10 =&gt 0)     --  array aggregate
Sum                --  variable
Integer'Last       --  attribute
Sine(X)            --  function call
Color'(Blue)       --  qualified expression
Real(M*N)          --  conversion
(Line_Count + 10)  --  parenthesized expression 

```

Examples of expressions: 

```ada
Volume                      -- primary
not Destroyed               -- factor
2*Line_Count                -- term
-4.0                        -- simple expression
-4.0 + A                    -- simple expression
B**2 - 4.0*A*C              -- simple expression
Password(1 .. 3) = "Bwv"    -- relation
Count in Small_Int          -- relation
Count not in Small_Int      -- relation
Index = 0 or Item_Hit       -- expression
(Cold and Sunny) or Warm    -- expression (parentheses are required)
A**(B**C)                   -- expression (parentheses are required)

```


#### Extensions to Ada 83

In Ada 83, out parameters and their nondiscriminant subcomponents are not allowed as primaries. These restrictions are eliminated in Ada 95.

In various contexts throughout the language where Ada 83 syntax rules had simple_expression, the corresponding Ada 95 syntax rule has expression instead. This reflects the inclusion of modular integer types, which makes the logical operators "and", "or", and "xor" more useful in expressions of an integer type. Requiring parentheses to use these operators in such contexts seemed unnecessary and potentially confusing. Note that the bounds of a range still have to be specified by simple_expressions, since otherwise expressions involving membership tests might be ambiguous. Essentially, the operation ".." is of higher precedence than the logical operators, and hence uses of logical operators still have to be parenthesized when used in a bound of a range. 


## 4.5  Operators and Expression Evaluation

[ The language defines the following six categories of operators (given in order of increasing precedence). The corresponding operator_symbols, and only those, can be used as designators in declarations of functions for user-defined operators. See 6.6, "Overloading of Operators".] 


#### Syntax

logical_operator ::= 	 and | or  | xor

relational_operator ::= 	 =   | /=  | &lt   | &lt= | &gt | &gt=

binary_adding_operator ::= 	 +   |    | &

unary_adding_operator ::= 	 +   | 

multiplying_operator ::= 	 *   | /   | mod | rem

highest_precedence_operator ::= 	 **  | abs | not

Discussion: Some of the above syntactic categories are not used in other syntax rules. They are just used for classification. The others are used for both classification and parsing. 


#### Static Semantics

For a sequence of operators of the same precedence level, the operators are associated with their operands in textual order from left to right. Parentheses can be used to impose specific associations. 

Discussion: The left-associativity is not directly inherent in the grammar of 4.4, though in the definition of the metasymbols {} implies left associativity. So this could be seen as redundant, depending on how literally one interprets the definition of the {} metasymbols.

See the Implementation Permissions below regarding flexibility in reassociating operators of the same precedence. 

For each form of type definition, certain of the above operators are predefined; that is, they are implicitly declared immediately after the type definition. For each such implicit operator declaration, the parameters are called Left and Right for binary operators; the single parameter is called Right for unary operators. [An expression of the form X op Y, where op is a binary operator, is equivalent to a function_call of the form "op"(X, Y). An expression of the form op Y, where op is a unary operator, is equivalent to a function_call of the form "op"(Y). The predefined operators and their effects are described in subclauses 4.5.1 through 4.5.6.] 


#### Dynamic Semantics

[ The predefined operations on integer types either yield the mathematically correct result or raise the exception Constraint_Error. For implementations that support the Numerics Annex, the predefined operations on real types yield results whose accuracy is defined in Annex G, or raise the exception Constraint_Error. ]

To be honest: Predefined operations on real types can "silently" give wrong results when the Machine_Overflows attribute is false, and the computation overflows. 


#### Implementation Requirements

The implementation of a predefined operator that delivers a result of an integer or fixed point type may raise Constraint_Error only if the result is outside the base range of the result type.

The implementation of a predefined operator that delivers a result of a floating point type may raise Constraint_Error only if the result is outside the safe range of the result type. 

To be honest: An exception is made for exponentiation by a negative exponent in 4.5.6.


#### Implementation Permissions

For a sequence of predefined operators of the same precedence level (and in the absence of parentheses imposing a specific association), an implementation may impose any association of the operators with operands so long as the result produced is an allowed result for the left-to-right association, but ignoring the potential for failure of language-defined checks in either the left-to-right or chosen order of association. 

Discussion: Note that the permission to reassociate the operands in any way subject to producing a result allowed for the left-to-right association is not much help for most floating point operators, since reassociation may introduce significantly different round-off errors, delivering a result that is outside the model interval for the left-to-right association. Similar problems arise for division with integer or fixed point operands.

Note that this permission does not apply to user-defined operators. 

NOTE 1   The two operands of an expression of the form X op Y, where op is a binary operator, are evaluated in an arbitrary order, as for any function_call (see 6.4).


#### Examples

Examples of precedence: 

```ada
not Sunny or Warm    --  same as (not Sunny) or Warm
X &gt 4.0 and Y &gt 0.0  --  same as (X &gt 4.0) and (Y &gt 0.0)

```

```ada
-4.0*A**2            --  same as (4.0 * (A**2))
abs(1 + A) + B       --  same as (abs (1 + A)) + B
Y**(-3)              --  parentheses are necessary
A / B * C            --  same as (A/B)*C
A + (B + C)          --  evaluate B + C before adding it to A 

```


#### Wording Changes from Ada 83

We don't give a detailed definition of precedence, since it is all implicit in the syntax rules anyway.

The permission to reassociate is moved here from RM83-11.6(5), so it is closer to the rules defining operator association. 


### 4.5.1  Logical Operators and Short-circuit Control Forms


#### Name Resolution Rules

An expression consisting of two relations connected by and then or or else (a short-circuit control form) shall resolve to be of some boolean type; the expected type for both relations is that same boolean type. 

Reason: This rule is written this way so that overload resolution treats the two operands symmetrically; the resolution of overloading present in either one can benefit from the resolution of the other. Furthermore, the type expected by context can help. 


#### Static Semantics

The following logical operators are predefined for every boolean type T, for every modular type T, and for every one-dimensional array type T whose component type is a boolean type: 

```ada
function "and"(Left, Right : T) return T
function "or" (Left, Right : T) return T
function "xor"(Left, Right : T) return T

```

To be honest: For predefined operators, the parameter and result subtypes shown as T are actually the unconstrained subtype of the type. 

For boolean types, the predefined logical operators and, or, and xor perform the conventional operations of conjunction, inclusive disjunction, and exclusive disjunction, respectively.

For modular types, the predefined logical operators are defined on a bit-by-bit basis, using the binary representation of the value of the operands to yield a binary representation for the result, where zero represents False and one represents True. If this result is outside the base range of the type, a final subtraction by the modulus is performed to bring the result into the base range of the type.

The logical operators on arrays are performed on a component-by-component basis on matching components (as for equality - see 4.5.2), using the predefined logical operator for the component type. The bounds of the resulting array are those of the left operand.


#### Dynamic Semantics

The short-circuit control forms and then and or else deliver the same result as the corresponding predefined and and or operators for boolean types, except that the left operand is always evaluated first, and the right operand is not evaluated if the value of the left operand determines the result.

For the logical operators on arrays, a check is made that for each component of the left operand there is a matching component of the right operand, and vice versa. Also, a check is made that each component of the result belongs to the component subtype. The exception Constraint_Error is raised if either of the above checks fails. 

Discussion: The check against the component subtype is per AI83-00535. 

NOTE   The conventional meaning of the logical operators is given by the following truth table: 

	  A	  B	(A and B)	(A or B)	(A xor B)

	True  	True  	True  	True  	False
	True  	False 	False 	True  	True
	False 	True  	False 	True  	True
	False 	False 	False 	False 	False


#### Examples

Examples of logical operators: 

```ada
Sunny or Warm
Filter(1 .. 10) and Filter(15 .. 24)   --   see 3.6.1 

```

Examples of short-circuit control forms: 

```ada
Next_Car.Owner /= null and then Next_Car.Owner.Age &gt 25   --   see 3.10.1
N = 0 or else A(N) = Hit_Value

```


### 4.5.2  Relational Operators and Membership Tests

[ The equality operators = (equals) and /= (not equals) are predefined for nonlimited types. The other relational_operators are the ordering operators &lt (less than), &lt= (less than or equal), &gt (greater than), and &gt= (greater than or equal). The ordering operators are predefined for scalar types, and for discrete array types, that is, one-dimensional array types whose components are of a discrete type. 

Ramification: The equality operators are not defined for every nonlimited type - see below for the exact rule. 

A membership test, using in or not in, determines whether or not a value belongs to a given subtype or range, or has a tag that identifies a type that is covered by a given type. Membership tests are allowed for all types.]


#### Name Resolution Rules

The tested type of a membership test is the type of the range or the type determined by the subtype_mark. If the tested type is tagged, then the simple_expression shall resolve to be of a type that covers or is covered by the tested type; if untagged, the expected type for the simple_expression is the tested type.

Reason: The part of the rule for untagged types is stated in a way that ensures that operands like null are still legal as operands of a membership test.

The significance of "covers or is covered by" is that we allow the simple_expression to be of any class-wide type that covers the tested type, not just the one rooted at the tested type.


#### Legality Rules

For a membership test, if the simple_expression is of a tagged class-wide type, then the tested type shall be (visibly) tagged. 

Ramification: Untagged types covered by the tagged class-wide type are not permitted. Such types can exist if they are descendants of a private type whose full type is tagged. This rule is intended to avoid confusion since such derivatives don't have their "own" tag, and hence are indistinguishable from one another at run time once converted to a covering class-wide type. 


#### Static Semantics

The result type of a membership test is the predefined type Boolean.

The equality operators are predefined for every specific type T that is not limited, and not an anonymous access type, with the following specifications: 

```ada
function "=" (Left, Right : T) return Boolean
function "/="(Left, Right : T) return Boolean

```

The ordering operators are predefined for every specific scalar type T, and for every discrete array type T, with the following specifications: 

```ada
function "&lt" (Left, Right : T) return Boolean
function "&lt="(Left, Right : T) return Boolean
function "&gt" (Left, Right : T) return Boolean
function "&gt="(Left, Right : T) return Boolean

```


#### Dynamic Semantics

For discrete types, the predefined relational operators are defined in terms of corresponding mathematical operations on the position numbers of the values of the operands.

For real types, the predefined relational operators are defined in terms of the corresponding mathematical operations on the values of the operands, subject to the accuracy of the type. 

Ramification: For floating point types, the results of comparing nearly equal values depends on the accuracy of the implementation (see G.2.1, "Model of Floating Point Arithmetic" for implementations that support the Numerics Annex). 

Implementation Note: On a machine with signed zeros, if the generated code generates both plus zero and minus zero, plus and minus zero must be equal by the predefined equality operators. 

Two access-to-object values are equal if they designate the same object, or if both are equal to the null value of the access type.

Two access-to-subprogram values are equal if they are the result of the same evaluation of an Access attribute_reference, or if both are equal to the null value of the access type. Two access-to-subprogram values are unequal if they designate different subprograms. [It is unspecified whether two access values that designate the same subprogram but are the result of distinct evaluations of Access attribute_references are equal or unequal.] 

Reason: This allows each Access attribute_reference for a subprogram to designate a distinct "wrapper" subprogram if necessary to support an indirect call. 

For a type extension, predefined equality is defined in terms of the primitive [(possibly user-defined)] equals operator of the parent type and of any tagged components of the extension part, and predefined equality for any other components not inherited from the parent type. 

Ramification: Two values of a type extension are not equal if there is a variant_part in the extension part and the two values have different variants present. This is a ramification of the requirement that a discriminant governing such a variant_part has to be a "new" discriminant, and so has to be equal in the two values for the values to be equal. Note that variant_parts in the parent part need not match if the primitive equals operator for the parent type considers them equal.

```ada
  

```

For a private type, if its full type is tagged, predefined equality is defined in terms of the primitive equals operator of the full type; if the full type is untagged, predefined equality for the private type is that of its full type.

For other composite types, the predefined equality operators [(and certain other predefined operations on composite types - see 4.5.1 and 4.6)] are defined in terms of the corresponding operation on matching components, defined as follows: 

For two composite objects or values of the same non-array type, matching components are those that correspond to the same component_declaration or discriminant_specification;

For two one-dimensional arrays of the same type, matching components are those (if any) whose index values match in the following sense: the lower bounds of the index ranges are defined to match, and the successors of matching indices are defined to match;

For two multidimensional arrays of the same type, matching components are those whose index values match in successive index positions. 

The analogous definitions apply if the types of the two objects or values are convertible, rather than being the same. 

Discussion: Ada 83 seems to omit this part of the definition, though it is used in array type conversions. See 4.6. 

Given the above definition of matching components, the result of the predefined equals operator for composite types (other than for those composite types covered earlier) is defined as follows: 

If there are no components, the result is defined to be True;

If there are unmatched components, the result is defined to be False;

Otherwise, the result is defined in terms of the primitive equals operator for any matching tagged components, and the predefined equals for any matching untagged components. 

Reason: This asymmetry between tagged and untagged components is necessary to preserve upward compatibility and corresponds with the corresponding situation with generics, where the predefined operations "reemerge" in a generic for untagged types, but do not for tagged types. Also, only tagged types support user-defined assignment (see 7.6), so only tagged types can fully handle levels of indirection in the implementation of the type. For untagged types, one reason for a user-defined equals operator might be to allow values with different bounds or discriminants to compare equal in certain cases. When such values are matching components, the bounds or discriminants will necessarily match anyway if the discriminants of the enclosing values match. 

Ramification: Two null arrays of the same type are always equal; two null records of the same type are always equal.

Note that if a composite object has a component of a floating point type, and the floating point type has both a plus and minus zero, which are considered equal by the predefined equality, then a block compare cannot be used for the predefined composite equality. Of course, with user-defined equals operators for tagged components, a block compare breaks down anyway, so this is not the only special case that requires component-by-component comparisons. On a one's complement machine, a similar situation might occur for integer types, since one's complement machines typically have both a plus and minus (integer) zero. 

The predefined "/=" operator gives the complementary result to the predefined "=" operator. 

Ramification: Furthermore, if the user defines an "=" operator that returns Boolean, then a "/=" operator is implicitly declared in terms of the user-defined "=" operator so as to give the complementary result. See 6.6. 

For a discrete array type, the predefined ordering operators correspond to lexicographic order using the predefined order relation of the component type: A null array is lexicographically less than any array having at least one component. In the case of nonnull arrays, the left operand is lexicographically less than the right operand if the first component of the left operand is less than that of the right; otherwise the left operand is lexicographically less than the right operand only if their first components are equal and the tail of the left operand is lexicographically less than that of the right (the tail consists of the remaining components beyond the first and can be null).

For the evaluation of a membership test, the simple_expression and the range (if any) are evaluated in an arbitrary order.

A membership test using in yields the result True if: 

The tested type is scalar, and the value of the simple_expression belongs to the given range, or the range of the named subtype; or 

Ramification: The scalar membership test only does a range check. It does not perform any other check, such as whether a value falls in a "hole" of a "holey" enumeration type. The Pos attribute function can be used for that purpose.

Even though Standard.Float is an unconstrained subtype, the test "X in Float" will still return False (presuming the evaluation of X does not raise Constraint_Error) when X is outside Float'Range. 

The tested type is not scalar, and the value of the simple_expression satisfies any constraints of the named subtype, and, if the type of the simple_expression is class-wide, the value has a tag that identifies a type covered by the tested type. 

Ramification: Note that the tag is not checked if the simple_expression is of a specific type. 

Otherwise the test yields the result False.

A membership test using not in gives the complementary result to the corresponding membership test using in.

NOTE   No exception is ever raised by a membership test, by a predefined ordering operator, or by a predefined equality operator for an elementary type, but an exception can be raised by the evaluation of the operands. A predefined equality operator for a composite type can only raise an exception if the type has a tagged part whose primitive equals operator propagates an exception.

NOTE   If a composite type has components that depend on discriminants, two values of this type have matching components if and only if their discriminants are equal. Two nonnull arrays have matching components if and only if the length of each dimension is the same for both. 


#### Examples

Examples of expressions involving relational operators and membership tests: 

```ada
X /= Y

```

```ada
"" &lt "A" and "A" &lt "Aa"     -- True
"A" &lt "B" and "A" &lt "A  "  -- True

```

```ada
My_Car = null               -- true if My_Car has been set to null (see 3.10.1)
My_Car = Your_Car           -- true if we both share the same car
My_Car.all = Your_Car.all   -- true if the two cars are identical

```

```ada
N not in 1 .. 10            -- range membership test
Today in Mon .. Fri         -- range membership test
Today in Weekday            -- subtype membership test (see 3.5.1)
Archive in Disk_Unit        -- subtype membership test (see 3.8.1)
Tree.all in Addition'Class  -- class membership test (see 3.9.1)

```


#### Extensions to Ada 83

Membership tests can be used to test the tag of a class-wide value.

Predefined equality for a composite type is defined in terms of the primitive equals operator for tagged components or the parent part. 


#### Wording Changes from Ada 83

The term "membership test" refers to the relation "X in S" rather to simply the reserved word in or not in.

We use the term "equality operator" to refer to both the = (equals) and /= (not equals) operators. Ada 83 referred to = as the equality operator, and /= as the inequality operator. The new wording is more consistent with the ISO 10646 name for "=" (equals sign) and provides a category similar to "ordering operator" to refer to both = and /=.

We have changed the term "catenate" to "concatenate". 


### 4.5.3  Binary Adding Operators


#### Static Semantics

The binary adding operators + (addition) and  (subtraction) are predefined for every specific numeric type T with their conventional meaning. They have the following specifications: 

```ada
function "+"(Left, Right : T) return T
function "-"(Left, Right : T) return T

```

The concatenation operators & are predefined for every nonlimited, one-dimensional array type T with component type C. They have the following specifications: 

```ada
function "&"(Left : T; Right : T) return T
function "&"(Left : T; Right : C) return T
function "&"(Left : C; Right : T) return T
function "&"(Left : C; Right : C) return T

```


#### Dynamic Semantics

For the evaluation of a concatenation with result type T, if both operands are of type T, the result of the concatenation is a one-dimensional array whose length is the sum of the lengths of its operands, and whose components comprise the components of the left operand followed by the components of the right operand. If the left operand is a null array, the result of the concatenation is the right operand. Otherwise, the lower bound of the result is determined as follows: 

If the ultimate ancestor of the array type was defined by a constrained_array_definition, then the lower bound of the result is that of the index subtype; 

Reason: This rule avoids Constraint_Error when using concatenation on an array type whose first subtype is constrained. 

If the ultimate ancestor of the array type was defined by an unconstrained_array_definition, then the lower bound of the result is that of the left operand. 

[The upper bound is determined by the lower bound and the length.] A check is made that the upper bound of the result of the concatenation belongs to the range of the index subtype, unless the result is a null array. Constraint_Error is raised if this check fails.

If either operand is of the component type C, the result of the concatenation is given by the above rules, using in place of such an operand an array having this operand as its only component (converted to the component subtype) and having the lower bound of the index subtype of the array type as its lower bound. 

Ramification: The conversion might raise Constraint_Error. The conversion provides "sliding" for the component in the case of an array-of-arrays, consistent with the normal Ada 95 rules that allow sliding during parameter passing. 

The result of a concatenation is defined in terms of an assignment to an anonymous object, as for any function call (see 6.5). 

Ramification: This implies that value adjustment is performed as appropriate - see 7.6. We don't bother saying this for other predefined operators, even though they are all function calls, because this is the only one where it matters. It is the only one that can return a value having controlled parts. 

NOTE   As for all predefined operators on modular types, the binary adding operators + and  on modular types include a final reduction modulo the modulus if the result is outside the base range of the type. 

Implementation Note: A full "modulus" operation need not be performed after addition or subtraction of modular types. For binary moduli, a simple mask is sufficient. For nonbinary moduli, a check after addition to see if the value is greater than the high bound of the base range can be followed by a conditional subtraction of the modulus. Conversely, a check after subtraction to see if a "borrow" was performed can be followed by a conditional addition of the modulus. 


#### Examples

Examples of expressions involving binary adding operators: 

```ada
Z + 0.1      --  Z has to be of a real type 

```

```ada
"A" & "BCD"  --  concatenation of two string literals
'A' & "BCD"  --  concatenation of a character literal and a string literal
'A' & 'A'    --  concatenation of two character literals 

```


#### Inconsistencies With Ada 83

The lower bound of the result of concatenation, for a type whose first subtype is constrained, is now that of the index subtype. This is inconsistent with Ada 83, but generally only for Ada 83 programs that raise Constraint_Error. For example, the concatenation operator in 

```ada
X : array(1..10) of Integer;
begin
X := X(6..10) & X(1..5);

```

would raise Constraint_Error in Ada 83 (because the bounds of the result of the concatenation would be 6..15, which is outside of 1..10), but would succeed and swap the halves of X (as expected) in Ada 95. 


#### Extensions to Ada 83

Concatenation is now useful for array types whose first subtype is constrained. When the result type of a concatenation is such an array type, Constraint_Error is avoided by effectively first sliding the left operand (if nonnull) so that its lower bound is that of the index subtype. 


### 4.5.4  Unary Adding Operators


#### Static Semantics

The unary adding operators + (identity) and  (negation) are predefined for every specific numeric type T with their conventional meaning. They have the following specifications: 

```ada
function "+"(Right : T) return T
function "-"(Right : T) return T

```

NOTE 1   For modular integer types, the unary adding operator , when given a nonzero operand, returns the result of subtracting the value of the operand from the modulus; for a zero operand, the result is zero. 


### 4.5.5  Multiplying Operators


#### Static Semantics

The multiplying operators * (multiplication), / (division), mod (modulus), and rem (remainder) are predefined for every specific integer type T: 

```ada
function "*"  (Left, Right : T) return T
function "/"  (Left, Right : T) return T
function "mod"(Left, Right : T) return T
function "rem"(Left, Right : T) return T

```

Signed integer multiplication has its conventional meaning.

Signed integer division and remainder are defined by the relation: 

```ada
A = (A/B)*B + (A rem B)

```

where (A rem B) has the sign of A and an absolute value less than the absolute value of B. Signed integer division satisfies the identity: 

```ada
(-A)/B = -(A/B) = A/(-B)

```

The signed integer modulus operator is defined such that the result of A mod B has the sign of B and an absolute value less than the absolute value of B; in addition, for some signed integer value N, this result satisfies the relation: 

```ada
A = B*N + (A mod B)

```

The multiplying operators on modular types are defined in terms of the corresponding signed integer operators[, followed by a reduction modulo the modulus if the result is outside the base range of the type] [(which is only possible for the "*" operator)]. 

Ramification: The above identity satisfied by signed integer division is not satisfied by modular division because of the difference in effect of negation. 

Multiplication and division operators are predefined for every specific floating point type T: 

```ada
function "*"(Left, Right : T) return T
function "/"(Left, Right : T) return T

```

The following multiplication and division operators, with an operand of the predefined type Integer, are predefined for every specific fixed point type T: 

```ada
function "*"(Left : T; Right : Integer) return T
function "*"(Left : Integer; Right : T) return T
function "/"(Left : T; Right : Integer) return T

```

[All of the above multiplying operators are usable with an operand of an appropriate universal numeric type.] The following additional multiplying operators for root_real are predefined[, and are usable when both operands are of an appropriate universal or root numeric type, and the result is allowed to be of type root_real, as in a number_declaration]: 

Ramification: These operators are analogous to the multiplying operators involving fixed or floating point types where root_real substitutes for the fixed or floating point type, and root_integer substitutes for Integer. Only values of the corresponding universal numeric types are implicitly convertible to these root numeric types, so these operators are really restricted to use with operands of a universal type, or the specified root numeric types. 

```ada
function "*"(Left, Right : root_real) return root_real
function "/"(Left, Right : root_real) return root_real

```

```ada
function "*"(Left : root_real; Right : root_integer) return root_real
function "*"(Left : root_integer; Right : root_real) return root_real
function "/"(Left : root_real; Right : root_integer) return root_real

```

Multiplication and division between any two fixed point types are provided by the following two predefined operators: 

Ramification: Universal_fixed is the universal type for the class of fixed point types, meaning that these operators take operands of any fixed point types (not necessarily the same) and return a result that is implicitly (or explicitly) convertible to any fixed point type. 

```ada
function "*"(Left, Right : universal_fixed) return universal_fixed
function "/"(Left, Right : universal_fixed) return universal_fixed

```


#### Legality Rules

The above two fixed-fixed multiplying operators shall not be used in a context where the expected type for the result is itself universal_fixed - [the context has to identify some other numeric type to which the result is to be converted, either explicitly or implicitly]. 

Discussion: The small of universal_fixed is infinitesimal; no loss of precision is permitted. However, fixed-fixed division is impractical to implement when an exact result is required, and multiplication will sometimes result in unanticipated overflows in such circumstances, so we require an explicit conversion to be inserted in expressions like A * B * C if A, B, and C are each of some fixed point type.

On the other hand, X := A * B; is permitted by this rule, even if X, A, and B are all of different fixed point types, since the expected type for the result of the multiplication is the type of X, which is necessarily not universal_fixed. 


#### Dynamic Semantics

The multiplication and division operators for real types have their conventional meaning. [For floating point types, the accuracy of the result is determined by the precision of the result type. For decimal fixed point types, the result is truncated toward zero if the mathematical result is between two multiples of the small of the specific result type (possibly determined by context); for ordinary fixed point types, if the mathematical result is between two multiples of the small, it is unspecified which of the two is the result. ]

The exception Constraint_Error is raised by integer division, rem, and mod if the right operand is zero. [Similarly, for a real type T with T'Machine_Overflows True, division by zero raises Constraint_Error.] 

NOTE 1   For positive A and B, A/B is the quotient and A rem B is the remainder when A is divided by B. The following relations are satisfied by the rem operator: 

```ada
     A  rem (-B) =   A rem B
   (-A) rem   B  = -(A rem B)

```

NOTE 2   For any signed integer K, the following identity holds: 

```ada
   A mod B   =   (A + K*B) mod B

```

The relations between signed integer division, remainder, and modulus are illustrated by the following table: 

```ada
   A      B   A/B   A rem B  A mod B     A     B    A/B   A rem B   A mod B

```

```ada
   10     5    2       0        0       -10    5    -2       0         0
   11     5    2       1        1       -11    5    -2      -1         4
   12     5    2       2        2       -12    5    -2      -2         3
   13     5    2       3        3       -13    5    -2      -3         2
   14     5    2       4        4       -14    5    -2      -4         1

```

```ada
   A      B   A/B   A rem B  A mod B     A     B    A/B   A rem B   A mod B

   10    -5   -2       0        0       -10   -5     2       0         0
   11    -5   -2       1       -4       -11   -5     2      -1        -1
   12    -5   -2       2       -3       -12   -5     2      -2        -2
   13    -5   -2       3       -2       -13   -5     2      -3        -3
   14    -5   -2       4       -1       -14   -5     2      -4        -4

```


#### Examples

Examples of expressions involving multiplying operators: 

```ada
I : Integer := 1;
J : Integer := 2;
K : Integer := 3;

```

```ada
X : Real := 1.0;                      --     see 3.5.7
Y : Real := 2.0;

```

```ada
F : Fraction := 0.25;                 --     see 3.5.9
G : Fraction := 0.5;

```

```ada
Expression  	Value  	Result Type

I*J            	2      	same as I and J, that is, Integer
K/J            	1      	same as K and J, that is, Integer
K mod J  	1      	same as K and J, that is, Integer

X/Y            	0.5    	same as X and Y, that is, Real
F/2            	0.125  	same as F, that is, Fraction

3*F            	0.75   	same as F, that is, Fraction
0.75*G         	0.375  	universal_fixed, implicitly convertible
               	       	to any fixed point type
Fraction(F*G)  	0.125  	Fraction, as stated by the conversion
Real(J)*Y      	4.0    	Real, the type of both operands after
               	       	conversion of J

```


#### Extensions to Ada 83

Explicit conversion of the result of multiplying or dividing two fixed point numbers is no longer required, provided the context uniquely determines some specific fixed point result type. This is to improve support for decimal fixed point, where requiring explicit conversion on every fixed-fixed multiply or divide was felt to be inappropriate.

The type universal_fixed is covered by universal_real, so real literals and fixed point operands may be multiplied or divided directly, without any explicit conversions required. 


#### Wording Changes from Ada 83

We have used the normal syntax for function definition rather than a tabular format. 


### 4.5.6  Highest Precedence Operators


#### Static Semantics

The highest precedence unary operator abs (absolute value) is predefined for every specific numeric type T, with the following specification: 

```ada
function "abs"(Right : T) return T

```

The highest precedence unary operator not (logical negation) is predefined for every boolean type T, every modular type T, and for every one-dimensional array type T whose components are of a boolean type, with the following specification: 

```ada
function "not"(Right : T) return T

```

The result of the operator not for a modular type is defined as the difference between the high bound of the base range of the type and the value of the operand. [For a binary modulus, this corresponds to a bit-wise complement of the binary representation of the value of the operand.]

The operator not that applies to a one-dimensional array of boolean components yields a one-dimensional boolean array with the same bounds; each component of the result is obtained by logical negation of the corresponding component of the operand (that is, the component that has the same index value). A check is made that each component of the result belongs to the component subtype; the exception Constraint_Error is raised if this check fails. 

Discussion: The check against the component subtype is per AI83-00535. 

The highest precedence exponentiation operator ** is predefined for every specific integer type T with the following specification: 

```ada
function "**"(Left : T; Right : Natural) return T

```

Exponentiation is also predefined for every specific floating point type as well as root_real, with the following specification (where T is root_real or the floating point type): 

```ada
function "**"(Left : T; Right : Integer'Base) return T

```

The right operand of an exponentiation is the exponent. The expression X**N with the value of the exponent N positive is equivalent to the expression X*X*...X (with N1 multiplications) except that the multiplications are associated in an arbitrary order. With N equal to zero, the result is one. With the value of N negative [(only defined for a floating point operand)], the result is the reciprocal of the result using the absolute value of N as the exponent. 

Ramification: The language does not specify the order of association of the multiplications inherent in an exponentiation. For a floating point type, the accuracy of the result might depend on the particular association order chosen. 


#### Implementation Permissions

The implementation of exponentiation for the case of a negative exponent is allowed to raise Constraint_Error if the intermediate result of the repeated multiplications is outside the safe range of the type, even though the final result (after taking the reciprocal) would not be. (The best machine approximation to the final result in this case would generally be 0.0.) 

NOTE 1   As implied by the specification given above for exponentiation of an integer type, a check is made that the exponent is not negative. Constraint_Error is raised if this check fails. 


#### Wording Changes from Ada 83

We now show the specification for "**" for integer types with a parameter subtype of Natural rather than Integer for the exponent. This reflects the fact that Constraint_Error is raised if a negative value is provided for the exponent. 

Version=[5],Kind=(AddedNormal),Group=[C],Term=[reduction expression], Def=[an expression that defines how to map or transform a collection of values into a new set of values, and then summarize the values by applying an operation to reduce the set to a single value] 


#### Dynamic Semantics








## 4.6  Type Conversions

[Explicit type conversions, both value conversions and view conversions, are allowed between closely related types as defined below. This clause also defines rules for value and view conversions to a particular subtype of a type, both explicit ones and those implicit in other constructs. ]


#### Syntax

type_conversion ::= 
    subtype_mark(expression)
  | subtype_mark(name)

The target subtype of a type_conversion is the subtype denoted by the subtype_mark. The operand of a type_conversion is the expression or name within the parentheses; its type is the operand type.

One type is convertible to a second type if a type_conversion with the first type as operand type and the second type as target type is legal according to the rules of this clause. Two types are convertible if each is convertible to the other. 

Ramification: Note that "convertible" is defined in terms of legality of the conversion. Whether the conversion would raise an exception at run time is irrelevant to this definition. 

A type_conversion whose operand is the name of an object is called a view conversion if its target type is tagged, or if it appears as an actual parameter of mode out or in out; other type_conversions are called value conversions. 

Ramification: A view conversion to a tagged type can appear in any context that requires an object name, including in an object renaming, the prefix of a selected_component, and if the operand is a variable, on the left side of an assignment_statement. View conversions to other types only occur as actual parameters. Allowing view conversions of untagged types in all contexts seemed to incur an undue implementation burden.


#### Name Resolution Rules

The operand of a type_conversion is expected to be of any type. 

Discussion: This replaces the "must be determinable" wording of Ada 83. This is equivalent to (but hopefully more intuitive than) saying that the operand of a type_conversion is a "complete context". 

The operand of a view conversion is interpreted only as a name; the operand of a value conversion is interpreted as an expression. 

Reason: This formally resolves the syntactic ambiguity between the two forms of type_conversion, not that it really matters. 


#### Legality Rules

If the target type is a numeric type, then the operand type shall be a numeric type. 

If the target type is an array type, then the operand type shall be an array type. Further: 

The types shall have the same dimensionality;

Corresponding index types shall be convertible; and 

The component subtypes shall statically match. 

If the target type is a general access type, then the operand type shall be an access-to-object type. Further: 

Discussion: The Legality Rules and Dynamic Semantics are worded so that a type_conversion T(X) (where T is an access type) is (almost) equivalent to the attribute_reference X.all'Access, where the result is of type T. The type_conversion accepts a null value, whereas the attribute_reference would raise Constraint_Error. 

If the target type is an access-to-variable type, then the operand type shall be an access-to-variable type; 

Ramification: If the target type is an access-to-constant type, then the operand type can be access-to-constant or access-to-variable. 

If the target designated type is tagged, then the operand designated type shall be convertible to the target designated type; 

If the target designated type is not tagged, then the designated types shall be the same, and either the designated subtypes shall statically match or the target designated subtype shall be discriminated and unconstrained; and 

Reason: These rules are designed to ensure that aliased array objects only need "dope" if their nominal subtype is unconstrained, but they can always have dope if required by the run-time model (since no sliding is permitted as part of access type conversion). By contrast, aliased discriminated objects will always need their discriminants stored with them, even if nominally constrained. (Here, we are assuming an implementation that represents an access value as a single pointer.) 

The accessibility level of the operand type shall not be statically deeper than that of the target type. In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit. 

Ramification: The access parameter case is handled by a runtime check. Runtime checks are also done in instance bodies. 

If the target type is an access-to-subprogram type, then the operand type shall be an access-to-subprogram type. Further: 

The designated profiles shall be subtype-conformant.

The accessibility level of the operand type shall not be statically deeper than that of the target type. In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit. If the operand type is declared within a generic body, the target type shall be declared within the generic body.

Reason: The reason it is illegal to convert from an access-to-subprogram type declared in a generic body to one declared outside that body is that in an implementation that shares generic bodies, procedures declared inside the generic need to have a different calling convention - they need an extra parameter pointing to the data declared in the current instance. For procedures declared in the spec, that's OK, because the compiler can know about them at compile time of the instantiation. 

If the target type is not included in any of the above four cases, there shall be a type that is an ancestor of both the target type and the operand type. Further, if the target type is tagged, then either: 

The operand type shall be covered by or descended from the target type; or 

Ramification: This is a conversion toward the root, which is always safe. 

The operand type shall be a class-wide type that covers the target type. 

Ramification: This is a conversion of a class-wide type toward the leaves, which requires a tag check. See Dynamic Semantics.

These two rules imply that a conversion from a parent type to a type extension is not permitted, as this would require specifying the values for additional components, in general, and changing the tag. An extension_aggregate has to be used instead, constructing a new value, rather than converting an existing value. However, a conversion from the class-wide type rooted at the parent type is permitted; such a conversion just verifies that the operand's tag is a descendant of the target. 

In a view conversion for an untagged type, the target type shall be convertible (back) to the operand type. 

Reason: Untagged view conversions appear only as [in] out parameters. Hence, the reverse conversion must be legal as well. The forward conversion must be legal even if an out parameter, because actual parameters of an access type are always copied in anyway. 


#### Static Semantics

A type_conversion that is a value conversion denotes the value that is the result of converting the value of the operand to the target subtype.

A type_conversion that is a view conversion denotes a view of the object denoted by the operand. This view is a variable of the target type if the operand denotes a variable; otherwise it is a constant of the target type.

The nominal subtype of a type_conversion is its target subtype. 


#### Dynamic Semantics

For the evaluation of a type_conversion that is a value conversion, the operand is evaluated, and then the value of the operand is converted to a corresponding value of the target type, if any. If there is no value of the target type that corresponds to the operand value, Constraint_Error is raised[; this can only happen on conversion to a modular type, and only when the operand value is outside the base range of the modular type.] Additional rules follow: 

Numeric Type Conversion 

If the target and the operand types are both integer types, then the result is the value of the target type that corresponds to the same mathematical integer as the operand.

If the target type is a decimal fixed point type, then the result is truncated (toward 0) if the value of the operand is not a multiple of the small of the target type.

If the target type is some other real type, then the result is within the accuracy of the target type (see G.2, "Numeric Performance Requirements", for implementations that support the Numerics Annex). 

Discussion: An integer type might have more bits of precision than a real type, so on conversion (of a large integer), some precision might be lost. 

If the target type is an integer type and the operand type is real, the result is rounded to the nearest integer (away from zero if exactly halfway between two integers). 

Discussion: This was implementation defined in Ada 83. There seems no reason to preserve the nonportability in Ada 95. Round-away-from-zero is the conventional definition of rounding, and standard Fortran and COBOL both specify rounding away from zero, so for interoperability, it seems important to pick this. This is also the most easily "undone" by hand. Round-to-nearest-even is an alternative, but that is quite complicated if not supported by the hardware. In any case, this operation is not expected to be part of an inner loop, so predictability and portability are judged most important. We anticipate that a floating point attribute function Unbiased_Rounding will be provided for those applications that require round-to-nearest-even. "Deterministic" rounding is required for static conversions to integer as well. See 4.9. 

Enumeration Type Conversion 

The result is the value of the target type with the same position number as that of the operand value. 

Array Type Conversion 

If the target subtype is a constrained array subtype, then a check is made that the length of each dimension of the value of the operand equals the length of the corresponding dimension of the target subtype. The bounds of the result are those of the target subtype.

If the target subtype is an unconstrained array subtype, then the bounds of the result are obtained by converting each bound of the value of the operand to the corresponding index type of the target type. For each nonnull index range, a check is made that the bounds of the range belong to the corresponding index subtype. 

Discussion: Only nonnull index ranges are checked, per AI83-00313. 

In either array case, the value of each component of the result is that of the matching component of the operand value (see 4.5.2). 

Ramification: This applies whether or not the component is initialized. 

Composite (Non-Array) Type Conversion 

The value of each nondiscriminant component of the result is that of the matching component of the operand value. 

Ramification: This applies whether or not the component is initialized. 

[The tag of the result is that of the operand.] If the operand type is class-wide, a check is made that the tag of the operand identifies a (specific) type that is covered by or descended from the target type. 

Ramification: This check is certain to succeed if the operand type is itself covered by or descended from the target type. 

Proof: The fact that a type_conversion preserves the tag is stated officially in 3.9, "Tagged Types and Type Extensions" 

For each discriminant of the target type that corresponds to a discriminant of the operand type, its value is that of the corresponding discriminant of the operand value; if it corresponds to more than one discriminant of the operand type, a check is made that all these discriminants are equal in the operand value.

For each discriminant of the target type that corresponds to a discriminant that is specified by the derived_type_definition for some ancestor of the operand type (or if class-wide, some ancestor of the specific type identified by the tag of the operand), its value in the result is that specified by the derived_type_definition. 

Ramification: It is a ramification of the rules for the discriminants of derived types that each discriminant of the result is covered either by this paragraph or the previous one. See 3.7. 

For each discriminant of the operand type that corresponds to a discriminant that is specified by the derived_type_definition for some ancestor of the target type, a check is made that in the operand value it equals the value specified for it.

For each discriminant of the result, a check is made that its value belongs to its subtype. 

Access Type Conversion 

For an access-to-object type, a check is made that the accessibility level of the operand type is not deeper than that of the target type. 

Ramification: This check is needed for operands that are access parameters and in instance bodies.

Note that this check can never fail for the implicit conversion to the anonymous type of an access parameter that is done when calling a subprogram with an access parameter. 

If the target type is an anonymous access type, a check is made that the value of the operand is not null; if the target is not an anonymous access type, then the result is null if the operand value is null. 

Ramification: A conversion to an anonymous access type happens implicitly as part of initializing an access discriminant or access parameter. 

Reason: As explained in 3.10, "Access Types", it is important that a value of an anonymous access type can never be null. 

If the operand value is not null, then the result designates the same object (or subprogram) as is designated by the operand value, but viewed as being of the target designated subtype (or profile); any checks associated with evaluating a conversion to the target designated subtype are performed. 

Ramification: The checks are certain to succeed if the target and operand designated subtypes statically match. 

After conversion of the value to the target type, if the target subtype is constrained, a check is performed that the value satisfies this constraint. 

Ramification: The above check is a Range_Check for scalar subtypes, a Discriminant_Check or Index_Check for access subtypes, and a Discriminant_Check for discriminated subtypes. The Length_Check for an array conversion is performed as part of the conversion to the target type. 

For the evaluation of a view conversion, the operand name is evaluated, and a new view of the object denoted by the operand is created, whose type is the target type; if the target type is composite, checks are performed as above for a value conversion.

The properties of this new view are as follows: 

If the target type is composite, the bounds or discriminants (if any) of the view are as defined above for a value conversion; each nondiscriminant component of the view denotes the matching component of the operand object; the subtype of the view is constrained if either the target subtype or the operand object is constrained, or if the operand type is a descendant of the target type, and has discriminants that were not inherited from the target type;

If the target type is tagged, then an assignment to the view assigns to the corresponding part of the object denoted by the operand; otherwise, an assignment to the view assigns to the object, after converting the assigned value to the subtype of the object (which might raise Constraint_Error); 

Reading the value of the view yields the result of converting the value of the operand object to the target subtype (which might raise Constraint_Error), except if the object is of an access type and the view conversion is passed as an out parameter; in this latter case, the value of the operand object is used to initialize the formal parameter without checking against any constraint of the target subtype (see 6.4.1). 

Reason: This ensures that even an out parameter of an access type is initialized reasonably. 

If an Accessibility_Check fails, Program_Error is raised. Any other check associated with a conversion raises Constraint_Error if it fails.

Conversion to a type is the same as conversion to an unconstrained subtype of the type. 

Reason: This definition is needed because the semantics of various constructs involves converting to a type, whereas an explicit type_conversion actually converts to a subtype. For example, the evaluation of a range is defined to convert the values of the expressions to the type of the range. 

Ramification: A conversion to a scalar type, or, equivalently, to an unconstrained scalar subtype, can raise Constraint_Error if the value is outside the base range of the type. 

NOTE 1   In addition to explicit type_conversions, type conversions are performed implicitly in situations where the expected type and the actual type of a construct differ, as is permitted by the type resolution rules (see 8.6). For example, an integer literal is of the type universal_integer, and is implicitly converted when assigned to a target of some specific integer type. Similarly, an actual parameter of a specific tagged type is implicitly converted when the corresponding formal parameter is of a class-wide type.

Even when the expected and actual types are the same, implicit subtype conversions are performed to adjust the array bounds (if any) of an operand to match the desired target subtype, or to raise Constraint_Error if the (possibly adjusted) value does not satisfy the constraints of the target subtype.

NOTE 2   A ramification of the overload resolution rules is that the operand of an (explicit) type_conversion cannot be the literal null, an allocator, an aggregate, a string_literal, a character_literal, or an attribute_reference for an Access or Unchecked_Access attribute. Similarly, such an expression enclosed by parentheses is not allowed. A qualified_expression (see 4.7) can be used instead of such a type_conversion.

NOTE 3   The constraint of the target subtype has no effect for a type_conversion of an elementary type passed as an out parameter. Hence, it is recommended that the first subtype be specified as the target to minimize confusion (a similar recommendation applies to renaming and generic formal in out objects). 


#### Examples

Examples of numeric type conversion: 

```ada
Real(2*J)      --  value is converted to floating point
Integer(1.6)   --  value is 2
Integer(-0.4)  --  value is 0

```

Example of conversion between derived types: 

```ada
type A_Form is new B_Form;

```

```ada
X : A_Form;
Y : B_Form;

```

```ada
X := A_Form(Y);
Y := B_Form(X);  --  the reverse conversion 

```

Examples of conversions between array types: 

```ada
type Sequence is array (Integer range &lt&gt) of Integer;
subtype Dozen is Sequence(1 .. 12);
Ledger : array(1 .. 100) of Integer;

```

```ada
Sequence(Ledger)            --  bounds are those of Ledger
Sequence(Ledger(31 .. 42))  --  bounds are 31 and 42
Dozen(Ledger(31 .. 42))     --  bounds are those of Dozen 

```


#### Incompatibilities With Ada 83

A character_literal is not allowed as the operand of a type_conversion, since there are now two character types in package Standard.

The component subtypes have to statically match in an array conversion, rather than being checked for matching constraints at run time.

Because sliding of array bounds is now provided for operations where it was not in Ada 83, programs that used to raise Constraint_Error might now continue executing and produce a reasonable result. This is likely to fix more bugs than it creates. 


#### Extensions to Ada 83

A type_conversion is considered the name of an object in certain circumstances (such a type_conversion is called a view conversion). In particular, as in Ada 83, a type_conversion can appear as an in out or out actual parameter. In addition, if the target type is tagged and the operand is the name of an object, then so is the type_conversion, and it can be used as the prefix to a selected_component, in an object_renaming_declaration, etc.

We no longer require type-mark conformance between a parameter of the form of a type conversion, and the corresponding formal parameter. This had caused some problems for inherited subprograms (since there isn't really a type-mark for converted formals), as well as for renamings, formal subprograms, etc. See AI83-00245, AI83-00318, AI83-00547.

We now specify "deterministic" rounding from real to integer types when the value of the operand is exactly between two integers (rounding is away from zero in this case).

"Sliding" of array bounds (which is part of conversion to an array subtype) is performed in more cases in Ada 95 than in Ada 83. Sliding is not performed on the operand of a membership test, nor on the operand of a qualified_expression. It wouldn't make sense on a membership test, and we wish to retain a connection between subtype membership and subtype qualification. In general, a subtype membership test returns True if and only if a corresponding subtype qualification succeeds without raising an exception. Other operations that take arrays perform sliding. 


#### Wording Changes from Ada 83

We no longer explicitly list the kinds of things that are not allowed as the operand of a type_conversion, except in a NOTE.

The rules in this clause subsume the rules for "parameters of the form of a type conversion", and have been generalized to cover the use of a type conversion as a name. 


## 4.7  Qualified Expressions

[A qualified_expression is used to state explicitly the type, and to verify the subtype, of an operand that is either an expression or an aggregate. ]


#### Syntax

qualified_expression ::= 
   subtype_mark'(expression) | subtype_mark'aggregate


#### Name Resolution Rules

The operand (the expression or aggregate) shall resolve to be of the type determined by the subtype_mark, or a universal type that covers it. 


#### Dynamic Semantics

The evaluation of a qualified_expression evaluates the operand (and if of a universal type, converts it to the type determined by the subtype_mark) and checks that its value belongs to the subtype denoted by the subtype_mark. The exception Constraint_Error is raised if this check fails. 

Ramification: This is one of the few contexts in Ada 95 where implicit subtype conversion is not performed prior to a constraint check, and hence no "sliding" of array bounds is provided.

Reason: Implicit subtype conversion is not provided because a qualified_expression with a constrained target subtype is essentially an assertion about the subtype of the operand, rather than a request for conversion. An explicit type_conversion can be used rather than a qualified_expression if subtype conversion is desired.

NOTE 1   When a given context does not uniquely identify an expected type, a qualified_expression can be used to do so. In particular, if an overloaded name or aggregate is passed to an overloaded subprogram, it might be necessary to qualify the operand to resolve its type. 


#### Examples

Examples of disambiguating expressions using qualification: 

```ada
type Mask is (Fix, Dec, Exp, Signif);
type Code is (Fix, Cla, Dec, Tnz, Sub);

```

```ada
Print (Mask'(Dec));  --  Dec is of type Mask
Print (Code'(Dec));  --  Dec is of type Code 

```

```ada
for J in Code'(Fix) .. Code'(Dec) loop ... -- qualification needed for either Fix or Dec
for J in Code range Fix .. Dec loop ...    -- qualification unnecessary
for J in Code'(Fix) .. Dec loop ...        -- qualification unnecessary for Dec

```

```ada
Dozen'(1 | 3 | 5 | 7 =&gt 2, others =&gt 0) -- see 4.6 

```


## 4.8  Allocators

[The evaluation of an allocator creates an object and yields an access value that designates the object. ]


#### Syntax

allocator ::= 
   new subtype_indication | new qualified_expression


#### Name Resolution Rules

The expected type for an allocator shall be a single access-to-object type whose designated type  covers the type determined by the subtype_mark of the subtype_indication or qualified_expression. 

Discussion: See 8.6, "The Context of Overload Resolution" for the meaning of "shall be a single ... type whose ...". 


#### Legality Rules

An initialized allocator is an allocator with a qualified_expression. An uninitialized allocator is one with a subtype_indication. In the subtype_indication of an uninitialized allocator, a constraint is permitted only if the subtype_mark denotes an [unconstrained] composite subtype; if there is no constraint, then the subtype_mark shall denote a definite subtype. 

Ramification: For example, ... new S'Class ... (with no initialization expression) is illegal, but ... new S'Class'(X) ... is legal, and takes its tag and constraints from the initial value X. (Note that the former case cannot have a constraint.) 

If the type of the allocator is an access-to-constant type, the allocator shall be an initialized allocator. If the designated type is limited, the allocator shall be an uninitialized allocator. 

Ramification: For an access-to-constant type whose designated type is limited, allocators are illegal. The Access attribute is legal for such a type, however. 


#### Static Semantics

If the designated type of the type of the allocator is elementary, then the subtype of the created object is the designated subtype. If the designated type is composite, then the created object is always constrained; if the designated subtype is constrained, then it provides the constraint of the created object; otherwise, the object is constrained by its initial value [(even if the designated subtype is unconstrained with defaults)]. 

Discussion: See AI83-00331. 

Reason: All objects created by an allocator are aliased, and all aliased composite objects need to be constrained so that access subtypes work reasonably. 


#### Dynamic Semantics

For the evaluation of an allocator, the elaboration of the subtype_indication or the evaluation of the qualified_expression is performed first. For the evaluation of an initialized allocator, an object of the designated type is created and the value of the qualified_expression is converted to the designated subtype and assigned to the object. 

Ramification: The conversion might raise Constraint_Error. 

For the evaluation of an uninitialized allocator: 

If the designated type is elementary, an object of the designated subtype is created and any implicit initial value is assigned;

If the designated type is composite, an object of the designated type is created with tag, if any, determined by the subtype_mark of the subtype_indication; any per-object constraints on subcomponents are elaborated and any implicit initial values for the subcomponents of the object are obtained as determined by the subtype_indication and assigned to the corresponding subcomponents. A check is made that the value of the object belongs to the designated subtype. Constraint_Error is raised if this check fails. This check and the initialization of the object are performed in an arbitrary order.

Discussion: AI83-00150. 

[If the created object contains any tasks, they are activated (see 9.2).] Finally, an access value that designates the created object is returned. 

NOTE 1   Allocators cannot create objects of an abstract type. See 3.9.3.

NOTE 2   If any part of the created object is controlled, the initialization includes calls on corresponding Initialize or Adjust procedures. See 7.6.

NOTE 3   As explained in 13.11, "Storage Management", the storage for an object allocated by an allocator comes from a storage pool (possibly user defined). The exception Storage_Error is raised by an allocator if there is not enough storage. Instances of Unchecked_Deallocation may be used to explicitly reclaim storage.

NOTE 4   Implementations are permitted, but not required, to provide garbage collection (see 13.11.3). 

Ramification: Note that in an allocator, the exception Constraint_Error can be raised by the evaluation of the qualified_expression, by the elaboration of the subtype_indication, or by the initialization. 

Discussion: By default, the implementation provides the storage pool. The user may exercise more control over storage management by associating a user-defined pool with an access type. 


#### Examples

Examples of allocators: 

```ada
new Cell'(0, null, null)                          -- initialized explicitly, see 3.10.1
new Cell'(Value =&gt 0, Succ =&gt null, Pred =&gt null) -- initialized explicitly
new Cell                                          -- not initialized

```

```ada
new Matrix(1 .. 10, 1 .. 20)                      -- the bounds only are given
new Matrix'(1 .. 10 =&gt (1 .. 20 =&gt 0.0))          -- initialized explicitly

```

```ada
new Buffer(100)                                   -- the discriminant only is given
new Buffer'(Size =&gt 80, Pos =&gt 0, Value =&gt (1 .. 80 =&gt 'A')) -- initialized explicitly

```

```ada
Expr_Ptr'(new Literal)                  -- allocator for access-to-class-wide type, see 3.9.1
Expr_Ptr'(new Literal'(Expression with 3.5))      -- initialized explicitly

```


#### Incompatibilities With Ada 83

The subtype_indication of an uninitialized allocator may not have an explicit constraint if the designated type is an access type. In Ada 83, this was permitted even though the constraint had no affect on the subtype of the created object. 


#### Extensions to Ada 83

Allocators creating objects of type T are now overloaded on access types designating T'Class and all class-wide types that cover T.

Implicit array subtype conversion (sliding) is now performed as part of an initialized allocator. 


#### Wording Changes from Ada 83

We have used a new organization, inspired by the ACID document, that makes it clearer what is the subtype of the created object, and what subtype conversions take place.

Discussion of storage management issues, such as garbage collection and the raising of Storage_Error, has been moved to 13.11, "Storage Management". 


## 4.9  Static Expressions and Static Subtypes

Certain expressions of a scalar or string type are defined to be static. Similarly, certain discrete ranges are defined to be static, and certain scalar and string subtypes are defined to be static subtypes. [ Static means determinable at compile time, using the declared properties or values of the program entities.] 

Discussion: As opposed to more elaborate data flow analysis, etc. 


#### Language Design Principles

For an expression to be static, it has to be calculable at compile time.

Only scalar and string expressions are static.

To be static, an expression cannot have any nonscalar, nonstring subexpressions (though it can have nonscalar constituent names). A static scalar expression cannot have any nonscalar subexpressions. There is one exception - a membership test for a string subtype can be static, and the result is scalar, even though a subexpression is nonscalar.

The rules for evaluating static expressions are designed to maximize portability of static calculations.


#### Static Semantics

A static expression is [a scalar or string expression that is] one of the following:

a numeric_literal; 

Ramification: A numeric_literal is always a static expression, even if its expected type is not that of a static subtype. However, if its value is explicitly converted to, or qualified by, a nonstatic subtype, the resulting expression is nonstatic. 

a string_literal of a static string subtype; 

Ramification: That is, the constrained subtype defined by the index range of the string is static. Note that elementary values don't generally have subtypes, while composite values do (since the bounds or discriminants are inherent in the value). 

a name that denotes the declaration of a named number or a static constant;

Ramification: Note that enumeration literals are covered by the function_call case. 

a function_call whose function_name or function_prefix statically denotes a static function, and whose actual parameters, if any (whether given explicitly or by default), are all static expressions; 

Ramification: This includes uses of operators that are equivalent to function_calls. 

an attribute_reference that denotes a scalar value, and whose prefix denotes a static scalar subtype;

Ramification: Note that this does not include the case of an attribute that is a function; a reference to such an attribute is not even an expression. See above for function calls.

An implementation may define the staticness and other properties of implementation-defined attributes. 

an attribute_reference whose prefix statically denotes a statically constrained array object or array subtype, and whose attribute_designator is First, Last, or Length, with an optional dimension;

a type_conversion whose subtype_mark denotes a static scalar subtype, and whose operand is a static expression;

a qualified_expression whose subtype_mark denotes a static [(scalar or string)] subtype, and whose operand is a static expression; 

Ramification: This rules out the subtype_mark'aggregate case. 

Reason: Adding qualification to an expression shouldn't make it nonstatic, even for strings. 

a membership test whose simple_expression is a static expression, and whose range is a static range or whose subtype_mark denotes a static [(scalar or string)] subtype; 

Reason: Clearly, we should allow membership tests in exactly the same cases where we allow qualified_expressions. 

a short-circuit control form both of whose relations are static expressions;

a static expression enclosed in parentheses. 

Discussion: Informally, we talk about a static value. When we do, we mean a value specified by a static expression. 

Ramification: The language requires a static expression in a number_declaration, a numeric type definition, a discrete_choice (sometimes), certain representation items, an attribute_designator, and when specifying the value of a discriminant governing a variant_part in a record_aggregate or extension_aggregate. 

A name statically denotes an entity if it denotes the entity and: 

It is a direct_name, expanded name, or character_literal, and it denotes a declaration other than a renaming_declaration; or

It is an attribute_reference whose prefix statically denotes some entity; or

It denotes a renaming_declaration with a name that statically denotes the renamed entity. 

Ramification: Selected_components that are not expanded names and indexed_components do not statically denote things. 

A static function is one of the following: 

Ramification: These are the functions whose calls can be static expressions. 

a predefined operator whose parameter and result types are all scalar types none of which are descendants of formal scalar types;

a predefined concatenation operator whose result type is a string type;

an enumeration literal;

a language-defined attribute that is a function, if the prefix denotes a static scalar subtype, and if the parameter and result types are scalar. 

In any case, a generic formal subprogram is not a static function.

A static constant is a constant view declared by a full constant declaration or an object_renaming_declaration with a static nominal subtype, having a value defined by a static scalar expression or by a static string expression whose value has a length not exceeding the maximum length of a string_literal in the implementation. 

Ramification: A deferred constant is not static; the view introduced by the corresponding full constant declaration can be static. 

Reason: The reason for restricting the length of static string constants is so that compilers don't have to store giant strings in their symbol tables. Since most string constants will be initialized from string_literals, the length limit seems pretty natural. The reason for avoiding nonstring types is also to save symbol table space. We're trying to keep it cheap and simple (from the implementer's viewpoint), while still allowing, for example, the link name of a pragma Import to contain a concatenation.

The length we're talking about is the maximum number of characters in the value represented by a string_literal, not the number of characters in the source representation; the quotes don't count. 

A static range is a range whose bounds are static expressions, [or a range_attribute_reference that is equivalent to such a range.] A static discrete_range is one that is a static range or is a subtype_indication that defines a static scalar subtype. The base range of a scalar type is a static range, unless the type is a descendant of a formal scalar type.

A static subtype is either a static scalar subtype or a static string subtype. A static scalar subtype is an unconstrained scalar subtype whose type is not a descendant of a formal scalar type, or a constrained scalar subtype formed by imposing a compatible static constraint on a static scalar subtype. A static string subtype is an unconstrained string subtype whose index subtype and component subtype are static (and whose type is not a descendant of a formal array type), or a constrained string subtype formed by imposing a compatible static constraint on a static string subtype. In any case, the subtype of a generic formal object of mode in out, and the result subtype of a generic formal function, are not static. 

Ramification: String subtypes are the only composite subtypes that can be static. 

Reason: The part about generic formal objects of mode in out is necessary because the subtype of the formal is not required to have anything to do with the subtype of the actual. For example: 

```ada
subtype Int10 is Integer range 1..10;

```

```ada
generic
    F : in out Int10;
procedure G;

```

```ada
procedure G is
begin
    case F is
        when 1..10 =&gt null;
        -- Illegal!
    end case;
end G;

```

```ada
X : Integer range 1..20;
procedure I is new G(F =&gt X); -- OK.

```

The case_statement is illegal, because the subtype of F is not static, so the choices have to cover all values of Integer, not just those in the range 1..10. A similar issue arises for generic formal functions, now that function calls are object names. 

The different kinds of static constraint are defined as follows: 

A null constraint is always static;

A scalar constraint is static if it has no range_constraint, or one with a static range;

An index constraint is static if each discrete_range is static, and each index subtype of the corresponding array type is static;

A discriminant constraint is static if each expression of the constraint is static, and the subtype of each discriminant is static. 

A subtype is statically constrained if it is constrained, and its constraint is static. An object is statically constrained if its nominal subtype is statically constrained, or if it is a static string constant. 


#### Legality Rules

A static expression is evaluated at compile time except when it is part of the right operand of a static short-circuit control form whose value is determined by its left operand. This evaluation is performed exactly, without performing Overflow_Checks. For a static expression that is evaluated: 

The expression is illegal if its evaluation fails a language-defined check other than Overflow_Check.

If the expression is not part of a larger static expression, then its value shall be within the base range of its expected type. Otherwise, the value may be arbitrarily large or small. 

If the expression is of type universal_real and its expected type is a decimal fixed point type, then its value shall be a multiple of the small of the decimal type. 

Ramification: This means that a numeric_literal for a decimal type cannot have "extra" significant digits. 

The last two restrictions above do not apply if the expected type is a descendant of a formal scalar type (or a corresponding actual type in an instance).

Discussion: Values outside the base range are not permitted when crossing from the "static" domain to the "dynamic" domain. This rule is designed to enhance portability of programs containing static expressions. Note that this rule applies to the exact value, not the value after any rounding or truncation. (See below for the rounding and truncation requirements.)

Short-circuit control forms are a special case: 

```ada
N: constant := 0.0;
X: constant Boolean := (N = 0.0) or else (1.0/N &gt 0.5); -- Static.

```

The declaration of X is legal, since the divide-by-zero part of the expression is not evaluated. X is a static constant equal to True.

Ramification: There is no requirement to recheck these rules in an instance; the base range check will generally be performed at run time anyway. 


#### Implementation Requirements

For a real static expression that is not part of a larger static expression, and whose expected type is not a descendant of a formal scalar type, the implementation shall round or truncate the value (according to the Machine_Rounds attribute of the expected type) to the nearest machine number of the expected type; if the value is exactly half-way between two machine numbers, any rounding shall be performed away from zero. If the expected type is a descendant of a formal scalar type, no special rounding or truncating is required - normal accuracy rules apply (see Annex G). 

Reason: Discarding extended precision enhances portability by ensuring that the value of a static constant of a real type is always a machine number of the type. Deterministic rounding of exact halves also enhances portability.

When the expected type is a descendant of a formal floating point type, extended precision (beyond that of the machine numbers) can be retained when evaluating a static expression, to ease code sharing for generic instantiations. For similar reasons, normal (nondeterministic) rounding or truncating rules apply for descendants of a formal fixed point type.

Implementation Note: Note that the implementation of static expressions has to keep track of plus and minus zero for a type whose Signed_Zeros attribute is True.

Note that the only values of a fixed point type are the multiples of the small, so a static conversion to a fixed-point type, or division by an integer, must do truncation to a multiple of small. It is not correct for the implementation to do all static calculations in infinite precision.

NOTE 1   An expression can be static even if it occurs in a context where staticness is not required. 

Ramification: For example: 

```ada
X : Float := Float'(1.0E+400) + 1.0 - Float'(1.0E+400);

```

The expression is static, which means that the value of X must be exactly 1.0, independent of the accuracy or range of the run-time floating point implementation.

The following kinds of expressions are never static: explicit_dereference, indexed_component, slice, null, aggregate, allocator. 

NOTE 2   A static (or run-time) type_conversion from a real type to an integer type performs rounding. If the operand value is exactly half-way between two integers, the rounding is performed away from zero. 

Reason: We specify this for portability. The reason for not choosing round-to-nearest-even, for example, is that this method is easier to undo. 

Ramification: The attribute Truncation (see A.5.3) can be used to perform a (static) truncation prior to conversion, to prevent rounding. 

Implementation Note: The value of the literal 0E999999999999999999999999999999999999999999999 is zero. The implementation must take care to evaluate such literals properly.


#### Examples

Examples of static expressions: 

```ada
1 + 1       -- 2
abs(-10)*3  -- 30

```

```ada
Kilo : constant := 1000;
Mega : constant := Kilo*Kilo;   -- 1_000_000
Long : constant := Float'Digits*2;

```

```ada
Half_Pi    : constant := Pi/2;           -- see 3.3.2
Deg_To_Rad : constant := Half_Pi/90;
Rad_To_Deg : constant := 1.0/Deg_To_Rad; -- equivalent to 1.0/((3.14159_26536/2)/90)

```


#### Extensions to Ada 83

The rules for static expressions and static subtypes are generalized to allow more kinds of compile-time-known expressions to be used where compile-time-known values are required, as follows: 

Membership tests and short-circuit control forms may appear in a static expression.

The bounds and length of statically constrained array objects or subtypes are static.

The Range attribute of a statically constrained array subtype or object gives a static range.

A type_conversion is static if the subtype_mark denotes a static scalar subtype and the operand is a static expression.

All numeric literals are now static, even if the expected type is a formal scalar type. This is useful in case_statements and variant_parts, which both now allow a value of a formal scalar type to control the selection, to ease conversion of a package into a generic package. Similarly, named array aggregates are also permitted for array types with an index type that is a formal scalar type. 

The rules for the evaluation of static expressions are revised to require exact evaluation at compile time, and force a machine number result when crossing from the static realm to the dynamic realm, to enhance portability and predictability. Exact evaluation is not required for descendants of a formal scalar type, to simplify generic code sharing and to avoid generic contract model problems.

Static expressions are legal even if an intermediate in the expression goes outside the base range of the type. Therefore, the following will succeed in Ada 95, whereas it might raise an exception in Ada 83: 

```ada
type Short_Int is range -32_768 .. 32_767;
I : Short_Int := -32_768;

```

This might raise an exception in Ada 83 because "32_768" is out of range, even though "32_768" is not. In Ada 95, this will always succeed.

Certain expressions involving string operations (in particular concatenation and membership tests) are considered static in Ada 95.

The reason for this change is to simplify the rule requiring compile-time-known string expressions as the link name in an interfacing pragma, and to simplify the preelaborability rules. 


#### Incompatibilities With Ada 83

An Ada 83 program that uses an out-of-range static value is illegal in Ada 95, unless the expression is part of a larger static expression, or the expression is not evaluated due to being on the right-hand side of a short-circuit control form. 


#### Wording Changes from Ada 83

This clause (and 4.5.5, "Multiplying Operators") subsumes the RM83 section on Universal Expressions.

The existence of static string expressions necessitated changing the definition of static subtype to include string subtypes. Most occurrences of "static subtype" have been changed to "static scalar subtype", in order to preserve the effect of the Ada 83 rules. This has the added benefit of clarifying the difference between "static subtype" and "statically constrained subtype", which has been a source of confusion. In cases where we allow static string subtypes, we explicitly use phrases like "static string subtype" or "static (scalar or string) subtype", in order to clarify the meaning for those who have gotten used to the Ada 83 terminology.

In Ada 83, an expression was considered nonstatic if it raised an exception. Thus, for example: 

```ada
Bad: constant := 1/0; -- Illegal!

```

was illegal because 1/0 was not static. In Ada 95, the above example is still illegal, but for a different reason: 1/0 is static, but there's a separate rule forbidding the exception raising.


### 4.9.1  Statically Matching Constraints and Subtypes


#### Static Semantics

A constraint statically matches another constraint if both are null constraints, both are static and have equal corresponding bounds or discriminant values, or both are nonstatic and result from the same elaboration of a constraint of a subtype_indication or the same evaluation of a range of a discrete_subtype_definition. 

A subtype statically matches another subtype of the same type if they have statically matching constraints. Two anonymous access subtypes statically match if their designated subtypes statically match. 

Ramification: Statically matching constraints and subtypes are the basis for subtype conformance of profiles (see 6.3.1). 

Two ranges of the same type statically match if both result from the same evaluation of a range, or if both are static and have equal corresponding bounds. 

Ramification: The notion of static matching of ranges is used in 12.5.3, "Formal Array Types"; the index ranges of formal and actual constrained array subtypes have to statically match. 

A constraint is statically compatible with a scalar subtype if it statically matches the constraint of the subtype, or if both are static and the constraint is compatible with the subtype. A constraint is statically compatible with an access or composite subtype if it statically matches the constraint of the subtype, or if the subtype is unconstrained. One subtype is statically compatible with a second subtype if the constraint of the first is statically compatible with the second subtype. 

Discussion: Static compatibility is required when constraining a parent subtype with a discriminant from a new discriminant_part. See 3.7. Static compatibility is also used in matching generic formal derived types.

Note that statically compatible with a subtype does not imply compatible with a type. It is OK since the terms are used in different contexts. 


#### Wording Changes from Ada 83

This subclause is new to Ada 95. 


#### Static Semantics





S'Wide_Image

S'Image







