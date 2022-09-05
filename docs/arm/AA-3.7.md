---
sidebar_position:  23
---

# 3.7  Discriminants

{AI95-00251-01} {AI95-00326-01} [ A composite type (other than an array or interface type) can have discriminants, which parameterize the type. A [known_discriminant_part](./AA-3.7#S0061) specifies the discriminants of a composite type. A discriminant of an object is a component of the object, and is either of a discrete type or an access type. An [unknown_discriminant_part](./AA-3.7#S0060) in the declaration of a view of a type specifies that the discriminants of the type are unknown for the given view; all subtypes of such a view are indefinite subtypes.] 

Glossary entry: A discriminant is a parameter for a composite type. It can control, for example, the bounds of a component of the type if the component is an array. A discriminant for a task type can be used to pass data to a task of the type upon creation.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[discriminant], Def=[a parameter for a composite type, which can control, for example, the bounds of a component that is an array], Note1=[A discriminant for a task type can be used to pass data to a task of the type upon its creation.] 

Discussion: {AI95-00114-01} A view of a type, and all subtypes of the view, have unknown discriminants when the number or names of the discriminants, if any, are unknown at the point of the type declaration for the view. A [discriminant_part](./AA-3.7#S0059) of (&lt&gt) is used to indicate unknown discriminants. 


#### Language Design Principles

{AI95-00402-01} When an access discriminant is initialized at the time of object creation with an allocator of an anonymous type, the allocated object and the object with the discriminant are tied together for their lifetime. They should be allocated out of the same storage pool, and then at the end of the lifetime of the enclosing object, finalized and reclaimed together. In this case, the allocated object is called a coextension (see 3.10.2). 

Discussion: The above principle when applied to a nonlimited type implies that such an object may be copied only to a shorter-lived object, because attempting to assign it to a longer-lived object would fail because the access discriminants would not match. In a copy, the lifetime connection between the enclosing object and the allocated object does not exist. The allocated object is tied in the above sense only to the original object. Other copies have only secondary references to it.

Note that when an [allocator](./AA-4.8#S0164) appears as a constraint on an access discriminant in a [subtype_indication](./AA-3.2#S0027) that is elaborated independently from object creation, no such connection exists. For example, if a named constrained subtype is declared via "subtype Constr is Rec(Acc_Discrim =&gt new T);" or if such an [allocator](./AA-4.8#S0164) appears in the [subtype_indication](./AA-3.2#S0027) for a component, the allocator is evaluated when the [subtype_indication](./AA-3.2#S0027) is elaborated, and hence its lifetime is typically longer than the objects or components that will later be subject to the constraint. In these cases, the allocated object should not be reclaimed until the [subtype_indication](./AA-3.2#S0027) goes out of scope. 


#### Syntax

discriminant_part<a id="S0059"></a> ::= [unknown_discriminant_part](./AA-3.7#S0060) | [known_discriminant_part](./AA-3.7#S0061)

unknown_discriminant_part<a id="S0060"></a> ::= (&lt&gt)

known_discriminant_part<a id="S0061"></a> ::= 
   ([discriminant_specification](./AA-3.7#S0062) {; [discriminant_specification](./AA-3.7#S0062)})

{AI95-00231-01} {AI12-0398-1} discriminant_specification<a id="S0062"></a> ::= 
   [defining_identifier_list](./AA-3.3#S0033) : [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028) [:= [default_expression](./AA-3.7#S0063)]
      [[aspect_specification](./AA-13.1#S0346)] 
 | [defining_identifier_list](./AA-3.3#S0033) : [access_definition](./AA-3.10#S0084) [:= [default_expression](./AA-3.7#S0063)]
      [[aspect_specification](./AA-13.1#S0346)] 

Discussion: {AI12-0398-1} Only implementation-defined aspects are allowed on discriminants in Ada 2022. Implementers are cautioned that any aspect allowed on a discriminant will need conformance rules. If, for instance, an aspect changed the representation of a discriminant, rules would be needed to ensure that the representation is the same for all views of the type (partial and full). 

default_expression<a id="S0063"></a> ::= [expression](./AA-4.4#S0132)


#### Name Resolution Rules

The expected type for the [default_expression](./AA-3.7#S0063) of a [discriminant_specification](./AA-3.7#S0062) is that of the corresponding discriminant. 


#### Legality Rules

{8652/0007} {AI95-00098-01} {AI95-00251-01} A [discriminant_part](./AA-3.7#S0059) is only permitted in a declaration for a composite type that is not an array or interface type [(this includes generic formal types)]. A type declared with a [known_discriminant_part](./AA-3.7#S0061) is called a discriminated type, as is a type that inherits (known) discriminants. 

Implementation Note: Discriminants on array types were considered, but were omitted to ease (existing) implementations. 

Discussion: Note that the above definition for "discriminated type" does not include types declared with an [unknown_discriminant_part](./AA-3.7#S0060). This seems consistent with Ada 83, where such types (in a generic formal part) would not be considered discriminated types. Furthermore, the full type for a type with unknown discriminants need not even be composite, much less have any discriminants.

{8652/0007} {AI95-00098-01} On the other hand, [unknown_discriminant_part](./AA-3.7#S0060)s cannot be applied to type declarations that cannot have a [known_discriminant_part](./AA-3.7#S0061). There is no point in having unknown discriminants on a type that can never have discriminants (for instance, a formal modular type), even when these are allowed syntactically. 

{AI95-00231-01} {AI95-00254-01} The subtype of a discriminant may be defined by an optional [null_exclusion](./AA-3.10#S0083) and a [subtype_mark](./AA-3.2#S0028), in which case the [subtype_mark](./AA-3.2#S0028) shall denote a discrete or access subtype, or it may be defined by an [access_definition](./AA-3.10#S0084). A discriminant that is defined by an [access_definition](./AA-3.10#S0084) is called an access discriminant and is of an anonymous access type. 

This paragraph was deleted.{AI95-00230-01} 

Reason: Note that discriminants of a named access type are not considered "access discriminants". Similarly, "access parameter" only refers to a formal parameter defined by an [access_definition](./AA-3.10#S0084). 

{AI95-00402-01} {AI05-0214-1} [Default_expression](./AA-3.7#S0063)s shall be provided either for all or for none of the discriminants of a [known_discriminant_part](./AA-3.7#S0061). No [default_expression](./AA-3.7#S0063)s are permitted in a [known_discriminant_part](./AA-3.7#S0061) in a declaration of a nonlimited tagged type [or a generic formal type].

Reason: The all-or-none rule is related to the rule that a discriminant constraint shall specify values for all discriminants. One could imagine a different rule that allowed a constraint to specify only some of the discriminants, with the others provided by default. Having defaults for discriminants has a special significance - it allows objects of the type to be unconstrained, with the discriminants alterable as part of assigning to the object.

{AI05-0214-1} Defaults for discriminants of tagged types are disallowed so that every object of a nonlimited tagged type is constrained, either by an explicit constraint, or by its initial discriminant values. This substantially simplifies the semantic rules and the implementation of inherited dispatching operations. We don't need this rule for limited tagged types, as the discriminants of such objects cannot be changed after the object is created in any case - no full-object assignment is supported, and that is required to change discriminant values. For generic formal types, the restriction simplifies the type matching rules. If one simply wants a "default" value for the discriminants, a constrained subtype can be declared for future use. 

{AI95-00230-01} {AI95-00402-01} {AI95-00419-01} {AI05-0063-1} A [discriminant_specification](./AA-3.7#S0062) for an access discriminant may have a [default_expression](./AA-3.7#S0063) only in the declaration for an immutably limited type (see 7.5). In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit. 

Discussion: This rule implies that a type can have a default for an access discriminant if the type is limited, but not if the only reason it's limited is because of a limited component. Compare the definition of limited type and immutably limited type in 7.5. 

Ramification: A (nonformal) limited private type can always have a default for an access discriminant, because having the default itself makes the type immutably limited. Such a private type must necessarily have a full type with the same access discriminant with a default, and thus the full type will always be immutably limited (if legal). 

Reason: {AI95-00230-01} We considered the following rules for access discriminants: 

If a type has an access discriminant, this automatically makes it limited, just like having a limited component automatically makes a type limited. This was rejected because it decreases program readability, and because it seemed error prone (two bugs in a previous version of the RM9X were attributable to this rule).

A type with an access discriminant shall be limited. This is equivalent to the rule we actually chose for Ada 95, except that it allows a type to have an access discriminant if it is limited just because of a limited component. For example, any record containing a task would be allowed to have an access discriminant, whereas the actual rule requires "limited record". This rule was also rejected due to readability concerns, and because would interact badly with the rules for limited types that "become nonlimited".

{AI05-0063-1} A type may have an access discriminant if it is an immutably limited type. This was the rule chosen for Ada 95.

Any type may have an access discriminant. For nonlimited type, there is no special accessibility for access discriminants; they're the same as any other anonymous access component. For a limited type, they have the special accessibility of Ada 95. However, this doesn't work because a limited partial view can have a nonlimited full view -- giving the two views different accessibility.

{AI05-0063-1} Any type may have an access discriminant, as above. However, special accessibility rules only apply to types that are immutably limited (task, protected, and explicitly limited records). However, this breaks privacy; worse, Legality Rules depend on the definition of accessibility.

{AI05-0063-1} Any type may have an access discriminant, as above. Limited types have special accessibility, while nonlimited types have normal accessibility. However, a limited partial view with an access discriminant can only be completed by an immutably limited type. That prevents accessibility from changing. A runtime accessibility check is required on generic formal types with access discriminants. However, changing between limited and nonlimited types would have far-reaching consequences for access discriminants - which is uncomfortable.

Any type may have an access discriminant. All types have special accessibility. This was considered early during the Ada 9X process, but was dropped for "unpleasant complexities", which unfortunately aren't recorded. It does seem that an accessibility check would be needed on assignment of such a type, to avoid copying an object with a discriminant pointing to a local object into a more global object (and thus creating a dangling pointer).

Any type may have an access discriminant, but access discriminants cannot have defaults. All types have special accessibility. This gets rid of the problems on assignment (you couldn't change such a discriminant), but it would be horribly incompatible with Ada 95.

{AI05-0063-1} Any type may have an access discriminant, but access discriminants may have defaults only if they are of an immutably limited type. This is the rule chosen for Ada 2005, as it is not incompatible, and it doesn't require weird accessibility checks. 

This paragraph was deleted.{AI95-00402-01} 

For a type defined by a [derived_type_definition](./AA-3.4#S0035), if a [known_discriminant_part](./AA-3.7#S0061) is provided in its declaration, then: 

The parent subtype shall be constrained;

If the parent type is not a tagged type, then each discriminant of the derived type shall be used in the constraint defining the parent subtype;

Implementation Note: This ensures that the new discriminant can share storage with an existing discriminant.

If a discriminant is used in the constraint defining the parent subtype, the subtype of the discriminant shall be statically compatible (see 4.9.1) with the subtype of the corresponding parent discriminant. 

Reason: This ensures that on conversion (or extension via an extension aggregate) to a distantly related type, if the discriminants satisfy the target type's requirements they satisfy all the intermediate types' requirements as well. 

Ramification: There is no requirement that the new discriminant have the same (or any) [default_expression](./AA-3.7#S0063) as the parent's discriminant. 

This paragraph was deleted.{AI05-0102-1} 

This paragraph was deleted.


#### Static Semantics

A [discriminant_specification](./AA-3.7#S0062) declares a discriminant; the [subtype_mark](./AA-3.2#S0028) denotes its subtype unless it is an access discriminant, in which case the discriminant's subtype is the anonymous access-to-variable subtype defined by the [access_definition](./AA-3.10#S0084).

[For a type defined by a [derived_type_definition](./AA-3.4#S0035), each discriminant of the parent type is either inherited, constrained to equal some new discriminant of the derived type, or constrained to the value of an expression.] When inherited or constrained to equal some new discriminant, the parent discriminant and the discriminant of the derived type are said to correspond. Two discriminants also correspond if there is some common discriminant to which they both correspond. A discriminant corresponds to itself as well. If a discriminant of a parent type is constrained to a specific value by a [derived_type_definition](./AA-3.4#S0035), then that discriminant is said to be specified by that [derived_type_definition](./AA-3.4#S0035). 

Ramification: The correspondence relationship is transitive, symmetric, and reflexive. That is, if A corresponds to B, and B corresponds to C, then A, B, and C each corresponds to A, B, and C in all combinations.

A [constraint](./AA-3.2#S0029) that appears within the definition of a discriminated type depends on a discriminant of the type if it names the discriminant as a bound or discriminant value. A [component_definition](./AA-3.6#S0056) depends on a discriminant if its [constraint](./AA-3.2#S0029) depends on the discriminant, or on a discriminant that corresponds to it. 

Ramification: A [constraint](./AA-3.2#S0029) in a [task_body](./AA-9.1#S0248) is not considered to depend on a discriminant of the task type, even if it names it. It is only the [constraint](./AA-3.2#S0029)s in the type definition itself that are considered dependents. Similarly for protected types. 

A component depends on a discriminant if: 

Its [component_definition](./AA-3.6#S0056) depends on the discriminant; or 

Ramification: A component does not depend on a discriminant just because its [default_expression](./AA-3.7#S0063) refers to the discriminant.

It is declared in a [variant_part](./AA-3.8#S0071) that is governed by the discriminant; or

It is a component inherited as part of a [derived_type_definition](./AA-3.4#S0035), and the [constraint](./AA-3.2#S0029) of the parent_[subtype_indication](./AA-3.2#S0027) depends on the discriminant; or 

Reason: When the parent subtype depends on a discriminant, the parent part of the derived type is treated like a discriminant-dependent component. 

Ramification: Because of this rule, we don't really need to worry about "corresponding" discriminants, since all the inherited components will be discriminant-dependent if there is a new [known_discriminant_part](./AA-3.7#S0061) whose discriminants are used to constrain the old discriminants. 

It is a subcomponent of a component that depends on the discriminant. 

Reason: The concept of discriminant-dependent (sub)components is primarily used in various rules that disallow renaming or 'Access, or specify that certain discriminant-changing assignments are erroneous. The goal is to allow implementations to move around or change the size of discriminant-dependent subcomponents upon a discriminant-changing assignment to an enclosing object. The above definition specifies that all subcomponents of a discriminant-dependent component or parent part are themselves discriminant-dependent, even though their presence or size does not in fact depend on a discriminant. This is because it is likely that they will move in a discriminant-changing assignment if they are a component of one of several discriminant-dependent parts of the same record. 

Each value of a discriminated type includes a value for each component of the type that does not depend on a discriminant[; this includes the discriminants themselves]. The values of discriminants determine which other component values are present in the value of the discriminated type. 

To be honest: Which values are present might depend on discriminants of some ancestor type that are constrained in an intervening [derived_type_definition](./AA-3.4#S0035). That's why we say "values of discriminants" instead of "values of the discriminants" - a subtle point.

A type declared with a [known_discriminant_part](./AA-3.7#S0061) is said to have known discriminants; its first subtype is unconstrained. A type declared with an [unknown_discriminant_part](./AA-3.7#S0060) is said to have unknown discriminants. A type declared without a [discriminant_part](./AA-3.7#S0059) has no discriminants, unless it is a derived type; if derived, such a type has the same sort of discriminants (known, unknown, or none) as its parent (or ancestor) type. A tagged class-wide type also has unknown discriminants. [Any subtype of a type with unknown discriminants is an unconstrained and indefinite subtype (see 3.2 and 3.3).] 

Discussion: {AI95-00114-01} An [unknown_discriminant_part](./AA-3.7#S0060) "(&lt&gt)" is only permitted in the declaration of a (generic or nongeneric) private type, private extension, incomplete type, or formal derived type. Hence, only such types, descendants thereof, and class-wide types can have unknown discriminants. An [unknown_discriminant_part](./AA-3.7#S0060) is used to indicate that the corresponding actual or full type might have discriminants without defaults, or be an unconstrained array subtype. Tagged class-wide types are also considered to have unknown discriminants because discriminants can be added by type extensions, so the total number of discriminants of any given value of a tagged class-wide type is not known at compile time.

{AI95-00287-01} A subtype with unknown discriminants is indefinite, and hence an object of such a subtype needs explicit initialization. A limited private type with unknown discriminants is "extremely" limited; objects of such a type  can be initialized only by subprograms (either procedures with a parameter of the type, or a function returning the type) declared in the package. Subprograms declared elsewhere can operate on and even return the type, but they can only initialize the object by calling (ultimately) a subprogram in the package declaring the type. Such a type is useful for keeping complete control over object creation within the package declaring the type.

A partial view of a type might have unknown discriminants, while the full view of the same type might have known, unknown, or no discriminants. 


#### Dynamic Semantics

{AI95-00230-01} {AI95-00416-01} For an access discriminant, its [access_definition](./AA-3.10#S0084) is elaborated when the value of the access discriminant is defined: by evaluation of its [default_expression](./AA-3.7#S0063), by elaboration of a [discriminant_constraint](./AA-3.7#S0064), or by an assignment that initializes the enclosing object. 

Ramification: {AI95-00231-01} {AI95-00416-01} The conversion of the [expression](./AA-4.4#S0132) defining the access discriminant to the anonymous access type raises Program_Error for an object created by an allocator of an access type T, if the initial value is an access parameter that designates a view whose accessibility level is deeper than that of T. 

NOTE 1   If a discriminated type has [default_expression](./AA-3.7#S0063)s for its discriminants, then unconstrained variables of the type are permitted, and the values of the discriminants can be changed by an assignment to such a variable. If defaults are not provided for the discriminants, then all variables of the type are constrained, either by explicit constraint or by their initial value; the values of the discriminants of such a variable cannot be changed after initialization. 

Discussion: This connection between discriminant defaults and unconstrained variables can be a source of confusion. For Ada 95, we considered various ways to break the connection between defaults and unconstrainedness, but ultimately gave up for lack of a sufficiently simple and intuitive alternative.

An unconstrained discriminated subtype with defaults is called a mutable subtype, and a variable of such a subtype is called a mutable variable, because the discriminants of such a variable can change. There are no mutable arrays (that is, the bounds of an array object can never change), because there is no way in the language to define default values for the bounds. Similarly, there are no mutable class-wide subtypes, because there is no way to define the default tag, and defaults for discriminants are not allowed in the tagged case. Mutable tags would also require a way for the maximum possible size of such a class-wide subtype to be known. (In some implementations, all mutable variables are allocated with the maximum possible size. This approach is appropriate for real-time applications where implicit use of the heap is inappropriate.)

NOTE 2   The [default_expression](./AA-3.7#S0063) for a discriminant of a type is evaluated when an object of an unconstrained subtype of the type is created.

NOTE 3   Assignment to a discriminant of an object (after its initialization) is not allowed, since the name of a discriminant is a constant; neither [assignment_statement](./AA-5.2#S0173)s nor assignments inherent in passing as an in out or out parameter are allowed. Note however that the value of a discriminant can be changed by assigning to the enclosing object, presuming it is an unconstrained variable. 

Discussion: {AI95-00114-01} An [unknown_discriminant_part](./AA-3.7#S0060) is permitted only in the declaration of a private type (including generic formal private), private extension, incomplete type, or generic formal derived type. These are the things that will have a corresponding completion or generic actual, which will either define the discriminants, or say there are none. The (&lt&gt) indicates that the actual/full subtype might be an indefinite subtype. An [unknown_discriminant_part](./AA-3.7#S0060) is not permitted in a normal untagged derived type declaration, because there is no separate full type declaration for such a type. Note that (&lt&gt) allows unconstrained array bounds; those are somewhat like undefaulted discriminants.

For a derived type, either the discriminants are inherited as is, or completely respecified in a new [discriminant_part](./AA-3.7#S0059). In this latter case, each discriminant of the parent type shall be constrained, either to a specific value, or to equal one of the new discriminants. Constraining a parent type's discriminant to equal one of the new discriminants is like a renaming of the discriminant, except that the subtype of the new discriminant can be more restrictive than that of the parent's one. In any case, the new discriminant can share storage with the parent's discriminant. 

NOTE 4   A discriminant that is of a named access type is not called an access discriminant; that term is used only for discriminants defined by an [access_definition](./AA-3.10#S0084). 


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
{AI95-00433-01} {AI05-0229-1} task type Worker(Prio : System.Priority; Buf : access Buffer)
   with Priority =&gt Prio is -- see D.1
   -- discriminants used to parameterize the task type (see 9.1)
   entry Fill;
   entry Drain;
end Worker;

```


#### Extensions to Ada 83

The syntax for a [discriminant_specification](./AA-3.7#S0062) is modified to allow an access discriminant, with a type specified by an [access_definition](./AA-3.10#S0084) (see 3.10).

{AI95-00251-01} Discriminants are allowed on all composite types other than array and interface types.

Discriminants may be of an access type. 


#### Wording Changes from Ada 83

[Discriminant_part](./AA-3.7#S0059)s are not elaborated, though an [access_definition](./AA-3.10#S0084) is elaborated when the discriminant is initialized.


#### Extensions to Ada 95

{AI95-00230-01} {AI95-00402-01} {AI95-00416-01} Access discriminants (anonymous access types used as a discriminant) can be used on any type allowing discriminants. Defaults aren't allowed on discriminants of nonlimited types, however, so that accessibility problems don't happen on assignment.

{AI95-00231-01} [null_exclusion](./AA-3.10#S0083) can be used in the declaration of a discriminant. 


#### Wording Changes from Ada 95

{8652/0007} {AI95-00098-01} Corrigendum: The wording was clarified so that types that cannot have discriminants cannot have an [unknown_discriminant_part](./AA-3.7#S0060).

{AI95-00251-01} Added wording to prevent interfaces from having discriminants. We don't want interfaces to have any components.

{AI95-00254-01} Removed wording which implied or required an access discriminant to have an access-to-object type (anonymous access types can now be access-to-subprogram types as well).

{AI95-00326-01} {AI05-0299-1} Fixed the wording of the introduction to this subclause to reflect that both incomplete and partial views can have unknown discriminants. That was always true, but for some reason this wording specified partial views.

{AI95-00419-01} Changed the wording to use the new term "explicitly limited record", which makes the intent much clearer (and eliminates confusion with derived types that happen to contain the reserved word limited). 


#### Incompatibilities With Ada 2005

{AI05-0063-1} Correction: Changed the rules for when access discriminants can have defaults to depend on the new definition for immutably limited types; this will help ensure that unusual corner cases are properly handled. Note that the Ada 2005 rule was unintentionally incompatible with the Ada 95 rule (as enforced by the ACATS); this change brings it back into alignment with actual practice. So there should be no practical incompatibility. 


#### Extensions to Ada 2005

{AI05-0214-1} A limited tagged type may now have defaults for its discriminants. 


#### Wording Changes from Ada 2005

{AI05-0102-1} Correction: Moved implicit conversion Legality Rule to 8.6. 


#### Extensions to Ada 2012

{AI12-0398-1} Discriminants now can have an [aspect_specification](./AA-13.1#S0346), allowing the specification of (implementation-defined) aspects for individual discriminants. 


## 3.7.1  Discriminant Constraints

A [discriminant_constraint](./AA-3.7#S0064) specifies the values of the discriminants for a given discriminated type. 


#### Language Design Principles

{AI05-0299-1} The rules in this subclause are intentionally parallel to those given in 4.3.1, "Record Aggregates". 


#### Syntax

discriminant_constraint<a id="S0064"></a> ::= 
   ([discriminant_association](./AA-3.7#S0065) {, [discriminant_association](./AA-3.7#S0065)})

{AI12-0212-1} discriminant_association<a id="S0065"></a> ::= 
   [discriminant_[selector_name](./AA-4.1#S0099) {'|' discriminant_[selector_name](./AA-4.1#S0099)} =&gt] [expression](./AA-4.4#S0132)

A [discriminant_association](./AA-3.7#S0065) is said to be named if it has one or more discriminant_[selector_name](./AA-4.1#S0099)s; it is otherwise said to be positional. In a [discriminant_constraint](./AA-3.7#S0064), any positional associations shall precede any named associations. 


#### Name Resolution Rules

Each [selector_name](./AA-4.1#S0099) of a named [discriminant_association](./AA-3.7#S0065) shall resolve to denote a discriminant of the subtype being constrained; the discriminants so named are the associated discriminants of the named association. For a positional association, the associated discriminant is the one whose [discriminant_specification](./AA-3.7#S0062) occurred in the corresponding position in the [known_discriminant_part](./AA-3.7#S0061) that defined the discriminants of the subtype being constrained.

The expected type for the [expression](./AA-4.4#S0132) in a [discriminant_association](./AA-3.7#S0065) is that of the associated discriminant(s). 


#### Legality Rules

{8652/0008} {AI95-00168-01} {AI95-00363-01} {AI05-0041-1} A [discriminant_constraint](./AA-3.7#S0064) is only allowed in a [subtype_indication](./AA-3.2#S0027) whose [subtype_mark](./AA-3.2#S0028) denotes either an unconstrained discriminated subtype, or an unconstrained access subtype whose designated subtype is an unconstrained discriminated subtype. However, in the case of an access subtype, a [discriminant_constraint](./AA-3.7#S0064) is legal only if any dereference of a value of the access type is known to be constrained (see 3.3). In addition to the places where Legality Rules normally apply (see 12.3), these rules apply also in the private part of an instance of a generic unit.

This paragraph was deleted.{8652/0008} {AI95-00168-01} {AI95-00363-01} 

Reason: {AI95-00363-01} The second rule is necessary to prevent objects from changing so that they no longer match their constraint. In Ada 95, we attempted to prevent this by banning every case where an aliased object could be unconstrained or be changed by an enclosing assignment. New ways to cause this problem were being discovered frequently, meaning that new rules had to be dreamed up to cover them. Meanwhile, aliased objects and components were getting more and more limited. In Ada 2005, we sweep away all of that cruft and replace it by a simple rule "thou shalt not create an access subtype that can point to an item whose discriminants can be changed by assignment". 

Discussion: {AI05-0041-1} The second rule will only use the indefinite or dereference bullets in the definition of "known to be constrained". The rule is worded in terms of "known to be constrained" in order to capture the special rules that apply in generic bodies (rather than repeating them and getting them subtly wrong). 

A named [discriminant_association](./AA-3.7#S0065) with more than one [selector_name](./AA-4.1#S0099) is allowed only if the named discriminants are all of the same type. A [discriminant_constraint](./AA-3.7#S0064) shall provide exactly one value for each discriminant of the subtype being constrained.

This paragraph was deleted.{AI05-0102-1} 

Ramification: In addition, 8.6 requires that the [expression](./AA-4.4#S0132) associated with an access discriminant is convertible (see 4.6) to the anonymous access type. This implies both convertibility of designated types, and static accessibility. This implies that if an object of type T with an access discriminant is created by an allocator for an access type A, then it requires that the type of the [expression](./AA-4.4#S0132) associated with the access discriminant have an accessibility level that is not statically deeper than that of A. This is to avoid dangling references.


#### Dynamic Semantics

A [discriminant_constraint](./AA-3.7#S0064) is compatible with an unconstrained discriminated subtype if each discriminant value belongs to the subtype of the corresponding discriminant. 

Ramification: The "dependent compatibility check" has been eliminated in Ada 95. Any checking on subcomponents is performed when (and if) an object is created.

Discussion: There is no need to define compatibility with a constrained discriminated subtype, because one is not allowed to constrain it again.

A composite value satisfies a discriminant constraint if and only if each discriminant of the composite value has the value imposed by the discriminant constraint.

{AI12-0439-1} For the elaboration of a [discriminant_constraint](./AA-3.7#S0064), the [expression](./AA-4.4#S0132)s in the [discriminant_association](./AA-3.7#S0065)s are evaluated in an arbitrary order and converted to the type of the associated discriminant (which can raise Constraint_Error - see 4.6); the [expression](./AA-4.4#S0132) of a named association is evaluated (and converted) once for each associated discriminant. The result of each evaluation and conversion is the value imposed by the constraint for the associated discriminant. 

Reason: We convert to the type, not the subtype, so that the definition of compatibility of discriminant constraints is not vacuous.

NOTE   The rules of the language ensure that a discriminant of an object always has a value, either from explicit or implicit initialization. 

Discussion: Although it is illegal to constrain a class-wide tagged subtype, it is possible to have a partially constrained class-wide subtype: If the subtype S is defined by T(A =&gt B), then S'Class is partially constrained in the sense that objects of subtype S'Class have to have discriminants corresponding to A equal to B, but there can be other discriminants defined in extensions that are not constrained to any particular value. 


#### Examples

{AI05-0299-1} Examples (using types declared above in subclause 3.7): 

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


#### Incompatibilities With Ada 95

{8652/0008} {AI95-00168-01} {AI95-00363-01} The Corrigendum added a restriction on [discriminant_constraint](./AA-3.7#S0064)s for general access subtypes. Such constraints are prohibited if the designated type can be treated as constrained somewhere in the program. Ada 2005 goes further and prohibits such [discriminant_constraint](./AA-3.7#S0064)s if the designated type has (or might have, in the case of a formal type) defaults for its discriminants. The use of general access subtypes is rare, and this eliminates a boatload of problems that required many restrictions on the use of aliased objects and components (now lifted). Similarly, Ada 2005 prohibits [discriminant_constraint](./AA-3.7#S0064)s on any access type whose designated type has a partial view that is constrained. Such a type will not be constrained in the heap to avoid privacy problems. Again, the use of such subtypes is rare (they can only happen within the package and its child units). 


#### Wording Changes from Ada 2005

{AI05-0041-1} Correction: Revised the rules on access subtypes having discriminant constraints to depend on the "known to be constrained" rules. This centralizes the rules so that future fixes need to be made in only one place, as well as fixing bugs in obscure cases.

{AI05-0102-1} Correction: Moved implicit conversion Legality Rule to 8.6. 


## 3.7.2  Operations of Discriminated Types

[If a discriminated type has [default_expression](./AA-3.7#S0063)s for its discriminants, then unconstrained variables of the type are permitted, and the discriminants of such a variable can be changed by assignment to the variable. For a formal parameter of such a type, an attribute is provided to determine whether the corresponding actual parameter is constrained or unconstrained.] 


#### Static Semantics

For a [prefix](./AA-4.1#S0093) A that is of a discriminated type [(after any implicit dereference)], the following attribute is defined: 

A'Constrained{AI05-0214-1} {AI12-0183-1} Yields the value True if A denotes a constant, a value, a tagged object, or a constrained variable, and False otherwise. The value of this attribute is of the predefined type Boolean. 

Implementation Note: {AI05-0214-1} This attribute is primarily used on parameters, to determine whether the discriminants can be changed as part of an assignment. The Constrained attribute is statically True for in parameters. For in out and out parameters of a discriminated type, the value of this attribute needs to be passed as an implicit parameter, in general. However, if the type is tagged or does not have defaults for its discriminants, the attribute is statically True, so no implicit parameter is needed. Parameters of a limited untagged type with defaulted discriminants need this implicit parameter, unless there are no nonlimited views, because they might be passed to a subprogram whose body has visibility on a nonlimited view of the type, and hence might be able to assign to the object and change its discriminants. 

Reason: {AI05-0214-1} {AI12-0005-1} All tagged objects are known to be constrained (as nonlimited tagged types cannot have discriminant defaults, and limited tagged objects are immutably limited), and are always considered constrained by this attribute to avoid distributed overhead for parameters of limited class-wide types, as limited tagged objects may technically be unconstrained if they use defaulted discriminants. Such objects still cannot have their discriminants changed, as assignment is not supported for them, so there is no use for this attribute that would justify the overhead of passing it with all class-wide parameters. 

Discussion: {AI05-0005-1} {AI05-0214-1} If the type of A is a type derived from an untagged partial view of a tagged type such that it is not a tagged type, then A is not considered a tagged object, and A'Constrained can return either True or False depending on the nature of the object. 


#### Erroneous Execution

The execution of a construct is erroneous if the construct has a constituent that is a [name](./AA-4.1#S0091) denoting a subcomponent that depends on discriminants, and the value of any of these discriminants is changed by this execution between evaluating the [name](./AA-4.1#S0091) and the last use (within this execution) of the subcomponent denoted by the [name](./AA-4.1#S0091). 

Ramification: This rule applies to [assignment_statement](./AA-5.2#S0173)s, calls (except when the discriminant-dependent subcomponent is an in parameter passed by copy), [indexed_component](./AA-4.1#S0096)s, and [slice](./AA-4.1#S0097)s. Ada 83 only covered the first two cases. AI83-00585 pointed out the situation with the last two cases. The cases of [object_renaming_declaration](./AA-8.5#S0239)s and generic formal in out objects are handled differently, by disallowing the situation at compile time. 


#### Extensions to Ada 83

For consistency with other attributes, we are allowing the [prefix](./AA-4.1#S0093) of Constrained to be a value as well as an object of a discriminated type, and also an implicit dereference. These extensions are not important capabilities, but there seems no reason to make this attribute different from other similar attributes. We are curious what most Ada 83 compilers do with F(1).X'Constrained.

We now handle in a general way the cases of erroneousness identified by AI83-00585, where the [prefix](./AA-4.1#S0093) of an [indexed_component](./AA-4.1#S0096) or [slice](./AA-4.1#S0097) is discriminant-dependent, and the evaluation of the index or discrete range changes the value of a discriminant. 


#### Wording Changes from Ada 83

We have moved all discussion of erroneous use of [name](./AA-4.1#S0091)s that denote discriminant-dependent subcomponents to this subclause. In Ada 83, it used to appear separately under [assignment_statement](./AA-5.2#S0173)s and subprogram calls. 


#### Wording Changes from Ada 2005

{AI05-0214-1} A'Constrained is now defined to return True for any A that is a tagged object. This doesn't change the result for any A allowed by previous versions of Ada; the change is necessary to avoid unnecessary overhead for limited tagged parameters. 

