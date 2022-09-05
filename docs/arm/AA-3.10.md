---
sidebar_position:  26
---

# 3.10  Access Types

A value of an access type (an access value) provides indirect access to the object or subprogram it designates. Depending on its type, an access value can designate either subprograms, objects created by allocators (see 4.8), or more generally aliased objects of an appropriate type. 

Discussion: A [name](./AA-4.1#S0091) denotes an entity; an access value designates an entity. The "dereference" of an access value X, written "X.all", is a [name](./AA-4.1#S0091) that denotes the entity designated by X. 


#### Language Design Principles

{AI05-0299-1} Access values should always be well defined (barring uses of certain unchecked features of Clause 13). In particular, uninitialized access variables should be prevented by compile-time rules. 


#### Syntax

{AI95-00231-01} access_type_definition<a id="S0079"></a> ::= 
    [[null_exclusion](./AA-3.10#S0083)] [access_to_object_definition](./AA-3.10#S0080)
  | [[null_exclusion](./AA-3.10#S0083)] [access_to_subprogram_definition](./AA-3.10#S0082)

access_to_object_definition<a id="S0080"></a> ::= 
    access [[general_access_modifier](./AA-3.10#S0081)] [subtype_indication](./AA-3.2#S0027)

general_access_modifier<a id="S0081"></a> ::= all | constant

access_to_subprogram_definition<a id="S0082"></a> ::= 
    access [protected] procedure [parameter_profile](./AA-6.1#S0204)
  | access [protected] function  [parameter_and_result_profile](./AA-6.1#S0205)

{AI95-00231-01} null_exclusion<a id="S0083"></a> ::= not null

{AI95-00231-01} {AI95-00254-01} {AI95-00404-01} access_definition<a id="S0084"></a> ::= 
    [[null_exclusion](./AA-3.10#S0083)] access [constant] [subtype_mark](./AA-3.2#S0028)
  | [[null_exclusion](./AA-3.10#S0083)] access [protected] procedure [parameter_profile](./AA-6.1#S0204)
  | [[null_exclusion](./AA-3.10#S0083)] access [protected] function [parameter_and_result_profile](./AA-6.1#S0205)


#### Static Semantics

{8652/0012} {AI95-00062-01} There are two kinds of access types, access-to-object types, whose values designate objects, and access-to-subprogram types, whose values designate subprograms. Associated with an access-to-object type is a storage pool; several access types may share the same storage pool. All descendants of an access type share the same storage pool. A storage pool is an area of storage used to hold dynamically allocated objects (called pool elements) created by allocators[; storage pools are described further in 13.11, "Storage Management"].

Access-to-object types are further subdivided into pool-specific access types, whose values can designate only the elements of their associated storage pool, and general access types, whose values can designate the elements of any storage pool, as well as aliased objects created by declarations rather than allocators, and aliased subcomponents of other objects. 

Implementation Note: The value of an access type will typically be a machine address. However, a value of a pool-specific access type can be represented as an offset (or index) relative to its storage pool, since it can point only to the elements of that pool. 

{AI95-00225-01} {AI95-00363-01} {AI05-0053-1} {AI05-0142-4} {AI05-0277-1} {AI12-0228-1} {AI12-0324-1} A view of an object is defined to be aliased if it is defined by an [object_declaration](./AA-3.3#S0032), [component_definition](./AA-3.6#S0056), [parameter_specification](./AA-6.1#S0207), or [extended_return_object_declaration](./AA-6.5#S0224) with the reserved word aliased, or by a renaming of an aliased view. In addition, the dereference of an access-to-object value denotes an aliased view, as does a view conversion (see 4.6) of an aliased view. A [qualified_expression](./AA-4.7#S0163) denotes an aliased view when the operand denotes an aliased view. The current instance of an immutably limited type (see 7.5) is defined to be aliased. Finally, a formal parameter or generic formal object of a tagged type is defined to be aliased. [Aliased views are the ones that can be designated by an access value.] 

Glossary entry: An aliased view of an object is one that can be designated by an access value. Objects allocated by allocators are aliased. Objects can also be explicitly declared as aliased with the reserved word aliased. The Access attribute can be used to create an access value designating an aliased object.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[aliased view], Def=[a view of an object that can be designated by an access value], Note1=[Objects allocated by allocators are aliased. Objects can also be explicitly declared as aliased with the reserved word aliased. The Access attribute can be used to create an access value designating an aliased object.] 

Ramification: The current instance of a nonlimited type is not aliased.

The object created by an allocator is aliased, but not its subcomponents, except of course for those that themselves have aliased in their [component_definition](./AA-3.6#S0056).

The renaming of an aliased object is aliased.

Slices are never aliased. See 4.1.2 for more discussion. 

Reason: {AI95-00225-01} The current instance of a limited type is defined to be aliased so that an access discriminant of a component can be initialized with T'Access inside the definition of T. Note that we don't want this to apply to a type that could become nonlimited later within its immediate scope, so we require the full definition to be limited.

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

This paragraph was deleted.{AI95-00363-01} 

We considered making more kinds of objects aliased by default. In particular, any object of a by-reference type will pretty much have to be allocated at an addressable location, so it can be passed by reference without using bit-field pointers. Therefore, one might wish to allow the Access and Unchecked_Access attributes for such objects. However, private parts are transparent to the definition of "by-reference type", so if we made all objects of a by-reference type aliased, we would be violating the privacy of private parts. Instead, we would have to define a concept of "visibly by-reference" and base the rule on that. This seemed to complicate the rules more than it was worth, especially since there is no way to declare an untagged limited private type to be by-reference, since the full type might by nonlimited. 

Discussion: Note that we do not use the term "aliased" to refer to formal parameters that are referenced through multiple access paths (see 6.2). 

An [access_to_object_definition](./AA-3.10#S0080) defines an access-to-object type and its first subtype; the [subtype_indication](./AA-3.2#S0027) defines the designated subtype of the access type. If a [general_access_modifier](./AA-3.10#S0081) appears, then the access type is a general access type. If the modifier is the reserved word constant, then the type is an access-to-constant type[; a designated object cannot be updated through a value of such a type]. If the modifier is the reserved word all, then the type is an access-to-variable type[; a designated object can be both read and updated through a value of such a type]. If no [general_access_modifier](./AA-3.10#S0081) appears in the [access_to_object_definition](./AA-3.10#S0080), the access type is a pool-specific access-to-variable type. 

To be honest: The type of the designated subtype is called the designated type. 

Reason: The modifier all was picked to suggest that values of a general access type could point into "all" storage pools, as well as to objects declared aliased, and that "all" access (both read and update) to the designated object was provided. We couldn't think of any use for pool-specific access-to-constant types, so any access type defined with the modifier constant is considered a general access type, and can point into any storage pool or at other (appropriate) aliased objects. 

Implementation Note: The predefined generic Unchecked_Deallocation can be instantiated for any named access-to-variable type. There is no (language-defined) support for deallocating objects designated by a value of an access-to-constant type. Because of this, an allocator for an access-to-constant type can allocate out of a storage pool with no support for deallocation. Frequently, the allocation can be done at link-time, if the size and initial value are known then. 

Discussion: For the purpose of generic formal type matching, the relevant subclasses of access types are access-to-subprogram types, access-to-constant types, and (named) access-to-variable types, with its subclass (named) general access-to-variable types. Pool-specific access-to-variable types are not a separately matchable subclass of types, since they don't have any "extra" operations relative to all (named) access-to-variable types. 

An [access_to_subprogram_definition](./AA-3.10#S0082) defines an access-to-subprogram type and its first subtype; the [parameter_profile](./AA-6.1#S0204) or [parameter_and_result_profile](./AA-6.1#S0205) defines the designated profile of the access type. There is a calling convention associated with the designated profile[; only subprograms with this calling convention can be designated by values of the access type.] By default, the calling convention is "protected" if the reserved word protected appears, and "Ada" otherwise. [See Annex B for how to override this default.] 

Ramification: The calling convention protected is in italics to emphasize that it cannot be specified explicitly by the user. This is a consequence of it being a reserved word. 

Implementation Note: {AI95-00254-01} For a named access-to-subprogram type, the representation of an access value might include implementation-defined information needed to support up-level references - for example, a static link. The accessibility rules (see 3.10.2) ensure that in a "global-display-based" implementation model (as opposed to a static-link-based model), a named access-to-(unprotected)-subprogram value need consist only of the address of the subprogram. The global display is guaranteed to be properly set up any time the designated subprogram is called. Even in a static-link-based model, the only time a static link is definitely required is for an access-to-subprogram type declared in a scope nested at least two levels deep within subprogram or task bodies, since values of such a type might designate subprograms nested a smaller number of levels. For the normal case of a named access-to-subprogram type declared at the outermost (library) level, a code address by itself should be sufficient to represent the access value in many implementations.

For access-to-protected-subprogram, the access values will necessarily include both an address (or other identification) of the code of the subprogram, as well as the address of the associated protected object. This could be thought of as a static link, but it will be needed even for global-display-based implementation models. It corresponds to the value of the "implicit parameter" that is passed into every call of a protected operation, to identify the current instance of the protected type on which they are to operate.

Any Elaboration_Check is performed when a call is made through an access value, rather than when the access value is first "created" via a 'Access. For implementation models that normally put that check at the call-site, an access value will have to point to a separate entry point that does the check. Alternatively, the access value could point to a "subprogram descriptor" that consisted of two words (or perhaps more), the first being the address of the code, the second being the elaboration bit. Or perhaps more efficiently, just the address of the code, but using the trick that the descriptor is initialized to point to a Raise-Program-Error routine initially, and then set to point to the "real" code when the body is elaborated.

For implementations that share code between generic instantiations, the extra level of indirection suggested above to support Elaboration_Checks could also be used to provide a pointer to the per-instance data area normally required when calling shared code. The trick would be to put a pointer to the per-instance data area into the subprogram descriptor, and then make sure that the address of the subprogram descriptor is loaded into a "known" register whenever an indirect call is performed. Once inside the shared code, the address of the per-instance data area can be retrieved out of the subprogram descriptor, by indexing off the "known" register.

This paragraph was deleted.{AI95-00344-01} 

{AI95-00254-01} Note that access parameters of an anonymous access-to-subprogram type are permitted. Such parameters represent full "downward" closures, meaning that in an implementation that uses a per-task (global) display, the display will have to be passed as a hidden parameter, and reconstructed at the point of call. 

{AI95-00230-01} {AI95-00231-01} {AI95-00254-01} {AI05-0264-1} An [access_definition](./AA-3.10#S0084) defines an anonymous general access type or an anonymous access-to-subprogram type. For a general access type, the [subtype_mark](./AA-3.2#S0028) denotes its designated subtype; if the [general_access_modifier](./AA-3.10#S0081) constant appears, the type is an access-to-constant type; otherwise, it is an access-to-variable type. For an access-to-subprogram type, the [parameter_profile](./AA-6.1#S0204) or [parameter_and_result_profile](./AA-6.1#S0205) denotes its designated profile.

{AI95-00230-01} {AI95-00231-01} For each access type, there is a null access value designating no entity at all, which can be obtained by (implicitly) converting the literal null to the access type. [The null value of an access type is the default initial value of the type.] Nonnull values of an access-to-object type are obtained by evaluating an [allocator](./AA-4.8#S0164)[, which returns an access value designating a newly created object (see 3.10.2)], or in the case of a general access-to-object type, evaluating an [attribute_reference](./AA-4.1#S0100) for the Access or Unchecked_Access attribute of an aliased view of an object. Nonnull values of an access-to-subprogram type are obtained by evaluating an [attribute_reference](./AA-4.1#S0100) for the Access attribute of a nonintrinsic subprogram.

This paragraph was deleted.{AI95-00231-01} 

This paragraph was deleted.{AI95-00231-01} 

{AI95-00231-01} A [null_exclusion](./AA-3.10#S0083) in a construct specifies that the null value does not belong to the access subtype defined by the construct, that is, the access subtype excludes null. In addition, the anonymous access subtype defined by the [access_definition](./AA-3.10#S0084) for a controlling access parameter (see 3.9.2) excludes null. Finally, for a [subtype_indication](./AA-3.2#S0027) without a [null_exclusion](./AA-3.10#S0083), the subtype denoted by the [subtype_indication](./AA-3.2#S0027) excludes null if and only if the subtype denoted by the [subtype_mark](./AA-3.2#S0028) in the [subtype_indication](./AA-3.2#S0027) excludes null. 

Reason: {AI95-00231-01} An [access_definition](./AA-3.10#S0084) used in a controlling parameter excludes null because it is necessary to read the tag to dispatch, and null has no tag. We would have preferred to require not null to be specified for such parameters, but that would have been too incompatible with Ada 95 code to require.

{AI95-00416-01} Note that we considered imposing a similar implicit null exclusion for controlling access results, but chose not to do that, because there is no Ada 95 compatibility issue, and there is no automatic null check inherent in the use of a controlling access result. If a null check is necessary, it is because there is a dereference of the result, or because the value is passed to a parameter whose subtype excludes null. If there is no dereference of the result, a null return value is perfectly acceptable, and can be a useful indication of a particular status of the call. 

{8652/0013} {AI95-00012-01} {AI05-0264-1} [All subtypes of an access-to-subprogram type are constrained.] The first subtype of a type defined by an [access_definition](./AA-3.10#S0084) or an [access_to_object_definition](./AA-3.10#S0080) is unconstrained if the designated subtype is an unconstrained array or discriminated subtype; otherwise, it is constrained. 

Proof: The Legality Rules on [range_constraint](./AA-3.5#S0036)s (see 3.5) do not permit the [subtype_mark](./AA-3.2#S0028) of the [subtype_indication](./AA-3.2#S0027) to denote an access-to-scalar type, only a scalar type. The Legality Rules on [index_constraint](./AA-3.6#S0057)s (see 3.6.1) and [discriminant_constraint](./AA-3.7#S0064)s (see 3.7.1) both permit access-to-composite types in a [subtype_indication](./AA-3.2#S0027) with such _[constraint](./AA-3.2#S0029)s. Note that an access-to-access-to-composite is never permitted in a [subtype_indication](./AA-3.2#S0027) with a [constraint](./AA-3.2#S0029). 

Reason: {AI95-00363-01} Only [composite_constraint](./AA-3.2#S0031)s are permitted for an access type, and only on access-to-composite types. A constraint on an access-to-scalar or access-to-access type might be violated due to assignments via other access paths that were not so constrained. By contrast, if the designated subtype is an array or discriminated type without defaults, the constraint could not be violated by unconstrained assignments, since array objects are always constrained, and  discriminated objects are also constrained when the type does not have defaults for its discriminants. Constraints are not allowed on general access-to-unconstrained discriminated types if the type has defaults for its discriminants; constraints on pool-specific access types are usually allowed because allocated objects are usually constrained by their initial value. 


#### Legality Rules

{AI95-00231-01} If a [subtype_indication](./AA-3.2#S0027), [discriminant_specification](./AA-3.7#S0062), [parameter_specification](./AA-6.1#S0207), [parameter_and_result_profile](./AA-6.1#S0205), [object_renaming_declaration](./AA-8.5#S0239), or [formal_object_declaration](./AA-12.4#S0319) has a [null_exclusion](./AA-3.10#S0083), the [subtype_mark](./AA-3.2#S0028) in that construct shall denote an access subtype that does not exclude null. 

To be honest: {AI95-00231-01} This means "directly allowed in"; we are not talking about a [null_exclusion](./AA-3.10#S0083) that occurs in an [access_definition](./AA-3.10#S0084) in one of these constructs (for an [access_definition](./AA-3.10#S0084), the [subtype_mark](./AA-3.2#S0028) in such an [access_definition](./AA-3.10#S0084) is not restricted). 

Reason: {AI95-00231-01} This is similar to doubly constraining a composite subtype, which we also don't allow. 


#### Dynamic Semantics

{AI95-00231-01} A [composite_constraint](./AA-3.2#S0031) is compatible with an unconstrained access subtype if it is compatible with the designated subtype. A [null_exclusion](./AA-3.10#S0083) is compatible with any access subtype that does not exclude null. An access value satisfies a [composite_constraint](./AA-3.2#S0031) of an access subtype if it equals the null value of its type or if it designates an object whose value satisfies the constraint. An access value satisfies an exclusion of the null value if it does not equal the null value of its type.

The elaboration of an [access_type_definition](./AA-3.10#S0079) creates the access type and its first subtype. For an access-to-object type, this elaboration includes the elaboration of the [subtype_indication](./AA-3.2#S0027), which creates the designated subtype.

{AI95-00230-01} {AI95-00254-01} The elaboration of an [access_definition](./AA-3.10#S0084) creates an anonymous access type. 

NOTE 1   Access values are called "pointers" or "references" in some other languages.

NOTE 2   Each access-to-object type has an associated storage pool; several access types can share the same pool. An object can be created in the storage pool of an access type by an [allocator](./AA-4.8#S0164) (see 4.8) for the access type. A storage pool (roughly) corresponds to what some other languages call a "heap". See 13.11 for a discussion of pools.

NOTE 3   Only [index_constraint](./AA-3.6#S0057)s and [discriminant_constraint](./AA-3.7#S0064)s can be applied to access types (see 3.6.1 and 3.7.1). 


#### Examples

Examples of access-to-object types: 

```ada
{AI95-00433-01} {AI12-0056-1} type Frame is access Matrix;    --  see 3.6
type Peripheral_Ref is not null access Peripheral;  --  see 3.8.1
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

The syntax for [access_type_definition](./AA-3.10#S0079) is changed to support general access types (including access-to-constants) and access-to-subprograms. The syntax rules for [general_access_modifier](./AA-3.10#S0081) and [access_definition](./AA-3.10#S0084) are new. 


#### Wording Changes from Ada 83

{AI05-0190-1} We use the term "storage pool" to talk about the data area from which allocation takes place. The term "collection" is only used for finalization. ("Collection" and "storage pool" are not the same thing because multiple unrelated access types can share the same storage pool; see 13.11 for more discussion.) 


#### Inconsistencies With Ada 95

{AI95-00231-01} Access discriminants and noncontrolling access parameters no longer exclude null. A program which passed null to such an access discriminant or access parameter and expected it to raise Constraint_Error may fail when compiled with Ada 2005. One hopes that there no such programs outside of the ACATS. (Of course, a program which actually wants to pass null will work, which is far more likely.)

{AI95-00363-01} Most unconstrained aliased objects with defaulted discriminants are no longer constrained by their initial values. This means that a program that raised Constraint_Error from an attempt to change the discriminants will no longer do so. The change only affects programs that depended on the raising of Constraint_Error in this case, so the inconsistency is unlikely to occur outside of the ACATS. This change may however cause compilers to implement these objects differently, possibly taking additional memory or time. This is unlikely to be worse than the differences caused by any major compiler upgrade. 


#### Incompatibilities With Ada 95

{AI95-00225-01} Amendment Correction: The rule defining when a current instance of a limited type is considered to be aliased has been tightened to apply only to types that cannot become nonlimited. A program that attempts to take 'Access of the current instance of a limited type that can become nonlimited will be illegal in Ada 2005. While original Ada 95 allowed the current instance of any limited type to be treated as aliased, this was inconsistently implemented in compilers, and was likely to not work as expected for types that are ultimately nonlimited.

{AI12-0289-1} Correction: Static matching (see 4.9.1) requires that both anonymous access types exclude null; and full conformance requires statically matching subtypes. Because of the definition of "excludes null" given in this subclause, an access parameter that designates an untagged private type P (which does not exclude null) does not match its completion if P is completed with a tagged type (in that case, the parameter is controlling and thus excludes null, regardless of whether there is an explicit null exclusion on the body). Similar considerations apply in contexts where mode conformance and subtype conformance are required (for instance, subprogram renaming). 


#### Extensions to Ada 95

{AI95-00231-01} The [null_exclusion](./AA-3.10#S0083) is new. It can be used in both anonymous and named access type definitions. It is most useful to declare that parameters cannot be null, thus eliminating the need for checks on use.

{AI95-00231-01} {AI95-00254-01} {AI95-00404-01} The kinds of anonymous access types allowed were increased by adding anonymous access-to-constant and anonymous access-to-subprogram types. Anonymous access-to-subprogram types used as parameters allow passing of subprograms at any level. 


#### Wording Changes from Ada 95

{8652/0012} {AI95-00062-01} Corrigendum: Added accidentally-omitted wording that says that a derived access type shares its storage pool with its parent type. This was clearly intended, both because of a note in 3.4, and because anything else would have been incompatible with Ada 83.

{8652/0013} {AI95-00012-01} Corrigendum: Fixed typographical errors in the description of when access types are constrained.

{AI95-00230-01} The wording was fixed to allow [allocator](./AA-4.8#S0164)s and the literal null for anonymous access types. The former was clearly intended by Ada 95; see the Implementation Advice in 13.11.

{AI95-00363-01} The rules about aliased objects being constrained by their initial values now apply only to allocated objects, and thus have been moved to 4.8, "Allocators". 


#### Wording Changes from Ada 2005

{AI05-0053-1} {AI05-0277-1} Correction: The rule about a current instance being aliased now is worded in terms of immutably limited types. Wording was also added to make extended return object declarations that have the keyword aliased be considered aliased. This latter was a significant oversight in Ada 2005 - technically, the keyword aliased had no effect. But of course implementations followed the intent, not the letter of the Reference Manual.

{AI05-0142-4} Explicitly aliased parameters (see 6.1) are defined to be aliased. 


#### Extensions to Ada 2012

{AI12-0228-1} Correction: A [qualified_expression](./AA-4.7#S0163) of an aliased object is now also aliased. 


## 3.10.1  Incomplete Type Declarations

There are no particular limitations on the designated type of an access type. In particular, the type of a component of the designated type can be another access type, or even the same access type. This permits mutually dependent and recursive access types. An [incomplete_type_declaration](./AA-3.10#S0085) can be used to introduce a type to be used as a designated type, while deferring its full definition to a subsequent [full_type_declaration](./AA-3.2#S0024). 


#### Syntax

{AI95-00326-01} incomplete_type_declaration<a id="S0085"></a> ::= type [defining_identifier](./AA-3.1#S0022) [[discriminant_part](./AA-3.7#S0059)] [is tagged];


#### Static Semantics

{AI95-00326-01} {AI12-0137-1} An [incomplete_type_declaration](./AA-3.10#S0085) declares an incomplete view of a type and its first subtype; the first subtype is unconstrained if a [discriminant_part](./AA-3.7#S0059) appears. If the [incomplete_type_declaration](./AA-3.10#S0085) includes the reserved word tagged, it declares a tagged incomplete view. If T denotes a tagged incomplete view, then T'Class denotes a tagged incomplete view. [An incomplete view of a type is a limited view of the type (see 7.5).]

{AI95-00326-01} Given an access type A whose designated type T is an incomplete view, a dereference of a value of type A also has this incomplete view except when: 

Discussion: {AI05-0208-1} Whether the designated type is an incomplete view (and thus whether this set of rules applies) is determined by the view of the type at the declaration of the access type; it does not change during the life of the type. 

it occurs within the immediate scope of the completion of T, or

{AI05-0208-1} it occurs within the scope of a [nonlimited_with_clause](./AA-10.1#S0296) that mentions a library package in whose visible part the completion of T is declared, or

{AI05-0208-1} it occurs within the scope of the completion of T and T is an incomplete view declared by an [incomplete_type_declaration](./AA-3.10#S0085). 

{AI05-0162-1} In these cases, the dereference has the view of T visible at the point of the dereference. 

Discussion: We need the "in whose visible part" rule so that the second rule doesn't trigger in the body of a package with a with of a child unit: 

```ada
package P is
private
   type T;
   type PtrT is access T;
end P;

```

```ada
private package P.C is
   Ptr : PtrT;
end P.C;

```

```ada
{AI05-0005-1} with P.C;
package body P is
    -- Ptr.all'Size is not legal here, but we are within the scope
    -- of a [nonlimited_with_clause](./AA-10.1#S0296) for P.
type T is ...
    --  Ptr.all'Size is legal here.
end P;

```

{AI95-00412-01} {AI05-0162-1} {AI05-0208-1} Similarly, if a [subtype_mark](./AA-3.2#S0028) denotes a [subtype_declaration](./AA-3.2#S0026) defining a subtype of an incomplete view T, the [subtype_mark](./AA-3.2#S0028) denotes an incomplete view except under the same three circumstances given above, in which case it denotes the view of T visible at the point of the [subtype_mark](./AA-3.2#S0028). 


#### Legality Rules

{AI05-0162-1} An [incomplete_type_declaration](./AA-3.10#S0085) requires a completion, which shall be a [type_declaration](./AA-3.2#S0023) other than an [incomplete_type_declaration](./AA-3.10#S0085). [If the [incomplete_type_declaration](./AA-3.10#S0085) occurs immediately within either the visible part of a [package_specification](./AA-7.1#S0230) or a [declarative_part](./AA-3.11#S0086), then the [type_declaration](./AA-3.2#S0023) shall occur later and immediately within this visible part or [declarative_part](./AA-3.11#S0086). If the [incomplete_type_declaration](./AA-3.10#S0085) occurs immediately within the private part of a given [package_specification](./AA-7.1#S0230), then the [type_declaration](./AA-3.2#S0023) shall occur later and immediately within either the private part itself, or the [declarative_part](./AA-3.11#S0086) of the corresponding [package_body](./AA-7.2#S0231).] 

Proof: This is implied by the next AARM-only rule, plus the rules in 3.11.1, "Completions of Declarations" which require a completion to appear later and immediately within the same declarative region. 

To be honest: If the [incomplete_type_declaration](./AA-3.10#S0085) occurs immediately within the visible part of a [package_specification](./AA-7.1#S0230), then the completing [type_declaration](./AA-3.2#S0023) shall occur immediately within this visible part. 

To be honest: If the implementation supports it, an [incomplete_type_declaration](./AA-3.10#S0085) can be imported (using aspect Import, see B.1), in which case no explicit completion is allowed. 

{AI95-00326-01} {AI05-0162-1} If an [incomplete_type_declaration](./AA-3.10#S0085) includes the reserved word tagged, then a [type_declaration](./AA-3.2#S0023) that completes it shall declare a tagged type. If an [incomplete_type_declaration](./AA-3.10#S0085) has a [known_discriminant_part](./AA-3.7#S0061), then a [type_declaration](./AA-3.2#S0023) that completes it shall have a fully conforming (explicit) [known_discriminant_part](./AA-3.7#S0061) (see 6.3.1). [If an [incomplete_type_declaration](./AA-3.10#S0085) has no [discriminant_part](./AA-3.7#S0059) (or an [unknown_discriminant_part](./AA-3.7#S0060)), then a corresponding [type_declaration](./AA-3.2#S0023) is nevertheless allowed to have discriminants, either explicitly, or inherited via derivation.]

{AI95-00326-01} A [name](./AA-4.1#S0091) that denotes an incomplete view of a type may be used as follows: 

{AI05-0098-1} as the [subtype_mark](./AA-3.2#S0028) in the [subtype_indication](./AA-3.2#S0027) of an [access_to_object_definition](./AA-3.10#S0080); [the only form of [constraint](./AA-3.2#S0029) allowed in this [subtype_indication](./AA-3.2#S0027) is a [discriminant_constraint](./AA-3.7#S0064) [(a [null_exclusion](./AA-3.10#S0083) is not allowed)];] 

Implementation Note: We now allow [discriminant_constraint](./AA-3.7#S0064)s even if the full type is deferred to the package body. However, there is no particular implementation burden because we have dropped the concept of the dependent compatibility check. In other words, we have effectively repealed AI83-00007. 

{AI95-00326-01} {AI95-00412-01} as the [subtype_mark](./AA-3.2#S0028) in the [subtype_indication](./AA-3.2#S0027) of a [subtype_declaration](./AA-3.2#S0026); the [subtype_indication](./AA-3.2#S0027) shall not have a [null_exclusion](./AA-3.10#S0083) or a [constraint](./AA-3.2#S0029); 

{AI95-00326-01} {AI05-0151-1} as the [subtype_mark](./AA-3.2#S0028) in an [access_definition](./AA-3.10#S0084) for an access-to-object type; 

To be honest: This does not mean any random [subtype_mark](./AA-3.2#S0028) in a construct that makes up an [access_definition](./AA-3.10#S0084), such as a [formal_part](./AA-6.1#S0206), just the one given directly in the syntax of [access_definition](./AA-3.10#S0084). 

{AI05-0151-1} as the [subtype_mark](./AA-3.2#S0028) defining the subtype of a parameter or result in a profile occurring within a [basic_declaration](./AA-3.1#S0021); 

Ramification: But not in the profile for a body or entry. 

{AI05-0213-1} as a generic actual parameter whose corresponding generic formal parameter is a formal incomplete type (see 12.5.1). 

{AI95-00326-01} If such a [name](./AA-4.1#S0091) denotes a tagged incomplete view, it may also be used:

{AI95-00326-01} {AI05-0151-1} as the [subtype_mark](./AA-3.2#S0028) defining the subtype of a parameter in the profile for a [subprogram_body](./AA-6.3#S0216), [entry_body](./AA-9.5#S0260), or [accept_statement](./AA-9.5#S0258);

{AI95-00326-01} as the [prefix](./AA-4.1#S0093) of an [attribute_reference](./AA-4.1#S0100) whose [attribute_designator](./AA-4.1#S0101) is Class; such an [attribute_reference](./AA-4.1#S0100) is restricted to the uses allowed here; it denotes a tagged incomplete view. 

This paragraph was deleted.{AI95-00326-01} 

This paragraph was deleted.{AI95-00326-01} {AI05-0151-1} 

This paragraph was deleted.{AI95-00326-01} {AI05-0098-1} {AI05-0151-1} 

This paragraph was deleted.

{AI95-00326-01} If any of the above uses occurs as part of the declaration of a primitive subprogram of the incomplete view, and the declaration occurs immediately within the private part of a package, then the completion of the incomplete view shall also occur immediately within the private part; it shall not be deferred to the package body. 

Reason: This fixes a hole in Ada 95 where a dispatching operation with an access parameter could be declared in a private part and a dispatching call on it could occur in a child even though there is no visibility on the full type, requiring access to the controlling tag without access to the representation of the type. 

{AI95-00326-01} No other uses of a [name](./AA-4.1#S0091) that denotes an incomplete view of a type are allowed.

{AI95-00326-01} {AI05-0151-1} A [prefix](./AA-4.1#S0093) that denotes an object shall not be of an incomplete view. An actual parameter in a call shall not be of an untagged incomplete view. The result object of a function call shall not be of an incomplete view. A [prefix](./AA-4.1#S0093) shall not denote a subprogram having a formal parameter of an untagged incomplete view, nor a return type that is an incomplete view. 

Reason: We used to disallow all dereferences of an incomplete type. Now we only disallow such dereferences when used as a [prefix](./AA-4.1#S0093). Dereferences used in other contexts do not pose a problem since normal type matching will preclude their use except when the full type is "nearby" as context (for example, as the expected type).

This also disallows [prefix](./AA-4.1#S0093)es that are directly of an incomplete view. For instance, a parameter P can be declared of a tagged incomplete type, but we don't want to allow P'Size, P'Alignment, or the like, as representation values aren't known for an incomplete view.

We say "denotes an object" so that prefixes that directly name an incomplete view are not covered; the previous rules cover such cases, and we certainly don't want to ban Incomp'Class.

{AI05-0151-1} As subprogram profiles now may include any kind of incomplete type, we also disallow passing objects of untagged incomplete types in subprogram calls (as the parameter passing method is not known as it is for tagged types) and disallow returning any sort of incomplete objects (since we don't know how big they are). 

{AI12-0155-1} The controlling operand or controlling result of a dispatching call shall not be of an incomplete view if the operand or result is dynamically tagged.

Reason: This rule is needed to prevent the following case:

```ada
package Pack is
   type T is tagged;
   function F return access T'Class;
   function G (X : access T) return Integer;
   I : Integer := G (F);                 -- Illegal by above rule.
   type T is tagged null record;
end Pack;

```

If this was not illegal, the compiler would have to generate a dispatching call on G without necessarily knowing where the tag of type T is stored (the completion of T might not be until the body of Pack). The fact that any such call will raise Program_Error does not absolve us of detecting the problem; see the Language Design Principles in 13.14. 

Paragraph 11 was deleted. 


#### Dynamic Semantics

The elaboration of an [incomplete_type_declaration](./AA-3.10#S0085) has no effect. 

Reason: An incomplete type has no real existence, so it doesn't need to be "created" in the usual sense we do for other types. It is roughly equivalent to a "forward;" declaration in Pascal. Private types are different, because they have a different set of characteristics from their full type. 

NOTE 1   Within a [declarative_part](./AA-3.11#S0086), an [incomplete_type_declaration](./AA-3.10#S0085) and a corresponding [full_type_declaration](./AA-3.2#S0024) cannot be separated by an intervening body. This is because a type has to be completely defined before it is frozen, and a body freezes all types declared prior to it in the same [declarative_part](./AA-3.11#S0086) (see 13.14).

NOTE 2   {AI05-0151-1} {AI05-0269-1} A [name](./AA-4.1#S0091) that denotes an object of an incomplete view is defined to be of a limited type. Hence, the target of an assignment statement cannot be of an incomplete view.


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
{AI95-00433-01} type Person(&lt&gt);    -- incomplete type declaration
type Car is tagged; -- incomplete type declaration

```

```ada
{AI95-00433-01} type Person_Name is access Person;
type Car_Name    is access all Car'Class;

```

```ada
{AI95-00433-01} type Car is tagged
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
{AI12-0312-1} My_Car, Your_Car, Next_Car : Car_Name := new Car;  -- see 4.8
Casey : Person_Name := new Person(M);
   ...
Casey.Vehicle := Your_Car;

```


#### Extensions to Ada 83

The [full_type_declaration](./AA-3.2#S0024) that completes an [incomplete_type_declaration](./AA-3.10#S0085) may have a [known_discriminant_part](./AA-3.7#S0061) even if the [incomplete_type_declaration](./AA-3.10#S0085) does not.

A [discriminant_constraint](./AA-3.7#S0064) may be applied to an incomplete type, even if its completion is deferred to the package body, because there is no "dependent compatibility check" required any more. Of course, the constraint can be specified only if a [known_discriminant_part](./AA-3.7#S0061) was given in the [incomplete_type_declaration](./AA-3.10#S0085). As mentioned in the previous paragraph, that is no longer required even when the full type has discriminants. 


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


#### Incompatibilities With Ada 95

{AI95-00326-01} It is now illegal to use an incomplete view (type) as the parameter or result of an access-to-subprogram type unless the incomplete view is completed in the same declaration list as the use. This was allowed in Ada 95 for incomplete types where the completion was deferred to the body. By disallowing this rare use of incomplete views, we can allow the use of incomplete views in many more places, which is especially valuable for limited views.

{AI95-00326-01} It is now illegal to use an incomplete view (type) in a primitive subprogram of the type unless the incomplete view is completed in the package specification. This was allowed in Ada 95 for incomplete types where the completion was deferred to the body (the use would have to be in an access parameter). This incompatibility was caused by the fix for the hole noted in Legality Rules above. 


#### Extensions to Ada 95

{AI95-00326-01} Tagged incomplete types are new. They are allowed in parameter declarations as well as the usual places, as tagged types are always by-reference types (and thus there can be no code generation issue).

{AI95-00412-01} A [subtype_declaration](./AA-3.2#S0026) can be used to give a new name to an incomplete view of a type. This is valuable to give shorter names to entities imported with a [limited_with_clause](./AA-10.1#S0295). 


#### Wording Changes from Ada 95

{AI95-00326-01} The description of incomplete types as incomplete views is new. Ada 95 defined these as separate types, but neglected to give any rules for matching them with other types. Luckily, implementers did the right thing anyway. This change also makes it easier to describe the meaning of a limited view. 


#### Extensions to Ada 2005

{AI05-0098-1} Correction: Fixed the definition so that an anonymous access-to-subprogram type can use an incomplete view in the same way that a named access-to-subprogram type can.

{AI05-0151-1} Incomplete types now can be used in subprogram declarations. The type has to be complete before any calls or the body is declared. This reduces the places where access types are required for types imported from limited views of packages.

{AI05-0162-1} Incomplete types now can be completed by private types and private extensions. Since this can already happen for limited views, there is no remaining reason to disallow it for explicitly declared incomplete types. 


#### Wording Changes from Ada 2005

{AI05-0208-1} Correction: Changed the rules of uses of dereferences of incomplete views such that it does not introduce an unintentional incompatibility with Ada 83 and Ada 95.

{AI05-0213-1} Incomplete types now can be used as actuals to formal incomplete types (see 12.5.1). 


#### Wording Changes from Ada 2012

{AI12-0137-1} {AI12-0005-1} Corrigendum: Changed the wording to clarify that the class-wide type associated with a specific tagged type that has an incomplete view is also an incomplete view. While this was previously undefined, an interpretation where it is not an incomplete view leads to semantic nonsense, and thus we don't consider this a potential incompatibility, as compilers most likely are doing the right thing.

{AI12-0155-1} Correction: Added a rule preventing a dispatching call on an incomplete type before the completion. This is not an incompatibility as previously this was prevented by the freezing rules. However, that violated the intended design of the freezing rules, so we've now changed to an explicit Legality Rule here, and eliminated the associated freezing rules. 


## 3.10.2  Operations of Access Types

{AI05-0299-1} [The attribute Access is used to create access values designating aliased objects and nonintrinsic subprograms. The "accessibility" rules prevent dangling references (in the absence of uses of certain unchecked features - see Clause 13).] 


#### Language Design Principles

It should be possible for an access value to designate an object declared by an object declaration, or a subcomponent thereof. In implementation terms, this means pointing at stack-allocated and statically allocated data structures. However, dangling references should be prevented, primarily via compile-time rules, so long as features like Unchecked_Access and Unchecked_Deallocation are not used.

In order to create such access values, we require that the access type be a general access type, that the designated object be aliased, and that the accessibility rules be obeyed. 


#### Name Resolution Rules

{AI95-00235-01} For an [attribute_reference](./AA-4.1#S0100) with [attribute_designator](./AA-4.1#S0101) Access (or Unchecked_Access - see 13.10), the expected type shall be a single access type A such that: 

{AI95-00235-01} A is an access-to-object type with designated type D and the type of the [prefix](./AA-4.1#S0093) is D'Class or is covered by D, or

{AI95-00235-01} A is an access-to-subprogram type whose designated profile is type conformant with that of the prefix. 

{AI95-00235-01} [The [prefix](./AA-4.1#S0093) of such an [attribute_reference](./AA-4.1#S0100) is never interpreted as an [implicit_dereference](./AA-4.1#S0095) or a parameterless [function_call](./AA-6.4#S0218) (see 4.1.4).] The designated type or profile of the expected type of the [attribute_reference](./AA-4.1#S0100) is the expected type or profile for the [prefix](./AA-4.1#S0093). 

Discussion: Saying that the expected type shall be a "single access type" is our "new" way of saying that the type has to be determinable from context using only the fact that it is an access type. See 4.2 and 8.6. Specifying the expected profile only implies type conformance. The more stringent subtype conformance is required by a Legality Rule. This is the only Resolution Rule that applies to the [name](./AA-4.1#S0091) in a [prefix](./AA-4.1#S0093) of an [attribute_reference](./AA-4.1#S0100). In all other cases, the [name](./AA-4.1#S0091) has to be resolved without using context. See 4.1.4.

{AI95-00235-01} Saying "single access type" is a bit of a fudge. Both the context and the [prefix](./AA-4.1#S0093) may provide both multiple types; "single" only means that a single, specific interpretation must remain after resolution. We say "single" here to trigger the Legality Rules of 8.6. The resolution of an access attribute is similar to that of an [assignment_statement](./AA-5.2#S0173). For example: 

```ada
type Int_Ptr is access all Integer;
type Char_Ptr is access all Character;
type Float_Ptr is access all Float;

```

```ada
function Zap (Val : Int_Ptr) return Float;   -- (1)
function Zap (Val : Float_Ptr) return Float; -- (2)
function Zop return Int_Ptr;  -- (3)
function Zop return Char_Ptr; -- (4)

```

```ada
Result : Float := Zap (Zop.all'Access); -- Resolves to Zap (1) and Zop (3).

```


#### Static Semantics

{AI95-00162-01} {AI12-0406-1} [The accessibility rules, which prevent dangling references, are written in terms of accessibility levels, which reflect the run-time nesting of masters. As explained in 7.6.1, a master is the execution of a certain construct (called a master construct), such as a [subprogram_body](./AA-6.3#S0216). An accessibility level is deeper than another if it is more deeply nested at run time. For example, an object declared local to a called subprogram has a deeper accessibility level than an object declared local to the calling subprogram. The accessibility rules for access types require that the accessibility level of an object designated by an access value be no deeper than that of the access type. This ensures that the object will live at least as long as the access type, which in turn ensures that the access value cannot later designate an object that no longer exists. The Unchecked_Access attribute may be used to circumvent the accessibility rules.]

Discussion: {AI05-0005-1} The Unchecked_Access attribute acts as if the object was declared at library-level; this applies even when it is used as the value of anonymous access type. See 13.10.

Subclause 3.10.2, home of the accessibility rules, is informally known as the "Heart of Darkness" amongst the maintainers of Ada. Woe unto all who enter here (well, at least unto anyone that needs to understand any of these rules). 

Glossary entry: An accessibility level is a representation of the lifetime of an entity in terms of the level of dynamic nesting within which the entity is known to exist. Dynamic accessibility checks ensure that a reference does not outlive the entity to which it refers, by checking that the level of the entity is no deeper than the level of the reference. Based on static nesting, there are corresponding legality rules that the level of the entity is not statically deeper than that of the reference.

Version=[5],Kind=(Added),Group=[T],Term=[accessibility level], Def=[a representation of the lifetime of an entity in terms of the level of dynamic nesting within which the entity is known to exist] [A given accessibility level is said to be statically deeper than another if the given level is known at compile time (as defined below) to be deeper than the other for all possible executions. In most cases, accessibility is enforced at compile time by Legality Rules. Run-time accessibility checks are also used, since the Legality Rules do not cover certain cases involving access parameters and generic packages.]

{AI12-0345-1} Each master, and each entity and view created by it, has an accessibility level; when two levels are defined to be the same, the accessibility levels of the two associated entities are said to be tied to each other. Accessibility levels are defined as follows: 

The accessibility level of a given master is deeper than that of each dynamically enclosing master, and deeper than that of each master upon which the task executing the given master directly depends (see 9.3).

{AI95-00162-01} {AI95-00416-01} {AI05-0235-1} {AI12-0067-1} {AI12-0089-1} {AI12-0345-1} An entity or view defined by a declaration and created as part of its elaboration has the same accessibility level as the innermost master of the declaration except in the cases of renaming and derived access types described below. A formal parameter of a callable entity has the same accessibility level as the master representing the invocation of the entity. 

Reason: {AI95-00416-01} This rule defines the "normal" accessibility of entities. In the absence of special rules below, we intend for this rule to apply. 

Discussion: {AI95-00416-01} {AI12-0005-1} {AI12-0005-1} This rule defines the accessibility of all named access types, as well as the accessibility level of anonymous access types defined by a [component_definition](./AA-3.6#S0056) Special rules exist for the accessibility level of other anonymous types. Components whose (anonymous) type is defined by an [access_definition](./AA-3.10#S0084) have accessibility levels corresponding to named access types defined at the same point. 

Ramification: {AI95-00230-01} Because accessibility level is determined by where the [access_definition](./AA-3.10#S0084) is elaborated, for a type extension, the anonymous access types of components (other than access discriminants) inherited from the parent have the same accessibility as they did in the parent; those in the extension part have the accessibility determined by the scope where the type extension is declared. Similarly, the types of the nondiscriminant access components of a derived untagged type have the same accessibility as they did in the parent. 

To be honest: {AI05-0235-1} We use "invocation of" in the parameter case as a master is formally an execution of something. But we mean this to be interpreted statically (for instance, as the body of the subprogram) for the purposes of computing "statically deeper than" (see below). 

Ramification: {AI05-0235-1} Note that accessibility can differ depending on the view of an object (for both static and dynamic accessibility). For instance, the accessibility level of a formal parameter may be different than the accessibility level of the corresponding actual parameter. This occurs in other cases as well. 

Reason: {AI05-0235-1} We define the (dynamic) accessibility of formal parameters in order that it does not depend on the parameter passing model (by-reference or by-copy) as that is implementation defined. Otherwise, there would be a portability issue. 

{AI12-0371-1} The accessibility level of a view of an object or subprogram defined by a [renaming_declaration](./AA-8.5#S0238) is the same as that of the renamed view, unless the renaming is of a formal subprogram, in which case the accessibility level is that of the instance.

{AI95-00416-01} The accessibility level of a view conversion, [qualified_expression](./AA-4.7#S0163), or parenthesized expression, is the same as that of the operand.

{AI05-0188-1} {AI12-0292-1} The accessibility level of a [conditional_expression](./AA-4.5#S0148) (see 4.5.7) is the accessibility level of the evaluated dependent_[expression](./AA-4.4#S0132).

{AI12-0236-1} The accessibility level of a [declare_expression](./AA-4.5#S0156) (see 4.5.9) is the accessibility level of the body_[expression](./AA-4.4#S0132).

{AI95-00318-02} {AI95-00416-01} {AI05-0234-1} {AI12-0027-1} The accessibility level of an [aggregate](./AA-4.3#S0106) that is used (in its entirety) to directly initialize part of an object is that of the object being initialized. In other contexts, the accessibility level of an [aggregate](./AA-4.3#S0106) is that of the innermost master that evaluates the [aggregate](./AA-4.3#S0106). Corresponding rules apply to a value conversion (see 4.6).

{AI05-0234-1} The accessibility level of the result of a function call is that of the master of the function call, which is determined by the point of call as follows:

{AI05-0234-1} {AI12-0402-1} If the result type at the point of the function (or access-to-function type) declaration is a composite type, and the result is used (in its entirety) to directly initialize part of an object, the master is that of the object being initialized. In the case where the initialized object is a coextension (see below) that becomes a coextension of another object, the master is that of the eventual object to which the coextension will be transferred.

To be honest: {AI95-00416-01} The first sentence is talking about a static use of the entire return object - a slice that happens to be the entire return object doesn't count. On the other hand, this is intended to allow parentheses and [qualified_expression](./AA-4.7#S0163)s. 

Ramification: {AI95-00416-01} {AI05-0234-1} If the function is used as a [prefix](./AA-4.1#S0093), this bullet does not apply. Similarly, an [assignment_statement](./AA-5.2#S0173) is not an initialization of an object, so this bullet does not apply. 

Reason: {AI12-0402-1} We restrict the above rule to apply only if the function result is declared to be of a composite type. This makes sense since only results of a composite type (or of an anonymous access type, which are handled separately below) are potentially affected by the master of the function call. Note that a private type is considered composite, so this result assumes the worst for a function returning a private type. We could have made the rule more complex, depending on whether the result might be built in place, or might have an access discriminant, but we chose to keep the rule simpler. The wording says "the result type at the point of function declaration is a composite type", to make it clear that this depends on the properties at the point of the declaration of the function, rather than properties that might be known at the point of call or inside the function body. 

{AI05-0234-1} {AI12-0278-1} {AI12-0390-1} If the result is of an anonymous access type and is converted to a (named or anonymous) access type, the master is determined following the rules given below for determining the master of an object created by an allocator (even if the access result is of an access-to-subprogram type);

Ramification: {AI12-0278-1} The conversion can be an explicit type conversion, or an implicit subtype conversion (these happen when anonymous access types are allowed to match named general access types, see 8.6). 

This paragraph was deleted.{AI05-0234-1} {AI12-0390-1} 

{AI05-0234-1} {AI12-0345-1} {AI12-0372-1} If the call itself defines the result of a function F, or has an accessibility level that is tied to the result of such a function F, then the master of the call is that of the master of the call invoking F;

{AI05-0234-1} In other cases, the master of the call is that of the innermost master that evaluates the function call.

Ramification: {AI95-00318-02} {AI95-00416-01} The "innermost master which evaluated the function call" does not include the function call itself (which might be a master).

{AI95-00318-02} {AI95-00416-01} We really mean the innermost master here, which could be a very short lifetime. Consider a function call used as a parameter of a procedure call. In this case the innermost master which evaluated the function call is the procedure call. 

Ramification: {AI05-0234-1} These rules do not mention whether the result object is built-in-place (see 7.6). In particular, in the case where building in place is optional, the choice whether or not to build-in-place has no effect on masters, lifetimes, or accessibility. 

Implementation Note: {AI05-0234-1} There are several cases where the implementation may have to pass in the accessibility level of the result object on a call, to support later rules where the accessibility level comes from the master of the call:

when the function result may have a part with access discriminants;

when the function result type is an anonymous access type;

{AI12-0345-1} when the function result is built-in-place.

This paragraph was deleted.{AI12-0345-1} 

In particular, this implies passing a level parameter when the result type is class-wide, since descendants may add access discriminants. For most implementations this will mean that functions with controlling results will also need a level parameter. 

{AI05-0284-1} In the case of a call to a function whose result type is an anonymous access type, the accessibility level of the type of the result of the function call is also determined by the point of call as described above.

{AI95-00416-01} Within a return statement, the accessibility level of the return object is that of the execution of the return statement. If the return statement completes normally by returning from the function, then prior to leaving the function, the accessibility level of the return object changes to be a level determined by the point of call, as does the level of any coextensions (see below) of the return object.

Reason: We define the accessibility level of the return object during the return statement to be that of the return statement itself so that the object may be designated by objects local to the return statement, but not by objects outside the return statement. In addition, the intent is that the return object gets finalized if the return statement ends without actually returning (for example, due to propagating an exception, or a goto). For a normal return, of course, no finalization is done before returning. 

The accessibility level of a derived access type is the same as that of its ultimate ancestor.

{AI95-00230-01} The accessibility level of the anonymous access type defined by an [access_definition](./AA-3.10#S0084) of an [object_renaming_declaration](./AA-8.5#S0239) is the same as that of the renamed view.

{AI12-0156-1} The accessibility level of the anonymous access type defined by an [access_definition](./AA-3.10#S0084) of a [loop_parameter_subtype_indication](./AA-5.5#S0184) is that of the loop parameter.

{AI95-00230-01} {AI95-00416-01} The accessibility level of the anonymous access type of an access discriminant in the [subtype_indication](./AA-3.2#S0027) or [qualified_expression](./AA-4.7#S0163) of an [allocator](./AA-4.8#S0164), or in the [expression](./AA-4.4#S0132) or [return_subtype_indication](./AA-6.5#S0226) of a return statement is determined as follows:

If the value of the access discriminant is determined by a [discriminant_association](./AA-3.7#S0065) in a [subtype_indication](./AA-3.2#S0027), the accessibility level of the object or subprogram designated by the associated value (or library level if the value is null); 

Discussion: This deals with the following cases, when they occur in the context of an [allocator](./AA-4.8#S0164) or return statement: 

An [extension_aggregate](./AA-4.3#S0111) where the [ancestor_part](./AA-4.3#S0112) is a [subtype_mark](./AA-3.2#S0028) denoting a constrained subtype;

An uninitialized [allocator](./AA-4.8#S0164) where the [subtype_indication](./AA-3.2#S0027) defines a constrained subtype;

A discriminant of an object with a constrained nominal subtype, including constrained components, the result of calling a function with a constrained result subtype, the dereference of an access-to-constrained subtype, etc. 

Ramification: {AI05-0281-1} The [subtype_indication](./AA-3.2#S0027) mentioned in this bullet is not necessarily the one given in the [allocator](./AA-4.8#S0164) or return statement that is determining the accessibility level; the constrained subtype might have been defined in an earlier declaration (as a named subtype).

{AI05-0005-1} If the value for this rule and the next one is derived from an Unchecked_Access attribute, the accessibility is library-level no matter what the accessibility level of the object is (see 13.10). 

{AI05-0234-1} If the value of the access discriminant is determined by a [default_expression](./AA-3.7#S0063) in the declaration of the discriminant, the level of the object or subprogram designated by the associated value (or library level if null); 

Discussion: This covers the case of an unconstrained subcomponent of a limited type with defaulted access discriminants. 

{AI05-0004-1} If the value of the access discriminant is determined by a [record_component_association](./AA-4.3#S0109) in an [aggregate](./AA-4.3#S0106), the accessibility level of the object or subprogram designated by the associated value (or library level if the value is null);

Discussion: In this bullet, the [aggregate](./AA-4.3#S0106) has to occur in the context of an [allocator](./AA-4.8#S0164) or return statement, while the [subtype_indication](./AA-3.2#S0027) of the previous bullet can occur anywhere (it doesn't have to be directly given in the [allocator](./AA-4.8#S0164) or return statement). 

In other cases, where the value of the access discriminant is determined by an object with an unconstrained nominal subtype, the accessibility level of the object.

Discussion: {AI95-00416-01} In other words, if you know the value of the discriminant for an [allocator](./AA-4.8#S0164) or return statement from a discriminant constraint or an [aggregate](./AA-4.3#S0106) component association, then that determines the accessibility level; if you don't know it, then it is based on the object itself. 

{AI95-00416-01} The accessibility level of the anonymous access type of an access discriminant in any other context is that of the enclosing object.

{AI95-00162-01} {AI95-00254-01} {AI05-0270-1} The accessibility level of the anonymous access type of an access parameter specifying an access-to-object type is the same as that of the view designated by the actual (or library-level if the actual is null). 

Ramification: {AI05-0005-1} If the value of the actual is derived from an Unchecked_Access attribute, the accessibility is always library-level (see 13.10). 

{AI95-00254-01} The accessibility level of the anonymous access type of an access parameter specifying an access-to-subprogram type is deeper than that of any master; all such anonymous access types have this same level. 

Reason: These represent "downward closures" and thus require passing of static links or global display information (along with generic sharing information if the implementation does sharing) along with the address of the subprogram. We must prevent conversions of these to types with "normal" accessibility, as those typically don't include the extra information needed to make a call. 

{AI12-0070-1} The accessibility level of the anonymous access subtype defined by a [return_subtype_indication](./AA-6.5#S0226) that is an [access_definition](./AA-3.10#S0084) (see 6.5) is that of the result subtype of the enclosing function.

{AI05-0148-1} {AI05-0240-1} {AI12-0070-1} The accessibility level of the type of a stand-alone object of an anonymous access-to-object type is the same as the accessibility level of the type of the access value most recently assigned to the object[; accessibility checks ensure that this is never deeper than that of the declaration of the stand-alone object].

Proof: {AI12-0005-1} Conversions into the anonymous access type of a stand-alone object use a stricter (static) accessibility rule - see 4.6; these checks are the ones referred to above. 

{AI95-00416-01} {AI05-0051-1} {AI05-0253-1} The accessibility level of an object created by an [allocator](./AA-4.8#S0164) is the same as that of the access type, except for an [allocator](./AA-4.8#S0164) of an anonymous access type (an anonymous allocator) in certain contexts, as follows: For an anonymous allocator that defines the result of a function with an access result, the accessibility level is determined as though the [allocator](./AA-4.8#S0164) were in place of the call of the function; in the special case of a call that is the operand of a type conversion, the level is that of the target access type of the conversion. For an anonymous allocator defining the value of an access parameter, the accessibility level is that of the innermost master of the call. For an anonymous allocator whose type is that of a stand-alone object of an anonymous access-to-object type, the accessibility level is that of the declaration of the stand-alone object. For one defining an access discriminant, the accessibility level is determined as follows:

{AI95-00416-01} {AI05-0024-1} for an [allocator](./AA-4.8#S0164) used to define the discriminant of an object, the level of the object;

{AI95-00416-01} {AI05-0024-1} for an [allocator](./AA-4.8#S0164) used to define the constraint in a [subtype_indication](./AA-3.2#S0027) in any other context, the level of the master that elaborates the [subtype_indication](./AA-3.2#S0027).

This paragraph was deleted.{AI95-00416-01} {AI05-0024-1} 

{AI95-00416-01} {AI05-0024-1} {AI05-0066-1} In the first case, the allocated object is said to be a coextension of the object whose discriminant designates it, as well as of any object of which the discriminated object is itself a coextension or subcomponent. If the allocated object is a coextension of an anonymous object representing the result of an aggregate or function call that is used (in its entirety) to directly initialize a part of an object, after the result is assigned, the coextension becomes a coextension of the object being initialized and is no longer considered a coextension of the anonymous object. All coextensions of an object [(which have not thus been transfered by such an initialization)] are finalized when the object is finalized (see 7.6.1). 

Ramification: The rules of access discriminants are such that when the space for an object with a coextension is reclaimed, the space for the coextensions can be reclaimed. Hence, there is implementation advice (see 13.11) that an object and its coextensions all be allocated from the same storage pool (or stack frame, in the case of a declared object). 

{AI05-0051-1} Within a return statement, the accessibility level of the anonymous access type of an access result is that of the master of the call.

{AI05-0014-1} The accessibility level of a view of an object or subprogram designated by an access value is the same as that of the access type. 

Discussion: {AI05-0005-1} {AI05-0014-1} This rule applies even when no dereference exists, for example when an access value is passed as an access parameter. This rule ensures that implementations are not required to include dynamic accessibility values with all access values. 

The accessibility level of a component, protected subprogram, or entry of (a view of) a composite object is the same as that of (the view of) the composite object. 

{AI95-00416-01} {AI05-0262-1} {AI12-0236-1} {AI12-0317-1} In the above rules, the operative constituents of a [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132) (see 4.4) are considered to be used in a given context if the enclosing [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132) is used in that context.

Discussion: This means that constructs like parenthesized expressions, [qualified_expression](./AA-4.7#S0163)s, and [conditional_expression](./AA-4.5#S0148)s are ignored for the purposes of calculating accessibility levels, determining the master of a function call, and so on. 

One accessibility level is defined to be statically deeper than another in the following cases: 

{AI12-0406-1} For a master construct that is statically nested within another master construct, the accessibility level of the inner master construct is statically deeper than that of the outer master construct. 

This paragraph was deleted.{AI12-0406-1} 

To be honest: If a given accessibility level is statically deeper than another, then each level defined to be the same as the given level is statically deeper than each level defined to be the same as the other level. 

{AI95-00254-01} The accessibility level of the anonymous access type of an access parameter specifying an access-to-subprogram type is statically deeper than that of any master; all such anonymous access types have this same level. 

Ramification: This rule means that it is illegal to convert an access parameter specifying an access to subprogram to a named access to subprogram type, but it is allowed to pass such an access parameter to another access parameter (the implicit conversion's accessibility will succeed). 

{AI95-00254-01} {AI05-0082-1} {AI12-0406-1} The statically deeper relationship does not apply to the accessibility level of the following:

{AI12-0406-1} the anonymous type of an access parameter specifying an access-to-object type;

{AI12-0406-1} the type of a stand-alone object of an anonymous access-to-object type;

{AI12-0392-1} {AI12-0406-1} a [raise_expression](./AA-11.3#S0309);

{AI12-0406-1} a descendant of a generic formal type;

{AI12-0406-1} a descendant of a type declared in a generic formal package. 

{AI05-0148-1} {AI12-0406-1} When the statically deeper relationship does not apply, the accessibility level is not considered to be statically deeper, nor statically shallower, than any other.

Ramification: {AI12-0005-1} In these cases, no static accessibility checks are made, and we use dynamic accessibility checks to prevent problems. 

This paragraph was deleted.{AI05-0142-4} {AI05-0235-1} {AI12-0089-1} {AI12-0157-1} {AI12-0277-1} {AI12-0324-1} {AI12-0345-1} 

{AI05-0051-1} {AI05-0234-1} {AI05-0235-1} {AI12-0089-1} {AI12-0157-1} {AI12-0372-1} When within a function body or the return expression of an expression function, the accessibility level of the master representing an execution of the function is statically deeper than that of the master of the function call invoking that execution[, independent of how the master of the function call is determined (see above)].

This paragraph was deleted.{AI05-0235-1} {AI12-0345-1} 

To be honest: {AI12-0005-1} "Function body" includes bodies of generic functions. 

{AI12-0445-1} [For determining whether one level is statically deeper than another when within a generic package body, the generic package is presumed to be instantiated at the same level as where it was declared; runtime checks are required in the case of more deeply nested instantiations.] 

Proof: {AI05-0082-1} A generic package does not introduce a new master, so it has the static level of its declaration; the rest follows from the other "statically deeper" rules. 

For determining whether one level is statically deeper than another when within the declarative region of a [type_declaration](./AA-3.2#S0023), the current instance of the type is presumed to be an object created at a deeper level than that of the type. 

Ramification: In other words, the rules are checked at compile time of the [type_declaration](./AA-3.2#S0023), in an assume-the-worst manner. 

{AI12-0345-1} Notwithstanding other rules given above, the accessibility level of an entity that is tied to that of an explicitly aliased formal parameter of an enclosing function is considered (both statically and dynamically) to be the same as that of an entity whose accessibility level is tied to that of the return object of that function.

Ramification: This rule only applies when the level of an explicitly aliased parameter of a function is compared to that of the return object of the function. If a value designating the explicitly aliased parameter is created and stored in a stand-alone object or passed as a parameter, this special property is lost (even for the dynamic accessibility of anonymous access types in these contexts). 

Implementation Note: When this rule applies, no dynamic accessibility check should be made, even when one would normally be part of the execution of the construct. All places where this rule applies are known at compile-time. 

The accessibility level of all library units is called the library level; a library-level declaration or entity is one whose accessibility level is the library level. 

Ramification: [Library_unit_declaration](./AA-10.1#S0288)s are library level. Nested declarations are library level if they are nested only within packages (possibly more than one), and not within subprograms, tasks, etc. 

To be honest: The definition of the accessibility level of the anonymous type of an access parameter specifying an access-to-object type cheats a bit, since it refers to the view designated by the actual, but access values designate objects, not views of objects. What we really mean is the view that "would be" denoted by an expression "X.all", where X is the actual, even though such an expression is a figment of our imagination. The definition is intended to be equivalent to the following more verbose version: The accessibility level of the anonymous type of an access parameter is as follows: 

if the actual is an expression of a named access type - the accessibility level of that type;

if the actual is an [allocator](./AA-4.8#S0164) - the accessibility level of the execution of the called subprogram;

if the actual is a reference to the Access attribute - the accessibility level of the view denoted by the [prefix](./AA-4.1#S0093);

if the actual is a reference to the Unchecked_Access attribute - library accessibility level;

if the actual is an access parameter - the accessibility level of its type. 

Note that the [allocator](./AA-4.8#S0164) case is explicitly mentioned in the RM95, because otherwise the definition would be circular: the level of the anonymous type is that of the view designated by the actual, which is that of the access type. 

Discussion: A deeper accessibility level implies a shorter maximum lifetime. Hence, when a rule requires X to have a level that is "not deeper than" Y's level, this requires that X has a lifetime at least as long as Y. (We say "maximum lifetime" here, because the accessibility level really represents an upper bound on the lifetime; an object created by an [allocator](./AA-4.8#S0164) can have its lifetime prematurely ended by an instance of Unchecked_Deallocation.)

Package elaborations are not masters, and are therefore invisible to the accessibility rules: an object declared immediately within a package has the same accessibility level as an object declared immediately within the declarative region containing the package. This is true even in the body of a package; it jibes with the fact that objects declared in a [package_body](./AA-7.2#S0231) live as long as objects declared outside the package, even though the body objects are not visible outside the package.

Note that the level of the view denoted by X.all can be different from the level of the object denoted by X.all. The former is determined by the type of X; the latter is determined either by the type of the [allocator](./AA-4.8#S0164), or by the master in which the object was declared. The former is used in several Legality Rules and runtime checks; the latter is used to define when X.all gets finalized. The level of a view reflects what we can conservatively "know" about the object of that view; for example, due to [type_conversion](./AA-4.6#S0162)s, an access value might designate an object that was allocated by an [allocator](./AA-4.8#S0164) for a different access type.

Similarly, the level of the view denoted by X.all.Comp can be different from the level of the object denoted by X.all.Comp.

If Y is statically deeper than X, this implies that Y will be (dynamically) deeper than X in all possible executions.

Most accessibility checking is done at compile time; the rules are stated in terms of "statically deeper than". The exceptions are: 

Checks involving access parameters of an access-to-object type. The fact that "statically deeper than" is not defined for the anonymous access type of an access parameter implies that any rule saying "shall not be statically deeper than" does not apply to such a type, nor to anything defined to have "the same" level as such a type.

{AI05-0082-1} Checks involving generic formal types and their descendants. This is because the actual type can be more or less deeply nested than the generic unit. Note that this only applies to the generic unit itself, and not to the instance. Any static checks needed in the instance will be performed. Any other checks (such as those in the generic body) will require a run-time check of some sort (although implementations that macro-expand generics can determine the result of the check when the generic is expanded).

{AI05-0082-1} Checks involving other entities and views within generic packages. This is because an instantiation can be at a level that is more deeply nested than the generic package itself. In implementations that use a macro-expansion model of generics, these violations can be detected at macro-expansion time. For implementations that share generics, run-time code is needed to detect the error.

{AI95-00318-02} {AI95-00344-01} {AI95-00416-01} Checks during function return and [allocator](./AA-4.8#S0164)s, for nested type extensions and access discriminants. 

{AI05-0005-1} Note that runtime checks are not required for access discriminants (except during function returns and [allocator](./AA-4.8#S0164)s), because their accessibility is determined statically by the accessibility level of the enclosing object.

The accessibility level of the result object of a function reflects the time when that object will be finalized; we don't allow pointers to the object to survive beyond that time.

We sometimes use the terms "accessible" and "inaccessible" to mean that something has an accessibility level that is not deeper, or deeper, respectively, than something else. 

Implementation Note: {AI95-00318-02} {AI95-00344-01} {AI95-00416-01} If an accessibility Legality Rule is satisfied, then the corresponding runtime check (if any) cannot fail (and a reasonable implementation will not generate any checking code) unless one of the cases requiring runtime checks mentioned previously is involved.

Accessibility levels are defined in terms of the relations "the same as" and "deeper than". To make the discussion more concrete, we can assign actual numbers to each level. Here, we assume that library-level accessibility is level 0, and each level defined as "deeper than" is one level deeper. Thus, a subprogram directly called from the environment task (such as the main subprogram) would be at level 1, and so on.

Accessibility is not enforced at compile time for access parameters of an access-to-object type. The "obvious" implementation of the runtime checks would be inefficient, and would involve distributed overhead; therefore, an efficient method is given below. The "obvious" implementation would be to pass the level of the caller at each subprogram call, task creation, etc. This level would be incremented by 1 for each dynamically nested master. An Accessibility_Check would be implemented as a simple comparison - checking that X is not deeper than Y would involve checking that X &lt= Y.

A more efficient method is based on passing static nesting levels (within constructs that correspond at run time to masters - packages don't count). Whenever an access parameter is passed, an implicit extra parameter is passed with it. The extra parameter represents (in an indirect way) the accessibility level of the anonymous access type, and, therefore, the level of the view denoted by a dereference of the access parameter. This is analogous to the implicit "Constrained" bit associated with certain formal parameters of an unconstrained but definite composite subtype. In this method, we avoid distributed overhead: it is not necessary to pass any extra information to subprograms that have no access parameters. For anything other than an access parameter and its anonymous type, the static nesting level is known at compile time, and is defined analogously to the RM95 definition of accessibility level (e.g. derived access types get their nesting level from their parent). Checking "not deeper than" is a "&lt=" test on the levels.

For each access parameter of an access-to-object type, the static depth passed depends on the actual, as follows: 

If the actual is an expression of a named access type, pass the static nesting level of that type.

If the actual is an [allocator](./AA-4.8#S0164), pass the static nesting level of the caller, plus one.

If the actual is a reference to the Access attribute, pass the level of the view denoted by the [prefix](./AA-4.1#S0093).

If the actual is a reference to the Unchecked_Access attribute, pass 0 (the library accessibility level).

If the actual is an access parameter of an access-to-object type, usually just pass along the level passed in. However, if the static nesting level of the formal (access) parameter is greater than the static nesting level of the actual (access) parameter, the level to be passed is the minimum of the static nesting level of the access parameter and the actual level passed in. 

For the Accessibility_Check associated with a [type_conversion](./AA-4.6#S0162) of an access parameter of an access-to-object type of a given subprogram to a named access type, if the target type is statically nested within the subprogram, do nothing; the check can't fail in this case. Otherwise, check that the value passed in is &lt= the static nesting depth of the target type. The other Accessibility_Checks are handled in a similar manner.

This method, using statically known values most of the time, is efficient, and, more importantly, avoids distributed overhead.

{AI05-0148-1} The implementation of accessibility checks for stand-alone objects of anonymous access-to-object types can be similar to that for anonymous access-to-object parameters. A static level suffices; it can be calculated using rules similar to those previously described for access parameters.

{AI05-0148-1} One important difference between the stand-alone access variables and access parameters is that one can assign a local access parameter to a more global stand-alone access variable. Similarly, one can assign a more global access parameter to a more local stand-alone access variable.

{AI05-0148-1} For these cases, it is important to note that the "correct" static accessibility level for an access parameter assigned to a stand-alone access object is the minimum of the passed in level and the static accessibility level of the stand-alone object itself. This is true since the static accessibility level passed in might be deeper than that of the stand-alone object, but the dynamic accessibility of the passed in object clearly must be shallower than the stand-alone object (whatever is passed in must live at least as long as the subprogram call). We do not need to keep a more local static level as accesses to objects statically deeper than the stand-alone object cannot be stored into the stand-alone object. 

Discussion: Examples of accessibility: 

```ada
{AI05-0005-1} package body Lib_Unit is
    type T is tagged ...;
    type A0 is access all T;
    Global: A0 := ...;
    procedure P(X: in out T) is
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

{AI05-0005-1} The above illegal statements are illegal because the accessibility levels of X and Y are statically deeper than the accessibility level of A0. In every possible execution of any program including this library unit, if P is called, the accessibility level of X will be (dynamically) deeper than that of A0. Note that the accessibility levels of X and Y are the same.

Here's an example involving access parameters of an access-to-object type: 

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

Within a [type_declaration](./AA-3.2#S0023), the rules are checked in an assume-the-worst manner. For example: 

```ada
{AI05-0298-1} package P is
    type Int_Ptr is access all Integer;
    type Rec(D: access Integer) is limited private;
private
    type Rec_Ptr is access all Rec;
    function F(X: Rec_Ptr) return Boolean;
    function G(X: access Rec) return Boolean;
    type Rec(D: access Integer) is
        limited record
            C1: Int_Ptr := Int_Ptr(D); -- Illegal!
            C2: Rec_Ptr := Rec'Access; -- Illegal!
            C3: Boolean := F(Rec'Access); -- Illegal!
            C4: Boolean := G(Rec'Access);
        end record;
end P;

```

C1, C2, and C3 are all illegal, because one might declare an object of type Rec at a more deeply nested place than the declaration of the type. C4 is legal, but the accessibility level of the object will be passed to function G, and constraint checks within G will prevent it from doing any evil deeds.

Note that we cannot defer the checks on C1, C2, and C3 until compile-time of the object creation, because that would cause violation of the privacy of private parts. Furthermore, the problems might occur within a task or protected body, which the compiler can't see while compiling an object creation. 

The following attribute is defined for a [prefix](./AA-4.1#S0093) X that denotes an aliased view of an object: 

X'Access{8652/0010} {AI95-00127-01} X'Access yields an access value that designates the object denoted by X. The type of X'Access is an access-to-object type, as determined by the expected type. The expected type shall be a general access type. X shall denote an aliased view of an object[, including possibly the current instance (see 8.6) of a limited type within its definition, or a formal parameter or generic formal object of a tagged type]. The view denoted by the [prefix](./AA-4.1#S0093) X shall satisfy the following additional requirements, presuming the expected type for X'Access is the general access type A with designated type D: 

If A is an access-to-variable type, then the view shall be a variable; [on the other hand, if A is an access-to-constant type, the view may be either a constant or a variable.] 

Discussion: The current instance of a limited type is considered a variable. 

{AI95-00363-01} {AI05-0008-1} {AI05-0041-1} The view shall not be a subcomponent that depends on discriminants of an object unless the object is known to be constrained.

Discussion: This restriction is intended to be similar to the restriction on renaming discriminant-dependent subcomponents. 

Reason: This prevents references to subcomponents that might disappear or move or change constraints after creating the reference. 

Implementation Note: There was some thought to making this restriction more stringent, roughly: "X shall not denote a subcomponent of a variable with discriminant-dependent subcomponents, if the nominal subtype of the variable is an unconstrained definite subtype." This was because in some implementations, it is not just the discriminant-dependent subcomponents that might move as the result of an assignment that changed the discriminants of the enclosing object. However, it was decided not to make this change because a reasonable implementation strategy was identified to avoid such problems, as follows: 

Place nondiscriminant-dependent components with any aliased parts at offsets preceding any discriminant-dependent components in a discriminated record type with defaulted discriminants.

Preallocate the maximum space for unconstrained discriminated variables with aliased subcomponents, rather than allocating the initial size and moving them to a larger (heap-resident) place if they grow as the result of an assignment. 

Note that for objects of a by-reference type, it is not an error for a programmer to take advantage of the fact that such objects are passed by reference. Therefore, the above approach is also necessary for discriminated record types with components of a by-reference type.

To make the above strategy work, it is important that a component of a derived type is defined to be discriminant-dependent if it is inherited and the parent subtype constraint is defined in terms of a discriminant of the derived type (see 3.7). 

{8652/0010} {AI95-00127-01} {AI95-00363-01} If A is a named access type and D is a tagged type, then the type of the view shall be covered by D; if A is anonymous and D is tagged, then the type of the view shall be either D'Class or a type covered by D; if D is untagged, then the type of the view shall be D, and either: 

{AI95-00363-01} the designated subtype of A shall statically match the nominal subtype of the view; or

{AI95-00363-01} {AI05-0041-1} {AI12-0095-1} D shall be discriminated in its full view and unconstrained in any partial view, and the designated subtype of A shall be unconstrained. 

Implementation Note: This ensures that the dope for an aliased array object can always be stored contiguous with it, but need not be if its nominal subtype is constrained. 

Ramification: {8652/0010} {AI95-00127-01} An access attribute can be used as the controlling operand in a dispatching call; see 3.9.2.

{AI95-00363-01} This does not require that types have a partial view in order to allow an access attribute of an unconstrained discriminated object, only that any partial view that does exist is unconstrained. 

Discussion: {AI12-0095-1} We assume the worst in a generic body whether or not a formal subtype has a constrained partial view; specifically, in a generic body a discriminated subtype is considered to have a constrained partial view if it is a descendant of an untagged generic formal private or derived type (see 12.5.1 for the formal definition of this rule). 

{AI05-0041-1} The accessibility level of the view shall not be statically deeper than that of the access type A. 

Ramification: In an instance body, a runtime check applies.

{AI95-00230-01} If A is an anonymous access-to-object type of an access parameter, then the view can never have a deeper accessibility level than A. The same is true for an anonymous access-to-object type of an access discriminant, except when X'Access is used to initialize an access discriminant of an object created by an [allocator](./AA-4.8#S0164). The latter case is illegal if the accessibility level of X is statically deeper than that of the access type of the [allocator](./AA-4.8#S0164); a runtime check is needed in the case where the initial value comes from an access parameter. Other anonymous access-to-object types have "normal" accessibility checks. 

Discussion: {AI12-0363-1} Additional restrictions exist in the specialized needs annexes. For instance, C.6 includes additional restrictions on atomic and volatile [prefix](./AA-4.1#S0093)es of the Access attribute. 

{AI05-0041-1} In addition to the places where Legality Rules normally apply (see 12.3), these requirements apply also in the private part of an instance of a generic unit.

A check is made that the accessibility level of X is not deeper than that of the access type A. If this check fails, Program_Error is raised. 

Ramification: The check is needed for access parameters  of an access-to-object type and in instance bodies.

{AI05-0024-1} Because there are no access parameters permitted for task entries, the accessibility levels are always comparable. We would have to switch to the terminology used in 4.8 and 6.5 based on inclusion within masters if we relax this restriction. That might introduce unacceptable distributed overhead. 

Implementation Note: {AI05-0148-1} This check requires that some indication of lifetime is passed as an implicit parameter along with access parameters of an access-to-object type. A similar indication is required for stand-alone objects of anonymous access-to-object types.No such requirement applies to other anonymous access types, since the checks associated with them are all compile-time checks. 

{AI12-0439-1} If the nominal subtype of X does not statically match the designated subtype of A, a view conversion of X to the designated subtype is evaluated (which can raise Constraint_Error - see 4.6) and the value of X'Access designates that view. 

The following attribute is defined for a [prefix](./AA-4.1#S0093) P that denotes a subprogram: 

P'Access{AI95-00229-01} {AI95-00254-01} {AI05-0239-1} {AI12-0064-2} P'Access yields an access value that designates the subprogram denoted by P. The type of P'Access is an access-to-subprogram type (S), as determined by the expected type. The accessibility level of P shall not be statically deeper than that of S. If S is nonblocking, P shall be nonblocking. In addition to the places where Legality Rules normally apply (see 12.3), these rules apply also in the private part of an instance of a generic unit. The profile of P shall be subtype conformant with the designated profile of S, and shall not be Intrinsic. If the subprogram denoted by P is declared within a generic unit, and the expression P'Access occurs within the body of that generic unit or within the body of a generic unit declared within the declarative region of the generic unit, then the ultimate ancestor of S shall be either a nonformal type declared within the generic unit or an anonymous access type of an access parameter.

Discussion: {AI95-00229-01} The part about generic bodies is worded in terms of the denoted subprogram, not the denoted view; this implies that renaming is invisible to this part of the rule. "Declared within the declarative region of the generic" is referring to child and nested generic units. This rule is partly to prevent contract model problems with respect to the accessibility rules, and partly to ease shared-generic-body implementations, in which a subprogram declared in an instance needs to have a different calling convention from other subprograms with the same profile.

Overload resolution ensures only that the profile is type conformant. This rule specifies that subtype conformance is required (which also requires matching calling conventions). P cannot denote an entry because access-to-subprogram types never have the entry calling convention. P cannot denote an enumeration literal or an attribute function because these have intrinsic calling conventions. 


#### Legality Rules

{AI05-0188-1} An [expression](./AA-4.4#S0132) is said to have distributed accessibility if it is

a [conditional_expression](./AA-4.5#S0148) (see 4.5.7); or

{AI12-0236-1} a [declare_expression](./AA-4.5#S0156) (see 4.5.9) whose body_[expression](./AA-4.4#S0132) has distributed accessibility; or

a view conversion, [qualified_expression](./AA-4.7#S0163), or parenthesized expression whose operand has distributed accessibility. 

{AI05-0188-1} The statically deeper relationship does not apply to the accessibility level of an [expression](./AA-4.4#S0132) having distributed accessibility; that is, such an accessibility level is not considered to be statically deeper, nor statically shallower, than any other.

{AI05-0188-1} Any static accessibility requirement that is imposed on an [expression](./AA-4.4#S0132) that has distributed accessibility (or on its type) is instead imposed on the dependent_[expression](./AA-4.4#S0132)s of the underlying [conditional_expression](./AA-4.5#S0148). This rule is applied recursively if a dependent_[expression](./AA-4.4#S0132) also has distributed accessibility.

Discussion: This means that any Legality Rule requiring that the accessibility level of an [expression](./AA-4.4#S0132) (or that of the type of an [expression](./AA-4.4#S0132)) shall or shall not be statically deeper than some other level also applies, in the case where the [expression](./AA-4.4#S0132) has distributed accessibility, to each dependent_[expression](./AA-4.4#S0132) of the underlying [conditional_expression](./AA-4.5#S0148). 

NOTE 1   The Unchecked_Access attribute yields the same result as the Access attribute for objects, but has fewer restrictions (see 13.10). There are other predefined operations that yield access values: an [allocator](./AA-4.8#S0164) can be used to create an object, and return an access value that designates it (see 4.8); evaluating the literal null yields a null access value that designates no entity at all (see 4.2).

NOTE 2   {AI95-00230-01} The predefined operations of an access type also include the assignment operation, qualification, and membership tests. Explicit conversion is allowed between general access types with matching designated subtypes; explicit conversion is allowed between access-to-subprogram types with subtype conformant profiles (see 4.6). Named access types have predefined equality operators; anonymous access types do not, but they can use the predefined equality operators for universal_access (see 4.5.2). 

Reason: {AI95-00230-01} Anonymous access types can use the universal access equality operators declared in Standard, while named access types cannot for compatibility reasons. By not having equality operators for anonymous access types, we eliminate the need to specify exactly where the predefined operators for anonymous access types would be defined, as well as the need for an implementer to insert an implicit declaration for "=", etc. at the appropriate place in their symbol table. Note that ":=", 'Access, and ".all" are defined. 

NOTE 3   The object or subprogram designated by an access value can be named with a dereference, either an [explicit_dereference](./AA-4.1#S0094) or an [implicit_dereference](./AA-4.1#S0095). See 4.1.

NOTE 4   A call through the dereference of an access-to-subprogram value is never a dispatching call. 

Proof: See 3.9.2. 

NOTE 5   {AI95-00254-01} {AI12-0440-1} The Access attribute for subprograms and parameters of an anonymous access-to-subprogram type can be used together to implement "downward closures" - that is, to pass a more-nested subprogram as a parameter to a less-nested subprogram, as can be appropriate for an iterator abstraction or numerical integration. Downward closures can also be implemented using generic formal subprograms (see 12.6). Note that Unchecked_Access is not allowed for subprograms.

NOTE 6   Note that using an access-to-class-wide tagged type with a dispatching operation is a potentially more structured alternative to using an access-to-subprogram type.

NOTE 7   {AI12-0442-1} An implementation can consider two access-to-subprogram values to be unequal, even though they designate the same subprogram. For instance, this can happen because one points directly to the subprogram, while the other points to a special prologue that performs an Elaboration_Check and then jumps to the subprogram. See 4.5.2. 

Ramification: If equality of access-to-subprogram values is important to the logic of a program, a reference to the Access attribute of a subprogram should be evaluated only once and stored in a global constant for subsequent use and equality comparison. 


#### Examples

Example of use of the Access attribute: 

```ada
{AI12-0312-1} Becky : Person_Name := new Person(F);       -- see 3.10.1
Cars  : array (1..2) of aliased Car;
   ...
Becky.Vehicle := Cars(1)'Access;
Casey.Vehicle := Cars(2)'Access;

```


#### Extensions to Ada 83

We no longer make things like 'Last and ".component" (basic) operations of an access type that need to be "declared" somewhere. Instead, implicit dereference in a [prefix](./AA-4.1#S0093) takes care of them all. This means that there should never be a case when X.all'Last is legal while X'Last is not. See AI83-00154. 


#### Incompatibilities With Ada 95

{AI95-00363-01}  Aliased variables are not necessarily constrained in Ada 2005 (see 3.6). Therefore, a subcomponent of an aliased variable may disappear or change shape, and taking 'Access of such a subcomponent thus is illegal, while the same operation would have been legal in Ada 95. Note that most allocated objects are still constrained by their initial value (see 4.8), and thus legality of 'Access didn't change for them. For example: 

```ada
type T1 (D1 : Boolean := False) is
   record
      case D1 is
         when False =&gt
            C1 : aliased Integer;
         when True =&gt
            null;
      end case;
   end record;
type Acc_Int is access all Integer;

```

```ada
A_T : aliased T1;
Ptr : Acc_Int := A_T.C1'Access; -- Illegal in Ada 2005, legal in Ada 95
A_T := (D1 =&gt True);            -- Raised Constraint_Error in Ada 95, but does not
                                -- in Ada 2005, so Ptr would become invalid when this
                                -- is assigned (thus Ptr is illegal).

```

{AI95-00363-01} If a discriminated full type has a partial view (private type) that is constrained, we do not allow 'Access on objects to create a value of an object of an access-to-unconstrained type. Ada 95 allowed this attribute and various access subtypes, requiring that the heap object be constrained and thus making details of the implementation of the private type visible to the client of the private type. See 4.8 for more on this topic.

{AI95-00229-01} {AI95-00254-01} Amendment Correction: Taking 'Access of a subprogram declared in a generic unit in the body of that generic is no longer allowed. Such references can easily be used to create dangling pointers, as Legality Rules are not rechecked in instance bodies. At the same time, the rules were loosened a bit where that is harmless, and also to allow any routine to be passed to an access parameter of an access-to-subprogram type. The now illegal uses of 'Access can almost always be moved to the private part of the generic unit, where they are still legal (and rechecked upon instantiation for possibly dangling pointers). 


#### Extensions to Ada 95

{8652/0010} {AI95-00127-01} Corrigendum: Access attributes of objects of class-wide types can be used as the controlling parameter in a dispatching calls (see 3.9.2). This was an oversight in Ada 95.

{AI95-00235-01} Amendment Correction: The type of the prefix can now be used in resolving Access attributes. This allows more uses of the Access attribute to resolve. For example: 

```ada
type Int_Ptr is access all Integer;
type Float_Ptr is access all Float;

```

```ada
function Zap (Val : Int_Ptr) return Float;
function Zap (Val : Float_Ptr) return Float;

```

```ada
Value : aliased Integer := 10;

```

```ada
Result1 : Float := Zap (Value'access); -- Ambiguous in Ada 95; resolves in Ada 2005.
Result2 : Float := Zap (Int_Ptr'(Value'access)); -- Resolves in Ada 95 and Ada 2005.

```

This change is upward compatible; any expression that does not resolve by the new rules would have failed a Legality Rule. 


#### Wording Changes from Ada 95

{AI95-00162-01} Adjusted the wording to reflect the fact that expressions and function calls are masters.

{AI95-00230-01} {AI95-00254-01} {AI95-00318-02} {AI95-00385-01} {AI95-00416-01} Defined the accessibility of the various new kinds and uses of anonymous access types. 


#### Incompatibilities With Ada 2005

{AI05-0008-1} Correction: Simplified the description of when a discriminant-dependent component is allowed as the prefix of 'Access to when the object is known to be constrained. This fixes a confusion as to whether a subcomponent of an object that is not certain to be constrained can be used as a prefix of 'Access. The fix introduces an incompatibility, as the rule did not apply in Ada 95 if the prefix was a constant; but it now applies no matter what kind of object is involved. The incompatibility is not too bad, since most kinds of constants are known to be constrained.

{AI05-0041-1} Correction: Corrected the checks for the constrainedness of the prefix of the Access attribute so that assume-the-worst is used in generic bodies. This may make some programs illegal, but those programs were at risk having objects disappear while valid access values still pointed at them. 


#### Extensions to Ada 2005

{AI05-0082-1} Correction: Eliminated the static accessibility definition for generic formal types, as the actual can be more or less nested than the generic itself. This allows programs that were illegal for Ada 95 and for Ada 2005.

{AI05-0148-1} {AI05-0253-1} Eliminate the static accessibility definition for stand-alone objects of anonymous access-to-object types. This allows such objects to be used as temporaries without causing accessibility problems. 


#### Wording Changes from Ada 2005

{AI05-0014-1} Correction: Corrected the rules so that the accessibility of the object designated by an access object is that of the access type, even when no dereference is given. The accessibility was not specified in the past. This correction applies to both Ada 95 and Ada 2005.

{AI05-0024-1} Correction: Corrected accessibility rules for access discriminants so that no cases are omitted.

{AI05-0051-1} {AI05-0234-1} {AI05-0235-1} {AI05-0284-1} Correction: Corrected accessibility rules for anonymous access return types and access discriminants in return statements.

{AI05-0066-1} Correction: Changed coextension rules so that coextensions that belong to an anonymous object are transfered to the ultimate object.

{AI05-0142-4} {AI05-0188-1} {AI05-0235-1} Defined the accessibility of explicitly aliased parameters (see 6.1) and [conditional_expression](./AA-4.5#S0148)s (see 4.5.7).

{AI05-0234-1} Correction: Defined the term "master of the call" to simplify other wording, especially that for the accessibility checks associated with return statements and explicitly aliased parameters.

{AI05-0270-1} Correction: Defined the (omitted) accessibility level of null values when those are passed as the actual of an access-to-object parameter. 


#### Inconsistencies With Ada 2012

{AI12-0278-1} {AI12-0390-1} Correction: Defined that the accessibility of a function that returns an anonymous access type is the same for implicit and explicit conversions. This could cause code involving implicit conversions to named access types that is legal and does not raise an exception in original Ada 2012 to become illegal or raise Program_Error because of an accessibility failure in Ada 2022. This is more likely to prevent a dangling pointer bug than to prevent something useful.

{AI12-0345-1} Correction: Tightened the cases where an explicitly aliased parameter has special accessibility, to avoid needing to pass the required dynamic accessibility to functions that have explicitly aliased parameters. The changes affects programs that use the dynamic accessibility of an explicitly aliased parameter within a return statement of a function (typically using anonymous access types). This can mean that a program that would have been legal and worked in Ada 2012 as defined would raise Program_Error or be rejected in Ada 2022. One such example is: 

```ada
type Rec is record
    Comp : aliased Integer;
    ...
end record;

```

```ada
function F1 (A : aliased in out Rec) return access Integer is
   function F2 (B : access Integer) return access Integer
      is (B); -- (1)
begin
   return F2 (A.Comp'Access); -- (2)
end F1;

```

At (2), there is a check that the dynamic accessibility level of B is not deeper than the master of the call for F2 (which is defined to be the master of the call for F1). In Ada 2012, since the reference is inside of a return statement, the dynamic accessibility of A.Comp'Access is the master of the call for F1 - meaning the check at (2) should pass. In Ada 2022, the dynamic accessibility of A.Comp'Access is local for for F1 - meaning the check at (2) should fail and raise Program_Error.

We're not aware of any Ada 2012 compilers that correctly implemented the rule as written (because of the overhead that is implied), so the practical effect of this change is to make the rules more consistent with actual practice.

{AI12-0345-1} Correction: Adjusted the rules so that using a part of a return object in a return expression does not break the connection between the inner and outer function calls. This means that some cases that (unnecessarily) failed an accessibility check in Ada 2012 will not do so in Ada 2022. This change will mostly remove latent problems from Ada code; very little code depends on an accessibility check failing. 


#### Incompatibilities With Ada 2012

{AI12-0027-1} Corrigendum: Defined the accessibility of a value conversion, so that it is treated like an [aggregate](./AA-4.3#S0106) built at the point of the conversion. This was previously unspecified, so this change might be incompatible if an Ada implementation treated the accessibility of such conversions as that of the operand type; in that case, previous legal conversions probably will become illegal as the lifetime of the conversion is very short. However, code that could tell this difference is fairly rare (taking 'Access of a component of a result of a type conversion), code depending on this accessibility was not portable, and such code could have created an immediately dangling pointer if the conversion actually made a copy (which is necessary in some instances).

{AI12-0371-1} The accessibility of a renaming of a formal subprogram in a generic package specification has changed to reflect the fact that a wrapper may be needed for the formal subprogram. Thus, 'Access of such a renaming found in an instance that is not library-level may become illegal. We think this case is unlikely to occur in practice, and a simple workaround is available (rename the actual at the point of the instance, use that rename as the prefix of 'Access). 


#### Extensions to Ada 2012

{AI12-0402-1} Adjusted the special accessibility for functions initializing objects to only apply to composite types. This makes some function calls of functions that return elementary types and have aliased parameters legal that otherwise would be illegal. Such a function cannot return a part of a parameter, and thus one does not need parameter checks to make that possible. 


#### Wording Changes from Ada 2012

{AI12-0067-1} Corrigendum: Corrected so that it is clear that explicitly aliased parameters in procedures have the same accessibility as other parameters. Only explicitly aliased parameters in functions are special.

{AI12-0070-1} Corrigendum: The accessibility of an anonymous access type of an [extended_return_statement](./AA-6.5#S0225) is defined here rather than in 6.5 so that all accessibility rules are here.

{AI12-0089-1} Corrigendum: Corrected a number of rules so that they clearly apply to generic functions as well as functions. (Remember, a generic function is not a function.)

{AI12-0095-1} Corrigendum: Moved the assume the worst rule about constrainedness of the prefix of attribute Access to 12.5.1, as a number of places in the Reference Manual need this rule.

{AI12-0157-1} Corrigendum: Ensured that the statically deeper relationship applies within the return expression of an expression function. (Dynamic properties apply by equivalence, but static properties are handled separately.)

{AI12-0064-2} Added Nonblocking (see 9.5) matching to P'Access.

{AI12-0156-1} Added text to define the accessibility of anonymous access types declaring a loop parameter.

{AI12-0236-1} Added [declare_expression](./AA-4.5#S0156)s to the accessibility rules.

{AI12-0277-1} Correction: Clarified the static level of explicitly aliased parameters.

{AI12-0372-1} Correction: Redefined the statically deeper relationship for the master of a function call and locals of that call. This might cause programs to be rejected that were previously legal, but any such program should have failed a dynamic accessibility check. Thus this is not an incompatibility as it is changing the detection of an error from runtime to compile-time.

{AI12-0392-1} Correction: Added wording to clarify that the accessibility of a [raise_expression](./AA-11.3#S0309) does not need any static checks (it is considered to match any accessibility required).

{AI12-0406-1} Correction: Used the new term "master construct", to put static accessibility rules on a firmer basis, including ensuring that those rules apply inside of generic bodies. 

