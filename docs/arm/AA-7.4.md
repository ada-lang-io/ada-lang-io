---
sidebar_position:  61
---

# 7.4  Deferred Constants

[Deferred constant declarations may be used to declare constants in the visible part of a package, but with the value of the constant given in the private part. They may also be used to declare constants imported from other languages (see Annex B).] 


#### Legality Rules

{AI05-0229-1} {AI05-0269-1} [ A deferred constant declaration is an [object_declaration](./AA-3.3#S0032) with the reserved word constant but no initialization expression.] The constant declared by a deferred constant declaration is called a deferred constant. [Unless the Import aspect (see B.1) is True for a deferred constant declaration, the ] deferred constant declaration requires a completion, which shall be a full constant declaration (called the full declaration of the deferred constant). 

Proof: The first sentence is redundant, as it is stated officially in 3.3.1.

{AI05-0229-1} {AI05-0269-1} The first part of the last sentence is redundant, as no imported entity may have a completion, as stated in B.1. 

A deferred constant declaration that is completed by a full constant declaration shall occur immediately within the visible part of a [package_specification](./AA-7.1#S0230). For this case, the following additional rules apply to the corresponding full declaration: 

The full declaration shall occur immediately within the private part of the same package;

{AI95-00385-01} The deferred and full constants shall have the same type, or shall have statically matching anonymous access subtypes; 

Ramification: {AI95-00385-01} This implies that both the deferred declaration and the full declaration have to have a [subtype_indication](./AA-3.2#S0027) or [access_definition](./AA-3.10#S0084) rather than an [array_type_definition](./AA-3.6#S0051), because each [array_type_definition](./AA-3.6#S0051) would define a new type. 

{AI95-00385-01} {AI05-0062-1} {AI05-0262-1} If the deferred constant declaration includes a [subtype_indication](./AA-3.2#S0027) S that defines a constrained subtype, then the constraint defined by the [subtype_indication](./AA-3.2#S0027) in the full declaration shall match the constraint defined by S statically.[ On the other hand, if the subtype of the deferred constant is unconstrained, then the full declaration is still allowed to impose a constraint. The constant itself will be constrained, like all constants;]

{AI95-00231-01} If the deferred constant declaration includes the reserved word aliased, then the full declaration shall also; 

Ramification: On the other hand, the full constant can be aliased even if the deferred constant is not. 

{AI95-00231-01} If the subtype of the deferred constant declaration excludes null, the subtype of the full declaration shall also exclude null. 

Ramification: On the other hand, the full constant can exclude null even if the deferred constant does not. But that can only happen for a [subtype_indication](./AA-3.2#S0027), as anonymous access types are required to statically match (which includes any [null_exclusion](./AA-3.10#S0083)). 

{AI05-0229-1} {AI12-0444-1} [A deferred constant declaration for which the Import aspect is True can appear anywhere that an [object_declaration](./AA-3.3#S0032) is allowed, and has no full constant declaration.]

{AI95-00256-01} The completion of a deferred constant declaration shall occur before the constant is frozen (see 13.14).


#### Dynamic Semantics

{AI05-0004-1} The elaboration of a deferred constant declaration elaborates the [subtype_indication](./AA-3.2#S0027), [access_definition](./AA-3.10#S0084), or (only allowed in the case of an imported constant) the [array_type_definition](./AA-3.6#S0051). 

Ramification: {AI05-0004-1} For nonimported constants, these elaborations cannot require any code or checks for a legal program, because the given [subtype_indication](./AA-3.2#S0027) has to be indefinite or statically match that of the full constant, meaning that either it is a [subtype_mark](./AA-3.2#S0028) or it has static constraints. If the deferred constant instead has an [access_definition](./AA-3.10#S0084), the designated subtype must be a [subtype_mark](./AA-3.2#S0028). We still say that these are elaborated, however, because part of elaboration is creating the type, which is clearly needed for [access_definition](./AA-3.10#S0084)s. (A deferred constant and its full constant have different types when they are specified by an [access_definition](./AA-3.10#S0084), although there is no visible effect of these types being different as neither can be named.) 

NOTE   The full constant declaration for a deferred constant that is of a given private type or private extension is not allowed before the corresponding [full_type_declaration](./AA-3.2#S0024). This is a consequence of the freezing rules for types (see 13.14). 

Ramification: Multiple or single declarations are allowed for the deferred and the full declarations, provided that the equivalent single declarations would be allowed.

Deferred constant declarations are useful for declaring constants of private views, and types with components of private views. They are also useful for declaring access-to-constant objects that designate variables declared in the private part of a package. 


#### Examples

Examples of deferred constant declarations: 

```ada
Null_Key : constant Key;      -- see 7.3.1

```

```ada
{AI05-0229-1} CPU_Identifier : constant String(1..8)
   with Import =&gt True, Convention =&gt Assembler, Link_Name =&gt "CPU_ID";
                              -- see B.1

```


#### Extensions to Ada 83

In Ada 83, a deferred constant is required to be of a private type declared in the same visible part. This restriction is removed for Ada 95; deferred constants can be of any type.

In Ada 83, a deferred constant declaration was not permitted to include a constraint, nor the reserved word aliased.

In Ada 83, the rules required conformance of type marks; here we require static matching of subtypes if the deferred constant is constrained.

A deferred constant declaration can be completed with a [pragma](./AA-2.8#S0019) Import. Such a deferred constant declaration need not be within a [package_specification](./AA-7.1#S0230).

The rules for too-early uses of deferred constants are modified in Ada 95 to allow more cases, and catch all errors at compile time. This change is necessary in order to allow deferred constants of a tagged type without violating the principle that for a dispatching call, there is always an implementation to dispatch to. It has the beneficial side effect of catching some Ada-83-erroneous programs at compile time. The new rule fits in well with the new freezing-point rules. Furthermore, we are trying to convert undefined-value problems into bounded errors, and we were having trouble for the case of deferred constants. Furthermore, uninitialized deferred constants cause trouble for the shared variable / tasking rules, since they are really variable, even though they purport to be constant. In Ada 95, they cannot be touched until they become constant.

Note that we do not consider this change to be an upward incompatibility, because it merely changes an erroneous execution in Ada 83 into a compile-time error.

The Ada 83 semantics are unclear in the case where the full view turns out to be an access type. It is a goal of the language design to prevent uninitialized access objects. One wonders if the implementation is required to initialize the deferred constant to null, and then initialize it (again!) to its real value. In Ada 95, the problem goes away. 


#### Wording Changes from Ada 83

{AI05-0299-1} Since deferred constants can now be of a nonprivate type, we have made this a stand-alone subclause, rather than a subclause of 7.3, "Private Types and Private Extensions".

Deferred constant declarations used to have their own syntax, but now they are simply a special case of [object_declaration](./AA-3.3#S0032)s. 


#### Extensions to Ada 95

{AI95-00385-01} Deferred constants were enhanced to allow the use of anonymous access types in them. 


#### Wording Changes from Ada 95

{AI95-00231-01} Added matching rules for subtypes that exclude null. 


#### Wording Changes from Ada 2005

{AI05-0062-1} Correction: Corrected rules so that the intent that a full constant may have a null exclusion even if the deferred constant does not is actually met. 

