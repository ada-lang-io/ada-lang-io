---
sidebar_position:  31
---

# 4.3  Aggregates

[ An aggregate combines component values into a composite value of an array type, record type, or record extension.] 

Glossary entry: An aggregate is a construct used to define a value of a composite type by specifying the values of the components of the type.

Version=[5],Kind=(AddedNormal),Group=[C],Term=[aggregate], Def=[a construct used to define a value of a composite type by specifying the values of the components of the type]


#### Syntax

{AI12-0127-1} {AI12-0212-1} aggregate<a id="S0106"></a> ::= 
    [record_aggregate](./AA-4.3#S0107) | [extension_aggregate](./AA-4.3#S0111) | [array_aggregate](./AA-4.3#S0113)
  | [delta_aggregate](./AA-4.3#S0120) | [container_aggregate](./AA-4.3#S0123)


#### Name Resolution Rules

{AI95-00287-01} {AI12-0127-1} {AI12-0212-1} {AI12-0307-1} The expected type for an [aggregate](./AA-4.3#S0106) shall be a single array type, a single type with the Aggregate aspect specified, or a single descendant of a record type or of a record extension. 

Discussion: See 8.6, "The Context of Overload Resolution" for the meaning of "shall be a single ... type". 

Ramification: {AI05-0005-1} There are additional rules for each kind of aggregate. These aggregate rules are additive; a legal expression needs to satisfy all of the applicable rules. That means the rule given here must be satisfied even when it is syntactically possible to tell which specific kind of aggregate is being used. 

Reason: {AI12-0005-1} {AI12-0127-1} {AI12-0212-1} {AI12-0307-1} Generally, we don't want to look in [aggregate](./AA-4.3#S0106)s to resolve them. For instance, Ada 95 allowed array types to match [extension_aggregate](./AA-4.3#S0111)s, even though those have to be record types. Thus, we allow any record type with any visible (nondiscriminant) components to match an [aggregate](./AA-4.3#S0106), even though only [delta_aggregate](./AA-4.3#S0120)s allow private types or private extensions. Similarly, we allow any container type to match an [aggregate](./AA-4.3#S0106), even though only [container_aggregate](./AA-4.3#S0123)s allow container types. 


#### Legality Rules

{AI12-0127-1} A [record_aggregate](./AA-4.3#S0107) or [extension_aggregate](./AA-4.3#S0111) shall not be of a class-wide type. 

Ramification: When the expected type in some context is class-wide, an aggregate has to be explicitly qualified by the specific type of value to be created, so that the expected type for the aggregate itself is specific. 

Discussion: We used to disallow [aggregate](./AA-4.3#S0106)s of a type with unknown discriminants. However, that was unnecessarily restrictive in the case of an extension aggregate, and irrelevant to a record aggregate (since a type that is legal for a record aggregate could not possibly have unknown discriminants) and to an array aggregate (the only specific types that can have unknown discriminants are private types, private extensions, and types derived from them). 

Reason: {AI12-0127-1} We don't mention [delta_aggregate](./AA-4.3#S0120)s, as those can get the specific type from the object represented by the base_[expression](./AA-4.4#S0132) (possibly at run time). We don't mention [array_aggregate](./AA-4.3#S0113)s, as those cannot even be of a tagged type, so being class-wide is impossible. 


#### Dynamic Semantics

For the evaluation of an [aggregate](./AA-4.3#S0106), an anonymous object is created and values for the components or ancestor part are obtained (as described in the subsequent subclause for each kind of the [aggregate](./AA-4.3#S0106)) and assigned into the corresponding components or ancestor part of the anonymous object. Obtaining the values and the assignments occur in an arbitrary order. The value of the [aggregate](./AA-4.3#S0106) is the value of this object. 

Discussion: The ancestor part is the set of components inherited from the ancestor type. The syntactic category [ancestor_part](./AA-4.3#S0112) is the [expression](./AA-4.4#S0132) or [subtype_mark](./AA-3.2#S0028) that specifies how the ancestor part of the anonymous object should be initialized. 

Ramification: The assignment operations do the necessary value adjustment, as described in 7.6. Note that the value as a whole is not adjusted - just the subcomponents (and ancestor part, if any). 7.6 also describes when this anonymous object is finalized.

If the [ancestor_part](./AA-4.3#S0112) is a [subtype_mark](./AA-3.2#S0028) the Initialize procedure for the ancestor type is applied to the ancestor part after default-initializing it, unless the procedure is abstract, as described in 7.6. The Adjust procedure for the ancestor type is not called in this case, since there is no assignment to the ancestor part as a whole. 

If an [aggregate](./AA-4.3#S0106) is of a tagged type, a check is made that its value belongs to the first subtype of the type. Constraint_Error is raised if this check fails. 

Ramification: This check ensures that no values of a tagged type are ever outside the first subtype, as required for inherited dispatching operations to work properly (see 3.4). This check will always succeed if the first subtype is unconstrained. This check is not extended to untagged types to preserve upward compatibility. 


#### Extensions to Ada 83

We now allow [extension_aggregate](./AA-4.3#S0111)s. 


#### Wording Changes from Ada 83

We have adopted new wording for expressing the rule that the type of an aggregate shall be determinable from the outside, though using the fact that it is nonlimited record (extension) or array.

An [aggregate](./AA-4.3#S0106) now creates an anonymous object. This is necessary so that controlled types will work (see 7.6). 


#### Incompatibilities With Ada 95

{AI95-00287-01} In Ada 95, a limited type is not considered when resolving an [aggregate](./AA-4.3#S0106). Since Ada 2005 now allows limited [aggregate](./AA-4.3#S0106)s, we can have incompatibilities. For example: 

```ada
type Lim is limited
   record
      Comp: Integer;
   end record;

```

```ada
type Not_Lim is
   record
      Comp: Integer;
   end record;

```

```ada
procedure P(X: Lim);
procedure P(X: Not_Lim);

```

```ada
P((Comp =&gt 123)); -- Illegal in Ada 2005, legal in Ada 95

```

The call to P is ambiguous in Ada 2005, while it would not be ambiguous in Ada 95 as the [aggregate](./AA-4.3#S0106) could not have a limited type. Qualifying the [aggregate](./AA-4.3#S0106) will eliminate any ambiguity. This construction would be rather confusing to a maintenance programmer, so it should be avoided, and thus we expect it to be rare. 


#### Extensions to Ada 95

{AI95-00287-01} [Aggregate](./AA-4.3#S0106)s can be of a limited type. 


#### Incompatibilities With Ada 2012

{AI12-0005-1} {AI12-0127-1} {AI12-0212-1} We now allow types with the Aggregate aspect specified ("container types"), as well as private types and extensions descended from a record type or extension, to match all forms of [aggregate](./AA-4.3#S0106). These types are only allowed for new types of [aggregate](./AA-4.3#S0106) ([container_aggregate](./AA-4.3#S0123)s for the Aggregate aspect, and [delta_aggregate](./AA-4.3#S0120)s for private types), but, consistent with other forms of [aggregate](./AA-4.3#S0106), we do not look at the form of the [aggregate](./AA-4.3#S0106) to determine resolution. This can be incompatible in usually unlikely cases, where overloading of a container or private type with a type that was previously allowed in [aggregate](./AA-4.3#S0106)s makes an existing call ambiguous. Unfortunately, Ada.Containers.Vectors has a number of such overloadings for Insert, Append, Prepend, and "&", so the problem may appear for any element type of a Vector that allows aggregates. For instance, if My_Vector is an instance of Ada.Containers.Vectors with an element type of Not_Lim as defined above, and V is an object of My_Vector.Vector, then My_Vector.Append (V, (Comp =&gt 123)); will be illegal in Ada 2022 and legal in Ada 2012. This can easily be fixed by qualifying the [aggregate](./AA-4.3#S0106) with the correct type. 


## 4.3.1  Record Aggregates

[In a [record_aggregate](./AA-4.3#S0107), a value is specified for each component of the record or record extension value, using either a named or a positional association.] 


#### Syntax

record_aggregate<a id="S0107"></a> ::= ([record_component_association_list](./AA-4.3#S0108))

record_component_association_list<a id="S0108"></a> ::= 
    [record_component_association](./AA-4.3#S0109) {, [record_component_association](./AA-4.3#S0109)}
  | null record

{AI95-00287-01} record_component_association<a id="S0109"></a> ::= 
    [[component_choice_list](./AA-4.3#S0110) =&gt] [expression](./AA-4.4#S0132)
   | [component_choice_list](./AA-4.3#S0110) =&gt &lt&gt

{AI12-0212-1} component_choice_list<a id="S0110"></a> ::= 
     component_[selector_name](./AA-4.1#S0099) {'|' component_[selector_name](./AA-4.1#S0099)}
   | others

A [record_component_association](./AA-4.3#S0109) is a named component association if it has a [component_choice_list](./AA-4.3#S0110); otherwise, it is a positional component association. Any positional component associations shall precede any named component associations. If there is a named association with a [component_choice_list](./AA-4.3#S0110) of others, it shall come last. 

Discussion: These rules were implied by the BNF in an early version of the RM9X, but it made the grammar harder to read, and was inconsistent with how we handle discriminant constraints. Note that for array aggregates we still express some of the rules in the grammar, but array aggregates are significantly different because an array aggregate is either all positional (with a possible others at the end), or all named. 

In the [record_component_association_list](./AA-4.3#S0108) for a [record_aggregate](./AA-4.3#S0107), if there is only one association, it shall be a named association. 

Reason: {AI05-0264-1} Otherwise, the construct would be interpreted as a parenthesized expression. This is considered a syntax rule, since it is relevant to overload resolution. We choose not to express it with BNF so we can share the definition of [record_component_association_list](./AA-4.3#S0108) in both [record_aggregate](./AA-4.3#S0107) and [extension_aggregate](./AA-4.3#S0111). 

Ramification: The [record_component_association_list](./AA-4.3#S0108) of an [extension_aggregate](./AA-4.3#S0111) does not have such a restriction. 


#### Name Resolution Rules

{AI95-00287-01} The expected type for a [record_aggregate](./AA-4.3#S0107) shall be a single record type or record extension. 

Ramification: This rule is used to resolve whether an [aggregate](./AA-4.3#S0106) is an [array_aggregate](./AA-4.3#S0113) or a [record_aggregate](./AA-4.3#S0107). The presence of a with is used to resolve between a [record_aggregate](./AA-4.3#S0107) and an [extension_aggregate](./AA-4.3#S0111). 

{AI12-0127-1} For the [record_component_association_list](./AA-4.3#S0108) of a [record_aggregate](./AA-4.3#S0107), all components of the composite value defined by the aggregate are needed[; for the association list of an [extension_aggregate](./AA-4.3#S0111), only those components not determined by the ancestor expression or subtype are needed (see 4.3.2).] Each component_[selector_name](./AA-4.1#S0099) in a [record_component_association](./AA-4.3#S0109) of a [record_aggregate](./AA-4.3#S0107) or [extension_aggregate](./AA-4.3#S0111) shall denote a needed component [(including possibly a discriminant)]. Each component_[selector_name](./AA-4.1#S0099) in a [record_component_association](./AA-4.3#S0109) of a [record_delta_aggregate](./AA-4.3#S0121) (see 4.3.4) shall denote a nondiscriminant component of the type of the [aggregate](./AA-4.3#S0106).

Ramification: For the association list of a [record_aggregate](./AA-4.3#S0107), "needed components" includes every component of the composite value, but does not include those in unchosen [variant](./AA-3.8#S0072)s (see AI83-309). If there are [variant](./AA-3.8#S0072)s, then the value specified for the discriminant that governs them determines which [variant](./AA-3.8#S0072) is chosen, and hence which components are needed.

If an extension defines a new [known_discriminant_part](./AA-3.7#S0061), then all of its discriminants are needed in the component association list of an extension aggregate for that type, even if the discriminants have the same names and types as discriminants of the type of the ancestor expression. This is necessary to ensure that the positions in the [record_component_association_list](./AA-4.3#S0108) are well defined, and that discriminants that govern [variant_part](./AA-3.8#S0071)s can be given by static expressions. 

Reason: We don't define "needed" for [record_delta_aggregate](./AA-4.3#S0121)s so that there is no completeness requirement. But that means that we need to ensure that the rules using "needed" doesn't appear to apply to [record_delta_aggregate](./AA-4.3#S0121)s, and we also need Legality Rules to prevent giving the same component twice and giving components from two different variants. 

Version=[5],Kind=(AddedNormal),Group=[T],Term=[needed component], Def=[a component of a record type or record extension that is required to have its value specified within a given aggregate]

The expected type for the [expression](./AA-4.4#S0132) of a [record_component_association](./AA-4.3#S0109) is the type of the associated component(s); the associated component(s) are as follows: 

For a positional association, the component [(including possibly a discriminant)] in the corresponding relative position (in the declarative region of the type), counting only the needed components; 

Ramification: This means that for an association list of an [extension_aggregate](./AA-4.3#S0111), only noninherited components are counted to determine the position.

{AI05-0005-1} For a derived type (including type extensions), the order of declaration is defined in 3.4, "Derived Types and Classes". In particular, all discriminants come first, regardless of whether they are defined for the parent type or are newly added to the derived type. 

For a named association with one or more component_[selector_name](./AA-4.1#S0099)s, the named component(s);

For a named association with the reserved word others, all needed components that are not associated with some previous association. 


#### Legality Rules

If the type of a [record_aggregate](./AA-4.3#S0107) is a record extension, then it shall be a descendant of a record type, through one or more record extensions (and no private extensions).

{AI05-0016-1} {AI12-0127-1} A [record_component_association_list](./AA-4.3#S0108) shall be null record only if the list occurs in a [record_aggregate](./AA-4.3#S0107) or [extension_aggregate](./AA-4.3#S0111), and there are no components needed for that list.

Ramification: For example, "(null record)" is a [record_aggregate](./AA-4.3#S0107) for a null record type. Similarly, "(T'(A) with null record)" is an [extension_aggregate](./AA-4.3#S0111) for a type defined as a null record extension of T.

{AI05-0016-1} If no components are needed and null record is not used, the [record_component_association](./AA-4.3#S0109) must necessarily be others =&gt &lt&gt, as that is the only [record_component_association](./AA-4.3#S0109) that does not require an associated component. 

{AI95-00287-01} {AI05-0199-1} {AI12-0046-1} {AI12-0127-1} For a [record_aggregate](./AA-4.3#S0107) or [extension_aggregate](./AA-4.3#S0111), each [record_component_association](./AA-4.3#S0109) other than an others choice with a &lt&gt shall have at least one associated component, and each needed component shall be associated with exactly one [record_component_association](./AA-4.3#S0109). For a [record_delta_aggregate](./AA-4.3#S0121), each component_[selector_name](./AA-4.1#S0099) of each [component_choice_list](./AA-4.3#S0110) shall denote a distinct nondiscriminant component of the type of the aggregate.

Ramification: {AI95-00287-01} These rules apply to an association with an others choice with an expression. An others choice with a &lt&gt can match zero components or several components with different types. 

Reason: {AI95-00287-01} Without these rules, there would be no way to know what was the expected type for the [expression](./AA-4.4#S0132) of the association. Note that some of the rules do not apply to &lt&gt associations, as we do not need to resolve anything. We allow others =&gt &lt&gt to match no components as this is similar to array aggregates. That means that (others =&gt &lt&gt) always represents a default-initialized record or array value. 

This paragraph was deleted.{AI12-0046-1} 

Ramification: The rule that requires at least one associated component for each [record_component_association](./AA-4.3#S0109) implies that there can be no extra associations for components that don't exist in the composite value, or that are already determined by the ancestor expression or subtype of an [extension_aggregate](./AA-4.3#S0111).

The second part of the first sentence ensures that no needed components are left out, nor specified twice. 

{AI12-0046-1} {AI12-0127-1} If a [record_component_association](./AA-4.3#S0109) with an [expression](./AA-4.4#S0132) has two or more associated components, all of them shall be of the same type, or all of them shall be of anonymous access types whose subtypes statically match. In addition, Legality Rules are enforced separately for each associated component.

Discussion: {AI12-0046-1} AI83-00244 also requires that the [expression](./AA-4.4#S0132) shall be legal for each associated component. Ada 95 omitted this wording, as it was thought that all cases of difference had been eliminated. That probably was true, but Ada 2005 reintroduced cases where the types match but the legality differs. For example:

```ada
type Rec (D : access Integer) is record
          F : access Integer;
end record;

```

```ada
begin
   declare
      X : aliased Integer;
      R : Rec := (D | F =&gt X'Access); -- Legal for D, illegal for F

```

There are additional ways for this to happen; because of cases like the above we require that the Legality Rules are checked individually for each associated component. 

{AI05-0220-1} {AI12-0086-1} {AI12-0127-1} For a [record_aggregate](./AA-4.3#S0107) or [extension_aggregate](./AA-4.3#S0111), if a [variant_part](./AA-3.8#S0071) P is nested within a [variant](./AA-3.8#S0072) V that is not selected by the discriminant value governing the [variant_part](./AA-3.8#S0071) enclosing V, then there is no restriction on the discriminant governing P. Otherwise, the value of the discriminant that governs P shall be given by a static expression, or by a nonstatic expression having a constrained static nominal subtype. In this latter case of a nonstatic expression, there shall be exactly one [discrete_choice_list](./AA-3.8#S0073) of P that covers each value that belongs to the nominal subtype and satisfies the predicates of the subtype, and there shall be at least one such value. 

Ramification: This expression might either be given within the aggregate itself, or in a constraint on the parent subtype in a [derived_type_definition](./AA-3.4#S0035) for some ancestor of the type of the aggregate.

{AI12-0086-1} This means that in the nonstatic value case, the (static) subtype cannot have a null range. 

{AI95-00287-01} A [record_component_association](./AA-4.3#S0109) for a discriminant without a [default_expression](./AA-3.7#S0063) shall have an [expression](./AA-4.4#S0132) rather than &lt&gt. 

Reason: A discriminant must always have a defined value, but &lt&gt means uninitialized for a discrete type unless the component has a default value. 

{AI12-0127-1} A [record_component_association](./AA-4.3#S0109) of the [record_component_association_list](./AA-4.3#S0108) of a [record_delta_aggregate](./AA-4.3#S0121) shall not:

{AI12-0418-1} use the box compound delimiter &lt&gt rather than an [expression](./AA-4.4#S0132);

have an [expression](./AA-4.4#S0132) of a limited type;

omit the [component_choice_list](./AA-4.3#S0110); or

Ramification: {AI12-0127-1} A [record_delta_aggregate](./AA-4.3#S0121) cannot use positional notation; all choices shall be named choices. 

have a [component_choice_list](./AA-4.3#S0110) that is an others choice. 

{AI12-0127-1} For a [record_delta_aggregate](./AA-4.3#S0121), no two component_[selector_name](./AA-4.1#S0099)s shall denote components declared within different [variant](./AA-3.8#S0072)s of the same [variant_part](./AA-3.8#S0071).


#### Dynamic Semantics

The evaluation of a [record_aggregate](./AA-4.3#S0107) consists of the evaluation of the [record_component_association_list](./AA-4.3#S0108).

{AI12-0086-1} For the evaluation of a [record_component_association_list](./AA-4.3#S0108), any per-object constraints (see 3.8) for components specified in the association list are elaborated and any [expression](./AA-4.4#S0132)s are evaluated and converted to the subtype of the associated component. Any constraint elaborations and [expression](./AA-4.4#S0132) evaluations (and conversions) occur in an arbitrary order, except that the [expression](./AA-4.4#S0132) for a discriminant is evaluated (and converted) prior to the elaboration of any per-object constraint that depends on it, which in turn occurs prior to the evaluation and conversion of the [expression](./AA-4.4#S0132) for the component with the per-object constraint. If the value of a discriminant that governs a selected [variant_part](./AA-3.8#S0071)  P is given by a nonstatic [expression](./AA-4.4#S0132), and the evaluation of that [expression](./AA-4.4#S0132) yields a value that does not belong to the nominal subtype of the [expression](./AA-4.4#S0132), then Constraint_Error is raised.

Ramification: The conversion in the first rule might raise Constraint_Error. 

Discussion: This check in the first rule presumably happened as part of the dependent compatibility check in Ada 83. 

Ramification: {AI12-0086-1} The test on the value of the discriminant can only fail if the value is outside the base range of its type, does not satisfy the (static) predicates of the subtype (possible when the predicate is disabled), or is an invalid representation. This is a rule similar to that used for case statements. As with case statements, this is not a "check"; it cannot be Suppressed. 

{AI95-00287-01} For a [record_component_association](./AA-4.3#S0109) with an [expression](./AA-4.4#S0132), the [expression](./AA-4.4#S0132) defines the value for the associated component(s). For a [record_component_association](./AA-4.3#S0109) with &lt&gt, if the [component_declaration](./AA-3.8#S0070) has a [default_expression](./AA-3.7#S0063), that [default_expression](./AA-3.7#S0063) defines the value for the associated component(s); otherwise, the associated component(s) are initialized by default as for a stand-alone object of the component subtype (see 3.3.1).

The [expression](./AA-4.4#S0132) of a [record_component_association](./AA-4.3#S0109) is evaluated (and converted) once for each associated component.

Ramification: {AI05-0005-1} We don't need similar language for &lt&gt, as we're considering the value of &lt&gt for each individual component. Each component has its own default expression or its own default initialization (they can be different for each component; the components even could have different types), and each one has to be evaluated. So there is no need to repeat that. 

NOTE 1   For a [record_aggregate](./AA-4.3#S0107) with positional associations, expressions specifying discriminant values appear first since the [known_discriminant_part](./AA-3.7#S0061) is given first in the declaration of the type; they have to be in the same order as in the [known_discriminant_part](./AA-3.7#S0061). 


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

{AI95-00287-01} Examples of component associations with several choices: 

```ada
(Value =&gt 0, Succ|Pred =&gt new Cell'(0, null, null)) 	--  see 3.10.1

```

```ada
 --  The allocator is evaluated twice: Succ and Pred designate different cells

```

```ada
(Value =&gt 0, Succ|Pred =&gt &lt&gt) 	--  see 3.10.1

```

```ada
 --  Succ and Pred will be set to null

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


#### Extensions to Ada 95

{AI95-00287-01} &lt&gt can be used in place of an [expression](./AA-4.4#S0132) in a [record_aggregate](./AA-4.3#S0107), default initializing the component. 


#### Wording Changes from Ada 95

{AI95-00287-01} Limited [record_aggregate](./AA-4.3#S0107)s are allowed (since all kinds of aggregates can now be limited, see 4.3). 


#### Incompatibilities With Ada 2005

{AI05-0220-1} Correction: Corrected wording so that the rule for discriminants governing [variant_part](./AA-3.8#S0071)s was not effectively circular. The change makes a few [aggregate](./AA-4.3#S0106)s where a nonstatic discriminant governs an empty [variant_part](./AA-3.8#S0071) illegal. However, most Ada implementations already enforce some version of the new rule and already reject these [aggregate](./AA-4.3#S0106)s. So it is unlikely that any incompatibility will be noticed in practice. 


#### Extensions to Ada 2005

{AI05-0016-1} Correction: Fixed the wording so that others =&gt &lt&gt can be used in place of null record. This is needed to avoid a generic contract issue for generic bodies: we do not want to have to assume the worst to disallow others =&gt &lt&gt if the record type might be a null record.

{AI05-0199-1} Correction: We now allow multiple components with anonymous access types to be specified with a single component association. This is to be consistent with the capabilities of a named access type. 


#### Extensions to Ada 2012

{AI12-0086-1} The value of a discriminant that governs a [variant_part](./AA-3.8#S0071) in an [aggregate](./AA-4.3#S0106) can be nonstatic if the nominal subtype of the discriminant expression is static and all possible values select a single [variant](./AA-3.8#S0072). 


#### Wording Changes from Ada 2012

{AI12-0046-1} Corrigendum: We explicitly say that the Legality Rules have to be rechecked for each component individually. This seems obvious, but as the AARM note 4.3.1 (16.c) appeared to say that this was not necessary, and since we explicitly state this sort of thing for generic instances, it seemed better to be explicit.

{AI12-0127-1} Made wording changes for [delta_aggregate](./AA-4.3#S0120)s.


## 4.3.2  Extension Aggregates

[An [extension_aggregate](./AA-4.3#S0111) specifies a value for a type that is a record extension by specifying a value or subtype for an ancestor of the type, followed by associations for any components not determined by the [ancestor_part](./AA-4.3#S0112).] 


#### Language Design Principles

The model underlying this syntax is that a record extension can also be viewed as a regular record type with an ancestor "prefix". The [record_component_association_list](./AA-4.3#S0108) corresponds to exactly what would be needed if there were no ancestor/prefix type. The [ancestor_part](./AA-4.3#S0112) determines the value of the ancestor/prefix. 


#### Syntax

extension_aggregate<a id="S0111"></a> ::= 
    ([ancestor_part](./AA-4.3#S0112) with [record_component_association_list](./AA-4.3#S0108))

ancestor_part<a id="S0112"></a> ::= [expression](./AA-4.4#S0132) | [subtype_mark](./AA-3.2#S0028)


#### Name Resolution Rules

{AI95-00287-01} The expected type for an [extension_aggregate](./AA-4.3#S0111) shall be a single type that is a record extension. If the [ancestor_part](./AA-4.3#S0112) is an [expression](./AA-4.4#S0132), it is expected to be of any tagged type. 

Reason: We could have made the expected type T'Class where T is the ultimate ancestor of the type of the aggregate, or we could have made it even more specific than that. However, if the overload resolution rules get too complicated, the implementation gets more difficult and it becomes harder to produce good error messages. 

Ramification: {AI05-0005-1} This rule is additive with the rule given in 4.3. That means the 4.3 rule must be satisfied even though it is always syntactically possible to tell that something is an extension aggregate rather than another kind of aggregate. Specifically, that means that an extension aggregate is ambiguous if the context is overloaded on array and/or untagged record types, even though those are never legal contexts for an extension aggregate. Thus, this rule acts more like a Legality Rule than a Name Resolution Rule. 


#### Legality Rules

{AI95-00306-01} {AI05-0115-1} If the [ancestor_part](./AA-4.3#S0112) is a [subtype_mark](./AA-3.2#S0028), it shall denote a specific tagged subtype. If the [ancestor_part](./AA-4.3#S0112) is an [expression](./AA-4.4#S0132), it shall not be dynamically tagged. The type of the [extension_aggregate](./AA-4.3#S0111) shall be a descendant of the type of the [ancestor_part](./AA-4.3#S0112) (the ancestor type), through one or more record extensions (and no private extensions). If the [ancestor_part](./AA-4.3#S0112) is a [subtype_mark](./AA-3.2#S0028), the view of the ancestor type from which the type is descended (see 7.3.1) shall not have unknown discriminants. 

Reason: {AI95-00306-01} The expression cannot be dynamically tagged to prevent implicit "truncation" of a dynamically-tagged value to the specific ancestor type. This is similar to the rules in 3.9.2. 

{AI05-0067-1} {AI05-0244-1} {AI12-0236-1} {AI12-0317-1} If the type of the [ancestor_part](./AA-4.3#S0112) is limited and at least one component is needed in the [record_component_association_list](./AA-4.3#S0108), then the [ancestor_part](./AA-4.3#S0112) shall not have an operative constituent expression (see 4.4) that is a call to a function with an unconstrained result subtype. 

Reason: {AI05-0067-1} {AI05-0244-1} This restriction simplifies implementation, because it ensures that either the caller or the callee knows the size to allocate for the aggregate. Without this restriction, information from both caller and callee would have to be combined to determine the appropriate size.

{AI05-0067-1} The (F(...) with null record) case is exempt from this rule, because such extension aggregates are created internally for inherited functions returning null-extension types - we can't very well make those illegal. Moreover, we don't need the rule for null extensions, as the result can simply use the space returned by the function call.

{AI12-0317-1} The mention of "operative constituents" means that constructs like parenthesized expressions, [qualified_expression](./AA-4.7#S0163)s, and [conditional_expression](./AA-4.5#S0148)s are ignored when enforcing this rule. 


#### Static Semantics

For the [record_component_association_list](./AA-4.3#S0108) of an [extension_aggregate](./AA-4.3#S0111), the only components needed are those of the composite value defined by the aggregate that are not inherited from the type of the [ancestor_part](./AA-4.3#S0112), plus any inherited discriminants if the [ancestor_part](./AA-4.3#S0112) is a [subtype_mark](./AA-3.2#S0028) that denotes an unconstrained subtype. 


#### Dynamic Semantics

For the evaluation of an [extension_aggregate](./AA-4.3#S0111), the [record_component_association_list](./AA-4.3#S0108) is evaluated. If the [ancestor_part](./AA-4.3#S0112) is an [expression](./AA-4.4#S0132), it is also evaluated; if the [ancestor_part](./AA-4.3#S0112) is a [subtype_mark](./AA-3.2#S0028), the components of the value of the aggregate not given by the [record_component_association_list](./AA-4.3#S0108) are initialized by default as for an object of the ancestor type. Any implicit initializations or evaluations are performed in an arbitrary order, except that the [expression](./AA-4.4#S0132) for a discriminant is evaluated prior to any other evaluation or initialization that depends on it.

{AI05-0282-1} If the type of the [ancestor_part](./AA-4.3#S0112) has discriminants and the [ancestor_part](./AA-4.3#S0112) is not a [subtype_mark](./AA-3.2#S0028) that denotes an unconstrained subtype, then a check is made that each discriminant determined by the [ancestor_part](./AA-4.3#S0112) has the value specified for a corresponding discriminant, if any, either in the [record_component_association_list](./AA-4.3#S0108), or in the [derived_type_definition](./AA-3.4#S0035) for some ancestor of the type of the [extension_aggregate](./AA-4.3#S0111). Constraint_Error is raised if this check fails. 

Ramification: Corresponding and specified discriminants are defined in 3.7. The rules requiring static compatibility between new discriminants of a derived type and the parent discriminant(s) they constrain ensure that at most one check is required per discriminant of the ancestor expression.

{AI05-0282-1} The check needs to be made any time that the ancestor is constrained; the source of the discriminants or the constraints is irrelevant. 

NOTE 1   If all components of the value of the [extension_aggregate](./AA-4.3#S0111) are determined by the [ancestor_part](./AA-4.3#S0112), then the [record_component_association_list](./AA-4.3#S0108) is required to be simply null record.

NOTE 2   If the [ancestor_part](./AA-4.3#S0112) is a [subtype_mark](./AA-3.2#S0028), then its type can be abstract. If its type is controlled, then as the last step of evaluating the aggregate, the Initialize procedure of the ancestor type is called, unless the Initialize procedure is abstract (see 7.6). 


#### Examples

Examples of extension aggregates (for types defined in 3.9.1): 

```ada
Painted_Point'(Point with Red)
(Point'(P) with Paint =&gt Black)

```

```ada
{AI12-0178-1} (Expression with Left =&gt  new Literal'(Value =&gt 1.2),
                 Right =&gt new Literal'(Value =&gt 3.4))
Addition'(Binop with null record)
             -- presuming Binop is of type Binary_Operation

```


#### Extensions to Ada 83

The extension aggregate syntax is new. 


#### Incompatibilities With Ada 95

{AI95-00306-01} Amendment Correction: Eliminated implicit "truncation" of a dynamically tagged value when it is used as an ancestor [expression](./AA-4.4#S0132). If an [aggregate](./AA-4.3#S0106) includes such an [expression](./AA-4.4#S0132), it is illegal in Ada 2005. Such [aggregate](./AA-4.3#S0106)s are thought to be rare; the problem can be fixed with a type conversion to the appropriate specific type if it occurs. 


#### Wording Changes from Ada 95

{AI95-00287-01} Limited [extension_aggregate](./AA-4.3#S0111)s are allowed (since all kinds of aggregates can now be limited, see 4.3). 


#### Inconsistencies With Ada 2005

{AI05-0282-1} Correction: An [extension_aggregate](./AA-4.3#S0111) with an [ancestor_part](./AA-4.3#S0112) whose discriminants are constrained and inherited might now raise Constraint_Error if the [aggregate](./AA-4.3#S0106)'s type is constrained, while it was OK in Ada 2005. In almost all cases, this will make no difference as the constraint will be checked by the immediately following use of the [aggregate](./AA-4.3#S0106), but it is possible to compare such an aggregate for equality; in this case, no exception would be raised by Ada 2005, while Ada 2012 will raise Constraint_Error. This should be very rare, and having the possibility means that the representation of the aggregate type has to be able to support unconstrained values of the type, even if the first subtype is constrained and no such objects can be created any other way. 


#### Incompatibilities With Ada 2005

{AI05-0067-1} Correction: A limited unconstrained ancestor expression that is a function call is now illegal unless the extension part is null. Such [aggregate](./AA-4.3#S0106)s were first introduced in Ada 2005 and are very complex to implement as they must be built-in-place with an unknown size; as such, it is unlikely that they are implemented correctly in existing compilers and thus not often used in existing code.

{AI05-0115-1} Correction: An [ancestor_part](./AA-4.3#S0112) that is a subtype with unknown discriminants is now explicitly illegal. Such a subtype should not be used to declare an object, and the [ancestor_part](./AA-4.3#S0112) acts like an object. The Ada 95 rules did not disallow such cases, so it is possible that code exists that uses such an ancestor, but this should be rare. 


#### Wording Changes from Ada 2012

{AI12-0317-1} Rewrote a Legality Rule to use the new term "operative constituent" in order to reduce duplication of text about ignoring parenthesized expressions and similar constructs. 


## 4.3.3  Array Aggregates

[In an [array_aggregate](./AA-4.3#S0113), a value is specified for each component of an array, either positionally or by its index.] For a [positional_array_aggregate](./AA-4.3#S0114), the components are given in increasing-index order, with a final others, if any, representing any remaining components. For a [named_array_aggregate](./AA-4.3#S0116), the components are identified by the values covered by the [discrete_choice](./AA-3.8#S0074)s.


#### Language Design Principles

The rules in this subclause are based on terms and rules for [discrete_choice_list](./AA-3.8#S0073)s defined in 3.8.1, "Variant Parts and Discrete Choices". For example, the requirements that others come last and stand alone are found there. 


#### Syntax

{AI12-0306-1} array_aggregate<a id="S0113"></a> ::= 
    [positional_array_aggregate](./AA-4.3#S0114) | [null_array_aggregate](./AA-4.3#S0115) | [named_array_aggregate](./AA-4.3#S0116)

{AI95-00287-01} {AI12-0212-1} {AI12-0306-1} positional_array_aggregate<a id="S0114"></a> ::= 
    ([expression](./AA-4.4#S0132), [expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)})
  | ([expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)}, others =&gt [expression](./AA-4.4#S0132))
  | ([expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)}, others =&gt &lt&gt)
  | '[' [expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)}[, others =&gt [expression](./AA-4.4#S0132)] ']'
  | '[' [expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)}, others =&gt &lt&gt ']'

{AI12-0306-1} null_array_aggregate<a id="S0115"></a> ::= '[' ']'

{AI12-0127-1} {AI12-0212-1} named_array_aggregate<a id="S0116"></a> ::= 
    ([array_component_association_list](./AA-4.3#S0117))
  | '[' [array_component_association_list](./AA-4.3#S0117) ']'

{AI12-0127-1} array_component_association_list<a id="S0117"></a> ::= 
    [array_component_association](./AA-4.3#S0118) {, [array_component_association](./AA-4.3#S0118)}

{AI95-00287-01} {AI12-0061-1} array_component_association<a id="S0118"></a> ::= 
    [discrete_choice_list](./AA-3.8#S0073) =&gt [expression](./AA-4.4#S0132)
  | [discrete_choice_list](./AA-3.8#S0073) =&gt &lt&gt
  | [iterated_component_association](./AA-4.3#S0119)

{AI12-0061-1} {AI12-0212-1} iterated_component_association<a id="S0119"></a> ::= 
    for [defining_identifier](./AA-3.1#S0022) in [discrete_choice_list](./AA-3.8#S0073) =&gt [expression](./AA-4.4#S0132)
  | for [iterator_specification](./AA-5.5#S0183) =&gt [expression](./AA-4.4#S0132)

An n-dimensional [array_aggregate](./AA-4.3#S0113) is one that is written as n levels of nested [array_aggregate](./AA-4.3#S0113)s (or at the bottom level, equivalent [string_literal](./AA-2.6#S0016)s). For the multidimensional case (n &gt= 2) the [array_aggregate](./AA-4.3#S0113)s (or equivalent [string_literal](./AA-2.6#S0016)s) at the n1 lower levels are called subaggregates of the enclosing n-dimensional [array_aggregate](./AA-4.3#S0113). The [expression](./AA-4.4#S0132)s of the bottom level subaggregates (or of the [array_aggregate](./AA-4.3#S0113) itself if one-dimensional) are called the array component expressions of the enclosing n-dimensional [array_aggregate](./AA-4.3#S0113). 

Ramification: Subaggregates do not have a type. They correspond to part of an array. For example, with a matrix, a subaggregate would correspond to a single row of the matrix. The definition of "n-dimensional" [array_aggregate](./AA-4.3#S0113) applies to subaggregates as well as [aggregate](./AA-4.3#S0106)s that have a type. 

To be honest: An others choice is the reserved word others as it appears in a [positional_array_aggregate](./AA-4.3#S0114) or as the [discrete_choice](./AA-3.8#S0074) of the [discrete_choice_list](./AA-3.8#S0073) in an [array_component_association](./AA-4.3#S0118). 

{AI12-0061-1} The [defining_identifier](./AA-3.1#S0022) of an [iterated_component_association](./AA-4.3#S0119) declares an index parameter, an object of the corresponding index type.


#### Name Resolution Rules

{AI95-00287-01} The expected type for an [array_aggregate](./AA-4.3#S0113) (that is not a subaggregate) shall be a single array type. The component type of this array type is the expected type for each array component expression of the [array_aggregate](./AA-4.3#S0113). 

Ramification: {AI95-00287-01} We already require a single array or record type or record extension for an [aggregate](./AA-4.3#S0106). The above rule requiring a single array type (and similar ones for record and extension aggregates) resolves which kind of aggregate you have. 

The expected type for each [discrete_choice](./AA-3.8#S0074) in any [discrete_choice_list](./AA-3.8#S0073) of a [named_array_aggregate](./AA-4.3#S0116) is the type of the corresponding index; the corresponding index for an [array_aggregate](./AA-4.3#S0113) that is not a subaggregate is the first index of its type; for an (nm)-dimensional subaggregate within an [array_aggregate](./AA-4.3#S0113) of an n-dimensional type, the corresponding index is the index in position m+1. 


#### Legality Rules

{AI12-0212-1} An [array_aggregate](./AA-4.3#S0113) of an n-dimensional array type shall be written as an n-dimensional [array_aggregate](./AA-4.3#S0113), or as a [null_array_aggregate](./AA-4.3#S0115). 

Ramification: In an m-dimensional [array_aggregate](./AA-4.3#S0113) [(including a subaggregate)], where m &gt= 2, each of the [expression](./AA-4.4#S0132)s has to be an (m1)-dimensional subaggregate. 

{AI12-0418-1} An others choice is allowed for an [array_aggregate](./AA-4.3#S0113) only if an applicable index constraint applies to the [array_aggregate](./AA-4.3#S0113). [An applicable index constraint is a constraint provided by certain contexts that can be used to determine the bounds of the array value specified by an [array_aggregate](./AA-4.3#S0113).] Each of the following contexts (and none other) defines an applicable index constraint: 

{AI95-00318-02} {AI12-0157-1} For an [explicit_actual_parameter](./AA-6.4#S0221), an [explicit_generic_actual_parameter](./AA-12.3#S0318), the [expression](./AA-4.4#S0132) of a return statement, the return expression of an expression function, the initialization expression in an [object_declaration](./AA-3.3#S0032), or a [default_expression](./AA-3.7#S0063) [(for a parameter or a component)], when the nominal subtype of the corresponding formal parameter, generic formal parameter, function return object, expression function return object, object, or component is a constrained array subtype, the applicable index constraint is the constraint of the subtype;

For the [expression](./AA-4.4#S0132) of an [assignment_statement](./AA-5.2#S0173) where the [name](./AA-4.1#S0091) denotes an array variable, the applicable index constraint is the constraint of the array variable; 

Reason: This case is broken out because the constraint comes from the actual subtype of the variable (which is always constrained) rather than its nominal subtype (which might be unconstrained). 

For the operand of a [qualified_expression](./AA-4.7#S0163) whose [subtype_mark](./AA-3.2#S0028) denotes a constrained array subtype, the applicable index constraint is the constraint of the subtype;

For a component [expression](./AA-4.4#S0132) in an [aggregate](./AA-4.3#S0106), if the component's nominal subtype is a constrained array subtype, the applicable index constraint is the constraint of the subtype; 

Discussion: Here, the [array_aggregate](./AA-4.3#S0113) with others is being used within a larger aggregate. 

{AI12-0127-1} For the base_[expression](./AA-4.4#S0132) of a [delta_aggregate](./AA-4.3#S0120), if the nominal subtype of the [delta_aggregate](./AA-4.3#S0120) is a constrained array subtype, the applicable index constraint is the constraint of the subtype;

{AI05-0147-1} For a parenthesized [expression](./AA-4.4#S0132), the applicable index constraint is that, if any, defined for the [expression](./AA-4.4#S0132); 

Discussion: RM83 omitted this case, presumably as an oversight. We want to minimize situations where an [expression](./AA-4.4#S0132) becomes illegal if parenthesized. 

{AI05-0147-1} {AI12-0236-1} For a [conditional_expression](./AA-4.5#S0148) (see 4.5.7), the applicable index constraint for each dependent_[expression](./AA-4.4#S0132) is that, if any, defined for the [conditional_expression](./AA-4.5#S0148);

{AI05-0147-1} For a [declare_expression](./AA-4.5#S0156) (see 4.5.9), the applicable index constraint for the body_[expression](./AA-4.4#S0132) is that, if any, defined for the [declare_expression](./AA-4.5#S0156). 

The applicable index constraint applies to an [array_aggregate](./AA-4.3#S0113) that appears in such a context, as well as to any subaggregates thereof. In the case of an [explicit_actual_parameter](./AA-6.4#S0221) (or [default_expression](./AA-3.7#S0063)) for a call on a generic formal subprogram, no applicable index constraint is defined. 

Reason: This avoids generic contract model problems, because only mode conformance is required when matching actual subprograms with generic formal subprograms. 

{AI05-0153-3} {AI12-0061-1} {AI12-0127-1} The [discrete_choice_list](./AA-3.8#S0073) of an [array_component_association](./AA-4.3#S0118) (including an [iterated_component_association](./AA-4.3#S0119)) is allowed to have a [discrete_choice](./AA-3.8#S0074) that is a nonstatic [choice_expression](./AA-4.4#S0133) or that is a [subtype_indication](./AA-3.2#S0027) or [range](./AA-3.5#S0037) that defines a nonstatic or null range, only if it is the single [discrete_choice](./AA-3.8#S0074) of its [discrete_choice_list](./AA-3.8#S0073), and either there is only one [array_component_association](./AA-4.3#S0118) in the enclosing [array_component_association_list](./AA-4.3#S0117) or the enclosing [aggregate](./AA-4.3#S0106) is an [array_delta_aggregate](./AA-4.3#S0122)[, not an [array_aggregate](./AA-4.3#S0113)].

Discussion: We now allow a nonstatic others choice even if there are other array component expressions as well. 

Ramification: {AI12-0127-1} We allow multiple dynamic choices in [array_delta_aggregate](./AA-4.3#S0122)s, but only one dynamic choice per association even in that case. 

{AI12-0212-1} Either all or none of the [array_component_association](./AA-4.3#S0118)s of an [array_component_association_list](./AA-4.3#S0117) shall be [iterated_component_association](./AA-4.3#S0119)s with an [iterator_specification](./AA-5.5#S0183).

{AI05-0262-1} In a [named_array_aggregate](./AA-4.3#S0116) where all [discrete_choice](./AA-3.8#S0074)s are static, no two [discrete_choice](./AA-3.8#S0074)s are allowed to cover the same value (see 3.8.1); if there is no others choice, the [discrete_choice](./AA-3.8#S0074)s taken together shall exactly cover a contiguous sequence of values of the corresponding index type. 

Ramification: This implies that each component must be specified exactly once. See AI83-309. 

Reason: {AI05-0262-1} This has to apply even if there is only one static [discrete_choice](./AA-3.8#S0074); a single choice has to represent a contiguous range (a [subtype_mark](./AA-3.2#S0028) with a static predicate might represent a discontiguous set of values). If the (single) choice is a dynamic subtype, we don't need to make this check as no predicates are allowed (see 3.2.4) and thus the range has to be contiguous. 

A bottom level subaggregate of a multidimensional [array_aggregate](./AA-4.3#S0113) of a given array type is allowed to be a [string_literal](./AA-2.6#S0016) only if the component type of the array type is a character type; each character of such a [string_literal](./AA-2.6#S0016) shall correspond to a [defining_character_literal](./AA-3.5#S0040) of the component type. 


#### Static Semantics

A subaggregate that is a [string_literal](./AA-2.6#S0016) is equivalent to one that is a [positional_array_aggregate](./AA-4.3#S0114) of the same length, with each [expression](./AA-4.4#S0132) being the [character_literal](./AA-2.5#S0015) for the corresponding character of the [string_literal](./AA-2.6#S0016).

To be honest: {AI12-0306-1} This is true even in cases where there is no corresponding legal [positional_array_aggregate](./AA-4.3#S0114). 

{AI12-0061-1} The subtype (and nominal subtype) of an index parameter is the corresponding index subtype. 


#### Dynamic Semantics

{AI12-0212-1} For an [array_aggregate](./AA-4.3#S0113) that contains only [array_component_association](./AA-4.3#S0118)s that are [iterated_component_association](./AA-4.3#S0119)s with [iterator_specification](./AA-5.5#S0183)s, evaluation proceeds in two steps: 

a){AI12-0212-1} {AI12-0250-1} Each [iterator_specification](./AA-5.5#S0183) is elaborated (in an arbitrary order) and an iteration is performed solely to determine a maximum count for the number of values produced by the iteration; all of these counts are combined to determine the overall length of the array, and ultimately the limits on the bounds of the array (defined below);

b){AI12-0212-1} {AI12-0250-1} {AI12-0327-1} A second iteration is performed for each of the [iterator_specification](./AA-5.5#S0183)s, in the order given in the [aggregate](./AA-4.3#S0106), and for each value conditionally produced by the iteration (see 5.5 and 5.5.2), the associated [expression](./AA-4.4#S0132) is evaluated, its value is converted to the component subtype of the array type, and used to define the value of the next component of the array starting at the low bound and proceeding sequentially toward the high bound. A check is made that the second iteration results in an array length no greater than the maximum determined by the first iteration; Constraint_Error is raised if this check fails.

To be honest: {AI12-0212-1} {AI12-0250-1} Constraint_Error should be raised no later than when the iterations exceed the maximum array length; memory that doesn't belong to the aggregate temporary should not be overwritten. 

{AI12-0212-1} The evaluation of any other [array_aggregate](./AA-4.3#S0113) of a given array type proceeds in two steps: 

a)Any [discrete_choice](./AA-3.8#S0074)s of this aggregate and of its subaggregates are evaluated in an arbitrary order, and converted to the corresponding index type; 

b)The array component expressions of the aggregate are evaluated in an arbitrary order and their values are converted to the component subtype of the array type; an array component expression is evaluated once for each associated component. 

Ramification: Subaggregates are not separately evaluated. The conversion of the value of the component expressions to the component subtype might raise Constraint_Error.

{AI05-0005-1} We don't need to say that &lt&gt is evaluated once for each component, as &lt&gt means that each component is initialized by default. That means that the actions defined for default initialization are applied to each component individually. Initializing one component by default and copying that to the others would be an incorrect implementation in general (although it might be OK if the default initialization is known to be constant). 

{AI95-00287-01} {AI12-0084-1} Each [expression](./AA-4.4#S0132) in an [array_component_association](./AA-4.3#S0118) defines the value for the associated component(s). For an [array_component_association](./AA-4.3#S0118) with &lt&gt, the associated component(s) are initialized to the Default_Component_Value of the array type if this aspect has been specified for the array type; otherwise, they are initialized by default as for a stand-alone object of the component subtype (see 3.3.1).

{AI12-0061-1} {AI12-0212-1} During an evaluation of the [expression](./AA-4.4#S0132) of an [iterated_component_association](./AA-4.3#S0119) with a [discrete_choice_list](./AA-3.8#S0073), the value of the corresponding index parameter is that of the corresponding index of the corresponding array component. During an evaluation of the [expression](./AA-4.4#S0132) of an [iterated_component_association](./AA-4.3#S0119) with an [iterator_specification](./AA-5.5#S0183), the value of the loop parameter of the [iterator_specification](./AA-5.5#S0183) is the value produced by the iteration (as described in 5.5.2).

Ramification: Taken together with the preceding rule that "The array component expressions of the aggregate are evaluated in an arbitrary order", this implies that an index parameter of an [iterated_component_association](./AA-4.3#S0119) can take on its values in an arbitrary order. This is different from other constructs, such as a [loop_statement](./AA-5.5#S0178). In contrast, a loop parameter of an [iterated_component_association](./AA-4.3#S0119) takes its values in the order defined by the iteration, the same as a [loop_statement](./AA-5.5#S0178). 

The bounds of the index range of an [array_aggregate](./AA-4.3#S0113) [(including a subaggregate)] are determined as follows: 

For an [array_aggregate](./AA-4.3#S0113) with an others choice, the bounds are those of the corresponding index range from the applicable index constraint;

For a [positional_array_aggregate](./AA-4.3#S0114) [(or equivalent [string_literal](./AA-2.6#S0016))] without an others choice, the lower bound is that of the corresponding index range in the applicable index constraint, if defined, or that of the corresponding index subtype, if not; in either case, the upper bound is determined from the lower bound and the number of [expression](./AA-4.4#S0132)s [(or the length of the [string_literal](./AA-2.6#S0016))];

{AI12-0212-1} {AI12-0306-1} For a [null_array_aggregate](./AA-4.3#S0115), bounds for each dimension are determined as for a [positional_array_aggregate](./AA-4.3#S0114) without an others choice that has no expressions for each dimension;

Reason: We need a separate rule to describe what happens for a multidimensional [null_array_aggregate](./AA-4.3#S0115); we could've combined the single-dimension case with the [positional_array_aggregate](./AA-4.3#S0114) rule. 

{AI12-0212-1} {AI12-0250-1} For a [named_array_aggregate](./AA-4.3#S0116) containing only [iterated_component_association](./AA-4.3#S0119)s with an [iterator_specification](./AA-5.5#S0183), the lower bound is determined as for a [positional_array_aggregate](./AA-4.3#S0114) without an others choice, and the upper bound is determined from the lower bound and the total number of values produced by the second set of iterations;

{AI12-0212-1} For any other [named_array_aggregate](./AA-4.3#S0116) without an others choice, the bounds are determined by the smallest and largest index values covered by any [discrete_choice_list](./AA-3.8#S0073). 

Reason: We don't need to say that each index value has to be covered exactly once, since that is a ramification of the general rule on [aggregate](./AA-4.3#S0106)s that each component's value has to be specified exactly once. 

For an [array_aggregate](./AA-4.3#S0113), a check is made that the index range defined by its bounds is compatible with the corresponding index subtype. 

Discussion: In RM83, this was phrased more explicitly, but once we define "compatibility" between a range and a subtype, it seems to make sense to take advantage of that definition. 

Ramification: The definition of compatibility handles the special case of a null range, which is always compatible with a subtype. See AI83-00313. 

{AI05-0037-1} For an [array_aggregate](./AA-4.3#S0113) with an others choice, a check is made that no [expression](./AA-4.4#S0132) or &lt&gt is specified for an index value outside the bounds determined by the applicable index constraint. 

Discussion: RM83 omitted this case, apparently through an oversight. AI83-00309 defines this as a dynamic check, even though other Ada 83 rules ensured that this check could be performed statically. We now allow an others choice to be dynamic, even if it is not the only choice, so this check now needs to be dynamic, in some cases. Also, within a generic unit, this would be a nonstatic check in some cases. 

For a multidimensional [array_aggregate](./AA-4.3#S0113), a check is made that all subaggregates that correspond to the same index have the same bounds. 

Ramification: No array bounds "sliding" is performed on subaggregates. 

Reason: If sliding were performed, it would not be obvious which subaggregate would determine the bounds of the corresponding index. 

The exception Constraint_Error is raised if any of the above checks fail. 


#### Implementation Permissions

{AI12-0212-1} {AI12-0250-1} When evaluating [iterated_component_association](./AA-4.3#S0119)s for an [array_aggregate](./AA-4.3#S0113) that contains only [iterated_component_association](./AA-4.3#S0119)s with [iterator_specification](./AA-5.5#S0183)s, the first step of evaluating an [iterated_component_association](./AA-4.3#S0119) can be omitted if the implementation can determine the maximum number of values by some other means.

Discussion: For instance, if the type of the aggregate is constrained, the implementation can (but does not have to) calculate the expected length from the constraint. 

NOTE 1   {AI05-0004-1} {AI12-0306-1} {AI12-0440-1} In an [array_aggregate](./AA-4.3#S0113) delimited by parentheses, positional notation can only be used with two or more [expression](./AA-4.4#S0132)s; a single [expression](./AA-4.4#S0132) in parentheses is interpreted as a parenthesized expression. An [array_aggregate](./AA-4.3#S0113) delimited by square brackets can be used to specify an array with a single component.

NOTE 2   {AI12-0061-1} An index parameter is a constant object (see 3.3). 


#### Examples

Examples of array aggregates with positional associations: 

```ada
(7, 9, 5, 1, 3, 2, 4, 8, 6, 0)
Table'(5, 8, 4, 1, others =&gt 0)  --  see 3.6 

```

Examples of array aggregates with named associations: 

```ada
{AI12-0306-1} (1 .. 5 =&gt (1 .. 8 =&gt 0.0))      --  two-dimensional
[1 .. N =&gt new Cell]             --  N new cells, in particular for N = 0

```

```ada
{AI12-0306-1} Table'(2 | 4 | 10 =&gt 1, others =&gt 0)
Schedule'(Mon .. Fri =&gt True,  others =&gt False)  --  see 3.6
Schedule'[Wed | Sun  =&gt False, others =&gt True]
Vector'(1 =&gt 2.5)                                --  single-component vector

```

Examples of two-dimensional array aggregates: 

```ada
-- Three aggregates for the same value of subtype Matrix(1..2,1..3) (see 3.6):

```

```ada
{AI12-0306-1} ((1.1, 1.2, 1.3), (2.1, 2.2, 2.3))
(1 =&gt [1.1, 1.2, 1.3], 2 =&gt [2.1, 2.2, 2.3])
[1 =&gt (1 =&gt 1.1, 2 =&gt 1.2, 3 =&gt 1.3), 2 =&gt (1 =&gt 2.1, 2 =&gt 2.2, 3 =&gt 2.3)]

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

```ada
{AI12-0061-1} G : constant Matrix :=
    (for I in 1 .. 4 =&gt
       (for J in 1 .. 4 =&gt
          (if I=J then 1.0 else 0.0))); -- Identity matrix

```

```ada
{AI12-0312-1} Empty_Matrix : constant Matrix := []; -- A matrix without elements

```

{AI95-00433-01} Example of an array aggregate with defaulted others choice and with an applicable index constraint provided by an enclosing record aggregate:

```ada
{AI12-0178-1} Buffer'(Size =&gt 50, Pos =&gt 1, Value =&gt ('x', others =&gt &lt&gt))  -- see 3.7

```


#### Incompatibilities With Ada 83

In Ada 95, no applicable index constraint is defined for a parameter in a call to a generic formal subprogram; thus, some aggregates that are legal in Ada 83 are illegal in Ada 95. For example: 

```ada
subtype S3 is String (1 .. 3);
...
generic
   with function F (The_S3 : in S3) return Integer;
package Gp is
   I : constant Integer := F ((1 =&gt '!', others =&gt '?'));
       -- The aggregate is legal in Ada 83, illegal in Ada 95.
end Gp;

```

This change eliminates generic contract model problems. 


#### Extensions to Ada 83

We now allow "named with others" aggregates in all contexts where there is an applicable index constraint, effectively eliminating what was RM83-4.3.2(6). Sliding never occurs on an aggregate with others, because its bounds come from the applicable index constraint, and therefore already match the bounds of the target.

The legality of an others choice is no longer affected by the staticness of the applicable index constraint. This substantially simplifies several rules, while being slightly more flexible for the user. It obviates the rulings of AI83-00244 and AI83-00310, while taking advantage of the dynamic nature of the "extra values" check required by AI83-00309.

Named array aggregates are permitted even if the index type is descended from a formal scalar type. See 4.9 and AI83-00190. 


#### Wording Changes from Ada 83

We now separate named and positional array aggregate syntax, since, unlike other aggregates, named and positional associations cannot be mixed in array aggregates (except that an others choice is allowed in a positional array aggregate).

We have also reorganized the presentation to handle multidimensional and one-dimensional aggregates more uniformly, and to incorporate the rulings of AI83-00019, AI83-00309, etc. 


#### Extensions to Ada 95

{AI95-00287-01} &lt&gt can be used in place of an [expression](./AA-4.4#S0132) in an [array_aggregate](./AA-4.3#S0113), default-initializing the component. 


#### Wording Changes from Ada 95

{AI95-00287-01} Limited [array_aggregate](./AA-4.3#S0113)s are allowed (since all kinds of aggregates can now be limited, see 4.3).

{AI95-00318-02} Fixed [aggregate](./AA-4.3#S0106)s to use the subtype of the return object of a function, rather than the result subtype, because they can be different for an [extended_return_statement](./AA-6.5#S0225), and we want to use the subtype that's explicitly in the code at the point of the [expression](./AA-4.4#S0132). 


#### Inconsistencies With Ada 2005

{AI05-0037-1} Correction: Fixed so the check for components outside of the array applies to both [expression](./AA-4.4#S0132)s and &lt&gts. As &lt&gt was a new feature in Ada 2005, there should be little existing code that depends on a &lt&gt component that is specified outside of the array (and that is nonsense anyway, that a compiler is likely to detect even without an explicit language rule disallowing it). 


#### Wording Changes from Ada 2005

{AI05-0147-1} Added a definition of the applicable index constraint for [conditional_expression](./AA-4.5#S0148)s (which are new). 


#### Inconsistencies With Ada 2012

{AI05-0084-1} Corrigendum: Fixed so that the Default_Component_Value (if any) is used to initialize components specified with &lt&gt. This is what users would expect, and all Ada 2012 implementation known at the time of this writing initialize with the Default_Component_Value, so it is unlikely that anyone will be affected by this inconsistency. 


#### Extensions to Ada 2012

{AI12-0061-1} {AI12-0212-1} {AI12-0250-1} The [iterated_component_association](./AA-4.3#S0119) is new (more than 25 years after it was originally proposed). It has been extended to allow container iterators as well as the basic index iterators.

{AI12-0212-1} An [array_aggregate](./AA-4.3#S0113) can be surrounded by brackets rather than parentheses. This allows consistent writing of zero- and one-component positional array aggregates, and is consistent with the syntax for writing container aggregates.

{AI12-0212-1} {AI12-0306-1} A [null_array_aggregate](./AA-4.3#S0115) is new. 


#### Wording Changes from Ada 2012

{AI05-0157-1} Corrigendum: Added expression functions to the contexts that provide an applicable index constraint, because expression functions are handled separately in static semantics and legality rules.

{AI12-0127-1} Made syntax and wording changes for [delta_aggregate](./AA-4.3#S0120)s.

{AI12-0236-1} Added a definition of the applicable index constraint for [declare_expression](./AA-4.5#S0156)s (which are new). This allows others in array aggregates inside of [declare_expression](./AA-4.5#S0156)s. 


## 4.3.4  Delta Aggregates

{AI12-0127-1} {AI12-0324-1} Evaluating a (record or array) delta aggregate yields a composite value that starts with a copy of another value of the same type and then assigns to some (but typically not all) components of the copy. 


#### Syntax

{AI12-0127-1} delta_aggregate<a id="S0120"></a> ::= [record_delta_aggregate](./AA-4.3#S0121) | [array_delta_aggregate](./AA-4.3#S0122)

{AI12-0127-1} record_delta_aggregate<a id="S0121"></a> ::= 
    (base_[expression](./AA-4.4#S0132) with delta [record_component_association_list](./AA-4.3#S0108))

{AI12-0127-1} {AI12-0212-1} array_delta_aggregate<a id="S0122"></a> ::= 
    (base_[expression](./AA-4.4#S0132) with delta [array_component_association_list](./AA-4.3#S0117))
  | '[' base_[expression](./AA-4.4#S0132) with delta [array_component_association_list](./AA-4.3#S0117) ']'


#### Name Resolution Rules

{AI12-0127-1} The expected type for a [record_delta_aggregate](./AA-4.3#S0121) shall be a single descendant of a record type or record extension.

{AI12-0127-1} The expected type for an [array_delta_aggregate](./AA-4.3#S0122) shall be a single array type.

{AI12-0127-1} The expected type for the base_[expression](./AA-4.4#S0132) of any [delta_aggregate](./AA-4.3#S0120) is the type of the enclosing [delta_aggregate](./AA-4.3#S0120). 

{AI12-0127-1} [The Name Resolution Rules and Legality Rules for each [record_component_association](./AA-4.3#S0109) of a [record_delta_aggregate](./AA-4.3#S0121) are as defined in 4.3.1.]

{AI12-0127-1} For an [array_delta_aggregate](./AA-4.3#S0122), the expected type for each [discrete_choice](./AA-3.8#S0074) in an [array_component_association](./AA-4.3#S0118) is the index type of the type of the [delta_aggregate](./AA-4.3#S0120).

{AI12-0127-1} The expected type of the [expression](./AA-4.4#S0132) in an [array_component_association](./AA-4.3#S0118) is defined as for an [array_component_association](./AA-4.3#S0118) occurring within an [array_aggregate](./AA-4.3#S0113) of the type of the [delta_aggregate](./AA-4.3#S0120).


#### Legality Rules

{AI12-0127-1} For an [array_delta_aggregate](./AA-4.3#S0122), the [array_component_association](./AA-4.3#S0118) shall not use the box symbol &lt&gt, and the [discrete_choice](./AA-3.8#S0074) shall not be others.

{AI12-0127-1} For an [array_delta_aggregate](./AA-4.3#S0122), the dimensionality of the type of the [delta_aggregate](./AA-4.3#S0120) shall be 1.

{AI12-0127-1} For an [array_delta_aggregate](./AA-4.3#S0122), the base_[expression](./AA-4.4#S0132) and each [expression](./AA-4.4#S0132) in every [array_component_association](./AA-4.3#S0118) shall be of a nonlimited type.

Ramification: The base_[expression](./AA-4.4#S0132) of a [record_delta_aggregate](./AA-4.3#S0121) may be of a limited type (for example a record with limited components), as it is not restricted. A rule in 4.3.1 ensures that we do not assign to a limited component. We do not allow any part of an [array_delta_aggregate](./AA-4.3#S0122) to be of a limited type, even the base_[expression](./AA-4.4#S0132), as this is a useless construct (you would not be able to update anything because the components necessarily are also limited except in pathological cases). 


#### Dynamic Semantics

{AI12-0127-1} {AI12-0324-1} {AI12-0381-1} The evaluation of a [delta_aggregate](./AA-4.3#S0120) begins with the evaluation of the base_[expression](./AA-4.4#S0132) of the [delta_aggregate](./AA-4.3#S0120); then that value is used to create and initialize the anonymous object of the [aggregate](./AA-4.3#S0106). The bounds of the anonymous object of an [array_delta_aggregate](./AA-4.3#S0122) and the discriminants (if any) of the anonymous object of a [record_delta_aggregate](./AA-4.3#S0121) are those of the base_[expression](./AA-4.4#S0132). If a [record_delta_aggregate](./AA-4.3#S0121) is of a specific tagged type, its tag is that of the specific type; if it is of a class-wide type, its tag is that of the base_[expression](./AA-4.4#S0132).

Ramification: This is the same anonymous object as described in 7.6, "Assignment and Finalization"; in particular, it might be required to be built in place.

For a specific tagged type, any extension components that the actual object underlying the base_[expression](./AA-4.4#S0132) might have are not used to initialize the anonymous object, which is critical if the aggregate is required to be built-in-place. 

To be honest: The anonymous object associated with the evaluation of a [delta_aggregate](./AA-4.3#S0120) begins its life as a variable, not a constant (3.3 notwithstanding). This must be the case because the object is initialized and then subsequently modified. After evaluation of the [delta_aggregate](./AA-4.3#S0120) is complete, the object is a constant object. This is similar to the way that an extended return statement can provide a variable view of an object that will eventually be a constant object after the function returns its result. 

{AI12-0127-1} For a [record_delta_aggregate](./AA-4.3#S0121), for each component associated with each [record_component_association](./AA-4.3#S0109) (in an unspecified order): 

if the associated component belongs to a [variant](./AA-3.8#S0072), a check is made that the values of the discriminants are such that the anonymous object has this component. The exception Constraint_Error is raised if this check fails.

the [expression](./AA-4.4#S0132) of the [record_component_association](./AA-4.3#S0109) is evaluated, converted to the nominal subtype of the associated component, and assigned to the component of the anonymous object.

{AI12-0127-1} For an [array_delta_aggregate](./AA-4.3#S0122), for each [discrete_choice](./AA-3.8#S0074) of each [array_component_association](./AA-4.3#S0118) (in the order given in the enclosing [discrete_choice_list](./AA-3.8#S0073) and [array_component_association_list](./AA-4.3#S0117), respectively) the [discrete_choice](./AA-3.8#S0074) is evaluated; for each represented index value (in ascending order, if the [discrete_choice](./AA-3.8#S0074) represents a range): 

the index value is converted to the index type of the array type.

a check is made that the index value belongs to the index range of the anonymous object of the [aggregate](./AA-4.3#S0106); Constraint_Error is raised if this check fails.

the component [expression](./AA-4.4#S0132) is evaluated, converted to the array component subtype, and assigned to the component of the anonymous object identified by the index value. 

Reason: Unlike other [aggregate](./AA-4.3#S0106)s, an [array_delta_aggregate](./AA-4.3#S0122) is evaluated in the order that it is written. This is necessary to get deterministic behavior, as (unlike other [aggregate](./AA-4.3#S0106)s, including [record_delta_aggregate](./AA-4.3#S0121)s) there is no requirement that the specified components be distinct. As such, the order requirement ensures that every [array_delta_aggregate](./AA-4.3#S0122) has a well-defined result, even if the same component is specified multiple times. 


#### Examples

{AI12-0127-1} {AI12-0429-1} Examples of use of delta aggregates in a postcondition:

```ada
procedure Twelfth (D : in out Date) -- see 3.8 for type Date
   with Post =&gt D = (D'Old with delta Day =&gt 12);

```

```ada
procedure The_Answer (V : in out Vector;
                      A, B : in Integer) -- see 3.6 for type Vector
   with Post =&gt V = (V'Old with delta A .. B =&gt 42.0, V'First =&gt 0.0);

```

{AI12-0127-1} {AI12-0324-1} {AI12-0379-1} {AI12-0386-1} {AI12-0429-1} Examples where the base expression is nontrivial:

```ada
New_Cell : Cell := (Min_Cell (Head) with delta Value =&gt 42);
   -- see 3.10.1 for Cell and Head; 6.1 for Min_Cell

```

```ada
A1 : Vector := ((0 =&gt 1.0, 1 =&gt 2.0, 2 =&gt 3.0)
       with delta Integer(Random * 2.0) =&gt 14.2);
   -- see 3.6 for declaration of type Vector
   -- see 6.1 for declaration of Random

```

```ada
Tomorrow := ((Yesterday with delta Day =&gt 12)
                  with delta Month =&gt April); -- see 3.8

```

{AI12-0127-1} {AI12-0379-1} {AI12-0429-1} Example where the base expression is class-wide:

```ada
function Translate (P : Point'Class; X, Y : Real) return Point'Class is
   (P with delta X =&gt P.X + X,
                 Y =&gt P.Y + Y); -- see 3.9 for declaration of type Point

```


#### Extensions to Ada 2012

{AI12-0127-1} Delta aggregates are new. 


## 4.3.5  Container Aggregates

{AI12-0212-1} In a [container_aggregate](./AA-4.3#S0123), values are specified for elements of a container; for a [positional_container_aggregate](./AA-4.3#S0125), the elements are given sequentially; for a [named_container_aggregate](./AA-4.3#S0126), the elements are specified by a sequence of key/value pairs, or using an iterator. The Aggregate aspect of the type of the [aggregate](./AA-4.3#S0106) determines how the elements are combined to form the container.

Glossary entry: A container aggregate is a construct used to define a value of a type that represents a collection of elements, by explicitly specifying the elements in the collection.

Version=[5],Kind=(AddedNormal),Group=[C],Term=[container aggregate], Def=[a construct used to define a value of a type that represents a collection of elements, by explicitly specifying the elements in the collection]

{AI12-0212-1} For a type other than an array type, the following type-related operational aspect may be specified:

AggregateThis aspect is an [aggregate](./AA-4.3#S0106) of the form:

Aspect Description for Aggregate: Mechanism to define user-defined aggregates.

   (Empty =&gt [name](./AA-4.1#S0091)[,
    Add_Named =&gt procedure_[name](./AA-4.1#S0091)][,
    Add_Unnamed =&gt procedure_[name](./AA-4.1#S0091)][,
    New_Indexed =&gt function_[name](./AA-4.1#S0091),
    Assign_Indexed =&gt procedure_[name](./AA-4.1#S0091)])

Ramification: {AI22-0020-1} As this aspect is described with syntax, it has to be given exactly as specified here. That means it cannot be given as a positional aggregate, nor can the order of the elements be changed. For instance, New_Indexed always has to occur before Assign_Indexed, and Empty always has to be first. 

{AI12-0212-1} The type for which this aspect is specified is known as the container type of the Aggregate aspect. A procedure_[name](./AA-4.1#S0091) shall be specified for at least one of Add_Named, Add_Unnamed, or Assign_Indexed. If Add_Named is specified, neither Add_Unnamed nor Assign_Indexed shall be specified. Either both or neither of New_Indexed and Assign_Indexed shall be specified.


#### Name Resolution Rules

{AI12-0212-1} The [name](./AA-4.1#S0091) specified for Empty for an Aggregate aspect shall denote a constant of the container type, or denote exactly one function with a result type of the container type that has no parameters, or that has one in parameter of a signed integer type.

Reason: In the function case, the parameter, if present may be used to specify an initial size for the container, in anticipation of adding elements to it. For a positional aggregate, or a named aggregate that doesn't use an iterator, it will be initialized with the number of elements. For a named aggregate that uses an iterator, the implementation is permitted to estimate the number of elements that the iterator will produce, but it is not required to do so. 

{AI12-0212-1} {AI12-0437-1} The procedure_[name](./AA-4.1#S0091) specified for Add_Unnamed for an Aggregate aspect shall denote exactly one procedure that has two parameters, the first an in out parameter of the container type, and the second an in parameter of some nonlimited type, called the element type of the container type.

{AI12-0212-1} {AI12-0437-1} The function_[name](./AA-4.1#S0091) specified for New_Indexed for an Aggregate aspect shall denote exactly one function with a result type of the container type, and two parameters of the same discrete type, with that type being the key type of the container type.

Reason: The New_Indexed function is used instead of Empty as the first step of creating an aggregate that is initialized using the Assign_Indexed procedure. 

{AI12-0212-1} {AI12-0437-1} The procedure_[name](./AA-4.1#S0091) specified for Add_Named or Assign_Indexed for an Aggregate aspect shall denote exactly one procedure that has three parameters, the first an in out parameter of the container type, the second an in parameter of a nonlimited type (the key type of the container type), and the third, an in parameter of a nonlimited type that is called the element type of the container type.


#### Legality Rules

{AI12-0212-1} If the container type of an Aggregate aspect is a private type, the full type of the container type shall not be an array type. If the container type is limited, the name specified for Empty shall denote a function rather than a constant object.

{AI12-0212-1} For an Aggregate aspect, the key type of Assign_Indexed shall be the same type as that of the parameters of New_Indexed. Additionally, if both Add_Unnamed and Assign_Indexed are specified, the final parameters shall be of the same type - the element type of the container type.


#### Static Semantics

{AI12-0212-1} {AI12-0388-1} The Aggregate aspect is nonoverridable (see 13.1.1). 


#### Syntax

{AI12-0212-1} container_aggregate<a id="S0123"></a> ::= 
    [null_container_aggregate](./AA-4.3#S0124)
  | [positional_container_aggregate](./AA-4.3#S0125)
  | [named_container_aggregate](./AA-4.3#S0126)

{AI12-0212-1} null_container_aggregate<a id="S0124"></a> ::= '[' ']'

{AI12-0212-1} positional_container_aggregate<a id="S0125"></a> ::= '[' [expression](./AA-4.4#S0132){, [expression](./AA-4.4#S0132)} ']'

{AI12-0212-1} named_container_aggregate<a id="S0126"></a> ::= '[' [container_element_association_list](./AA-4.3#S0127) ']'

Reason: {AI12-0212-1} Unlike other [aggregate](./AA-4.3#S0106)s, container aggregates have to be surrounded with brackets rather than parentheses. Using brackets allows writing zero- and one-element positional [aggregate](./AA-4.3#S0106)s. (Were we to use parentheses, a one-element positional aggregate would look the same as and would always be interpreted as a parenthesized expression.) Unlike the case for arrays, where it is always possible to give bounds to work around this gap, such an [aggregate](./AA-4.3#S0106) could not be written at all for a sequence type (such as a list) where there is no index or key to use. 

{AI12-0212-1} container_element_association_list<a id="S0127"></a> ::= 
    [container_element_association](./AA-4.3#S0128) {, [container_element_association](./AA-4.3#S0128)}

{AI12-0212-1} container_element_association<a id="S0128"></a> ::= 
    [key_choice_list](./AA-4.3#S0129) =&gt [expression](./AA-4.4#S0132)
  | [key_choice_list](./AA-4.3#S0129) =&gt &lt&gt
  | [iterated_element_association](./AA-4.3#S0131)

{AI12-0212-1} key_choice_list<a id="S0129"></a> ::= [key_choice](./AA-4.3#S0130) {'|' [key_choice](./AA-4.3#S0130)}

{AI12-0212-1} key_choice<a id="S0130"></a> ::= key_[expression](./AA-4.4#S0132) | [discrete_range](./AA-3.6#S0058)

{AI12-0212-1} iterated_element_association<a id="S0131"></a> ::= 
    for [loop_parameter_specification](./AA-5.5#S0181)[ use key_[expression](./AA-4.4#S0132)] =&gt [expression](./AA-4.4#S0132)
  | for [iterator_specification](./AA-5.5#S0183)[ use key_[expression](./AA-4.4#S0132)] =&gt [expression](./AA-4.4#S0132)


#### Name Resolution Rules

{AI12-0212-1} {AI12-0437-1} The expected type for a [container_aggregate](./AA-4.3#S0123) shall be a single type for which the Aggregate aspect has been specified. The expected type for each [expression](./AA-4.4#S0132) of a [container_aggregate](./AA-4.3#S0123) is the element type of the expected type.

{AI12-0212-1} The expected type for a key_[expression](./AA-4.4#S0132), or a [discrete_range](./AA-3.6#S0058) of a [key_choice](./AA-4.3#S0130), is the key type of the expected type of the [aggregate](./AA-4.3#S0106).


#### Legality Rules

{AI12-0212-1} The expected type for a [positional_container_aggregate](./AA-4.3#S0125) shall have an Aggregate aspect that includes a specification for an Add_Unnamed procedure or an Assign_Indexed procedure. The expected type for a [named_container_aggregate](./AA-4.3#S0126) that contains one or more [iterated_element_association](./AA-4.3#S0131)s with a key_[expression](./AA-4.4#S0132) shall have an Aggregate aspect that includes a specification for the Add_Named procedure. The expected type for a [named_container_aggregate](./AA-4.3#S0126) that contains one or more [key_choice_list](./AA-4.3#S0129)s shall have an Aggregate aspect that includes a specification for the Add_Named or Assign_Indexed procedure. [A [null_container_aggregate](./AA-4.3#S0124) can be of any type with an Aggregate aspect.]

{AI12-0212-1} A non-null container aggregate is called an indexed aggregate if the expected type T of the aggregate specifies an Assign_Indexed procedure in its Aggregate aspect, and either there is no Add_Unnamed procedure specified for the type, or the aggregate is a [named_container_aggregate](./AA-4.3#S0126) with a [container_element_association](./AA-4.3#S0128) that contains a [key_choice_list](./AA-4.3#S0129) or a [loop_parameter_specification](./AA-5.5#S0181). The key type of an indexed aggregate is also called the index type of the aggregate.

{AI12-0212-1} A [container_element_association](./AA-4.3#S0128) with a &lt&gt rather than an [expression](./AA-4.4#S0132), or with a [key_choice](./AA-4.3#S0130) that is a [discrete_range](./AA-3.6#S0058), is permitted only in an indexed aggregate.

Reason: Only an element of an indexed aggregate can be left uninitialized, because the compiler can simply omit producing an Assign_Indexed operation for the given index. For other kinds of aggregates, there are no operations that add an element without giving it a value. We could have defined such (optional) operations, but they would have added significant complication to an already complex feature without adding much additional expressive power. Note that, to be consistent with array aggregates, we do not support specifying &lt&gt in an [iterated_element_association](./AA-4.3#S0131). 

{AI12-0212-1} For an [iterated_element_association](./AA-4.3#S0131) without a key_[expression](./AA-4.4#S0132), if the [aggregate](./AA-4.3#S0106) is an indexed aggregate or the expected type of the [aggregate](./AA-4.3#S0106) specifies an Add_Named procedure in its Aggregate aspect, then the type of the loop parameter of the [iterated_element_association](./AA-4.3#S0131) shall be the same as the key type of the [aggregate](./AA-4.3#S0106).

Ramification: If there is a key_[expression](./AA-4.4#S0132) in an [iterated_element_association](./AA-4.3#S0131), it determines the key of each added key/value pair, rather than the loop parameter. But if there is no key_[expression](./AA-4.4#S0132), the loop parameter itself is used as the key. 

{AI12-0212-1} {AI12-0250-1} For a [named_container_aggregate](./AA-4.3#S0126) that is an indexed aggregate, all [container_element_association](./AA-4.3#S0128)s shall contain either a [key_choice_list](./AA-4.3#S0129), or a [loop_parameter_specification](./AA-5.5#S0181) without a key_[expression](./AA-4.4#S0132) or [iterator_filter](./AA-5.5#S0182). Furthermore, for such an aggregate, either: 

all [key_choice](./AA-4.3#S0130)s shall be static expressions or static ranges, and every [loop_parameter_specification](./AA-5.5#S0181) shall have a [discrete_subtype_definition](./AA-3.6#S0055) that defines a non-null static range, and the set of values of the index type covered by the [key_choice](./AA-4.3#S0130)s and the [discrete_subtype_definition](./AA-3.6#S0055)s shall form a contiguous range of values with no duplications; or

there shall be exactly one [container_element_association](./AA-4.3#S0128), and if it has a [key_choice_list](./AA-4.3#S0129), the list shall have exactly one [key_choice](./AA-4.3#S0130). 

Reason: The above is trying to mimic the rules for [named_array_aggregate](./AA-4.3#S0116)s, without others. 


#### Dynamic Semantics

{AI12-0212-1} The evaluation of a [container_aggregate](./AA-4.3#S0123) starts by creating an anonymous object A of the expected type T, initialized as follows:

if the [aggregate](./AA-4.3#S0106) is an indexed aggregate, from the result of a call on the New_Indexed function; the actual parameters in this call represent the lower and upper bound of the [aggregate](./AA-4.3#S0106), and are determined as follows: 

if the [aggregate](./AA-4.3#S0106) is a [positional_container_aggregate](./AA-4.3#S0125), the lower bound is the low bound of the subtype of the key parameter of the Add_Indexed procedure, and the upper bound has a position number that is the sum of the position number of the lower bound and one less than the number of [expression](./AA-4.4#S0132)s in the [aggregate](./AA-4.3#S0106);

if the [aggregate](./AA-4.3#S0106) is a [named_container_aggregate](./AA-4.3#S0126), the lower bound is the lowest value covered by a [key_choice_list](./AA-4.3#S0129) or is the low bound of a range defined by a [discrete_subtype_definition](./AA-3.6#S0055) of a [loop_parameter_specification](./AA-5.5#S0181); the upper bound is the highest value covered by a [key_choice_list](./AA-4.3#S0129) or is the high bound of a range defined by a [discrete_subtype_definition](./AA-3.6#S0055) of a [loop_parameter_specification](./AA-5.5#S0181). 

if the [aggregate](./AA-4.3#S0106) is not an indexed aggregate, by assignment from the Empty constant, or from a call on the Empty function specified in the Aggregate aspect. In the case of an Empty function with a formal parameter, the actual parameter has the following value:

for a [null_container_aggregate](./AA-4.3#S0124), the value zero;

for a [positional_container_aggregate](./AA-4.3#S0125), the number of [expression](./AA-4.4#S0132)s;

for a [named_container_aggregate](./AA-4.3#S0126) without an [iterated_element_association](./AA-4.3#S0131), the number of key_[expression](./AA-4.4#S0132)s;

for a [named_container_aggregate](./AA-4.3#S0126) where every [iterated_element_association](./AA-4.3#S0131) contains a [loop_parameter_specification](./AA-5.5#S0181), the total number of elements specified by all of the [container_element_association](./AA-4.3#S0128)s;

otherwise, to an implementation-defined value.

Implementation Note: This value ought to be an estimate for the number of elements in the [aggregate](./AA-4.3#S0106), if one is available. If not, it is suggested to use the default value for the parameter if one exists, and zero otherwise. 

Implementation defined: The value of the parameter to Empty for some [container_aggregate](./AA-4.3#S0123)s.

{AI12-0212-1} The evaluation then proceeds as follows:

for a [null_container_aggregate](./AA-4.3#S0124), the anonymous object A is the result;

for a [positional_container_aggregate](./AA-4.3#S0125) of a type with a specified Add_Unnamed procedure, each [expression](./AA-4.4#S0132) is evaluated in an arbitrary order, and the Add_Unnamed procedure is invoked in sequence with the anonymous object A as the first parameter and the result of evaluating each [expression](./AA-4.4#S0132) as the second parameter, in the order of the [expression](./AA-4.4#S0132)s;

for a [positional_container_aggregate](./AA-4.3#S0125) that is an indexed aggregate, each [expression](./AA-4.4#S0132) is evaluated in an arbitrary order, and the Assign_Indexed procedure is invoked in sequence with the anonymous object A as the first parameter, the key value as the second parameter, computed by starting with the low bound of the subtype of the key formal parameter of the Assign_Indexed procedure and taking the successor of this value for each successive [expression](./AA-4.4#S0132), and the result of evaluating each [expression](./AA-4.4#S0132) as the third parameter;

for a [named_container_aggregate](./AA-4.3#S0126) for a type with an Add_Named procedure in its Aggregate aspect, the [container_element_association](./AA-4.3#S0128)s are evaluated in an arbitrary order: 

for a [container_element_association](./AA-4.3#S0128) with a [key_choice_list](./AA-4.3#S0129), for each [key_choice](./AA-4.3#S0130) of the list in an arbitrary order, the [key_choice](./AA-4.3#S0130) is evaluated as is the [expression](./AA-4.4#S0132) of the [container_element_association](./AA-4.3#S0128) (in an arbitrary order), and the Add_Named procedure is invoked once for each value covered by the [key_choice](./AA-4.3#S0130), with the anonymous object A as the first parameter, the value from the [key_choice](./AA-4.3#S0130) as the second parameter, and the result of evaluating the [expression](./AA-4.4#S0132) as the third parameter;

{AI12-0212-1} {AI12-0327-1} for a [container_element_association](./AA-4.3#S0128) with an [iterated_element_association](./AA-4.3#S0131), first the [iterated_element_association](./AA-4.3#S0131) is elaborated, then an iteration is performed, and for each value conditionally produced by the iteration (see 5.5 and 5.5.2) the Add_Named procedure is invoked with the anonymous object A as the first parameter, the result of evaluating the [expression](./AA-4.4#S0132) as the third parameter, and:

if there is a key_[expression](./AA-4.4#S0132), the result of evaluating the key_[expression](./AA-4.4#S0132) as the second parameter;

otherwise, with the loop parameter as the second parameter;

for a [named_container_aggregate](./AA-4.3#S0126) that is an indexed aggregate, the evaluation proceeds as above for the case of Add_Named, but with the Assign_Indexed procedure being invoked instead of Add_Named; in the case of a [container_element_association](./AA-4.3#S0128) with a &lt&gt rather than an [expression](./AA-4.4#S0132), the corresponding call on Assign_Indexed is not performed, leaving the component as it was upon return from the New_Indexed function;

for any other [named_container_aggregate](./AA-4.3#S0126), the [container_element_association](./AA-4.3#S0128)s (which are necessarily [iterated_element_association](./AA-4.3#S0131)s) are evaluated in the order given; each such evaluation comprises two steps: 

a)the [iterated_element_association](./AA-4.3#S0131) is elaborated;

b){AI12-0212-1} {AI12-0327-1} an iteration is performed, and for each value conditionally produced by the iteration (see 5.5 and 5.5.2) the Add_Unnamed procedure is invoked, with the anonymous object A as the first parameter and the result of evaluating the [expression](./AA-4.4#S0132) as the second parameter.

Ramification: In this case, the value of the loop parameter is not directly relevant, though presumably it appears within the [expression](./AA-4.4#S0132) of the [iterated_element_association](./AA-4.3#S0131). 


#### Examples

{AI12-0212-1} {AI12-0429-1} Examples of specifying the Aggregate aspect for a Set_Type, a Map_Type, and a Vector_Type:

```ada
   --  Set_Type is a set-like container type.
   type Set_Type is private
      with Aggregate =&gt (Empty       =&gt Empty_Set,
                         Add_Unnamed =&gt Include);
   function Empty_Set return Set_Type;

```

```ada
   subtype Small_Int is Integer range -1000..1000;

```

```ada
   procedure Include (S : in out Set_Type; N : in Small_Int);

```

```ada
   --  Map_Type is a map-like container type.
   type Map_Type is private
      with Aggregate =&gt  (Empty     =&gt Empty_Map,
                          Add_Named =&gt Add_To_Map);

```

```ada
   procedure Add_To_Map (M     : in out Map_Type;
                         Key   : in Integer;
                         Value : in String);

```

```ada
   Empty_Map : constant Map_Type;

```

```ada
   --  Vector_Type is an extensible array-like container type.
   type Vector_Type is private
      with Aggregate =&gt (Empty          =&gt Empty_Vector,
                         Add_Unnamed    =&gt Append_One,
                         New_Indexed    =&gt New_Vector,
                         Assign_Indexed =&gt Assign_Element);

```

```ada
   function Empty_Vector (Capacity : Integer := 0) return Vector_Type;

```

```ada
   procedure Append_One (V : in out Vector_Type; New_Item : in String);

```

```ada
   procedure Assign_Element (V     : in out Vector_Type;
                             Index : in Positive;
                             Item  : in String);

```

```ada
   function New_Vector (First, Last : Positive) return Vector_Type
      with Pre =&gt First = Positive'First;
      --  Vectors are always indexed starting at the
      --  lower bound of their index subtype.

```

```ada
-- Private part not shown.

```

{AI12-0212-1} {AI12-0429-1} Examples of container aggregates for Set_Type, Map_Type, and Vector_Type:

```ada
--  Example aggregates using Set_Type.
S : Set_Type;

```

```ada
--  Assign the empty set to S:
S := [];

```

```ada
--  Is equivalent to:
S := Empty_Set;

```

```ada
--  A positional set aggregate:
S := [1, 2];

```

```ada
--  Is equivalent to:
S := Empty_Set;
Include (S, 1);
Include (S, 2);

```

```ada
--  A set aggregate with an [iterated_element_association](./AA-4.3#S0131):
S := [for Item in 1 .. 5 =&gt Item * 2];

```

```ada
--  Is equivalent to:
S := Empty_Set;
for Item in 1 .. 5 loop
   Include (S, Item * 2);
end loop;

```

```ada
--  A set aggregate consisting of two [iterated_element_association](./AA-4.3#S0131)s:
S := [for Item in 1 .. 5 =&gt Item,
      for Item in 1 .. 5 =&gt -Item];

```

```ada
{AI12-0379-1} --  Is equivalent (assuming set semantics) to:
S := Empty_Set;
for Item in 1 .. 5 loop
   Include (S, Item);
end loop;
for Item in -5 .. -1 loop
   Include (S, Item);
end loop;

```

```ada
--  Example aggregates using Map_Type.
M : Map_Type;

```

```ada
--  A simple named map aggregate:
M := [12 =&gt "house", 14 =&gt "beige"];

```

```ada
--  Is equivalent to:
M := Empty_Map;
Add_To_Map (M, 12, "house");
Add_To_Map (M, 14, "beige");

```

```ada
--  Define a table of pairs:
type Pair is record
   Key : Integer;
   Value : access constant String;
end record;

```

```ada
Table : constant array(Positive range &lt&gt) of Pair :=
   [(Key =&gt 33, Value =&gt new String'("a nice string")),
    (Key =&gt 44, Value =&gt new String'("an even better string"))];

```

```ada
--  A map aggregate using an [iterated_element_association](./AA-4.3#S0131)
--  and a key_[expression](./AA-4.4#S0132), built from from a table of key/value pairs:
M := [for P of Table use P.Key =&gt P.Value.all];

```

```ada
--  Is equivalent to:
M := Empty_Map;
for P of Table loop
   Add_To_Map (M, P.Key, P.Value.all);
end loop;

```

```ada
--  Create an image table for an array of integers:
Keys : constant array(Positive range &lt&gt) of Integer := [2, 3, 5, 7, 11];

```

```ada
--  A map aggregate where the values produced by the
--  [iterated_element_association](./AA-4.3#S0131) are of the same type as the key
--  (hence a separate key_[expression](./AA-4.4#S0132) is unnecessary):
M := [for Key of Keys =&gt Integer'Image (Key)];

```

```ada
--  Is equivalent to:
M := Empty_Map;
for Key of Keys loop
   Add_To_Map (M, Key, Integer'Image (Key));
end loop;

```

```ada
--  Example aggregates using Vector_Type.
V : Vector_Type;

```

```ada
--  A positional vector aggregate:
V := ["abc", "def"];

```

```ada
--  Is equivalent to:
V := Empty_Vector (2);
Append_One (V, "abc");
Append_One (V, "def");

```

```ada
--  An indexed vector aggregate:
V := [1 =&gt "this", 2 =&gt "is", 3 =&gt "a", 4 =&gt "test"];

```

```ada
--  Is equivalent to:
V := New_Vector (1, 4);
Assign_Element (V, 1, "this");
Assign_Element (V, 2, "is");
Assign_Element (V, 3, "a");
Assign_Element (V, 4, "test");

```


#### Extensions to Ada 2012

{AI12-0212-1} Container aggregates are new. 

