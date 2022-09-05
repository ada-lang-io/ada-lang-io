---
sidebar_position:  60
---

# 7.3  Private Types and Private Extensions

[The declaration (in the visible part of a package) of a type as a private type or private extension serves to separate the characteristics that can be used directly by outside program units (that is, the logical properties) from other characteristics whose direct use is confined to the package (the details of the definition of the type itself). See 3.9.1 for an overview of type extensions. ]


#### Language Design Principles

A private (untagged) type can be thought of as a record type with the type of its single (hidden) component being the full view.

A private tagged type can be thought of as a private extension of an anonymous parent with no components. The only dispatching operation of the parent is equality (although the Size attribute, and, if nonlimited, assignment are allowed, and those will presumably be implemented in terms of dispatching). 


#### Syntax

{AI05-0183-1} private_type_declaration<a id="S0232"></a> ::= 
   type [defining_identifier](./AA-3.1#S0022) [[discriminant_part](./AA-3.7#S0059)] is [[abstract] tagged] [limited] private
      [[aspect_specification](./AA-13.1#S0346)];

{AI95-00251-01} {AI95-00419-01} {AI95-00443-01} {AI05-0183-1} private_extension_declaration<a id="S0233"></a> ::= 
   type [defining_identifier](./AA-3.1#S0022) [[discriminant_part](./AA-3.7#S0059)] is
     [abstract] [limited | synchronized] new ancestor_[subtype_indication](./AA-3.2#S0027)
     [and [interface_list](./AA-3.9#S0078)] with private
       [[aspect_specification](./AA-13.1#S0346)];


#### Legality Rules

A [private_type_declaration](./AA-7.3#S0232) or [private_extension_declaration](./AA-7.3#S0233) declares a partial view of the type; such a declaration is allowed only as a [declarative_item](./AA-3.11#S0087) of the visible part of a package, and it requires a completion, which shall be a [full_type_declaration](./AA-3.2#S0024) that occurs as a [declarative_item](./AA-3.11#S0087) of the private part of the package. [ The view of the type declared by the [full_type_declaration](./AA-3.2#S0024) is called the full view.] A generic formal private type or a generic formal private extension is also a partial view. 

To be honest: A private type can also be imported (using aspect Import, see B.1), in which case no completion is allowed, if supported by an implementation. 

Reason: We originally used the term "private view", but this was easily confused with the view provided from the private part, namely the full view. 

Proof: {AI95-00326-01} Full view is now defined in 3.2.1, "Type Declarations", as all types now have them. 

[A type shall be completely defined before it is frozen (see 3.11.1 and 13.14). Thus, neither the declaration of a variable of a partial view of a type, nor the creation by an [allocator](./AA-4.8#S0164) of an object of the partial view are allowed before the full declaration of the type. Similarly, before the full declaration, the name of the partial view cannot be used in a [generic_instantiation](./AA-12.3#S0315) or in a representation item.] 

Proof: This rule is stated officially in 3.11.1, "Completions of Declarations". 

{AI95-00419-01} {AI95-00443-01} [A private type is limited if its declaration includes the reserved word limited; a private extension is limited if its ancestor type is a limited type that is not an interface type, or if the reserved word limited or synchronized appears in its definition.] If the partial view is nonlimited, then the full view shall be nonlimited. If a tagged partial view is limited, then the full view shall be limited. [On the other hand, if an untagged partial view is limited, the full view may be limited or nonlimited.]

If the partial view is tagged, then the full view shall be tagged. [On the other hand, if the partial view is untagged, then the full view may be tagged or untagged.] In the case where the partial view is untagged and the full view is tagged, no derivatives of the partial view are allowed within the immediate scope of the partial view; [derivatives of the full view are allowed.] 

Ramification: Note that deriving from a partial view within its immediate scope can only occur in a package that is a child of the one where the partial view is declared. The rule implies that in the visible part of a public child package, it is impossible to derive from an untagged private type declared in the visible part of the parent package in the case where the full view of the parent type turns out to be tagged. We considered a model in which the derived type was implicitly redeclared at the earliest place within its immediate scope where characteristics needed to be added. However, we rejected that model, because (1) it would imply that (for an untagged type) subprograms explicitly declared after the derived type could be inherited, and (2) to make this model work for composite types as well, several implicit redeclarations would be needed, since new characteristics can become visible one by one; that seemed like too much mechanism. 

Discussion: The rule for tagged partial views is redundant for partial views that are private extensions, since all extensions of a given ancestor tagged type are tagged, and limited if the ancestor is limited. We phrase this rule partially redundantly to keep its structure parallel with the other rules. 

To be honest: This rule is checked in a generic unit, rather than using the "assume the best" or "assume the worst" method. 

Reason: {AI95-00230-01} Tagged limited private types have certain capabilities that are incompatible with having assignment for the full view of the type. In particular, tagged limited private types can be extended with components of a limited type, which works only because assignment is not allowed. Consider the following example: 

```ada
package P1 is
    type T1 is tagged limited private;
    procedure Foo(X : in T1'Class);
private
    type T1 is tagged null record; -- Illegal!
        -- This should say "tagged limited null record".
end P1;

```

```ada
package body P1 is
    type A is access T1'Class;
    Global : A;
    procedure Foo(X : in T1'Class) is
    begin
        Global := new T1'Class'(X);
            -- This would be illegal if the full view of
            -- T1 were limited, like it's supposed to be.
    end Foo;
end P1;

```

```ada
{AI95-00230-01} with P1;
package P2 is
    type T2(D : access Integer)
            is new P1.T1 with
        record
            My_Task : Some_Task_Type; -- Trouble!
        end record;
end P2;

```

```ada
with P1;
with P2;
procedure Main is
    Local : aliased Integer;
    Y : P2.T2(D =&gt Local'Access);
begin
    P1.Foo(Y);
end Main;
  

```

{AI95-00230-01} If the above example were legal, we would have succeeded in doing an assignment of a task object, which is supposed to be a no-no. 

This rule is not needed for private extensions, because they inherit their limitedness from their ancestor, and there is a separate rule forbidding limited components of the corresponding record extension if the parent is nonlimited. 

Ramification: A type derived from an untagged private type is untagged, even if the full view of the parent is tagged, and even at places that can see the parent: 

```ada
package P is
    type Parent is private;
private
    type Parent is tagged
        record
            X: Integer;
        end record;
end P;

```

```ada
with P;
package Q is
    type T is new P.Parent;
end Q;

```

```ada
with Q; use Q;
package body P is
    ... T'Class ... -- Illegal!
    Object: T;
    ... Object.X ... -- Illegal!
    ... Parent(Object).X ... -- OK.
end P;

```

The declaration of T declares an untagged view. This view is always untagged, so T'Class is illegal, it would be illegal to extend T, and so forth. The component name X is never visible for this view, although the component is still there - one can get one's hands on it via a [type_conversion](./AA-4.6#S0162).

{AI95-00396-01} If a full type has a partial view that is tagged, then: 

the partial view shall be a synchronized tagged type (see 3.9.4) if and only if the full type is a synchronized tagged type;

Reason: Since we do not allow record extensions of synchronized tagged types, this property has to be visible in the partial view to avoid privacy breaking. Generic formals do not need a similar rule as any extensions are rechecked for legality in the specification, and extensions of tagged formals are always illegal in a generic body. 

the partial view shall be a descendant of an interface type (see 3.9.4) if and only if the full type is a descendant of the interface type.

Reason: Consider the following example: 

```ada
package P is
   package Pkg is
      type Ifc is interface;
      procedure Foo (X : Ifc) is abstract;
   end Pkg;

```

```ada
   type Parent_1 is tagged null record;

```

```ada
   type T1 is new Parent_1 with private;
private
   type Parent_2 is new Parent_1 and Pkg.Ifc with null record;
   procedure Foo (X : Parent_2); -- Foo #1

```

```ada
   type T1 is new Parent_2 with null record; -- Illegal.
end P;

```

```ada
with P;
package P_Client is
   type T2 is new P.T1 and P.Pkg.Ifc with null record;
   procedure Foo (X : T2); -- Foo #2
   X : T2;
end P_Client;

```

```ada
with P_Client;
package body P is
   ...

```

```ada
{AI12-0005-1}    procedure Bar (X : T1'Class) is
   begin
      Pkg.Foo (Pkg.Ifc'Class (X)); -- should call Foo #1 or an override thereof
   end;

```

```ada
begin
   Pkg.Foo (Pkg.Ifc'Class (P_Client.X));      -- should call Foo #2
   Bar (T1'Class (P_Client.X));
end P;

```

This example is illegal because the completion of T1 is descended from an interface that the partial view is not descended from. If it were legal, T2 would implement Ifc twice, once in the visible part of P, and once in the visible part of P_Client. We would need to decide how Foo #1 and Foo #2 relate to each other. There are two options: either Foo #2 overrides Foo #1, or it doesn't.

If Foo #2 overrides Foo #1, we have a problem because the client redefines a behavior that it doesn't know about, and we try to avoid this at all costs, as it would lead to a breakdown of whatever abstraction was implemented. If the abstraction didn't expose that it implements Ifc, there must be a reason, and it should be able to depend on the fact that no overriding takes place in clients. Also, during maintenance, things may change and the full view might implement a different set of interfaces. Furthermore, the situation is even worse if the full type implements another interface Ifc2 that happens to have a conforming Foo (otherwise unrelated, except for its name and profile).

If Foo #2 doesn't override Foo #1, there is some similarity with the case of normal tagged private types, where a client can declare an operation that happens to conform to some private operation, and that's OK, it gets a different slot in the type descriptor. The problem here is that T2 would implement Ifc in two different ways, and through conversions to Ifc'Class we could end up with visibility on both of these two different implementations. This is the "diamond inheritance" problem of C++ all over again, and we would need some kind of a preference rule to pick one implementation. We don't want to go there (if we did, we might as well provide full-fledged multiple inheritance).

Note that there wouldn't be any difficulty to implement the first option, so the restriction is essentially methodological. The second option might be harder to implement, depending on the language rules that we would choose. 

Ramification: {AI05-0005-1} This rule also prevents completing a private type with an interface. An interface, like all types, is a descendant of itself, and thus this rule is triggered. One reason this is necessary is that a client of a private extension should be able to inherit limitedness without having to look in the private part to see if the type is an interface (remember that limitedness of interfaces is never inherited, while it is inherited from other types). 

The ancestor subtype of a [private_extension_declaration](./AA-7.3#S0233) is the subtype defined by the ancestor_[subtype_indication](./AA-3.2#S0027); the ancestor type shall be a specific tagged type. The full view of a private extension shall be derived (directly or indirectly) from the ancestor type. In addition to the places where Legality Rules normally apply (see 12.3), the requirement that the ancestor be specific applies also in the private part of an instance of a generic unit. 

Reason: This rule allows the full view to be defined through several intermediate derivations, possibly from a series of types produced by [generic_instantiation](./AA-12.3#S0315)s. 

{AI95-00419-01} {AI95-00443-01} If the reserved word limited appears in a [private_extension_declaration](./AA-7.3#S0233), the ancestor type shall be a limited type. If the reserved word synchronized appears in a [private_extension_declaration](./AA-7.3#S0233), the ancestor type shall be a limited interface.

If the declaration of a partial view includes a [known_discriminant_part](./AA-3.7#S0061), then the [full_type_declaration](./AA-3.2#S0024) shall have a fully conforming [(explicit)] [known_discriminant_part](./AA-3.7#S0061) [(see 6.3.1, "Conformance Rules")]. [The ancestor subtype may be unconstrained; the parent subtype of the full view is required to be constrained (see 3.7).] 

Discussion: If the ancestor subtype has discriminants, then it is usually best to make it unconstrained. 

Ramification: If the partial view has a [known_discriminant_part](./AA-3.7#S0061), then the full view has to be a composite, non-array type, since only such types may have known discriminants. Also, the full view cannot inherit the discriminants in this case; the [known_discriminant_part](./AA-3.7#S0061) has to be explicit.

That is, the following is illegal: 

```ada
package P is
    type T(D : Integer) is private;
private
    type T is new Some_Other_Type; -- Illegal!
end P;
  

```

even if Some_Other_Type has an integer discriminant called D.

It is a ramification of this and other rules that in order for a tagged type to privately inherit unconstrained discriminants, the private type declaration has to have an [unknown_discriminant_part](./AA-3.7#S0060). 

If a private extension inherits known discriminants from the ancestor subtype, then the full view shall also inherit its discriminants from the ancestor subtype, and the parent subtype of the full view shall be constrained if and only if the ancestor subtype is constrained. 

Reason: The first part ensures that the full view has the same discriminants as the partial view. The second part ensures that if the partial view is unconstrained, then the full view is also unconstrained; otherwise, a client might constrain the partial view in a way that conflicts with the constraint on the full view. 

{AI95-00419-01} {AI05-0004-1} If the [full_type_declaration](./AA-3.2#S0024) for a private extension includes a [derived_type_definition](./AA-3.4#S0035), then the reserved word limited shall appear in the [full_type_declaration](./AA-3.2#S0024) if and only if it also appears in the [private_extension_declaration](./AA-7.3#S0233). 

Reason: {AI05-0004-1} The word limited is optional (unless the ancestor is an interface), but it should be used consistently. Otherwise things would be too confusing for the reader. Of course, we only require that if the full type includes a [derived_type_definition](./AA-3.4#S0035), as we want to allow task and protected types to complete extensions of synchronized interfaces. 

[If a partial view has unknown discriminants, then the [full_type_declaration](./AA-3.2#S0024) may define a definite or an indefinite subtype, with or without discriminants.]

If a partial view has neither known nor unknown discriminants, then the [full_type_declaration](./AA-3.2#S0024) shall define a definite subtype.

If the ancestor subtype of a private extension has constrained discriminants, then the parent subtype of the full view shall impose a statically matching constraint on those discriminants. 

Ramification: If the parent type of the full view is not the ancestor type, but is rather some descendant thereof, the constraint on the discriminants of the parent type might come from the declaration of some intermediate type in the derivation chain between the ancestor type and the parent type. 

Reason: This prevents the following: 

```ada
package P is
    type T2 is new T1(Discrim =&gt 3) with private;
private
    type T2 is new T1(Discrim =&gt 999) -- Illegal!
        with record ...;
end P;

```

The constraints in this example do not statically match.

If the constraint on the parent subtype of the full view depends on discriminants of the full view, then the ancestor subtype has to be unconstrained: 

```ada
type One_Discrim(A: Integer) is tagged ...;
...
package P is
    type Two_Discrims(B: Boolean; C: Integer) is new One_Discrim with private;
private
    type Two_Discrims(B: Boolean; C: Integer) is new One_Discrim(A =&gt C) with
        record
            ...
        end record;
end P;

```

The above example would be illegal if the private extension said "is new One_Discrim(A =&gt C);", because then the constraints would not statically match. (Constraints that depend on discriminants are not static.)


#### Static Semantics

A [private_type_declaration](./AA-7.3#S0232) declares a private type and its first subtype. Similarly, a [private_extension_declaration](./AA-7.3#S0233) declares a private extension and its first subtype. 

Discussion: A package-private type is one declared by a [private_type_declaration](./AA-7.3#S0232); that is, a private type other than a generic formal private type. Similarly, a package-private extension is one declared by a [private_extension_declaration](./AA-7.3#S0233). These terms are not used in the RM95 version of this document. 

{AI05-0269-1} A declaration of a partial view and the corresponding [full_type_declaration](./AA-3.2#S0024) define two views of a single type. The declaration of a partial view together with the visible part define the operations that are available to outside program units; the declaration of the full view together with the private part define other operations whose direct use is possible only within the declarative region of the package itself. Moreover, within the scope of the declaration of the full view, the characteristics (see 3.4) of the type are determined by the full view; in particular, within its scope, the full view determines the classes that include the type, which components, entries, and protected subprograms are visible, what attributes and other predefined operations are allowed, and whether the first subtype is static. See 7.3.1.

{AI95-00401-01} {AI05-0110-1} For a private extension, the characteristics (including components, but excluding discriminants if there is a new [discriminant_part](./AA-3.7#S0059) specified), predefined operators, and inherited user-defined primitive subprograms are determined by its ancestor type and its progenitor types (if any), in the same way that those of a record extension are determined by those of its parent type and its progenitor types (see 3.4 and 7.3.1). 

To be honest: {AI05-0110-1} If an operation of the ancestor or parent type is abstract, then the abstractness of the inherited operation is different for nonabstract record extensions than for nonabstract private extensions (see 3.9.3). 


#### Dynamic Semantics

The elaboration of a [private_type_declaration](./AA-7.3#S0232) creates a partial view of a type. The elaboration of a [private_extension_declaration](./AA-7.3#S0233) elaborates the ancestor_[subtype_indication](./AA-3.2#S0027), and creates a partial view of a type. 

NOTE 1   {AI12-0442-1} The partial view of a type as declared by a [private_type_declaration](./AA-7.3#S0232) is defined to be a composite view (in 3.2). The full view of the type can be elementary or composite. A private extension is also composite, as is its full view.

NOTE 2   {AI95-00318-02} Declaring a private type with an [unknown_discriminant_part](./AA-3.7#S0060) is a way of preventing clients from creating uninitialized objects of the type; they are then forced to initialize each object by calling some operation declared in the visible part of the package. 

Discussion: Packages with private types are analogous to generic packages with formal private types, as follows: The declaration of a package-private type is like the declaration of a formal private type. The visible part of the package is like the generic formal part; these both specify a contract (that is, a set of operations and other things available for the private type). The private part of the package is like an instantiation of the generic; they both give a [full_type_declaration](./AA-3.2#S0024) that specifies implementation details of the private type. The clients of the package are like the body of the generic; usage of the private type in these places is restricted to the operations defined by the contract.

In other words, being inside the package is like being outside the generic, and being outside the package is like being inside the generic; a generic is like an "inside-out" package.

This analogy also works for private extensions in the same inside-out way.

Many of the legality rules are defined with this analogy in mind. See, for example, the rules relating to operations of [formal] derived types.

The completion rules for a private type are intentionally quite similar to the matching rules for a generic formal private type.

This analogy breaks down in one respect: a generic actual subtype is a subtype, whereas the full view for a private type is always a new type. (We considered allowing the completion of a [private_type_declaration](./AA-7.3#S0232) to be a [subtype_declaration](./AA-3.2#S0026), but the semantics just won't work.) This difference is behind the fact that a generic actual type can be class-wide, whereas the completion of a private type always declares a specific type. 

NOTE 3   {AI95-00401} {AI12-0442-1} The ancestor type specified in a [private_extension_declaration](./AA-7.3#S0233) and the parent type specified in the corresponding declaration of a record extension given in the private part can be different. If the ancestor type is not an interface type, the parent type of the full view can be any descendant of the ancestor type. In this case, for a primitive subprogram that is inherited from the ancestor type and not overridden, the formal parameter names and default expressions (if any) come from the corresponding primitive subprogram of the specified ancestor type, while the body comes from the corresponding primitive subprogram of the parent type of the full view. See 3.9.2.

NOTE 4   {AI95-00401} {AI12-0442-1} If the ancestor type specified in a [private_extension_declaration](./AA-7.3#S0233) is an interface type, the parent type can be any type so long as the full view is a descendant of the ancestor type. The progenitor types specified in a [private_extension_declaration](./AA-7.3#S0233) and the progenitor types specified in the corresponding declaration of a record extension given in the private part are not necessarily the same - it is only necessary that the private extension and the record extension be descended from the same set of interfaces. 


#### Examples

Examples of private type declarations: 

```ada
type Key is private;
type File_Name is limited private;

```

Example of a private extension declaration: 

```ada
type List is new Ada.Finalization.Controlled with private;

```


#### Extensions to Ada 83

The syntax for a [private_type_declaration](./AA-7.3#S0232) is augmented to allow the reserved word tagged.

In Ada 83, a private type without discriminants cannot be completed with a type with discriminants. Ada 95 allows the full view to have discriminants, so long as they have defaults (that is, so long as the first subtype is definite). This change is made for uniformity with generics, and because the rule as stated is simpler and easier to remember than the Ada 83 rule. In the original version of Ada 83, the same restriction applied to generic formal private types. However, the restriction was removed by the ARG for generics. In order to maintain the "generic contract/private type contract analogy" discussed above, we have to apply the same rule to package-private types. Note that a private untagged type without discriminants can be completed with a tagged type with discriminants only if the full view is constrained, because discriminants of tagged types cannot have defaults. 


#### Wording Changes from Ada 83

RM83-7.4.1(4), "Within the specification of the package that declares a private type and before the end of the corresponding full type declaration, a restriction applies....", is subsumed (and corrected) by the rule that a type shall be completely defined before it is frozen, and the rule that the parent type of a derived type declaration shall be completely defined, unless the derived type is a private extension. 


#### Extensions to Ada 95

{AI95-00251-01} {AI95-00396-01} {AI95-00401-01} Added [interface_list](./AA-3.9#S0078) to private extensions to support interfaces and multiple inheritance (see 3.9.4).

{AI95-00419-01} A private extension may specify that it is a limited type. This is required for interface ancestors (from which limitedness is not inherited), but it is generally useful as documentation of limitedness.

{AI95-00443-01} A private extension may specify that it is a synchronized type. This is required in order so that a regular limited interface can be used as the ancestor of a synchronized type (we do not allow hiding of synchronization). 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [private_type_declaration](./AA-7.3#S0232) and a [private_extension_declaration](./AA-7.3#S0233). This is described in 13.1.1. 


#### Wording Changes from Ada 2005

{AI05-0110-1} Correction: The description of how a private extension inherits characteristics was made consistent with the way formal derived types inherit characteristics (see 12.5.1). 


## 7.3.1  Private Operations

[For a type declared in the visible part of a package or generic package, certain operations on the type do not become visible until later in the package - either in the private part or the body. Such private operations are available only inside the declarative region of the package or generic package.] 


#### Static Semantics

The predefined operators that exist for a given type are determined by the classes to which the type belongs. For example, an integer type has a predefined "+" operator. In most cases, the predefined operators of a type are declared immediately after the definition of the type; the exceptions are explained below. Inherited subprograms are also implicitly declared immediately after the definition of the type, except as stated below.

{8652/0019} {AI95-00033-01} {AI05-0029-1} For a composite type, the characteristics (see 7.3) of the type are determined in part by the characteristics of its component types. At the place where the composite type is declared, the only characteristics of component types used are those characteristics visible at that place. If later immediately within the declarative region in which the composite type is declared additional characteristics become visible for a component type, then any corresponding characteristics become visible for the composite type. Any additional predefined operators are implicitly declared at that place. If there is no such place, then additional predefined operators are not declared at all, but they still exist.

Reason: {AI05-0029-1} We say that the predefined operators exist because they can emerge in some unusual generic instantiations. See 12.5. 

Discussion: {AI05-0029-1} The predefined operators for the underlying class of a type always exist, even if there is no visibility on that underlying class. This rule is simply about where (if ever) those operators are declared (and thus become usable). The "additional predefined operators" defined by this rule are any that are not declared at the point of the original type declaration. For instance, a type derived from a private type whose full type is type String always will have a "&gt" operator, but where that operator is declared (and thus whether it is visible) will depend on the visibility of the full type of the parent type. 

{8652/0019} {AI95-00033-01} The corresponding rule applies to a type defined by a [derived_type_definition](./AA-3.4#S0035), if there is a place immediately within the declarative region in which the type is declared where additional characteristics of its parent type become visible.

{8652/0019} {AI95-00033-01} [For example, an array type whose component type is limited private becomes nonlimited if the full view of the component type is nonlimited and visible at some later place immediately within the declarative region in which the array type is declared. In such a case, the predefined "=" operator is implicitly declared at that place, and assignment is allowed after that place.]

{AI12-0140-1} The characteristics and constraints of the designated subtype of an access type follow a somewhat different rule. The view of the designated subtype of (a view of) an access type at a given place is determined by the view of the designated subtype that is visible at that place, rather than the view at the place where the access type is declared.

Ramification: {AI12-0140-1} Whether or not the designated subtype is considered incomplete is determined by rules in 3.10.1; this rule has no effect on that property. 

{AI05-0115-1} {AI05-0269-1} {AI12-0140-1} A type is a descendant of the full view of some ancestor of its parent type only if the current view it has of its parent is a descendant of the full view of that ancestor. More generally, at any given place, a type is descended from the same view of an ancestor as that from which the current view of its parent is descended. This view determines what characteristics are inherited from the ancestor[, and, for example, whether the type is considered to be a descendant of a record type, or a descendant only through record extensions of a more distant ancestor].

{AI05-0115-1} {AI12-0065-1} {AI12-0140-1} [Furthermore, it is possible for there to be places where a derived type is known to be derived indirectly from an ancestor type, but is not a descendant of even a partial view of the ancestor type, because the parent of the derived type is not visibly a descendant of the ancestor.  In this case, the derived type inherits no characteristics from that ancestor, but nevertheless is within the derivation class of the ancestor for the purposes of type conversion, the "covers" relationship, and matching against a formal derived type. In this case the derived type is effectively a descendant of an incomplete view of the ancestor.]

Discussion: Here is an example of this situation: 

```ada
package P is
   type T is private;
   C : constant T;
private
   type T is new Integer;
   C : constant T := 42;
end P;

```

```ada
{AI12-0065-1} with P;
package Q is
    type T2 is new P.T;  -- T2 is not a descendant of Integer
end Q;

```

```ada
{AI12-0065-1} with Q;
package P.Child is
    type T3 is new Q.T2;
private
    -- Here T3 is known to be indirectly derived from Integer, but inherits
    -- no characteristics from Integer, since T2 inherits no characteristics
    -- from Integer.
    -- However, we allow an explicit conversion of T3 to/from Integer.
    -- Hence, T3 is effectively a descendant of an "incomplete" view of Integer.
    Int : Integer := 52;
    V : T3 := T3(P.C);  -- Legal: conversion allowed
    W : T3 := T3(Int);  -- Legal: conversion allowed
    X : T3 := T3(42);   -- Error: T3 is not a numeric type
    Y : T3 := X + 1;    -- Error: no visible "+" operator
    Z : T3 := T3(Integer(W) + 1);   -- Legal: convert to Integer first
end P.Child;

```

{8652/0019} {AI95-00033-01} {AI05-0029-1} Inherited primitive subprograms follow a different rule. For a [derived_type_definition](./AA-3.4#S0035), each inherited primitive subprogram is implicitly declared at the earliest place, if any, immediately within the declarative region in which the [type_declaration](./AA-3.2#S0023) occurs, but after the [type_declaration](./AA-3.2#S0023), where the corresponding declaration from the parent is visible. If there is no such place, then the inherited subprogram is not declared at all, but it still exists. [For a tagged type, it is possible to dispatch to an inherited subprogram that is not declared at all.]

{AI12-0439-1} For a [private_extension_declaration](./AA-7.3#S0233), each inherited subprogram is declared immediately after the [private_extension_declaration](./AA-7.3#S0233) if the corresponding declaration from the ancestor is visible at that place. Otherwise, the inherited subprogram is not declared for the private extension, [though it can be for the full type]. 

Reason: There is no need for the "earliest place immediately within the declarative region" business here, because a [private_extension_declaration](./AA-7.3#S0233) will be completed with a [full_type_declaration](./AA-3.2#S0024), so we can hang the necessary private implicit declarations on the [full_type_declaration](./AA-3.2#S0024). 

Discussion: The above rules matter only when the component type (or parent type) is declared in the visible part of a package, and the composite type (or derived type) is declared within the declarative region of that package (possibly in a nested package or a child package).

Consider: 

```ada
package Parent is
    type Root is tagged null record;
    procedure Op1(X : Root);

```

```ada
    type My_Int is range 1..10;
private
    procedure Op2(X : Root);

```

```ada
    type Another_Int is new My_Int;
    procedure Int_Op(X : My_Int);
end Parent;

```

```ada
with Parent; use Parent;
package Unrelated is
    type T2 is new Root with null record;
    procedure Op2(X : T2);
end Unrelated;

```

```ada
package Parent.Child is
    type T3 is new Root with null record;
    -- Op1(T3) implicitly declared here.

```

```ada
    package Nested is
        type T4 is new Root with null record;
    private
        ...
    end Nested;
private
    -- Op2(T3) implicitly declared here.
    ...
end Parent.Child;

```

```ada
with Unrelated; use Unrelated;
package body Parent.Child is
    package body Nested is
        -- Op2(T4) implicitly declared here.
    end Nested;

```

```ada
    type T5 is new T2 with null record;
end Parent.Child;

```

Another_Int does not inherit Int_Op, because Int_Op does not "exist" at the place where Another_Int is declared.

Type T2 inherits Op1 and Op2 from Root. However, the inherited Op2 is never declared, because Parent.Op2 is never visible immediately within the declarative region of T2. T2 explicitly declares its own Op2, but this is unrelated to the inherited one - it does not override the inherited one, and occupies a different slot in the type descriptor.

T3 inherits both Op1 and Op2. Op1 is implicitly declared immediately after the type declaration, whereas Op2 is declared at the beginning of the private part. Note that if Child were a private child of Parent, then Op1 and Op2 would both be implicitly declared immediately after the type declaration.

T4 is similar to T3, except that the earliest place immediately within the declarative region containing T4 where Root's Op2 is visible is in the body of Nested.

If T3 or T4 were to declare a type-conformant Op2, this would override the one inherited from Root. This is different from the situation with T2.

T5 inherits Op1 and two Op2's from T2. Op1 is implicitly declared immediately after the declaration of T5, as is the Op2 that came from Unrelated.Op2. However, the Op2 that originally came from Parent.Op2 is never implicitly declared for T5, since T2's version of that Op2 is never visible (anywhere - it never got declared either).

For all of these rules, implicit private parts and bodies are assumed as needed.

It is possible for characteristics of a type to be revealed in more than one place:

```ada
package P is
    type Comp1 is private;
private
    type Comp1 is new Boolean;
end P;

```

```ada
package P.Q is
    package R is
        type Comp2 is limited private;
        type A is array(Integer range &lt&gt) of Comp2;
    private
        type Comp2 is new Comp1;
        -- A becomes nonlimited here.
        -- "="(A, A) return Boolean is implicitly declared here.
        ...
    end R;
private
    -- Now we find out what Comp1 really is, which reveals
    -- more information about Comp2, but we're not within
    -- the immediate scope of Comp2, so we don't do anything
    -- about it yet.
end P.Q;

```

```ada
package body P.Q is
    package body R is
        -- Things like "xor"(A,A) return A are implicitly
        -- declared here.
    end R;
end P.Q;

```

{8652/0019} {AI95-00033-01} We say immediately within the declarative region in order that types do not gain operations within a nested scope. Consider: 

```ada
package Outer is
    package Inner is
        type Inner_Type is private;
    private
        type Inner_Type is new Boolean;
    end Inner;
    type Outer_Type is array(Natural range &lt&gt) of Inner.Inner_Type;
end Outer;

```

```ada
package body Outer is
    package body Inner is
        -- At this point, we can see that Inner_Type is a Boolean type.
        -- But we don't want Outer_Type to gain an "and" operator here.
    end Inner;
end Outer;

```

[The Class attribute is defined for tagged subtypes in 3.9. In addition,] for every subtype S of an untagged private type whose full view is tagged, the following attribute is defined: 

S'ClassDenotes the class-wide subtype corresponding to the full view of S. This attribute is allowed only from the beginning of the private part in which the full view is declared, until the declaration of the full view. [After the full view, the Class attribute of the full view can be used.] 

NOTE 1   Because a partial view and a full view are two different views of one and the same type, outside of the defining package the characteristics of the type are those defined by the visible part. Within these outside program units the type is just a private type or private extension, and any language rule that applies only to another class of types does not apply. The fact that the full declaration can implement a private type with a type of a particular class (for example, as an array type) is relevant only within the declarative region of the package itself including any child units.

The consequences of this actual implementation are, however, valid everywhere. For example: any default initialization of components takes place; the attribute Size provides the size of the full view; finalization is still done for controlled components of the full view; task dependence rules still apply to components that are task objects.

NOTE 2   {AI95-00287-01} Partial views provide initialization, membership tests, selected components for the selection of discriminants and inherited components, qualification, and explicit conversion. Nonlimited partial views also allow use of [assignment_statement](./AA-5.2#S0173)s.

NOTE 3   For a subtype S of a partial view, S'Size is defined (see 13.3). For an object A of a partial view, the attributes A'Size and A'Address are defined (see 13.3). The Position, First_Bit, and Last_Bit attributes are also defined for discriminants and inherited components. 


#### Examples

Example of a type with private operations: 

```ada
package Key_Manager is
   type Key is private;
   Null_Key : constant Key; -- a deferred constant declaration (see 7.4)
   procedure Get_Key(K : out Key);
   function "&lt" (X, Y : Key) return Boolean;
private
   type Key is new Natural;
   Null_Key : constant Key := Key'First;
end Key_Manager;

```

```ada
package body Key_Manager is
   Last_Key : Key := Null_Key;
   procedure Get_Key(K : out Key) is
   begin
      Last_Key := Last_Key + 1;
      K := Last_Key;
   end Get_Key;

```

```ada
   function "&lt" (X, Y : Key) return Boolean is
   begin
      return Natural(X) &lt Natural(Y);
   end "&lt";
end Key_Manager;

```

NOTE 4   Notes on the example: Outside of the package Key_Manager, the operations available for objects of type Key include assignment, the comparison for equality or inequality, the procedure Get_Key and the operator "&lt"; they do not include other relational operators such as "&gt=", or arithmetic operators.

The explicitly declared operator "&lt" hides the predefined operator "&lt" implicitly declared by the [full_type_declaration](./AA-3.2#S0024). Within the body of the function, an explicit conversion of X and Y to the subtype Natural is necessary to invoke the "&lt" operator of the parent type. Alternatively, the result of the function can be written as not (X &gt= Y), since the operator "&gt=" is not redefined.

The value of the variable Last_Key, declared in the package body, remains unchanged between calls of the procedure Get_Key. (See also the NOTES of 7.2.) 


#### Wording Changes from Ada 83

The phrase in RM83-7.4.2(7), "...after the full type declaration", doesn't work in the presence of child units, so we define that rule in terms of visibility.

The definition of the Constrained attribute for private types has been moved to "Obsolescent Features". (The Constrained attribute of an object has not been moved there.) 


#### Wording Changes from Ada 95

{8652/0018} {AI95-00033-01} Corrigendum: Clarified when additional operations are declared.

{AI95-00287-01} Revised the note on operations of partial views to reflect that limited types do have an assignment operation, but not [assignment_statement](./AA-5.2#S0173)s. 


#### Wording Changes from Ada 2005

{AI05-0029-1} Correction: Revised the wording to say that predefined operations still exist even if they are never declared, because it is possible to reference them in a generic unit.

{AI05-0115-1} Correction: Clarified that the characteristics of a descendant of a private type depend on the visibility of the full view of the direct ancestor. This has to be the case (so that privacy is not violated), but it wasn't spelled out in earlier versions of Ada. 


#### Wording Changes from Ada 2012

{AI12-0065-1} Corrigendum: Clarified the clarification added by AI05-0115-1, as it turned out to not be that clear. Hopefully this version is better.

{AI12-0140-1} Correction: Clarified the constraints and properties that apply to a designated subtype. This additional wording does not mean to change existing practice. 


## 7.3.2  Type Invariants

{AI05-0146-1} {AI12-0041-1} {AI12-0396-1} For a private type, private extension, or interface, the following language-defined assertion aspects may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1):

{AI05-0146-1} {AI05-0250-1} Type_InvariantThis aspect shall be specified by an [expression](./AA-4.4#S0132), called an invariant expression. Type_Invariant may be specified on a [private_type_declaration](./AA-7.3#S0232), on a [private_extension_declaration](./AA-7.3#S0233), or on a [full_type_declaration](./AA-3.2#S0024) that declares the completion of a private type or private extension.

Aspect Description for Type_Invariant: A condition that will hold true for all objects of a type.

{AI05-0146-1} {AI12-0041-1} {AI12-0150-1} {AI12-0419-1} Type_Invariant'ClassThis aspect shall be specified by an [expression](./AA-4.4#S0132), called an invariant expression. Type_Invariant'Class may be specified on a [private_type_declaration](./AA-7.3#S0232), a [private_extension_declaration](./AA-7.3#S0233), or a [full_type_declaration](./AA-3.2#S0024) for an interface type. Type_Invariant'Class determines a class-wide type invariant for a tagged type. [The Type_Invariant'Class aspect is not inherited, but its effects are additive, as defined below.]

Reason: {AI05-0254-1} A class-wide type invariant cannot be hidden in the private part, as the creator of an extension needs to know about it in order to conform to it in any new or overriding operations. On the other hand, a specific type invariant is not inherited, so that no operation outside of the original package needs to conform to it; thus there is no need for it to be visible. 

Aspect Description for Type_Invariant'Class: A condition that will hold true for all objects in a class of types.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[type invariant], Def=[see invariant] Version=[5],Kind=(AddedNormal),Group=[T],Term=[invariant], Def=[an assertion that is expected to be True for all objects of a given private type when viewed from outside the defining package] 


#### Name Resolution Rules

{AI05-0146-1} The expected type for an invariant expression is any boolean type.

{AI05-0146-1} {AI12-0150-1} {AI12-0159-1} {AI12-0199-1} [Within an invariant expression, the identifier of the first subtype of the associated type denotes the current instance of the type.] Within an invariant expression for the Type_Invariant aspect of a type T, the type of this current instance is T. Within an invariant expression for the Type_Invariant'Class aspect of a type T, the type of this current instance is interpreted as though it had a (notional) nonabstract type NT that is a visible formal derived type whose ancestor type is T.[ The effect of this interpretation is that the only operations that can be applied to this current instance are those defined for such a formal derived type.]

Proof: The first sentence is given formally in 13.1.1. 

Reason: {AI12-0159-1} The rules for Type_Invariant'Class ensure that the invariant expression is well-defined for any type descended from T. 


#### Legality Rules

{AI05-0146-1} [The Type_Invariant'Class aspect shall not be specified for an untagged type.] The Type_Invariant aspect shall not be specified for an abstract type.

Proof: The first sentence is given formally in 13.1.1. 

{AI12-0042-1} {AI12-0382-1} If a type extension occurs immediately within the visible part of a package specification, at a point where a private operation of some ancestor is visible and inherited, and a Type_Invariant'Class expression applies to that ancestor, then the inherited operation shall be abstract or shall be overridden. 


#### Static Semantics

{AI05-0250-1} [If the Type_Invariant aspect is specified for a type T, then the invariant expression applies to T.]

{AI05-0146-1} {AI12-0199-1} If the Type_Invariant'Class aspect is specified for a tagged type T, then a corresponding expression also applies to each nonabstract descendant T1 of T [(including T itself if it is nonabstract)]. The corresponding expression is constructed from the associated expression as follows:

{AI12-0199-1} References to nondiscriminant components of T (or to T itself) are replaced with references to the corresponding components of T1 (or to T1 as a whole).

Ramification: {AI12-0199-1} The only nondiscriminant components visible at the point of such an aspect specification are necessarily inherited from some nonprivate ancestor. 

{AI12-0199-1} References to discriminants of T are replaced with references to the corresponding discriminant of T1, or to the specified value for the discriminant, if the discriminant is specified by the [derived_type_definition](./AA-3.4#S0035) for some type that is an ancestor of T1 and a descendant of T (see 3.7). 

This paragraph was deleted.

Discussion: The associated expression from which the corresponding expression is constructed is the one that applies to the descendant type; "applies" is formally defined in 13.1.1. 

{AI12-0075-1} {AI12-0191-1} For a nonabstract type T, a callable entity is said to be a boundary entity for T if it is declared within the immediate scope of T (or by an instance of a generic unit, and the generic is declared within the immediate scope of type T), or is the Read or Input stream-oriented attribute of type T, and either:

T is a private type or a private extension and the callable entity is visible outside the immediate scope of type T or overrides an inherited operation that is visible outside the immediate scope of T; or

T is a record extension, and the callable entity is a primitive operation visible outside the immediate scope of type T or overrides an inherited operation that is visible outside the immediate scope of T.

Reason: {AI12-0191-1} A boundary entity for type T is one that might require an invariant check for T. It includes subprograms that don't (visibly) involve T; since we don't want to break privacy, we can't statically know if some private type has some part of T. We'll reduce the set when we describe the actual checks. 


#### Dynamic Semantics

{AI05-0146-1} {AI05-0247-1} {AI05-0290-1} {AI12-0150-1} If one or more invariant expressions apply to a nonabstract type T, then an invariant check is performed at the following places, on the specified object(s):

{AI12-0133-1} After successful initialization of an object of type T by default (see 3.3.1), the check is performed on the new object unless the partial view of T has unknown discriminants;

Reason: {AI12-0133-1} The check applies everywhere, even in the package body, because default initialization has to work the same for clients as it does within the package. As such, checks within the package are either harmless or will uncover a bug that could also happen to a client. However, if the partial view of the type has unknown discriminants, no client of the package can declare a default-initialized object. Therefore, no invariant check is needed, as all default initialized objects are necessarily inside the package. 

{AI12-0049-1} {AI12-0191-1} After successful explicit initialization of the completion of a deferred constant whose nominal type has a part of type T, if the completion is inside the immediate scope of the full view of T, and the deferred constant is visible outside the immediate scope of T, the check is performed on the part(s) of type T;

After successful conversion to type T, the check is performed on the result of the conversion;

{AI05-0146-1} {AI05-0269-1} For a view conversion, outside the immediate scope of T, that converts from a descendant of T (including T itself) to an ancestor of type T (other than T itself), a check is performed on the part of the object that is of type T:

after assigning to the view conversion; and

after successful return from a call that passes the view conversion as an in out or out parameter. 

Ramification: For a single view conversion that converts between distantly related types, this rule could be triggered for multiple types and thus multiple invariant checks may be needed. 

Implementation Note: {AI05-0299-1} For calls to inherited subprograms (including dispatching calls), the implied view conversions mean that a wrapper is probably needed. (See the Note at the bottom of this subclause for more on the model of checks for inherited subprograms.)

For view conversions involving class-wide types, the exact checks needed may not be known at compile-time. One way to deal with this is to have an implicit dispatching operation that is given the object to check and the tag of the target of the conversion, and which first checks if the passed tag is not for itself, and if not, checks the its invariant on the object and then calls the operation of its parent type. If the tag is for itself, the operation is complete. 

{AI12-0146-1} {AI12-0075-1} {AI12-0191-1} {AI12-0193-1} Upon successful return from a call on any callable entity which is a boundary entity for T, an invariant check is performed on each object which is subject to an invariant check for T. In the case of a call to a protected operation, the check is performed before the end of the protected action. In the case of a call to a task entry, the check is performed before the end of the rendezvous. The following objects of a callable entity are subject to an invariant check for T:

Paragraph 16 was merged above. 

{AI05-0146-1} {AI05-0269-1} {AI12-0042-1} {AI12-0075-1} {AI12-0191-1} a result with a nominal type that has a part of type T;

{AI12-0042-1} {AI12-0191-1} an out or in out parameter whose nominal type has a part of type T;

{AI12-0075-1} {AI12-0191-1} an access-to-object parameter or result whose designated nominal type has a part of type T; or

{AI05-0289-1} {AI12-0042-1} {AI12-0044-1} {AI12-0075-1} {AI12-0191-1} for a procedure or entry, an in parameter whose nominal type has a part of type T.

Ramification: {AI12-0167-1} {AI12-0191-1} This is a Dynamic Semantics rule, so we ignore privacy when determining if a check is needed. We do, however, use the nominal type of objects to determine if a part of type T is present; therefore, parts that aren't known at compile-time (after ignoring privacy) are never subject to an invariant check. This is preferable, as we don't want overhead associated with the possibility that there might exist an extension of a tagged type that has a part of type T. (See the "leaks" note below for avoidance advice.) 

Discussion: We don't check in parameters for functions to avoid infinite recursion for calls to public functions appearing in invariant expressions. Such function calls are unavoidable for class-wide invariants and likely for other invariants. This is the simplest rule that avoids trouble, and functions are much more likely to be queries that don't modify their parameters than other callable entities. 

{AI05-0146-1} {AI05-0269-1} {AI12-0075-1} {AI12-0338-1} If the nominal type of a formal parameter (or the designated nominal type of an access-to-object parameter or result) is incomplete at the point of the declaration of the callable entity, and if the completion of that incomplete type does not occur in the same declaration list as the incomplete declaration, then  for purposes of the preceding rules the nominal type is considered to have no parts of type T.

{AI12-0042-1} For a view conversion to a class-wide type occurring within the immediate scope of T, from a specific type that is a descendant of T (including T itself), a check is performed on the part of the object that is of type T.

Reason: Class-wide objects are treated as though they exist outside the scope of every type, and may be passed across package "boundaries" freely without further invariant checks.

{AI12-0167-1} Despite this model, if an object of type T that is a component of a class-wide object is modified within the scope of the full view of type T, then there is no invariant check for T at that point. 

{AI05-0290-1} {AI12-0080-1} {AI12-0159-1} If performing checks is required by the Type_Invariant or Type_Invariant'Class assertion policies (see 11.4.2) in effect at the point of the corresponding aspect specification applicable to a given type, then the respective invariant expression is considered enabled.

Ramification: If a class-wide invariant expression is enabled for a type, it remains enabled when inherited by descendants of that type, even if the policy in effect is Ignore for the inheriting type. 

{AI05-0146-1} {AI05-0250-1} {AI05-0289-1} {AI05-0290-1} The invariant check consists of the evaluation of each enabled invariant expression that applies to T, on each of the objects specified above. If any of these evaluate to False, Assertions.Assertion_Error is raised at the point of the object initialization, conversion, or call. If a given call requires more than one evaluation of an invariant expression, either for multiple objects of a single type or for multiple types with invariants, the evaluations are performed in an arbitrary order, and if one of them evaluates to False, it is not specified whether the others are evaluated. Any invariant check is performed prior to copying back any by-copy in out or out parameters. Invariant checks, any postcondition check, and any constraint or predicate checks associated with in out or out parameters are performed in an arbitrary order.

{AI12-0150-1} {AI12-0159-1} For an invariant check on a value of type T1 based on a class-wide invariant expression inherited from an ancestor type T, any operations within the invariant expression that were resolved as primitive operations of the (notional) formal derived type NT are bound to the corresponding operations of type T1 in the evaluation of the invariant expression for the check on T1.

{AI05-0146-1} {AI05-0247-1} {AI05-0250-1} The invariant checks performed on a call are determined by the subprogram or entry actually invoked, whether directly, as part of a dispatching call, or as part of a call through an access-to-subprogram value.

Ramification: {AI12-0149-1} {AI12-0167-1} {AI12-0210-1} Type invariant checks are intended to prevent invariant-violating values from inadvertently "leaking out"; that is, code which cannot see the completion of the private type should not be able to reference invariant-violating values of the type (assuming the type invariant condition itself is well behaved - for example, no uses of global variables during evaluation of the invariant expression). This goal is not completely achieved; such leaking is possible but, importantly, it requires assistance (deliberate or not) of some form from the package that declares the invariant-bearing private type (or a child unit thereof); a client of a well-crafted package cannot use these holes to obtain an invariant-violating value without help.

{AI12-0210-1} The list of known techniques whereby this kind of leak can occur (ignoring things like erroneous execution and the various forms of unchecked type conversion) consists of:

A boundary entity might assign an invariant-violating value to a global variable that is visible to client code.

{AI12-0149-1} Invariant checks on subprogram return are not performed on objects that are accessible only through access-valued components of other objects. This can only cause a leak if there is a type with access-valued components that is used as a parameter or result type of a boundary entity. For a type T that has a type invariant, avoiding the declaration of types with access-valued components designating objects with parts of T in the package that contains T is sufficient to prevent this leak.

A client could call through an access-to-subprogram value and reach a subprogram body that has visibility on the full declaration of a type; no invariant checks will be performed if the designated subprogram is not itself a boundary subprogram. This leak can only happen if an access-to-subprogram value of a subprogram that is not visible to clients is passed out to clients.

{AI12-0167-1} {AI12-0191-1} Invariant checks are only performed for parts of the nominal type for tagged parameters and function results. This means that components of extensions are not checked (these would be very expensive to check as any tagged type might have such an extension in the future, even though that would be very unlikely). For this leak to occur for a type T that has a type invariant, the body of a boundary entity of T needs to have visibility on a type extension that has components of T or access-to-T and also has an ancestor type (or class) as a parameter or result of the subprogram.

{AI12-0338-1} Invariant checks are not performed for parts of incomplete types when the completion is not available. For this leak to occur for a type T that has a type invariant and is declared in a package P, one has to use a limited with on a package that has P in its semantic closure, and then use a type from that package as a parameter or result of a boundary subprogram for T (or as the designated type of a parameter or result of such a subprogram).

{AI12-0210-1} Consider a package P which declares an invariant-bearing private type T and a generic package P.G1, which in turn declares another generic package P.G1.G2. Outside of package P, there are declarations of an instantiation I1 of P.G1 and an instantiation I2 of I1.G2. I2 can declare visible subprograms whose bodies see the full view of T and yet these subprograms are not boundary subprograms (because the generic I1.G2 is not declared within the immediate scope of T - G1.G2 is, but that's irrelevant). So a call to one of these subprograms from outside of P could yield an invariant-violating value. So long as a nested generic of a nested generic unit of P is not declared, no such leaks are possible.

{AI12-0210-1} All of these leaks require cooperation of some form (as detailed above) from within the immediate scope of the invariant-bearing type.

Implementation Note: The implementation might want to produce a warning if a private extension has an ancestor type that is a visible extension, and an invariant expression depends on the value of one of the components from a visible extension part. 

NOTE   {AI05-0250-1} {AI05-0269-1} For a call of a primitive subprogram of type NT that is inherited from type T, the specified checks of the specific invariants of both the types NT and T are performed. For a call of a primitive subprogram of type NT that is overridden for type NT, the specified checks of the specific invariants of only type NT are performed.

Proof: This follows from the definition of a call on an inherited subprogram as view conversions of the parameters of the type and a call to the original subprogram (see 3.4), along with the normal invariant checking rules. In particular, the call to the original subprogram takes care of any checks needed on type T, and the checks required on view conversions take care of any checks needed on type NT, specifically on in out and out parameters. We require this in order that the semantics of an explicitly defined wrapper that does nothing but call the original subprogram is the same as that of an inherited subprogram. 


#### Examples

{AI12-0312-1} {AI12-0429-1} Example of a work scheduler where only urgent work can be scheduled for weekends:

```ada
package Work_Orders is

```

```ada
   -- See 3.5.1 for type declarations of Level, Day, and Weekday

```

```ada
   type Work_Order is private with
     Type_Invariant =&gt Day_Scheduled (Work_Order) in Weekday
                       or else Priority (Work_Order) = Urgent;

```

```ada
   function Schedule_Work (Urgency  : in Level;
                           To_Occur : in Day) return Work_Order
     with Pre =&gt Urgency = Urgent or else To_Occur in Weekday;

```

```ada
   function Day_Scheduled (Order : in Work_Order) return Day;

```

```ada
   function Priority (Order : in Work_Order) return Level;

```

```ada
   procedure Change_Priority (Order        : in out Work_Order;
                              New_Priority : in     Level;
                              Changed      : out    Boolean)
      with Post =&gt Changed = (Day_Scheduled(Order) in Weekday
                              or else Priority(Order) = Urgent);

```

```ada
private

```

```ada
   type Work_Order is record
      Scheduled : Day;
      Urgency   : Level;
   end record;

```

```ada
end Work_Orders;

```

```ada
package body Work_Orders is

```

```ada
   function Schedule_Work (Urgency  : in Level;
                           To_Occur : in Day) return Work_Order is
     (Scheduled =&gt To_Occur, Urgency =&gt Urgency);

```

```ada
   function Day_Scheduled (Order : in Work_Order) return Day is
     (Order.Scheduled);

```

```ada
   function Priority (Order : in Work_Order) return Level is
     (Order.Urgency);

```

```ada
   procedure Change_Priority (Order        : in out Work_Order;
                              New_Priority : in     Level;
                              Changed      : out    Boolean) is
   begin
      -- Ensure type invariant is not violated
      if Order.Urgency = Urgent or else (Order.Scheduled in Weekday) then
         Changed := True;
         Order.Urgency := New_Priority;
      else
         Changed := False;
      end if;
   end Change_Priority;

```

```ada
end Work_Orders;

```


#### Extensions to Ada 2005

{AI05-0146-1} {AI05-0247-1} {AI05-0250-1} {AI05-0289-1} Type_Invariant aspects are new. 


#### Inconsistencies With Ada 2012

{AI12-0042-1} Corrigendum: Clarified the definition of when invariant checks occur for inherited subprograms. This might cause checks to be added or removed in some cases. These are all rare cases involving class-wide type invariants and either record extensions or multiple levels of derivation. Additionally, implementations probably make the checks as the intent seems clear, even though the formal language did not include them. So we do not expect this to be a problem in practice.

{AI12-0042-1} Corrigendum: Added invariant checks for conversions to class-wide types. This might cause an invariant check to fail in some cases where they would not be made in the original definition of Ada 2012. Such cases represent a hole where a value that fails an invariant could "leak out" of a package, and as such will detect far more bugs than it causes.

{AI12-0044-1} Corrigendum: Removed the invariant check for in parameters of functions, so that typical invariants don't cause infinite recursion. This is strictly inconsistent, as the Ada 2012 definition has this check; therefore, programs could depend on Assertion_Error being raised upon the return from some call on a public function. However, as the intent of assertion checking is to uncover bugs, a program that depends on a bug occurring seems very unlikely.

{AI12-0049-1} {AI12-0149-1} Corrigendum: Added an invariant check for deferred constants and for access values returned from functions, so they cannot be used to "leak" values that violate the invariant from a package. This is strictly inconsistent, as the Ada 2012 definition is missing these checks; therefore, programs could depend on using values that violate an invariant outside of the package of definition. These will not raise Assertion_Error in Ada 2012 as defined in the Ada 2012 Reference Manual, but ought to do so (as noted by this change). As these are a violation of the intent of invariants, we think that this change will mainly reveal bugs rather than cause them.

{AI12-0150-1} {AI12-0159-1} Corrigendum: Eliminated unintentional redispatching from class-wide type invariants. This means that a different body might be evaluated for a type invariant check where the value has a different tag than that of the type. The change means that the behavior of Type_Invariant and Type_Invariant'Class will be the same for a particular subprogram, and that the known behavior of the operations can be assumed. We expect that this change will primarily fix bugs, as it will make class-wide type invariants work more like expected. In the case where redispatching is desired, an explicit conversion to a class-wide type can be used.

{AI12-0199-1} Correction: Class-wide type invariants are no longer checked for abstract types. Thus, a program that previously raised Assertion_Error because of a call to a concrete subprogram of an abstract type will no longer do so. However, programs that depend on assertion failure are likely to be very rare, some explicit conversion to the abstract type is needed to get static binding, and additionally many such checks would call abstract functions (likely causing some compiler failure). As such, this incompatibility most likely will never be seen in practice. 


#### Incompatibilities With Ada 2012

{AI12-0042-1} {AI12-0382-1} Corrigendum: A private operation that is inherited in the visible part of a package to which a class-wide invariant applies now requires overriding. This is a very unlikely situation, and will prevent problems with invariant checks being added to routines that assume that they don't have them. Note: The original wording was missing the restriction to the visible part of the package, this was added later for Ada 2022. 


#### Extensions to Ada 2012

{AI12-0041-1} Corrigendum: Class-wide type invariants can now be specified on interfaces as well as private types. 


#### Wording Changes from Ada 2012

{AI12-0133-1} Corrigendum: Clarified that all objects that are initialized by default should have an invariant check, and added an exception for types with unknown discriminants, as in that case the client cannot declare a default-initialized object. This exception to the check is formally inconsistent, but since it is only removing an assertion failure that occurs where no assertion should be checked anyway (meaning it's more likely to fix a bug than cause one), and programs depending on assertion failure should be very rare outside of test cases, we don't document this as inconsistent.

{AI12-0075-1} {AI12-0191-1} Defined the term "boundary entity" to separate the static rules from the dynamic rules, and allow rules in other subclauses to reference these rules. No semantic change is intended.

{AI12-0191-1} Clarified that invariant checks only apply to parts of the nominal type of objects.

{AI12-0193-1} Correction: Clarified when type invariant checks happen for protected actions and entry calls.

{AI12-0338-1} Correction: Clarified that type invariant checks do not occur for parts of incomplete types unless the completion is available. 


## 7.3.3  Default Initial Conditions

{AI12-0265-1} {AI12-0272-1} {AI12-0396-1} For a private type or private extension (including a generic formal type), the following language-defined assertion aspect may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1):

{AI12-0265-1} {AI12-0419-1} Default_Initial_ConditionThis aspect shall be specified by an [expression](./AA-4.4#S0132), called a default initial condition expression. Default_Initial_Condition may be specified on a [private_type_declaration](./AA-7.3#S0232), a [private_extension_declaration](./AA-7.3#S0233), a [formal_private_type_definition](./AA-12.5#S0324), or a [formal_derived_type_definition](./AA-12.5#S0325). [The Default_Initial_Condition aspect is not inherited, but its effects are additive, as defined below.]

Aspect Description for Default_Initial_Condition: A condition that will hold true after the default initialization of an object.

Glossary entry: A default initial condition is a property that holds for every default-initialized object of a given type.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[default initial condition], Def=[a property that holds for every default-initialized object of a given type] 


#### Name Resolution Rules

{AI12-0265-1} The expected type for a default initial condition expression is any boolean type.

{AI12-0397-1} Within a default initial condition expression associated with a declaration for a type T, a name that denotes the declaration is interpreted as a current instance of a notional (nonabstract) formal derived type NT with ancestor T, that has directly visible primitive operations.

Reason: This is analogous to the rule for Post'Class (see 6.1.1) and ensures that the expression is well-defined for any descendant of type T. 

Ramification: The operations of NT are also nonabstract, so the rule against a call of an abstract subprogram does not trigger for a default initial condition for an abstract type. Note that, presuming T is tagged, it is possible to call class-wide operations of the type T given an object of type NT. Similarly it is possible to explicitly convert an object of type NT to a subtype of T, and pass it to a nonprimitive operation expecting a parameter of type T. [Note that one cannot directly convert to (the first subtype of) T since it represents the current instance of the type within the aspect expression, but one can convert to a subtype of T (including a subtype that matches the first subtype).] 


#### Legality Rules

{AI12-0265-1} The Default_Initial_Condition aspect shall not be specified for a type whose partial view has unknown discriminants[, whether explicitly declared or inherited].


#### Static Semantics

{AI12-0265-1} If the Default_Initial_Condition aspect is specified for a type T, then the default initial condition expression applies to T and to all descendants of T.


#### Dynamic Semantics

{AI12-0265-1} {AI12-0397-1} If one or more default initial condition expressions apply to a [(nonabstract)] type T, then a default initial condition check is performed after successful initialization of an object of type T by default (see 3.3.1). In the case of a controlled type, the check is performed after the call to the type's Initialize procedure (see 7.6).

Proof: {AI12-0397-1} If T is an abstract type, then there will never be an initialization of an object of the type. 

{AI12-0265-1} If performing checks is required by the Default_Initial_Condition assertion policy (see 11.4.2) in effect at the point of the corresponding [aspect_specification](./AA-13.1#S0346) applicable to a given type, then the respective default initial condition expression is considered enabled.

{AI12-0265-1} {AI12-0397-1} The default initial condition check consists of the evaluation of each enabled default initial condition expression that applies to T. Any operations within such an expression that were resolved as primitive operations of the (notional) formal derived type NT, are in the evaluation of the expression resolved as for a formal derived type in an instance with T as the actual type for NT (see 12.5.1). These evaluations, if there are more than one, are performed in an arbitrary order. If any of these evaluate to False, Assertions.Assertion_Error is raised at the point of the object initialization.

Ramification: {AI12-0397-1} Just as is true for a formal derived type (see 12.5.1), for a tagged type T, the controlling tag of a call on a primitive of NT will cause the body of the corresponding primitive of T to be executed. For an untagged type T, invoking a primitive of NT will cause the body of the operation of the type where the aspect originated to be executed, with conversions performed as for an inherited subprogram. 

{AI12-0272-1} [For a generic formal type T, default initial condition checks performed are as determined by the actual type, along with any default initial condition of the formal type itself.]

Proof: This follows from the general dynamic semantics rules given above, but we mention it explicitly so that there can be no doubt that it is intended. 


#### Implementation Permissions

{AI12-0332-1} Implementations may extend the syntax or semantics of the Default_Initial_Condition aspect in an implementation-defined manner.

Implementation defined: Any extensions of the Default_Initial_Condition aspect.

Reason: This is intended to allow preexisting usages from SPARK 2014 to remain acceptable in conforming implementations, as well as to provide future flexibility. Note the word "extend" in this permission; we expect that any aspect usage that conforms with the (other) rules of this clause will be accepted by any Ada implementation, regardless of any implementation-defined extensions. 

NOTE 1   {AI12-0312-1} For an example of the use of this aspect, see the Vector container definition in A.18.2. 


#### Extensions to Ada 2012

{AI12-0265-1} {AI12-0272-1} {AI12-0397-1} Aspect Default_Initial_Condition is new. 


## 7.3.4  Stable Properties of a Type

{AI12-0187-1} {AI12-0324-1} Certain characteristics of an object of a given type are unchanged by most of the primitive operations of the type. Such characteristics are called stable properties of the type.

Glossary entry: A stable property is a characteristic associated with objects of a given type that is preserved by many of the primitive operations of the type.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[stable property], Def=[a characteristic associated with objects of a given type that is preserved by many of the primitive operations of the type] 


#### Static Semantics

{AI12-0187-1} A property function of type T is a function with a single parameter of type T or of a class-wide type that covers T.

Reason: This provides restrictions on name resolution so overloaded functions can be used as a stable property function. 

{AI12-0285-1} {AI12-0324-1} {AI12-0388-1} A type property aspect definition is a list of [name](./AA-4.1#S0091)s written in the syntax of a [positional_array_aggregate](./AA-4.3#S0114). A subprogram property aspect definition is a list of [name](./AA-4.1#S0091)s, each optionally preceded by reserved word not, also written in the syntax of a [positional_array_aggregate](./AA-4.3#S0114).

To be honest: A single [name](./AA-4.1#S0091) would technically be a parenthesized [expression](./AA-4.4#S0132) rather than an [aggregate](./AA-4.3#S0106); we mean to include that here. We say "syntax of a [positional_array_aggregate](./AA-4.3#S0114)" to hopefully clarify that the specification is in no other way an actual [aggregate](./AA-4.3#S0106). 

{AI12-0187-1} {AI12-0272-1} For a nonformal private type, nonformal private extension, or full type that does not have a partial view, the following language-defined aspects may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1):

{AI12-0187-1} {AI12-0285-1} {AI12-0405-1} Stable_PropertiesThis aspect shall be specified by a type property aspect definition; each [name](./AA-4.1#S0091) shall statically denote a single property function of the type. This aspect defines the specific stable property functions of the associated type.

Discussion: We do not allow this aspect on generic formal types, as it is only meaningful for primitive subprograms and generic formal types have no such subprograms. 

Ramification: Class-wide aspects are only allowed on tagged types (see 13.1.1), so we don't say that here. 

Aspect Description for Stable_Properties: A list of functions describing characteristics that usually are unchanged by primitive operations of the type or an individual primitive subprogram.

{AI12-0187-1} {AI12-0285-1} Stable_Properties'ClassThis aspect shall be specified by a type property aspect definition; each [name](./AA-4.1#S0091) shall statically denote a single property function of the type. This aspect defines the class-wide stable property functions of the associated type. [Unlike most class-wide aspects, Stable_Properties'Class is not inherited by descendant types and subprograms, but the enhanced class-wide postconditions are inherited in the normal manner.]

Proof: Class-wide inheritance has to be explicitly defined. Here we are not making such a definition, so there is no inheritance. 6.1.1 defines the inheritance of class-wide postconditions. 

Discussion: Since class-wide postconditions are inherited by descendants, we don't need the stable property functions to be inherited; if they were inherited, we'd be duplicating the checks, which we don't want. 

Ramification: Class-wide aspects are only allowed on primitive subprograms of tagged types (see 13.1.1), so we don't say that here. 

Aspect Description for Stable_Properties'Class: A list of functions describing characteristics that usually are unchanged by primitive operations of a class of types or a primitive subprogram for such a class.

{AI12-0405-1} The specific and class-wide stable properties of a type together comprise the stable properties of the type. 

{AI12-0187-1} For a primitive subprogram, the following language-defined aspects may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1):

{AI12-0187-1} {AI12-0285-1} Stable_PropertiesThis aspect shall be specified by a subprogram property aspect definition; each [name](./AA-4.1#S0091) shall statically denote a single property function of a type for which the associated subprogram is primitive.

{AI12-0187-1} {AI12-0285-1} Stable_Properties'ClassThis aspect shall be specified by a subprogram property aspect definition; each [name](./AA-4.1#S0091) shall statically denote a single property function of a tagged type for which the associated subprogram is primitive. [Unlike most class-wide aspects, Stable_Properties'Class is not inherited by descendant subprograms, but the enhanced class-wide postconditions are inherited in the normal manner.]

Reason: The subprogram versions of Stable_Properties are provided to allow overriding the stable properties of a type for an individual primitive subprogram. While they can be used even if the type has no stable properties, that is not an intended use (as simply modifying the postcondition directly makes more sense for something that only happens in one place). 


#### Legality Rules

{AI12-0187-1} A stable property function of a type T shall have a nonlimited return type and shall be:

a primitive function with a single parameter of mode in of type T; or

a function that is declared immediately within the declarative region in which an ancestor type of T is declared and has a single parameter of mode in of a class-wide type that covers T. 

{AI12-0187-1} {AI12-0285-1} In a subprogram property aspect definition for a subprogram S:

all or none of the items shall be preceded by not;

Ramification: Mixing not functions with regular functions is not allowed. 

any property functions mentioned after not shall be a stable property function of a type for which S is primitive. 

{AI12-0405-1} If a [subprogram_renaming_declaration](./AA-8.5#S0242) declares a primitive subprogram of a type T, then the renamed callable entity shall also be a primitive subprogram of type T and the two primitive subprograms shall have the same specific stable property functions and the same class-wide stable property functions (see below).


#### Static Semantics

{AI12-0187-1} For a primitive subprogram S of a type T, the specific stable property functions of S for type T are:

if S has an aspect Stable_Properties specified that does not include not, those functions denoted in the aspect Stable_Properties for S that have a parameter of T or T'Class;

if S has an aspect Stable_Properties specified that includes not, those functions denoted in the aspect Stable_Properties for T, excluding those denoted in the aspect Stable_Properties for S;

if S does not have an aspect Stable_Properties, those functions denoted in the aspect Stable_Properties for T, if any.

Discussion: A primitive subprogram can be primitive for more than one type, and thus there can be more than one such set of specific stable properties for a subprogram. Thus we say "specific stable property functions for subprogram S for type T". 

{AI12-0187-1} A similar definition applies for class-wide stable property functions by replacing aspect Stable_Properties with aspect Stable_Properties'Class in the above definition.

{AI12-0187-1} The explicit specific postcondition expression for a subprogram S is the [expression](./AA-4.4#S0132) directly specified for S with the Post aspect. Similarly, the explicit class-wide postcondition expression for a subprogram S is the [expression](./AA-4.4#S0132) directly specified for S with the Post'Class aspect.

{AI12-0187-1} For a primitive subprogram S of a type T that has a parameter P of type T, the parameter is excluded from stable property checks if:

S is a stable property function of T;

Reason: This prevents possible infinite recursion, where the postcondition calls the function itself (directly or indirectly). 

P has mode out;

Reason: A parameter of mode out doesn't necessarily have a defined input value, so there is no old value to compare with. Ideally, the postcondition will include expressions defining the values of the stable properties after the call, but we do not try to ensure this. 

the Global aspect of S is null, and P has mode in and the mode is not overridden by a global aspect.

Reason: An in parameter of a Global =&gt null subprogram cannot be modified, even if it has indirect parts, without violating the Global aspect. Thus, there is no need to assert that the properties don't change. 

{AI12-0187-1} {AI12-0324-1} {AI12-0405-1} For every primitive subprogram S of a type T that is not an abstract subprogram or null procedure, the specific postcondition expression of S is modified to include expressions of the form F(P) = F(P)'Old, all anded with each other and any explicit specific postcondition expression, with one such equality included for each specific stable property function F of S for type T that does not occur in the explicit specific postcondition expression of S, and P is each parameter of S that has type T and is not excluded from stable property checks. The resulting specific postcondition expression of S is used in place of the explicit specific postcondition expression of S [when interpreting the meaning of the postcondition as defined in 6.1.1].

Ramification: There is one F(P) = F(P)'Old subexpression for every combination of a specific stable property function of type T and a parameter of type T. For instance, if there are three specific stable property functions for type T and two parameters of type T, then there are six such subexpressions appended to the postcondition.

The resulting specific postcondition is evaluated as described in 6.1.1. One hopes that compilers can be smart enough to prove that many of these added postcondition subexpressions cannot fail, but that is not required here. 

Reason: Null procedures and abstract subprograms are excluded as they do not allow specific postconditions. Moreover, for null procedures, static analysis tools can be certain that their parameters aren't modified so there is no need to assert that the properties don't change. Abstract subprograms cannot be directly called. 

{AI12-0187-1} {AI12-0324-1} {AI12-0405-1} For every primitive subprogram S of a type T, the class-wide postcondition expression of S is modified to include expressions of the form F(P) = F(P)'Old, all anded with each other and any explicit class-wide postcondition expression, with one such equality included for each class-wide stable property function F of S for type T that does not occur in any class-wide postcondition expression that applies to S, and P is each parameter of S that has type T and is not excluded from stable property checks. The resulting class-wide postcondition expression of S is used in place of the explicit class-wide postcondition expression of S [when interpreting the meaning of the postcondition as defined in 6.1.1].

Reason: We suppress stable property expressions if the property function appears in the explicit class-wide postcondition, or in any inherited class-wide postconditions. If we didn't do that, we could have conflicting requirements in an inherited postcondition and the current one. We also avoid redundant property checks. 

Ramification: The resulting class-wide postcondition is evaluated as described in 6.1.1. In particular, the enhanced class-wide postcondition is the class-wide postcondition for S, and therefore inherited postconditions include any stable property expressions for S.

{AI12-0405-1} In the case of a derived type T, when the preceding rules refer to "every primitive subprogram S of a type T", the referenced set of subprograms includes any inherited subprograms. 

{AI12-0405-1} The equality operation that is used in the aforementioned equality expressions is as described in the case of an individual membership test whose [membership_choice](./AA-4.4#S0137) is a choice_[simple_expression](./AA-4.4#S0138) (see 4.5.2).

{AI12-0405-1} The Post expression additions described above are enabled or disabled depending on the Post assertion policy that is in effect at the point of declaration of the subprogram S. A similar rule applies to the Post'Class expression additions. 

NOTE 1   {AI12-0112-1} For an example of the use of these aspects, see the Vector container definition in A.18.2. 


#### Extensions to Ada 2012

{AI12-0187-1} {AI12-0285-1} {AI12-0405-1} These aspects are new. 

