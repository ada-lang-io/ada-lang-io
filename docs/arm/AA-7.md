---
sidebar_position:  8
---

# 7 Packages

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[Packages are program units that allow the specification of groups of logically related entities. Typically, a package contains the declaration of a type (often a private type or private extension) along with the declarations of primitive subprograms of the type, which can be called from outside the package, while their inner workings remain hidden from outside users. ]Version=[5],Kind=(AddedNormal),Group=[C],Term=[package], Def=[a program unit that defines the interface to a group of logically related entities, along with their implementation], Note1=[Typically, a package contains the declaration of a type (often a private type or private extension) along with the declarations of primitive subprograms of the type, which can be called from outside the package, while their inner workings remain hidden from outside users.] 


## 7.1  Package Specifications and Declarations

[A package is generally provided in two parts: a [package_specification](S0162) and a [package_body](S0163). Every package has a [package_specification](S0162), but not all packages have a [package_body](S0163).] 


#### Syntax

package_declaration ::= [package_specification](S0162);

package_specification ::= 
    package [defining_program_unit_name](S0146) is
      {[basic_declarative_item](S0081)}
   [private
      {[basic_declarative_item](S0081)}]
    end [[[parent_unit_name](S0220).][identifier](S0002)]

If an [identifier](S0002) or [parent_unit_name](S0220).[identifier](S0002) appears at the end of a [package_specification](S0162), then this sequence of lexical elements shall repeat the [defining_program_unit_name](S0146). 


#### Legality Rules

A [package_declaration](S0161) or [generic_package_declaration](S0238) requires a completion [(a body)] if it contains any [declarative_item](S0080) that requires a completion, but whose completion is not in its [package_specification](S0162). 

To be honest: If an implementation supports it, a [pragma](S0016) Import may substitute for the body of a package or generic package. 


#### Static Semantics

The first list of [declarative_item](S0080)s of a [package_specification](S0162) of a package other than a generic formal package is called the visible part of the package. [ The optional list of [declarative_item](S0080)s after the reserved word private (of any [package_specification](S0162)) is called the private part of the package. If the reserved word private does not appear, the package has an implicit empty private part.]

Ramification: This definition of visible part does not apply to generic formal packages - 12.7 defines the visible part of a generic formal package.

The implicit empty private part is important because certain implicit declarations occur there if the package is a child package, and it defines types in its visible part that are derived from, or contain as components, private types declared within the parent package. These implicit declarations are visible in children of the child package. See 10.1.1. 

[An entity declared in the private part of a package is visible only within the declarative region of the package itself (including any child units - see 10.1.1). In contrast, expanded names denoting entities declared in the visible part can be used even outside the package; furthermore, direct visibility of such entities can be achieved by means of [use_clause](S0166)s (see 4.1.3 and 8.4).] 


#### Dynamic Semantics

The elaboration of a [package_declaration](S0161) consists of the elaboration of its [basic_declarative_item](S0081)s in the given order. 

NOTE 1   The visible part of a package contains all the information that another program unit is able to know about the package.

NOTE 2   If a declaration occurs immediately within the specification of a package, and the declaration has a corresponding completion that is a body, then that body has to occur immediately within the body of the package. 

Proof: This follows from the fact that the declaration and completion are required to occur immediately within the same declarative region, and the fact that bodies are disallowed (by the Syntax Rules) in [package_specification](S0162)s. This does not apply to instances of generic units, whose bodies can occur in [package_specification](S0162)s. 


#### Examples

Example of a package declaration: 

```ada
package Rational_Numbers is

```

```ada
   type Rational is
      record
         Numerator   : Integer;
         Denominator : Positive;
      end record;

```

```ada
   function "="(X,Y : Rational) return Boolean;

```

```ada
   function "/"  (X,Y : Integer)  return Rational;  --  to construct a rational number

```

```ada
   function "+"  (X,Y : Rational) return Rational;
   function "-"  (X,Y : Rational) return Rational;
   function "*"  (X,Y : Rational) return Rational;
   function "/"  (X,Y : Rational) return Rational;
end Rational_Numbers;

```

There are also many examples of package declarations in the predefined language environment (see Annex A). 


#### Incompatibilities With Ada 83

In Ada 83, a library package is allowed to have a body even if it doesn't need one. In Ada 95, a library package body is either required or forbidden - never optional. The workaround is to add pragma Elaborate_Body, or something else requiring a body, to each library package that has a body that isn't otherwise required. 


#### Wording Changes from Ada 83

We have moved the syntax into this clause and the next clause from RM83-7.1, "Package Structure", which we have removed.

RM83 was unclear on the rules about when a package requires a body. For example, RM83-7.1(4) and RM83-7.1(8) clearly forgot about the case of an incomplete type declared in a [package_declaration](S0161) but completed in the body. In addition, RM83 forgot to make this rule apply to a generic package. We have corrected these rules. Finally, since we now allow a [pragma](S0016) Import for any explicit declaration, the completion rules need to take this into account as well. 


## 7.2  Package Bodies

[In contrast to the entities declared in the visible part of a package, the entities declared in the [package_body](S0163) are visible only within the [package_body](S0163) itself. As a consequence, a package with a [package_body](S0163) can be used for the construction of a group of related subprograms in which the logical operations available to clients are clearly isolated from the internal entities.] 


#### Syntax

package_body ::= 
    package body [defining_program_unit_name](S0146) is
       [declarative_part](S0079)
   [begin
        [handled_sequence_of_statements](S0231)]
    end [[[parent_unit_name](S0220).][identifier](S0002)];

If an [identifier](S0002) or [parent_unit_name](S0220).[identifier](S0002) appears at the end of a [package_body](S0163), then this sequence of lexical elements shall repeat the [defining_program_unit_name](S0146). 


#### Legality Rules

A [package_body](S0163) shall be the completion of a previous [package_declaration](S0161) or [generic_package_declaration](S0238). A library [package_declaration](S0161) or library [generic_package_declaration](S0238) shall not have a body unless it requires a body[; pragma Elaborate_Body can be used to require a [library_unit_declaration](S0217) to have a body (see 10.2.1) if it would not otherwise require one]. 

Ramification: The first part of the rule forbids a [package_body](S0163) from standing alone - it has to belong to some previous [package_declaration](S0161) or [generic_package_declaration](S0238).

A nonlibrary [package_declaration](S0161) or nonlibrary [generic_package_declaration](S0238) that does not require a completion may have a corresponding body anyway. 


#### Static Semantics

In any [package_body](S0163) without [statement](S0124)s there is an implicit [null_statement](S0127). For any [package_declaration](S0161) without an explicit completion, there is an implicit [package_body](S0163) containing a single [null_statement](S0127). For a noninstance, nonlibrary package, this body occurs at the end of the [declarative_part](S0079) of the innermost enclosing program unit or [block_statement](S0138); if there are several such packages, the order of the implicit package_bodies is unspecified. [(For an instance, the implicit [package_body](S0163) occurs at the place of the instantiation (see 12.3). For a library package, the place is partially determined by the elaboration dependences (see Section 10).)] 

Discussion: Thus, for example, we can refer to something happening just after the begin of a [package_body](S0163), and we can refer to the [handled_sequence_of_statements](S0231) of a [package_body](S0163), without worrying about all the optional pieces. The place of the implicit body makes a difference for tasks activated by the package. See also RM83-9.3(5).

The implicit body would be illegal if explicit in the case of a library package that does not require (and therefore does not allow) a body. This is a bit strange, but not harmful. 


#### Dynamic Semantics

For the elaboration of a nongeneric [package_body](S0163), its [declarative_part](S0079) is first elaborated, and its [handled_sequence_of_statements](S0231) is then executed. 

NOTE 1   A variable declared in the body of a package is only visible within this body and, consequently, its value can only be changed within the [package_body](S0163). In the absence of local tasks, the value of such a variable remains unchanged between calls issued from outside the package to subprograms declared in the visible part. The properties of such a variable are similar to those of a "static" variable of C.

NOTE 2   The elaboration of the body of a subprogram explicitly declared in the visible part of a package is caused by the elaboration of the body of the package. Hence a call of such a subprogram by an outside program unit raises the exception Program_Error if the call takes place before the elaboration of the [package_body](S0163) (see 3.11). 


#### Examples

Example of a package body (see 7.1): 

```ada
package body Rational_Numbers is

```

```ada
   procedure Same_Denominator (X,Y : in out Rational) is
   begin
      --  reduces X and Y to the same denominator:
      ...
   end Same_Denominator;

```

```ada
   function "="(X,Y : Rational) return Boolean is
      U : Rational := X;
      V : Rational := Y;
   begin
      Same_Denominator (U,V);
      return U.Numerator = V.Numerator;
   end "=";

```

```ada
   function "/" (X,Y : Integer) return Rational is
   begin
      if Y &gt 0 then
         return (Numerator =&gt X,  Denominator =&gt Y);
      else
         return (Numerator =&gt -X, Denominator =&gt -Y);
      end if;
   end "/";

```

```ada
   function "+" (X,Y : Rational) return Rational is ... end "+";
   function "-" (X,Y : Rational) return Rational is ... end "-";
   function "*" (X,Y : Rational) return Rational is ... end "*";
   function "/" (X,Y : Rational) return Rational is ... end "/";

```

```ada
end Rational_Numbers;

```


#### Wording Changes from Ada 83

The syntax rule for [package_body](S0163) now uses the syntactic category [handled_sequence_of_statements](S0231).

The [declarative_part](S0079) of a [package_body](S0163) is now required; that doesn't make any real difference, since a [declarative_part](S0079) can be empty.

RM83 seems to have forgotten to say that a [package_body](S0163) can't stand alone, without a previous declaration. We state that rule here.

RM83 forgot to restrict the definition of elaboration of package_bodies to nongeneric ones. We have corrected that omission.

The rule about implicit bodies (from RM83-9.3(5)) is moved here, since it is more generally applicable. 


## 7.3  Private Types and Private Extensions

[The declaration (in the visible part of a package) of a type as a private type or private extension serves to separate the characteristics that can be used directly by outside program units (that is, the logical properties) from other characteristics whose direct use is confined to the package (the details of the definition of the type itself). See 3.9.1 for an overview of type extensions. ]


#### Language Design Principles

A private (untagged) type can be thought of as a record type with the type of its single (hidden) component being the full view.

A private tagged type can be thought of as a private extension of an anonymous parent with no components. The only dispatching operation of the parent is equality (although the Size attribute, and, if nonlimited, assignment are allowed, and those will presumably be implemented in terms of dispatching). 


#### Syntax

private_type_declaration ::= 
   type [defining_identifier](S0019) [[discriminant_part](S0056)] is [[abstract] tagged] [limited] private;

private_extension_declaration ::= 
   type [defining_identifier](S0019) [[discriminant_part](S0056)] is
     [abstract]  new ancestor_[subtype_indication](S0024) with private;


#### Legality Rules

A [private_type_declaration](S0164) or [private_extension_declaration](S0165) declares a partial view of the type; such a declaration is allowed only as a [declarative_item](S0080) of the visible part of a package, and it requires a completion, which shall be a [full_type_declaration](S0021) that occurs as a [declarative_item](S0080) of the private part of the package. [ The view of the type declared by the [full_type_declaration](S0021) is called the full view.] A generic formal private type or a generic formal private extension is also a partial view. 

To be honest: A private type can also becompleted by a [pragma](S0016) Import, if supported by an implementation. 

Reason: We originally used the term "private view", but this was easily confused with the view provided from the private part, namely the full view. 

[A type shall be completely defined before it is frozen (see 3.11.1 and 13.14). Thus, neither the declaration of a variable of a partial view of a type, nor the creation by an [allocator](S0122) of an object of the partial view are allowed before the full declaration of the type. Similarly, before the full declaration, the name of the partial view cannot be used in a [generic_instantiation](S0241) or in a representation item.] 

Proof: This rule is stated officially in 3.11.1, "Completions of Declarations". 

[A private type is limited if its declaration includes the reserved word limited; a private extension is limited if its ancestor type is limited.] If the partial view is nonlimited, then the full view shall be nonlimited. If a tagged partial view is limited, then the full view shall be limited. [On the other hand, if an untagged partial view is limited, the full view may be limited or nonlimited.]

If the partial view is tagged, then the full view shall be tagged. [On the other hand, if the partial view is untagged, then the full view may be tagged or untagged.] In the case where the partial view is untagged and the full view is tagged, no derivatives of the partial view are allowed within the immediate scope of the partial view; [derivatives of the full view are allowed.] 

Ramification: Note that deriving from a partial view within its immediate scope can only occur in a package that is a child of the one where the partial view is declared. The rule implies that in the visible part of a public child package, it is impossible to derive from an untagged private type declared in the visible part of the parent package in the case where the full view of the parent type turns out to be tagged. We considered a model in which the derived type was implicitly redeclared at the earliest place within its immediate scope where characteristics needed to be added. However, we rejected that model, because (1) it would imply that (for an untagged type) subprograms explicitly declared after the derived type could be inherited, and (2) to make this model work for composite types as well, several implicit redeclarations would be needed, since new characteristics can become visible one by one; that seemed like too much mechanism. 

Discussion: The rule for tagged partial views is redundant for partial views that are private extensions, since all extensions of a given ancestor tagged type are tagged, and limited if the ancestor is limited. We phrase this rule partially redundantly to keep its structure parallel with the other rules. 

To be honest: This rule is checked in a generic unit, rather than using the "assume the best" or "assume the worst" method. 

Reason: Tagged limited private types have certain capabilities that are incompatible with having assignment for the full view of the type. In particular, tagged limited private types can be extended with access discriminants and components of a limited type, which works only because assignment is not allowed. Consider the following example: 

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
    end A;
end P1;

```

```ada
with P1;
package P2 is
    type T2(D : access Integer) -- Trouble!
            is new P1.T1 with
        record
            My_Task : Some_Task_Type; -- More trouble!
        end record;
end P2;

```

```ada
with P1;
with P2;
procedure Main is
    Local : aliased Integer;
    Y : P2.T2(A =&gt Local'Access);
begin
    P1.Foo(Y);
end Main;
  

```

If the above example were legal, we would have succeeded in making an access value that points to Main.Local after Main has been left, and we would also have succeeded in doing an assignment of a task object, both of which are supposed to be no-no's. 

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
package Q is
    type T is new Parent;
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

The declaration of T declares an untagged view. This view is always untagged, so T'Class is illegal, it would be illegal to extend T, and so forth. The component name X is never visible for this view, although the component is still there - one can get one's hands on it via a [type_conversion](S0120).

The ancestor subtype of a [private_extension_declaration](S0165) is the subtype defined by the ancestor_[subtype_indication](S0024); the ancestor type shall be a specific tagged type. The full view of a private extension shall be derived (directly or indirectly) from the ancestor type. In addition to the places where Legality Rules normally apply (see 12.3), the requirement that the ancestor be specific applies also in the private part of an instance of a generic unit. 

Reason: This rule allows the full view to be defined through several intermediate derivations, possibly from a series of types produced by [generic_instantiation](S0241)s. 

If the declaration of a partial view includes a [known_discriminant_part](S0058), then the [full_type_declaration](S0021) shall have a fully conforming [(explicit)] [known_discriminant_part](S0058) [(see 6.3.1, "Conformance Rules")]. [The ancestor subtype may be unconstrained; the parent subtype of the full view is required to be constrained (see 3.7).] 

Discussion: If the ancestor subtype has discriminants, then it is usually best to make it unconstrained. 

Ramification: If the partial view has a [known_discriminant_part](S0058), then the full view has to be a composite, non-array type, since only such types may have known discriminants. Also, the full view cannot inherit the discriminants in this case; the [known_discriminant_part](S0058) has to be explicit.

That is, the following is illegal: 

```ada
package P is
    type T(D : Integer) is private;
private
    type T is new Some_Other_Type; -- Illegal!
end P;
  

```

even if Some_Other_Type has an integer discriminant called D.

It is a ramification of this and other rules that in order for a tagged type to privately inherit unconstrained discriminants, the private type declaration has to have an [unknown_discriminant_part](S0057). 

If a private extension inherits known discriminants from the ancestor subtype, then the full view shall also inherit its discriminants from the ancestor subtype, and the parent subtype of the full view shall be constrained if and only if the ancestor subtype is constrained. 

Reason: The first part ensures that the full view has the same discriminants as the partial view. The second part ensures that if the partial view is unconstrained, then the full view is also unconstrained; otherwise, a client might constrain the partial view in a way that conflicts with the constraint on the full view. 

[If a partial view has unknown discriminants, then the [full_type_declaration](S0021) may define a definite or an indefinite subtype, with or without discriminants.]

If a partial view has neither known nor unknown discriminants, then the [full_type_declaration](S0021) shall define a definite subtype.

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

A [private_type_declaration](S0164) declares a private type and its first subtype. Similarly, a [private_extension_declaration](S0165) declares a private extension and its first subtype. 

Discussion: A package-private type is one declared by a [private_type_declaration](S0164); that is, a private type other than a generic formal private type. Similarly, a package-private extension is one declared by a [private_extension_declaration](S0165). These terms are not used in the RM95 version of this document. 

A declaration of a partial view and the corresponding [full_type_declaration](S0021) define two views of a single type. The declaration of a partial view together with the visible part define the operations that are available to outside program units; the declaration of the full view together with the private part define other operations whose direct use is possible only within the declarative region of the package itself. Moreover, within the scope of the declaration of the full view, the characteristics of the type are determined by the full view; in particular, within its scope, the full view determines the classes that include the type, which components, entries, and protected subprograms are visible, what attributes and other predefined operations are allowed, and whether the first subtype is static. See 7.3.1.

A private extensioninherits components (including discriminants unless there is a new [discriminant_part](S0056) specified) and user-defined primitive subprograms from its ancestor type, in the same way that a record extension inherits components and user-defined primitive subprograms from its parent type (see 3.4). 

To be honest: If an operation of the  parent type is abstract, then the abstractness of the inherited operation is different for nonabstract record extensions than for nonabstract private extensions (see 3.9.3). 


#### Dynamic Semantics

The elaboration of a [private_type_declaration](S0164) creates a partial view of a type. The elaboration of a [private_extension_declaration](S0165) elaborates the ancestor_[subtype_indication](S0024), and creates a partial view of a type. 

NOTE 1   The partial view of a type as declared by a [private_type_declaration](S0164) is defined to be a composite view (in 3.2). The full view of the type might or might not be composite. A private extension is also composite, as is its full view.

NOTE 2   Declaring a private type with an [unknown_discriminant_part](S0057) is a way of preventing clients from creating uninitialized objects of the type; they are then forced to initialize each object by calling some operation declared in the visible part of the package. If such a type is also limited, then no objects of the type can be declared outside the scope of the [full_type_declaration](S0021), restricting all object creation to the package defining the type. This allows complete control over all storage allocation for the type. Objects of such a type can still be passed as parameters, however. 

Discussion: Packages with private types are analogous to generic packages with formal private types, as follows: The declaration of a package-private type is like the declaration of a formal private type. The visible part of the package is like the generic formal part; these both specify a contract (that is, a set of operations and other things available for the private type). The private part of the package is like an instantiation of the generic; they both give a [full_type_declaration](S0021) that specifies implementation details of the private type. The clients of the package are like the body of the generic; usage of the private type in these places is restricted to the operations defined by the contract.

In other words, being inside the package is like being outside the generic, and being outside the package is like being inside the generic; a generic is like an "inside-out" package.

This analogy also works for private extensions in the same inside-out way.

Many of the legality rules are defined with this analogy in mind. See, for example, the rules relating to operations of [formal] derived types.

The completion rules for a private type are intentionally quite similar to the matching rules for a generic formal private type.

This analogy breaks down in one respect: a generic actual subtype is a subtype, whereas the full view for a private type is always a new type. (We considered allowing the completion of a [private_type_declaration](S0164) to be a [subtype_declaration](S0023), but the semantics just won't work.) This difference is behind the fact that a generic actual type can be class-wide, whereas the completion of a private type always declares a specific type. 

NOTE 3   The ancestor type specified in a [private_extension_declaration](S0165) and the parent type specified in the corresponding declaration of a record extension given in the private part need not be the same - the parent type of the full view can be any descendant of the ancestor type. In this case, for a primitive subprogram that is inherited from the ancestor type and not overridden, the formal parameter names and default expressions (if any) come from the corresponding primitive subprogram of the specified ancestor type, while the body comes from the corresponding primitive subprogram of the parent type of the full view. See 3.9.2.


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

The syntax for a [private_type_declaration](S0164) is augmented to allow the reserved word tagged.

In Ada 83, a private type without discriminants cannot be completed with a type with discriminants. Ada 95 allows the full view to have discriminants, so long as they have defaults (that is, so long as the first subtype is definite). This change is made for uniformity with generics, and because the rule as stated is simpler and easier to remember than the Ada 83 rule. In the original version of Ada 83, the same restriction applied to generic formal private types. However, the restriction was removed by the ARG for generics. In order to maintain the "generic contract/private type contract analogy" discussed above, we have to apply the same rule to package-private types. Note that a private untagged type without discriminants can be completed with a tagged type with discriminants only if the full view is constrained, because discriminants of tagged types cannot have defaults. 


#### Wording Changes from Ada 83

RM83-7.4.1(4), "Within the specification of the package that declares a private type and before the end of the corresponding full type declaration, a restriction applies....", is subsumed (and corrected) by the rule that a type shall be completely defined before it is frozen, and the rule that the parent type of a derived type declaration shall be completely defined, unless the derived type is a private extension. 


### 7.3.1  Private Operations

[For a type declared in the visible part of a package or generic package, certain operations on the type do not become visible until later in the package - either in the private part or the body. Such private operations are available only inside the declarative region of the package or generic package.] 


#### Static Semantics

The predefined operators that exist for a given type are determined by the classes to which the type belongs. For example, an integer type has a predefined "+" operator. In most cases, the predefined operators of a type are declared immediately after the definition of the type; the exceptions are explained below. Inherited subprograms are also implicitly declared immediately after the definition of the type, except as stated below.

For a composite type, the characteristics (see 7.3) of the type are determined in part by the characteristics of its component types. At the place where the composite type is declared, the only characteristics of component types used are those characteristics visible at that place. If later within the immediate scope of the composite type additional characteristics become visible for a component type, then any corresponding characteristics become visible for the composite type. Any additional predefined operators are implicitly declared at that place.

The corresponding rule applies to a type defined by a [derived_type_definition](S0032), if there is a place within its immediate scope where additional characteristics of its parent type become visible.

[For example, an array type whose component type is limited private becomes nonlimited if the full view of the component type is nonlimited and visible at some later place within the immediate scope of the array type. In such a case, the predefined "=" operator is implicitly declared at that place, and assignment is allowed after that place.]

Inherited primitive subprograms follow a different rule. For a [derived_type_definition](S0032), each inherited primitive subprogram is implicitly declared at the earliest place, if any, within the immediate scope of the [type_declaration](S0020), but after the [type_declaration](S0020), where the corresponding declaration from the parent is visible. If there is no such place, then the inherited subprogram is not declared at all. [An inherited subprogram that is not declared at allcannot be named in a call and cannot be overridden, but for a tagged type, it is possible to dispatch to it.]

For a [private_extension_declaration](S0165), each inherited subprogram is declared immediately after the [private_extension_declaration](S0165) if the corresponding declaration from the ancestor is visible at that place. Otherwise, the inherited subprogram is not declared for the private extension, [though it might be for the full type]. 

Reason: There is no need for the "earliest place within the immediate scope" business here, because a [private_extension_declaration](S0165) will be completed with a [full_type_declaration](S0021), so we can hang the necessary private implicit declarations on the [full_type_declaration](S0021). 

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

Type T2 inherits Op1 and Op2 from Root. However, the inherited Op2 is never declared, because Parent.Op2 is never visible within the immediate scope of T2. T2 explicitly declares its own Op2, but this is unrelated to the inherited one - it does not override the inherited one, and occupies a different slot in the type descriptor.

T3 inherits both Op1 and Op2. Op1 is implicitly declared immediately after the type declaration, whereas Op2 is declared at the beginning of the private part. Note that if Child were a private child of Parent, then Op1 and Op2 would both be implicitly declared immediately after the type declaration.

T4 is similar to T3, except that the earliest place within T4's immediate scope where Root's Op2 is visible is in the body of Nested.

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

[The Class attribute is defined for tagged subtypes in 3.9. In addition,] for every subtype S of an untagged private type whose full view is tagged, the following attribute is defined: 

S'ClassDenotes the class-wide subtype corresponding to the full view of S. This attribute is allowed only from the beginning of the private part in which the full view is declared, until the declaration of the full view. [After the full view, the Class attribute of the full view can be used.] 

NOTE 1   Because a partial view and a full view are two different views of one and the same type, outside of the defining package the characteristics of the type are those defined by the visible part. Within these outside program units the type is just a private type or private extension, and any language rule that applies only to another class of types does not apply. The fact that the full declaration might implement a private type with a type of a particular class (for example, as an array type) is relevant only within the declarative region of the package itself including any child units.

The consequences of this actual implementation are, however, valid everywhere. For example: any default initialization of components takes place; the attribute Size provides the size of the full view; finalization is still done for controlled components of the full view; task dependence rules still apply to components that are task objects.

NOTE 2   Partial views provide assignment (unless the view is limited), membership tests, selected components for the selection of discriminants and inherited components, qualification, and explicit conversion.

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

The explicitly declared operator "&lt" hides the predefined operator "&lt" implicitly declared by the [full_type_declaration](S0021). Within the body of the function, an explicit conversion of X and Y to the subtype Natural is necessary to invoke the "&lt" operator of the parent type. Alternatively, the result of the function could be written as not (X &gt= Y), since the operator "&gt=" is not redefined.

The value of the variable Last_Key, declared in the package body, remains unchanged between calls of the procedure Get_Key. (See also the NOTES of 7.2.) 


#### Wording Changes from Ada 83

The phrase in RM83-7.4.2(7), "...after the full type declaration", doesn't work in the presence of child units, so we define that rule in terms of visibility.

The definition of the Constrained attribute for private types has been moved to "Obsolescent Features". (The Constrained attribute of an object has not been moved there.) 

Version=[5],Kind=(AddedNormal),Group=[T],Term=[type invariant], Def=[see invariant] Version=[5],Kind=(AddedNormal),Group=[T],Term=[invariant], Def=[an assertion that is expected to be True for all objects of a given private type when viewed from outside the defining package] 

Version=[5],Kind=(AddedNormal),Group=[T],Term=[default initial condition], Def=[a property that holds for every default-initialized object of a given type] 

Version=[5],Kind=(AddedNormal),Group=[T],Term=[stable property], Def=[a characteristic associated with objects of a given type that is preserved by many of the primitive operations of the type] 


## 7.4  Deferred Constants

[Deferred constant declarations may be used to declare constants in the visible part of a package, but with the value of the constant given in the private part. They may also be used to declare constants imported from other languages (see Annex B).] 


#### Legality Rules

[ A deferred constant declaration is an [object_declaration](S0029) with the reserved word constant but no initialization expression.] The constant declared by a deferred constant declaration is called a deferred constant. A deferred constant declaration requires a completion, which shall be a full constant declaration (called the full declaration of the deferred constant), or a [pragma](S0016) Import (see Annex B). 

Proof: The first sentence is redundant, as it is stated officially in 3.3.1.

A deferred constant declaration that is completed by a full constant declaration shall occur immediately within the visible part of a [package_specification](S0162). For this case, the following additional rules apply to the corresponding full declaration: 

The full declaration shall occur immediately within the private part of the same package;

The deferred and full constants shall have the same type; 

Ramification: This implies that both the deferred declaration and the full declaration have to have a [subtype_indication](S0024) rather than an [array_type_definition](S0048), because each [array_type_definition](S0048) would define a new type. 

If the subtype defined by the [subtype_indication](S0024) in the deferred declaration is constrained, then the subtype defined by the [subtype_indication](S0024) in the full declaration shall match it statically.[ On the other hand, if the subtype of the deferred constant is unconstrained, then the full declaration is still allowed to impose a constraint. The constant itself will be constrained, like all constants;]

If the deferred constant declaration includes the reserved word aliased, then the full declaration shall also. 

Ramification: On the other hand, the full constant can be aliased even if the deferred constant is not. 

[A deferred constant declaration that is completed by a [pragma](S0016) Import need not appear in the visible part of a [package_specification](S0162), and has no full constant declaration.]

The completion of a deferred constant declaration shall occur before the constant is frozen (see 7.4).


#### Dynamic Semantics

The elaboration of a deferred constant declaration elaborates the [subtype_indication](S0024) or (only allowed in the case of an imported constant) the [array_type_definition](S0048). 

NOTE   The full constant declaration for a deferred constant that is of a given private type or private extension is not allowed before the corresponding [full_type_declaration](S0021). This is a consequence of the freezing rules for types (see 13.14). 

Ramification: Multiple or single declarations are allowed for the deferred and the full declarations, provided that the equivalent single declarations would be allowed.

Deferred constant declarations are useful for declaring constants of private views, and types with components of private views. They are also useful for declaring access-to-constant objects that designate variables declared in the private part of a package. 


#### Examples

Examples of deferred constant declarations: 

```ada
Null_Key : constant Key;      -- see 7.3.1

```

```ada
CPU_Identifier : constant String(1..8);
pragma Import(Assembler, CPU_Identifier, Link_Name =&gt "CPU_ID");
                              -- see B.1

```


#### Extensions to Ada 83

In Ada 83, a deferred constant is required to be of a private type declared in the same visible part. This restriction is removed for Ada 95; deferred constants can be of any type.

In Ada 83, a deferred constant declaration was not permitted to include a constraint, nor the reserved word aliased.

In Ada 83, the rules required conformance of type marks; here we require static matching of subtypes if the deferred constant is constrained.

A deferred constant declaration can be completed with a [pragma](S0016) Import. Such a deferred constant declaration need not be within a [package_specification](S0162).

The rules for too-early uses of deferred constants are modified in Ada 95 to allow more cases, and catch all errors at compile time. This change is necessary in order to allow deferred constants of a tagged type without violating the principle that for a dispatching call, there is always an implementation to dispatch to. It has the beneficial side effect of catching some Ada-83-erroneous programs at compile time. The new rule fits in well with the new freezing-point rules. Furthermore, we are trying to convert undefined-value problems into bounded errors, and we were having trouble for the case of deferred constants. Furthermore, uninitialized deferred constants cause trouble for the shared variable / tasking rules, since they are really variable, even though they purport to be constant. In Ada 95, they cannot be touched until they become constant.

Note that we do not consider this change to be an upward incompatibility, because it merely changes an erroneous execution in Ada 83 into a compile-time error.

The Ada 83 semantics are unclear in the case where the full view turns out to be an access type. It is a goal of the language design to prevent uninitialized access objects. One wonders if the implementation is required to initialize the deferred constant to null, and then initialize it (again!) to its real value. In Ada 95, the problem goes away. 


#### Wording Changes from Ada 83

Since deferred constants can now be of a nonprivate type, we have made this a stand-alone clause, rather than a subclause of 7.3, "Private Types and Private Extensions".

Deferred constant declarations used to have their own syntax, but now they are simply a special case of [object_declaration](S0029)s. 


## 7.5  Limited Types

[A limited type is (a view of) a type for which the assignment operation is not allowed. A nonlimited type is a (view of a) type for which the assignment operation is allowed.] 

Discussion: The concept of the value of a limited type is difficult to define, since the abstract value of a limited type often extends beyond its physical representation. In some sense, values of a limited type cannot be divorced from their object. The value is the object.

In Ada 83, in the two places where limited types were defined by the language, namely tasks and files, an implicit level of indirection was implied by the semantics to avoid the separation of the value from an associated object. In Ada 95, most limited types are passed by reference, and even return-ed by reference. 

To be honest: For a limited partial view whose full view is nonlimited, assignment is possible on parameter passing and function return. To prevent any copying whatsoever, one should make both the partial and full views limited. 

Glossary entry: A limited type is (a view of) a type for which the assignment operation is not allowed. A nonlimited type is a (view of a) type for which the assignment operation is allowed.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[limited type], Def=[a type for which copying (such as in an [assignment_statement](S0130)) is not allowed], Note1=[A nonlimited type is a type for which copying is allowed.] 


#### Legality Rules

If a tagged record type has any limited components, then the reserved word limited shall appear in its [record_type_definition](S0063). 

Reason: This prevents tagged limited types from becoming nonlimited. Otherwise, the following could happen: 

```ada
package P is
    type T is limited private;
    type R is tagged
        record -- Illegal!
               -- This should say "limited record".
            X : T;
        end record;
private
    type T is new Integer; -- R becomes nonlimited here.
end P;

```

```ada
package Q is
    type R2(Access_Discrim : access ...) is new R with
        record
            Y : Some_Task_Type;
        end record;
end Q;

```

If the above were legal, then assignment would be defined for R'Class in the body of P, which is bad news, given the access discriminant and the task. 


#### Static Semantics

A type is limited if it is a descendant of one of the following: 

a type with the reserved word limited in its definition; 

Ramification: Note that there is always a "definition", conceptually, even if there is no syntactic category called "..._definition".

a task or protected type;

a composite type with a limited component.

Otherwise, the type is nonlimited.

[There are no predefined equality operators for a limited type.]

NOTE 1   The following are consequences of the rules for limited types: 

An initialization expression is not allowed in an [object_declaration](S0029) if the type of the object is limited.

A default expression is not allowed in a [component_declaration](S0067) if the type of the record component is limited.

An initialized allocator is not allowed if the designated type is limited.

A generic formal parameter of mode in must not be of a limited type. 

NOTE 2   [Aggregate](S0097)s are not available for a limited composite type. Concatenation is not available for a limited array type.

NOTE 3   The rules do not exclude a [default_expression](S0060) for a formal parameter of a limited type; they do not exclude a deferred constant of a limited type if the full declaration of the constant is of a nonlimited type.

NOTE 4   As illustrated in 7.3.1, an untagged limited type can become nonlimited under certain circumstances. 

Ramification: Limited private types do not become nonlimited; instead, their full view can be nonlimited, which has a similar effect.

It is important to remember that a single nonprivate type can be both limited and nonlimited in different parts of its scope. In other words, "limited" is a property that depends on where you are in the scope of the type. We don't call this a "view property" because there is no particular declaration to declare the nonlimited view.

Tagged types never become nonlimited. 


#### Examples

Example of a package with a limited type: 

```ada
package IO_Package is
   type File_Name is limited private;

```

```ada
   procedure Open (F : in out File_Name);
   procedure Close(F : in out File_Name);
   procedure Read (F : in File_Name; Item : out Integer);
   procedure Write(F : in File_Name; Item : in  Integer);
private
   type File_Name is
      limited record
         Internal_Name : Integer := 0;
      end record;
end IO_Package;

```

```ada
package body IO_Package is
   Limit : constant := 200;
   type File_Descriptor is record  ...  end record;
   Directory : array (1 .. Limit) of File_Descriptor;
   ...
   procedure Open (F : in out File_Name) is  ...  end;
   procedure Close(F : in out File_Name) is  ...  end;
   procedure Read (F : in File_Name; Item : out Integer) is ... end;
   procedure Write(F : in File_Name; Item : in  Integer) is ... end;
begin
   ...
end IO_Package;

```

NOTE 5   Notes on the example: In the example above, an outside subprogram making use of IO_Package may obtain a file name by calling Open and later use it in calls to Read and Write. Thus, outside the package, a file name obtained from Open acts as a kind of password; its internal properties (such as containing a numeric value) are not known and no other operations (such as addition or comparison of internal names) can be performed on a file name. Most importantly, clients of the package cannot make copies of objects of type File_Name.

This example is characteristic of any case where complete control over the operations of a type is desired. Such packages serve a dual purpose. They prevent a user from making use of the internal structure of the type. They also implement the notion of an encapsulated data type where the only operations on the type are those given in the package specification.

The fact that the full view of File_Name is explicitly declared limited means that parameter passing and function return will always be by reference (see 6.2 and 6.5).


#### Extensions to Ada 83

The restrictions in RM83-7.4.4(4), which disallowed out parameters of limited types in certain cases, are removed. 


#### Wording Changes from Ada 83

Since limitedness and privateness are orthogonal in Ada 95 (and to some extent in Ada 83), this is now its own clause rather than being a subclause of 7.3, "Private Types and Private Extensions". 


## 7.6  User-Defined Assignment and Finalization

[ Three kinds of actions are fundamental to the manipulation of objects: initialization, finalization, and assignment. Every object is initialized, either explicitly or by default, after being created (for example, by an [object_declaration](S0029) or [allocator](S0122)). Every object is finalized before being destroyed (for example, by leaving a [subprogram_body](S0154) containing an [object_declaration](S0029), or by a call to an instance of Unchecked_Deallocation). An assignment operation is used as part of [assignment_statement](S0130)s, explicit initialization, parameter passing, and other operations. 

Default definitions for these three fundamental operations are provided by the language, but a controlled type gives the user additional control over parts of these operations. In particular, the user can define, for a controlled type, an Initialize procedure which is invoked immediately after the normal default initialization of a controlled object, a Finalize procedure which is invoked immediately before finalization of any of the components of a controlled object, and an Adjust procedure which is invoked as the last step of an assignment to a (nonlimited) controlled object.] 

Glossary entry: A controlled type supports user-defined assignment and finalization. Objects are always finalized before being destroyed.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[controlled type], Def=[a type that supports user-defined assignment and finalization], Note1=[Objects are always finalized before being destroyed.] 

Ramification: Here's the basic idea of initialization, value adjustment, and finalization, whether or not user defined: When an object is created, if it is explicitly assigned an initial value, the assignment copies and adjusts the initial value. Otherwise, Initialize is applied to it (except in the case of an [aggregate](S0097) as a whole). An [assignment_statement](S0130) finalizes the target before copying in and adjusting the new value. Whenever an object goes away, it is finalized. Calls on Initialize and Adjust happen bottom-up; that is, components first, followed by the containing object. Calls on Finalize happens top-down; that is, first the containing object, and then its components. These ordering rules ensure that any components will be in a well-defined state when Initialize, Adjust, or Finalize is applied to the containing object. 


#### Static Semantics

The following language-defined library package exists: 

```ada
package Ada.Finalization is
    pragma Preelaborate(Finalization);

```

```ada
    type Controlled is abstract tagged private;

```

```ada
    procedure Initialize (Object : in out Controlled);
    procedure Adjust     (Object : in out Controlled);
    procedure Finalize   (Object : in out Controlled);

```

```ada
    type Limited_Controlled is abstract tagged limited private;

```

```ada
    procedure Initialize (Object : in out Limited_Controlled);
    procedure Finalize   (Object : in out Limited_Controlled);
private
    ... -- not specified by the language
end Ada.Finalization;

```

A controlled type is a descendant of Controlled or Limited_Controlled. The (default) implementations of Initialize, Adjust, and Finalize have no effect. The predefined "=" operator of type Controlled always returns True, [since this operator is incorporated into the implementation of the predefined equality operator of types derived from Controlled, as explained in 4.5.2.] The type Limited_Controlled is like Controlled, except that it is limited and it lacks the primitive subprogram Adjust. 

Discussion: We say "nonlimited controlled type" (rather than just "controlled type";) when we want to talk about descendants of Controlled only. 

Reason: We considered making Adjust and Finalize abstract. However, a reasonable coding convention is e.g. for Finalize to always call the parent's Finalize after doing whatever work is needed for the extension part. (Unlike CLOS, we have no way to do that automatically in Ada 95.) For this to work, Finalize cannot be abstract. In a generic unit, for a generic formal abstract derived type whose ancestor is Controlled or Limited_Controlled, calling the ancestor's Finalize would be illegal if it were abstract, even though the actual type might have a concrete version.

Types Controlled and Limited_Controlled are abstract, even though they have no abstract primitive subprograms. It is not clear that they need to be abstract, but there seems to be no harm in it, and it might make an implementation's life easier to know that there are no objects of these types - in case the implementation wishes to make them "magic" in some way.


#### Dynamic Semantics

During the elaboration of an [object_declaration](S0029), for every controlled subcomponent of the object that is not assigned an initial value (as defined in 3.3.1), Initialize is called on that subcomponent. Similarly, if the object as a whole is controlled and is not assigned an initial value, Initialize is called on the object. The same applies to the evaluation of an [allocator](S0122), as explained in 4.8.

For an [extension_aggregate](S0102) whose [ancestor_part](S0103) is a [subtype_mark](S0025), Initialize is called on all controlled subcomponents of the ancestor part; if the type of the ancestor part is itself controlled, the Initialize procedure of the ancestor type is called, unless that Initialize procedure is abstract. 

Discussion: Example: 

```ada
type T1 is new Controlled with
    record
        ... -- some components might have defaults
    end record;

```

```ada
type T2 is new Controlled with
    record
        X : T1; -- no default
        Y : T1 := ...; -- default
    end record;

```

```ada
A : T2;
B : T2 := ...;

```

As part of the elaboration of A's declaration, A.Y is assigned a value; therefore Initialize is not applied to A.Y. Instead, Adjust is applied to A.Y as part of the assignment operation. Initialize is applied to A.X and to A, since those objects are not assigned an initial value. The assignment to A.Y is not considered an assignment to A.

For the elaboration of B's declaration, Initialize is not called at all. Instead the assignment adjusts B's value; that is, it applies Adjust to B.X, B.Y, and B.

Initialize and other initialization operations are done in an arbitrary order, except as follows. Initialize is applied to an object after initialization of its subcomponents, if any [(including both implicit initialization and Initialize calls)]. If an object has a component with an access discriminant constrained by a per-object expression, Initialize is applied to this component after any components that do not have such discriminants. For an object with several components with such a discriminant, Initialize is applied to them in order of their [component_declaration](S0067)s. For an [allocator](S0122), any task activations follow all calls on Initialize. 

Reason: The fact that Initialize is done for subcomponents first allows Initialize for a composite object to refer to its subcomponents knowing they have been properly initialized.

The fact that Initialize is done for components with access discriminants after other components allows the Initialize operation for a component with a self-referential access discriminant to assume that other components of the enclosing object have already been properly initialized. For multiple such components, it allows some predictability. 

When a target object with any controlled parts is assigned a value, [either when created or in a subsequent [assignment_statement](S0130),] the assignment operation proceeds as follows: 

The value of the target becomes the assigned value.

The value of the target is adjusted. 

Ramification: If any parts of the object are controlled, abort is deferred during the assignment operation. 

To adjust the value of a [(nonlimited)] composite object, the values of the components of the object are first adjusted in an arbitrary order, and then, if the object is controlled, Adjust is called. Adjusting the value of an elementary object has no effect[, nor does adjusting the value of a composite object with no controlled parts.] 

Ramification: Adjustment is never performed for values of a by-reference limited type, since these types do not support copying. 

Reason: The verbiage in the Initialize rule about access discriminants constrained by per-object expressions is not necessary here, since such types are limited, and therefore are never adjusted. 

For an [assignment_statement](S0130), [ after the [name](S0084) and [expression](S0108) have been evaluated, and any conversion (including constraint checking) has been done,] an anonymous object is created, and the value is assigned into it; [that is, the assignment operation is applied]. [(Assignment includes value adjustment.)] The target of the [assignment_statement](S0130) is then finalized. The value of the anonymous object is then assigned into the target of the [assignment_statement](S0130). Finally, the anonymous object is finalized. [As explained below, the implementation may eliminate the intermediate anonymous object, so this description subsumes the one given in 5.2, "Assignment Statements".] 

Reason: An alternative design for user-defined assignment might involve an Assign operation instead of Adjust: 

```ada
procedure Assign(Target : in out Controlled; Source : in out Controlled);

```

Or perhaps even a syntax like this: 

```ada
procedure ":="(Target : in out Controlled; Source : in out Controlled);

```

Assign (or ":=") would have the responsibility of doing the copy, as well as whatever else is necessary. This would have the advantage that the Assign operation knows about both the target and the source at the same time - it would be possible to do things like reuse storage belonging to the target, for example, which Adjust cannot do. However, this sort of design would not work in the case of unconstrained discriminated variables, because there is no way to change the discriminants individually. For example: 

```ada
type Mutable(D : Integer := 0) is
    record
        X : Array_Of_Controlled_Things(1..D);
        case D is
            when 17 =&gt Y : Controlled_Thing;
            when others =&gt null;
        end D;
    end record;

```

An assignment to an unconstrained variable of type Mutable can cause some of the components of X, and the component Y, to appear and/or disappear. There is no way to write the Assign operation to handle this sort of case.

Forbidding such cases is not an option - it would cause generic contract model violations. 


#### Implementation Permissions

An implementation is allowed to relax the above rules [(for nonlimited controlled types)] in the following ways: 

Proof: The phrase "for nonlimited controlled types" follows from the fact that all of the following permissions apply to cases involving assignment. It is important because the programmer can count on a stricter semantics for limited controlled types. 

For an [assignment_statement](S0130) that assigns to an object the value of that same object, the implementation need not do anything. 

Ramification: In other words, even if an object is controlled and a combination of Finalize and Adjust on the object might have a net side effect, they need not be performed. 

For an [assignment_statement](S0130) for a noncontrolled type, the implementation may finalize and assign each component of the variable separately (rather than finalizing the entire variable and assigning the entire new value) unless a discriminant of the variable is changed by the assignment. 

Reason: For example, in a slice assignment, an anonymous object is not necessary if the slice is copied component-by-component in the right direction, since array types are not controlled (although their components may be). Note that the direction, and even the fact that it's a slice assignment, can in general be determined only at run time. 

For an [aggregate](S0097) or function call whose value is assigned into a target object, the implementation need not create a separate anonymous object if it can safely create the value of the [aggregate](S0097) or function call directly in the target object. Similarly, for an [assignment_statement](S0130), the implementation need not create an anonymous object if the value being assigned is the result of evaluating a [name](S0084) denoting an object (the source object) whose storage cannot overlap with the target. If the source object might overlap with the target object, then the implementation can avoid the need for an intermediary anonymous object by exercising one of the above permissions and perform the assignment one component at a time (for an overlapping array assignment), or not at all (for an assignment where the target and the source of the assignment are the same object). Even if an anonymous object is created, the implementation may move its value to the target object as part of the assignment without re-adjusting so long as the anonymous object has no aliased subcomponents. 

Ramification: In the [aggregate](S0097) case, only one value adjustment is necessary, and there is no anonymous object to be finalized.

In the [assignment_statement](S0130) case as well, no finalization of the anonymous object is needed. On the other hand, if the target has aliased subcomponents, then an adjustment takes place directly on the target object as the last step of the assignment, since some of the subcomponents may be self-referential or otherwise position-dependent. 


#### Extensions to Ada 83

Controlled types and user-defined finalization are new to Ada 95. (Ada 83 had finalization semantics only for masters of tasks.) 


### 7.6.1  Completion and Finalization

[This subclause defines completion and leaving of the execution of constructs and entities. A master is the execution of a construct that includes finalization of local objects after it is complete (and after waiting for any local tasks - see 9.3), but before leaving. Other constructs and entities are left immediately upon completion. ]


#### Dynamic Semantics

The execution of a construct or entity is complete when the end of that execution has been reached, or when a transfer of control (see 5.1) causes it to be abandoned. Completion due to reaching the end of execution, or due to the transfer of control of an exit_, return_, goto_, or [requeue_statement](S0195) or of the selection of a [terminate_alternative](S0205) is normal completion. Completion is abnormal otherwise [- when control is transferred out of a construct due to abort or the raising of an exception]. 

Discussion: Don't confuse the run-time concept of completion with the compile-time concept of completion defined in 3.11.1. 

After execution of a construct or entity is complete, it is left, meaning that execution continues with the next action, as defined for the execution that is taking place. Leaving an execution happens immediately after its completion, except in the case of a master: the execution of a [task_body](S0179), a [block_statement](S0138), a [subprogram_body](S0154), an [entry_body](S0190), or an [accept_statement](S0188). A master is finalized after it is complete, and before it is left.

Reason: Note that although an [accept_statement](S0188) has no [declarative_part](S0079), it can call functions and evaluate [aggregate](S0097)s, possibly causing anonymous controlled objects to be created, and we don't want those objects to escape outside the rendezvous.

Version=[5],Kind=(AddedNormal),Group=[R],Term=[master], Def=[the execution of a master construct], Note1=[Each object and task is associated with a master. When a master is left, associated tasks are awaited and associated objects are finalized.] Version=[5],Kind=(AddedNormal),Group=[C],Term=[master construct], Def=[one of certain executable constructs for which there can be objects or tasks whose lifetime ends when the construct completes], Note1=[Execution of a master construct is a master, with which objects and tasks are associated for the purposes of waiting and finalization.] For the finalization of a master, dependent tasks are first awaited, as explained in 9.3. Then each object whose accessibility level is the same as that of the master is finalized if the object was successfully initialized and still exists. [These actions are performed whether the master is left by reaching the last statement or via a transfer of control.] When a transfer of control causes completion of an execution, each included master is finalized in order, from innermost outward. 

Ramification: As explained in 3.10.2, the set of objects with the same accessibility level as that of the master includes objects declared immediately within the master, objects declared in nested packages, objects created by [allocator](S0122)s (if the ultimate ancestor access type is declared in one of those places) and subcomponents of all of these things. If an object was already finalized by Unchecked_Deallocation, then it is not finalized again when the master is left.

Note that any object whose accessibility level is deeper than that of the master would no longer exist; those objects would have been finalized by some inner master. Thus, after leaving a master, the only objects yet to be finalized are those whose accessibility level is less deep than that of the master.

To be honest: Subcomponents of objects due to be finalized are not finalized by the finalization of the master; they are finalized by the finalization of the containing object. 

Reason: We need to finalize subcomponents of objects even if the containing object is not going to get finalized because it was not fully initialized. But if the containing object is finalized, we don't want to require repeated finalization of the subcomponents, as might normally be implied by the recursion in finalization of a master and the recursion in finalization of an object. 

To be honest: Formally, completion and leaving refer to executions of constructs or entities. However, the standard sometimes (informally) refers to the constructs or entities whose executions are being completed. Thus, for example, "the subprogram call or task is complete" really means "the execution of the subprogram call or task is complete". 

For the finalization of an object: 

If the object is of an elementary type, finalization has no effect; 

If the object is of a controlled type, the Finalize procedure is called;

If the object is of a protected type, the actions defined in 9.4 are performed;

If the object is of a composite type, then after performing the above actions, if any, every component of the object is finalized in an arbitrary order, except as follows: if the object has a component with an access discriminant constrained by a per-object expression, this component is finalized before any components that do not have such discriminants; for an object with several components with such a discriminant, they are finalized in the reverse of the order of their [component_declaration](S0067)s. 

Reason: This allows the finalization of a component with an access discriminant to refer to other components of the enclosing object prior to their being finalized. 

Immediately before an instance of Unchecked_Deallocation reclaims the storage of an object, the object is finalized. [If an instance of Unchecked_Deallocation is never applied to an object created by an [allocator](S0122), the object will still exist when the corresponding master completes, and it will be finalized then.]

The order in which the finalization of a master performs finalization of objects is as follows: Objects created by declarations in the master are finalized in the reverse order of their creation. For objects that were created by [allocator](S0122)s for an access type whose ultimate ancestor is declared in the master, this rule is applied as though each such object that still exists had been created in an arbitrary order at the first freezing point (see 13.14) of the ultimate ancestor type. 

Reason: Note that we talk about the type of the [allocator](S0122) here. There may be access values of a (general) access type pointing at objects created by [allocator](S0122)s for some other type; these are not finalized at this point.

The freezing point of the ultimate ancestor access type is chosen because before that point, pool elements cannot be created, and after that point, access values designating (parts of) the pool elements can be created. This is also the point after which the pool object cannot have been declared. We don't want to finalize the pool elements until after anything finalizing objects that contain access values designating them. Nor do we want to finalize pool elements after finalizing the pool object itself. 

Ramification: Finalization of allocated objects is done according to the (ultimate ancestor) [allocator](S0122) type, not according to the storage pool in which they are allocated. Pool finalization might reclaim storage (see 13.11, "Storage Management"), but has nothing (directly) to do with finalization of the pool elements.

Note that finalization is done only for objects that still exist; if an instance of Unchecked_Deallocation has already gotten rid of a given pool element, that pool element will not be finalized when the master is left.

Note that a deferred constant declaration does not create the constant; the full constant declaration creates it. Therefore, the order of finalization depends on where the full constant declaration occurs, not the deferred constant declaration.

An imported object is not created by its declaration. It is neither initialized nor finalized. 

Implementation Note: An implementation has to ensure that the storage for an object is not reclaimed when references to the object are still possible (unless, of course, the user explicitly requests reclamation via an instance of Unchecked_Deallocation). This implies, in general, that objects cannot be deallocated one by one as they are finalized; a subsequent finalization might reference an object that has been finalized, and that object had better be in its (well-defined) finalized state. 

The target of an assignment statement is finalized before copying in the new value, as explained in 7.6.

The anonymous objects created by function calls and by [aggregate](S0097)s are finalized no later than the end of the innermost enclosing [declarative_item](S0080) or [statement](S0124); if that is a [compound_statement](S0126), they are finalized before starting the execution of any [statement](S0124) within the [compound_statement](S0126). 

To be honest: This is not to be construed as permission to call Finalize asynchronously with respect to normal user code. For example, 

```ada
declare
    X : Some_Controlled_Type := F(G(...));
    -- The anonymous objects created for F and G are finalized
    -- no later than this point.
    Y : ...
begin
    ...
end;

```

The anonymous object for G should not be finalized at some random point in the middle of the body of F, because F might manipulate the same data structures as the Finalize operation, resulting in erroneous access to shared variables. 

Reason: It might be quite inconvenient for the implementation to defer finalization of the anonymous object for G until after copying the value of F into X, especially if the size of the result is not known at the call site.


#### Bounded (Run-Time) Errors

It is a bounded error for a call on Finalize or Adjust to propagate an exception. The possible consequences depend on what action invoked the Finalize or Adjust operation: 

Ramification: It is not a bounded error for Initialize to propagate an exception. If Initialize propagates an exception, then no further calls on Initialize are performed, and those components that have already been initialized (either explicitly or by default) are finalized in the usual way.

For a Finalize invoked as part of an [assignment_statement](S0130), Program_Error is raised at that point.

For an Adjust invoked as part of an assignment operation, any other adjustments due to be performed are performed, and then Program_Error is raised. 

For a Finalize invoked as part of a call on an instance of Unchecked_Deallocation, any other finalizations due to be performed are performed, and then Program_Error is raised. 

For a Finalize invoked by the transfer of control of an exit_, return_, goto_, or [requeue_statement](S0195), Program_Error is raised no earlier than after the finalization of the master being finalized when the exception occurred, and no later than the point where normal execution would have continued. Any other finalizations due to be performed up to that point are performed before raising Program_Error. 

Ramification: For example, upon leaving a [block_statement](S0138) due to a [goto_statement](S0140), the Program_Error would be raised at the point of the target statement denoted by the label, or else in some more dynamically nested place, but not so nested as to allow an [exception_handler](S0232) that has visibility upon the finalized object to handle it. For example, 

```ada
procedure Main is
begin
    &lt&ltThe_Label&gt&gt
    Outer_Block_Statement : declare
        X : Some_Controlled_Type;
    begin
        Inner_Block_Statement : declare
            Y : Some_Controlled_Type;
            Z : Some_Controlled_Type;
        begin
            goto The_Label;
        exception
            when Program_Error =&gt ... -- Handler number 1.
        end;
    exception
        when Program_Error =&gt ... -- Handler number 2.
    end;
exception
    when Program_Error =&gt ... -- Handler number 3.
end Main;

```

The [goto_statement](S0140) will first cause Finalize(Y) to be called. Suppose that Finalize(Y) propagates an exception. Program_Error will be raised after leaving Inner_Block_Statement, but before leaving Main. Thus, handler number 1 cannot handle this Program_Error; it will be handled either by handler number 2 or handler number 3. If it is handled by handler number 2, then Finalize(Z) will be done before executing the handler. If it is handled by handler number 3, then Finalize(Z) and Finalize(X) will both be done before executing the handler. 

For a Finalize invoked by a transfer of control that is due to raising an exception, any other finalizations due to be performed for the same master are performed; Program_Error is raised immediately after leaving the master. 

Ramification: If, in the above example, the [goto_statement](S0140) were replaced by a [raise_statement](S0235), then the Program_Error would be handled by handler number 2, and Finalize(Z) would be done before executing the handler. 

Reason: We considered treating this case in the same way as the others, but that would render certain [exception_handler](S0232)s useless. For example, suppose the only [exception_handler](S0232) is one for others in the main subprogram. If some deeply nested call raises an exception, causing some Finalize operation to be called, which then raises an exception, then normal execution "would have continued" at the beginning of the [exception_handler](S0232). Raising Program_Error at that point would cause that handler's code to be skipped. One would need two nested [exception_handler](S0232)s to be sure of catching such cases!

On the other hand, the [exception_handler](S0232) for a given master should not be allowed to handle exceptions raised during finalization of that master. 

For a Finalize invoked by a transfer of control due to an abort or selection of a terminate alternative, the exception is ignored; any other finalizations due to be performed are performed. 

Ramification: This case includes an asynchronous transfer of control. 

To be honest: This violates the general principle that it is always possible for a bounded error to raise Program_Error (see ). 

NOTE 1   The rules of Section 10 imply that immediately prior to partition termination, Finalize operations are applied to library-level controlled objects (including those created by [allocator](S0122)s of library-level access types, except those already finalized). This occurs after waiting for library-level tasks to terminate. 

Discussion: We considered defining a pragma that would apply to a controlled type that would suppress Finalize operations for library-level objects of the type upon partition termination. This would be useful for types whose finalization actions consist of simply reclaiming global heap storage, when this is already provided automatically by the environment upon program termination. 

NOTE 2   A constant is only constant between its initialization and finalization. Both initialization and finalization are allowed to change the value of a constant.

NOTE 3   Abort is deferred during certain operations related to controlled types, as explained in 9.8. Those rules prevent an abort from causing a controlled object to be left in an ill-defined state.

NOTE 4   The Finalize procedure is called upon finalization of a controlled object, even if Finalize was called earlier, either explicitly or as part of an assignment; hence, if a controlled type is visibly controlled (implying that its Finalize primitive is directly callable), or is nonlimited (implying that assignment is allowed), its Finalize procedure should be designed to have no ill effect if it is applied a second time to the same object. 

Discussion: Or equivalently, a Finalize procedure should be "idempotent"; applying it twice to the same object should be equivalent to applying it once. 

Reason: A user-written Finalize procedure should be idempotent since it can be called explicitly by a client (at least if the type is "visibly" controlled). Also, Finalize is used implicitly as part of the [assignment_statement](S0130) if the type is nonlimited, and an abort is permitted to disrupt an [assignment_statement](S0130) between finalizing the left-hand side and assigning the new value to it (an abort is not permitted to disrupt an assignment operation between copying in the new value and adjusting it). 

Discussion: Either Initialize or Adjust, but not both, is applied to (almost) every controlled object when it is created: Initialize is done when no initial value is assigned to the object, whereas Adjust is done as part of assigning the initial value. The one exception is the anonymous object created by an [aggregate](S0097); Initialize is not applied to the [aggregate](S0097) as a whole, nor is the value of the [aggregate](S0097) adjusted.

All of the following use the assignment operation, and thus perform value adjustment: 

the [assignment_statement](S0130) (see 5.2);

explicit initialization of a stand-alone object (see 3.3.1) or of a pool element (see 4.8);

default initialization of a component of a stand-alone object or pool element (in this case, the value of each component is assigned, and therefore adjusted, but the value of the object as a whole is not adjusted);

function return, when the result type is not a return-by-reference type (see 6.5); (adjustment of the result happens before finalization of the function; values of return-by-reference types are not adjusted);

predefined operators (although the only one that matters is concatenation; see 4.5.3);

generic formal objects of mode in (see 12.4); these are defined in terms of constant declarations; and

[aggregate](S0097)s (see 4.3) (in this case, the value of each component, and the parent part, for an [extension_aggregate](S0102), is assigned, and therefore adjusted, but the value of the [aggregate](S0097) as a whole is not adjusted; neither is Initialize called); 

The following also use the assignment operation, but adjustment never does anything interesting in these cases: 

By-copy parameter passing uses the assignment operation (see 6.4.1), but controlled objects are always passed by reference, so the assignment operation never does anything interesting in this case. If we were to allow by-copy parameter passing for controlled objects, we would need to make sure that the actual is finalized before doing the copy back for [in] out parameters. The finalization of the parameter itself needs to happen after the copy back (if any), similar to the finalization of an anonymous function return object or [aggregate](S0097) object.

For loops use the assignment operation (see 5.5), but since the type of the loop parameter is never controlled, nothing interesting happens there, either.

Because Controlled and Limited_Controlled are library-level tagged types, all controlled types will be library-level types, because of the accessibility rules (see 3.10.2 and 3.9.1). This ensures that the Finalize operations may be applied without providing any "display" or "static-link". This simplifies finalization as a result of garbage collection, abort, and asynchronous transfer of control.

Finalization of the parts of a protected object are not done as protected actions. It is possible (in pathological cases) to create tasks during finalization that access these parts in parallel with the finalization itself. This is an erroneous use of shared variables. 

Implementation Note: One implementation technique for finalization is to chain the controlled objects together on a per-task list. When leaving a master, the list can be walked up to a marked place. The links needed to implement the list can be declared (privately) in types Controlled and Limited_Controlled, so they will be inherited by all controlled types.

Another implementation technique, which we refer to as the "PC-map" approach essentially implies inserting exception handlers at various places, and finalizing objects based on where the exception was raised.

The PC-map approach is for the compiler/linker to create a map of code addresses; when an exception is raised, or abort occurs, the map can be consulted to see where the task was executing, and what finalization needs to be performed. This approach was given in the Ada 83 Rationale as a possible implementation strategy for exception handling - the map is consulted to determine which exception handler applies.

If the PC-map approach is used, the implementation must take care in the case of arrays. The generated code will generally contain a loop to initialize an array. If an exception is raised part way through the array, the components that have been initialized must be finalized, and the others must not be finalized.

It is our intention that both of these implementation methods should be possible. 


#### Wording Changes from Ada 83

Finalization depends on the concepts of completion and leaving, and on the concept of a master. Therefore, we have moved the definitions of these concepts here, from where they used to be in Section 9. These concepts also needed to be generalized somewhat. Task waiting is closely related to user-defined finalization; the rules here refer to the task-waiting rules of Section 9. 

