---
sidebar_position:  51
---

# 6.3  Subprogram Bodies

[A [subprogram_body](./AA-6.3#S0216) specifies the execution of a subprogram.] 


#### Syntax

{AI95-00218-03} {AI05-0183-1} subprogram_body<a id="S0216"></a> ::= 
    [[overriding_indicator](./AA-8.3#S0234)]
    [subprogram_specification](./AA-6.1#S0196)
       [[aspect_specification](./AA-13.1#S0346)] is
       [declarative_part](./AA-3.11#S0086)
    begin
        [handled_sequence_of_statements](./AA-11.2#S0304)
    end [[designator](./AA-6.1#S0199)];

If a [designator](./AA-6.1#S0199) appears at the end of a [subprogram_body](./AA-6.3#S0216), it shall repeat the [defining_designator](./AA-6.1#S0200) of the [subprogram_specification](./AA-6.1#S0196). 


#### Legality Rules

{AI12-0444-1} [In contrast to other bodies,] a [subprogram_body](./AA-6.3#S0216) is allowed to be defined without it being the completion of a previous declaration[, in which case the body declares the subprogram]. If the body is a completion, it shall be the completion of a [subprogram_declaration](./AA-6.1#S0195) or [generic_subprogram_declaration](./AA-12.1#S0311). The profile of a [subprogram_body](./AA-6.3#S0216) that completes a declaration shall conform fully to that of the declaration. 


#### Static Semantics

A [subprogram_body](./AA-6.3#S0216) is considered a declaration. It can either complete a previous declaration, or itself be the initial declaration of the subprogram. 


#### Dynamic Semantics

The elaboration of a nongeneric [subprogram_body](./AA-6.3#S0216) has no other effect than to establish that the subprogram can from then on be called without failing the Elaboration_Check. 

Ramification: See 12.2 for elaboration of a generic body. Note that protected subprogram_bodies never get elaborated; the elaboration of the containing [protected_body](./AA-9.4#S0254) allows them to be called without failing the Elaboration_Check. 

[The execution of a [subprogram_body](./AA-6.3#S0216) is invoked by a subprogram call.] For this execution the [declarative_part](./AA-3.11#S0086) is elaborated, and the [handled_sequence_of_statements](./AA-11.2#S0304) is then executed. 


#### Examples

Example of procedure body: 

```ada
procedure Push(E : in Element_Type; S : in out Stack) is
begin
   if S.Index = S.Size then
      raise Stack_Overflow;
   else
      S.Index := S.Index + 1;
      S.Space(S.Index) := E;
   end if;
end Push;

```

Example of a function body: 

```ada
function Dot_Product(Left, Right : Vector) return Real is
   Sum : Real := 0.0;
begin
   Check(Left'First = Right'First and Left'Last = Right'Last);
   for J in Left'Range loop
      Sum := Sum + Left(J)*Right(J);
   end loop;
   return Sum;
end Dot_Product;

```


#### Extensions to Ada 83

A [renaming_declaration](./AA-8.5#S0238) may be used instead of a [subprogram_body](./AA-6.3#S0216). 


#### Wording Changes from Ada 83

The syntax rule for [subprogram_body](./AA-6.3#S0216) now uses the syntactic category [handled_sequence_of_statements](./AA-11.2#S0304).

The [declarative_part](./AA-3.11#S0086) of a [subprogram_body](./AA-6.3#S0216) is now required; that doesn't make any real difference, because a [declarative_part](./AA-3.11#S0086) can be empty.

We have incorporated some rules from RM83-6.5 here.

RM83 forgot to restrict the definition of elaboration of a [subprogram_body](./AA-6.3#S0216) to nongenerics. 


#### Wording Changes from Ada 95

{AI95-00218-03} [Overriding_indicator](./AA-8.3#S0234) is added to [subprogram_body](./AA-6.3#S0216). 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [subprogram_body](./AA-6.3#S0216). This is described in 13.1.1. 


## 6.3.1  Conformance Rules

[When subprogram profiles are given in more than one place, they are required to conform in one of four ways: type conformance, mode conformance, subtype conformance, or full conformance.] 


#### Static Semantics

{8652/0011} {AI95-00117-01} [As explained in B.1, "Interfacing Aspects", a convention can be specified for an entity.] Unless this document states otherwise, the default convention of an entity is Ada. [For a callable entity or access-to-subprogram type, the convention is called the calling convention.] The following conventions are defined by the language: 

{AI05-0229-1} The default calling convention for any subprogram not listed below is Ada. [The Convention aspect may be specified to override the default calling convention (see B.1)]. 

Ramification: See also the rule about renamings-as-body in 8.5.4. 

The Intrinsic calling convention represents subprograms that are "built in" to the compiler. The default calling convention is Intrinsic for the following: 

an enumeration literal;

a "/=" operator declared implicitly due to the declaration of "=" (see 6.6);

any other implicitly declared subprogram unless it is a dispatching operation of a tagged type;

an inherited subprogram of a generic formal tagged type with unknown discriminants; 

Reason: Consider: 

```ada
package P is
    type Root is tagged null record;
    procedure Proc(X: Root);
end P;

```

```ada
generic
    type Formal(&lt&gt) is new Root with private;
package G is
    ...
end G;

```

```ada
package body G is
    ...
    X: Formal := ...;
    ...
    Proc(X); -- This is a dispatching call in Instance, because
             -- the actual type for Formal is class-wide.
    ...
    -- Proc'Access would be illegal here, because it is of
    -- convention Intrinsic, by the above rule.
end G;

```

```ada
type Actual is new Root with ...;
procedure Proc(X: Actual);
package Instance is new G(Formal =&gt Actual'Class);
    -- It is legal to pass in a class-wide actual, because Formal
    -- has unknown discriminants.

```

Within Instance, all calls to Proc will be dispatching calls, so Proc doesn't really exist in machine code, so we wish to avoid taking 'Access of it. This rule applies to those cases where the actual type might be class-wide, and makes these Intrinsic, thus forbidding 'Access. 

an attribute that is a subprogram;

{AI95-00252-01} a subprogram declared immediately within a [protected_body](./AA-9.4#S0254);

{AI95-00252-01} {AI95-00407-01} {AI12-0107-1} any prefixed view of a subprogram (see 4.1.3) without synchronization kind (see 9.5) By_Entry or By_Protected_Procedure. 

Reason: {AI12-0005-1} The profile of a prefixed view is different from the "real" profile of the subprogram (it doesn't have the first parameter), so we don't want to be able to take 'Access of it, as that would require generating a wrapper of some sort.

{AI12-0107-1} We except prefixed views that have synchronization kind By_Protected_Procedure so that they can be used with an access-to-protected-procedure type. These don't require special wrappers (this is the normal form for a protected subprogram call). The By_Entry part is just for consistency (there is no access-to-entry type in Ada). 

[The Access attribute is not allowed for Intrinsic subprograms.] 

Ramification: The Intrinsic calling convention really represents any number of calling conventions at the machine code level; the compiler might have a different instruction sequence for each intrinsic. That's why the Access attribute is disallowed. We do not wish to require the implementation to generate an out of line body for an intrinsic.

{AI05-0229-1} Whenever we wish to disallow the Access attribute in order to ease implementation, we make the subprogram Intrinsic. Several language-defined subprograms have "with Convention =&gt Intrinsic;". An implementation might actually implement this as "with Import =&gt True, Convention =&gt Intrinsic;", if there is really no body, and the implementation of the subprogram is built into the code generator.

Subprograms declared in protected_bodies will generally have a special calling convention so as to pass along the identification of the current instance of the protected type. The convention is not protected since such local subprograms need not contain any "locking" logic since they are not callable via "external" calls; this rule prevents an access value designating such a subprogram from being passed outside the protected unit.

The "implicitly declared subprogram" above refers to predefined operators (other than the "=" of a tagged type) and the inherited subprograms of untagged types. 

{AI12-0107-1} {AI12-0159-1} The default calling convention is protected for a protected subprogram, for a prefixed view of a subprogram with a synchronization kind of By_Protected_Procedure, and for an access-to-subprogram type with the reserved word protected in its definition.

{AI12-0107-1} {AI12-0159-1} The default calling convention is entry for an entry and for a prefixed view of a subprogram with a synchronization kind of By_Entry.

{AI95-00254-01} {AI95-00409-01} {AI05-0264-1} {AI12-0207-1} The calling convention for an anonymous access-to-subprogram parameter or anonymous access-to-subprogram result is protected if the reserved word protected appears in its definition; otherwise, it is the convention of the entity that has the parameter or result, unless that entity has convention protected, entry, or Intrinsic, in which case the convention is Ada. 

Ramification: The calling convention for other anonymous access-to-subprogram types is Ada. 

{8652/0011} {AI95-00117-01} [If not specified above as Intrinsic, the calling convention for any inherited or overriding dispatching operation of a tagged type is that of the corresponding subprogram of the parent type.] The default calling convention for a new dispatching operation of a tagged type is the convention of the type. 

Reason: The first rule is officially stated in 3.9.2. The second is intended to make interfacing to foreign OOP languages easier, by making the default be that the type and operations all have the same convention. 

{AI05-0229-1} Of these four conventions, only Ada and Intrinsic are allowed as a convention_[identifier](./AA-2.3#S0002) in the specification of a Convention aspect. 

Discussion: {AI05-0229-1} The names of the protected and entry calling conventions cannot be used in the specification of Convention. Note that protected and entry are reserved words. 

{AI95-00409-01} Two profiles are type conformant if they have the same number of parameters, and both have a result if either does, and corresponding parameter and result types are the same, or, for access parameters or access results, corresponding designated types are the same, or corresponding designated profiles are type conformant. 

Discussion: {AI95-00409-01} For anonymous access-to-object parameters, the designated types have to be the same for type conformance, not the access types, since in general each access parameter has its own anonymous access type, created when the subprogram is called. Of course, corresponding parameters have to be either both access parameters or both not access parameters.

{AI95-00409-01} Similarly, for anonymous access-to-subprogram parameters, the designated profiles of the types, not the types themselves, have to be conformant. 

{AI95-00318-02} {AI95-00409-01} {AI05-0142-4} Two profiles are mode conformant if:

{AI05-0142-4} {AI05-0262-1} they are type conformant; and

{AI05-0142-4} corresponding parameters have identical modes and both or neither are explicitly aliased parameters; and

{AI05-0207-1} for corresponding access parameters and any access result type, the designated subtypes statically match and either both or neither are access-to-constant, or the designated profiles are subtype conformant. 

{AI05-0239-1} Two profiles are subtype conformant if they are mode conformant, corresponding subtypes of the profile statically match, and the associated calling conventions are the same. The profile of a generic formal subprogram is not subtype conformant with any other profile. 

Ramification: 

{AI05-0134-1} {AI05-0262-1} Two profiles are fully conformant if they are subtype conformant, if they have access-to-subprogram results whose designated profiles are fully conformant, and for corresponding parameters: 

{AI05-0262-1} they have the same names; and

{AI05-0046-1} both or neither have [null_exclusion](./AA-3.10#S0083)s; and

neither have [default_expression](./AA-3.7#S0063)s, or they both have [default_expression](./AA-3.7#S0063)s that are fully conformant with one another; and

{AI05-0134-1} for access-to-subprogram parameters, the designated profiles are fully conformant. 

Ramification: Full conformance requires subtype conformance, which requires the same calling conventions. However, the calling convention of the declaration and body of a subprogram or entry are always the same by definition. 

Reason: {AI05-0046-1} The part about [null_exclusion](./AA-3.10#S0083)s is necessary to prevent controlling parameters from having different exclusions, as such a parameter is defined to exclude null whether or not an exclusion is given.

{AI05-0134-1} The parts about access-to-subprogram parameters and results is necessary to prevent such types from having different [default_expression](./AA-3.7#S0063)s in the specification and body of a subprogram. If that was allowed, it would be undefined which [default_expression](./AA-3.7#S0063) was used in a call of an access-to-subprogram parameter. 

Two expressions are fully conformant if, [after replacing each use of an operator with the equivalent [function_call](./AA-6.4#S0218):] 

each constituent construct of one corresponds to an instance of the same syntactic category in the other, except that an expanded name may correspond to a [direct_name](./AA-4.1#S0092) (or [character_literal](./AA-2.5#S0015)) or to a different expanded name in the other; and

{AI12-0050-1} corresponding [defining_identifier](./AA-3.1#S0022)s occurring within the two expressions are the same; and

{AI12-0050-1} each [direct_name](./AA-4.1#S0092), [character_literal](./AA-2.5#S0015), and [selector_name](./AA-4.1#S0099) that is not part of the [prefix](./AA-4.1#S0093) of an expanded name in one denotes the same declaration as the corresponding [direct_name](./AA-4.1#S0092), [character_literal](./AA-2.5#S0015), or [selector_name](./AA-4.1#S0099) in the other, or they denote corresponding declarations occurring within the two expressions; and 

Ramification: {AI12-0300-1} Note that it doesn't say "respectively" because a [direct_name](./AA-4.1#S0092) can correspond to a [selector_name](./AA-4.1#S0099), and vice versa, by the previous bullet. This rule allows the [prefix](./AA-4.1#S0093) of an expanded name to be removed, or replaced with a different [prefix](./AA-4.1#S0093) that denotes a renaming of the same entity. However, it does not allow a [direct_name](./AA-4.1#S0092) or [selector_name](./AA-4.1#S0099) to be replaced with one denoting a distinct renaming (except for [direct_name](./AA-4.1#S0092)s and [selector_name](./AA-4.1#S0099)s in [prefix](./AA-4.1#S0093)es of expanded names). Note that calls using operator notation are equivalent to calls using prefix notation.

Given the following declarations: 

```ada
package A is
    function F(X : Integer := 1) return Boolean;
end A;

```

```ada
{AI05-0005-1} with A;
package B is
    package A_View renames A;
    function F_View(X : Integer := 9999) return Boolean renames A.F;
end B;

```

```ada
with A, B; use A, B;
procedure Main is ...

```

Within Main, the expressions "F", "A.F", "B.A_View.F", and "A_View.F" are all fully conformant with one another. However, "F" and "F_View" are not fully conformant. If they were, it would be bad news, since the two denoted views have different [default_expression](./AA-3.7#S0063)s. 

Discussion: {AI12-0050-1} We talk about [defining_identifier](./AA-3.1#S0022)s and "corresponding declarations" because of the possibility of [iterator_specification](./AA-5.5#S0183)s occurring within the expressions; each [iterator_specification](./AA-5.5#S0183) is a separate declaration, which we need to allow, but we do want to require that the [defining_identifier](./AA-3.1#S0022)s are the same. 

{8652/0018} {AI95-00175-01} {AI05-0092-1} each [attribute_designator](./AA-4.1#S0101) in one is the same as the corresponding [attribute_designator](./AA-4.1#S0101) in the other; and

{AI12-0342-1} each [primary](./AA-4.4#S0141) that is a literal in one is a user-defined literal if and only if the corresponding literal in the other is also a user-defined literal. Furthermore, if neither are user-defined literals then they shall have the same values[, but they may have differing textual representations]; if both are user-defined literals then they shall have the same textual representation.

Ramification: {AI12-0342-1} This rule applies to [character_literal](./AA-2.5#S0015)s, so even though other rules would allow a rename of a character literal to conform to the literal, this rule prevents that. 

Ramification: Note that the above definition makes full conformance a transitive relation. 

Two [known_discriminant_part](./AA-3.7#S0061)s are fully conformant if they have the same number of discriminants, and discriminants in the same positions have the same names, statically matching subtypes, and [default_expression](./AA-3.7#S0063)s that are fully conformant with one another. 

Two [discrete_subtype_definition](./AA-3.6#S0055)s are fully conformant if they are both [subtype_indication](./AA-3.2#S0027)s or are both [range](./AA-3.5#S0037)s, the [subtype_mark](./AA-3.2#S0028)s (if any) denote the same subtype, and the corresponding [simple_expression](./AA-4.4#S0138)s of the [range](./AA-3.5#S0037)s (if any) fully conform. 

Ramification: In the [subtype_indication](./AA-3.2#S0027) case, any ranges have to be corresponding; that is, two [subtype_indication](./AA-3.2#S0027)s cannot conform unless both or neither has a [range](./AA-3.5#S0037). 

Discussion: This definition is used in 9.5.2, "Entries and Accept Statements" for the conformance required between the [discrete_subtype_definition](./AA-3.6#S0055)s of an [entry_declaration](./AA-9.5#S0257) for a family of entries and the corresponding [entry_index_specification](./AA-9.5#S0263) of the [entry_body](./AA-9.5#S0260). 

{AI95-00345-01} {AI95-00397-01} The prefixed view profile of a subprogram is the profile obtained by omitting the first parameter of that subprogram. There is no prefixed view profile for a parameterless subprogram. For the purposes of defining subtype and mode conformance, the convention of a prefixed view profile is considered to match that of either an entry or a protected operation.

Discussion: This definition is used to define how primitive subprograms of interfaces match operations in task and protected type definitions (see 9.1 and 9.4). 

Reason: The weird rule about conventions is pretty much required for synchronized interfaces to make any sense. There will be wrappers all over the place for interfaces anyway. Of course, this doesn't imply that entries have the same convention as protected operations. 


#### Implementation Permissions

An implementation may declare an operator declared in a language-defined library unit to be intrinsic. 

NOTE 1   {AI12-0398-1} Any conformance requirements between [aspect_specification](./AA-13.1#S0346)s that are part of a profile or [known_discriminant_part](./AA-3.7#S0061) are defined by the semantics of each particular aspect. In particular, there is no general requirement for [aspect_specification](./AA-13.1#S0346)s to match in conforming profiles or discriminant parts. 


#### Extensions to Ada 83

The rules for full conformance are relaxed - they are now based on the structure of constructs, rather than the sequence of lexical elements. This implies, for example, that "(X, Y: T)" conforms fully with "(X: T; Y: T)", and "(X: T)" conforms fully with "(X: in T)". 


#### Wording Changes from Ada 95

{8652/0011} {AI95-00117-01} Corrigendum: Clarified that the default convention is Ada. Also clarified that the convention of a primitive operation of a tagged type is the same as that of the type.

{8652/0018} {AI95-00175-01} Corrigendum: Added wording to ensure that two attributes conform only if they have the same [attribute_designator](./AA-4.1#S0101).

{AI95-00252-01} {AI95-00254-01} {AI95-00407-01} Defined the calling convention for anonymous access-to-subprogram types and for prefixed views of subprograms (see 4.1.3).

{AI95-00318-02} Defined the conformance of access result types (see 6.1).

{AI95-00345-01} {AI95-00397-01} Defined the prefixed view profile of subprograms for later use.

{AI95-00409-01} Defined the conformance of anonymous access-to-subprogram parameters. 


#### Incompatibilities With Ada 2005

{AI05-0046-1} Correction: Now require [null_exclusion](./AA-3.10#S0083)s to match for full conformance. While this is technically incompatible with Ada 2005 as defined by Amendment 1, it is a new Ada 2005 feature and it is unlikely that users have been intentionally taking advantage of the ability to write mismatching exclusions. In any case, it is easy to fix: add a [null_exclusion](./AA-3.10#S0083) where needed for conformance.

{AI05-0134-1} Correction: Now require full conformance of anonymous access-to-subprogram parameters and results for full conformance. This is necessary so that there is no confusion about the default expression that is used for a call. While this is technically incompatible with Ada 2005 as defined by Amendment 1, it is a new Ada 2005 feature and it is unlikely that users have been intentionally taking advantage and writing different default expressions. In any case, it is easy to fix: change any default expressions that don't conform so that they do conform.

{AI05-0207-1} Correction: Now include the presence or absence of constant in access parameters to be considered when checking mode conformance. This is necessary to prevent modification of constants. While this is technically incompatible with Ada 2005 as defined by Amendment 1, it is a new Ada 2005 feature and it is unlikely that users have been intentionally taking advantage and writing mismatching access types. 


#### Wording Changes from Ada 2005

{AI05-0142-4} Explicitly aliased parameters are included as part of mode conformance (since it affects the parameter passing mechanism). 


#### Incompatibilities With Ada 2012

{AI12-0207-1} Correction: The convention of an anonymous access-to-subprogram parameter of a protected entry or subprogram is Ada; if one wants it to be protected it can be declared with the keyword protected. This is incompatible, but only in a very rare case; usually the intent is to pass a normal subprogram to a protected subprogram (and this was impossible in Ada 2012). 


#### Extensions to Ada 2012

{AI12-0107-1} {AI12-0159-1} Corrigendum: We now define that a prefixed view of a subprogram with synchronization kind By_Protected_Procedure can be used as the prefix of 'Access for an access-to-protected type. We consider this a correction as it certainly appears that it ought to work, but in original Ada 2012 it would have had a convention mismatch. 


#### Wording Changes from Ada 2012

{AI12-0050-1} Corrigendum: We now define how two expressions containing quantified expressions can fully conform. This isn't incompatible, as the original Ada 2012 never allowed such expressions to conform (the declarations in each formally being different). Neither is it an extension as one would expect these to conform.

{AI12-0342-1} The conformance of literals is tightened up to not assume anything about the value of a user-defined literal. 


## 6.3.2  Inline Expansion of Subprograms

[Subprograms may be expanded in line at the call site.] 

Paragraphs 2 through 4 were moved to Annex J, "Obsolescent Features". 


#### Static Semantics

{AI05-0229-1} For a callable entity or a generic subprogram, the following language-defined representation aspect may be specified:

InlineThe type of aspect Inline is Boolean. When aspect Inline is True for a callable entity, inline expansion is desired for all calls to that entity. When aspect Inline is True for a generic subprogram, inline expansion is desired for all calls to all instances of that generic subprogram.

If directly specified, the [aspect_definition](./AA-13.1#S0348) shall be a static expression. [This aspect is never inherited;] if not directly specified, the aspect is False.

Aspect Description for Inline: For efficiency, Inline calls are requested for a subprogram.

This paragraph was deleted.{AI05-0229-1} 

```ada
This paragraph was deleted.

```

```ada
This paragraph was deleted.

```

Ramification: {AI05-0229-1} The meaning of a subprogram can be changed by inline expansion as requested by aspect Inline only in the presence of failing checks (see 11.6). 


#### Implementation Permissions

{AI05-0229-1} For each call, an implementation is free to follow or to ignore the recommendation determined by the Inline aspect. 

Ramification: Note, in particular, that the recommendation cannot always be followed for a recursive call, and is often infeasible for entries. Note also that the implementation can inline calls even when no such desire was expressed via the Inline aspect, so long as the semantics of the program remains unchanged. 


#### Incompatibilities With Ada 83

This paragraph was deleted.{AI95-00309-01} {AI05-0229-1} 


#### Extensions to Ada 83

This paragraph was deleted.{AI05-0229-1} 


#### Extensions to Ada 95

This paragraph was deleted.{AI95-00309-01} {AI05-0229-1} 


#### Extensions to Ada 2005

{AI05-0229-1} Aspect Inline is new; [pragma](./AA-2.8#S0019) Inline is now obsolescent. 

