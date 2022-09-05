---
sidebar_position:  69
---

# 8.5  Renaming Declarations

[A [renaming_declaration](./AA-8.5#S0238) declares another name for an entity, such as an object, exception, package, subprogram, entry, or generic unit. Alternatively, a [subprogram_renaming_declaration](./AA-8.5#S0242) can be the completion of a previous [subprogram_declaration](./AA-6.1#S0195).]

Glossary entry: A [renaming_declaration](./AA-8.5#S0238) is a declaration that does not define a new entity, but instead defines a view of an existing entity.

Version=[5],Kind=(AddedNormal),Group=[C],Term=[renaming], Def=[a declaration that does not define a new entity, but instead defines a new view of an existing entity] 


#### Syntax

renaming_declaration<a id="S0238"></a> ::= 
      [object_renaming_declaration](./AA-8.5#S0239)
    | [exception_renaming_declaration](./AA-8.5#S0240)
    | [package_renaming_declaration](./AA-8.5#S0241)
    | [subprogram_renaming_declaration](./AA-8.5#S0242)
    | [generic_renaming_declaration](./AA-8.5#S0243)


#### Dynamic Semantics

The elaboration of a [renaming_declaration](./AA-8.5#S0238) evaluates the [name](./AA-4.1#S0091) that follows the reserved word renames and thereby determines the view and entity denoted by this name (the renamed view and renamed entity). [A [name](./AA-4.1#S0091) that denotes the [renaming_declaration](./AA-8.5#S0238) denotes (a new view of) the renamed entity.] 

NOTE 1   {AI12-0442-1} Renaming can be used to resolve name conflicts and to act as a shorthand. Renaming with a different [identifier](./AA-2.3#S0002) or [operator_symbol](./AA-6.1#S0202) does not hide the old [name](./AA-4.1#S0091); the new [name](./AA-4.1#S0091) and the old [name](./AA-4.1#S0091) can be visible at different places.

This paragraph was deleted.{AI12-0427-1} 

NOTE 2   A subtype defined without any additional constraint can be used to achieve the effect of renaming another subtype (including a task or protected subtype) as in 

```ada
   subtype Mode is Ada.Text_IO.File_Mode;

```


#### Wording Changes from Ada 83

The second sentence of RM83-8.5(3), "At any point where a renaming declaration is visible, the identifier, or operator symbol of this declaration denotes the renamed entity." is incorrect. It doesn't say directly visible. Also, such an [identifier](./AA-2.3#S0002) might resolve to something else.

The verbiage about renamings being legal "only if exactly one...", which appears in RM83-8.5(4) (for objects) and RM83-8.5(7) (for subprograms) is removed, because it follows from the normal rules about overload resolution. For language lawyers, these facts are obvious; for programmers, they are irrelevant, since failing these tests is highly unlikely. 


## 8.5.1  Object Renaming Declarations

{AI12-0383-1} [An [object_renaming_declaration](./AA-8.5#S0239) is used to rename an object or value.] 


#### Syntax

{AI95-00230-01} {AI95-00423-01} {AI05-0183-1} {AI12-0275-1} object_renaming_declaration<a id="S0239"></a> ::= 
    [defining_identifier](./AA-3.1#S0022) [: [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028)] renames object_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];
  | [defining_identifier](./AA-3.1#S0022) : [access_definition](./AA-3.10#S0084) renames object_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];


#### Name Resolution Rules

{AI95-00230-01} {AI95-00254-01} {AI95-00409-01} {AI12-0275-1} The type of the object_[name](./AA-4.1#S0091) shall resolve to the type determined by the [subtype_mark](./AA-3.2#S0028), if present. If no [subtype_mark](./AA-3.2#S0028) or [access_definition](./AA-3.10#S0084) is present, the expected type of the object_[name](./AA-4.1#S0091) is any type.

{AI12-0275-1} In the case where the type is defined by an [access_definition](./AA-3.10#S0084), the type of the object_[name](./AA-4.1#S0091) shall resolve to an anonymous access type. If the anonymous access type is an access-to-object type, the type of the object_[name](./AA-4.1#S0091) shall have the same designated type as that of the [access_definition](./AA-3.10#S0084). If the anonymous access type is an access-to-subprogram type, the type of the object_[name](./AA-4.1#S0091) shall have a designated profile that is type conformant with that of the [access_definition](./AA-3.10#S0084).

Reason: A previous version of Ada 9X used the usual "expected type" wording:
"The expected type for the object_[name](./AA-4.1#S0091) is that determined by the [subtype_mark](./AA-3.2#S0028)."
We changed it so that this would be illegal: 

```ada
X: T;
Y: T'Class renames X; -- Illegal!

```

When the above was legal, it was unclear whether Y was of type T or T'Class. Note that we still allow this: 

```ada
Z: T'Class := ...;
W: T renames F(Z);

```

where F is a function with a controlling parameter and result. This is admittedly a bit odd.

Note that the matching rule for generic formal parameters of mode in out was changed to keep it consistent with the rule for renaming. That makes the rule different for in vs. in out.


#### Legality Rules

{AI12-0383-1} The renamed entity shall be an object or value.

{AI95-00231-01} {AI95-00409-01} {AI12-0383-1} In the case where the type is defined by an [access_definition](./AA-3.10#S0084), the type of the renamed entity and the type defined by the [access_definition](./AA-3.10#S0084): 

{AI95-00231-01} {AI95-00409-01} shall both be access-to-object types with statically matching designated subtypes and with both or neither being access-to-constant types; or 

{AI95-00409-01} shall both be access-to-subprogram types with subtype conformant designated profiles. 

{AI95-00423-01} {AI12-0287-1} For an [object_renaming_declaration](./AA-8.5#S0239) with a [null_exclusion](./AA-3.10#S0083) or an [access_definition](./AA-3.10#S0084) that has a [null_exclusion](./AA-3.10#S0083), the subtype of the object_[name](./AA-4.1#S0091) shall exclude null. In addition, if the [object_renaming_declaration](./AA-8.5#S0239) occurs within the body of a generic unit G or within the body of a generic unit declared within the declarative region of generic unit G, then:

{AI12-0287-1} if the object_[name](./AA-4.1#S0091) statically denotes a generic formal object of mode in out of G, then the declaration of that object shall have a [null_exclusion](./AA-3.10#S0083);

{AI12-0287-1} if the object_[name](./AA-4.1#S0091) statically denotes a call of a generic formal function of G, then the declaration of the result of that function shall have a [null_exclusion](./AA-3.10#S0083). 

Reason: {AI12-0287-1} {AI12-0005-1} These rules prevent "lying". Null must never be the value of an object with an explicit [null_exclusion](./AA-3.10#S0083). The bullets are assume-the-worst rules that prevent trouble in two obscure cases: 

```ada
type Acc_I is access Integer;
subtype Acc_NN_I is not null Acc_I;
Obj : Acc_I := null;

```

```ada
generic
   B : in out Acc_NN_I;
package Gen is
   ...
end Gen;

```

```ada
package body Gen is
   D : not null Acc_I renames B;
end Gen;

```

```ada
package Inst is new Gen (B =&gt Obj);

```

{AI12-0287-1} Without the first bullet rule, D would be legal, and contain the value null, because the rule about lying is satisfied for generic matching (Obj matches B; B does not explicitly state not null), Legality Rules are not rechecked in the body of any instance, and the template passes the lying rule as well. The second bullet handles a similar case involving formal functions. The rules are so complex because they have to apply to formals used in bodies of child generics as well as in the bodies of generics. 

{AI12-0401-1} In the case where the object_[name](./AA-4.1#S0091) is a [qualified_expression](./AA-4.7#S0163) with a nominal subtype S and whose [expression](./AA-4.4#S0132) is a [name](./AA-4.1#S0091) that denotes an object Q: 

if S is an elementary subtype, then: 

Q shall be a constant other than a dereference of an access type; or

the nominal subtype of Q shall be statically compatible with S; or

S shall statically match the base subtype of its type if scalar, or the first subtype of its type if an access type. 

if S is a composite subtype, then Q shall be known to be constrained or S shall statically match the first subtype of its type. 

Ramification: There's no restriction if the [expression](./AA-4.4#S0132) is a value. 

Reason: This check prevents the renamed object from violating its nominal subtype. As the subtype is only checked when the object is renamed, we make it illegal if the actual object is a variable whose value could be changed afterwards to violate the subtype. This is messy as "known to be constrained" is only defined for composite objects, so we have to handle elementary objects and all values separately. 

{8652/0017} {AI95-00184-01} {AI95-00363-01} {AI05-0008-1} {AI12-0401-1} The renamed entity shall not be a subcomponent that depends on discriminants of an object whose nominal subtype is unconstrained unless the object is known to be constrained. A [slice](./AA-4.1#S0097) of an array shall not be renamed if this restriction disallows renaming of the array. 

{AI12-0401-1} In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit.

Discussion: This applies to all of the Legality Rules in this subclause. Rechecks are needed for most of the rules (but not the first two). 

Reason: This prevents renaming of subcomponents that might disappear, which might leave dangling references. Similar restrictions exist for the Access attribute.

{8652/0017} {AI95-00184-01} {AI05-0008-1} The "recheck on instantiation" requirement on generics is necessary to avoid renaming of components which could disappear even when the nominal subtype would prevent the problem:

```ada
type T1 (D1 : Boolean) is
   record
      case D1 is
         when False =&gt
            C1 : Integer;
         when True =&gt
            null;
         end case;
      end record;

```

```ada
generic
   type F is new T1;
   X : in out F;
package G is
   C1_Ren : Integer renames X.C1;
end G;

```

```ada
type T2 (D2 : Boolean := False) is new T1 (D1 =&gt D2);

Y : T2;

package I is new G (T2, Y);

Y := (D1 =&gt True); -- Oops!  What happened to I.C1_Ren?

```

{AI05-0008-1} In addition, the "known to be constrained" rules include assume-the-worst rules for generic bodies partially to prevent such problems.

Implementation Note: Note that if an implementation chooses to deallocate-then-reallocate on [assignment_statement](./AA-5.2#S0173)s assigning to unconstrained definite objects, then it cannot represent renamings and access values as simple addresses, because the above rule does not apply to all components of such an object. 

Ramification: If it is a generic formal object, then the assume-the-best or assume-the-worst rules are applied as appropriate. 


#### Static Semantics

{AI95-00230-01} {AI95-00409-01} {AI12-0383-1} An [object_renaming_declaration](./AA-8.5#S0239) declares a new view [of the renamed entity] whose properties are identical to those of the renamed view. [Thus, the properties of the renamed entity are not affected by the [renaming_declaration](./AA-8.5#S0238). In particular, its nominal subtype, whether it is a value or an object, its value if it is an object, and whether or not it is a constant, are unaffected; similarly, the constraints and other properties of its nominal subtype are not affected by renaming (any constraint implied by the [subtype_mark](./AA-3.2#S0028) or [access_definition](./AA-3.10#S0084) of the [object_renaming_declaration](./AA-8.5#S0239) is ignored).] 

Discussion: Because the constraints are ignored, it is a good idea to use the nominal subtype of the renamed object when writing an [object_renaming_declaration](./AA-8.5#S0239).

{AI95-00409-01} If no [null_exclusion](./AA-3.10#S0083) is given in the renaming, the object may or may not exclude null. This is similar to the way that constraints need not match, and constant is not specified. The renaming defines a view of the renamed entity, inheriting the original properties. 


#### Examples

Example of renaming an object: 

```ada
declare
   L : Person renames Leftmost_Person; -- see 3.10.1
begin
   L.Age := L.Age + 1;
end;

```

{AI12-0383-1} Example of renaming a value:

```ada
{AI12-0383-1} Uno renames One;  -- see 3.3.2

```


#### Wording Changes from Ada 83

The phrase "subtype ... as defined in a corresponding object declaration, component declaration, or component subtype indication", from RM83-8.5(5), is incorrect in Ada 95; therefore we removed it. It is incorrect in the case of an object with an indefinite unconstrained nominal subtype. 


#### Incompatibilities With Ada 95

{AI95-00363-01} Aliased variables are not necessarily constrained in Ada 2005 (see 3.6). Therefore, a subcomponent of an aliased variable may disappear or change shape, and renaming such a subcomponent thus is illegal, while the same operation would have been legal in Ada 95. Note that most allocated objects are still constrained by their initial value (see 4.8), and thus have no change in the legality of renaming for them. For example, using the type T2 of the previous example: 

```ada
   AT2 : aliased T2;
   C1_Ren : Integer renames AT2.C1; -- Illegal in Ada 2005, legal in Ada 95
   AT2 := (D1 =&gt True);             -- Raised Constraint_Error in Ada 95,
                                    -- but does not in Ada 2005, so C1_Ren becomes
                                    -- invalid when this is assigned.

```


#### Extensions to Ada 95

{AI95-00230-01} {AI95-00231-01} {AI95-00254-01} {AI95-00409-01} A renaming can have an anonymous access type. In that case, the accessibility of the renaming is that of the original object (accessibility is not lost as it is for assignment to a component or stand-alone object).

{AI95-00231-01} {AI95-00423-01} A renaming can have a [null_exclusion](./AA-3.10#S0083); if so, the renamed object must also exclude null, so that the [null_exclusion](./AA-3.10#S0083) does not lie. On the other hand, if the renaming does not have a [null_exclusion](./AA-3.10#S0083). it excludes null if the renamed object does. 


#### Wording Changes from Ada 95

{8652/0017} {AI95-00184-01} Corrigendum: Fixed to forbid renamings of depends-on-discriminant components if the type might be definite. 


#### Incompatibilities With Ada 2005

{AI05-0008-1} Correction: Simplified the description of when a discriminant-dependent component is allowed to be renamed - it's now simply when the object is known to be constrained. This fixes a confusion as to whether a subcomponent of an object that is not certain to be constrained can be renamed. The fix introduces an incompatibility, as the rule did not apply in Ada 95 if the prefix was a constant; but it now applies no matter what kind of object is involved. The incompatibility is not too bad, since most kinds of constants are known to be constrained. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in an [object_renaming_declaration](./AA-8.5#S0239). This is described in 13.1.1. 


#### Incompatibilities With Ada 2012

{AI12-0287-1} Correction: The Legality Rules for renames with null exclusions no longer applies to generic formal objects of mode in, but does apply to renames of generic formal functions. This means a few unlikely programs are now illegal that were previously allowed by original Ada 2012, while more programs that were previously llegal will be allowed.

{AI12-0401-1} Correction: Added a rule to ensure that a renaming of a [qualified_expression](./AA-4.7#S0163) of a variable is allowed only if the variable will always remain within the nominal subtype of the [qualified_expression](./AA-4.7#S0163). This was not required in Ada 2012. Renamings that are now illegal are at risk of causing erroneous execution if the variable value is changed to a bad value; this is consistent with other rules preventing renamings from changing to violate their known properties. 


#### Extensions to Ada 2012

{AI12-0275-1} The [subtype_mark](./AA-3.2#S0028) in an object renaming is now optional, as the subtype information it provides is not trustworthy anyway (that comes from the renamed object and there is no requirement that it is the same as that of the object).

{AI12-0383-1} An object renaming can now rename values, such as named numbers. The renamed entity still has to be a [name](./AA-4.1#S0091), but an arbitrary [expression](./AA-4.4#S0132) can be renamed by qualifying it. 


## 8.5.2  Exception Renaming Declarations

[An [exception_renaming_declaration](./AA-8.5#S0240) is used to rename an exception.] 


#### Syntax

{AI05-0183-1} exception_renaming_declaration<a id="S0240"></a> ::= [defining_identifier](./AA-3.1#S0022) : exception renames exception_[name](./AA-4.1#S0091)
   [[aspect_specification](./AA-13.1#S0346)];


#### Legality Rules

The renamed entity shall be an exception. 


#### Static Semantics

An [exception_renaming_declaration](./AA-8.5#S0240) declares a new view [of the renamed exception]. 


#### Examples

Example of renaming an exception: 

```ada
EOF : exception renames Ada.IO_Exceptions.End_Error; -- see A.13

```


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in an [exception_renaming_declaration](./AA-8.5#S0240). This is described in 13.1.1. 


## 8.5.3  Package Renaming Declarations

[A [package_renaming_declaration](./AA-8.5#S0241) is used to rename a package.] 


#### Syntax

{AI05-0183-1} package_renaming_declaration<a id="S0241"></a> ::= package [defining_program_unit_name](./AA-6.1#S0201) renames package_[name](./AA-4.1#S0091)
   [[aspect_specification](./AA-13.1#S0346)];


#### Legality Rules

The renamed entity shall be a package.

{AI95-00217-06} {AI95-00412-01} If the package_[name](./AA-4.1#S0091) of a [package_renaming_declaration](./AA-8.5#S0241) denotes a limited view of a package P, then a name that denotes the [package_renaming_declaration](./AA-8.5#S0241) shall occur only within the immediate scope of the renaming or the scope of a [with_clause](./AA-10.1#S0294) that mentions the package P or, if P is a nested package, the innermost library package enclosing P. 

Discussion: The use of a renaming that designates a limited view is restricted to locations where we know whether the view is limited or nonlimited (based on a [with_clause](./AA-10.1#S0294)). We don't want to make an implicit limited view, as those are not transitive like a regular view. Implementations should be able to see all limited views needed based on the [context_clause](./AA-10.1#S0292). 


#### Static Semantics

A [package_renaming_declaration](./AA-8.5#S0241) declares a new view [of the renamed package].

{AI95-00412-01} [At places where the declaration of the limited view of the renamed package is visible, a [name](./AA-4.1#S0091) that denotes the [package_renaming_declaration](./AA-8.5#S0241) denotes a limited view of the package (see 10.1.1).] 

Proof: This rule is found in 8.3, "Visibility". 


#### Examples

Example of renaming a package: 

```ada
package TM renames Table_Manager;

```


#### Wording Changes from Ada 95

{AI95-00217-06} {AI95-00412-01} Uses of renamed limited views of packages can only be used within the scope of a with_clause for the renamed package. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [package_renaming_declaration](./AA-8.5#S0241). This is described in 13.1.1. 


## 8.5.4  Subprogram Renaming Declarations

{AI05-0299-1} A [subprogram_renaming_declaration](./AA-8.5#S0242) can serve as the completion of a [subprogram_declaration](./AA-6.1#S0195); such a [renaming_declaration](./AA-8.5#S0238) is called a renaming-as-body. A [subprogram_renaming_declaration](./AA-8.5#S0242) that is not a completion is called a renaming-as-declaration[, and is used to rename a subprogram (possibly an enumeration literal) or an entry]. 

Ramification: {AI05-0299-1} A renaming-as-body is a declaration, as defined in Clause 3. 


#### Syntax

{AI95-00218-03} {AI05-0183-1} subprogram_renaming_declaration<a id="S0242"></a> ::= 
    [[overriding_indicator](./AA-8.3#S0234)]
    [subprogram_specification](./AA-6.1#S0196) renames callable_entity_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];


#### Name Resolution Rules

The expected profile for the callable_entity_[name](./AA-4.1#S0091) is the profile given in the [subprogram_specification](./AA-6.1#S0196). 


#### Legality Rules

{AI05-0239-1} {AI12-0373-1} The profile of a renaming-as-declaration shall be mode conformant with that of the renamed callable entity. 

{AI95-00423-01} For a parameter or result subtype of the [subprogram_specification](./AA-6.1#S0196) that has an explicit [null_exclusion](./AA-3.10#S0083):

{AI12-0287-1} if the callable_entity_[name](./AA-4.1#S0091) statically denotes a generic formal subprogram of a generic unit G, and the [subprogram_renaming_declaration](./AA-8.5#S0242) occurs within the body of a generic unit G or within the body of a generic unit declared within the declarative region of the generic unit G, then the corresponding parameter or result subtype of the formal subprogram of G shall have a [null_exclusion](./AA-3.10#S0083);

otherwise, the subtype of the corresponding parameter or result type of the renamed callable entity shall exclude null. In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit. 

Reason: This rule prevents "lying". Null must never be the value of a parameter or result with an explicit [null_exclusion](./AA-3.10#S0083). The first bullet is an assume-the-worst rule which prevents trouble in generic bodies (including bodies of child units) when the formal subtype excludes null implicitly. 

{8652/0027} {8652/0028} {AI95-00135-01} {AI95-00145-01} {AI05-0239-1} The profile of a renaming-as-body shall conform fully to that of the declaration it completes. If the renaming-as-body completes that declaration before the subprogram it declares is frozen, the profile shall be mode conformant with that of the renamed callable entity and the subprogram it declares takes its convention from the renamed subprogram; otherwise, the profile shall be subtype conformant with that of the renamed callable entity and the convention of the renamed subprogram shall not be Intrinsic. A renaming-as-body is illegal if the declaration occurs before the subprogram whose declaration it completes is frozen, and the renaming renames the subprogram itself, through one or more subprogram renaming declarations, none of whose subprograms has been frozen. 

Reason: The otherwise part of the second sentence is to allow an implementation of a renaming-as-body as a single jump instruction to the target subprogram. Among other things, this prevents a subprogram from being completed with a renaming of an entry. (In most cases, the target of the jump can be filled in at link time. In some cases, such as a renaming of a name like "A(I).all", an indirect jump is needed. Note that the name is evaluated at renaming time, not at call time.)

{8652/0028} {AI95-00145-01} The first part of the second sentence is intended to allow renaming-as-body of predefined operators before the [subprogram_declaration](./AA-6.1#S0195) is frozen. For some types (such as integer types), the parameter type for operators is the base type, and it would be very strange for
   function Equal (A, B : in T) return Boolean;
   function Equal (A, B : in T) return Boolean renames "=";
to be illegal. (Note that predefined operators cannot be renamed this way after the [subprogram_declaration](./AA-6.1#S0195) is frozen, as they have convention Intrinsic.)

The first sentence is the normal rule for completions of [subprogram_declaration](./AA-6.1#S0195)s. 

Ramification: An [entry_declaration](./AA-9.5#S0257), unlike a [subprogram_declaration](./AA-6.1#S0195), cannot be completed with a [renaming_declaration](./AA-8.5#S0238). Nor can a [generic_subprogram_declaration](./AA-12.1#S0311).

The syntax rules prevent a protected subprogram declaration from being completed by a renaming. This is fortunate, because it allows us to avoid worrying about whether the implicit protected object parameter of a protected operation is involved in the conformance rules. 

Reason: {8652/0027} {AI95-00135-01} Circular renames before freezing is illegal, as the compiler would not be able to determine the convention of the subprogram. Other circular renames are handled below; see Bounded (Run-Time) Errors. 

{AI95-00228-01} The callable_entity_[name](./AA-4.1#S0091) of a renaming shall not denote a subprogram that requires overriding (see 3.9.3). 

Reason: {AI95-00228-01} Such a rename cannot be of the inherited subprogram (which requires overriding because it cannot be called), and thus cannot squirrel away a subprogram (see below). That would be confusing, so we make it illegal. The renaming is allowed after the overriding, as then the [name](./AA-4.1#S0091) will denote the overriding subprogram, not the inherited one. 

{AI95-00228-01} The callable_entity_[name](./AA-4.1#S0091) of a renaming-as-body shall not denote an abstract subprogram. 

Reason: {AI95-00228-01} Such a subprogram has no body, so it hardly can replace one in the program. 

{AI12-0204-1} If the callable_entity_[name](./AA-4.1#S0091) of a renaming is a prefixed view, the prefix of that view shall denote an object for which renaming is allowed.

Reason: {AI12-0204-1} The prefix in such a case is essentially renamed and passed to any calls of the renamed subprogram. If the prefix isn't legal to rename, that doesn't make sense (and allowing it might end up passing a nonexistent object to some calls). 

A [name](./AA-4.1#S0091) that denotes a formal parameter of the [subprogram_specification](./AA-6.1#S0196) is not allowed within the callable_entity_[name](./AA-4.1#S0091). 

Reason: This is to prevent things like this: 

```ada
function F(X : Integer) return Integer renames Table(X).all;

```

A similar rule in 6.1 forbids things like this: 

```ada
function F(X : Integer; Y : Integer := X) return Integer;

```


#### Static Semantics

A renaming-as-declaration declares a new view of the renamed entity. The profile of this new view takes its subtypes, parameter modes, and calling convention from the original profile of the callable entity, while taking the formal parameter [name](./AA-4.1#S0091)s and [default_expression](./AA-3.7#S0063)s from the profile given in the [subprogram_renaming_declaration](./AA-8.5#S0242). The new view is a function or procedure, never an entry. 

To be honest: When renaming an entry as a procedure, the compile-time rules apply as if the new view is a procedure, but the run-time semantics of a call are that of an entry call. 

Ramification: For example, it is illegal for the [entry_call_statement](./AA-9.5#S0264) of a [timed_entry_call](./AA-9.7#S0276) to call the new view. But what looks like a procedure call will do things like barrier waiting.

{8652/0105} {AI95-00211-01} {AI95-00228-01} {AI05-0095-1} All properties of the renamed entity are inherited by the new view unless otherwise stated by this document. In particular, if the renamed entity is abstract, the new view also is abstract. Similarly, if the renamed entity is not a program unit, then neither is the renaming. (Implicitly declared subprograms are not program units, see 10.1). 


#### Dynamic Semantics

{8652/0014} {AI95-00064-01} For a call to a subprogram whose body is given as a renaming-as-body, the execution of the renaming-as-body is equivalent to the execution of a [subprogram_body](./AA-6.3#S0216) that simply calls the renamed subprogram with its formal parameters as the actual parameters and, if it is a function, returns the value of the call. 

Ramification: This implies that the subprogram completed by the renaming-as-body has its own elaboration check. 

{AI05-0123-1} For a call on a renaming of a dispatching subprogram that is overridden, if the overriding occurred before the renaming, then the body executed is that of the overriding declaration, even if the overriding declaration is not visible at the place of the renaming; otherwise, the inherited or predefined subprogram is called. A corresponding rule applies to a call on a renaming of a predefined equality operator for an untagged record type. 

Discussion: Note that whether or not the renaming is itself primitive has nothing to do with the renamed subprogram.

{AI05-0123-1} Note that the above rule is only for tagged types and equality of untagged record types.

Consider the following example: 

```ada
package P is
    type T is tagged null record;
    function Predefined_Equal(X, Y : T) return Boolean renames "=";
private
    function "="(X, Y : T) return Boolean; -- Override predefined "=".
end P;

```

```ada
with P; use P;
package Q is
    function User_Defined_Equal(X, Y : T) return Boolean renames P."=";
end Q;

```

A call on Predefined_Equal will execute the predefined equality operator of T, whereas a call on User_Defined_Equal will execute the body of the overriding declaration in the private part of P.

Thus a renaming allows one to squirrel away a copy of an inherited or predefined subprogram before later overriding it. 


#### Bounded (Run-Time) Errors

{8652/0027} {AI95-00135-01} If a subprogram directly or indirectly renames itself, then it is a bounded error to call that subprogram. Possible consequences are that Program_Error or Storage_Error is raised, or that the call results in infinite recursion. 

Reason: {8652/0027} {AI95-00135-01} This has to be a bounded error, as it is possible for a renaming-as-body appearing in a package body to cause this problem. Thus it is not possible in general to detect this problem at compile time. 

NOTE 1   A procedure can only be renamed as a procedure. A function whose [defining_designator](./AA-6.1#S0200) is either an [identifier](./AA-2.3#S0002) or an [operator_symbol](./AA-6.1#S0202) can be renamed with either an [identifier](./AA-2.3#S0002) or an [operator_symbol](./AA-6.1#S0202); for renaming as an operator, the subprogram specification given in the [renaming_declaration](./AA-8.5#S0238) is subject to the rules given in 6.6 for operator declarations. Enumeration literals can be renamed as functions; similarly, [attribute_reference](./AA-4.1#S0100)s that denote functions (such as references to Succ and Pred) can be renamed as functions. An entry can only be renamed as a procedure; the new [name](./AA-4.1#S0091) is only allowed to appear in contexts that allow a procedure [name](./AA-4.1#S0091). An entry of a family can be renamed, but an entry family cannot be renamed as a whole.

NOTE 2   The operators of the root numeric types cannot be renamed because the types in the profile are anonymous, so the corresponding specifications cannot be written; the same holds for certain attributes, such as Pos.

This paragraph was deleted.{AI12-0292-1} 

NOTE 3   The primitiveness of a renaming-as-declaration is determined by its profile, and by where it occurs, as for any declaration of (a view of) a subprogram; primitiveness is not determined by the renamed view. In order to perform a dispatching call, the subprogram name has to denote a primitive subprogram, not a nonprimitive renaming of a primitive subprogram. 

Reason: A [subprogram_renaming_declaration](./AA-8.5#S0242) could more properly be called renaming_as_subprogram_declaration, since you're renaming something as a subprogram, but you're not necessarily renaming a subprogram. But that's too much of a mouthful. Or, alternatively, we could call it a callable_entity_renaming_declaration, but that's even worse. Not only is it a mouthful, it emphasizes the entity being renamed, rather than the new view, which we think is a bad idea. We'll live with the oddity. 


#### Examples

Examples of subprogram renaming declarations: 

```ada
procedure My_Write(C : in Character) renames Pool(K).Write; --  see 4.1.3

```

```ada
function Real_Plus(Left, Right : Real   ) return Real    renames "+";
function Int_Plus (Left, Right : Integer) return Integer renames "+";

```

```ada
function Rouge return Color renames Red;  --  see 3.5.1
function Rot   return Color renames Red;
function Rosso return Color renames Rouge;

```

```ada
function Next(X : Color) return Color renames Color'Succ; -- see 3.5.1

```

Example of a subprogram renaming declaration with new parameter names: 

```ada
function "*" (X,Y : Vector) return Real renames Dot_Product; -- see 6.1

```

Example of a subprogram renaming declaration with a new default expression: 

```ada
function Minimum(L : Link := Head) return Cell renames Min_Cell; -- see 6.1

```


#### Extensions to Ada 95

{8652/0028} {AI95-00145-01} Corrigendum: Allowed a renaming-as-body to be just mode conformant with the specification if the subprogram is not yet frozen.

{AI95-00218-03} [Overriding_indicator](./AA-8.3#S0234) (see 8.3.1) is optionally added to subprogram renamings. 


#### Wording Changes from Ada 95

{8652/0014} {AI95-00064-01} Corrigendum: Described the semantics of renaming-as-body, so that the location of elaboration checks is clear.

{8652/0027} {AI95-00135-01} Corrigendum: Clarified that circular renaming-as-body is illegal (if it can be detected in time) or a bounded error.

{AI95-00228-01} Amendment Correction: Clarified that renaming a shall-be-overridden subprogram is illegal, as well as renaming-as-body an abstract subprogram.

{AI95-00423-01} Added matching rules for [null_exclusion](./AA-3.10#S0083)s. 


#### Inconsistencies With Ada 2005

{AI05-0123-1} Renaming of user-defined untagged record equality is now defined to call the overridden body so long as the overriding occurred before the renames. This could change the body called in unusual cases; the change is necessary to preserve the principle that the body called for an explicit call to "=" (via a renames in this case) is the same as the one inherited for a derived type and used in generics. Note that any renamings before the overriding will be unchanged. Any differences caused by the change will be rare and most likely will fix a bug. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [subprogram_renaming_declaration](./AA-8.5#S0242). This is described in 13.1.1. 


#### Incompatibilities With Ada 2012

{AI12-0204-1} Correction: Added a rule to ensure that the prefix of a renaming of a prefixed view continues to exist during the life of a renames. If the prefix is a subcomponent that depends on discriminants, Ada 2005 and 2012 would have allowed the prefix while Ada 2022 would not. Without this change, explicit forms (renaming the object and then using that in calls) would be safer than the renaming; that's inconsistent with other kinds of renaming. 


#### Wording Changes from Ada 2012

{AI12-0287-1} Correction: Added wording to ensure that the object subject to a Legality Rule can be determined at compile-time. The alternative being nonsense, we treat this as a wording change. 


## 8.5.5  Generic Renaming Declarations

[A [generic_renaming_declaration](./AA-8.5#S0243) is used to rename a generic unit.] 


#### Syntax

{AI05-0183-1} generic_renaming_declaration<a id="S0243"></a> ::= 
    generic package	[defining_program_unit_name](./AA-6.1#S0201) renames generic_package_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];
  | generic procedure	[defining_program_unit_name](./AA-6.1#S0201) renames generic_procedure_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];
  | generic function	[defining_program_unit_name](./AA-6.1#S0201) renames generic_function_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];


#### Legality Rules

The renamed entity shall be a generic unit of the corresponding kind. 


#### Static Semantics

A [generic_renaming_declaration](./AA-8.5#S0243) declares a new view [of the renamed generic unit]. 

NOTE 1   {AI12-0440-1} Although the properties of the new view are the same as those of the renamed view, the place where the [generic_renaming_declaration](./AA-8.5#S0243) occurs can affect the legality of subsequent renamings and instantiations that denote the [generic_renaming_declaration](./AA-8.5#S0243), in particular if the renamed generic unit is a library unit (see 10.1.1). 


#### Examples

Example of renaming a generic unit: 

```ada
generic package Enum_IO renames Ada.Text_IO.Enumeration_IO;  -- see A.10.10

```


#### Extensions to Ada 83

Renaming of generic units is new to Ada 95. It is particularly important for renaming child library units that are generic units. For example, it might be used to rename Numerics.Generic_Elementary_Functions as simply Generic_Elementary_Functions, to match the name for the corresponding Ada-83-based package. 


#### Wording Changes from Ada 83

The information in RM83-8.6, "The Package Standard", has been updated for the child unit feature, and moved to Annex A, except for the definition of "predefined type", which has been moved to 3.2.1. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [generic_renaming_declaration](./AA-8.5#S0243). This is described in 13.1.1. 

