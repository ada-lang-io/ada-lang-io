---
sidebar_position:  67
---

# 8.3  Visibility

[ The visibility rules, given below, determine which declarations are visible and directly visible at each place within a program. The visibility rules apply to both explicit and implicit declarations.] 


#### Static Semantics

A declaration is defined to be directly visible at places where a [name](./AA-4.1#S0091) consisting of only an [identifier](./AA-2.3#S0002) or [operator_symbol](./AA-6.1#S0202) is sufficient to denote the declaration; that is, no [selected_component](./AA-4.1#S0098) notation or special context (such as preceding =&gt in a named association) is necessary to denote the declaration. A declaration is defined to be visible wherever it is directly visible, as well as at other places where some [name](./AA-4.1#S0091) (such as a [selected_component](./AA-4.1#S0098)) can denote the declaration.

The syntactic category [direct_name](./AA-4.1#S0092) is used to indicate contexts where direct visibility is required. The syntactic category [selector_name](./AA-4.1#S0099) is used to indicate contexts where visibility, but not direct visibility, is required.

There are two kinds of direct visibility: immediate visibility and use-visibility. A declaration is immediately visible at a place if it is directly visible because the place is within its immediate scope. A declaration is use-visible if it is directly visible because of a [use_clause](./AA-8.4#S0235) (see 8.4). Both conditions can apply.

A declaration can be hidden, either from direct visibility, or from all visibility, within certain parts of its scope. Where hidden from all visibility, it is not visible at all (neither using a [direct_name](./AA-4.1#S0092) nor a [selector_name](./AA-4.1#S0099)). Where hidden from direct visibility, only direct visibility is lost; visibility using a [selector_name](./AA-4.1#S0099) is still possible.

[ Two or more declarations are overloaded if they all have the same defining name and there is a place where they are all directly visible.] 

Ramification: Note that a [name](./AA-4.1#S0091) can have more than one possible interpretation even if it denotes a nonoverloadable entity. For example, if there are two functions F that return records, both containing a component called C, then the name F.C has two possible interpretations, even though component declarations are not overloadable. 

The declarations of callable entities [(including enumeration literals)] are overloadable[, meaning that overloading is allowed for them]. 

Ramification: A [generic_declaration](./AA-12.1#S0310) is not overloadable within its own [generic_formal_part](./AA-12.1#S0313). This follows from the rules about when a [name](./AA-4.1#S0091) denotes a current instance. See AI83-00286. This implies that within a [generic_formal_part](./AA-12.1#S0313), outer declarations with the same defining name are hidden from direct visibility. It also implies that if a generic formal parameter has the same defining name as the generic itself, the formal parameter hides the generic from direct visibility. 

Two declarations are homographs if they have the same defining name, and, if both are overloadable, their profiles are type conformant. [An inner declaration hides any outer homograph from direct visibility.]

Glossary entry: An overriding operation is one that replaces an inherited primitive operation. Operations may be marked explicitly as overriding or not overriding.

Version=[5],Kind=(AddedNormal),Group=[S],Term=[overriding operation], Def=[an operation that replaces an inherited primitive operation], Note1=[Operations can be marked explicitly as overriding or not overriding.]

{8652/0025} {AI95-00044-01} [Two homographs are not generally allowed immediately within the same declarative region unless one overrides the other (see Legality Rules below).] The only declarations that are overridable are the implicit declarations for predefined operators and inherited primitive subprograms. A declaration overrides another homograph that occurs immediately within the same declarative region in the following cases: 

{8652/0025} {AI95-00044-01} A declaration that is not overridable overrides one that is overridable, [regardless of which declaration occurs first]; 

Ramification: {8652/0025} {AI95-00044-01} And regardless of whether the nonoverridable declaration is overloadable or not. For example, [statement_identifier](./AA-5.1#S0172)s are covered by this rule.

The "regardless of which declaration occurs first" is there because the explicit declaration could be a primitive subprogram of a partial view, and then the full view might inherit a homograph. We are saying that the explicit one wins (within its scope), even though the implicit one comes later.

If the overriding declaration is also a subprogram, then it is a primitive subprogram.

As explained in 7.3.1, "Private Operations", some inherited primitive subprograms are never declared. Such subprograms cannot be overridden, although they can be reached by dispatching calls in the case of a tagged type. 

The implicit declaration of an inherited operator overrides that of a predefined operator; 

Ramification: In a previous version of Ada 9X, we tried to avoid the notion of predefined operators, and say that they were inherited from some magical root type. However, this seemed like too much mechanism. Therefore, a type can have a predefined "+" as well as an inherited "+". The above rule says the inherited one wins.

{AI95-00114-01} The "regardless of which declaration occurs first" applies here as well, in the case where [derived_type_definition](./AA-3.4#S0035) in the visible part of a public library unit derives from a private type declared in the parent unit, and the full view of the parent type has additional predefined operators, as explained in 7.3.1, "Private Operations". Those predefined operators can be overridden by inherited subprograms implicitly declared earlier. 

An implicit declaration of an inherited subprogram overrides a previous implicit declaration of an inherited subprogram.

{AI95-00251-01} If two or more homographs are implicitly declared at the same place:

{AI95-00251-01} If at least one is a subprogram that is neither a null procedure nor an abstract subprogram, and does not require overriding (see 3.9.3), then they override those that are null procedures, abstract subprograms, or require overriding. If more than one such homograph remains that is not thus overridden, then they are all hidden from all visibility.

{AI95-00251-01} {AI12-0183-1} Otherwise (all are null procedures, abstract subprograms, or require overriding), then any null procedure overrides all abstract subprograms and all subprograms that require overriding; if more than one such homograph remains that is not thus overridden, then if the profiles of the remaining homographs are all fully conformant with one another, one is chosen arbitrarily; if not, they are all hidden from all visibility. 

Discussion: In the case where the implementation arbitrarily chooses one overrider from among a group of inherited subprograms, users should not be able to determine which member was chosen, as the set of inherited subprograms which are chosen from must be fully conformant. This rule is needed in order to allow

```ada
package Outer is
   package P1 is
      type Ifc1 is interface;
      procedure Null_Procedure (X : Ifc1) is null;
      procedure Abstract_Subp  (X : Ifc1) is abstract;
   end P1;

```

```ada
   package P2 is
      type Ifc2 is interface;
      procedure Null_Procedure (X : Ifc2) is null;
      procedure Abstract_Subp  (X : Ifc2) is abstract;
   end P2;

```

```ada
   type T is abstract new P1.Ifc1 and P2.Ifc2 with null record;
end Outer;

```

without requiring that T explicitly override any of its inherited operations.

Full conformance is required here, as we cannot allow the parameter names to differ. If they did differ, the routine which was selected for overriding could be determined by using named parameter notation in a call.

{AI12-0005-1} When the subprograms do not conform, we chose not to adopt the "use clause" rule which would make them all visible resulting in likely ambiguity. If we had used such a rule, any successful calls would be confusing; and the fact that there are no Beaujolais-like effects to worry about means we can consider other rules. The hidden-from-all-visibility homographs are still inherited by further derivations, which avoids order-of-declaration dependencies and other anomalies.

We have to be careful to not include arbitrary selection if the routines have real bodies. (This can happen in generics, see the example in the incompatibilities section below.) We don't want the ability to successfully call routines where the body executed depends on the compiler or a phase of the moon.

Note that if the type is concrete, abstract subprograms are inherited as subprograms that require overriding. We include functions that require overriding as well; these don't have real bodies, so they can use the more liberal rules. 

[For an implicit declaration of a primitive subprogram in a generic unit, there is a copy of this declaration in an instance.] However, a whole new set of primitive subprograms is implicitly declared for each type declared within the visible part of the instance. These new declarations occur immediately after the type declaration, and override the copied ones. [The copied ones can be called only from within the instance; the new ones can be called only from outside the instance, although for tagged types, the body of a new one can be executed by a call to an old one.] 

Discussion: In addition, this is also stated redundantly (again), and is repeated, in 12.3, "Generic Instantiation". The rationale for the rule is explained there. 

To be honest: {AI05-0042-1} The implicit subprograms declared when an operation of a progenitor is implemented by an entry or subprogram also override the appropriate implicitly declared inherited operations of the progenitor. 

A declaration is visible within its scope, except where hidden from all visibility, as follows: 

An overridden declaration is hidden from all visibility within the scope of the overriding declaration. 

Ramification: We have to talk about the scope of the overriding declaration, not its visibility, because it hides even when it is itself hidden.

Note that the scope of an explicit [subprogram_declaration](./AA-6.1#S0195) does not start until after its profile. 

A declaration is hidden from all visibility until the end of the declaration, except: 

For a record type or record extension, the declaration is hidden from all visibility only until the reserved word record;

{AI95-00345-01} {AI05-0177-1} For a [package_declaration](./AA-7.1#S0229), [generic_package_declaration](./AA-12.1#S0312), [subprogram_body](./AA-6.3#S0216), or [expression_function_declaration](./AA-6.8#S0228), the declaration is hidden from all visibility only until the reserved word is of the declaration; 

Ramification: We're talking about the is of the construct itself, here, not some random is that might appear in a [generic_formal_part](./AA-12.1#S0313). 

{AI95-00345-01} For a task declaration or protected declaration, the declaration is hidden from all visibility only until the reserved word with of the declaration if there is one, or the reserved word is of the declaration if there is no with.

To be honest: If there is neither a with nor is, then the exception does not apply and the name is hidden from all visibility until the end of the declaration. This oddity was inherited from Ada 95. 

Reason: We need the "with or is" rule so that the visibility within an [interface_list](./AA-3.9#S0078) does not vary by construct. That would make it harder to complete private extensions and would complicate implementations. 

If the completion of a declaration is a declaration, then within the scope of the completion, the first declaration is hidden from all visibility. Similarly, a [discriminant_specification](./AA-3.7#S0062) or [parameter_specification](./AA-6.1#S0207) is hidden within the scope of a corresponding [discriminant_specification](./AA-3.7#S0062) or [parameter_specification](./AA-6.1#S0207) of a corresponding completion, or of a corresponding [accept_statement](./AA-9.5#S0258). 

Ramification: This rule means, for example, that within the scope of a [full_type_declaration](./AA-3.2#S0024) that completes a [private_type_declaration](./AA-7.3#S0232), the name of the type will denote the [full_type_declaration](./AA-3.2#S0024), and therefore the full view of the type. On the other hand, if the completion is not a declaration, then it doesn't hide anything, and you can't denote it. 

{AI95-00217-06} {AI95-00412-01} The declaration of a library unit (including a [library_unit_renaming_declaration](./AA-10.1#S0289)) is hidden from all visibility at places outside its declarative region that are not within the scope of a [nonlimited_with_clause](./AA-10.1#S0296) that mentions it. The limited view of a library package is hidden from all visibility at places that are not within the scope of a [limited_with_clause](./AA-10.1#S0295) that mentions it; in addition, the limited view is hidden from all visibility within the declarative region of the package, as well as within the scope of any [nonlimited_with_clause](./AA-10.1#S0296) that mentions the package. Where the declaration of the limited view of a package is visible, any name that denotes the package denotes the limited view, including those provided by a package renaming.

Discussion: {AI95-00217-06} This is the rule that prevents [with_clause](./AA-10.1#S0294)s from being transitive; the [immediate] scope includes indirect semantic dependents. This rule also prevents the limited view of a package from being visible in the same place as the full view of the package, which prevents various ripple effects. 

{AI95-00217-06} {AI95-00412-01} [For each declaration or renaming of a generic unit as a child of some parent generic package, there is a corresponding declaration nested immediately within each instance of the parent.] Such a nested declaration is hidden from all visibility except at places that are within the scope of a [with_clause](./AA-10.1#S0294) that mentions the child.

A declaration with a [defining_identifier](./AA-3.1#S0022) or [defining_operator_symbol](./AA-6.1#S0203) is immediately visible [(and hence directly visible)] within its immediate scope  except where hidden from direct visibility, as follows: 

A declaration is hidden from direct visibility within the immediate scope of a homograph of the declaration, if the homograph occurs within an inner declarative region;

A declaration is also hidden from direct visibility where hidden from all visibility. 

{AI95-00195-01} {AI95-00408-01} {AI05-0183-1} An [attribute_definition_clause](./AA-13.3#S0349) or an [aspect_specification](./AA-13.1#S0346) is visible everywhere within its scope.


#### Name Resolution Rules

A [direct_name](./AA-4.1#S0092) shall resolve to denote a directly visible declaration whose defining name is the same as the [direct_name](./AA-4.1#S0092). A [selector_name](./AA-4.1#S0099) shall resolve to denote a visible declaration whose defining name is the same as the [selector_name](./AA-4.1#S0099). 

Discussion: "The same as" has the obvious meaning here, so for +, the possible interpretations are declarations whose defining name is "+" (an [operator_symbol](./AA-6.1#S0202)). 

These rules on visibility and direct visibility do not apply in a [context_clause](./AA-10.1#S0292), a [parent_unit_name](./AA-10.1#S0291), or a [pragma](./AA-2.8#S0019) that appears at the place of a [compilation_unit](./AA-10.1#S0286). For those contexts, see the rules in 10.1.6, "Environment-Level Visibility Rules". 

Ramification: Direct visibility is irrelevant for [character_literal](./AA-2.5#S0015)s. In terms of overload resolution [character_literal](./AA-2.5#S0015)s are similar to other literals, like null - see 4.2. For [character_literal](./AA-2.5#S0015)s, there is no need to worry about hiding, since there is no way to declare homographs. 


#### Legality Rules

{8652/0025} {8652/0026} {AI95-00044-01} {AI95-00150-01} {AI95-00377-01} A nonoverridable declaration is illegal if there is a homograph occurring immediately within the same declarative region that is visible at the place of the declaration, and is not hidden from all visibility by the nonoverridable declaration. In addition, a type extension is illegal if somewhere within its immediate scope it has two visible components with the same name. Similarly, the [context_clause](./AA-10.1#S0292) for a compilation unit is illegal if it mentions (in a [with_clause](./AA-10.1#S0294)) some library unit, and there is a homograph of the library unit that is visible at the place of the compilation unit, and the homograph and the mentioned library unit are both declared immediately within the same declarative region. These rules also apply to dispatching operations declared in the visible part of an instance of a generic unit. However, they do not apply to other overloadable declarations in an instance[; such declarations may have type conformant profiles in the instance, so long as the corresponding declarations in the generic were not type conformant]. 

Discussion: Normally, these rules just mean you can't explicitly declare two homographs immediately within the same declarative region. The wording is designed to handle the following special cases: 

If the second declaration completes the first one, the second declaration is legal.

If the body of a library unit contains an explicit homograph of a child of that same library unit, this is illegal only if the body mentions the child in its [context_clause](./AA-10.1#S0292), or if some subunit mentions the child. Here's an example: 

```ada
package P is
end P;

```

```ada
package P.Q is
end P.Q;

```

```ada
package body P is
    Q : Integer; -- OK; we cannot see package P.Q here.
    procedure Sub is separate;
end P;

```

```ada
with P.Q;
separate(P)
procedure Sub is -- Illegal.
begin
    null;
end Sub;

```

If package body P said "with P.Q;", then it would be illegal to declare the homograph Q: Integer. But it does not, so the body of P is OK. However, the subunit would be able to see both P.Q's, and is therefore illegal.

A previous version of Ada 9X allowed the subunit, and said that references to P.Q would tend to be ambiguous. However, that was a bad idea, because it requires overload resolution to resolve references to directly visible nonoverloadable homographs, which is something compilers have never before been required to do.

{8652/0026} {8652/0102} {AI95-00150-01} {AI95-00157-01} If a type extension contains a component with the same name as a component in an ancestor type, there must be no place where both components are visible. For instance: 

```ada
package A is
   type T is tagged private;
   package B is
      type NT is new T with record
         I: Integer; -- Illegal because T.I is visible in the body.
      end record; -- T.I is not visible here.
   end B;
private
   type T is tagged record
      I: Integer; -- Illegal because T.I is visible in the body.
   end record;
end A;

```

```ada
{AI95-00114-01} package body A is
   package body B is
      -- T.I becomes visible here.
   end B;
end A;

```

```ada
package A.C is
   type NT2 is new A.T with record
      I: Integer; -- Illegal because T.I is visible in the private part.
   end record; -- T.I is not visible here.
private
    -- T.I is visible here.
end A.C;

```

```ada
with A;
package D is
   type NT3 is new A.T with record
      I: Integer; -- Legal because T.I is never visible in this package.
   end record;
end D;

```

```ada
with D;
package A.E is
   type NT4 is new D.NT3 with null record;
   X : NT4;
   I1 : Integer := X.I;        -- D.NT3.I
   I2 : Integer := D.NT3(X).I; -- D.NT3.I
   I3 : Integer := A.T(X).I;   -- A.T.I
end A.E;

```

{8652/0102} {AI95-00157-01} D.NT3 can have a component I because the component I of the parent type is never visible. The parent component exists, of course, but is never declared for the type D.NT3. In the child package A.E, the component I of A.T is visible, but that does not change the fact that the A.T.I component was never declared for type D.NT3. Thus, A.E.NT4 does not (visibly) inherit the component I from A.T, while it does inherit the component I from D.NT3. Of course, both components exist, and can be accessed by a type conversion as shown above. This behavior stems from the fact that every characteristic of a type (including components) must be declared somewhere in the innermost declarative region containing the type - if the characteristic is never visible in that declarative region, it is never declared. Therefore, such characteristics do not suddenly become available even if they are in fact visible in some other scope. See 7.3.1 for more on the rules.

{AI95-00377-01} It is illegal to mention both an explicit child of an instance, and a child of the generic from which the instance was instantiated. This is easier to understand with an example: 

```ada
generic
package G1 is
end G1;

```

```ada
generic
package G1.G2 is
end G1.G2;

```

```ada
with G1;
package I1 is new G1;

```

```ada
package I1.G2 renames ...

```

```ada
with G1.G2;
with I1.G2;             -- Illegal
package Bad is ...

```

{AI12-0300-1} The context clause for Bad is illegal as I1 has an implicit declaration of I1.G2 based on the generic child G1.G2, as well as the mention of the explicit child I1.G2. As in the previous cases, this is illegal only if the context clause makes both children visible; the explicit child can be mentioned as long as the generic child is not (and vice versa). 

Note that we need to be careful which things we make "hidden from all visibility" versus which things we make simply illegal for names to denote. The distinction is subtle. The rules that disallow names denoting components within a type declaration (see 3.7) do not make the components invisible at those places, so that the above rule makes components with the same name illegal. The same is true for the rule that disallows names denoting formal parameters within a [formal_part](./AA-6.1#S0206) (see 6.1). 

Discussion: The part about instances is from AI83-00012. The reason it says "overloadable declarations" is because we don't want it to apply to type extensions that appear in an instance; components are not overloadable. 

NOTE 1   Visibility for compilation units follows from the definition of the environment in 10.1.4, except that it is necessary to apply a [with_clause](./AA-10.1#S0294) to obtain visibility to a [library_unit_declaration](./AA-10.1#S0288) or [library_unit_renaming_declaration](./AA-10.1#S0289).

NOTE 2   In addition to the visibility rules given above, the meaning of the occurrence of a [direct_name](./AA-4.1#S0092) or [selector_name](./AA-4.1#S0099) at a given place in the text can depend on the overloading rules (see 8.6).

NOTE 3   Not all contexts where an [identifier](./AA-2.3#S0002), [character_literal](./AA-2.5#S0015), or [operator_symbol](./AA-6.1#S0202) are allowed require visibility of a corresponding declaration. Contexts where visibility is not required are identified by using one of these three syntactic categories directly in a syntax rule, rather than using [direct_name](./AA-4.1#S0092) or [selector_name](./AA-4.1#S0099). 

Ramification: An [identifier](./AA-2.3#S0002), [character_literal](./AA-2.5#S0015) or [operator_symbol](./AA-6.1#S0202) that occurs in one of the following contexts is not required to denote a visible or directly visible declaration: 

a)A defining name.

b)The [identifier](./AA-2.3#S0002)s or [operator_symbol](./AA-6.1#S0202) that appear after the reserved word end in a [proper_body](./AA-3.11#S0090). Similarly for "end loop", etc.

c)An [attribute_designator](./AA-4.1#S0101).

d)A [pragma](./AA-2.8#S0019) [identifier](./AA-2.3#S0002).

e)A pragma_argument_[identifier](./AA-2.3#S0002).

f)An [identifier](./AA-2.3#S0002) specific to a pragma used in a pragma argument.

g){AI05-0183-1} An [aspect_mark](./AA-13.1#S0347);

h){AI05-0183-1} An [identifier](./AA-2.3#S0002) specific to an aspect used in an [aspect_definition](./AA-13.1#S0348). 

The visibility rules have nothing to do with the above cases; the meanings of such things are defined elsewhere. Reserved words are not [identifier](./AA-2.3#S0002)s; the visibility rules don't apply to them either.

Because of the way we have defined "declaration", it is possible for a usage name to denote a [subprogram_body](./AA-6.3#S0216), either within that body, or (for a nonlibrary unit) after it (since the body hides the corresponding declaration, if any). Other bodies do not work that way. Completions of [type_declaration](./AA-3.2#S0023)s and deferred constant declarations do work that way. [Accept_statement](./AA-9.5#S0258)s are never denoted, although the [parameter_specification](./AA-6.1#S0207)s in their profiles can be.

The scope of a subprogram does not start until after its profile. Thus, the following is legal: 

```ada
X : constant Integer := 17;
...
package P is
    procedure X(Y : in Integer := X);
end P;

```

The body of the subprogram will probably be illegal, however, since the constant X will be hidden by then.

The rule is different for generic subprograms, since they are not overloadable; the following is illegal: 

```ada
X : constant Integer := 17;
package P is
    generic
      Z : Integer := X; -- Illegal!
    procedure X(Y : in Integer := X); -- Illegal!
end P;

```

The constant X is hidden from direct visibility by the generic declaration. 


#### Extensions to Ada 83

Declarations with the same defining name as that of a subprogram or entry being defined are nevertheless visible within the subprogram specification or entry declaration. 


#### Wording Changes from Ada 83

The term "visible by selection" is no longer defined. We use the terms "directly visible" and "visible" (among other things). There are only two regions of text that are of interest, here: the region in which a declaration is visible, and the region in which it is directly visible.

Visibility is defined only for declarations. 


#### Incompatibilities With Ada 95

{AI95-00251-01} Added rules to handle the inheritance and overriding of multiple homographs for a single type declaration, in order to support multiple inheritance from interfaces. The new rules are intended to be compatible with the existing rules so that programs that do not use interfaces do not change their legality. However, there is a very rare case where this is not true: 

```ada
generic
   type T1 is private;
   type T2 is private;
package G is
   type T is null record;
   procedure P (X : T; Y : T1);
   procedure P (X : T; Z : T2);
end G;]

```

```ada
package I is new G (Integer, Integer); -- Exports homographs of P.

```

```ada
type D is new I.T; -- Both Ps are inherited.

```

```ada
Obj : D;

```

```ada
P (Obj, Z =&gt 10); -- Legal in Ada 95, illegal in Ada 2005.

```

The call to P would resolve in Ada 95 by using the parameter name, while the procedures P would be hidden from all visibility in Ada 2005 and thus would not resolve. This case doesn't seem worth making the rules any more complex than they already are.

{AI95-00377-01} Amendment Correction: A [with_clause](./AA-10.1#S0294) is illegal if it would create a homograph of an implicitly declared generic child (see 10.1.1). An Ada 95 compiler could have allowed this, but which unit of the two units involved would be denoted wasn't specified, so any successful use isn't portable. Removing one of the two [with_clause](./AA-10.1#S0294)s involved will fix the problem. 


#### Wording Changes from Ada 95

{8652/0025} {AI95-00044-01} Corrigendum: Clarified the overriding rules so that "/=" and [statement_identifier](./AA-5.1#S0172)s are covered.

{8652/0026} {AI95-00150-01} Corrigendum: Clarified that is it never possible for two components with the same name to be visible; any such program is illegal.

{AI95-00195-01} {AI95-00408-01} The visibility of an [attribute_definition_clause](./AA-13.3#S0349) is defined so that it can be used by the stream attribute availability rules (see 13.13.2).

{AI95-00217-06} The visibility of a limited view of a library package is defined (see 10.1.1). 


#### Wording Changes from Ada 2005

{AI05-0177-1} {AI12-0157-1} Added wording so that the parameters of an [expression_function_declaration](./AA-6.8#S0228) are visible in the return expression of the function. (It would be pretty useless without such a rule.)

{AI05-0183-1} The visibility of an [aspect_specification](./AA-13.1#S0346) is defined so that it can be used in various other rules. 


## 8.3.1  Overriding Indicators

{AI95-00218-03} An [overriding_indicator](./AA-8.3#S0234) is used to declare that an operation is intended to override (or not override) an inherited operation. 


#### Syntax

{AI95-00218-03} overriding_indicator<a id="S0234"></a> ::= [not] overriding


#### Legality Rules

{AI95-00218-03} {AI95-00348-01} {AI95-00397-01} {AI05-0177-1} If an [abstract_subprogram_declaration](./AA-3.9#S0076), [null_procedure_declaration](./AA-6.7#S0227), [expression_function_declaration](./AA-6.8#S0228), [subprogram_body](./AA-6.3#S0216), [subprogram_body_stub](./AA-10.1#S0298), [subprogram_renaming_declaration](./AA-8.5#S0242), [generic_instantiation](./AA-12.3#S0315) of a subprogram, or [subprogram_declaration](./AA-6.1#S0195) other than a protected subprogram has an [overriding_indicator](./AA-8.3#S0234), then:

the operation shall be a primitive operation for some type;

if the [overriding_indicator](./AA-8.3#S0234) is overriding, then the operation shall override a homograph at the place of the declaration or body;

To be honest: {AI05-0005-1} This doesn't require that the overriding happen at precisely the place of the declaration or body; it only requires that the region in  which the overriding is known to have happened includes this place. That is, the overriding can happen at or before the place of the declaration or body. 

if the [overriding_indicator](./AA-8.3#S0234) is not overriding, then the operation shall not override any homograph (at any place). 

In addition to the places where Legality Rules normally apply, these rules also apply in the private part of an instance of a generic unit.

Discussion: The overriding and not overriding rules differ slightly. For overriding, we want the indicator to reflect the overriding state at the place of the declaration; otherwise the indicator would be "lying". Whether a homograph is implicitly declared after the declaration (see 7.3.1 to see how this can happen) has no impact on this check. However, not overriding is different; "lying" would happen if a homograph declared later actually is overriding. So, we require this check to take into account later overridings. That can be implemented either by looking ahead, or by rechecking when additional operations are declared.

The "no lying" rules are needed to prevent a [subprogram_declaration](./AA-6.1#S0195) and [subprogram_body](./AA-6.3#S0216) from having contradictory [overriding_indicator](./AA-8.3#S0234)s. 

NOTE 1   {AI95-00397-01} Rules for [overriding_indicator](./AA-8.3#S0234)s of task and protected entries and of protected subprograms are found in 9.5.2 and 9.4, respectively. 


#### Examples

{AI95-00433-01} {AI12-0429-1} Example of use of an overriding indicator when declaring a security queue derived from the Queue interface of 3.9.4:

```ada
type Security_Queue is new Queue with record ...;

```

```ada
overriding
procedure Append(Q : in out Security_Queue; Person : in Person_Name);

```

```ada
{AI12-0178-1} overriding
procedure Remove_First(Q : in out Security_Queue; Person : out Person_Name);

```

```ada
overriding
function Cur_Count(Q : in Security_Queue) return Natural;

```

```ada
overriding
function Max_Count(Q : in Security_Queue) return Natural;

```

```ada
not overriding
procedure Arrest(Q : in out Security_Queue; Person : in Person_Name);

```

{AI12-0429-1} The first four subprogram declarations guarantee that these subprograms will override the four subprograms inherited from the Queue interface. A misspelling in one of these subprograms will be detected at compile time by the implementation. Conversely, the declaration of Arrest guarantees that this is a new operation.

Discussion: In this case, the subprograms are abstract, so misspellings will get detected anyway. But for other subprograms (especially when deriving from concrete types), the error might never be detected, and a body other than the one the programmer intended might be executed without warning. Thus our new motto: "Overriding indicators - don't derive a type without them!" 


#### Extensions to Ada 95

{AI95-00218-03} [Overriding_indicator](./AA-8.3#S0234)s are new. These let the programmer state her overriding intentions to the compiler; if the compiler disagrees, an error will be produced rather than a hard to find bug. 


#### Wording Changes from Ada 2005

{AI95-0177-1} Expression functions can have overriding indicators. 

