---
sidebar_position:  9
---

# 8 Visibility Rules

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[The rules defining the scope of declarations and the rules defining which [identifier](S0002)s, [character_literal](S0012)s, and [operator_symbol](S0147)s are visible at (or from) various places in the text of the program are described in this section. The formulation of these rules uses the notion of a declarative region.

As explained in Section 3, a declaration declares a view of an entity and associates a defining name with that view. The view comprises an identification of the viewed entity, and possibly additional properties. A usage name denotes a declaration. It also denotes the view declared by that declaration, and denotes the entity of that view. Thus, two different usage names might denote two different views of the same entity; in this case they denote the same entity.] 

To be honest: In some cases, a usage name that denotes a declaration does not denote the view declared by that declaration, nor the entity of that view, but instead denotes a view of the current instance of the entity, and denotes the current instance of the entity. This sometimes happens when the usage name occurs inside the declarative region of the declaration. 


#### Wording Changes from Ada 83

We no longer define the term "basic operation;" thus we no longer have to worry about the visibility of them. Since they were essentially always visible in Ada 83, this change has no effect. The reason for this change is that the definition in Ada 83 was confusing, and not quite correct, and we found it difficult to fix. For example, one wonders why an [if_statement](S0131) was not a basic operation of type Boolean. For another example, one wonders what it meant for a basic operation to be "inherent in" something. Finally, this fixes the problem addressed by AI83-00027/07. 


## 8.1  Declarative Region


#### Static Semantics

For each of the following constructs, there is a portion of the program text called its declarative region, [within which nested declarations can occur]: 

any declaration, other than that of an enumeration type, that is not a completion [of a previous declaration];

a [block_statement](S0138);

a [loop_statement](S0135);

an [accept_statement](S0188);

an [exception_handler](S0232). 

The declarative region includes the text of the construct together with additional text determined [(recursively)], as follows: 

If a declaration is included, so is its completion, if any.

If the declaration of a library unit [(including Standard - see 10.1.1)] is included, so are the declarations of any child units [(and their completions, by the previous rule)]. The child declarations occur after the declaration.

If a [body_stub](S0224) is included, so is the corresponding [subunit](S0229).

If a [type_declaration](S0020) is included, then so is a corresponding [record_representation_clause](S0268), if any. 

Reason: This is so that the [component_declaration](S0067)s can be directly visible in the [record_representation_clause](S0268). 

The declarative region of a declaration is also called the declarative region of any view or entity declared by the declaration. 

Reason: The constructs that have declarative regions are the constructs that can have declarations nested inside them. Nested declarations are declared in that declarative region. The one exception is for enumeration literals; although they are nested inside an enumeration type declaration, they behave as if they were declared at the same level as the type. 

To be honest: A declarative region does not include [parent_unit_name](S0220)s. 

Ramification: A declarative region does not include [context_clause](S0221)s. 

A declaration occurs immediately within a declarative region if this region is the innermost declarative region that encloses the declaration (the immediately enclosing declarative region), not counting the declarative region (if any) associated with the declaration itself. 

Discussion: Don't confuse the declarative region of a declaration with the declarative region in which it immediately occurs. 

[ A declaration is local to a declarative region if the declaration occurs immediately within the declarative region.] [An entity is local to a declarative region if the entity is declared by a declaration that is local to the declarative region.] 

Ramification: "Occurs immediately within" and "local to" are synonyms (when referring to declarations).

Thus, "local to" applies to both declarations and entities, whereas "occurs immediately within" only applies to declarations. We use this term only informally; for cases where precision is required, we use the term "occurs immediately within", since it is less likely to cause confusion. 

A declaration is global to a declarative region if the declaration occurs immediately within another declarative region that encloses the declarative region. An entity is global to a declarative region if the entity is declared by a declaration that is global to the declarative region. 

NOTE 1   The children of a parent library unit are inside the parent's declarative region, even though they do not occur inside the parent's declaration or body. This implies that one can use (for example) "P.Q" to refer to a child of P whose defining name is Q, and that after "use P;" Q can refer (directly) to that child.

NOTE 2   As explained above and in 10.1.1, "Compilation Units - Library Units", all library units are descendants of Standard, and so are contained in the declarative region of Standard. They are not inside the declaration or body of Standard, but they are inside its declarative region.

NOTE 3   For a declarative region that comes in multiple parts, the text of the declarative region does not contain any text that might appear between the parts. Thus, when a portion of a declarative region is said to extend from one place to another in the declarative region, the portion does not contain any text that might appear between the parts of the declarative region. 

Discussion: It is necessary for the things that have a declarative region to include anything that contains declarations (except for enumeration type declarations). This includes any declaration that has a profile (that is, [subprogram_declaration](S0141), [subprogram_body](S0154), [entry_declaration](S0187), [subprogram_renaming_declaration](S0173), [formal_subprogram_declaration](S0258), access-to-subprogram [type_declaration](S0020)), anything that has a [discriminant_part](S0056) (that is, various kinds of [type_declaration](S0020)), anything that has a [component_list](S0065) (that is, record [type_declaration](S0020) and record extension [type_declaration](S0020)), and finally the declarations of task and protected units and packages. 


#### Wording Changes from Ada 83

It was necessary to extend Ada 83's definition of declarative region to take the following Ada 95 features into account: 

Child library units.

Derived types/type extensions - we need a declarative region for inherited components and also for new components.

All the kinds of types that allow discriminants.

Protected units.

Entries that have bodies instead of accept statements.

The [choice_parameter_specification](S0233) of an [exception_handler](S0232).

The formal parameters of access-to-subprogram types.

Renamings-as-body. 

Discriminated and access-to-subprogram type declarations need a declarative region. Enumeration type declarations cannot have one, because you don't have to say "Color.Red" to refer to the literal Red of Color. For other type declarations, it doesn't really matter whether or not there is an associated declarative region, so for simplicity, we give one to all types except enumeration types.

We now say that an [accept_statement](S0188) has its own declarative region, rather than being part of the declarative region of the [entry_declaration](S0187), so that declarative regions are properly nested regions of text, so that it makes sense to talk about "inner declarative regions", and "...extends to the end of a declarative region". Inside an [accept_statement](S0188), the [name](S0084) of one of the parameters denotes the [parameter_specification](S0152) of the [accept_statement](S0188), not that of the [entry_declaration](S0187). If the [accept_statement](S0188) is nested within a [block_statement](S0138), these [parameter_specification](S0152)s can hide declarations of the [block_statement](S0138). The semantics of such cases was unclear in RM83. 

To be honest: Unfortunately, we have the same problem for the entry name itself - it should denote the [accept_statement](S0188), but [accept_statement](S0188)s are not declarations. They should be, and they should hide the entry from all visibility within themselves. 

Note that we can't generalize this to entry_bodies, or other bodies, because the [declarative_part](S0079) of a body is not supposed to contain (explicit) homographs of things in the declaration. It works for [accept_statement](S0188)s only because an [accept_statement](S0188) does not have a [declarative_part](S0079).

To avoid confusion, we use the term "local to" only informally in Ada 95. Even RM83 used the term incorrectly (see, for example, RM83-12.3(13)).

In Ada 83, (root) library units were inside Standard; it was not clear whether the declaration or body of Standard was meant. In Ada 95, they are children of Standard, and so occur immediately within Standard's declarative region, but not within either the declaration or the body. (See RM83-8.6(2) and RM83-10.1.1(5).) 


## 8.2  Scope of Declarations

[For each declaration, the language rules define a certain portion of the program text called the scope of the declaration. The scope of a declaration is also called the scope of any view or entity declared by the declaration. Within the scope of an entity, and only there, there are places where it is legal to refer to the declared entity. These places are defined by the rules of visibility and overloading.] 


#### Static Semantics

The immediate scope of a declaration is a portion of the declarative region immediately enclosing the declaration. The immediate scope starts at the beginning of the declaration, except in the case of an overloadable declaration, in which case the immediate scope starts just after the place where the profile of the callable entity is determined (which is at the end of the _specification for the callable entity, or at the end of the [generic_instantiation](S0241) if an instance). The immediate scope extends to the end of the declarative region, with the following exceptions: 

Reason: The reason for making overloadable declarations with profiles special is to simplify compilation: until the compiler has determined the profile, it doesn't know which other declarations are homographs of this one, so it doesn't know which ones this one should hide. Without this rule, two passes over the _specification or [generic_instantiation](S0241) would be required to resolve names that denote things with the same name as this one. 

The immediate scope of a [library_item](S0216) includes only its semantic dependents. 

Reason: Section 10 defines only a partial ordering of [library_item](S0216)s. Therefore, it is a good idea to restrict the immediate scope (and the scope, defined below) to semantic dependents.

Consider also examples like this: 

```ada
package P is end P;

```

```ada
package P.Q is
    I : Integer := 0;
end P.Q;

```

```ada
with P;
package R is
    package X renames P;
    X.Q.I := 17; -- Illegal!
end R;

```

The scope of P.Q does not contain R. Hence, neither P.Q nor X.Q are visible within R. However, the name R.X.Q would be visible in some other library unit where both R and P.Q are visible (assuming R were made legal by removing the offending declaration). 

The immediate scope of a declaration in the private part of a library unit does not include the visible part of any public descendant of that library unit. 

Ramification: In other words, a declaration in the private part can be visible within the visible part, private part and body of a private child unit. On the other hand, such a declaration can be visible within only the private part and body of a public child unit. 

Reason: The purpose of this rule is to prevent children from giving private information to clients. 

Ramification: For a public child subprogram, this means that the parent's private part is not visible in the [formal_part](S0151)s of the declaration and of the body. This is true even for subprogram_bodies that are not completions. For a public child generic unit, it means that the parent's private part is not visible in the [generic_formal_part](S0239), as well as in the first list of [basic_declarative_item](S0081)s (for a generic package), or the [formal_part](S0151)(s) (for a generic subprogram). 

[The visible part of (a view of) an entity is a portion of the text of its declaration containing declarations that are visible from outside.] The private part of (a view of) an entity that has a visible part contains all declarations within the declaration of (the view of) the entity, except those in the visible part; [these are not visible from outside. Visible and private parts are defined only for these kinds of entities: callable entities, other program units, and composite types.] 

The visible part of a view of a callable entity is its profile.

The visible part of a composite type other than a task or protected type consists of the declarations of all components declared [(explicitly or implicitly)] within the [type_declaration](S0020).

The visible part of a generic unit includes the [generic_formal_part](S0239). For a generic package, it also includes the first list of [basic_declarative_item](S0081)s of the [package_specification](S0162). For a generic subprogram, it also includes the profile. 

Reason: Although there is no way to reference anything but the formals from outside a generic unit, they are still in the visible part in the sense that the corresponding declarations in an instance can be referenced (at least in some cases). In other words, these declarations have an effect on the outside world. The visible part of a generic unit needs to be defined this way in order to properly support the rule that makes a parent's private part invisible within a public child's visible part. 

Ramification: The visible part of an instance of a generic unit is as defined for packages and subprograms; it is not defined in terms of the visible part of a generic unit. 

[The visible part of a package, task unit, or protected unit consists of declarations in the program unit's declaration other than those following the reserved word private, if any; see 7.1 and 12.7 for packages, 9.1 for task units, and 9.4 for protected units.] 

The scope of a declaration always contains the immediate scope of the declaration. In addition, for a given declaration that occurs immediately within the visible part of an outer declaration, or is a public child of an outer declaration, the scope of the given declaration extends to the end of the scope of the outer declaration, except that the scope of a [library_item](S0216) includes only its semantic dependents. 

Ramification: Note the recursion. If a declaration appears in the visible part of a library unit, its scope extends to the end of the scope of the library unit, but since that only includes dependents of the declaration of the library unit, the scope of the inner declaration also only includes those dependents. If X renames library package P, which has a child Q, a [with_clause](S0223) mentioning P.Q is necessary to be able to refer to X.Q, even if P.Q is visible at the place where X is declared. 

The immediate scope of a declaration is also the immediate scope of the entity or view declared by the declaration. Similarly, the scope of a declaration is also the scope of the entity or view declared by the declaration. 

Ramification: The rule for immediate scope implies the following: 

If the declaration is that of a library unit, then the immediate scope includes the declarative region of the declaration itself, but not other places, unless they are within the scope of a [with_clause](S0223) that mentions the library unit.

It is necessary to attach the semantics of [with_clause](S0223)s to [immediate] scopes (as opposed to visibility), in order for various rules to work properly. A library unit should hide a homographic implicit declaration that appears in its parent, but only within the scope of a [with_clause](S0223) that mentions the library unit. Otherwise, we would violate the "legality determinable via semantic dependences" rule of 10, "Program Structure and Compilation Issues". The declaration of a library unit should be allowed to be a homograph of an explicit declaration in its parent's body, so long as that body does not mention the library unit in a [with_clause](S0223).

This means that one cannot denote the declaration of the library unit, but one might still be able to denote the library unit via another view.

A [with_clause](S0223) does not make the declaration of a library unit visible; the lack of a [with_clause](S0223) prevents it from being visible. Even if a library unit is mentioned in a [with_clause](S0223), its declaration can still be hidden.

The completion of the declaration of a library unit (assuming that's also a declaration) is not visible, neither directly nor by selection, outside that completion.

The immediate scope of a declaration immediately within the body of a library unit does not include any child of that library unit.

This is needed to prevent children from looking inside their parent's body. The children are in the declarative region of the parent, and they might be after the parent's body. Therefore, the scope of a declaration that occurs immediately within the body might include some children. 

NOTE   There are notations for denoting visible declarations that are not directly visible. For example, [parameter_specification](S0152)s are in the visible part of a [subprogram_declaration](S0141) so that they can be used in named-notation calls appearing outside the called subprogram. For another example, declarations of the visible part of a package can be denoted by expanded names appearing outside the package, and can be made directly visible by a [use_clause](S0166). 

Ramification: There are some obscure involving generics cases in which there is no such notation. See Section 12. 


#### Extensions to Ada 83

The fact that the immediate scope of an overloadable declaration does not include its profile is new to Ada 95. It replaces RM83-8.3(16), which said that within a subprogram specification and within the formal part of an entry declaration or accept statement, all declarations with the same designator as the subprogram or entry were hidden from all visibility. The RM83-8.3(16) rule seemed to be overkill, and created both implementation difficulties and unnecessary semantic complexity. 


#### Wording Changes from Ada 83

We no longer need to talk about the scope of notations, [identifier](S0002)s, [character_literal](S0012)s, and [operator_symbol](S0147)s.

The notion of "visible part" has been extended in Ada 95. The syntax of task and protected units now allows private parts, thus requiring us to be able to talk about the visible part as well. It was necessary to extend the concept to subprograms and to generic units, in order for the visibility rules related to child library units to work properly. It was necessary to define the concept separately for generic formal packages, since their visible part is slightly different from that of a normal package. Extending the concept to composite types made the definition of scope slightly simpler. We define visible part for some things elsewhere, since it makes a big difference to the user for those things. For composite types and subprograms, however, the concept is used only in arcane visibility rules, so we localize it to this clause.

In Ada 83, the semantics of [with_clause](S0223)s was described in terms of visibility. It is now described in terms of [immediate] scope.

We have clarified that the following is illegal (where Q and R are library units): 

```ada
package Q is
    I : Integer := 0;
end Q;

```

```ada
package R is
    package X renames Standard;
    X.Q.I := 17; -- Illegal!
end R;

```

even though Q is declared in the declarative region of Standard, because R does not mention Q in a [with_clause](S0223). 


## 8.3  Visibility

[ The visibility rules, given below, determine which declarations are visible and directly visible at each place within a program. The visibility rules apply to both explicit and implicit declarations.] 


#### Static Semantics

A declaration is defined to be directly visible at places where a [name](S0084) consisting of only an [identifier](S0002) or [operator_symbol](S0147) is sufficient to denote the declaration; that is, no [selected_component](S0091) notation or special context (such as preceding =&gt in a named association) is necessary to denote the declaration. A declaration is defined to be visible wherever it is directly visible, as well as at other places where some [name](S0084) (such as a [selected_component](S0091)) can denote the declaration.

The syntactic category [direct_name](S0085) is used to indicate contexts where direct visibility is required. The syntactic category [selector_name](S0092) is used to indicate contexts where visibility, but not direct visibility, is required.

There are two kinds of direct visibility: immediate visibility and use-visibility. A declaration is immediately visible at a place if it is directly visible because the place is within its immediate scope. A declaration is use-visible if it is directly visible because of a [use_clause](S0166) (see 8.4). Both conditions can apply.

A declaration can be hidden, either from direct visibility, or from all visibility, within certain parts of its scope. Where hidden from all visibility, it is not visible at all (neither using a [direct_name](S0085) nor a [selector_name](S0092)). Where hidden from direct visibility, only direct visibility is lost; visibility using a [selector_name](S0092) is still possible.

[ Two or more declarations are overloaded if they all have the same defining name and there is a place where they are all directly visible.] 

Ramification: Note that a [name](S0084) can have more than one possible interpretation even if it denotes a nonoverloadable entity. For example, if there are two functions F that return records, both containing a component called C, then the name F.C has two possible interpretations, even though component declarations are not overloadable. 

The declarations of callable entities [(including enumeration literals)] are overloadable[, meaning that overloading is allowed for them]. 

Ramification: A [generic_declaration](S0236) is not overloadable within its own [generic_formal_part](S0239). This follows from the rules about when a [name](S0084) denotes a current instance. See AI83-00286. This implies that within a [generic_formal_part](S0239), outer declarations with the same defining name are hidden from direct visibility. It also implies that if a generic formal parameter has the same defining name as the generic itself, the formal parameter hides the generic from direct visibility. 

Two declarations are homographs if they have the same defining name, and, if both are overloadable, their profiles are type conformant. [An inner declaration hides any outer homograph from direct visibility.]

Version=[5],Kind=(AddedNormal),Group=[S],Term=[overriding operation], Def=[an operation that replaces an inherited primitive operation], Note1=[Operations can be marked explicitly as overriding or not overriding.]

[Two homographs are not generally allowed immediately within the same declarative region unless one overrides the other (see Legality Rules below).] A declaration overrides another homograph that occurs immediately within the same declarative region in the following cases: 

An explicit declaration overrides an implicit declaration of a primitive subprogram, [regardless of which declaration occurs first]; 

Ramification: And regardless of whether the explicit declaration is overloadable or not. 

The "regardless of which declaration occurs first" is there because the explicit declaration could be a primitive subprogram of a partial view, and then the full view might inherit a homograph. We are saying that the explicit one wins (within its scope), even though the implicit one comes later.

If the overriding declaration is also a subprogram, then it is a primitive subprogram.

As explained in 7.3.1, "Private Operations", some inherited primitive subprograms are never declared. Such subprograms cannot be overridden, although they can be reached by dispatching calls in the case of a tagged type. 

The implicit declaration of an inherited operator overrides that of a predefined operator; 

Ramification: In a previous version of Ada 9X, we tried to avoid the notion of predefined operators, and say that they were inherited from some magical root type. However, this seemed like too much mechanism. Therefore, a type can have a predefined "+" as well as an inherited "+". The above rule says the inherited one wins.

The "regardless of which declaration occurs first" applies here as well, in the case where derived_type_declaration in the visible part of a public library unit derives from a private type declared in the parent unit, and the full view of the parent type has additional predefined operators, as explained in 7.3.1, "Private Operations". Those predefined operators can be overridden by inherited subprograms implicitly declared earlier. 

An implicit declaration of an inherited subprogram overrides a previous implicit declaration of an inherited subprogram.

[For an implicit declaration of a primitive subprogram in a generic unit, there is a copy of this declaration in an instance.] However, a whole new set of primitive subprograms is implicitly declared for each type declared within the visible part of the instance. These new declarations occur immediately after the type declaration, and override the copied ones. [The copied ones can be called only from within the instance; the new ones can be called only from outside the instance, although for tagged types, the body of a new one can be executed by a call to an old one.] 

Discussion: In addition, this is also stated redundantly (again), and is repeated, in 12.3, "Generic Instantiation". The rationale for the rule is explained there. 

A declaration is visible within its scope, except where hidden from all visibility, as follows: 

An overridden declaration is hidden from all visibility within the scope of the overriding declaration. 

Ramification: We have to talk about the scope of the overriding declaration, not its visibility, because it hides even when it is itself hidden.

Note that the scope of an explicit [subprogram_declaration](S0141) does not start until after its profile. 

A declaration is hidden from all visibility until the end of the declaration, except: 

For a record type or record extension, the declaration is hidden from all visibility only until the reserved word record;

For a [package_declaration](S0161), task declaration, protected declaration, [generic_package_declaration](S0238), or [subprogram_body](S0154), the declaration is hidden from all visibility only until the reserved word is of the declaration. 

Ramification: We're talking about the is of the construct itself, here, not some random is that might appear in a [generic_formal_part](S0239). 

If the completion of a declaration is a declaration, then within the scope of the completion, the first declaration is hidden from all visibility. Similarly, a [discriminant_specification](S0059) or [parameter_specification](S0152) is hidden within the scope of a corresponding [discriminant_specification](S0059) or [parameter_specification](S0152) of a corresponding completion, or of a corresponding [accept_statement](S0188). 

Ramification: This rule means, for example, that within the scope of a [full_type_declaration](S0021) that completes a [private_type_declaration](S0164), the name of the type will denote the [full_type_declaration](S0021), and therefore the full view of the type. On the other hand, if the completion is not a declaration, then it doesn't hide anything, and you can't denote it. 

The declaration of a library unit (including a [library_unit_renaming_declaration](S0218)) is hidden from all visibility except at places that are within its declarative region or within the scope of a [with_clause](S0223) that mentions it.[For each declaration or renaming of a generic unit as a child of some parent generic package, there is a corresponding declaration nested immediately within each instance of the parent.] Such a nested declaration is hidden from all visibility except at places that are within the scope of a [with_clause](S0223) that mentions the child.

Discussion: This is the rule that prevents [with_clause](S0223)s from being transitive; the [immediate] scope includes indirect semantic dependents. 

A declaration with a [defining_identifier](S0019) or [defining_operator_symbol](S0148) is immediately visible [(and hence directly visible)] within its immediate scope  except where hidden from direct visibility, as follows: 

A declaration is hidden from direct visibility within the immediate scope of a homograph of the declaration, if the homograph occurs within an inner declarative region;

A declaration is also hidden from direct visibility where hidden from all visibility. 


#### Name Resolution Rules

A [direct_name](S0085) shall resolve to denote a directly visible declaration whose defining name is the same as the [direct_name](S0085). A [selector_name](S0092) shall resolve to denote a visible declaration whose defining name is the same as the [selector_name](S0092). 

Discussion: "The same as" has the obvious meaning here, so for +, the possible interpretations are declarations whose defining name is "+" (an [operator_symbol](S0147)). 

These rules on visibility and direct visibility do not apply in a [context_clause](S0221), a [parent_unit_name](S0220), or a [pragma](S0016) that appears at the place of a [compilation_unit](S0215). For those contexts, see the rules in 10.1.6, "Environment-Level Visibility Rules". 

Ramification: Direct visibility is irrelevant for [character_literal](S0012)s. In terms of overload resolution [character_literal](S0012)s are similar to other literals, like null - see 4.2. For [character_literal](S0012)s, there is no need to worry about hiding, since there is no way to declare homographs. 


#### Legality Rules

An explicit declaration is illegal if there is a homograph occurring immediately within the same declarative region that is visible at the place of the declaration, and is not hidden from all visibility by the explicit declaration. Similarly, the [context_clause](S0221) for a [subunit](S0229) is illegal if it mentions (in a [with_clause](S0223)) some library unit, and there is a homograph of the library unit that is visible at the place of the corresponding stub, and the homograph and the mentioned library unit are both declared immediately within the same declarative region. These rules also apply to dispatching operations declared in the visible part of an instance of a generic unit. However, they do not apply to other overloadable declarations in an instance[; such declarations may have type conformant profiles in the instance, so long as the corresponding declarations in the generic were not type conformant]. 

Discussion: Normally, these rules just mean you can't explicitly declare two homographs immediately within the same declarative region. The wording is designed to handle the following special cases: 

If the second declaration completes the first one, the second declaration is legal.

If the body of a library unit contains an explicit homograph of a child of that same library unit, this is illegal only if the body mentions the child in its [context_clause](S0221), or if some subunit mentions the child. Here's an example: 

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

Note that we need to be careful which things we make "hidden from all visibility" versus which things we make simply illegal for names to denote. The distinction is subtle. The rules that disallow names denoting components within a type declaration (see 3.7) do not make the components invisible at those places, so that the above rule makes components with the same name illegal. The same is true for the rule that disallows names denoting formal parameters within a [formal_part](S0151) (see 6.1). 

Discussion: The part about instances is from AI83-00012. The reason it says "overloadable declarations" is because we don't want it to apply to type extensions that appear in an instance; components are not overloadable. 

NOTE 1   Visibility for compilation units follows from the definition of the environment in 10.1.4, except that it is necessary to apply a [with_clause](S0223) to obtain visibility to a [library_unit_declaration](S0217) or [library_unit_renaming_declaration](S0218).

NOTE 2   In addition to the visibility rules given above, the meaning of the occurrence of a [direct_name](S0085) or [selector_name](S0092) at a given place in the text can depend on the overloading rules (see 8.6).

NOTE 3   Not all contexts where an [identifier](S0002), [character_literal](S0012), or [operator_symbol](S0147) are allowed require visibility of a corresponding declaration. Contexts where visibility is not required are identified by using one of these three syntactic categories directly in a syntax rule, rather than using [direct_name](S0085) or [selector_name](S0092). 

Ramification: An [identifier](S0002), [character_literal](S0012) or [operator_symbol](S0147) that occurs in one of the following contexts is not required to denote a visible or directly visible declaration: 

a)A defining name.

b)The [identifier](S0002)s or [operator_symbol](S0147) that appear after the reserved word end in a [proper_body](S0083). Similarly for "end loop", etc.

c)An [attribute_designator](S0094).

d)A [pragma](S0016) [identifier](S0002).

e)A pragma_argument_[identifier](S0002).

f)An [identifier](S0002) specific to a pragma used in a pragma argument.

The visibility rules have nothing to do with the above cases; the meanings of such things are defined elsewhere. Reserved words are not [identifier](S0002)s; the visibility rules don't apply to them either.

Because of the way we have defined "declaration", it is possible for a usage name to denote a [subprogram_body](S0154), either within that body, or (for a nonlibrary unit) after it (since the body hides the corresponding declaration, if any). Other bodies do not work that way. Completions of [type_declaration](S0020)s and deferred constant declarations do work that way. [Accept_statement](S0188)s are never denoted, although the [parameter_specification](S0152)s in their profiles can be.

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

```ada
]

```


## 8.4  Use Clauses

[A [use_package_clause](S0167) achieves direct visibility of declarations that appear in the visible part of a package; a [use_type_clause](S0168) achieves direct visibility of the primitive operators of a type.] 


#### Language Design Principles

If and only if the visibility rules allow P.A, "use P;" should make A directly visible (barring name conflicts). This means, for example, that child library units, and generic formals of a formal package whose [formal_package_actual_part](S0262) is (&lt&gt), should be made visible by a [use_clause](S0166) for the appropriate package.

The rules for [use_clause](S0166)s were carefully constructed to avoid so-called Beaujolais effects, where the addition or removal of a single [use_clause](S0166), or a single declaration in a "use"d package, would change the meaning of a program from one legal interpretation to another. 


#### Syntax

use_clause ::= [use_package_clause](S0167) | [use_type_clause](S0168)

use_package_clause ::= use package_[name](S0084) {, package_[name](S0084)};

use_type_clause ::= use type [subtype_mark](S0025) {, [subtype_mark](S0025)};


#### Legality Rules

A package_[name](S0084) of a [use_package_clause](S0167) shall denote a package. 

Ramification: This includes formal packages. 


#### Static Semantics

For each [use_clause](S0166), there is a certain region of text called the scope of the [use_clause](S0166). For a [use_clause](S0166) within a [context_clause](S0221) of a [library_unit_declaration](S0217) or [library_unit_renaming_declaration](S0218), the scope is the entire declarative region of the declaration. For a [use_clause](S0166) within a [context_clause](S0221) of a body, the scope is the entire body [and any subunits (including multiply nested subunits). The scope does not include [context_clause](S0221)s themselves.]

For a [use_clause](S0166) immediately within a declarative region, the scope is the portion of the declarative region starting just after the [use_clause](S0166) and extending to the end of the declarative region. However, the scope of a [use_clause](S0166) in the private part of a library unit does not include the visible part of any public descendant of that library unit. 

Reason: The exception echoes the similar exception for "immediate scope (of a declaration)" (see 8.2). It makes [use_clause](S0166)s work like this: 

```ada
package P is
    type T is range 1..10;
end P;

```

```ada
with P;
package Parent is
private
    use P;
    X : T;
end Parent;

```

```ada
package Parent.Child is
    Y : T; -- Illegal!
    Z : P.T;
private
    W : T;
end Parent.Child;

```

The declaration of Y is illegal because the scope of the "use P" does not include that place, so T is not directly visible there. The declarations of X, Z, and W are legal. 

For each package denoted by a package_[name](S0084) of a [use_package_clause](S0167) whose scope encloses a place, each declaration that occurs immediately within the declarative region of the package is potentially use-visible at this place if the declaration is visible at this place. For each type T or T'Class determined by a [subtype_mark](S0025) of a [use_type_clause](S0168) whose scope encloses a place, the declaration of each primitive operator of type T is potentially use-visible at this place if its declaration is visible at this place. 

Ramification: Primitive subprograms whose defining name is an [identifier](S0002) are not made potentially visible by a [use_type_clause](S0168). A [use_type_clause](S0168) is only for operators.

The semantics described here should be similar to the semantics for expanded names given in 4.1.3, "Selected Components" so as to achieve the effect requested by the "principle of equivalence of [use_clause](S0166)s and [selected_component](S0091)s". Thus, child library units and generic formal parameters of a formal package are potentially use-visible when their enclosing package is use'd.

The "visible at that place" part implies that applying a [use_clause](S0166) to a parent unit does not make all of its children use-visible - only those that have been made visible by a [with_clause](S0223). It also implies that we don't have to worry about hiding in the definition of "directly visible" - a declaration cannot be use-visible unless it is visible.

Note that "use type T'Class;" is equivalent to "use type T;", which helps avoid breaking the generic contract model. 

A declaration is use-visible if it is potentially use-visible, except in these naming-conflict cases: 

A potentially use-visible declaration is not use-visible if the place considered is within the immediate scope of a homograph of the declaration.

Potentially use-visible declarations that have the same [identifier](S0002) are not use-visible unless each of them is an overloadable declaration. 

Ramification: Overloadable declarations don't cancel each other out, even if they are homographs, though if they are not distinguishable by formal parameter names or the presence or absence of [default_expression](S0060)s, any use will be ambiguous. We only mention [identifier](S0002)s here, because declarations named by [operator_symbol](S0147)s are always overloadable, and hence never cancel each other. Direct visibility is irrelevant for [character_literal](S0012)s. 


#### Dynamic Semantics

The elaboration of a [use_clause](S0166) has no effect. 


#### Examples

Example of a use clause in a context clause: 

```ada
with Ada.Calendar; use Ada;

```

Example of a use type clause: 

```ada
use type Rational_Numbers.Rational; -- see 7.1
Two_Thirds: Rational_Numbers.Rational := 2/3;

```

Ramification: In "use X, Y;", Y cannot refer to something made visible by the "use" of X. Thus, it's not (quite) equivalent to "use X; use Y;".

If a given declaration is already immediately visible, then a [use_clause](S0166) that makes it potentially use-visible has no effect. Therefore, a [use_type_clause](S0168) for a type whose declaration appears in a place other than the visible part of a package has no effect; it cannot make a declaration use-visible unless that declaration is already immediately visible.

"Use type S1;" and "use type S2;" are equivalent if S1 and S2 are both subtypes of the same type. In particular, "use type S;" and "use type S'Base;" are equivalent. 

Reason: We considered adding a rule that prevented several declarations of views of the same entity that all have the same semantics from cancelling each other out. For example, if a (possibly implicit) [subprogram_declaration](S0141) for "+" is potentially use-visible, and a fully conformant renaming of it is also potentially use-visible, then they (annoyingly) cancel each other out; neither one is use-visible. The considered rule would have made just one of them use-visible. We gave up on this idea due to the complexity of the rule. It would have had to account for both overloadable and nonoverloadable [renaming_declaration](S0169)s, the case where the rule should apply only to some subset of the declarations with the same defining name, and the case of [subtype_declaration](S0023)s (since they are claimed to be sufficient for renaming of subtypes). 


#### Extensions to Ada 83

The [use_type_clause](S0168) is new to Ada 95. 


#### Wording Changes from Ada 83

The phrase "omitting from this set any packages that enclose this place" is no longer necessary to avoid making something visible outside its scope, because we explicitly state that the declaration has to be visible in order to be potentially use-visible. 


## 8.5  Renaming Declarations

[A [renaming_declaration](S0169) declares another name for an entity, such as an object, exception, package, subprogram, entry, or generic unit. Alternatively, a [subprogram_renaming_declaration](S0173) can be the completion of a previous [subprogram_declaration](S0141).]

Version=[5],Kind=(AddedNormal),Group=[C],Term=[renaming], Def=[a declaration that does not define a new entity, but instead defines a new view of an existing entity] 


#### Syntax

renaming_declaration ::= 
      [object_renaming_declaration](S0170)
    | [exception_renaming_declaration](S0171)
    | [package_renaming_declaration](S0172)
    | [subprogram_renaming_declaration](S0173)
    | [generic_renaming_declaration](S0174)


#### Dynamic Semantics

The elaboration of a [renaming_declaration](S0169) evaluates the [name](S0084) that follows the reserved word renames and thereby determines the view and entity denoted by this name (the renamed view and renamed entity). [A [name](S0084) that denotes the [renaming_declaration](S0169) denotes (a new view of) the renamed entity.] 

NOTE 1   Renaming may be used to resolve name conflicts and to act as a shorthand. Renaming with a different [identifier](S0002) or [operator_symbol](S0147) does not hide the old [name](S0084); the new [name](S0084) and the old [name](S0084) need not be visible at the same places.

NOTE 2   A task or protected object that is declared by an explicit [object_declaration](S0029) can be renamed as an object. However, a single task or protected object cannot be renamed since the corresponding type is anonymous (meaning it has no nameable subtypes). For similar reasons, an object of an anonymous array or access type cannot be renamed.

NOTE 3   A subtype defined without any additional constraint can be used to achieve the effect of renaming another subtype (including a task or protected subtype) as in 

```ada
   subtype Mode is Ada.Text_IO.File_Mode;

```


#### Wording Changes from Ada 83

The second sentence of RM83-8.5(3), "At any point where a renaming declaration is visible, the identifier, or operator symbol of this declaration denotes the renamed entity." is incorrect. It doesn't say directly visible. Also, such an [identifier](S0002) might resolve to something else.

The verbiage about renamings being legal "only if exactly one...", which appears in RM83-8.5(4) (for objects) and RM83-8.5(7) (for subprograms) is removed, because it follows from the normal rules about overload resolution. For language lawyers, these facts are obvious; for programmers, they are irrelevant, since failing these tests is highly unlikely. 


### 8.5.1  Object Renaming Declarations

[An [object_renaming_declaration](S0170) is used to rename an object.] 


#### Syntax

object_renaming_declaration ::= [defining_identifier](S0019) : [subtype_mark](S0025) renames object_[name](S0084);


#### Name Resolution Rules

The type of the object_[name](S0084) shall resolve to the type determined by the [subtype_mark](S0025).

Reason: A previous version of Ada 9X used the usual "expected type" wording:
"The expected type for the object_[name](S0084) is that determined by the [subtype_mark](S0025)."
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

The renamed entity shall be an object.

The renamed entity shall not be a subcomponent that depends on discriminants of a variable whose nominal subtype is unconstrained, unless this subtype is indefinite, or the variable is aliased. A [slice](S0090) of an array shall not be renamed if this restriction disallows renaming of the array. 

Reason: This prevents renaming of subcomponents that might disappear, which might leave dangling references. Similar restrictions exist for the Access attribute.

Implementation Note: Note that if an implementation chooses to deallocate-then-reallocate on [assignment_statement](S0130)s assigning to unconstrained definite objects, then it cannot represent renamings and access values as simple addresses, because the above rule does not apply to all components of such an object. 

Ramification: If it is a generic formal object, then the assume-the-best or assume-the-worst rules are applied as appropriate. 


#### Static Semantics

An [object_renaming_declaration](S0170) declares a new view [of the renamed object] whose properties are identical to those of the renamed view. [Thus, the properties of the renamed object are not affected by the [renaming_declaration](S0169). In particular, its value and whether or not it is a constant are unaffected; similarly, the constraints that apply to an object are not affected by renaming (any constraint implied by the [subtype_mark](S0025) of the [object_renaming_declaration](S0170) is ignored).] 

Discussion: Because the constraints are ignored, it is a good idea to use the nominal subtype of the renamed object when writing an [object_renaming_declaration](S0170).


#### Examples

Example of renaming an object: 

```ada
declare
   L : Person renames Leftmost_Person; -- see 3.10.1
begin
   L.Age := L.Age + 1;
end;

```

```ada
Uno renames One;  -- see 3.3.2

```


#### Wording Changes from Ada 83

The phrase "subtype ... as defined in a corresponding object declaration, component declaration, or component subtype indication", from RM83-8.5(5), is incorrect in Ada 95; therefore we removed it. It is incorrect in the case of an object with an indefinite unconstrained nominal subtype. 


### 8.5.2  Exception Renaming Declarations

[An [exception_renaming_declaration](S0171) is used to rename an exception.] 


#### Syntax

exception_renaming_declaration ::= [defining_identifier](S0019) : exception renames exception_[name](S0084);


#### Legality Rules

The renamed entity shall be an exception. 


#### Static Semantics

An [exception_renaming_declaration](S0171) declares a new view [of the renamed exception]. 


#### Examples

Example of renaming an exception: 

```ada
EOF : exception renames Ada.IO_Exceptions.End_Error; -- see A.13

```


### 8.5.3  Package Renaming Declarations

[A [package_renaming_declaration](S0172) is used to rename a package.] 


#### Syntax

package_renaming_declaration ::= package [defining_program_unit_name](S0146) renames package_[name](S0084);


#### Legality Rules

The renamed entity shall be a package.


#### Static Semantics

A [package_renaming_declaration](S0172) declares a new view [of the renamed package].


#### Examples

Example of renaming a package: 

```ada
package TM renames Table_Manager;

```


### 8.5.4  Subprogram Renaming Declarations

A [subprogram_renaming_declaration](S0173) can serve as the completion of a [subprogram_declaration](S0141); such a [renaming_declaration](S0169) is called a renaming-as-body. A [subprogram_renaming_declaration](S0173) that is not a completion is called a renaming-as-declaration[, and is used to rename a subprogram (possibly an enumeration literal) or an entry]. 

Ramification: A renaming-as-body is a declaration, as defined in Section 3. 


#### Syntax

subprogram_renaming_declaration ::= [subprogram_specification](S0143) renames callable_entity_[name](S0084);


#### Name Resolution Rules

The expected profile for the callable_entity_[name](S0084) is the profile given in the [subprogram_specification](S0143). 


#### Legality Rules

The profile of a renaming-as-declaration shall be mode-conformant with that of the renamed callable entity. 

The profile of a renaming-as-body shall be subtype-conformant with that of the renamed callable entity, and shall conform fully to that of the declaration it completes. If the renaming-as-body completes that declaration before the subprogram it declares is frozen, the subprogram it declares takes its convention from the renamed subprogram; otherwise the convention of the renamed subprogram shall not be Intrinsic. 

Reason: The first part of the first sentence is to allow an implementation of a renaming-as-body as a single jump instruction to the target subprogram. Among other things, this prevents a subprogram from being completed with a renaming of an entry. (In most cases, the target of the jump can be filled in at link time. In some cases, such as a renaming of a name like "A(I).all", an indirect jump is needed. Note that the name is evaluated at renaming time, not at call time.)

The second part of the first sentence is the normal rule for completions of [subprogram_declaration](S0141)s. 

Ramification: An [entry_declaration](S0187), unlike a [subprogram_declaration](S0141), cannot be completed with a [renaming_declaration](S0169). Nor can a [generic_subprogram_declaration](S0237).

The syntax rules prevent a protected subprogram declaration from being completed by a renaming. This is fortunate, because it allows us to avoid worrying about whether the implicit protected object parameter of a protected operation is involved in the conformance rules. 

A [name](S0084) that denotes a formal parameter of the [subprogram_specification](S0143) is not allowed within the callable_entity_[name](S0084). 

Reason: This is to prevent things like this: 

```ada
function F(X : Integer) return Integer renames Table(X).all;

```

A similar rule in 6.1 forbids things like this: 

```ada
function F(X : Integer; Y : Integer := X) return Integer;

```


#### Static Semantics

A renaming-as-declaration declares a new view of the renamed entity. The profile of this new view takes its subtypes, parameter modes, and calling convention from the original profile of the callable entity, while taking the formal parameter [name](S0084)s and [default_expression](S0060)s from the profile given in the [subprogram_renaming_declaration](S0173). The new view is a function or procedure, never an entry. 

To be honest: When renaming an entry as a procedure, the compile-time rules apply as if the new view is a procedure, but the run-time semantics of a call are that of an entry call. 

Ramification: For example, it is illegal for the [entry_call_statement](S0194) of a [timed_entry_call](S0206) to call the new view. But what looks like a procedure call will do things like barrier waiting.


#### Dynamic Semantics

For a call on a renaming of a dispatching subprogram that is overridden, if the overriding occurred before the renaming, then the body executed is that of the overriding declaration, even if the overriding declaration is not visible at the place of the renaming; otherwise, the inherited or predefined subprogram is called. 

Discussion: Note that whether or not the renaming is itself primitive has nothing to do with the renamed subprogram.

Note that the above rule is only for tagged types.

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

NOTE 1   A procedure can only be renamed as a procedure. A function whose [defining_designator](S0145) is either an [identifier](S0002) or an [operator_symbol](S0147) can be renamed with either an [identifier](S0002) or an [operator_symbol](S0147); for renaming as an operator, the subprogram specification given in the [renaming_declaration](S0169) is subject to the rules given in 6.6 for operator declarations. Enumeration literals can be renamed as functions; similarly, [attribute_reference](S0093)s that denote functions (such as references to Succ and Pred) can be renamed as functions. An entry can only be renamed as a procedure; the new [name](S0084) is only allowed to appear in contexts that allow a procedure [name](S0084). An entry of a family can be renamed, but an entry family cannot be renamed as a whole.

NOTE 2   The operators of the root numeric types cannot be renamed because the types in the profile are anonymous, so the corresponding specifications cannot be written; the same holds for certain attributes, such as Pos.

NOTE 3   Calls with the new [name](S0084) of a renamed entry are [procedure_call_statement](S0155)s and are not allowed at places where the syntax requires an [entry_call_statement](S0194) in conditional_ and [timed_entry_call](S0206)s, nor in an [asynchronous_select](S0209); similarly, the Count attribute is not available for the new [name](S0084).

NOTE 4   The primitiveness of a renaming-as-declaration is determined by its profile, and by where it occurs, as for any declaration of (a view of) a subprogram; primitiveness is not determined by the renamed view. In order to perform a dispatching call, the subprogram name has to denote a primitive subprogram, not a nonprimitive renaming of a primitive subprogram. 

Reason: A [subprogram_renaming_declaration](S0173) could more properly be called renaming_as_subprogram_declaration, since you're renaming something as a subprogram, but you're not necessarily renaming a subprogram. But that's too much of a mouthful. Or, alternatively, we could call it a callable_entity_renaming_declaration, but that's even worse. Not only is it a mouthful, it emphasizes the entity being renamed, rather than the new view, which we think is a bad idea. We'll live with the oddity. 


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


### 8.5.5  Generic Renaming Declarations

[A [generic_renaming_declaration](S0174) is used to rename a generic unit.] 


#### Syntax

generic_renaming_declaration ::= 
    generic package	[defining_program_unit_name](S0146) renames generic_package_[name](S0084);
  | generic procedure	[defining_program_unit_name](S0146) renames generic_procedure_[name](S0084);
  | generic function	[defining_program_unit_name](S0146) renames generic_function_[name](S0084);


#### Legality Rules

The renamed entity shall be a generic unit of the corresponding kind. 


#### Static Semantics

A [generic_renaming_declaration](S0174) declares a new view [of the renamed generic unit]. 

NOTE 1   Although the properties of the new view are the same as those of the renamed view, the place where the [generic_renaming_declaration](S0174) occurs may affect the legality of subsequent renamings and instantiations that denote the [generic_renaming_declaration](S0174), in particular if the renamed generic unit is a library unit (see 10.1.1). 


#### Examples

Example of renaming a generic unit: 

```ada
generic package Enum_IO renames Ada.Text_IO.Enumeration_IO;  -- see A.10.10

```


#### Extensions to Ada 83

Renaming of generic units is new to Ada 95. It is particularly important for renaming child library units that are generic units. For example, it might be used to rename Numerics.Generic_Elementary_Functions as simply Generic_Elementary_Functions, to match the name for the corresponding Ada-83-based package. 


#### Wording Changes from Ada 83

The information in RM83-8.6, "The Package Standard", has been updated for the child unit feature, and moved to Annex A, except for the definition of "predefined type", which has been moved to 3.2.1. 


## 8.6  The Context of Overload Resolution

[ Because declarations can be overloaded, it is possible for an occurrence of a usage name to have more than one possible interpretation; in most cases, ambiguity is disallowed. This clause describes how the possible interpretations resolve to the actual interpretation.

Certain rules of the language (the Name Resolution Rules) are considered "overloading rules". If a possible interpretation violates an overloading rule, it is assumed not to be the intended interpretation; some other possible interpretation is assumed to be the actual interpretation. On the other hand, violations of nonoverloading rules do not affect which interpretation is chosen; instead, they cause the construct to be illegal. To be legal, there usually has to be exactly one acceptable interpretation of a construct that is a "complete context", not counting any nested complete contexts.

The syntax rules of the language and the visibility rules given in 8.3 determine the possible interpretations. Most type checking rules (rules that require a particular type, or a particular class of types, for example) are overloading rules. Various rules for the matching of formal and actual parameters are overloading rules.] 


#### Language Design Principles

The type resolution rules are intended to minimize the need for implicit declarations and preference rules associated with implicit conversion and dispatching operations. 


#### Name Resolution Rules

[Overload resolution is applied separately to each complete context, not counting inner complete contexts.] Each of the following constructs is a complete context: 

A [context_item](S0222).

A [declarative_item](S0080) or declaration. 

Ramification: A [loop_parameter_specification](S0137) is a declaration, and hence a complete context. 

A [statement](S0124).

A [pragma_argument_association](S0017). 

Reason: We would make it the whole [pragma](S0016), except that certain pragma arguments are allowed to be ambiguous, and ambiguity applies to a complete context. 

The [expression](S0108) of a [case_statement](S0133). 

Ramification: This means that the [expression](S0108) is resolved without looking at the choices. 

An (overall) interpretation of a complete context embodies its meaning, and includes the following information about the constituents of the complete context, not including constituents of inner complete contexts: 

for each constituent of the complete context, to which syntactic categories it belongs, and by which syntax rules; and 

Ramification: Syntactic categories is plural here, because there are lots of trivial productions - an [expression](S0108) might also be all of the following, in this order: [identifier](S0002), [name](S0084), [primary](S0113), [factor](S0112), [term](S0111), [simple_expression](S0110), and [relation](S0109). Basically, we're trying to capture all the information in the parse tree here, without using compiler-writer's jargon like "parse tree". 

for each usage name, which declaration it denotes (and, therefore, which view and which entity it denotes); and 

Ramification: In most cases, a usage name denotes the view declared by the denoted declaration. However, in certain cases, a usage name that denotes a declaration and appears inside the declarative region of that same declaration, denotes the current instance of the declaration. For example, within a [task_body](S0179), a usage name that denotes the [task_type_declaration](S0175) denotes the object containing the currently executing task, and not the task type declared by the declaration. 

for a complete context that is a [declarative_item](S0080), whether or not it is a completion of a declaration, and (if so) which declaration it completes. 

Ramification: Unfortunately, we are not confident that the above list is complete. We'll have to live with that. 

To be honest: For "possible" interpretations, the above information is tentative. 

Discussion: A possible interpretation (an input to overload resolution) contains information about what a usage name might denote, but what it actually does denote requires overload resolution to determine. Hence the term "tentative" is needed for possible interpretations; otherwise, the definition would be circular. 

A possible interpretation is one that obeys the syntax rules and the visibility rules. An acceptable interpretation is a possible interpretation that obeys the overloading rules[, that is, those rules that specify an expected type or expected profile, or specify how a construct shall resolve or be interpreted.] 

To be honest: One rule that falls into this category, but does not use the above-mentioned magic words, is the rule about numbers of parameter associations in a call (see 6.4). 

Ramification: The Name Resolution Rules are the ones that appear under the Name Resolution Rules heading. Some Syntax Rules are written in English, instead of BNF. No rule is a Syntax Rule or Name Resolution Rule unless it appears under the appropriate heading. 

The interpretation of a constituent of a complete context is determined from the overall interpretation of the complete context as a whole. [Thus, for example, "interpreted as a [function_call](S0156)", means that the construct's interpretation says that it belongs to the syntactic category [function_call](S0156).]

[Each occurrence of] a usage name denotes the declaration determined by its interpretation. It also denotes the view declared by its denoted declaration, except in the following cases: 

Ramification: As explained below, a pragma argument is allowed to be ambiguous, so it can denote several declarations, and all of the views declared by those declarations. 

If a usage name appears within the declarative region of a [type_declaration](S0020) and denotes that same [type_declaration](S0020), then it denotes the current instance of the type (rather than the type itself). The current instance of a type is the object or value of the type that is associated with the execution that evaluates the usage name. 

Reason: This is needed, for example, for references to the Access attribute from within the [type_declaration](S0020). Also, within a [task_body](S0179) or [protected_body](S0185), we need to be able to denote the current task or protected object. (For a [single_task_declaration](S0176) or [single_protected_declaration](S0181), the rule about current instances is not needed.) 

If a usage name appears within the declarative region of a [generic_declaration](S0236) (but not within its [generic_formal_part](S0239)) and it denotes that same [generic_declaration](S0236), then it denotes the current instance of the generic unit (rather than the generic unit itself). See also 12.3. 

To be honest: The current instance of a generic unit is the instance created by whichever [generic_instantiation](S0241) is of interest at any given time. 

Ramification: Within a [generic_formal_part](S0239), a [name](S0084) that denotes the [generic_declaration](S0236) denotes the generic unit, which implies that it is not overloadable. 

A usage name that denotes a view also denotes the entity of that view. 

Ramification: Usually, a usage name denotes only one declaration, and therefore one view and one entity. 

The expected type for a given [expression](S0108), [name](S0084), or other construct determines, according to the type resolution rules given below, the types considered for the construct during overload resolution. [ The type resolution rules provide support for class-wide programming, universal numeric literals, dispatching operations, and anonymous access types:] 

Ramification: Expected types are defined throughout the RM95. The most important definition is that, for a subprogram, the expected type for the actual parameter is the type of the formal parameter.

The type resolution rules are trivial unless either the actual or expected type is universal, class-wide, or of an anonymous access type. 

If a construct is expected to be of any type in a class of types, or of the universal or class-wide type for a class, then the type of the construct shall resolve to a type in that class or to a universal type that covers the class. 

Ramification: This matching rule handles (among other things) cases like the Val attribute, which denotes a function that takes a parameter of type universal_integer.

The last part of the rule, "or to a universal type that includes the class" implies that if the expected type for an expression is universal_fixed, then an expression whose type is universal_real (such as a real literal) is OK. 

If the expected type for a construct is a specific type T, then the type of the construct shall resolve either to T, or: 

Ramification: This rule is not intended to create a preference for the specific type - such a preference would cause Beaujolais effects. 

to T'Class; or 

Ramification: This will only be legal as part of a call on a dispatching operation; see 3.9.2, "Dispatching Operations of Tagged Types". Note that that rule is not a Name Resolution Rule. 

to a universal type that covers T; or

when T is an anonymous access type (see 3.10) with designated type D, to an access-to-variable type whose designated type is D'Class or is covered by D.

Ramification: Because it says "access-to-variable" instead of "access-to-object", two subprograms that differ only in that one has a parameter of an access-to-constant type, and the other has an access parameter, are distinguishable during overload resolution.

The case where the actual is access-to-D'Class will only be legal as part of a call on a dispatching operation; see 3.9.2, "Dispatching Operations of Tagged Types". Note that that rule is not a Name Resolution Rule. 

In certain contexts, [such as in a [subprogram_renaming_declaration](S0173),] the Name Resolution Rules define an expected profile for a given [name](S0084); in such cases, the [name](S0084) shall resolve to the name of a callable entity whose profile is type conformant with the expected profile. 

Ramification: The parameter and result subtypes are not used in overload resolution. Only type conformance of profiles is considered during overload resolution. Legality rules generally require at least mode-conformance in addition, but those rules are not used in overload resolution. 


#### Legality Rules

When the expected type for a construct is required to be a single type in a given class, the type expected for the construct shall be determinable solely from the context in which the construct appears, excluding the construct itself, but using the requirement that it be in the given class; the type of the construct is then this single expected type. Furthermore, the context shall not be one that expects any type in some class that contains types of the given class; in particular, the construct shall not be the operand of a [type_conversion](S0120).

Ramification: For example, the expected type for the literal null is required to be a single access type. But the expected type for the operand of a [type_conversion](S0120) is any type. Therefore, the literal null is not allowed as the operand of a [type_conversion](S0120). This is true even if there is only one access type in scope. The reason for these rules is so that the compiler will not have to search "everywhere" to see if there is exactly one type in a class in scope. 

A complete context shall have at least one acceptable interpretation; if there is exactly one, then that one is chosen. 

Ramification: This, and the rule below about ambiguity, are the ones that suck in all the Syntax Rules and Name Resolution Rules as compile-time rules. Note that this and the ambiguity rule have to be Legality Rules. 

There is a preference for the primitive operators (and [range](S0034)s) of the root numeric types root_integer and root_real. In particular, if two acceptable interpretations of a constituent of a complete context differ only in that one is for a primitive operator (or [range](S0034)) of the type root_integer or root_real, and the other is not, the interpretation using the primitive operator (or [range](S0034)) of the root numeric type is preferred.

Reason: The reason for this preference is so that expressions involving literals and named numbers can be unambiguous. For example, without the preference rule, the following would be ambiguous: 

```ada
N : constant := 123;
if N &gt 100 then -- Preference for root_integer "&lt" operator.
    ...
end if;

```

For a complete context, if there is exactly one overall acceptable interpretation where each constituent's interpretation is the same as or preferred (in the above sense) over those in all other overall acceptable interpretations, then that one overall acceptable interpretation is chosen. Otherwise, the complete context is ambiguous.

A complete context other than a [pragma_argument_association](S0017) shall not be ambiguous.

A complete context that is a [pragma_argument_association](S0017) is allowed to be ambiguous (unless otherwise specified for the particular pragma), but only if every acceptable interpretation of the pragma argument is as a [name](S0084) that statically denotes a callable entity. Such a [name](S0084) denotes all of the declarations determined by its interpretations, and all of the views declared by these declarations. 

Ramification: This applies to Inline, Suppress, Import, Export, and Convention [pragma](S0016)s. For example, it is OK to say "pragma Suppress(Elaboration_Check, On =&gt P.Q);", even if there are two directly visible P's, and there are two Q's declared in the visible part of each P. In this case, P.Q denotes four different declarations. This rule also applies to certain pragmas defined in the Specialized Needs Annexes. It almost applies to Pure, Elaborate_Body, and Elaborate_All [pragma](S0016)s, but those can't have overloading for other reasons. 

Note that if a pragma argument denotes a call to a callable entity, rather than the entity itself, this exception does not apply, and ambiguity is disallowed.

Note that we need to carefully define which pragma-related rules are Name Resolution Rules, so that, for example, a [pragma](S0016) Inline does not pick up subprograms declared in enclosing declarative regions, and therefore make itself illegal.

We say "statically denotes" in the above rule in order to avoid having to worry about how many times the [name](S0084) is evaluated, in case it denotes more than one callable entity. 

NOTE   If a usage name has only one acceptable interpretation, then it denotes the corresponding entity. However, this does not mean that the usage name is necessarily legal since other requirements exist which are not considered for overload resolution; for example, the fact that an expression is static, whether an object is constant, mode and subtype conformance rules, freezing rules, order of elaboration, and so on.

Similarly, subtypes are not considered for overload resolution (the violation of a constraint does not make a program illegal but raises an exception during program execution). 


#### Incompatibilities With Ada 83

The new preference rule for operators of root numeric types is upward incompatible, but only in cases that involved Beaujolais effects in Ada 83. Such cases are ambiguous in Ada 95. 


#### Extensions to Ada 83

The rule that allows an expected type to match an actual expression of a universal type, in combination with the new preference rule for operators of root numeric types, subsumes the Ada 83 "implicit conversion" rules for universal types. 


#### Wording Changes from Ada 83

In Ada 83, it is not clear what the "syntax rules" are. AI83-00157 states that a certain textual rule is a syntax rule, but it's still not clear how one tells in general which textual rules are syntax rules. We have solved the problem by stating exactly which rules are syntax rules - the ones that appear under the "Syntax" heading.

RM83 has a long list of the "forms" of rules that are to be used in overload resolution (in addition to the syntax rules). It is not clear exactly which rules fall under each form. We have solved the problem by explicitly marking all rules that are used in overload resolution. Thus, the list of kinds of rules is unnecessary. It is replaced with some introductory (intentionally vague) text explaining the basic idea of what sorts of rules are overloading rules.

It is not clear from RM83 what information is embodied in a "meaning" or an "interpretation". "Meaning" and "interpretation" were intended to be synonymous; we now use the latter only in defining the rules about overload resolution. "Meaning" is used only informally. This clause attempts to clarify what is meant by "interpretation".

For example, RM83 does not make it clear that overload resolution is required in order to match subprogram_bodies with their corresponding declarations (and even to tell whether a given [subprogram_body](S0154) is the completion of a previous declaration). Clearly, the information needed to do this is part of the "interpretation" of a [subprogram_body](S0154). The resolution of such things is defined in terms of the "expected profile" concept. Ada 95 has some new cases where expected profiles are needed - the resolution of P'Access, where P might denote a subprogram, is an example.

RM83-8.7(2) might seem to imply that an interpretation embodies information about what is denoted by each usage name, but not information about which syntactic category each construct belongs to. However, it seems necessary to include such information, since the Ada grammar is highly ambiguous. For example, X(Y) might be a [function_call](S0156) or an [indexed_component](S0089), and no context-free/syntactic information can tell the difference. It seems like we should view X(Y) as being, for example, "interpreted as a [function_call](S0156)" (if that's what overload resolution decides it is). Note that there are examples where the denotation of each usage name does not imply the syntactic category. However, even if that were not true, it seems that intuitively, the interpretation includes that information. Here's an example: 

```ada
type T;
type A is access T;
type T is array(Integer range 1..10) of A;
I : Integer := 3;
function F(X : Integer := 7) return A;
Y : A := F(I); -- Ambiguous? (We hope so.)

```

Consider the declaration of Y (a complete context). In the above example, overload resolution can easily determine the declaration, and therefore the entity, denoted by Y, A, F, and I. However, given all of that information, we still don't know whether F(I) is a [function_call](S0156) or an [indexed_component](S0089) whose prefix is a [function_call](S0156). (In the latter case, it is equivalent to F(7).all(I).)

It seems clear that the declaration of Y ought to be considered ambiguous. We describe that by saying that there are two interpretations, one as a [function_call](S0156), and one as an [indexed_component](S0089). These interpretations are both acceptable to the overloading rules. Therefore, the complete context is ambiguous, and therefore illegal.

It is the intent that the Ada 95 preference rule for root numeric operators is more locally enforceable than that of RM83-4.6(15). It should also eliminate interpretation shifts due to the addition or removal of a [use_clause](S0166) (the so called Beaujolais effect).

RM83-8.7 seems to be missing some complete contexts, such as [pragma_argument_association](S0017)s, [declarative_item](S0080)s that are not declarations or [representation_clause](S0263)s, and [context_item](S0222)s. We have added these, and also replaced the "must be determinable" wording of RM83-5.4(3) with the notion that the expression of a [case_statement](S0133) is a complete context.

Cases like the Val attribute are now handled using the normal type resolution rules, instead of having special cases that explicitly allow things like "any integer type". 

