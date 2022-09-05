---
sidebar_position:  66
---

# 8.2  Scope of Declarations

[For each declaration, the language rules define a certain portion of the program text called the scope of the declaration. The scope of a declaration is also called the scope of any view or entity declared by the declaration. Within the scope of an entity, and only there, there are places where it is legal to refer to the declared entity. These places are defined by the rules of visibility and overloading.] 


#### Static Semantics

The immediate scope of a declaration is a portion of the declarative region immediately enclosing the declaration. The immediate scope starts at the beginning of the declaration, except in the case of an overloadable declaration, in which case the immediate scope starts just after the place where the profile of the callable entity is determined (which is at the end of the _specification for the callable entity, or at the end of the [generic_instantiation](./AA-12.3#S0315) if an instance). The immediate scope extends to the end of the declarative region, with the following exceptions: 

Reason: The reason for making overloadable declarations with profiles special is to simplify compilation: until the compiler has determined the profile, it doesn't know which other declarations are homographs of this one, so it doesn't know which ones this one should hide. Without this rule, two passes over the _specification or [generic_instantiation](./AA-12.3#S0315) would be required to resolve names that denote things with the same name as this one. 

The immediate scope of a [library_item](./AA-10.1#S0287) includes only its semantic dependents. 

Reason: {AI05-0299-1} Clause 10 defines only a partial ordering of [library_item](./AA-10.1#S0287)s. Therefore, it is a good idea to restrict the immediate scope (and the scope, defined below) to semantic dependents.

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
    J : Integer := X.Q.I; -- Illegal!
end R;

```

The scope of P.Q does not contain R. Hence, neither P.Q nor X.Q are visible within R. However, the name R.X.Q would be visible in some other library unit where both R and P.Q are visible (assuming R were made legal by removing the offending declaration). 

Ramification: {AI95-00217-06} This rule applies to limited views as well as "normal" library items. In that case, the semantic dependents are the units that have a [limited_with_clause](./AA-10.1#S0295) for the limited view. 

The immediate scope of a declaration in the private part of a library unit does not include the visible part of any public descendant of that library unit. 

Ramification: In other words, a declaration in the private part can be visible within the visible part, private part and body of a private child unit. On the other hand, such a declaration can be visible within only the private part and body of a public child unit. 

Reason: The purpose of this rule is to prevent children from giving private information to clients. 

Ramification: {AI95-00231-01} For a public child subprogram, this means that the parent's private part is not visible in the profile of the declaration and of the body. This is true even for subprogram_bodies that are not completions. For a public child generic unit, it means that the parent's private part is not visible in the [generic_formal_part](./AA-12.1#S0313), as well as in the first list of [basic_declarative_item](./AA-3.11#S0088)s (for a generic package), or the (syntactic) profile (for a generic subprogram). 

[The visible part of (a view of) an entity is a portion of the text of its declaration containing declarations that are visible from outside.] The private part of (a view of) an entity that has a visible part contains all declarations within the declaration of (the view of) the entity, except those in the visible part; [these are not visible from outside. Visible and private parts are defined only for these kinds of entities: callable entities, other program units, and composite types.] 

The visible part of a view of a callable entity is its profile.

The visible part of a composite type other than a task or protected type consists of the declarations of all components declared [(explicitly or implicitly)] within the [type_declaration](./AA-3.2#S0023).

The visible part of a generic unit includes the [generic_formal_part](./AA-12.1#S0313). For a generic package, it also includes the first list of [basic_declarative_item](./AA-3.11#S0088)s of the [package_specification](./AA-7.1#S0230). For a generic subprogram, it also includes the profile. 

Reason: Although there is no way to reference anything but the formals from outside a generic unit, they are still in the visible part in the sense that the corresponding declarations in an instance can be referenced (at least in some cases). In other words, these declarations have an effect on the outside world. The visible part of a generic unit needs to be defined this way in order to properly support the rule that makes a parent's private part invisible within a public child's visible part. 

Ramification: The visible part of an instance of a generic unit is as defined for packages and subprograms; it is not defined in terms of the visible part of a generic unit. 

[The visible part of a package, task unit, or protected unit consists of declarations in the program unit's declaration other than those following the reserved word private, if any; see 7.1 and 12.7 for packages, 9.1 for task units, and 9.4 for protected units.] 

The scope of a declaration always contains the immediate scope of the declaration. In addition, for a given declaration that occurs immediately within the visible part of an outer declaration, or is a public child of an outer declaration, the scope of the given declaration extends to the end of the scope of the outer declaration, except that the scope of a [library_item](./AA-10.1#S0287) includes only its semantic dependents. 

Ramification: Note the recursion. If a declaration appears in the visible part of a library unit, its scope extends to the end of the scope of the library unit, but since that only includes dependents of the declaration of the library unit, the scope of the inner declaration also only includes those dependents. If X renames library package P, which has a child Q, a [with_clause](./AA-10.1#S0294) mentioning P.Q is necessary to be able to refer to X.Q, even if P.Q is visible at the place where X is declared. 

{AI95-00408-01} {AI05-0183-1} The scope of an [attribute_definition_clause](./AA-13.3#S0349) is identical to the scope of a declaration that would occur at the point of the [attribute_definition_clause](./AA-13.3#S0349). The scope of an [aspect_specification](./AA-13.1#S0346) is identical to the scope of the associated declaration.

The immediate scope of a declaration is also the immediate scope of the entity or view declared by the declaration. Similarly, the scope of a declaration is also the scope of the entity or view declared by the declaration. 

Ramification: The rule for immediate scope implies the following: 

If the declaration is that of a library unit, then the immediate scope includes the declarative region of the declaration itself, but not other places, unless they are within the scope of a [with_clause](./AA-10.1#S0294) that mentions the library unit.

It is necessary to attach the semantics of [with_clause](./AA-10.1#S0294)s to [immediate] scopes (as opposed to visibility), in order for various rules to work properly. A library unit should hide a homographic implicit declaration that appears in its parent, but only within the scope of a [with_clause](./AA-10.1#S0294) that mentions the library unit. Otherwise, we would violate the "legality determinable via semantic dependences" rule of 10, "Program Structure and Compilation Issues". The declaration of a library unit should be allowed to be a homograph of an explicit declaration in its parent's body, so long as that body does not mention the library unit in a [with_clause](./AA-10.1#S0294).

This means that one cannot denote the declaration of the library unit, but one might still be able to denote the library unit via another view.

A [with_clause](./AA-10.1#S0294) does not make the declaration of a library unit visible; the lack of a [with_clause](./AA-10.1#S0294) prevents it from being visible. Even if a library unit is mentioned in a [with_clause](./AA-10.1#S0294), its declaration can still be hidden.

The completion of the declaration of a library unit (assuming that's also a declaration) is not visible, neither directly nor by selection, outside that completion.

The immediate scope of a declaration immediately within the body of a library unit does not include any child of that library unit.

This is needed to prevent children from looking inside their parent's body. The children are in the declarative region of the parent, and they might be after the parent's body. Therefore, the scope of a declaration that occurs immediately within the body might include some children. 

{AI12-0003-1} The immediate scope of a pragma that is not used as a configuration pragma is defined to be the region extending from immediately after the pragma to the end of the declarative region immediately enclosing the pragma. 

NOTE   {AI05-0299-1} There are notations for denoting visible declarations that are not directly visible. For example, [parameter_specification](./AA-6.1#S0207)s are in the visible part of a [subprogram_declaration](./AA-6.1#S0195) so that they can be used in named-notation calls appearing outside the called subprogram. For another example, declarations of the visible part of a package can be denoted by expanded names appearing outside the package, and can be made directly visible by a [use_clause](./AA-8.4#S0235). 

Ramification: {AI95-00114-01} {AI05-0299-1} There are some obscure cases involving generics in which there is no such notation. See Clause 12. 


#### Extensions to Ada 83

The fact that the immediate scope of an overloadable declaration does not include its profile is new to Ada 95. It replaces RM83-8.3(16), which said that within a subprogram specification and within the formal part of an entry declaration or accept statement, all declarations with the same designator as the subprogram or entry were hidden from all visibility. The RM83-8.3(16) rule seemed to be overkill, and created both implementation difficulties and unnecessary semantic complexity. 


#### Wording Changes from Ada 83

We no longer need to talk about the scope of notations, [identifier](./AA-2.3#S0002)s, [character_literal](./AA-2.5#S0015)s, and [operator_symbol](./AA-6.1#S0202)s.

{AI05-0299-1} The notion of "visible part" has been extended in Ada 95. The syntax of task and protected units now allows private parts, thus requiring us to be able to talk about the visible part as well. It was necessary to extend the concept to subprograms and to generic units, in order for the visibility rules related to child library units to work properly. It was necessary to define the concept separately for generic formal packages, since their visible part is slightly different from that of a normal package. Extending the concept to composite types made the definition of scope slightly simpler. We define visible part for some things elsewhere, since it makes a big difference to the user for those things. For composite types and subprograms, however, the concept is used only in arcane visibility rules, so we localize it to this subclause.

In Ada 83, the semantics of [with_clause](./AA-10.1#S0294)s was described in terms of visibility. It is now described in terms of [immediate] scope.

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

even though Q is declared in the declarative region of Standard, because R does not mention Q in a [with_clause](./AA-10.1#S0294). 


#### Wording Changes from Ada 95

{AI95-00408-01} The scope of an [attribute_definition_clause](./AA-13.3#S0349) is defined so that it can be used to define the visibility of such a clause, so that can be used by the stream attribute availability rules (see 13.13.2). 


#### Wording Changes from Ada 2005

{AI05-0183-1} The scope of an [aspect_specification](./AA-13.1#S0346) is defined for similar reasons that it was defined for [attribute_definition_clause](./AA-13.3#S0349)s. 


#### Wording Changes from Ada 2012

{AI12-0003-1} The immediate scope of a [pragma](./AA-2.8#S0019) is defined as it is used in other rules in the Reference Manual. 

