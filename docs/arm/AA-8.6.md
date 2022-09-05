---
sidebar_position:  70
---

# 8.6  The Context of Overload Resolution

{AI05-0299-1} [ Because declarations can be overloaded, it is possible for an occurrence of a usage name to have more than one possible interpretation; in most cases, ambiguity is disallowed. This subclause describes how the possible interpretations resolve to the actual interpretation.

Certain rules of the language (the Name Resolution Rules) are considered "overloading rules". If a possible interpretation violates an overloading rule, it is assumed not to be the intended interpretation; some other possible interpretation is assumed to be the actual interpretation. On the other hand, violations of nonoverloading rules do not affect which interpretation is chosen; instead, they cause the construct to be illegal. To be legal, there usually has to be exactly one acceptable interpretation of a construct that is a "complete context", not counting any nested complete contexts.

The syntax rules of the language and the visibility rules given in 8.3 determine the possible interpretations. Most type checking rules (rules that require a particular type, or a particular class of types, for example) are overloading rules. Various rules for the matching of formal and actual parameters are overloading rules.] 


#### Language Design Principles

The type resolution rules are intended to minimize the need for implicit declarations and preference rules associated with implicit conversion and dispatching operations. 


#### Name Resolution Rules

[Overload resolution is applied separately to each complete context, not counting inner complete contexts.] Each of the following constructs is a complete context: 

A [context_item](./AA-10.1#S0293).

A [declarative_item](./AA-3.11#S0087) or declaration. 

Ramification: A [loop_parameter_specification](./AA-5.5#S0181) is a declaration, and hence a complete context. 

A [statement](./AA-5.1#S0167).

A [pragma_argument_association](./AA-2.8#S0020). 

Reason: We would make it the whole [pragma](./AA-2.8#S0019), except that certain pragma arguments are allowed to be ambiguous, and ambiguity applies to a complete context. 

{AI12-0040-1} The selecting_[expression](./AA-4.4#S0132) of a [case_statement](./AA-5.4#S0176) or [case_expression](./AA-4.5#S0151). 

Ramification: This means that the [expression](./AA-4.4#S0132) is resolved without looking at the choices. 

{AI12-0125-3} The variable_[name](./AA-4.1#S0091) of an [assignment_statement](./AA-5.2#S0173) A, if the [expression](./AA-4.4#S0132) of A contains one or more [target_name](./AA-5.2#S0174)s. 

An (overall) interpretation of a complete context embodies its meaning, and includes the following information about the constituents of the complete context, not including constituents of inner complete contexts: 

for each constituent of the complete context, to which syntactic categories it belongs, and by which syntax rules; and 

Ramification: Syntactic categories is plural here, because there are lots of trivial productions - an [expression](./AA-4.4#S0132) might also be all of the following, in this order: [identifier](./AA-2.3#S0002), [name](./AA-4.1#S0091), [primary](./AA-4.4#S0141), [factor](./AA-4.4#S0140), [term](./AA-4.4#S0139), [simple_expression](./AA-4.4#S0138), and [relation](./AA-4.4#S0135). Basically, we're trying to capture all the information in the parse tree here, without using compiler-writer's jargon like "parse tree". 

for each usage name, which declaration it denotes (and, therefore, which view and which entity it denotes); and 

Ramification: {AI95-00382-01} In most cases, a usage name denotes the view declared by the denoted declaration. However, in certain cases, a usage name that denotes a declaration and appears inside the declarative region of that same declaration, denotes the current instance of the declaration. For example, within a [task_body](./AA-9.1#S0248) other than in an [access_definition](./AA-3.10#S0084), a usage name that denotes the [task_type_declaration](./AA-9.1#S0244) denotes the object containing the currently executing task, and not the task type declared by the declaration. 

for a complete context that is a [declarative_item](./AA-3.11#S0087), whether or not it is a completion of a declaration, and (if so) which declaration it completes. 

Ramification: Unfortunately, we are not confident that the above list is complete. We'll have to live with that. 

To be honest: For "possible" interpretations, the above information is tentative. 

Discussion: A possible interpretation (an input to overload resolution) contains information about what a usage name might denote, but what it actually does denote requires overload resolution to determine. Hence the term "tentative" is needed for possible interpretations; otherwise, the definition would be circular. 

A possible interpretation is one that obeys the syntax rules and the visibility rules. An acceptable interpretation is a possible interpretation that obeys the overloading rules[, that is, those rules that specify an expected type or expected profile, or specify how a construct shall resolve or be interpreted.] 

To be honest: One rule that falls into this category, but does not use the above-mentioned magic words, is the rule about numbers of parameter associations in a call (see 6.4). 

Ramification: The Name Resolution Rules are the ones that appear under the Name Resolution Rules heading. Some Syntax Rules are written in English, instead of BNF. No rule is a Syntax Rule or Name Resolution Rule unless it appears under the appropriate heading. 

The interpretation of a constituent of a complete context is determined from the overall interpretation of the complete context as a whole. [Thus, for example, "interpreted as a [function_call](./AA-6.4#S0218)", means that the construct's interpretation says that it belongs to the syntactic category [function_call](./AA-6.4#S0218).]

[Each occurrence of] a usage name denotes the declaration determined by its interpretation. It also denotes the view declared by its denoted declaration, except in the following cases: 

Ramification: As explained below, a pragma argument is allowed to be ambiguous, so it can denote several declarations, and all of the views declared by those declarations. 

{AI95-00382-01} {AI05-0287-1} If a usage name appears within the declarative region of a [type_declaration](./AA-3.2#S0023) and denotes that same [type_declaration](./AA-3.2#S0023), then it denotes the current instance of the type (rather than the type itself); the current instance of a type is the object or value of the type that is associated with the execution that evaluates the usage name.  Similarly, if a usage name appears within the declarative region of a [subtype_declaration](./AA-3.2#S0026) and denotes that same [subtype_declaration](./AA-3.2#S0026), then it denotes the current instance of the subtype. These rules do not apply if the usage name appears within the [subtype_mark](./AA-3.2#S0028) of an [access_definition](./AA-3.10#S0084) for an access-to-object type, or within the subtype of a parameter or result of an access-to-subprogram type. 

Reason: {AI95-00382-01} This is needed, for example, for references to the Access attribute from within the [type_declaration](./AA-3.2#S0023). Also, within a [task_body](./AA-9.1#S0248) or [protected_body](./AA-9.4#S0254), we need to be able to denote the current task or protected object. (For a [single_task_declaration](./AA-9.1#S0245) or [single_protected_declaration](./AA-9.4#S0250), the rule about current instances is not needed.) We exclude anonymous access types so that they can be used to create self-referencing types in the natural manner (otherwise such types would be illegal). 

Discussion: {AI95-00382-01} The phrase "within the [subtype_mark](./AA-3.2#S0028)" in the "this rule does not apply" part is intended to cover a case like access T'Class appearing within the declarative region of T: here T denotes the type, not the current instance. 

{AI12-0068-1} {AI12-0324-1} {AI12-0427-1} Within an [aspect_specification](./AA-13.1#S0346) for a type or subtype, the current instance represents a value of the type; it is not an object. Unless otherwise specified, the nominal subtype of this value is given by the subtype itself (the first subtype in the case of a [type_declaration](./AA-3.2#S0023)), prior to applying any predicate specified directly on the type or subtype. If the type or subtype is by-reference, the associated object of the value is the object associated (see 6.2) with the evaluation of the usage name.

Ramification: For the purposes of Legality Rules, the current instance acts as a value within an [aspect_specification](./AA-13.1#S0346). It might really be an object (and has to be for a by-reference type), but that isn't discoverable by direct use of the name of the current instance. 

If a usage name appears within the declarative region of a [generic_declaration](./AA-12.1#S0310) (but not within its [generic_formal_part](./AA-12.1#S0313)) and it denotes that same [generic_declaration](./AA-12.1#S0310), then it denotes the current instance of the generic unit (rather than the generic unit itself). See also 12.3. 

To be honest: The current instance of a generic unit is the instance created by whichever [generic_instantiation](./AA-12.3#S0315) is of interest at any given time. 

Ramification: Within a [generic_formal_part](./AA-12.1#S0313), a [name](./AA-4.1#S0091) that denotes the [generic_declaration](./AA-12.1#S0310) denotes the generic unit, which implies that it is not overloadable. 

A usage name that denotes a view also denotes the entity of that view. 

Ramification: Usually, a usage name denotes only one declaration, and therefore one view and one entity. 

{AI95-00231-01} The expected type for a given [expression](./AA-4.4#S0132), [name](./AA-4.1#S0091), or other construct determines, according to the type resolution rules given below, the types considered for the construct during overload resolution. [ The type resolution rules provide support for class-wide programming, universal literals, dispatching operations, and anonymous access types:] 

Ramification: Expected types are defined throughout the RM95. The most important definition is that, for a subprogram, the expected type for the actual parameter is the type of the formal parameter.

The type resolution rules are trivial unless either the actual or expected type is universal, class-wide, or of an anonymous access type. 

If a construct is expected to be of any type in a class of types, or of the universal or class-wide type for a class, then the type of the construct shall resolve to a type in that class or to a universal type that covers the class. 

Ramification: This matching rule handles (among other things) cases like the Val attribute, which denotes a function that takes a parameter of type universal_integer.

The last part of the rule, "or to a universal type that covers the class" implies that if the expected type for an expression is universal_fixed, then an expression whose type is universal_real (such as a real literal) is OK. 

If the expected type for a construct is a specific type T, then the type of the construct shall resolve either to T, or: 

Ramification: This rule is not intended to create a preference for the specific type - such a preference would cause Beaujolais effects. 

to T'Class; or 

Ramification: This will only be legal as part of a call on a dispatching operation; see 3.9.2, "Dispatching Operations of Tagged Types". Note that that rule is not a Name Resolution Rule. 

to a universal type that covers T; or

{AI95-00230-01} {AI95-00231-01} {AI95-00254-01} {AI95-00409-01} when T is a specific anonymous access-to-object type (see 3.10) with designated type D, to an access-to-object type whose designated type is D'Class or is covered by D; or

This paragraph was deleted.{AI95-00409-01} 

Ramification: The case where the actual is access-to-D'Class will only be legal as part of a call on a dispatching operation; see 3.9.2, "Dispatching Operations of Tagged Types". Note that that rule is not a Name Resolution Rule. 

{AI05-0149-1} when T is a named general access-to-object type (see 3.10) with designated type D, to an anonymous access-to-object type whose designated type covers or is covered by D; or

{AI95-00254-01} {AI95-00409-01} {AI05-0239-1} when T is an anonymous access-to-subprogram type (see 3.10), to an access-to-subprogram type whose designated profile is type conformant with that of T.

In certain contexts, [such as in a [subprogram_renaming_declaration](./AA-8.5#S0242),] the Name Resolution Rules define an expected profile for a given [name](./AA-4.1#S0091); in such cases, the [name](./AA-4.1#S0091) shall resolve to the name of a callable entity whose profile is type conformant with the expected profile. 

Ramification: {AI05-0239-1} The parameter and result subtypes are not used in overload resolution. Only type conformance of profiles is considered during overload resolution. Legality rules generally require at least mode conformance in addition, but those rules are not used in overload resolution. 


#### Legality Rules

{AI95-00332-01} When a construct is one that requires that its expected type be a single type in a given class, the type of the construct shall be determinable solely from the context in which the construct appears, excluding the construct itself, but using the requirement that it be in the given class. Furthermore, the context shall not be one that expects any type in some class that contains types of the given class; in particular, the construct shall not be the operand of a [type_conversion](./AA-4.6#S0162).

Ramification: {AI95-00230-01} {AI12-0005-1} For example, the expected type for a character literal is required to be a single character type. But the expected type for the operand of a [type_conversion](./AA-4.6#S0162) is any type. Therefore, a character literal is not allowed as the operand of a [type_conversion](./AA-4.6#S0162). This is true even if there is only one character type in scope (which is never the case). The reason for these rules is so that the compiler will not have to search "everywhere" to see if there is exactly one type in a class in scope. 

Discussion: {AI95-00332-01} The first sentence is carefully worded so that it only mentions "expected type" as part of identifying the interesting case, but doesn't require that the context actually provide such an expected type. This allows such constructs to be used inside of constructs that don't provide an expected type (like qualified expressions and renames). Otherwise, such constructs wouldn't allow [aggregate](./AA-4.3#S0106)s, 'Access, and so on. 

{AI05-0102-1} {AI05-0149-1} {AI05-0299-1} {AI12-0039-1} Other than for the tested_[simple_expression](./AA-4.4#S0138) of a membership test, if the expected type for a [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132) is not the same as the actual type of the [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132), the actual type shall be convertible to the expected type (see 4.6); further, if the expected type is a named access-to-object type with designated type D1 and the actual type is an anonymous access-to-object type with designated type D2, then D1 shall cover D2, and the [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132) shall denote a view with an accessibility level for which the statically deeper relationship applies[; in particular it shall not denote an access parameter nor a stand-alone access object].

Reason: This rule prevents an implicit conversion that would be illegal if it was an explicit conversion. For instance, this prevents assigning an access-to-constant value into a stand-alone anonymous access-to-variable object. It also covers convertibility of the designated type and accessibility checks.

The rule also minimizes cases of implicit conversions when the tag check or the accessibility check might fail. We word it this way because access discriminants should also be disallowed if their enclosing object is designated by an access parameter. 

Ramification: This rule does not apply to expressions that don't have expected types (such as the operand of a qualified expression or the expression of a renames). We don't need a rule like this in those cases, as the type needs to be the same; there is no implicit conversion. 

A complete context shall have at least one acceptable interpretation; if there is exactly one, then that one is chosen. 

Ramification: This, and the rule below about ambiguity, are the ones that suck in all the Syntax Rules and Name Resolution Rules as compile-time rules. Note that this and the ambiguity rule have to be Legality Rules. 

There is a preference for the primitive operators (and [range](./AA-3.5#S0037)s) of the root numeric types root_integer and root_real. In particular, if two acceptable interpretations of a constituent of a complete context differ only in that one is for a primitive operator (or [range](./AA-3.5#S0037)) of the type root_integer or root_real, and the other is not, the interpretation using the primitive operator (or [range](./AA-3.5#S0037)) of the root numeric type is preferred.

Reason: The reason for this preference is so that expressions involving literals and named numbers can be unambiguous. For example, without the preference rule, the following would be ambiguous: 

```ada
N : constant := 123;
if N &gt 100 then -- Preference for root_integer "&gt" operator.
    ...
end if;

```

{AI05-0149-1} Similarly, there is a preference for the equality operators of the universal_access type (see 4.5.2). If two acceptable interpretations of a constituent of a complete context differ only in that one is for an equality operator of the universal_access type, and the other is not, the interpretation using the equality operator of the universal_access type is preferred.

Reason: This preference is necessary because of implicit conversion from an anonymous access type to a named access type, which would allow the equality operator of any named access type to be used to compare anonymous access values (and that way lies madness). 

For a complete context, if there is exactly one overall acceptable interpretation where each constituent's interpretation is the same as or preferred (in the above sense) over those in all other overall acceptable interpretations, then that one overall acceptable interpretation is chosen. Otherwise, the complete context is ambiguous.

A complete context other than a [pragma_argument_association](./AA-2.8#S0020) shall not be ambiguous.

A complete context that is a [pragma_argument_association](./AA-2.8#S0020) is allowed to be ambiguous (unless otherwise specified for the particular pragma), but only if every acceptable interpretation of the pragma argument is as a [name](./AA-4.1#S0091) that statically denotes a callable entity. Such a [name](./AA-4.1#S0091) denotes all of the declarations determined by its interpretations, and all of the views declared by these declarations. 

Ramification: {AI95-00224-01} {AI05-0229-1} This applies to Inline, Suppress, Import, Export, and Convention [pragma](./AA-2.8#S0019)s. For example, it is OK to say "pragma Export(C, Entity_Name =&gt P.Q);", even if there are two directly visible P's, and there are two Q's declared in the visible part of each P. In this case, P.Q denotes four different declarations. This rule also applies to certain pragmas defined in the Specialized Needs Annexes. It almost applies to Pure, Elaborate_Body, and Elaborate_All [pragma](./AA-2.8#S0019)s, but those can't have overloading for other reasons.  Note that almost all of these pragmas are obsolescent (see J.10 and J.15), and a major reason is that this rule has proven to be too broad in practice (it is common to want to specify something on a single subprogram of an overloaded set, that can't be done easily with this rule). [Aspect_specification](./AA-13.1#S0346)s, which are given on individual declarations, are preferred in Ada 2012.

Note that if a pragma argument denotes a call to a callable entity, rather than the entity itself, this exception does not apply, and ambiguity is disallowed.

Note that we need to carefully define which pragma-related rules are Name Resolution Rules, so that, for example, a [pragma](./AA-2.8#S0019) Inline does not pick up subprograms declared in enclosing declarative regions, and therefore make itself illegal.

We say "statically denotes" in the above rule in order to avoid having to worry about how many times the [name](./AA-4.1#S0091) is evaluated, in case it denotes more than one callable entity. 

NOTE   If a usage name has only one acceptable interpretation, then it denotes the corresponding entity. However, this does not mean that the usage name is necessarily legal since other requirements exist which are not considered for overload resolution; for example, the fact that an expression is static, whether an object is constant, mode and subtype conformance rules, freezing rules, order of elaboration, and so on.

Similarly, subtypes are not considered for overload resolution (the violation of a constraint does not make a program illegal but raises an exception during program execution). 


#### Incompatibilities With Ada 83

The new preference rule for operators of root numeric types is upward incompatible, but only in cases that involved Beaujolais effects in Ada 83. Such cases are ambiguous in Ada 95. 


#### Extensions to Ada 83

The rule that allows an expected type to match an actual expression of a universal type, in combination with the new preference rule for operators of root numeric types, subsumes the Ada 83 "implicit conversion" rules for universal types. 


#### Wording Changes from Ada 83

In Ada 83, it is not clear what the "syntax rules" are. AI83-00157 states that a certain textual rule is a syntax rule, but it's still not clear how one tells in general which textual rules are syntax rules. We have solved the problem by stating exactly which rules are syntax rules - the ones that appear under the "Syntax" heading.

RM83 has a long list of the "forms" of rules that are to be used in overload resolution (in addition to the syntax rules). It is not clear exactly which rules fall under each form. We have solved the problem by explicitly marking all rules that are used in overload resolution. Thus, the list of kinds of rules is unnecessary. It is replaced with some introductory (intentionally vague) text explaining the basic idea of what sorts of rules are overloading rules.

{AI05-0299-1} It is not clear from RM83 what information is embodied in a "meaning" or an "interpretation". "Meaning" and "interpretation" were intended to be synonymous; we now use the latter only in defining the rules about overload resolution. "Meaning" is used only informally. This subclause attempts to clarify what is meant by "interpretation".

For example, RM83 does not make it clear that overload resolution is required in order to match subprogram_bodies with their corresponding declarations (and even to tell whether a given [subprogram_body](./AA-6.3#S0216) is the completion of a previous declaration). Clearly, the information needed to do this is part of the "interpretation" of a [subprogram_body](./AA-6.3#S0216). The resolution of such things is defined in terms of the "expected profile" concept. Ada 95 has some new cases where expected profiles are needed - the resolution of P'Access, where P might denote a subprogram, is an example.

RM83-8.7(2) might seem to imply that an interpretation embodies information about what is denoted by each usage name, but not information about which syntactic category each construct belongs to. However, it seems necessary to include such information, since the Ada grammar is highly ambiguous. For example, X(Y) might be a [function_call](./AA-6.4#S0218) or an [indexed_component](./AA-4.1#S0096), and no context-free/syntactic information can tell the difference. It seems like we should view X(Y) as being, for example, "interpreted as a [function_call](./AA-6.4#S0218)" (if that's what overload resolution decides it is). Note that there are examples where the denotation of each usage name does not imply the syntactic category. However, even if that were not true, it seems that intuitively, the interpretation includes that information. Here's an example: 

```ada
type T;
type A is access T;
type T is array(Integer range 1..10) of A;
I : Integer := 3;
function F(X : Integer := 7) return A;
Y : A := F(I); -- Ambiguous? (We hope so.)

```

Consider the declaration of Y (a complete context). In the above example, overload resolution can easily determine the declaration, and therefore the entity, denoted by Y, A, F, and I. However, given all of that information, we still don't know whether F(I) is a [function_call](./AA-6.4#S0218) or an [indexed_component](./AA-4.1#S0096) whose [prefix](./AA-4.1#S0093) is a [function_call](./AA-6.4#S0218). (In the latter case, it is equivalent to F(7).all(I).)

It seems clear that the declaration of Y ought to be considered ambiguous. We describe that by saying that there are two interpretations, one as a [function_call](./AA-6.4#S0218), and one as an [indexed_component](./AA-4.1#S0096). These interpretations are both acceptable to the overloading rules. Therefore, the complete context is ambiguous, and therefore illegal.

It is the intent that the Ada 95 preference rule for root numeric operators is more locally enforceable than that of RM83-4.6(15). It should also eliminate interpretation shifts due to the addition or removal of a [use_clause](./AA-8.4#S0235) (the so called Beaujolais effect).

{AI95-00114-01} RM83-8.7 seems to be missing some complete contexts, such as [pragma_argument_association](./AA-2.8#S0020)s, [declarative_item](./AA-3.11#S0087)s that are not declarations or [aspect_clause](./AA-13.1#S0343)s, and [context_item](./AA-10.1#S0293)s. We have added these, and also replaced the "must be determinable" wording of RM83-5.4(3) with the notion that the expression of a [case_statement](./AA-5.4#S0176) is a complete context.

Cases like the Val attribute are now handled using the normal type resolution rules, instead of having special cases that explicitly allow things like "any integer type". 


#### Incompatibilities With Ada 95

{AI95-00409-01} Ada 95 allowed name resolution to distinguish between anonymous access-to-variable and access-to-constant types. This is similar to distinguishing between subprograms with in and in out parameters, which is known to be bad. Thus, that part of the rule was dropped as we now have anonymous access-to-constant types, making this much more likely. 

```ada
type Cacc is access constant Integer;
procedure Proc (Acc : access Integer) ...
procedure Proc (Acc : Cacc) ...
List : Cacc := ...;
Proc (List); -- OK in Ada 95, ambiguous in Ada 2005.

```

If there is any code like this (such code should be rare), it will be ambiguous in Ada 2005. 


#### Extensions to Ada 95

{AI95-00230-01} {AI95-00231-01} {AI95-00254-01} Generalized the anonymous access resolution rules to support the new capabilities of anonymous access types (that is, access-to-subprogram and access-to-constant).

{AI95-00382-01} We now allow the creation of self-referencing types via anonymous access types. This is an extension in unusual cases involving task and protected types. For example: 

```ada
task type T;

```

```ada
task body T is
   procedure P (X : access T) is -- Illegal in Ada 95, legal in Ada 2005
      ...
   end P;
begin
   ...
end T;

```


#### Wording Changes from Ada 95

{AI95-00332-01} Corrected the "single expected type" so that it works in contexts that don't have expected types (like object renames and qualified expressions). This fixes a hole in Ada 95 that appears to prohibit using [aggregate](./AA-4.3#S0106)s, 'Access, character literals, string literals, and [allocator](./AA-4.8#S0164)s in qualified expressions. 


#### Incompatibilities With Ada 2005

{AI05-0149-1} Implicit conversion is now allowed from anonymous access-to-object types to general access-to-object types. Such conversions can make calls ambiguous. That can only happen when there are two visible subprograms with the same name and have profiles that differ only by a parameter that is of a named or anonymous access type, and the actual argument is of an anonymous access type. This should be rare, as many possible calls would be ambiguous even in Ada 2005 (including [allocator](./AA-4.8#S0164)s and any actual of a named access type if the designated types are the same). 


#### Extensions to Ada 2005

{AI05-0149-1} Implicit conversion is allowed from anonymous access-to-object types to general access-to-object types if the designated type is convertible and runtime checks are minimized. See also the incompatibilities section. 


#### Wording Changes from Ada 2005

{AI05-0102-1} Added a requirement here that implicit conversions are convertible to the appropriate type. This rule was scattered about the Reference Manual, we moved a single generalized version here. 


#### Inconsistencies With Ada 2012

{AI12-0068-1} Corrigendum: Added a rule to specify that the current instance of a type or subtype is a value within an [aspect_specification](./AA-13.1#S0346). This could be inconsistent if a predicate or invariant uses the Constrained attribute on the current instance (it will always be False now, while it might have returned True in original Ada 2012). More likely, a usage of a current instance as a prefix of an attribute will become illegal (such as Size or Alignment). Any such code is very tricky. Moreover, as this is a new feature of Ada 2012, there are not that many predicates and invariants, and the ones that exist are very unlikely to be this tricky. Thus we do not believe that there will be any practical effect to this change, other than to explicitly allow common implementation strategies. 


#### Wording Changes from Ada 2012

{AI12-0040-1} Corrigendum: Added wording to clarify that the selecting_[expression](./AA-4.4#S0132) of a [case_expression](./AA-4.5#S0151) is a complete context, just like that of a [case_statement](./AA-5.4#S0176). Clearly, everyone expects these to work the same way. Moreover, since it would be a lot of extra work to treat [case_expression](./AA-4.5#S0151)s differently, it is quite unlikely that any compiler would implement the much more complicated resolution necessary (and we are not aware of any that did). Therefore, we didn't document this as a potential incompatibility.

{AI12-0125-3} Added a resolution rule for [target_name](./AA-5.2#S0174) (see 5.2.1). 

