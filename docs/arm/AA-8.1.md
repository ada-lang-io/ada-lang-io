---
sidebar_position:  65
---

# 8.1  Declarative Region


#### Static Semantics

For each of the following constructs, there is a portion of the program text called its declarative region, [within which nested declarations can occur]: 

any declaration, other than that of an enumeration type, that is not a completion [of a previous declaration];

{AI12-0094-1} an [access_definition](./AA-3.10#S0084);

{AI12-0061-1} an [iterated_component_association](./AA-4.3#S0119);

{AI12-0308-1} an [iterated_element_association](./AA-4.3#S0131);

{AI05-0255-1} {AI12-0308-1} a [quantified_expression](./AA-4.5#S0153);

{AI12-0236-1} a [declare_expression](./AA-4.5#S0156);

a [block_statement](./AA-5.6#S0191);

a [loop_statement](./AA-5.5#S0178);

This paragraph was deleted.{AI05-0255-1} {AI12-0308-1} 

{AI95-00318-02} an [extended_return_statement](./AA-6.5#S0225);

an [accept_statement](./AA-9.5#S0258);

an [exception_handler](./AA-11.2#S0305). 

The declarative region includes the text of the construct together with additional text determined [(recursively)], as follows: 

If a declaration is included, so is its completion, if any.

If the declaration of a library unit [(including Standard - see 10.1.1)] is included, so are the declarations of any child units [(and their completions, by the previous rule)]. The child declarations occur after the declaration.

If a [body_stub](./AA-10.1#S0297) is included, so is the corresponding [subunit](./AA-10.1#S0302).

If a [type_declaration](./AA-3.2#S0023) is included, then so is a corresponding [record_representation_clause](./AA-13.5#S0352), if any. 

Reason: This is so that the [component_declaration](./AA-3.8#S0070)s can be directly visible in the [record_representation_clause](./AA-13.5#S0352). 

The declarative region of a declaration is also called the declarative region of any view or entity declared by the declaration. 

Reason: The constructs that have declarative regions are the constructs that can have declarations nested inside them. Nested declarations are declared in that declarative region. The one exception is for enumeration literals; although they are nested inside an enumeration type declaration, they behave as if they were declared at the same level as the type. 

To be honest: A declarative region does not include [parent_unit_name](./AA-10.1#S0291)s. 

Ramification: A declarative region does not include [context_clause](./AA-10.1#S0292)s. 

A declaration occurs immediately within a declarative region if this region is the innermost declarative region that encloses the declaration (the immediately enclosing declarative region), not counting the declarative region (if any) associated with the declaration itself. 

Discussion: Don't confuse the declarative region of a declaration with the declarative region in which it immediately occurs. 

[ A declaration is local to a declarative region if the declaration occurs immediately within the declarative region.] [An entity is local to a declarative region if the entity is declared by a declaration that is local to the declarative region.] 

Ramification: "Occurs immediately within" and "local to" are synonyms (when referring to declarations).

Thus, "local to" applies to both declarations and entities, whereas "occurs immediately within" only applies to declarations. We use this term only informally; for cases where precision is required, we use the term "occurs immediately within", since it is less likely to cause confusion. 

A declaration is global to a declarative region if the declaration occurs immediately within another declarative region that encloses the declarative region. An entity is global to a declarative region if the entity is declared by a declaration that is global to the declarative region. 

NOTE 1   The children of a parent library unit are inside the parent's declarative region, even though they do not occur inside the parent's declaration or body. This implies that one can use (for example) "P.Q" to refer to a child of P whose defining name is Q, and that after "use P;" Q can refer (directly) to that child.

NOTE 2   As explained above and in 10.1.1, "Compilation Units - Library Units", all library units are descendants of Standard, and so are contained in the declarative region of Standard. They are not inside the declaration or body of Standard, but they are inside its declarative region.

NOTE 3   {AI12-0442-1} For a declarative region that comes in multiple parts, the text of the declarative region does not include any of the text that appears between the parts. Thus, when a portion of a declarative region is said to extend from one place to another in the declarative region, the portion does not contain any of the text that appears between the parts of the declarative region. 

Discussion: It is necessary for the things that have a declarative region to include anything that contains declarations (except for enumeration type declarations). This includes any declaration that has a profile (that is, [subprogram_declaration](./AA-6.1#S0195), [subprogram_body](./AA-6.3#S0216), [entry_declaration](./AA-9.5#S0257), [subprogram_renaming_declaration](./AA-8.5#S0242), [formal_subprogram_declaration](./AA-12.6#S0335), access-to-subprogram [type_declaration](./AA-3.2#S0023)), anything that has a [discriminant_part](./AA-3.7#S0059) (that is, various kinds of [type_declaration](./AA-3.2#S0023)), anything that has a [component_list](./AA-3.8#S0068) (that is, record [type_declaration](./AA-3.2#S0023) and record extension [type_declaration](./AA-3.2#S0023)), and finally the declarations of task and protected units and packages. 


#### Wording Changes from Ada 83

It was necessary to extend Ada 83's definition of declarative region to take the following Ada 95 features into account: 

Child library units.

Derived types/type extensions - we need a declarative region for inherited components and also for new components.

All the kinds of types that allow discriminants.

Protected units.

Entries that have bodies instead of accept statements.

The [choice_parameter_specification](./AA-11.2#S0306) of an [exception_handler](./AA-11.2#S0305).

The formal parameters of access-to-subprogram types.

Renamings-as-body. 

Discriminated and access-to-subprogram type declarations need a declarative region. Enumeration type declarations cannot have one, because you don't have to say "Color.Red" to refer to the literal Red of Color. For other type declarations, it doesn't really matter whether or not there is an associated declarative region, so for simplicity, we give one to all types except enumeration types.

We now say that an [accept_statement](./AA-9.5#S0258) has its own declarative region, rather than being part of the declarative region of the [entry_declaration](./AA-9.5#S0257), so that declarative regions are properly nested regions of text, so that it makes sense to talk about "inner declarative regions", and "...extends to the end of a declarative region". Inside an [accept_statement](./AA-9.5#S0258), the [name](./AA-4.1#S0091) of one of the parameters denotes the [parameter_specification](./AA-6.1#S0207) of the [accept_statement](./AA-9.5#S0258), not that of the [entry_declaration](./AA-9.5#S0257). If the [accept_statement](./AA-9.5#S0258) is nested within a [block_statement](./AA-5.6#S0191), these [parameter_specification](./AA-6.1#S0207)s can hide declarations of the [block_statement](./AA-5.6#S0191). The semantics of such cases was unclear in RM83. 

To be honest: Unfortunately, we have the same problem for the entry name itself - it should denote the [accept_statement](./AA-9.5#S0258), but [accept_statement](./AA-9.5#S0258)s are not declarations. They should be, and they should hide the entry from all visibility within themselves. 

Note that we can't generalize this to entry_bodies, or other bodies, because the [declarative_part](./AA-3.11#S0086) of a body is not supposed to contain (explicit) homographs of things in the declaration. It works for [accept_statement](./AA-9.5#S0258)s only because an [accept_statement](./AA-9.5#S0258) does not have a [declarative_part](./AA-3.11#S0086).

To avoid confusion, we use the term "local to" only informally in Ada 95. Even RM83 used the term incorrectly (see, for example, RM83-12.3(13)).

In Ada 83, (root) library units were inside Standard; it was not clear whether the declaration or body of Standard was meant. In Ada 95, they are children of Standard, and so occur immediately within Standard's declarative region, but not within either the declaration or the body. (See RM83-8.6(2) and RM83-10.1.1(5).) 


#### Wording Changes from Ada 95

{AI95-00318-02} [Extended_return_statement](./AA-6.5#S0225) (see 6.5) is added to the list of constructs that have a declarative region. 


#### Extensions to Ada 2012

{AI12-0094-1} Corrigendum: [access_definition](./AA-3.10#S0084) is added to the list of constructs that have a declarative region. This allows parameter names declared in anonymous access type subprogram types to be the same as other names declared outside. For instance: 

```ada
type Foo is record
   A : Natural;
   B : access procedure (A : Boolean);
end record;

```

{AI12-0005-1} This is now legal, as one would expect; it was illegal in previous versions of Ada as the parameter A and the component A were homographs in the same declarative region (see 8.3). Note that some implementations already allow this common sense interpretation, so this extension might in fact already be used in existing code. 


#### Wording Changes from Ada 2012

{AI12-0061-1} {AI12-0236-1} {AI12-0308-1} Added [iterated_component_association](./AA-4.3#S0119), [iterated_element_association](./AA-4.3#S0131), and [declare_expression](./AA-4.5#S0156) to the rapidly expanding list of constructs that have a declarative region. 

