---
sidebar_position:  24
---

# 3.8  Record Types

A record object is a composite object consisting of named components. The value of a record object is a composite value consisting of the values of the components. 


#### Syntax

record_type_definition<a id="S0066"></a> ::= [[abstract] tagged] [limited] [record_definition](./AA-3.8#S0067)

{AI12-0213-1} record_definition<a id="S0067"></a> ::= 
    record
       [component_list](./AA-3.8#S0068)
    end record [record_[identifier](./AA-2.3#S0002)]
  | null record

component_list<a id="S0068"></a> ::= 
      [component_item](./AA-3.8#S0069) {[component_item](./AA-3.8#S0069)}
   | {[component_item](./AA-3.8#S0069)} [variant_part](./AA-3.8#S0071)
   |  null;

{8652/0009} {AI95-00137-01} component_item<a id="S0069"></a> ::= [component_declaration](./AA-3.8#S0070) | [aspect_clause](./AA-13.1#S0343)

{AI05-0183-1} component_declaration<a id="S0070"></a> ::= 
   [defining_identifier_list](./AA-3.3#S0033) : [component_definition](./AA-3.6#S0056) [:= [default_expression](./AA-3.7#S0063)]
        [[aspect_specification](./AA-13.1#S0346)];

{AI12-0213-1} If a record_[identifier](./AA-2.3#S0002) appears at the end of the [record_definition](./AA-3.8#S0067), it shall repeat the [defining_identifier](./AA-3.1#S0022) of the enclosing [full_type_declaration](./AA-3.2#S0024). 


#### Name Resolution Rules

The expected type for the [default_expression](./AA-3.7#S0063), if any, in a [component_declaration](./AA-3.8#S0070) is the type of the component. 


#### Legality Rules

This paragraph was deleted.{AI95-00287-01} 

{AI95-00366-01} Each [component_declaration](./AA-3.8#S0070) declares a component of the record type. Besides components declared by [component_declaration](./AA-3.8#S0070)s, the components of a record type include any components declared by [discriminant_specification](./AA-3.7#S0062)s of the record type declaration. [The identifiers of all components of a record type shall be distinct.] 

Proof: {AI05-0299-1} The identifiers of all components of a record type have to be distinct because they are all declared immediately within the same declarative region. See Clause 8. 

Within a [type_declaration](./AA-3.2#S0023), a [name](./AA-4.1#S0091) that denotes a component, protected subprogram, or entry of the type is allowed only in the following cases:

{AI05-0004-1} {AI05-0295-1} A [name](./AA-4.1#S0091) that denotes any component, protected subprogram, or entry is allowed within an [aspect_specification](./AA-13.1#S0346), an operational item, or a representation item that occurs within the declaration of the composite type.

{AI05-0264-1} A [name](./AA-4.1#S0091) that denotes a noninherited discriminant is allowed within the declaration of the type, but not within the [discriminant_part](./AA-3.7#S0059). If the discriminant is used to define the constraint of a component, the bounds of an entry family, or the constraint of the parent subtype in a [derived_type_definition](./AA-3.4#S0035), then its name shall appear alone as a [direct_name](./AA-4.1#S0092) (not as part of a larger expression or expanded name). A discriminant shall not be used to define the constraint of a scalar component. 

Reason: The penultimate restriction simplifies implementation, and allows the outer discriminant and the inner discriminant or bound to possibly share storage. 

Ramification: Other rules prevent such a discriminant from being an inherited one. 

Reason: The last restriction is inherited from Ada 83. The restriction is not really necessary from a language design point of view, but we did not remove it, in order to avoid unnecessary changes to existing compilers. 

Discussion: Note that a discriminant can be used to define the constraint for a component that is of an access-to-composite type. 

Reason: {AI95-00373-01} The above rules, and a similar one in 6.1 for formal parameters, are intended to allow initializations of components or parameters to occur in a (nearly) arbitrary order - whatever order is most efficient (subject to the restrictions of 3.3.1), since one [default_expression](./AA-3.7#S0063) cannot depend on the value of another one. They also prevent circularities. 

Ramification: {AI05-0295-1} Inherited discriminants are not allowed to be denoted, except within [aspect_specification](./AA-13.1#S0346)s and representation items. However, the discriminant_[selector_name](./AA-4.1#S0099) of the parent [subtype_indication](./AA-3.2#S0027) is allowed to denote a discriminant of the parent. 

If the name of the current instance of a type (see 8.6) is used to define the constraint of a component, then it shall appear as a [direct_name](./AA-4.1#S0092) that is the [prefix](./AA-4.1#S0093) of an [attribute_reference](./AA-4.1#S0100) whose result is of an access type, and the [attribute_reference](./AA-4.1#S0100) shall appear alone. 

Reason: This rule allows T'Access or T'Unchecked_Access, but disallows, for example, a range constraint (1..T'Size). Allowing things like (1..T'Size) would mean that a per-object constraint could affect the size of the object, which would be bad. 


#### Static Semantics

{AI95-00318-02} {AI05-0004-1} If a [record_type_definition](./AA-3.8#S0066) includes the reserved word limited, the type is called an explicitly limited record type.

The [component_definition](./AA-3.6#S0056) of a [component_declaration](./AA-3.8#S0070) defines the (nominal) subtype of the component. If the reserved word aliased appears in the [component_definition](./AA-3.6#S0056), then the component is aliased (see 3.10). 

If the [component_list](./AA-3.8#S0068) of a record type is defined by the reserved word null and there are no discriminants, then the record type has no components and all records of the type are null records. A [record_definition](./AA-3.8#S0067) of null record is equivalent to record null; end record. 

Ramification: {AI12-0426-1} This shorthand is available both for declaring a record type and a record extension - see 3.9.1. 


#### Dynamic Semantics

The elaboration of a [record_type_definition](./AA-3.8#S0066) creates the record type and its first subtype, and consists of the elaboration of the [record_definition](./AA-3.8#S0067). The elaboration of a [record_definition](./AA-3.8#S0067) consists of the elaboration of its [component_list](./AA-3.8#S0068), if any.

The elaboration of a [component_list](./AA-3.8#S0068) consists of the elaboration of the [component_item](./AA-3.8#S0069)s and [variant_part](./AA-3.8#S0071), if any, in the order in which they appear. The elaboration of a [component_declaration](./AA-3.8#S0070) consists of the elaboration of the [component_definition](./AA-3.6#S0056). 

Discussion: If the [defining_identifier_list](./AA-3.3#S0033) has more than one [defining_identifier](./AA-3.1#S0022), we presume here that the transformation explained in 3.3.1 has already taken place. Alternatively, we could say that the [component_definition](./AA-3.6#S0056) is elaborated once for each [defining_identifier](./AA-3.1#S0022) in the list. 

{8652/0002} {AI95-00171-01} {AI95-00230-01} Within the definition of a composite type, if a [component_definition](./AA-3.6#S0056) or [discrete_subtype_definition](./AA-3.6#S0055) (see 9.5.2) includes a [name](./AA-4.1#S0091) that denotes a discriminant of the type, or that is an [attribute_reference](./AA-4.1#S0100) whose [prefix](./AA-4.1#S0093) denotes the current instance of the type, the expression containing the [name](./AA-4.1#S0091) is called a per-object expression, and the [constraint](./AA-3.2#S0029) or [range](./AA-3.5#S0037) being defined is called a per-object constraint. For the elaboration of a [component_definition](./AA-3.6#S0056) of a [component_declaration](./AA-3.8#S0070) or the [discrete_subtype_definition](./AA-3.6#S0055) of an [entry_declaration](./AA-9.5#S0257) for an entry family (see 9.5.2), if the component subtype is defined by an [access_definition](./AA-3.10#S0084) or if the [constraint](./AA-3.2#S0029) or [range](./AA-3.5#S0037) of the [subtype_indication](./AA-3.2#S0027) or [discrete_subtype_definition](./AA-3.6#S0055) is not a per-object constraint, then the [access_definition](./AA-3.10#S0084), [subtype_indication](./AA-3.2#S0027), or [discrete_subtype_definition](./AA-3.6#S0055) is elaborated. On the other hand, if the [constraint](./AA-3.2#S0029) or [range](./AA-3.5#S0037) is a per-object constraint, then the elaboration consists of the evaluation of any included expression that is not part of a per-object expression. Each such expression is evaluated once unless it is part of a named association in a discriminant constraint, in which case it is evaluated once for each associated discriminant.

{8652/0002} {AI95-00171-01} When a per-object constraint is elaborated [(as part of creating an object)], each per-object expression of the constraint is evaluated. For other expressions, the values determined during the elaboration of the [component_definition](./AA-3.6#S0056) or [entry_declaration](./AA-9.5#S0257) are used. Any checks associated with the enclosing [subtype_indication](./AA-3.2#S0027) or [discrete_subtype_definition](./AA-3.6#S0055) are performed[, including the subtype compatibility check (see 3.2.2),] and the associated subtype is created. 

Discussion: The evaluation of other expressions that appear in [component_definition](./AA-3.6#S0056)s and [discrete_subtype_definition](./AA-3.6#S0055)s is performed when the type definition is elaborated. The evaluation of expressions that appear as [default_expression](./AA-3.7#S0063)s is postponed until an object is created. Expressions in representation items that appear within a composite type definition are evaluated according to the rules of the particular representation item. 

NOTE 1   A [component_declaration](./AA-3.8#S0070) with several identifiers is equivalent to a sequence of single [component_declaration](./AA-3.8#S0070)s, as explained in 3.3.1.

NOTE 2   The [default_expression](./AA-3.7#S0063) of a record component is only evaluated upon the creation of a default-initialized object of the record type (presuming the object has the component, if it is in a [variant_part](./AA-3.8#S0071) - see 3.3.1).

NOTE 3   The subtype defined by a [component_definition](./AA-3.6#S0056) (see 3.6) has to be a definite subtype.

NOTE 4   If a record type does not have a [variant_part](./AA-3.8#S0071), then the same components are present in all values of the type.

NOTE 5   A record type is limited if it has the reserved word limited in its definition, or if any of its components are limited (see 7.5).

NOTE 6   The predefined operations of a record type include membership tests, qualification, and explicit conversion. If the record type is nonlimited, they also include assignment and the predefined equality operators.

NOTE 7   {AI95-00287-01} A component of a record can be named with a [selected_component](./AA-4.1#S0098). A value of a record can be specified with a [record_aggregate](./AA-4.3#S0107).


#### Examples

Examples of record type declarations: 

```ada
{AI12-0430-1} type Date is
   record
      Day   : Integer range 1 .. 31;
      Month : Month_Name;                  -- see 3.5.1
      Year  : Integer range 0 .. 4000;
   end record;

```

```ada
{AI12-0213-1} type Complex is
   record
      Re : Real := 0.0;
      Im : Real := 0.0;
   end record Complex;

```

Examples of record variables: 

```ada
Tomorrow, Yesterday : Date;
A, B, C : Complex;

```

```ada
-- both components of A, B, and C are implicitly initialized to zero 

```


#### Extensions to Ada 83

The syntax rule for [component_declaration](./AA-3.8#S0070) is modified to use [component_definition](./AA-3.6#S0056) (instead of component_subtype_definition). The effect of this change is to allow the reserved word aliased before the component_subtype_definition.

A short-hand is provided for defining a null record type (and a null record extension), as these will be more common for abstract root types (and derived types without additional components).

The syntax rule for [record_type_definition](./AA-3.8#S0066) is modified to allow the reserved words tagged and limited. Tagging is new. Limitedness is now orthogonal to privateness. In Ada 83 the syntax implied that limited private was sort of more private than private. However, limitedness really has nothing to do with privateness; limitedness simply indicates the lack of assignment capabilities, and makes perfect sense for nonprivate types such as record types. 


#### Wording Changes from Ada 83

{8652/0009} {AI95-00137-01} The syntax rules now allow [aspect_clause](./AA-13.1#S0343)s to appear in a [record_definition](./AA-3.8#S0067). This is not a language extension, because Legality Rules prevent all language-defined representation clauses from appearing there. However, an implementation-defined [attribute_definition_clause](./AA-13.3#S0349) could appear there. The reason for this change is to allow the rules for [aspect_clause](./AA-13.1#S0343)s and representation pragmas to be as similar as possible. 


#### Extensions to Ada 95

{AI95-00287-01} Record components can have an anonymous access type.

{AI95-00287-01} Limited components can be initialized, so long as the expression is one that allows building the object in place (such as an [aggregate](./AA-4.3#S0106) or [function_call](./AA-6.4#S0218)). 


#### Wording Changes from Ada 95

{8652/0002} {AI95-00171-01} Corrigendum: Improved the description of the elaboration of per-object constraints.

{8652/0009} {AI95-00137-01} Corrigendum: Changed representation clauses to aspect clauses to reflect that they are used for more than just representation.

{AI95-00318-02} Defined explicitly limited record type to use in other rules. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [component_declaration](./AA-3.8#S0070). This is described in 13.1.1. 


#### Extensions to Ada 2012

{AI12-0213-1} The record_[identifier](./AA-2.3#S0002) following end record is new. 


## 3.8.1  Variant Parts and Discrete Choices

A record type with a [variant_part](./AA-3.8#S0071) specifies alternative lists of components. Each [variant](./AA-3.8#S0072) defines the components for the value or values of the discriminant covered by its [discrete_choice_list](./AA-3.8#S0073). 

Discussion: [Discrete_choice_list](./AA-3.8#S0073)s and [discrete_choice](./AA-3.8#S0074)s are said to cover values as defined below; which [discrete_choice_list](./AA-3.8#S0073) covers a value determines which of various alternatives is chosen. These are used in [variant_part](./AA-3.8#S0071)s, [array_aggregate](./AA-4.3#S0113)s, and [case_statement](./AA-5.4#S0176)s. 


#### Language Design Principles

The definition of "cover" in this subclause and the rules about discrete choices are designed so that they are also appropriate for array aggregates and case statements.

The rules of this subclause intentionally parallel those for case statements. 


#### Syntax

variant_part<a id="S0071"></a> ::= 
   case discriminant_[direct_name](./AA-4.1#S0092) is
       [variant](./AA-3.8#S0072)
      {[variant](./AA-3.8#S0072)}
   end case;

variant<a id="S0072"></a> ::= 
   when [discrete_choice_list](./AA-3.8#S0073) =&gt
      [component_list](./AA-3.8#S0068)

{AI12-0212-1} discrete_choice_list<a id="S0073"></a> ::= [discrete_choice](./AA-3.8#S0074) {'|' [discrete_choice](./AA-3.8#S0074)}

{AI05-0153-3} {AI05-0158-1} discrete_choice<a id="S0074"></a> ::= [choice_expression](./AA-4.4#S0133) | discrete_[subtype_indication](./AA-3.2#S0027) | [range](./AA-3.5#S0037) | others


#### Name Resolution Rules

The discriminant_[direct_name](./AA-4.1#S0092) shall resolve to denote a discriminant (called the discriminant of the [variant_part](./AA-3.8#S0071)) specified in the [known_discriminant_part](./AA-3.7#S0061) of the [full_type_declaration](./AA-3.2#S0024) that contains the [variant_part](./AA-3.8#S0071). The expected type for each [discrete_choice](./AA-3.8#S0074) in a [variant](./AA-3.8#S0072) is the type of the discriminant of the [variant_part](./AA-3.8#S0071). 

Ramification: A [full_type_declaration](./AA-3.2#S0024) with a [variant_part](./AA-3.8#S0071) has to have a (new) [known_discriminant_part](./AA-3.7#S0061); the discriminant of the [variant_part](./AA-3.8#S0071) cannot be an inherited discriminant. 


#### Legality Rules

The discriminant of the [variant_part](./AA-3.8#S0071) shall be of a discrete type. 

Ramification: It shall not be of an access type, named or anonymous.

{AI05-0153-3} The [choice_expression](./AA-4.4#S0133)s, [subtype_indication](./AA-3.2#S0027)s, and [range](./AA-3.5#S0037)s given as [discrete_choice](./AA-3.8#S0074)s in a [variant_part](./AA-3.8#S0071) shall be static. The [discrete_choice](./AA-3.8#S0074) others shall appear alone in a [discrete_choice_list](./AA-3.8#S0073), and such a [discrete_choice_list](./AA-3.8#S0073), if it appears, shall be the last one in the enclosing construct.

A [discrete_choice](./AA-3.8#S0074) is defined to cover a value in the following cases: 

{AI05-0262-1} A [discrete_choice](./AA-3.8#S0074) that is a [choice_expression](./AA-4.4#S0133) covers a value if the value equals the value of the [choice_expression](./AA-4.4#S0133) converted to the expected type.

{AI05-0153-3} {AI05-0262-1} {AI12-0071-1} A [discrete_choice](./AA-3.8#S0074) that is a [subtype_indication](./AA-3.2#S0027) covers all values (possibly none) that belong to the subtype and that satisfy the static predicates of the subtype (see 3.2.4).

Ramification: {AI05-0262-1} A dynamic predicate is never allowed in this case (for [variant](./AA-3.8#S0072)s, [case_statement](./AA-5.4#S0176)s, and [case_expression](./AA-4.5#S0151)s, a subtype with a dynamic predicate isn't static and thus isn't allowed in a [discrete_choice](./AA-3.8#S0074), and for a choice in an [array_aggregate](./AA-4.3#S0113), a dynamic predicate is explicitly disallowed - see 3.2.4). 

{AI05-0153-3} A [discrete_choice](./AA-3.8#S0074) that is a [range](./AA-3.5#S0037) covers all values (possibly none) that belong to the range.

The [discrete_choice](./AA-3.8#S0074) others covers all values of its expected type that are not covered by previous [discrete_choice_list](./AA-3.8#S0073)s of the same construct. 

Ramification: For [case_statement](./AA-5.4#S0176)s, this includes values outside the range of the static subtype (if any) to be covered by the choices. It even includes values outside the base range of the case expression's type, since values of numeric types (and undefined values of any scalar type?) can be outside their base range. 

A [discrete_choice_list](./AA-3.8#S0073) covers a value if one of its [discrete_choice](./AA-3.8#S0074)s covers the value.

The possible values of the discriminant of a [variant_part](./AA-3.8#S0071) shall be covered as follows: 

{AI05-0153-3} {AI05-0188-1} {AI05-0262-1} {AI12-0071-1} If the discriminant is of a static constrained scalar subtype then, except within an instance of a generic unit, each non-others [discrete_choice](./AA-3.8#S0074) shall cover only values in that subtype that satisfy its predicates, and each value of that subtype that satisfies its predicates shall be covered by some [discrete_choice](./AA-3.8#S0074) [(either explicitly or by others)];

Reason: {AI05-0188-1} The exemption for a discriminated type declared in an instance allows the following example: 

```ada
generic
   type T is new Integer;
package G is
   type Rec (Discrim : T) is record
      case Discrim is
         when -10 .. -1 =&gt
            Foo : Float;
         when others =&gt
            null;
      end case;
   end record;
end G;

```

```ada
package I is new G (Natural); -- Legal

```

{AI05-0264-1} If the type of the discriminant is a descendant of a generic formal scalar type, then the [variant_part](./AA-3.8#S0071) shall have an others [discrete_choice](./AA-3.8#S0074); 

Reason: The base range is not known statically in this case. 

Otherwise, each value of the base range of the type of the discriminant shall be covered [(either explicitly or by others)]. 

Two distinct [discrete_choice](./AA-3.8#S0074)s of a [variant_part](./AA-3.8#S0071) shall not cover the same value.


#### Static Semantics

If the [component_list](./AA-3.8#S0068) of a [variant](./AA-3.8#S0072) is specified by null, the variant has no components.

The discriminant of a [variant_part](./AA-3.8#S0071) is said to govern the [variant_part](./AA-3.8#S0071) and its [variant](./AA-3.8#S0072)s. In addition, the discriminant of a derived type governs a [variant_part](./AA-3.8#S0071) and its [variant](./AA-3.8#S0072)s if it corresponds (see 3.7) to the discriminant of the [variant_part](./AA-3.8#S0071).


#### Dynamic Semantics

A record value contains the values of the components of a particular [variant](./AA-3.8#S0072) only if the value of the discriminant governing the [variant](./AA-3.8#S0072) is covered by the [discrete_choice_list](./AA-3.8#S0073) of the [variant](./AA-3.8#S0072). This rule applies in turn to any further [variant](./AA-3.8#S0072) that is, itself, included in the [component_list](./AA-3.8#S0068) of the given [variant](./AA-3.8#S0072).

{AI05-0290-1} When an object of a discriminated type T is initialized by default, Constraint_Error is raised if no [discrete_choice_list](./AA-3.8#S0073) of any [variant](./AA-3.8#S0072) of a [variant_part](./AA-3.8#S0071) of T covers the value of the discriminant that governs the [variant_part](./AA-3.8#S0071). When a [variant_part](./AA-3.8#S0071) appears in the [component_list](./AA-3.8#S0068) of another [variant](./AA-3.8#S0072) V, this test is only applied if the value of the discriminant governing V is covered by the [discrete_choice_list](./AA-3.8#S0073) of V. 

Implementation Note: This is not a "check"; it cannot be suppressed. However, in most cases it is not necessary to generate any code to raise this exception. A test is needed (and can fail) in the case where the discriminant subtype has a Static_Predicate specified, it also has predicate checking disabled, and the discriminant governs a [variant_part](./AA-3.8#S0071) which lacks a when others choice.

The test also could fail for a static discriminant subtype with range checking suppressed and the discriminant governs a [variant_part](./AA-3.8#S0071) which lacks a when others choice. But execution is erroneous if a range check that would have failed is suppressed (see 11.5), so an implementation does not have to generate code to check this case. (An unchecked failed predicate does not cause erroneous execution, so the test is required in that case.)

Like the checks associated with a per-object constraint, this test is not made during the elaboration of a [subtype_indication](./AA-3.2#S0027). 

The elaboration of a [variant_part](./AA-3.8#S0071) consists of the elaboration of the [component_list](./AA-3.8#S0068) of each [variant](./AA-3.8#S0072) in the order in which they appear. 


#### Examples

Example of record type with a variant part: 

```ada
type Device is (Printer, Disk, Drum);
type State  is (Open, Closed);

```

```ada
type Peripheral(Unit : Device := Disk) is
   record
      Status : State;
      case Unit is
         when Printer =&gt
            Line_Count : Integer range 1 .. Page_Size;
         when others =&gt
            Cylinder   : Cylinder_Index;
            Track      : Track_Number;
      end case;
   end record;

```

Examples of record subtypes: 

```ada
subtype Drum_Unit is Peripheral(Drum);
subtype Disk_Unit is Peripheral(Disk);

```

Examples of constrained record variables: 

```ada
Writer   : Peripheral(Unit  =&gt Printer);
Archive  : Disk_Unit;

```


#### Extensions to Ada 83

In Ada 83, the discriminant of a [variant_part](./AA-3.8#S0071) is not allowed to be of a generic formal type. This restriction is removed in Ada 95; an others [discrete_choice](./AA-3.8#S0074) is required in this case. 


#### Wording Changes from Ada 83

The syntactic category choice is removed. The syntax rules for [variant](./AA-3.8#S0072), [array_aggregate](./AA-4.3#S0113), and [case_statement](./AA-5.4#S0176) now use [discrete_choice_list](./AA-3.8#S0073) or [discrete_choice](./AA-3.8#S0074) instead. The syntax rule for [record_aggregate](./AA-4.3#S0107) now defines its own syntax for named associations.

{AI05-0299-1} We have added the term Discrete Choice to the title since this is where they are talked about. This is analogous to the name of the subclause "Index Constraints and Discrete Ranges" in the subclause on Array Types.

The rule requiring that the discriminant denote a discriminant of the type being defined seems to have been left implicit in RM83. 


#### Incompatibilities With Ada 2005

{AI05-0158-1} Membership tests are no longer allowed as a [discrete_choice](./AA-3.8#S0074), in order that those tests can be expanded to allow multiple tests in a single expression without ambiguity. Since a membership test has a boolean type, they are very unlikely to be used as a [discrete_choice](./AA-3.8#S0074). 


#### Extensions to Ada 2005

{AI05-0153-3} Subtypes with static predicates can be used in [discrete_choice](./AA-3.8#S0074)s, and the coverage rules are modified to respect the predicates.

{AI05-0188-1} Variants in generic specifications are no longer rejected if the subtype of the actual type does not include all of the case choices. This probably isn't useful, but it is consistent with the treatment of [case_expression](./AA-4.5#S0151)s. 


#### Wording Changes from Ada 2005

{AI05-0290-1} Added a test that some [variant](./AA-3.8#S0072) covers the value of a discriminant that governs a [variant_part](./AA-3.8#S0071). This is similar to the test that some case limb covers the value of the Selecting_[expression](./AA-4.4#S0132) of a [case_statement](./AA-5.4#S0176). This test cannot change the behavior of any nonerroneous Ada 2005 program, so it is not an inconsistency. 


#### Wording Changes from Ada 2012

{AI12-0071-1} Corrigendum: Updated wording of the coverage rules to use the new term "satisfies the predicates" (see 3.2.4). 

