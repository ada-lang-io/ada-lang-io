---
sidebar_position:  58
---

# 7.1  Package Specifications and Declarations

[A package is generally provided in two parts: a [package_specification](./AA-7.1#S0230) and a [package_body](./AA-7.2#S0231). Every package has a [package_specification](./AA-7.1#S0230), but not all packages have a [package_body](./AA-7.2#S0231).] 


#### Syntax

package_declaration<a id="S0229"></a> ::= [package_specification](./AA-7.1#S0230);

{AI05-0183-1} package_specification<a id="S0230"></a> ::= 
    package [defining_program_unit_name](./AA-6.1#S0201)
        [[aspect_specification](./AA-13.1#S0346)] is
      {[basic_declarative_item](./AA-3.11#S0088)}
   [private
      {[basic_declarative_item](./AA-3.11#S0088)}]
    end [[[parent_unit_name](./AA-10.1#S0291).][identifier](./AA-2.3#S0002)]

If an [identifier](./AA-2.3#S0002) or [parent_unit_name](./AA-10.1#S0291).[identifier](./AA-2.3#S0002) appears at the end of a [package_specification](./AA-7.1#S0230), then this sequence of lexical elements shall repeat the [defining_program_unit_name](./AA-6.1#S0201). 


#### Legality Rules

{AI95-00434-01} A [package_declaration](./AA-7.1#S0229) or [generic_package_declaration](./AA-12.1#S0312) requires a completion [(a body)] if it contains any [basic_declarative_item](./AA-3.11#S0088) that requires a completion, but whose completion is not in its [package_specification](./AA-7.1#S0230). 

To be honest: {AI05-0229-1} If an implementation supports it, the body of a package or generic package may be imported (using aspect Import, see B.1), in which case no explicit body is allowed. 


#### Static Semantics

{AI95-00420-01} {AI95-00434-01} The first list of [basic_declarative_item](./AA-3.11#S0088)s of a [package_specification](./AA-7.1#S0230) of a package other than a generic formal package is called the visible part of the package. [ The optional list of [basic_declarative_item](./AA-3.11#S0088)s after the reserved word private (of any [package_specification](./AA-7.1#S0230)) is called the private part of the package. If the reserved word private does not appear, the package has an implicit empty private part.] Each list of [basic_declarative_item](./AA-3.11#S0088)s of a [package_specification](./AA-7.1#S0230) forms a declaration list of the package.

Ramification: This definition of visible part does not apply to generic formal packages - 12.7 defines the visible part of a generic formal package.

The implicit empty private part is important because certain implicit declarations occur there if the package is a child package, and it defines types in its visible part that are derived from, or contain as components, private types declared within the parent package. These implicit declarations are visible in children of the child package. See 10.1.1. 

[An entity declared in the private part of a package is visible only within the declarative region of the package itself (including any child units - see 10.1.1). In contrast, expanded names denoting entities declared in the visible part can be used even outside the package; furthermore, direct visibility of such entities can be achieved by means of [use_clause](./AA-8.4#S0235)s (see 4.1.3 and 8.4).] 


#### Dynamic Semantics

The elaboration of a [package_declaration](./AA-7.1#S0229) consists of the elaboration of its [basic_declarative_item](./AA-3.11#S0088)s in the given order. 

NOTE 1   The visible part of a package contains all the information that another program unit is able to know about the package.

NOTE 2   If a declaration occurs immediately within the specification of a package, and the declaration has a corresponding completion that is a body, then that body has to occur immediately within the body of the package. 

Proof: This follows from the fact that the declaration and completion are required to occur immediately within the same declarative region, and the fact that bodies are disallowed (by the Syntax Rules) in [package_specification](./AA-7.1#S0230)s. This does not apply to instances of generic units, whose bodies can occur in [package_specification](./AA-7.1#S0230)s. 


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

{AI12-0417-1} In Ada 83, a library package is allowed to have a body even if it doesn't need one. In Ada 95, a library package body is either required or forbidden - never optional. The workaround is to add aspect Elaborate_Body, or something else requiring a body, to each library package that has a body that isn't otherwise required. 


#### Wording Changes from Ada 83

{AI05-0299-1} We have moved the syntax into this subclause and the next subclause from RM83-7.1, "Package Structure", which we have removed.

RM83 was unclear on the rules about when a package requires a body. For example, RM83-7.1(4) and RM83-7.1(8) clearly forgot about the case of an incomplete type declared in a [package_declaration](./AA-7.1#S0229) but completed in the body. In addition, RM83 forgot to make this rule apply to a generic package. We have corrected these rules. Finally, since we now allow a [pragma](./AA-2.8#S0019) Import for any explicit declaration, the completion rules need to take this into account as well. 


#### Wording Changes from Ada 95

{AI95-00420-01} Defined "declaration list" to avoid ambiguity in other rules as to whether packages are included. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [package_specification](./AA-7.1#S0230). This is described in 13.1.1. 

