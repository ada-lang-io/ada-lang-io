---
sidebar_position:  68
---

# 8.4  Use Clauses

[A [use_package_clause](./AA-8.4#S0236) achieves direct visibility of declarations that appear in the visible part of a package; a [use_type_clause](./AA-8.4#S0237) achieves direct visibility of the primitive operators of a type.] 


#### Language Design Principles

If and only if the visibility rules allow P.A, "use P;" should make A directly visible (barring name conflicts). This means, for example, that child library units, and generic formals of a formal package whose [formal_package_actual_part](./AA-12.7#S0341) is (&lt&gt), should be made visible by a [use_clause](./AA-8.4#S0235) for the appropriate package.

The rules for [use_clause](./AA-8.4#S0235)s were carefully constructed to avoid so-called Beaujolais effects, where the addition or removal of a single [use_clause](./AA-8.4#S0235), or a single declaration in a "use"d package, would change the meaning of a program from one legal interpretation to another. 


#### Syntax

use_clause<a id="S0235"></a> ::= [use_package_clause](./AA-8.4#S0236) | [use_type_clause](./AA-8.4#S0237)

use_package_clause<a id="S0236"></a> ::= use package_[name](./AA-4.1#S0091) {, package_[name](./AA-4.1#S0091)};

{AI05-0150-1} use_type_clause<a id="S0237"></a> ::= use [all] type [subtype_mark](./AA-3.2#S0028) {, [subtype_mark](./AA-3.2#S0028)};


#### Legality Rules

{AI95-00217-06} A package_[name](./AA-4.1#S0091) of a [use_package_clause](./AA-8.4#S0236) shall denote a nonlimited view of a package. 

Ramification: This includes formal packages. 


#### Static Semantics

For each [use_clause](./AA-8.4#S0235), there is a certain region of text called the scope of the [use_clause](./AA-8.4#S0235). For a [use_clause](./AA-8.4#S0235) within a [context_clause](./AA-10.1#S0292) of a [library_unit_declaration](./AA-10.1#S0288) or [library_unit_renaming_declaration](./AA-10.1#S0289), the scope is the entire declarative region of the declaration. For a [use_clause](./AA-8.4#S0235) within a [context_clause](./AA-10.1#S0292) of a body, the scope is the entire body [and any subunits (including multiply nested subunits). The scope does not include [context_clause](./AA-10.1#S0292)s themselves.]

For a [use_clause](./AA-8.4#S0235) immediately within a declarative region, the scope is the portion of the declarative region starting just after the [use_clause](./AA-8.4#S0235) and extending to the end of the declarative region. However, the scope of a [use_clause](./AA-8.4#S0235) in the private part of a library unit does not include the visible part of any public descendant of that library unit. 

Reason: The exception echoes the similar exception for "immediate scope (of a declaration)" (see 8.2). It makes [use_clause](./AA-8.4#S0235)s work like this: 

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

{AI95-00217-06} A package is named in a [use_package_clause](./AA-8.4#S0236) if it is denoted by a package_[name](./AA-4.1#S0091) of that clause. A type is named in a [use_type_clause](./AA-8.4#S0237) if it is determined by a [subtype_mark](./AA-3.2#S0028) of that clause.

{AI95-00217-06} {AI05-0150-1} For each package named in a [use_package_clause](./AA-8.4#S0236) whose scope encloses a place, each declaration that occurs immediately within the declarative region of the package is potentially use-visible at this place if the declaration is visible at this place. For each type T or T'Class named in a [use_type_clause](./AA-8.4#S0237) whose scope encloses a place, the declaration of each primitive operator of type T is potentially use-visible at this place if its declaration is visible at this place. If a [use_type_clause](./AA-8.4#S0237) whose scope encloses a place includes the reserved word all, then the following entities are also potentially use-visible at this place if the declaration of the entity is visible at this place: 

{AI05-0150-1} Each primitive subprogram of T including each enumeration literal (if any);

{AI05-0150-1} Each subprogram that is declared immediately within the declarative region in which an ancestor type of T is declared and that operates on a class-wide type that covers T. 

Ramification: {AI05-0150-1} Primitive subprograms whose defining name is an [identifier](./AA-2.3#S0002) are not made potentially visible by a [use_type_clause](./AA-8.4#S0237) unless reserved word all is included. A [use_type_clause](./AA-8.4#S0237) without all is only for operators.

The semantics described here should be similar to the semantics for expanded names given in 4.1.3, "Selected Components" so as to achieve the effect requested by the "principle of equivalence of [use_clause](./AA-8.4#S0235)s and [selected_component](./AA-4.1#S0098)s". Thus, child library units and generic formal parameters of a formal package are potentially use-visible when their enclosing package is use'd.

The "visible at that place" part implies that applying a [use_clause](./AA-8.4#S0235) to a parent unit does not make all of its children use-visible - only those that have been made visible by a [with_clause](./AA-10.1#S0294). It also implies that we don't have to worry about hiding in the definition of "directly visible" - a declaration cannot be use-visible unless it is visible.

Note that "use type T'Class;" is equivalent to "use type T;", which helps avoid breaking the generic contract model. 

{AI05-0131-1} Certain implicit declarations may become potentially use-visible in certain contexts as described in 12.6.

A declaration is use-visible if it is potentially use-visible, except in these naming-conflict cases: 

A potentially use-visible declaration is not use-visible if the place considered is within the immediate scope of a homograph of the declaration.

Potentially use-visible declarations that have the same [identifier](./AA-2.3#S0002) are not use-visible unless each of them is an overloadable declaration. 

Ramification: Overloadable declarations don't cancel each other out, even if they are homographs, though if they are not distinguishable by formal parameter names or the presence or absence of [default_expression](./AA-3.7#S0063)s, any use will be ambiguous. We only mention [identifier](./AA-2.3#S0002)s here, because declarations named by [operator_symbol](./AA-6.1#S0202)s are always overloadable, and hence never cancel each other. Direct visibility is irrelevant for [character_literal](./AA-2.5#S0015)s. 


#### Dynamic Semantics

The elaboration of a [use_clause](./AA-8.4#S0235) has no effect. 


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

If a given declaration is already immediately visible, then a [use_clause](./AA-8.4#S0235) that makes it potentially use-visible has no effect. Therefore, a [use_type_clause](./AA-8.4#S0237) for a type whose declaration appears in a place other than the visible part of a package has no effect; it cannot make a declaration use-visible unless that declaration is already immediately visible.

"Use type S1;" and "use type S2;" are equivalent if S1 and S2 are both subtypes of the same type. In particular, "use type S;" and "use type S'Base;" are equivalent. 

Reason: We considered adding a rule that prevented several declarations of views of the same entity that all have the same semantics from cancelling each other out. For example, if a (possibly implicit) [subprogram_declaration](./AA-6.1#S0195) for "+" is potentially use-visible, and a fully conformant renaming of it is also potentially use-visible, then they (annoyingly) cancel each other out; neither one is use-visible. The considered rule would have made just one of them use-visible. We gave up on this idea due to the complexity of the rule. It would have had to account for both overloadable and nonoverloadable [renaming_declaration](./AA-8.5#S0238)s, the case where the rule should apply only to some subset of the declarations with the same defining name, and the case of [subtype_declaration](./AA-3.2#S0026)s (since they are claimed to be sufficient for renaming of subtypes). 


#### Extensions to Ada 83

The [use_type_clause](./AA-8.4#S0237) is new to Ada 95. 


#### Wording Changes from Ada 83

The phrase "omitting from this set any packages that enclose this place" is no longer necessary to avoid making something visible outside its scope, because we explicitly state that the declaration has to be visible in order to be potentially use-visible. 


#### Wording Changes from Ada 95

{AI95-00217-06} Limited views of packages are not allowed in use clauses. Defined named in a use clause for use in other limited view rules (see 10.1.2). 


#### Extensions to Ada 2005

{AI05-0150-1} The use all type version of the [use_type_clause](./AA-8.4#S0237) is new to Ada 2012. It works similarly to prefixed views. 


#### Wording Changes from Ada 2005

{AI05-0131-1} Correction: Added wording to allow other declarations to be potentially use-visible, to support corrections to formal subprograms. 

