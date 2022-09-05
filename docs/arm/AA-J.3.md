---
sidebar_position:  193
---

# J.3  Reduced Accuracy Subtypes

A [digits_constraint](./AA-3.5#S0050) may be used to define a floating point subtype with a new value for its requested decimal precision, as reflected by its Digits attribute. Similarly, a [delta_constraint](./AA-J.3#S0367) may be used to define an ordinary fixed point subtype with a new value for its delta, as reflected by its Delta attribute. 

Discussion: It might be more direct to make these attributes specifiable via an [attribute_definition_clause](./AA-13.3#S0349), and eliminate the syntax for these _constraints. 


#### Syntax

{AI12-0152-1} delta_constraint<a id="S0367"></a> ::= delta static_[simple_expression](./AA-4.4#S0138) [[range_constraint](./AA-3.5#S0036)]


#### Name Resolution Rules

{AI12-0152-1} The [simple_expression](./AA-4.4#S0138) of a [delta_constraint](./AA-J.3#S0367) is expected to be of any real type. 


#### Legality Rules

{AI12-0152-1} The [simple_expression](./AA-4.4#S0138) of a [delta_constraint](./AA-J.3#S0367) shall be static.

For a [subtype_indication](./AA-3.2#S0027) with a [delta_constraint](./AA-J.3#S0367), the [subtype_mark](./AA-3.2#S0028) shall denote an ordinary fixed point subtype.

For a [subtype_indication](./AA-3.2#S0027) with a [digits_constraint](./AA-3.5#S0050), the [subtype_mark](./AA-3.2#S0028) shall denote either a decimal fixed point subtype or a floating point subtype (notwithstanding the rule given in 3.5.9 that only allows a decimal fixed point subtype). 

This paragraph was deleted.{AI95-00114-01} 


#### Static Semantics

{AI12-0152-1} A [subtype_indication](./AA-3.2#S0027) with a [subtype_mark](./AA-3.2#S0028) that denotes an ordinary fixed point subtype and a [delta_constraint](./AA-J.3#S0367) defines an ordinary fixed point subtype with a delta given by the value of the [simple_expression](./AA-4.4#S0138) of the [delta_constraint](./AA-J.3#S0367). If the [delta_constraint](./AA-J.3#S0367) includes a [range_constraint](./AA-3.5#S0036), then the ordinary fixed point subtype is constrained by the [range_constraint](./AA-3.5#S0036).

{AI12-0152-1} A [subtype_indication](./AA-3.2#S0027) with a [subtype_mark](./AA-3.2#S0028) that denotes a floating point subtype and a [digits_constraint](./AA-3.5#S0050) defines a floating point subtype with a requested decimal precision (as reflected by its Digits attribute) given by the value of the [simple_expression](./AA-4.4#S0138) of the [digits_constraint](./AA-3.5#S0050). If the [digits_constraint](./AA-3.5#S0050) includes a [range_constraint](./AA-3.5#S0036), then the floating point subtype is constrained by the [range_constraint](./AA-3.5#S0036). 


#### Dynamic Semantics

{AI12-0152-1} A [delta_constraint](./AA-J.3#S0367) is compatible with an ordinary fixed point subtype if the value of the [simple_expression](./AA-4.4#S0138) is no less than the delta of the subtype, and the [range_constraint](./AA-3.5#S0036), if any, is compatible with the subtype.

{AI12-0152-1} A [digits_constraint](./AA-3.5#S0050) is compatible with a floating point subtype if the value of the [simple_expression](./AA-4.4#S0138) is no greater than the requested decimal precision of the subtype, and the [range_constraint](./AA-3.5#S0036), if any, is compatible with the subtype.

The elaboration of a [delta_constraint](./AA-J.3#S0367) consists of the elaboration of the [range_constraint](./AA-3.5#S0036), if any. 

Reason: A numeric subtype is considered "constrained" only if a range constraint applies to it. The only effect of a [digits_constraint](./AA-3.5#S0050) or a [delta_constraint](./AA-J.3#S0367) without a [range_constraint](./AA-3.5#S0036) is to specify the value of the corresponding Digits or Delta attribute in the new subtype. The set of values of the subtype is not "constrained" in any way by such _constraints. 


#### Wording Changes from Ada 83

In Ada 83, a [delta_constraint](./AA-J.3#S0367) is called a fixed_point_constraint, and a [digits_constraint](./AA-3.5#S0050) is called a floating_point_constraint. We have adopted other terms because [digits_constraint](./AA-3.5#S0050)s apply primarily to decimal fixed point types now (they apply to floating point types only as an obsolescent feature). 


#### Wording Changes from Ada 2012

{AI12-0152-1} Corrigendum: Changed the syntax so that the value following delta in a [delta_constraint](./AA-J.3#S0367) is a [simple_expression](./AA-4.4#S0138). This is compatible as any expressions that would require extra parentheses are already illegal. The change is necessary to eliminate syntax ambguities in [derived_type_definition](./AA-3.4#S0035)s. The similar change for [digits_constraint](./AA-3.5#S0050) is documented in 3.5.9. 

