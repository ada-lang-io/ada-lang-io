---
sidebar_position:  22
---

# 3.6  Array Types

An array object is a composite object consisting of components which all have the same subtype. The name for a component of an array uses one or more index values belonging to specified discrete types. The value of an array object is a composite value consisting of the values of the components. 


#### Syntax

array_type_definition<a id="S0051"></a> ::= 
   [unconstrained_array_definition](./AA-3.6#S0052) | [constrained_array_definition](./AA-3.6#S0054)

unconstrained_array_definition<a id="S0052"></a> ::= 
   array([index_subtype_definition](./AA-3.6#S0053) {, [index_subtype_definition](./AA-3.6#S0053)}) of [component_definition](./AA-3.6#S0056)

index_subtype_definition<a id="S0053"></a> ::= [subtype_mark](./AA-3.2#S0028) range &lt&gt

constrained_array_definition<a id="S0054"></a> ::= 
   array ([discrete_subtype_definition](./AA-3.6#S0055) {, [discrete_subtype_definition](./AA-3.6#S0055)}) of [component_definition](./AA-3.6#S0056)

discrete_subtype_definition<a id="S0055"></a> ::= discrete_[subtype_indication](./AA-3.2#S0027) | [range](./AA-3.5#S0037)

{AI95-00230-01} {AI95-00406-01} component_definition<a id="S0056"></a> ::= 
   [aliased] [subtype_indication](./AA-3.2#S0027)
 | [aliased] [access_definition](./AA-3.10#S0084)


#### Name Resolution Rules

For a [discrete_subtype_definition](./AA-3.6#S0055) that is a [range](./AA-3.5#S0037), the [range](./AA-3.5#S0037) shall resolve to be of some specific discrete type[; which discrete type shall be determined without using any context other than the bounds of the [range](./AA-3.5#S0037) itself (plus the preference for root_integer - see 8.6).] 


#### Legality Rules

Each [index_subtype_definition](./AA-3.6#S0053) or [discrete_subtype_definition](./AA-3.6#S0055) in an [array_type_definition](./AA-3.6#S0051) defines an index subtype; its type (the index type) shall be discrete. 

Discussion: An index is a discrete quantity used to select along a given dimension of an array. A component is selected by specifying corresponding values for each of the indices. 

The subtype defined by the [subtype_indication](./AA-3.2#S0027) of a [component_definition](./AA-3.6#S0056) (the component subtype) shall be a definite subtype. 

Ramification: This applies to all uses of [component_definition](./AA-3.6#S0056), including in [record_type_definition](./AA-3.8#S0066)s and [protected_definition](./AA-9.4#S0251)s.

This paragraph was deleted.{AI95-00363-01} 


#### Static Semantics

An array is characterized by the number of indices (the dimensionality of the array), the type and position of each index, the lower and upper bounds for each index, and the subtype of the components. The order of the indices is significant.

A one-dimensional array has a distinct component for each possible index value. A multidimensional array has a distinct component for each possible sequence of index values that can be formed by selecting one value for each index position (in the given order). The possible values for a given index are all the values between the lower and upper bounds, inclusive; this range of values is called the index range. The bounds of an array are the bounds of its index ranges. The length of a dimension of an array is the number of values of the index range of the dimension (zero for a null range). The length of a one-dimensional array is the length of its only dimension.

An [array_type_definition](./AA-3.6#S0051) defines an array type and its first subtype. For each object of this array type, the number of indices, the type and position of each index, and the subtype of the components are as in the type definition[; the values of the lower and upper bounds for each index belong to the corresponding index subtype of its type, except for null arrays (see 3.6.1)].

{AI12-0444-1} An [unconstrained_array_definition](./AA-3.6#S0052) defines an array type with an unconstrained first subtype. Each [index_subtype_definition](./AA-3.6#S0053) defines the corresponding index subtype to be the subtype denoted by the [subtype_mark](./AA-3.2#S0028). [ The compound delimiter &lt&gt (called a box) of an [index_subtype_definition](./AA-3.6#S0053) stands for an undefined range (different objects of the type can have different bounds).]

A [constrained_array_definition](./AA-3.6#S0054) defines an array type with a constrained first subtype. Each [discrete_subtype_definition](./AA-3.6#S0055) defines the corresponding index subtype, as well as the corresponding index range for the constrained first subtype. The constraint of the first subtype consists of the bounds of the index ranges. 

Discussion: {AI05-0005-1} Although there is no nameable unconstrained array subtype in this case, the predefined slicing and concatenation operations can operate on and yield values that do not necessarily belong to the first array subtype. This is also true for Ada 83. 

The discrete subtype defined by a [discrete_subtype_definition](./AA-3.6#S0055) is either that defined by the [subtype_indication](./AA-3.2#S0027), or a subtype determined by the [range](./AA-3.5#S0037) as follows: 

If the type of the [range](./AA-3.5#S0037) resolves to root_integer, then the [discrete_subtype_definition](./AA-3.6#S0055) defines a subtype of the predefined type Integer with bounds given by a conversion to Integer of the bounds of the [range](./AA-3.5#S0037); 

Reason: This ensures that indexing over the discrete subtype can be performed with regular Integers, rather than only universal_integers. 

Discussion: We considered doing this by simply creating a "preference" for Integer when resolving the [range](./AA-3.5#S0037). However, this can introduce Beaujolais effects when the [simple_expression](./AA-4.4#S0138)s involve calls on functions visible due to use clauses. 

Otherwise, the [discrete_subtype_definition](./AA-3.6#S0055) defines a subtype of the type of the [range](./AA-3.5#S0037), with the bounds given by the [range](./AA-3.5#S0037). 

The [component_definition](./AA-3.6#S0056) of an [array_type_definition](./AA-3.6#S0051) defines the nominal subtype of the components. If the reserved word aliased appears in the [component_definition](./AA-3.6#S0056), then each component of the array is aliased (see 3.10). 


#### Dynamic Semantics

The elaboration of an [array_type_definition](./AA-3.6#S0051) creates the array type and its first subtype, and consists of the elaboration of any [discrete_subtype_definition](./AA-3.6#S0055)s and the [component_definition](./AA-3.6#S0056).

{8652/0002} {AI95-00171-01} {AI95-00230-01} The elaboration of a [discrete_subtype_definition](./AA-3.6#S0055) that does not contain any per-object expressions creates the discrete subtype, and consists of the elaboration of the [subtype_indication](./AA-3.2#S0027) or the evaluation of the [range](./AA-3.5#S0037). The elaboration of a [discrete_subtype_definition](./AA-3.6#S0055) that contains one or more per-object expressions is defined in 3.8. The elaboration of a [component_definition](./AA-3.6#S0056) in an [array_type_definition](./AA-3.6#S0051) consists of the elaboration of the [subtype_indication](./AA-3.2#S0027) or [access_definition](./AA-3.10#S0084). The elaboration of any [discrete_subtype_definition](./AA-3.6#S0055)s and the elaboration of the [component_definition](./AA-3.6#S0056) are performed in an arbitrary order. 


#### Static Semantics

{AI05-0228-1} For an array type with a scalar component type, the following language-defined representation aspect may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1): 

Default_Component_ValueThis aspect shall be specified by a static expression, and that expression shall be explicit, even if the aspect has a boolean type. Default_Component_Value shall be specified only on a [full_type_declaration](./AA-3.2#S0024). 

Reason: The part about requiring an explicit expression is to disallow omitting the value for this aspect, which would otherwise be allowed by the rules of 13.1.1.

This is a representation attribute in order to disallow specifying it on a derived type that has inherited primitive subprograms; that is necessary as the sizes of out parameters could be different whether or not a Default_Value is specified (see 6.4.1). 

Aspect Description for Default_Component_Value: Default value for the components of an array-of-scalar subtype.

{AI05-0228-1} {AI12-0427-1} If a derived type inherits a boolean Default_Component_Value aspect, the aspect may be specified to have any value for the derived type. 

Reason: This overrides the 13.1.1 rule that says that a boolean aspect with a value True cannot be changed. 


#### Name Resolution Rules

{AI05-0228-1} The expected type for the [expression](./AA-4.4#S0132) specified for the Default_Component_Value aspect is the component type of the array type defined by the [full_type_declaration](./AA-3.2#S0024) on which it appears. 

NOTE 1   All components of an array have the same subtype. In particular, for an array of components that are one-dimensional arrays, this means that all components have the same bounds and hence the same length.

NOTE 2   Each elaboration of an [array_type_definition](./AA-3.6#S0051) creates a distinct array type. A consequence of this is that each object whose [object_declaration](./AA-3.3#S0032) contains an [array_type_definition](./AA-3.6#S0051) is of its own unique type. 


#### Examples

Examples of type declarations with unconstrained array definitions: 

```ada
type Vector     is array(Integer  range &lt&gt) of Real;
type Matrix     is array(Integer  range &lt&gt, Integer range &lt&gt) of Real;
type Bit_Vector is array(Integer  range &lt&gt) of Boolean;
type Roman      is array(Positive range &lt&gt) of Roman_Digit; -- see 3.5.2

```

Examples of type declarations with constrained array definitions: 

```ada
type Table    is array(1 .. 10) of Integer;
type Schedule is array(Day) of Boolean;
type Line     is array(1 .. Max_Line_Size) of Character;

```

Examples of object declarations with array type definitions: 

```ada
{AI95-00433-01} Grid      : array(1 .. 80, 1 .. 100) of Boolean;
Mix       : array(Color range Red .. Green) of Boolean;
Msg_Table : constant array(Error_Code) of access constant String :=
      (Too_Big =&gt new String'("Result too big"), Too_Small =&gt ...);
Page      : array(Positive range &lt&gt) of Line :=  --  an array of arrays
  (1 | 50  =&gt Line'(1 | Line'Last =&gt '+', others =&gt '-'),  -- see 4.3.3
   2 .. 49 =&gt Line'(1 | Line'Last =&gt '|', others =&gt ' '));
    -- Page is constrained by its initial value to (1..50)

```


#### Extensions to Ada 83

The syntax rule for [component_definition](./AA-3.6#S0056) is modified to allow the reserved word aliased.

The syntax rules for [unconstrained_array_definition](./AA-3.6#S0052) and [constrained_array_definition](./AA-3.6#S0054) are modified to use [component_definition](./AA-3.6#S0056) (instead of component_[subtype_indication](./AA-3.2#S0027)). The effect of this change is to allow the reserved word aliased before the component [subtype_indication](./AA-3.2#S0027).

A [range](./AA-3.5#S0037) in a [discrete_subtype_definition](./AA-3.6#S0055) may use arbitrary universal expressions for each bound (e.g. 1 .. 3+5), rather than strictly "implicitly convertible" operands. The subtype defined will still be a subtype of Integer. 


#### Wording Changes from Ada 83

We introduce a new syntactic category, [discrete_subtype_definition](./AA-3.6#S0055), as distinct from [discrete_range](./AA-3.6#S0058). These two constructs have the same syntax, but their semantics are quite different (one defines a subtype, with a preference for Integer subtypes, while the other just selects a subrange of an existing subtype). We use this new syntactic category in for loops and entry families.

The syntax for [index_constraint](./AA-3.6#S0057) and [discrete_range](./AA-3.6#S0058) have been moved to their own subclause, since they are no longer used here.

The syntax rule for [component_definition](./AA-3.6#S0056) (formerly component_subtype_definition) is moved here from RM83-3.7. 


#### Extensions to Ada 95

{AI95-00230-01} {AI95-00406-01} Array components can have an anonymous access type.

{AI95-00363-01} The prohibition against unconstrained discriminated aliased components has been lifted. It has been replaced by a prohibition against the actual troublemakers: general access discriminant constraints (see 3.7.1). 


#### Wording Changes from Ada 95

{8652/0002} {AI95-00171-01} Corrigendum: Added wording to allow the elaboration of per-object constraints for constrained arrays. 


#### Extensions to Ada 2005

{AI05-0228-1} The new aspect Default_Component_Value allows defining implicit initial values (see 3.3.1) for arrays of scalar types. 


## 3.6.1  Index Constraints and Discrete Ranges

An [index_constraint](./AA-3.6#S0057) determines the range of possible values for every index of an array subtype, and thereby the corresponding array bounds. 


#### Syntax

index_constraint<a id="S0057"></a> ::=  ([discrete_range](./AA-3.6#S0058) {, [discrete_range](./AA-3.6#S0058)})

discrete_range<a id="S0058"></a> ::= discrete_[subtype_indication](./AA-3.2#S0027) | [range](./AA-3.5#S0037)


#### Name Resolution Rules

The type of a [discrete_range](./AA-3.6#S0058) is the type of the subtype defined by the [subtype_indication](./AA-3.2#S0027), or the type of the [range](./AA-3.5#S0037). For an [index_constraint](./AA-3.6#S0057), each [discrete_range](./AA-3.6#S0058) shall resolve to be of the type of the corresponding index. 

Discussion: In Ada 95, [index_constraint](./AA-3.6#S0057)s only appear in a [subtype_indication](./AA-3.2#S0027); they no longer appear in [constrained_array_definition](./AA-3.6#S0054)s. 


#### Legality Rules

An [index_constraint](./AA-3.6#S0057) shall appear only in a [subtype_indication](./AA-3.2#S0027) whose [subtype_mark](./AA-3.2#S0028) denotes either an unconstrained array subtype, or an unconstrained access subtype whose designated subtype is an unconstrained array subtype; in either case, the [index_constraint](./AA-3.6#S0057) shall provide a [discrete_range](./AA-3.6#S0058) for each index of the array type. 


#### Static Semantics

A [discrete_range](./AA-3.6#S0058) defines a range whose bounds are given by the [range](./AA-3.5#S0037), or by the range of the subtype defined by the [subtype_indication](./AA-3.2#S0027). 


#### Dynamic Semantics

An [index_constraint](./AA-3.6#S0057) is compatible with an unconstrained array subtype if and only if the index range defined by each [discrete_range](./AA-3.6#S0058) is compatible (see 3.5) with the corresponding index subtype. If any of the [discrete_range](./AA-3.6#S0058)s defines a null range, any array thus constrained is a null array, having no components. An array value satisfies an [index_constraint](./AA-3.6#S0057) if at each index position the array value and the [index_constraint](./AA-3.6#S0057) have the same index bounds. 

Ramification: There is no need to define compatibility with a constrained array subtype, because one is not allowed to constrain it again.

The elaboration of an [index_constraint](./AA-3.6#S0057) consists of the evaluation of the [discrete_range](./AA-3.6#S0058)(s), in an arbitrary order. The evaluation of a [discrete_range](./AA-3.6#S0058) consists of the elaboration of the [subtype_indication](./AA-3.2#S0027) or the evaluation of the [range](./AA-3.5#S0037). 

NOTE 1   The elaboration of a [subtype_indication](./AA-3.2#S0027) consisting of a [subtype_mark](./AA-3.2#S0028) followed by an [index_constraint](./AA-3.6#S0057) checks the compatibility of the [index_constraint](./AA-3.6#S0057) with the [subtype_mark](./AA-3.2#S0028) (see 3.2.2).

NOTE 2   Even if an array value does not satisfy the index constraint of an array subtype, Constraint_Error is not raised on conversion to the array subtype, so long as the length of each dimension of the array value and the array subtype match. See 4.6. 


#### Examples

Examples of array declarations including an index constraint: 

```ada
{AI12-0442-1} Board     : Matrix(1 .. 8,  1 .. 8);  --  see 3.6
Rectangle : Matrix(1 .. 20, 1 .. 30);
Inverse   : Matrix(1 .. N,  1 .. N);  --  N can be nonstatic

```

```ada
{AI125-0430-1} Filter    : Bit_Vector(0 .. 31);      --  see 3.6

```

Example of array declaration with a constrained array subtype: 

```ada
My_Schedule : Schedule;  --  all arrays of type Schedule have the same bounds

```

Example of record type with a component that is an array: 

```ada
type Var_Line(Length : Natural) is
   record
      Image : String(1 .. Length);
   end record;

```

```ada
Null_Line : Var_Line(0);  --  Null_Line.Image is a null array

```


#### Extensions to Ada 83

We allow the declaration of a variable with a nominally unconstrained array subtype, so long as it has an initialization expression to determine its bounds. 


#### Wording Changes from Ada 83

We have moved the syntax for [index_constraint](./AA-3.6#S0057) and [discrete_range](./AA-3.6#S0058) here since they are no longer used in [constrained_array_definition](./AA-3.6#S0054)s. We therefore also no longer have to describe the (special) semantics of [index_constraint](./AA-3.6#S0057)s and [discrete_range](./AA-3.6#S0058)s that appear in [constrained_array_definition](./AA-3.6#S0054)s.

The rules given in RM83-3.6.1(5,7-10), which define the bounds of an array object, are redundant with rules given elsewhere, and so are not repeated here. RM83-3.6.1(6), which requires that the (nominal) subtype of an array variable be constrained, no longer applies, so long as the variable is explicitly initialized. 


## 3.6.2  Operations of Array Types


#### Legality Rules

[The argument N used in the [attribute_designator](./AA-4.1#S0101)s for the N-th dimension of an array shall be a static [expression](./AA-4.4#S0132) of some integer type.] The value of N shall be positive (nonzero) and no greater than the dimensionality of the array. 


#### Static Semantics

{8652/0006} {AI95-00030-01} The following attributes are defined for a [prefix](./AA-4.1#S0093) A that is of an array type [(after any implicit dereference)], or denotes a constrained array subtype: 

Ramification: These attributes are not defined if A is a subtype-mark for an access-to-array subtype. They are defined (by implicit dereference) for access-to-array values.

A'FirstA'First denotes the lower bound of the first index range; its type is the corresponding index type.

A'First(N)A'First(N) denotes the lower bound of the N-th index range; its type is the corresponding index type.

A'LastA'Last denotes the upper bound of the first index range; its type is the corresponding index type.

A'Last(N)A'Last(N) denotes the upper bound of the N-th index range; its type is the corresponding index type.

A'RangeA'Range is equivalent to the range A'First .. A'Last, except that the [prefix](./AA-4.1#S0093) A is only evaluated once.

A'Range(N)A'Range(N) is equivalent to the range A'First(N) .. A'Last(N), except that the [prefix](./AA-4.1#S0093) A is only evaluated once.

A'LengthA'Length denotes the number of values of the first index range (zero for a null range); its type is universal_integer.

A'Length(N)A'Length(N) denotes the number of values of the N-th index range (zero for a null range); its type is universal_integer. 


#### Implementation Advice

{AI05-0229-1} An implementation should normally represent multidimensional arrays in row-major order, consistent with the notation used for multidimensional array aggregates (see 4.3.3). However, if convention Fortran is specified for a multidimensional array type, then column-major order should be used instead (see B.5, "Interfacing with Fortran"). 

Implementation Advice: Multidimensional arrays should be represented in row-major order, unless the array has convention Fortran.

NOTE 1   The [attribute_reference](./AA-4.1#S0100)s A'First and A'First(1) denote the same value. A similar relation exists for the [attribute_reference](./AA-4.1#S0100)s A'Last, A'Range, and A'Length. The following relation is satisfied (except for a null array) by the above attributes if the index type is an integer type: 

```ada
   A'Length(N) = A'Last(N) - A'First(N) + 1

```

NOTE 2   An array type is limited if its component type is limited (see 7.5).

NOTE 3   The predefined operations of an array type include the membership tests, qualification, and explicit conversion. If the array type is not limited, they also include assignment and the predefined equality operators. For a one-dimensional array type, they include the predefined concatenation operators (if nonlimited) and, if the component type is discrete, the predefined relational operators; if the component type is boolean, the predefined logical operators are also included.

NOTE 4   {AI95-00287-01} A component of an array can be named with an [indexed_component](./AA-4.1#S0096). A value of an array type can be specified with an [array_aggregate](./AA-4.3#S0113). For a one-dimensional array type, a slice of the array can be named; also, string literals are defined if the component type is a character type. 


#### Examples

Examples (using arrays declared in the examples of subclause 3.6.1): 

```ada
--  Filter'First      =   0   Filter'Last       =  31   Filter'Length =  32
--  Rectangle'Last(1) =  20   Rectangle'Last(2) =  30

```


## 3.6.3  String Types


#### Static Semantics

A one-dimensional array type whose component type is a character type is called a string type.

{AI95-00285-01} [There are three predefined string types, String, Wide_String, and Wide_Wide_String, each indexed by values of the predefined subtype Positive; these are declared in the visible part of package Standard:] 

```ada
[subtype Positive is Integer range 1 .. Integer'Last;

```

```ada
{AI95-00285-01} type String is array(Positive range &lt&gt) of Character;
type Wide_String is array(Positive range &lt&gt) of Wide_Character;
type Wide_Wide_String is array(Positive range &lt&gt) of Wide_Wide_Character;
]

```

NOTE 1   String literals (see 2.6 and 4.2) are defined for all string types. The concatenation operator & is predefined for string types, as for all nonlimited one-dimensional array types. The ordering operators &lt, &lt=, &gt, and &gt= are predefined for string types, as for all one-dimensional discrete array types; these ordering operators correspond to lexicographic order (see 4.5.2).


#### Examples

Examples of string objects: 

```ada
Stars      : String(1 .. 120) := (1 .. 120 =&gt '*' );
Question   : constant String  := "How many characters?";
	-- Question'First = 1, Question'Last = 20
	-- Question'Length = 20 (the number of characters)

```

```ada
Ask_Twice  : String  := Question & Question;	-- constrained to (1..40)
Ninety_Six : constant Roman   := "XCVI";	-- see 3.5.2 and 3.6

```


#### Inconsistencies With Ada 83

The declaration of Wide_String in Standard hides a use-visible declaration with the same [defining_identifier](./AA-3.1#S0022). In rare cases, this might result in an inconsistency between Ada 83 and Ada 95. 


#### Incompatibilities With Ada 83

Because both String and Wide_String are always directly visible, an expression like 

```ada
"a" &lt "bc"

```

is now ambiguous, whereas in Ada 83 both string literals could be resolved to type String. 


#### Extensions to Ada 83

The type Wide_String is new (though it was approved by ARG for Ada 83 compilers as well). 


#### Wording Changes from Ada 83

We define the term string type as a natural analogy to the term character type. 


#### Inconsistencies With Ada 95

{AI95-00285-01} The declaration of Wide_Wide_String in Standard hides a use-visible declaration with the same [defining_identifier](./AA-3.1#S0022). In the (very) unlikely event that an Ada 95 program had depended on such a use-visible declaration, and the program remains legal after the substitution of Standard.Wide_Wide_String, the meaning of the program will be different. 


#### Extensions to Ada 95

{AI95-00285-01} The type Wide_Wide_String is new. 

