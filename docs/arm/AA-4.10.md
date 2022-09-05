---
sidebar_position:  38
---

# 4.10  Image Attributes

{AI12-0315-1} An image of a value is a string representing the value in display form. The attributes Image, Wide_Image, and Wide_Wide_Image are available to produce the image of a value as a String, Wide_String, or Wide_Wide_String (respectively). User-defined images for a given type can be implemented by overriding the default implementation of the attribute Put_Image. 


#### Static Semantics

{AI12-0020-1} {AI12-0315-1} For every subtype S of a type T other than universal_real or universal_fixed, the following type-related operational attribute is defined:

S'Put_Image{AI12-0020-1} {AI12-0320-1} {AI12-0340-1} S'Put_Image denotes a procedure with the following specification: 

```ada
procedure S'Put_Image
   (Buffer : in out 
             Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
    Arg   : in T);

```

{AI12-0315-1} {AI12-0340-1} The default implementation of S'Put_Image writes (using Wide_Wide_Put) an image of the value of Arg.

{AI12-0020-1} {AI12-0419-1} The Put_Image attribute may be specified for any specific type T either via an [attribute_definition_clause](./AA-13.3#S0349) or via an [aspect_specification](./AA-13.1#S0346) specifying the Put_Image aspect of the type.[ The Put_Image aspect is not inherited, but rather is implicitly composed for derived types, as defined below.]

Aspect Description for Put_Image: Procedure to define the image of a given type.

Discussion: In contrast, the Image, Wide_Image, and Wide_Wide_Image attributes and their associated aspects can not be specified. The behavior of any of these attributes is defined in terms of calls to the corresponding Put_Image procedure, so changes in their behavior may be accomplished via a Put_Image specification.

In earlier versions of Ada, Image and related attributes were defined only for scalar types. The definition of these attributes is now very different, but there should be no change in the behavior of existing programs as a result of these changes. 

{AI12-0427-1} For an [aspect_specification](./AA-13.1#S0346) or [attribute_definition_clause](./AA-13.3#S0349) specifying Put_Image, the subtype of the Arg parameter shall be the first subtype or the base subtype if scalar, and the first subtype if not scalar.

{AI12-0020-1} {AI12-0419-1} The behavior of the default implementation of S'Put_Image depends on the class of T.

{AI12-0020-1} {AI12-0419-1} {AI12-0435-1} For an untagged derived type, or a null extension, the default implementation of T'Put_Image invokes the Put_Image for its parent type on a conversion of the parameter of type T to the parent type.

{AI12-0020-1} {AI12-0340-1} {AI12-0384-2} {AI12-0419-1} For a nonderived elementary type, the implementation is equivalent to: 

```ada
procedure Scalar_Type'Put_Image
  (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
   Arg    : in Scalar_Type) is
begin
   Buffer.Wide_Wide_Put (&ltdescribed below&gt);
end Scalar_Type'Put_Image;

```

where the Wide_Wide_String value written out to the text buffer is defined as follows:

{AI12-0020-1} For an integer type, the image written out is the corresponding decimal literal, without underlines, leading zeros, exponent, or trailing spaces, but with a single leading character that is either a minus sign or a space.

Implementation Note: If the machine supports negative zeros for signed integer types, it is not specified whether " 0" or "-0" should be returned for negative zero. We don't have enough experience with such machines to know what is appropriate, and what other languages do. In any case, the implementation should be consistent. 

Discussion: We allow S'Put_Image when S is universal_integer or root_integer, because the details of the desired string do not depend on properties of an integer type. While S'Put_Image cannot be called directly for these types (as they cannot be named), it can be called as part of evaluating an Image attribute. Note that other rules of the language ensure that an implementation can evaluate any universal_integer attribute using type root_integer; therefore, Constraint_Error could be raised by the evaluation of an Image attribute if the value of the prefix is outside of the range of root_integer. 

{AI12-0020-1} For an enumeration type, the image written out is either the corresponding identifier in upper case or the corresponding character literal (including the two apostrophes); neither leading nor trailing spaces are included. For a nongraphic character (a value of a character type that has no enumeration literal associated with it), the value is a corresponding language-defined name in upper case (for example, the image of the nongraphic character identified as nul is "NUL" - the quotes are not part of the image).

Implementation Note: For an enumeration type T that has "holes" (caused by an enumeration_representation_clause), T'Put_Image should raise Program_Error if the value is one of the holes (which is a bounded error anyway, since holes can be generated only via uninitialized variables and similar things). 

{AI12-0020-1} For a floating point type, the image written out is a decimal real literal best approximating the value (rounded away from zero if halfway between) with a single leading character that is either a minus sign or a space, a single digit (that is nonzero unless the value is zero), a decimal point, S'Digits-1 (see 3.5.8) digits  after the decimal point (but one if S'Digits is one), an upper case E, the sign of the exponent (either + or -), and two or more digits (with leading zeros if necessary) representing the exponent. If S'Signed_Zeros is True, then the leading character is a minus sign for a negatively signed zero.

To be honest: Leading zeros are present in the exponent only if necessary to make the exponent at least two digits. 

Reason: This image is intended to conform to that produced by Text_IO.Float_IO.Put in its default format. 

Implementation Note: The rounding direction is specified here to ensure portability of output results. 

Reason: {AI12-0315-1} We do not allow S'Put_Image when S is universal_real, as the details of the desired string depend on the properties of the (specific) type of S. Specifically, universal_real does not have a defined value for S'Digits. 

{AI12-0020-1} For a fixed point type, the image written out is a decimal real literal best approximating the value (rounded away from zero if halfway between) with a single leading character that is either a minus sign or a space, one or more digits before the decimal point (with no redundant leading zeros), a decimal point, and S'Aft (see 3.5.10) digits after the decimal point.

Reason: This image is intended to conform to that produced by Text_IO.Fixed_IO.Put. 

Implementation Note: The rounding direction is specified here to ensure portability of output results.

For a machine that supports negative zeros, it is not specified whether " 0.000" or "-0.000" is returned. See corresponding comment above about integer types with signed zeros. 

Reason: {AI12-0315-1} We do not allow S'Put_Image when S is universal_fixed, as the details of the desired string depend on the properties of the (specific) type of S. Specifically, universal_fixed does not have a defined value for S'Aft. 

{AI12-0020-1} For an access type (named or anonymous), the image written out depends on whether the value is null. If it is null, then the image is "NULL". Otherwise the image is a left parenthesis followed by "ACCESS", a space, and a sequence of graphic characters, other than space or right parenthesis, representing the location of the designated object, followed by a right parenthesis, as in "(ACCESS FF0012AC)".

To be honest: {AI12-0315-1} S'Put_Image is defined for universal_access, but it can never be called (as no legal [prefix](./AA-4.1#S0093) of Image has that type, and that type cannot be named preventing direct calls). 

{AI12-0020-1} {AI12-0419-1} {AI12-0435-1} For a nonnull type extension, the default implementation of T'Put_Image depends on whether there exists a noninterface ancestor of T (other than T itself) for which the Put_Image aspect has been directly specified. If so, then T'Put_Image will generate an image based on extension aggregate syntax where the ancestor type of the extension aggregate is the nearest ancestor type whose Put_Image aspect has been specified. If no such ancestor exists, then the default implementation of T'Put_Image is the same as described below for a nonderived record type.

Discussion: This might generate an image such as "(This Text Was User-Generated with C1 =&gt  123, C2 =&gt  456)" where the "This Text was User-Generated" portion of the text was generated by the call to the user-specified Put_Image routine. 

{AI12-0419-1} For a specific, nonderived composite type:

{AI12-0020-1} If the default implementation of Put_Image writes components, the order in which components are written is the same canonical order in which components of a composite type T are written out by the default implementation of T'Write.  [This is also the order that is used in determining the meaning of a positional aggregate of type T.]

Discussion: {AI12-0020-1} {AI12-0384-2} In general, the default implementation of T'Put_Image for a composite type will involve some sequence of calls to Put and its Wide and Wide_Wide variants and calls to the Put_Image procedures of component types and, in the case of an array type, index types. The Put calls may pass in either literal values (e.g., "(", ")", "'(", " =&gt ", or ", "), or other things (such as component names for record values, task_id images for tasks, or the Wide_Wide_Expanded_Name of the tag in the class-wide case). 

{AI12-0020-1} {AI12-0419-1} For an array type T, the default implementation of T'Put_Image generates an image based on named (not positional) array aggregate syntax (with '[' and ']' as the delimiters) using calls to the Put_Image procedures of the index type(s) and the element type to generate images for values of those types.

Discussion: An array type might generate an image such as: 

```ada
[ 1 =&gt [ 1 =&gt [ 123 =&gt True,  124 =&gt False]
         2 =&gt [ 123 =&gt False,  124 =&gt False]],
  2 =&gt [ 1 =&gt [ 123 =&gt True,  124 =&gt True],
         2 =&gt [ 123 =&gt True,  124 =&gt False]]]

```

although perhaps with different white space and/or line breaking. 

{AI12-0020-1} The case of a null array is handled specially, using ranges for index bounds and "&lt&gt" as a syntactic component-value placeholder.

Discussion: This might generate an image such as "[ 1 ..  3 =&gt [ 1 ..  0 =&gt [ 1 .. 5 =&gt &lt&gt]]]", where the use of "&lt&gt" (among other things) indicates that the overall array is a null array and has no actual elements. 

{AI12-0020-1} {AI12-0419-1} For a record type (or, as indicated above, a type extension with no noninterface ancestor specifying Put_Image), or a protected type, the default implementation of T'Put_Image generates an image based on named (not positional) record aggregate syntax (except that for a protected type, the initial left parenthesis is followed by "PROTECTED with "). Component names are displayed in upper case, following the rules for the image of an enumeration value. Component values are displayed via calls to the component type's Put_Image procedure.

Discussion: This might generate an image such as: 

```ada
"(FOO =&gt [1 =&gt 'c',  2 =&gt 'a',  3 =&gt 't'], BAR =&gt TRUE)"

```

{AI12-0020-1} The image written out for a record having no components (including any interface type) is "(NULL RECORD)". The image written out for a componentless protected type is "(PROTECTED NULL RECORD)". In the case of a protected type T, a call to the default implementation of T'Put_Image begins only one protected (read-only) action.

Implementation Note: The expected, but not required, implementation model for generating the image of a protected record involves the compiler producing a "helper" protected function which T'Put_Image would call. The result type of this function might be a null record; it is only a function because it does not need a write-lock, not because it returns a meaningful result.

The assertion in the following example should succeed: 

```ada
type T1 (D1, D2 : Positive) is record ... end record; -- untagged
type T2 (D : Positive) is new T1 (D1 =&gt D, D2 =&gt D);
X : T2 (D =&gt 123) := ... ;
pragma Assert (X'Image /= T1(X)'Image);

```

{AI12-0020-1} For an undiscriminated task type, the default implementation of T'Put_Image generates an image of the form "(TASK &lttask_id_image&gt)" where &lttask_id_image&gt is the result obtained by calling Task_Identification.Image with the id of the given task and then passing that String to Characters.Conversions.To_Wide_Wide_String.

{AI12-0020-1} For a discriminated task type, the default implementation of T'Put_Image also includes discriminant values, as in: 

```ada
"(TASK &lttask_id_image&gt with D1 =&gt  123, D2 =&gt  456)"

```

{AI12-0020-1} {AI12-0384-2} For a class-wide type, the default implementation of T'Put_Image generates an image based on qualified expression syntax. Wide_Wide_Put is called with Wide_Wide_Expanded_Name of Arg'Tag. Then S'Put_Image is called, where S is the specific type identified by Arg'Tag.

Implementation Note: This will typically require a dispatching call. 

Discussion: This might generate an image such as: 

```ada
SOME_PACKAGE.SOME_TAGGED_TYPE'
   (COMPONENT_1 =&gt  123, COMPONENT_2 =&gt 456)

```

The parentheses in this case are generated by the call to Some_Tagged_Type'Put_Image. 

{AI12-0020-1} {AI12-0384-2} [T'Put_Image is the same for both the partial view and full view of T, if T has a partial view.]

Proof: {AI12-0384-2} A type-related operational aspect is the same for the full view and partial view of a type. See 13.1. 

{AI12-0435-1} In the [parameter_and_result_profile](./AA-6.1#S0205) for the default implementation of Put_Image, the subtype of the Arg parameter is the base subtype of T if T is a scalar type, and the first subtype otherwise. For an [aspect_specification](./AA-13.1#S0346) or [attribute_definition_clause](./AA-13.3#S0349) specifying Put_Image, the subprogram name shall denote a nonabstract procedure whose second parameter is either of the first subtype of T, or as an option when T is scalar, the base subtype of T.

Ramification: Put_Image is never an abstract routine, even for an abstract type. Thus, Put_Image and Image can be called for any type. 

{AI12-0020-1} For every subtype S of a type T, the following attributes are defined:

S'Wide_Wide_Image{AI12-0020-1} {AI12-0340-1} {AI12-0384-2} S'Wide_Wide_Image denotes a function with the following specification: 

```ada
function S'Wide_Wide_Image(Arg : S'Base)
  return Wide_Wide_String

```

S'Wide_Wide_Image calls S'Put_Image passing Arg (which will typically store a sequence of character values in a text buffer) and then returns the result of retrieving the contents of that buffer with function Wide_Wide_Get. The lower bound of the result is one. [Any exception propagated by the call of S'Put_Image is propagated.]

S'Wide_Image{AI12-0020-1} {AI12-0340-1} {AI12-0384-2} S'Wide_Image denotes a function with the following specification: 

```ada
function S'Wide_Image(Arg : S'Base)
  return Wide_String

```

S'Wide_Image calls S'Put_Image passing Arg (which will typically store a sequence of character values in a text buffer) and then returns the result of retrieving the contents of that buffer with function Wide_Get. The lower bound of the result is one. [Any exception propagated by the call of S'Put_Image is propagated.]

S'Image{AI12-0020-1} {AI12-0340-1} {AI12-0384-2} S'Image denotes a function with the following specification: 

```ada
function S'Image(Arg : S'Base)
  return String

```

S'Image calls S'Put_Image passing Arg (which will typically store a sequence of character values in a text buffer) and then returns the result of retrieving the contents of that buffer with function Get. The lower bound of the result is one. [Any exception propagated by the call of S'Put_Image is propagated.]

{AI12-0124-1} {AI12-0020-1} {AI12-0315-1} For a [prefix](./AA-4.1#S0093) X of a type T other than universal_real or universal_fixed, the following attributes are defined:

X'Wide_Wide_Image{AI12-0124-1} {AI12-0020-1} X'Wide_Wide_Image denotes the result of calling function S'Wide_Wide_Image with Arg being X, where S is the nominal subtype of X.

X'Wide_Image{AI12-0124-1} {AI12-0020-1} X'Wide_Image denotes the result of calling function S'Wide_Image with Arg being X, where S is the nominal subtype of X.

X'Image{AI12-0124-1} {AI12-0020-1} X'Image denotes the result of calling function S'Image with Arg being X, where S is the nominal subtype of X. 


#### Implementation Permissions

{AI12-0020-1} An implementation may transform the image generated by the default implementation of S'Put_Image for a composite subtype S in the following ways:

If S is a composite subtype, the leading character of the image M of a component value or index value is a space, and the immediately preceding character (if any) is an open parenthesis, open bracket, or space, then the leading space of the image M may be omitted. 

Discussion: This means that it is permitted to generate "[1 =&gt 123, 2 =&gt 456]" instead of "[ 1 =&gt  123,  2 =&gt  456]". 

If S is an array subtype, the low bound of the array in each dimension equals the low bound of the corresponding index subtype, and the array value is not a null array value, then positional array aggregate syntax may be used.

Discussion: This means that it is permitted to generate "[TRUE, TRUE, FALSE]" instead of "[ 1 =&gt TRUE,  2 =&gt TRUE,  3 =&gt FALSE]" if the low bound of the index subtype is one. 

If S is an array subtype and the given value can be displayed using [named_array_aggregate](./AA-4.3#S0116) syntax where some [discrete_choice_list](./AA-3.8#S0073) identifies more than one index value by identifying a sequence of one or more ranges and values separated by vertical bars, then this image may be generated instead; this may involve the reordering of component values.

Discussion: This means that it is permitted to generate "[ 1 ..  2 |  5 =&gt TRUE,  3 ..  4 =&gt FALSE]" instead of "[ 1 =&gt TRUE,  2 =&gt TRUE,  3 =&gt FALSE,  4 =&gt FALSE,  5 =&gt TRUE]". 

Similarly, if S is a record subtype (or a discriminated type) and the given value can be displayed using named component association syntax where the length of some component_choice_list is greater than one, then this image may be generated instead; this may involve the reordering of component values.

Discussion: This means that it is permitted to generate "(F1 | F2 =&gt TRUE)" instead of "(F1 =&gt TRUE, F2 =&gt TRUE)". 

{AI12-0020-1} {AI12-0340-1} {AI12-0384-2} Additional spaces (Wide_Wide_Characters with position 32), and calls to the New_Line operation of a text buffer, may be inserted to improve readability of the generated image, with the spaces inserted directly or via use of the Increase_Indent and Decrease_Indent procedures.

{AI12-0384-2} For a string type, implementations may produce an image corresponding to a string literal.

{AI12-0384-2} For an unchecked union type, implementations may raise Program_Error or produce some recognizable image (such as "(UNCHECKED UNION)") that does not require reading the discriminants. 

{AI12-0304-1} For each language-defined nonscalar type T, T'Put_Image may be specified.

Discussion: This permission applies, in particular, to nonscalar types declared in language-defined generic packages, and to any language-defined private type, even if an implementation chooses to complete it as a scalar type. 

Ramification: For any language-defined scalar type T, T'Put_Image should not be specified; the Image attribute needs to return the language-defined image for such types. This is important for compatibility: the Image attribute has been available for scalar types for many Ada revisions, and programs can (and do!) depend on its behavior. 


#### Implementation Requirements

{AI12-0304-1} For each language-defined container type T (that is, each of the Vector, List, Map, Set, Tree, and Holder types defined in the various children of Ada.Containers), T'Put_Image shall be specified so that T'Image produces a result consistent with array aggregate syntax (using '[' and ']' as delimiters) as follows:

Vector images shall be consistent with the default image of an array type with the same index and component types.

Discussion: In particular, this means that the format is 	that of a named array aggregate. We have no recommendation on how to 	handle empty elements; if the implementation can identify them, it may 	wish to display them specially, but otherwise, they're just 	uninitialized elements. 

Map images shall be consistent with named array aggregate syntax, using key value images in place of discrete choice names. For example, [Key1 =&gt Value1, Key2 =&gt Value2].

Discussion: There is no recommendation about the order in which key/element pairs occur within a map image. In the case of multiple key values whose corresponding element values have the same image, there is no recommendation about factoring (that is, generating Key1 | Key2 =&gt Some_Value instead of Key1 =&gt Some_Value, Key2 =&gt Some_Value). 

Set, List, and Holder images shall be consistent with positional array aggregate syntax. List elements shall occur in order within an image of a list. The image of an empty holder shall be [].

Discussion: There is no recommendation about the order in which set elements occur within the image of a set. 

Tree images (and images of subtrees of trees) shall be consistent with positional array aggregate syntax. For example, [[1, 2], [111, 222, 333]].

{AI12-0304-1} For each language-defined nonscalar type T that has a primitive language-defined Image function whose profile is type conformant with that of T'Image (for example, Ada.Numerics.Float_Random.State has such an Image function), T'Put_Image shall be specified so that T'Image yields the same result as that Image function.


#### Implementation Advice

For each language-defined private type T, T'Image should generate an image that would be meaningful based only on the relevant public interfaces, as opposed to requiring knowledge of the implementation of the private type.

Implementation Advice: For each language-defined private type T, T'Image should generate an image that would be meaningful based only on the relevant public interfaces.


#### Extensions to Ada 2012

{AI12-0020-1} {AI12-0315-1} {AI12-0340-1} {AI12-0384-2} {AI12-0419-1} {AI12-0427-1} {AI12-0435-1} Attribute Put_Image is new. Attributes Image, Wide_Image, and Wide_Wide_Image now can be used with any type, and are defined in terms of Put_Image so that they can be redefined. 

