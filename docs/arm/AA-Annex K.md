---
sidebar_position:  24
---

# Annex K Language-Defined Attributes

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
All_Calls_Remote See E.2.3.

CPU See D.13.

No_Return See 6.5.1.

This annex summarizes the definitions given elsewhere of the language-defined attributes.

P'AccessFor a [prefix](S0086) P that denotes a subprogram:

See 3.10.2.

X'AccessFor a [prefix](S0086) X that denotes an aliased view of an object:

See 3.10.2.

X'AddressFor a prefix X that denotes an object, program unit, or label:

Denotes the address of the first of the storage elements allocated to X. For a program unit or label, this value refers to the machine code associated with the corresponding body or [statement](S0124). The value of this attribute is of type System.Address. See 13.3.

S'AdjacentFor every subtype S of a floating point type T:

S'Adjacent denotes a function with the following specification: 

```ada
function S'Adjacent (X, Towards : T)
  return T

```

If Towards = X, the function yields X; otherwise, it yields the machine number of the type T adjacent to X in the direction of Towards, if that machine number exists. If the result would be outside the base range of S, Constraint_Error is raised. When T'Signed_Zeros is True, a zero result has the sign of X. When Towards is zero, its sign has no bearing on the result. See A.5.3.

S'AftFor every fixed point subtype S:

S'Aft yields the number of decimal digits needed after the decimal point to accommodate the delta of the subtype S, unless the delta of the subtype S is greater than 0.1, in which case the attribute yields the value one. (S'Aft is the smallest positive integer N for which (10**N)*S'Delta is greater than or equal to one.) The value of this attribute is of the type universal_integer. See 3.5.10.

X'AlignmentFor a prefix X that denotes a subtype or object:

The Address of an object that is allocated under control of the implementation is an integral multiple of the Alignment of the object (that is, the Address modulo the Alignment is zero).The offset of a record component is a multiple of the Alignment of the component. For an object that is not allocated under control of the implementation (that is, one that is imported, that is allocated by a user-defined allocator, whose Address has been specified, or is designated by an access value returned by an instance of Unchecked_Conversion), the implementation may assume that the Address is an integral multiple of its Alignment. The implementation shall not assume a stricter alignment.

The value of this attribute is of type universal_integer, and nonnegative; zero means that the object is not necessarily aligned on a storage element boundary. See 13.3.

S'BaseFor every scalar subtype S:

S'Base denotes an unconstrained subtype of the type of S. This unconstrained subtype is called the base subtype of the type. See 3.5.

S'Bit_OrderFor every specific record subtype S:

Denotes the bit ordering for the type of S. The value of this attribute is of type System.Bit_Order. See 13.5.3.

P'Body_VersionFor a prefix P that statically denotes a program unit:

Yields a value of the predefined type String that identifies the version of the compilation unit that contains the body (but not any subunits) of the program unit. See E.3.

T'CallableFor a [prefix](S0086) T that is of a task type (after any implicit dereference):

Yields the value True when the task denoted by T is callable, and False otherwise; See 9.9.

E'CallerFor a [prefix](S0086) E that denotes an [entry_declaration](S0187):

Yields a value of the type Task_Id that identifies the task whose call is now being serviced. Use of this attribute is allowed only inside an [entry_body](S0190) or [accept_statement](S0188)corresponding to the [entry_declaration](S0187) denoted by E. See C.7.1.

S'CeilingFor every subtype S of a floating point type T:

S'Ceiling denotes a function with the following specification: 

```ada
function S'Ceiling (X : T)
  return T

```

The function yields the value X, i.e., the smallest (most negative) integral value greater than or equal to X. When X is zero, the result has the sign of X; a zero result otherwise has a negative sign when S'Signed_Zeros is True. See A.5.3.

S'ClassFor every subtype S of a tagged type T (specific or class-wide):

S'Class denotes a subtype of the class-wide type (called T'Class in this document) for the class rooted at T (or if S already denotes a class-wide subtype, then S'Class is the same as S).

S'Class is unconstrained. However, if S is constrained, then the values of S'Class are only those that when converted to the type T belong to S. See 3.9.

S'ClassFor every subtype S of an untagged private type whose full view is tagged:

Denotes the class-wide subtype corresponding to the full view of S. This attribute is allowed only from the beginning of the private part in which the full view is declared, until the declaration of the full view. After the full view, the Class attribute of the full view can be used. See 7.3.1.

X'Component_SizeFor a prefix X that denotes an array subtype or array object (after any implicit dereference):

Denotes the size in bits of components of the type of X. The value of this attribute is of type universal_integer. See 13.3.

S'ComposeFor every subtype S of a floating point type T:

S'Compose denotes a function with the following specification: 

```ada
function S'Compose (Fraction : T;
                    Exponent : universal_integer)
  return T

```

Let v be the value Fraction � T'Machine_RadixExponentk, where k is the normalized exponent of Fraction. If v is a machine number of the type T, or if |v|  T'Model_Small, the function yields v; otherwise, it yields either one of the machine numbers of the type T adjacent to v. Constraint_Error is optionally raised if v is outside the base range of S. A zero result has the sign of Fraction when S'Signed_Zeros is True. See A.5.3.

A'ConstrainedFor a [prefix](S0086) A that is of a discriminated type (after any implicit dereference):

Yields the value True if A denotes a constant, a value, or a constrained variable, and False otherwise. See 3.7.2.

S'Copy_SignFor every subtype S of a floating point type T:

S'Copy_Sign denotes a function with the following specification: 

```ada
function S'Copy_Sign (Value, Sign : T)
  return T

```

If the value of Value is nonzero, the function yields a result whose magnitude is that of Value and whose sign is that of Sign; otherwise, it yields the value zero. Constraint_Error is optionally raised if the result is outside the base range of S. A zero result has the sign of Sign when S'Signed_Zeros is True. See A.5.3.

E'CountFor a [prefix](S0086) E that denotes an entry of a task or protected unit:

Yields the number of calls presently queued on the entry E of the current instance of the unit. The value of this attribute is of the type universal_integer. See 9.9.

S'DefiniteFor a prefix S that denotes a formal indefinite subtype:

S'Definite yields True if the actual subtype corresponding to S is definite; otherwise it yields False. The value of this attribute is of the predefined type Boolean. See 12.5.1.

S'DeltaFor every fixed point subtype S:

S'Delta denotes the delta of the fixed point subtype S. The value of this attribute is of the type universal_real. See 3.5.10.

S'DenormFor every subtype S of a floating point type T:

Yields the value True if every value expressible in the form
    � mantissa � T'Machine_RadixT'Machine_Emin
where mantissa is a nonzero T'Machine_Mantissa-digit fraction in the number base T'Machine_Radix, the first digit of which is zero, is a machine number (see 3.5.7) of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. See A.5.3.

S'DigitsFor every floating point subtype S:

See 3.5.8.

S'DigitsFor every decimal fixed point subtype S:

S'Digits denotes the digits of the decimal fixed point subtype S, which corresponds to the number of decimal digits that are representable in objects of the subtype. The value of this attribute is of the type universal_integer. See 3.5.10.

S'ExponentFor every subtype S of a floating point type T:

S'Exponent denotes a function with the following specification: 

```ada
function S'Exponent (X : T)
  return universal_integer

```

The function yields the normalized exponent of X. See A.5.3.

S'External_TagFor every subtype S of a tagged type T (specific or class-wide):

S'External_Tag denotes an external string representation for S'Tag; it is of the predefined type String. External_Tag may be specified for a specific tagged type via an [attribute_definition_clause](S0265); the expression of such a clause shall be static. The default external tag representation is implementation defined. See 3.9.2 and 13.13.2. See 13.3.

A'FirstFor a prefix A that is of an array type (after any implicit dereference), or denotes a constrained array subtype:

A'First denotes the lower bound of the first index range; its type is the corresponding index type. See 3.6.2.

S'FirstFor every scalar subtype S:

S'First denotes the lower bound of the range of S. The value of this attribute is of the type of S. See 3.5.

A'First(N)For a prefix A that is of an array type (after any implicit dereference), or denotes a constrained array subtype:

A'First(N) denotes the lower bound of the N-th index range; its type is the corresponding index type. See 3.6.2.

R.C'First_BitFor a component C of a composite, non-array object R:

Denotes the offset, from the start of the first of the storage elements occupied by C, of the first bit occupied by C. This offset is measured in bits. The first bit of a storage element is numbered zero. The value of this attribute is of the type universal_integer. See 13.5.2.

S'FloorFor every subtype S of a floating point type T:

S'Floor denotes a function with the following specification: 

```ada
function S'Floor (X : T)
  return T

```

The function yields the value X, i.e., the largest (most positive) integral value less than or equal to X. When X is zero, the result has the sign of X; a zero result otherwise has a positive sign. See A.5.3.

S'ForeFor every fixed point subtype S:

S'Fore yields the minimum number of characters needed before the decimal point for the decimal representation of any value of the subtype S, assuming that the representation does not include an exponent, but includes a one-character prefix that is either a minus sign or a space. (This minimum number does not include superfluous zeros or underlines, and is at least 2.) The value of this attribute is of the type universal_integer. See 3.5.10.

S'FractionFor every subtype S of a floating point type T:

S'Fraction denotes a function with the following specification: 

```ada
function S'Fraction (X : T)
  return T

```

The function yields the value X � T'Machine_Radixk, where k is the normalized exponent of X. A zero result, which can only occur when X is zero, has the sign of X. See A.5.3.

E'IdentityFor a prefix E that denotes an exception:

E'Identity returns the unique identity of the exception. The type of this attribute is Exception_Id. See 11.4.1.

T'IdentityFor a [prefix](S0086) T that is of a task type (after any implicit dereference):

Yields a value of the type Task_Id that identifies the task denoted by T. See C.7.1.

S'ImageFor NONE!:

See 4.9.1.

S'Class'InputFor every subtype S'Class of a class-wide type T'Class:

S'Class'Input denotes a function with the following specification: 

```ada
function S'Class'Input(
   Stream : access Ada.Streams.Root_Stream_Type'Class)
   return T'Class

```

First reads the external tag from Stream and determines the corresponding internal tag (by calling Tags.Internal_Tag(String'Input(Stream)) - see 3.9) and then dispatches to the subprogram denoted by the Input attribute of the specific type identified by the internal tag; returns that result. See 13.13.2.

S'InputFor every subtype S of a specific type T:

S'Input denotes a function with the following specification: 

```ada
function S'Input(
   Stream : access Ada.Streams.Root_Stream_Type'Class)
   return T

```

S'Input reads and returns one value from Stream, using any bounds or discriminants written by a corresponding S'Output to determine how much to read. See 13.13.2.

A'LastFor a prefix A that is of an array type (after any implicit dereference), or denotes a constrained array subtype:

A'Last denotes the upper bound of the first index range; its type is the corresponding index type. See 3.6.2.

S'LastFor every scalar subtype S:

S'Last denotes the upper bound of the range of S. The value of this attribute is of the type of S. See 3.5.

A'Last(N)For a prefix A that is of an array type (after any implicit dereference), or denotes a constrained array subtype:

A'Last(N) denotes the upper bound of the N-th index range; its type is the corresponding index type. See 3.6.2.

R.C'Last_BitFor a component C of a composite, non-array object R:

Denotes the offset, from the start of the first of the storage elements occupied by C, of the last bit occupied by C. This offset is measured in bits. The value of this attribute is of the type universal_integer. See 13.5.2.

S'Leading_PartFor every subtype S of a floating point type T:

S'Leading_Part denotes a function with the following specification: 

```ada
function S'Leading_Part (X : T;
                         Radix_Digits : universal_integer)
  return T

```

Let v be the value T'Machine_RadixkRadix_Digits, where k is the normalized exponent of X. The function yields the value 

X/v � v, when X is nonnegative and Radix_Digits is positive;

X/v � v, when X is negative and Radix_Digits is positive. 

Constraint_Error is raised when Radix_Digits is zero or negative. A zero result, which can only occur when X is zero, has the sign of X. See A.5.3.

A'LengthFor a prefix A that is of an array type (after any implicit dereference), or denotes a constrained array subtype:

A'Length denotes the number of values of the first index range (zero for a null range); its type is universal_integer. See 3.6.2.

A'Length(N)For a prefix A that is of an array type (after any implicit dereference), or denotes a constrained array subtype:

A'Length(N) denotes the number of values of the N-th index range (zero for a null range); its type is universal_integer. See 3.6.2.

S'MachineFor every subtype S of a floating point type T:

S'Machine denotes a function with the following specification: 

```ada
function S'Machine (X : T)
  return T

```

If X is a machine number of the type T, the function yields X; otherwise, it yields the value obtained by rounding or truncating X to either one of the adjacent machine numbers of the type T. Constraint_Error is raised if rounding or truncating X to the precision of the machine numbers results in a value outside the base range of S. A zero result has the sign of X when S'Signed_Zeros is True. See A.5.3.

S'Machine_EmaxFor every subtype S of a floating point type T:

Yields the largest (most positive) value of exponent such that every value expressible in the canonical form (for the type T), having a mantissa of T'Machine_Mantissa digits, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer. See A.5.3.

S'Machine_EminFor every subtype S of a floating point type T:

Yields the smallest (most negative) value of exponent such that every value expressible in the canonical form (for the type T), having a mantissa of T'Machine_Mantissa digits, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer. See A.5.3.

S'Machine_MantissaFor every subtype S of a floating point type T:

Yields the largest value of p such that every value expressible in the canonical form (for the type T), having a p-digit mantissa and an exponent between T'Machine_Emin and T'Machine_Emax, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer. See A.5.3.

S'Machine_OverflowsFor every subtype S of a floating point type T:

Yields the value True if overflow and divide-by-zero are detected and reported by raising Constraint_Error for every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. See A.5.3.

S'Machine_OverflowsFor every subtype S of a fixed point type T:

Yields the value True if overflow and divide-by-zero are detected and reported by raising Constraint_Error for every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. See A.5.4.

S'Machine_RadixFor every subtype S of a floating point type T:

Yields the radix of the hardware representation of the type T. The value of this attribute is of the type universal_integer. See A.5.3.

S'Machine_RadixFor every subtype S of a fixed point type T:

Yields the radix of the hardware representation of the type T. The value of this attribute is of the type universal_integer. See A.5.4.

S'Machine_RoundsFor every subtype S of a floating point type T:

Yields the value True if rounding is performed on inexact results of every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. See A.5.3.

S'Machine_RoundsFor every subtype S of a fixed point type T:

Yields the value True if rounding is performed on inexact results of every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. See A.5.4.

S'MaxFor every scalar subtype S:

S'Max denotes a function with the following specification: 

```ada
function S'Max(Left, Right : S'Base)
  return S'Base

```

The function returns the greater of the values of the two parameters. See 3.5.

S'Max_Size_In_Storage_ElementsFor every subtype S:

Denotes the maximum value for Size_In_Storage_Elements that will be requested via Allocate for an access type whose designated subtype is S. The value of this attribute is of type universal_integer. See 13.11.1.

S'MinFor every scalar subtype S:

S'Min denotes a function with the following specification: 

```ada
function S'Min(Left, Right : S'Base)
  return S'Base

```

The function returns the lesser of the values of the two parameters. See 3.5.

S'ModelFor every subtype S of a floating point type T:

S'Model denotes a function with the following specification: 

```ada
function S'Model (X : T)
  return T

```

If the Numerics Annex is not supported, the meaning of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex. See A.5.3.

S'Model_EminFor every subtype S of a floating point type T:

If the Numerics Annex is not supported, this attribute yields an implementation defined value that is greater than or equal to the value of T'Machine_Emin. See G.2.2 for further requirements that apply to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_integer. See A.5.3.

S'Model_EpsilonFor every subtype S of a floating point type T:

Yields the value T'Machine_Radix1  T'Model_Mantissa. The value of this attribute is of the type universal_real. See A.5.3.

S'Model_MantissaFor every subtype S of a floating point type T:

If the Numerics Annex is not supported, this attribute yields an implementation defined value that is greater than or equal to d � log(10) / log(T'Machine_Radix) + 1, where d is the requested decimal precision of T, and less than or equal to the value of T'Machine_Mantissa. See G.2.2 for further requirements that apply to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_integer. See A.5.3.

S'Model_SmallFor every subtype S of a floating point type T:

Yields the value T'Machine_RadixT'Model_Emin  1. The value of this attribute is of the type universal_real. See A.5.3.

S'ModulusFor every modular subtype S:

S'Modulus yields the modulus of the type of S, as a value of the type universal_integer. See 3.5.4.

S'Class'OutputFor every subtype S'Class of a class-wide type T'Class:

S'Class'Output denotes a procedure with the following specification: 

```ada
procedure S'Class'Output(
   Stream : access Ada.Streams.Root_Stream_Type'Class;
   Item   : in T'Class)

```

First writes the external tag of Item to Stream (by calling String'Output(Tags.External_Tag(Item'Tag) - see 3.9) and then dispatches to the subprogram denoted by the Output attribute of the specific type identified by the tag. See 13.13.2.

S'OutputFor every subtype S of a specific type T:

S'Output denotes a procedure with the following specification: 

```ada
procedure S'Output(
   Stream : access Ada.Streams.Root_Stream_Type'Class;
   Item : in T)

```

S'Output writes the value of Item to Stream, including any bounds or discriminants. See 13.13.2.

D'Partition_IdFor a prefix D that denotes a library-level declaration, excepting a declaration of or within a declared-pure library unit:

Denotes a value of the type universal_integer that identifies the partition in which D was elaborated. If D denotes the declaration of a remote call interface library unit (see E.2.3) the given partition is the one where the body of D was elaborated. See E.1.

S'PosFor every discrete subtype S:

S'Pos denotes a function with the following specification: 

```ada
function S'Pos(Arg : S'Base)
  return universal_integer

```

This function returns the position number of the value of Arg, as a value of type universal_integer. See 3.5.5.

R.C'PositionFor a component C of a composite, non-array object R:

Denotes the same value as R.C'Address  R'Address. The value of this attribute is of the type universal_integer. See 13.5.2.

S'PredFor every scalar subtype S:

S'Pred denotes a function with the following specification: 

```ada
function S'Pred(Arg : S'Base)
  return S'Base

```

For an enumeration type, the function returns the value whose position number is one less than that of the value of Arg; Constraint_Error is raised if there is no such value of the type. For an integer type, the function returns the result of subtracting one from the value of Arg. For a fixed point type, the function returns the result of subtracting small from the value of Arg. For a floating point type, the function returns the machine number (as defined in 3.5.7) immediately below the value of Arg; Constraint_Error is raised if there is no such machine number. See 3.5.

A'RangeFor a prefix A that is of an array type (after any implicit dereference), or denotes a constrained array subtype:

A'Range is equivalent to the range A'First .. A'Last, except that the [prefix](S0086) A is only evaluated once. See 3.6.2.

S'RangeFor every scalar subtype S:

S'Range is equivalent to the [range](S0034) S'First .. S'Last. See 3.5.

A'Range(N)For a prefix A that is of an array type (after any implicit dereference), or denotes a constrained array subtype:

A'Range(N) is equivalent to the range A'First(N) .. A'Last(N), except that the [prefix](S0086) A is only evaluated once. See 3.6.2.

S'Class'ReadFor every subtype S'Class of a class-wide type T'Class:

S'Class'Read denotes a procedure with the following specification: 

```ada
procedure S'Class'Read(
   Stream : access Ada.Streams.Root_Stream_Type'Class;
   Item : out T'Class)

```

Dispatches to the subprogram denoted by the Read attribute of the specific type identified by the tag of Item. See 13.13.2.

S'ReadFor every subtype S of a specific type T:

S'Read denotes a procedure with the following specification: 

```ada
procedure S'Read(
   Stream : access Ada.Streams.Root_Stream_Type'Class;
   Item : out T)

```

S'Read reads the value of Item from Stream. See 13.13.2.

S'RemainderFor every subtype S of a floating point type T:

S'Remainder denotes a function with the following specification: 

```ada
function S'Remainder (X, Y : T)
  return T

```

For nonzero Y, let v be the value X  n � Y, where n is the integer nearest to the exact value of X/Y; if |n  X/Y| = 1/2, then n is chosen to be even. If v is a machine number of the type T, the function yields v; otherwise, it yields zero. Constraint_Error is raised if Y is zero. A zero result has the sign of X when S'Signed_Zeros is True. See A.5.3.

S'RoundFor every decimal fixed point subtype S:

S'Round denotes a function with the following specification: 

```ada
function S'Round(X : universal_real)
  return S'Base

```

The function returns the value obtained by rounding X (away from 0, if X is midway between two values of the type of S). See 3.5.10.

S'RoundingFor every subtype S of a floating point type T:

S'Rounding denotes a function with the following specification: 

```ada
function S'Rounding (X : T)
  return T

```

The function yields the integral value nearest to X, rounding away from zero if X lies exactly halfway between two integers. A zero result has the sign of X when S'Signed_Zeros is True. See A.5.3.

S'Safe_FirstFor every subtype S of a floating point type T:

Yields the lower bound of the safe range (see 3.5.7) of the type T. If the Numerics Annex is not supported, the value of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_real. See A.5.3.

S'Safe_LastFor every subtype S of a floating point type T:

Yields the upper bound of the safe range (see 3.5.7) of the type T. If the Numerics Annex is not supported, the value of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_real. See A.5.3.

S'ScaleFor every decimal fixed point subtype S:

S'Scale denotes the scale of the subtype S, defined as the value N such that S'Delta = 10.0**(N). The scale indicates the position of the point relative to the rightmost significant digits of values of subtype S. The value of this attribute is of the type universal_integer. See 3.5.10.

S'ScalingFor every subtype S of a floating point type T:

S'Scaling denotes a function with the following specification: 

```ada
function S'Scaling (X : T;
                    Adjustment : universal_integer)
  return T

```

Let v be the value X � T'Machine_RadixAdjustment. If v is a machine number of the type T, or if |v|  T'Model_Small, the function yields v; otherwise, it yields either one of the machine numbers of the type T adjacent to v. Constraint_Error is optionally raised if v is outside the base range of S. A zero result has the sign of X when S'Signed_Zeros is True. See A.5.3.

S'Signed_ZerosFor every subtype S of a floating point type T:

Yields the value True if the hardware representation for the type T has the capability of representing both positively and negatively signed zeros, these being generated and used by the predefined operations of the type T as specified in IEC 559:1989; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. See A.5.3.

S'SizeFor every subtype S:

If S is definite, denotes the size (in bits) that the implementation would choose for the following objects of subtype S: 

A record component of subtype S when the record type is packed.

The formal parameter of an instance of Unchecked_Conversion that converts from subtype S to some other subtype. 

If S is indefinite, the meaning is implementation defined. The value of this attribute is of the type universal_integer. See 13.3.

X'SizeFor a prefix X that denotes an object:

Denotes the size in bits of the representation of the object. The value of this attribute is of the type universal_integer. See 13.3.

S'SmallFor every fixed point subtype S:

See 3.5.10.

S'Storage_PoolFor every access subtype S:

Denotes the storage pool of the type of S. The type of this attribute is Root_Storage_Pool'Class. See 13.11.

S'Storage_SizeFor every access subtype S:

Yields the result of calling Storage_Size(S'Storage_Pool), which is intended to be a measure of the number of storage elements reserved for the pool. The type of this attribute is universal_integer. See 13.11.

T'Storage_SizeFor a prefix T that denotes a task object (after any implicit dereference):

See 13.3.

S'SuccFor every scalar subtype S:

S'Succ denotes a function with the following specification: 

```ada
function S'Succ(Arg : S'Base)
  return S'Base

```

For an enumeration type, the function returns the value whose position number is one more than that of the value of Arg; Constraint_Error is raised if there is no such value of the type. For an integer type, the function returns the result of adding one to the value of Arg. For a fixed point type, the function returns the result of adding small to the value of Arg. For a floating point type, the function returns the machine number (as defined in 3.5.7) immediately above the value of Arg; Constraint_Error is raised if there is no such machine number. See 3.5.

S'TagFor every subtype S of a tagged type T (specific or class-wide):

S'Tag denotes the tag of the type T (or if T is class-wide, the tag of the root type of the corresponding class). The value of this attribute is of type Tag. See 3.9.

X'TagFor a [prefix](S0086) X that is of a class-wide tagged type (after any implicit dereference):

X'Tag denotes the tag of X. The value of this attribute is of type Tag. See 3.9.

T'TerminatedFor a [prefix](S0086) T that is of a task type (after any implicit dereference):

Yields the value True if the task denoted by T is terminated, and False otherwise. The value of this attribute is of the predefined type Boolean. See 9.9.

S'TruncationFor every subtype S of a floating point type T:

S'Truncation denotes a function with the following specification: 

```ada
function S'Truncation (X : T)
  return T

```

The function yields the value X when X is negative, and X otherwise. A zero result has the sign of X when S'Signed_Zeros is True. See A.5.3.

S'Unbiased_RoundingFor every subtype S of a floating point type T:

S'Unbiased_Rounding denotes a function with the following specification: 

```ada
function S'Unbiased_Rounding (X : T)
  return T

```

The function yields the integral value nearest to X, rounding toward the even integer if X lies exactly halfway between two integers. A zero result has the sign of X when S'Signed_Zeros is True. See A.5.3.

X'Unchecked_AccessFor a [prefix](S0086) X that denotes an aliased view of an object:

All rules and semantics that apply to X'Access (see 3.10.2) apply also to X'Unchecked_Access, except that, for the purposes of accessibility rules and checks, it is as if X were declared immediately within a library package. See 13.10.

S'ValFor every discrete subtype S:

S'Val denotes a function with the following specification: 

```ada
function S'Val(Arg : universal_integer)
  return S'Base

```

This function returns a value of the type of S whose position number equals the value of Arg. See 3.5.5.

X'ValidFor a [prefix](S0086) X that denotes a scalar object (after any implicit dereference):

Yields True if and only if the object denoted by X is normal and has a valid representation. The value of this attribute is of the predefined type Boolean. See 13.9.2.

S'ValueFor every scalar subtype S:

S'Value denotes a function with the following specification: 

```ada
function S'Value(Arg : String)
  return S'Base

```

This function returns a value given an image of the value as a String, ignoring any leading or trailing spaces. See 3.5.

P'VersionFor a prefix P that statically denotes a program unit:

Yields a value of the predefined type String that identifies the version of the compilation unit that contains the declaration of the program unit. See E.3.

S'Wide_ImageFor NONE!:

See 4.9.1.

S'Wide_ValueFor every scalar subtype S:

S'Wide_Value denotes a function with the following specification: 

```ada
function S'Wide_Value(Arg : Wide_String)
  return S'Base

```

This function returns a value given an image of the value as a Wide_String, ignoring any leading or trailing spaces. See 3.5.

S'Wide_WidthFor every scalar subtype S:

S'Wide_Width denotes the maximum length of a Wide_String returned by S'Wide_Image over all values of the subtype S. It denotes zero for a subtype that has a null range. Its type is universal_integer. See 3.5.

S'WidthFor every scalar subtype S:

S'Width denotes the maximum length of a String returned by S'Image over all values of the subtype S. It denotes zero for a subtype that has a null range. Its type is universal_integer. See 3.5.

S'Class'WriteFor every subtype S'Class of a class-wide type T'Class:

S'Class'Write denotes a procedure with the following specification: 

```ada
procedure S'Class'Write(
   Stream : access Ada.Streams.Root_Stream_Type'Class;
   Item   : in T'Class)

```

Dispatches to the subprogram denoted by the Write attribute of the specific type identified by the tag of Item. See 13.13.2.

S'WriteFor every subtype S of a specific type T:

S'Write denotes a procedure with the following specification: 

```ada
procedure S'Write(
   Stream : access Ada.Streams.Root_Stream_Type'Class;
   Item : in T)

```

S'Write writes the value of Item to Stream. See 13.13.2.

