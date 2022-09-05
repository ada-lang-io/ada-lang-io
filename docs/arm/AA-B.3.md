---
sidebar_position:  140
---

# B.3  Interfacing with C and C++

{8652/0059} {AI95-00131-01} {AI95-00376-01} {AI05-0229-1} {AI12-0028-1} The facilities relevant to interfacing with the C language and the corresponding subset of the C++ language are the package Interfaces.C and its children, and support for specifying the Convention aspect with convention_[identifier](./AA-2.3#S0002)s C, C_Pass_By_Copy, and any of the C_Variadic_n conventions described below.

{AI95-00376-01} {AI95-0262-1} {AI95-0299-1} The package Interfaces.C contains the basic types, constants, and subprograms that allow an Ada program to pass scalars and strings to C and C++ functions. When this subclause mentions a C entity, the reference also applies to the corresponding entity in C++. 


#### Static Semantics

The library package Interfaces.C has the following declaration: 

```ada
{AI12-0414-1} package Interfaces.C
   with Pure is

```

```ada
   -- Declarations based on C's &ltlimits.h&gt

```

```ada
   CHAR_BIT  : constant := implementation-defined;  -- typically 8
   SCHAR_MIN : constant := implementation-defined;  -- typically 128
   SCHAR_MAX : constant := implementation-defined;  -- typically 127
   UCHAR_MAX : constant := implementation-defined;  -- typically 255

```

```ada
   -- Signed and Unsigned Integers
   type int   is range implementation-defined;
   type short is range implementation-defined;
   type long  is range implementation-defined;

```

```ada
   type signed_char is range SCHAR_MIN .. SCHAR_MAX;
   for signed_char'Size use CHAR_BIT;

```

```ada
   type unsigned       is mod implementation-defined;
   type unsigned_short is mod implementation-defined;
   type unsigned_long  is mod implementation-defined;

```

```ada
   type unsigned_char is mod (UCHAR_MAX+1);
   for unsigned_char'Size use CHAR_BIT;

```

```ada
   subtype plain_char is implementation-defined;

```

```ada
   type ptrdiff_t is range implementation-defined;

```

```ada
   type size_t is mod implementation-defined;

```

```ada
{AI12-0411-1}    -- Boolean Type
   type C_bool is new Boolean;

```

```ada
   -- Floating Point

```

```ada
   type C_float     is digits implementation-defined;

```

```ada
   type double      is digits implementation-defined;

```

```ada
   type long_double is digits implementation-defined;

```

```ada
   -- Characters and Strings 

```

```ada
   type char is &ltimplementation-defined character type&gt;

```

```ada
{8652/0060} {AI95-00037-01}    nul : constant char := implementation-defined;

```

```ada
   function To_C   (Item : in Character) return char;

```

```ada
   function To_Ada (Item : in char) return Character;

```

```ada
{AI05-0229-1} {AI05-0269-1}    type char_array is array (size_t range &lt&gt) of aliased char
      with Pack;
   for char_array'Component_Size use CHAR_BIT;

```

```ada
   function Is_Nul_Terminated (Item : in char_array) return Boolean;

```

```ada
   function To_C   (Item       : in String;
                    Append_Nul : in Boolean := True)
      return char_array;

```

```ada
   function To_Ada (Item     : in char_array;
                    Trim_Nul : in Boolean := True)
      return String;

```

```ada
   procedure To_C (Item       : in  String;
                   Target     : out char_array;
                   Count      : out size_t;
                   Append_Nul : in  Boolean := True);

```

```ada
   procedure To_Ada (Item     : in  char_array;
                     Target   : out String;
                     Count    : out Natural;
                     Trim_Nul : in  Boolean := True);

```

```ada
   -- Wide Character and Wide String

```

```ada
{8652/0060} {AI95-00037-01}    type wchar_t is &ltimplementation-defined character type&gt;

```

```ada
{8652/0060} {AI95-00037-01}    wide_nul : constant wchar_t := implementation-defined;

```

```ada
   function To_C   (Item : in Wide_Character) return wchar_t;
   function To_Ada (Item : in wchar_t       ) return Wide_Character;

```

```ada
{AI05-0229-1}    type wchar_array is array (size_t range &lt&gt) of aliased wchar_t
      with Pack;

```

```ada
This paragraph was deleted.{AI05-0229-1} 

```

```ada
   function Is_Nul_Terminated (Item : in wchar_array) return Boolean;

```

```ada
   function To_C   (Item       : in Wide_String;
                    Append_Nul : in Boolean := True)
      return wchar_array;

```

```ada
   function To_Ada (Item     : in wchar_array;
                    Trim_Nul : in Boolean := True)
      return Wide_String;

```

```ada
   procedure To_C (Item       : in  Wide_String;
                   Target     : out wchar_array;
                   Count      : out size_t;
                   Append_Nul : in  Boolean := True);

```

```ada
   procedure To_Ada (Item     : in  wchar_array;
                     Target   : out Wide_String;
                     Count    : out Natural;
                     Trim_Nul : in  Boolean := True);

```

```ada
{AI95-00285-01}    -- ISO/IEC 10646:2003 compatible types defined by ISO/IEC TR 19769:2004.

```

```ada
{AI95-00285-01}    type char16_t is &ltimplementation-defined character type&gt;

```

```ada
   char16_nul : constant char16_t := implementation-defined;

```

```ada
   function To_C (Item : in Wide_Character) return char16_t;
   function To_Ada (Item : in char16_t) return Wide_Character;

```

```ada
{AI05-0229-1}    type char16_array is array (size_t range &lt&gt) of aliased char16_t
      with Pack;

```

```ada
This paragraph was deleted.{AI05-0229-1} 

```

```ada
   function Is_Nul_Terminated (Item : in char16_array) return Boolean;
   function To_C (Item       : in Wide_String;
                  Append_Nul : in Boolean := True)
      return char16_array;

```

```ada
   function To_Ada (Item     : in char16_array;
                    Trim_Nul : in Boolean := True)
      return Wide_String;

```

```ada
   procedure To_C (Item       : in  Wide_String;
                   Target     : out char16_array;
                   Count      : out size_t;
                   Append_Nul : in  Boolean := True);

```

```ada
   procedure To_Ada (Item     : in  char16_array;
                     Target   : out Wide_String;
                     Count    : out Natural;
                     Trim_Nul : in  Boolean := True);

```

```ada
{AI95-00285-01}    type char32_t is &ltimplementation-defined character type&gt;

```

```ada
   char32_nul : constant char32_t := implementation-defined;

```

```ada
   function To_C (Item : in Wide_Wide_Character) return char32_t;
   function To_Ada (Item : in char32_t) return Wide_Wide_Character;

```

```ada
{AI05-0229-1}    type char32_array is array (size_t range &lt&gt) of aliased char32_t
      with Pack;

```

```ada
This paragraph was deleted.{AI05-0229-1} 

```

```ada
   function Is_Nul_Terminated (Item : in char32_array) return Boolean;
   function To_C (Item       : in Wide_Wide_String;
                  Append_Nul : in Boolean := True)
      return char32_array;

```

```ada
   function To_Ada (Item     : in char32_array;
                    Trim_Nul : in Boolean := True)
      return Wide_Wide_String;

```

```ada
   procedure To_C (Item       : in  Wide_Wide_String;
                   Target     : out char32_array;
                   Count      : out size_t;
                   Append_Nul : in  Boolean := True);

```

```ada
   procedure To_Ada (Item     : in  char32_array;
                     Target   : out Wide_Wide_String;
                     Count    : out Natural;
                     Trim_Nul : in  Boolean := True);

```

```ada
   Terminator_Error : exception;

```

```ada
end Interfaces.C;

```

Implementation defined: The definitions of certain types and constants in Interfaces.C.

Each of the types declared in Interfaces.C is C-compatible.

{AI95-00285-01} {AI12-0411-1} The types int, short, long, unsigned, ptrdiff_t, size_t, double, char, wchar_t, char16_t, and char32_t correspond respectively to the C types having the same names. The types signed_char, unsigned_short, unsigned_long, unsigned_char, C_bool, C_float, and long_double correspond respectively to the C types signed char, unsigned short, unsigned long, unsigned char, bool,  float, and long double.

Discussion: The C types wchar_t and char16_t seem to be the same. However, wchar_t has an implementation-defined size, whereas char16_t is guaranteed to be an unsigned type of at least 16 bits. Also, char16_t and char32_t are encouraged to have UTF-16 and UTF-32 representations; that means that they are not directly the same as the Ada types, which most likely don't use any UTF encoding. 

The type of the subtype plain_char is either signed_char or unsigned_char, depending on the C implementation. 

```ada
function To_C   (Item : in Character) return char;
function To_Ada (Item : in char     ) return Character;

```

The functions To_C and To_Ada map between the Ada type Character and the C type char.

Implementation Note: {8652/0114} {AI95-00038-01} The To_C and To_Ada functions map between corresponding characters, not necessarily between characters with the same internal representation. Corresponding characters are characters defined by the same enumeration literal, if such exist; otherwise, the correspondence is unspecified.

The following definition is equivalent to the above summary:

To_C (Latin_1_Char) = char'Value(Character'Image(Latin_1_Char))
provided that char'Value does not raise an exception; otherwise the result is unspecified.

To_Ada (Native_C_Char) = Character'Value(char'Image(Native_C_Char))
provided that Character'Value does not raise an exception; otherwise the result is unspecified. 

```ada
function Is_Nul_Terminated (Item : in char_array) return Boolean;

```

The result of Is_Nul_Terminated is True if Item contains nul, and is False otherwise.

```ada
function To_C   (Item : in String;     Append_Nul : in Boolean := True)
   return char_array;

function To_Ada (Item : in char_array; Trim_Nul   : in Boolean := True)
   return String;

```

{AI95-00258-01} The result of To_C is a char_array value of length Item'Length (if Append_Nul is False) or Item'Length+1 (if Append_Nul is True). The lower bound is 0. For each component Item(I), the corresponding component in the result is To_C applied to Item(I). The value nul is appended if Append_Nul is True. If Append_Nul is False and Item'Length is 0, then To_C propagates Constraint_Error.

The result of To_Ada is a String whose length is Item'Length (if Trim_Nul is False) or the length of the slice of Item preceding the first nul (if Trim_Nul is True). The lower bound of the result is 1. If Trim_Nul is False, then for each component Item(I) the corresponding component in the result is To_Ada applied to Item(I). If Trim_Nul is True, then for each component Item(I) before the first nul the corresponding component in the result is To_Ada applied to Item(I). The function propagates Terminator_Error if Trim_Nul is True and Item does not contain nul.

```ada
procedure To_C (Item       : in  String;
                Target     : out char_array;
                Count      : out size_t;
                Append_Nul : in  Boolean := True);

procedure To_Ada (Item     : in  char_array;
                  Target   : out String;
                  Count    : out Natural;
                  Trim_Nul : in  Boolean := True);

```

For procedure To_C, each element of Item is converted (via the To_C function) to a char, which is assigned to the corresponding element of Target. If Append_Nul is True, nul is then assigned to the next element of Target. In either case, Count is set to the number of Target elements assigned. If Target is not long enough, Constraint_Error is propagated.

For procedure To_Ada, each element of Item (if Trim_Nul is False) or each element of Item preceding the first nul (if Trim_Nul is True) is converted (via the To_Ada function) to a Character, which is assigned to the corresponding element of Target. Count is set to the number of Target elements assigned. If Target is not long enough, Constraint_Error is propagated. If Trim_Nul is True and Item does not contain nul, then Terminator_Error is propagated.

```ada
function Is_Nul_Terminated (Item : in wchar_array) return Boolean;

```

The result of Is_Nul_Terminated is True if Item contains wide_nul, and is False otherwise.

```ada
function To_C   (Item : in Wide_Character) return wchar_t;
function To_Ada (Item : in wchar_t       ) return Wide_Character;

```

To_C and To_Ada provide the mappings between the Ada and C wide character types.

```ada
function To_C   (Item       : in Wide_String;
                 Append_Nul : in Boolean := True)
   return wchar_array;

function To_Ada (Item     : in wchar_array;
                 Trim_Nul : in Boolean := True)
   return Wide_String;

procedure To_C (Item       : in  Wide_String;
                Target     : out wchar_array;
                Count      : out size_t;
                Append_Nul : in  Boolean := True);

procedure To_Ada (Item     : in  wchar_array;
                  Target   : out Wide_String;
                  Count    : out Natural;
                  Trim_Nul : in  Boolean := True);

```

The To_C and To_Ada subprograms that convert between Wide_String and wchar_array have analogous effects to the To_C and To_Ada subprograms that convert between String and char_array, except that wide_nul is used instead of nul.

```ada
function Is_Nul_Terminated (Item : in char16_array) return Boolean;

```

{AI95-00285-01} The result of Is_Nul_Terminated is True if Item contains char16_nul, and is False otherwise.

```ada
function To_C (Item : in Wide_Character) return char16_t;
function To_Ada (Item : in char16_t ) return Wide_Character;

```

{AI95-00285-01} To_C and To_Ada provide mappings between the Ada and C 16-bit character types.

```ada
function To_C (Item       : in Wide_String;
               Append_Nul : in Boolean := True)
   return char16_array;

function To_Ada (Item     : in char16_array;
                 Trim_Nul : in Boolean := True)
   return Wide_String;

procedure To_C (Item       : in  Wide_String;
                Target     : out char16_array;
                Count      : out size_t;
                Append_Nul : in  Boolean := True);

procedure To_Ada (Item     : in  char16_array;
                  Target   : out Wide_String;
                  Count    : out Natural;
                  Trim_Nul : in  Boolean := True);

```

{AI95-00285-01} The To_C and To_Ada subprograms that convert between Wide_String and char16_array have analogous effects to the To_C and To_Ada subprograms that convert between String and char_array, except that char16_nul is used instead of nul.

```ada
function Is_Nul_Terminated (Item : in char32_array) return Boolean;

```

{AI95-00285-01} {AI12-0437-1} The result of Is_Nul_Terminated is True if Item contains char32_nul, and is False otherwise.

```ada
function To_C (Item : in Wide_Wide_Character) return char32_t;
function To_Ada (Item : in char32_t ) return Wide_Wide_Character;

```

{AI95-00285-01} To_C and To_Ada provide mappings between the Ada and C 32-bit character types.

```ada
function To_C (Item       : in Wide_Wide_String;
               Append_Nul : in Boolean := True)
   return char32_array;

function To_Ada (Item     : in char32_array;
                 Trim_Nul : in Boolean := True)
   return Wide_Wide_String;

procedure To_C (Item       : in  Wide_Wide_String;
                Target     : out char32_array;
                Count      : out size_t;
                Append_Nul : in  Boolean := True);

procedure To_Ada (Item     : in  char32_array;
                  Target   : out Wide_Wide_String;
                  Count    : out Natural;
                  Trim_Nul : in  Boolean := True);

```

{AI95-00285-01} The To_C and To_Ada subprograms that convert between Wide_Wide_String and char32_array have analogous effects to the To_C and To_Ada subprograms that convert between String and char_array, except that char32_nul is used instead of nul.

Discussion: The Interfaces.C package provides an implementation-defined character type, char, designed to model the C run-time character set, and mappings between the types char and Character.

One application of the C interface package is to compose a C string and pass it to a C function. One way to do this is for the programmer to declare an object that will hold the C array, and then pass this array to the C function. This is realized via the type char_array: 

```ada
type char_array is array (size_t range &lt&gt) of Char;

```

The programmer can declare an Ada String, convert it to a char_array, and pass the char_array as actual parameter to the C function that is expecting a char *.

An alternative approach is for the programmer to obtain a C char pointer from an Ada String (or from a char_array) by invoking an allocation function. The package Interfaces.C.Strings (see below) supplies the needed facilities, including a private type chars_ptr that corresponds to C's char *, and two allocation functions. To avoid storage leakage, a Free procedure releases the storage that was allocated by one of these allocate functions.

It is typical for a C function that deals with strings to adopt the convention that the string is delimited by a nul char. The C interface packages support this convention. A constant nul of type Char is declared, and the function Value(Chars_Ptr) in Interfaces.C.Strings returns a char_array up to and including the first nul in the array that the chars_ptr points to. The Allocate_Chars function allocates an array that is nul terminated.

Some C functions that deal with strings take an explicit length as a parameter, thus allowing strings to be passed that contain nul as a data element. Other C functions take an explicit length that is an upper bound: the prefix of the string up to the char before nul, or the prefix of the given length, is used by the function, whichever is shorter. The C Interface packages support calling such functions. 

{8652/0059} {AI95-00131-01} {AI05-0229-1} The Convention aspect with convention_[identifier](./AA-2.3#S0002) C_Pass_By_Copy shall only be specified for a type.

{8652/0059} {AI95-00131-01} {AI95-00216-01} The eligibility rules in B.1 do not apply to convention C_Pass_By_Copy. Instead, a type T is eligible for convention C_Pass_By_Copy if T is an unchecked union type or if T is a record type that has no discriminants and that only has components with statically constrained subtypes, and each component is C-compatible.

{8652/0059} {AI95-00131-01} {AI05-0264-1} If a type is C_Pass_By_Copy-compatible, then it is also C-compatible.

{AI12-0028-1} The identifiers C_Variadic_0, C_Variadic_1, C_Variadic_2, and so on are convention_[identifier](./AA-2.3#S0002)s. These conventions are said to be C_Variadic. The convention C_Variadic_n is the calling convention for a variadic C function taking n fixed parameters and then a variable number of additional parameters. The C_Variadic_n convention shall only be specified as the convention aspect for a subprogram, or for an access-to-subprogram type, having at least n parameters. A type is compatible with a C_Variadic convention if and only if the type is C-compatible.

To be honest: It is implementation defined what the largest n in C_Variadic_n is supported. We don't say this because it complicates the wording and it is true for almost any convention_[identifier](./AA-2.3#S0002) (only Ada is required to be supported by the language, all others need to be documented in order for programmers to know that they are available). 


#### Implementation Requirements

{8652/0059} {AI95-00131-01} {AI05-0229-1} An implementation shall support specifying aspect Convention with a C convention_[identifier](./AA-2.3#S0002) for a C-eligible type (see B.1). An implementation shall support specifying aspect Convention with a C_Pass_By_Copy convention_[identifier](./AA-2.3#S0002) for a C_Pass_By_Copy-eligible type. 


#### Implementation Permissions

An implementation may provide additional declarations in the C interface packages.

{AI05-0002-1} {AI05-0229-1} {AI12-0444-1} An implementation is not required to support specifying the Convention aspect with convention_[identifier](./AA-2.3#S0002) C in the following cases:

{AI05-0248-1} for a subprogram that has a parameter of an unconstrained array subtype, unless the Import aspect has the value True for the subprogram;

for a function with an unconstrained array result subtype;

for an object whose nominal subtype is an unconstrained array subtype. 

Implementation Note: {AI05-0002-1} These rules ensure that an implementation never needs to create bounds for an unconstrained array that originates in C (and thus does not have bounds). An implementation can do so if it wishes, of course. Note that these permissions do not extend to passing an unconstrained array as a parameter to a C function; in this case, the bounds can simply be dropped and thus support is required. 


#### Implementation Advice

{8652/0060} {AI95-00037-01} {AI95-00285-01} The constants nul, wide_nul, char16_nul, and char32_nul should have a representation of zero. 

Implementation Advice: The constants nul, wide_nul, char16_nul, and char32_nul in package Interfaces.C should have a representation of zero.

An implementation should support the following interface correspondences between Ada and C. 

An Ada procedure corresponds to a void-returning C function. 

Discussion: The programmer can also choose an Ada procedure when the C function returns an int that is to be discarded.

An Ada function corresponds to a non-void C function.

{AI12-0135-1} An Ada enumeration type corresponds to a C enumeration type with corresponding enumeration literals having the same internal codes, provided the internal codes fall within the range of the C int type.

An Ada in scalar parameter is passed as a scalar argument to a C function.

An Ada in parameter of an access-to-object type with designated type T is passed as a t* argument to a C function, where t is the C type corresponding to the Ada type T.

An Ada access T parameter, or an Ada out or in out parameter of an elementary type T, is passed as a t* argument to a C function, where t is the C type corresponding to the Ada type T. In the case of an elementary out or in out parameter, a pointer to a temporary copy is used to preserve by-copy semantics.

{8652/0059} {AI95-00131-01} {AI95-00343-01} An Ada parameter of a (record) type T of convention C_Pass_By_Copy, of mode in, is passed as a t argument to a C function, where t is the C struct corresponding to the Ada type T.

{8652/0059} {AI95-00131-01} {AI95-00343-01} {AI12-0219-1} An Ada parameter of a record type T, other than an in parameter of a type of convention C_Pass_By_Copy, is passed as a t* argument to a C function, with the const modifier if the Ada mode is in, where t is the C struct corresponding to the Ada type T.

{AI12-0219-1} An Ada parameter of an array type with component type T is passed as a t* argument to a C function, with the const modifier if the Ada mode is in, where t is the C type corresponding to the Ada type T.

An Ada parameter of an access-to-subprogram type is passed as a pointer to a C function whose prototype corresponds to the designated subprogram's specification.

{AI05-0002-1} An Ada parameter of a private type is passed as specified for the full view of the type.

{AI05-0002-1} The rules of correspondence given above for parameters of mode in also apply to the return object of a function.

{AI95-00337-01} {AI05-0002-1} {AI12-0184-1} An implementation should provide unsigned_long_long and long_long as 64-bit modular and signed integer types (respectively) in package Interfaces.C if the C implementation supports unsigned long long and long long as 64-bit types.

Implementation Advice: If C interfacing is supported, the interface correspondences between Ada and C should be supported.

Implementation Advice: If the C implementation supports unsigned long long and long long, unsigned_long_long and long_long should be supported.

NOTE 1   Values of type char_array are not implicitly terminated with nul. If a char_array is to be passed as a parameter to an imported C function requiring nul termination, it is the programmer's responsibility to obtain this effect.

NOTE 2   To obtain the effect of C's sizeof(item_type), where Item_Type is the corresponding Ada type, evaluate the expression: size_t(Item_Type'Size/CHAR_BIT).

This paragraph was deleted.{AI95-00216-01} 

NOTE 3   {AI12-0028-1} A variadic C function can correspond to several Ada subprograms, taking various specific numbers and types of parameters. 


#### Examples

{AI12-0312-1} Example of using the Interfaces.C package: 

```ada
--Calling the C Library Functions strcpy and printf
with Interfaces.C;
procedure Test is
   package C renames Interfaces.C;
   use type C.char_array;
   -- Call &ltstring.h&gtstrcpy:
   -- C definition of strcpy:  char *strcpy(char *s1, const char *s2);
   --    This function copies the string pointed to by s2 (including the terminating null character)
   --     into the array pointed to by s1. If copying takes place between objects that overlap,
   --     the behavior is undefined. The strcpy function returns the value of s1.

```

```ada
{AI05-0229-1}    -- Note: since the C function's return value is of no interest, the Ada interface is a procedure
   procedure Strcpy (Target : out C.char_array;
                     Source : in  C.char_array)
      with Import =&gt True, Convention =&gt C, External_Name =&gt "strcpy";

```

```ada
{AI05-0229-1} {AI12-0312-1}    -- Call &ltsdtio.h&gtprintf:
   -- C definition of printf:  int printf ( const char * format, ... );
   --    This function writes the C string pointed by format to the standard output (stdout).
   --     If format includes format specifiers (subsequences beginning with %), the additional
   --     arguments following format are formatted and inserted in the resulting string
   --     replacing their respective specifiers. If the number of arguments does not match
   --     the number of format specifiers, or if the types of the arguments do not match
   --     the corresponding format specifier, the behaviour is undefined. On success, the
   --     printf function returns the total number of characters written to the standard output.
   --     If a writing error occurs, a negative number is returned.

```

```ada
{AI12-0312-1}    -- Note: since the C function's return value is of no interest, the Ada interface is a procedure
   procedure Printf (Format : in C.char_array;
                     Param1 : in C.char_array;
                     Param2 : in C.int)
      with Import =&gt True, Convention =&gt C_Variadic_1, External_Name =&gt "printf";

```

```ada
   Chars1 :  C.char_array(1..20);
   Chars2 :  C.char_array(1..20);

```

```ada
begin
   Chars2(1..6) := "qwert" & C.nul;

```

```ada
   Strcpy(Chars1, Chars2);

```

```ada
   -- Now Chars1(1..6) = "qwert" & C.Nul

```

```ada
{AI12-0312-1}    Printf("The String=%s, Length=%d", Chars1, Chars1'Length);

```

```ada
end Test;

```


#### Incompatibilities With Ada 95

{AI95-00285-01} {AI05-0005-1} Types char16_t and char32_t and their related types and operations are added to Interfaces.C. If Interfaces.C is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity in Interfaces.C is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 95

{8652/0059} {AI95-00131-01} Corrigendum: Convention C_Pass_By_Copy is new. 


#### Wording Changes from Ada 95

{8652/0060} {AI95-00037-01} Corrigendum: Clarified the intent for Nul and Wide_Nul.

{AI95-00216-01} Specified that an unchecked union type (see B.3.3) is eligible for convention C_Pass_By_Copy.

{AI95-00258-01} Specified what happens if the To_C function tries to return a null string.

{AI95-00337-01} Clarified that the interface correspondences also apply to private types whose full types have the specified characteristics.

{AI95-00343-01} Clarified that a type must have convention C_Pass_By_Copy in order to be passed by copy (not just a type that could have that convention).

{AI95-00376-01} Added wording to make it clear that these facilities can also be used with C++. 


#### Incompatibilities With Ada 2005

{AI05-0002-1} Correction: Added a definition of correspondences for function results. Also added wording to make it clear that we do not expect the implementation to conjure bounds for unconstrained arrays out of thin air. These changes allow (but don't require) compilers to reject unreasonable uses of array types. Such uses probably didn't work anyway (and probably were rejected, no matter what the language definition said), so little existing code should be impacted. 


#### Incompatibilities With Ada 2012

{AI12-0411-1} Added type C_bool and (implicitly) the enumeration literals True and False to the Interfaces.C package. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0028-1} Corrigendum: The convention_[identifier](./AA-2.3#S0002)s C_Variadic_0, C_Variadic_1, and so on are new. These are classified as a correction as any implementation can add such identifiers and it is important that special conventions be available for variadic functions as typical x64 conventions are different for normal and variadic C functions.

{AI12-0135-1} Corrigendum: Defined the correspondence between an Ada enumeration type and a C enumeration type; implementations should support convention C for enumeration types. 


#### Wording Changes from Ada 2012

{AI12-0184-1} Added Implementation Advice that types be defined in Interfaces.C corresponding to long long and unsigned long long.

{AI12-0219-1} Correction: Added advice that const t* map to Ada in parameters and vice versa.

{AI12-0411-1} Added advice about mapping type Boolean. 


## B.3.1  The Package Interfaces.C.Strings

{AI05-0229-1} The package Interfaces.C.Strings declares types and subprograms allowing an Ada program to allocate, reference, update, and free C-style strings. In particular, the private type chars_ptr corresponds to a common use of "char *" in C programs, and an object of this type can be passed to a subprogram to which with Import =&gt True, Convention =&gt C has been specified, and for which "char *" is the type of the argument of the C function. 


#### Static Semantics

The library package Interfaces.C.Strings has the following declaration: 

```ada
{AI12-0241-1} {AI12-0302-1} package Interfaces.C.Strings 
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
   type char_array_access is access all char_array;

```

```ada
{AI95-00161-01} {AI12-0399-1}    type chars_ptr is private
      with Preelaborable_Initialization;

```

```ada
{AI95-00276-01}    type chars_ptr_array is array (size_t range &lt&gt) of aliased chars_ptr;

```

```ada
   Null_Ptr : constant chars_ptr;

```

```ada
   function To_Chars_Ptr (Item      : in char_array_access;
                          Nul_Check : in Boolean := False)
      return chars_ptr;

```

```ada
   function New_Char_Array (Chars   : in char_array) return chars_ptr;

```

```ada
   function New_String (Str : in String) return chars_ptr;

```

```ada
   procedure Free (Item : in out chars_ptr);

```

```ada
   Dereference_Error : exception;

```

```ada
   function Value (Item : in chars_ptr) return char_array;

```

```ada
   function Value (Item : in chars_ptr; Length : in size_t)
      return char_array;

```

```ada
   function Value (Item : in chars_ptr) return String;

```

```ada
   function Value (Item : in chars_ptr; Length : in size_t)
      return String;

```

```ada
   function Strlen (Item : in chars_ptr) return size_t;

```

```ada
   procedure Update (Item   : in chars_ptr;
                     Offset : in size_t;
                     Chars  : in char_array;
                     Check  : in Boolean := True);

```

```ada
   procedure Update (Item   : in chars_ptr;
                     Offset : in size_t;
                     Str    : in String;
                     Check  : in Boolean := True);

```

```ada
   Update_Error : exception;

```

```ada
private
   ... -- not specified by the language
end Interfaces.C.Strings;

```

Discussion: The string manipulation types and subprograms appear in a child of Interfaces.C versus being there directly, since it is useful to have Interfaces.C specified as [pragma](./AA-2.8#S0019) Pure.

Differently named functions New_String and New_Char_Array are declared, since if there were a single overloaded function a call with a string literal as actual parameter would be ambiguous. 

The type chars_ptr is C-compatible and corresponds to the use of C's "char *" for a pointer to the first char in a char array terminated by nul. When an object of type chars_ptr is declared, its value is by default set to Null_Ptr, unless the object is imported (see B.1). 

Discussion: The type char_array_access is not necessarily C-compatible, since an object of this type may carry "dope" information. The programmer should convert from char_array_access to chars_ptr for objects imported from, exported to, or passed to C.

```ada
function To_Chars_Ptr (Item      : in char_array_access;
                       Nul_Check : in Boolean := False)
   return chars_ptr;

```

{8652/0061} {AI95-00140-01} {AI05-0264-1} If Item is null, then To_Chars_Ptr returns Null_Ptr. If Item is not null, Nul_Check is True, and Item.all does not contain nul, then the function propagates Terminator_Error; otherwise, To_Chars_Ptr performs a pointer conversion with no allocation of memory.

```ada
function New_Char_Array (Chars   : in char_array) return chars_ptr;

```

This function returns a pointer to an allocated object initialized to Chars(Chars'First .. Index) & nul, where 

Index = Chars'Last if Chars does not contain nul, or

Index is the smallest size_t value I such that Chars(I+1) = nul. 

Storage_Error is propagated if the allocation fails.

```ada
function New_String (Str : in String) return chars_ptr;

```

This function is equivalent to New_Char_Array(To_C(Str)).

```ada
procedure Free (Item : in out chars_ptr);

```

If Item is Null_Ptr, then Free has no effect. Otherwise, Free releases the storage occupied by Value(Item), and resets Item to Null_Ptr.

```ada
function Value (Item : in chars_ptr) return char_array;

```

{AI05-0264-1} If Item = Null_Ptr, then Value propagates Dereference_Error. Otherwise, Value returns the prefix of the array of chars pointed to by Item, up to and including the first nul. The lower bound of the result is 0. If Item does not point to a nul-terminated string, then execution of Value is erroneous.

```ada
function Value (Item : in chars_ptr; Length : in size_t)
   return char_array;

```

{8652/0062} {AI95-00139-01} {AI05-0264-1} If Item = Null_Ptr, then Value propagates Dereference_Error. Otherwise, Value returns the shorter of two arrays, either the first Length chars pointed to by Item, or Value(Item). The lower bound of the result is 0. If Length is 0, then Value propagates Constraint_Error. 

Ramification: Value(New_Char_Array(Chars)) = Chars if Chars does not contain nul; else Value(New_Char_Array( Chars)) is the prefix of Chars up to and including the first nul. 

```ada
function Value (Item : in chars_ptr) return String;

```

Equivalent to To_Ada(Value(Item), Trim_Nul=&gtTrue).

```ada
function Value (Item : in chars_ptr; Length : in size_t)
   return String;

```

{8652/0063} {AI95-00177-01} Equivalent to To_Ada(Value(Item, Length) & nul, Trim_Nul=&gtTrue).

```ada
function Strlen (Item : in chars_ptr) return size_t;

```

Returns Val'Length1 where Val = Value(Item); propagates Dereference_Error if Item = Null_Ptr. 

Ramification: Strlen returns the number of chars in the array pointed to by Item, up to and including the char immediately before the first nul.

Strlen has the same possibility for erroneous execution as Value, in cases where the string has not been nul-terminated.

Strlen has the effect of C's strlen function. 

```ada
procedure Update (Item   : in chars_ptr;
                  Offset : in size_t;
                  Chars  : in char_array;
                  Check  : Boolean := True);

```

{8652/0064} {AI95-00039-01} If Item = Null_Ptr, then Update propagates Dereference_Error. Otherwise, this procedure updates the value pointed to by Item, starting at position Offset, using Chars as the data to be copied into the array. Overwriting the nul terminator, and skipping with the Offset past the nul terminator, are both prevented if Check is True, as follows: 

Let N = Strlen(Item). If Check is True, then: 

If Offset+Chars'Length&gtN, propagate Update_Error.

Otherwise, overwrite the data in the array pointed to by Item, starting at the char at position Offset, with the data in Chars. 

If Check is False, then processing is as above, but with no check that Offset+Chars'Length&gtN. 

Ramification: If Chars contains nul, Update's effect may be to "shorten" the pointed-to char array.

```ada
procedure Update (Item   : in chars_ptr;
                  Offset : in size_t;
                  Str    : in String;
                  Check  : in Boolean := True);

```

{AI95-00242-01} Equivalent to Update(Item, Offset, To_C(Str, Append_Nul =&gt False), Check). 

Discussion: {AI95-00242-01} To truncate the Item to the length of Str, use Update(Item, Offset, To_C(Str), Check) instead of Update(Item, Offset, Str, Check). Note that when truncating Item, Item must be longer than Str. 


#### Erroneous Execution

Execution of any of the following is erroneous if the Item parameter is not null_ptr and Item does not point to a nul-terminated array of chars. 

a Value function not taking a Length parameter,

the Free procedure,

the Strlen function. 

Execution of Free(X) is also erroneous if the chars_ptr X was not returned by New_Char_Array or New_String.

Reading or updating a freed char_array is erroneous.

Execution of Update is erroneous if Check is False and a call with Check equal to True would have propagated Update_Error. 

NOTE   {AI12-0440-1} New_Char_Array and New_String can be implemented either through the allocation function from the C environment ("malloc") or through Ada dynamic memory allocation ("new"). The key points are 

{AI12-0440-1} the returned value (a chars_ptr) is represented as a C "char *" so that it can be passed to C functions;

{AI12-0442-1} the allocated object can be freed by the programmer via a call of Free, rather than by calling a C function. 


#### Inconsistencies With Ada 95

{AI95-00242-01} Amendment Correction: Update for a String parameter is now defined to not add a nul character. It did add a nul in Ada 95. This means that programs that used this behavior of Update to truncate a string will no longer work (the string will not be truncated). This change makes Update for a string consistent with Update for a char_array (no implicit nul is added to the end of a char_array). 


#### Extensions to Ada 95

{AI95-00161-01} Amendment Correction: Added [pragma](./AA-2.8#S0019) Preelaborable_Initialization to type chars_ptr, so that it can be used in preelaborated units.

{AI95-00276-01} Amendment Correction: The components of chars_ptr_array are aliased so that it can be used to instantiate Interfaces.C.Pointers (that is its intended purpose, which is otherwise mysterious as it has no operations). 


#### Wording Changes from Ada 95

{8652/0061} {AI95-00140-01} Corrigendum: Fixed the missing semantics of To_Char_Ptr when Nul_Check is False.

{8652/0062} {AI95-00139-01} Corrigendum: Fixed the missing semantics of Value when the Length is 0.

{8652/0063} {AI95-00177-01} Corrigendum: Corrected the definition of Value to avoid raising Terminator_Error.

{8652/0064} {AI95-00039-01} Corrigendum: Fixed the missing semantics of Update when Item is Null_Ptr. 


## B.3.2  The Generic Package Interfaces.C.Pointers

The generic package Interfaces.C.Pointers allows the Ada programmer to perform C-style operations on pointers. It includes an access type Pointer, Value functions that dereference a Pointer and deliver the designated array, several pointer arithmetic operations, and "copy" procedures that copy the contents of a source pointer into the array designated by a destination pointer. As in C, it treats an object Ptr of type Pointer as a pointer to the first element of an array, so that for example, adding 1 to Ptr yields a pointer to the second element of the array.

{AI12-0445-1} The generic allows two styles of usage: one in which the array is terminated by a special terminator element; and another in which the programmer keeps track of the length. 


#### Static Semantics

The generic library package Interfaces.C.Pointers has the following declaration: 

```ada
{AI12-0241-1} {AI12-0302-1} generic
   type Index is (&lt&gt);
   type Element is private;
   type Element_Array is array (Index range &lt&gt) of aliased Element;
   Default_Terminator : Element;
package Interfaces.C.Pointers 
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
   type Pointer is access all Element;

```

```ada
   function Value(Ref        : in Pointer;
                  Terminator : in Element := Default_Terminator)
      return Element_Array;

```

```ada
   function Value(Ref    : in Pointer;
                  Length : in ptrdiff_t)
      return Element_Array;

```

```ada
   Pointer_Error : exception;

```

```ada
   -- C-style Pointer arithmetic

```

```ada
{AI05-0229-1}    function "+" (Left : in Pointer;   Right : in ptrdiff_t) return Pointer
      with Convention =&gt Intrinsic;
   function "+" (Left : in ptrdiff_t; Right : in Pointer)   return Pointer
      with Convention =&gt Intrinsic;
   function "-" (Left : in Pointer;   Right : in ptrdiff_t) return Pointer
      with Convention =&gt Intrinsic;
   function "-" (Left : in Pointer;   Right : in Pointer) return ptrdiff_t
      with Convention =&gt Intrinsic;

```

```ada
{AI05-0229-1}    procedure Increment (Ref : in out Pointer)
      with Convention =&gt Intrinsic;
   procedure Decrement (Ref : in out Pointer)
      with Convention =&gt Intrinsic;

```

```ada
This paragraph was deleted.{AI05-0229-1} 

```

```ada
   function Virtual_Length (Ref        : in Pointer;
                            Terminator : in Element := Default_Terminator)
      return ptrdiff_t;

```

```ada
   procedure Copy_Terminated_Array
      (Source     : in Pointer;
       Target     : in Pointer;
       Limit      : in ptrdiff_t := ptrdiff_t'Last;
       Terminator : in Element :=  Default_Terminator);

```

```ada
   procedure Copy_Array (Source  : in Pointer;
                         Target  : in Pointer;
                         Length  : in ptrdiff_t);

```

```ada
end Interfaces.C.Pointers;

```

The type Pointer is C-compatible and corresponds to one use of C's "Element *". An object of type Pointer is interpreted as a pointer to the initial Element in an Element_Array. Two styles are supported: 

Explicit termination of an array value with Default_Terminator (a special terminator value);

Programmer-managed length, with Default_Terminator treated simply as a data element. 

```ada
function Value(Ref        : in Pointer;
               Terminator : in Element := Default_Terminator)
   return Element_Array;

```

This function returns an Element_Array whose value is the array pointed to by Ref, up to and including the first Terminator; the lower bound of the array is Index'First. Interfaces.C.Strings.Dereference_Error is propagated if Ref is null.

```ada
function Value(Ref    : in Pointer;
               Length : in ptrdiff_t)
   return Element_Array;

```

This function returns an Element_Array comprising the first Length elements pointed to by Ref. The exception Interfaces.C.Strings.Dereference_Error is propagated if Ref is null. 

The "+" and "" functions perform arithmetic on Pointer values, based on the Size of the array elements. In each of these functions, Pointer_Error is propagated if a Pointer parameter is null. 

```ada
procedure Increment (Ref : in out Pointer);

```

Equivalent to Ref := Ref+1.

```ada
procedure Decrement (Ref : in out Pointer);

```

Equivalent to Ref := Ref1.

```ada
function Virtual_Length (Ref        : in Pointer;
                         Terminator : in Element := Default_Terminator)
   return ptrdiff_t;

```

Returns the number of Elements, up to the one just before the first Terminator, in Value(Ref, Terminator).

```ada
procedure Copy_Terminated_Array
   (Source     : in Pointer;
    Target     : in Pointer;
    Limit      : in ptrdiff_t := ptrdiff_t'Last;
    Terminator : in Element := Default_Terminator);

```

This procedure copies Value(Source, Terminator) into the array pointed to by Target; it stops either after Terminator has been copied, or the number of elements copied is Limit, whichever occurs first. Dereference_Error is propagated if either Source or Target is null. 

Ramification: It is the programmer's responsibility to ensure that elements are not copied beyond the logical length of the target array. 

Implementation Note: The implementation has to take care to check the Limit first. 

```ada
procedure Copy_Array (Source  : in Pointer;
                      Target  : in Pointer;
                      Length  : in ptrdiff_t);

```

This procedure copies the first Length elements from the array pointed to by Source, into the array pointed to by Target. Dereference_Error is propagated if either Source or Target is null. 


#### Erroneous Execution

It is erroneous to dereference a Pointer that does not designate an aliased Element. 

Discussion: Such a Pointer could arise via "+", "", Increment, or Decrement.

Execution of Value(Ref, Terminator) is erroneous if Ref does not designate an aliased Element in an Element_Array terminated by Terminator.

Execution of Value(Ref, Length) is erroneous if Ref does not designate an aliased Element in an Element_Array containing at least Length Elements between the designated Element and the end of the array, inclusive.

Execution of Virtual_Length(Ref, Terminator) is erroneous if Ref does not designate an aliased Element in an Element_Array terminated by Terminator.

Execution of Copy_Terminated_Array(Source, Target, Limit, Terminator) is erroneous in either of the following situations: 

Execution of both Value(Source, Terminator) and Value(Source, Limit) are erroneous, or

Copying writes past the end of the array containing the Element designated by Target. 

Execution of Copy_Array(Source, Target, Length) is erroneous if either Value(Source, Length) is erroneous, or copying writes past the end of the array containing the Element designated by Target. 

NOTE   To compose a Pointer from an Element_Array, use 'Access on the first element. For example (assuming appropriate instantiations): 

```ada
Some_Array   : Element_Array(0..5) ;
Some_Pointer : Pointer := Some_Array(0)'Access;

```


#### Examples

Example of Interfaces.C.Pointers: 

```ada
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
procedure Test_Pointers is
   package C renames Interfaces.C;
   package Char_Ptrs is
      new C.Pointers (Index              =&gt C.size_t,
                      Element            =&gt C.char,
                      Element_Array      =&gt C.char_array,
                      Default_Terminator =&gt C.nul);

```

```ada
   use type Char_Ptrs.Pointer;
   subtype Char_Star is Char_Ptrs.Pointer;

```

```ada
   procedure Strcpy (Target_Ptr, Source_Ptr : Char_Star) is
      Target_Temp_Ptr : Char_Star := Target_Ptr;
      Source_Temp_Ptr : Char_Star := Source_Ptr;
      Element : C.char;
   begin
      if Target_Temp_Ptr = null or Source_Temp_Ptr = null then
         raise C.Strings.Dereference_Error;
      end if;

```

```ada
{8652/0065} {AI95-00142-01}       loop
         Element             := Source_Temp_Ptr.all;
         Target_Temp_Ptr.all := Element;
         exit when C."="(Element, C.nul);
         Char_Ptrs.Increment(Target_Temp_Ptr);
         Char_Ptrs.Increment(Source_Temp_Ptr);
      end loop;
   end Strcpy;
begin
   ...
end Test_Pointers;

```


## B.3.3  Unchecked Union Types

{AI95-00216-01} {AI05-0229-1} {AI05-0269-1} [Specifying aspect Unchecked_Union to have the value True defines an interface correspondence between a given discriminated type and some C union. The aspect requires that the associated type shall be given a representation that allocates no space for its discriminant(s).] 

Paragraphs 2 through 3 were moved to Annex J, "Obsolescent Features". 


#### Static Semantics

{AI05-0229-1} For a discriminated record type having a [variant_part](./AA-3.8#S0071), the following language-defined representation aspect may be specified:

Unchecked_UnionThe type of aspect Unchecked_Union is Boolean. If directly specified, the [aspect_definition](./AA-13.1#S0348) shall be a static expression. If not specified (including by inheritance), the aspect is False.

Aspect Description for Unchecked_Union: Type is used to interface to a C union type.


#### Legality Rules

Paragraphs 4 and 5 were deleted. 

{AI95-00216-01} {AI05-0229-1} A type for which aspect Unchecked_Union is True is called an unchecked union type. A subtype of an unchecked union type is defined to be an unchecked union subtype. An object of an unchecked union type is defined to be an unchecked union object.

{AI95-00216-01} All component subtypes of an unchecked union type shall be C-compatible.

{AI95-00216-01} If a component subtype of an unchecked union type is subject to a per-object constraint, then the component subtype shall be an unchecked union subtype.

{AI95-00216-01} {AI05-0026-1} {AI12-0174-1} Any name that denotes a discriminant of an object of an unchecked union type shall occur within the declarative region of the type or as the [selector_name](./AA-4.1#S0099) of an [aggregate](./AA-4.3#S0106), and shall not occur within a [record_representation_clause](./AA-13.5#S0352).

{AI95-00216-01} {AI05-0026-1} The type of a component declared in a [variant_part](./AA-3.8#S0071) of an unchecked union type shall not need finalization. In addition to the places where Legality Rules normally apply (see 12.3), this rule also applies in the private part of an instance of a generic unit. For an unchecked union type declared within the body of a generic unit, or within the body of any of its descendant library units, no part of the type of a component declared in a [variant_part](./AA-3.8#S0071) of the unchecked union type shall be of a formal private type or formal private extension declared within the formal part of the generic unit.

Reason: {AI05-0026-1} The last part is a classic assume-the-worst rule that avoids dependence on the actuals in a generic body. We did not include this in the definition of "needs finalization" as it has a bad interaction with the use of that term for the No_Nested_Finalization restriction. 

{AI95-00216-01} The completion of an incomplete or private type declaration having a [known_discriminant_part](./AA-3.7#S0061) shall not be an unchecked union type.

{AI95-00216-01} An unchecked union subtype shall only be passed as a generic actual parameter if the corresponding formal type has no known discriminants or is an unchecked union type.

Ramification: This includes formal private types without a [known_discriminant_part](./AA-3.7#S0061), formal derived types that do not inherit any discriminants (formal derived types do not have [known_discriminant_part](./AA-3.7#S0061)s), and formal derived types that are unchecked union types. 


#### Static Semantics

{AI95-00216-01} An unchecked union type is eligible for convention C.

{AI95-00216-01} All objects of an unchecked union type have the same size.

{AI95-00216-01} Discriminants of objects of an unchecked union type are of size zero.

{AI95-00216-01} Any check which would require reading a discriminant of an unchecked union object is suppressed (see 11.5). These checks include:

The check performed when addressing a variant component (i.e., a component that was declared in a variant part) of an unchecked union object that the object has this component (see 4.1.3).

Any checks associated with a type or subtype conversion of a value of an unchecked union type (see 4.6). This includes, for example, the check associated with the implicit subtype conversion of an assignment statement.

The subtype membership check associated with the evaluation of a qualified expression (see 4.7) or an uninitialized allocator (see 4.8). 

Discussion: If a suppressed check would have failed, execution is erroneous (see 11.5). An implementation is always allowed to make a suppressed check if it can somehow determine the discriminant value. 


#### Dynamic Semantics

{AI95-00216-01} A view of an unchecked union object (including a type conversion or function call) has inferable discriminants if it has a constrained nominal subtype, unless the object is a component of an enclosing unchecked union object that is subject to a per-object constraint and the enclosing object lacks inferable discriminants.

{AI95-00216-01} An expression of an unchecked union type has inferable discriminants if it is either a name of an object with inferable discriminants or a qualified expression whose [subtype_mark](./AA-3.2#S0028) denotes a constrained subtype.

{AI95-00216-01} Program_Error is raised in the following cases:

Evaluation of the predefined equality operator for an unchecked union type if either of the operands lacks inferable discriminants.

Evaluation of the predefined equality operator for a type which has a subcomponent of an unchecked union type whose nominal subtype is unconstrained.

{AI12-0162-1} Evaluation of an individual membership test if the [subtype_mark](./AA-3.2#S0028) (if any) denotes a constrained unchecked union subtype and the tested_[simple_expression](./AA-4.4#S0138) lacks inferable discriminants.

Conversion from a derived unchecked union type to an unconstrained non-unchecked-union type if the operand of the conversion lacks inferable discriminants.

Execution of the default implementation of the Write or Read attribute of an unchecked union type.

Execution of the default implementation of the Output or Input attribute of an unchecked union type if the type lacks default discriminant values. 

Paragraph 29 was deleted. 

NOTE   {AI95-00216-01} The use of an unchecked union to obtain the effect of an unchecked conversion results in erroneous execution (see 11.5). Execution of the following example is erroneous even if Float'Size = Integer'Size:

```ada
{AI05-0229-1} type T (Flag : Boolean := False) is
   record
       case Flag is
           when False =&gt
               F1 : Float := 0.0;
           when True =&gt
               F2 : Integer := 0;
       end case;
    end record
    with Unchecked_Union;

```

```ada
X : T;
Y : Integer := X.F2; -- erroneous

```


#### Extensions to Ada 95

{AI95-00216-01} [Pragma](./AA-2.8#S0019) Unchecked_Union is new. 


#### Incompatibilities With Ada 2005

{AI05-0026-1} Correction: The use of discriminants on Unchecked_Union types is now illegal in [record_representation_clause](./AA-13.5#S0352)s, as it makes no sense to specify a position for something that is not supposed to exist. It is very unlikely that this change will have any impact on existing code. 


#### Extensions to Ada 2005

{AI05-0229-1} Aspect Unchecked_Union is new; [pragma](./AA-2.8#S0019) Unchecked_Union is now obsolescent. 


#### Wording Changes from Ada 2005

{AI05-0026-1} Correction: Revised the rules to use the "needs finalization" definition, and eliminated generic contract issues. 


#### Wording Changes from Ada 2012

{AI05-0162-1} Correction: Adjusted the wording to reflect that membership tests can have more than one expression or [subtype_mark](./AA-3.2#S0028).

{AI05-0174-1} Correction: Adjusted the wording to allow named aggregates of an unchecked union type; it is clearly madness to allow positional record components in an [aggregate](./AA-4.3#S0106) but not named component associations. 

