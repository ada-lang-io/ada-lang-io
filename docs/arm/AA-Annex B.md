---
sidebar_position:  16
---

# Annex B Interface to Other Languages

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
This Annex describes features for writing mixed-language programs. General interface support is presented first; then specific support for C, COBOL, and Fortran is defined, in terms of language interface packages for each of these languages. 

Ramification: This Annex is not a "Specialized Needs" annex. Every implementation must support all nonoptional features defined here (mainly the package Interfaces). 


#### Language Design Principles

Ada should have strong support for mixed-language programming. 


#### Extensions to Ada 83

Much of the functionality in this Annex is new to Ada 95. 


#### Wording Changes from Ada 83

This Annex contains what used to be RM83-13.8. 


## B.1  Interfacing Pragmas

A [pragma](S0016) Import is used to import an entity defined in a foreign language into an Ada program, thus allowing a foreign-language subprogram to be called from Ada, or a foreign-language variable to be accessed from Ada. In contrast, a [pragma](S0016) Export is used to export an Ada entity to a foreign language, thus allowing an Ada subprogram to be called from a foreign language, or an Ada object to be accessed from a foreign language. The [pragma](S0016)s Import and Export are intended primarily for objects and subprograms, although implementations are allowed to support other entities.

A [pragma](S0016) Convention is used to specify that an Ada entity should use the conventions of another language. It is intended primarily for types and "callback" subprograms. For example, "pragma Convention(Fortran, Matrix);" implies that Matrix should be represented according to the conventions of the supported Fortran implementation, namely column-major order.

A [pragma](S0016) Linker_Options is used to specify the system linker parameters needed when a given compilation unit is included in a partition.


#### Syntax

An interfacing pragma is a representation [pragma](S0016) that is one of the [pragma](S0016)s Import, Export, or Convention. Their forms, together with that of the related [pragma](S0016) Linker_Options, are as follows: 

  pragma Import(
     [Convention =&gt] convention_[identifier](S0002), [Entity =&gt] [local_name](S0264)
  [, [External_Name =&gt] string_[expression](S0108)] [, [Link_Name =&gt] string_[expression](S0108)]);

  pragma Export(
     [Convention =&gt] convention_[identifier](S0002), [Entity =&gt] [local_name](S0264)
  [, [External_Name =&gt] string_[expression](S0108)] [, [Link_Name =&gt] string_[expression](S0108)]);

  pragma Convention([Convention =&gt] convention_[identifier](S0002),[Entity =&gt] [local_name](S0264));

  pragma Linker_Options(string_[expression](S0108));

A [pragma](S0016) Linker_Options is allowed only at the place of a [declarative_item](S0080).


#### Name Resolution Rules

The expected type for a string_[expression](S0108) in an interfacing pragma or in pragma Linker_Options is String. 

Ramification: There is no language-defined support for external or link names of type Wide_String, or of other string types. Implementations may, of course, have additional pragmas for that purpose. Note that allowing both String and Wide_String in the same [pragma](S0016) would cause ambiguities. 


#### Legality Rules

The convention_[identifier](S0002) of an interfacing pragma shall be the name of a convention. The convention names are implementation defined, except for certain language-defined ones, such as Ada and Intrinsic, as explained in 6.3.1, "Conformance Rules". [Additional convention names generally represent the calling conventions of foreign languages, language implementations, or specific run-time models.] The convention of a callable entity is its calling convention. 

Implementation defined: Implementation-defined convention names.

Discussion: We considered representing the convention names using an enumeration type declared in System. Then, convention_[identifier](S0002) would be changed to convention_[name](S0084), and we would make its expected type be the enumeration type. We didn't do this because it seems to introduce extra complexity, and because the list of available languages is better represented as the list of children of package Interfaces - a more open-ended sort of list. 

If L is a convention_[identifier](S0002) for a language, then a type T is said to be compatible with convention L, (alternatively, is said to be an L-compatible type) if any of the following conditions are met: 

T is declared in a language interface package corresponding to L and is defined to be L-compatible (see B.3, B.3.1, B.3.2, B.4, B.5),

Convention L has been specified for T in a [pragma](S0016) Convention, and T is eligible for convention L; that is: 

T is an array type with either an unconstrained or statically-constrained first subtype, and its component type is L-compatible,

T is a record type that has no discriminants and that only has components with statically-constrained subtypes, and each component type is L-compatible,

T is an access-to-object type, and its designated type is L-compatible,

T is an access-to-subprogram type, and its designated profile's parameter and result types are all L-compatible. 

T is derived from an L-compatible type,

The implementation permits T as an L-compatible type.

Discussion: For example, an implementation might permit Integer as a C-compatible type, though the C type to which it corresponds might be different in different environments. 

If [pragma](S0016) Convention applies to a type, then the type shall either be compatible with or eligible for the convention specified in the pragma. 

Ramification: If a type is derived from an L-compatible type, the derived type is by default L-compatible, but it is also permitted to specify pragma Convention for the derived type.

It is permitted to specify pragma Convention for an incomplete type, but in the complete declaration each component must be L-compatible.

If each component of a record type is L-compatible, then the record type itself is only L-compatible if it has a pragma Convention. 

A [pragma](S0016) Import shall be the completion of a declaration. Notwithstanding any rule to the contrary, a [pragma](S0016) Import may serve as the completion of any kind of (explicit) declaration if supported by an implementation for that kind of declaration. If a completion is a [pragma](S0016) Import, then it shall appear in the same [declarative_part](S0079), [package_specification](S0162), [task_definition](S0177) or [protected_definition](S0182) as the declaration. For a library unit, it shall appear in the same [compilation](S0214), before any subsequent [compilation_unit](S0215)s other than [pragma](S0016)s. If the [local_name](S0264) denotes more than one entity, then the [pragma](S0016) Import is the completion of all of them. 

Discussion: For declarations of deferred constants and subprograms, we mention pragma Import explicitly as a possible completion. For other declarations that require completions, we ignore the possibility of pragma Import. Nevertheless, if an implementation chooses to allow a [pragma](S0016) Import to complete the declaration of a task, protected type, incomplete type, private type, etc., it may do so, and the normal completion is then not allowed for that declaration. 

 An entity specified as the Entity argument to a [pragma](S0016) Import (or [pragma](S0016) Export) is said to be imported (respectively, exported).

The declaration of an imported object shall not include an explicit initialization expression. [Default initializations are not performed.] 

Proof: This follows from the "Notwithstanding ..." wording in the Dynamics Semantics paragraphs below. 

The type of an imported or exported object shall be compatible with the convention specified in the corresponding [pragma](S0016). 

Ramification: This implies, for example, that importing an Integer object might be illegal, whereas importing an object of type Interfaces.C.int would be permitted. 

For an imported or exported subprogram, the result and parameter types shall each be compatible with the convention specified in the corresponding pragma.

The external name and link name string_[expression](S0108)s of a [pragma](S0016) Import or Export, and the string_[expression](S0108) of a [pragma](S0016) Linker_Options, shall be static.


#### Static Semantics

Import, Export, and Convention [pragma](S0016)s are representation pragmas that specify the convention aspect of representation. In addition, Import and Export [pragma](S0016)s specify the imported and exported aspects of representation, respectively.

An interfacing pragma is a program unit pragma when applied to a program unit (see 10.1.5).

An interfacing pragma defines the convention of the entity denoted by the [local_name](S0264). The convention represents the calling convention or representation convention of the entity. For an access-to-subprogram type, it represents the calling convention of designated subprograms. In addition: 

A [pragma](S0016) Import specifies that the entity is defined externally (that is, outside the Ada program).

A [pragma](S0016) Export specifies that the entity is used externally.

A [pragma](S0016) Import or Export optionally specifies an entity's external name, link name, or both. 

An external name is a string value for the name used by a foreign language program either for an entity that an Ada program imports, or for referring to an entity that an Ada program exports.

A link name is a string value for the name of an exported or imported entity, based on the conventions of the foreign language's compiler in interfacing with the system's linker tool.

The meaning of link names is implementation defined. If neither a link name nor the Address attribute of an imported or exported entity is specified, then a link name is chosen in an implementation-defined manner, based on the external name if one is specified. 

Implementation defined: The meaning of link names.

Ramification: For example, an implementation might always prepend "_", and then pass it to the system linker. 

Implementation defined: The manner of choosing link names when neither the link name nor the address of an imported or exported entity is specified.

Ramification: Normally, this will be the entity's defining name, or some simple transformation thereof. 

Pragma Linker_Options has the effect of passing its string argument as a parameter to the system linker (if one exists), if the immediately enclosing compilation unit is included in the partition being linked. The interpretation of the string argument, and the way in which the string arguments from multiple Linker_Options pragmas are combined, is implementation defined. 

Implementation defined: The effect of pragma Linker_Options.


#### Dynamic Semantics

Notwithstanding what this document says elsewhere, the elaboration of a declaration denoted by the [local_name](S0264) of a [pragma](S0016) Import does not create the entity. Such an elaboration has no other effect than to allow the defining name to denote the external entity. 

Ramification: This implies that default initializations are skipped. (Explicit initializations are illegal.) For example, an imported access object is not initialized to null.

Note that the [local_name](S0264) in a [pragma](S0016) Import might denote more than one declaration; in that case, the entity of all of those declarations will be the external entity. 

Discussion: This "notwithstanding" wording is better than saying "unless named by a [pragma](S0016) Import" on every definition of elaboration. It says we recognize the contradiction, and this rule takes precedence. 


#### Implementation Advice

If an implementation supports pragma Export to a given language, then it should also allow the main subprogram to be written in that language. It should support some mechanism for invoking the elaboration of the Ada library units included in the system, and for invoking the finalization of the environment task. On typical systems, the recommended mechanism is to provide two subprograms whose link names are "adainit" and "adafinal". Adainit should contain the elaboration code for library units. Adafinal should contain the finalization code. These subprograms should have no effect the second and subsequent time they are called. 

Implementation Advice: 

Ramification: For example, if the main subprogram is written in C, it can call adainit before the first call to an Ada subprogram, and adafinal after the last. 

Automatic elaboration of preelaborated packages should be provided when [pragma](S0016) Export is supported. 

Implementation Advice: 

For each supported convention L other than Intrinsic, an implementation should support Import and Export [pragma](S0016)s for objects of L-compatible types and for subprograms, and [pragma](S0016) Convention for L-eligible types and for subprograms, presuming the other language has corresponding features. [Pragma](S0016) Convention need not be supported for scalar types. 

Implementation Advice: 

Reason: Pragma Convention is not necessary for scalar types, since the language interface packages declare scalar types corresponding to those provided by the respective foreign languages. 

Implementation Note: If an implementation supports interfacing to C++, it should do so via the convention identifier C_Plus_Plus (in additional to any C++-implementation-specific ones). 

Reason: The reason for giving the advice about C++ is to encourage uniformity among implementations, given that the name of the language is not syntactically legal as an [identifier](S0002). We place this advice in the AARM, rather than the RM95 proper, because (as of this writing) C++ is not an international standard, and we don't want to refer to a such a language from an international standard. 

NOTE 1   Implementations may place restrictions on interfacing pragmas; for example, requiring each exported entity to be declared at the library level. 

Proof: Arbitrary restrictions are allowed by 13.1. 

Ramification: Such a restriction might be to disallow them altogether. Alternatively, the implementation might allow them only for certain kinds of entities, or only for certain conventions. 

NOTE 2   A [pragma](S0016) Import specifies the conventions for accessing external entities. It is possible that the actual entity is written in assembly language, but reflects the conventions of a particular language. For example, pragma Import(Ada, ...) can be used to interface to an assembly language routine that obeys the Ada compiler's calling conventions.

NOTE 3   To obtain "call-back" to an Ada subprogram from a foreign language environment, pragma Convention should be specified both for the access-to-subprogram type and the specific subprogram(s) to which 'Access is applied.

NOTE 4   It is illegal to specify more than one of Import, Export, or Convention for a given entity.

NOTE 5   The [local_name](S0264) in an interfacing pragma can denote more than one entity in the case of overloading. Such a [pragma](S0016) applies to all of the denoted entities.

NOTE 6   See also 13.8, "Machine Code Insertions". 

Ramification: The Intrinsic convention (see 6.3.1) implies that the entity is somehow "built in" to the implementation. Thus, it generally does not make sense for users to specify Intrinsic in a [pragma](S0016) Import. The intention is that only implementations will specify Intrinsic in a [pragma](S0016) Import. The language also defines certain subprograms to be Intrinsic. 

Discussion: There are many imaginable interfacing pragmas that don't make any sense. For example, setting the Convention of a protected procedure to Ada is probably wrong. Rather than enumerating all such cases, however, we leave it up to implementations to decide what is sensible. 

NOTE 7   If both External_Name and Link_Name are specified for an Import or Export pragma, then the External_Name is ignored.

NOTE 8   An interfacing pragma might result in an effect that violates Ada semantics. 


#### Examples

Example of interfacing pragmas: 

```ada
package Fortran_Library is
  function Sqrt (X : Float) return Float;
  function Exp  (X : Float) return Float;
private
  pragma Import(Fortran, Sqrt);
  pragma Import(Fortran, Exp);
end Fortran_Library;

```


#### Extensions to Ada 83

Interfacing pragmas are new to Ada 95. Pragma Import replaces Ada 83's pragma Interface. Existing implementations can continue to support pragma Interface for upward compatibility. 


## B.2  The Package Interfaces

Package Interfaces is the parent of several library packages that declare types and other entities useful for interfacing to foreign languages. It also contains some implementation-defined types that are useful across more than one language (in particular for interfacing to assembly language). 

Implementation defined: The contents of the visible part of package Interfaces and its language-defined descendants.


#### Static Semantics

The library package Interfaces has the following skeletal declaration: 

```ada
package Interfaces is
   pragma Pure(Interfaces);

```

```ada
   type Integer_n is range -2**(n-1) .. 2**(n-1) - 1;  --2's complement

```

```ada
   type Unsigned_n is mod 2**n;

```

```ada
   function Shift_Left  (Value : Unsigned_n; Amount : Natural)
      return Unsigned_n;
   function Shift_Right (Value : Unsigned_n; Amount : Natural)
      return Unsigned_n;
   function Shift_Right_Arithmetic (Value : Unsigned_n; Amount : Natural)
      return Unsigned_n;
   function Rotate_Left  (Value : Unsigned_n; Amount : Natural)
      return Unsigned_n;
   function Rotate_Right (Value : Unsigned_n; Amount : Natural)
      return Unsigned_n;
   ...
end Interfaces;

```


#### Implementation Requirements

An implementation shall provide the following declarations in the visible part of package Interfaces: 

Signed and modular integer types of n bits, if supported by the target architecture, for each n that is at least the size of a storage element and that is a factor of the word size. The names of these types are of the form Integer_n for the signed types, and Unsigned_n for the modular types; 

Ramification: For example, for a typical 32-bit machine the corresponding types might be Integer_8, Unsigned_8, Integer_16, Unsigned_16, Integer_32, and Unsigned_32.

The wording above implies, for example, that Integer_16'Size = Unsigned_16'Size = 16. Unchecked conversions between same-Sized types will work as expected. 

For each such modular type in Interfaces, shifting and rotating subprograms as specified in the declaration of Interfaces above. These subprograms are Intrinsic. They operate on a bit-by-bit basis, using the binary representation of the value of the operands to yield a binary representation for the result. The Amount parameter gives the number of bits by which to shift or rotate. For shifting, zero bits are shifted in, except in the case of Shift_Right_Arithmetic, where one bits are shifted in if Value is at least half the modulus. 

Reason: We considered making shifting and rotating be primitive operations of all modular types. However, it is a design principle of Ada that all predefined operations should be operators (not functions named by identifiers). (Note that an early version of Ada had "abs" as an identifier, but it was changed to a reserved word operator before standardization of Ada 83.) This is important because the implicit declarations would hide nonoverloadable declarations with the same name, whereas operators are always overloadable. Therefore, we would have had to make shift and rotate into reserved words, which would have been upward incompatible, or else invent new operator symbols, which seemed like too much mechanism. 

Floating point types corresponding to each floating point format fully supported by the hardware. 

Implementation Note: The names for these floating point types are not specified. However, if IEEE arithmetic is supported, then the names should be IEEE_Float_32 and IEEE_Float_64 for single and double precision, respectively.


#### Implementation Permissions

An implementation may provide implementation-defined library units that are children of Interfaces, and may add declarations to the visible part of Interfaces in addition to the ones defined above. 

Implementation defined: Implementation-defined children of package Interfaces. The contents of the visible part of package Interfaces.


#### Implementation Advice

For each implementation-defined convention identifier, there should be a child package of package Interfaces with the corresponding name. This package should contain any declarations that would be useful for interfacing to the language (implementation) represented by the convention. Any declarations useful for interfacing to any language on the given hardware architecture should be provided directly in Interfaces. 

Ramification: For example, package Interfaces.XYZ_Pascal might contain declarations of types that match the data types provided by the XYZ implementation of Pascal, so that it will be more convenient to pass parameters to a subprogram whose convention is XYZ_Pascal. 

An implementation supporting an interface to C, COBOL, or Fortran should provide the corresponding package or packages described in the following clauses. 

Implementation Note: The intention is that an implementation might support several implementations of the foreign language: Interfaces.This_Fortran and Interfaces.That_Fortran might both exist. The "default" implementation, overridable by the user, should be declared as a renaming: 

```ada
package Interfaces.Fortran renames Interfaces.This_Fortran;

```


## B.3  Interfacing with C

The facilities relevant to interfacing with the C language are the package Interfaces.C and its children; and support for the Import, Export, and Convention pragmas with convention_[identifier](S0002) C.

The package Interfaces.C contains the basic types, constants and subprograms that allow an Ada program to pass scalars and strings to C functions. 


#### Static Semantics

The library package Interfaces.C has the following declaration: 

```ada
package Interfaces.C is
   pragma Pure(C);

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
   nul : constant char := char'First;

```

```ada
   function To_C   (Item : in Character) return char;

```

```ada
   function To_Ada (Item : in char) return Character;

```

```ada
   type char_array is array (size_t range &lt&gt) of aliased char;
   pragma Pack(char_array);
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
   type wchar_t is implementation-defined;

```

```ada
   wide_nul : constant wchar_t := wchar_t'First;

```

```ada
   function To_C   (Item : in Wide_Character) return wchar_t;
   function To_Ada (Item : in wchar_t       ) return Wide_Character;

```

```ada
   type wchar_array is array (size_t range &lt&gt) of aliased wchar_t;

```

```ada
   pragma Pack(wchar_array);

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
   Terminator_Error : exception;

```

```ada
end Interfaces.C;

```

Each of the types declared in Interfaces.C is C-compatible.

The types int, short, long, unsigned, ptrdiff_t, size_t, double, char, and wchar_t correspond respectively to the C types having the same names. The types signed_char, unsigned_short, unsigned_long, unsigned_char, C_float, and long_double correspond respectively to the C types signed char, unsigned short, unsigned long, unsigned char, float, and long double.

The type of the subtype plain_char is either signed_char or unsigned_char, depending on the C implementation. 

```ada
function To_C   (Item : in Character) return char;
function To_Ada (Item : in char     ) return Character;

```

The functions To_C and To_Ada map between the Ada type Character and the C type char.

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

The result of To_C is a char_array value of length Item'Length (if Append_Nul is False) or Item'Length+1 (if Append_Nul is True). The lower bound is 0. For each component Item(I), the corresponding component in the result is To_C applied to Item(I). The value nul is appended if Append_Nul is True.

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

Discussion: The Interfaces.C package provides an implementation-defined character type, char, designed to model the C run-time character set, and mappings between the types char and Character.

One application of the C interface package is to compose a C string and pass it to a C function. One way to do this is for the programmer to declare an object that will hold the C array, and then pass this array to the C function. This is realized via the type char_array: 

```ada
type char_array is array (size_t range &lt&gt) of Char;

```

The programmer can declare an Ada String, convert it to a char_array, and pass the char_array as actual parameter to the C function that is expecting a char *.

An alternative approach is for the programmer to obtain a C char pointer from an Ada String (or from a char_array) by invoking an allocation function. The package Interfaces.C.Strings (see below) supplies the needed facilities, including a private type chars_ptr that corresponds to C's char *, and two allocation functions. To avoid storage leakage, a Free procedure releases the storage that was allocated by one of these allocate functions.

It is typical for a C function that deals with strings to adopt the convention that the string is delimited by a nul char. The C interface packages support this convention. A constant nul of type Char is declared, and the function Value(Chars_Ptr) in Interfaces.C.Strings returns a char_array up to and including the first nul in the array that the chars_ptr points to. The Allocate_Chars function allocates an array that is nul terminated.

Some C functions that deal with strings take an explicit length as a parameter, thus allowing strings to be passed that contain nul as a data element. Other C functions take an explicit length that is an upper bound: the prefix of the string up to the char before nul, or the prefix of the given length, is used by the function, whichever is shorter. The C Interface packages support calling such functions. 


#### Implementation Requirements

An implementation shall support pragma Convention with a C convention_[identifier](S0002) for a C-eligible type (see B.1) 


#### Implementation Permissions

An implementation may provide additional declarations in the C interface packages.


#### Implementation Advice

An implementation should support the following interface correspondences between Ada and C. 

An Ada procedure corresponds to a void-returning C function. 

Discussion: The programmer can also choose an Ada procedure when the C function returns an int that is to be discarded.

An Ada function corresponds to a non-void C function.

An Ada in scalar parameter is passed as a scalar argument to a C function.

An Ada in parameter of an access-to-object type with designated type T is passed as a t* argument to a C function, where t is the C type corresponding to the Ada type T.

An Ada access T parameter, or an Ada out or in out parameter of an elementary type T, is passed as a t* argument to a C function, where t is the C type corresponding to the Ada type T. In the case of an elementary out or in out parameter, a pointer to a temporary copy is used to preserve by-copy semantics.

An Ada parameter of a record type T, of any mode, is passed as a t* argument to a C function, where t is the C struct corresponding to the Ada type T.

An Ada parameter of an array type with component type T, of any mode, is passed as a t* argument to a C function, where t is the C type corresponding to the Ada type T.

An Ada parameter of an access-to-subprogram type is passed as a pointer to a C function whose prototype corresponds to the designated subprogram's specification.

NOTE 1   Values of type char_array are not implicitly terminated with nul. If a char_array is to be passed as a parameter to an imported C function requiring nul termination, it is the programmer's responsibility to obtain this effect.

NOTE 2   To obtain the effect of C's sizeof(item_type), where Item_Type is the corresponding Ada type, evaluate the expression: size_t(Item_Type'Size/CHAR_BIT).

NOTE 3   There is no explicit support for C's union types. Unchecked conversions can be used to obtain the effect of C unions.

NOTE 4   A C function that takes a variable number of arguments can correspond to several Ada subprograms, taking various specific numbers and types of parameters. 


#### Examples

Example of using the Interfaces.C package: 

```ada
--Calling the C Library Function strcpy
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
   -- Note: since the C function's return value is of no interest, the Ada interface is a procedure
   procedure Strcpy (Target : out C.char_array;
                     Source : in  C.char_array);

```

```ada
   pragma Import(C, Strcpy, "strcpy");

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
end Test;

```


### B.3.1  The Package Interfaces.C.Strings

The package Interfaces.C.Strings declares types and subprograms allowing an Ada program to allocate, reference, update, and free C-style strings. In particular, the private type chars_ptr corresponds to a common use of "char *" in C programs, and an object of this type can be passed to a subprogram to which [pragma](S0016) Import(C,...) has been applied, and for which "char *" is the type of the argument of the C function. 


#### Static Semantics

The library package Interfaces.C.Strings has the following declaration: 

```ada
package Interfaces.C.Strings  is
   pragma Preelaborate(Strings);

```

```ada
   type char_array_access is access all char_array;

```

```ada
   type chars_ptr is private;

```

```ada
   type chars_ptr_array is array (size_t range &lt&gt) of chars_ptr;

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

Discussion: The string manipulation types and subprograms appear in a child of Interfaces.C versus being there directly, since it is useful to have Interfaces.C specified as [pragma](S0016) Pure.

Differently named functions New_String and New_Char_Array are declared, since if there were a single overloaded function a call with a string literal as actual parameter would be ambiguous. 

The type chars_ptr is C-compatible and corresponds to the use of C's "char *" for a pointer to the first char in a char array terminated by nul. When an object of type chars_ptr is declared, its value is by default set to Null_Ptr, unless the object is imported (see B.1). 

Discussion: The type char_array_access is not necessarily C-compatible, since an object of this type may carry "dope" information. The programmer should convert from char_array_access to chars_ptr for objects imported from, exported to, or passed to C.

```ada
function To_Chars_Ptr (Item      : in char_array_access;
                       Nul_Check : in Boolean := False)
   return chars_ptr;

```

If Item is null, then To_Chars_Ptr returns Null_Ptr. Otherwise, if Nul_Check is True and Item.all does not contain nul, then the function propagates Terminator_Error; if Nul_Check is True and Item.all does contain nul, To_Chars_Ptr performs a pointer conversion with no allocation of memory.

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

If Item = Null_Ptr then Value propagates Dereference_Error. Otherwise Value returns the prefix of the array of chars pointed to by Item, up to and including the first nul. The lower bound of the result is 0. If Item does not point to a nul-terminated string, then execution of Value is erroneous.

```ada
function Value (Item : in chars_ptr; Length : in size_t)
   return char_array;

```

If Item = Null_Ptr then Value(Item) propagates Dereference_Error. Otherwise Value returns the shorter of two arrays: the first Length chars pointed to by Item, and Value(Item). The lower bound of the result is 0. 

Ramification: Value(New_Char_Array(Chars)) = Chars if Chars does not contain nul; else Value(New_Char_Array( Chars)) is the prefix of Chars up to and including the first nul. 

```ada
function Value (Item : in chars_ptr) return String;

```

Equivalent to To_Ada(Value(Item), Trim_Nul=&gtTrue).

```ada
function Value (Item : in chars_ptr; Length : in size_t)
   return String;

```

Equivalent to To_Ada(Value(Item, Length), Trim_Nul=&gtTrue).

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

This procedure updates the value pointed to by Item, starting at position Offset, using Chars as the data to be copied into the array. Overwriting the nul terminator, and skipping with the Offset past the nul terminator, are both prevented if Check is True, as follows: 

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

Equivalent to Update(Item, Offset, To_C(Str), Check). 


#### Erroneous Execution

Execution of any of the following is erroneous if the Item parameter is not null_ptr and Item does not point to a nul-terminated array of chars. 

a Value function not taking a Length parameter,

the Free procedure,

the Strlen function. 

Execution of Free(X) is also erroneous if the chars_ptr X was not returned by New_Char_Array or New_String.

Reading or updating a freed char_array is erroneous.

Execution of Update is erroneous if Check is False and a call with Check equal to True would have propagated Update_Error. 

NOTE   New_Char_Array and New_String might be implemented either through the allocation function from the C environment ("malloc") or through Ada dynamic memory allocation ("new"). The key points are 

the returned value (a chars_ptr) is represented as a C "char *" so that it may be passed to C functions;

the allocated object should be freed by the programmer via a call of Free, not by a called C function. 


### B.3.2  The Generic Package Interfaces.C.Pointers

The generic package Interfaces.C.Pointers allows the Ada programmer to perform C-style operations on pointers. It includes an access type Pointer, Value functions that dereference a Pointer and deliver the designated array, several pointer arithmetic operations, and "copy" procedures that copy the contents of a source pointer into the array designated by a destination pointer. As in C, it treats an object Ptr of type Pointer as a pointer to the first element of an array, so that for example, adding 1 to Ptr yields a pointer to the second element of the array.

The generic allows two styles of usage: one in which the array is terminated by a special terminator element; and another in which the programmer needs to keep track of the length. 


#### Static Semantics

The generic library package Interfaces.C.Pointers has the following declaration: 

```ada
generic
   type Index is (&lt&gt);
   type Element is private;
   type Element_Array is array (Index range &lt&gt) of aliased Element;
   Default_Terminator : Element;
package Interfaces.C.Pointers  is
   pragma Preelaborate(Pointers);

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
   function "+" (Left : in Pointer;   Right : in ptrdiff_t) return Pointer;
   function "+" (Left : in ptrdiff_t; Right : in Pointer)   return Pointer;
   function "-" (Left : in Pointer;   Right : in ptrdiff_t) return Pointer;
   function "-" (Left : in Pointer;   Right : in Pointer) return ptrdiff_t;

```

```ada
   procedure Increment (Ref : in out Pointer);
   procedure Decrement (Ref : in out Pointer);

```

```ada
   pragma Convention (Intrinsic, "+");
   pragma Convention (Intrinsic, "-");
   pragma Convention (Intrinsic, Increment);
   pragma Convention (Intrinsic, Decrement);

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
      loop
         Element             := Source_Temp_Ptr.all;
         Target_Temp_Ptr.all := Element;
         exit when Element = C.nul;
         Char_Ptrs.Increment(Target_Temp_Ptr);
         Char_Ptrs.Increment(Source_Temp_Ptr);
      end loop;
   end Strcpy;
begin
   ...
end Test_Pointers;

```


#### Syntax

  


## B.4  Interfacing with COBOL

The facilities relevant to interfacing with the COBOL language are the package Interfaces.COBOL and support for the Import, Export and Convention pragmas with convention_[identifier](S0002) COBOL.

The COBOL interface package supplies several sets of facilities: 

A set of types corresponding to the native COBOL types of the supported COBOL implementation (so-called "internal COBOL representations"), allowing Ada data to be passed as parameters to COBOL programs

A set of types and constants reflecting external data representations such as might be found in files or databases, allowing COBOL-generated data to be read by an Ada program, and Ada-generated data to be read by COBOL programs

A generic package for converting between an Ada decimal type value and either an internal or external COBOL representation 


#### Static Semantics

The library package Interfaces.COBOL has the following declaration: 

```ada
package Interfaces.COBOL  is
   pragma Preelaborate(COBOL);

```

```ada
-- Types and operations for internal data representations

```

```ada
   type Floating      is digits implementation-defined;
   type Long_Floating is digits implementation-defined;

```

```ada
   type Binary      is range implementation-defined;
   type Long_Binary is range implementation-defined;

```

```ada
   Max_Digits_Binary      : constant := implementation-defined;
   Max_Digits_Long_Binary : constant := implementation-defined;

```

```ada
   type Decimal_Element  is mod implementation-defined;
   type Packed_Decimal is array (Positive range &lt&gt) of Decimal_Element;
   pragma Pack(Packed_Decimal);

```

```ada
   type COBOL_Character is implementation-defined character type;

```

```ada
   Ada_To_COBOL : array (Character) of COBOL_Character := implementation-defined;

```

```ada
   COBOL_To_Ada : array (COBOL_Character) of Character := implementation-defined;

```

```ada
   type Alphanumeric is array (Positive range &lt&gt) of COBOL_Character;
   pragma Pack(Alphanumeric);

```

```ada
   function To_COBOL (Item : in String) return Alphanumeric;
   function To_Ada   (Item : in Alphanumeric) return String;

```

```ada
   procedure To_COBOL (Item       : in String;
                       Target     : out Alphanumeric;
                       Last       : out Natural);

```

```ada
   procedure To_Ada (Item     : in Alphanumeric;
                     Target   : out String;
                     Last     : out Natural);

```

```ada
   type Numeric is array (Positive range &lt&gt) of COBOL_Character;
   pragma Pack(Numeric);

```

```ada
-- Formats for COBOL data representations

```

```ada
   type Display_Format is private;

```

```ada
   Unsigned             : constant Display_Format;
   Leading_Separate     : constant Display_Format;
   Trailing_Separate    : constant Display_Format;
   Leading_Nonseparate  : constant Display_Format;
   Trailing_Nonseparate : constant Display_Format;

```

```ada
   type Binary_Format is private;

```

```ada
   High_Order_First  : constant Binary_Format;
   Low_Order_First   : constant Binary_Format;
   Native_Binary     : constant Binary_Format;

```

```ada
   type Packed_Format is private;

```

```ada
   Packed_Unsigned   : constant Packed_Format;
   Packed_Signed     : constant Packed_Format;

```

```ada
-- Types for external representation of COBOL binary data

```

```ada
   type Byte is mod 2**COBOL_Character'Size;
   type Byte_Array is array (Positive range &lt&gt) of Byte;
   pragma Pack (Byte_Array);

```

```ada
   Conversion_Error : exception;

```

```ada
   generic
      type Num is delta &lt&gt digits &lt&gt;
   package Decimal_Conversions is

```

```ada
      -- Display Formats: data values are represented as Numeric

```

```ada
      function Valid (Item   : in Numeric;
                      Format : in Display_Format) return Boolean;

```

```ada
      function Length (Format : in Display_Format) return Natural;

```

```ada
      function To_Decimal (Item   : in Numeric;
                           Format : in Display_Format) return Num;

```

```ada
      function To_Display (Item   : in Num;
                           Format : in Display_Format) return Numeric;

```

```ada
      -- Packed Formats: data values are represented as Packed_Decimal

```

```ada
      function Valid (Item   : in Packed_Decimal;
                      Format : in Packed_Format) return Boolean;

```

```ada
      function Length (Format : in Packed_Format) return Natural;

```

```ada
      function To_Decimal (Item   : in Packed_Decimal;
                           Format : in Packed_Format) return Num;

```

```ada
      function To_Packed (Item   : in Num;
                          Format : in Packed_Format) return Packed_Decimal;

```

```ada
      -- Binary Formats: external data values are represented as Byte_Array

```

```ada
      function Valid (Item   : in Byte_Array;
                      Format : in Binary_Format) return Boolean;

```

```ada
      function Length (Format : in Binary_Format) return Natural;
      function To_Decimal (Item   : in Byte_Array;
                           Format : in Binary_Format) return Num;

```

```ada
      function To_Binary (Item   : in Num;
                        Format : in Binary_Format) return Byte_Array;

```

```ada
      -- Internal Binary formats: data values are of type Binary or Long_Binary

```

```ada
      function To_Decimal (Item : in Binary)      return Num;
      function To_Decimal (Item : in Long_Binary) return Num;

```

```ada
      function To_Binary      (Item : in Num)  return Binary;
      function To_Long_Binary (Item : in Num)  return Long_Binary;

```

```ada
   end Decimal_Conversions;

```

```ada
private
   ... -- not specified by the language
end Interfaces.COBOL;

```

Implementation defined: The types Floating, Long_Floating, Binary, Long_Binary, Decimal_Element, and COBOL_Character; and the initializations of the variables Ada_To_COBOL and COBOL_To_Ada, in Interfaces.COBOL

Each of the types in Interfaces.COBOL is COBOL-compatible.

The types Floating and Long_Floating correspond to the native types in COBOL for data items with computational usage implemented by floating point. The types Binary and Long_Binary correspond to the native types in COBOL for data items with binary usage, or with computational usage implemented by binary.

Max_Digits_Binary is the largest number of decimal digits in a numeric value that is represented as Binary. Max_Digits_Long_Binary is the largest number of decimal digits in a numeric value that is represented as Long_Binary.

The type Packed_Decimal corresponds to COBOL's packed-decimal usage.

The type COBOL_Character defines the run-time character set used in the COBOL implementation. Ada_To_COBOL and COBOL_To_Ada are the mappings between the Ada and COBOL run-time character sets. 

Reason: The character mappings are visible variables, since the user needs the ability to modify them at run time. 

Type Alphanumeric corresponds to COBOL's alphanumeric data category.

Each of the functions To_COBOL and To_Ada converts its parameter based on the mappings Ada_To_COBOL and COBOL_To_Ada, respectively. The length of the result for each is the length of the parameter, and the lower bound of the result is 1. Each component of the result is obtained by applying the relevant mapping to the corresponding component of the parameter.

Each of the procedures To_COBOL and To_Ada copies converted elements from Item to Target, using the appropriate mapping (Ada_To_COBOL or COBOL_To_Ada, respectively). The index in Target of the last element assigned is returned in Last (0 if Item is a null array). If Item'Length exceeds Target'Length, Constraint_Error is propagated.

Type Numeric corresponds to COBOL's numeric data category with display usage.

The types Display_Format, Binary_Format, and Packed_Format are used in conversions between Ada decimal type values and COBOL internal or external data representations. The value of the constant Native_Binary is either High_Order_First or Low_Order_First, depending on the implementation. 

```ada
function Valid (Item   : in Numeric;
                Format : in Display_Format) return Boolean;

```

The function Valid checks that the Item parameter has a value consistent with the value of Format. If the value of Format is other than Unsigned, Leading_Separate, and Trailing_Separate, the effect is implementation defined. If Format does have one of these values, the following rules apply: 

Format=Unsigned: if Item comprises zero or more leading space characters followed by one or more decimal digit characters then Valid returns True, else it returns False.

Format=Leading_Separate: if Item comprises zero or more leading space characters, followed by a single occurrence of the plus or minus sign character, and then one or more decimal digit characters, then Valid returns True, else it returns False.

Format=Trailing_Separate: if Item comprises zero or more leading space characters, followed by one or more decimal digit characters and finally a plus or minus sign character, then Valid returns True, else it returns False. 

```ada
function Length (Format : in Display_Format) return Natural;

```

The Length function returns the minimal length of a Numeric value sufficient to hold any value of type Num when represented as Format.

```ada
function To_Decimal (Item   : in Numeric;
                     Format : in Display_Format) return Num;

```

Produces a value of type Num corresponding to Item as represented by Format. The number of digits after the assumed radix point in Item is Num'Scale. Conversion_Error is propagated if the value represented by Item is outside the range of Num. 

Discussion: There is no issue of truncation versus rounding, since the number of decimal places is established by Num'Scale.

```ada
function To_Display (Item   : in Num;
                     Format : in Display_Format) return Numeric;

```

This function returns the Numeric value for Item, represented in accordance with Format. Conversion_Error is propagated if Num is negative and Format is Unsigned.

```ada
function Valid (Item   : in Packed_Decimal;
                Format : in Packed_Format) return Boolean;

```

This function returns True if Item has a value consistent with Format, and False otherwise. The rules for the formation of Packed_Decimal values are implementation defined.

```ada
function Length (Format : in Packed_Format) return Natural;

```

This function returns the minimal length of a Packed_Decimal value sufficient to hold any value of type Num when represented as Format.

```ada
function To_Decimal (Item   : in Packed_Decimal;
                     Format : in Packed_Format) return Num;

```

Produces a value of type Num corresponding to Item as represented by Format. Num'Scale is the number of digits after the assumed radix point in Item. Conversion_Error is propagated if the value represented by Item is outside the range of Num.

```ada
function To_Packed (Item   : in Num;
                    Format : in Packed_Format) return Packed_Decimal;

```

This function returns the Packed_Decimal value for Item, represented in accordance with Format. Conversion_Error is propagated if Num is negative and Format is Packed_Unsigned.

```ada
function Valid (Item   : in Byte_Array;
                Format : in Binary_Format) return Boolean;

```

This function returns True if Item has a value consistent with Format, and False otherwise. 

Ramification: This function returns False only when the represented value is outside the range of Num.

```ada
function Length (Format : in Binary_Format) return Natural;

```

This function returns the minimal length of a Byte_Array value sufficient to hold any value of type Num when represented as Format.

```ada
function To_Decimal (Item   : in Byte_Array;
                     Format : in Binary_Format) return Num;

```

Produces a value of type Num corresponding to Item as represented by Format. Num'Scale is the number of digits after the assumed radix point in Item. Conversion_Error is propagated if the value represented by Item is outside the range of Num.

```ada
function To_Binary (Item   : in Num;
                    Format : in Binary_Format) return Byte_Array;

```

This function returns the Byte_Array value for Item, represented in accordance with Format. 

```ada
function To_Decimal (Item : in Binary)      return Num;

function To_Decimal (Item : in Long_Binary) return Num;

```

These functions convert from COBOL binary format to a corresponding value of the decimal type Num. Conversion_Error is propagated if Item is too large for Num. 

Ramification: There is no rescaling performed on the conversion. That is, the returned value in each case is a "bit copy" if Num has a binary radix. The programmer is responsible for maintaining the correct scale. 

```ada
function To_Binary      (Item : in Num)  return Binary;

function To_Long_Binary (Item : in Num)  return Long_Binary;

```

These functions convert from Ada decimal to COBOL binary format. Conversion_Error is propagated if the value of Item is too large to be represented in the result type. 

Discussion: One style of interface supported for COBOL, similar to what is provided for C, is the ability to call and pass parameters to an existing COBOL program. Thus the interface package supplies types that can be used in an Ada program as parameters to subprograms whose bodies will be in COBOL. These types map to COBOL's alphanumeric and numeric data categories.

Several types are provided for support of alphanumeric data. Since COBOL's run-time character set is not necessarily the same as Ada's, Interfaces.COBOL declares an implementation-defined character type COBOL_Character, and mappings between Character and COBOL_Character. These mappings are visible variables (rather than, say, functions or constant arrays), since in the situation where COBOL_Character is EBCDIC, the flexibility of dynamically modifying the mappings is needed. Corresponding to COBOL's alphanumeric data is the string type Alphanumeric.

Numeric data may have either a "display" or "computational" representation in COBOL. On the Ada side, the data is of a decimal fixed point type. Passing an Ada decimal data item to a COBOL program requires conversion from the Ada decimal type to some type that reflects the representation expected on the COBOL side. 

Computational Representation

Floating point representation is modeled by Ada floating point types, Floating and Long_Floating. Conversion between these types and Ada decimal types is obtained directly, since the type name serves as a conversion function.

Binary representation is modeled by an Ada integer type, Binary, and possibly other types such as Long_Binary. Conversion between, say, Binary and a decimal type is through functions from an instantiation of the generic package Decimal_Conversions.

Packed decimal representation is modeled by the Ada array type Packed_Decimal. Conversion between packed decimal and a decimal type is through functions from an instantiation of the generic package Decimal_Conversions.

Display Representation

Display representation for numeric data is modeled by the array type Numeric. Conversion between display representation and a decimal type is through functions from an instantiation of the generic package Decimal_Conversions. A parameter to the conversion function indicates the desired interpretation of the data (e.g., signed leading separate, etc.) 

Pragma Convention(COBOL, T) may be applied to a record type T to direct the compiler to choose a COBOL-compatible representation for objects of the type.

The package Interfaces.COBOL allows the Ada programmer to deal with data from files (or databases) created by a COBOL program. For data that is alphanumeric, or in display or packed decimal format, the approach is the same as for passing parameters (instantiate Decimal_Conversions to obtain the needed conversion functions). For binary data, the external representation is treated as a Byte array, and an instantiation of Decimal_IO produces a package that declares the needed conversion functions. A parameter to the conversion function indicates the desired interpretation of the data (e.g., high- versus low-order byte first). 


#### Implementation Requirements

An implementation shall support pragma Convention with a COBOL convention_[identifier](S0002) for a COBOL-eligible type (see B.1). 

Ramification: An implementation supporting this package shall ensure that if the bounds of a Packed_Decimal, Alphanumeric, or Numeric variable are static, then the representation of the object comprises solely the array components (that is, there is no implicit run-time "descriptor" that is part of the object). 


#### Implementation Permissions

An implementation may provide additional constants of the private types Display_Format, Binary_Format, or Packed_Format. 

Reason: This is to allow exploitation of other external formats that may be available in the COBOL implementation.

An implementation may provide further floating point and integer types in Interfaces.COBOL to match additional native COBOL types, and may also supply corresponding conversion functions in the generic package Decimal_Conversions. 


#### Implementation Advice

An Ada implementation should support the following interface correspondences between Ada and COBOL. 

An Ada access T parameter is passed as a "BY REFERENCE" data item of the COBOL type corresponding to T.

An Ada in scalar parameter is passed as a "BY CONTENT" data item of the corresponding COBOL type.

Any other Ada parameter is passed as a "BY REFERENCE" data item of the COBOL type corresponding to the Ada parameter type; for scalars, a local copy is used if necessary to ensure by-copy semantics. 

NOTE 1   An implementation is not required to support pragma Convention for access types, nor is it required to support pragma Import, Export or Convention for functions. 

Reason: COBOL does not have a pointer facility, and a COBOL program does not return a value. 

NOTE 2   If an Ada subprogram is exported to COBOL, then a call from COBOL call may specify either "BY CONTENT" or "BY REFERENCE". 


#### Examples

Examples of Interfaces.COBOL: 

```ada
with Interfaces.COBOL;
procedure Test_Call is

```

```ada
   -- Calling a foreign COBOL program
   -- Assume that a COBOL program PROG has the following declaration
   --  in its LINKAGE section:
   --  01 Parameter-Area
   --     05 NAME   PIC X(20).
   --     05 SSN    PIC X(9).
   --     05 SALARY PIC 99999V99 USAGE COMP.
   -- The effect of PROG is to update SALARY based on some algorithm

```

```ada
   package COBOL renames Interfaces.COBOL;

```

```ada
   type Salary_Type is delta 0.01 digits 7;

```

```ada
   type COBOL_Record is
      record
         Name   : COBOL.Numeric(1..20);
         SSN    : COBOL.Numeric(1..9);
         Salary : COBOL.Binary;  -- Assume Binary = 32 bits
      end record;
   pragma Convention (COBOL, COBOL_Record);

```

```ada
   procedure Prog (Item : in out COBOL_Record);
   pragma Import (COBOL, Prog, "PROG");

```

```ada
   package Salary_Conversions is
      new COBOL.Decimal_Conversions(Salary_Type);

```

```ada
   Some_Salary : Salary_Type := 12_345.67;
   Some_Record : COBOL_Record :=
      (Name   =&gt "Johnson, John       ",
       SSN    =&gt "111223333",
       Salary =&gt Salary_Conversions.To_Binary(Some_Salary));

```

```ada
begin
   Prog (Some_Record);
   ...
end Test_Call;

```

```ada
with Interfaces.COBOL;
with COBOL_Sequential_IO; -- Assumed to be supplied by implementation
procedure Test_External_Formats is

```

```ada
   -- Using data created by a COBOL program
   -- Assume that a COBOL program has created a sequential file with
   --  the following record structure, and that we need to
   --  process the records in an Ada program
   --  01 EMPLOYEE-RECORD
   --     05 NAME    PIC X(20).
   --     05 SSN     PIC X(9).
   --     05 SALARY  PIC 99999V99 USAGE COMP.
   --     05 ADJUST  PIC S999V999 SIGN LEADING SEPARATE.
   -- The COMP data is binary (32 bits), high-order byte first

```

```ada
   package COBOL renames Interfaces.COBOL;

```

```ada
   type Salary_Type      is delta 0.01  digits 7;
   type Adjustments_Type is delta 0.001 digits 6;

```

```ada
   type COBOL_Employee_Record_Type is  -- External representation
      record
         Name    : COBOL.Alphanumeric(1..20);
         SSN     : COBOL.Alphanumeric(1..9);
         Salary  : COBOL.Byte_Array(1..4);
         Adjust  : COBOL.Numeric(1..7);  -- Sign and 6 digits
      end record;
   pragma Convention (COBOL, COBOL_Employee_Record_Type);

```

```ada
   package COBOL_Employee_IO is
      new COBOL_Sequential_IO(COBOL_Employee_Record_Type);
   use COBOL_Employee_IO;

```

```ada
   COBOL_File : File_Type;

```

```ada
   type Ada_Employee_Record_Type is  -- Internal representation
      record
         Name    : String(1..20);
         SSN     : String(1..9);
         Salary  : Salary_Type;
         Adjust  : Adjustments_Type;
      end record;

```

```ada
   COBOL_Record : COBOL_Employee_Record_Type;
   Ada_Record   : Ada_Employee_Record_Type;

```

```ada
   package Salary_Conversions is
      new COBOL.Decimal_Conversions(Salary_Type);
   use Salary_Conversions;

```

```ada
   package Adjustments_Conversions is
      new COBOL.Decimal_Conversions(Adjustments_Type);
   use Adjustments_Conversions;

```

```ada
begin
   Open (COBOL_File, Name =&gt "Some_File");

```

```ada
   loop
     Read (COBOL_File, COBOL_Record);

```

```ada
     Ada_Record.Name := To_Ada(COBOL_Record.Name);
     Ada_Record.SSN  := To_Ada(COBOL_Record.SSN);
     Ada_Record.Salary :=
        To_Decimal(COBOL_Record.Salary, COBOL.High_Order_First);
     Ada_Record.Adjust :=
        To_Decimal(COBOL_Record.Adjust, COBOL.Leading_Separate);
     ... -- Process Ada_Record
   end loop;
exception
   when End_Error =&gt ...
end Test_External_Formats;

```


## B.5  Interfacing with Fortran

The facilities relevant to interfacing with the Fortran language are the package Interfaces.Fortran and support for the Import, Export and Convention pragmas with convention_[identifier](S0002) Fortran.

The package Interfaces.Fortran defines Ada types whose representations are identical to the default representations of the Fortran intrinsic types Integer, Real, Double Precision, Complex, Logical, and Character in a supported Fortran implementation. These Ada types can therefore be used to pass objects between Ada and Fortran programs. 


#### Static Semantics

The library package Interfaces.Fortran has the following declaration: 

```ada
with Ada.Numerics.Generic_Complex_Types;  -- see G.1.1
pragma Elaborate_All(Ada.Numerics.Generic_Complex_Types);
package Interfaces.Fortran is
    pragma Pure(Fortran);

```

```ada
   type Fortran_Integer is range implementation-defined;

```

```ada
   type Real             is digits implementation-defined;
   type Double_Precision is digits implementation-defined;

```

```ada
   type Logical is new Boolean;

```

```ada
   package Single_Precision_Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (Real);

```

```ada
   type Complex is new Single_Precision_Complex_Types.Complex;

```

```ada
   subtype Imaginary is Single_Precision_Complex_Types.Imaginary;
   i : Imaginary renames Single_Precision_Complex_Types.i;
   j : Imaginary renames Single_Precision_Complex_Types.j;

```

```ada
   type Character_Set is implementation-defined character type;

```

```ada
   type Fortran_Character is array (Positive range &lt&gt) of Character_Set;
   pragma Pack (Fortran_Character);

```

```ada
   function To_Fortran (Item : in Character) return Character_Set;
   function To_Ada (Item : in Character_Set) return Character;

```

```ada
   function To_Fortran (Item : in String) return Fortran_Character;
   function To_Ada     (Item : in Fortran_Character) return String;

```

```ada
   procedure To_Fortran (Item       : in String;
                         Target     : out Fortran_Character;
                         Last       : out Natural);

```

```ada
   procedure To_Ada (Item     : in Fortran_Character;
                     Target   : out String;
                     Last     : out Natural);

```

```ada
end Interfaces.Fortran;

```

Ramification: The means by which the Complex type is provided in Interfaces.Fortran creates a dependence of Interfaces.Fortran on Numerics.Generic_Complex_Types (see G.1.1). This dependence is intentional and unavoidable, if the Fortran-compatible Complex type is to be useful in Ada code without duplicating facilities defined elsewhere. 

The types Fortran_Integer, Real, Double_Precision, Logical, Complex, and Fortran_Character are Fortran-compatible.

The To_Fortran and To_Ada functions map between the Ada type Character and the Fortran type Character_Set, and also between the Ada type String and the Fortran type Fortran_Character. The To_Fortran and To_Ada procedures have analogous effects to the string conversion subprograms found in Interfaces.COBOL. 


#### Implementation Requirements

An implementation shall support [pragma](S0016) Convention with a Fortran convention_[identifier](S0002) for a Fortran-eligible type (see B.1). 


#### Implementation Permissions

An implementation may add additional declarations to the Fortran interface packages. For example, the Fortran interface package for an implementation of Fortran 77 (ANSI X3.9-1978) that defines types like Integer*n, Real*n, Logical*n, and Complex*n may contain the declarations of types named Integer_Star_n, Real_Star_n, Logical_Star_n, and Complex_Star_n. (This convention should not apply to Character*n, for which the Ada analog is the constrained array subtype Fortran_Character (1..n).) Similarly, the Fortran interface package for an implementation of Fortran 90 that provides multiple kinds of intrinsic types, e.g. Integer (Kind=n), Real (Kind=n), Logical (Kind=n), Complex (Kind=n), and Character (Kind=n), may contain the declarations of types with the recommended names Integer_Kind_n, Real_Kind_n, Logical_Kind_n, Complex_Kind_n, and Character_Kind_n. 

Discussion: Implementations may add auxiliary declarations as needed to assist in the declarations of additional Fortran-compatible types. For example, if a double precision complex type is defined, then Numerics.Generic_Complex_Types may be instantiated for the double precision type. Similarly, if a wide character type is defined to match a Fortran 90 wide character type (accessible in Fortran 90 with the Kind modifier), then an auxiliary character set may be declared to serve as its component type. 


#### Implementation Advice

An Ada implementation should support the following interface correspondences between Ada and Fortran: 

An Ada procedure corresponds to a Fortran subroutine.

An Ada function corresponds to a Fortran function.

An Ada parameter of an elementary, array, or record type T is passed as a TF argument to a Fortran procedure, where TF is the Fortran type corresponding to the Ada type T, and where the INTENT attribute of the corresponding dummy argument matches the Ada formal parameter mode; the Fortran implementation's parameter passing conventions are used. For elementary types, a local copy is used if necessary to ensure by-copy semantics.

An Ada parameter of an access-to-subprogram type is passed as a reference to a Fortran procedure whose interface corresponds to the designated subprogram's specification. 

NOTE 1   An object of a Fortran-compatible record type, declared in a library package or subprogram, can correspond to a Fortran common block; the type also corresponds to a Fortran "derived type".


#### Examples

Example of Interfaces.Fortran: 

```ada
with Interfaces.Fortran;
use Interfaces.Fortran;
procedure Ada_Application is

```

```ada
   type Fortran_Matrix is 
      array (Integer range &lt&gt,
             Integer range &lt&gt) of Double_Precision;
   pragma Convention (Fortran, Fortran_Matrix);    -- stored in Fortran's
                                                   -- column-major order
   procedure Invert (Rank : in Fortran_Integer; X : in out Fortran_Matrix);
   pragma Import (Fortran, Invert);               -- a Fortran subroutine

```

```ada
   Rank      : constant Fortran_Integer := 100;
   My_Matrix : Fortran_Matrix (1 .. Rank, 1 .. Rank);

```

```ada
begin

```

```ada
   ...
   My_Matrix := ...;
   ...
   Invert (Rank, My_Matrix);
   ...

```

```ada
end Ada_Application;

```

