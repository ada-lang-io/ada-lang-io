---
sidebar_position:  139
---

# B.2  The Package Interfaces

Package Interfaces is the parent of several library packages that declare types and other entities useful for interfacing to foreign languages. It also contains some implementation-defined types that are useful across more than one language (in particular for interfacing to assembly language). 

Implementation defined: The contents of the visible part of package Interfaces and its language-defined descendants.


#### Static Semantics

The library package Interfaces has the following skeletal declaration: 

```ada
{AI12-0414-1} package Interfaces
   with Pure is

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

To be honest: {AI12-0264-1} "Shifting" and "rotating" have the conventional meaning. Neither of these terms is usefully defined by the usual normative references of the Reference Manual, so we provide pseudo-code here to describe the intended semantics of the above wording (all operations in these examples are using modular semantics).

```ada
function Rotate_Left (Value : Unsigned_n; Amount : Natural)
   return Unsigned_n is
   Result : Unsigned_n := Value;
   Bit : Unsigned_n range 0 .. 1;
begin
   for Count in 1 .. Amount loop
      Bit := Result/2**(n-1); -- High-bit of Result
      Result := Result*2 + Bit;
   end loop;
   return Result;
end Rotate_Left;

```

```ada
function Rotate_Right (Value : Unsigned_n; Amount : Natural)
   return Unsigned_n is
   Result : Unsigned_n := Value;
   Bit : Unsigned_n range 0 .. 1;
begin
   for Count in 1 .. Amount loop
      Bit := Result mod 2; -- Low-bit of Result
      Result := Result/2 + (Bit * 2**(n-1));
   end loop;
   return Result;
end Rotate_Right;

```

```ada
function Shift_Left (Value : Unsigned_n; Amount : Natural)
   return Unsigned_n is
   Result : Unsigned_n := Value;
begin
   for Count in 1 .. Amount loop
      Result := Result * 2;
   end loop;
   return Result;
end Shift_Left;

```

```ada
function Shift_Right (Value : Unsigned_n; Amount : Natural)
   return Unsigned_n is
   Result : Unsigned_n := Value;
begin
   for Count in 1 .. Amount loop
      Result := Result / 2;
   end loop;
   return Result;
end Shift_Right;

```

```ada
function Shift_Right_Arithmetic (Value : Unsigned_n; Amount : Natural)
   return Unsigned_n is
   Result : Unsigned_n := Value;
   Neg : constant Boolean :=
      Result/2**(n-1) = 1; -- High-bit of Result
begin
   for Count in 1 .. Amount loop
      if Neg then
         Result := Result / 2 + 2**(n-1);
      else
         Result := Result / 2;
      end if;
   end loop;
   return Result;
end Shift_Right_Arithmetic;

```

{AI12-0264-1} These generally correspond to machine instructions, although there may not be an exact match in terms of boundary conditions, as Ada requires the correct result to be produced for all values of Amount. 

Floating point types corresponding to each floating point format fully supported by the hardware. 

Implementation Note: The names for these floating point types are not specified. However, if IEEE arithmetic is supported, then the names should be IEEE_Float_32 and IEEE_Float_64 for single and double precision, respectively.


#### Implementation Permissions

An implementation may provide implementation-defined library units that are children of Interfaces, and may add declarations to the visible part of Interfaces in addition to the ones defined above. 

Implementation defined: Implementation-defined children of package Interfaces.

{AI95-00204-01} {AI05-0229-1} A child package of package Interfaces with the name of a convention may be provided independently of whether the convention is supported by the Convention aspect and vice versa. Such a child package should contain any declarations that would be useful for interfacing to the language (implementation) represented by the convention. Any declarations useful for interfacing to any language on the given hardware architecture should be provided directly in Interfaces. 

Ramification: For example, package Interfaces.XYZ_Pascal might contain declarations of types that match the data types provided by the XYZ implementation of Pascal, so that it will be more convenient to pass parameters to a subprogram whose convention is XYZ_Pascal. 


#### Implementation Advice

This paragraph was deleted.{AI95-00204-01} 

This paragraph was deleted.

{AI05-0299-1} An implementation supporting an interface to C, COBOL, or Fortran should provide the corresponding package or packages described in the following subclauses. 

Implementation Advice: If an interface to C, COBOL, or Fortran is provided, the corresponding package or packages described in Annex B, "Interface to Other Languages" should also be provided.

Implementation Note: The intention is that an implementation might support several implementations of the foreign language: Interfaces.This_Fortran and Interfaces.That_Fortran might both exist. The "default" implementation, overridable by the user, should be declared as a renaming: 

```ada
package Interfaces.Fortran renames Interfaces.This_Fortran;

```


#### Wording Changes from Ada 95

{AI95-00204-01} Clarified that interfacing to foreign languages is optional and has the same restrictions as a Specialized Needs Annex. 


#### Wording Changes from Ada 2005

{AI05-0262-1} Move the restrictions on implementations of optional features to the start of this Annex. 

