---
sidebar_position:  141
---

# B.4  Interfacing with COBOL

{AI05-0229-1} The facilities relevant to interfacing with the COBOL language are the package Interfaces.COBOL and support for specifying the Convention aspect with convention_[identifier](./AA-2.3#S0002) COBOL.

The COBOL interface package supplies several sets of facilities: 

A set of types corresponding to the native COBOL types of the supported COBOL implementation (so-called "internal COBOL representations"), allowing Ada data to be passed as parameters to COBOL programs

{AI12-0439-1} A set of types and constants reflecting external data representations such as can be found in files or databases, allowing COBOL-generated data to be read by an Ada program, and Ada-generated data to be read by COBOL programs

A generic package for converting between an Ada decimal type value and either an internal or external COBOL representation 


#### Static Semantics

The library package Interfaces.COBOL has the following declaration: 

```ada
{AI12-0241-1} {AI12-0302-1} package Interfaces.COBOL 
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

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
{AI05-0229-1}    type Decimal_Element  is mod implementation-defined;
   type Packed_Decimal is array (Positive range &lt&gt) of Decimal_Element
      with Pack;

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
{AI05-0229-1}    type Alphanumeric is array (Positive range &lt&gt) of COBOL_Character
      with Pack;

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
{AI05-0229-1}    type Numeric is array (Positive range &lt&gt) of COBOL_Character
      with Pack;

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
{AI05-0229-1}    type Byte is mod 2**COBOL_Character'Size;
   type Byte_Array is array (Positive range &lt&gt) of Byte
      with Pack;

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

Implementation defined: The types Floating, Long_Floating, Binary, Long_Binary, Decimal_Element, and COBOL_Character; and the initializations of the variables Ada_To_COBOL and COBOL_To_Ada, in Interfaces.COBOL.

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

{8652/0066} {AI95-00071-01} {AI05-0264-1} Format=Unsigned: if Item comprises one or more decimal digit characters, then Valid returns True, else it returns False.

{8652/0066} {AI95-00071-01} Format=Leading_Separate: if Item comprises a single occurrence of the plus or minus sign character, and then one or more decimal digit characters, then Valid returns True, else it returns False.

{8652/0066} {AI95-00071-01} Format=Trailing_Separate: if Item comprises one or more decimal digit characters and finally a plus or minus sign character, then Valid returns True, else it returns False. 

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

{8652/0067} {AI95-00072-01} This function returns the Numeric value for Item, represented in accordance with Format. The length of the returned value is Length(Format), and the lower bound is 1. Conversion_Error is propagated if Num is negative and Format is Unsigned.

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

{8652/0067} {AI95-00072-01} This function returns the Packed_Decimal value for Item, represented in accordance with Format. The length of the returned value is Length(Format), and the lower bound is 1. Conversion_Error is propagated if Num is negative and Format is Packed_Unsigned.

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

{8652/0067} {AI95-00072-01} This function returns the Byte_Array value for Item, represented in accordance with Format. The length of the returned value is Length(Format), and the lower bound is 1.

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

{AI05-0229-1} The Convention of a record type may be specified as COBOL to direct the compiler to choose a COBOL-compatible representation for objects of the type.

The package Interfaces.COBOL allows the Ada programmer to deal with data from files (or databases) created by a COBOL program. For data that is alphanumeric, or in display or packed decimal format, the approach is the same as for passing parameters (instantiate Decimal_Conversions to obtain the needed conversion functions). For binary data, the external representation is treated as a Byte array, and an instantiation of Decimal_IO produces a package that declares the needed conversion functions. A parameter to the conversion function indicates the desired interpretation of the data (e.g., high- versus low-order byte first). 


#### Implementation Requirements

{AI05-0229-1} An implementation shall support specifying aspect Convention with a COBOL convention_[identifier](./AA-2.3#S0002) for a COBOL-eligible type (see B.1). 

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

Implementation Advice: If COBOL interfacing is supported, the interface correspondences between Ada and COBOL should be supported.

NOTE 1   {AI05-0229-1} An implementation is not required to support specifying aspect Convention for access types, nor is it required to support specifying aspects Import, Export, or Convention for functions. 

Reason: COBOL does not have a pointer facility, and a COBOL program does not return a value. 

NOTE 2   {AI12-0440-1} If an Ada subprogram is exported to COBOL, then a call from COBOL call can specify either "BY CONTENT" or "BY REFERENCE". 


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
{AI05-0229-1}    type COBOL_Record is
      record
         Name   : COBOL.Numeric(1..20);
         SSN    : COBOL.Numeric(1..9);
         Salary : COBOL.Binary;  -- Assume Binary = 32 bits
      end record
      with Convention =&gt COBOL;

```

```ada
{AI05-0229-1}    procedure Prog (Item : in out COBOL_Record)
      with Import =&gt True, Convention =&gt COBOL;

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
{AI12-0442-1}    -- Using data created by a COBOL program
   -- Assume that a COBOL program has created a sequential file with
   --  the following record structure, and that we want
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
{AI05-0229-1}    type COBOL_Employee_Record_Type is  -- External representation
      record
         Name    : COBOL.Alphanumeric(1..20);
         SSN     : COBOL.Alphanumeric(1..9);
         Salary  : COBOL.Byte_Array(1..4);
         Adjust  : COBOL.Numeric(1..7);  -- Sign and 6 digits
      end record
      with Convention =&gt COBOL;

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
{AI12-0178-1}      Ada_Record.Name := COBOL.To_Ada(COBOL_Record.Name);
     Ada_Record.SSN  := COBOL.To_Ada(COBOL_Record.SSN);
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


#### Wording Changes from Ada 95

{8652/0066} {AI95-00071-01} Corrigendum: Corrected the definition of Valid to match COBOL.

{8652/0067} {AI95-00072-01} Corrigendum: Specified the bounds of the results of To_Display, To_Packed, and To_Binary. 

