---
sidebar_position:  142
---

# B.5  Interfacing with Fortran

{AI05-0229-1} The facilities relevant to interfacing with the Fortran language are the package Interfaces.Fortran and support for specifying the Convention aspect with convention_[identifier](./AA-2.3#S0002) Fortran.

The package Interfaces.Fortran defines Ada types whose representations are identical to the default representations of the Fortran intrinsic types Integer, Real, Double Precision, Complex, Logical, and Character in a supported Fortran implementation. These Ada types can therefore be used to pass objects between Ada and Fortran programs. 


#### Static Semantics

The library package Interfaces.Fortran has the following declaration: 

```ada
{AI12-0414-1} with Ada.Numerics.Generic_Complex_Types;  -- see G.1.1
pragma Elaborate_All(Ada.Numerics.Generic_Complex_Types);
package Interfaces.Fortran
   with Pure is

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
{AI12-0058-1}    package Double_Precision_Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (Double_Precision);

```

```ada
{AI12-0058-1}    type Double_Complex is new Double_Precision_Complex_Types.Complex;

```

```ada
{AI12-0058-1}    subtype Double_Imaginary is Double_Precision_Complex_Types.Imaginary;

```

```ada
   type Character_Set is implementation-defined character type;

```

```ada
{AI05-0229-1}    type Fortran_Character is array (Positive range &lt&gt) of Character_Set
      with Pack;

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

Implementation defined: The types Fortran_Integer, Real, Double_Precision, and Character_Set in Interfaces.Fortran.

Ramification: {AI12-0058-1} The means by which the Complex and Double_Complex types are provided in Interfaces.Fortran creates a dependence of Interfaces.Fortran on Numerics.Generic_Complex_Types (see G.1.1). This dependence is intentional and unavoidable, if the Fortran-compatible Complex and Double_Complex types are to be useful in Ada code without duplicating facilities defined elsewhere. 

{AI12-0058-1} The types Fortran_Integer, Real, Double_Precision, Logical, Complex, Double_Complex, Character_Set, and Fortran_Character are Fortran-compatible.

The To_Fortran and To_Ada functions map between the Ada type Character and the Fortran type Character_Set, and also between the Ada type String and the Fortran type Fortran_Character. The To_Fortran and To_Ada procedures have analogous effects to the string conversion subprograms found in Interfaces.COBOL. 


#### Implementation Requirements

{AI05-0229-1} An implementation shall support specifying aspect Convention with a Fortran convention_[identifier](./AA-2.3#S0002) for a Fortran-eligible type (see B.1). 


#### Implementation Permissions

{AI12-0058-1} {AI12-0263-1} An implementation may add additional declarations to the Fortran interface packages. For example, declarations are permitted for the character types corresponding to Fortran character kinds 'ascii' and 'iso_10646', which in turn correspond to ISO/IEC 646:1991 and to UCS-4 as specified in ISO/IEC 10646:2017. 

Reason: {AI12-0058-1} Fortran compilers are required to recognize 'ascii' and 'iso_10646' as arguments to the SELECTED_CHAR_KIND intrinsic function, but are not required to support those kinds. 

Discussion: {AI12-0058-1} Implementations may add auxiliary declarations as needed to assist in the declarations of additional Fortran-compatible types. For example,  if a wide character type is defined to match a Fortran 90 wide character type (accessible in Fortran 90 with the Kind attribute), then an auxiliary character set may be declared to serve as its component type. 


#### Implementation Advice

An Ada implementation should support the following interface correspondences between Ada and Fortran: 

An Ada procedure corresponds to a Fortran subroutine.

An Ada function corresponds to a Fortran function.

An Ada parameter of an elementary, array, or record type T is passed as a TF argument to a Fortran procedure, where TF is the Fortran type corresponding to the Ada type T, and where the INTENT attribute of the corresponding dummy argument matches the Ada formal parameter mode; the Fortran implementation's parameter passing conventions are used. For elementary types, a local copy is used if necessary to ensure by-copy semantics.

An Ada parameter of an access-to-subprogram type is passed as a reference to a Fortran procedure whose interface corresponds to the designated subprogram's specification. 

Implementation Advice: If Fortran interfacing is supported, the interface correspondences between Ada and Fortran should be supported.

NOTE 1   An object of a Fortran-compatible record type, declared in a library package or subprogram, can correspond to a Fortran common block; the type also corresponds to a Fortran "derived type".

NOTE 2   {AI12-0224-1} For Fortran facilities not addressed by this subclause, consider using the Fortran to C interoperability features defined in ISO/IEC 1594-1:2018 along with the C interfacing features defined in B.3.


#### Examples

Example of Interfaces.Fortran: 

```ada
with Interfaces.Fortran;
use Interfaces.Fortran;
procedure Ada_Application is

```

```ada
{AI05-0229-1} {AI12-0178-1}    type Fortran_Matrix is 
      array (Fortran_Integer range &lt&gt,
             Fortran_Integer range &lt&gt) of Double_Precision
      with Convention =&gt Fortran;                  -- stored in Fortran's
                                                   -- column-major order
   procedure Invert (Rank : in Fortran_Integer; X : in out Fortran_Matrix)
      with Import =&gt True, Convention =&gt Fortran; -- a Fortran subroutine

```

```ada
   Rank      : constant Fortran_Integer := 100;
   My_Matrix : Fortran_Matrix (1 .. Rank, 1 .. Rank);

```

```ada
{AI12-0058-1}    Precision: constant := 6;
   type Standard_Deviation is digits Precision
      with Convention =&gt Fortran;
   Deviation : Standard_Deviation;
      -- Declarations to match the following Fortran declarations:
      --   integer, parameter :: precision = selected_real_kind(p=6)
      --   real(precision) :: deviation

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
{AI12-0058-1}    Deviation := ...;
   ...

```

```ada
end Ada_Application;

```


#### Wording Changes from Ada 2012

{AI12-0058-1} Correction: The package Double_Precision_Complex_Types and associated types are added to package Interfaces.Fortran. In unusual circumstances, this could cause an incompatibility; we don't document it as an incompatibility as implementations are allowed to add declarations to this package, so that risk of an incompatibility is present for any move from one version of an implementation to another (not to mention to another implementation). As such, the language-defined additions make no change in the risk of incompatibility. 

