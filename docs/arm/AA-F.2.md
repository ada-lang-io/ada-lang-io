---
sidebar_position:  176
---

# F.2  The Package Decimal


#### Static Semantics

The library package Decimal has the following declaration: 

```ada
{AI12-0414-1} package Ada.Decimal
   with Pure is

```

```ada
   Max_Scale : constant := implementation-defined;
   Min_Scale : constant := implementation-defined;

```

```ada
   Min_Delta : constant := 10.0**(-Max_Scale);
   Max_Delta : constant := 10.0**(-Min_Scale);

```

```ada
   Max_Decimal_Digits : constant := implementation-defined;

```

```ada
{AI05-0229-1}    generic
      type Dividend_Type  is delta &lt&gt digits &lt&gt;
      type Divisor_Type   is delta &lt&gt digits &lt&gt;
      type Quotient_Type  is delta &lt&gt digits &lt&gt;
      type Remainder_Type is delta &lt&gt digits &lt&gt;
   procedure Divide (Dividend  : in Dividend_Type;
                     Divisor   : in Divisor_Type;
                     Quotient  : out Quotient_Type;
                     Remainder : out Remainder_Type)
      with Convention =&gt Intrinsic;

```

```ada
end Ada.Decimal;

```

Implementation defined: The values of named numbers in the package Decimal.

Max_Scale is the largest N such that 10.0**(N) is allowed as a decimal type's delta. Its type is universal_integer.

Min_Scale is the smallest N such that 10.0**(N) is allowed as a decimal type's delta. Its type is universal_integer.

Min_Delta is the smallest value allowed for delta in a [decimal_fixed_point_definition](./AA-3.5#S0049). Its type is universal_real.

Max_Delta is the largest value allowed for delta in a [decimal_fixed_point_definition](./AA-3.5#S0049). Its type is universal_real.

Max_Decimal_Digits is the largest value allowed for digits in a [decimal_fixed_point_definition](./AA-3.5#S0049). Its type is universal_integer. 

Reason: The name is Max_Decimal_Digits versus Max_Digits, in order to avoid confusion with the named number System.Max_Digits relevant to floating point. 


#### Static Semantics

The effect of Divide is as follows. The value of Quotient is Quotient_Type(Dividend/Divisor). The value of Remainder is Remainder_Type(Intermediate), where Intermediate is the difference between Dividend and the product of Divisor and Quotient; this result is computed exactly. 


#### Implementation Requirements

Decimal.Max_Decimal_Digits shall be at least 18.

Decimal.Max_Scale shall be at least 18.

Decimal.Min_Scale shall be at most 0. 

NOTE 1   The effect of division yielding a quotient with control over rounding versus truncation is obtained by applying either the function attribute Quotient_Type'Round or the conversion Quotient_Type to the expression Dividend/Divisor. 

