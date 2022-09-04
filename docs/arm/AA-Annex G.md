---
sidebar_position:  21
---

# Annex G Numerics

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
The Numerics Annex specifies 

features for complex arithmetic, including complex I/O;

a mode ("strict mode"), in which the predefined arithmetic operations of floating point and fixed point types and the functions and operations of various predefined packages have to provide guaranteed accuracy or conform to other numeric performance requirements, which the Numerics Annex also specifies;

a mode ("relaxed mode"), in which no accuracy or other numeric performance requirements need be satisfied, as for implementations not conforming to the Numerics Annex;

models of floating point and fixed point arithmetic on which the accuracy requirements of strict mode are based; and

the definitions of the model-oriented attributes of floating point types that apply in the strict mode.


#### Implementation Advice

If Fortran (respectively, C) is widely supported in the target environment, implementations supporting the Numerics Annex should provide the child package Interfaces.Fortran (respectively, Interfaces.C) specified in Annex B and should support a convention_[identifier](S0002) of Fortran (respectively, C) in the interfacing pragmas (see Annex B), thus allowing Ada programs to interface with programs written in that language. 


#### Extensions to Ada 83

This Annex is new to Ada 95. 


## G.1  Complex Arithmetic

Types and arithmetic operations for complex arithmetic are provided in Generic_Complex_Types, which is defined in G.1.1. Implementation-defined approximations to the complex analogs of the mathematical functions known as the "elementary functions" are provided by the subprograms in Generic_Complex_Elementary_Functions, which is defined in G.1.2. Both of these library units are generic children of the predefined package Numerics (see A.5). Nongeneric equivalents of these generic packages for each of the predefined floating point types are also provided as children of Numerics. 

Implementation defined: The accuracy actually achieved by the complex elementary functions and by other complex arithmetic operations.

Discussion: Complex arithmetic is defined in the Numerics Annex, rather than in the core, because it is considered to be a specialized need of (some) numeric applications. 


### G.1.1  Complex Types


#### Static Semantics

The generic library package Numerics.Generic_Complex_Types has the following declaration: 

```ada
generic
   type Real is digits &lt&gt;
package Ada.Numerics.Generic_Complex_Types is
   pragma Pure(Generic_Complex_Types);

```

```ada
   type Complex is
      record
         Re, Im : Real'Base;
      end record;

```

```ada
   type Imaginary is private;

```

```ada
   i : constant Imaginary;
   j : constant Imaginary;

```

```ada
   function Re (X : Complex)   return Real'Base;
   function Im (X : Complex)   return Real'Base;
   function Im (X : Imaginary) return Real'Base;

```

```ada
   procedure Set_Re (X  : in out Complex;
                     Re : in     Real'Base);
   procedure Set_Im (X  : in out Complex;
                     Im : in     Real'Base);
   procedure Set_Im (X  :    out Imaginary;
                     Im : in     Real'Base);

```

```ada
   function Compose_From_Cartesian (Re, Im : Real'Base) return Complex;
   function Compose_From_Cartesian (Re     : Real'Base) return Complex;
   function Compose_From_Cartesian (Im     : Imaginary) return Complex;

```

```ada
   function Modulus (X     : Complex) return Real'Base;
   function "abs"   (Right : Complex) return Real'Base renames Modulus;

```

```ada
   function Argument (X     : Complex)   return Real'Base;
   function Argument (X     : Complex;
                      Cycle : Real'Base) return Real'Base;

```

```ada
   function Compose_From_Polar (Modulus, Argument        : Real'Base)
      return Complex;
   function Compose_From_Polar (Modulus, Argument, Cycle : Real'Base)
      return Complex;

```

```ada
   function "+"       (Right : Complex) return Complex;
   function "-"       (Right : Complex) return Complex;
   function Conjugate (X     : Complex) return Complex;

```

```ada
   function "+" (Left, Right : Complex) return Complex;
   function "-" (Left, Right : Complex) return Complex;
   function "*" (Left, Right : Complex) return Complex;
   function "/" (Left, Right : Complex) return Complex;

```

```ada
   function "**" (Left : Complex; Right : Integer) return Complex;

```

```ada
   function "+"       (Right : Imaginary) return Imaginary;
   function "-"       (Right : Imaginary) return Imaginary;
   function Conjugate (X     : Imaginary) return Imaginary renames "-";
   function "abs"     (Right : Imaginary) return Real'Base;

```

```ada
   function "+" (Left, Right : Imaginary) return Imaginary;
   function "-" (Left, Right : Imaginary) return Imaginary;
   function "*" (Left, Right : Imaginary) return Real'Base;
   function "/" (Left, Right : Imaginary) return Real'Base;

```

```ada
   function "**" (Left : Imaginary; Right : Integer) return Complex;

```

```ada
   function "&lt"  (Left, Right : Imaginary) return Boolean;
   function "&lt=" (Left, Right : Imaginary) return Boolean;
   function "&gt"  (Left, Right : Imaginary) return Boolean;
   function "&gt=" (Left, Right : Imaginary) return Boolean;

```

```ada
   function "+" (Left : Complex;   Right : Real'Base) return Complex;
   function "+" (Left : Real'Base; Right : Complex)   return Complex;
   function "-" (Left : Complex;   Right : Real'Base) return Complex;
   function "-" (Left : Real'Base; Right : Complex)   return Complex;
   function "*" (Left : Complex;   Right : Real'Base) return Complex;
   function "*" (Left : Real'Base; Right : Complex)   return Complex;
   function "/" (Left : Complex;   Right : Real'Base) return Complex;
   function "/" (Left : Real'Base; Right : Complex)   return Complex;

```

```ada
   function "+" (Left : Complex;   Right : Imaginary) return Complex;
   function "+" (Left : Imaginary; Right : Complex)   return Complex;
   function "-" (Left : Complex;   Right : Imaginary) return Complex;
   function "-" (Left : Imaginary; Right : Complex)   return Complex;
   function "*" (Left : Complex;   Right : Imaginary) return Complex;
   function "*" (Left : Imaginary; Right : Complex)   return Complex;
   function "/" (Left : Complex;   Right : Imaginary) return Complex;
   function "/" (Left : Imaginary; Right : Complex)   return Complex;

```

```ada
   function "+" (Left : Imaginary; Right : Real'Base) return Complex;
   function "+" (Left : Real'Base; Right : Imaginary) return Complex;
   function "-" (Left : Imaginary; Right : Real'Base) return Complex;
   function "-" (Left : Real'Base; Right : Imaginary) return Complex;
   function "*" (Left : Imaginary; Right : Real'Base) return Imaginary;
   function "*" (Left : Real'Base; Right : Imaginary) return Imaginary;
   function "/" (Left : Imaginary; Right : Real'Base) return Imaginary;
   function "/" (Left : Real'Base; Right : Imaginary) return Imaginary;

```

```ada
private

```

```ada
   type Imaginary is new Real'Base;
   i : constant Imaginary := 1.0;
   j : constant Imaginary := 1.0;

```

```ada
end Ada.Numerics.Generic_Complex_Types;

```

The library package Numerics.Complex_Types defines the same types, constants, and subprograms as Numerics.Generic_Complex_Types, except that the predefined type Float is systematically substituted for Real'Base throughout. Nongeneric equivalents of Numerics.Generic_Complex_Types for each of the other predefined floating point types are defined similarly, with the names Numerics.Short_Complex_Types, Numerics.Long_Complex_Types, etc. 

Reason: The nongeneric equivalents are provided to allow the programmer to construct simple mathematical applications without being required to understand and use generics. 

Reason: The nongeneric equivalents all export the types Complex and Imaginary and the constants i and j (rather than uniquely named types and constants, such as Short_Complex, Long_Complex, etc.) to preserve their equivalence to actual instantiations of the generic package and to allow the programmer to change the precision of an application globally by changing a single context clause. 

[Complex is a visible type with cartesian components.] 

Reason: The cartesian representation is far more common than the polar representation, in practice. The accuracy of the results of the complex arithmetic operations and of the complex elementary functions is dependent on the representation; thus, implementers need to know that representation. The type is visible so that complex "literals" can be written in aggregate notation, if desired. 

[Imaginary is a private type; its full type is derived from Real'Base.] 

Reason: The Imaginary type and the constants i and j are provided for two reasons: 

They allow complex "literals" to be written in the alternate form of a + b*i (or a + b*j), if desired. Of course, in some contexts the sum will need to be parenthesized.

When an Ada binding to IEC 559:1989 that provides (signed) infinities as the result of operations that overflow becomes available, it will be important to allow arithmetic between pure-imaginary and complex operands without requiring the former to be represented as (or promoted to) complex values with a real component of zero. For example, the multiplication of a + b*i by d*i should yield b� d + a� d*i, but if one cannot avoid representing the pure-imaginary value d*i as the complex value 0.0 + d*i, then a NaN ("Not-a-Number") could be produced as the result of multiplying a by 0.0 (e.g., when a is infinite); the NaN could later trigger an exception. Providing the Imaginary type and overloadings of the arithmetic operators for mixtures of Imaginary and Complex operands gives the programmer the same control over avoiding premature coercion of pure-imaginary values to complex as is already provided for pure-real values. 

Reason: The Imaginary type is private, rather than being visibly derived from Real'Base, for two reasons: 

to preclude implicit conversions of real literals to the Imaginary type (such implicit conversions would make many common arithmetic expressions ambiguous); and

to suppress the implicit derivation of the multiplication, division, and absolute value operators with Imaginary operands and an Imaginary result (the result type would be incorrect). 

Reason: The base subtype Real'Base is used for the component type of Complex, the parent type of Imaginary, and the parameter and result types of some of the subprograms to maximize the chances of being able to pass meaningful values into the subprograms and receive meaningful results back. The generic formal parameter Real therefore plays only one role, that of providing the precision to be maintained in complex arithmetic calculations. Thus, the subprograms in Numerics.Generic_Complex_Types share with those in Numerics.Generic_Elementary_Functions, and indeed even with the predefined arithmetic operations (see 4.5), the property of being free of range checks on input and output, i.e., of being able to exploit the base range of the relevant floating point type fully. As a result, the user loses the ability to impose application-oriented bounds on the range of values that the components of a complex variable can acquire; however, it can be argued that few, if any, applications have a naturally square domain (as opposed to a circular domain) anyway. 

The arithmetic operations and the Re, Im, Modulus, Argument, and Conjugate functions have their usual mathematical meanings. When applied to a parameter of pure-imaginary type, the "imaginary-part" function Im yields the value of its parameter, as the corresponding real value. The remaining subprograms have the following meanings: 

Reason: The middle case can be understood by considering the parameter of pure-imaginary type to represent a complex value with a zero real part. 

The Set_Re and Set_Im procedures replace the designated component of a complex parameter with the given real value; applied to a parameter of pure-imaginary type, the Set_Im procedure replaces the value of that parameter with the imaginary value corresponding to the given real value.

The Compose_From_Cartesian function constructs a complex value from the given real and imaginary components. If only one component is given, the other component is implicitly zero.

The Compose_From_Polar function constructs a complex value from the given modulus (radius) and argument (angle). When the value of the parameter Modulus is positive (resp., negative), the result is the complex value represented by the point in the complex plane lying at a distance from the origin given by the absolute value of Modulus and forming an angle measured counterclockwise from the positive (resp., negative) real axis given by the value of the parameter Argument. 

When the Cycle parameter is specified, the result of the Argument function and the parameter Argument of the Compose_From_Polar function are measured in units such that a full cycle of revolution has the given value; otherwise, they are measured in radians.

The computed results of the mathematically multivalued functions are rendered single-valued by the following conventions, which are meant to imply the principal branch: 

The result of the Modulus function is nonnegative.

The result of the Argument function is in the quadrant containing the point in the complex plane represented by the parameter X. This may be any quadrant (I through IV); thus, the range of the Argument function is approximately  to  (Cycle/2.0 to Cycle/2.0, if the parameter Cycle is specified). When the point represented by the parameter X lies on the negative real axis, the result approximates 

 (resp., ) when the sign of the imaginary component of X is positive (resp., negative), if Real'Signed_Zeros is True;

, if Real'Signed_Zeros is False. 

Because a result lying on or near one of the axes may not be exactly representable, the approximation inherent in computing the result may place it in an adjacent quadrant, close to but on the wrong side of the axis. 


#### Dynamic Semantics

The exception Numerics.Argument_Error is raised by the Argument and Compose_From_Polar functions with specified cycle, signaling a parameter value outside the domain of the corresponding mathematical function, when the value of the parameter Cycle is zero or negative.

The exception Constraint_Error is raised by the division operator when the value of the right operand is zero, and by the exponentiation operator when the value of the left operand is zero and the value of the exponent is negative, provided that Real'Machine_Overflows is True; when Real'Machine_Overflows is False, the result is unspecified. [Constraint_Error can also be raised when a finite result overflows (see G.2.6).] 

Discussion: It is anticipated that an Ada binding to IEC 559:1989 will be developed in the future. As part of such a binding, the Machine_Overflows attribute of a conformant floating point type will be specified to yield False, which will permit implementations of the complex arithmetic operations to deliver results with an infinite component (and set the overflow flag defined by the binding) instead of raising Constraint_Error in overflow situations, when traps are disabled. Similarly, it is appropriate for the complex arithmetic operations to deliver results with infinite components (and set the zero-divide flag defined by the binding) instead of raising Constraint_Error in the situations defined above, when traps are disabled. Finally, such a binding should also specify the behavior of the complex arithmetic operations, when sensible, given operands with infinite components. 


#### Implementation Requirements

In the implementation of Numerics.Generic_Complex_Types, the range of intermediate values allowed during the calculation of a final result shall not be affected by any range constraint of the subtype Real. 

Implementation Note: Implementations of Numerics.Generic_Complex_Types written in Ada should therefore avoid declaring local variables of subtype Real; the subtype Real'Base should be used instead. 

In the following cases, evaluation of a complex arithmetic operation shall yield the prescribed result, provided that the preceding rules do not call for an exception to be raised: 

The results of the Re, Im, and Compose_From_Cartesian functions are exact.

The real (resp., imaginary) component of the result of a binary addition operator that yields a result of complex type is exact when either of its operands is of pure-imaginary (resp., real) type. 

Ramification: The result of the addition operator is exact when one of its operands is of real type and the other is of pure-imaginary type. In this particular case, the operator is analogous to the Compose_From_Cartesian function; it performs no arithmetic. 

The real (resp., imaginary) component of the result of a binary subtraction operator that yields a result of complex type is exact when its right operand is of pure-imaginary (resp., real) type.

The real component of the result of the Conjugate function for the complex type is exact.

When the point in the complex plane represented by the parameter X lies on the nonnegative real axis, the Argument function yields a result of zero. 

Discussion: Argument(X + i*Y) is analogous to EF.Arctan(Y, X), where EF is an appropriate instance of Numerics.Generic_Elementary_Functions, except when X and Y are both zero, in which case the former yields the value zero while the latter raises Numerics.Argument_Error. 

When the value of the parameter Modulus is zero, the Compose_From_Polar function yields a result of zero.

When the value of the parameter Argument is equal to a multiple of the quarter cycle, the result of the Compose_From_Polar function with specified cycle lies on one of the axes. In this case, one of its components is zero, and the other has the magnitude of the parameter Modulus.

Exponentiation by a zero exponent yields the value one. Exponentiation by a unit exponent yields the value of the left operand. Exponentiation of the value one yields the value one. Exponentiation of the value zero yields the value zero, provided that the exponent is nonzero. When the left operand is of pure-imaginary type, one component of the result of the exponentiation operator is zero. 

When the result, or a result component, of any operator of Numerics.Generic_Complex_Types has a mathematical definition in terms of a single arithmetic or relational operation, that result or result component exhibits the accuracy of the corresponding operation of the type Real.

Other accuracy requirements for the Modulus, Argument, and Compose_From_Polar functions, and accuracy requirements for the multiplication of a pair of complex operands or for division by a complex operand, all of which apply only in the strict mode, are given in G.2.6.

The sign of a zero result or zero result component yielded by a complex arithmetic operation or function is implementation defined when Real'Signed_Zeros is True. 

Implementation defined: The sign of a zero result (or a component thereof) from any operator or function in Numerics.Generic_Complex_Types, when Real'Signed_Zeros is True.


#### Implementation Permissions

The nongeneric equivalent packages may, but need not, be actual instantiations of the generic package for the appropriate predefined type.

Implementations may obtain the result of exponentiation of a complex or pure-imaginary operand by repeated complex multiplication, with arbitrary association of the factors and with a possible final complex reciprocation (when the exponent is negative). Implementations are also permitted to obtain the result of exponentiation of a complex operand, but not of a pure-imaginary operand, by converting the left operand to a polar representation; exponentiating the modulus by the given exponent; multiplying the argument by the given exponent, when the exponent is positive, or dividing the argument by the absolute value of the given exponent, when the exponent is negative; and reconverting to a cartesian representation. Because of this implementation freedom, no accuracy requirement is imposed on complex exponentiation (except for the prescribed results given above, which apply regardless of the implementation method chosen). 


#### Implementation Advice

Because the usual mathematical meaning of multiplication of a complex operand and a real operand is that of the scaling of both components of the former by the latter, an implementation should not perform this operation by first promoting the real operand to complex type and then performing a full complex multiplication. In systems that, in the future, support an Ada binding to IEC 559:1989, the latter technique will not generate the required result when one of the components of the complex operand is infinite. (Explicit multiplication of the infinite component by the zero component obtained during promotion yields a NaN that propagates into the final result.) Analogous advice applies in the case of multiplication of a complex operand and a pure-imaginary operand, and in the case of division of a complex operand by a real or pure-imaginary operand. 

Likewise, because the usual mathematical meaning of addition of a complex operand and a real operand is that the imaginary operand remains unchanged, an implementation should not perform this operation by first promoting the real operand to complex type and then performing a full complex addition. In implementations in which the Signed_Zeros attribute of the component type is True (and which therefore conform to IEC 559:1989 in regard to the handling of the sign of zero in predefined arithmetic operations), the latter technique will not generate the required result when the imaginary component of the complex operand is a negatively signed zero. (Explicit addition of the negative zero to the zero obtained during promotion yields a positive zero.) Analogous advice applies in the case of addition of a complex operand and a pure-imaginary operand, and in the case of subtraction of a complex operand and a real or pure-imaginary operand.

Implementations in which Real'Signed_Zeros is True should attempt to provide a rational treatment of the signs of zero results and result components. As one example, the result of the Argument function should have the sign of the imaginary component of the parameter X when the point represented by that parameter lies on the positive real axis; as another, the sign of the imaginary component of the Compose_From_Polar function should be the same as (resp., the opposite of) that of the Argument parameter when that parameter has a value of zero and the Modulus parameter has a nonnegative (resp., negative) value. 

Implementation Advice: 


#### Wording Changes from Ada 83

The semantics of Numerics.Generic_Complex_Types differs from Generic_Complex_Types as defined in ISO/IEC CD 13813 (for Ada 83) in the following ways: 

The generic package is a child of the package defining the Argument_Error exception.

The nongeneric equivalents export types and constants with the same names as those exported by the generic package, rather than with names unique to the package.

Implementations are not allowed to impose an optional restriction that the generic actual parameter associated with Real be unconstrained. (In view of the ability to declare variables of subtype Real'Base in implementations of Numerics.Generic_Complex_Types, this flexibility is no longer needed.)

The dependence of the Argument function on the sign of a zero parameter component is tied to the value of Real'Signed_Zeros.

Conformance to accuracy requirements is conditional. 


### G.1.2  Complex Elementary Functions


#### Static Semantics

The generic library package Numerics.Generic_Complex_Elementary_Functions has the following declaration: 

```ada
with Ada.Numerics.Generic_Complex_Types;
generic
   with package Complex_Types is
         new Ada.Numerics.Generic_Complex_Types (&lt&gt);
   use Complex_Types;
package Ada.Numerics.Generic_Complex_Elementary_Functions is
   pragma Pure(Generic_Complex_Elementary_Functions);

```

```ada
   function Sqrt (X : Complex)   return Complex;
   function Log  (X : Complex)   return Complex;
   function Exp  (X : Complex)   return Complex;
   function Exp  (X : Imaginary) return Complex;
   function "**" (Left : Complex;   Right : Complex)   return Complex;
   function "**" (Left : Complex;   Right : Real'Base) return Complex;
   function "**" (Left : Real'Base; Right : Complex)   return Complex;

```

```ada
   function Sin (X : Complex) return Complex;
   function Cos (X : Complex) return Complex;
   function Tan (X : Complex) return Complex;
   function Cot (X : Complex) return Complex;

```

```ada
   function Arcsin (X : Complex) return Complex;
   function Arccos (X : Complex) return Complex;
   function Arctan (X : Complex) return Complex;
   function Arccot (X : Complex) return Complex;

```

```ada
   function Sinh (X : Complex) return Complex;
   function Cosh (X : Complex) return Complex;
   function Tanh (X : Complex) return Complex;
   function Coth (X : Complex) return Complex;

```

```ada
   function Arcsinh (X : Complex) return Complex;
   function Arccosh (X : Complex) return Complex;
   function Arctanh (X : Complex) return Complex;
   function Arccoth (X : Complex) return Complex;

```

```ada
end Ada.Numerics.Generic_Complex_Elementary_Functions;

```

The library package Numerics.Complex_Elementary_Functions defines the same subprograms as Numerics.Generic_Complex_Elementary_Functions, except that the predefined type Float is systematically substituted for Real'Base, and the Complex and Imaginary types exported by Numerics.Complex_Types are systematically substituted for Complex and Imaginary, throughout. Nongeneric equivalents of Numerics.Generic_Complex_Elementary_Functions corresponding to each of the other predefined floating point types are defined similarly, with the names Numerics.Short_Complex_Elementary_Functions, Numerics.Long_Complex_Elementary_Functions, etc. 

Reason: The nongeneric equivalents are provided to allow the programmer to construct simple mathematical applications without being required to understand and use generics. 

The overloading of the Exp function for the pure-imaginary type is provided to give the user an alternate way to compose a complex value from a given modulus and argument. In addition to Compose_From_Polar(Rho, Theta) (see G.1.1), the programmer may write Rho * Exp(i * Theta).

The imaginary (resp., real) component of the parameter X of the forward hyperbolic (resp., trigonometric) functions and of the Exp function (and the parameter X, itself, in the case of the overloading of the Exp function for the pure-imaginary type) represents an angle measured in radians, as does the imaginary (resp., real) component of the result of the Log and inverse hyperbolic (resp., trigonometric) functions.

The functions have their usual mathematical meanings. However, the arbitrariness inherent in the placement of branch cuts, across which some of the complex elementary functions exhibit discontinuities, is eliminated by the following conventions: 

The imaginary component of the result of the Sqrt and Log functions is discontinuous as the parameter X crosses the negative real axis.

The result of the exponentiation operator when the left operand is of complex type is discontinuous as that operand crosses the negative real axis.

The real (resp., imaginary) component of the result of the Arcsin and Arccos(resp., Arctanh) functions is discontinuous as the parameter X crosses the real axis to the left of 1.0 or the right of 1.0.

The real (resp., imaginary) component of the result of the Arctan (resp., Arcsinh) function is discontinuous as the parameter X crosses the imaginary axis below i or above i.

The real component of the result of the Arccot function is discontinuous as the parameter X crosses the imaginary axis between i and i.

The imaginary component of the Arccosh function is discontinuous as the parameter X crosses the real axis to the left of 1.0.

The imaginary component of the result of the Arccoth function is discontinuous as the parameter X crosses the real axis between 1.0 and 1.0. 

The computed results of the mathematically multivalued functions are rendered single-valued by the following conventions, which are meant to imply the principal branch: 

The real component of the result of the Sqrt and Arccosh functions is nonnegative.

The same convention applies to the imaginary component of the result of the Log function as applies to the result of the natural-cycle version of the Argument function of Numerics.Generic_Complex_Types (see G.1.1).

The range of the real (resp., imaginary) component of the result of the Arcsin and Arctan (resp., Arcsinh and Arctanh) functions is approximately /2.0 to /2.0.

The real (resp., imaginary) component of the result of the Arccos and Arccot (resp., Arccoth) functions ranges from 0.0 to approximately .

The range of the imaginary component of the result of the Arccosh function is approximately  to . 

In addition, the exponentiation operator inherits the single-valuedness of the Log function. 


#### Dynamic Semantics

The exception Numerics.Argument_Error is raised by the exponentiation operator, signaling a parameter value outside the domain of the corresponding mathematical function, when the value of the left operand is zero and the real component of the exponent (or the exponent itself, when it is of real type) is zero.

The exception Constraint_Error is raised, signaling a pole of the mathematical function (analogous to dividing by zero), in the following cases, provided that Complex_Types.Real'Machine_Overflows is True: 

by the Log, Cot, and Coth functions, when the value of the parameter X is zero;

by the exponentiation operator, when the value of the left operand is zero and the real component of the exponent (or the exponent itself, when it is of real type) is negative;

by the Arctan and Arccot functions, when the value of the parameter X is � i;

by the Arctanh and Arccoth functions, when the value of the parameter X is � 1.0. 

[Constraint_Error can also be raised when a finite result overflows (see G.2.6); this may occur for parameter values sufficiently near poles, and, in the case of some of the functions, for parameter values having components of sufficiently large magnitude.] When Complex_Types.Real'Machine_Overflows is False, the result at poles is unspecified. 

Reason: The purpose of raising Constraint_Error (rather than Numerics.Argument_Error) at the poles of a function, when Float_Type'Machine_Overflows is True, is to provide continuous behavior as the actual parameters of the function approach the pole and finally reach it. 

Discussion: It is anticipated that an Ada binding to IEC 559:1989 will be developed in the future. As part of such a binding, the Machine_Overflows attribute of a conformant floating point type will be specified to yield False, which will permit implementations of the complex elementary functions to deliver results with an infinite component (and set the overflow flag defined by the binding) instead of raising Constraint_Error in overflow situations, when traps are disabled. Similarly, it is appropriate for the complex elementary functions to deliver results with an infinite component (and set the zero-divide flag defined by the binding) instead of raising Constraint_Error at poles, when traps are disabled. Finally, such a binding should also specify the behavior of the complex elementary functions, when sensible, given parameters with infinite components. 


#### Implementation Requirements

In the implementation of Numerics.Generic_Complex_Elementary_Functions, the range of intermediate values allowed during the calculation of a final result shall not be affected by any range constraint of the subtype Complex_Types.Real. 

Implementation Note: Implementations of Numerics.Generic_Complex_Elementary_Functions written in Ada should therefore avoid declaring local variables of subtype Complex_Types.Real; the subtype Complex_Types.Real'Base should be used instead. 

In the following cases, evaluation of a complex elementary function shall yield the prescribed result (or a result having the prescribed component), provided that the preceding rules do not call for an exception to be raised: 

When the parameter X has the value zero, the Sqrt, Sin, Arcsin, Tan, Arctan, Sinh, Arcsinh, Tanh, and Arctanh functions yield a result of zero; the Exp, Cos, and Cosh functions yield a result of one; the Arccos and Arccot functions yield a real result; and the Arccoth function yields an imaginary result.

When the parameter X has the value one, the Sqrt function yields a result of one; the Log, Arccos, and Arccosh functions yield a result of zero; and the Arcsin function yields a real result.

When the parameter X has the value 1.0, the Sqrt function yields the result 

i (resp., i), when the sign of the imaginary component of X is positive (resp., negative), if Complex_Types.Real'Signed_Zeros is True;

i, if Complex_Types.Real'Signed_Zeros is False; 

the Log function yields an imaginary result; and the Arcsin and Arccos functions yield a real result.

When the parameter X has the value � i, the Log function yields an imaginary result.

Exponentiation by a zero exponent yields the value one. Exponentiation by a unit exponent yields the value of the left operand (as a complex value). Exponentiation of the value one yields the value one. Exponentiation of the value zero yields the value zero. 

Discussion: It is possible to give many other prescribed results restricting the result to the real or imaginary axis when the parameter X is appropriately restricted to easily testable portions of the domain. We follow the proposed ISO/IEC standard for Generic_Complex_Elementary_Functions (for Ada 83), CD 13813, in not doing so, however. 

Other accuracy requirements for the complex elementary functions, which apply only in the strict mode, are given in G.2.6.

The sign of a zero result or zero result component yielded by a complex elementary function is implementation defined when Complex_Types.Real'Signed_Zeros is True. 

Implementation defined: The sign of a zero result (or a component thereof) from any operator or function in Numerics.Generic_Complex_Elementary_Functions, when Complex_Types.Real'Signed_Zeros is True.


#### Implementation Permissions

The nongeneric equivalent packages may, but need not, be actual instantiations of the generic package with the appropriate predefined nongeneric equivalent of Numerics.Generic_Complex_Types; if they are, then the latter shall have been obtained by actual instantiation of Numerics.Generic_Complex_Types.

The exponentiation operator may be implemented in terms of the Exp and Log functions. Because this implementation yields poor accuracy in some parts of the domain, no accuracy requirement is imposed on complex exponentiation.

The implementation of the Exp function of a complex parameter X is allowed to raise the exception Constraint_Error, signaling overflow, when the real component of X exceeds an unspecified threshold that is approximately log(Complex_Types.Real'Safe_Last). This permission recognizes the impracticality of avoiding overflow in the marginal case that the exponential of the real component of X exceeds the safe range of Complex_Types.Real but both components of the final result do not. Similarly, the Sin and Cos (resp., Sinh and Cosh) functions are allowed to raise the exception Constraint_Error, signaling overflow, when the absolute value of the imaginary (resp., real) component of the parameter X exceeds an unspecified threshold that is approximately log(Complex_Types.Real'Safe_Last) + log(2.0). This permission recognizes the impracticality of avoiding overflow in the marginal case that the hyperbolic sine or cosine of the imaginary (resp., real) component of X exceeds the safe range of Complex_Types.Real but both components of the final result do not. 


#### Implementation Advice

Implementations in which Complex_Types.Real'Signed_Zeros is True should attempt to provide a rational treatment of the signs of zero results and result components. For example, many of the complex elementary functions have components that are odd functions of one of the parameter components; in these cases, the result component should have the sign of the parameter component at the origin. Other complex elementary functions have zero components whose sign is opposite that of a parameter component at the origin, or is always positive or always negative. 

Implementation Advice: 


#### Wording Changes from Ada 83

The semantics of Numerics.Generic_Complex_Elementary_Functions differs from Generic_Complex_Elementary_Functions as defined in ISO/IEC CD 13814 (for Ada 83) in the following ways: 

The generic package is a child unit of the package defining the Argument_Error exception.

The proposed Generic_Complex_Elementary_Functions standard (for Ada 83) specified names for the nongeneric equivalents, if provided. Here, those nongeneric equivalents are required.

The generic package imports an instance of Numerics.Generic_Complex_Types rather than a long list of individual types and operations exported by such an instance.

The dependence of the imaginary component of the Sqrt and Log functions on the sign of a zero parameter component is tied to the value of Complex_Types.Real'Signed_Zeros.

Conformance to accuracy requirements is conditional. 


### G.1.3  Complex Input-Output

The generic package Text_IO.Complex_IO defines procedures for the formatted input and output of complex values. The generic actual parameter in an instantiation of Text_IO.Complex_IO is an instance of Numerics.Generic_Complex_Types for some floating point subtype. Exceptional conditions are reported by raising the appropriate exception defined in Text_IO. 

Implementation Note: An implementation of Text_IO.Complex_IO can be built around an instance of Text_IO.Float_IO for the base subtype of Complex_Types.Real, where Complex_Types is the generic formal package parameter of Text_IO.Complex_IO. There is no need for an implementation of Text_IO.Complex_IO to parse real values. 


#### Static Semantics

The generic library package Text_IO.Complex_IO has the following declaration: 

Ramification: Because this is a child of Text_IO, the declarations of the visible part of Text_IO are directly visible within it. 

```ada
with Ada.Numerics.Generic_Complex_Types;
generic
   with package Complex_Types is
         new Ada.Numerics.Generic_Complex_Types (&lt&gt);
package Ada.Text_IO.Complex_IO is

```

```ada
   use Complex_Types;

```

```ada
   Default_Fore : Field := 2;
   Default_Aft  : Field := Real'Digits - 1;
   Default_Exp  : Field := 3;

```

```ada
   procedure Get (File  : in  File_Type;
                  Item  : out Complex;
                  Width : in  Field := 0);
   procedure Get (Item  : out Complex;
                  Width : in  Field := 0);

```

```ada
   procedure Put (File : in File_Type;
                  Item : in Complex;
                  Fore : in Field := Default_Fore;
                  Aft  : in Field := Default_Aft;
                  Exp  : in Field := Default_Exp);
   procedure Put (Item : in Complex;
                  Fore : in Field := Default_Fore;
                  Aft  : in Field := Default_Aft;
                  Exp  : in Field := Default_Exp);

```

```ada
   procedure Get (From : in  String;
                  Item : out Complex;
                  Last : out Positive);
   procedure Put (To   : out String;
                  Item : in  Complex;
                  Aft  : in  Field := Default_Aft;
                  Exp  : in  Field := Default_Exp);

```

```ada
end Ada.Text_IO.Complex_IO;

```

The semantics of the Get and Put procedures are as follows: 

```ada
procedure Get (File  : in  File_Type;
               Item  : out Complex;
               Width : in  Field := 0);
procedure Get (Item  : out Complex;
               Width : in  Field := 0);

```

The input sequence is a pair of optionally signed real literals representing the real and imaginary components of a complex value; optionally, the pair of components may be separated by a comma and/or surrounded by a pair of parentheses. Blanks are freely allowed before each of the components and before the parentheses and comma, if either is used. If the value of the parameter Width is zero, then 

line and page terminators are also allowed in these places;

the components shall be separated by at least one blank or line terminator if the comma is omitted; and

reading stops when the right parenthesis has been read, if the input sequence includes a left parenthesis, or when the imaginary component has been read, otherwise. 

If a nonzero value of Width is supplied, then

the components shall be separated by at least one blank if the comma is omitted; and

exactly Width characters are read, or the characters (possibly none) up to a line terminator, whichever comes first (blanks are included in the count). 

Reason: The parenthesized and comma-separated form is the form produced by Put on output (see below), and also by list-directed output in Fortran. The other allowed forms match several common styles of edit-directed output in Fortran, allowing most preexisting Fortran data files containing complex data to be read easily. When such files contain complex values with no separation between the real and imaginary components, the user will have to read those components separately, using an instance of Text_IO.Float_IO. 

Returns, in the parameter Item, the value of type Complex that corresponds to the input sequence.

The exception Text_IO.Data_Error is raised if the input sequence does not have the required syntax or if the components of the complex value obtained are not of the base subtype of Complex_Types.Real.

```ada
procedure Put (File : in File_Type;
               Item : in Complex;
               Fore : in Field := Default_Fore;
               Aft  : in Field := Default_Aft;
               Exp  : in Field := Default_Exp);
procedure Put (Item : in Complex;
               Fore : in Field := Default_Fore;
               Aft  : in Field := Default_Aft;
               Exp  : in Field := Default_Exp);

```

Outputs the value of the parameter Item as a pair of decimal literals representing the real and imaginary components of the complex value, using the syntax of an aggregate. More specifically, 

outputs a left parenthesis;

outputs the value of the real component of the parameter Item with the format defined by the corresponding Put procedure of an instance of Text_IO.Float_IO for the base subtype of Complex_Types.Real, using the given values of Fore, Aft, and Exp;

outputs a comma;

outputs the value of the imaginary component of the parameter Item with the format defined by the corresponding Put procedure of an instance of Text_IO.Float_IO for the base subtype of Complex_Types.Real, using the given values of Fore, Aft, and Exp;

outputs a right parenthesis. 

Discussion: If the file has a bounded line length, a line terminator may be output implicitly before any element of the sequence itemized above. 

Discussion: The option of outputting the complex value as a pair of reals without additional punctuation is not provided, since it can be accomplished by outputting the real and imaginary components of the complex value separately. 

```ada
procedure Get (From : in  String;
               Item : out Complex;
               Last : out Positive);

```

Reads a complex value from the beginning of the given string, following the same rule as the Get procedure that reads a complex value from a file, but treating the end of the string as a line terminator. Returns, in the parameter Item, the value of type Complex that corresponds to the input sequence. Returns in Last the index value such that From(Last) is the last character read.

The exception Text_IO.Data_Error is raised if the input sequence does not have the required syntax or if the components of the complex value obtained are not of the base subtype of Complex_Types.Real.

```ada
procedure Put (To   : out String;
               Item : in  Complex;
               Aft  : in  Field := Default_Aft;
               Exp  : in  Field := Default_Exp);

```

Outputs the value of the parameter Item to the given string as a pair of decimal literals representing the real and imaginary components of the complex value, using the syntax of an aggregate. More specifically, 

a left parenthesis, the real component, and a comma are left justified in the given string, with the real component having the format defined by the Put procedure (for output to a file) of an instance of Text_IO.Float_IO for the base subtype of Complex_Types.Real, using a value of zero for Fore and the given values of Aft and Exp;

the imaginary component and a right parenthesis are right justified in the given string, with the imaginary component having the format defined by the Put procedure (for output to a file) of an instance of Text_IO.Float_IO for the base subtype of Complex_Types.Real, using a value for Fore that completely fills the remainder of the string, together with the given values of Aft and Exp. 

Reason: This rule is the one proposed in LSN-1051. Other rules were considered, including one that would have read "Outputs the value of the parameter Item to the given string, following the same rule as for output to a file, using a value for Fore such that the sequence of characters output exactly fills, or comes closest to filling, the string; in the latter case, the string is filled by inserting one extra blank immediately after the comma." While this latter rule might be considered the closest analogue to the rule for output to a string in Text_IO.Float_IO, it requires a more difficult and inefficient implementation involving special cases when the integer part of one component is substantially longer than that of the other and the string is too short to allow both to be preceded by blanks. Unless such a special case applies, the latter rule might produce better columnar output if several such strings are ultimately output to a file, but very nearly the same output can be produced by outputting to the file directly, with the appropriate value of Fore; in any case, it might validly be assumed that output to a string is intended for further computation rather than for display, so that the precise formatting of the string to achieve a particular appearance is not the major concern. 

The exception Text_IO.Layout_Error is raised if the given string is too short to hold the formatted output. 


#### Implementation Permissions

Other exceptions declared (by renaming) in Text_IO may be raised by the preceding procedures in the appropriate circumstances, as for the corresponding procedures of Text_IO.Float_IO. 


### G.1.4  The Package Wide_Text_IO.Complex_IO


#### Static Semantics

Implementations shall also provide the generic library package Wide_Text_IO.Complex_IO. Its declaration is obtained from that of Text_IO.Complex_IO by systematically replacing Text_IO by Wide_Text_IO and String by Wide_String; the description of its behavior is obtained by additionally replacing references to particular characters (commas, parentheses, etc.) by those for the corresponding wide characters. 


## G.2  Numeric Performance Requirements


#### Implementation Requirements

Implementations shall provide a user-selectable mode in which the accuracy and other numeric performance requirements detailed in the following subclauses are observed. This mode, referred to as the strict mode, may or may not be the default mode; it directly affects the results of the predefined arithmetic operations of real types and the results of the subprograms in children of the Numerics package, and indirectly affects the operations in other language defined packages. Implementations shall also provide the opposing mode, which is known as the relaxed mode. 

Reason: On the assumption that the users of an implementation that does not support the Numerics Annex have no particular need for numerical performance, such an implementation has no obligation to meet any particular requirements in this area. On the other hand, users of an implementation that does support the Numerics Annex are provided with a way of ensuring that their programs achieve a known level of numerical performance and that the performance is portable to other such implementations. The relaxed mode is provided to allow implementers to offer an efficient but not fully accurate alternative in the case that the strict mode entails a time overhead that some users may find excessive. In some of its areas of impact, the relaxed mode may be fully equivalent to the strict mode. 

Implementation Note: The relaxed mode may, for example, be used to exploit the implementation of (some of) the elementary functions in hardware, when available. Such implementations often do not meet the accuracy requirements of the strict mode, or do not meet them over the specified range of parameter values, but compensate in other ways that may be important to the user, such as their extreme speed. 

Ramification: For implementations supporting the Numerics Annex, the choice of mode has no effect on the selection of a representation for a real type or on the values of attributes of a real type. 


#### Implementation Permissions

Either mode may be the default mode. 

Implementation defined: Whether the strict mode or the relaxed mode is the default.

The two modes need not actually be different. 


#### Extensions to Ada 83

The choice between strict and relaxed numeric performance was not available in Ada 83. 


### G.2.1  Model of Floating Point Arithmetic

In the strict mode, the predefined operations of a floating point type shall satisfy the accuracy requirements specified here and shall avoid or signal overflow in the situations described. This behavior is presented in terms of a model of floating point arithmetic that builds on the concept of the canonical form (see A.5.3). 


#### Static Semantics

Associated with each floating point type is an infinite set of model numbers. The model numbers of a type are used to define the accuracy requirements that have to be satisfied by certain predefined operations of the type; through certain attributes of the model numbers, they are also used to explain the meaning of a user-declared floating point type declaration. The model numbers of a derived type are those of the parent type; the model numbers of a subtype are those of its type.

The model numbers of a floating point type T are zero and all the values expressible in the canonical form (for the type T), in which mantissa has T'Model_Mantissa digits and exponent has a value greater than or equal to T'Model_Emin. (These attributes are defined in G.2.2.) 

Discussion: The model is capable of describing the behavior of most existing hardware that has a mantissa-exponent representation. As applied to a type T, it is parameterized by the values of T'Machine_Radix, T'Model_Mantissa, T'Model_Emin, T'Safe_First, and T'Safe_Last. The values of these attributes are determined by how, and how well, the hardware behaves. They in turn determine the set of model numbers and the safe range of the type, which figure in the accuracy and range (overflow avoidance) requirements.

In hardware that is free of arithmetic anomalies, T'Model_Mantissa, T'Model_Emin, T'Safe_First, and T'Safe_Last will yield the same values as T'Machine_Mantissa, T'Machine_Emin, T'Base'First, and T'Base'Last, respectively, and the model numbers in the safe range of the type T will coincide with the machine numbers of the type T. In less perfect hardware, it is not possible for the model-oriented attributes to have these optimal values, since the hardware, by definition, and therefore the implementation, cannot conform to the stringencies of the resulting model; in this case, the values yielded by the model-oriented parameters have to be made more conservative (i.e., have to be penalized), with the result that the model numbers are more widely separated than the machine numbers, and the safe range is a subrange of the base range. The implementation will then be able to conform to the requirements of the weaker model defined by the sparser set of model numbers and the smaller safe range. 

A model interval of a floating point type is any interval whose bounds are model numbers of the type. The model interval of a type T associated with a value v is the smallest model interval of T that includes v. (The model interval associated with a model number of a type consists of that number only.) 


#### Implementation Requirements

The accuracy requirements for the evaluation of certain predefined operations of floating point types are as follows. 

Discussion: This subclause does not cover the accuracy of an operation of a static expression; such operations have to be evaluated exactly (see 4.9). It also does not cover the accuracy of the predefined attributes of a floating point subtype that yield a value of the type; such operations also yield exact results (see 3.5.8 and A.5.3). 

An operand interval is the model interval, of the type specified for the operand of an operation, associated with the value of the operand.

For any predefined arithmetic operation that yields a result of a floating point type T, the required bounds on the result are given by a model interval of T (called the result interval) defined in terms of the operand values as follows: 

The result interval is the smallest model interval of T that includes the minimum and the maximum of all the values obtained by applying the (exact) mathematical operation to values arbitrarily selected from the respective operand intervals. 

The result interval of an exponentiation is obtained by applying the above rule to the sequence of multiplications defined by the exponent, assuming arbitrary association of the factors, and to the final division in the case of a negative exponent.

The result interval of a conversion of a numeric value to a floating point type T is the model interval of T associated with the operand value, except when the source expression is of a fixed point type with a small that is not a power of T'Machine_Radix or is a fixed point multiplication or division either of whose operands has a small that is not a power of T'Machine_Radix; in these cases, the result interval is implementation defined. 

Implementation defined: The result interval in certain cases of fixed-to-float conversion.

For any of the foregoing operations, the implementation shall deliver a value that belongs to the result interval when both bounds of the result interval are in the safe range of the result type T, as determined by the values of T'Safe_First and T'Safe_Last; otherwise, 

if T'Machine_Overflows is True, the implementation shall either deliver a value that belongs to the result interval or raise Constraint_Error;

if T'Machine_Overflows is False, the result is implementation defined. 

Implementation defined: The result of a floating point arithmetic operation in overflow situations, when the Machine_Overflows attribute of the result type is False.

For any predefined relation on operands of a floating point type T, the implementation may deliver any value (i.e., either True or False) obtained by applying the (exact) mathematical comparison to values arbitrarily chosen from the respective operand intervals.

The result of a membership test is defined in terms of comparisons of the operand value with the lower and upper bounds of the given range or type mark (the usual rules apply to these comparisons). 


#### Implementation Permissions

If the underlying floating point hardware implements division as multiplication by a reciprocal, the result interval for division (and exponentiation by a negative exponent) is implementation defined. 

Implementation defined: The result interval for division (or exponentiation by a negative exponent), when the floating point hardware implements division as multiplication by a reciprocal.


#### Wording Changes from Ada 83

The Ada 95 model numbers of a floating point type that are in the safe range of the type are comparable to the Ada 83 safe numbers of the type. There is no analog of the Ada 83 model numbers. The Ada 95 model numbers, when not restricted to the safe range, are an infinite set. 


#### Inconsistencies With Ada 83

Giving the model numbers the hardware radix, instead of always a radix of two, allows (in conjunction with other changes) some borderline declared types to be represented with less precision than in Ada 83 (i.e., with single precision, whereas Ada 83 would have used double precision). Because the lower precision satisfies the requirements of the model (and did so in Ada 83 as well), this change is viewed as a desirable correction of an anomaly, rather than a worrisome inconsistency. (Of course, the wider representation chosen in Ada 83 also remains eligible for selection in Ada 95.)

As an example of this phenomenon, assume that Float is represented in single precision and that a double precision type is also available. Also assume hexadecimal hardware with clean properties, for example certain IBM hardware. Then, 

```ada
type T is digits Float'Digits range -Float'Last .. Float'Last;

```

results in T being represented in double precision in Ada 83 and in single precision in Ada 95. The latter is intuitively correct; the former is counterintuitive. The reason why the double precision type is used in Ada 83 is that Float has model and safe numbers (in Ada 83) with 21 binary digits in their mantissas, as is required to model the hypothesized hexadecimal hardware using a binary radix; thus Float'Last, which is not a model number, is slightly outside the range of safe numbers of the single precision type, making that type ineligible for selection as the representation of T even though it provides adequate precision. In Ada 95, Float'Last (the same value as before) is a model number and is in the safe range of Float on the hypothesized hardware, making Float eligible for the representation of T. 


#### Extensions to Ada 83

Giving the model numbers the hardware radix allows for practical implementations on decimal hardware. 


#### Wording Changes from Ada 83

The wording of the model of floating point arithmetic has been simplified to a large extent. 


### G.2.2  Model-Oriented Attributes of Floating Point Types

In implementations that support the Numerics Annex, the model-oriented attributes of floating point types shall yield the values defined here, in both the strict and the relaxed modes. These definitions add conditions to those in A.5.3. 


#### Static Semantics

For every subtype S of a floating point type T: 

S'Model_Mantissa Yields the number of digits in the mantissa of the canonical form of the model numbers of T (see A.5.3). The value of this attribute shall be greater than or equal to d � log(10) / log(T'Machine_Radix) + 1, where d is the requested decimal precision of T. In addition, it shall be less than or equal to the value of T'Machine_Mantissa. This attribute yields a value of the type universal_integer. 

Ramification: S'Model_Epsilon, which is defined in terms of S'Model_Mantissa (see A.5.3), yields the absolute value of the difference between one and the next model number of the type T above one. It is equal to or larger than the absolute value of the difference between one and the next machine number of the type T above one. 

S'Model_Emin Yields the minimum exponent of the canonical form of the model numbers of T (see A.5.3). The value of this attribute shall be greater than or equal to the value of T'Machine_Emin. This attribute yields a value of the type universal_integer. 

Ramification: S'Model_Small, which is defined in terms of S'Model_Emin (see A.5.3), yields the smallest positive (nonzero) model number of the type T. 

S'Safe_First Yields the lower bound of the safe range of T. The value of this attribute shall be a model number of T and greater than or equal to the lower bound of the base range of T. In addition, if T is declared by a [floating_point_definition](S0042) or is derived from such a type, and the [floating_point_definition](S0042) includes a [real_range_specification](S0043) specifying a lower bound of lb, then the value of this attribute shall be less than or equal to lb; otherwise, it shall be less than or equal to 10.0 4 � d, where d is the requested decimal precision of T. This attribute yields a value of the type universal_real.

S'Safe_Last Yields the upper bound of the safe range of T. The value of this attribute shall be a model number of T and less than or equal to the upper bound of the base range of T. In addition, if T is declared by a [floating_point_definition](S0042) or is derived from such a type, and the [floating_point_definition](S0042) includes a [real_range_specification](S0043) specifying an upper bound of ub, then the value of this attribute shall be greater than or equal to ub; otherwise, it shall be greater than or equal to 10.0 4 � d, where d is the requested decimal precision of T. This attribute yields a value of the type universal_real.

S'Model Denotes a function (of a parameter X) whose specification is given in A.5.3. If X is a model number of T, the function yields X; otherwise, it yields the value obtained by rounding or truncating X to either one of the adjacent model numbers of T. Constraint_Error is raised if the resulting model number is outside the safe range of S. A zero result has the sign of X when S'Signed_Zeros is True. 

Subject to the constraints given above, the values of S'Model_Mantissa and S'Safe_Last are to be maximized, and the values of S'Model_Emin and S'Safe_First minimized, by the implementation as follows: 

First, S'Model_Mantissa is set to the largest value for which values of S'Model_Emin, S'Safe_First, and S'Safe_Last can be chosen so that the implementation satisfies the strict-mode requirements of G.2.1 in terms of the model numbers and safe range induced by these attributes.

Next, S'Model_Emin is set to the smallest value for which values of S'Safe_First and S'Safe_Last can be chosen so that the implementation satisfies the strict-mode requirements of G.2.1 in terms of the model numbers and safe range induced by these attributes and the previously determined value of S'Model_Mantissa.

Finally, S'Safe_First and S'Safe_last are set (in either order) to the smallest and largest values, respectively, for which the implementation satisfies the strict-mode requirements of G.2.1 in terms of the model numbers and safe range induced by these attributes and the previously determined values of S'Model_Mantissa and S'Model_Emin. 

Ramification: The following table shows appropriate attribute values for IEEE basic single and double precision types (ANSI/IEEE Std 754-1985, IEC 559:1989). Here, we use the names IEEE_Float_32 and IEEE_Float_64, the names that would typically be declared in package Interfaces, in an implementation that supports IEEE arithmetic. In such an implementation, the attributes would typically be the same for Standard.Float and Long_Float, respectively. 

```ada
Attribute                        IEEE_Float_32                 IEEE_Float_64

```

```ada
'Machine_Radix                               2                             2
'Machine_Mantissa                           24                            53
'Machine_Emin                             -125                         -1021
'Machine_Emax                              128                          1024
'Denorm                                   True                          True
'Machine_Rounds                           True                          True
'Machine_Overflows                  True/False                    True/False
'Signed_Zeros                   should be True                should be True

```

```ada
'Model_Mantissa    (same as 'Machine_Mantissa)   (same as 'Machine_Mantissa)
'Model_Emin            (same as 'Machine_Emin)       (same as 'Machine_Emin)
'Model_Epsilon                      2.0**(-23)                    2.0**(-52)
'Model_Small                       2.0**(-126)                  2.0**(-1022)
'Safe_First         -2.0**128*(1.0-2.0**(-24))   -2.0**1024*(1.0-2.0**(-53))
'Safe_Last           2.0**128*(1.0-2.0**(-24))    2.0**1024*(1.0-2.0**(-53))

```

```ada
'Digits                                      6                            15
'Base'Digits                 (same as 'Digits)             (same as 'Digits)

```

```ada
'First                   (same as 'Safe_First)         (same as 'Safe_First)
'Last                     (same as 'Safe_Last)          (same as 'Safe_Last)
'Size                                       32                            64

```

Note: 'Machine_Overflows can be True or False, depending on whether the Ada implementation raises Constraint_Error or delivers a signed infinity in overflow and zerodivide situations (and at poles of the elementary functions).


### G.2.3  Model of Fixed Point Arithmetic

In the strict mode, the predefined arithmetic operations of a fixed point type shall satisfy the accuracy requirements specified here and shall avoid or signal overflow in the situations described. 


#### Implementation Requirements

The accuracy requirements for the predefined fixed point arithmetic operations and conversions, and the results of relations on fixed point operands, are given below. 

Discussion: This subclause does not cover the accuracy of an operation of a static expression; such operations have to be evaluated exactly (see 4.9). 

The operands of the fixed point adding operators, absolute value, and comparisons have the same type. These operations are required to yield exact results, unless they overflow.

Multiplications and divisions are allowed between operands of any two fixed point types; the result has to be (implicitly or explicitly) converted to some other numeric type. For purposes of defining the accuracy rules, the multiplication or division and the conversion are treated as a single operation whose accuracy depends on three types (those of the operands and the result). For decimal fixed point types, the attribute T'Round may be used to imply explicit conversion with rounding (see 3.5.10).

When the result type is a floating point type, the accuracy is as given in G.2.1. For some combinations of the operand and result types in the remaining cases, the result is required to belong to a small set of values called the perfect result set; for other combinations, it is required merely to belong to a generally larger and implementation-defined set of values called the close result set. When the result type is a decimal fixed point type, the perfect result set contains a single value; thus, operations on decimal types are always fully specified. 

Implementation defined: The definition of close result set, which determines the accuracy of certain fixed point multiplications and divisions.

When one operand of a fixed-fixed multiplication or division is of type universal_real, that operand is not implicitly converted in the usual sense, since the context does not determine a unique target type, but the accuracy of the result of the multiplication or division (i.e., whether the result has to belong to the perfect result set or merely the close result set) depends on the value of the operand of type universal_real and on the types of the other operand and of the result. 

Discussion: We need not consider here the multiplication or division of two such operands, since in that case either the operation is evaluated exactly (i.e., it is an operation of a static expression all of whose operators are of a root numeric type) or it is considered to be an operation of a floating point type. 

For a fixed point multiplication or division whose (exact) mathematical result is v, and for the conversion of a value v to a fixed point type, the perfect result set and close result set are defined as follows: 

If the result type is an ordinary fixed point type with a small of s, 

if v is an integer multiple of s, then the perfect result set contains only the value v;

otherwise, it contains the integer multiple of s just below v and the integer multiple of s just above v. 

The close result set is an implementation-defined set of consecutive integer multiples of s containing the perfect result set as a subset.

If the result type is a decimal type with a small of s, 

if v is an integer multiple of s, then the perfect result set contains only the value v;

otherwise, if truncation applies then it contains only the integer multiple of s in the direction toward zero, whereas if rounding applies then it contains only the nearest integer multiple of s (with ties broken by rounding away from zero). 

The close result set is an implementation-defined set of consecutive integer multiples of s containing the perfect result set as a subset. 

Ramification: As a consequence of subsequent rules, this case does not arise when the operand types are also decimal types. 

If the result type is an integer type, 

if v is an integer, then the perfect result set contains only the value v;

otherwise, it contains the integer nearest to the value v (if v lies equally distant from two consecutive integers, the perfect result set contains the one that is further from zero). 

The close result set is an implementation-defined set of consecutive integers containing the perfect result set as a subset. 

The result of a fixed point multiplication or division shall belong either to the perfect result set or to the close result set, as described below, if overflow does not occur. In the following cases, if the result type is a fixed point type, let s be its small; otherwise, i.e. when the result type is an integer type, let s be 1.0. 

For a multiplication or division neither of whose operands is of type universal_real, let l and r be the smalls of the left and right operands. For a multiplication, if (l � r) / s is an integer or the reciprocal of an integer (the smalls are said to be "compatible" in this case), the result shall belong to the perfect result set; otherwise, it belongs to the close result set. For a division, if l / (r � s) is an integer or the reciprocal of an integer (i.e., the smalls are compatible), the result shall belong to the perfect result set; otherwise, it belongs to the close result set. 

Ramification: When the operand and result types are all decimal types, their smalls are necessarily compatible; the same is true when they are all ordinary fixed point types with binary smalls. 

For a multiplication or division having one universal_real operand with a value of v, note that it is always possible to factor v as an integer multiple of a "compatible" small, but the integer multiple may be "too big". If there exists a factorization in which that multiple is less than some implementation-defined limit, the result shall belong to the perfect result set; otherwise, it belongs to the close result set. 

Implementation defined: Conditions on a universal_real operand of a fixed point multiplication or division for which the result shall be in the perfect result set.

A multiplication P * Q of an operand of a fixed point type F by an operand of an integer type I, or vice-versa, and a division P / Q of an operand of a fixed point type F by an operand of an integer type I, are also allowed. In these cases, the result has a type of F; explicit conversion of the result is never required. The accuracy required in these cases is the same as that required for a multiplication F(P * Q) or a division F(P / Q) obtained by interpreting the operand of the integer type to have a fixed point type with a small of 1.0.

The accuracy of the result of a conversion from an integer or fixed point type to a fixed point type, or from a fixed point type to an integer type, is the same as that of a fixed point multiplication of the source value by a fixed point operand having a small of 1.0 and a value of 1.0, as given by the foregoing rules. The result of a conversion from a floating point type to a fixed point type shall belong to the close result set. The result of a conversion of a universal_real operand to a fixed point type shall belong to the perfect result set.

The possibility of overflow in the result of a predefined arithmetic operation or conversion yielding a result of a fixed point type T is analogous to that for floating point types, except for being related to the base range instead of the safe range. If all of the permitted results belong to the base range of T, then the implementation shall deliver one of the permitted results; otherwise, 

if T'Machine_Overflows is True, the implementation shall either deliver one of the permitted results or raise Constraint_Error;

if T'Machine_Overflows is False, the result is implementation defined. 

Implementation defined: The result of a fixed point arithmetic operation in overflow situations, when the Machine_Overflows attribute of the result type is False.


#### Inconsistencies With Ada 83

Since the values of a fixed point type are now just the integer multiples of its small, the possibility of using extra bits available in the chosen representation for extra accuracy rather than for increasing the base range would appear to be removed, raising the possibility that some fixed point expressions will yield less accurate results than in Ada 83. However, this is partially offset by the ability of an implementation to choose a smaller default small than before. Of course, if it does so for a type T then T'Small will have a different value than it previously had.

The accuracy requirements in the case of incompatible smalls are relaxed to foster wider support for nonbinary smalls. If this relaxation is exploited for a type that was previously supported, lower accuracy could result; however, there is no particular incentive to exploit the relaxation in such a case. 


#### Wording Changes from Ada 83

The fixed point accuracy requirements are now expressed without reference to model or safe numbers, largely because the full generality of the former model was never exploited in the case of fixed point types (particularly in regard to operand perturbation). Although the new formulation in terms of perfect result sets and close result sets is still verbose, it can be seen to distill down to two cases: 

a case where the result must be the exact result, if the exact result is representable, or, if not, then either one of the adjacent values of the type (in some subcases only one of those adjacent values is allowed);

a case where the accuracy is not specified by the language. 


### G.2.4  Accuracy Requirements for the Elementary Functions

In the strict mode, the performance of Numerics.Generic_Elementary_Functions shall be as specified here. 


#### Implementation Requirements

When an exception is not raised, the result of evaluating a function in an instance EF of Numerics.Generic_Elementary_Functions belongs to a result interval, defined as the smallest model interval of EF.Float_Type that contains all the values of the form f � (1.0 + d), where f is the exact value of the corresponding mathematical function at the given parameter values, d is a real number, and |d| is less than or equal to the function's maximum relative error. The function delivers a value that belongs to the result interval when both of its bounds belong to the safe range of EF.Float_Type; otherwise, 

if EF.Float_Type'Machine_Overflows is True, the function either delivers a value that belongs to the result interval or raises Constraint_Error, signaling overflow;

if EF.Float_Type'Machine_Overflows is False, the result is implementation defined. 

Implementation defined: The result of an elementary function reference in overflow situations, when the Machine_Overflows attribute of the result type is False.

The maximum relative error exhibited by each function is as follows: 

2.0 � EF.Float_Type'Model_Epsilon, in the case of the Sqrt, Sin, and Cos functions;

4.0 � EF.Float_Type'Model_Epsilon, in the case of the Log, Exp, Tan, Cot, and inverse trigonometric functions; and

8.0 � EF.Float_Type'Model_Epsilon, in the case of the forward and inverse hyperbolic functions. 

The maximum relative error exhibited by the exponentiation operator, which depends on the values of the operands, is (4.0 + |Right � log(Left)| / 32.0) � EF.Float_Type'Model_Epsilon.

The maximum relative error given above applies throughout the domain of the forward trigonometric functions when the Cycle parameter is specified. When the Cycle parameter is omitted, the maximum relative error given above applies only when the absolute value of the angle parameter X is less than or equal to some implementation-defined angle threshold, which shall be at least EF.Float_Type'Machine_Radix EF.Float_Type'Machine_Mantissa/2. Beyond the angle threshold, the accuracy of the forward trigonometric functions is implementation defined. 

Implementation defined: The value of the angle threshold, within which certain elementary functions, complex arithmetic operations, and complex elementary functions yield results conforming to a maximum relative error bound.

Implementation defined: The accuracy of certain elementary functions for parameters beyond the angle threshold.

Implementation Note: The angle threshold indirectly determines the amount of precision that the implementation has to maintain during argument reduction. 

The prescribed results specified in A.5.1 for certain functions at particular parameter values take precedence over the maximum relative error bounds; effectively, they narrow to a single value the result interval allowed by the maximum relative error bounds. Additional rules with a similar effect are given by the table below for the inverse trigonometric functions, at particular parameter values for which the mathematical result is possibly not a model number of EF.Float_Type (or is, indeed, even transcendental). In each table entry, the values of the parameters are such that the result lies on the axis between two quadrants; the corresponding accuracy rule, which takes precedence over the maximum relative error bounds, is that the result interval is the model interval of EF.Float_Type associated with the exact mathematical result given in the table.

 

The last line of the table is meant to apply when EF.Float_Type'Signed_Zeros is False; the two lines just above it, when EF.Float_Type'Signed_Zeros is True and the parameter Y has a zero value with the indicated sign.

Tightly Approximated Elementary Function ResultsFunctionValue of XValue of YExact Result 
when Cycle 
SpecifiedExact Result 
when Cycle 
OmittedArcsin1.0n.a.Cycle/4.0/2.0Arcsin1.0n.a.Cycle/4.0/2.0Arccos0.0n.a.Cycle/4.0/2.0Arccos1.0n.a.Cycle/2.0Arctan and Arccot0.0positiveCycle/4.0/2.0Arctan and Arccot0.0negativeCycle/4.0/2.0Arctan and Arccotnegative+0.0Cycle/2.0Arctan and Arccotnegative0.0Cycle/2.0Arctan and Arccotnegative0.0Cycle/2.0The amount by which the result of an inverse trigonometric function is allowed to spill over into a quadrant adjacent to the one corresponding to the principal branch, as given in A.5.1, is limited. The rule is that the result belongs to the smallest model interval of EF.Float_Type that contains both boundaries of the quadrant corresponding to the principal branch. This rule also takes precedence over the maximum relative error bounds, effectively narrowing the result interval allowed by them.

Finally, the following specifications also take precedence over the maximum relative error bounds: 

The absolute value of the result of the Sin, Cos, and Tanh functions never exceeds one.

The absolute value of the result of the Coth function is never less than one.

The result of the Cosh function is never less than one. 


#### Implementation Advice

The versions of the forward trigonometric functions without a Cycle parameter should not be implemented by calling the corresponding version with a Cycle parameter of 2.0*Numerics.Pi, since this will not provide the required accuracy in some portions of the domain. For the same reason, the version of Log without a Base parameter should not be implemented by calling the corresponding version with a Base parameter of Numerics.e. 


#### Wording Changes from Ada 83

The semantics of Numerics.Generic_Elementary_Functions differs from Generic_Elementary_Functions as defined in ISO/IEC DIS 11430 (for Ada 83) in the following ways related to the accuracy specified for strict mode: 

The maximum relative error bounds use the Model_Epsilon attribute instead of the Base'Epsilon attribute.

The accuracy requirements are expressed in terms of result intervals that are model intervals. On the one hand, this facilitates the description of the required results in the presence of underflow; on the other hand, it slightly relaxes the requirements expressed in ISO/IEC DIS 11430. 


### G.2.5  Performance Requirements for Random Number Generation

In the strict mode, the performance of Numerics.Float_Random and Numerics.Discrete_Random shall be as specified here. 


#### Implementation Requirements

Two different calls to the time-dependent Reset procedure shall reset the generator to different states, provided that the calls are separated in time by at least one second and not more than fifty years.

The implementation's representations of generator states and its algorithms for generating random numbers shall yield a period of at least 2312; much longer periods are desirable but not required.

The implementations of Numerics.Float_Random.Random and Numerics.Discrete_Random.Random shall pass at least 85% of the individual trials in a suite of statistical tests. For Numerics.Float_Random, the tests are applied directly to the floating point values generated (i.e., they are not converted to integers first), while for Numerics.Discrete_Random they are applied to the generated values of various discrete types. Each test suite performs 6 different tests, with each test repeated 10 times, yielding a total of 60 individual trials. An individual trial is deemed to pass if the chi-square value (or other statistic) calculated for the observed counts or distribution falls within the range of values corresponding to the 2.5 and 97.5 percentage points for the relevant degrees of freedom (i.e., it shall be neither too high nor too low). For the purpose of determining the degrees of freedom, measurement categories are combined whenever the expected counts are fewer than 5. 

Implementation Note: In the floating point random number test suite, the generator is reset to a time-dependent state at the beginning of the run. The test suite incorporates the following tests, adapted from D. E. Knuth, The Art of Computer Programming, vol. 2: Seminumerical Algorithms.  In the descriptions below, the given number of degrees of freedom is the number before reduction due to any necessary combination of measurement categories with small expected counts; it is one less than the number of measurement categories. 

Proportional Distribution Test (a variant of the Equidistribution Test). The interval 0.0 .. 1.0 is partitioned into K subintervals. K is chosen randomly between 4 and 25 for each repetition of the test, along with the boundaries of the subintervals (subject to the constraint that at least 2 of the subintervals have a width of 0.001 or more). 5000 random floating point numbers are generated. The counts of random numbers falling into each subinterval are tallied and compared with the expected counts, which are proportional to the widths of the subintervals. The number of degrees of freedom for the chi-square test is K1.

Gap Test. The bounds of a range A .. B, with 0.0  A &lt B  1.0, are chosen randomly for each repetition of the test, subject to the constraint that 0.2  BA  0.6. Random floating point numbers are generated until 5000 falling into the range A .. B have been encountered. Each of these 5000 is preceded by a "gap" (of length greater than or equal to 0) of consecutive random numbers not falling into the range A .. B. The counts of gaps of each length from 0 to 15, and of all lengths greater than 15 lumped together, are tallied and compared with the expected counts. Let P = BA. The probability that a gap has a length of L is (1P) L � P for L  15, while the probability that a gap has a length of 16 or more is (1P) 16. The number of degrees of freedom for the chi-square test is 16.

Permutation Test. 5000 tuples of 4 different random floating point numbers are generated. (An entire 4-tuple is discarded in the unlikely event that it contains any two exactly equal components.) The counts of each of the 4! = 24 possible relative orderings of the components of the 4-tuples are tallied and compared with the expected counts. Each of the possible relative orderings has an equal probability. The number of degrees of freedom for the chi-square test is 23.

Increasing-Runs Test. Random floating point numbers are generated until 5000 increasing runs have been observed. An "increasing run" is a sequence of random numbers in strictly increasing order; it is followed by a random number that is strictly smaller than the preceding random number. (A run under construction is entirely discarded in the unlikely event that one random number is followed immediately by an exactly equal random number.) The decreasing random number that follows an increasing run is discarded and not included with the next increasing run. The counts of increasing runs of each length from 1 to 4, and of all lengths greater than 4 lumped together, are tallied and compared with the expected counts. The probability that an increasing run has a length of L is 1/L!  1/(L+1)! for L  4, while the probability that an increasing run has a length of 5 or more is 1/5!. The number of degrees of freedom for the chi-square test is 4.

Decreasing-Runs Test. The test is similar to the Increasing Runs Test, but with decreasing runs.

Maximum-of-t Test (with t = 5). 5000 tuples of 5 random floating point numbers are generated. The maximum of the components of each 5-tuple is determined and raised to the 5th power. The uniformity of the resulting values over the range 0.0 .. 1.0 is tested as in the Proportional Distribution Test. 

Implementation Note: In the discrete random number test suite, Numerics.Discrete_Random is instantiated as described below. The generator is reset to a time-dependent state after each instantiation. The test suite incorporates the following tests, adapted from D. E. Knuth (op. cit.) and other sources. The given number of degrees of freedom for the chi-square test is reduced by any necessary combination of measurement categories with small expected counts, as described above. 

Equidistribution Test. In each repetition of the test, a number R between 2 and 30 is chosen randomly, and Numerics.Discrete_Random is instantiated with an integer subtype whose range is 1 .. R. 5000 integers are generated randomly from this range. The counts of occurrences of each integer in the range are tallied and compared with the expected counts, which have equal probabilities. The number of degrees of freedom for the chi-square test is R1.

Simplified Poker Test. Numerics.Discrete_Random is instantiated once with an enumeration subtype representing the 13 denominations (Two through Ten, Jack, Queen, King, and Ace) of an infinite deck of playing cards. 2000 "poker" hands (5-tuples of values of this subtype) are generated randomly. The counts of hands containing exactly K different denominations (1  K  5) are tallied and compared with the expected counts. The probability that a hand contains exactly K different denominations is given by a formula in Knuth. The number of degrees of freedom for the chi-square test is 4.

Coupon Collector's Test. Numerics.Discrete_Random is instantiated in each repetition of the test with an integer subtype whose range is 1 .. R, where R varies systematically from 2 to 11. Integers are generated randomly from this range until each value in the range has occurred, and the number K of integers generated is recorded. This constitutes a "coupon collector's segment" of length K. 2000 such segments are generated. The counts of segments of each length from R to R+29, and of all lengths greater than R+29 lumped together, are tallied and compared with the expected counts. The probability that a segment has any given length is given by formulas in Knuth. The number of degrees of freedom for the chi-square test is 30.

Craps Test (Lengths of Games). Numerics.Discrete_Random is instantiated once with an integer subtype whose range is 1 .. 6 (representing the six numbers on a die). 5000 craps games are played, and their lengths are recorded. (The length of a craps game is the number of rolls of the pair of dice required to produce a win or a loss. A game is won on the first roll if the dice show 7 or 11; it is lost if they show 2, 3, or 12. If the dice show some other sum on the first roll, it is called the point, and the game is won if and only if the point is rolled again before a 7 is rolled.) The counts of games of each length from 1 to 18, and of all lengths greater than 18 lumped together, are tallied and compared with the expected counts. For 2  S  12, let D S be the probability that a roll of a pair of dice shows the sum S, and let Q S(L) = D S � (1  (D S + D 7)) L2 � (D S + D 7). Then, the probability that a game has a length of 1 is D 7 + D 11 + D 2 + D 3 + D 12 and, for L &gt 1, the probability that a game has a length of L is Q 4(L) + Q 5(L) + Q 6(L) + Q 8(L) + Q 9(L) + Q 10(L). The number of degrees of freedom for the chi-square test is 18.

Craps Test (Lengths of Passes). This test is similar to the last, but enough craps games are played for 3000 losses to occur. A string of wins followed by a loss is called a pass, and its length is the number of wins preceding the loss. The counts of passes of each length from 0 to 7, and of all lengths greater than 7 lumped together, are tallied and compared with the expected counts. For L  0, the probability that a pass has a length of L is W L � (1W), where W, the probability that a game ends in a win, is 244.0/495.0. The number of degrees of freedom for the chi-square test is 8.

Collision Test. Numerics.Discrete_Random is instantiated once with an integer or enumeration type representing binary bits. 15 successive calls on the Random function are used to obtain the bits of a 15-bit binary integer between 0 and 32767. 3000 such integers are generated, and the number of collisions (integers previously generated) is counted and compared with the expected count. A chi-square test is not used to assess the number of collisions; rather, the limits on the number of collisions, corresponding to the 2.5 and 97.5 percentage points, are (from formulas in Knuth) 112 and 154. The test passes if and only if the number of collisions is in this range. 


### G.2.6  Accuracy Requirements for Complex Arithmetic

In the strict mode, the performance of Numerics.Generic_Complex_Types and Numerics.Generic_Complex_Elementary_Functions shall be as specified here. 


#### Implementation Requirements

When an exception is not raised, the result of evaluating a real function of an instance CT of Numerics.Generic_Complex_Types (i.e., a function that yields a value of subtype CT.Real'Base or CT.Imaginary) belongs to a result interval defined as for a real elementary function (see G.2.4).

When an exception is not raised, each component of the result of evaluating a complex function of such an instance, or of an instance of Numerics.Generic_Complex_Elementary_Functions obtained by instantiating the latter with CT (i.e., a function that yields a value of subtype CT.Complex), also belongs to a result interval. The result intervals for the components of the result are either defined by a maximum relative error bound or by a maximum box error bound. When the result interval for the real (resp., imaginary) component is defined by maximum relative error, it is defined as for that of a real function, relative to the exact value of the real (resp., imaginary) part of the result of the corresponding mathematical function. When defined by maximum box error, the result interval for a component of the result is the smallest model interval of CT.Real that contains all the values of the corresponding part of f � (1.0 + d), where f is the exact complex value of the corresponding mathematical function at the given parameter values, d is complex, and |d| is less than or equal to the given maximum box error. The function delivers a value that belongs to the result interval (or a value both of whose components belong to their respective result intervals) when both bounds of the result interval(s) belong to the safe range of CT.Real; otherwise, 

Discussion: The maximum relative error could be specified separately for each component, but we do not take advantage of that freedom here. 

Discussion: Note that f � (1.0 + d) defines a small circular region of the complex plane centered at f, and the result intervals for the real and imaginary components of the result define a small rectangular box containing that circle. 

Reason: Box error is used when the computation of the result risks loss of significance in a component due to cancellation. 

Ramification: The components of a complex function that exhibits bounded relative error in each component have to have the correct sign. In contrast, one of the components of a complex function that exhibits bounded box error may have the wrong sign, since the dimensions of the box containing the result are proportional to the modulus of the mathematical result and not to either component of the mathematical result individually. Thus, for example, the box containing the computed result of a complex function whose mathematical result has a large modulus but lies very close to the imaginary axis might well straddle that axis, allowing the real component of the computed result to have the wrong sign. In this case, the distance between the computed result and the mathematical result is, nevertheless, a small fraction of the modulus of the mathematical result. 

if CT.Real'Machine_Overflows is True, the function either delivers a value that belongs to the result interval (or a value both of whose components belong to their respective result intervals) or raises Constraint_Error, signaling overflow;

if CT.Real'Machine_Overflows is False, the result is implementation defined. 

Implementation defined: The result of a complex arithmetic operation or complex elementary function reference in overflow situations, when the Machine_Overflows attribute of the corresponding real type is False.

The error bounds for particular complex functions are tabulated below. In the table, the error bound is given as the coefficient of CT.Real'Model_Epsilon.

 

Error Bounds for Particular Complex FunctionsFunction or OperatorNature of 
ResultNature of 
BoundError BoundModulusrealmax. rel. error3.0Argumentrealmax. rel. error4.0Compose_From_Polarcomplexmax. rel. error3.0"*" (both operands complex)complexmax. box error5.0"/" (right operand complex)complexmax. box error13.0Sqrtcomplexmax. rel. error6.0Logcomplexmax. box error13.0Exp (complex parameter)complexmax. rel. error7.0Exp (imaginary parameter)complexmax. rel. error2.0Sin, Cos, Sinh, and Coshcomplexmax. rel. error11.0Tan, Cot, Tanh, and Cothcomplexmax. rel. error35.0inverse trigonometriccomplexmax. rel. error14.0inverse hyperboliccomplexmax. rel. error14.0The maximum relative error given above applies throughout the domain of the Compose_From_Polar function when the Cycle parameter is specified. When the Cycle parameter is omitted, the maximum relative error applies only when the absolute value of the parameter Argument is less than or equal to the angle threshold (see G.2.4). For the Exp function, and for the forward hyperbolic (resp., trigonometric) functions, the maximum relative error given above likewise applies only when the absolute value of the imaginary (resp., real) component of the parameter X (or the absolute value of the parameter itself, in the case of the Exp function with a parameter of pure-imaginary type) is less than or equal to the angle threshold. For larger angles, the accuracy is implementation defined. 

Implementation defined: The accuracy of certain complex arithmetic operations and certain complex elementary functions for parameters (or components thereof) beyond the angle threshold.

The prescribed results specified in G.1.2 for certain functions at particular parameter values take precedence over the error bounds; effectively, they narrow to a single value the result interval allowed by the error bounds for a component of the result. Additional rules with a similar effect are given below for certain inverse trigonometric and inverse hyperbolic functions, at particular parameter values for which a component of the mathematical result is transcendental. In each case, the accuracy rule, which takes precedence over the error bounds, is that the result interval for the stated result component is the model interval of CT.Real associated with the component's exact mathematical value. The cases in question are as follows: 

When the parameter X has the value zero, the real (resp., imaginary) component of the result of the Arccot (resp., Arccoth) function is in the model interval of CT.Real associated with the value /2.0.

When the parameter X has the value one, the real component of the result of the Arcsin function is in the model interval of CT.Real associated with the value /2.0.

When the parameter X has the value 1.0, the real component of the result of the Arcsin (resp., Arccos) function is in the model interval of CT.Real associated with the value /2.0 (resp., ). 

Discussion: It is possible to give many other prescribed results in which a component of the parameter is restricted to a similar model interval when the parameter X is appropriately restricted to an easily testable portion of the domain. We follow the proposed ISO/IEC standard for Generic_Complex_Elementary_Functions (for Ada 83) in not doing so, however. 

The amount by which a component of the result of an inverse trigonometric or inverse hyperbolic function is allowed to spill over into a quadrant adjacent to the one corresponding to the principal branch, as given in G.1.2, is limited. The rule is that the result belongs to the smallest model interval of CT.Real that contains both boundaries of the quadrant corresponding to the principal branch. This rule also takes precedence to the maximum error bounds, effectively narrowing the result interval allowed by them.

Finally, the results allowed by the error bounds are narrowed by one further rule: The absolute value of each component of the result of the Exp function, for a pure-imaginary parameter, never exceeds one. 


#### Implementation Advice

The version of the Compose_From_Polar function without a Cycle parameter should not be implemented by calling the corresponding version with a Cycle parameter of 2.0*Numerics.Pi, since this will not provide the required accuracy in some portions of the domain. 


#### Wording Changes from Ada 83

The semantics of Numerics.Generic_Complex_Types and Numerics.Generic_Complex_Elementary_Functions differs from Generic_Complex_Types and Generic_Complex_Elementary_Functions as defined in ISO/IEC CDs 13813 and 13814 (for Ada 83) in ways analogous to those identified for the elementary functions in G.2.4. In addition, we do not generally specify the signs of zero results (or result components), although those proposed standards do. 

