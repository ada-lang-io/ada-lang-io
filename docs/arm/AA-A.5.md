---
sidebar_position:  122
---

# A.5  The Numerics Packages

The library package Numerics is the parent of several child units that provide facilities for mathematical computation. One child, the generic package Generic_Elementary_Functions, is defined in A.5.1, together with nongeneric equivalents; two others, the package Float_Random and the generic package Discrete_Random, are defined in A.5.2. Additional (optional) children are defined in Annex G, "Numerics". 


#### Static Semantics

This paragraph was deleted.

```ada
{AI95-00388-01} {AI12-0414-1} package Ada.Numerics
   with Pure is
   Argument_Error : exception;
   Pi : constant :=
          3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;
     : constant := Pi;
   e  : constant :=
          2.71828_18284_59045_23536_02874_71352_66249_77572_47093_69996;
end Ada.Numerics;

```

The Argument_Error exception is raised by a subprogram in a child unit of Numerics to signal that one or more of the actual subprogram parameters are outside the domain of the corresponding mathematical function.


#### Implementation Permissions

The implementation may specify the values of Pi and e to a larger number of significant digits. 

Reason: 51 digits seem more than adequate for all present computers; converted to binary, the values given above are accurate to more than 160 bits. Nevertheless, the permission allows implementations to accommodate unforeseen hardware advances. 


#### Extensions to Ada 83

Numerics and its children were not predefined in Ada 83. 


#### Extensions to Ada 95

{AI95-00388-01} The alternative declaration of  is new. 


## A.5.1  Elementary Functions

Implementation-defined approximations to the mathematical functions known as the "elementary functions" are provided by the subprograms in Numerics.Generic_Elementary_Functions. Nongeneric equivalents of this generic package for each of the predefined floating point types are also provided as children of Numerics. 

Implementation defined: The accuracy actually achieved by the elementary functions.


#### Static Semantics

The generic library package Numerics.Generic_Elementary_Functions has the following declaration: 

```ada
{AI12-0241-1} generic
   type Float_Type is digits &lt&gt;

package Ada.Numerics.Generic_Elementary_Functions
   with Pure, Nonblocking is

```

```ada
   function Sqrt    (X           : Float_Type'Base) return Float_Type'Base;
   function Log     (X           : Float_Type'Base) return Float_Type'Base;
   function Log     (X, Base     : Float_Type'Base) return Float_Type'Base;
   function Exp     (X           : Float_Type'Base) return Float_Type'Base;
   function "**"    (Left, Right : Float_Type'Base) return Float_Type'Base;

```

```ada
   function Sin     (X           : Float_Type'Base) return Float_Type'Base;
   function Sin     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Cos     (X           : Float_Type'Base) return Float_Type'Base;
   function Cos     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Tan     (X           : Float_Type'Base) return Float_Type'Base;
   function Tan     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Cot     (X           : Float_Type'Base) return Float_Type'Base;
   function Cot     (X, Cycle    : Float_Type'Base) return Float_Type'Base;

```

```ada
   function Arcsin  (X           : Float_Type'Base) return Float_Type'Base;
   function Arcsin  (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Arccos  (X           : Float_Type'Base) return Float_Type'Base;
   function Arccos  (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Arctan  (Y           : Float_Type'Base;
                     X           : Float_Type'Base := 1.0)
                                                    return Float_Type'Base;
   function Arctan  (Y           : Float_Type'Base;
                     X           : Float_Type'Base := 1.0;
                     Cycle       : Float_Type'Base) return Float_Type'Base;
   function Arccot  (X           : Float_Type'Base;
                     Y           : Float_Type'Base := 1.0)
                                                    return Float_Type'Base;
   function Arccot  (X           : Float_Type'Base;
                     Y           : Float_Type'Base := 1.0;
                     Cycle       : Float_Type'Base) return Float_Type'Base;

```

```ada
   function Sinh    (X           : Float_Type'Base) return Float_Type'Base;
   function Cosh    (X           : Float_Type'Base) return Float_Type'Base;
   function Tanh    (X           : Float_Type'Base) return Float_Type'Base;
   function Coth    (X           : Float_Type'Base) return Float_Type'Base;
   function Arcsinh (X           : Float_Type'Base) return Float_Type'Base;
   function Arccosh (X           : Float_Type'Base) return Float_Type'Base;
   function Arctanh (X           : Float_Type'Base) return Float_Type'Base;
   function Arccoth (X           : Float_Type'Base) return Float_Type'Base;

```

```ada
end Ada.Numerics.Generic_Elementary_Functions;

```

{8652/0020} {AI95-00126-01} The library package Numerics.Elementary_Functions is declared pure and defines the same subprograms as Numerics.Generic_Elementary_Functions, except that the predefined type Float is systematically substituted for Float_Type'Base throughout. Nongeneric equivalents of Numerics.Generic_Elementary_Functions for each of the other predefined floating point types are defined similarly, with the names Numerics.Short_Elementary_Functions, Numerics.Long_Elementary_Functions, etc. 

Reason: The nongeneric equivalents are provided to allow the programmer to construct simple mathematical applications without being required to understand and use generics. 

The functions have their usual mathematical meanings. When the Base parameter is specified, the Log function computes the logarithm to the given base; otherwise, it computes the natural logarithm. When the Cycle parameter is specified, the parameter X of the forward trigonometric functions (Sin, Cos, Tan, and Cot) and the results of the inverse trigonometric functions (Arcsin, Arccos, Arctan, and Arccot) are measured in units such that a full cycle of revolution has the given value; otherwise, they are measured in radians.

The computed results of the mathematically multivalued functions are rendered single-valued by the following conventions, which are meant to imply the principal branch: 

The results of the Sqrt and Arccosh functions and that of the exponentiation operator are nonnegative.

The result of the Arcsin function is in the quadrant containing the point (1.0, x), where x is the value of the parameter X. This quadrant is I or IV; thus, the range of the Arcsin function is approximately /2.0 to /2.0 (Cycle/4.0 to Cycle/4.0, if the parameter Cycle is specified).

The result of the Arccos function is in the quadrant containing the point (x, 1.0), where x is the value of the parameter X. This quadrant is I or II; thus, the Arccos function ranges from 0.0 to approximately  (Cycle/2.0, if the parameter Cycle is specified).

The results of the Arctan and Arccot functions are in the quadrant containing the point (x, y), where x and y are the values of the parameters X and Y, respectively. This may be any quadrant (I through IV) when the parameter X (resp., Y) of Arctan (resp., Arccot) is specified, but it is restricted to quadrants I and IV (resp., I and II) when that parameter is omitted. Thus, the range when that parameter is specified is approximately  to  (Cycle/2.0 to Cycle/2.0, if the parameter Cycle is specified); when omitted, the range of Arctan (resp., Arccot) is that of Arcsin (resp., Arccos), as given above. When the point (x, y) lies on the negative x-axis, the result approximates 

 (resp., ) when the sign of the parameter Y is positive (resp., negative), if Float_Type'Signed_Zeros is True;

, if Float_Type'Signed_Zeros is False. 

(In the case of the inverse trigonometric functions, in which a result lying on or near one of the axes may not be exactly representable, the approximation inherent in computing the result may place it in an adjacent quadrant, close to but on the wrong side of the axis.) 


#### Dynamic Semantics

The exception Numerics.Argument_Error is raised, signaling a parameter value outside the domain of the corresponding mathematical function, in the following cases: 

by any forward or inverse trigonometric function with specified cycle, when the value of the parameter Cycle is zero or negative;

by the Log function with specified base, when the value of the parameter Base is zero, one, or negative;

by the Sqrt and Log functions, when the value of the parameter X is negative;

by the exponentiation operator, when the value of the left operand is negative or when both operands have the value zero;

by the Arcsin, Arccos, and Arctanh functions, when the absolute value of the parameter X exceeds one;

by the Arctan and Arccot functions, when the parameters X and Y both have the value zero;

by the Arccosh function, when the value of the parameter X is less than one; and

by the Arccoth function, when the absolute value of the parameter X is less than one. 

The exception Constraint_Error is raised, signaling a pole of the mathematical function (analogous to dividing by zero), in the following cases, provided that Float_Type'Machine_Overflows is True: 

by the Log, Cot, and Coth functions, when the value of the parameter X is zero;

by the exponentiation operator, when the value of the left operand is zero and the value of the exponent is negative;

by the Tan function with specified cycle, when the value of the parameter X is an odd multiple of the quarter cycle;

by the Cot function with specified cycle, when the value of the parameter X is zero or a multiple of the half cycle; and

by the Arctanh and Arccoth functions, when the absolute value of the parameter X is one. 

[Constraint_Error can also be raised when a finite result overflows (see G.2.4); this may occur for parameter values sufficiently near poles, and, in the case of some of the functions, for parameter values with sufficiently large magnitudes.] When Float_Type'Machine_Overflows is False, the result at poles is unspecified. 

Reason: The purpose of raising Constraint_Error (rather than Numerics.Argument_Error) at the poles of a function, when Float_Type'Machine_Overflows is True, is to provide continuous behavior as the actual parameters of the function approach the pole and finally reach it. 

Discussion: It is anticipated that an Ada binding to IEC 559:1989 will be developed in the future. As part of such a binding, the Machine_Overflows attribute of a conformant floating point type will be specified to yield False, which will permit both the predefined arithmetic operations and implementations of the elementary functions to deliver signed infinities (and set the overflow flag defined by the binding) instead of raising Constraint_Error in overflow situations, when traps are disabled. Similarly, it is appropriate for the elementary functions to deliver signed infinities (and set the zero-divide flag defined by the binding) instead of raising Constraint_Error at poles, when traps are disabled. Finally, such a binding should also specify the behavior of the elementary functions, when sensible, given parameters with infinite values. 

When one parameter of a function with multiple parameters represents a pole and another is outside the function's domain, the latter takes precedence (i.e., Numerics.Argument_Error is raised). 


#### Implementation Requirements

In the implementation of Numerics.Generic_Elementary_Functions, the range of intermediate values allowed during the calculation of a final result shall not be affected by any range constraint of the subtype Float_Type. 

Implementation Note: Implementations of Numerics.Generic_Elementary_Functions written in Ada should therefore avoid declaring local variables of subtype Float_Type; the subtype Float_Type'Base should be used instead. 

In the following cases, evaluation of an elementary function shall yield the prescribed result, provided that the preceding rules do not call for an exception to be raised: 

When the parameter X has the value zero, the Sqrt, Sin, Arcsin, Tan, Sinh, Arcsinh, Tanh, and Arctanh functions yield a result of zero, and the Exp, Cos, and Cosh functions yield a result of one.

When the parameter X has the value one, the Sqrt function yields a result of one, and the Log, Arccos, and Arccosh functions yield a result of zero.

When the parameter Y has the value zero and the parameter X has a positive value, the Arctan and Arccot functions yield a result of zero.

The results of the Sin, Cos, Tan, and Cot functions with specified cycle are exact when the mathematical result is zero; those of the first two are also exact when the mathematical result is � 1.0.

Exponentiation by a zero exponent yields the value one. Exponentiation by a unit exponent yields the value of the left operand. Exponentiation of the value one yields the value one. Exponentiation of the value zero yields the value zero. 

Other accuracy requirements for the elementary functions, which apply only in implementations conforming to the Numerics Annex, and then only in the "strict" mode defined there (see G.2), are given in G.2.4.

When Float_Type'Signed_Zeros is True, the sign of a zero result shall be as follows: 

A prescribed zero result delivered at the origin by one of the odd functions (Sin, Arcsin, Sinh, Arcsinh, Tan, Arctan or Arccot as a function of Y when X is fixed and positive, Tanh, and Arctanh) has the sign of the parameter X (Y, in the case of Arctan or Arccot).

A prescribed zero result delivered by one of the odd functions away from the origin, or by some other elementary function, has an implementation-defined sign. 

Implementation defined: The sign of a zero result from some of the operators or functions in Numerics.Generic_Elementary_Functions, when Float_Type'Signed_Zeros is True.

[A zero result that is not a prescribed result (i.e., one that results from rounding or underflow) has the correct mathematical sign.] 

Reason: This is a consequence of the rules specified in IEC 559:1989 as they apply to underflow situations with traps disabled. 


#### Implementation Permissions

{AI12-0444-1} The nongeneric equivalent packages can be actual instantiations of the generic package for the appropriate predefined type, though that is not required. 


#### Wording Changes from Ada 83

The semantics of Numerics.Generic_Elementary_Functions differs from Generic_Elementary_Functions as defined in ISO/IEC DIS 11430 (for Ada 83) in the following ways: 

The generic package is a child unit of the package defining the Argument_Error exception.

DIS 11430 specified names for the nongeneric equivalents, if provided. Here, those nongeneric equivalents are required.

Implementations are not allowed to impose an optional restriction that the generic actual parameter associated with Float_Type be unconstrained. (In view of the ability to declare variables of subtype Float_Type'Base in implementations of Numerics.Generic_Elementary_Functions, this flexibility is no longer needed.)

The sign of a prescribed zero result at the origin of the odd functions is specified, when Float_Type'Signed_Zeros is True. This conforms with recommendations of Kahan and other numerical analysts.

The dependence of Arctan and Arccot on the sign of a parameter value of zero is tied to the value of Float_Type'Signed_Zeros.

Sqrt is prescribed to yield a result of one when its parameter has the value one. This guarantee makes it easier to achieve certain prescribed results of the complex elementary functions (see G.1.2, "Complex Elementary Functions").

Conformance to accuracy requirements is conditional. 


#### Wording Changes from Ada 95

{8652/0020} {AI95-00126-01} Corrigendum: Explicitly stated that the nongeneric equivalents of Generic_Elementary_Functions are pure. 


## A.5.2  Random Number Generation

[Facilities for the generation of pseudo-random floating point numbers are provided in the package Numerics.Float_Random; the generic package Numerics.Discrete_Random provides similar facilities for the generation of pseudo-random integers and pseudo-random values of enumeration types. For brevity, pseudo-random values of any of these types are called random numbers.

Some of the facilities provided are basic to all applications of random numbers. These include a limited private type each of whose objects serves as the generator of a (possibly distinct) sequence of random numbers; a function to obtain the "next" random number from a given sequence of random numbers (that is, from its generator); and subprograms to initialize or reinitialize a given generator to a time-dependent state or a state denoted by a single integer.

Other facilities are provided specifically for advanced applications. These include subprograms to save and restore the state of a given generator; a private type whose objects can be used to hold the saved state of a generator; and subprograms to obtain a string representation of a given generator state, or, given such a string representation, the corresponding state.] 

Discussion: These facilities support a variety of requirements ranging from repeatable sequences (for debugging) to unique sequences in each execution of a program. 


#### Static Semantics

The library package Numerics.Float_Random has the following declaration: 

```ada
{AI12-0302-1} package Ada.Numerics.Float_Random
   with Global =&gt in out synchronized is

```

```ada
   -- Basic facilities

```

```ada
   type Generator is limited private;

```

```ada
{AI12-0302-1}    subtype Uniformly_Distributed is Float range 0.0 .. 1.0;
   function Random (Gen : Generator) return Uniformly_Distributed
      with Global =&gt overriding in out Gen;

```

```ada
{AI12-0302-1}    procedure Reset (Gen       : in Generator;
                    Initiator : in Integer)
      with Global =&gt overriding in out Gen;
   procedure Reset (Gen       : in Generator)
      with Global =&gt overriding in out Gen;

```

```ada
   -- Advanced facilities

```

```ada
   type State is private;

```

```ada
{AI12-0302-1}    procedure Save  (Gen        : in  Generator;
                    To_State   : out State);
   procedure Reset (Gen        : in  Generator;
                    From_State : in  State)
      with Global =&gt overriding in out Gen;

```

```ada
   Max_Image_Width : constant := implementation-defined integer value;

```

```ada
   function Image (Of_State    : State)  return String;
   function Value (Coded_State : String) return State;

```

```ada
private
   ... -- not specified by the language
end Ada.Numerics.Float_Random;

```

{AI95-00360-01} The type Generator needs finalization (see 7.6).

The generic library package Numerics.Discrete_Random has the following declaration: 

```ada
{AI12-0302-1} generic
   type Result_Subtype is (&lt&gt);
package Ada.Numerics.Discrete_Random
   with Global =&gt in out synchronized is

```

```ada
   -- Basic facilities

```

```ada
   type Generator is limited private;

```

```ada
{AI12-0302-1}    function Random (Gen : Generator) return Result_Subtype
      with Global =&gt overriding in out Gen;

```

```ada
{AI12-0144-1} {AI12-0302-1}    function Random (Gen   : Generator;
                    First : Result_Subtype;
                    Last  : Result_Subtype) return Result_Subtype
      with Post =&gt Random'Result in First .. Last,
           Global =&gt overriding in out Gen;

```

```ada
{AI12-0302-1}    procedure Reset (Gen       : in Generator;
                    Initiator : in Integer)
      with Global =&gt overriding in out Gen;
   procedure Reset (Gen       : in Generator)
      with Global =&gt overriding in out Gen;

```

```ada
   -- Advanced facilities

```

```ada
   type State is private;

```

```ada
{AI12-0302-1}    procedure Save  (Gen        : in  Generator;
                    To_State   : out State);
   procedure Reset (Gen        : in  Generator;
                    From_State : in  State)
      with Global =&gt overriding in out Gen;

```

```ada
   Max_Image_Width : constant := implementation-defined integer value;

```

```ada
   function Image (Of_State    : State)  return String;
   function Value (Coded_State : String) return State;

```

```ada
private
   ... -- not specified by the language
end Ada.Numerics.Discrete_Random;

```

Implementation defined: The value of Numerics.Float_Random.Max_Image_Width.

Implementation defined: The value of Numerics.Discrete_Random.Max_Image_Width.

Implementation Note: {8652/0097} {AI95-00115-01} The following is a possible implementation of the private part of Numerics.Float_Random (assuming the presence of "with Ada.Finalization;" as a context clause): 

```ada
type State is ...;
type Access_State is access State;
type Generator is new Finalization.Limited_Controlled with
   record
      S : Access_State := new State'(...);
   end record;
procedure Finalize (G : in out Generator);

```

{8652/0097} {AI95-00115-01} {AI95-00344-01} Numerics.Discrete_Random.Generator also can be implemented this way.

Clearly some level of indirection is required in the implementation of a Generator, since the parameter mode is in for all operations on a Generator. For this reason, Numerics.Float_Random and Numerics.Discrete_Random cannot be declared pure. 

{AI95-00360-01} The type Generator needs finalization (see 7.6) in every instantiation of Numerics.Discrete_Random.

An object of the limited private type Generator is associated with a sequence of random numbers. Each generator has a hidden (internal) state, which the operations on generators use to determine the position in the associated sequence. All generators are implicitly initialized to an unspecified state that does not vary from one program execution to another; they may also be explicitly initialized, or reinitialized, to a time-dependent state, to a previously saved state, or to a state uniquely denoted by an integer value. 

Discussion: The repeatability provided by the implicit initialization may be exploited for testing or debugging purposes. 

{AI05-0280-1} {AI12-0445-1} An object of the private type State can be used to hold the internal state of a generator. Such objects are only necessary if the application is designed to save and restore generator states or to examine or manufacture them. The implicit initial value of type State corresponds to the implicit initial value of all generators.

Discussion: {AI05-0280-1} All generators are implicitly initialized to the same unchanging value, and using Reset on a default initialized object of type State will produce a generator with that same value. 

The operations on generators affect the state and therefore the future values of the associated sequence. The semantics of the operations on generators and states are defined below. 

```ada
function Random (Gen : Generator) return Uniformly_Distributed;
function Random (Gen : Generator) return Result_Subtype;

```

{AI12-0144-1} Obtains the "next" random number from the given generator, relative to its current state, according to an implementation-defined algorithm. 

This paragraph was deleted.

Discussion: The algorithm is the subject of a Documentation Requirement, so we don't separately summarize this implementation-defined item. 

Reason: The requirement for a level of indirection in accessing the internal state of a generator arises from the desire to make Random a function, rather than a procedure. 

```ada
{AI12-0144-1} function Random (Gen   : Generator;
                 First : Result_Subtype;
                 Last  : Result_Subtype) return Result_Subtype
   with Post =&gt Random'Result in First .. Last;

```

{AI12-0144-1} Obtains the "next" random number from the given generator, relative to its current state, according to an implementation-defined algorithm. If the range First .. Last is a null range, Constraint_Error is raised.

```ada
procedure Reset (Gen       : in Generator;
                 Initiator : in Integer);
procedure Reset (Gen       : in Generator);

```

Sets the state of the specified generator to one that is an unspecified function of the value of the parameter Initiator (or to a time-dependent state, if only a generator parameter is specified). The latter form of the procedure is known as the time-dependent Reset procedure. 

Implementation Note: The time-dependent Reset procedure can be implemented by mapping the current time and date as determined by the system clock into a state, but other implementations are possible. For example, a white-noise generator or a radioactive source can be used to generate time-dependent states. 

```ada
procedure Save  (Gen        : in  Generator;
                 To_State   : out State);
procedure Reset (Gen        : in  Generator;
                 From_State : in  State);

```

Save obtains the current state of a generator. Reset gives a generator the specified state. A generator that is reset to a state previously obtained by invoking Save is restored to the state it had when Save was invoked.

```ada
function Image (Of_State    : State)  return String;
function Value (Coded_State : String) return State;

```

Image provides a representation of a state coded (in an implementation-defined way) as a string whose length is bounded by the value of Max_Image_Width. Value is the inverse of Image: Value(Image(S)) = S for each state S that can be obtained from a generator by invoking Save. 

Implementation defined: The string representation of a random number generator's state.


#### Dynamic Semantics

Instantiation of Numerics.Discrete_Random with a subtype having a null range raises Constraint_Error.

This paragraph was deleted.{8652/0050} {AI95-00089} 


#### Bounded (Run-Time) Errors

{8652/0050} {AI95-00089} {AI12-0445-1} It is a bounded error to invoke Value with a string that is not the image of any generator state. If the error is detected, Constraint_Error or Program_Error is raised. Otherwise, a call to Reset with the resulting state will produce a generator such that calls to Random with this generator will produce a sequence of values of the appropriate subtype, but which are not necessarily random in character. That is, the sequence of values do not necessarily fulfill the implementation requirements of this subclause. 


#### Implementation Requirements

{AI12-0144-1} Each call of a Random function has a result range; this is the range First .. Last for the version of Random with First and Last parameters and the range of the result subtype of the function otherwise.

{AI12-0144-1} A sufficiently long sequence of random numbers obtained by consecutive calls to Random that have the same generator and result range is approximately uniformly distributed over the result range.

Discussion: {AI12-0005-1} {AI12-0144-1} In this rule, "consecutive" means at least that there are no intervening explicit calls involving the same generator. This restricts the rule to only applying to cases where just the Random function changes the generator. We don't mean to impose a requirement if there are intervening calls to Reset, to Random with the same generator but a different result range, or any other case that would affect the sequence of values returned. Operations that use the resulting random values (for instance, to store them somewhere) are not considered in determining if calls are consecutive. 

{AI12-0144-1} A Random function in an instantiation of Numerics.Discrete_Random is guaranteed to yield each value in its result range in a finite number of calls, provided that the number of such values does not exceed 215.

Other performance requirements for the random number generator, which apply only in implementations conforming to the Numerics Annex, and then only in the "strict" mode defined there (see G.2), are given in G.2.5. 


#### Documentation Requirements

No one algorithm for random number generation is best for all applications. To enable the user to determine the suitability of the random number generators for the intended application, the implementation shall describe the algorithm used and shall give its period, if known exactly, or a lower bound on the period, if the exact period is unknown. Periods that are so long that the periodicity is unobservable in practice can be described in such terms, without giving a numerical bound. 

Documentation Requirement: The algorithm used for random number generation, including a description of its period.

The implementation also shall document the minimum time interval between calls to the time-dependent Reset procedure that are guaranteed to initiate different sequences, and it shall document the nature of the strings that Value will accept without raising Constraint_Error. 

This paragraph was deleted.

Documentation Requirement: The minimum time interval between calls to the time-dependent Reset procedure that is guaranteed to initiate different random number sequences.


#### Implementation Advice

Any storage associated with an object of type Generator should be reclaimed on exit from the scope of the object. 

Implementation Advice: Any storage associated with an object of type Generator of the random number packages should be reclaimed on exit from the scope of the object.

Ramification: A level of indirection is implicit in the semantics of the operations, given that they all take parameters of mode in. This implies that the full type of Generator probably should be a controlled type, with appropriate finalization to reclaim any heap-allocated storage. 

If the generator period is sufficiently long in relation to the number of distinct initiator values, then each possible value of Initiator passed to Reset should initiate a sequence of random numbers that does not, in a practical sense, overlap the sequence initiated by any other value. If this is not possible, then the mapping between initiator values and generator states should be a rapidly varying function of the initiator value. 

Implementation Advice: Each value of Initiator passed to Reset for the random number packages should initiate a distinct sequence of random numbers, or, if that is not possible, be at least a rapidly varying function of the initiator value.

NOTE 1   If two or more tasks are to share the same generator, then the tasks have to synchronize their access to the generator as for any shared variable (see 9.10).

NOTE 2   Within a given implementation, a repeatable random number sequence can be obtained by relying on the implicit initialization of generators or by explicitly initializing a generator with a repeatable initiator value. Different sequences of random numbers can be obtained from a given generator in different program executions by explicitly initializing the generator to a time-dependent state.

NOTE 3   {AI12-0442-1} A given implementation of the Random function in Numerics.Float_Random is not guaranteed to be capable of delivering the values 0.0 or 1.0. Applications will be more portable if they assume that these values, or values sufficiently close to them to behave indistinguishably from them, can occur. If a sequence of random integers from some range is necessary, it is preferred that the application uses one of the Random functions in an appropriate instantiation of Numerics.Discrete_Random, rather than transforming the result of the Random function in Numerics.Float_Random. 

```ada
This paragraph was deleted.{AI12-0442-1} 

```

Reason: {AI12-0442-1} One might think that a simple transformation of the result of the floating point Random function such as Integer(Float(M) * Random(G)) mod M would give a uniform distribution. But this is only true if the period of the underlying generator is a multiple of M. (This usually requires that M be a power of two.) In other cases, the mod operation maps slightly more random values to a some result values than others. It is easy to see this: consider a 4-bit random integer (with a range of 0 .. 15). If one mods this by 6 to get a value in 0 .. 5 (to which one would add 1 to get the value of a die roll), 3 values would be mapped to each value 0 .. 3, but only 2 values would be mapped to 4 and 5. Even when the input is uniformly distributed, the output clearly is not. A similar effect occurs regardless of the number of bits in the random integer. Since it takes care to get this right, users should use the provided functions (which presumably do this correctly  AI12-0144-1 contains a correct algorithm) and resist the urge to "roll-their-own". 

NOTE 4   {AI12-0442-1} Exponentially distributed (floating point) random numbers with mean and standard deviation 1.0 can be obtained by the transformation 

```ada
{AI95-00434-01}    -Log(Random(G) + Float'Model_Small)

```

where Log comes from Numerics.Elementary_Functions (see A.5.1); in this expression, the addition of Float'Model_Small avoids the exception that would be raised were Log to be given the value zero, without affecting the result (in most implementations) when Random returns a nonzero value. 


#### Examples

Example of a program that plays a simulated dice game: 

```ada
with Ada.Numerics.Discrete_Random;
procedure Dice_Game is
   subtype Die is Integer range 1 .. 6;
   subtype Dice is Integer range 2*Die'First .. 2*Die'Last;
   package Random_Die is new Ada.Numerics.Discrete_Random (Die);
   use Random_Die;
   G : Generator;
   D : Dice;
begin
   Reset (G);  -- Start the generator in a unique state in each run
   loop
      -- Roll a pair of dice; sum and process the results
      D := Random(G) + Random(G);
      ...
   end loop;
end Dice_Game;

```

Example of a program that simulates coin tosses: 

```ada
with Ada.Numerics.Discrete_Random;
procedure Flip_A_Coin is
   type Coin is (Heads, Tails);
   package Random_Coin is new Ada.Numerics.Discrete_Random (Coin);
   use Random_Coin;
   G : Generator;
begin
   Reset (G);  -- Start the generator in a unique state in each run
   loop
      -- Toss a coin and process the result
      case Random(G) is
          when Heads =&gt
             ...
          when Tails =&gt
             ...
      end case;
   ...
   end loop;
end Flip_A_Coin;

```

Example of a parallel simulation of a physical system, with a separate generator of event probabilities in each task: 

```ada
with Ada.Numerics.Float_Random;
procedure Parallel_Simulation is
   use Ada.Numerics.Float_Random;
   task type Worker is
      entry Initialize_Generator (Initiator : in Integer);
      ...
   end Worker;
   W : array (1 .. 10) of Worker;
   task body Worker is
      G : Generator;
      Probability_Of_Event : Uniformly_Distributed;
   begin
      accept Initialize_Generator (Initiator : in Integer) do
         Reset (G, Initiator);
      end Initialize_Generator;
      loop
         ...
         Probability_Of_Event := Random(G);
         ...
      end loop;
   end Worker;
begin
   -- Initialize the generators in the Worker tasks to different states
   for I in W'Range loop
      W(I).Initialize_Generator (I);
   end loop;
   ... -- Wait for the Worker tasks to terminate
end Parallel_Simulation;

```

NOTE 5   Notes on the last example: Although each Worker task initializes its generator to a different state, those states will be the same in every execution of the program. The generator states can be initialized uniquely in each program execution by instantiating Ada.Numerics.Discrete_Random for the type Integer in the main procedure, resetting the generator obtained from that instance to a time-dependent state, and then using random integers obtained from that generator to initialize the generators in each Worker task. 


#### Incompatibilities With Ada 95

{AI95-00360-01} Amendment Correction: Type Generator in Numerics.Float_Random and in an instance of Numerics.Discrete_Random is defined to need finalization. If the restriction No_Nested_Finalization (see D.7) applies to the partition, and Generator does not have a controlled part, it will not be allowed in local objects in Ada 2005 whereas it would be allowed in original Ada 95. Such code is not portable, as another Ada compiler may have a controlled part in Generator, and thus would be illegal. 


#### Wording Changes from Ada 95

{8652/0050} {AI95-00089-01} {AI05-0005-1} Corrigendum: Made the passing of an incorrect Image of a generator a bounded error, as it might not be practical to check for problems (if a generator consists of several related values). 


#### Wording Changes from Ada 2005

{AI05-0280-1} Correction: Specified the implicit initial value for (sub)type State. This was unspecified in Ada 95 and Ada 2005, so a program depending on some other initial value is very unlikely and certainly was not portable. An implementation can use default expressions, aspect Default_Value, or aspect Default_Component_Value to keep the representation of the type unchanged while meeting this new requirement. 


#### Extensions to Ada 2012

{AI12-0144-1}  The function Random with First and Last parameters is new. 


## A.5.3  Attributes of Floating Point Types


#### Static Semantics

The following representation-oriented attributes are defined for every subtype S of a floating point type T. 

S'Machine_RadixYields the radix of the hardware representation of the type T. The value of this attribute is of the type universal_integer. 

The values of other representation-oriented attributes of a floating point subtype, and of the "primitive function" attributes of a floating point subtype described later, are defined in terms of a particular representation of nonzero values called the canonical form. The canonical form (for the type T) is the form
    � mantissa � T'Machine_Radixexponent
where 

mantissa is a fraction in the number base T'Machine_Radix, the first digit of which is nonzero, and

exponent is an integer. 

S'Machine_MantissaYields the largest value of p such that every value expressible in the canonical form (for the type T), having a p-digit mantissa and an exponent between T'Machine_Emin and T'Machine_Emax, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer. 

Ramification: Values of a type held in an extended register are, in general, not machine numbers of the type, since they cannot be expressed in the canonical form with a sufficiently short mantissa. 

S'Machine_EminYields the smallest (most negative) value of exponent such that every value expressible in the canonical form (for the type T), having a mantissa of T'Machine_Mantissa digits, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer.

S'Machine_EmaxYields the largest (most positive) value of exponent such that every value expressible in the canonical form (for the type T), having a mantissa of T'Machine_Mantissa digits, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer. 

Ramification: Note that the above definitions do not determine unique values for the representation-oriented attributes of floating point types. The implementation may choose any set of values that collectively satisfies the definitions.

S'DenormYields the value True if every value expressible in the form
    � mantissa � T'Machine_RadixT'Machine_Emin
where mantissa is a nonzero T'Machine_Mantissa-digit fraction in the number base T'Machine_Radix, the first digit of which is zero, is a machine number (see 3.5.7) of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. 

The values described by the formula in the definition of S'Denorm are called denormalized numbers. A nonzero machine number that is not a denormalized number is a normalized number. A normalized number x of a given type T is said to be represented in canonical form when it is expressed in the canonical form (for the type T) with a mantissa having T'Machine_Mantissa digits; the resulting form is the canonical-form representation of x. 

Discussion: The intent is that S'Denorm be True when such denormalized numbers exist and are generated in the circumstances defined by IEC 559:1989, though the latter requirement is not formalized here. 

S'Machine_RoundsYields the value True if rounding is performed on inexact results of every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. 

Discussion: It is difficult to be more precise about what it means to round the result of a predefined operation. If the implementation does not use extended registers, so that every arithmetic result is necessarily a machine number, then rounding seems to imply two things: 

S'Model_Mantissa = S'Machine_Mantissa, so that operand preperturbation never occurs;

when the exact mathematical result is not a machine number, the result of a predefined operation must be the nearer of the two adjacent machine numbers. 

Technically, this attribute should yield False when extended registers are used, since a few computed results will cross over the half-way point as a result of double rounding, if and when a value held in an extended register has to be reduced in precision to that of the machine numbers. It does not seem desirable to preclude the use of extended registers when S'Machine_Rounds could otherwise be True. 

S'Machine_OverflowsYields the value True if overflow and divide-by-zero are detected and reported by raising Constraint_Error for every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean.

S'Signed_ZerosYields the value True if the hardware representation for the type T has the capability of representing both positively and negatively signed zeros, these being generated and used by the predefined operations of the type T as specified in IEC 559:1989; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. 

For every value x of a floating point type T, the normalized exponent of x is defined as follows: 

the normalized exponent of zero is (by convention) zero;

for nonzero x, the normalized exponent of x is the unique integer k such that T'Machine_Radixk1  |x| &lt T'Machine_Radixk. 

Ramification: The normalized exponent of a normalized number x is the value of exponent in the canonical-form representation of x.

The normalized exponent of a denormalized number is less than the value of T'Machine_Emin. 

The following primitive function attributes are defined for any subtype S of a floating point type T. 

S'ExponentS'Exponent denotes a function with the following specification: 

```ada
function S'Exponent (X : T)
  return universal_integer

```

The function yields the normalized exponent of X.

S'FractionS'Fraction denotes a function with the following specification: 

```ada
function S'Fraction (X : T)
  return T

```

The function yields the value X � T'Machine_Radixk, where k is the normalized exponent of X. A zero result[, which can only occur when X is zero,] has the sign of X. 

Discussion: Informally, when X is a normalized number, the result is the value obtained by replacing the exponent by zero in the canonical-form representation of X. 

Ramification: Except when X is zero, the magnitude of the result is greater than or equal to the reciprocal of T'Machine_Radix and less than one; consequently, the result is always a normalized number, even when X is a denormalized number. 

Implementation Note: When X is a denormalized number, the result is the value obtained by replacing the exponent by zero in the canonical-form representation of the result of scaling X up sufficiently to normalize it. 

S'ComposeS'Compose denotes a function with the following specification: 

```ada
function S'Compose (Fraction : T;
                    Exponent : universal_integer)
  return T

```

Let v be the value Fraction � T'Machine_RadixExponentk, where k is the normalized exponent of Fraction. If v is a machine number of the type T, or if |v|  T'Model_Small, the function yields v; otherwise, it yields either one of the machine numbers of the type T adjacent to v. Constraint_Error is optionally raised if v is outside the base range of S. A zero result has the sign of Fraction when S'Signed_Zeros is True. 

Discussion: Informally, when Fraction and v are both normalized numbers, the result is the value obtained by replacing the exponent by Exponent in the canonical-form representation of Fraction. 

Ramification: If Exponent is less than T'Machine_Emin and Fraction is nonzero, the result is either zero, T'Model_Small, or (if T'Denorm is True) a denormalized number. 

S'ScalingS'Scaling denotes a function with the following specification: 

```ada
function S'Scaling (X : T;
                    Adjustment : universal_integer)
  return T

```

Let v be the value X � T'Machine_RadixAdjustment. If v is a machine number of the type T, or if |v|  T'Model_Small, the function yields v; otherwise, it yields either one of the machine numbers of the type T adjacent to v. Constraint_Error is optionally raised if v is outside the base range of S. A zero result has the sign of X when S'Signed_Zeros is True. 

Discussion: Informally, when X and v are both normalized numbers, the result is the value obtained by increasing the exponent by Adjustment in the canonical-form representation of X. 

Ramification: If Adjustment is sufficiently small (i.e., sufficiently negative), the result is either zero, T'Model_Small, or (if T'Denorm is True) a denormalized number. 

S'FloorS'Floor denotes a function with the following specification: 

```ada
function S'Floor (X : T)
  return T

```

The function yields the value X, i.e., the largest (most positive) integral value less than or equal to X. When X is zero, the result has the sign of X; a zero result otherwise has a positive sign.

S'CeilingS'Ceiling denotes a function with the following specification: 

```ada
function S'Ceiling (X : T)
  return T

```

The function yields the value X, i.e., the smallest (most negative) integral value greater than or equal to X. When X is zero, the result has the sign of X; a zero result otherwise has a negative sign when S'Signed_Zeros is True.

S'RoundingS'Rounding denotes a function with the following specification: 

```ada
function S'Rounding (X : T)
  return T

```

The function yields the integral value nearest to X, rounding away from zero if X lies exactly halfway between two integers. A zero result has the sign of X when S'Signed_Zeros is True.

S'Unbiased_RoundingS'Unbiased_Rounding denotes a function with the following specification: 

```ada
function S'Unbiased_Rounding (X : T)
  return T

```

The function yields the integral value nearest to X, rounding toward the even integer if X lies exactly halfway between two integers. A zero result has the sign of X when S'Signed_Zeros is True.

S'Machine_Rounding{AI95-00267-01} S'Machine_Rounding denotes a function with the following specification: 

```ada
function S'Machine_Rounding (X : T)
  return T

```

The function yields the integral value nearest to X. If X lies exactly halfway between two integers, one of those integers is returned, but which of them is returned is unspecified. A zero result has the sign of X when S'Signed_Zeros is True. This function provides access to the rounding behavior which is most efficient on the target processor.

Discussion: We leave the rounding unspecified, so that users cannot depend on a particular rounding. This attribute is intended for use in cases where the particular rounding chosen is irrelevant. If there is a need to know which way values halfway between two integers are rounded, one of the other rounding attributes should be used. 

S'TruncationS'Truncation denotes a function with the following specification: 

```ada
function S'Truncation (X : T)
  return T

```

The function yields the value X when X is negative, and X otherwise. A zero result has the sign of X when S'Signed_Zeros is True.

S'RemainderS'Remainder denotes a function with the following specification: 

```ada
function S'Remainder (X, Y : T)
  return T

```

For nonzero Y, let v be the value X  n � Y, where n is the integer nearest to the exact value of X/Y; if |n  X/Y| = 1/2, then n is chosen to be even. If v is a machine number of the type T, the function yields v; otherwise, it yields zero. Constraint_Error is raised if Y is zero. A zero result has the sign of X when S'Signed_Zeros is True. 

Ramification: The magnitude of the result is less than or equal to one-half the magnitude of Y. 

Discussion: Given machine numbers X and Y of the type T, v is necessarily a machine number of the type T, except when Y is in the neighborhood of zero, X is sufficiently close to a multiple of Y, and T'Denorm is False. 

S'AdjacentS'Adjacent denotes a function with the following specification: 

```ada
function S'Adjacent (X, Towards : T)
  return T

```

If Towards = X, the function yields X; otherwise, it yields the machine number of the type T adjacent to X in the direction of Towards, if that machine number exists. If the result would be outside the base range of S, Constraint_Error is raised. When T'Signed_Zeros is True, a zero result has the sign of X. When Towards is zero, its sign has no bearing on the result. 

Ramification: The value of S'Adjacent(0.0, 1.0) is the smallest normalized positive number of the type T when T'Denorm is False and the smallest denormalized positive number of the type T when T'Denorm is True. 

S'Copy_SignS'Copy_Sign denotes a function with the following specification: 

```ada
function S'Copy_Sign (Value, Sign : T)
  return T

```

If the value of Value is nonzero, the function yields a result whose magnitude is that of Value and whose sign is that of Sign; otherwise, it yields the value zero. Constraint_Error is optionally raised if the result is outside the base range of S. A zero result has the sign of Sign when S'Signed_Zeros is True. 

Discussion: S'Copy_Sign is provided for convenience in restoring the sign to a quantity from which it has been temporarily removed, or to a related quantity. When S'Signed_Zeros is True, it is also instrumental in determining the sign of a zero quantity, when required. (Because negative and positive zeros compare equal in systems conforming to IEC 559:1989, a negative zero does not appear to be negative when compared to zero.) The sign determination is accomplished by transferring the sign of the zero quantity to a nonzero quantity and then testing for a negative result. 

S'Leading_PartS'Leading_Part denotes a function with the following specification: 

```ada
function S'Leading_Part (X : T;
                         Radix_Digits : universal_integer)
  return T

```

Let v be the value T'Machine_RadixkRadix_Digits, where k is the normalized exponent of X. The function yields the value 

X/v � v, when X is nonnegative and Radix_Digits is positive;

X/v � v, when X is negative and Radix_Digits is positive. 

Constraint_Error is raised when Radix_Digits is zero or negative. A zero result[, which can only occur when X is zero,] has the sign of X. 

Discussion: Informally, if X is nonzero, the result is the value obtained by retaining only the specified number of (leading) significant digits of X (in the machine radix), setting all other digits to zero. 

Implementation Note: The result can be obtained by first scaling X up, if necessary to normalize it, then masking the mantissa so as to retain only the specified number of leading digits, then scaling the result back down if X was scaled up. 

S'MachineS'Machine denotes a function with the following specification: 

```ada
function S'Machine (X : T)
  return T

```

If X is a machine number of the type T, the function yields X; otherwise, it yields the value obtained by rounding or truncating X to either one of the adjacent machine numbers of the type T. Constraint_Error is raised if rounding or truncating X to the precision of the machine numbers results in a value outside the base range of S. A zero result has the sign of X when S'Signed_Zeros is True. 

Discussion: {AI05-0005-1} All of the primitive function attributes except Rounding and Machine correspond to subprograms in the Generic_Primitive_Functions generic package that was proposed as a separate ISO standard (ISO/IEC DIS 11729) for Ada 83. The Scaling, Unbiased_Rounding, and Truncation attributes correspond to the Scale, Round, and Truncate functions, respectively, in Generic_Primitive_Functions. The Rounding attribute rounds away from zero; this functionality was not provided in Generic_Primitive_Functions. The name Round was not available for either of the primitive function attributes that perform rounding, since an attribute of that name is used for a different purpose for decimal fixed point types. Likewise, the name Scale was not available, since an attribute of that name is also used for a different purpose for decimal fixed point types. The functionality of the Machine attribute was also not provided in Generic_Primitive_Functions. The functionality of the Decompose procedure of Generic_Primitive_Functions is only provided in the form of the separate attributes Exponent and Fraction. The functionality of the Successor and Predecessor functions of Generic_Primitive_Functions is provided by the extension of the existing Succ and Pred attributes. 

Implementation Note: The primitive function attributes may be implemented either with appropriate floating point arithmetic operations or with integer and logical operations that act on parts of the representation directly. The latter is strongly encouraged when it is more efficient than the former; it is mandatory when the former cannot deliver the required accuracy due to limitations of the implementation's arithmetic operations. 

The following model-oriented attributes are defined for any subtype S of a floating point type T. 

S'Model_MantissaIf the Numerics Annex is not supported, this attribute yields an implementation defined value that is greater than or equal to d � log(10) / log(T'Machine_Radix) + 1, where d is the requested decimal precision of T, and less than or equal to the value of T'Machine_Mantissa. See G.2.2 for further requirements that apply to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_integer.

S'Model_EminIf the Numerics Annex is not supported, this attribute yields an implementation defined value that is greater than or equal to the value of T'Machine_Emin. See G.2.2 for further requirements that apply to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_integer.

S'Model_EpsilonYields the value T'Machine_Radix1  T'Model_Mantissa. The value of this attribute is of the type universal_real. 

Discussion: In most implementations, this attribute yields the absolute value of the difference between one and the smallest machine number of the type T above one which, when added to one, yields a machine number different from one. Further discussion can be found in G.2.2. 

S'Model_SmallYields the value T'Machine_RadixT'Model_Emin  1. The value of this attribute is of the type universal_real. 

Discussion: In most implementations, this attribute yields the smallest positive normalized number of the type T, i.e. the number corresponding to the positive underflow threshold. In some implementations employing a radix-complement representation for the type T, the positive underflow threshold is closer to zero than is the negative underflow threshold, with the consequence that the smallest positive normalized number does not coincide with the positive underflow threshold (i.e., it exceeds the latter). Further discussion can be found in G.2.2. 

S'ModelS'Model denotes a function with the following specification: 

```ada
function S'Model (X : T)
  return T

```

If the Numerics Annex is not supported, the meaning of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex.

S'Safe_FirstYields the lower bound of the safe range (see 3.5.7) of the type T. If the Numerics Annex is not supported, the value of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_real.

S'Safe_LastYields the upper bound of the safe range (see 3.5.7) of the type T. If the Numerics Annex is not supported, the value of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_real. 

Discussion: A predefined floating point arithmetic operation that yields a value in the safe range of its result type is guaranteed not to overflow. 

To be honest: An exception is made for exponentiation by a negative exponent in 4.5.6.

Implementation defined: The values of the Model_Mantissa, Model_Emin, Model_Epsilon, Model, Safe_First, and Safe_Last attributes, if the Numerics Annex is not supported.


#### Incompatibilities With Ada 83

The Epsilon and Mantissa attributes of floating point types are removed from the language and replaced by Model_Epsilon and Model_Mantissa, which may have different values (as a result of changes in the definition of model numbers); the replacement of one set of attributes by another is intended to convert what would be an inconsistent change into an incompatible change.

The Emax, Small, Large, Safe_Emax, Safe_Small, and Safe_Large attributes of floating point types are removed from the language. Small and Safe_Small are collectively replaced by Model_Small, which is functionally equivalent to Safe_Small, though it may have a slightly different value. The others are collectively replaced by Safe_First and Safe_Last. Safe_Last is functionally equivalent to Safe_Large, though it may have a different value; Safe_First is comparable to the negation of Safe_Large but may differ slightly from it as well as from the negation of Safe_Last. Emax and Safe_Emax had relatively few uses in Ada 83; T'Safe_Emax can be computed in the revised language as Integer'Min(T'Exponent(T'Safe_First), T'Exponent(T'Safe_Last)).

Implementations are encouraged to eliminate the incompatibilities discussed here by retaining the old attributes, during a transition period, in the form of implementation-defined attributes with their former values. 


#### Extensions to Ada 83

The Model_Emin attribute is new. It is conceptually similar to the negation of Safe_Emax attribute of Ada 83, adjusted for the fact that the model numbers now have the hardware radix. It is a fundamental determinant, along with Model_Mantissa, of the set of model numbers of a type (see G.2.1).

The Denorm and Signed_Zeros attributes are new, as are all of the primitive function attributes. 


#### Extensions to Ada 95

{AI95-00388-01} The Machine_Rounding attribute is new. 


## A.5.4  Attributes of Fixed Point Types


#### Static Semantics

The following representation-oriented attributes are defined for every subtype S of a fixed point type T. 

S'Machine_RadixYields the radix of the hardware representation of the type T. The value of this attribute is of the type universal_integer.

S'Machine_RoundsYields the value True if rounding is performed on inexact results of every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean.

S'Machine_OverflowsYields the value True if overflow and divide-by-zero are detected and reported by raising Constraint_Error for every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. 


#### Incompatibilities With Ada 83

The Mantissa, Large, Safe_Small, and Safe_Large attributes of fixed point types are removed from the language.

Implementations are encouraged to eliminate the resulting incompatibility by retaining these attributes, during a transition period, in the form of implementation-defined attributes with their former values. 


#### Extensions to Ada 83

The Machine_Radix attribute is now allowed for fixed point types. It is also specifiable in an attribute definition clause (see F.1). 


## A.5.5  Big Numbers

{AI12-0208-1} Support is provided for integer arithmetic involving values larger than those supported by the target machine, and for arbitrary-precision real numbers. 


#### Static Semantics

{AI12-0208-1} The library package Numerics.Big_Numbers has the following declaration:

```ada
package Ada.Numerics.Big_Numbers
   with Pure, Nonblocking, Global =&gt null is
   subtype Field is Integer range 0 .. implementation-defined;
   subtype Number_Base is Integer range 2 .. 16;
end Ada.Numerics.Big_Numbers;

```


#### Extensions to Ada 2012

{AI12-0208-1} The package Numerics.Big_Numbers is new. 


## A.5.6  Big Integers


#### Static Semantics

{AI12-0208-1} {AI12-0366-1} The library package Numerics.Big_Numbers.Big_Integers has the following declaration:

```ada
with Ada.Strings.Text_Buffers;
package Ada.Numerics.Big_Numbers.Big_Integers
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI12-0208-1} {AI12-0366-1} {AI12-0407-1}    type Big_Integer is private
     with Integer_Literal =&gt From_Universal_Image,
          Put_Image =&gt Put_Image;

```

```ada
   function Is_Valid (Arg : Big_Integer) return Boolean
      with Convention =&gt Intrinsic;

```

Discussion: {AI12-0005-1} The result of Is_Valid on a default-initialized object of type Big_Integer is unspecified, analogous to the value of a Valid [attribute_reference](./AA-4.1#S0100) applied to a default-initialized object of an integer type (see 13.9.2). The language-provided functions in the Big_Integers package only return values for which Is_Valid is certain to be True. 

```ada
   subtype Valid_Big_Integer is Big_Integer
      with Dynamic_Predicate =&gt Is_Valid (Valid_Big_Integer),
           Predicate_Failure =&gt (raise Program_Error);

```

```ada
   function "=" (L, R : Valid_Big_Integer) return Boolean;
   function "&lt" (L, R : Valid_Big_Integer) return Boolean;
   function "&lt=" (L, R : Valid_Big_Integer) return Boolean;
   function "&gt" (L, R : Valid_Big_Integer) return Boolean;
   function "&gt=" (L, R : Valid_Big_Integer) return Boolean;

```

```ada
   function To_Big_Integer (Arg : Integer) return Valid_Big_Integer;

```

```ada
   subtype Big_Positive is Big_Integer
      with Dynamic_Predicate =&gt (if Is_Valid (Big_Positive)
                                 then Big_Positive &gt 0),
           Predicate_Failure =&gt (raise Constraint_Error);

```

```ada
   subtype Big_Natural is Big_Integer
      with Dynamic_Predicate =&gt (if Is_Valid (Big_Natural)
                                 then Big_Natural &gt= 0),
           Predicate_Failure =&gt (raise Constraint_Error);

```

```ada
   function In_Range (Arg, Low, High : Valid_Big_Integer) return Boolean is
     (Low &lt= Arg and Arg &lt= High);

```

```ada
   function To_Integer (Arg : Valid_Big_Integer) return Integer
      with Pre =&gt In_Range (Arg,
                            Low  =&gt To_Big_Integer (Integer'First),
                            High =&gt To_Big_Integer (Integer'Last))
                   or else raise Constraint_Error;

```

```ada
   generic
      type Int is range &lt&gt;
   package Signed_Conversions is
      function To_Big_Integer (Arg : Int) return Valid_Big_Integer;
      function From_Big_Integer (Arg : Valid_Big_Integer) return Int
         with Pre =&gt In_Range (Arg,
                               Low  =&gt To_Big_Integer (Int'First),
                               High =&gt To_Big_Integer (Int'Last))
                      or else raise Constraint_Error;
   end Signed_Conversions;

```

```ada
   generic
      type Int is mod &lt&gt;
   package Unsigned_Conversions is
      function To_Big_Integer (Arg : Int) return Valid_Big_Integer;
      function From_Big_Integer (Arg : Valid_Big_Integer) return Int
         with Pre =&gt In_Range (Arg,
                               Low  =&gt To_Big_Integer (Int'First),
                               High =&gt To_Big_Integer (Int'Last))
                      or else raise Constraint_Error;
   end Unsigned_Conversions;

```

```ada
   function To_String (Arg : Valid_Big_Integer;
                       Width : Field := 0;
                       Base  : Number_Base := 10) return String
      with Post =&gt To_String'Result'First = 1;

```

```ada
   function From_String (Arg : String) return Valid_Big_Integer;

```

```ada
{AI12-0407-1}    function From_Universal_Image (Arg : String) return Valid_Big_Integer
      renames From_String;

```

```ada
   procedure Put_Image
     (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in Valid_Big_Integer);

```

```ada
   function "+" (L : Valid_Big_Integer) return Valid_Big_Integer;
   function "-" (L : Valid_Big_Integer) return Valid_Big_Integer;
   function "abs" (L : Valid_Big_Integer) return Valid_Big_Integer;
   function "+" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
   function "-" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
   function "*" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
   function "/" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
   function "mod" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
   function "rem" (L, R : Valid_Big_Integer) return Valid_Big_Integer;
   function "**" (L : Valid_Big_Integer; R : Natural)
      return Valid_Big_Integer;
   function Min (L, R : Valid_Big_Integer) return Valid_Big_Integer;
   function Max (L, R : Valid_Big_Integer) return Valid_Big_Integer;

```

```ada
   function Greatest_Common_Divisor
     (L, R : Valid_Big_Integer) return Big_Positive
     with Pre =&gt (L /= 0 and R /= 0) or else raise Constraint_Error;

```

```ada
private
   ... -- not specified by the language
end Ada.Numerics.Big_Numbers.Big_Integers;

```

{AI12-0208-1} {AI12-0366-1} To_String and From_String behave analogously to the Put and Get procedures defined in Text_IO.Integer_IO (in particular, with respect to the interpretation of the Width and Base parameters) except that Constraint_Error, not Data_Error, is propagated in error cases and the result of a call to To_String with a Width parameter of 0 and a nonnegative Arg parameter does not include a leading blank. Put_Image calls To_String (passing in the default values for the Width and Base parameters), prepends a leading blank if the argument is nonnegative, and writes the resulting value to the buffer using Text_Buffers.Put.

{AI12-0208-1} The other functions have their usual mathematical meanings.

{AI12-0208-1} {AI12-0366-1} The type Big_Integer needs finalization (see 7.6). 


#### Dynamic Semantics

{AI12-0208-1} {AI12-0366-1} For purposes of determining whether predicate checks are performed as part of default initialization, the type Big_Integer is considered to have a subcomponent that has a [default_expression](./AA-3.7#S0063).

Ramification: {AI12-0005-1} {AI12-0208-1} This means that the elaboration of 

```ada
Default_Initialized_Object : Valid_Big_Integer;

```

either produces a value for which Is_Valid is True. or it propagates Program_Error. 


#### Implementation Requirements

{AI12-0208-1} {AI12-0366-1} No storage associated with a Big_Integer object shall be lost upon assignment or scope exit.

Implementation Note: {AI12-0208-1} The "No storage ... shall be lost" requirement does not preclude implementation techniques such as caching or unique number tables. 


#### Extensions to Ada 2012

{AI12-0208-1} {AI12-0366-1} The package Numerics.Big_Numbers.Big_Integers is new. 


## A.5.7  Big Reals


#### Static Semantics

{AI12-0208-1} {AI12-0366-1} The library package Numerics.Big_Numbers.Big_Reals has the following declaration:

```ada
with Ada.Numerics.Big_Numbers.Big_Integers;
   use all type Big_Integers.Big_Integer;
with Ada.Strings.Text_Buffers;
package Ada.Numerics.Big_Numbers.Big_Reals
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI12-0208-1} {AI12-0366-1} {AI12-0407-1}    type Big_Real is private
      with Real_Literal =&gt From_Universal_Image,
           Put_Image =&gt Put_Image;

```

```ada
   function Is_Valid (Arg : Big_Real) return Boolean
      with Convention =&gt Intrinsic;

```

Discussion: {AI12-0005-1} The result of Is_Valid on a default-initialized object of type Big_Real is unspecified, analogous to the value of a Valid [attribute_reference](./AA-4.1#S0100) applied to a default-initialized object of a real type (see 13.9.2). The language-provided functions in the Big_Reals package only return values for which Is_Valid is certain to be True. 

```ada
   subtype Valid_Big_Real is Big_Real
      with Dynamic_Predicate =&gt Is_Valid (Valid_Big_Real),
           Predicate_Failure =&gt raise Program_Error;

```

```ada
   function "/" (Num, Den : Big_Integers.Valid_Big_Integer)
      return Valid_Big_Real
      with Pre =&gt Den /= 0
                   or else raise Constraint_Error;

```

```ada
   function Numerator
      (Arg : Valid_Big_Real) return Big_Integers.Valid_Big_Integer
     with Post =&gt (if Arg = 0.0 then Numerator'Result = 0);

```

Reason: The postcondition of Numerator cannot be complete as it cannot mention Denominator. Since the postcondition of Denominator uses Numerator, we would get an infinite mutual recursion if both postconditions are enabled. The postcondition of Denominator serves as the postcondition for Numerator as well unless Arg = 0.0. 

```ada
   function Denominator (Arg : Valid_Big_Real)
      return Big_Integers.Big_Positive
      with Post =&gt
        (if Arg = 0.0 then Denominator'Result = 1
         else Big_Integers.Greatest_Common_Divisor
                (Numerator (Arg), Denominator'Result) = 1);

```

```ada
   function To_Big_Real (Arg : Big_Integers.Valid_Big_Integer)
      return Valid_Big_Real is (Arg / 1);

```

```ada
   function To_Real (Arg : Integer) return Valid_Big_Real is
      (Big_Integers.To_Big_Integer (Arg) / 1);

```

```ada
   function "=" (L, R : Valid_Big_Real) return Boolean;
   function "&lt" (L, R : Valid_Big_Real) return Boolean;
   function "&lt=" (L, R : Valid_Big_Real) return Boolean;
   function "&gt" (L, R : Valid_Big_Real) return Boolean;
   function "&gt=" (L, R : Valid_Big_Real) return Boolean;

```

```ada
   function In_Range (Arg, Low, High : Valid_Big_Real) return Boolean is
      (Low &lt= Arg and Arg &lt= High);

```

```ada
   generic
      type Num is digits &lt&gt;
   package Float_Conversions is
      function To_Big_Real (Arg : Num) return Valid_Big_Real;
      function From_Big_Real (Arg : Valid_Big_Real) return Num
         with Pre =&gt In_Range (Arg,
                               Low  =&gt To_Big_Real (Num'First),
                               High =&gt To_Big_Real (Num'Last))
                     or else (raise Constraint_Error);
   end Float_Conversions;

```

```ada
   generic
      type Num is delta &lt&gt;
   package Fixed_Conversions is
      function To_Big_Real (Arg : Num) return Valid_Big_Real;
      function From_Big_Real (Arg : Valid_Big_Real) return Num
         with Pre =&gt In_Range (Arg,
                               Low  =&gt To_Big_Real (Num'First),
                               High =&gt To_Big_Real (Num'Last))
                     or else (raise Constraint_Error);
   end Fixed_Conversions;

```

```ada
   function To_String (Arg  : Valid_Big_Real;
                       Fore : Field := 2;
                       Aft  : Field := 3;
                       Exp  : Field := 0) return String
      with Post =&gt To_String'Result'First = 1;

```

```ada
   function From_String (Arg   : String) return Valid_Big_Real;

```

```ada
{AI12-0407-1}    function From_Universal_Image (Arg : String) return Valid_Big_Real
      renames From_String;

```

```ada
{AI12-0407-1}    function From_Universal_Image (Num, Den : String)
      return Valid_Big_Real is
         (Big_Integers.From_Universal_Image (Num) /
          Big_Integers.From_Universal_Image (Den));

```

```ada
   function To_Quotient_String (Arg : Valid_Big_Real) return String is
      (To_String (Numerator (Arg)) & " / " & To_String (Denominator (Arg)));
   function From_Quotient_String (Arg : String) return Valid_Big_Real;

```

```ada
   procedure Put_Image
     (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : in Valid_Big_Real);

```

```ada
   function "+" (L : Valid_Big_Real) return Valid_Big_Real;
   function "-" (L : Valid_Big_Real) return Valid_Big_Real;
   function "abs" (L : Valid_Big_Real) return Valid_Big_Real;
   function "+" (L, R : Valid_Big_Real) return Valid_Big_Real;
   function "-" (L, R : Valid_Big_Real) return Valid_Big_Real;
   function "*" (L, R : Valid_Big_Real) return Valid_Big_Real;
   function "/" (L, R : Valid_Big_Real) return Valid_Big_Real;
   function "**" (L : Valid_Big_Real; R : Integer)
      return Valid_Big_Real;
   function Min (L, R : Valid_Big_Real) return Valid_Big_Real;
   function Max (L, R : Valid_Big_Real) return Valid_Big_Real;

```

```ada
private
   ... -- not specified by the language
end Ada.Numerics.Big_Numbers.Big_Reals;

```

{AI12-0208-1} {AI12-0366-1} To_String and From_String behave analogously to the Put and Get procedures defined in Text_IO.Float_IO (in particular, with respect to the interpretation of the Fore, Aft, and Exp parameters), except that Constraint_Error (not Data_Error) is propagated in error cases. From_Quotient_String implements the inverse function of To_Quotient_String; Constraint_Error is propagated in error cases. Put_Image calls To_String, and writes the resulting value to the buffer using Text_Buffers.Put.

{AI12-0208-1} For an instance of Float_Conversions or Fixed_Conversions, To_Big_Real is exact (that is, the result represents exactly the same mathematical value as the argument) and From_Big_Real is subject to the same precision rules as a type conversion of a value of type T to the target type Num, where T is a hypothetical floating point type whose model numbers include all of the model numbers of Num as well as the exact mathematical value of the argument.

{AI12-0208-1} The other functions have their usual mathematical meanings.

{AI12-0208-1} {AI12-0366-1} The type Big_Real needs finalization (see 7.6).


#### Dynamic Semantics

{AI12-0208-1} {AI12-0366-1} For purposes of determining whether predicate checks are performed as part of default initialization, the type Big_Real is considered to have a subcomponent that has a [default_expression](./AA-3.7#S0063).

Ramification: {AI12-0005-1} {AI12-0208-1} This means that the elaboration of 

```ada
Default_Initialized_Object : Valid_Big_Real;

```

either produces a value for which Is_Valid is True. or it propagates Program_Error. 


#### Implementation Requirements

{AI12-0208-1} {AI12-0366-1} No storage associated with a Big_Real object shall be lost upon assignment or scope exit.

Implementation Note: {AI12-0208-1} The "No storage ... shall be lost" requirement does not preclude implementation techniques such as caching or unique number tables. 


#### Extensions to Ada 2012

{AI12-0208-1} {AI12-0366-1} The package Numerics.Big_Numbers.Big_Reals is new. 

