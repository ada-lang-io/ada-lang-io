---
sidebar_position:  178
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

{AI12-0445-1} a mode ("relaxed mode"), in which there are no accuracy or other numeric performance requirements to be satisfied, as for implementations not conforming to the Numerics Annex;

{AI95-00296-01} models of floating point and fixed point arithmetic on which the accuracy requirements of strict mode are based;

{AI95-00296-01} the definitions of the model-oriented attributes of floating point types that apply in the strict mode; and

{AI95-00296-01} features for the manipulation of real and complex vectors and matrices. 


#### Implementation Advice

{AI05-0229-1} If Fortran (respectively, C) is widely supported in the target environment, implementations supporting the Numerics Annex should provide the child package Interfaces.Fortran (respectively, Interfaces.C) specified in Annex B and should support a convention_[identifier](./AA-2.3#S0002) of Fortran (respectively, C) for the Convention aspect (see Annex B), thus allowing Ada programs to interface with programs written in that language. 

Implementation Advice: If Fortran (respectively, C) is supported in the target environment, then interfacing to Fortran (respectively, C) should be supported as specified in Annex B.


#### Extensions to Ada 83

This Annex is new to Ada 95. 

