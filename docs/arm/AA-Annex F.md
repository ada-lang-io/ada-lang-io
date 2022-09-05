---
sidebar_position:  174
---

# Annex F Information Systems

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
This Annex provides a set of facilities relevant to Information Systems programming. These fall into several categories: 

an attribute definition clause specifying Machine_Radix for a decimal subtype;

the package Decimal, which declares a set of constants defining the implementation's capacity for decimal types, and a generic procedure for decimal division; and

{AI95-00285-01} the child packages Text_IO.Editing, Wide_Text_IO.Editing, and Wide_Wide_Text_IO.Editing, which support formatted and localized output of decimal data, based on "picture String" values. 

{AI95-00434-01} See also: 3.5.9, "Fixed Point Types"; 3.5.10, "Operations of Fixed Point Types"; 4.6, "Type Conversions"; 13.3, "Operational and Representation Attributes"; A.10.9, "Input-Output for Real Types"; B.3, "Interfacing with C and C++"; B.4, "Interfacing with COBOL"; Annex G, "Numerics".

The character and string handling packages in Annex A, "Predefined Language Environment" are also relevant for Information Systems. 


#### Implementation Advice

{AI05-0229-1} If COBOL (respectively, C) is widely supported in the target environment, implementations supporting the Information Systems Annex should provide the child package Interfaces.COBOL (respectively, Interfaces.C) specified in Annex B and should support a convention_[identifier](./AA-2.3#S0002) of COBOL (respectively, C) for the Convention aspect (see Annex B), thus allowing Ada programs to interface with programs written in that language. 

Implementation Advice: If COBOL (respectively, C) is supported in the target environment, then interfacing to COBOL (respectively, C) should be supported as specified in Annex B.


#### Extensions to Ada 83

This Annex is new to Ada 95. 


#### Wording Changes from Ada 95

{AI95-00285-01} Added a mention of Wide_Wide_Text_IO.Editing, part of the support for 32-bit characters. 

