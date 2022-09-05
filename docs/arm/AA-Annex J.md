---
sidebar_position:  190
---

# Annex J Obsolescent Features

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
{AI95-00368-01} [ This Annex contains descriptions of features of the language whose functionality is largely redundant with other features defined by this Reference Manual. Use of these features is not recommended in newly written programs. Use of these features can be prevented by using pragma Restrictions (No_Obsolescent_Features), see 13.12.1.] 

Ramification: These features are still part of the language, and have to be implemented by conforming implementations. The primary reason for putting these descriptions here is to get redundant features out of the way of most readers. The designers of the next version of Ada will have to assess whether or not it makes sense to drop these features from the language. 


#### Wording Changes from Ada 83

The following features have been removed from the language, rather than declared to be obsolescent: 

The package Low_Level_IO (see A.6).

The Epsilon, Mantissa, Emax, Small, Large, Safe_Emax, Safe_Small, and Safe_Large attributes of floating point types (see A.5.3).

This paragraph was deleted.{AI95-00284-02} 

The pragmas System_Name, Storage_Unit, and Memory_Size (see 13.7).

The pragma Shared (see C.6). 

Implementations can continue to support the above features for upward compatibility. 


#### Wording Changes from Ada 95

{AI95-00368-01} A mention of the No_Obsolescent_Features restriction was added. 


#### Wording Changes from Ada 2005

{AI05-0229-1} [Pragma](./AA-2.8#S0019) Controlled has been removed from the language, rather than declared to be obsolescent. No existing implementation gives it any effect. An implementation could continue to support the pragma as an implementation-defined pragma for upward compatibility. 

