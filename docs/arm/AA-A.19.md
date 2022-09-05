---
sidebar_position:  136
---

# A.19  The Package Locales

{AI05-0127-2} {AI05-0248-1} A locale identifies a geopolitical place or region and its associated language, which can be used to determine other internationalization-related characteristics. 


#### Static Semantics

{AI05-0127-2} The library package Locales has the following declaration: 

```ada
{AI12-0414-1} package Ada.Locales
   with Preelaborate, Remote_Types is

```

```ada
{AI12-0037-1}    type Language_Code is new String (1 .. 3)
      with Dynamic_Predicate =&gt
         (for all E of Language_Code =&gt E in 'a' .. 'z');
   type Country_Code is new String (1 .. 2)
      with Dynamic_Predicate =&gt
         (for all E of Country_Code  =&gt E in 'A' .. 'Z');

```

Discussion: {AI12-0037-1} These types are derived from type String so that they can easily be converted to or from type String. That's important if one of these values needs to be input or displayed (via Text_IO, perhaps). We use the predicate to ensure that only possible component values are used. Ada does not allow converting between unrelated types with components that don't statically match, so we cannot declare new types with constrained components if we want conversions to or from type String. 

```ada
   Language_Unknown : constant Language_Code := "und";
   Country_Unknown : constant Country_Code := "ZZ";

```

```ada
   function Language return Language_Code;
   function Country return Country_Code;

```

```ada
end Ada.Locales;

```

{AI05-0127-2} {AI05-0233-1} The active locale is the locale associated with the partition of the current task.

Implementation Note: {AI05-0233-1} Some environments define both a system locale and the locale of the current user. For such environments, the active locale is that of current user if any; otherwise (as in a partition running on a server without a user), the system locale should be used. 

{AI05-0127-2} Language_Code is a lower-case string representation of an ISO 639-3 alpha-3 code that identifies a language.

Discussion: Some common language codes are: "eng"  English; "fra"  French; "deu"  German; "zho"  Chinese. These are the same codes as used by POSIX systems. We considered including constants for the most common languages, but that was rejected as the likely source of continual arguments about the constant names and which languages are important enough to include. 

{AI05-0127-2} Country_Code is an upper-case string representation of an ISO 3166-1 alpha-2 code that identifies a country.

Discussion: Some common country codes are: "CA"  Canada; "FR"  France; "DE"  Germany; "IT"  Italy; "ES"  Spain; "GB"  United Kingdom; "US"  United States. These are the same codes as used by POSIX systems. We didn't include any country constants for the same reasons that we didn't include any language constants. 

{AI05-0127-2} {AI05-0248-1} Function Language returns the code of the language associated with the active locale. If the Language_Code associated with the active locale cannot be determined from the environment, then Language returns Language_Unknown.

{AI05-0127-2} {AI05-0248-1} Function Country returns the code of the country associated with the active locale. If the Country_Code associated with the active locale cannot be determined from the environment, then Country returns Country_Unknown.


#### Extensions to Ada 2005

{AI05-0127-2} {AI05-0233-1} Package Locales is new. 


#### Inconsistencies With Ada 2012

{AI12-0037-1} Corrigendum: Types Language_Code and Country_Code are defined with predicates rather than constrained components so that they can be converted to/from type String. This changes the exception raised from Constraint_Error to Assertion_Error if an assignment is attempted with an incorrect value. This could only matter if there is a handler specifically for Constraint_Error surrounding this assignment; as this exception raise is certainly caused by a bug (why would anyone want to use invalid language or country codes?), such a handler seems very unlikely. (In addition, this is a new Ada 2012 package, so there is not likely to be a lot of code using it.) 

