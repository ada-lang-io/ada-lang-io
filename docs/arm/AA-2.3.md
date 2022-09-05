---
sidebar_position:  9
---

# 2.3  Identifiers

[Identifier](./AA-2.3#S0002)s are used as names. 


#### Syntax

{AI95-00285-01} {AI95-00395-01} identifier<a id="S0002"></a> ::= 
   [identifier_start](./AA-2.3#S0003) {[identifier_start](./AA-2.3#S0003) | [identifier_extend](./AA-2.3#S0005)}

{AI95-00285-01} {AI95-00395-01} identifier_start<a id="S0003"></a><a id="S0004"></a> ::= 
     letter_uppercase
   | letter_lowercase
   | letter_titlecase
   | letter_modifier
   | letter_other
   | number_letter

{AI95-00285-01} {AI95-00395-01} {AI05-0091-1} identifier_extend<a id="S0005"></a> ::= 
     mark_non_spacing
   | mark_spacing_combining
   | number_decimal
   | punctuation_connector

{AI95-00395-01} {AI05-0091-1} An [identifier](./AA-2.3#S0002) shall not contain two consecutive characters in category punctuation_connector, or end with a character in that category. 

Reason: This rule was stated in the syntax in Ada 95, but that has gotten too complex in Ada 2005. 


#### Legality Rules

{AI12-0004-1} {AI12-0263-1} An identifier shall only contain characters that may be present in Normalization Form KC (as defined by Clause 21 of ISO/IEC 10646:2017).

Implementation Note: {AI12-0004-1} {AI12-0005-1} An implementation can usually detect this during lexical processing. The code points not allowed are those for which Unicode property NFKC_QC (Normalization Form KC Quick_Check) has the value No. We say "may be present" so that characters for which the value is Maybe (really, one of the possible values is Maybe) are allowed (these are mainly combining marks). The necessary tables can be found in [http://www.unicode.org/Public/UCD/latest/ucd/DerivedNormalizationProps.txt](http://www.unicode.org/Public/UCD/latest/ucd/DerivedNormalizationProps.txt). Versions for older Unicode versions can be found on this site as well; start at [http://www.unicode.org/Public/](http://www.unicode.org/Public/) and find the appropriate version number. 


#### Static Semantics

{AI95-00285-01} {AI05-0091-1} {AI05-0227-1} {AI05-0266-1} {AI05-0299-1} {AI12-0263-1} Two [identifier](./AA-2.3#S0002)s are considered the same if they consist of the same sequence of characters after applying locale-independent simple case folding, as defined by documents referenced in Clause 2 of ISO/IEC 10646:2017. 

Discussion: {AI05-0227-1} Simple case folding is a mapping to lower case, so this is matching the defining (lower case) version of a reserved word. We could have mentioned case folding of the reserved words, but as that is an identity function, it would have no effect.

{AI05-0227-1} {AI12-0263-1} The "documents referenced" means Unicode, Chapter 4 (specifically, section 4.2 - Case). Note that simple case folding is supposed to be compatible between Unicode versions, so the Unicode version used doesn't matter. A machine-readable version of the needed mapping can be found at: [http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt](http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt). 

{AI95-00395-01} {AI05-0091-1} {AI05-0227-1} After applying simple case folding, an [identifier](./AA-2.3#S0002) shall not be identical to a reserved word. 

Implementation Note: We match the reserved words after applying case folding so that the rules for [identifier](./AA-2.3#S0002)s and reserved words are the same. Since a compiler usually will lexically process [identifier](./AA-2.3#S0002)s and reserved words the same way (often with the same code), this will prevent a lot of headaches. 

Ramification: {AI05-0227-1} The rules for reserved words differ in one way: they define case conversion on letters rather than sequences. This means that it is possible that there exist some unusual sequences that are neither [identifier](./AA-2.3#S0002)s nor reserved words. We are not aware of any such sequences so long as we use simple case folding (as opposed to full case folding), but we have defined the rules in case any are introduced in future character set standards. This originally was a problem when converting to upper case: "f" and "acceﬂ" have upper case conversions of "IF" and "ACCESS" respectively. We would not want these to be treated as reserved words. But neither of these cases exist when using simple case folding. 


#### Implementation Permissions

In a nonstandard mode, an implementation may support other upper/lower case equivalence rules for [identifier](./AA-2.3#S0002)s[, to accommodate local conventions]. 

Discussion: {AI95-00285-01} {AI05-0227-1} For instance, in most languages, the simple case folded equivalent of LATIN CAPITAL LETTER I (an upper case letter without a dot above) is LATIN SMALL LETTER I (a lower case letter with a dot above). In Turkish, though, LATIN CAPITAL LETTER I and LATIN CAPITAL LETTER I WITH DOT ABOVE are two distinct letters, so the case folded equivalent of LATIN CAPITAL LETTER I is LATIN SMALL LETTER DOTLESS I, and the case folded equivalent of LATIN CAPITAL LETTER I WITH DOT ABOVE is LATIN SMALL LETTER I. Take for instance the following identifier (which is the name of a city on the Tigris river in Eastern Anatolia):

```ada
DYARBAKIR -- The first i is dotted, the second isn't.

```

A Turkish reader would expect that the above identifier is equivalent to: 

```ada
diyarbakr

```

However, locale-independent simple case folding (and thus Ada) maps this to: 

```ada
dyarbakir

```

which is different from any of the following identifiers: 

```ada
diyarbakir
diyarbakr
dyarbakir
dyarbakr

```

including the "correct" matching identifier for Turkish. Upper case conversion (used in '[Wide_]Wide_Image) introduces additional problems.

An implementation targeting the Turkish market is allowed (in fact, expected) to provide a nonstandard mode where case folding is appropriate for Turkish. 

Lithuanian and Azeri are two other languages that present similar idiosyncrasies. 

NOTE 1   {AI95-00285-01} [Identifier](./AA-2.3#S0002)s differing only in the use of corresponding upper and lower case letters are considered the same. 


#### Examples

Examples of identifiers: 

```ada
{AI95-00433-01} Count      X    Get_Symbol   Ethelyn   Marion
Snobol_4   X1   Page_Count   Store_Next_Item
      -- Plato
  -- Tchaikovsky
          -- Angles

```


#### Wording Changes from Ada 83

We no longer include reserved words as [identifier](./AA-2.3#S0002)s. This is not a language change. In Ada 83, [identifier](./AA-2.3#S0002) included reserved words. However, this complicated several other rules (for example, regarding implementation-defined attributes and pragmas, etc.). We now explicitly allow certain reserved words for attribute designators, to make up for the loss. 

Ramification: Because syntax rules are relevant to overload resolution, it means that if it looks like a reserved word, it is not an [identifier](./AA-2.3#S0002). As a side effect, implementations cannot use reserved words as implementation-defined attributes or pragma names. 


#### Extensions to Ada 95

{AI95-00285-01} An [identifier](./AA-2.3#S0002) can use any letter defined by ISO-10646:2003, along with several other categories. This should ease programming in languages other than English. 


#### Incompatibilities With Ada 2005

{AI05-0091-1} Correction: other_format characters were removed from identifiers as the Unicode recommendations have changed. This change can only affect programs written for the original Ada 2005, so there should be few such programs.

{AI05-0227-1} Correction: We now specify simple case folding rather than full case folding. That potentially could change identifier equivalence, although it is more likely that identifiers that are considered the same in original Ada 2005 will now be considered different. This change was made because the original Ada 2005 definition was incompatible (and even inconsistent in unusual cases) with the Ada 95 identifier equivalence rules. As such, the Ada 2005 rules were rarely fully implemented, and in any case, only Ada 2005 identifiers containing wide characters could be affected. 


#### Incompatibilities With Ada 2012

{AI12-0004-1} Correction: An identifier that contains any characters not permitted in Normalization Form KC is now illegal. Ada 2012 allowed such identifiers, but their interpretation was implementation-defined (so the use of such identifiers was not portable). Identifiers that may be interpreted differently by different compilers are a safety and security hazard, so we no longer allow them. 

