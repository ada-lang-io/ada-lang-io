---
sidebar_position:  2
---

# 1 General

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
Discussion: This Annotated Ada Reference Manual (AARM) contains the entire text of the third edition of the Ada Reference Manual as updated for Ada 2022 (referred to here as the Ada 2022 RM), plus certain annotations. The annotations give a more in-depth analysis of the language. They describe the reason for each nonobvious rule, and point out interesting ramifications of the rules and interactions among the rules (interesting to language lawyers, that is). Differences between Ada 83, Ada 95, Ada 2005, Ada 2012, and Ada 2022 are listed. (The text you are reading now is an annotation.)

The AARM stresses detailed correctness and uniformity over readability and understandability. We're not trying to make the language "appear" simple here; on the contrary, we're trying to expose hidden complexities, so we can more easily detect language bugs. The Ada 2022 RM, on the other hand, is intended to be a more readable document for programmers.

The annotations in the AARM are as follows: 

Text that is logically redundant is shown [in square brackets, like this]. Technically, such text could be written as a Note in the Ada 2022 RM (and the Ada 95 and 2005 RMs before it), since it is really a theorem that can be proven from the nonredundant rules of the language. We use the square brackets instead when it seems to make the Ada 2022 RM more readable.

The rules of the language (and some AARM-only text) are categorized, and placed under certain sub-headings that indicate the category. For example, the distinction between Name Resolution Rules and Legality Rules is particularly important, as explained in 8.6.

Text under the following sub-headings appears in both documents: 

The unlabeled text at the beginning of each clause or subclause,

Syntax,

Name Resolution Rules,

Legality Rules,

Static Semantics,

Post-Compilation Rules,

Dynamic Semantics,

Bounded (Run-Time) Errors,

Erroneous Execution,

Implementation Requirements,

Documentation Requirements,

Metrics,

Implementation Permissions,

Implementation Advice,

NOTES,

Examples. 

Text under the following sub-headings does not appear in the Ada 2022 RM: 

Language Design Principles,

Inconsistencies With Ada 83,

Incompatibilities With Ada 83,

Extensions to Ada 83,

Wording Changes from Ada 83,

Inconsistencies With Ada 95,

Incompatibilities With Ada 95,

Extensions to Ada 95,

Wording Changes from Ada 95,

Inconsistencies With Ada 2005,

Incompatibilities With Ada 2005,

Extensions to Ada 2005,

Wording Changes from Ada 2005,

Inconsistencies With Ada 2012,

Incompatibilities With Ada 2012,

Extensions to Ada 2012,

Wording Changes from Ada 2012. 

The AARM also includes the following kinds of annotations. These do not necessarily annotate the immediately preceding rule, although they often do. 

Reason: An explanation of why a certain rule is necessary, or why it is worded in a certain way. 

Ramification: An obscure ramification of the rules that is of interest only to language lawyers. (If a ramification of the rules is of interest to programmers, then it appears under NOTES.) 

Proof: An informal proof explaining how a given Note or [marked-as-redundant] piece of text follows from the other rules of the language. 

Implementation Note: A hint about how to implement a feature, or a particular potential pitfall that an implementer needs to be aware of.

Change: Change annotations are not used in this version. Changes from previous versions have been removed. Changes in this version are marked with versioned paragraph numbers, as explained in the "Corrigendum Changes" clause of the "Introduction". 

Discussion: Other annotations not covered by the above. 

To be honest: A rule that is considered logically necessary to the definition of the language, but which is so obscure or pedantic that only a language lawyer would care. These are the only annotations that could be considered part of the language definition. 

This paragraph was deleted.{AI12-0443-1} 

{AI12-0443-1} The text of a Term and Definition entry - this text will also appear in 1.3, "Terms and Definitions". 

Discussion: In general, the Ada 2022 RM text appears in the normal font, whereas AARM-only text appears in a smaller font. Notes also appear in the smaller font, as recommended by ISO/IEC style guidelines. Ada examples are also usually printed in a smaller font.

If you have trouble finding things, be sure to use the index. Each defined term appears there, and also in italics, like this. Syntactic categories defined in BNF are also indexed.

A definition marked "[distributed]" is the main definition for a term whose complete definition is given in pieces distributed throughout the document. The pieces are marked "[partial]" or with a phrase explaining what cases the partial definition applies to. 

