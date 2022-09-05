---
sidebar_position:  192
---

# J.2  Allowed Replacements of Characters


#### Syntax

The following replacements are allowed for the vertical line, number sign, and quotation mark characters: 

A vertical line character (|) can be replaced by an exclamation mark (!) where used as a delimiter.

The number sign characters (#) of a [based_literal](./AA-2.4#S0011) can be replaced by colons (:) provided that the replacement is done for both occurrences. 

To be honest: {AI95-00285-01} The intent is that such a replacement works in the Value, Wide_Value, and Wide_Wide_Value attributes, and in the Get procedures of Text_IO (and Wide_Text_IO and Wide_Wide_Text_IO as well)}, so that things like "16:.123:" is acceptable. 

The quotation marks (") used as string brackets at both ends of a string literal can be replaced by percent signs (%) provided that the enclosed sequence of characters contains no quotation mark, and provided that both string brackets are replaced. Any percent sign within the sequence of characters shall then be doubled and each such doubled percent sign is interpreted as a single percent sign character value. 

These replacements do not change the meaning of the program. 

Reason: The original purpose of this feature was to support hardware (for example, teletype machines) that has long been obsolete. The feature is no longer necessary for that reason. Another use of the feature has been to replace the vertical line character (|) when using certain hardware that treats that character as a (non-English) letter. The feature is no longer necessary for that reason, either, since Ada 95 has full support for international character sets. Therefore, we believe this feature is no longer necessary.

Users of equipment that still uses | to represent a letter will continue to do so. Perhaps by next the time Ada is revised, such equipment will no longer be in use.

Note that it was never legal to use this feature as a convenient method of including double quotes in a string without doubling them - the string literal: 

```ada
%"This is quoted."%

```

{AI05-0248-1} is not legal in Ada (and never was legal). One has to write: 

```ada
"""This is quoted."""

```

