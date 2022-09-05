---
sidebar_position:  12
---

# 2.6  String Literals

[A [string_literal](./AA-2.6#S0016) is formed by a sequence of graphic characters (possibly none) enclosed between two quotation marks used as string brackets. They are used to represent [operator_symbol](./AA-6.1#S0202)s (see 6.1), values of a string type (see 4.2), and array subaggregates (see 4.3.3). ]


#### Syntax

string_literal<a id="S0016"></a> ::= "{[string_element](./AA-2.6#S0017)}"

string_element<a id="S0017"></a> ::= "" | non_quotation_mark_graphic_character

A [string_element](./AA-2.6#S0017) is either a pair of quotation marks (""), or a single graphic_character other than a quotation mark. 


#### Static Semantics

The sequence of characters of a [string_literal](./AA-2.6#S0016) is formed from the sequence of [string_element](./AA-2.6#S0017)s between the bracketing quotation marks, in the given order, with a [string_element](./AA-2.6#S0017) that is "" becoming a single quotation mark in the sequence of characters, and any other [string_element](./AA-2.6#S0017) being reproduced in the sequence.

A null string literal is a [string_literal](./AA-2.6#S0016) with no [string_element](./AA-2.6#S0017)s between the quotation marks.

NOTE 1   An end of line cannot appear in a [string_literal](./AA-2.6#S0016).

NOTE 2   {AI95-00285-01} No transformation is performed on the sequence of characters of a [string_literal](./AA-2.6#S0016). 


#### Examples

Examples of string literals: 

```ada
{AI95-00433-01} "Message of the day:"

""                    --  a null string literal
" "   "A"   """"      --  three string literals of length 1

"Characters such as $, %, and } are allowed in string literals"
"Archimedes said """""
"Volume of cylinder (r²h) = "

```


#### Wording Changes from Ada 83

The wording has been changed to be strictly lexical. No mention is made of string or character values, since [string_literal](./AA-2.6#S0016)s are also used to represent [operator_symbol](./AA-6.1#S0202)s, which don't have a defined value.

The syntax is described differently. 


#### Wording Changes from Ada 95

{AI95-00285-01} We explicitly say that the characters of a [string_literal](./AA-2.6#S0016) should be used as is. In particular, no normalization or folding should be performed on a [string_literal](./AA-2.6#S0016). 

