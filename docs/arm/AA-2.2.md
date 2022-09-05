---
sidebar_position:  8
---

# 2.2  Lexical Elements, Separators, and Delimiters


#### Static Semantics

The text of a program consists of the texts of one or more [compilation](./AA-10.1#S0285)s. The text of each [compilation](./AA-10.1#S0285) is a sequence of separate lexical elements. Each lexical element is formed from a sequence of characters, and is either a delimiter, an [identifier](./AA-2.3#S0002), a reserved word, a [numeric_literal](./AA-2.4#S0006), a [character_literal](./AA-2.5#S0015), a [string_literal](./AA-2.6#S0016), or a comment. The meaning of a program depends only on the particular sequences of lexical elements that form its [compilation](./AA-10.1#S0285)s, excluding [comment](./AA-2.7#S0018)s.

{AI95-00285-01} {AI05-0262-1} The text of a [compilation](./AA-10.1#S0285) is divided into lines. In general, the representation for an end of line is implementation defined. However, a sequence of one or more format_effectors other than the character whose code point is 16#09# (CHARACTER TABULATION) signifies at least one end of line. 

Implementation defined: The representation for an end of line.

{AI95-00285-01} [In some cases an explicit separator is required to separate adjacent lexical elements.] A separator is any of a separator_space, a format_effector, or the end of a line, as follows: 

{AI95-00285-01} A separator_space is a separator except within a [comment](./AA-2.7#S0018), a [string_literal](./AA-2.6#S0016), or a [character_literal](./AA-2.5#S0015).

{AI95-00285-01} {AI05-0262-1} The character whose code point is 16#09# (CHARACTER TABULATION) is a separator except within a [comment](./AA-2.7#S0018).

The end of a line is always a separator. 

One or more separators are allowed between any two adjacent lexical elements, before the first of each [compilation](./AA-10.1#S0285), or after the last. At least one separator is required between an [identifier](./AA-2.3#S0002), a reserved word, or a [numeric_literal](./AA-2.4#S0006) and an adjacent [identifier](./AA-2.3#S0002), reserved word, or [numeric_literal](./AA-2.4#S0006).

{AI05-0079-1} One or more other_format characters are allowed anywhere that a separator is[; any such characters have no effect on the meaning of an Ada program].

{AI95-00285-01} A delimiter is either one of the following characters: 

{AI12-0125-3} {AI12-0212-1} &    '    (    )    *    +    ,        .    /    :    ;    &lt    =    &gt    @    [    ]    |

or one of the following compound delimiters each composed of two adjacent special characters 

=&gt    ..    **    :=    /=    &gt=    &lt=    &lt&lt    &gt&gt    &lt&gt

Each of the special characters listed for single character delimiters is a single delimiter except if this character is used as a character of a compound delimiter, or as a character of a [comment](./AA-2.7#S0018), [string_literal](./AA-2.6#S0016), [character_literal](./AA-2.5#S0015), or [numeric_literal](./AA-2.4#S0006).

The following names are used when referring to compound delimiters:


#### Implementation Requirements

delimiter name=&gtarrow..double dot**double star, exponentiate:=assignment (pronounced: "becomes")/=inequality (pronounced: "not equal")&gt=greater than or equal&lt=less than or equal&lt&ltleft label bracket&gt&gtright label bracket&lt&gtboxAn implementation shall support lines of at least 200 characters in length, not counting any characters used to signify the end of a line. An implementation shall support lexical elements of at least 200 characters in length. The maximum supported line length and lexical element length are implementation defined. 

Implementation defined: Maximum supported line length and lexical element length.

Discussion: From URG recommendation. 


#### Wording Changes from Ada 95

{AI95-00285-01} {AI05-0299-1} The wording was updated to use the new character categories defined in the preceding subclause. 


#### Extensions to Ada 2005

{AI05-0079-1} Correction: Clarified that other_format characters are allowed anywhere that separators are allowed. This was intended in Ada 2005, but didn't actually make it into the wording. 


#### Wording Changes from Ada 2012

{AI12-0125-3} {AI12-0212-1} Added square brackets and the [target_name](./AA-5.2#S0174) symbol (see 5.2.1) to the list of delimiters. 

