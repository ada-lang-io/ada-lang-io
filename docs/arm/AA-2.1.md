---
sidebar_position:  7
---

# 2.1  Character Set

{AI95-00285-01} {AI95-00395-01} {AI05-0266-1} {AI12-0263-1} The character repertoire for the text of an Ada program consists of the entire coding space described by the ISO/IEC 10646:2017 Universal Coded Character Set. This coding space is organized in planes, each plane comprising 65536 characters. 

This paragraph was deleted.{AI95-00285-01} 

This paragraph was deleted.{AI95-00285-01} 

Discussion: {AI95-00285-01} {AI05-0266-1} {AI12-0263-1} It is our intent to follow the terminology of ISO/IEC 10646:2017 where appropriate, and to remain compatible with the character classifications defined in A.3, "Character Handling". 


#### Syntax

Paragraphs 2 and 3 were deleted. 

{AI95-00285-01} {AI95-00395-01} {AI05-0266-1} {AI12-0263-1} A character is defined by this Reference Manual for each cell in the coding space described by ISO/IEC 10646:2017, regardless of whether or not ISO/IEC 10646:2017 allocates a character to that cell. 


#### Static Semantics

{AI95-00285-01} {AI95-00395-01} {AI05-0079-1} {AI05-0262-1} {AI05-0266-1} {AI12-0263-1} {AI12-0444-1} The coded representation for characters is implementation defined [(it can be a representation that is not defined within ISO/IEC 10646:2017)]. A character whose relative code point in its plane is 16#FFFE# or 16#FFFF# is not allowed anywhere in the text of a program. The only characters allowed outside of comments are those in categories other_format, format_effector, and graphic_character. 

Implementation defined: The coded representation for the text of an Ada program.

Ramification: {AI95-00285-01} Note that this rule doesn't really have much force, since the implementation can represent characters in the source in any way it sees fit. For example, an implementation could simply define that what seems to be an other_private_use character is actually a representation of the space character. 

{AI95-00285-01} {AI05-0266-1} {AI05-0299-1} {AI12-0004-1} {AI12-0263-1} The semantics of an Ada program whose text is not in Normalization Form C (as defined by Clause 21 of ISO/IEC 10646:2017) is implementation defined. 

Implementation defined: The semantics of an Ada program whose text is not in Normalization Form C.

Ramification: {AI12-0004-1} In particular, an implementation can reject such program source. It is easy during lexical analysis to reject source that contains any code point not present in Normalization Form C. Portable programs should always be encoded in Normalization Form C. 

Reason: {AI12-0004-1} Normalization Form C ensures that all source is in a unique format; it eliminates ambiguities and security issues potentially caused by source using unusual sequences of characters. Note that WC3 (the Internet standards group) recommends that all Internet content be in Normalization Form C. We don't require this as there is a potentially significant cost to checking this (just rejecting unallowed code points is not enough), and some implementations may need to be interoperable with tools that produce unnormalized text. 

{AI95-00285-01} {AI05-0266-1} {AI05-0299-1} {AI12-0263-1} The description of the language definition in this document uses the character properties General Category, Simple Uppercase Mapping, Uppercase Mapping, and Special Case Condition of the documents referenced by Clause 2 of ISO/IEC 10646:2017. The actual set of graphic symbols used by an implementation for the visual representation of the text of an Ada program is not specified. 

Discussion: {AI12-0263-1} The "documents referenced" means Unicode, Chapter 4. See the Discussion after the the character categorization definition for a source for machine-readable definitions of these properties. 

{AI95-00285-01} {AI05-0266-1} Characters are categorized as follows: 

Discussion: {AI05-0005-1} {AI05-0262-1} {AI05-0266-1} {AI12-0263-1} Our character classification considers that the cells not allocated in ISO/IEC 10646:2017 are graphic characters, except for those whose relative code point in their plane is 16#FFFE# or 16#FFFF#. This seems to provide the best compatibility with future versions of ISO/IEC 10646, as future characters can already be used in Ada character and string literals. 

This paragraph was deleted.{AI95-00285-01} 

{AI95-00285-01} letter_uppercaseAny character whose General Category is defined to be "Letter, Uppercase".

{AI95-00285-01} letter_lowercaseAny character whose General Category is defined to be "Letter, Lowercase". 

This paragraph was deleted.{8652/0001} {AI95-00124-01} 

{AI95-00285-01} letter_titlecaseAny character whose General Category is defined to be "Letter, Titlecase".

{AI95-00285-01} letter_modifierAny character whose General Category is defined to be "Letter, Modifier".

{AI95-00285-01} letter_otherAny character whose General Category is defined to be "Letter, Other".

{AI95-00285-01} mark_non_spacingAny character whose General Category is defined to be "Mark, Non-Spacing".

{AI95-00285-01} mark_spacing_combiningAny character whose General Category is defined to be "Mark, Spacing Combining".

{AI95-00285-01} number_decimalAny character whose General Category is defined to be "Number, Decimal".

{AI95-00285-01} number_letterAny character whose General Category is defined to be "Number, Letter".

{AI95-00285-01} punctuation_connectorAny character whose General Category is defined to be "Punctuation, Connector".

{AI95-00285-01} other_formatAny character whose General Category is defined to be "Other, Format".

{AI95-00285-01} separator_spaceAny character whose General Category is defined to be "Separator, Space".

{AI95-00285-01} separator_lineAny character whose General Category is defined to be "Separator, Line". 

{AI95-00285-01} separator_paragraphAny character whose General Category is defined to be "Separator, Paragraph".

{AI95-00285-01} {AI05-0262-1} format_effectorThe characters whose code points are 16#09# (CHARACTER TABULATION), 16#0A# (LINE FEED), 16#0B# (LINE TABULATION), 16#0C# (FORM FEED), 16#0D# (CARRIAGE RETURN), 16#85# (NEXT LINE), and the characters in categories separator_line and separator_paragraph. 

Discussion: ISO/IEC 10646:2003 does not define the names of control characters, but rather refers to the names defined by ISO/IEC 6429:1992. These are the names that we use here. 

{AI95-00285-01} other_controlAny character whose General Category is defined to be "Other, Control", and which is not defined to be a format_effector.

{AI95-00285-01} other_private_useAny character whose General Category is defined to be "Other, Private Use".

{AI95-00285-01} other_surrogateAny character whose General Category is defined to be "Other, Surrogate".

{AI95-00285-01} {AI95-00395-01} {AI05-0262-1} graphic_characterAny character that is not in the categories other_control, other_private_use, other_surrogate, format_effector, and whose relative code point in its plane is neither 16#FFFE# nor 16#FFFF#. 

This paragraph was deleted.

Discussion: {AI95-00285-01} We considered basing the definition of lexical elements on Annex A of ISO/IEC TR 10176 (4th edition), which lists the characters which should be supported in identifiers for all programming languages, but we finally decided against this option. Note that it is not our intent to diverge from ISO/IEC TR 10176, except to the extent that ISO/IEC TR 10176 itself diverges from ISO/IEC 10646:2003 (which is the case at the time of this writing [January 2005]).

More precisely, we intend to align strictly with ISO/IEC 10646:2003. It must be noted that ISO/IEC TR 10176 is a Technical Report while ISO/IEC 10646:2003 is a Standard. If one has to make a choice, one should conform with the Standard rather than with the Technical Report. And, it turns out that one must make a choice because there are important differences between the two:

ISO/IEC TR 10176 is still based on ISO/IEC 10646:2000 while ISO/IEC 10646:2003 has already been published for a year. We cannot afford to delay the adoption of our amendment until ISO/IEC TR 10176 has been revised.

There are considerable differences between the two editions of ISO/IEC 10646, notably in supporting characters beyond the BMP (this might be significant for some languages, e.g. Korean).

ISO/IEC TR 10176 does not define case conversion tables, which are essential for a case-insensitive language like Ada. To get case conversion tables, we would have to reference either ISO/IEC 10646:2003 or Unicode, or we would have to invent our own. 

For the purpose of defining the lexical elements of the language, we need character properties like categorization, as well as case conversion tables. These are mentioned in ISO/IEC 10646:2003 as useful for implementations, with a reference to Unicode. Machine-readable tables are available on the web at URLs: 

```ada
[http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt](http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt)
[http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt](http://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)

```

with an explanatory document found at URL: 

```ada
[http://www.unicode.org/reports/tr44/tr44-20.html](http://www.unicode.org/reports/tr44/tr44-20.html)

```

{AI12-0263-1} The actual text of the standard only makes specific references to the corresponding clauses of ISO/IEC 10646, not to Unicode.

{AI95-00285-01} {AI05-0266-1} {AI12-0263-1} The following names are used when referring to certain characters (the first name is that given in ISO/IEC 10646:2017): 

Discussion: {AI95-00285-01} {AI05-0266-1} {AI12-0125-3} {AI12-0212-1} {AI12-0263-1} This table serves to show the correspondence between ISO/IEC 10646:2017 names and the graphic symbols (glyphs) used in this document. These are the characters that play a special role in the syntax of Ada. 

  graphic symbol

         "
         #
         &
         '
         (
         )
         *
         +
         ,
         
         .
         @

name

quotation mark
number sign
ampersand
apostrophe, tick
left parenthesis
right parenthesis
asterisk, multiply
plus sign
comma
hyphen-minus, minus
full stop, dot, point
commercial at, at sign

  graphic symbol

         :
         ;
         &lt
         =
         &gt
         _
         |
         /
         !
         %
         [
         ] 

name

colon
semicolon
less-than sign
equals sign
greater-than sign
low line, underline
vertical line
solidus, divide
exclamation point
percent sign
left square bracket
right square bracket 


#### Implementation Requirements

{AI05-0286-1} An Ada implementation shall accept Ada source code in UTF-8 encoding, with or without a BOM (see A.4.11), where every character is represented by its code point. The character pair CARRIAGE RETURN/LINE FEED (code points 16#0D# 16#0A#) signifies a single end of line (see 2.2); every other occurrence of a format_effector other than the character whose code point position is 16#09# (CHARACTER TABULATION) also signifies a single end of line.

Reason: {AI05-0079-1} {AI05-0286-1} This is simply requiring that an Ada implementation be able to directly process the ACATS, which is provided in the described format. Note that files that only contain characters with code points in the first 128 (which is the majority of the ACATS) are represented in the same way in both UTF-8 and in "plain" string format. The ACATS includes a BOM in files that have any characters with code points greater than 127. Note that the BOM contains characters not legal in Ada source code, so an implementation can use that to automatically distinguish between files formatted as plain Latin-1 strings and UTF-8 with BOM.

We allow line endings to be both represented as the pair CR LF (as in Windows and the ACATS), and as single format_effector characters (usually LF, as in Linux), in order that files created by standard tools on most operating systems will meet the standard format. We specify how many line endings each represent so that compilers use the same line numbering for standard source files.

This requirement increases portability by having a format that is accepted by all Ada compilers. Note that implementations can support other source representations, including structured representations like a parse tree.


#### Implementation Permissions

{AI95-00285-01} {AI05-0266-1} The categories defined above, as well as case mapping and folding, may be based on an implementation-defined version of ISO/IEC 10646 (2003 edition or later). 

Ramification: The exact categories, case mapping, and case folding chosen affects identifiers, the result of '[[Wide_]Wide_]Image, and packages Wide_Characters.Handling and Wide_Wide_Characters.Handling. 

Discussion: This permission allows implementations to upgrade to using a newer character set standard whenever that makes sense, rather than having to wait for the next Ada Standard. But the character set standard used cannot be older than ISO/IEC 10646:2003 (which is essentially similar to Unicode 4.0). 

NOTE   {AI95-00285-01} The characters in categories other_control, other_private_use, and other_surrogate are only allowed in comments.

This paragraph was deleted.{AI05-0286-1} 


#### Extensions to Ada 83

Ada 95 allows 8-bit and 16-bit characters, as well as implementation-specified character sets. 


#### Wording Changes from Ada 83

{AI95-00285-01} {AI05-0299-1} The syntax rules in this subclause are modified to remove the emphasis on basic characters vs. others. (In this day and age, there is no need to point out that you can write programs without using (for example) lower case letters.) In particular, character (representing all characters usable outside comments) is added, and basic_graphic_character, other_special_character, and basic_character are removed. Special_character is expanded to include Ada 83's other_special_character, as well as new 8-bit characters not present in Ada 83. Ada 2005 removes special_character altogether; we want to stick to ISO/IEC 10646:2003 character classifications. Note that the term "basic letter" is used in A.3, "Character Handling" to refer to letters without diacritical marks.

{AI95-00285-01} Character names now come from ISO/IEC 10646:2003.

This paragraph was deleted.{AI95-00285-01} 


#### Extensions to Ada 95

{AI95-00285-01} {AI95-00395-01} Program text can use most characters defined by ISO-10646:2003. This subclause has been rewritten to use the categories defined in that Standard. This should ease programming in languages other than English. 


#### Inconsistencies With Ada 2005

{AI05-0299-1} {AI05-0266-1} An implementation is allowed (but not required) to use a newer character set standard to determine the categories, case mapping, and case folding. Doing so will change the results of attributes '[[Wide_]Wide_]Image and the packages [Wide_]Wide_Characters.Handling in the case of a few rarely used characters. (This also could make some identifiers illegal, for characters that are no longer classified as letters.) This is unlikely to be a problem in practice. Moreover, truly portable Ada 2012 programs should avoid using in these contexts any characters that would have different classifications in any character set standards issued since 10646:2003 (since the compiler can use any such standard as the basis for its classifications). 


#### Wording Changes from Ada 2005

{AI05-0079-1} Correction: Clarified that only characters in the categories defined here are allowed in the source of an Ada program. This was clear in Ada 95, but Amendment 1 dropped the wording instead of correcting it.

{AI05-0286-1} A standard source representation is defined that all compilers are expected to process. Since this is the same format as the ACATS, it seems unlikely that there are any implementations that don't meet this requirement. Moreover, other representations are still permitted, and the "impossible or impractical" loophole (see ) can be invoked for any implementations that cannot directly process the ACATS. 


#### Wording Changes from Ada 2012

{AI12-0004-1} Correction: The interpretation of Ada source that is in Normalization Form C but not in Normalization Form KC is no longer implementation-defined. This change could potentially change the meaning of a program for a compiler that normalized all program source to Normalization Form KC before processing it. We don't document this as an inconsistency as such handling was previously implementation defined (so any such code was already defined to be not portable), and we're not aware of any compiler that normalized source code (so we don't expect to see this problem in the real world). 

