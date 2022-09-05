---
sidebar_position:  120
---

# A.3  Character Handling

{AI95-00285-01} {AI05-0243-1} {AI05-0299-1} This subclause presents the packages related to character processing: an empty declared pure package Characters and child packages Characters.Handling and Characters.Latin_1. The package Characters.Handling provides classification and conversion functions for Character data, and some simple functions for dealing with Wide_Character and Wide_Wide_Character data. The child package Characters.Latin_1 declares a set of constants initialized to values of type Character. 


#### Extensions to Ada 83

{AI05-0299-1} This subclause is new to Ada 95. 


#### Wording Changes from Ada 95

{AI95-00285-01} Included Wide_Wide_Character in this description; the individual changes are documented as extensions as needed. 


## A.3.1  The Packages Characters, Wide_Characters, and Wide_Wide_Characters


#### Static Semantics

The library package Characters has the following declaration: 

```ada
{AI12-0414-1} package Ada.Characters
   with Pure is
end Ada.Characters;

```

{AI95-00395-01} The library package Wide_Characters has the following declaration: 

```ada
{AI12-0414-1} package Ada.Wide_Characters
   with Pure is
end Ada.Wide_Characters;

```

{AI95-00395-01} The library package Wide_Wide_Characters has the following declaration: 

```ada
{AI12-0414-1} package Ada.Wide_Wide_Characters
   with Pure is
end Ada.Wide_Wide_Characters;

```


#### Implementation Advice

{AI95-00395-01} {AI05-0185-1} If an implementation chooses to provide implementation-defined operations on Wide_Character or Wide_String (such as collating and sorting, etc.) it should do so by providing child units of Wide_Characters. Similarly if it chooses to provide implementation-defined operations on Wide_Wide_Character or Wide_Wide_String it should do so by providing child units of Wide_Wide_Characters. 

Implementation Advice: Implementation-defined operations on Wide_Character, Wide_String, Wide_Wide_Character, and Wide_Wide_String should be child units of Wide_Characters or Wide_Wide_Characters.


#### Extensions to Ada 95

{AI95-00395-01} The packages Wide_Characters and Wide_Wide_Characters are new. 


## A.3.2  The Package Characters.Handling


#### Static Semantics

The library package Characters.Handling has the following declaration: 

```ada
{AI95-00362-01} {AI95-00395-01} {AI12-0414-1} with Ada.Characters.Conversions;
package Ada.Characters.Handling
  with Pure is

```

```ada
--Character classification functions

```

```ada
{AI05-0185-1} {AI12-0004-1}   function Is_Control           (Item : in Character) return Boolean;
  function Is_Graphic           (Item : in Character) return Boolean;
  function Is_Letter            (Item : in Character) return Boolean;
  function Is_Lower             (Item : in Character) return Boolean;
  function Is_Upper             (Item : in Character) return Boolean;
  function Is_Basic             (Item : in Character) return Boolean;
  function Is_Digit             (Item : in Character) return Boolean;
  function Is_Decimal_Digit     (Item : in Character) return Boolean
                     renames Is_Digit;
  function Is_Hexadecimal_Digit (Item : in Character) return Boolean;
  function Is_Alphanumeric      (Item : in Character) return Boolean;
  function Is_Special           (Item : in Character) return Boolean;
  function Is_Line_Terminator   (Item : in Character) return Boolean;
  function Is_Mark              (Item : in Character) return Boolean;
  function Is_Other_Format      (Item : in Character) return Boolean;
  function Is_Punctuation_Connector (Item : in Character) return Boolean;
  function Is_Space             (Item : in Character) return Boolean;
  function Is_NFKC              (Item : in Character) return Boolean;

```

```ada
--Conversion functions for Character and String

```

```ada
  function To_Lower (Item : in Character) return Character;
  function To_Upper (Item : in Character) return Character;
  function To_Basic (Item : in Character) return Character;

```

```ada
  function To_Lower (Item : in String) return String;
  function To_Upper (Item : in String) return String;
  function To_Basic (Item : in String) return String;

```

```ada
--Classifications of and conversions between Character and ISO 646

```

```ada
  subtype ISO_646 is
    Character range Character'Val(0) .. Character'Val(127);

```

```ada
  function Is_ISO_646 (Item : in Character) return Boolean;
  function Is_ISO_646 (Item : in String)    return Boolean;

```

```ada
  function To_ISO_646 (Item       : in Character;
                       Substitute : in ISO_646 := ' ')
    return ISO_646;

```

```ada
  function To_ISO_646 (Item       : in String;
                       Substitute : in ISO_646 := ' ')
    return String;

```

```ada
{AI95-00285-01} {AI95-00395-01} -- The functions Is_Character, Is_String, To_Character, To_String, To_Wide_Character,
-- and To_Wide_String are obsolescent; see J.14.

```

```ada
Paragraphs 14 through 18 were deleted.

```

```ada
end Ada.Characters.Handling;

```

Discussion: {AI95-00395-01} The [with_clause](./AA-10.1#S0294) for Ada.Characters.Conversions is needed for the definition of the obsolescent functions (see J.14). It would be odd to put this clause into J.14 as it was not present in Ada 95, and [with_clause](./AA-10.1#S0294)s are semantically neutral to clients anyway. 

In the description below for each function that returns a Boolean result, the effect is described in terms of the conditions under which the value True is returned. If these conditions are not met, then the function returns False.

Each of the following classification functions has a formal Character parameter, Item, and returns a Boolean result. 

Is_Control True if Item is a control character. A control character is a character whose position is in one of the ranges 0..31 or 127..159.

Is_Graphic True if Item is a graphic character. A graphic character is a character whose position is in one of the ranges 32..126 or 160..255.

Is_Letter True if Item is a letter. A letter is a character that is in one of the ranges 'A'..'Z' or 'a'..'z', or whose position is in one of the ranges 192..214, 216..246, or 248..255.

Is_Lower True if Item is a lower-case letter. A lower-case letter is a character that is in the range 'a'..'z', or whose position is in one of the ranges 223..246 or 248..255.

Is_UpperTrue if Item is an upper-case letter. An upper-case letter is a character that is in the range 'A'..'Z' or whose position is in one of the ranges 192..214 or 216.. 222.

Is_Basic True if Item is a basic letter. A basic letter is a character that is in one of the ranges 'A'..'Z' and 'a'..'z', or that is one of the following: 'Æ', 'æ', 'Ð', 'ð', 'Þ', 'þ', or 'ß'.

Is_Digit True if Item is a decimal digit. A decimal digit is a character in the range '0'..'9'.

Is_Decimal_Digit A renaming of Is_Digit.

Is_Hexadecimal_Digit True if Item is a hexadecimal digit. A hexadecimal digit is a character that is either a decimal digit or that is in one of the ranges 'A' .. 'F' or 'a' .. 'f'.

Is_Alphanumeric True if Item is an alphanumeric character. An alphanumeric character is a character that is either a letter or a decimal digit.

Is_Special True if Item is a special graphic character. A special graphic character is a graphic character that is not alphanumeric.

{AI05-0185-1} Is_Line_TerminatorTrue if Item is a character with position 10 .. 13 (Line_Feed, Line_Tabulation, Form_Feed, Carriage_Return) or 133 (Next_Line).

{AI05-0185-1} Is_MarkNever True (no value of type Character has categories Mark, Non-Spacing or Mark, Spacing Combining).

{AI05-0185-1} Is_Other_FormatTrue if Item is a character with position 173 (Soft_Hyphen).

{AI05-0185-1} Is_Punctuation_ConnectorTrue if Item is a character with position 95 ('_', known as Low_Line or Underscore).

{AI05-0185-1} Is_SpaceTrue if Item is a character with position 32 (' ') or 160 (No_Break_Space).

{AI12-0004-1} {AI12-0263-1} {AI12-0439-1} Is_NFKCTrue if Item can be present in a string normalized to Normalization Form KC (as defined by Clause 21 of ISO/IEC 10646:2017); this includes all characters except those with positions 160, 168, 170, 175, 178, 179, 180, 181, 184, 185, 186, 188, 189, and 190. 

Each of the names To_Lower, To_Upper, and To_Basic refers to two functions: one that converts from Character to Character, and the other that converts from String to String. The result of each Character-to-Character function is described below, in terms of the conversion applied to Item, its formal Character parameter. The result of each  String-to-String conversion is obtained by applying to each element of the function's String parameter the corresponding Character-to-Character conversion; the result is the null String if the value of the formal parameter is the null String. The lower bound of the result String is 1. 

To_LowerReturns the corresponding lower-case value for Item if Is_Upper(Item), and returns Item otherwise.

To_UpperReturns the corresponding upper-case value for Item if Is_Lower(Item) and Item has an upper-case form, and returns Item otherwise. The lower case letters 'ß' and 'ÿ' do not have upper case forms.

To_BasicReturns the letter corresponding to Item but with no diacritical mark, if Item is a letter but not a basic letter; returns Item otherwise. 

The following set of functions test for membership in the ISO 646 character range, or convert between ISO 646 and Character. 

Is_ISO_646The function whose formal parameter, Item, is of type Character returns True if Item is in the subtype ISO_646.

Is_ISO_646The function whose formal parameter, Item, is of type String returns True if Is_ISO_646(Item(I)) is True for each I in Item'Range.

To_ISO_646The function whose first formal parameter, Item, is of type Character returns Item if Is_ISO_646(Item), and returns the Substitute ISO_646 character otherwise.

To_ISO_646The function whose first formal parameter, Item, is of type String returns the String whose Range is 1..Item'Length and each of whose elements is given by To_ISO_646 of the corresponding element in Item. 

Paragraphs 42 through 49 were deleted. 

NOTE 1   A basic letter is a letter without a diacritical mark.

NOTE 2   Except for the hexadecimal digits, basic letters, and ISO_646 characters, the categories identified in the classification functions form a strict hierarchy: 

- Control characters

- Graphic characters

	- Alphanumeric characters

		- Letters

			- Upper-case letters

			- Lower-case letters

		- Decimal digits

	- Special graphic characters

Ramification: Thus each Character value is either a control character or a graphic character but not both; each graphic character is either an alphanumeric or special graphic but not both; each alphanumeric is either a letter or decimal digit but not both; each letter is either upper case or lower case but not both.

NOTE 3   {AI05-0114-1} There are certain characters which are defined to be lower case letters by ISO 10646 and are therefore allowed in identifiers, but are not considered lower case letters by Ada.Characters.Handling.

Reason: {AI12-0263-1} This is to maintain runtime compatibility with the Ada 95 definitions of these functions. We don't list the exact characters involved because they're likely to change in future character set standards; the list for ISO 10646:2017 can be found in AI05-0114-1. 

Ramification: No version of Characters.Handling is intended to do portable (Ada-version independent) manipulation of Ada identifiers. The classification given by Wide_Characters.Handling will be correct for the current implementation for Ada 2012 identifiers, but it might not be correct for a different implementation or version of Ada. 


#### Extensions to Ada 95

{AI95-00362-01} Characters.Handling is now Pure, so it can be used in pure units. 


#### Incompatibilities With Ada 2005

{AI05-0185-1} Added additional classification routines so that Characters.Handling has all of the routines available in Wide_Characters.Handling. If Characters.Handling is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) that is the same as one of the new functions is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Wording Changes from Ada 95

{AI95-00285-01} {AI95-00395-01} The conversion functions are made obsolescent; a more complete set is available in Characters.Conversions - see A.3.4.

{AI95-00285-01} {AI05-0248-1} We no longer talk about localized character sets; these are a nonstandard mode, which is none of our business. 


#### Wording Changes from Ada 2005

{AI05-0114-1} Correction: Added a note to clarify that these functions don't have any relationship to the characters allowed in identifiers. 


#### Incompatibilities With Ada 2012

{AI12-0004-1} Added an additional classification routine Is_NFKC. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


## A.3.3  The Package Characters.Latin_1

The package Characters.Latin_1 declares constants for characters in ISO 8859-1. 

Reason: The constants for the ISO 646 characters could have been declared as renamings of objects declared in package ASCII, as opposed to explicit constants. The main reason for explicit constants was for consistency of style with the upper-half constants, and to avoid emphasizing the package ASCII.


#### Static Semantics

The library package Characters.Latin_1 has the following declaration: 

```ada
{AI12-0414-1} package Ada.Characters.Latin_1
    with Pure is

```

```ada
-- Control characters:

```

```ada
    NUL                  : constant Character := Character'Val(0);
    SOH                  : constant Character := Character'Val(1);
    STX                  : constant Character := Character'Val(2);
    ETX                  : constant Character := Character'Val(3);
    EOT                  : constant Character := Character'Val(4);
    ENQ                  : constant Character := Character'Val(5);
    ACK                  : constant Character := Character'Val(6);
    BEL                  : constant Character := Character'Val(7);
    BS                   : constant Character := Character'Val(8);
    HT                   : constant Character := Character'Val(9);
    LF                   : constant Character := Character'Val(10);
    VT                   : constant Character := Character'Val(11);
    FF                   : constant Character := Character'Val(12);
    CR                   : constant Character := Character'Val(13);
    SO                   : constant Character := Character'Val(14);
    SI                   : constant Character := Character'Val(15);

```

```ada
    DLE                  : constant Character := Character'Val(16);
    DC1                  : constant Character := Character'Val(17);
    DC2                  : constant Character := Character'Val(18);
    DC3                  : constant Character := Character'Val(19);
    DC4                  : constant Character := Character'Val(20);
    NAK                  : constant Character := Character'Val(21);
    SYN                  : constant Character := Character'Val(22);
    ETB                  : constant Character := Character'Val(23);
    CAN                  : constant Character := Character'Val(24);
    EM                   : constant Character := Character'Val(25);
    SUB                  : constant Character := Character'Val(26);
    ESC                  : constant Character := Character'Val(27);
    FS                   : constant Character := Character'Val(28);
    GS                   : constant Character := Character'Val(29);
    RS                   : constant Character := Character'Val(30);
    US                   : constant Character := Character'Val(31);

```

```ada
-- ISO 646 graphic characters:

```

```ada
    Space                : constant Character := ' ';  -- Character'Val(32)
    Exclamation          : constant Character := '!';  -- Character'Val(33)
    Quotation            : constant Character := '"';  -- Character'Val(34)
    Number_Sign          : constant Character := '#';  -- Character'Val(35)
    Dollar_Sign          : constant Character := '$';  -- Character'Val(36)
    Percent_Sign         : constant Character := '%';  -- Character'Val(37)
    Ampersand            : constant Character := '&';  -- Character'Val(38)
    Apostrophe           : constant Character := ''';  -- Character'Val(39)
    Left_Parenthesis     : constant Character := '(';  -- Character'Val(40)
    Right_Parenthesis    : constant Character := ')';  -- Character'Val(41)
    Asterisk             : constant Character := '*';  -- Character'Val(42)
    Plus_Sign            : constant Character := '+';  -- Character'Val(43)
    Comma                : constant Character := ',';  -- Character'Val(44)
    Hyphen               : constant Character := '-';  -- Character'Val(45)
    Minus_Sign           : Character renames Hyphen;
    Full_Stop            : constant Character := '.';  -- Character'Val(46)
    Solidus              : constant Character := '/';  -- Character'Val(47)

```

```ada
    -- Decimal digits '0' though '9' are at positions 48 through 57

```

```ada
    Colon                : constant Character := ':';  -- Character'Val(58)
    Semicolon            : constant Character := ';';  -- Character'Val(59)
    Less_Than_Sign       : constant Character := '&lt';  -- Character'Val(60)
    Equals_Sign          : constant Character := '=';  -- Character'Val(61)
    Greater_Than_Sign    : constant Character := '&gt';  -- Character'Val(62)
    Question             : constant Character := '?';  -- Character'Val(63)
    Commercial_At        : constant Character := '@';  -- Character'Val(64)

```

```ada
    -- Letters 'A' through 'Z' are at positions 65 through 90

```

```ada
    Left_Square_Bracket  : constant Character := '[';  -- Character'Val(91)
    Reverse_Solidus      : constant Character := '\';  -- Character'Val(92)
    Right_Square_Bracket : constant Character := ']';  -- Character'Val(93)
    Circumflex           : constant Character := '^';  -- Character'Val(94)
    Low_Line             : constant Character := '_';  -- Character'Val(95)

```

```ada
    Grave                : constant Character := '`';  -- Character'Val(96)
    LC_A                 : constant Character := 'a';  -- Character'Val(97)
    LC_B                 : constant Character := 'b';  -- Character'Val(98)
    LC_C                 : constant Character := 'c';  -- Character'Val(99)
    LC_D                 : constant Character := 'd';  -- Character'Val(100)
    LC_E                 : constant Character := 'e';  -- Character'Val(101)
    LC_F                 : constant Character := 'f';  -- Character'Val(102)
    LC_G                 : constant Character := 'g';  -- Character'Val(103)
    LC_H                 : constant Character := 'h';  -- Character'Val(104)
    LC_I                 : constant Character := 'i';  -- Character'Val(105)
    LC_J                 : constant Character := 'j';  -- Character'Val(106)
    LC_K                 : constant Character := 'k';  -- Character'Val(107)
    LC_L                 : constant Character := 'l';  -- Character'Val(108)
    LC_M                 : constant Character := 'm';  -- Character'Val(109)
    LC_N                 : constant Character := 'n';  -- Character'Val(110)
    LC_O                 : constant Character := 'o';  -- Character'Val(111)

```

```ada
    LC_P                 : constant Character := 'p';  -- Character'Val(112)
    LC_Q                 : constant Character := 'q';  -- Character'Val(113)
    LC_R                 : constant Character := 'r';  -- Character'Val(114)
    LC_S                 : constant Character := 's';  -- Character'Val(115)
    LC_T                 : constant Character := 't';  -- Character'Val(116)
    LC_U                 : constant Character := 'u';  -- Character'Val(117)
    LC_V                 : constant Character := 'v';  -- Character'Val(118)
    LC_W                 : constant Character := 'w';  -- Character'Val(119)
    LC_X                 : constant Character := 'x';  -- Character'Val(120)
    LC_Y                 : constant Character := 'y';  -- Character'Val(121)
    LC_Z                 : constant Character := 'z';  -- Character'Val(122)
    Left_Curly_Bracket   : constant Character := '{';  -- Character'Val(123)
    Vertical_Line        : constant Character := '|';  -- Character'Val(124)
    Right_Curly_Bracket  : constant Character := '}';  -- Character'Val(125)
    Tilde                : constant Character := '~';  -- Character'Val(126)
    DEL                  : constant Character := Character'Val(127);

```

```ada
-- ISO 6429 control characters:

```

```ada
    IS4                  : Character renames FS;
    IS3                  : Character renames GS;
    IS2                  : Character renames RS;
    IS1                  : Character renames US;

```

```ada
    Reserved_128         : constant Character := Character'Val(128);
    Reserved_129         : constant Character := Character'Val(129);
    BPH                  : constant Character := Character'Val(130);
    NBH                  : constant Character := Character'Val(131);
    Reserved_132         : constant Character := Character'Val(132);
    NEL                  : constant Character := Character'Val(133);
    SSA                  : constant Character := Character'Val(134);
    ESA                  : constant Character := Character'Val(135);
    HTS                  : constant Character := Character'Val(136);
    HTJ                  : constant Character := Character'Val(137);
    VTS                  : constant Character := Character'Val(138);
    PLD                  : constant Character := Character'Val(139);
    PLU                  : constant Character := Character'Val(140);
    RI                   : constant Character := Character'Val(141);
    SS2                  : constant Character := Character'Val(142);
    SS3                  : constant Character := Character'Val(143);

```

```ada
    DCS                  : constant Character := Character'Val(144);
    PU1                  : constant Character := Character'Val(145);
    PU2                  : constant Character := Character'Val(146);
    STS                  : constant Character := Character'Val(147);
    CCH                  : constant Character := Character'Val(148);
    MW                   : constant Character := Character'Val(149);
    SPA                  : constant Character := Character'Val(150);
    EPA                  : constant Character := Character'Val(151);

```

```ada
    SOS                  : constant Character := Character'Val(152);
    Reserved_153         : constant Character := Character'Val(153);
    SCI                  : constant Character := Character'Val(154);
    CSI                  : constant Character := Character'Val(155);
    ST                   : constant Character := Character'Val(156);
    OSC                  : constant Character := Character'Val(157);
    PM                   : constant Character := Character'Val(158);
    APC                  : constant Character := Character'Val(159);

```

```ada
-- Other graphic characters:

```

```ada
{AI05-0181-1} -- Character positions 160 (16#A0#) .. 175 (16#AF#):
    No_Break_Space             : constant Character := ' '; --Character'Val(160)
    NBSP                       : Character renames No_Break_Space;
    Inverted_Exclamation       : constant Character := '¡'; --Character'Val(161)
    Cent_Sign                  : constant Character := '¢'; --Character'Val(162)
    Pound_Sign                 : constant Character := '£'; --Character'Val(163)
    Currency_Sign              : constant Character := '¤'; --Character'Val(164)
    Yen_Sign                   : constant Character := '¥'; --Character'Val(165)
    Broken_Bar                 : constant Character := '¦'; --Character'Val(166)
    Section_Sign               : constant Character := '§'; --Character'Val(167)
    Diaeresis                  : constant Character := '¨'; --Character'Val(168)
    Copyright_Sign             : constant Character := '©'; --Character'Val(169)
    Feminine_Ordinal_Indicator : constant Character := 'ª'; --Character'Val(170)
    Left_Angle_Quotation       : constant Character := '«'; --Character'Val(171)
    Not_Sign                   : constant Character := '¬'; --Character'Val(172)
    Soft_Hyphen                : constant Character := Character'Val(173);
    Registered_Trade_Mark_Sign : constant Character := '®'; --Character'Val(174)
    Macron                     : constant Character := '¯'; --Character'Val(175)

```

```ada
-- Character positions 176 (16#B0#) .. 191 (16#BF#):
    Degree_Sign                : constant Character := '°'; --Character'Val(176)
    Ring_Above                 : Character renames Degree_Sign;
    Plus_Minus_Sign            : constant Character := '±'; --Character'Val(177)
    Superscript_Two            : constant Character := '²'; --Character'Val(178)
    Superscript_Three          : constant Character := '³'; --Character'Val(179)
    Acute                      : constant Character := '´'; --Character'Val(180)
    Micro_Sign                 : constant Character := 'µ'; --Character'Val(181)
    Pilcrow_Sign               : constant Character := '¶'; --Character'Val(182)
    Paragraph_Sign             : Character renames Pilcrow_Sign;
    Middle_Dot                 : constant Character := '·'; --Character'Val(183)
    Cedilla                    : constant Character := '¸'; --Character'Val(184)
    Superscript_One            : constant Character := '¹'; --Character'Val(185)
    Masculine_Ordinal_Indicator: constant Character := 'º'; --Character'Val(186)
    Right_Angle_Quotation      : constant Character := '»'; --Character'Val(187)
    Fraction_One_Quarter       : constant Character := '¼'; --Character'Val(188)
    Fraction_One_Half          : constant Character := '½'; --Character'Val(189)
    Fraction_Three_Quarters    : constant Character := '¾'; --Character'Val(190)
    Inverted_Question          : constant Character := '¿'; --Character'Val(191)

```

```ada
-- Character positions 192 (16#C0#) .. 207 (16#CF#):
    UC_A_Grave                 : constant Character := 'À'; --Character'Val(192)
    UC_A_Acute                 : constant Character := 'Á'; --Character'Val(193)
    UC_A_Circumflex            : constant Character := 'Â'; --Character'Val(194)
    UC_A_Tilde                 : constant Character := 'Ã'; --Character'Val(195)
    UC_A_Diaeresis             : constant Character := 'Ä'; --Character'Val(196)
    UC_A_Ring                  : constant Character := 'Å'; --Character'Val(197)
    UC_AE_Diphthong            : constant Character := 'Æ'; --Character'Val(198)
    UC_C_Cedilla               : constant Character := 'Ç'; --Character'Val(199)
    UC_E_Grave                 : constant Character := 'È'; --Character'Val(200)
    UC_E_Acute                 : constant Character := 'É'; --Character'Val(201)
    UC_E_Circumflex            : constant Character := 'Ê'; --Character'Val(202)
    UC_E_Diaeresis             : constant Character := 'Ë'; --Character'Val(203)
    UC_I_Grave                 : constant Character := 'Ì'; --Character'Val(204)
    UC_I_Acute                 : constant Character := 'Í'; --Character'Val(205)
    UC_I_Circumflex            : constant Character := 'Î'; --Character'Val(206)
    UC_I_Diaeresis             : constant Character := 'Ï'; --Character'Val(207)

```

```ada
-- Character positions 208 (16#D0#) .. 223 (16#DF#):
    UC_Icelandic_Eth           : constant Character := 'Ð'; --Character'Val(208)
    UC_N_Tilde                 : constant Character := 'Ñ'; --Character'Val(209)
    UC_O_Grave                 : constant Character := 'Ò'; --Character'Val(210)
    UC_O_Acute                 : constant Character := 'Ó'; --Character'Val(211)
    UC_O_Circumflex            : constant Character := 'Ô'; --Character'Val(212)
    UC_O_Tilde                 : constant Character := 'Õ'; --Character'Val(213)
    UC_O_Diaeresis             : constant Character := 'Ö'; --Character'Val(214)
    Multiplication_Sign        : constant Character := '×'; --Character'Val(215)
    UC_O_Oblique_Stroke        : constant Character := 'Ø'; --Character'Val(216)
    UC_U_Grave                 : constant Character := 'Ù'; --Character'Val(217)
    UC_U_Acute                 : constant Character := 'Ú'; --Character'Val(218)
    UC_U_Circumflex            : constant Character := 'Û'; --Character'Val(219)
    UC_U_Diaeresis             : constant Character := 'Ü'; --Character'Val(220)
    UC_Y_Acute                 : constant Character := 'Ý'; --Character'Val(221)
    UC_Icelandic_Thorn         : constant Character := 'Þ'; --Character'Val(222)
    LC_German_Sharp_S          : constant Character := 'ß'; --Character'Val(223)

```

```ada
-- Character positions 224 (16#E0#) .. 239 (16#EF#):
    LC_A_Grave                 : constant Character := 'à'; --Character'Val(224)
    LC_A_Acute                 : constant Character := 'á'; --Character'Val(225)
    LC_A_Circumflex            : constant Character := 'â'; --Character'Val(226)
    LC_A_Tilde                 : constant Character := 'ã'; --Character'Val(227)
    LC_A_Diaeresis             : constant Character := 'ä'; --Character'Val(228)
    LC_A_Ring                  : constant Character := 'å'; --Character'Val(229)
    LC_AE_Diphthong            : constant Character := 'æ'; --Character'Val(230)
    LC_C_Cedilla               : constant Character := 'ç'; --Character'Val(231)
    LC_E_Grave                 : constant Character := 'è'; --Character'Val(232)
    LC_E_Acute                 : constant Character := 'é'; --Character'Val(233)
    LC_E_Circumflex            : constant Character := 'ê'; --Character'Val(234)
    LC_E_Diaeresis             : constant Character := 'ë'; --Character'Val(235)
    LC_I_Grave                 : constant Character := 'ì'; --Character'Val(236)
    LC_I_Acute                 : constant Character := 'í'; --Character'Val(237)
    LC_I_Circumflex            : constant Character := 'î'; --Character'Val(238)
    LC_I_Diaeresis             : constant Character := 'ï'; --Character'Val(239)

```

```ada
-- Character positions 240 (16#F0#) .. 255 (16#FF#):
    LC_Icelandic_Eth           : constant Character := 'ð'; --Character'Val(240)
    LC_N_Tilde                 : constant Character := 'ñ'; --Character'Val(241)
    LC_O_Grave                 : constant Character := 'ò'; --Character'Val(242)
    LC_O_Acute                 : constant Character := 'ó'; --Character'Val(243)
    LC_O_Circumflex            : constant Character := 'ô'; --Character'Val(244)
    LC_O_Tilde                 : constant Character := 'õ'; --Character'Val(245)
    LC_O_Diaeresis             : constant Character := 'ö'; --Character'Val(246)
    Division_Sign              : constant Character := '÷'; --Character'Val(247)
    LC_O_Oblique_Stroke        : constant Character := 'ø'; --Character'Val(248)
    LC_U_Grave                 : constant Character := 'ù'; --Character'Val(249)
    LC_U_Acute                 : constant Character := 'ú'; --Character'Val(250)
    LC_U_Circumflex            : constant Character := 'û'; --Character'Val(251)
    LC_U_Diaeresis             : constant Character := 'ü'; --Character'Val(252)
    LC_Y_Acute                 : constant Character := 'ý'; --Character'Val(253)
    LC_Icelandic_Thorn         : constant Character := 'þ'; --Character'Val(254)
    LC_Y_Diaeresis             : constant Character := 'ÿ'; --Character'Val(255)
end Ada.Characters.Latin_1;

```


#### Implementation Permissions

An implementation may provide additional packages as children of Ada.Characters, to declare names for the symbols of the local character set or other character sets. 


#### Wording Changes from Ada 2005

{AI05-0181-1} Correction: Soft_Hyphen is not a graphic character, and thus a character literal for it is illegal. So we have to use the position value. This makes no semantic change to users of the constant. 


## A.3.4  The Package Characters.Conversions


#### Static Semantics

{AI95-00395-01} The library package Characters.Conversions has the following declaration: 

```ada
{AI12-0414-1} package Ada.Characters.Conversions
   with Pure is

```

```ada
   function Is_Character (Item : in Wide_Character)      return Boolean;
   function Is_String    (Item : in Wide_String)         return Boolean;
   function Is_Character (Item : in Wide_Wide_Character) return Boolean;
   function Is_String    (Item : in Wide_Wide_String)    return Boolean;
   function Is_Wide_Character (Item : in Wide_Wide_Character)
      return Boolean;
   function Is_Wide_String    (Item : in Wide_Wide_String)
      return Boolean;

```

```ada
   function To_Wide_Character (Item : in Character) return Wide_Character;
   function To_Wide_String    (Item : in String)    return Wide_String;
   function To_Wide_Wide_Character (Item : in Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_String    (Item : in String)
      return Wide_Wide_String;
   function To_Wide_Wide_Character (Item : in Wide_Character)
      return Wide_Wide_Character;
   function To_Wide_Wide_String    (Item : in Wide_String)
      return Wide_Wide_String;

```

```ada
   function To_Character (Item       : in Wide_Character;
                         Substitute : in Character := ' ')
      return Character;
   function To_String    (Item       : in Wide_String;
                          Substitute : in Character := ' ')
      return String;
   function To_Character (Item :       in Wide_Wide_Character;
                          Substitute : in Character := ' ')
      return Character;
   function To_String    (Item :       in Wide_Wide_String;
                          Substitute : in Character := ' ')
      return String;
   function To_Wide_Character (Item :       in Wide_Wide_Character;
                               Substitute : in Wide_Character := ' ')
      return Wide_Character;
   function To_Wide_String    (Item :       in Wide_Wide_String;
                               Substitute : in Wide_Character := ' ')
      return Wide_String;

```

```ada
end Ada.Characters.Conversions;

```

{AI95-00395-01} The functions in package Characters.Conversions test Wide_Wide_Character or Wide_Character values for membership in Wide_Character or Character, or convert between corresponding characters of Wide_Wide_Character, Wide_Character, and Character.

```ada
function Is_Character (Item : in Wide_Character) return Boolean;

```

{AI95-00395-01} Returns True if Wide_Character'Pos(Item) &lt= Character'Pos(Character'Last).

```ada
function Is_Character (Item : in Wide_Wide_Character) return Boolean;

```

{AI95-00395-01} Returns True if Wide_Wide_Character'Pos(Item) &lt= Character'Pos(Character'Last).

```ada
function Is_Wide_Character (Item : in Wide_Wide_Character) return Boolean;

```

{AI95-00395-01} Returns True if Wide_Wide_Character'Pos(Item) &lt= Wide_Character'Pos(Wide_Character'Last).

```ada
function Is_String (Item : in Wide_String)      return Boolean;
function Is_String (Item : in Wide_Wide_String) return Boolean;

```

{AI95-00395-01} Returns True if Is_Character(Item(I)) is True for each I in Item'Range.

```ada
function Is_Wide_String (Item : in Wide_Wide_String) return Boolean;

```

{AI95-00395-01} Returns True if Is_Wide_Character(Item(I)) is True for each I in Item'Range.

```ada
function To_Character (Item :       in Wide_Character;
                       Substitute : in Character := ' ') return Character;
function To_Character (Item :       in Wide_Wide_Character;
                       Substitute : in Character := ' ') return Character;

```

{AI95-00395-01} Returns the Character corresponding to Item if Is_Character(Item), and returns the Substitute Character otherwise.

```ada
function To_Wide_Character (Item : in Character) return Wide_Character;

```

{AI95-00395-01} Returns the Wide_Character X such that Character'Pos(Item) = Wide_Character'Pos (X).

```ada
function To_Wide_Character (Item :       in Wide_Wide_Character;
                            Substitute : in Wide_Character := ' ')
   return Wide_Character;

```

{AI95-00395-01} Returns the Wide_Character corresponding to Item if Is_Wide_Character(Item), and returns the Substitute Wide_Character otherwise.

```ada
function To_Wide_Wide_Character (Item : in Character)
   return Wide_Wide_Character;

```

{AI95-00395-01} Returns the Wide_Wide_Character X such that Character'Pos(Item) = Wide_Wide_Character'Pos (X).

```ada
function To_Wide_Wide_Character (Item : in Wide_Character)
   return Wide_Wide_Character;

```

{AI95-00395-01} Returns the Wide_Wide_Character X such that Wide_Character'Pos(Item) = Wide_Wide_Character'Pos (X).

```ada
function To_String (Item :       in Wide_String;
                    Substitute : in Character := ' ') return String;
function To_String (Item :       in Wide_Wide_String;
                    Substitute : in Character := ' ') return String;

```

{AI95-00395-01} Returns the String whose range is 1..Item'Length and each of whose elements is given by To_Character of the corresponding element in Item.

```ada
function To_Wide_String (Item : in String) return Wide_String;

```

{AI95-00395-01} Returns the Wide_String whose range is 1..Item'Length and each of whose elements is given by To_Wide_Character of the corresponding element in Item.

```ada
function To_Wide_String (Item :       in Wide_Wide_String;
                         Substitute : in Wide_Character := ' ')
   return Wide_String;

```

{AI95-00395-01} Returns the Wide_String whose range is 1..Item'Length and each of whose elements is given by To_Wide_Character of the corresponding element in Item with the given Substitute Wide_Character.

```ada
function To_Wide_Wide_String (Item : in String) return Wide_Wide_String;
function To_Wide_Wide_String (Item : in Wide_String)
   return Wide_Wide_String;

```

{AI95-00395-01} Returns the Wide_Wide_String whose range is 1..Item'Length and each of whose elements is given by To_Wide_Wide_Character of the corresponding element in Item.


#### Extensions to Ada 95

{AI95-00395-01} The package Characters.Conversions is new, replacing functions previously found in Characters.Handling. 


## A.3.5  The Package Wide_Characters.Handling

{AI05-0185-1} The package Wide_Characters.Handling provides operations for classifying Wide_Characters and case folding for Wide_Characters. 


#### Static Semantics

{AI05-0185-1} The library package Wide_Characters.Handling has the following declaration:

```ada
{AI05-0185-1} {AI05-0266-1} {AI12-0414-1} package Ada.Wide_Characters.Handling
   with Pure is

```

```ada
{AI05-0266-1}    function Character_Set_Version return String;

```

```ada
   function Is_Control (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Letter (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Lower (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Upper (Item : Wide_Character) return Boolean;

```

```ada
{AI12-0260-1}    function Is_Basic (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Digit (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Decimal_Digit (Item : Wide_Character) return Boolean
      renames Is_Digit;

```

```ada
   function Is_Hexadecimal_Digit (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Alphanumeric (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Special (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Line_Terminator (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Mark (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Other_Format (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Punctuation_Connector (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Space (Item : Wide_Character) return Boolean;

```

```ada
{AI12-0004-1}    function Is_NFKC (Item : Wide_Character) return Boolean;

```

```ada
   function Is_Graphic (Item : Wide_Character) return Boolean;

```

```ada
   function To_Lower (Item : Wide_Character) return Wide_Character;
   function To_Upper (Item : Wide_Character) return Wide_Character;

```

```ada
{AI12-0260-1}    function To_Basic (Item : Wide_Character) return Wide_Character;

```

```ada
   function To_Lower (Item : Wide_String) return Wide_String;
   function To_Upper (Item : Wide_String) return Wide_String;

```

```ada
{AI12-0260-1}    function To_Basic (Item : Wide_String) return Wide_String;

```

```ada
end Ada.Wide_Characters.Handling;

```

{AI05-0185-1} The subprograms defined in Wide_Characters.Handling are locale independent.

```ada
function Character_Set_Version return String;

```

{AI05-0266-1} Returns an implementation-defined identifier that identifies the version of the character set standard that is used for categorizing characters by the implementation.

```ada
function Is_Control (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as other_control; otherwise returns False.

```ada
function Is_Letter (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as letter_uppercase, letter_lowercase, letter_titlecase, letter_modifier, letter_other, or number_letter; otherwise returns False.

```ada
function Is_Lower (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as letter_lowercase; otherwise returns False.

```ada
function Is_Upper (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as letter_uppercase; otherwise returns False.

```ada
function Is_Basic (Item : Wide_Character) return Boolean;

```

{AI12-0260-1} Returns True if the Wide_Character designated by Item has no Decomposition Mapping in the code charts of ISO/IEC 10646:2017; otherwise returns False.

Implementation Note: Decomposition Mapping is defined in Clause 33 of ISO/IEC 10646:2017. Machine-readable (and normative!) versions of this can be found as Character Decomposition Mapping, described in file [http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt](http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt), field 5 (which is the 6th item, Unicode counts from zero). 

```ada
function Is_Digit (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as number_decimal; otherwise returns False.

```ada
function Is_Hexadecimal_Digit (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as number_decimal, or is in the range 'A' .. 'F' or 'a' .. 'f'; otherwise returns False.

```ada
function Is_Alphanumeric (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as letter_uppercase, letter_lowercase, letter_titlecase, letter_modifier, letter_other, number_letter, or number_decimal; otherwise returns False.

```ada
function Is_Special (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as graphic_character, but not categorized as letter_uppercase, letter_lowercase, letter_titlecase, letter_modifier, letter_other, number_letter, or number_decimal; otherwise returns False.

```ada
function Is_Line_Terminator (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as separator_line or separator_paragraph, or if Item is a conventional line terminator character (Line_Feed, Line_Tabulation, Form_Feed, Carriage_Return, Next_Line); otherwise returns False.

```ada
function Is_Mark (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as mark_non_spacing or mark_spacing_combining; otherwise returns False.

```ada
function Is_Other_Format (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as other_format; otherwise returns False.

```ada
function Is_Punctuation_Connector (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as punctuation_connector; otherwise returns False.

```ada
function Is_Space (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as separator_space; otherwise returns False.

```ada
function Is_NFKC (Item : Wide_Character) return Boolean;

```

{AI12-0004-1} {AI12-0263-1} {AI12-0439-1} Returns True if the Wide_Character designated by Item can be present in a string normalized to Normalization Form KC (as defined by Clause 21 of ISO/IEC 10646:2017), otherwise returns False.

Reason: Wide_Characters for which this function returns False are not allowed in identifiers (see 2.3) even if they are categorized as letters or digits. 

Implementation Note: This function returns False if the Unicode property NFKC Quick Check (NFKC_QC in the files) has the value No. See the Implementation Notes in 2.3 for the source of this property. 

Discussion: A string for which Is_NFKC is true for every character may still not be in Normalization Form KC, as Is_NFKC returns true for characters that are dependent on characters around them as to whether they are removed by normalization. Ada does not provide a full normalization operation (it is complex and expensive). 

```ada
function Is_Graphic (Item : Wide_Character) return Boolean;

```

{AI05-0185-1} Returns True if the Wide_Character designated by Item is categorized as graphic_character; otherwise returns False.

```ada
function To_Lower (Item : Wide_Character) return Wide_Character;

```

{AI05-0185-1} {AI05-0266-1} {AI05-0299-1} {AI12-0263-1} Returns the Simple Lowercase Mapping as defined by documents referenced in Clause 2 of ISO/IEC 10646:2017 of the Wide_Character designated by Item. If the Simple Lowercase Mapping does not exist for the Wide_Character designated by Item, then the value of Item is returned.

Discussion: {AI12-0263-1} The "documents referenced" means Unicode, Chapter 4 (specifically, section 4.2 - Case). The case mappings come from Unicode as ISO/IEC 10646:2017 does not include complete case mappings. See the Implementation Notes in subclause  for machine-readable versions of both Uppercase and Lowercase mappings. 

```ada
function To_Lower (Item : Wide_String) return Wide_String;

```

{AI05-0185-1} Returns the result of applying the To_Lower conversion to each Wide_Character element of the Wide_String designated by Item. The result is the null Wide_String if the value of the formal parameter is the null Wide_String. The lower bound of the result Wide_String is 1.

```ada
function To_Upper (Item : Wide_Character) return Wide_Character;

```

{AI05-0185-1} {AI05-0266-1} {AI05-0299-1} {AI12-0263-1} Returns the Simple Uppercase Mapping as defined by documents referenced in Clause 2 of ISO/IEC 10646:2017 of the Wide_Character designated by Item. If the Simple Uppercase Mapping does not exist for the Wide_Character designated by Item, then the value of Item is returned.

```ada
function To_Upper (Item : Wide_String) return Wide_String;

```

{AI05-0185-1} Returns the result of applying the To_Upper conversion to each Wide_Character element of the Wide_String designated by Item. The result is the null Wide_String if the value of the formal parameter is the null Wide_String. The lower bound of the result Wide_String is 1.

```ada
function To_Basic (Item : Wide_Character) return Wide_Character;

```

{AI12-0260-1} Returns the Wide_Character whose code point is given by the first value of its Decomposition Mapping in the code charts of ISO/IEC 10646:2017 if any; returns Item otherwise.

```ada
function To_Basic (Item : Wide_String) return Wide_String;

```

{AI12-0260-1} Returns the result of applying the To_Basic conversion to each Wide_Character element of the Wide_String designated by Item. The result is the null Wide_String if the value of the formal parameter is the null Wide_String. The lower bound of the result Wide_String is 1.


#### Implementation Advice

{AI05-0266-1} The string returned by Character_Set_Version should include either "10646:" or "Unicode".

Implementation Advice: The string returned by Wide_Characters.Handling.Character_Set_Version should include either "10646:" or "Unicode".

Discussion: {AI12-0263-1} The intent is that the returned string include the year for 10646 (as in "10646:2017"), and the version number for Unicode (as in "Unicode 10.0"). We don't try to specify that further so we don't need to decide how to represent Corrigenda for 10646, nor which of these is preferred. (Giving a Unicode version is more accurate, as the case folding and mapping rules always come from a Unicode version [10646 just tells one to look at Unicode to get those], and the character classifications ought to be the same for equivalent versions, but we don't want to talk about non-ISO standards in an ISO standard.) 

NOTE 1   {AI05-0266-1} {AI12-0440-1} The results returned by these functions can depend on which particular version of the 10646 standard is supported by the implementation (see 2.1).

NOTE 2   {AI05-0286-1} The case insensitive equality comparison routines provided in A.4.10, "String Comparison" are also available for wide strings (see A.4.7). 


#### Extensions to Ada 2005

{AI05-0185-1} {AI05-0266-1} The package Wide_Characters.Handling is new. 


#### Incompatibilities With Ada 2012

{AI12-0004-1} {AI12-0260-1} Added additional classification routines Is_Basic and Is_NFKC, and additional conversion routine To_Basic. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


## A.3.6  The Package Wide_Wide_Characters.Handling

{AI05-0185-1} The package Wide_Wide_Characters.Handling has the same contents as Wide_Characters.Handling except that each occurrence of Wide_Character is replaced by Wide_Wide_Character, and each occurrence of Wide_String is replaced by Wide_Wide_String. 


#### Extensions to Ada 2005

{AI05-0185-1} The package Wide_Wide_Characters.Handling is new. 

