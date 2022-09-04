---
sidebar_position:  20
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

the child packages Text_IO.Editing and Wide_Text_IO.Editing, which support formatted and localized output of decimal data, based on "picture String" values. 

See also: 3.5.9, "Fixed Point Types"; 3.5.10, "Operations of Fixed Point Types"; 4.6, "Type Conversions"; 13.3, "Representation Attributes"; A.10.9, "Input-Output for Real Types";B.4, "Interfacing with COBOL"; B.3, "Interfacing with C"; Annex G, "Numerics".

The character and string handling packages in Annex A, "Predefined Language Environment" are also relevant for Information Systems. 


#### Implementation Advice

If COBOL (respectively, C) is widely supported in the target environment, implementations supporting the Information Systems Annex should provide the child package Interfaces.COBOL (respectively, Interfaces.C) specified in Annex B and should support a convention_identifier of COBOL (respectively, C) in the interfacing pragmas (see Annex B), thus allowing Ada programs to interface with programs written in that language. 


#### Extensions to Ada 83

This Annex is new to Ada 95. 


## F.1  Machine_Radix Attribute Definition Clause


#### Static Semantics

Machine_Radix may be specified for a decimal first subtype (see 3.5.9) via an attribute_definition_clause; the expression of such a clause shall be static, and its value shall be 2 or 10. A value of 2 implies a binary base range; a value of 10 implies a decimal base range. 

Ramification: In the absence of a Machine_Radix clause, the choice of 2 versus 10 for S'Machine_Radix is not specified. 


#### Implementation Advice

Packed decimal should be used as the internal representation for objects of subtype S when S'Machine_Radix = 10. 

Discussion: The intent of a decimal Machine_Radix attribute definition clause is to allow the programmer to declare an Ada decimal data object whose representation matches a particular COBOL implementation's representation of packed decimal items. The Ada object may then be passed to an interfaced COBOL program that takes a packed decimal data item as a parameter, assuming that convention COBOL has been specified for the Ada object's type in a pragma Convention.

Additionally, the Ada compiler may choose to generate arithmetic instructions that exploit the packed decimal representation.


#### Examples

Example of Machine_Radix attribute definition clause: 

```ada
type Money is delta 0.01 digits 15;
for Money'Machine_Radix use 10;

```


## F.2  The Package Decimal


#### Static Semantics

The library package Decimal has the following declaration: 

```ada
package Ada.Decimal is
   pragma Pure(Decimal);

```

```ada
   Max_Scale : constant := implementation-defined;
   Min_Scale : constant := implementation-defined;

```

```ada
   Min_Delta : constant := 10.0**(-Max_Scale);
   Max_Delta : constant := 10.0**(-Min_Scale);

```

```ada
   Max_Decimal_Digits : constant := implementation-defined;

```

```ada
   generic
      type Dividend_Type  is delta &lt&gt digits &lt&gt;
      type Divisor_Type   is delta &lt&gt digits &lt&gt;
      type Quotient_Type  is delta &lt&gt digits &lt&gt;
      type Remainder_Type is delta &lt&gt digits &lt&gt;
   procedure Divide (Dividend  : in Dividend_Type;
                     Divisor   : in Divisor_Type;
                     Quotient  : out Quotient_Type;
                     Remainder : out Remainder_Type);
   pragma Convention(Intrinsic, Divide);

```

```ada
end Ada.Decimal;

```

Implementation defined: The values of named numbers in the package Decimal.

Max_Scale is the largest N such that 10.0**(N) is allowed as a decimal type's delta. Its type is universal_integer.

Min_Scale is the smallest N such that 10.0**(N) is allowed as a decimal type's delta. Its type is universal_integer.

Min_Delta is the smallest value allowed for delta in a decimal_fixed_point_definition. Its type is universal_real.

Max_Delta is the largest value allowed for delta in a decimal_fixed_point_definition. Its type is universal_real.

Max_Decimal_Digits is the largest value allowed for digits in a decimal_fixed_point_definition. Its type is universal_integer. 

Reason: The name is Max_Decimal_Digits versus Max_Digits, in order to avoid confusion with the named number System.Max_Digits relevant to floating point. 


#### Static Semantics

The effect of Divide is as follows. The value of Quotient is Quotient_Type(Dividend/Divisor). The value of Remainder is Remainder_Type(Intermediate), where Intermediate is the difference between Dividend and the product of Divisor and Quotient; this result is computed exactly. 


#### Implementation Requirements

Decimal.Max_Decimal_Digits shall be at least 18.

Decimal.Max_Scale shall be at least 18.

Decimal.Min_Scale shall be at most 0. 

NOTE 1   The effect of division yielding a quotient with control over rounding versus truncation is obtained by applying either the function attribute Quotient_Type'Round or the conversion Quotient_Type to the expression Dividend/Divisor. 


## F.3  Edited Output for Decimal Types

The child packages Text_IO.Editing and Wide_Text_IO.Editing provide localizable formatted text output, known as edited output, for decimal types. An edited output string is a function of a numeric value, program-specifiable locale elements, and a format control value. The numeric value is of some decimal type. The locale elements are: 

the currency string;

the digits group separator character;

the radix mark character; and

the fill character that replaces leading zeros of the numeric value. 

For Text_IO.Editing the edited output and currency strings are of type String, and the locale characters are of type Character. For Wide_Text_IO.Editing their types are Wide_String and Wide_Character, respectively.

Each of the locale elements has a default value that can be replaced or explicitly overridden.

A format-control value is of the private type Picture; it determines the composition of the edited output string and controls the form and placement of the sign, the position of the locale elements and the decimal digits, the presence or absence of a radix mark, suppression of leading zeros, and insertion of particular character values.

A Picture object is composed from a String value, known as a picture String, that serves as a template for the edited output string, and a Boolean value that controls whether a string of all space characters is produced when the number's value is zero. A picture String comprises a sequence of one- or two-Character symbols, each serving as a placeholder for a character or string at a corresponding position in the edited output string. The picture String symbols fall into several categories based on their effect on the edited output string: 

	Decimal Digit: 	'9'
	Radix Control: 	'.' 	'V'
	Sign Control: 	'+' 	'' 	'&lt' 	'&gt' 	"CR" 	"DB"
	Currency Control: 	'$' 	'#'
	Zero Suppression: 	'Z' 	'*'
	Simple Insertion: 	'_' 	'B' 	'0' 	'/'

The entries are not case-sensitive. Mixed- or lower-case forms for "CR" and "DB", and lower-case forms for 'V', 'Z', and 'B', have the same effect as the upper-case symbols shown.

An occurrence of a '9' Character in the picture String represents a decimal digit position in the edited output string.

A radix control Character in the picture String indicates the position of the radix mark in the edited output string: an actual character position for '.', or an assumed position for 'V'.

A sign control Character in the picture String affects the form of the sign in the edited output string. The '&lt' and '&gt' Character values indicate parentheses for negative values. A Character '+', '', or '&lt' appears either singly, signifying a fixed-position sign in the edited output, or repeated, signifying a floating-position sign that is preceded by zero or more space characters and that replaces a leading 0.

A currency control Character in the picture String indicates an occurrence of the currency string in the edited output string. The '$' Character represents the complete currency string; the '#' Character represents one character of the currency string. A '$' Character appears either singly, indicating a fixed-position currency string in the edited output, or repeated, indicating a floating-position currency string that occurs in place of a leading 0. A sequence of '#' Character values indicates either a fixed- or floating-position currency string, depending on context.

A zero suppression Character in the picture String allows a leading zero to be replaced by either the space character (for 'Z') or the fill character (for '*').

A simple insertion Character in the picture String represents, in general, either itself (if '/' or '0'), the space character (if 'B'), or the digits group separator character (if '_'). In some contexts it is treated as part of a floating sign, floating currency, or zero suppression string.

An example of a picture String is "&lt###Z_ZZ9.99&gt". If the currency string is "FF", the separator character is ',', and the radix mark is '.' then the edited output string values for the decimal values 32.10 and 5432.10 are "bbFFbbb32.10b" and "(bFF5,432.10)", respectively, where 'b' indicates the space character.

The generic packages Text_IO.Decimal_IO and Wide_Text_IO.Decimal_IO (see A.10.9, "Input-Output for Real Types") provide text input and nonedited text output for decimal types. 

NOTE 1   A picture String is of type Standard.String, both for Text_IO.Editing and Wide_Text_IO.Editing. 


### F.3.1  Picture String Formation

A well-formed picture String, or simply picture String, is a String value that conforms to the syntactic rules, composition constraints, and character replication conventions specified in this clause. 


#### Dynamic Semantics

 

picture_string ::=
   fixed_$_picture_string
 | fixed_#_picture_string
 | floating_currency_picture_string
 | non_currency_picture_string


fixed_$_picture_string ::=
   [fixed_LHS_sign] fixed_$_char {direct_insertion} [zero_suppression]
     number [RHS_sign]

 | [fixed_LHS_sign {direct_insertion}] [zero_suppression]
     number fixed_$_char {direct_insertion} [RHS_sign]

 | floating_LHS_sign number fixed_$_char {direct_insertion} [RHS_sign]

 | [fixed_LHS_sign] fixed_$_char {direct_insertion}
     all_zero_suppression_number {direct_insertion}  [RHS_sign]

 | [fixed_LHS_sign {direct_insertion}] all_zero_suppression_number {direct_insertion}
     fixed_$_char {direct_insertion} [RHS_sign]

 | all_sign_number {direct_insertion} fixed_$_char {direct_insertion} [RHS_sign]


fixed_#_picture_string ::=
   [fixed_LHS_sign] single_#_currency {direct_insertion}
     [zero_suppression] number [RHS_sign]

 | [fixed_LHS_sign] multiple_#_currency {direct_insertion}
     zero_suppression number [RHS_sign]

 | [fixed_LHS_sign {direct_insertion}] [zero_suppression]
     number fixed_#_currency {direct_insertion} [RHS_sign]

 | floating_LHS_sign number fixed_#_currency {direct_insertion} [RHS_sign]

 | [fixed_LHS_sign] single_#_currency {direct_insertion}
     all_zero_suppression_number {direct_insertion} [RHS_sign]

 | [fixed_LHS_sign] multiple_#_currency {direct_insertion}
     all_zero_suppression_number {direct_insertion} [RHS_sign]

 | [fixed_LHS_sign {direct_insertion}] all_zero_suppression_number {direct_insertion}
     fixed_#_currency {direct_insertion} [RHS_sign]

 | all_sign_number {direct_insertion} fixed_#_currency {direct_insertion} [RHS_sign]


floating_currency_picture_string ::=
   [fixed_LHS_sign] {direct_insertion} floating_$_currency number [RHS_sign]
 | [fixed_LHS_sign] {direct_insertion} floating_#_currency number [RHS_sign]
 | [fixed_LHS_sign] {direct_insertion} all_currency_number {direct_insertion} [RHS_sign]


non_currency_picture_string ::=
   [fixed_LHS_sign {direct_insertion}] zero_suppression number [RHS_sign]
 | [floating_LHS_sign] number [RHS_sign]
 | [fixed_LHS_sign {direct_insertion}] all_zero_suppression_number {direct_insertion}
     [RHS_sign]
 | all_sign_number {direct_insertion}
 | fixed_LHS_sign direct_insertion {direct_insertion} number [RHS_sign]


fixed_LHS_sign ::=  LHS_Sign

LHS_Sign ::=  + |  | &lt


fixed_$_char ::= $


direct_insertion ::=  simple_insertion

simple_insertion ::=  _ | B | 0 | /


zero_suppression ::=  Z {Z | context_sensitive_insertion} | fill_string

context_sensitive_insertion ::=  simple_insertion


fill_string ::=  * {* | context_sensitive_insertion}


number ::=
   fore_digits [radix [aft_digits] {direct_insertion}]
 | radix aft_digits {direct_insertion}

fore_digits ::= 9 {9 | direct_insertion}

aft_digits ::=  {9 | direct_insertion} 9

radix ::= . | V


RHS_sign ::= + |  | &gt | CR | DB


floating_LHS_sign ::=
   LHS_Sign {context_sensitive_insertion} LHS_Sign {LHS_Sign | context_sensitive_insertion}


single_#_currency ::= #

multiple_#_currency ::= ## {#}


fixed_#_currency ::= single_#_currency | multiple_#_currency


floating_$_currency ::=
   $ {context_sensitive_insertion} $ {$ | context_sensitive_insertion}


floating_#_currency ::=
   # {context_sensitive_insertion} # {# | context_sensitive_insertion}


all_sign_number ::=  all_sign_fore [radix [all_sign_aft]] [&gt]

all_sign_fore ::=
   sign_char {context_sensitive_insertion} sign_char {sign_char | context_sensitive_insertion}

all_sign_aft ::= {all_sign_aft_char} sign_char

all_sign_aft_char ::=  sign_char | context_sensitive_insertion

sign_char ::= + |  | &lt


all_currency_number ::=  all_currency_fore [radix [all_currency_aft]]

all_currency_fore ::=
   currency_char {context_sensitive_insertion}
     currency_char {currency_char | context_sensitive_insertion}

all_currency_aft ::= {all_currency_aft_char} currency_char

all_currency_aft_char ::= currency_char | context_sensitive_insertion

currency_char ::= $ | #


all_zero_suppression_number ::=  all_zero_suppression_fore [ radix [all_zero_suppression_aft]]

all_zero_suppression_fore ::=
   zero_suppression_char {zero_suppression_char | context_sensitive_insertion}

all_zero_suppression_aft ::= {all_zero_suppression_aft_char} zero_suppression_char

all_zero_suppression_aft_char ::=  zero_suppression_char | context_sensitive_insertion

zero_suppression_char ::= Z | *

The following composition constraints apply to a picture String: 

A floating_LHS_sign does not have occurrences of different LHS_Sign Character values.

If a picture String has '&lt' as fixed_LHS_sign, then it has '&gt' as RHS_sign.

If a picture String has '&lt' in a floating_LHS_sign or in an all_sign_number, then it has an occurrence of '&gt'.

If a picture String has '+' or '' as fixed_LHS_sign, in a floating_LHS_sign, or in an all_sign_number, then it has no RHS_sign.

An instance of all_sign_number does not have occurrences of different sign_char Character values.

An instance of all_currency_number does not have occurrences of different currency_char Character values.

An instance of all_zero_suppression_number does not have occurrences of different zero_suppression_char Character values, except for possible case differences between 'Z' and 'z'. 

A replicable Character is a Character that, by the above rules, can occur in two consecutive positions in a picture String.

A Character replication is a String 

```ada
char & '(' & spaces & count_string & ')'

```

where char is a replicable Character, spaces is a String (possibly empty) comprising only space Character values, and count_string is a String of one or more decimal digit Character values. A Character replication in a picture String has the same effect as (and is said to be equivalent to) a String comprising n consecutive occurrences of char, where n=Integer'Value(count_string).

An expanded picture String is a picture String containing no Character replications. 

Discussion: Since 'B' is not allowed after a RHS sign, there is no need for a special rule to disallow "9.99DB(2)" as an abbreviation for "9.99DBB" 

NOTE 1   Although a sign to the left of the number can float, a sign to the right of the number is in a fixed position. 


### F.3.2  Edited Output Generation


#### Dynamic Semantics

The contents of an edited output string are based on: 

A value, Item, of some decimal type Num,

An expanded picture String Pic_String,

A Boolean value, Blank_When_Zero,

A Currency string,

A Fill character,

A Separator character, and

A Radix_Mark character. 

The combination of a True value for Blank_When_Zero and a '*' character in Pic_String is inconsistent; no edited output string is defined. 

A layout error is identified in the rules below if leading nonzero digits of Item, character values of the Currency string, or a negative sign would be truncated; in such cases no edited output string is defined.

The edited output string has lower bound 1 and upper bound N where N = Pic_String'Length + Currency_Length_Adjustment  Radix_Adjustment, and 

Currency_Length_Adjustment = Currency'Length  1 if there is some occurrence of '$' in Pic_String, and 0 otherwise.

Radix_Adjustment = 1 if there is an occurrence of 'V' or 'v' in Pic_Str, and 0 otherwise. 

Let the magnitude of Item be expressed as a base-10 number Ip···I1.F1···Fq, called the displayed magnitude of Item, where: 

q = Min(Max(Num'Scale, 0), n) where n is 0 if Pic_String has no radix and is otherwise the number of digit positions following radix in Pic_String, where a digit position corresponds to an occurrence of '9', a zero_suppression_char (for an all_zero_suppression_number), a currency_char (for an all_currency_number), or a sign_char (for an all_sign_number).

Ip /= 0 if p&gt0. 

If n &lt Num'Scale, then the above number is the result of rounding (away from 0 if exactly midway between values).

If Blank_When_Zero = True and the displayed magnitude of Item is zero, then the edited output string comprises all space character values. Otherwise, the picture String is treated as a sequence of instances of syntactic categories based on the rules in F.3.1, and the edited output string is the concatenation of string values derived from these categories according to the following mapping rules.

Table F-1 shows the mapping from a sign control symbol to a corresponding character or string in the edited output. In the columns showing the edited output, a lower-case 'b' represents the space character. If there is no sign control symbol but the value of Item is negative, a layout error occurs and no edited output string is produced.

Table F-1: Edited Output for Sign Control SymbolsSign Control SymbolEdited Output for 
Nonnegative NumberEdited Output for 
Negative Number'+''+''''''b''''&lt''b''(''&gt''b'')'"CR""bb""CR""DB""bb""DB"An instance of fixed_LHS_sign maps to a character as shown in Table F-1.

An instance of fixed_$_char maps to Currency.

An instance of direct_insertion maps to Separator if direct_insertion = '_', and to the direct_insertion Character otherwise.

An instance of number maps to a string integer_part & radix_part & fraction_part where: 

The string for integer_part is obtained as follows: 

a)Occurrences of '9' in fore_digits of number are replaced from right to left with the decimal digit character values for I1, ..., Ip, respectively.

b)Each occurrence of '9' in fore_digits to the left of the leftmost '9' replaced according to rule 1 is replaced with '0'.

c)If p exceeds the number of occurrences of '9' in fore_digits of number, then the excess leftmost digits are eligible for use in the mapping of an instance of zero_suppression, floating_LHS_sign, floating_$_currency, or floating_#_currency to the left of number; if there is no such instance, then a layout error occurs and no edited output string is produced. 

The radix_part is: 

"" if number does not include a radix, if radix = 'V', or if radix = 'v'

Radix_Mark if number includes '.' as radix 

The string for fraction_part is obtained as follows: 

a)Occurrences of '9' in aft_digits of number are replaced from left to right with the decimal digit character values for F1, ... Fq.

b)Each occurrence of '9' in aft_digits to the right of the rightmost '9' replaced according to rule 1 is replaced by '0'. 

An instance of zero_suppression maps to the string obtained as follows: 

a)The rightmost 'Z', 'z', or '*' Character values are replaced with the excess digits (if any) from the integer_part of the mapping of the number to the right of the zero_suppression instance,

b)A context_sensitive_insertion Character is replaced as though it were a direct_insertion Character, if it occurs to the right of some 'Z', 'z', or '*' in zero_suppression that has been mapped to an excess digit,

c)Each Character to the left of the leftmost Character replaced according to rule 1 above is replaced by: 

the space character if the zero suppression Character is 'Z' or 'z', or

the Fill character if the zero suppression Character is '*'. 

d)A layout error occurs if some excess digits remain after all 'Z', 'z', and '*' Character values in zero_suppression have been replaced via rule 1; no edited output string is produced. 

An instance of RHS_sign maps to a character or string as shown in Table F-1.

An instance of floating_LHS_sign maps to the string obtained as follows. 

a)Up to all but one of the rightmost LHS_Sign Character values are replaced by the excess digits (if any) from the integer_part of the mapping of the number to the right of the floating_LHS_sign instance.

b)The next Character to the left is replaced with the character given by the entry in Table F-1 corresponding to the LHS_Sign Character.

c)A context_sensitive_insertion Character is replaced as though it were a direct_insertion Character, if it occurs to the right of the leftmost LHS_Sign character replaced according to rule 1.

d)Any other Character is replaced by the space character.

e)A layout error occurs if some excess digits remain after replacement via rule 1; no edited output string is produced. 

An instance of fixed_#_currency maps to the Currency string with n space character values concatenated on the left (if the instance does not follow a radix) or on the right (if the instance does follow a radix), where n is the difference between the length of the fixed_#_currency instance and Currency'Length. A layout error occurs if Currency'Length exceeds the length of the fixed_#_currency instance; no edited output string is produced.

An instance of floating_$_currency maps to the string obtained as follows: 

a)Up to all but one of the rightmost '$' Character values are replaced with the excess digits (if any) from the integer_part of the mapping of the number to the right of the floating_$_currency instance.

b)The next Character to the left is replaced by the Currency string.

c)A context_sensitive_insertion Character is replaced as though it were a direct_insertion Character, if it occurs to the right of the leftmost '$' Character replaced via rule 1.

d)Each other Character is replaced by the space character.

e)A layout error occurs if some excess digits remain after replacement by rule 1; no edited output string is produced. 

An instance of floating_#_currency maps to the string obtained as follows: 

a)Up to all but one of the rightmost '#' Character values are replaced with the excess digits (if any) from the integer_part of the mapping of the number to the right of the floating_#_currency instance.

b)The substring whose last Character occurs at the position immediately preceding the leftmost Character replaced via rule 1, and whose length is Currency'Length, is replaced by the Currency string.

c)A context_sensitive_insertion Character is replaced as though it were a direct_insertion Character, if it occurs to the right of the leftmost '#' replaced via rule 1.

d)Any other Character is replaced by the space character.

e)A layout error occurs if some excess digits remain after replacement rule 1, or if there is no substring with the required length for replacement rule 2; no edited output string is produced. 

An instance of all_zero_suppression_number maps to: 

a string of all spaces if the displayed magnitude of Item is zero, the zero_suppression_char is 'Z' or 'z', and the instance of all_zero_suppression_number does not have a radix at its last character position;

a string containing the Fill character in each position except for the character (if any) corresponding to radix, if zero_suppression_char = '*' and the displayed magnitude of Item is zero;

otherwise, the same result as if each zero_suppression_char in all_zero_suppression_aft were '9', interpreting the instance of all_zero_suppression_number as either zero_suppression number (if a radix and all_zero_suppression_aft are present), or as zero_suppression otherwise. 

An instance of all_sign_number maps to: 

a string of all spaces if the displayed magnitude of Item is zero and the instance of all_sign_number does not have a radix at its last character position;

otherwise, the same result as if each sign_char in all_sign_number_aft were '9', interpreting the instance of all_sign_number as either floating_LHS_sign number (if a radix and all_sign_number_aft are present), or as floating_LHS_sign otherwise. 

An instance of all_currency_number maps to: 

a string of all spaces if the displayed magnitude of Item is zero and the instance of all_currency_number does not have a radix at its last character position;

otherwise, the same result as if each currency_char in all_currency_number_aft were '9', interpreting the instance of all_currency_number as floating_$_currency number or floating_#_currency number (if a radix and all_currency_number_aft are present), or as floating_$_currency or floating_#_currency otherwise. 


#### Examples

In the result string values shown below, 'b' represents the space character.

```ada
Item:         Picture and Result Strings:

```

```ada
123456.78     Picture:  "-###**_***_**9.99"
                        "bbb$***123,456.78"
                        "bbFF***123.456,78" (currency = "FF",
                                             separator = '.',
                                             radix mark = ',')

```

```ada
123456.78     Picture:  "-$$$**_***_**9.99"
              Result:   "bbb$***123,456.78"
                       "bbbFF***123.456,78" (currency = "FF",
                                             separator = '.',
                                             radix mark = ',')

```

```ada
0.0          Picture: "-$$$$$$.$$"
             Result:  "bbbbbbbbbb"

```

```ada
0.20         Picture: "-$$$$$$.$$"
             Result:  "bbbbbb$.20"

```

```ada
-1234.565    Picture: "&lt&lt&lt&lt_&lt&lt&lt.&lt&lt###&gt"
             Result:  "bb(1,234.57DMb)"  (currency = "DM")

```

```ada
12345.67     Picture: "###_###_##9.99"
             Result:  "bbCHF12,345.67"   (currency = "CHF")

```


### F.3.3  The Package Text_IO.Editing

The package Text_IO.Editing provides a private type Picture with associated operations, and a generic package Decimal_Output. An object of type Picture is composed from a well-formed picture String (see F.3.1) and a Boolean item indicating whether a zero numeric value will result in an edited output string of all space characters. The package Decimal_Output contains edited output subprograms implementing the effects defined in F.3.2. 


#### Static Semantics

The library package Text_IO.Editing has the following declaration: 

```ada
package Ada.Text_IO.Editing is

```

```ada
   type Picture is private;

```

```ada
   function Valid (Pic_String      : in String;
                   Blank_When_Zero : in Boolean := False) return Boolean;

```

```ada
   function To_Picture (Pic_String      : in String;
                        Blank_When_Zero : in Boolean := False)
      return Picture;

```

```ada
   function Pic_String      (Pic : in Picture) return String;
   function Blank_When_Zero (Pic : in Picture) return Boolean;

```

```ada
   Max_Picture_Length  : constant := implementation_defined;

```

```ada
   Picture_Error       : exception;

```

```ada
   Default_Currency    : constant String    := "$";
   Default_Fill        : constant Character := '*';
   Default_Separator   : constant Character := ',';
   Default_Radix_Mark  : constant Character := '.';

```

```ada
   generic
      type Num is delta &lt&gt digits &lt&gt;
      Default_Currency   : in String    := Text_IO.Editing.Default_Currency;
      Default_Fill       : in Character := Text_IO.Editing.Default_Fill;
      Default_Separator  : in Character :=
                              Text_IO.Editing.Default_Separator;
      Default_Radix_Mark : in Character :=
                              Text_IO.Editing.Default_Radix_Mark;
   package Decimal_Output is
      function Length (Pic      : in Picture;
                       Currency : in String := Default_Currency)
         return Natural;

```

```ada
      function Valid (Item     : in Num;
                      Pic      : in Picture;
                      Currency : in String := Default_Currency)
         return Boolean;

```

```ada
      function Image (Item       : in Num;
                      Pic        : in Picture;
                      Currency   : in String    := Default_Currency;
                      Fill       : in Character := Default_Fill;
                      Separator  : in Character := Default_Separator;
                      Radix_Mark : in Character := Default_Radix_Mark)
         return String;

```

```ada
      procedure Put (File       : in File_Type;
                     Item       : in Num;
                     Pic        : in Picture;
                     Currency   : in String    := Default_Currency;
                     Fill       : in Character := Default_Fill;
                     Separator  : in Character := Default_Separator;
                     Radix_Mark : in Character := Default_Radix_Mark);

```

```ada
      procedure Put (Item       : in Num;
                     Pic        : in Picture;
                     Currency   : in String    := Default_Currency;
                     Fill       : in Character := Default_Fill;
                     Separator  : in Character := Default_Separator;
                     Radix_Mark : in Character := Default_Radix_Mark);

```

```ada
      procedure Put (To         : out String;
                     Item       : in Num;
                     Pic        : in Picture;
                     Currency   : in String    := Default_Currency;
                     Fill       : in Character := Default_Fill;
                     Separator  : in Character := Default_Separator;
                     Radix_Mark : in Character := Default_Radix_Mark);
   end Decimal_Output;
private
   ... -- not specified by the language
end Ada.Text_IO.Editing;

```

Implementation defined: The value of Max_Picture_Length in the package Text_IO.Editing

The exception Constraint_Error is raised if the Image function or any of the Put procedures is invoked with a null string for Currency. 

```ada
function Valid (Pic_String      : in String;
                Blank_When_Zero : in Boolean := False) return Boolean;

```

Valid returns True if Pic_String is a well-formed picture String (see F.3.1) the length of whose expansion does not exceed Max_Picture_Length, and if either Blank_When_Zero is False or Pic_String contains no '*'.

```ada
function To_Picture (Pic_String      : in String;
                     Blank_When_Zero : in Boolean := False)
   return Picture;

```

To_Picture returns a result Picture such that the application of the function Pic_String to this result yields an expanded picture String equivalent to Pic_String, and such that Blank_When_Zero applied to the result Picture is the same value as the parameter Blank_When_Zero. Picture_Error is raised if not Valid(Pic_String, Blank_When_Zero).

```ada
function Pic_String      (Pic : in Picture) return String;

function Blank_When_Zero (Pic : in Picture) return Boolean;

```

If Pic is To_Picture(String_Item, Boolean_Item) for some String_Item and Boolean_Item, then: 

Pic_String(Pic) returns an expanded picture String equivalent to String_Item and with any lower-case letter replaced with its corresponding upper-case form, and

Blank_When_Zero(Pic) returns Boolean_Item. 

If Pic_1 and Pic_2 are objects of type Picture, then "="(Pic_1, Pic_2) is True when 

Pic_String(Pic_1) = Pic_String(Pic_2), and

Blank_When_Zero(Pic_1) = Blank_When_Zero(Pic_2). 

```ada
function Length (Pic      : in Picture;
                 Currency : in String := Default_Currency)
   return Natural;

```

Length returns Pic_String(Pic)'Length + Currency_Length_Adjustment  Radix_Adjustment where 

Currency_Length_Adjustment = 

Currency'Length  1 if there is some occurrence of '$' in Pic_String(Pic), and

0 otherwise. 

Radix_Adjustment = 

1 if there is an occurrence of 'V' or 'v' in Pic_Str(Pic), and

0 otherwise. 

```ada
function Valid (Item     : in Num;
                Pic      : in Picture;
                Currency : in String := Default_Currency)
   return Boolean;

```

Valid returns True if Image(Item, Pic, Currency) does not raise Layout_Error, and returns False otherwise.

```ada
function Image (Item       : in Num;
                Pic        : in Picture;
                Currency   : in String    := Default_Currency;
                Fill       : in Character := Default_Fill;
                Separator  : in Character := Default_Separator;
                Radix_Mark : in Character := Default_Radix_Mark)
   return String;

```

Image returns the edited output String as defined in F.3.2 for Item, Pic_String(Pic), Blank_When_Zero(Pic), Currency, Fill, Separator, and Radix_Mark. If these rules identify a layout error, then Image raises the exception Layout_Error.

```ada
procedure Put (File       : in File_Type;
               Item       : in Num;
               Pic        : in Picture;
               Currency   : in String    := Default_Currency;
               Fill       : in Character := Default_Fill;
               Separator  : in Character := Default_Separator;
               Radix_Mark : in Character := Default_Radix_Mark);

procedure Put (Item       : in Num;
               Pic        : in Picture;
               Currency   : in String    := Default_Currency;
               Fill       : in Character := Default_Fill;
               Separator  : in Character := Default_Separator;
               Radix_Mark : in Character := Default_Radix_Mark);

```

Each of these Put procedures outputs Image(Item, Pic, Currency, Fill, Separator, Radix_Mark) consistent with the conventions for Put for other real types in case of bounded line length (see A.10.6, "Get and Put Procedures").

```ada
procedure Put (To         : out String;
               Item       : in Num;
               Pic        : in Picture;
               Currency   : in String    := Default_Currency;
               Fill       : in Character := Default_Fill;
               Separator  : in Character := Default_Separator;
               Radix_Mark : in Character := Default_Radix_Mark);

```

Put copies Image(Item, Pic, Currency, Fill, Separator, Radix_Mark) to the given string, right justified. Otherwise unassigned Character values in To are assigned the space character. If To'Length is less than the length of the string resulting from Image, then Layout_Error is raised. 


#### Implementation Requirements

Max_Picture_Length shall be at least 30. The implementation shall support currency strings of length up to at least 10, both for Default_Currency in an instantiation of Decimal_Output, and for Currency in an invocation of Image or any of the Put procedures. 

Discussion: This implies that a picture string with character replications need not be supported (i.e., To_Picture will raise Picture_Error) if its expanded form exceeds 30 characters. 

NOTE   The rules for edited output are based on COBOL (ANSI X3.23:1985, endorsed by ISO as ISO 1989-1985), with the following differences: 

The COBOL provisions for picture string localization and for 'P' format are absent from Ada.

The following Ada facilities are not in COBOL: 

currency symbol placement after the number,

localization of edited output string for multi-character currency string values, including support for both length-preserving and length-expanding currency symbols in picture strings

localization of the radix mark, digits separator, and fill character, and

parenthesization of negative values. 

The value of 30 for Max_Picture_Length is the same limit as in COBOL. 

Reason: There are several reasons we have not adopted the COBOL-style permission to provide a single-character replacement in the picture string for the `$' as currency symbol, or to interchange the roles of `.' and `,' in picture strings 

It would have introduced considerable complexity into Ada, as well as confusion between run-time and compile-time character interpretation, since picture Strings are dynamically computable in Ada, in contrast with COBOL

Ada's rules for real literals provide a natural interpretation of `_' as digits separator and `.' for radix mark; it is not essential to allow these to be localized in picture strings, since Ada does not allow them to be localized in real literals.

The COBOL restriction for the currency symbol in a picture string to be replaced by a single character currency symbol is a compromise solution. For general international usage a mechanism is needed to localize the edited output to be a multi-character currency string. Allowing a single-Character localization for the picture Character, and a multiple-character localization for the currency string, would be an unnecessary complication. 


### F.3.4  The Package Wide_Text_IO.Editing


#### Static Semantics

The child package Wide_Text_IO.Editing has the same contents as Text_IO.Editing, except that: 

each occurrence of Character is replaced by Wide_Character,

each occurrence of Text_IO is replaced by Wide_Text_IO,

the subtype of Default_Currency is Wide_String rather than String, and

each occurrence of String in the generic package Decimal_Output is replaced by Wide_String. 

Implementation defined: The value of Max_Picture_Length in the package Wide_Text_IO.Editing

NOTE 1   Each of the functions Wide_Text_IO.Editing.Valid, To_Picture, and Pic_String has String (versus Wide_String) as its parameter or result subtype, since a picture String is not localizable. 

