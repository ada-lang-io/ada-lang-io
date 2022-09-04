---
sidebar_position:  3
---

# 2 Lexical Elements

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[The text of a program consists of the texts of one or more [compilation](S0214)s. The text of a [compilation](S0214) is a sequence of lexical elements, each composed of characters; the rules of composition are given in this section. [Pragma](S0016)s, which provide certain information for the compiler, are also described in this section.] 


## 2.1  Character Set

The only characters allowed outside of [comment](S0015)s are the [graphic_character](S0001)s and format_effectors.

Ramification: Any character, including an other_control_function, is allowed in a comment.

Note that this rule doesn't really have much force, since the implementation can represent characters in the source in any way it sees fit. For example, an implementation could simply define that what seems to be a nongraphic, non-format-effector character is actually a representation of the space character. 

Discussion: It is our intent to follow the terminology of ISO 10646 BMP where appropriate, and to remain compatible with the character classifications defined in A.3, "Character Handling".Note that our definition for [graphic_character](S0001) is more inclusive than that of ISO 10646-1. 


#### Syntax

character ::= [graphic_character](S0001) | format_effector | other_control_function

graphic_character ::= identifier_letter | digit | space_character | special_character


#### Static Semantics

The character repertoire for the text of an Ada program consists of the collection of characters called the Basic Multilingual Plane (BMP) of the ISO 10646 Universal Multiple-Octet Coded Character Set, plus a set of format_effectors and, in comments only, a set of other_control_functions; the coded representation for these characters is implementation defined [(it need not be a representation defined within ISO-10646-1)]. 

Implementation defined: The coded representation for the text of an Ada program.

The description of the language definition in this document uses the graphic symbols defined for Row 00: Basic Latin and Row 00: Latin-1 Supplement of the ISO 10646 BMP; these correspond to the graphic symbols of ISO 8859-1 (Latin-1); no graphic symbols are used in this document for characters outside of Row 00 of the BMP. The actual set of graphic symbols used by an implementation for the visual representation of the text of an Ada program is not specified. 

The categories of characters are defined as follows: 

identifier_letterupper_case_identifier_letter | lower_case_identifier_letter 

Discussion: We use identifier_letter instead of simply letter because ISO 10646 BMP includes many other characters that would generally be considered "letters". 

upper_case_identifier_letterAny character of Row 00 of ISO 10646 BMP whose name begins "Latin Capital Letter".

lower_case_identifier_letterAny character of Row 00 of ISO 10646 BMP whose name begins "Latin Small Letter". 

To be honest: The above rules do not include the ligatures Æ and æ. However, the intent is to include these characters as identifier letters. This problem was pointed out by a comment from the Netherlands. 

digitOne of the characters 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.

space_characterThe character of ISO 10646 BMP named "Space".

special_characterAny character of the ISO 10646 BMP that is not reserved for a control function, and is not the space_character, an identifier_letter, or a digit. 

Ramification: Note that the no break space and soft hyphen are special_characters, and therefore [graphic_character](S0001)s. They are not the same characters as space and hyphen-minus. 

format_effectorThe control functions of ISO 6429 called character tabulation (HT), line tabulation (VT), carriage return (CR), line feed (LF), and form feed (FF). 

other_control_functionAny control function, other than a format_effector, that is allowed in a comment; the set of other_control_functions allowed in comments is implementation defined. 

This paragraph was deleted.Implementation defined: The control functions allowed in comments.

The following names are used when referring to certain special_characters: 

Discussion: These are the ones that play a special role in the syntax of Ada 95, or in the syntax rules; we don't bother to define names for all characters. The first name given is the name from ISO 10646-1; the subsequent names, if any, are those used within the standard, depending on context. 

      symbol

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

         / 

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

solidus, divide

      symbol

         :
         ;
         &lt
         =
         &gt
         _
         |
         [
         ]
         {

,Old=&lt&gt&gt],Old=&lt         }&gt&gt 

name

colon
semicolon
less-than sign
equals sign
greater-than sign
low line, underline
vertical line
left square bracket
right square bracket
left curly bracket

right curly bracket 


#### Implementation Permissions

In a nonstandard mode, the implementation may support a different character repertoire[; in particular, the set of characters that are considered identifier_letters can be extended or changed to conform to local conventions]. 

Ramification: If an implementation supports other character sets, it defines which characters fall into each category, such as "identifier_letter", and what the corresponding rules of this section are, such as which characters are allowed in the text of a program.

NOTE   Every code position of ISO 10646 BMP that is not reserved for a control function is defined to be a [graphic_character](S0001) by this document. This includes all code positions other than 0000 - 001F, 007F - 009F, and FFFE - FFFF.

NOTE   The language does not specify the source representation of programs. 

Discussion: Any source representation is valid so long as the implementer can produce an (information-preserving) algorithm for translating both directions between the representation and the standard character set. (For example, every character in the standard character set has to be representable, even if the output devices attached to a given computer cannot print all of those characters properly.) From a practical point of view, every implementer will have to provide some way to process the ACVC. It is the intent to allow source representations, such as parse trees, that are not even linear sequences of characters. It is also the intent to allow different fonts: reserved words might be in bold face, and that should be irrelevant to the semantics. 


#### Extensions to Ada 83

Ada 95 allows 8-bit and 16-bit characters, as well as implementation-specified character sets. 


#### Wording Changes from Ada 83

The syntax rules in this clause are modified to remove the emphasis on basic characters vs. others. (In this day and age, there is no need to point out that you can write programs without using (for example) lower case letters.) In particular, character (representing all characters usable outside comments) is added, and basic_graphic_character, other_special_character, and basic_character are removed. Special_character is expanded to include Ada 83's other_special_character, as well as new 8-bit characters not present in Ada 83. Note that the term "basic letter" is used in A.3, "Character Handling" to refer to letters without diacritical marks.

Character names now come from ISO 10646.

We use identifier_letter rather than letter since ISO 10646 BMP includes many "letters' that are not permitted in identifiers (in the standard mode). 


## 2.2  Lexical Elements, Separators, and Delimiters


#### Static Semantics

The text of a program consists of the texts of one or more [compilation](S0214)s. The text of each [compilation](S0214) is a sequence of separate lexical elements. Each lexical element is formed from a sequence of characters, and is either a delimiter, an [identifier](S0002), a reserved word, a [numeric_literal](S0004), a [character_literal](S0012), a [string_literal](S0013), or a comment. The meaning of a program depends only on the particular sequences of lexical elements that form its [compilation](S0214)s, excluding [comment](S0015)s.

The text of a [compilation](S0214) is divided into lines. In general, the representation for an end of line is implementation defined. However, a sequence of one or more format_effectors other than character tabulation (HT) signifies at least one end of line. 

Implementation defined: The representation for an end of line.

[In some cases an explicit separator is required to separate adjacent lexical elements.] A separator is any of a space character, a format effector, or the end of a line, as follows: 

Discussion: It might be useful to define "white space" and use it here. 

A space character is a separator except within a [comment](S0015), a [string_literal](S0013), or a [character_literal](S0012).

Character tabulation (HT) is a separator except within a [comment](S0015).

The end of a line is always a separator. 

One or more separators are allowed between any two adjacent lexical elements, before the first of each [compilation](S0214), or after the last. At least one separator is required between an [identifier](S0002), a reserved word, or a [numeric_literal](S0004) and an adjacent [identifier](S0002), reserved word, or [numeric_literal](S0004).

A delimiter is either one of the following special characters 

&    '    (    )    *    +    ,        .    /    :    ;    &lt    =    &gt    |

or one of the following compound delimiters each composed of two adjacent special characters 

=&gt    ..    **    :=    /=    &gt=    &lt=    &lt&lt    &gt&gt    &lt&gt

Each of the special characters listed for single character delimiters is a single delimiter except if this character is used as a character of a compound delimiter, or as a character of a [comment](S0015), [string_literal](S0013), [character_literal](S0012), or [numeric_literal](S0004).

The following names are used when referring to compound delimiters:


#### Implementation Requirements

delimiter name=&gtarrow..double dot**double star, exponentiate:=assignment (pronounced: "becomes")/=inequality (pronounced: "not equal")&gt=greater than or equal&lt=less than or equal&lt&ltleft label bracket&gt&gtright label bracket&lt&gtboxAn implementation shall support lines of at least 200 characters in length, not counting any characters used to signify the end of a line. An implementation shall support lexical elements of at least 200 characters in length. The maximum supported line length and lexical element length are implementation defined. 

Implementation defined: Maximum supported line length and lexical element length.

Discussion: From URG recommendation. 


## 2.3  Identifiers

[Identifier](S0002)s are used as names. 


#### Syntax

identifier ::= 
   identifier_letter {[underline] [letter_or_digit](S0003)}

letter_or_digit ::= identifier_letter | digit

An [identifier](S0002) shall not be a reserved word. 


#### Static Semantics

All characters of an [identifier](S0002) are significant, including any underline character. [Identifier](S0002)s differing only in the use of corresponding upper and lower case letters are considered the same. 

Discussion: Two of the letters of ISO 8859-1 appear only as lower case, "sharp s" and "y with diaeresis". These two letters have no corresponding upper case letter (in particular, they are not considered equivalent to one another).


#### Implementation Permissions

In a nonstandard mode, an implementation may support other upper/lower case equivalence rules for [identifier](S0002)s[, to accommodate local conventions]. 


#### Examples

Examples of identifiers: 

```ada
Count      X    Get_Symbol   Ethelyn   Marion
Snobol_4   X1   Page_Count   Store_Next_Item

```


#### Wording Changes from Ada 83

We no longer include reserved words as [identifier](S0002)s. This is not a language change. In Ada 83, [identifier](S0002) included reserved words. However, this complicated several other rules (for example, regarding implementation-defined attributes and pragmas, etc.). We now explicitly allow certain reserved words for attribute designators, to make up for the loss. 

Ramification: Because syntax rules are relevant to overload resolution, it means that if it looks like a reserved word, it is not an [identifier](S0002). As a side effect, implementations cannot use reserved words as implementation-defined attributes or pragma names. 


## 2.4  Numeric Literals

There are two kinds of [numeric_literal](S0004)s, real literals and integer literals. A real literal is a [numeric_literal](S0004) that includes a point; an integer literal is a [numeric_literal](S0004) without a point. 


#### Syntax

numeric_literal ::= [decimal_literal](S0005) | [based_literal](S0008)

NOTE 1   The type of an integer literal is universal_integer. The type of a real literal is universal_real. 


### 2.4.1  Decimal Literals

A [decimal_literal](S0005) is a [numeric_literal](S0004) in the conventional decimal notation (that is, the base is ten). 


#### Syntax

decimal_literal ::= [numeral](S0006) [.[numeral](S0006)] [[exponent](S0007)]

numeral ::= digit {[underline] digit}

exponent ::= E [+] [numeral](S0006) | E  [numeral](S0006)

An [exponent](S0007) for an integer literal shall not have a minus sign. 

Ramification: Although this rule is in this subclause, it applies also to the next subclause. 


#### Static Semantics

An underline character in a [numeric_literal](S0004) does not affect its meaning. The letter E of an [exponent](S0007) can be written either in lower case or in upper case, with the same meaning. 

Ramification: Although these rules are in this subclause, they apply also to the next subclause. 

An [exponent](S0007) indicates the power of ten by which the value of the [decimal_literal](S0005) without the [exponent](S0007) is to be multiplied to obtain the value of the [decimal_literal](S0005) with the [exponent](S0007). 


#### Examples

Examples of decimal literals: 

```ada
12        0      1E6    123_456    --  integer literals

12.0      0.0    0.456  3.14159_26 --  real literals

```


#### Wording Changes from Ada 83

We have changed the syntactic category name integer to be [numeral](S0006). We got this idea from ACID. It avoids the confusion between this and integers. (Other places don't offer similar confusions. For example, a [string_literal](S0013) is different from a string.) 


### 2.4.2  Based Literals

[ A [based_literal](S0008) is a [numeric_literal](S0004) expressed in a form that specifies the base explicitly.] 


#### Syntax

based_literal ::= 
   [base](S0009) # [based_numeral](S0010) [.[based_numeral](S0010)] # [[exponent](S0007)]

base ::= [numeral](S0006)

based_numeral ::= 
   [extended_digit](S0011) {[underline] [extended_digit](S0011)}

extended_digit ::= digit | A | B | C | D | E | F


#### Legality Rules

The base (the numeric value of the decimal [numeral](S0006) preceding the first #) shall be at least two and at most sixteen. The [extended_digit](S0011)s A through F represent the digits ten through fifteen, respectively. The value of each [extended_digit](S0011) of a [based_literal](S0008) shall be less than the base. 


#### Static Semantics

The conventional meaning of based notation is assumed. An [exponent](S0007) indicates the power of the base by which the value of the [based_literal](S0008) without the [exponent](S0007) is to be multiplied to obtain the value of the [based_literal](S0008) with the [exponent](S0007). The [base](S0009) and the [exponent](S0007), if any, are in decimal notation.

The [extended_digit](S0011)s A through F can be written either in lower case or in upper case, with the same meaning. 


#### Examples

Examples of based literals: 

```ada
2#1111_1111#  16#FF#       016#0ff#   --  integer literals of value 255
16#E#E1       2#1110_0000#            --  integer literals of value 224
16#F.FF#E+2   2#1.1111_1111_1110#E11  --  real literals of value 4095.0

```


#### Wording Changes from Ada 83

The rule about which letters are allowed is now encoded in BNF, as suggested by Mike Woodger. This is clearly more readable. 


## 2.5  Character Literals

[A [character_literal](S0012) is formed by enclosing a graphic character between two apostrophe characters.] 


#### Syntax

character_literal ::= 'graphic_character'

NOTE 1   A [character_literal](S0012) is an enumeration literal of a character type. See 3.5.2. 


#### Examples

Examples of character literals: 

```ada
'A'     '*'     '''     ' '

```


#### Wording Changes from Ada 83

The definitions of the values of literals are in Sections 3 and 4, rather than here, since it requires knowledge of types. 


## 2.6  String Literals

[A [string_literal](S0013) is formed by a sequence of graphic characters (possibly none) enclosed between two quotation marks used as string brackets. They are used to represent [operator_symbol](S0147)s (see 6.1), values of a string type (see 4.2), and array subaggregates (see 4.3.3). ]


#### Syntax

string_literal ::= "{[string_element](S0014)}"

string_element ::= "" | non_quotation_mark_graphic_character

A [string_element](S0014) is either a pair of quotation marks (""), or a single graphic_character other than a quotation mark. 


#### Static Semantics

The sequence of characters of a [string_literal](S0013) is formed from the sequence of [string_element](S0014)s between the bracketing quotation marks, in the given order, with a [string_element](S0014) that is "" becoming a single quotation mark in the sequence of characters, and any other [string_element](S0014) being reproduced in the sequence.

A null string literal is a [string_literal](S0013) with no [string_element](S0014)s between the quotation marks.

NOTE 1   An end of line cannot appear in a [string_literal](S0013).


#### Examples

Examples of string literals: 

```ada
"Message of the day:"

""                    --  a null string literal
" "   "A"   """"      --  three string literals of length 1

"Characters such as $, %, and } are allowed in string literals"


```


#### Wording Changes from Ada 83

The wording has been changed to be strictly lexical. No mention is made of string or character values, since [string_literal](S0013)s are also used to represent [operator_symbol](S0147)s, which don't have a defined value.

The syntax is described differently. 


## 2.7  Comments

A [comment](S0015) starts with two adjacent hyphens and extends up to the end of the line. 


#### Syntax

comment ::= --{non_end_of_line_character}

A [comment](S0015) may appear on any line of a program. 


#### Static Semantics

The presence or absence of [comment](S0015)s has no influence on whether a program is legal or illegal. Furthermore, [comment](S0015)s do not influence the meaning of a program; their sole purpose is the enlightenment of the human reader. 


#### Examples

Examples of comments: 

```ada
--  the last sentence above echoes the Algol 68 report 

end;  --  processing of Line is complete 

--  a long comment may be split onto
--  two or more consecutive lines   

----------------  the first two hyphens start the comment  

```


## 2.8  Pragmas

A pragma is a compiler directive. There are language-defined pragmas that give instructions for optimization, listing control, etc. An implementation may support additional (implementation-defined) pragmas. Version=[5],Kind=(AddedNormal),Group=[C],Term=[pragma], Def=[a compiler directive to provide control over and above that provided by the other syntactic constructs of the language], Note1=[There are language-defined pragmas that give instructions for optimization, listing control, etc. An implementation can support additional (implementation-defined) pragmas.] 


#### Syntax

pragma ::= 
   pragma [identifier](S0002) [([pragma_argument_association](S0017) {, [pragma_argument_association](S0017)})];

pragma_argument_association ::= 
     [pragma_argument_[identifier](S0002) =&gt] [name](S0084)
   | [pragma_argument_[identifier](S0002) =&gt] [expression](S0108)

In a [pragma](S0016), any [pragma_argument_association](S0017)s without a pragma_argument_[identifier](S0002) shall precede any associations with a pragma_argument_[identifier](S0002).

[Pragma](S0016)s are only allowed at the following places in a program: 

After a semicolon delimiter, but not within a [formal_part](S0151) or [discriminant_part](S0056).

At any place where the syntax rules allow a construct defined by a syntactic category whose name ends with "declaration", "statement", "clause", or "alternative", or one of the syntactic categories [variant](S0069) or [exception_handler](S0232); but not in place of such a construct. Also at any place where a [compilation_unit](S0215) would be allowed.

Additional syntax rules and placement restrictions exist for specific pragmas. 

Discussion: The above rule is written in text, rather than in BNF; the syntactic category [pragma](S0016) is not used in any BNF syntax rule. 

Ramification: A [pragma](S0016) is allowed where a [generic_formal_parameter_declaration](S0240) is allowed. 

The name of a [pragma](S0016) is the identifier following the reserved word pragma. The [name](S0084) or [expression](S0108) of a [pragma_argument_association](S0017) is a pragma argument.

An identifier specific to a pragma is an identifier that is used in a pragma argument with special meaning for that pragma. 

To be honest: Whenever the syntax rules for a given pragma allow "[identifier](S0002)" as an argument of the [pragma](S0016), that [identifier](S0002) is an identifier specific to that pragma.


#### Static Semantics

If an implementation does not recognize the name of a [pragma](S0016), then it has no effect on the semantics of the program. Inside such a [pragma](S0016), the only rules that apply are the Syntax Rules. 

To be honest: This rule takes precedence over any other rules that imply otherwise. 

Ramification: Note well: this rule applies only to [pragma](S0016)s whose name is not recognized. If anything else is wrong with a [pragma](S0016) (at compile time), the [pragma](S0016) is illegal. This is true whether the [pragma](S0016) is language defined or implementation defined.

For example, an expression in an unrecognized [pragma](S0016) does not cause freezing, even though the rules in 13.14, "Freezing Rules" say it does; the above rule overrules those other rules. On the other hand, an expression in a recognized [pragma](S0016) causes freezing, even if this makes something illegal.

For another example, an expression that would be ambiguous is not illegal if it is inside an unrecognized [pragma](S0016).

Note, however, that implementations have to recognize pragma Inline(Foo) and freeze things accordingly, even if they choose to never do inlining.

Obviously, the contradiction needs to be resolved one way or the other. The reasons for resolving it this way are: The implementation is simple - the compiler can just ignore the [pragma](S0016) altogether. The interpretation of constructs appearing inside implementation-defined [pragma](S0016)s is implementation defined. For example: "pragma Mumble(X);". If the current implementation has never heard of Mumble, then it doesn't know whether X is a name, an expression, or an identifier specific to the pragma Mumble. 

To be honest: The syntax of individual pragmas overrides the general syntax for [pragma](S0016). 

Ramification: Thus, an identifier specific to a [pragma](S0016) is not a [name](S0084), syntactically; if it were, the visibility rules would be invoked, which is not what we want.

This also implies that named associations do not allow one to give the arguments in an arbitrary order - the order given in the syntax rule for each individual pragma must be obeyed. However, it is generally possible to leave out earlier arguments when later ones are given; for example, this is allowed by the syntax rule for pragma Import (see B.1, "Interfacing Pragmas"). As for subprogram calls, positional notation precedes named notation.

Note that Ada 83 had no pragmas for which the order of named associations mattered, since there was never more than one argument that allowed named associations. 

To be honest: The interpretation of the arguments of implementation-defined pragmas is implementation defined. However, the syntax rules have to be obeyed. 


#### Dynamic Semantics

Any [pragma](S0016) that appears at the place of an executable construct is executed. Unless otherwise specified for a particular pragma, this execution consists of the evaluation of each evaluable pragma argument in an arbitrary order. 

Ramification: For a [pragma](S0016) that appears at the place of an elaborable construct, execution is elaboration.

An identifier specific to a pragma is neither a [name](S0084) nor an [expression](S0108) - such identifiers are not evaluated (unless an implementation defines them to be evaluated in the case of an implementation-defined [pragma](S0016)).

The "unless otherwise specified" part allows us (and implementations) to make exceptions, so a [pragma](S0016) can contain an expression that is not evaluated. Note that [pragma](S0016)s in [type_definition](S0022)s may contain expressions that depend on discriminants.

When we wish to define a pragma with some run-time effect, we usually make sure that it appears in an executable context; otherwise, special rules are needed to define the run-time effect and when it happens. 


#### Implementation Requirements

The implementation shall give a warning message for an unrecognized pragma name. 

Ramification: An implementation is also allowed to have modes in which a warning message is suppressed, or in which the presence of an unrecognized [pragma](S0016) is a compile-time error. 


#### Implementation Permissions

An implementation may provide implementation-defined pragmas; the name of an implementation-defined pragma shall differ from those of the language-defined pragmas. 

Implementation defined: Implementation-defined pragmas.

Ramification: The semantics of implementation-defined pragmas, and any associated rules (such as restrictions on their placement or arguments), are, of course, implementation defined. Implementation-defined pragmas may have run-time effects. 

An implementation may ignore an unrecognized pragma even if it violates some of the Syntax Rules, if detecting the syntax error is too complex. 

Reason: Many compilers use extra post-parsing checks to enforce the syntax rules, since the Ada syntax rules are not LR(k) (for any k). (The grammar is ambiguous, in fact.) This paragraph allows them to ignore an unrecognized pragma, without having to perform such post-parsing checks. 


#### Implementation Advice

Normally, implementation-defined pragmas should have no semantic effect for error-free programs; that is, if the implementation-defined pragmas are removed from a working program, the program should still be legal, and should still have the same semantics. 

Ramification: Note that "semantics" is not the same as "effect;" as explained in , the semantics defines a set of possible effects.

Note that adding a [pragma](S0016) to a program might cause an error (either at compile time or at run time). On the other hand, if the language-specified semantics for a feature are in part implementation defined, it makes sense to support pragmas that control the feature, and that have real semantics; thus, this paragraph is merely a recommendation. 

Normally, an implementation should not define pragmas that can make an illegal program legal, except as follows: 

A [pragma](S0016) used to complete a declaration, such as a [pragma](S0016) Import;

A [pragma](S0016) used to configure the environment by adding, removing, or replacing [library_item](S0216)s. 

Ramification: For example, it is OK to support Interface, System_Name, Storage_Unit, and Memory_Size [pragma](S0016)s for upward compatibility reasons, even though all of these [pragma](S0016)s can make an illegal program legal. (The latter three can affect legality in a rather subtle way: They affect the value of named numbers in System, and can therefore affect the legality in cases where static expressions are required.)

On the other hand, adding implementation-defined pragmas to a legal program can make it illegal. For example, a common kind of implementation-defined pragma is one that promises some property that allows more efficient code to be generated. If the promise is a lie, it is best if the user gets an error message. 


#### Incompatibilities With Ada 83

In Ada 83, "bad" [pragma](S0016)s are ignored. In Ada 95, they are illegal, except in the case where the name of the [pragma](S0016) itself is not recognized by the implementation. 


#### Extensions to Ada 83

Implementation-defined [pragma](S0016)s may affect the legality of a program. 


#### Wording Changes from Ada 83

Implementation-defined [pragma](S0016)s may affect the run-time semantics of the program. This was always true in Ada 83 (since it was not explicitly forbidden by RM83), but it was not clear, because there was no definition of "executing" or "elaborating" a [pragma](S0016). 


#### Syntax

The forms of List, Page, and Optimize [pragma](S0016)s are as follows:

  pragma List([identifier](S0002));

  pragma Page;

  pragma Optimize([identifier](S0002));

[Other pragmas are defined throughout this Reference Manual, and are summarized in Annex L.] 

Ramification: The language-defined pragmas are supported by every implementation, although "supporting" some of them (for example, Inline) requires nothing more than checking the arguments, since they act only as advice to the implementation. 


#### Static Semantics

A [pragma](S0016) List takes one of the [identifier](S0002)s On or Off as the single argument. This pragma is allowed anywhere a [pragma](S0016) is allowed. It specifies that listing of the compilation is to be continued or suspended until a List [pragma](S0016) with the opposite argument is given within the same compilation. The [pragma](S0016) itself is always listed if the compiler is producing a listing.

A [pragma](S0016) Page is allowed anywhere a [pragma](S0016) is allowed. It specifies that the program text which follows the [pragma](S0016) should start on a new page (if the compiler is currently producing a listing).

A [pragma](S0016) Optimize takes one of the [identifier](S0002)s Time, Space, or Off as the single argument. This [pragma](S0016) is allowed anywhere a [pragma](S0016) is allowed, and it applies until the end of the immediately enclosing declarative region, or for a [pragma](S0016) at the place of a [compilation_unit](S0215), to the end of the [compilation](S0214). It gives advice to the implementation as to whether time or space is the primary optimization criterion, or that optional optimizations should be turned off. [It is implementation defined how this advice is followed.] 

Implementation defined: Effect of pragma Optimize.

Discussion: For example, a compiler might use Time vs. Space to control whether generic instantiations are implemented with a macro-expansion model, versus a shared-generic-body model.

We don't define what constitutes an "optimization" - in fact, it cannot be formally defined in the context of Ada. One compiler might call something an optional optimization, whereas another compiler might consider that same thing to be a normal part of code generation. Thus, the programmer cannot rely on this pragma having any particular portable effect on the generated code. Some compilers might even ignore the pragma altogether. 


#### Examples

Examples of pragmas: 

```ada
pragma List(Off); -- turn off listing generation
pragma Optimize(Off); -- turn off optional optimizations
pragma Inline(Set_Mask); -- generate code for Set_Mask inline
pragma Suppress(Range_Check, On =&gt Index); -- turn off range checking on Index

```


#### Extensions to Ada 83

The Optimize [pragma](S0016) now allows the identifier Off to request that normal optimization be turned off.

An Optimize [pragma](S0016) may appear anywhere pragmas are allowed. 


#### Wording Changes from Ada 83

We now describe the pragmas Page, List, and Optimize here, to act as examples, and to remove the normative material from Annex L, "Language-Defined Pragmas", so it can be entirely an informative annex. 


## 2.9  Reserved Words


#### Syntax

 

The following are the reserved words (ignoring upper/lower case distinctions): 

Discussion: Reserved words have special meaning in the syntax. In addition, certain reserved words are used as attribute names.

The syntactic category [identifier](S0002) no longer allows reserved words. We have added the few reserved words that are legal explicitly to the syntax for [attribute_reference](S0093). Allowing identifier to include reserved words has been a source of confusion for some users, and differs from the way they are treated in the C and Pascal language definitions.

abort
abs
abstract
accept
access
aliased
all
and
array
at

begin
body

case
constant

declare
delay
delta
digits
do

else
elsif
end
entry
exception
exit

for
function

generic
goto

if
in

is

limited
loop

mod

new
not
null

of
or
others
out


package

pragma
private
procedure
protected

raise
range
record
rem
renames
requeue

return
reverse

select
separate

subtype


tagged
task
terminate
then
type

until
use

when
while
with

xor

NOTE 1   The reserved words appear in lower case boldface in this document, except when used in the [designator](S0144) of an attribute (see 4.1.4). Lower case boldface is also used for a reserved word in a [string_literal](S0013) used as an [operator_symbol](S0147). This is merely a convention - programs may be written in whatever typeface is desired and available. 


#### Incompatibilities With Ada 83

The following words are not reserved in Ada 83, but are reserved in Ada 95: abstract, aliased, protected, requeue, tagged, until. 


#### Wording Changes from Ada 83

The clause entitled "Allowed Replacements of Characters" has been moved to Annex J, "Obsolescent Features". 

