---
sidebar_position:  15
---

# Annex A Predefined Language Environment

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[ This Annex contains the specifications of library units that shall be provided by every implementation. There are three root library units: Ada, Interfaces, and System; other library units are children of these:]

 
 

[Standard - A.1
	Ada - A.2
		Asynchronous_Task_Control - D.11
		Calendar - 9.6
		Characters - A.3.1
			Handling - A.3.2
			Latin_1 - A.3.3
		Command_Line - A.15
		Decimal - F.2
		Direct_IO - A.8.4
		Dynamic_Priorities - 
		Exceptions - 11.4.1
		Finalization - 7.6
		Interrupts - C.3.2
			Names - C.3.2
		IO_Exceptions - A.13
		Numerics - A.5
			Complex_Elementary_Functions - G.1.2
			Complex_Types - G.1.1
			Discrete_Random - A.5.2
			Elementary_Functions - A.5.1
			Float_Random - A.5.2
			Generic_Complex_Elementary_Functions
						- G.1.2
			Generic_Complex_Types - G.1.1
			Generic_Elementary_Functions - A.5.1
		Real_Time - D.8
		Sequential_IO - A.8.1
		Storage_IO - A.9
		Streams - 13.13.1
			Stream_IO - A.12.1


Standard (...continued)
	Ada (...continued)
		Strings - A.4.1
			Bounded - A.4.4
			Fixed - A.4.3
			Maps - A.4.2
				Constants - A.4.6
			Unbounded - A.4.5
			Wide_Bounded - A.4.7
			Wide_Fixed - A.4.7
			Wide_Maps - A.4.7
				Wide_Constants - A.4.7
			Wide_Unbounded - A.4.7
		Synchronous_Task_Control - D.10
		Tags - 3.9
		Task_Attributes - C.7.2
		Task_Identification - C.7.1

		Text_IO - A.10.1
			Complex_IO - G.1.3
			Editing - F.3.3
			Text_Streams - A.12.2
		Unchecked_Conversion - 13.9
		Unchecked_Deallocation - 13.11.2
		Wide_Text_IO - A.11
			Complex_IO - G.1.4
			Editing - F.3.4
			Text_Streams - A.12.3


	Interfaces - B.2
		C - B.3
			Pointers - B.3.2
			Strings - B.3.1
		COBOL - B.4
		Fortran - B.5

	System - 13.7
		Address_To_Access_Conversions - 13.7.2
		Machine_Code - 13.8
		RPC - E.5
		Storage_Elements - 13.7.1
		Storage_Pools - 13.11]

Discussion: In running text, we generally leave out the "Ada." when referring to a child of Ada. 

Reason: We had no strict rule for which of Ada, Interfaces, or System should be the parent of a given library unit. However, we have tried to place as many things as possible under Ada, except that interfacing is a separate category, and we have tried to place library units whose use is highly nonportable under System. 


#### Implementation Requirements

The implementation shall ensure that each languagedefined subprogram is reentrant in the sense that concurrent calls on the same subprogram perform as specified, so long as all parameters that could be passed by reference denote nonoverlapping objects. 

Ramification: For example, simultaneous calls to Text_IO.Put will work properly, so long as they are going to two different files. On the other hand, simultaneous output to the same file constitutes erroneous use of shared variables. 

To be honest: Here, "language defined subprogram" means a language defined library subprogram, a subprogram declared in the visible part of a language defined library package, an instance of a language defined generic library subprogram, or a subprogram declared in the visible part of an instance of a language defined generic library package. 

Ramification: The rule implies that any data local to the private part or body of the package has to be somehow protected against simultaneous access. 


#### Implementation Permissions

The implementation may restrict the replacement of language-defined compilation units. The implementation may restrict children of language-defined library units (other than Standard). 

Ramification: For example, the implementation may say, "you cannot compile a library unit called System" or "you cannot compile a child of package System" or "if you compile a library unit called System, it has to be a package, and it has to contain at least the following declarations: ...". 


#### Wording Changes from Ada 83

Many of Ada 83's language-defined library units are now children of Ada or System. For upward compatibility, these are renamed as root library units (see J.1).

The order and lettering of the annexes has been changed. 


## A.1  The Package Standard

This clause outlines the specification of the package Standard containing all predefined identifiers in the language. The corresponding package body is not specified by the language.

The operators that are predefined for the types declared in the package Standard are given in comments since they are implicitly declared. Italics are used for pseudo-names of anonymous types (such as root_real) and for undefined information (such as implementation-defined). 

Ramification: All of the predefined operators are of convention Intrinsic. 


#### Static Semantics

The library package Standard has the following declaration: 

Implementation defined: The names and characteristics of the numeric subtypes declared in the visible part of package Standard.

```ada
package Standard is
   pragma Pure(Standard);

```

```ada
   type Boolean is (False, True);

```

```ada
   -- The predefined relational operators for this type are as follows:

```

```ada
   -- function "="   (Left, Right : Boolean) return Boolean;
   -- function "/="  (Left, Right : Boolean) return Boolean;
   -- function "&lt"   (Left, Right : Boolean) return Boolean;
   -- function "&lt="  (Left, Right : Boolean) return Boolean;
   -- function "&gt"   (Left, Right : Boolean) return Boolean;
   -- function "&gt="  (Left, Right : Boolean) return Boolean;

```

```ada
   -- The predefined logical operators and the predefined logical
   -- negation operator are as follows:

```

```ada
   -- function "and" (Left, Right : Boolean) return Boolean;
   -- function "or"  (Left, Right : Boolean) return Boolean;
   -- function "xor" (Left, Right : Boolean) return Boolean;

```

```ada
   -- function "not" (Right : Boolean) return Boolean;

```

```ada
   -- The integer type root_integer is predefined.
   -- The corresponding universal type is universal_integer.

```

```ada
   type Integer is range implementation-defined;

```

```ada
   subtype Natural  is Integer range 0 .. Integer'Last;
   subtype Positive is Integer range 1 .. Integer'Last;

```

```ada
   -- The predefined operators for type Integer are as follows:

```

```ada
   -- function "="  (Left, Right : Integer'Base) return Boolean;
   -- function "/=" (Left, Right : Integer'Base) return Boolean;
   -- function "&lt"  (Left, Right : Integer'Base) return Boolean;
   -- function "&lt=" (Left, Right : Integer'Base) return Boolean;
   -- function "&gt"  (Left, Right : Integer'Base) return Boolean;
   -- function "&gt=" (Left, Right : Integer'Base) return Boolean;

```

```ada
   -- function "+"   (Right : Integer'Base) return Integer'Base;
   -- function "-"   (Right : Integer'Base) return Integer'Base;
   -- function "abs" (Right : Integer'Base) return Integer'Base;

```

```ada
   -- function "+"   (Left, Right : Integer'Base) return Integer'Base;
   -- function "-"   (Left, Right : Integer'Base) return Integer'Base;
   -- function "*"   (Left, Right : Integer'Base) return Integer'Base;
   -- function "/"   (Left, Right : Integer'Base) return Integer'Base;
   -- function "rem" (Left, Right : Integer'Base) return Integer'Base;
   -- function "mod" (Left, Right : Integer'Base) return Integer'Base;

```

```ada
   -- function "**"  (Left : Integer'Base; Right : Natural)
   --                  return Integer'Base;

```

```ada
   -- The specification of each operator for the type
   -- root_integer, or for any additional predefined integer
   -- type, is obtained by replacing Integer by the name of the type
   -- in the specification of the corresponding operator of the type
   -- Integer. The right operand of the exponentiation operator
   -- remains as subtype Natural.

```

```ada
   -- The floating point type root_real is predefined.
   -- The corresponding universal type is universal_real.

```

```ada
   type Float is digits implementation-defined;

```

```ada
   -- The predefined operators for this type are as follows:

```

```ada
   -- function "="   (Left, Right : Float) return Boolean;
   -- function "/="  (Left, Right : Float) return Boolean;
   -- function "&lt"   (Left, Right : Float) return Boolean;
   -- function "&lt="  (Left, Right : Float) return Boolean;
   -- function "&gt"   (Left, Right : Float) return Boolean;
   -- function "&gt="  (Left, Right : Float) return Boolean;

```

```ada
   -- function "+"   (Right : Float) return Float;
   -- function "-"   (Right : Float) return Float;
   -- function "abs" (Right : Float) return Float;

```

```ada
   -- function "+"   (Left, Right : Float) return Float;
   -- function "-"   (Left, Right : Float) return Float;
   -- function "*"   (Left, Right : Float) return Float;
   -- function "/"   (Left, Right : Float) return Float;

```

```ada
   -- function "**"  (Left : Float; Right : Integer'Base) return Float;

```

```ada
   -- The specification of each operator for the type root_real, or for
   -- any additional predefined floating point type, is obtained by
   -- replacing Float by the name of the type in the specification of the
   -- corresponding operator of the type Float.

```

```ada
   -- In addition, the following operators are predefined for the root
   -- numeric types:

```

```ada
   function "*" (Left : root_integer; Right : root_real)
     return root_real;

```

```ada
   function "*" (Left : root_real;    Right : root_integer)
     return root_real;

```

```ada
   function "/" (Left : root_real;    Right : root_integer)
     return root_real;

```

```ada
   -- The type universal_fixed is predefined.
   -- The only multiplying operators defined between
   -- fixed point types are

```

```ada
   function "*" (Left : universal_fixed; Right : universal_fixed)
     return universal_fixed;

```

```ada
   function "/" (Left : universal_fixed; Right : universal_fixed)
     return universal_fixed;

```

```ada
      -- The declaration of type Character is based on the standard ISO 8859-1 character set.

      -- There are no character literals corresponding to the positions for control characters.
      -- They are indicated in italics in this definition. See 3.5.2.

   type Character is
     (nul,	soh,	stx,	etx,	eot,	enq,	ack,	bel,	--0 (16#00#) .. 7 (16#07#)
      bs,	ht,	lf,	vt,	ff,	cr,	so,	si,	--8 (16#08#) .. 15 (16#0F#)

      dle,	dc1,	dc2,	dc3,	dc4,	nak,	syn,	etb,	--16 (16#10#) .. 23 (16#17#)
      can,	em,	sub,	esc,	fs,	gs,	rs,	us,	--24 (16#18#) .. 31 (16#1F#)

      ' ',	'!',	'"',	'#',	'$',	'%',	'&',	''',	--32 (16#20#) .. 39 (16#27#)
      '(',	')',	'*',	'+',	',',	'-',	'.',	'/',	--40 (16#28#) .. 47 (16#2F#)

      '0',	'1',	'2',	'3',	'4',	'5',	'6',	'7',	--48 (16#30#) .. 55 (16#37#)
      '8',	'9',	':',	';',	'&lt',	'=',	'&gt',	'?',	--56 (16#38#) .. 63 (16#3F#)

      '@',	'A',	'B',	'C',	'D',	'E',	'F',	'G',	--64 (16#40#) .. 71 (16#47#)
      'H',	'I',	'J',	'K',	'L',	'M',	'N',	'O',	--72 (16#48#) .. 79 (16#4F#)

      'P',	'Q',	'R',	'S',	'T',	'U',	'V',	'W',	--80 (16#50#) .. 87 (16#57#)
      'X',	'Y',	'Z',	'[',	'\',	']',	'^',	'_',	--88 (16#58#) .. 95 (16#5F#)

      '`',	'a',	'b',	'c',	'd',	'e',	'f',	'g',	--96 (16#60#) .. 103 (16#67#)
      'h',	'i',	'j',	'k',	'l',	'm',	'n',	'o',	--104 (16#68#) .. 111 (16#6F#)

      'p',	'q',	'r',	's',	't',	'u',	'v',	'w',	--112 (16#70#) .. 119 (16#77#)
      'x',	'y',	'z',	'{',	'|',	'}',	'~',	del,	--120 (16#78#) .. 127 (16#7F#)

      reserved_128,	reserved_129,	bph,	nbh,			--128 (16#80#) .. 131 (16#83#)
      reserved_132,	nel,	ssa,	esa,				--132 (16#84#) .. 135 (16#87#)
      hts,	htj,	vts,	pld,	plu,	ri,	ss2,	ss3,	--136 (16#88#) .. 143 (16#8F#)

      dcs,	pu1,	pu2,	sts,	cch,	mw,	spa,	epa,	--144 (16#90#) .. 151 (16#97#)
      sos,	reserved_153,	sci,	csi,				--152 (16#98#) .. 155 (16#9B#)
      st,	osc,	pm,	apc,					--156 (16#9C#) .. 159 (16#9F#)

      ' ',	'¡',	'¢',	'£',	'¤',	'¥',	'¦',	'§',	--160 (16#A0#) .. 167 (16#A7#)
      '¨',	'©',	'ª',	'«',	'¬',	'­',	'®',	'¯',	--168 (16#A8#) .. 175 (16#AF#)

      '°',	'±',	'²',	'³',	'´',	'µ',	'¶',	'·',	--176 (16#B0#) .. 183 (16#B7#)
      '¸',	'¹',	'º',	'»',	'¼',	'½',	'¾',	'¿',	--184 (16#B8#) .. 191 (16#BF#)

      'À',	'Á',	'Â',	'Ã',	'Ä',	'Å',	'Æ',	'Ç',	--192 (16#C0#) .. 199 (16#C7#)
      'È',	'É',	'Ê',	'Ë',	'Ì',	'Í',	'Î',	'Ï',	--200 (16#C8#) .. 207 (16#CF#)

      'Ð',	'Ñ',	'Ò',	'Ó',	'Ô',	'Õ',	'Ö',	'×',	--208 (16#D0#) .. 215 (16#D7#)
      'Ø',	'Ù',	'Ú',	'Û',	'Ü',	'Ý',	'Þ',	'ß',	--216 (16#D8#) .. 223 (16#DF#)

      'à',	'á',	'â',	'ã',	'ä',	'å',	'æ',	'ç',	--224 (16#E0#) .. 231 (16#E7#)
      'è',	'é',	'ê',	'ë',	'ì',	'í',	'î',	'ï',	--232 (16#E8#) .. 239 (16#EF#)

      'ð',	'ñ',	'ò',	'ó',	'ô',	'õ',	'ö',	'÷',	--240 (16#F0#) .. 247 (16#F7#)
      'ø',	'ù',	'ú',	'û',	'ü',	'ý',	'þ',	'ÿ',	--248 (16#F8#) .. 255 (16#FF#)

```

```ada
   -- The predefined operators for the type Character are the same as for
   -- any enumeration type.


```

```ada
   -- The declaration of type Wide_Character is based on the standard ISO 10646 BMP character set.
   -- The first 256 positions have the same contents as type Character. See 3.5.2.

   type Wide_Character is (nul, soh ... FFFE, FFFF);

```

```ada
   package ASCII is ... end ASCII;  --Obsolescent; see J.5



```

```ada
   -- Predefined string types:

   type String is array(Positive range &lt&gt) of Character;
   pragma Pack(String);

```

```ada
   -- The predefined operators for this type are as follows:

```

```ada
   --     function "="  (Left, Right: String) return Boolean;
   --     function "/=" (Left, Right: String) return Boolean;
   --     function "&lt"  (Left, Right: String) return Boolean;
   --     function "&lt=" (Left, Right: String) return Boolean;
   --     function "&gt"  (Left, Right: String) return Boolean;
   --     function "&gt=" (Left, Right: String) return Boolean;

```

```ada
   --     function "&" (Left: String;    Right: String)    return String;
   --     function "&" (Left: Character; Right: String)    return String;
   --     function "&" (Left: String;    Right: Character) return String;
   --     function "&" (Left: Character; Right: Character) return String;

```

```ada
   type Wide_String is array(Positive range &lt&gt) of Wide_Character;
   pragma Pack(Wide_String);

```

```ada
   -- The predefined operators for this type correspond to those for String.

```

```ada
   type Duration is delta implementation-defined range implementation-defined;

```

```ada
      -- The predefined operators for the type Duration are the same as for
      -- any fixed point type.

```

```ada
   -- The predefined exceptions:

```

```ada
   Constraint_Error: exception;
   Program_Error   : exception;
   Storage_Error   : exception;
   Tasking_Error   : exception;

```

```ada
end Standard;

```

Standard has no private part. 

Reason: This is important for portability. All library packages are children of Standard, and if Standard had a private part then it would be visible to all of them. 

In each of the types Character and Wide_Character, the character literals for the space character (position 32) and the non-breaking space character (position 160) correspond to different values. Unless indicated otherwise, each occurrence of the character literal ' ' in this Reference Manual refers to the space character. Similarly, the character literals for hyphen (position 45) and soft hyphen (position 173) correspond to different values. Unless indicated otherwise, each occurrence of the character literal '' in this Reference Manual refers to the hyphen character. 


#### Dynamic Semantics

Elaboration of the body of Standard has no effect. 

Discussion: Note that the language does not define where this body appears in the environment [declarative_part](S0079) - see 10, "Program Structure and Compilation Issues". 


#### Implementation Permissions

An implementation may provide additional predefined integer types and additional predefined floating point types. Not all of these types need have names. 

To be honest: An implementation may add representation items to package Standard, for example to specify the internal codes of type Boolean, or the Small of type Duration.


#### Implementation Advice

If an implementation provides additional named predefined integer types, then the names should end with "Integer" as in "Long_Integer". If an implementation provides additional named predefined floating point types, then the names should end with "Float" as in "Long_Float". 

NOTE 1   Certain aspects of the predefined entities cannot be completely described in the language itself. For example, although the enumeration type Boolean can be written showing the two enumeration literals False and True, the short-circuit control forms cannot be expressed in the language.

NOTE 2   As explained in 8.1, "Declarative Region" and 10.1.4, "The Compilation Process", the declarative region of the package Standard encloses every library unit and consequently the main subprogram; the declaration of every library unit is assumed to occur within this declarative region. [Library_item](S0216)s are assumed to be ordered in such a way that there are no forward semantic dependences. However, as explained in 8.3, "Visibility", the only library units that are visible within a given compilation unit are the library units named by all [with_clause](S0223)s that apply to the given unit, and moreover, within the declarative region of a given library unit, that library unit itself.

NOTE 3   If all [block_statement](S0138)s of a program are named, then the name of each program unit can always be written as an expanded name starting with Standard (unless Standard is itself hidden). The name of a library unit cannot be a homograph of a name (such as Integer) that is already declared in Standard.

NOTE 4   The exception Standard.Numeric_Error is defined in J.6. 

Discussion: The declaration of Natural needs to appear between the declaration of Integer and the (implicit) declaration of the "**" operator for Integer, because a formal parameter of "**" is of subtype Natural. This would be impossible in normal code, because the implicit declarations for a type occur immediately after the type declaration, with no possibility of intervening explicit declarations. But we're in Standard, and Standard is somewhat magic anyway.

Using Natural as the subtype of the formal of "**" seems natural; it would be silly to have a textual rule about Constraint_Error being raised when there is a perfectly good subtype that means just that. Furthermore, by not using Integer for that formal, it helps remind the reader that the exponent remains Natural even when the left operand is replaced with the derivative of Integer. It doesn't logically imply that, but it's still useful as a reminder.

In any case, declaring these general-purpose subtypes of Integer close to Integer seems more readable than declaring them much later. 


#### Extensions to Ada 83

Package Standard is declared to be pure. 

Discussion: The introduction of the types Wide_Character and Wide_String is not an Ada 95 extension to Ada 83, since ISO WG9 has approved these as an authorized extension of the original Ada 83 standard that is part of that standard. 


#### Wording Changes from Ada 83

Numeric_Error is made obsolescent.

The declarations of Natural and Positive are moved to just after the declaration of Integer, so that "**" can refer to Natural without a forward reference. There's no real need to move Positive, too - it just came along for the ride. 


## A.2  The Package Ada


#### Static Semantics

The following language-defined library package exists: 

```ada
package Ada is
    pragma Pure(Ada);
end Ada;

```

Ada serves as the parent of most of the other language-defined library units; its declaration is empty (except for the [pragma](S0016) Pure). 


#### Legality Rules

In the standard mode, it is illegal to compile a child of package Ada. 

Reason: The intention is that mentioning, say, Ada.Text_IO in a [with_clause](S0223) is guaranteed (at least in the standard mode) to refer to the standard version of Ada.Text_IO. The user can compile a root library unit Text_IO that has no relation to the standard version of Text_IO. 

Ramification: Note that Ada can have non-language-defined grandchildren, assuming the implementation allows it. Also, packages System and Interfaces can have children, assuming the implementation allows it. 

Implementation Note: An implementation will typically support a nonstandard mode in which compiling the language defined library units is allowed. Whether or not this mode is made available to users is up to the implementer.

An implementation could theoretically have private children of Ada, since that would be semantically neutral. However, a programmer cannot compile such a library unit. 


#### Extensions to Ada 83

This clause is new to Ada 95. 


## A.3  Character Handling

This clause presents the packages related to character processing: an empty pure package Characters and child packages Characters.Handling and Characters.Latin_1. The package Characters.Handling provides classification and conversion functions for Character data, and some simple functions for dealing with Wide_Character data. The child package Characters.Latin_1 declares a set of constants initialized to values of type Character. 


#### Extensions to Ada 83

This clause is new to Ada 95. 


### A.3.1  The Package Characters


#### Static Semantics

The library package Characters has the following declaration: 

```ada
package Ada.Characters is
  pragma Pure(Characters);
end Ada.Characters;

```


### A.3.2  The Package Characters.Handling


#### Static Semantics

The library package Characters.Handling has the following declaration: 

```ada
package Ada.Characters.Handling is
  pragma Preelaborate(Handling);

```

```ada
--Character classification functions

```

```ada
  function Is_Control           (Item : in Character) return Boolean;
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
--Classifications of and conversions between Wide_Character and Character.

```

```ada
  function Is_Character (Item : in Wide_Character) return Boolean;
  function Is_String    (Item : in Wide_String)    return Boolean;

```

```ada
  function To_Character (Item       : in Wide_Character;
                         Substitute : in Character := ' ')
    return Character;

```

```ada
  function To_String    (Item       : in Wide_String;
                         Substitute : in Character := ' ')
    return String;

```

```ada
  function To_Wide_Character (Item : in Character) return Wide_Character;

```

```ada
  function To_Wide_String    (Item : in String)    return Wide_String;

```

```ada
end Ada.Characters.Handling;

```

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

Each of the names To_Lower, To_Upper, and To_Basic refers to two functions: one that converts from Character to Character, and the other that converts from String to String. The result of each Character-to-Character function is described below, in terms of the conversion applied to Item, its formal Character parameter. The result of each  String-to-String conversion is obtained by applying to each element of the function's String parameter the corresponding Character-to-Character conversion; the result is the null String if the value of the formal parameter is the null String. The lower bound of the result String is 1. 

To_LowerReturns the corresponding lower-case value for Item if Is_Upper(Item), and returns Item otherwise.

To_UpperReturns the corresponding upper-case value for Item if Is_Lower(Item) and Item has an upper-case form, and returns Item otherwise. The lower case letters 'ß' and 'ÿ' do not have upper case forms.

To_BasicReturns the letter corresponding to Item but with no diacritical mark, if Item is a letter but not a basic letter; returns Item otherwise. 

The following set of functions test for membership in the ISO 646 character range, or convert between ISO 646 and Character. 

Is_ISO_646The function whose formal parameter, Item, is of type Character returns True if Item is in the subtype ISO_646.

Is_ISO_646The function whose formal parameter, Item, is of type String returns True if Is_ISO_646(Item(I)) is True for each I in Item'Range.

To_ISO_646The function whose first formal parameter, Item, is of type Character returns Item if Is_ISO_646(Item), and returns the Substitute ISO_646 character otherwise.

To_ISO_646The function whose first formal parameter, Item, is of type String returns the String whose Range is 1..Item'Length and each of whose elements is given by To_ISO_646 of the corresponding element in Item. 

The following set of functions test Wide_Character values for membership in Character, or convert between corresponding characters of Wide_Character and Character. 

Is_CharacterReturns True if Wide_Character'Pos(Item) &lt= Character'Pos(Character'Last).

Is_StringReturns True if Is_Character(Item(I)) is True for each I in Item'Range.

To_CharacterReturns the Character corresponding to Item if Is_Character(Item), and returns the Substitute Character otherwise.

To_StringReturns the String whose range is 1..Item'Length and each of whose elements is given by To_Character of the corresponding element in Item.

To_Wide_CharacterReturns the Wide_Character X such that Character'Pos(Item) = Wide_Character'Pos(X).

To_Wide_StringReturns the Wide_String whose range is 1..Item'Length and each of whose elements is given by To_Wide_Character of the corresponding element in Item.


#### Implementation Advice

If an implementation provides a localized definition of Character or Wide_Character, then the effects of the subprograms in Characters.Handling should reflect the localizations. See also 3.5.2. 

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


### A.3.3  The Package Characters.Latin_1

The package Characters.Latin_1 declares constants for characters in ISO 8859-1. 

Reason: The constants for the ISO 646 characters could have been declared as renamings of objects declared in package ASCII, as opposed to explicit constants. The main reason for explicit constants was for consistency of style with the upper-half constants, and to avoid emphasizing the package ASCII.


#### Static Semantics

The library package Characters.Latin_1 has the following declaration: 

```ada
package Ada.Characters.Latin_1 is
    pragma Pure(Latin_1);

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
-- Character positions 160 (16#A0#) .. 175 (16#AF#):
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
    Soft_Hyphen                : constant Character := '­'; --Character'Val(173)
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


## A.4  String Handling

This clause presents the specifications of the package Strings and several child packages, which provide facilities for dealing with string data. Fixed-length, bounded-length, and unbounded-length strings are supported, for both String and Wide_String. The string-handling subprograms include searches for pattern strings and for characters in program-specified sets, translation (via a character-to-character mapping), and transformation (replacing, inserting, overwriting, and deleting of substrings). 


#### Extensions to Ada 83

This clause is new to Ada 95. 


### A.4.1  The Package Strings

The package Strings provides declarations common to the string handling packages. 


#### Static Semantics

The library package Strings has the following declaration: 

```ada
package Ada.Strings is
   pragma Pure(Strings);

```

```ada
   Space      : constant Character      := ' ';
   Wide_Space : constant Wide_Character := ' ';

```

```ada
   Length_Error, Pattern_Error, Index_Error, Translation_Error : exception;

```

```ada
   type Alignment  is (Left, Right, Center);
   type Truncation is (Left, Right, Error);
   type Membership is (Inside, Outside);
   type Direction  is (Forward, Backward);
   type Trim_End   is (Left, Right, Both);
end Ada.Strings;

```


### A.4.2  The Package Strings.Maps

The package Strings.Maps defines the types, operations, and other entities needed for character sets and character-to-character mappings. 


#### Static Semantics

The library package Strings.Maps has the following declaration: 

```ada
package Ada.Strings.Maps is
   pragma Preelaborate(Maps);

```

```ada
   -- Representation for a set of character values:
   type Character_Set is private;

```

```ada
   Null_Set : constant Character_Set;

```

```ada
   type Character_Range is
     record
        Low  : Character;
        High : Character;
     end record;
   -- Represents Character range Low..High

```

```ada
   type Character_Ranges is array (Positive range &lt&gt) of Character_Range;

```

```ada
   function To_Set    (Ranges : in Character_Ranges)return Character_Set;

```

```ada
   function To_Set    (Span   : in Character_Range)return Character_Set;

```

```ada
   function To_Ranges (Set    : in Character_Set)  return Character_Ranges;

```

```ada
   function "="   (Left, Right : in Character_Set) return Boolean;

```

```ada
   function "not" (Right : in Character_Set)       return Character_Set;
   function "and" (Left, Right : in Character_Set) return Character_Set;
   function "or"  (Left, Right : in Character_Set) return Character_Set;
   function "xor" (Left, Right : in Character_Set) return Character_Set;
   function "-"   (Left, Right : in Character_Set) return Character_Set;

```

```ada
   function Is_In (Element : in Character;
                   Set     : in Character_Set)
      return Boolean;

```

```ada
   function Is_Subset (Elements : in Character_Set;
                       Set      : in Character_Set)
      return Boolean;

```

```ada
   function "&lt=" (Left  : in Character_Set;
                  Right : in Character_Set)
      return Boolean renames Is_Subset;

```

```ada
   -- Alternative representation for a set of character values:
   subtype Character_Sequence is String;

```

```ada
   function To_Set (Sequence  : in Character_Sequence)return Character_Set;

```

```ada
   function To_Set (Singleton : in Character)     return Character_Set;

```

```ada
   function To_Sequence (Set  : in Character_Set) return Character_Sequence;

```

```ada
   -- Representation for a character to character mapping:
   type Character_Mapping is private;

```

```ada
   function Value (Map     : in Character_Mapping;
                   Element : in Character)
      return Character;

```

```ada
   Identity : constant Character_Mapping;

```

```ada
   function To_Mapping (From, To : in Character_Sequence)
      return Character_Mapping;

```

```ada
   function To_Domain (Map : in Character_Mapping)
      return Character_Sequence;
   function To_Range  (Map : in Character_Mapping)
      return Character_Sequence;

```

```ada
   type Character_Mapping_Function is
      access function (From : in Character) return Character;

```

```ada
private
   ... -- not specified by the language
end Ada.Strings.Maps;

```

An object of type Character_Set represents a set of characters.

Null_Set represents the set containing no characters.

An object Obj of type Character_Range represents the set of characters in the range Obj.Low .. Obj.High.

An object Obj of type Character_Ranges represents the union of the sets corresponding to Obj(I) for I in Obj'Range. 

```ada
function To_Set (Ranges : in Character_Ranges) return Character_Set;

```

If Ranges'Length=0 then Null_Set is returned; otherwise the returned value represents the set corresponding to Ranges.

```ada
function To_Set (Span : in Character_Range) return Character_Set;

```

The returned value represents the set containing each character in Span. 

```ada
function To_Ranges (Set : in Character_Set) return Character_Ranges;

```

If Set = Null_Set then an empty Character_Ranges array is returned; otherwise the shortest array of contiguous ranges of Character values in Set, in increasing order of Low, is returned.

```ada
function "=" (Left, Right : in Character_Set) return Boolean;

```

The function "=" returns True if Left and Right represent identical sets, and False otherwise. 

Each of the logical operators "not", "and", "or", and "xor" returns a Character_Set value that represents the set obtained by applying the corresponding operation to the set(s) represented by the parameter(s) of the operator. ""(Left, Right) is equivalent to "and"(Left, "not"(Right)). 

Reason: The set minus operator is provided for efficiency.

```ada
function Is_In (Element : in Character;
                Set     : in Character_Set);
   return Boolean;

```

Is_In returns True if Element is in Set, and False otherwise.

```ada
function Is_Subset (Elements : in Character_Set;
                    Set      : in Character_Set)
   return Boolean;

```

Is_Subset returns True if Elements is a subset of Set, and False otherwise.

```ada
subtype Character_Sequence is String;

```

The Character_Sequence subtype is used to portray a set of character values and also to identify the domain and range of a character mapping. 

Reason: Although a named subtype is redundant - the predefined type String could have been used for the parameter to To_Set and To_Mapping below - the use of a differently named subtype identifies the intended purpose of the parameter. 

```ada
function To_Set (Sequence  : in Character_Sequence) return Character_Set;

function To_Set (Singleton : in Character)          return Character_Set;

```

Sequence portrays the set of character values that it explicitly contains (ignoring duplicates). Singleton portrays the set comprising a single Character. Each of the To_Set functions returns a Character_Set value that represents the set portrayed by Sequence or Singleton.

```ada
function To_Sequence (Set : in Character_Set) return Character_Sequence;

```

The function To_Sequence returns a Character_Sequence value containing each of the characters in the set represented by Set, in ascending order with no duplicates.

```ada
type Character_Mapping is private;

```

An object of type Character_Mapping represents a Character-to-Character mapping.

```ada
function Value (Map     : in Character_Mapping;
                Element : in Character)
   return Character;

```

The function Value returns the Character value to which Element maps with respect to the mapping represented by Map. 

A character C matches a pattern character P with respect to a given Character_Mapping value Map if Value(Map, C) = P. A string S matches a pattern string P with respect to a given Character_Mapping if their lengths are the same and if each character in S matches its corresponding character in the pattern string P. 

Discussion: In an earlier version of the string handling packages, the definition of matching was symmetrical, namely C matches P if Value(Map,C) = Value(Map,P). However, applying the mapping to the pattern was confusing according to some reviewers. Furthermore, if the symmetrical version is needed, it can be achieved by applying the mapping to the pattern (via translation) prior to passing it as a parameter. 

String handling subprograms that deal with character mappings have parameters whose type is Character_Mapping. 

```ada
Identity : constant Character_Mapping;

```

Identity maps each Character to itself.

```ada
function To_Mapping (From, To : in Character_Sequence)
    return Character_Mapping;

```

To_Mapping produces a Character_Mapping such that each element of From maps to the corresponding element of To, and each other character maps to itself. If From'Length /= To'Length, or if some character is repeated in From, then Translation_Error is propagated.

```ada
function To_Domain (Map : in Character_Mapping) return Character_Sequence;

```

To_Domain returns the shortest Character_Sequence value D such that each character not in D maps to itself, and such that the characters in D are in ascending order. The lower bound of D is 1.

```ada
function To_Range  (Map : in Character_Mapping) return Character_Sequence;

```

To_Range returns the Character_Sequence value R, with lower bound 1 and upper bound Map'Length, such that if D = To_Domain(Map) then D(I) maps to R(I) for each I in D'Range. 

An object F of type Character_Mapping_Function maps a Character value C to the Character value F.all(C), which is said to match C with respect to mapping function F. 

NOTE 1   Character_Mapping and Character_Mapping_Function are used both for character equivalence mappings in the search subprograms (such as for case insensitivity) and as transformational mappings in the Translate subprograms.

NOTE 2   To_Domain(Identity) and To_Range(Identity) each returns the null string. 

Reason: Package Strings.Maps is not pure, since it declares an access-to-subprogram type. 


#### Examples

To_Mapping("ABCD", "ZZAB") returns a Character_Mapping that maps 'A' and 'B' to 'Z', 'C' to 'A', 'D' to 'B', and each other Character to itself. 


### A.4.3  Fixed-Length String Handling

The language-defined package Strings.Fixed provides string-handling subprograms for fixed-length strings; that is, for values of type Standard.String. Several of these subprograms are procedures that modify the contents of a String that is passed as an out or an in out parameter; each has additional parameters to control the effect when the logical length of the result differs from the parameter's length.

For each function that returns a String, the lower bound of the returned value is 1. 

Discussion: Most operations that yields a String are provided both as a function and as a procedure. The functional form is possibly a more aesthetic style but may introduce overhead due to extra copying or dynamic memory usage in some implementations. Thus a procedural form, with an in out parameter so that all copying is done `in place', is also supplied.

The basic model embodied in the package is that a fixed-length string comprises significant characters and possibly padding (with space characters) on either or both ends. When a shorter string is copied to a longer string, padding is inserted, and when a longer string is copied to a shorter one, padding is stripped. The Move procedure in Strings.Fixed, which takes a String as an out parameter, allows the programmer to control these effects. Similar control is provided by the string transformation procedures. 


#### Static Semantics

The library package Strings.Fixed has the following declaration: 

```ada
with Ada.Strings.Maps;
package Ada.Strings.Fixed is
   pragma Preelaborate(Fixed);

```

```ada
-- "Copy" procedure for strings of possibly different lengths

```

```ada
   procedure Move (Source  : in  String;
                   Target  : out String;
                   Drop    : in  Truncation := Error;
                   Justify : in  Alignment  := Left;
                   Pad     : in  Character  := Space);

```

```ada
-- Search subprograms

```

```ada
   function Index (Source   : in String;
                   Pattern  : in String;
                   Going    : in Direction := Forward;
                   Mapping  : in Maps.Character_Mapping
                                := Maps.Identity)
      return Natural;

```

```ada
   function Index (Source   : in String;
                   Pattern  : in String;
                   Going    : in Direction := Forward;
                   Mapping  : in Maps.Character_Mapping_Function)
      return Natural;

```

```ada
   function Index (Source : in String;
                   Set    : in Maps.Character_Set;
                   Test   : in Membership := Inside;
                   Going  : in Direction  := Forward)
      return Natural;

```

```ada
   function Index_Non_Blank (Source : in String;
                             Going  : in Direction := Forward)
      return Natural;

```

```ada
   function Count (Source   : in String;
                   Pattern  : in String;
                   Mapping  : in Maps.Character_Mapping
                                 := Maps.Identity)
      return Natural;

```

```ada
   function Count (Source   : in String;
                   Pattern  : in String;
                   Mapping  : in Maps.Character_Mapping_Function)
      return Natural;

```

```ada
   function Count (Source   : in String;
                   Set      : in Maps.Character_Set)
      return Natural;

```

```ada
   procedure Find_Token (Source : in String;
                         Set    : in Maps.Character_Set;
                         Test   : in Membership;
                         First  : out Positive;
                         Last   : out Natural);

```

```ada
-- String translation subprograms

```

```ada
   function Translate (Source  : in String;
                       Mapping : in Maps.Character_Mapping)
      return String;

```

```ada
   procedure Translate (Source  : in out String;
                        Mapping : in Maps.Character_Mapping);

```

```ada
   function Translate (Source  : in String;
                       Mapping : in Maps.Character_Mapping_Function)
      return String;

```

```ada
   procedure Translate (Source  : in out String;
                        Mapping : in Maps.Character_Mapping_Function);

```

```ada
-- String transformation subprograms

```

```ada
   function Replace_Slice (Source   : in String;
                           Low      : in Positive;
                           High     : in Natural;
                           By       : in String)
      return String;

```

```ada
   procedure Replace_Slice (Source   : in out String;
                            Low      : in Positive;
                            High     : in Natural;
                            By       : in String;
                            Drop     : in Truncation := Error;
                            Justify  : in Alignment  := Left;
                            Pad      : in Character  := Space);

```

```ada
   function Insert (Source   : in String;
                    Before   : in Positive;
                    New_Item : in String)
      return String;

```

```ada
   procedure Insert (Source   : in out String;
                     Before   : in Positive;
                     New_Item : in String;
                     Drop     : in Truncation := Error);

```

```ada
   function Overwrite (Source   : in String;
                       Position : in Positive;
                       New_Item : in String)
      return String;

```

```ada
   procedure Overwrite (Source   : in out String;
                        Position : in Positive;
                        New_Item : in String;
                        Drop     : in Truncation := Right);

```

```ada
   function Delete (Source  : in String;
                    From    : in Positive;
                    Through : in Natural)
      return String;

```

```ada
   procedure Delete (Source  : in out String;
                     From    : in Positive;
                     Through : in Natural;
                     Justify : in Alignment := Left;
                     Pad     : in Character := Space);

```

```ada
 --String selector subprograms
   function Trim (Source : in String;
                  Side   : in Trim_End)
      return String;

```

```ada
   procedure Trim (Source  : in out String;
                   Side    : in Trim_End;
                   Justify : in Alignment := Left;
                   Pad     : in Character := Space);

```

```ada
   function Trim (Source : in String;
                  Left   : in Maps.Character_Set;
                  Right  : in Maps.Character_Set)
      return String;

```

```ada
   procedure Trim (Source  : in out String;
                   Left    : in Maps.Character_Set;
                   Right   : in Maps.Character_Set;
                   Justify : in Alignment := Strings.Left;
                   Pad     : in Character := Space);

```

```ada
   function Head (Source : in String;
                  Count  : in Natural;
                  Pad    : in Character := Space)
      return String;

```

```ada
   procedure Head (Source  : in out String;
                   Count   : in Natural;
                   Justify : in Alignment := Left;
                   Pad     : in Character := Space);

```

```ada
   function Tail (Source : in String;
                  Count  : in Natural;
                  Pad    : in Character := Space)
      return String;

```

```ada
   procedure Tail (Source  : in out String;
                   Count   : in Natural;
                   Justify : in Alignment := Left;
                   Pad     : in Character := Space);

```

```ada
--String constructor functions

```

```ada
   function "*" (Left  : in Natural;
                 Right : in Character) return String;

```

```ada
   function "*" (Left  : in Natural;
                 Right : in String) return String;

```

```ada
end Ada.Strings.Fixed;

```

The effects of the above subprograms are as follows. 

```ada
procedure Move (Source  : in  String;
                Target  : out String;
                Drop    : in  Truncation := Error;
                Justify : in  Alignment  := Left;
                Pad     : in  Character  := Space);

```

The Move procedure copies characters from Source to Target. If Source has the same length as Target, then the effect is to assign Source to Target. If Source is shorter than Target then: 

If Justify=Left, then Source is copied into the first Source'Length characters of Target.

If Justify=Right, then Source is copied into the last Source'Length characters of Target.

If Justify=Center, then Source is copied into the middle Source'Length characters of Target. In this case, if the difference in length between Target and Source is odd, then the extra Pad character is on the right.

Pad is copied to each Target character not otherwise assigned. 

If Source is longer than Target, then the effect is based on Drop. 

If Drop=Left, then the rightmost Target'Length characters of Source are copied into Target.

If Drop=Right, then the leftmost Target'Length characters of Source are copied into Target.

If Drop=Error, then the effect depends on the value of the Justify parameter and also on whether any characters in Source other than Pad would fail to be copied: 

If Justify=Left, and if each of the rightmost Source'Length-Target'Length characters in Source is Pad, then the leftmost Target'Length characters of Source are copied to Target.

If Justify=Right, and if each of the leftmost Source'Length-Target'Length characters in Source is Pad, then the rightmost Target'Length characters of Source are copied to Target.

Otherwise, Length_Error is propagated. 

Ramification: The Move procedure will work even if Source and Target overlap.

Reason: The order of parameters (Source before Target) corresponds to the order in COBOL's MOVE verb.

```ada
function Index (Source   : in String;
                Pattern  : in String;
                Going    : in Direction := Forward;
                Mapping  : in Maps.Character_Mapping
                              := Maps.Identity)
   return Natural;

function Index (Source   : in String;
                Pattern  : in String;
                Going    : in Direction := Forward;
                Mapping  : in Maps.Character_Mapping_Function)
   return Natural;

```

Each Index function searches for a slice of Source, with length Pattern'Length, that matches Pattern with respect to Mapping; the parameter Going indicates the direction of the lookup. If Going = Forward, then Index returns the smallest index I such that the slice of Source starting at I matches Pattern. If Going = Backward, then Index returns the largest index I such that the slice of Source starting at I matches Pattern. If there is no such slice, then 0 is returned. If Pattern is the null string then Pattern_Error is propagated. 

```ada
function Index (Source : in String;
                Set    : in Maps.Character_Set;
                Test   : in Membership := Inside;
                Going  : in Direction  := Forward)
   return Natural;

```

Index searches for the first or last occurrence of any of a set of characters (when Test=Inside), or any of the complement of a set of characters (when Test=Outside). It returns the smallest index I (if Going=Forward) or the largest index I (if Going=Backward) such that Source(I) satisfies the Test condition with respect to Set; it returns 0 if there is no such Character in Source. 

```ada
function Index_Non_Blank (Source : in String;
                          Going  : in Direction := Forward)
   return Natural;

```

Returns Index(Source, Maps.To_Set(Space), Outside, Going)

```ada
function Count (Source   : in String;
                Pattern  : in String;
                Mapping  : in Maps.Character_Mapping
                             := Maps.Identity)
   return Natural;

function Count (Source   : in String;
                Pattern  : in String;
                Mapping  : in Maps.Character_Mapping_Function)
   return Natural;

```

Returns the maximum number of nonoverlapping slices of Source that match Pattern with respect to Mapping. If Pattern is the null string then Pattern_Error is propagated. 

Reason: We say `maximum number' because it is possible to slice a source string in different ways yielding different numbers of matches. For example if Source is "ABABABA" and Pattern is "ABA", then Count yields 2, although there is a partitioning of Source that yields just 1 match, for the middle slice. Saying `maximum number' is equivalent to saying that the pattern match starts either at the low index or the high index position. 

```ada
function Count (Source   : in String;
                Set      : in Maps.Character_Set)
   return Natural;

```

Returns the number of occurrences in Source of characters that are in Set.

```ada
procedure Find_Token (Source : in String;
                      Set    : in Maps.Character_Set;
                      Test   : in Membership;
                      First  : out Positive;
                      Last   : out Natural);

```

Find_Token returns in First and Last the indices of the beginning and end of the first slice of Source all of whose elements satisfy the Test condition, and such that the elements (if any) immediately before and after the slice do not satisfy the Test condition. If no such slice exists, then the value returned for Last is zero, and the value returned for First is Source'First.

```ada
function Translate (Source  : in String;
                    Mapping : in Maps.Character_Mapping)
   return String;

function Translate (Source  : in String;
                    Mapping : in Maps.Character_Mapping_Function)
   return String;

```

Returns the string S whose length is Source'Length and such that S(I) is the character to which Mapping maps the corresponding element of Source, for I in 1..Source'Length.

```ada
procedure Translate (Source  : in out String;
                     Mapping : in Maps.Character_Mapping);

procedure Translate (Source  : in out String;
                     Mapping : in Maps.Character_Mapping_Function);

```

Equivalent to Source := Translate(Source, Mapping).

```ada
function Replace_Slice (Source   : in String;
                        Low      : in Positive;
                        High     : in Natural;
                        By       : in String)
   return String;

```

If Low &gt Source'Last+1, or High &lt Source'First1, then Index_Error is propagated. Otherwise, if High &gt= Low then the returned string comprises Source(Source'First..Low1) & By & Source(High+1..Source'Last), and if High &lt Low then the returned string is Insert(Source, Before=&gtLow, New_Item=&gtBy).

```ada
procedure Replace_Slice (Source   : in out String;
                         Low      : in Positive;
                         High     : in Natural;
                         By       : in String;
                         Drop     : in Truncation := Error;
                         Justify  : in Alignment  := Left;
                         Pad      : in Character  := Space);

```

Equivalent to Move(Replace_Slice(Source, Low, High, By), Source, Drop, Justify, Pad).

```ada
function Insert (Source   : in String;
                 Before   : in Positive;
                 New_Item : in String)
   return String;

```

Propagates Index_Error if Before is not in Source'First .. Source'Last+1; otherwise returns Source(Source'First..Before1) & New_Item & Source(Before..Source'Last), but with lower bound 1.

```ada
procedure Insert (Source   : in out String;
                  Before   : in Positive;
                  New_Item : in String;
                  Drop     : in Truncation := Error);

```

Equivalent to Move(Insert(Source, Before, New_Item), Source, Drop).

```ada
function Overwrite (Source   : in String;
                    Position : in Positive;
                    New_Item : in String)
   return String;

```

Propagates Index_Error if Position is not in Source'First .. Source'Last+1; otherwise returns the string obtained from Source by consecutively replacing characters starting at Position with corresponding characters from New_Item. If the end of Source is reached before the characters in New_Item are exhausted, the remaining characters from New_Item are appended to the string.

```ada
procedure Overwrite (Source   : in out String;
                     Position : in Positive;
                     New_Item : in String;
                     Drop     : in Truncation := Right);

```

Equivalent to Move(Overwrite(Source, Position, New_Item), Source, Drop).

```ada
function Delete (Source  : in String;
                 From    : in Positive;
                 Through : in Natural)
   return String;

```

If From &lt= Through, the returned string is Replace_Slice(Source, From, Through, ""), otherwise it is Source.

```ada
procedure Delete (Source  : in out String;
                  From    : in Positive;
                  Through : in Natural;
                  Justify : in Alignment := Left;
                  Pad     : in Character := Space);

```

Equivalent to Move(Delete(Source, From, Through), Source, Justify =&gt Justify, Pad =&gt Pad).

```ada
function Trim (Source : in String;
               Side   : in Trim_End)
  return String;

```

Returns the string obtained by removing from Source all leading Space characters (if Side = Left), all trailing Space characters (if Side = Right), or all leading and trailing Space characters (if Side = Both).

```ada
procedure Trim (Source  : in out String;
                Side    : in Trim_End;
                Justify : in Alignment := Left;
                Pad     : in Character := Space);

```

Equivalent to Move(Trim(Source, Side), Source, Justify=&gtJustify, Pad=&gtPad).

```ada
function Trim (Source : in String;
               Left   : in Maps.Character_Set;
               Right  : in Maps.Character_Set)
   return String;

```

Returns the string obtained by removing from Source all leading characters in Left and all trailing characters in Right.

```ada
procedure Trim (Source  : in out String;
                Left    : in Maps.Character_Set;
                Right   : in Maps.Character_Set;
                Justify : in Alignment := Strings.Left;
                Pad     : in Character := Space);

```

Equivalent to Move(Trim(Source, Left, Right), Source, Justify =&gt Justify, Pad=&gtPad).

```ada
function Head (Source : in String;
               Count  : in Natural;
               Pad    : in Character := Space)
   return String;

```

Returns a string of length Count. If Count &lt= Source'Length, the string comprises the first Count characters of Source. Otherwise its contents are Source concatenated with CountSource'Length Pad characters.

```ada
procedure Head (Source  : in out String;
                Count   : in Natural;
                Justify : in Alignment := Left;
                Pad     : in Character := Space);

```

Equivalent to Move(Head(Source, Count, Pad), Source, Drop=&gtError, Justify=&gtJustify, Pad=&gtPad).

```ada
function Tail (Source : in String;
               Count  : in Natural;
               Pad    : in Character := Space)
   return String;

```

Returns a string of length Count. If Count &lt= Source'Length, the string comprises the last Count characters of Source. Otherwise its contents are Count-Source'Length Pad characters concatenated with Source.

```ada
procedure Tail (Source  : in out String;
                Count   : in Natural;
                Justify : in Alignment := Left;
                Pad     : in Character := Space);

```

Equivalent to Move(Tail(Source, Count, Pad), Source, Drop=&gtError, Justify=&gtJustify, Pad=&gtPad).

```ada
function "*" (Left  : in Natural;
              Right : in Character) return String;

function "*" (Left  : in Natural;
              Right : in String) return String;

```

These functions replicate a character or string a specified number of times. The first function returns a string whose length is Left and each of whose elements is Right. The second function returns a string whose length is Left*Right'Length and whose value is the null string if Left = 0 and is (Left1)*Right & Right otherwise. 

NOTE 1   In the Index and Count functions taking Pattern and Mapping parameters, the actual String parameter passed to Pattern should comprise characters occurring as target characters of the mapping. Otherwise the pattern will not match.

NOTE 2   In the Insert subprograms, inserting at the end of a string is obtained by passing Source'Last+1 as the Before parameter.

NOTE 3   If a null Character_Mapping_Function is passed to any of the string handling subprograms, Constraint_Error is propagated. 


### A.4.4  Bounded-Length String Handling

The language-defined package Strings.Bounded provides a generic package each of whose instances yields a private type Bounded_String and a set of operations. An object of a particular Bounded_String type represents a String whose low bound is 1 and whose length can vary conceptually between 0 and a maximum size established at the generic instantiation. The subprograms for fixed-length string handling are either overloaded directly for Bounded_String, or are modified as needed to reflect the variability in length. Additionally, since the Bounded_String type is private, appropriate constructor and selector operations are provided. 

Reason: Strings.Bounded declares an inner generic package, versus itself being directly a generic child of Strings, in order to retain compatibility with a version of the string-handling packages that is generic with respect to the character and string types.

Reason: The bound of a bounded-length string is specified as a parameter to a generic, versus as the value for a discriminant, because of the inappropriateness of assignment and equality of discriminated types for the copying and comparison of bounded strings.


#### Static Semantics

The library package Strings.Bounded has the following declaration: 

```ada
with Ada.Strings.Maps;
package Ada.Strings.Bounded is
   pragma Preelaborate(Bounded);

```

```ada
   generic
      Max   : Positive;    -- Maximum length of a Bounded_String
   package Generic_Bounded_Length is

```

```ada
      Max_Length : constant Positive := Max;

```

```ada
      type Bounded_String is private;

```

```ada
      Null_Bounded_String : constant Bounded_String;

```

```ada
      subtype Length_Range is Natural range 0 .. Max_Length;

```

```ada
      function Length (Source : in Bounded_String) return Length_Range;

```

```ada
   -- Conversion, Concatenation, and Selection functions

```

```ada
      function To_Bounded_String (Source : in String;
                                  Drop   : in Truncation := Error)
         return Bounded_String;

```

```ada
      function To_String (Source : in Bounded_String) return String;

```

```ada
      function Append (Left, Right : in Bounded_String;
                       Drop        : in Truncation  := Error)
         return Bounded_String;

```

```ada
      function Append (Left  : in Bounded_String;
                       Right : in String;
                       Drop  : in Truncation := Error)
         return Bounded_String;

```

```ada
      function Append (Left  : in String;
                       Right : in Bounded_String;
                       Drop  : in Truncation := Error)
         return Bounded_String;

```

```ada
      function Append (Left  : in Bounded_String;
                       Right : in Character;
                       Drop  : in Truncation := Error)
         return Bounded_String;

```

```ada
      function Append (Left  : in Character;
                       Right : in Bounded_String;
                       Drop  : in Truncation := Error)
         return Bounded_String;

```

```ada
      procedure Append (Source   : in out Bounded_String;
                        New_Item : in Bounded_String;
                        Drop     : in Truncation  := Error);

```

```ada
      procedure Append (Source   : in out Bounded_String;
                        New_Item : in String;
                        Drop     : in Truncation  := Error);

```

```ada
      procedure Append (Source   : in out Bounded_String;
                        New_Item : in Character;
                        Drop     : in Truncation  := Error);

```

```ada
      function "&" (Left, Right : in Bounded_String)
         return Bounded_String;

```

```ada
      function "&" (Left : in Bounded_String; Right : in String)
         return Bounded_String;

```

```ada
      function "&" (Left : in String; Right : in Bounded_String)
         return Bounded_String;

```

```ada
      function "&" (Left : in Bounded_String; Right : in Character)
         return Bounded_String;

```

```ada
      function "&" (Left : in Character; Right : in Bounded_String)
         return Bounded_String;

```

```ada
      function Element (Source : in Bounded_String;
                        Index  : in Positive)
         return Character;

```

```ada
      procedure Replace_Element (Source : in out Bounded_String;
                                 Index  : in Positive;
                                 By     : in Character);

```

```ada
      function Slice (Source : in Bounded_String;
                      Low    : in Positive;
                      High   : in Natural)
         return String;

```

```ada
      function "="  (Left, Right : in Bounded_String) return Boolean;
      function "="  (Left : in Bounded_String; Right : in String)
        return Boolean;

```

```ada
      function "="  (Left : in String; Right : in Bounded_String)
        return Boolean;

```

```ada
      function "&lt"  (Left, Right : in Bounded_String) return Boolean;

```

```ada
      function "&lt"  (Left : in Bounded_String; Right : in String)
        return Boolean;

```

```ada
      function "&lt"  (Left : in String; Right : in Bounded_String)
        return Boolean;

```

```ada
      function "&lt=" (Left, Right : in Bounded_String) return Boolean;

```

```ada
      function "&lt="  (Left : in Bounded_String; Right : in String)
        return Boolean;

```

```ada
      function "&lt="  (Left : in String; Right : in Bounded_String)
        return Boolean;

```

```ada
      function "&gt"  (Left, Right : in Bounded_String) return Boolean;

```

```ada
      function "&gt"  (Left : in Bounded_String; Right : in String)
        return Boolean;

```

```ada
      function "&gt"  (Left : in String; Right : in Bounded_String)
        return Boolean;

```

```ada
      function "&gt=" (Left, Right : in Bounded_String) return Boolean;

```

```ada
      function "&gt="  (Left : in Bounded_String; Right : in String)
        return Boolean;

```

```ada
      function "&gt="  (Left : in String; Right : in Bounded_String)
        return Boolean;

```

```ada
   -- Search functions

```

```ada
      function Index (Source   : in Bounded_String;
                      Pattern  : in String;
                      Going    : in Direction := Forward;
                      Mapping  : in Maps.Character_Mapping
                                 := Maps.Identity)
         return Natural;

```

```ada
      function Index (Source   : in Bounded_String;
                      Pattern  : in String;
                      Going    : in Direction := Forward;
                      Mapping  : in Maps.Character_Mapping_Function)
         return Natural;

```

```ada
      function Index (Source : in Bounded_String;
                      Set    : in Maps.Character_Set;
                      Test   : in Membership := Inside;
                      Going  : in Direction  := Forward)
         return Natural;

```

```ada
      function Index_Non_Blank (Source : in Bounded_String;
                                Going  : in Direction := Forward)
         return Natural;

```

```ada
      function Count (Source   : in Bounded_String;
                      Pattern  : in String;
                      Mapping  : in Maps.Character_Mapping
                                   := Maps.Identity)
         return Natural;

```

```ada
      function Count (Source   : in Bounded_String;
                      Pattern  : in String;
                      Mapping  : in Maps.Character_Mapping_Function)
         return Natural;

```

```ada
      function Count (Source   : in Bounded_String;
                      Set      : in Maps.Character_Set)
         return Natural;

```

```ada
      procedure Find_Token (Source : in Bounded_String;
                            Set    : in Maps.Character_Set;
                            Test   : in Membership;
                            First  : out Positive;
                            Last   : out Natural);

```

```ada
   -- String translation subprograms

```

```ada
      function Translate (Source  : in Bounded_String;
                          Mapping : in Maps.Character_Mapping)
         return Bounded_String;

```

```ada
      procedure Translate (Source  : in out Bounded_String;
                           Mapping : in Maps.Character_Mapping);

```

```ada
      function Translate (Source  : in Bounded_String;
                          Mapping : in Maps.Character_Mapping_Function)
         return Bounded_String;

```

```ada
      procedure Translate (Source  : in out Bounded_String;
                           Mapping : in Maps.Character_Mapping_Function);

```

```ada
   -- String transformation subprograms

```

```ada
      function Replace_Slice (Source   : in Bounded_String;
                              Low      : in Positive;
                              High     : in Natural;
                              By       : in String;
                              Drop     : in Truncation := Error)
         return Bounded_String;

```

```ada
      procedure Replace_Slice (Source   : in out Bounded_String;
                               Low      : in Positive;
                               High     : in Natural;
                               By       : in String;
                               Drop     : in Truncation := Error);

```

```ada
      function Insert (Source   : in Bounded_String;
                       Before   : in Positive;
                       New_Item : in String;
                       Drop     : in Truncation := Error)
         return Bounded_String;

```

```ada
      procedure Insert (Source   : in out Bounded_String;
                        Before   : in Positive;
                        New_Item : in String;
                        Drop     : in Truncation := Error);

```

```ada
      function Overwrite (Source    : in Bounded_String;
                          Position  : in Positive;
                          New_Item  : in String;
                          Drop      : in Truncation := Error)
         return Bounded_String;

```

```ada
      procedure Overwrite (Source    : in out Bounded_String;
                           Position  : in Positive;
                           New_Item  : in String;
                           Drop      : in Truncation := Error);

```

```ada
      function Delete (Source  : in Bounded_String;
                       From    : in Positive;
                       Through : in Natural)
         return Bounded_String;

```

```ada
      procedure Delete (Source  : in out Bounded_String;
                        From    : in Positive;
                        Through : in Natural);

```

```ada
   --String selector subprograms

```

```ada
      function Trim (Source : in Bounded_String;
                     Side   : in Trim_End)
         return Bounded_String;
      procedure Trim (Source : in out Bounded_String;
                      Side   : in Trim_End);

```

```ada
      function Trim (Source : in Bounded_String;
                     Left   : in Maps.Character_Set;
                     Right  : in Maps.Character_Set)
         return Bounded_String;

```

```ada
      procedure Trim (Source : in out Bounded_String;
                      Left   : in Maps.Character_Set;
                      Right  : in Maps.Character_Set);

```

```ada
      function Head (Source : in Bounded_String;
                     Count  : in Natural;
                     Pad    : in Character  := Space;
                     Drop   : in Truncation := Error)
         return Bounded_String;

```

```ada
      procedure Head (Source : in out Bounded_String;
                      Count  : in Natural;
                      Pad    : in Character  := Space;
                      Drop   : in Truncation := Error);

```

```ada
      function Tail (Source : in Bounded_String;
                     Count  : in Natural;
                     Pad    : in Character  := Space;
                     Drop   : in Truncation := Error)
         return Bounded_String;

```

```ada
      procedure Tail (Source : in out Bounded_String;
                      Count  : in Natural;
                      Pad    : in Character  := Space;
                      Drop   : in Truncation := Error);

```

```ada
   --String constructor subprograms

```

```ada
      function "*" (Left  : in Natural;
                    Right : in Character)
         return Bounded_String;

```

```ada
      function "*" (Left  : in Natural;
                    Right : in String)
         return Bounded_String;

```

```ada
      function "*" (Left  : in Natural;
                    Right : in Bounded_String)
         return Bounded_String;

```

```ada
      function Replicate (Count : in Natural;
                          Item  : in Character;
                          Drop  : in Truncation := Error)
         return Bounded_String;

```

```ada
      function Replicate (Count : in Natural;
                          Item  : in String;
                          Drop  : in Truncation := Error)
         return Bounded_String;

```

```ada
      function Replicate (Count : in Natural;
                          Item  : in Bounded_String;
                          Drop  : in Truncation := Error)
         return Bounded_String;

```

```ada
   private
       ... -- not specified by the language
   end Generic_Bounded_Length;

```

```ada
end Ada.Strings.Bounded;

```

Null_Bounded_String represents the null string. If an object of type Bounded_String is not otherwise initialized, it will be initialized to the same value as Null_Bounded_String. 

```ada
function Length (Source : in Bounded_String) return Length_Range;

```

The Length function returns the length of the string represented by Source.

```ada
function To_Bounded_String (Source : in String;
                            Drop   : in Truncation := Error)
   return Bounded_String;

```

If Source'Length &lt= Max_Length then this function returns a Bounded_String that represents Source. Otherwise the effect depends on the value of Drop: 

If Drop=Left, then the result is a Bounded_String that represents the string comprising the rightmost Max_Length characters of Source.

If Drop=Right, then the result is a Bounded_String that represents the string comprising the leftmost Max_Length characters of Source.

If Drop=Error, then Strings.Length_Error is propagated. 

```ada
function To_String (Source : in Bounded_String) return String;

```

To_String returns the String value with lower bound 1 represented by Source. If B is a Bounded_String, then B = To_Bounded_String(To_String(B)).

Each of the Append functions returns a Bounded_String obtained by concatenating the string or character given or represented by one of the parameters, with the string or character given or represented by the other parameter, and applying To_Bounded_String to the concatenation result string, with Drop as provided to the Append function.

Each of the procedures Append(Source, New_Item, Drop) has the same effect as the corresponding assignment Source := Append(Source, New_Item, Drop).

Each of the "&" functions has the same effect as the corresponding Append function, with Error as the Drop parameter. 

```ada
function Element (Source : in Bounded_String;
                  Index  : in Positive)
   return Character;

```

Returns the character at position Index in the string represented by Source; propagates Index_Error if Index &gt Length(Source).

```ada
procedure Replace_Element (Source : in out Bounded_String;
                           Index  : in Positive;
                           By     : in Character);

```

Updates Source such that the character at position Index in the string represented by Source is By; propagates Index_Error if Index &gt Length(Source).

```ada
function Slice (Source : in Bounded_String;
                Low    : in Positive;
                High   : in Natural)
   return String;

```

Returns the slice at positions Low through High in the string represented by Source; propagates Index_Error if Low &gt Length(Source)+1.

Each of the functions "=", "&lt", "&gt", "&lt=", and "&gt=" returns the same result as the corresponding String operation applied to the String values given or represented by the two parameters.

Each of the search subprograms (Index, Index_Non_Blank, Count, Find_Token) has the same effect as the corresponding subprogram in Strings.Fixed applied to the string represented by the Bounded_String parameter.

Each of the Translate subprograms, when applied to a Bounded_String, has an analogous effect to the corresponding subprogram in Strings.Fixed. For the Translate function, the translation is applied to the string represented by the Bounded_String parameter, and the result is converted (via To_Bounded_String) to a Bounded_String. For the Translate procedure, the string represented by the Bounded_String parameter after the translation is given by the Translate function for fixed-length strings applied to the string represented by the original value of the parameter.

Each of the transformation subprograms (Replace_Slice, Insert, Overwrite, Delete), selector subprograms (Trim, Head, Tail), and constructor functions ("*") has an effect based on its corresponding subprogram in Strings.Fixed, and Replicate is based on Fixed."*". For each of these subprograms, the corresponding fixed-length string subprogram is applied to the string represented by the Bounded_String parameter. To_Bounded_String is applied the result string, with Drop (or Error in the case of Generic_Bounded_Length."*") determining the effect when the string length exceeds Max_Length. 

Ramification: The "/=" operations between Bounded_String and String, and between String and Bounded_String, are automatically defined based on the corrsponding "=" operations. 


#### Implementation Advice

Bounded string objects should not be implemented by implicit pointers and dynamic allocation. 

Implementation Note: The following is a possible implementation of the private part of the package: 

```ada
type Bounded_String_Internals (Length : Length_Range := 0) is
   record
      Data : String(1..Length);
   end record;

```

```ada
type Bounded_String is
   record
      Data : Bounded_String_Internals;  -- Unconstrained
   end record;

```

```ada
Null_Bounded_String : constant Bounded_String :=
   (Data =&gt (Length =&gt 0,
             Data   =&gt (1..0 =&gt ' ')));

```


### A.4.5  Unbounded-Length String Handling

The language-defined package Strings.Unbounded provides a private type Unbounded_String and a set of operations. An object of type Unbounded_String represents a String whose low bound is 1 and whose length can vary conceptually between 0 and Natural'Last. The subprograms for fixed-length string handling are either overloaded directly for Unbounded_String, or are modified as needed to reflect the flexibility in length. Since the Unbounded_String type is private, relevant constructor and selector operations are provided. 

Reason: The transformation operations for fixed- and bounded-length strings that are not necessarily length preserving are supplied for Unbounded_String as procedures as well as functions. This allows an implementation to do an initial allocation for an unbounded string and to avoid further allocations as long as the length does not exceed the allocated length. 


#### Static Semantics

The library package Strings.Unbounded has the following declaration: 

```ada
with Ada.Strings.Maps;
package Ada.Strings.Unbounded is
   pragma Preelaborate(Unbounded);

```

```ada
   type Unbounded_String is private;

```

```ada
   Null_Unbounded_String : constant Unbounded_String;

```

```ada
   function Length (Source : in Unbounded_String) return Natural;

```

```ada
   type String_Access is access all String;
   procedure Free (X : in out String_Access);

```

```ada
-- Conversion, Concatenation, and Selection functions

```

```ada
   function To_Unbounded_String (Source : in String)
      return Unbounded_String;

```

```ada
   function To_Unbounded_String (Length : in Natural)
      return Unbounded_String;

```

```ada
   function To_String (Source : in Unbounded_String) return String;

```

```ada
   procedure Append (Source   : in out Unbounded_String;
                     New_Item : in Unbounded_String);

```

```ada
   procedure Append (Source   : in out Unbounded_String;
                     New_Item : in String);

```

```ada
   procedure Append (Source   : in out Unbounded_String;
                     New_Item : in Character);

```

```ada
   function "&" (Left, Right : in Unbounded_String)
      return Unbounded_String;

```

```ada
   function "&" (Left : in Unbounded_String; Right : in String)
      return Unbounded_String;

```

```ada
   function "&" (Left : in String; Right : in Unbounded_String)
      return Unbounded_String;

```

```ada
   function "&" (Left : in Unbounded_String; Right : in Character)
      return Unbounded_String;

```

```ada
   function "&" (Left : in Character; Right : in Unbounded_String)
      return Unbounded_String;

```

```ada
   function Element (Source : in Unbounded_String;
                     Index  : in Positive)
      return Character;

```

```ada
   procedure Replace_Element (Source : in out Unbounded_String;
                              Index  : in Positive;
                              By     : in Character);

```

```ada
   function Slice (Source : in Unbounded_String;
                   Low    : in Positive;
                   High   : in Natural)
      return String;

```

```ada
   function "="  (Left, Right : in Unbounded_String) return Boolean;

```

```ada
   function "="  (Left : in Unbounded_String; Right : in String)
     return Boolean;

```

```ada
   function "="  (Left : in String; Right : in Unbounded_String)
     return Boolean;

```

```ada
   function "&lt"  (Left, Right : in Unbounded_String) return Boolean;

```

```ada
   function "&lt"  (Left : in Unbounded_String; Right : in String)
     return Boolean;

```

```ada
   function "&lt"  (Left : in String; Right : in Unbounded_String)
     return Boolean;

```

```ada
   function "&lt=" (Left, Right : in Unbounded_String) return Boolean;

```

```ada
   function "&lt="  (Left : in Unbounded_String; Right : in String)
     return Boolean;

```

```ada
   function "&lt="  (Left : in String; Right : in Unbounded_String)
     return Boolean;

```

```ada
   function "&gt"  (Left, Right : in Unbounded_String) return Boolean;

```

```ada
   function "&gt"  (Left : in Unbounded_String; Right : in String)
     return Boolean;

```

```ada
   function "&gt"  (Left : in String; Right : in Unbounded_String)
     return Boolean;

```

```ada
   function "&gt=" (Left, Right : in Unbounded_String) return Boolean;

```

```ada
   function "&gt="  (Left : in Unbounded_String; Right : in String)
     return Boolean;

```

```ada
   function "&gt="  (Left : in String; Right : in Unbounded_String)
     return Boolean;

```

```ada
-- Search subprograms

```

```ada
   function Index (Source   : in Unbounded_String;
                   Pattern  : in String;
                   Going    : in Direction := Forward;
                   Mapping  : in Maps.Character_Mapping
                                := Maps.Identity)
      return Natural;

```

```ada
   function Index (Source   : in Unbounded_String;
                   Pattern  : in String;
                   Going    : in Direction := Forward;
                   Mapping  : in Maps.Character_Mapping_Function)
      return Natural;

```

```ada
   function Index (Source : in Unbounded_String;
                   Set    : in Maps.Character_Set;
                   Test   : in Membership := Inside;
                   Going  : in Direction  := Forward) return Natural;

```

```ada
   function Index_Non_Blank (Source : in Unbounded_String;
                             Going  : in Direction := Forward)
      return Natural;

```

```ada
   function Count (Source   : in Unbounded_String;
                   Pattern  : in String;
                   Mapping  : in Maps.Character_Mapping
                                := Maps.Identity)
      return Natural;

```

```ada
   function Count (Source   : in Unbounded_String;
                   Pattern  : in String;
                   Mapping  : in Maps.Character_Mapping_Function)
      return Natural;

```

```ada
   function Count (Source   : in Unbounded_String;
                   Set      : in Maps.Character_Set)
      return Natural;

```

```ada
   procedure Find_Token (Source : in Unbounded_String;
                         Set    : in Maps.Character_Set;
                         Test   : in Membership;
                         First  : out Positive;
                         Last   : out Natural);

```

```ada
-- String translation subprograms

```

```ada
   function Translate (Source  : in Unbounded_String;
                       Mapping : in Maps.Character_Mapping)
      return Unbounded_String;

```

```ada
   procedure Translate (Source  : in out Unbounded_String;
                        Mapping : in Maps.Character_Mapping);

```

```ada
   function Translate (Source  : in Unbounded_String;
                       Mapping : in Maps.Character_Mapping_Function)
      return Unbounded_String;

```

```ada
   procedure Translate (Source  : in out Unbounded_String;
                        Mapping : in Maps.Character_Mapping_Function);

```

```ada
-- String transformation subprograms

```

```ada
   function Replace_Slice (Source   : in Unbounded_String;
                           Low      : in Positive;
                           High     : in Natural;
                           By       : in String)
      return Unbounded_String;

```

```ada
   procedure Replace_Slice (Source   : in out Unbounded_String;
                            Low      : in Positive;
                            High     : in Natural;
                            By       : in String);

```

```ada
   function Insert (Source   : in Unbounded_String;
                    Before   : in Positive;
                    New_Item : in String)
      return Unbounded_String;

```

```ada
   procedure Insert (Source   : in out Unbounded_String;
                     Before   : in Positive;
                     New_Item : in String);

```

```ada
   function Overwrite (Source    : in Unbounded_String;
                       Position  : in Positive;
                       New_Item  : in String)
      return Unbounded_String;

```

```ada
   procedure Overwrite (Source    : in out Unbounded_String;
                        Position  : in Positive;
                        New_Item  : in String);

```

```ada
   function Delete (Source  : in Unbounded_String;
                    From    : in Positive;
                    Through : in Natural)
      return Unbounded_String;

```

```ada
   procedure Delete (Source  : in out Unbounded_String;
                     From    : in Positive;
                     Through : in Natural);

```

```ada
   function Trim (Source : in Unbounded_String;
                  Side   : in Trim_End)
      return Unbounded_String;

```

```ada
   procedure Trim (Source : in out Unbounded_String;
                   Side   : in Trim_End);

```

```ada
   function Trim (Source : in Unbounded_String;
                  Left   : in Maps.Character_Set;
                  Right  : in Maps.Character_Set)
      return Unbounded_String;

```

```ada
   procedure Trim (Source : in out Unbounded_String;
                   Left   : in Maps.Character_Set;
                   Right  : in Maps.Character_Set);

```

```ada
   function Head (Source : in Unbounded_String;
                  Count  : in Natural;
                  Pad    : in Character := Space)
      return Unbounded_String;

```

```ada
   procedure Head (Source : in out Unbounded_String;
                   Count  : in Natural;
                   Pad    : in Character := Space);

```

```ada
   function Tail (Source : in Unbounded_String;
                  Count  : in Natural;
                  Pad    : in Character := Space)
      return Unbounded_String;

```

```ada
   procedure Tail (Source : in out Unbounded_String;
                   Count  : in Natural;
                   Pad    : in Character := Space);

```

```ada
   function "*" (Left  : in Natural;
                 Right : in Character)
      return Unbounded_String;

```

```ada
   function "*" (Left  : in Natural;
                 Right : in String)
      return Unbounded_String;

```

```ada
   function "*" (Left  : in Natural;
                 Right : in Unbounded_String)
      return Unbounded_String;

```

```ada
private
   ... -- not specified by the language
end Ada.Strings.Unbounded;

```

Null_Unbounded_String represents the null String. If an object of type Unbounded_String is not otherwise initialized, it will be initialized to the same value as Null_Unbounded_String.

The function Length returns the length of the String represented by Source.

The type String_Access provides a (nonprivate) access type for explicit processing of unbounded-length strings. The procedure Free performs an unchecked deallocation of an object of type String_Access.

The function To_Unbounded_String(Source : in String) returns an Unbounded_String that represents Source. The function To_Unbounded_String(Length : in Natural) returns an Unbounded_String that represents an uninitialized String whose length is Length.

The function To_String returns the String with lower bound 1 represented by Source. To_String and To_Unbounded_String are related as follows: 

If S is a String, then To_String(To_Unbounded_String(S)) = S.

If U is an Unbounded_String, then To_Unbounded_String(To_String(U)) = U. 

For each of the Append procedures, the resulting string represented by the Source parameter is given by the concatenation of the original value of Source and the value of New_Item.

Each of the "&" functions returns an Unbounded_String obtained by concatenating the string or character given or represented by one of the parameters, with the string or character given or represented by the other parameter, and applying To_Unbounded_String to the concatenation result string.

The Element, Replace_Element, and Slice subprograms have the same effect as the corresponding bounded-length string subprograms.

Each of the functions "=", "&lt", "&gt", "&lt=", and "&gt=" returns the same result as the corresponding String operation applied to the String values given or represented by Left and Right.

Each of the search subprograms (Index, Index_Non_Blank, Count, Find_Token) has the same effect as the corresponding subprogram in Strings.Fixed applied to the string represented by the Unbounded_String parameter.

The Translate function has an analogous effect to the corresponding subprogram in Strings.Fixed. The translation is applied to the string represented by the Unbounded_String parameter, and the result is converted (via To_Unbounded_String) to an Unbounded_String.

Each of the transformation functions (Replace_Slice, Insert, Overwrite, Delete), selector functions (Trim, Head, Tail), and constructor functions ("*") is likewise analogous to its corresponding subprogram in Strings.Fixed. For each of the subprograms, the corresponding fixed-length string subprogram is applied to the string represented by the Unbounded_String parameter, and To_Unbounded_String is applied the result string.

For each of the procedures Translate, Replace_Slice, Insert, Overwrite, Delete, Trim, Head, and Tail, the resulting string represented by the Source parameter is given by the corresponding function for fixed-length strings applied to the string represented by Source's original value. 


#### Implementation Requirements

No storage associated with an Unbounded_String object shall be lost upon assignment or scope exit. 

Implementation Note: A sample implementation of the private part of the package and several of the subprograms appears in the Rationale.


### A.4.6  String-Handling Sets and Mappings

The language-defined package Strings.Maps.Constants declares Character_Set and Character_Mapping constants corresponding to classification and conversion functions in package Characters.Handling. 

Discussion: The Constants package is a child of Strings.Maps since it needs visibility of the private part of Strings.Maps in order to initialize the constants in a preelaborable way (i.e. via aggregates versus function calls). 


#### Static Semantics

The library package Strings.Maps.Constants has the following declaration:

```ada
package Ada.Strings.Maps.Constants is
   pragma Preelaborate(Constants);

```

```ada
   Control_Set           : constant Character_Set;
   Graphic_Set           : constant Character_Set;
   Letter_Set            : constant Character_Set;
   Lower_Set             : constant Character_Set;
   Upper_Set             : constant Character_Set;
   Basic_Set             : constant Character_Set;
   Decimal_Digit_Set     : constant Character_Set;
   Hexadecimal_Digit_Set : constant Character_Set;
   Alphanumeric_Set      : constant Character_Set;
   Special_Set           : constant Character_Set;
   ISO_646_Set           : constant Character_Set;

```

```ada
   Lower_Case_Map        : constant Character_Mapping;
     --Maps to lower case for letters, else identity
   Upper_Case_Map        : constant Character_Mapping;
     --Maps to upper case for letters, else identity
   Basic_Map             : constant Character_Mapping;
     --Maps to basic letter for letters, else identity

```

```ada
private
   ... -- not specified by the language
end Ada.Strings.Maps.Constants;

```

Each of these constants represents a correspondingly named set of characters or character mapping in Characters.Handling (see A.3.2). 


### A.4.7  Wide_String Handling

Facilities for handling strings of Wide_Character elements are found in the packages Strings.Wide_Maps, Strings.Wide_Fixed, Strings.Wide_Bounded, Strings.Wide_Unbounded, and Strings.Wide_Maps.Wide_Constants. They provide the same string-handling operations as the corresponding packages for strings of Character elements. 


#### Static Semantics

The package Strings.Wide_Maps has the following declaration. 

```ada
package Ada.Strings.Wide_Maps is
   pragma Preelaborate(Wide_Maps);

```

```ada
   -- Representation for a set of Wide_Character values:
   type Wide_Character_Set is private;

```

```ada
   Null_Set : constant Wide_Character_Set;

```

```ada
   type Wide_Character_Range is
     record
         Low  : Wide_Character;
         High : Wide_Character;
     end record;
   -- Represents Wide_Character range Low..High

```

```ada
   type Wide_Character_Ranges is array (Positive range &lt&gt)
      of Wide_Character_Range;

```

```ada
   function To_Set    (Ranges : in Wide_Character_Ranges)
      return Wide_Character_Set;

```

```ada
   function To_Set    (Span   : in Wide_Character_Range)
      return Wide_Character_Set;

```

```ada
   function To_Ranges (Set    : in Wide_Character_Set)
      return Wide_Character_Ranges;

```

```ada
   function "="   (Left, Right : in Wide_Character_Set) return Boolean;

```

```ada
   function "not" (Right : in Wide_Character_Set)
      return Wide_Character_Set;
   function "and" (Left, Right : in Wide_Character_Set)
      return Wide_Character_Set;
   function "or"  (Left, Right : in Wide_Character_Set)
      return Wide_Character_Set;
   function "xor" (Left, Right : in Wide_Character_Set)
      return Wide_Character_Set;
   function "-"   (Left, Right : in Wide_Character_Set)
      return Wide_Character_Set;

```

```ada
   function Is_In (Element : in Wide_Character;
                   Set     : in Wide_Character_Set)
      return Boolean;

```

```ada
   function Is_Subset (Elements : in Wide_Character_Set;
                       Set      : in Wide_Character_Set)
      return Boolean;

```

```ada
   function "&lt=" (Left  : in Wide_Character_Set;
                  Right : in Wide_Character_Set)
      return Boolean renames Is_Subset;

```

```ada
   -- Alternative representation for a set of Wide_Character values:
   subtype Wide_Character_Sequence is Wide_String;

```

```ada
   function To_Set (Sequence  : in Wide_Character_Sequence)
      return Wide_Character_Set;

```

```ada
   function To_Set (Singleton : in Wide_Character)
      return Wide_Character_Set;

```

```ada
   function To_Sequence (Set  : in Wide_Character_Set)
      return Wide_Character_Sequence;

```

```ada
   -- Representation for a Wide_Character to Wide_Character mapping:
   type Wide_Character_Mapping is private;

```

```ada
   function Value (Map     : in Wide_Character_Mapping;
                   Element : in Wide_Character)
      return Wide_Character;

```

```ada
   Identity : constant Wide_Character_Mapping;

```

```ada
   function To_Mapping (From, To : in Wide_Character_Sequence)
      return Wide_Character_Mapping;

```

```ada
   function To_Domain (Map : in Wide_Character_Mapping)
      return Wide_Character_Sequence;

```

```ada
   function To_Range  (Map : in Wide_Character_Mapping)
      return Wide_Character_Sequence;

```

```ada
   type Wide_Character_Mapping_Function is
      access function (From : in Wide_Character) return Wide_Character;

```

```ada
private
   ... -- not specified by the language
end Ada.Strings.Wide_Maps;

```

The context clause for each of the packages Strings.Wide_Fixed, Strings.Wide_Bounded, and Strings.Wide_Unbounded identifies Strings.Wide_Maps instead of Strings.Maps.

For each of the packages Strings.Fixed, Strings.Bounded, Strings.Unbounded, and Strings.Maps.Constantsthe corresponding wide string package has the same contents except that 

Wide_Space replaces Space

Wide_Character replaces Character

Wide_String replaces String

Wide_Character_Set replaces Character_Set

Wide_Character_Mapping replaces Character_Mapping

Wide_Character_Mapping_Function replaces Character_Mapping_Function

Wide_Maps replaces Maps

Bounded_Wide_String replaces Bounded_String

Null_Bounded_Wide_String replaces Null_Bounded_String

To_Bounded_Wide_String replaces To_Bounded_String

To_Wide_String replaces To_String

Unbounded_Wide_String replaces Unbounded_String

Null_Unbounded_Wide_String replaces Null_Unbounded_String

Wide_String_Access replaces String_Access

To_Unbounded_Wide_String replaces To_Unbounded_String

The following additional declaration is present in Strings.Wide_Maps.Wide_Constants: 

```ada
Character_Set : constant Wide_Maps.Wide_Character_Set;
--Contains each Wide_Character value WC such thatCharacters.Is_Character(WC) is True

```

NOTE   If a null Wide_Character_Mapping_Function is passed to any of the Wide_String handling subprograms, Constraint_Error is propagated.

NOTE   Each Wide_Character_Set constant in the package Strings.Wide_Maps.Wide_Constants contains no values outside the Character portion of Wide_Character. Similarly, each Wide_Character_Mapping constant in this package is the identity mapping when applied to any element outside the Character portion of Wide_Character. 


## A.5  The Numerics Packages

The library package Numerics is the parent of several child units that provide facilities for mathematical computation. One child, the generic package Generic_Elementary_Functions, is defined in A.5.1, together with nongeneric equivalents; two others, the package Float_Random and the generic package Discrete_Random, are defined in A.5.2. Additional (optional) children are defined in Annex G, "Numerics". 


#### Static Semantics

 

```ada
package Ada.Numerics is
    pragma Pure(Numerics);
   Argument_Error : exception;
   Pi : constant :=
          3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;
   e  : constant :=
          2.71828_18284_59045_23536_02874_71352_66249_77572_47093_69996;
end Ada.Numerics;

```

The Argument_Error exception is raised by a subprogram in a child unit of Numerics to signal that one or more of the actual subprogram parameters are outside the domain of the corresponding mathematical function.


#### Implementation Permissions

The implementation may specify the values of Pi and e to a larger number of significant digits. 

Reason: 51 digits seem more than adequate for all present computers; converted to binary, the values given above are accurate to more than 160 bits. Nevertheless, the permission allows implementations to accommodate unforeseen hardware advances. 


#### Extensions to Ada 83

Numerics and its children were not predefined in Ada 83. 


### A.5.1  Elementary Functions

Implementation-defined approximations to the mathematical functions known as the "elementary functions" are provided by the subprograms in Numerics.Generic_Elementary_Functions. Nongeneric equivalents of this generic package for each of the predefined floating point types are also provided as children of Numerics. 

Implementation defined: The accuracy actually achieved by the elementary functions.


#### Static Semantics

The generic library package Numerics.Generic_Elementary_Functions has the following declaration: 

```ada
generic
   type Float_Type is digits &lt&gt;

package Ada.Numerics.Generic_Elementary_Functions is
   pragma Pure(Generic_Elementary_Functions);

```

```ada
   function Sqrt    (X           : Float_Type'Base) return Float_Type'Base;
   function Log     (X           : Float_Type'Base) return Float_Type'Base;
   function Log     (X, Base     : Float_Type'Base) return Float_Type'Base;
   function Exp     (X           : Float_Type'Base) return Float_Type'Base;
   function "**"    (Left, Right : Float_Type'Base) return Float_Type'Base;

```

```ada
   function Sin     (X           : Float_Type'Base) return Float_Type'Base;
   function Sin     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Cos     (X           : Float_Type'Base) return Float_Type'Base;
   function Cos     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Tan     (X           : Float_Type'Base) return Float_Type'Base;
   function Tan     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Cot     (X           : Float_Type'Base) return Float_Type'Base;
   function Cot     (X, Cycle    : Float_Type'Base) return Float_Type'Base;

```

```ada
   function Arcsin  (X           : Float_Type'Base) return Float_Type'Base;
   function Arcsin  (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Arccos  (X           : Float_Type'Base) return Float_Type'Base;
   function Arccos  (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Arctan  (Y           : Float_Type'Base;
                     X           : Float_Type'Base := 1.0)
                                                    return Float_Type'Base;
   function Arctan  (Y           : Float_Type'Base;
                     X           : Float_Type'Base := 1.0;
                     Cycle       : Float_Type'Base) return Float_Type'Base;
   function Arccot  (X           : Float_Type'Base;
                     Y           : Float_Type'Base := 1.0)
                                                    return Float_Type'Base;
   function Arccot  (X           : Float_Type'Base;
                     Y           : Float_Type'Base := 1.0;
                     Cycle       : Float_Type'Base) return Float_Type'Base;

```

```ada
   function Sinh    (X           : Float_Type'Base) return Float_Type'Base;
   function Cosh    (X           : Float_Type'Base) return Float_Type'Base;
   function Tanh    (X           : Float_Type'Base) return Float_Type'Base;
   function Coth    (X           : Float_Type'Base) return Float_Type'Base;
   function Arcsinh (X           : Float_Type'Base) return Float_Type'Base;
   function Arccosh (X           : Float_Type'Base) return Float_Type'Base;
   function Arctanh (X           : Float_Type'Base) return Float_Type'Base;
   function Arccoth (X           : Float_Type'Base) return Float_Type'Base;

```

```ada
end Ada.Numerics.Generic_Elementary_Functions;

```

The library package Numerics.Elementary_Functions defines the same subprograms as Numerics.Generic_Elementary_Functions, except that the predefined type Float is systematically substituted for Float_Type'Base throughout. Nongeneric equivalents of Numerics.Generic_Elementary_Functions for each of the other predefined floating point types are defined similarly, with the names Numerics.Short_Elementary_Functions, Numerics.Long_Elementary_Functions, etc. 

Reason: The nongeneric equivalents are provided to allow the programmer to construct simple mathematical applications without being required to understand and use generics. 

The functions have their usual mathematical meanings. When the Base parameter is specified, the Log function computes the logarithm to the given base; otherwise, it computes the natural logarithm. When the Cycle parameter is specified, the parameter X of the forward trigonometric functions (Sin, Cos, Tan, and Cot) and the results of the inverse trigonometric functions (Arcsin, Arccos, Arctan, and Arccot) are measured in units such that a full cycle of revolution has the given value; otherwise, they are measured in radians.

The computed results of the mathematically multivalued functions are rendered single-valued by the following conventions, which are meant to imply the principal branch: 

The results of the Sqrt and Arccosh functions and that of the exponentiation operator are nonnegative.

The result of the Arcsin function is in the quadrant containing the point (1.0, x), where x is the value of the parameter X. This quadrant is I or IV; thus, the range of the Arcsin function is approximately /2.0 to /2.0 (Cycle/4.0 to Cycle/4.0, if the parameter Cycle is specified).

The result of the Arccos function is in the quadrant containing the point (x, 1.0), where x is the value of the parameter X. This quadrant is I or II; thus, the Arccos function ranges from 0.0 to approximately  (Cycle/2.0, if the parameter Cycle is specified).

The results of the Arctan and Arccot functions are in the quadrant containing the point (x, y), where x and y are the values of the parameters X and Y, respectively. This may be any quadrant (I through IV) when the parameter X (resp., Y) of Arctan (resp., Arccot) is specified, but it is restricted to quadrants I and IV (resp., I and II) when that parameter is omitted. Thus, the range when that parameter is specified is approximately  to  (Cycle/2.0 to Cycle/2.0, if the parameter Cycle is specified); when omitted, the range of Arctan (resp., Arccot) is that of Arcsin (resp., Arccos), as given above. When the point (x, y) lies on the negative x-axis, the result approximates 

 (resp., ) when the sign of the parameter Y is positive (resp., negative), if Float_Type'Signed_Zeros is True;

, if Float_Type'Signed_Zeros is False. 

(In the case of the inverse trigonometric functions, in which a result lying on or near one of the axes may not be exactly representable, the approximation inherent in computing the result may place it in an adjacent quadrant, close to but on the wrong side of the axis.) 


#### Dynamic Semantics

The exception Numerics.Argument_Error is raised, signaling a parameter value outside the domain of the corresponding mathematical function, in the following cases: 

by any forward or inverse trigonometric function with specified cycle, when the value of the parameter Cycle is zero or negative;

by the Log function with specified base, when the value of the parameter Base is zero, one, or negative;

by the Sqrt and Log functions, when the value of the parameter X is negative;

by the exponentiation operator, when the value of the left operand is negative or when both operands have the value zero;

by the Arcsin, Arccos, and Arctanh functions, when the absolute value of the parameter X exceeds one;

by the Arctan and Arccot functions, when the parameters X and Y both have the value zero;

by the Arccosh function, when the value of the parameter X is less than one; and

by the Arccoth function, when the absolute value of the parameter X is less than one. 

The exception Constraint_Error is raised, signaling a pole of the mathematical function (analogous to dividing by zero), in the following cases, provided that Float_Type'Machine_Overflows is True: 

by the Log, Cot, and Coth functions, when the value of the parameter X is zero;

by the exponentiation operator, when the value of the left operand is zero and the value of the exponent is negative;

by the Tan function with specified cycle, when the value of the parameter X is an odd multiple of the quarter cycle;

by the Cot function with specified cycle, when the value of the parameter X is zero or a multiple of the half cycle; and

by the Arctanh and Arccoth functions, when the absolute value of the parameter X is one. 

[Constraint_Error can also be raised when a finite result overflows (see G.2.4); this may occur for parameter values sufficiently near poles, and, in the case of some of the functions, for parameter values with sufficiently large magnitudes.] When Float_Type'Machine_Overflows is False, the result at poles is unspecified. 

Reason: The purpose of raising Constraint_Error (rather than Numerics.Argument_Error) at the poles of a function, when Float_Type'Machine_Overflows is True, is to provide continuous behavior as the actual parameters of the function approach the pole and finally reach it. 

Discussion: It is anticipated that an Ada binding to IEC 559:1989 will be developed in the future. As part of such a binding, the Machine_Overflows attribute of a conformant floating point type will be specified to yield False, which will permit both the predefined arithmetic operations and implementations of the elementary functions to deliver signed infinities (and set the overflow flag defined by the binding) instead of raising Constraint_Error in overflow situations, when traps are disabled. Similarly, it is appropriate for the elementary functions to deliver signed infinities (and set the zero-divide flag defined by the binding) instead of raising Constraint_Error at poles, when traps are disabled. Finally, such a binding should also specify the behavior of the elementary functions, when sensible, given parameters with infinite values. 

When one parameter of a function with multiple parameters represents a pole and another is outside the function's domain, the latter takes precedence (i.e., Numerics.Argument_Error is raised). 


#### Implementation Requirements

In the implementation of Numerics.Generic_Elementary_Functions, the range of intermediate values allowed during the calculation of a final result shall not be affected by any range constraint of the subtype Float_Type. 

Implementation Note: Implementations of Numerics.Generic_Elementary_Functions written in Ada should therefore avoid declaring local variables of subtype Float_Type; the subtype Float_Type'Base should be used instead. 

In the following cases, evaluation of an elementary function shall yield the prescribed result, provided that the preceding rules do not call for an exception to be raised: 

When the parameter X has the value zero, the Sqrt, Sin, Arcsin, Tan, Sinh, Arcsinh, Tanh, and Arctanh functions yield a result of zero, and the Exp, Cos, and Cosh functions yield a result of one.

When the parameter X has the value one, the Sqrt function yields a result of one, and the Log, Arccos, and Arccosh functions yield a result of zero.

When the parameter Y has the value zero and the parameter X has a positive value, the Arctan and Arccot functions yield a result of zero.

The results of the Sin, Cos, Tan, and Cot functions with specified cycle are exact when the mathematical result is zero; those of the first two are also exact when the mathematical result is ± 1.0.

Exponentiation by a zero exponent yields the value one. Exponentiation by a unit exponent yields the value of the left operand. Exponentiation of the value one yields the value one. Exponentiation of the value zero yields the value zero. 

Other accuracy requirements for the elementary functions, which apply only in implementations conforming to the Numerics Annex, and then only in the "strict" mode defined there (see G.2), are given in G.2.4.

When Float_Type'Signed_Zeros is True, the sign of a zero result shall be as follows: 

A prescribed zero result delivered at the origin by one of the odd functions (Sin, Arcsin, Sinh, Arcsinh, Tan, Arctan or Arccot as a function of Y when X is fixed and positive, Tanh, and Arctanh) has the sign of the parameter X (Y, in the case of Arctan or Arccot).

A prescribed zero result delivered by one of the odd functions away from the origin, or by some other elementary function, has an implementation-defined sign. 

Implementation defined: The sign of a zero result from some of the operators or functions in Numerics.Generic_Elementary_Functions, when Float_Type'Signed_Zeros is True.

[A zero result that is not a prescribed result (i.e., one that results from rounding or underflow) has the correct mathematical sign.] 

Reason: This is a consequence of the rules specified in IEC 559:1989 as they apply to underflow situations with traps disabled. 


#### Implementation Permissions

The nongeneric equivalent packages may, but need not, be actual instantiations of the generic package for the appropriate predefined type. 


#### Wording Changes from Ada 83

The semantics of Numerics.Generic_Elementary_Functions differs from Generic_Elementary_Functions as defined in ISO/IEC DIS 11430 (for Ada 83) in the following ways: 

The generic package is a child unit of the package defining the Argument_Error exception.

DIS 11430 specified names for the nongeneric equivalents, if provided. Here, those nongeneric equivalents are required.

Implementations are not allowed to impose an optional restriction that the generic actual parameter associated with Float_Type be unconstrained. (In view of the ability to declare variables of subtype Float_Type'Base in implementations of Numerics.Generic_Elementary_Functions, this flexibility is no longer needed.)

The sign of a prescribed zero result at the origin of the odd functions is specified, when Float_Type'Signed_Zeros is True. This conforms with recommendations of Kahan and other numerical analysts.

The dependence of Arctan and Arccot on the sign of a parameter value of zero is tied to the value of Float_Type'Signed_Zeros.

Sqrt is prescribed to yield a result of one when its parameter has the value one. This guarantee makes it easier to achieve certain prescribed results of the complex elementary functions (see G.1.2, "Complex Elementary Functions").

Conformance to accuracy requirements is conditional. 


### A.5.2  Random Number Generation

[Facilities for the generation of pseudo-random floating point numbers are provided in the package Numerics.Float_Random; the generic package Numerics.Discrete_Random provides similar facilities for the generation of pseudo-random integers and pseudo-random values of enumeration types. For brevity, pseudo-random values of any of these types are called random numbers.

Some of the facilities provided are basic to all applications of random numbers. These include a limited private type each of whose objects serves as the generator of a (possibly distinct) sequence of random numbers; a function to obtain the "next" random number from a given sequence of random numbers (that is, from its generator); and subprograms to initialize or reinitialize a given generator to a time-dependent state or a state denoted by a single integer.

Other facilities are provided specifically for advanced applications. These include subprograms to save and restore the state of a given generator; a private type whose objects can be used to hold the saved state of a generator; and subprograms to obtain a string representation of a given generator state, or, given such a string representation, the corresponding state.] 

Discussion: These facilities support a variety of requirements ranging from repeatable sequences (for debugging) to unique sequences in each execution of a program. 


#### Static Semantics

The library package Numerics.Float_Random has the following declaration: 

```ada
package Ada.Numerics.Float_Random is

```

```ada
   -- Basic facilities

```

```ada
   type Generator is limited private;

```

```ada
   subtype Uniformly_Distributed is Float range 0.0 .. 1.0;
   function Random (Gen : Generator) return Uniformly_Distributed;

```

```ada
   procedure Reset (Gen       : in Generator;
                    Initiator : in Integer);
   procedure Reset (Gen       : in Generator);

```

```ada
   -- Advanced facilities

```

```ada
   type State is private;

```

```ada
   procedure Save  (Gen        : in  Generator;
                    To_State   : out State);
   procedure Reset (Gen        : in  Generator;
                    From_State : in  State);

```

```ada
   Max_Image_Width : constant := implementation-defined integer value;

```

```ada
   function Image (Of_State    : State)  return String;
   function Value (Coded_State : String) return State;

```

```ada
private
   ... -- not specified by the language
end Ada.Numerics.Float_Random;

```

The generic library package Numerics.Discrete_Random has the following declaration: 

```ada
generic
   type Result_Subtype is (&lt&gt);
package Ada.Numerics.Discrete_Random is

```

```ada
   -- Basic facilities

```

```ada
   type Generator is limited private;

```

```ada
   function Random (Gen : Generator) return Result_Subtype;

```

```ada
   procedure Reset (Gen       : in Generator;
                    Initiator : in Integer);
   procedure Reset (Gen       : in Generator);

```

```ada
   -- Advanced facilities

```

```ada
   type State is private;

```

```ada
   procedure Save  (Gen        : in  Generator;
                    To_State   : out State);
   procedure Reset (Gen        : in  Generator;
                    From_State : in  State);

```

```ada
   Max_Image_Width : constant := implementation-defined integer value;

```

```ada
   function Image (Of_State    : State)  return String;
   function Value (Coded_State : String) return State;

```

```ada
private
   ... -- not specified by the language
end Ada.Numerics.Discrete_Random;

```

Implementation defined: The value of Numerics.Float_Random.Max_Image_Width.

Implementation defined: The value of Numerics.Discrete_Random.Max_Image_Width.

Implementation Note: The following is a possible implementation of the private part of each package (assuming the presence of "with Ada.Finalization;" as a context clause): 

```ada
type State is ...;
type Access_State is access State;
type Generator is new Finalization.Limited_Controlled with
   record
      S : Access_State := new State'(...);
   end record;
procedure Finalize (G : in out Generator);

```

Clearly some level of indirection is required in the implementation of a Generator, since the parameter mode is in for all operations on a Generator. For this reason, Numerics.Float_Random and Numerics.Discrete_Random cannot be declared pure. 

An object of the limited private type Generator is associated with a sequence of random numbers. Each generator has a hidden (internal) state, which the operations on generators use to determine the position in the associated sequence. All generators are implicitly initialized to an unspecified state that does not vary from one program execution to another; they may also be explicitly initialized, or reinitialized, to a time-dependent state, to a previously saved state, or to a state uniquely denoted by an integer value. 

Discussion: The repeatability provided by the implicit initialization may be exploited for testing or debugging purposes. 

An object of the private type State can be used to hold the internal state of a generator. Such objects are only needed if the application is designed to save and restore generator states or to examine or manufacture them.

The operations on generators affect the state and therefore the future values of the associated sequence. The semantics of the operations on generators and states are defined below. 

```ada
function Random (Gen : Generator) return Uniformly_Distributed;
function Random (Gen : Generator) return Result_Subtype;

```

Obtains the "next" random number from the given generator, relative to its current state, according to an implementation-defined algorithm. The result of the function in Numerics.Float_Random is delivered as a value of the subtype Uniformly_Distributed, which is a subtype of the predefined type Float having a range of 0.0 .. 1.0. The result of the function in an instantiation of Numerics.Discrete_Random is delivered as a value of the generic formal subtype Result_Subtype. 

This paragraph was deleted.Implementation defined: The algorithms for random number generation.

Reason: The requirement for a level of indirection in accessing the internal state of a generator arises from the desire to make Random a function, rather than a procedure. 

```ada
procedure Reset (Gen       : in Generator;
                 Initiator : in Integer);
procedure Reset (Gen       : in Generator);

```

Sets the state of the specified generator to one that is an unspecified function of the value of the parameter Initiator (or to a time-dependent state, if only a generator parameter is specified). The latter form of the procedure is known as the time-dependent Reset procedure. 

Implementation Note: The time-dependent Reset procedure can be implemented by mapping the current time and date as determined by the system clock into a state, but other implementations are possible. For example, a white-noise generator or a radioactive source can be used to generate time-dependent states. 

```ada
procedure Save  (Gen        : in  Generator;
                 To_State   : out State);
procedure Reset (Gen        : in  Generator;
                 From_State : in  State);

```

Save obtains the current state of a generator. Reset gives a generator the specified state. A generator that is reset to a state previously obtained by invoking Save is restored to the state it had when Save was invoked.

```ada
function Image (Of_State    : State)  return String;
function Value (Coded_State : String) return State;

```

Image provides a representation of a state coded (in an implementation-defined way) as a string whose length is bounded by the value of Max_Image_Width. Value is the inverse of Image: Value(Image(S)) = S for each state S that can be obtained from a generator by invoking Save. 

Implementation defined: The string representation of a random number generator's state.


#### Dynamic Semantics

Instantiation of Numerics.Discrete_Random with a subtype having a null range raises Constraint_Error.

Invoking Value with a string that is not the image of any generator state raises Constraint_Error. 


#### Implementation Requirements

A sufficiently long sequence of random numbers obtained by successive calls to Random is approximately uniformly distributed over the range of the result subtype.

The Random function in an instantiation of Numerics.Discrete_Random is guaranteed to yield each value in its result subtype in a finite number of calls, provided that the number of such values does not exceed 215.

Other performance requirements for the random number generator, which apply only in implementations conforming to the Numerics Annex, and then only in the "strict" mode defined there (see G.2), are given in G.2.5. 


#### Documentation Requirements

No one algorithm for random number generation is best for all applications. To enable the user to determine the suitability of the random number generators for the intended application, the implementation shall describe the algorithm used and shall give its period, if known exactly, or a lower bound on the period, if the exact period is unknown. Periods that are so long that the periodicity is unobservable in practice can be described in such terms, without giving a numerical bound. 

The implementation also shall document the minimum time interval between calls to the time-dependent Reset procedure that are guaranteed to initiate different sequences, and it shall document the nature of the strings that Value will accept without raising Constraint_Error. 

This paragraph was deleted.Implementation defined: The minimum time interval between calls to the time-dependent Reset procedure that are guaranteed to initiate different random number sequences.


#### Implementation Advice

Any storage associated with an object of type Generator should be reclaimed on exit from the scope of the object. 

Ramification: A level of indirection is implicit in the semantics of the operations, given that they all take parameters of mode in. This implies that the full type of Generator probably should be a controlled type, with appropriate finalization to reclaim any heap-allocated storage. 

If the generator period is sufficiently long in relation to the number of distinct initiator values, then each possible value of Initiator passed to Reset should initiate a sequence of random numbers that does not, in a practical sense, overlap the sequence initiated by any other value. If this is not possible, then the mapping between initiator values and generator states should be a rapidly varying function of the initiator value. 

NOTE 1   If two or more tasks are to share the same generator, then the tasks have to synchronize their access to the generator as for any shared variable (see 9.10).

NOTE 2   Within a given implementation, a repeatable random number sequence can be obtained by relying on the implicit initialization of generators or by explicitly initializing a generator with a repeatable initiator value. Different sequences of random numbers can be obtained from a given generator in different program executions by explicitly initializing the generator to a time-dependent state.

NOTE 3   A given implementation of the Random function in Numerics.Float_Random may or may not be capable of delivering the values 0.0 or 1.0. Portable applications should assume that these values, or values sufficiently close to them to behave indistinguishably from them, can occur. If a sequence of random integers from some fixed range is needed, the application should use the Random function in an appropriate instantiation of Numerics.Discrete_Random, rather than transforming the result of the Random function in Numerics.Float_Random.However, some applications with unusual requirements, such as for a sequence of random integers each drawn from a different range, will find it more convenient to transform the result of the floating point Random function. For M  1, the expression 

```ada
   Integer(Float(M) * Random(G)) mod M

```

transforms the result of Random(G) to an integer uniformly distributed over the range 0 .. M1; it is valid even if Random delivers 0.0 or 1.0. Each value of the result range is possible, provided that M is not too large. Exponentially distributed (floating point) random numbers with mean and standard deviation 1.0 can be obtained by the transformation 

```ada
   -Log(Random(G) + Float'Model_Small))

```

where Log comes from Numerics.Elementary_Functions (see A.5.1); in this expression, the addition of Float'Model_Small avoids the exception that would be raised were Log to be given the value zero, without affecting the result (in most implementations) when Random returns a nonzero value. 


#### Examples

Example of a program that plays a simulated dice game: 

```ada
with Ada.Numerics.Discrete_Random;
procedure Dice_Game is
   subtype Die is Integer range 1 .. 6;
   subtype Dice is Integer range 2*Die'First .. 2*Die'Last;
   package Random_Die is new Ada.Numerics.Discrete_Random (Die);
   use Random_Die;
   G : Generator;
   D : Dice;
begin
   Reset (G);  -- Start the generator in a unique state in each run
   loop
      -- Roll a pair of dice; sum and process the results
      D := Random(G) + Random(G);
      ...
   end loop;
end Dice_Game;

```

Example of a program that simulates coin tosses: 

```ada
with Ada.Numerics.Discrete_Random;
procedure Flip_A_Coin is
   type Coin is (Heads, Tails);
   package Random_Coin is new Ada.Numerics.Discrete_Random (Coin);
   use Random_Coin;
   G : Generator;
begin
   Reset (G);  -- Start the generator in a unique state in each run
   loop
      -- Toss a coin and process the result
      case Random(G) is
          when Heads =&gt
             ...
          when Tails =&gt
             ...
      end case;
   ...
   end loop;
end Flip_A_Coin;

```

Example of a parallel simulation of a physical system, with a separate generator of event probabilities in each task: 

```ada
with Ada.Numerics.Float_Random;
procedure Parallel_Simulation is
   use Ada.Numerics.Float_Random;
   task type Worker is
      entry Initialize_Generator (Initiator : in Integer);
      ...
   end Worker;
   W : array (1 .. 10) of Worker;
   task body Worker is
      G : Generator;
      Probability_Of_Event : Uniformly_Distributed;
   begin
      accept Initialize_Generator (Initiator : in Integer) do
         Reset (G, Initiator);
      end Initialize_Generator;
      loop
         ...
         Probability_Of_Event := Random(G);
         ...
      end loop;
   end Worker;
begin
   -- Initialize the generators in the Worker tasks to different states
   for I in W'Range loop
      W(I).Initialize_Generator (I);
   end loop;
   ... -- Wait for the Worker tasks to terminate
end Parallel_Simulation;

```

NOTE 4   Notes on the last example: Although each Worker task initializes its generator to a different state, those states will be the same in every execution of the program. The generator states can be initialized uniquely in each program execution by instantiating Ada.Numerics.Discrete_Random for the type Integer in the main procedure, resetting the generator obtained from that instance to a time-dependent state, and then using random integers obtained from that generator to initialize the generators in each Worker task. 


### A.5.3  Attributes of Floating Point Types


#### Static Semantics

The following representation-oriented attributes are defined for every subtype S of a floating point type T. 

S'Machine_RadixYields the radix of the hardware representation of the type T. The value of this attribute is of the type universal_integer. 

The values of other representation-oriented attributes of a floating point subtype, and of the "primitive function" attributes of a floating point subtype described later, are defined in terms of a particular representation of nonzero values called the canonical form. The canonical form (for the type T) is the form
    ± mantissa · T'Machine_Radixexponent
where 

mantissa is a fraction in the number base T'Machine_Radix, the first digit of which is nonzero, and

exponent is an integer. 

S'Machine_MantissaYields the largest value of p such that every value expressible in the canonical form (for the type T), having a p-digit mantissa and an exponent between T'Machine_Emin and T'Machine_Emax, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer. 

Ramification: Values of a type held in an extended register are, in general, not machine numbers of the type, since they cannot be expressed in the canonical form with a sufficiently short mantissa. 

S'Machine_EminYields the smallest (most negative) value of exponent such that every value expressible in the canonical form (for the type T), having a mantissa of T'Machine_Mantissa digits, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer.

S'Machine_EmaxYields the largest (most positive) value of exponent such that every value expressible in the canonical form (for the type T), having a mantissa of T'Machine_Mantissa digits, is a machine number (see 3.5.7) of the type T. This attribute yields a value of the type universal_integer. 

Ramification: Note that the above definitions do not determine unique values for the representation-oriented attributes of floating point types. The implementation may choose any set of values that collectively satisfies the definitions.

S'DenormYields the value True if every value expressible in the form
    ± mantissa · T'Machine_RadixT'Machine_Emin
where mantissa is a nonzero T'Machine_Mantissa-digit fraction in the number base T'Machine_Radix, the first digit of which is zero, is a machine number (see 3.5.7) of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. 

The values described by the formula in the definition of S'Denorm are called denormalized numbers. A nonzero machine number that is not a denormalized number is a normalized number. A normalized number x of a given type T is said to be represented in canonical form when it is expressed in the canonical form (for the type T) with a mantissa having T'Machine_Mantissa digits; the resulting form is the canonical-form representation of x. 

Discussion: The intent is that S'Denorm be True when such denormalized numbers exist and are generated in the circumstances defined by IEC 559:1989, though the latter requirement is not formalized here. 

S'Machine_RoundsYields the value True if rounding is performed on inexact results of every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. 

Discussion: It is difficult to be more precise about what it means to round the result of a predefined operation. If the implementation does not use extended registers, so that every arithmetic result is necessarily a machine number, then rounding seems to imply two things: 

S'Model_Mantissa = S'Machine_Mantissa, so that operand preperturbation never occurs;

when the exact mathematical result is not a machine number, the result of a predefined operation must be the nearer of the two adjacent machine numbers. 

Technically, this attribute should yield False when extended registers are used, since a few computed results will cross over the half-way point as a result of double rounding, if and when a value held in an extended register has to be reduced in precision to that of the machine numbers. It does not seem desirable to preclude the use of extended registers when S'Machine_Rounds could otherwise be True. 

S'Machine_OverflowsYields the value True if overflow and divide-by-zero are detected and reported by raising Constraint_Error for every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean.

S'Signed_ZerosYields the value True if the hardware representation for the type T has the capability of representing both positively and negatively signed zeros, these being generated and used by the predefined operations of the type T as specified in IEC 559:1989; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. 

For every value x of a floating point type T, the normalized exponent of x is defined as follows: 

the normalized exponent of zero is (by convention) zero;

for nonzero x, the normalized exponent of x is the unique integer k such that T'Machine_Radixk1  |x| &lt T'Machine_Radixk. 

Ramification: The normalized exponent of a normalized number x is the value of exponent in the canonical-form representation of x.

The normalized exponent of a denormalized number is less than the value of T'Machine_Emin. 

The following primitive function attributes are defined for any subtype S of a floating point type T. 

S'ExponentS'Exponent denotes a function with the following specification: 

```ada
function S'Exponent (X : T)
  return universal_integer

```

The function yields the normalized exponent of X.

S'FractionS'Fraction denotes a function with the following specification: 

```ada
function S'Fraction (X : T)
  return T

```

The function yields the value X · T'Machine_Radixk, where k is the normalized exponent of X. A zero result[, which can only occur when X is zero,] has the sign of X. 

Discussion: Informally, when X is a normalized number, the result is the value obtained by replacing the exponent by zero in the canonical-form representation of X. 

Ramification: Except when X is zero, the magnitude of the result is greater than or equal to the reciprocal of T'Machine_Radix and less than one; consequently, the result is always a normalized number, even when X is a denormalized number. 

Implementation Note: When X is a denormalized number, the result is the value obtained by replacing the exponent by zero in the canonical-form representation of the result of scaling X up sufficiently to normalize it. 

S'ComposeS'Compose denotes a function with the following specification: 

```ada
function S'Compose (Fraction : T;
                    Exponent : universal_integer)
  return T

```

Let v be the value Fraction · T'Machine_RadixExponentk, where k is the normalized exponent of Fraction. If v is a machine number of the type T, or if |v|  T'Model_Small, the function yields v; otherwise, it yields either one of the machine numbers of the type T adjacent to v. Constraint_Error is optionally raised if v is outside the base range of S. A zero result has the sign of Fraction when S'Signed_Zeros is True. 

Discussion: Informally, when Fraction and v are both normalized numbers, the result is the value obtained by replacing the exponent by Exponent in the canonical-form representation of Fraction. 

Ramification: If Exponent is less than T'Machine_Emin and Fraction is nonzero, the result is either zero, T'Model_Small, or (if T'Denorm is True) a denormalized number. 

S'ScalingS'Scaling denotes a function with the following specification: 

```ada
function S'Scaling (X : T;
                    Adjustment : universal_integer)
  return T

```

Let v be the value X · T'Machine_RadixAdjustment. If v is a machine number of the type T, or if |v|  T'Model_Small, the function yields v; otherwise, it yields either one of the machine numbers of the type T adjacent to v. Constraint_Error is optionally raised if v is outside the base range of S. A zero result has the sign of X when S'Signed_Zeros is True. 

Discussion: Informally, when X and v are both normalized numbers, the result is the value obtained by increasing the exponent by Adjustment in the canonical-form representation of X. 

Ramification: If Adjustment is sufficiently small (i.e., sufficiently negative), the result is either zero, T'Model_Small, or (if T'Denorm is True) a denormalized number. 

S'FloorS'Floor denotes a function with the following specification: 

```ada
function S'Floor (X : T)
  return T

```

The function yields the value X, i.e., the largest (most positive) integral value less than or equal to X. When X is zero, the result has the sign of X; a zero result otherwise has a positive sign.

S'CeilingS'Ceiling denotes a function with the following specification: 

```ada
function S'Ceiling (X : T)
  return T

```

The function yields the value X, i.e., the smallest (most negative) integral value greater than or equal to X. When X is zero, the result has the sign of X; a zero result otherwise has a negative sign when S'Signed_Zeros is True.

S'RoundingS'Rounding denotes a function with the following specification: 

```ada
function S'Rounding (X : T)
  return T

```

The function yields the integral value nearest to X, rounding away from zero if X lies exactly halfway between two integers. A zero result has the sign of X when S'Signed_Zeros is True.

S'Unbiased_RoundingS'Unbiased_Rounding denotes a function with the following specification: 

```ada
function S'Unbiased_Rounding (X : T)
  return T

```

The function yields the integral value nearest to X, rounding toward the even integer if X lies exactly halfway between two integers. A zero result has the sign of X when S'Signed_Zeros is True.



S'TruncationS'Truncation denotes a function with the following specification: 

```ada
function S'Truncation (X : T)
  return T

```

The function yields the value X when X is negative, and X otherwise. A zero result has the sign of X when S'Signed_Zeros is True.

S'RemainderS'Remainder denotes a function with the following specification: 

```ada
function S'Remainder (X, Y : T)
  return T

```

For nonzero Y, let v be the value X  n · Y, where n is the integer nearest to the exact value of X/Y; if |n  X/Y| = 1/2, then n is chosen to be even. If v is a machine number of the type T, the function yields v; otherwise, it yields zero. Constraint_Error is raised if Y is zero. A zero result has the sign of X when S'Signed_Zeros is True. 

Ramification: The magnitude of the result is less than or equal to one-half the magnitude of Y. 

Discussion: Given machine numbers X and Y of the type T, v is necessarily a machine number of the type T, except when Y is in the neighborhood of zero, X is sufficiently close to a multiple of Y, and T'Denorm is False. 

S'AdjacentS'Adjacent denotes a function with the following specification: 

```ada
function S'Adjacent (X, Towards : T)
  return T

```

If Towards = X, the function yields X; otherwise, it yields the machine number of the type T adjacent to X in the direction of Towards, if that machine number exists. If the result would be outside the base range of S, Constraint_Error is raised. When T'Signed_Zeros is True, a zero result has the sign of X. When Towards is zero, its sign has no bearing on the result. 

Ramification: The value of S'Adjacent(0.0, 1.0) is the smallest normalized positive number of the type T when T'Denorm is False and the smallest denormalized positive number of the type T when T'Denorm is True. 

S'Copy_SignS'Copy_Sign denotes a function with the following specification: 

```ada
function S'Copy_Sign (Value, Sign : T)
  return T

```

If the value of Value is nonzero, the function yields a result whose magnitude is that of Value and whose sign is that of Sign; otherwise, it yields the value zero. Constraint_Error is optionally raised if the result is outside the base range of S. A zero result has the sign of Sign when S'Signed_Zeros is True. 

Discussion: S'Copy_Sign is provided for convenience in restoring the sign to a quantity from which it has been temporarily removed, or to a related quantity. When S'Signed_Zeros is True, it is also instrumental in determining the sign of a zero quantity, when required. (Because negative and positive zeros compare equal in systems conforming to IEC 559:1989, a negative zero does not appear to be negative when compared to zero.) The sign determination is accomplished by transferring the sign of the zero quantity to a nonzero quantity and then testing for a negative result. 

S'Leading_PartS'Leading_Part denotes a function with the following specification: 

```ada
function S'Leading_Part (X : T;
                         Radix_Digits : universal_integer)
  return T

```

Let v be the value T'Machine_RadixkRadix_Digits, where k is the normalized exponent of X. The function yields the value 

X/v · v, when X is nonnegative and Radix_Digits is positive;

X/v · v, when X is negative and Radix_Digits is positive. 

Constraint_Error is raised when Radix_Digits is zero or negative. A zero result[, which can only occur when X is zero,] has the sign of X. 

Discussion: Informally, if X is nonzero, the result is the value obtained by retaining only the specified number of (leading) significant digits of X (in the machine radix), setting all other digits to zero. 

Implementation Note: The result can be obtained by first scaling X up, if necessary to normalize it, then masking the mantissa so as to retain only the specified number of leading digits, then scaling the result back down if X was scaled up. 

S'MachineS'Machine denotes a function with the following specification: 

```ada
function S'Machine (X : T)
  return T

```

If X is a machine number of the type T, the function yields X; otherwise, it yields the value obtained by rounding or truncating X to either one of the adjacent machine numbers of the type T. Constraint_Error is raised if rounding or truncating X to the precision of the machine numbers results in a value outside the base range of S. A zero result has the sign of X when S'Signed_Zeros is True. 

Discussion: All of the primitive function attributes except Rounding and Machine correspond to subprograms in the Generic_Primitive_Functions generic package proposed as a separate ISO standard (ISO/IEC DIS 11729) for Ada 83. The Scaling, Unbiased_Rounding, and Truncation attributes correspond to the Scale, Round, and Truncate functions, respectively, in Generic_Primitive_Functions. The Rounding attribute rounds away from zero; this functionality was not provided in Generic_Primitive_Functions. The name Round was not available for either of the primitive function attributes that perform rounding, since an attribute of that name is used for a different purpose for decimal fixed point types. Likewise, the name Scale was not available, since an attribute of that name is also used for a different purpose for decimal fixed point types. The functionality of the Machine attribute was also not provided in Generic_Primitive_Functions. The functionality of the Decompose procedure of Generic_Primitive_Functions is only provided in the form of the separate attributes Exponent and Fraction. The functionality of the Successor and Predecessor functions of Generic_Primitive_Functions is provided by the extension of the existing Succ and Pred attributes. 

Implementation Note: The primitive function attributes may be implemented either with appropriate floating point arithmetic operations or with integer and logical operations that act on parts of the representation directly. The latter is strongly encouraged when it is more efficient than the former; it is mandatory when the former cannot deliver the required accuracy due to limitations of the implementation's arithmetic operations. 

The following model-oriented attributes are defined for any subtype S of a floating point type T. 

S'Model_MantissaIf the Numerics Annex is not supported, this attribute yields an implementation defined value that is greater than or equal to d · log(10) / log(T'Machine_Radix) + 1, where d is the requested decimal precision of T, and less than or equal to the value of T'Machine_Mantissa. See G.2.2 for further requirements that apply to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_integer.

S'Model_EminIf the Numerics Annex is not supported, this attribute yields an implementation defined value that is greater than or equal to the value of T'Machine_Emin. See G.2.2 for further requirements that apply to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_integer.

S'Model_EpsilonYields the value T'Machine_Radix1  T'Model_Mantissa. The value of this attribute is of the type universal_real. 

Discussion: In most implementations, this attribute yields the absolute value of the difference between one and the smallest machine number of the type T above one which, when added to one, yields a machine number different from one. Further discussion can be found in G.2.2. 

S'Model_SmallYields the value T'Machine_RadixT'Model_Emin  1. The value of this attribute is of the type universal_real. 

Discussion: In most implementations, this attribute yields the smallest positive normalized number of the type T, i.e. the number corresponding to the positive underflow threshold. In some implementations employing a radix-complement representation for the type T, the positive underflow threshold is closer to zero than is the negative underflow threshold, with the consequence that the smallest positive normalized number does not coincide with the positive underflow threshold (i.e., it exceeds the latter). Further discussion can be found in G.2.2. 

S'ModelS'Model denotes a function with the following specification: 

```ada
function S'Model (X : T)
  return T

```

If the Numerics Annex is not supported, the meaning of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex.

S'Safe_FirstYields the lower bound of the safe range (see 3.5.7) of the type T. If the Numerics Annex is not supported, the value of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_real.

S'Safe_LastYields the upper bound of the safe range (see 3.5.7) of the type T. If the Numerics Annex is not supported, the value of this attribute is implementation defined; see G.2.2 for the definition that applies to implementations supporting the Numerics Annex. The value of this attribute is of the type universal_real. 

Discussion: A predefined floating point arithmetic operation that yields a value in the safe range of its result type is guaranteed not to overflow. 

To be honest: An exception is made for exponentiation by a negative exponent in 4.5.6.

Implementation defined: The values of the Model_Mantissa, Model_Emin, Model_Epsilon, Model, Safe_First, and Safe_Last attributes, if the Numerics Annex is not supported.


#### Incompatibilities With Ada 83

The Epsilon and Mantissa attributes of floating point types are removed from the language and replaced by Model_Epsilon and Model_Mantissa, which may have different values (as a result of changes in the definition of model numbers); the replacement of one set of attributes by another is intended to convert what would be an inconsistent change into an incompatible change.

The Emax, Small, Large, Safe_Emax, Safe_Small, and Safe_Large attributes of floating point types are removed from the language. Small and Safe_Small are collectively replaced by Model_Small, which is functionally equivalent to Safe_Small, though it may have a slightly different value. The others are collectively replaced by Safe_First and Safe_Last. Safe_Last is functionally equivalent to Safe_Large, though it may have a different value; Safe_First is comparable to the negation of Safe_Large but may differ slightly from it as well as from the negation of Safe_Last. Emax and Safe_Emax had relatively few uses in Ada 83; T'Safe_Emax can be computed in the revised language as Integer'Min(T'Exponent(T'Safe_First), T'Exponent(T'Safe_Last)).

Implementations are encouraged to eliminate the incompatibilities discussed here by retaining the old attributes, during a transition period, in the form of implementation-defined attributes with their former values. 


#### Extensions to Ada 83

The Model_Emin attribute is new. It is conceptually similar to the negation of Safe_Emax attribute of Ada 83, adjusted for the fact that the model numbers now have the hardware radix. It is a fundamental determinant, along with Model_Mantissa, of the set of model numbers of a type (see G.2.1).

The Denorm and Signed_Zeros attributes are new, as are all of the primitive function attributes. 


### A.5.4  Attributes of Fixed Point Types


#### Static Semantics

The following representation-oriented attributes are defined for every subtype S of a fixed point type T. 

S'Machine_RadixYields the radix of the hardware representation of the type T. The value of this attribute is of the type universal_integer.

S'Machine_RoundsYields the value True if rounding is performed on inexact results of every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean.

S'Machine_OverflowsYields the value True if overflow and divide-by-zero are detected and reported by raising Constraint_Error for every predefined operation that yields a result of the type T; yields the value False otherwise. The value of this attribute is of the predefined type Boolean. 


#### Incompatibilities With Ada 83

The Mantissa, Large, Safe_Small, and Safe_Large attributes of fixed point types are removed from the language.

Implementations are encouraged to eliminate the resulting incompatibility by retaining these attributes, during a transition period, in the form of implementation-defined attributes with their former values. 


#### Extensions to Ada 83

The Machine_Radix attribute is now allowed for fixed point types. It is also specifiable in an attribute definition clause (see F.1). 


## A.6  Input-Output

[ Input-output is provided through language-defined packages, each of which is a child of the root package Ada. The generic packages Sequential_IO and Direct_IO define input-output operations applicable to files containing elements of a given type. The generic package Storage_IO supports reading from and writing to an in-memory buffer. Additional operations for text input-output are supplied in the packages Text_IO and Wide_Text_IO. Heterogeneous input-output is provided through the child packages Streams.Stream_IO and Text_IO.Text_Streams (see also 13.13). The package IO_Exceptions defines the exceptions needed by the predefined input-output packages.] 


#### Inconsistencies With Ada 83

The introduction of Append_File as a new element of the enumeration type File_Mode in Sequential_IO and Text_IO, and the introduction of several new declarations in Text_IO, may result in name clashes in the presence of use clauses. 


#### Extensions to Ada 83

Text_IO enhancements (Get_Immediate, Look_Ahead, Standard_Error, Modular_IO, Decimal_IO), Wide_Text_IO, and the stream input-output facilities are new in Ada 95. 


#### Wording Changes from Ada 83

RM83-14.6, "Low Level Input-Output", is removed. This has no semantic effect, since the package was entirely implementation defined, nobody actually implemented it, and if they did, they can always provide it as a vendor-supplied package. 


## A.7  External Files and File Objects


#### Static Semantics

Values input from the external environment of the program, or output to the external environment, are considered to occupy external files. An external file can be anything external to the program that can produce a value to be read or receive a value to be written. An external file is identified by a string (the name). A second string (the form) gives further system-dependent characteristics that may be associated with the file, such as the physical organization or access rights. The conventions governing the interpretation of such strings shall be documented.

Input and output operations are expressed as operations on objects of some file type, rather than directly in terms of the external files. In the remainder of this section, the term file is always used to refer to a file object; the term external file is used otherwise.

Input-output for sequential files of values of a single element type is defined by means of the generic package Sequential_IO. In order to define sequential input-output for a given element type, an instantiation of this generic unit, with the given type as actual parameter, has to be declared. The resulting package contains the declaration of a file type (called File_Type) for files of such elements, as well as the operations applicable to these files, such as the Open, Read, and Write procedures.

Input-output for direct access files is likewise defined by a generic package called Direct_IO. Input-output in human-readable form is defined by the (nongeneric) packages Text_IO for Character and String data, and Wide_Text_IO for Wide_Character and Wide_String data. Input-output for files containing streams of elements representing values of possibly different types is defined by means of the (nongeneric) package Streams.Stream_IO.

Before input or output operations can be performed on a file, the file first has to be associated with an external file. While such an association is in effect, the file is said to be open, and otherwise the file is said to be closed.

The language does not define what happens to external files after the completion of the main program and all the library tasks (in particular, if corresponding files have not been closed). The effect of input-output for access types is unspecified.

An open file has a current mode, which is a value of one of the following enumeration types: 

```ada
type File_Mode is (In_File, Inout_File, Out_File);  --  for Direct_IO

```

These values correspond respectively to the cases where only reading, both reading and writing, or only writing are to be performed. 

```ada
type File_Mode is (In_File, Out_File, Append_File);
--  for Sequential_IO, Text_IO, Wide_Text_IO, and Stream_IO

```

These values correspond respectively to the cases where only reading, only writing, or only appending are to be performed.

The mode of a file can be changed. 

Several file management operations are common to Sequential_IO, Direct_IO, Text_IO, and Wide_Text_IO. These operations are described in subclause A.8.2 for sequential and direct files. Any additional effects concerning text input-output are described in subclause A.10.2.

The exceptions that can be propagated by the execution of an input-output subprogram are defined in the package IO_Exceptions; the situations in which they can be propagated are described following the description of the subprogram (and in clause A.13). The exceptions Storage_Error and Program_Error may be propagated. (Program_Error can only be propagated due to errors made by the caller of the subprogram.) Finally, exceptions can be propagated in certain implementation-defined situations. 

This paragraph was deleted.Implementation defined: Any implementation-defined characteristics of the input-output packages.

NOTE 1   Each instantiation of the generic packages Sequential_IO and Direct_IO declares a different type File_Type. In the case of Text_IO, Wide_Text_IO, and Streams.Stream_IO, the corresponding type File_Type is unique.

NOTE 2   A bidirectional device can often be modeled as two sequential files associated with the device, one of mode In_File, and one of mode Out_File. An implementation may restrict the number of files that may be associated with a given external file. 


## A.8  Sequential and Direct Files


#### Static Semantics

Two kinds of access to external files are defined in this subclause: sequential access and direct access. The corresponding file types and the associated operations are provided by the generic packages Sequential_IO and Direct_IO. A file object to be used for sequential access is called a sequential file, and one to be used for direct access is called a direct file. Access to stream files is described in A.12.1.

For sequential access, the file is viewed as a sequence of values that are transferred in the order of their appearance (as produced by the program or by the external environment). When the file is opened with mode In_File or Out_File, transfer starts respectively from or to the beginning of the file. When the file is opened with mode Append_File, transfer to the file starts after the last element of the file. 

Discussion: Adding stream I/O necessitates a review of the terminology. In Ada 83, `sequential' implies both the access method (purely sequential - that is, no indexing or positional access) and homogeneity. Direct access includes purely sequential access and indexed access, as well as homogeneity. In Ada 95, streams allow purely sequential access but also positional access to an individual element, and are heterogeneous. We considered generalizing the notion of `sequential file' to include both Sequential_IO and Stream_IO files, but since streams allow positional access it seems misleading to call them sequential files. Or, looked at differently, if the criterion for calling something a sequential file is whether it permits (versus requires) purely sequential access, then one could just as soon regard a Direct_IO file as a sequential file.

It seems better to regard `sequential file' as meaning `only permitting purely sequential access'; hence we have decided to supplement `sequential access' and `direct access' with a third category, informally called `access to streams'. (We decided against the term `stream access' because of possible confusion with the Stream_Access type declared in one of the stream packages.)

For direct access, the file is viewed as a set of elements occupying consecutive positions in linear order; a value can be transferred to or from an element of the file at any selected position. The position of an element is specified by its index, which is a number, greater than zero, of the implementation-defined integer type Count. The first element, if any, has index one; the index of the last element, if any, is called the current size; the current size is zero if there are no elements. The current size is a property of the external file.

An open direct file has a current index, which is the index that will be used by the next read or write operation. When a direct file is opened, the current index is set to one. The current index of a direct file is a property of a file object, not of an external file. 


### A.8.1  The Generic Package Sequential_IO


#### Static Semantics

The generic library package Sequential_IO has the following declaration: 

```ada
with Ada.IO_Exceptions;
generic
   type Element_Type(&lt&gt) is private;
package Ada.Sequential_IO is

```

```ada
   type File_Type is limited private;

```

```ada
   type File_Mode is (In_File, Out_File, Append_File);

```

```ada
   -- File management

```

```ada
   procedure Create(File : in out File_Type;
                    Mode : in File_Mode := Out_File;
                    Name : in String := "";
                    Form : in String := "");

```

```ada
   procedure Open  (File : in out File_Type;
                    Mode : in File_Mode;
                    Name : in String;
                    Form : in String := "");

```

```ada
   procedure Close (File : in out File_Type);
   procedure Delete(File : in out File_Type);
   procedure Reset (File : in out File_Type; Mode : in File_Mode);
   procedure Reset (File : in out File_Type);

```

```ada
   function Mode   (File : in File_Type) return File_Mode;
   function Name   (File : in File_Type) return String;
   function Form   (File : in File_Type) return String;

```

```ada
   function Is_Open(File : in File_Type) return Boolean;

```

```ada
   -- Input and output operations

```

```ada
   procedure Read  (File : in File_Type; Item : out Element_Type);
   procedure Write (File : in File_Type; Item : in Element_Type);

```

```ada
   function End_Of_File(File : in File_Type) return Boolean;

```

```ada
   -- Exceptions

```

```ada
   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error   : exception renames IO_Exceptions.Mode_Error;
   Name_Error   : exception renames IO_Exceptions.Name_Error;
   Use_Error    : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error    : exception renames IO_Exceptions.End_Error;
   Data_Error   : exception renames IO_Exceptions.Data_Error;

```

```ada
private
   ... -- not specified by the language
end Ada.Sequential_IO;

```


#### Incompatibilities With Ada 83

The new enumeration element Append_File may introduce upward incompatibilities. It is possible that a program based on the assumption that File_Mode'Last = Out_File will be illegal (e.g., case statement choice coverage) or execute with a different effect in Ada 95. 


### A.8.2  File Management


#### Static Semantics

The procedures and functions described in this subclause provide for the control of external files; their declarations are repeated in each of the packages for sequential, direct, text, and stream input-output. For text input-output, the procedures Create, Open, and Reset have additional effects described in subclause A.10.2. 

```ada
procedure Create(File : in out File_Type;
                 Mode : in File_Mode := default_mode;
                 Name : in String := "";
                 Form : in String := "");

```

Establishes a new external file, with the given name and form, and associates this external file with the given file. The given file is left open. The current mode of the given file is set to the given access mode. The default access mode is the mode Out_File for sequential and text input-output; it is the mode Inout_File for direct input-output. For direct access, the size of the created file is implementation defined.

A null string for Name specifies an external file that is not accessible after the completion of the main program (a temporary file). A null string for Form specifies the use of the default options of the implementation for the external file.

The exception Status_Error is propagated if the given file is already open. The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file. The exception Use_Error is propagated if, for the specified mode, the external environment does not support creation of an external file with the given name (in the absence of Name_Error) and form.

```ada
procedure Open(File : in out File_Type;
               Mode : in File_Mode;
               Name : in String;
               Form : in String := "");

```

Associates the given file with an existing external file having the given name and form, and sets the current mode of the given file to the given mode. The given file is left open.

The exception Status_Error is propagated if the given file is already open. The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file; in particular, this exception is propagated if no external file with the given name exists. The exception Use_Error is propagated if, for the specified mode, the external environment does not support opening for an external file with the given name (in the absence of Name_Error) and form.

```ada
procedure Close(File : in out File_Type);

```

Severs the association between the given file and its associated external file. The given file is left closed. In addition, for sequential files, if the file being closed has mode Out_File or Append_File, then the last element written since the most recent open or reset is the last element that can be read from the file. If no elements have been written and the file mode is Out_File, then the closed file is empty. If no elements have been written and the file mode is Append_File, then the closed file is unchanged.

The exception Status_Error is propagated if the given file is not open.

```ada
procedure Delete(File : in out File_Type);

```

Deletes the external file associated with the given file. The given file is closed, and the external file ceases to exist.

The exception Status_Error is propagated if the given file is not open. The exception Use_Error is propagated if deletion of the external file is not supported by the external environment.

```ada
procedure Reset(File : in out File_Type; Mode : in File_Mode);
procedure Reset(File : in out File_Type);

```

Resets the given file so that reading from its elements can be restarted from the beginning of the file (for modes In_File and Inout_File), and so that writing to its elements can be restarted at the beginning of the file (for modes Out_File and Inout_File) or after the last element of the file (for mode Append_File). In particular, for direct access this means that the current index is set to one. If a Mode parameter is supplied, the current mode of the given file is set to the given mode. In addition, for sequential files, if the given file has mode Out_File or Append_File when Reset is called, the last element written since the most recent open or reset is the last element that can be read from the file. If no elements have been written and the file mode is Out_File, the reset file is empty. If no elements have been written and the file mode is Append_File, then the reset file is unchanged.

The exception Status_Error is propagated if the file is not open. The exception Use_Error is propagated if the external environment does not support resetting for the external file and, also, if the external environment does not support resetting to the specified mode for the external file.

```ada
function Mode(File : in File_Type) return File_Mode;

```

Returns the current mode of the given file.

The exception Status_Error is propagated if the file is not open.

```ada
function Name(File : in File_Type) return String;

```

Returns a string which uniquely identifies the external file currently associated with the given file (and may thus be used in an Open operation). If an external environment allows alternative specifications of the name (for example, abbreviations), the string returned by the function should correspond to a full specification of the name.

The exception Status_Error is propagated if the given file is not open. The exception Use_Error is propagated if the associated external file is a temporary file that cannot be opened by any name.

```ada
function Form(File : in File_Type) return String;

```

Returns the form string for the external file currently associated with the given file. If an external environment allows alternative specifications of the form (for example, abbreviations using default options), the string returned by the function should correspond to a full specification (that is, it should indicate explicitly all options selected, including default options).

The exception Status_Error is propagated if the given file is not open.

```ada
function Is_Open(File : in File_Type) return Boolean;

```

Returns True if the file is open (that is, if it is associated with an external file), otherwise returns False.


#### Implementation Permissions

An implementation may propagate Name_Error or Use_Error if an attempt is made to use an I/O feature that cannot be supported by the implementation due to limitations in the external environment. Any such restriction should be documented. 


### A.8.3  Sequential Input-Output Operations


#### Static Semantics

The operations available for sequential input and output are described in this subclause. The exception Status_Error is propagated if any of these operations is attempted for a file that is not open. 

```ada
procedure Read(File : in File_Type; Item : out Element_Type);

```

Operates on a file of mode In_File. Reads an element from the given file, and returns the value of this element in the Item parameter. 

Discussion: We considered basing Sequential_IO.Read on Element_Type'Read from an implicit stream associated with the sequential file. However, Element_Type'Read is a type-related attribute, whereas Sequential_IO should take advantage of the particular constraints of the actual subtype corresponding to Element_Type to minimize the size of the external file. Furthermore, forcing the implementation of Sequential_IO to be based on Element_Type'Read would create an upward incompatibility since existing data files written by an Ada 83 program using Sequential_IO might not be readable by the identical program built with an Ada 95 implementation of Sequential_IO.

An Ada 95 implementation might still use an implementation-defined attribute analogous to 'Read to implement the procedure Read, but that attribute will likely have to be subtype-specific rather than type-related, and it need not be user-specifiable. Such an attribute will presumably be needed to implement the generic package Storage_IO (see A.9). 

The exception Mode_Error is propagated if the mode is not In_File. The exception End_Error is propagated if no more elements can be read from the given file. The exception Data_Error can be propagated if the element read cannot be interpreted as a value of the subtype Element_Type (see A.13, "Exceptions in Input-Output"). 

Discussion: Data_Error need not be propagated if the check is too complex. See A.13, "Exceptions in Input-Output". 

```ada
procedure Write(File : in File_Type; Item : in Element_Type);

```

Operates on a file of mode Out_File or Append_File. Writes the value of Item to the given file.

The exception Mode_Error is propagated if the mode is not Out_File or Append_File. The exception Use_Error is propagated if the capacity of the external file is exceeded.

```ada
function End_Of_File(File : in File_Type) return Boolean;

```

Operates on a file of mode In_File. Returns True if no more elements can be read from the given file; otherwise returns False.

The exception Mode_Error is propagated if the mode is not In_File. 


### A.8.4  The Generic Package Direct_IO


#### Static Semantics

The generic library package Direct_IO has the following declaration: 

```ada
with Ada.IO_Exceptions;
generic
   type Element_Type is private;
package Ada.Direct_IO is

```

```ada
   type File_Type is limited private;

```

```ada
   type File_Mode is (In_File, Inout_File, Out_File);
   type Count     is range 0 .. implementation-defined;
   subtype Positive_Count is Count range 1 .. Count'Last;

```

```ada
   -- File management

```

```ada
   procedure Create(File : in out File_Type;
                    Mode : in File_Mode := Inout_File;
                    Name : in String := "";
                    Form : in String := "");

```

```ada
   procedure Open  (File : in out File_Type;
                    Mode : in File_Mode;
                    Name : in String;
                    Form : in String := "");

```

```ada
   procedure Close (File : in out File_Type);
   procedure Delete(File : in out File_Type);
   procedure Reset (File : in out File_Type; Mode : in File_Mode);
   procedure Reset (File : in out File_Type);

```

```ada
   function Mode   (File : in File_Type) return File_Mode;
   function Name   (File : in File_Type) return String;
   function Form   (File : in File_Type) return String;

```

```ada
   function Is_Open(File : in File_Type) return Boolean;

```

```ada
   -- Input and output operations

```

```ada
   procedure Read (File : in File_Type; Item : out Element_Type;
                                        From : in Positive_Count);
   procedure Read (File : in File_Type; Item : out Element_Type);

```

```ada
   procedure Write(File : in File_Type; Item : in  Element_Type;
                                        To   : in Positive_Count);
   procedure Write(File : in File_Type; Item : in Element_Type);

```

```ada
   procedure Set_Index(File : in File_Type; To : in Positive_Count);

```

```ada
   function Index(File : in File_Type) return Positive_Count;
   function Size (File : in File_Type) return Count;

```

```ada
   function End_Of_File(File : in File_Type) return Boolean;

```

```ada
   -- Exceptions

```

```ada
   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error   : exception renames IO_Exceptions.Mode_Error;
   Name_Error   : exception renames IO_Exceptions.Name_Error;
   Use_Error    : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error    : exception renames IO_Exceptions.End_Error;
   Data_Error   : exception renames IO_Exceptions.Data_Error;

```

```ada
private
   ... -- not specified by the language
end Ada.Direct_IO;

```

Reason: The Element_Type formal of Direct_IO does not have an [unknown_discriminant_part](S0057) (unlike Sequential_IO) so that the implementation can make use of the ability to declare uninitialized variables of the type. 


### A.8.5  Direct Input-Output Operations


#### Static Semantics

The operations available for direct input and output are described in this subclause. The exception Status_Error is propagated if any of these operations is attempted for a file that is not open. 

```ada
procedure Read(File : in File_Type; Item : out Element_Type;
                                    From : in  Positive_Count);
procedure Read(File : in File_Type; Item : out Element_Type);

```

Operates on a file of mode In_File or Inout_File. In the case of the first form, sets the current index of the given file to the index value given by the parameter From. Then (for both forms) returns, in the parameter Item, the value of the element whose position in the given file is specified by the current index of the file; finally, increases the current index by one.

The exception Mode_Error is propagated if the mode of the given file is Out_File. The exception End_Error is propagated if the index to be used exceeds the size of the external file. The exception Data_Error can be propagated if the element read cannot be interpreted as a value of the subtype Element_Type (see A.13).

```ada
procedure Write(File : in File_Type; Item : in Element_Type;
                                     To   : in Positive_Count);
procedure Write(File : in File_Type; Item : in Element_Type);

```

Operates on a file of mode Inout_File or Out_File. In the case of the first form, sets the index of the given file to the index value given by the parameter To. Then (for both forms) gives the value of the parameter Item to the element whose position in the given file is specified by the current index of the file; finally, increases the current index by one.

The exception Mode_Error is propagated if the mode of the given file is In_File. The exception Use_Error is propagated if the capacity of the external file is exceeded.

```ada
procedure Set_Index(File : in File_Type; To : in Positive_Count);

```

Operates on a file of any mode. Sets the current index of the given file to the given index value (which may exceed the current size of the file).

```ada
function Index(File : in File_Type) return Positive_Count;

```

Operates on a file of any mode. Returns the current index of the given file.

```ada
function Size(File : in File_Type) return Count;

```

Operates on a file of any mode. Returns the current size of the external file that is associated with the given file.

```ada
function End_Of_File(File : in File_Type) return Boolean;

```

Operates on a file of mode In_File or Inout_File. Returns True if the current index exceeds the size of the external file; otherwise returns False.

The exception Mode_Error is propagated if the mode of the given file is Out_File.

NOTE 1   Append_File mode is not supported for the generic package Direct_IO. 


## A.9  The Generic Package Storage_IO

The generic package Storage_IO provides for reading from and writing to an in-memory buffer. This generic package supports the construction of user-defined input-output packages. 

Reason: This package exists to allow the portable construction of user-defined direct-access-oriented input-output packages. The Write procedure writes a value of type Element_Type into a Storage_Array of size Buffer_Size, flattening out any implicit levels of indirection used in the representation of the type. The Read procedure reads a value of type Element_Type from the buffer, reconstructing any implicit levels of indirection used in the representation of the type. It also properly initializes any type tags that appear within the value, presuming that the buffer was written by a different program and that tag values for the"same" type might vary from one executable to another. 


#### Static Semantics

The generic library package Storage_IO has the following declaration: 

```ada
with Ada.IO_Exceptions;
with System.Storage_Elements;
generic
   type Element_Type is private;
package Ada.Storage_IO is
   pragma Preelaborate(Storage_IO);

```

```ada
   Buffer_Size : constant System.Storage_Elements.Storage_Count :=
      implementation-defined;
   subtype Buffer_Type is
      System.Storage_Elements.Storage_Array(1..Buffer_Size);

```

```ada
   -- Input and output operations

```

```ada
   procedure Read (Buffer : in  Buffer_Type; Item : out Element_Type);

```

```ada
   procedure Write(Buffer : out Buffer_Type; Item : in  Element_Type);

```

```ada
   -- Exceptions

```

```ada
   Data_Error   : exception renames IO_Exceptions.Data_Error;
end Ada.Storage_IO;

```

In each instance, the constant Buffer_Size has a value that is the size (in storage elements) of the buffer required to represent the content of an object of subtype Element_Type, including any implicit levels of indirection used by the implementation. The Read and Write procedures of Storage_IO correspond to the Read and Write procedures of Direct_IO (see A.8.4), but with the content of the Item parameter being read from or written into the specified Buffer, rather than an external file.

Reason: As with Direct_IO, the Element_Type formal of Storage_IO does not have an [unknown_discriminant_part](S0057) so that there is a well-defined upper bound on the size of the buffer needed to hold the content of an object of the formal subtype (i.e. Buffer_Size). If there are no implicit levels of indirection, Buffer_Size will typically equal: 

```ada
(Element_Type'Size + System.Storage_Unit - 1) / System.Storage_Unit

```

Implementation defined: The value of Buffer_Size in Storage_IO.

NOTE 1   A buffer used for Storage_IO holds only one element at a time; an external file used for Direct_IO holds a sequence of elements. 


## A.10  Text Input-Output


#### Static Semantics

This clause describes the package Text_IO, which provides facilities for input and output in human-readable form. Each file is read or written sequentially, as a sequence of characters grouped into lines, and as a sequence of lines grouped into pages. The specification of the package is given below in subclause A.10.1.

The facilities for file management given above, in subclauses A.8.2 and A.8.3, are available for text input-output. In place of Read and Write, however, there are procedures Get and Put that input values of suitable types from text files, and output values to them. These values are provided to the Put procedures, and returned by the Get procedures, in a parameter Item. Several overloaded procedures of these names exist, for different types of Item. These Get procedures analyze the input sequences of characters based on lexical elements (see Section 2) and return the corresponding values; the Put procedures output the given values as appropriate lexical elements. Procedures Get and Put are also available that input and output individual characters treated as character values rather than as lexical elements. Related to character input are procedures to look ahead at the next character without reading it, and to read a character "immediately" without waiting for an end-of-line to signal availability.

In addition to the procedures Get and Put for numeric and enumeration types of Item that operate on text files, analogous procedures are provided that read from and write to a parameter of type String. These procedures perform the same analysis and composition of character sequences as their counterparts which have a file parameter.

For all Get and Put procedures that operate on text files, and for many other subprograms, there are forms with and without a file parameter. Each such Get procedure operates on an input file, and each such Put procedure operates on an output file. If no file is specified, a default input file or a default output file is used.

At the beginning of program execution the default input and output files are the so-called standard input file and standard output file. These files are open, have respectively the current modes In_File and Out_File, and are associated with two implementation-defined external files. Procedures are provided to change the current default input file and the current default output file. 

Implementation defined: external files for standard input, standard output, and standard error

At the beginning of program execution a default file for program-dependent error-related text output is the so-called standard error file. This file is open, has the current mode Out_File, and is associated with an implementation-defined external file. A procedure is provided to change the current default error file.

From a logical point of view, a text file is a sequence of pages, a page is a sequence of lines, and a line is a sequence of characters; the end of a line is marked by a line terminator; the end of a page is marked by the combination of a line terminator immediately followed by a page terminator; and the end of a file is marked by the combination of a line terminator immediately followed by a page terminator and then a file terminator. Terminators are generated during output; either by calls of procedures provided expressly for that purpose; or implicitly as part of other operations, for example, when a bounded line length, a bounded page length, or both, have been specified for a file.

The actual nature of terminators is not defined by the language and hence depends on the implementation. Although terminators are recognized or generated by certain of the procedures that follow, they are not necessarily implemented as characters or as sequences of characters. Whether they are characters (and if so which ones) in any particular implementation need not concern a user who neither explicitly outputs nor explicitly inputs control characters. The effect of input (Get) or output (Put) of control characters (other than horizontal tabulation) is not specified by the language. 

The characters of a line are numbered, starting from one; the number of a character is called its column number. For a line terminator, a column number is also defined: it is one more than the number of characters in the line. The lines of a page, and the pages of a file, are similarly numbered. The current column number is the column number of the next character or line terminator to be transferred. The current line number is the number of the current line. The current page number is the number of the current page. These numbers are values of the subtype Positive_Count of the type Count (by convention, the value zero of the type Count is used to indicate special conditions). 

```ada
type Count is range 0 .. implementation-defined;
subtype Positive_Count is Count range 1 .. Count'Last;

```

For an output file or an append file, a maximum line length can be specified and a maximum page length can be specified. If a value to be output cannot fit on the current line, for a specified maximum line length, then a new line is automatically started before the value is output; if, further, this new line cannot fit on the current page, for a specified maximum page length, then a new page is automatically started before the value is output. Functions are provided to determine the maximum line length and the maximum page length. When a file is opened with mode Out_File or Append_File, both values are zero: by convention, this means that the line lengths and page lengths are unbounded. (Consequently, output consists of a single line if the subprograms for explicit control of line and page structure are not used.) The constant Unbounded is provided for this purpose. 


#### Extensions to Ada 83

Append_File is new in Ada 95. 


### A.10.1  The Package Text_IO


#### Static Semantics

The library package Text_IO has the following declaration: 

```ada
with Ada.IO_Exceptions;
package Ada.Text_IO is

```

```ada
   type File_Type is limited private;

```

```ada
   type File_Mode is (In_File, Out_File, Append_File);

```

```ada
   type Count is range 0 .. implementation-defined;
   subtype Positive_Count is Count range 1 .. Count'Last;
   Unbounded : constant Count := 0; -- line and page length

```

```ada
   subtype Field       is Integer range 0 .. implementation-defined;
   subtype Number_Base is Integer range 2 .. 16;

```

```ada
   type Type_Set is (Lower_Case, Upper_Case);

```

```ada
   -- File Management

```

```ada
   procedure Create (File : in out File_Type;
                     Mode : in File_Mode := Out_File;
                     Name : in String    := "";
                     Form : in String    := "");

```

```ada
   procedure Open   (File : in out File_Type;
                     Mode : in File_Mode;
                     Name : in String;
                     Form : in String := "");

```

```ada
   procedure Close  (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset  (File : in out File_Type; Mode : in File_Mode);
   procedure Reset  (File : in out File_Type);

```

```ada
   function  Mode   (File : in File_Type) return File_Mode;
   function  Name   (File : in File_Type) return String;
   function  Form   (File : in File_Type) return String;

```

```ada
   function  Is_Open(File : in File_Type) return Boolean;

```

```ada
   -- Control of default input and output files

```

```ada
   procedure Set_Input (File : in File_Type);
   procedure Set_Output(File : in File_Type);
   procedure Set_Error (File : in File_Type);

```

```ada
   function Standard_Input  return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error  return File_Type;

```

```ada
   function Current_Input   return File_Type;
   function Current_Output  return File_Type;
   function Current_Error   return File_Type;

```

```ada
   type File_Access is access constant File_Type;

```

```ada
   function Standard_Input  return File_Access;
   function Standard_Output return File_Access;
   function Standard_Error  return File_Access;

```

```ada
   function Current_Input   return File_Access;
   function Current_Output  return File_Access;
   function Current_Error   return File_Access;

```

```ada
--Buffer control
   procedure Flush (File : in out File_Type);
   procedure Flush;

```

```ada
   -- Specification of line and page lengths

```

```ada
   procedure Set_Line_Length(File : in File_Type; To : in Count);
   procedure Set_Line_Length(To   : in Count);

```

```ada
   procedure Set_Page_Length(File : in File_Type; To : in Count);
   procedure Set_Page_Length(To   : in Count);

```

```ada
   function  Line_Length(File : in File_Type) return Count;
   function  Line_Length return Count;

```

```ada
   function  Page_Length(File : in File_Type) return Count;
   function  Page_Length return Count;

```

```ada
   -- Column, Line, and Page Control

```

```ada
   procedure New_Line   (File    : in File_Type;
                         Spacing : in Positive_Count := 1);
   procedure New_Line   (Spacing : in Positive_Count := 1);

```

```ada
   procedure Skip_Line  (File    : in File_Type;
                         Spacing : in Positive_Count := 1);
   procedure Skip_Line  (Spacing : in Positive_Count := 1);

```

```ada
   function  End_Of_Line(File : in File_Type) return Boolean;
   function  End_Of_Line return Boolean;

```

```ada
   procedure New_Page   (File : in File_Type);
   procedure New_Page;

```

```ada
   procedure Skip_Page  (File : in File_Type);
   procedure Skip_Page;

```

```ada
   function  End_Of_Page(File : in File_Type) return Boolean;
   function  End_Of_Page return Boolean;

```

```ada
   function  End_Of_File(File : in File_Type) return Boolean;
   function  End_Of_File return Boolean;

```

```ada
   procedure Set_Col (File : in File_Type; To : in Positive_Count);
   procedure Set_Col (To   : in Positive_Count);

```

```ada
   procedure Set_Line(File : in File_Type; To : in Positive_Count);
   procedure Set_Line(To   : in Positive_Count);

```

```ada
   function Col (File : in File_Type) return Positive_Count;
   function Col  return Positive_Count;

```

```ada
   function Line(File : in File_Type) return Positive_Count;
   function Line return Positive_Count;

```

```ada
   function Page(File : in File_Type) return Positive_Count;
   function Page return Positive_Count;

```

```ada
   -- Character Input-Output

```

```ada
   procedure Get(File : in  File_Type; Item : out Character);
   procedure Get(Item : out Character);

```

```ada
   procedure Put(File : in  File_Type; Item : in Character);
   procedure Put(Item : in  Character);

```

```ada
   procedure Look_Ahead (File        : in  File_Type;
                         Item        : out Character;
                         End_Of_Line : out Boolean);
   procedure Look_Ahead (Item        : out Character;
                         End_Of_Line : out Boolean);

```

```ada
   procedure Get_Immediate(File      : in  File_Type;
                           Item      : out Character);
   procedure Get_Immediate(Item      : out Character);

```

```ada
   procedure Get_Immediate(File      : in  File_Type;
                           Item      : out Character;
                           Available : out Boolean);
   procedure Get_Immediate(Item      : out Character;
                           Available : out Boolean);

```

```ada
   -- String Input-Output

```

```ada
   procedure Get(File : in  File_Type; Item : out String);
   procedure Get(Item : out String);

```

```ada
   procedure Put(File : in  File_Type; Item : in String);
   procedure Put(Item : in  String);

```

```ada
   procedure Get_Line(File : in  File_Type;
                      Item : out String;
                      Last : out Natural);
   procedure Get_Line(Item : out String; Last : out Natural);

```

```ada
   procedure Put_Line(File : in  File_Type; Item : in String);
   procedure Put_Line(Item : in  String);

```

```ada
-- Generic packages for Input-Output of Integer Types

```

```ada
   generic
      type Num is range &lt&gt;
   package Integer_IO is

```

```ada
      Default_Width : Field := Num'Width;
      Default_Base  : Number_Base := 10;

```

```ada
      procedure Get(File  : in  File_Type;
                    Item  : out Num;
                    Width : in Field := 0);
      procedure Get(Item  : out Num;
                    Width : in  Field := 0);

```

```ada
      procedure Put(File  : in File_Type;
                    Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base);
      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base);
      procedure Get(From : in  String;
                    Item : out Num;
                    Last : out Positive);
      procedure Put(To   : out String;
                    Item : in Num;
                    Base : in Number_Base := Default_Base);

```

```ada
   end Integer_IO;

```

```ada
   generic
      type Num is mod &lt&gt;
   package Modular_IO is

```

```ada
      Default_Width : Field := Num'Width;
      Default_Base  : Number_Base := 10;

```

```ada
      procedure Get(File  : in  File_Type;
                    Item  : out Num;
                    Width : in Field := 0);
      procedure Get(Item  : out Num;
                    Width : in  Field := 0);

```

```ada
      procedure Put(File  : in File_Type;
                    Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base);
      procedure Put(Item  : in Num;
                    Width : in Field := Default_Width;
                    Base  : in Number_Base := Default_Base);
      procedure Get(From : in  String;
                    Item : out Num;
                    Last : out Positive);
      procedure Put(To   : out String;
                    Item : in Num;
                    Base : in Number_Base := Default_Base);

```

```ada
   end Modular_IO;

```

```ada
   -- Generic packages for Input-Output of Real Types

```

```ada
   generic
      type Num is digits &lt&gt;
   package Float_IO is

```

```ada
      Default_Fore : Field := 2;
      Default_Aft  : Field := Num'Digits-1;
      Default_Exp  : Field := 3;

```

```ada
      procedure Get(File  : in  File_Type;
                    Item  : out Num;
                    Width : in  Field := 0);
      procedure Get(Item  : out Num;
                    Width : in  Field := 0);

```

```ada
      procedure Put(File : in File_Type;
                    Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);
      procedure Put(Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);

```

```ada
      procedure Get(From : in String;
                    Item : out Num;
                    Last : out Positive);
      procedure Put(To   : out String;
                    Item : in Num;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);
   end Float_IO;

```

```ada
   generic
      type Num is delta &lt&gt;
   package Fixed_IO is

```

```ada
      Default_Fore : Field := Num'Fore;
      Default_Aft  : Field := Num'Aft;
      Default_Exp  : Field := 0;

```

```ada
      procedure Get(File  : in  File_Type;
                    Item  : out Num;
                    Width : in  Field := 0);
      procedure Get(Item  : out Num;
                    Width : in  Field := 0);

```

```ada
      procedure Put(File : in File_Type;
                    Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);
      procedure Put(Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);

```

```ada
      procedure Get(From : in  String;
                    Item : out Num;
                    Last : out Positive);
      procedure Put(To   : out String;
                    Item : in Num;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);
   end Fixed_IO;

```

```ada
   generic
      type Num is delta &lt&gt digits &lt&gt;
   package Decimal_IO is

```

```ada
      Default_Fore : Field := Num'Fore;
      Default_Aft  : Field := Num'Aft;
      Default_Exp  : Field := 0;

```

```ada
      procedure Get(File  : in  File_Type;
                    Item  : out Num;
                    Width : in  Field := 0);
      procedure Get(Item  : out Num;
                    Width : in  Field := 0);

```

```ada
      procedure Put(File : in File_Type;
                    Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);
      procedure Put(Item : in Num;
                    Fore : in Field := Default_Fore;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);

```

```ada
      procedure Get(From : in  String;
                    Item : out Num;
                    Last : out Positive);
      procedure Put(To   : out String;
                    Item : in Num;
                    Aft  : in Field := Default_Aft;
                    Exp  : in Field := Default_Exp);
   end Decimal_IO;

```

```ada
   -- Generic package for Input-Output of Enumeration Types

```

```ada
   generic
      type Enum is (&lt&gt);
   package Enumeration_IO is

```

```ada
      Default_Width   : Field := 0;
      Default_Setting : Type_Set := Upper_Case;

```

```ada
      procedure Get(File : in  File_Type;
                    Item : out Enum);
      procedure Get(Item : out Enum);

```

```ada
      procedure Put(File  : in File_Type;
                    Item  : in Enum;
                    Width : in Field    := Default_Width;
                    Set   : in Type_Set := Default_Setting);
      procedure Put(Item  : in Enum;
                    Width : in Field    := Default_Width;
                    Set   : in Type_Set := Default_Setting);

```

```ada
      procedure Get(From : in  String;
                    Item : out Enum;
                    Last : out Positive);
      procedure Put(To   : out String;
                    Item : in  Enum;
                    Set  : in  Type_Set := Default_Setting);
   end Enumeration_IO;

```

```ada
-- Exceptions

```

```ada
   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error   : exception renames IO_Exceptions.Mode_Error;
   Name_Error   : exception renames IO_Exceptions.Name_Error;
   Use_Error    : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error    : exception renames IO_Exceptions.End_Error;
   Data_Error   : exception renames IO_Exceptions.Data_Error;
   Layout_Error : exception renames IO_Exceptions.Layout_Error;
private
   ... -- not specified by the language
end Ada.Text_IO;

```


#### Incompatibilities With Ada 83

Append_File is a new element of enumeration type File_Mode. 


#### Extensions to Ada 83

Get_Immediate, Look_Ahead, the subprograms for dealing with standard error, the type File_Access and its associated subprograms, and the generic packages Modular_IO and Decimal_IO are new in Ada 95. 


### A.10.2  Text File Management


#### Static Semantics

The only allowed file modes for text files are the modes In_File, Out_File, and Append_File. The subprograms given in subclause A.8.2 for the control of external files, and the function End_Of_File given in subclause A.8.3 for sequential input-output, are also available for text files. There is also a version of End_Of_File that refers to the current default input file. For text files, the procedures have the following additional effects: 

For the procedures Create and Open: After a file with mode Out_File or Append_File is opened, the page length and line length are unbounded (both have the conventional value zero). After a file (of any mode) is opened, the current column, current line, and current page numbers are set to one. If the mode is Append_File, it is implementation defined whether a page terminator will separate preexisting text in the file from the new text to be written. 

Reason: For a file with mode Append_File, although it may seem more sensible for Open to set the current column, line, and page number based on the number of pages in the file, the number of lines on the last page, and the number of columns in the last line, we rejected this approach because of implementation costs; it would require the implementation to scan the file before doing the append, or to do processing that would be equivalent in effect.

For similar reasons, there is no requirement to erase the last page terminator of the file, nor to insert an explicit page terminator in the case when the final page terminator of a file is represented implicitly by the implementation. 

For the procedure Close: If the file has the current mode Out_File or Append_File, has the effect of calling New_Page, unless the current page is already terminated; then outputs a file terminator.

For the procedure Reset: If the file has the current mode Out_File or Append_File, has the effect of calling New_Page, unless the current page is already terminated; then outputs a file terminator. The current column, line, and page numbers are set to one, and the line and page lengths to Unbounded. If the new mode is Append_File, it is implementation defined whether a page terminator will separate preexisting text in the file from the new text to be written. 

Reason: The behavior of Reset should be similar to closing a file and reopening it with the given mode

The exception Mode_Error is propagated by the procedure Reset upon an attempt to change the mode of a file that is the current default input file, the current default output file, or the current default error file. 

NOTE   An implementation can define the Form parameter of Create and Open to control effects including the following: 

the interpretation of line and column numbers for an interactive file, and

the interpretation of text formats in a file created by a foreign program. 


### A.10.3  Default Input, Output, and Error Files


#### Static Semantics

The following subprograms provide for the control of the particular default files that are used when a file parameter is omitted from a Get, Put, or other operation of text input-output described below, or when application-dependent error-related text is to be output. 

```ada
procedure Set_Input(File : in File_Type);

```

Operates on a file of mode In_File. Sets the current default input file to File.

The exception Status_Error is propagated if the given file is not open. The exception Mode_Error is propagated if the mode of the given file is not In_File.

```ada
procedure Set_Output(File : in File_Type);
procedure Set_Error (File : in File_Type);

```

Each operates on a file of mode Out_File or Append_File. Set_Output sets the current default output file to File. Set_Error sets the current default error file to File. The exception Status_Error is propagated if the given file is not open. The exception Mode_Error is propagated if the mode of the given file is not Out_File or Append_File.

```ada
function Standard_Input return File_Type;
function Standard_Input return File_Access;

```

Returns the standard input file (see A.10), or an access value designating the standard input file, respectively.

```ada
function Standard_Output return File_Type;
function Standard_Output return File_Access;

```

Returns the standard output file (see A.10) or an access value designating the standard output file, respectively.

```ada
function Standard_Error return File_Type;
function Standard_Error return File_Access;

```

Returns the standard error file (see A.10), or an access value designating the standard output file, respectively.

The Form strings implicitly associated with the opening of Standard_Input, Standard_Output, and Standard_Error at the start of program execution are implementation defined. 

```ada
function Current_Input return File_Type;
function Current_Input return File_Access;

```

Returns the current default input file, or an access value designating the current default input file, respectively.

```ada
function Current_Output return File_Type;
function Current_Output return File_Access;

```

Returns the current default output file, or an access value designating the current default output file, respectively.

```ada
function Current_Error return File_Type;
function Current_Error return File_Access;

```

Returns the current default error file, or an access value designating the current default error file, respectively.

```ada
procedure Flush (File : in out File_Type);
procedure Flush;

```

[The effect of Flush is the same as the corresponding subprogram in Streams.Stream_IO (see A.12.1).] If File is not explicitly specified, Current_Output is used. 


#### Erroneous Execution

The execution of a program is erroneous if it attempts to use a current default input, default output, or default error file that no longer exists. 

If the Close operation is applied to a file object that is also serving as the default input, default output, or default error file, then subsequent operations on such a default file are erroneous. 

NOTE 1   The standard input, standard output, and standard error files cannot be opened, closed, reset, or deleted, because the parameter File of the corresponding procedures has the mode in out.

NOTE 2   The standard input, standard output, and standard error files are different file objects, but not necessarily different external files. 


### A.10.4  Specification of Line and Page Lengths


#### Static Semantics

The subprograms described in this subclause are concerned with the line and page structure of a file of mode Out_File or Append_File. They operate either on the file given as the first parameter, or, in the absence of such a file parameter, on the current default output file. They provide for output of text with a specified maximum line length or page length. In these cases, line and page terminators are output implicitly and automatically when needed. When line and page lengths are unbounded (that is, when they have the conventional value zero), as in the case of a newly opened file, new lines and new pages are only started when explicitly called for.

In all cases, the exception Status_Error is propagated if the file to be used is not open; the exception Mode_Error is propagated if the mode of the file is not Out_File or Append_File. 

```ada
procedure Set_Line_Length(File : in File_Type; To : in Count);
procedure Set_Line_Length(To   : in Count);

```

Sets the maximum line length of the specified output or append file to the number of characters specified by To. The value zero for To specifies an unbounded line length. 

Ramification: The setting does not affect the lengths of lines in the existing file, rather it only influences subsequent output operations.

The exception Use_Error is propagated if the specified line length is inappropriate for the associated external file.

```ada
procedure Set_Page_Length(File : in File_Type; To : in Count);
procedure Set_Page_Length(To   : in Count);

```

Sets the maximum page length of the specified output or append file to the number of lines specified by To. The value zero for To specifies an unbounded page length.

The exception Use_Error is propagated if the specified page length is inappropriate for the associated external file.

```ada
function Line_Length(File : in File_Type) return Count;
function Line_Length return Count;

```

Returns the maximum line length currently set for the specified output or append file, or zero if the line length is unbounded.

```ada
function Page_Length(File : in File_Type) return Count;
function Page_Length return Count;

```

Returns the maximum page length currently set for the specified output or append file, or zero if the page length is unbounded. 


### A.10.5  Operations on Columns, Lines, and Pages


#### Static Semantics

The subprograms described in this subclause provide for explicit control of line and page structure; they operate either on the file given as the first parameter, or, in the absence of such a file parameter, on the appropriate (input or output) current default file. The exception Status_Error is propagated by any of these subprograms if the file to be used is not open. 

```ada
procedure New_Line(File : in File_Type; Spacing : in Positive_Count := 1);
procedure New_Line(Spacing : in Positive_Count := 1);

```

Operates on a file of mode Out_File or Append_File.

For a Spacing of one: Outputs a line terminator and sets the current column number to one. Then increments the current line number by one, except in the case that the current line number is already greater than or equal to the maximum page length, for a bounded page length; in that case a page terminator is output, the current page number is incremented by one, and the current line number is set to one.

For a Spacing greater than one, the above actions are performed Spacing times.

The exception Mode_Error is propagated if the mode is not Out_File or Append_File.

```ada
procedure Skip_Line(File  : in File_Type; Spacing : in Positive_Count := 1);
procedure Skip_Line(Spacing : in Positive_Count := 1);

```

Operates on a file of mode In_File.

For a Spacing of one: Reads and discards all characters until a line terminator has been read, and then sets the current column number to one. If the line terminator is not immediately followed by a page terminator, the current line number is incremented by one. Otherwise, if the line terminator is immediately followed by a page terminator, then the page terminator is skipped, the current page number is incremented by one, and the current line number is set to one.

For a Spacing greater than one, the above actions are performed Spacing times.

The exception Mode_Error is propagated if the mode is not In_File. The exception End_Error is propagated if an attempt is made to read a file terminator.

```ada
function End_Of_Line(File : in File_Type) return Boolean;
function End_Of_Line return Boolean;

```

Operates on a file of mode In_File. Returns True if a line terminator or a file terminator is next; otherwise returns False.

The exception Mode_Error is propagated if the mode is not In_File.

```ada
procedure New_Page(File : in File_Type);
procedure New_Page;

```

Operates on a file of mode Out_File or Append_File. Outputs a line terminator if the current line is not terminated, or if the current page is empty (that is, if the current column and line numbers are both equal to one). Then outputs a page terminator, which terminates the current page. Adds one to the current page number and sets the current column and line numbers to one.

The exception Mode_Error is propagated if the mode is not Out_File or Append_File.

```ada
procedure Skip_Page(File : in File_Type);
procedure Skip_Page;

```

Operates on a file of mode In_File. Reads and discards all characters and line terminators until a page terminator has been read. Then adds one to the current page number, and sets the current column and line numbers to one.

The exception Mode_Error is propagated if the mode is not In_File. The exception End_Error is propagated if an attempt is made to read a file terminator.

```ada
function End_Of_Page(File : in File_Type) return Boolean;
function End_Of_Page return Boolean;

```

Operates on a file of mode In_File. Returns True if the combination of a line terminator and a page terminator is next, or if a file terminator is next; otherwise returns False.

The exception Mode_Error is propagated if the mode is not In_File.

```ada
function End_Of_File(File : in File_Type) return Boolean;
function End_Of_File return Boolean;

```

Operates on a file of mode In_File. Returns True if a file terminator is next, or if the combination of a line, a page, and a file terminator is next; otherwise returns False.

The exception Mode_Error is propagated if the mode is not In_File.

The following subprograms provide for the control of the current position of reading or writing in a file. In all cases, the default file is the current output file. 

```ada
procedure Set_Col(File : in File_Type; To : in Positive_Count);
procedure Set_Col(To   : in Positive_Count);

```

If the file mode is Out_File or Append_File: 

If the value specified by To is greater than the current column number, outputs spaces, adding one to the current column number after each space, until the current column number equals the specified value. If the value specified by To is equal to the current column number, there is no effect. If the value specified by To is less than the current column number, has the effect of calling New_Line (with a spacing of one), then outputs (To  1) spaces, and sets the current column number to the specified value.

The exception Layout_Error is propagated if the value specified by To exceeds Line_Length when the line length is bounded (that is, when it does not have the conventional value zero). 

If the file mode is In_File: 

Reads (and discards) individual characters, line terminators, and page terminators, until the next character to be read has a column number that equals the value specified by To; there is no effect if the current column number already equals this value. Each transfer of a character or terminator maintains the current column, line, and page numbers in the same way as a Get procedure (see A.10.6). (Short lines will be skipped until a line is reached that has a character at the specified column position.)

The exception End_Error is propagated if an attempt is made to read a file terminator. 

```ada
procedure Set_Line(File : in File_Type; To : in Positive_Count);
procedure Set_Line(To   : in Positive_Count);

```

If the file mode is Out_File or Append_File: 

If the value specified by To is greater than the current line number, has the effect of repeatedly calling New_Line (with a spacing of one), until the current line number equals the specified value. If the value specified by To is equal to the current line number, there is no effect. If the value specified by To is less than the current line number, has the effect of calling New_Page followed by a call of New_Line with a spacing equal to (To  1).

The exception Layout_Error is propagated if the value specified by To exceeds Page_Length when the page length is bounded (that is, when it does not have the conventional value zero). 

If the mode is In_File: 

Has the effect of repeatedly calling Skip_Line (with a spacing of one), until the current line number equals the value specified by To; there is no effect if the current line number already equals this value. (Short pages will be skipped until a page is reached that has a line at the specified line position.)

The exception End_Error is propagated if an attempt is made to read a file terminator. 

```ada
function Col(File : in File_Type) return Positive_Count;
function Col return Positive_Count;

```

Returns the current column number.

The exception Layout_Error is propagated if this number exceeds Count'Last.

```ada
function Line(File : in File_Type) return Positive_Count;
function Line return Positive_Count;

```

Returns the current line number.

The exception Layout_Error is propagated if this number exceeds Count'Last.

```ada
function Page(File : in File_Type) return Positive_Count;
function Page return Positive_Count;

```

Returns the current page number.

The exception Layout_Error is propagated if this number exceeds Count'Last.

The column number, line number, or page number are allowed to exceed Count'Last (as a consequence of the input or output of sufficiently many characters, lines, or pages). These events do not cause any exception to be propagated. However, a call of Col, Line, or Page propagates the exception Layout_Error if the corresponding number exceeds Count'Last. 

NOTE 1   A page terminator is always skipped whenever the preceding line terminator is skipped. An implementation may represent the combination of these terminators by a single character, provided that it is properly recognized on input. 


### A.10.6  Get and Put Procedures


#### Static Semantics

The procedures Get and Put for items of the type Character, String, numeric types, and enumeration types are described in subsequent subclauses. Features of these procedures that are common to most of these types are described in this subclause. The Get and Put procedures for items of type Character and String deal with individual character values; the Get and Put procedures for numeric and enumeration types treat the items as lexical elements.

All procedures Get and Put have forms with a file parameter, written first. Where this parameter is omitted, the appropriate (input or output) current default file is understood to be specified. Each procedure Get operates on a file of mode In_File. Each procedure Put operates on a file of mode Out_File or Append_File.

All procedures Get and Put maintain the current column, line, and page numbers of the specified file: the effect of each of these procedures upon these numbers is the result of the effects of individual transfers of characters and of individual output or skipping of terminators. Each transfer of a character adds one to the current column number. Each output of a line terminator sets the current column number to one and adds one to the current line number. Each output of a page terminator sets the current column and line numbers to one and adds one to the current page number. For input, each skipping of a line terminator sets the current column number to one and adds one to the current line number; each skipping of a page terminator sets the current column and line numbers to one and adds one to the current page number. Similar considerations apply to the procedures Get_Line, Put_Line, and Set_Col.

Several Get and Put procedures, for numeric and enumeration types, have format parameters which specify field lengths; these parameters are of the nonnegative subtype Field of the type Integer.

Input-output of enumeration values uses the syntax of the corresponding lexical elements. Any Get procedure for an enumeration type begins by skipping any leading blanks, or line or page terminators. Get procedures for numeric or enumeration types start by skipping leading blanks, where a blank is defined as a space or a horizontal tabulation character. Next, characters are input only so long as the sequence input is an initial sequence of an identifier or of a character literal (in particular, input ceases when a line terminator is encountered). The character or line terminator that causes input to cease remains available for subsequent input.

For a numeric type, the Get procedures have a format parameter called Width. If the value given for this parameter is zero, the Get procedure proceeds in the same manner as for enumeration types, but using the syntax of numeric literals instead of that of enumeration literals. If a nonzero value is given, then exactly Width characters are input, or the characters up to a line terminator, whichever comes first; any skipped leading blanks are included in the count. The syntax used for numeric literals is an extended syntax that allows a leading sign (but no intervening blanks, or line or page terminators) and that also allows (for real types) an integer literal as well as forms that have digits only before the point or only after the point.

Any Put procedure, for an item of a numeric or an enumeration type, outputs the value of the item as a numeric literal, identifier, or character literal, as appropriate. This is preceded by leading spaces if required by the format parameters Width or Fore (as described in later subclauses), and then a minus sign for a negative value; for an enumeration type, the spaces follow instead of leading. The format given for a Put procedure is overridden if it is insufficiently wide, by using the minimum needed width.

Two further cases arise for Put procedures for numeric and enumeration types, if the line length of the specified output file is bounded (that is, if it does not have the conventional value zero). If the number of characters to be output does not exceed the maximum line length, but is such that they cannot fit on the current line, starting from the current column, then (in effect) New_Line is called (with a spacing of one) before output of the item. Otherwise, if the number of characters exceeds the maximum line length, then the exception Layout_Error is propagated and nothing is output.

The exception Status_Error is propagated by any of the procedures Get, Get_Line, Put, and Put_Line if the file to be used is not open. The exception Mode_Error is propagated by the procedures Get and Get_Line if the mode of the file to be used is not In_File; and by the procedures Put and Put_Line, if the mode is not Out_File or Append_File.

The exception End_Error is propagated by a Get procedure if an attempt is made to skip a file terminator. The exception Data_Error is propagated by a Get procedure if the sequence finally input is not a lexical element corresponding to the type, in particular if no characters were input; for this test, leading blanks are ignored; for an item of a numeric type, when a sign is input, this rule applies to the succeeding numeric literal. The exception Layout_Error is propagated by a Put procedure that outputs to a parameter of type String, if the length of the actual string is insufficient for the output of the item. 


#### Examples

In the examples, here and in subclauses A.10.8 and A.10.9, the string quotes and the lower case letter b are not transferred: they are shown only to reveal the layout and spaces.

```ada
N : Integer;
   ...
Get(N);

```

```ada
--  	Characters at input 	Sequence input 	Value of N

--  	bb12535b 	12535 	12535
--  	bb12_535e1b 	12_535e1 	125350
--  	bb12_535e; 	12_535e 	(none) Data_Error raised

```

Example of overridden width parameter: 

```ada
Put(Item =&gt -23, Width =&gt 2);  --  "23"

```


### A.10.7  Input-Output of Characters and Strings


#### Static Semantics

For an item of type Character the following procedures are provided: 

```ada
procedure Get(File : in File_Type; Item : out Character);
procedure Get(Item : out Character);

```

After skipping any line terminators and any page terminators, reads the next character from the specified input file and returns the value of this character in the out parameter Item.

The exception End_Error is propagated if an attempt is made to skip a file terminator.

```ada
procedure Put(File : in File_Type; Item : in Character);
procedure Put(Item : in Character);

```

If the line length of the specified output file is bounded (that is, does not have the conventional value zero), and the current column number exceeds it, has the effect of calling New_Line with a spacing of one. Then, or otherwise, outputs the given character to the file.

```ada
procedure Look_Ahead (File        : in  File_Type;
                      Item        : out Character;
                      End_Of_Line : out Boolean);
procedure Look_Ahead (Item        : out Character;
                      End_Of_Line : out Boolean);

```

Mode_Error is propagated if the mode of the file is not In_File. Sets End_Of_Line to True if at end of line, including if at end of page or at end of file; in each of these cases the value of Item is not specified. Otherwise End_Of_Line is set to False and Item is set to the the next character (without consuming it) from the file.

```ada
procedure Get_Immediate(File : in  File_Type;
                        Item : out Character);
procedure Get_Immediate(Item : out Character);

```

Reads the next character, either control or graphic, from the specified File or the default input file. Mode_Error is propagated if the mode of the file is not In_File. End_Error is propagated if at the end of the file. The current column, line and page numbers for the file are not affected.

```ada
procedure Get_Immediate(File      : in  File_Type;
                        Item      : out Character;
                        Available : out Boolean);
procedure Get_Immediate(Item      : out Character;
                        Available : out Boolean);

```

If a character, either control or graphic, is available from the specified File or the default input file, then the character is read; Available is True and Item contains the value of this character. If a character is not available, then Available is False and the value of Item is not specified. Mode_Error is propagated if the mode of the file is not In_File. End_Error is propagated if at the end of the file. The current column, line and page numbers for the file are not affected.

For an item of type String the following procedures are provided: 

```ada
procedure Get(File : in File_Type; Item : out String);
procedure Get(Item : out String);

```

Determines the length of the given string and attempts that number of Get operations for successive characters of the string (in particular, no operation is performed if the string is null).

```ada
procedure Put(File : in File_Type; Item : in String);
procedure Put(Item : in String);

```

Determines the length of the given string and attempts that number of Put operations for successive characters of the string (in particular, no operation is performed if the string is null).

```ada
procedure Get_Line(File : in File_Type;
                   Item : out String;
                   Last : out Natural);
procedure Get_Line(Item : out String;
                   Last : out Natural);

```

Reads successive characters from the specified input file and assigns them to successive characters of the specified string. Reading stops if the end of the string is met. Reading also stops if the end of the line is met before meeting the end of the string; in this case Skip_Line is (in effect) called with a spacing of 1. The values of characters not assigned are not specified.

If characters are read, returns in Last the index value such that Item(Last) is the last character assigned (the index of the first character assigned is Item'First). If no characters are read, returns in Last an index value that is one less than Item'First. The exception End_Error is propagated if an attempt is made to skip a file terminator.

```ada
procedure Put_Line(File : in File_Type; Item : in String);
procedure Put_Line(Item : in String);

```

Calls the procedure Put for the given string, and then the procedure New_Line with a spacing of one. 


#### Implementation Advice

The Get_Immediate procedures should be implemented with unbuffered input. For a device such as a keyboard, input should be "available" if a key has already been typed, whereas for a disk file, input should always be available except at end of file. For a file associated with a keyboard-like device, any line-editing features of the underlying operating system should be disabled during the execution of Get_Immediate. 

NOTE 1   Get_Immediate can be used to read a single key from the keyboard "immediately"; that is, without waiting for an end of line. In a call of Get_Immediate without the parameter Available, the caller will wait until a character is available.

NOTE 2   In a literal string parameter of Put, the enclosing string bracket characters are not output. Each doubled string bracket character in the enclosed string is output as a single string bracket character, as a consequence of the rule for string literals (see 2.6).

NOTE 3   A string read by Get or written by Put can extend over several lines. An implementation is allowed to assume that certain external files do not contain page terminators, in which case Get_Line and Skip_Line can return as soon as a line terminator is read. 


### A.10.8  Input-Output for Integer Types


#### Static Semantics

The following procedures are defined in the generic packages Integer_IO and Modular_IO, which have to be instantiated for the appropriate signed integer or modular type respectively (indicated by Num in the specifications).

Values are output as decimal or based literals, without low line characters or exponent, and, for Integer_IO, preceded by a minus sign if negative. The format (which includes any leading spaces and minus sign) can be specified by an optional field width parameter. Values of widths of fields in output formats are of the nonnegative integer subtype Field. Values of bases are of the integer subtype Number_Base. 

```ada
subtype Number_Base is Integer range 2 .. 16;

```

The default field width and base to be used by output procedures are defined by the following variables that are declared in the generic packages Integer_IO and Modular_IO:

```ada
Default_Width : Field := Num'Width;
Default_Base  : Number_Base := 10;

```

The following procedures are provided: 

```ada
procedure Get(File : in File_Type; Item : out Num; Width : in Field := 0);
procedure Get(Item : out Num; Width : in Field := 0);

```

If the value of the parameter Width is zero, skips any leading blanks, line terminators, or page terminators, then reads a plus sign if present or (for a signed type only) a minus sign if present, then reads the longest possible sequence of characters matching the syntax of a numeric literal without a point. If a nonzero value of Width is supplied, then exactly Width characters are input, or the characters (possibly none) up to a line terminator, whichever comes first; any skipped leading blanks are included in the count.

Returns, in the parameter Item, the value of type Num that corresponds to the sequence input.

The exception Data_Error is propagated if the sequence of characters read does not form a legal integer literal or if the value obtained is not of the subtype Num (for Integer_IO) or is not in the base range of Num (for Modular_IO).

```ada
procedure Put(File  : in File_Type;
              Item  : in Num;
              Width : in Field := Default_Width;
              Base  : in Number_Base := Default_Base);

procedure Put(Item  : in Num;
              Width : in Field := Default_Width;
              Base  : in Number_Base := Default_Base);

```

Outputs the value of the parameter Item as an integer literal, with no low lines, no exponent, and no leading zeros (but a single zero for the value zero), and a preceding minus sign for a negative value.

If the resulting sequence of characters to be output has fewer than Width characters, then leading spaces are first output to make up the difference.

Uses the syntax for decimal literal if the parameter Base has the value ten (either explicitly or through Default_Base); otherwise, uses the syntax for based literal, with any letters in upper case.

```ada
procedure Get(From : in String; Item : out Num; Last : out Positive);

```

Reads an integer value from the beginning of the given string, following the same rules as the Get procedure that reads an integer value from a file, but treating the end of the string as a file terminator. Returns, in the parameter Item, the value of type Num that corresponds to the sequence input. Returns in Last the index value such that From(Last) is the last character read.

The exception Data_Error is propagated if the sequence input does not have the required syntax or if the value obtained is not of the subtype Num.

```ada
procedure Put(To   : out String;
              Item : in Num;
              Base : in Number_Base := Default_Base);

```

Outputs the value of the parameter Item to the given string, following the same rule as for output to a file, using the length of the given string as the value for Width. 

Integer_Text_IO is a library package that is a nongeneric equivalent to Text_IO.Integer_IO for the predefined type Integer: 

```ada
with Ada.Text_IO;
package Ada.Integer_Text_IO is new Ada.Text_IO.Integer_IO(Integer);

```

For each predefined signed integer type, a nongeneric equivalent to Text_IO.Integer_IO is provided, with names such as Ada.Long_Integer_Text_IO.


#### Implementation Permissions

The nongeneric equivalent packages may, but need not, be actual instantiations of the generic package for the appropriate predefined type.

NOTE 1   For Modular_IO, execution of Get propagates Data_Error if the sequence of characters read forms an integer literal outside the range 0..Num'Last. 


#### Examples

 

```ada
package Int_IO is new Integer_IO(Small_Int); use Int_IO;
-- default format used at instantiation,
-- Default_Width = 4, Default_Base = 10

```

```ada
Put(126);                            -- "b126"
Put(-126, 7);                        -- "bbb126"
Put(126, Width =&gt 13, Base =&gt 2);    -- "bbb2#1111110#"

```


### A.10.9  Input-Output for Real Types


#### Static Semantics

The following procedures are defined in the generic packages Float_IO, Fixed_IO, and Decimal_IO, which have to be instantiated for the appropriate floating point, ordinary fixed point, or decimal fixed point type respectively (indicated by Num in the specifications).

Values are output as decimal literals without low line characters. The format of each value output consists of a Fore field, a decimal point, an Aft field, and (if a nonzero Exp parameter is supplied) the letter E and an Exp field. The two possible formats thus correspond to:

```ada
Fore  .  Aft

```

and to:

```ada
Fore  .  Aft  E  Exp

```

without any spaces between these fields. The Fore field may include leading spaces, and a minus sign for negative values. The Aft field includes only decimal digits (possibly with trailing zeros). The Exp field includes the sign (plus or minus) and the exponent (possibly with leading zeros).

For floating point types, the default lengths of these fields are defined by the following variables that are declared in the generic package Float_IO:

```ada
Default_Fore : Field := 2;
Default_Aft  : Field := Num'Digits-1;
Default_Exp  : Field := 3;

```

For ordinary or decimal fixed point types, the default lengths of these fields are defined by the following variables that are declared in the generic packages Fixed_IO and Decimal_IO, respectively:

```ada
Default_Fore : Field := Num'Fore;
Default_Aft  : Field := Num'Aft;
Default_Exp  : Field := 0;

```

The following procedures are provided: 

```ada
procedure Get(File : in File_Type; Item : out Num; Width : in Field := 0);
procedure Get(Item : out Num; Width : in Field := 0);

```

If the value of the parameter Width is zero, skips any leading blanks, line terminators, or page terminators, then reads the longest possible sequence of characters matching the syntax of any of the following (see 2.4): 

[+|][numeric_literal](S0004)

[+|][numeral](S0006).[[exponent](S0007)]

[+|].[numeral](S0006)[[exponent](S0007)]

[+|][base](S0009)#[based_numeral](S0010).#[[exponent](S0007)]

[+|][base](S0009)#.[based_numeral](S0010)#[[exponent](S0007)] 

If a nonzero value of Width is supplied, then exactly Width characters are input, or the characters (possibly none) up to a line terminator, whichever comes first; any skipped leading blanks are included in the count.

Returns in the parameter Item the value of type Num that corresponds to the sequence input, preserving the sign (positive if none has been specified) of a zero value if Num is a floating point type and Num'Signed_Zeros is True.

The exception Data_Error is propagated if the sequence input does not have the required syntax or if the value obtained is not of the subtype Num.

```ada
procedure Put(File : in File_Type;
              Item : in Num;
              Fore : in Field := Default_Fore;
              Aft  : in Field := Default_Aft;
              Exp  : in Field := Default_Exp);

procedure Put(Item : in Num;
              Fore : in Field := Default_Fore;
              Aft  : in Field := Default_Aft;
              Exp  : in Field := Default_Exp);

```

Outputs the value of the parameter Item as a decimal literal with the format defined by Fore, Aft and Exp. If the value is negative, or if Num is a floating point type where Num'Signed_Zeros is True and the value is a negatively signed zero, then a minus sign is included in the integer part. If Exp has the value zero, then the integer part to be output has as many digits as are needed to represent the integer part of the value of Item, overriding Fore if necessary, or consists of the digit zero if the value of Item has no integer part.

If Exp has a value greater than zero, then the integer part to be output has a single digit, which is nonzero except for the value 0.0 of Item.

In both cases, however, if the integer part to be output has fewer than Fore characters, including any minus sign, then leading spaces are first output to make up the difference. The number of digits of the fractional part is given by Aft, or is one if Aft equals zero. The value is rounded; a value of exactly one half in the last place is rounded away from zero.

If Exp has the value zero, there is no exponent part. If Exp has a value greater than zero, then the exponent part to be output has as many digits as are needed to represent the exponent part of the value of Item (for which a single digit integer part is used), and includes an initial sign (plus or minus). If the exponent part to be output has fewer than Exp characters, including the sign, then leading zeros precede the digits, to make up the difference. For the value 0.0 of Item, the exponent has the value zero.

```ada
procedure Get(From : in String; Item : out Num; Last : out Positive);

```

Reads a real value from the beginning of the given string, following the same rule as the Get procedure that reads a real value from a file, but treating the end of the string as a file terminator. Returns, in the parameter Item, the value of type Num that corresponds to the sequence input. Returns in Last the index value such that From(Last) is the last character read.

The exception Data_Error is propagated if the sequence input does not have the required syntax, or if the value obtained is not of the subtype Num.

```ada
procedure Put(To   : out String;
              Item : in Num;
              Aft  : in Field := Default_Aft;
              Exp  : in Field := Default_Exp);

```

Outputs the value of the parameter Item to the given string, following the same rule as for output to a file, using a value for Fore such that the sequence of characters output exactly fills the string, including any leading spaces. 

Float_Text_IO is a library package that is a nongeneric equivalent to Text_IO.Float_IO for the predefined type Float: 

```ada
with Ada.Text_IO;
package Ada.Float_Text_IO is new Ada.Text_IO.Float_IO(Float);

```

For each predefined floating point type, a nongeneric equivalent to Text_IO.Float_IO is provided, with names such as Ada.Long_Float_Text_IO.


#### Implementation Permissions

An implementation may extend Get [and Put] for floating point types to support special values such as infinities and NaNs.

Discussion: See also the similar permission for the Wide_Valueattribute in 3.5. 

The implementation of Put need not produce an output value with greater accuracy than is supported for the base subtype. The additional accuracy, if any, of the value produced by Put when the number of requested digits in the integer and fractional parts exceeds the required accuracy is implementation defined. 

Discussion: The required accuracy is thus Num'Base'Digits digits if Num is a floating point subtype. For a fixed point subtype the required accuracy is a function of the subtype's Fore, Aft, and Delta attributes. 

Implementation defined: The accuracy of the value produced by Put.

The nongeneric equivalent packages may, but need not, be actual instantiations of the generic package for the appropriate predefined type.

NOTE 1   For an item with a positive value, if output to a string exactly fills the string without leading spaces, then output of the corresponding negative value will propagate Layout_Error.

NOTE 2   The rules for the Value attribute (see 3.5) and the rules for Get are based on the same set of formats. 


#### Examples

 

```ada
package Real_IO is new Float_IO(Real); use Real_IO;
-- default format used at instantiation, Default_Exp = 3

```

```ada
X : Real := -123.4567;  --  digits 8      (see 3.5.7)

```

```ada
Put(X);  -- default format 	"1.2345670E+02"
Put(X, Fore =&gt 5, Aft =&gt 3, Exp =&gt 2); 	-- "bbb1.235E+2"
Put(X, 5, 3, 0);             	-- "b123.457"

```


### A.10.10  Input-Output for Enumeration Types


#### Static Semantics

The following procedures are defined in the generic package Enumeration_IO, which has to be instantiated for the appropriate enumeration type (indicated by Enum in the specification).

Values are output using either upper or lower case letters for identifiers. This is specified by the parameter Set, which is of the enumeration type Type_Set.

```ada
type Type_Set is (Lower_Case, Upper_Case);

```

The format (which includes any trailing spaces) can be specified by an optional field width parameter. The default field width and letter case are defined by the following variables that are declared in the generic package Enumeration_IO:

```ada
Default_Width   : Field := 0;
Default_Setting : Type_Set := Upper_Case;

```

The following procedures are provided: 

```ada
procedure Get(File : in File_Type; Item : out Enum);
procedure Get(Item : out Enum);

```

After skipping any leading blanks, line terminators, or page terminators, reads an identifier according to the syntax of this lexical element (lower and upper case being considered equivalent), or a character literal according to the syntax of this lexical element (including the apostrophes). Returns, in the parameter Item, the value of type Enum that corresponds to the sequence input.

The exception Data_Error is propagated if the sequence input does not have the required syntax, or if the identifier or character literal does not correspond to a value of the subtype Enum.

```ada
procedure Put(File  : in File_Type;
              Item  : in Enum;
              Width : in Field := Default_Width;
              Set   : in Type_Set := Default_Setting);

procedure Put(Item  : in Enum;
              Width : in Field := Default_Width;
              Set   : in Type_Set := Default_Setting);

```

Outputs the value of the parameter Item as an enumeration literal (either an identifier or a character literal). The optional parameter Set indicates whether lower case or upper case is used for identifiers; it has no effect for character literals. If the sequence of characters produced has fewer than Width characters, then trailing spaces are finally output to make up the difference. If Enum is a character type, the sequence of characters produced is as for Enum'Image(Item), as modified by the Width and Set parameters. 

Discussion: For a character type, the literal might be a Wide_Character or a control character. Whatever Image does for these things is appropriate here, too.

```ada
procedure Get(From : in String; Item : out Enum; Last : out Positive);

```

Reads an enumeration value from the beginning of the given string, following the same rule as the Get procedure that reads an enumeration value from a file, but treating the end of the string as a file terminator. Returns, in the parameter Item, the value of type Enum that corresponds to the sequence input. Returns in Last the index value such that From(Last) is the last character read.

The exception Data_Error is propagated if the sequence input does not have the required syntax, or if the identifier or character literal does not correspond to a value of the subtype Enum. 

To be honest: For a character type, it is permissible for the implementation to make Get do the inverse of what Put does, in the case of wide [character_literal](S0012)s and control characters. 

```ada
procedure Put(To   : out String;
              Item : in Enum;
              Set  : in Type_Set := Default_Setting);

```

Outputs the value of the parameter Item to the given string, following the same rule as for output to a file, using the length of the given string as the value for Width. 

Although the specification of the generic package Enumeration_IO would allow instantiation for an float type, this is not the intended purpose of this generic package, and the effect of such instantiations is not defined by the language. 

NOTE 1   There is a difference between Put defined for characters, and for enumeration values. Thus 

```ada
   Ada.Text_IO.Put('A');  --  outputs the character A

```

```ada
   package Char_IO is new Ada.Text_IO.Enumeration_IO(Character);
   Char_IO.Put('A');  --  outputs the character 'A', between apostrophes

```

NOTE 2   The type Boolean is an enumeration type, hence Enumeration_IO can be instantiated for this type. 


## A.11  Wide Text Input-Output

The package Wide_Text_IO provides facilities for input and output in human-readable form. Each file is read or written sequentially, as a sequence of wide characters grouped into lines, and as a sequence of lines grouped into pages. 


#### Static Semantics

The specification of package Wide_Text_IO is the same as that for Text_IO, except that in each Get, Look_Ahead, Get_Immediate, Get_Line, Put, and Put_Line procedure, any occurrence of Character is replaced by Wide_Character, and any occurrence of String is replaced by Wide_String.

Nongeneric equivalents of Wide_Text_IO.Integer_IO and Wide_Text_IO.Float_IO are provided (as for Text_IO) for each predefined numeric type, with names such as Ada.Integer_Wide_Text_IO, Ada.Long_Integer_Wide_Text_IO, Ada.Float_Wide_Text_IO, Ada.Long_Float_Wide_Text_IO.


#### Extensions to Ada 83

Support for Wide_Character and Wide_String I/O is new in Ada 95. 


## A.12  Stream Input-Output

The packages Streams.Stream_IO, Text_IO.Text_Streams, and Wide_Text_IO.Text_Streams provide stream-oriented operations on files. 


### A.12.1  The Package Streams.Stream_IO

[The subprograms in the child package Streams.Stream_IO provide control over stream files. Access to a stream file is either sequential, via a call on Read or Write to transfer an array of stream elements, or positional (if supported by the implementation for the given file), by specifying a relative index for an element. Since a stream file can be converted to a Stream_Access value, calling stream-oriented attribute subprograms of different element types with the same Stream_Access value provides heterogeneous input-output.] See 13.13 for a general discussion of streams. 


#### Static Semantics

The library package Streams.Stream_IO has the following declaration: 

```ada
with Ada.IO_Exceptions;
package Ada.Streams.Stream_IO is

```

```ada
    type Stream_Access is access all Root_Stream_Type'Class;

```

```ada
    type File_Type is limited private;

```

```ada
    type File_Mode is (In_File, Out_File, Append_File);

```

```ada
    type    Count          is range 0 .. implementation-defined;
    subtype Positive_Count is Count range 1 .. Count'Last;
      -- Index into file, in stream elements.

```

```ada
    procedure Create (File : in out File_Type;
                      Mode : in File_Mode := Out_File;
                      Name : in String    := "";
                      Form : in String    := "");

```

```ada
    procedure Open (File : in out File_Type;
                    Mode : in File_Mode;
                    Name : in String;
                    Form : in String := "");

```

```ada
    procedure Close  (File : in out File_Type);
    procedure Delete (File : in out File_Type);
    procedure Reset  (File : in out File_Type; Mode : in File_Mode);
    procedure Reset  (File : in out File_Type);

```

```ada
    function Mode (File : in File_Type) return File_Mode;
    function Name (File : in File_Type) return String;
    function Form (File : in File_Type) return String;

```

```ada
    function Is_Open     (File : in File_Type) return Boolean;
    function End_Of_File (File : in File_Type) return Boolean;

```

```ada
    function Stream (File : in File_Type) return Stream_Access;
        -- Return stream access for use with T'Input and T'Output

```

```ada
 

```

```ada
    -- Read array of stream elements from file
    procedure Read (File : in  File_Type;
                    Item : out Stream_Element_Array;
                    Last : out Stream_Element_Offset;
                    From : in  Positive_Count);

```

```ada
    procedure Read (File : in  File_Type;
                    Item : out Stream_Element_Array;
                    Last : out Stream_Element_Offset);

```

```ada
 

```

```ada
    -- Write array of stream elements into file
    procedure Write (File : in File_Type;
                     Item : in Stream_Element_Array;
                     To   : in Positive_Count);

```

```ada
    procedure Write (File : in File_Type;
                     Item : in Stream_Element_Array);

```

```ada
 

```

```ada
    -- Operations on position within file

```

```ada
    procedure Set_Index(File : in File_Type; To : in Positive_Count);

```

```ada
    function Index(File : in File_Type) return Positive_Count;
    function Size (File : in File_Type) return Count;

```

```ada
    procedure Set_Mode(File : in out File_Type; Mode : in File_Mode);

```

```ada
    procedure Flush(File : in out File_Type);

```

```ada
    -- exceptions
    Status_Error : exception renames IO_Exceptions.Status_Error;
    Mode_Error   : exception renames IO_Exceptions.Mode_Error;
    Name_Error   : exception renames IO_Exceptions.Name_Error;
    Use_Error    : exception renames IO_Exceptions.Use_Error;
    Device_Error : exception renames IO_Exceptions.Device_Error;
    End_Error    : exception renames IO_Exceptions.End_Error;
    Data_Error   : exception renames IO_Exceptions.Data_Error;

```

```ada
private
   ... -- not specified by the language
end Ada.Streams.Stream_IO;

```

The subprograms Create, Open, Close, Delete, Reset, Mode, Name, Form, Is_Open, and End_of_File have the same effect as the corresponding subprograms in Sequential_IO (see A.8.2).

The Stream function returns a Stream_Access result from a File_Type object, thus allowing the stream-oriented attributes Read, Write, Input, and Output to be used on the same file for multiple types. 

The procedures Read and Write are equivalent to the corresponding operations in the package Streams. Read propagates Mode_Error if the mode of File is not In_File. Write propagates Mode_Error if the mode of File is not Out_File or Append_File. The Read procedure with a Positive_Count parameter starts reading at the specified index. The Write procedure with a Positive_Count parameter starts writing at the specified index.

The Index function returns the current file index, as a count (in stream elements) from the beginning of the file. The position of the first element in the file is 1. 

Ramification: The notion of Index for Stream_IO is analogous to that of Index in Direct_IO, except that the former is measured in Stream_Element units, whereas the latter is in terms of Element_Type values. 

The Set_Index procedure sets the current index to the specified value.

If positioning is not supported for the given file, then a call of Index or Set_Index propagates Use_Error. Similarly, a call of Read or Write with a Positive_Count parameter propagates Use_Error.

The Size function returns the current size of the file, in stream elements.

The Set_Mode procedure changes the mode of the file. If the new mode is Append_File, the file is positioned to its end; otherwise, the position in the file is unchanged.

The Flush procedure synchronizes the external file with the internal file (by flushing any internal buffers) without closing the file or changing the position. Mode_Error is propagated if the mode of the file is In_File. 


### A.12.2  The Package Text_IO.Text_Streams

The package Text_IO.Text_Streams provides a function for treating a text file as a stream. 


#### Static Semantics

The library package Text_IO.Text_Streams has the following declaration: 

```ada
with Ada.Streams;
package Ada.Text_IO.Text_Streams is
   type Stream_Access is access all Streams.Root_Stream_Type'Class;

```

```ada
   function Stream (File : in File_Type) return Stream_Access;
end Ada.Text_IO.Text_Streams;

```

The Stream function has the same effect as the corresponding function in Streams.Stream_IO. 

NOTE 1   The ability to obtain a stream for a text file allows Current_Input, Current_Output, and Current_Error to be processed with the functionality of streams, including the mixing of text and binary input-output, and the mixing of binary input-output for different types.

NOTE 2   Performing operations on the stream associated with a text file does not affect the column, line, or page counts. 


### A.12.3  The Package Wide_Text_IO.Text_Streams

The package Wide_Text_IO.Text_Streams provides a function for treating a wide text file as a stream. 


#### Static Semantics

The library package Wide_Text_IO.Text_Streams has the following declaration: 

```ada
with Ada.Streams;
package Ada.Wide_Text_IO.Text_Streams is
   type Stream_Access is access all Streams.Root_Stream_Type'Class;

```

```ada
   function Stream (File : in File_Type) return Stream_Access;
end Ada.Wide_Text_IO.Text_Streams;

```

The Stream function has the same effect as the corresponding function in Streams.Stream_IO. 


## A.13  Exceptions in Input-Output

The package IO_Exceptions defines the exceptions needed by the predefined input-output packages. 


#### Static Semantics

The library package IO_Exceptions has the following declaration: 

```ada
package Ada.IO_Exceptions is
   pragma Pure(IO_Exceptions);

```

```ada
   Status_Error : exception;
   Mode_Error   : exception;
   Name_Error   : exception;
   Use_Error    : exception;
   Device_Error : exception;
   End_Error    : exception;
   Data_Error   : exception;
   Layout_Error : exception;

```

```ada
end Ada.IO_Exceptions;

```

If more than one error condition exists, the corresponding exception that appears earliest in the following list is the one that is propagated.

The exception Status_Error is propagated by an attempt to operate upon a file that is not open, and by an attempt to open a file that is already open.

The exception Mode_Error is propagated by an attempt to read from, or test for the end of, a file whose current mode is Out_File or Append_File, and also by an attempt to write to a file whose current mode is In_File. In the case of Text_IO, the exception Mode_Error is also propagated by specifying a file whose current mode is Out_File or Append_File in a call of Set_Input, Skip_Line, End_Of_Line, Skip_Page, or End_Of_Page; and by specifying a file whose current mode is In_File in a call of Set_Output, Set_Line_Length, Set_Page_Length, Line_Length, Page_Length, New_Line, or New_Page.

The exception Name_Error is propagated by a call of Create or Open if the string given for the parameter Name does not allow the identification of an external file. For example, this exception is propagated if the string is improper, or, alternatively, if either none or more than one external file corresponds to the string.

The exception Use_Error is propagated if an operation is attempted that is not possible for reasons that depend on characteristics of the external file. For example, this exception is propagated by the procedure Create, among other circumstances, if the given mode is Out_File but the form specifies an input only device, if the parameter Form specifies invalid access rights, or if an external file with the given name already exists and overwriting is not allowed.

The exception Device_Error is propagated if an input-output operation cannot be completed because of a malfunction of the underlying system.

The exception End_Error is propagated by an attempt to skip (read past) the end of a file.

The exception Data_Error can be propagated by the procedure Read (or by the Read attribute) if the element read cannot be interpreted as a value of the required subtype. This exception is also propagated by a procedure Get (defined in the package Text_IO) if the input character sequence fails to satisfy the required syntax, or if the value input does not belong to the range of the required subtype.

The exception Layout_Error is propagated (in text input-output) by Col, Line, or Page if the value returned exceeds Count'Last. The exception Layout_Error is also propagated on output by an attempt to set column or line numbers in excess of specified maximum line or page lengths, respectively (excluding the unbounded cases). It is also propagated by an attempt to Put too many characters to a string.


#### Documentation Requirements

The implementation shall document the conditions under which Name_Error, Use_Error and Device_Error are propagated. 


#### Implementation Permissions

If the associated check is too complex, an implementation need not propagate Data_Error as part of a procedure Read (or the Read attribute) if the value read cannot be interpreted as a value of the required subtype. 

Ramification: An example where the implementation may choose not to perform the check is an enumeration type with a representation clause with "holes" in the range of internal codes.


#### Erroneous Execution

[If the element read by the procedure Read (or by the Read attribute) cannot be interpreted as a value of the required subtype, but this is not detected and Data_Error is not propagated, then the resulting value can be abnormal, and subsequent references to the value can lead to erroneous execution, as explained in 13.9.1. ]


## A.14  File Sharing


#### Dynamic Semantics

It is not specified by the language whether the same external file can be associated with more than one file object. If such sharing is supported by the implementation, the following effects are defined: 

Operations on one text file object do not affect the column, line, and page numbers of any other file object.

Standard_Input and Standard_Output are associated with distinct external files, so operations on one of these files cannot affect operations on the other file. In particular, reading from Standard_Input does not affect the current page, line, and column numbers for Standard_Output, nor does writing to Standard_Output affect the current page, line, and column numbers for Standard_Input.

For direct and stream files, the current index is a property of each file object; an operation on one file object does not affect the current index of any other file object.

For direct and stream files, the current size of the file is a property of the external file. 

All other effects are identical. 


## A.15  The Package Command_Line

The package Command_Line allows a program to obtain the values of its arguments and to set the exit status code to be returned on normal termination. 

Implementation defined: The meaning of Argument_Count, Argument, and Command_Name.


#### Static Semantics

The library package Ada.Command_Line has the following declaration: 

```ada
package Ada.Command_Line is
  pragma Preelaborate(Command_Line);

```

```ada
  function Argument_Count return Natural;

```

```ada
  function Argument (Number : in Positive) return String;

```

```ada
  function Command_Name return String;

```

```ada
  type Exit_Status is implementation-defined integer type;

```

```ada
  Success : constant Exit_Status;
  Failure : constant Exit_Status;

```

```ada
  procedure Set_Exit_Status (Code : in Exit_Status);

```

```ada
private
  ... -- not specified by the language
end Ada.Command_Line;


```

```ada
function Argument_Count return Natural;

```

If the external execution environment supports passing arguments to a program, then Argument_Count returns the number of arguments passed to the program invoking the function. Otherwise it returns 0. The meaning of "number of arguments" is implementation defined.

```ada
function Argument (Number : in Positive) return String;

```

If the external execution environment supports passing arguments to a program, then Argument returns an implementation-defined value corresponding to the argument at relative position Number. If Number is outside the range 1..Argument_Count, then Constraint_Error is propagated. 

Ramification: If the external execution environment does not support passing arguments to a program, then Argument(N) for any N will raise Constraint_Error, since Argument_Count is 0.

```ada
function Command_Name return String;

```

If the external execution environment supports passing arguments to a program, then Command_Name returns an implementation-defined value corresponding to the name of the command invoking the program; otherwise Command_Name returns the null string.

The type Exit_Status represents the range of exit status values supported by the external execution environment. The constants Success and Failure correspond to success and failure, respectively.

```ada
procedure Set_Exit_Status (Code : in Exit_Status);

```

If the external execution environment supports returning an exit status from a program, then Set_Exit_Status sets Code as the status. Normal termination of a program returns as the exit status the value most recently set by Set_Exit_Status, or, if no such value has been set, then the value Success. If a program terminates abnormally, the status set by Set_Exit_Status is ignored, and an implementation-defined exit status value is set.

If the external execution environment does not support returning an exit value from a program, then Set_Exit_Status does nothing. 


#### Implementation Permissions

An alternative declaration is allowed for package Command_Line if different functionality is appropriate for the external execution environment. 

NOTE   Argument_Count, Argument, and Command_Name correspond to the C language's argc, argv[n] (for n&gt0) and argv[0], respectively. 

To be honest: The correspondence of Argument_Count to argc is not direct - argc would be one more than Argument_Count, since the argc count includes the command name, whereas Argument_Count does not. 


#### Extensions to Ada 83

This clause is new in Ada 95. 

Implementation Advice: 

```ada
Discussion:   

  

```

```ada
  

```

```ada
  

```

Version=[5],Kind=(AddedNormal),Group=[C],Term=[container], Def=[a structured object that represents a collection of elements all of the same (potentially class-wide) type, such as a vector or a tree], Note1=[Several predefined container types are provided by the children of package Ada.Containers (see ).]

```ada
Implementation Note:   
  

```

