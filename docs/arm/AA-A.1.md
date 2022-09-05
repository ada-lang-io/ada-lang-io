---
sidebar_position:  118
---

# A.1  The Package Standard

{AI05-0299-1} This subclause outlines the specification of the package Standard containing all predefined identifiers in the language. The corresponding package body is not specified by the language.

The operators that are predefined for the types declared in the package Standard are given in comments since they are implicitly declared. Italics are used for pseudo-names of anonymous types (such as root_real) and for undefined information (such as implementation-defined). 

Ramification: All of the predefined operators are of convention Intrinsic. 


#### Static Semantics

The library package Standard has the following declaration: 

Implementation defined: The names and characteristics of the numeric subtypes declared in the visible part of package Standard.

```ada
{AI12-0414-1} package Standard
   with Pure is

```

```ada
   type Boolean is (False, True);

```

```ada
   -- The predefined relational operators for this type are as follows:

```

```ada
{8652/0028} {AI95-00145-01}    -- function "="   (Left, Right : Boolean'Base) return Boolean;
   -- function "/="  (Left, Right : Boolean'Base) return Boolean;
   -- function "&lt"   (Left, Right : Boolean'Base) return Boolean;
   -- function "&lt="  (Left, Right : Boolean'Base) return Boolean;
   -- function "&gt"   (Left, Right : Boolean'Base) return Boolean;
   -- function "&gt="  (Left, Right : Boolean'Base) return Boolean;

```

```ada
   -- The predefined logical operators and the predefined logical
   -- negation operator are as follows:

```

```ada
{8652/0028} {AI95-00145-01}    -- function "and" (Left, Right : Boolean'Base) return Boolean'Base;
   -- function "or"  (Left, Right : Boolean'Base) return Boolean'Base;
   -- function "xor" (Left, Right : Boolean'Base) return Boolean'Base;

```

```ada
{8652/0028} {AI95-00145-01}    -- function "not" (Right : Boolean'Base) return Boolean'Base;

```

```ada
{AI95-00434-01}    -- The integer type root_integer and the
   -- corresponding universal type universal_integer are predefined.

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
{AI95-00434-01}    -- The floating point type root_real and the
   -- corresponding universal type universal_real are predefined.

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
{AI95-00230-01}    -- The type universal_access is predefined.
   -- The following equality operators are predefined:

```

```ada
{AI95-00230-01}    function "="  (Left, Right: universal_access) return Boolean;
   function "/=" (Left, Right: universal_access) return Boolean;

```

```ada
{AI95-00415-01} {AI05-0181-1} {AI05-0248-1}       -- The declaration of type Character is based on the standard ISO 8859-1 character set.

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
      '¨',	'©',	'ª',	'«',					--168 (16#A8#) .. 171 (16#AB#)
      '¬',	soft_hyphen,	'®',	'¯',				--172 (16#AC#) .. 175 (16#AF#)

      '°',	'±',	'²',	'³',	'´',	'µ',	'¶',	'·',	--176 (16#B0#) .. 183 (16#B7#)
      '¸',	'¹',	'º',	'»',	'¼',	'½',	'¾',	'¿',	--184 (16#B8#) .. 191 (16#BF#)

      'À',	'Á',	'Â',	'Ã',	'Ä',	'Å',	'Æ',	'Ç',	--192 (16#C0#) .. 199 (16#C7#)
      'È',	'É',	'Ê',	'Ë',	'Ì',	'Í',	'Î',	'Ï',	--200 (16#C8#) .. 207 (16#CF#)

      'Ð',	'Ñ',	'Ò',	'Ó',	'Ô',	'Õ',	'Ö',	'×',	--208 (16#D0#) .. 215 (16#D7#)
      'Ø',	'Ù',	'Ú',	'Û',	'Ü',	'Ý',	'Þ',	'ß',	--216 (16#D8#) .. 223 (16#DF#)

      'à',	'á',	'â',	'ã',	'ä',	'å',	'æ',	'ç',	--224 (16#E0#) .. 231 (16#E7#)
      'è',	'é',	'ê',	'ë',	'ì',	'í',	'î',	'ï',	--232 (16#E8#) .. 239 (16#EF#)

      'ð',	'ñ',	'ò',	'ó',	'ô',	'õ',	'ö',	'÷',	--240 (16#F0#) .. 247 (16#F7#)
      'ø',	'ù',	'ú',	'û',	'ü',	'ý',	'þ',	'ÿ');--248 (16#F8#) .. 255 (16#FF#)

```

```ada
   -- The predefined operators for the type Character are the same as for
   -- any enumeration type.


```

```ada
{AI95-00395-01} {AI05-0266-1} {AI12-0263-1}    -- The declaration of type Wide_Character is based on the standard ISO/IEC 10646:2017 BMP character
   -- set. The first 256 positions have the same contents as type Character. See 3.5.2.

   type Wide_Character is (nul, soh ... Hex_0000FFFE, Hex_0000FFFF);

```

```ada
{AI95-00285-01} {AI95-00395-01} {AI05-0266-1} {AI12-0263-1}    -- The declaration of type Wide_Wide_Character is based on the full
   -- ISO/IEC 10646:2017 character set. The first 65536 positions have the
   -- same contents as type Wide_Character. See 3.5.2.

   type Wide_Wide_Character is (nul, soh ... Hex_7FFFFFFE, Hex_7FFFFFFF);
   for Wide_Wide_Character'Size use 32;

```

```ada
   package ASCII is ... end ASCII;  --Obsolescent; see J.5



```

```ada
{AI05-0229-1}    -- Predefined string types:

   type String is array(Positive range &lt&gt) of Character
      with Pack;

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
{AI05-0229-1}    type Wide_String is array(Positive range &lt&gt) of Wide_Character
      with Pack;

```

```ada
   -- The predefined operators for this type correspond to those for String.

```

```ada
{AI95-00285-01} {AI05-0229-1}    type Wide_Wide_String is array (Positive range &lt&gt)
      of Wide_Wide_Character
         with Pack;

```

```ada
{AI95-00285-01}    -- The predefined operators for this type correspond to those for String.

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

{AI95-00285-01} In each of the types Character, Wide_Character, and Wide_Wide_Character, the character literals for the space character (position 32) and the non-breaking space character (position 160) correspond to different values. Unless indicated otherwise, each occurrence of the character literal ' ' in this Reference Manual refers to the space character. Similarly, the character literals for hyphen (position 45) and soft hyphen (position 173) correspond to different values. Unless indicated otherwise, each occurrence of the character literal '' in this Reference Manual refers to the hyphen character. 


#### Dynamic Semantics

Elaboration of the body of Standard has no effect. 

Discussion: Note that the language does not define where this body appears in the environment [declarative_part](./AA-3.11#S0086) - see 10, "Program Structure and Compilation Issues". 


#### Implementation Permissions

An implementation may provide additional predefined integer types and additional predefined floating point types. Some or all of these types may be anonymous. 

To be honest: An implementation may add representation items to package Standard, for example to specify the internal codes of type Boolean, or the Small of type Duration.


#### Implementation Advice

If an implementation provides additional named predefined integer types, then the names should end with "Integer" as in "Long_Integer". If an implementation provides additional named predefined floating point types, then the names should end with "Float" as in "Long_Float". 

Implementation Advice: If an implementation provides additional named predefined integer types, then the names should end with "Integer". If an implementation provides additional named predefined floating point types, then the names should end with "Float".

NOTE 1   Certain aspects of the predefined entities cannot be completely described in the language itself. For example, although the enumeration type Boolean can be written showing the two enumeration literals False and True, the short-circuit control forms cannot be expressed in the language.

NOTE 2   As explained in 8.1, "Declarative Region" and 10.1.4, "The Compilation Process", the declarative region of the package Standard encloses every library unit and consequently the main subprogram; the declaration of every library unit is assumed to occur within this declarative region. [Library_item](./AA-10.1#S0287)s are assumed to be ordered in such a way that there are no forward semantic dependences. However, as explained in 8.3, "Visibility", the only library units that are visible within a given compilation unit are the library units named by all [with_clause](./AA-10.1#S0294)s that apply to the given unit, and moreover, within the declarative region of a given library unit, that library unit itself.

NOTE 3   If all [block_statement](./AA-5.6#S0191)s of a program are named, then the name of each program unit can always be written as an expanded name starting with Standard (unless Standard is itself hidden). The name of a library unit cannot be a homograph of a name (such as Integer) that is already declared in Standard.

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


#### Extensions to Ada 95

{AI95-00285-01} Types Wide_Wide_Character and Wide_Wide_String are new. 

Discussion: The inconsistencies associated with these types are documented in 3.5.2 and 3.6.3. 

{AI95-00230-01} Type universal_access and the equality operations for it are new. 


#### Wording Changes from Ada 95

{8652/0028} {AI95-00145-01} Corrigendum: Corrected the parameter type for the Boolean operators declared in Standard. 


#### Wording Changes from Ada 2005

{AI05-0181-1} Correction: Since soft_hyphen (position 173) is defined to be nongraphic, gave it a name. 

Discussion: The inconsistencies associated with this change are documented in 3.5. 

