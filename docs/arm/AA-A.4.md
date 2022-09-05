---
sidebar_position:  121
---

# A.4  String Handling

{AI95-00285-01} {AI05-0299-1} This subclause presents the specifications of the package Strings and several child packages, which provide facilities for dealing with string data. Fixed-length, bounded-length, and unbounded-length strings are supported, for String, Wide_String, and Wide_Wide_String. The string-handling subprograms include searches for pattern strings and for characters in program-specified sets, translation (via a character-to-character mapping), and transformation (replacing, inserting, overwriting, and deleting of substrings). 


#### Extensions to Ada 83

{AI05-0299-1} This subclause is new to Ada 95. 


#### Wording Changes from Ada 95

{AI95-00285-01} Included Wide_Wide_String in this description; the individual changes are documented as extensions as needed. 


## A.4.1  The Package Strings

The package Strings provides declarations common to the string handling packages. 


#### Static Semantics

The library package Strings has the following declaration: 

```ada
{AI12-0414-1} package Ada.Strings
   with Pure is

```

```ada
{AI95-00285-01}    Space      : constant Character      := ' ';
   Wide_Space : constant Wide_Character := ' ';
   Wide_Wide_Space : constant Wide_Wide_Character := ' ';

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


#### Incompatibilities With Ada 95

{AI95-00285-01} {AI05-0005-1} Constant Wide_Wide_Space is added to Ada.Strings. If Ada.Strings is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) of Wide_Wide_Space is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


## A.4.2  The Package Strings.Maps

{AI12-0445-1} The package Strings.Maps defines the types, operations, and other entities necessary for character sets and character-to-character mappings. 


#### Static Semantics

The library package Strings.Maps has the following declaration: 

```ada
{AI95-00362-01} {AI12-0414-1} package Ada.Strings.Maps
   with Pure is

```

```ada
{AI95-00161-01} {AI12-0399-1}    -- Representation for a set of character values:
   type Character_Set is private
      with Preelaborable_Initialization;

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
{AI95-00161-01} {AI12-0399-1}    -- Representation for a character to character mapping:
   type Character_Mapping is private
      with Preelaborable_Initialization;

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

{AI05-0264-1} If Ranges'Length=0 then Null_Set is returned; otherwise, the returned value represents the set corresponding to Ranges.

```ada
function To_Set (Span : in Character_Range) return Character_Set;

```

The returned value represents the set containing each character in Span. 

```ada
function To_Ranges (Set : in Character_Set) return Character_Ranges;

```

{AI05-0264-1} If Set = Null_Set, then an empty Character_Ranges array is returned; otherwise, the shortest array of contiguous ranges of Character values in Set, in increasing order of Low, is returned.

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

{8652/0048} {AI95-00151-01} To_Range returns the Character_Sequence value R, such that if D = To_Domain(Map), then R has the same bounds as D, and D(I) maps to R(I) for each I in D'Range. 

An object F of type Character_Mapping_Function maps a Character value C to the Character value F.all(C), which is said to match C with respect to mapping function F. 

NOTE 1   Character_Mapping and Character_Mapping_Function are used both for character equivalence mappings in the search subprograms (such as for case insensitivity) and as transformational mappings in the Translate subprograms.

NOTE 2   To_Domain(Identity) and To_Range(Identity) each returns the null string. 

Reason: Package Strings.Maps is not pure, since it declares an access-to-subprogram type. 


#### Examples

{AI12-0429-1} Example of use of Strings.Maps.To_Mapping:

To_Mapping("ABCD", "ZZAB") returns a Character_Mapping that maps 'A' and 'B' to 'Z', 'C' to 'A', 'D' to 'B', and each other Character to itself. 


#### Extensions to Ada 95

{AI95-00161-01} Amendment Correction: Added [pragma](./AA-2.8#S0019) Preelaborable_Initialization to types Character_Set and Character_Mapping, so that they can be used to declare default-initialized objects in preelaborated units.

{AI95-00362-01} Strings.Maps is now Pure, so it can be used in pure units. 


#### Wording Changes from Ada 95

{8652/0048} {AI95-00151-01} Corrigendum: Corrected the definition of the range of the result of To_Range, since the Ada 95 definition makes no sense. 


## A.4.3  Fixed-Length String Handling

The language-defined package Strings.Fixed provides string-handling subprograms for fixed-length strings; that is, for values of type Standard.String. Several of these subprograms are procedures that modify the contents of a String that is passed as an out or an in out parameter; each has additional parameters to control the effect when the logical length of the result differs from the parameter's length.

For each function that returns a String, the lower bound of the returned value is 1. 

Discussion: {AI95-00114-01} Most operations that yield a String are provided both as a function and as a procedure. The functional form is possibly a more aesthetic style but may introduce overhead due to extra copying or dynamic memory usage in some implementations. Thus a procedural form, with an in out parameter so that all copying is done `in place', is also supplied.

The basic model embodied in the package is that a fixed-length string comprises significant characters and possibly padding (with space characters) on either or both ends. When a shorter string is copied to a longer string, padding is inserted, and when a longer string is copied to a shorter one, padding is stripped. The Move procedure in Strings.Fixed, which takes a String as an out parameter, allows the programmer to control these effects. Similar control is provided by the string transformation procedures. 


#### Static Semantics

The library package Strings.Fixed has the following declaration: 

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Strings.Maps;
package Ada.Strings.Fixed
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

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
{AI95-00301-01}    function Index (Source  : in String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Direction := Forward;
                   Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Natural;

```

```ada
{AI95-00301-01}    function Index (Source  : in String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Direction := Forward;
                   Mapping : in Maps.Character_Mapping_Function)
      return Natural;

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
{AI95-00301-01}    function Index (Source  : in String;
                   Set     : in Maps.Character_Set;
                   From    : in Positive;
                   Test    : in Membership := Inside;
                   Going   : in Direction := Forward)
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
{AI95-00301-01}    function Index_Non_Blank (Source : in String;
                             From   : in Positive;
                             Going  : in Direction := Forward)
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
{AI05-0031-1}    procedure Find_Token (Source : in String;
                         Set    : in Maps.Character_Set;
                         From   : in Positive;
                         Test   : in Membership;
                         First  : out Positive;
                         Last   : out Natural);

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

{AI05-0264-1} The Move procedure copies characters from Source to Target. If Source has the same length as Target, then the effect is to assign Source to Target. If Source is shorter than Target, then: 

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
function Index (Source  : in String;
                Pattern : in String;
                From    : in Positive;
                Going   : in Direction := Forward;
                Mapping : in Maps.Character_Mapping := Maps.Identity)
   return Natural;

function Index (Source  : in String;
                Pattern : in String;
                From    : in Positive;
                Going   : in Direction := Forward;
                Mapping : in Maps.Character_Mapping_Function)
   return Natural;

```

{AI95-00301-01} {AI05-0056-1} Each Index function searches, starting from From, for a slice of Source, with length Pattern'Length, that matches Pattern with respect to Mapping; the parameter Going indicates the direction of the lookup. If Source is the null string, Index returns 0; otherwise, if From is not in Source'Range, then Index_Error is propagated. If Going = Forward, then Index returns the smallest index I which is greater than or equal to From such that the slice of Source starting at I matches Pattern. If Going = Backward, then Index returns the largest index I such that the slice of Source starting at I matches Pattern and has an upper bound less than or equal to From. If there is no such slice, then 0 is returned. If Pattern is the null string, then Pattern_Error is propagated.

Discussion: There is no default parameter for From; the default value would need to depend on other parameters (the bounds of Source and the direction Going). It is better to use overloaded functions rather than a special value to represent the default.

There is no default value for the Mapping parameter that is a Character_Mapping_Function; if there were, a call would be ambiguous since there is also a default for the Mapping parameter that is a Character_Mapping.

{AI05-0056-1} The language does not define when the Pattern_Error check is made. (That's because many common searching implementations require a nonempty pattern) That means that the result for a call like Index ("", "") could be 0 or could raise Pattern_Error. Similarly, in the call Index ("", "", From =&gt 2), the language does not define whether Pattern_Error or Index_Error is raised.

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

{AI95-00301-01} If Going = Forward, returns 

```ada
      Index (Source, Pattern, Source'First, Forward, Mapping);

```

{AI05-0264-1} otherwise, returns 

```ada
      Index (Source, Pattern, Source'Last, Backward, Mapping);

```

This paragraph was deleted.There is no default value for the Mapping parameter that is a Character_Mapping_Function; if there were, a call would be ambiguous since there is also a default for the Mapping parameter that is a Character_Mapping. 

```ada
function Index (Source  : in String;
                Set     : in Maps.Character_Set;
                From    : in Positive;
                Test    : in Membership := Inside;
                Going   : in Direction := Forward)
   return Natural;

```

{AI95-00301-01} {AI05-0056-1} Index searches for the first or last occurrence of any of a set of characters (when Test=Inside), or any of the complement of a set of characters (when Test=Outside). If Source is the null string, Index returns 0; otherwise, if From is not in Source'Range, then Index_Error is propagated. Otherwise, it returns the smallest index I &gt= From (if Going=Forward) or the largest index I &lt= From (if Going=Backward) such that Source(I) satisfies the Test condition with respect to Set; it returns 0 if there is no such Character in Source.

```ada
function Index (Source : in String;
                Set    : in Maps.Character_Set;
                Test   : in Membership := Inside;
                Going  : in Direction  := Forward)
   return Natural;

```

{AI95-00301-01} If Going = Forward, returns 

```ada
      Index (Source, Set, Source'First, Test, Forward);

```

{AI05-0264-1} otherwise, returns 

```ada
      Index (Source, Set, Source'Last, Test, Backward);

```

```ada
function Index_Non_Blank (Source : in String;
                          From   : in Positive;
                          Going  : in Direction := Forward)
   return Natural;

```

{AI95-00301-01} Returns Index (Source, Maps.To_Set(Space), From, Outside, Going);

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
                      From   : in Positive;
                      Test   : in Membership;
                      First  : out Positive;
                      Last   : out Natural);

```

{AI05-0031-1} If Source is not the null string and From is not in Source'Range, then Index_Error is raised. Otherwise, First is set to the index of the first character in Source(From .. Source'Last) that satisfies the Test condition. Last is set to the largest index such that all characters in Source(First .. Last) satisfy the Test condition. If no characters in Source(From .. Source'Last) satisfy the Test condition, First is set to From, and Last is set to 0.

```ada
procedure Find_Token (Source : in String;
                      Set    : in Maps.Character_Set;
                      Test   : in Membership;
                      First  : out Positive;
                      Last   : out Natural);

```

{8652/0049} {AI95-00128-01} {AI05-0031-1} Equivalent to Find_Token (Source, Set, Source'First, Test, First, Last).

Ramification: {AI05-0031-1} If Source'First is not in Positive, which can only happen for an empty string, this will raise Constraint_Error. 

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

{8652/0049} {AI95-00128-01} If Low &gt Source'Last+1, or High &lt Source'First1, then Index_Error is propagated. Otherwise:

{8652/0049} {AI95-00128-01} If High &gt= Low, then the returned string comprises Source(Source'First..Low1) & By & Source(High+1..Source'Last), but with lower bound 1.

{8652/0049} {AI95-00128-01} If High &lt Low, then the returned string is Insert(Source, Before=&gtLow, New_Item=&gtBy). 

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

{AI05-0264-1} Propagates Index_Error if Before is not in Source'First .. Source'Last+1; otherwise, returns Source(Source'First..Before1) & New_Item & Source(Before..Source'Last), but with lower bound 1.

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

{AI05-0264-1} Propagates Index_Error if Position is not in Source'First .. Source'Last+1; otherwise, returns the string obtained from Source by consecutively replacing characters starting at Position with corresponding characters from New_Item. If the end of Source is reached before the characters in New_Item are exhausted, the remaining characters from New_Item are appended to the string.

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

{8652/0049} {AI95-00128-01} {AI05-0264-1} If From &lt= Through, the returned string is Replace_Slice(Source, From, Through, ""); otherwise, it is Source with lower bound 1.

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

{AI05-0264-1} Returns a string of length Count. If Count &lt= Source'Length, the string comprises the first Count characters of Source. Otherwise, its contents are Source concatenated with CountSource'Length Pad characters.

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

{AI05-0264-1} Returns a string of length Count. If Count &lt= Source'Length, the string comprises the last Count characters of Source. Otherwise, its contents are Count-Source'Length Pad characters concatenated with Source.

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

{8652/0049} {AI95-00128-01} These functions replicate a character or string a specified number of times. The first function returns a string whose length is Left and each of whose elements is Right. The second function returns a string whose length is Left*Right'Length and whose value is the null string if Left = 0 and otherwise is (Left1)*Right & Right with lower bound 1. 

NOTE 1   {AI05-0264-1} {AI12-0442-1} In the Index and Count functions taking Pattern and Mapping parameters, for there to be a match, the actual String parameter passed to Pattern can contain only characters occurring as target characters of the mapping.

NOTE 2   In the Insert subprograms, inserting at the end of a string is obtained by passing Source'Last+1 as the Before parameter.

NOTE 3   If a null Character_Mapping_Function is passed to any of the string handling subprograms, Constraint_Error is propagated. 


#### Incompatibilities With Ada 95

{AI95-00301-01} {AI05-0005-1} Overloaded versions of Index and Index_Non_Blank are added to Strings.Fixed. If Strings.Fixed is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) of Index or Index_Non_Blank is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Wording Changes from Ada 95

{8652/0049} {AI95-00128-01} Corrigendum: Clarified that Find_Token may raise Constraint_Error if Source'First is not in Positive (which is only possible for a null string).

{8652/0049} {AI95-00128-01} Corrigendum: Clarified that Replace_Slice, Delete, and "*" always return a string with lower bound 1. 


#### Incompatibilities With Ada 2005

{AI05-0031-1} An overloaded version of Find_Token is added to Strings.Fixed. If Strings.Fixed is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) of Find_Token is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Wording Changes from Ada 2005

{AI05-0056-1} Correction: Clarified that Index never raises Index_Error if the source string is null. 


## A.4.4  Bounded-Length String Handling

{AI12-0445-1} The language-defined package Strings.Bounded provides a generic package each of whose instances yields a private type Bounded_String and a set of operations. An object of a particular Bounded_String type represents a String whose low bound is 1 and whose length can vary conceptually between 0 and a maximum size established at the generic instantiation. The subprograms for fixed-length string handling are either overloaded directly for Bounded_String, or are modified as necessary to reflect the variability in length. Additionally, since the Bounded_String type is private, appropriate constructor and selector operations are provided. 

Reason: Strings.Bounded declares an inner generic package, versus itself being directly a generic child of Strings, in order to retain compatibility with a version of the string-handling packages that is generic with respect to the character and string types.

Reason: The bound of a bounded-length string is specified as a parameter to a generic, versus as the value for a discriminant, because of the inappropriateness of assignment and equality of discriminated types for the copying and comparison of bounded strings.


#### Static Semantics

The library package Strings.Bounded has the following declaration: 

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Strings.Maps;
package Ada.Strings.Bounded
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

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
{AI95-00301-01}       procedure Set_Bounded_String
         (Target :    out Bounded_String;
          Source : in     String;
          Drop   : in     Truncation := Error);

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
{AI95-00301-01}       function Bounded_Slice
         (Source : in Bounded_String;
          Low    : in Positive;
          High   : in Natural)
             return Bounded_String;

```

```ada
{AI95-00301-01}       procedure Bounded_Slice
         (Source : in     Bounded_String;
          Target :    out Bounded_String;
          Low    : in     Positive;
          High   : in     Natural);

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
{AI95-00301-01}    -- Search subprograms

```

```ada
{AI95-00301-01}       function Index (Source  : in Bounded_String;
                      Pattern : in String;
                      From    : in Positive;
                      Going   : in Direction := Forward;
                      Mapping : in Maps.Character_Mapping := Maps.Identity)
         return Natural;

```

```ada
{AI95-00301-01}       function Index (Source  : in Bounded_String;
                      Pattern : in String;
                      From    : in Positive;
                      Going   : in Direction := Forward;
                      Mapping : in Maps.Character_Mapping_Function)
         return Natural;

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
{AI95-00301-01}       function Index (Source  : in Bounded_String;
                      Set     : in Maps.Character_Set;
                      From    : in Positive;
                      Test    : in Membership := Inside;
                      Going   : in Direction := Forward)
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
{AI95-00301-01}       function Index_Non_Blank (Source : in Bounded_String;
                                From   : in Positive;
                                Going  : in Direction := Forward)
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
{AI05-0031-1}       procedure Find_Token (Source : in Bounded_String;
                            Set    : in Maps.Character_Set;
                            From   : in Positive;
                            Test   : in Membership;
                            First  : out Positive;
                            Last   : out Natural);

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

This paragraph was deleted.{8652/0097} {AI95-00115-01} {AI95-00344-01} 

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

{AI05-0264-1} If Source'Length &lt= Max_Length, then this function returns a Bounded_String that represents Source. Otherwise, the effect depends on the value of Drop: 

If Drop=Left, then the result is a Bounded_String that represents the string comprising the rightmost Max_Length characters of Source.

If Drop=Right, then the result is a Bounded_String that represents the string comprising the leftmost Max_Length characters of Source.

If Drop=Error, then Strings.Length_Error is propagated. 

```ada
function To_String (Source : in Bounded_String) return String;

```

To_String returns the String value with lower bound 1 represented by Source. If B is a Bounded_String, then B = To_Bounded_String(To_String(B)).

```ada
procedure Set_Bounded_String
   (Target :    out Bounded_String;
    Source : in     String;
    Drop   : in     Truncation := Error);

```

{AI95-00301-01} Equivalent to Target := To_Bounded_String (Source, Drop);

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

{8652/0049} {AI95-00128-01} {AI95-00238-01} Returns the slice at positions Low through High in the string represented by Source; propagates Index_Error if Low &gt Length(Source)+1 or High &gt Length(Source). The bounds of the returned string are Low and High.

```ada
function Bounded_Slice
   (Source : in Bounded_String;
    Low    : in Positive;
    High   : in Natural)
       return Bounded_String;

```

{AI95-00301-01} Returns the slice at positions Low through High in the string represented by Source as a bounded string; propagates Index_Error if Low &gt Length(Source)+1 or High &gt Length(Source).

```ada
procedure Bounded_Slice
   (Source : in     Bounded_String;
    Target :    out Bounded_String;
    Low    : in     Positive;
    High   : in     Natural);

```

{AI95-00301-01} Equivalent to Target := Bounded_Slice (Source, Low, High);

Each of the functions "=", "&lt", "&gt", "&lt=", and "&gt=" returns the same result as the corresponding String operation applied to the String values given or represented by the two parameters.

Each of the search subprograms (Index, Index_Non_Blank, Count, Find_Token) has the same effect as the corresponding subprogram in Strings.Fixed applied to the string represented by the Bounded_String parameter.

Each of the Translate subprograms, when applied to a Bounded_String, has an analogous effect to the corresponding subprogram in Strings.Fixed. For the Translate function, the translation is applied to the string represented by the Bounded_String parameter, and the result is converted (via To_Bounded_String) to a Bounded_String. For the Translate procedure, the string represented by the Bounded_String parameter after the translation is given by the Translate function for fixed-length strings applied to the string represented by the original value of the parameter.

{8652/0049} {AI95-00128-01} Each of the transformation subprograms (Replace_Slice, Insert, Overwrite, Delete), selector subprograms (Trim, Head, Tail), and constructor functions ("*") has an effect based on its corresponding subprogram in Strings.Fixed, and Replicate is based on Fixed."*". In the case of a function, the corresponding fixed-length string subprogram is applied to the string represented by the Bounded_String parameter. To_Bounded_String is applied the result string, with Drop (or Error in the case of Generic_Bounded_Length."*") determining the effect when the string length exceeds Max_Length. In the case of a procedure, the corresponding function in Strings.Bounded.Generic_Bounded_Length is applied, with the result assigned into the Source parameter. 

Ramification: {AI95-00114-01} The "/=" operations between Bounded_String and String, and between String and Bounded_String, are automatically defined based on the corresponding "=" operations. 


#### Implementation Advice

Bounded string objects should not be implemented by implicit pointers and dynamic allocation. 

Implementation Advice: Bounded string objects should not be implemented by implicit pointers and dynamic allocation.

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


#### Inconsistencies With Ada 95

{AI95-00238-01} Amendment Correction: The bounds of the string returned from Slice are now defined. This is technically an inconsistency; if a program depended on some other lower bound for the string returned from Slice, it could fail when compiled with Ada 2005. Such code is not portable even between Ada 95 implementations, so it should be very rare. 


#### Incompatibilities With Ada 95

{AI95-00301-01} {AI05-0005-1} Procedure Set_Bounded_String, two Bounded_Slice subprograms, and overloaded versions of Index and Index_Non_Blank are added to Strings.Bounded.Generic_Bounded_Length. If an instance of Generic_Bounded_Length is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the [defining_identifier](./AA-3.1#S0022) as a new entity in Generic_Bounded_Length is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Wording Changes from Ada 95

{8652/0049} {AI95-00128-01} Corrigendum: Corrected the conditions for which Slice raises Index_Error.

{8652/0049} {AI95-00128-01} Corrigendum: Clarified the meaning of transformation, selector, and constructor subprograms by describing the effects of procedures and functions separately. 


#### Incompatibilities With Ada 2005

{AI05-0031-1} An overloaded version of Find_Token is added to Strings.Bounded.Generic_Bounded_Length. If an instance of Generic_Bounded_Length is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) of Find_Token is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


## A.4.5  Unbounded-Length String Handling

{AI12-0445-1} The language-defined package Strings.Unbounded provides a private type Unbounded_String and a set of operations. An object of type Unbounded_String represents a String whose low bound is 1 and whose length can vary conceptually between 0 and Natural'Last. The subprograms for fixed-length string handling are either overloaded directly for Unbounded_String, or are modified as necessary to reflect the flexibility in length. Since the Unbounded_String type is private, relevant constructor and selector operations are provided. 

Reason: The transformation operations for fixed- and bounded-length strings that are not necessarily length preserving are supplied for Unbounded_String as procedures as well as functions. This allows an implementation to do an initial allocation for an unbounded string and to avoid further allocations as long as the length does not exceed the allocated length. 


#### Static Semantics

The library package Strings.Unbounded has the following declaration: 

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Strings.Maps;
package Ada.Strings.Unbounded
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI95-00161-01} {AI12-0399-1}    type Unbounded_String is private
      with Preelaborable_Initialization;

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
{AI95-00301-01}    procedure Set_Unbounded_String
     (Target :    out Unbounded_String;
      Source : in     String);

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
{AI95-00301-01}    function Unbounded_Slice
      (Source : in Unbounded_String;
       Low    : in Positive;
       High   : in Natural)
          return Unbounded_String;

```

```ada
{AI95-00301-01}    procedure Unbounded_Slice
      (Source : in     Unbounded_String;
       Target :    out Unbounded_String;
       Low    : in     Positive;
       High   : in     Natural);

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
{AI95-00301-01}    function Index (Source  : in Unbounded_String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Direction := Forward;
                   Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Natural;

```

```ada
{AI95-00301-01}    function Index (Source  : in Unbounded_String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Direction := Forward;
                   Mapping : in Maps.Character_Mapping_Function)
      return Natural;

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
{AI95-00301-01}    function Index (Source  : in Unbounded_String;
                   Set     : in Maps.Character_Set;
                   From    : in Positive;
                   Test    : in Membership := Inside;
                   Going    : in Direction := Forward)
      return Natural;

```

```ada
   function Index (Source : in Unbounded_String;
                   Set    : in Maps.Character_Set;
                   Test   : in Membership := Inside;
                   Going  : in Direction  := Forward) return Natural;

```

```ada
{AI95-00301-01}    function Index_Non_Blank (Source : in Unbounded_String;
                             From   : in Positive;
                             Going  : in Direction := Forward)
      return Natural;

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
{AI05-0031-1}    procedure Find_Token (Source : in Unbounded_String;
                         Set    : in Maps.Character_Set;
                         From   : in Positive;
                         Test   : in Membership;
                         First  : out Positive;
                         Last   : out Natural);

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

{AI95-00360-01} The type Unbounded_String needs finalization (see 7.6).

Null_Unbounded_String represents the null String. If an object of type Unbounded_String is not otherwise initialized, it will be initialized to the same value as Null_Unbounded_String.

The function Length returns the length of the String represented by Source.

The type String_Access provides a (nonprivate) access type for explicit processing of unbounded-length strings. The procedure Free performs an unchecked deallocation of an object of type String_Access.

The function To_Unbounded_String(Source : in String) returns an Unbounded_String that represents Source. The function To_Unbounded_String(Length : in Natural) returns an Unbounded_String that represents an uninitialized String whose length is Length.

The function To_String returns the String with lower bound 1 represented by Source. To_String and To_Unbounded_String are related as follows: 

If S is a String, then To_String(To_Unbounded_String(S)) = S.

If U is an Unbounded_String, then To_Unbounded_String(To_String(U)) = U. 

{AI95-00301-01} The procedure Set_Unbounded_String sets Target to an Unbounded_String that represents Source.

For each of the Append procedures, the resulting string represented by the Source parameter is given by the concatenation of the original value of Source and the value of New_Item.

Each of the "&" functions returns an Unbounded_String obtained by concatenating the string or character given or represented by one of the parameters, with the string or character given or represented by the other parameter, and applying To_Unbounded_String to the concatenation result string.

Ramification: {AI12-0005-1} If the resulting string is longer than Natural'Last, Constraint_Error is raised because the upper bound of the underlying String concatenation is outside of the range of the index subtype of Natural (see 4.5.3). Note that the same is true for other operations that attempt to create an overlong string: either they are defined in terms of this concatenation operation (as with Append) or they are defined in terms of Ada.Strings.Fixed operations (as with Insert and Replace_Slice), which themselves are defined in terms of String concatenation (which raises Constraint_Error for overlong strings as described above). Therefore, it is never possible to create an Unbounded_String with a length greater than Natural'Last. 

The Element, Replace_Element, and Slice subprograms have the same effect as the corresponding bounded-length string subprograms.

{AI95-00301-01} {AI05-0262-1} The function Unbounded_Slice returns the slice at positions Low through High in the string represented by Source as an Unbounded_String. The procedure Unbounded_Slice sets Target to the Unbounded_String representing the slice at positions Low through High in the string represented by Source. Both subprograms propagate Index_Error if Low &gt Length(Source)+1 or High &gt Length(Source).

Each of the functions "=", "&lt", "&gt", "&lt=", and "&gt=" returns the same result as the corresponding String operation applied to the String values given or represented by Left and Right.

Each of the search subprograms (Index, Index_Non_Blank, Count, Find_Token) has the same effect as the corresponding subprogram in Strings.Fixed applied to the string represented by the Unbounded_String parameter.

The Translate function has an analogous effect to the corresponding subprogram in Strings.Fixed. The translation is applied to the string represented by the Unbounded_String parameter, and the result is converted (via To_Unbounded_String) to an Unbounded_String.

Each of the transformation functions (Replace_Slice, Insert, Overwrite, Delete), selector functions (Trim, Head, Tail), and constructor functions ("*") is likewise analogous to its corresponding subprogram in Strings.Fixed. For each of the subprograms, the corresponding fixed-length string subprogram is applied to the string represented by the Unbounded_String parameter, and To_Unbounded_String is applied the result string.

For each of the procedures Translate, Replace_Slice, Insert, Overwrite, Delete, Trim, Head, and Tail, the resulting string represented by the Source parameter is given by the corresponding function for fixed-length strings applied to the string represented by Source's original value. 


#### Implementation Requirements

No storage associated with an Unbounded_String object shall be lost upon assignment or scope exit. 

Implementation Note: {AI95-00301-01} A sample implementation of the private part of the package and several of the subprograms appears in the Ada 95 Rationale.


#### Incompatibilities With Ada 95

{AI95-00360-01} Amendment Correction: Type Unbounded_String is defined to need finalization. If the restriction No_Nested_Finalization (see D.7) applies to the partition, and Unbounded_String does not have a controlled part, it will not be allowed in local objects in Ada 2005 whereas it would be allowed in original Ada 95. Such code is not portable, as most Ada compilers have a controlled part in Unbounded_String, and thus would be illegal.

{AI95-00301-01} {AI05-0005-1} Procedure Set_Unbounded_String, two Unbounded_Slice subprograms, and overloaded versions of Index and Index_Non_Blank are added to Strings.Unbounded. If Strings.Unbounded is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity in Strings.Unbounded is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 95

{AI95-00161-01} Amendment Correction: Added a [pragma](./AA-2.8#S0019) Preelaborable_Initialization to type Unbounded_String, so that it can be used to declare default-initialized objects in preelaborated units. 


#### Incompatibilities With Ada 2005

{AI05-0031-1} An overloaded version of Find_Token is added to Strings.Unbounded. If Strings.Unbounded is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) of Find_Token is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


## A.4.6  String-Handling Sets and Mappings

The language-defined package Strings.Maps.Constants declares Character_Set and Character_Mapping constants corresponding to classification and conversion functions in package Characters.Handling. 

Discussion: The Constants package is a child of Strings.Maps since it needs visibility of the private part of Strings.Maps in order to initialize the constants in a preelaborable way (i.e. via aggregates versus function calls). 


#### Static Semantics

The library package Strings.Maps.Constants has the following declaration:

```ada
{AI95-00362-01} {AI12-0414-1} package Ada.Strings.Maps.Constants
   with Pure is

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

NOTE   {AI05-0114-1} There are certain characters which are defined to be lower case letters by ISO 10646 and are therefore allowed in identifiers, but are not considered lower case letters by Ada.Strings.Maps.Constants.

Reason: This is to maintain runtime compatibility with the Ada 95 definitions of these constants; existing correct programs could break if the definitions were changed in a way the programs did not anticipate. 


#### Extensions to Ada 95

{AI95-00362-01} Strings.Maps.Constants is now Pure, so it can be used in pure units. 


#### Wording Changes from Ada 2005

{AI05-0114-1} Correction: Added a note to clarify that these constants don't have any relationship to the characters allowed in identifiers. 


## A.4.7  Wide_String Handling

{AI95-00302-03} {AI05-0286-1} Facilities for handling strings of Wide_Character elements are found in the packages Strings.Wide_Maps, Strings.Wide_Fixed, Strings.Wide_Bounded, Strings.Wide_Unbounded, and Strings.Wide_Maps.Wide_Constants, and in the library functions Strings.Wide_Hash, Strings.Wide_Fixed.Wide_Hash, Strings.Wide_Bounded.Wide_Hash, Strings.Wide_Unbounded.Wide_Hash, Strings.Wide_Hash_Case_Insensitive, Strings.Wide_Fixed.Wide_Hash_Case_Insensitive, Strings.Wide_Bounded.Wide_Hash_Case_Insensitive, Strings.Wide_Unbounded.Wide_Hash_Case_Insensitive, Strings.Wide_Equal_Case_Insensitive, Strings.Wide_Fixed.Wide_Equal_Case_Insensitive, Strings.Wide_Bounded.Wide_Equal_Case_Insensitive, and Strings.Wide_Unbounded.Wide_Equal_Case_Insensitive. They provide the same string-handling operations as the corresponding packages and functions for strings of Character elements. 


#### Static Semantics

The package Strings.Wide_Maps has the following declaration. 

```ada
{AI12-0241-1} {AI12-0302-1} package Ada.Strings.Wide_Maps
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI95-00161-01} {AI12-0399-1}    -- Representation for a set of Wide_Character values:
   type Wide_Character_Set is private
      with Preelaborable_Initialization;

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
{AI95-00161-01} {AI12-0399-1}    -- Representation for a Wide_Character to Wide_Character mapping:
   type Wide_Character_Mapping is private
       with Preelaborable_Initialization;

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

{AI05-0223-1} Types Wide_Character_Set and Wide_Character_Mapping need finalization.

{AI95-00302-03} {AI05-0286-1} For each of the packages Strings.Fixed, Strings.Bounded, Strings.Unbounded, and Strings.Maps.Constants, and for library functions Strings.Hash, Strings.Fixed.Hash, Strings.Bounded.Hash, Strings.Unbounded.Hash, Strings.Hash_Case_Insensitive, Strings.Fixed.Hash_Case_Insensitive, Strings.Bounded.Hash_Case_Insensitive, Strings.Unbounded.Hash_Case_Insensitive, Strings.Equal_Case_Insensitive, Strings.Fixed.Equal_Case_Insensitive, Strings.Bounded.Equal_Case_Insensitive, and Strings.Unbounded.Equal_Case_Insensitive, the corresponding wide string package or function has the same contents except that 

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

{AI95-00301-01} Set_Bounded_Wide_String replaces Set_Bounded_String

Unbounded_Wide_String replaces Unbounded_String

Null_Unbounded_Wide_String replaces Null_Unbounded_String

Wide_String_Access replaces String_Access

To_Unbounded_Wide_String replaces To_Unbounded_String

{AI95-00301-01} Set_Unbounded_Wide_String replaces Set_Unbounded_String

The following additional declaration is present in Strings.Wide_Maps.Wide_Constants: 

```ada
{AI95-00285-01} {AI95-00395-01} Character_Set : constant Wide_Maps.Wide_Character_Set;
--Contains each Wide_Character value WC such that
--Characters.Conversions.Is_Character(WC) is True

```

{AI95-00395-01} Each Wide_Character_Set constant in the package Strings.Wide_Maps.Wide_Constants contains no values outside the Character portion of Wide_Character. Similarly, each Wide_Character_Mapping constant in this package is the identity mapping when applied to any element outside the Character portion of Wide_Character.

{AI95-00362-01} {AI12-0302-1} {AI12-0414-1} Aspect Pure is replaced by aspects Preelaborate, Nonblocking, Global =&gt in out synchronized in Strings.Wide_Maps.Wide_Constants. 

NOTE   If a null Wide_Character_Mapping_Function is passed to any of the Wide_String handling subprograms, Constraint_Error is propagated.


#### Incompatibilities With Ada 95

{AI95-00301-01} Various new operations are added to Strings.Wide_Fixed, Strings.Wide_Bounded, and Strings.Wide_Unbounded. If one of these packages is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 95

{AI95-00161-01} Amendment Correction: Added [pragma](./AA-2.8#S0019) Preelaborable_Initialization to types Wide_Character_Set and Wide_Character_Mapping, so that they can be used to declare default-initialized objects in preelaborated units. 


#### Wording Changes from Ada 95

{AI95-00285-01} Corrected the description of Character_Set.

{AI95-00302-03} Added wide versions of Strings.Hash and Strings.Unbounded.Hash.

{AI95-00362-01} Added wording so that Strings.Wide_Maps.Wide_Constants does not change to Pure.

{AI95-00395-01} The second Note is now normative text, since there is no way to derive it from the other rules. It's a little weird given the use of Unicode character classifications in Ada 2005; but changing it would be inconsistent with Ada 95 and a one-to-one mapping isn't necessarily correct anyway. 


#### Extensions to Ada 2005

{AI05-0286-1} The case insenstive library functions (Strings.Wide_Equal_Case_Insensitive, Strings.Wide_Fixed.Wide_Equal_Case_Insensitive, Strings.Wide_Bounded.Wide_Equal_Case_Insensitive, Strings.Wide_Unbounded.Wide_Equal_Case_Insensitive, Strings.Wide_Hash_Case_Insensitive, Strings.Wide_Fixed.Wide_Hash_Case_Insensitive, Strings.Wide_Bounded.Wide_Hash_Case_Insensitive, and Strings.Wide_Unbounded.Wide_Hash_Case_Insensitive) are new. 


#### Wording Changes from Ada 2005

{AI05-0223-1} Correction: Identified Wide_Character_Set and Wide_Character_Mapping as needing finalization. It is likely that they are implemented with a controlled type, so this change is unlikely to make any difference in practice. 


## A.4.8  Wide_Wide_String Handling

{AI95-00285-01} {AI95-00395-01} {AI05-0286-1} Facilities for handling strings of Wide_Wide_Character elements are found in the packages Strings.Wide_Wide_Maps, Strings.Wide_Wide_Fixed, Strings.Wide_Wide_Bounded, Strings.Wide_Wide_Unbounded, and Strings.Wide_Wide_Maps.Wide_Wide_Constants, and in the library functions Strings.Wide_Wide_Hash, Strings.Wide_Wide_Fixed.Wide_Wide_Hash, Strings.Wide_Wide_Bounded.Wide_Wide_Hash, Strings.Wide_Wide_Unbounded.Wide_Wide_Hash, Strings.Wide_Wide_Hash_Case_Insensitive, Strings.Wide_Wide_Fixed.Wide_Wide_Hash_Case_Insensitive, Strings.Wide_Wide_Bounded.Wide_Wide_Hash_Case_Insensitive, Strings.Wide_Wide_Unbounded.Wide_Wide_Hash_Case_Insensitive, Strings.Wide_Wide_Equal_Case_Insensitive, Strings.Wide_Wide_Fixed.Wide_Wide_Equal_Case_Insensitive, Strings.Wide_Wide_Bounded.Wide_Wide_Equal_Case_Insensitive, and Strings.Wide_Wide_Unbounded.Wide_Wide_Equal_Case_Insensitive. They provide the same string-handling operations as the corresponding packages and functions for strings of Character elements. 


#### Static Semantics

{AI95-00285-01} The library package Strings.Wide_Wide_Maps has the following declaration.

```ada
{AI12-0241-1} {AI12-0302-1} package Ada.Strings.Wide_Wide_Maps
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI12-0399-1}    -- Representation for a set of Wide_Wide_Character values:
   type Wide_Wide_Character_Set is private
      with Preelaborable_Initialization;

```

```ada
   Null_Set : constant Wide_Wide_Character_Set;

```

```ada
   type Wide_Wide_Character_Range is
      record
         Low  : Wide_Wide_Character;
         High : Wide_Wide_Character;
      end record;
   -- Represents Wide_Wide_Character range Low..High

```

```ada
   type Wide_Wide_Character_Ranges is array (Positive range &lt&gt)
         of Wide_Wide_Character_Range;

```

```ada
   function To_Set (Ranges : in Wide_Wide_Character_Ranges)
         return Wide_Wide_Character_Set;

```

```ada
   function To_Set (Span : in Wide_Wide_Character_Range)
         return Wide_Wide_Character_Set;

```

```ada
   function To_Ranges (Set : in Wide_Wide_Character_Set)
         return Wide_Wide_Character_Ranges;

```

```ada
   function "=" (Left, Right : in Wide_Wide_Character_Set) return Boolean;

```

```ada
   function "not" (Right : in Wide_Wide_Character_Set)
         return Wide_Wide_Character_Set;
   function "and" (Left, Right : in Wide_Wide_Character_Set)
         return Wide_Wide_Character_Set;
   function "or" (Left, Right : in Wide_Wide_Character_Set)
         return Wide_Wide_Character_Set;
   function "xor" (Left, Right : in Wide_Wide_Character_Set)
         return Wide_Wide_Character_Set;
   function "-" (Left, Right : in Wide_Wide_Character_Set)
         return Wide_Wide_Character_Set;

```

```ada
   function Is_In (Element : in Wide_Wide_Character;
                   Set     : in Wide_Wide_Character_Set)
         return Boolean;

```

```ada
   function Is_Subset (Elements : in Wide_Wide_Character_Set;
                       Set      : in Wide_Wide_Character_Set)
         return Boolean;

```

```ada
   function "&lt=" (Left  : in Wide_Wide_Character_Set;
                  Right : in Wide_Wide_Character_Set)
         return Boolean renames Is_Subset;

```

```ada
   -- Alternative representation for a set of Wide_Wide_Character values:
   subtype Wide_Wide_Character_Sequence is Wide_Wide_String;

```

```ada
   function To_Set (Sequence : in Wide_Wide_Character_Sequence)
         return Wide_Wide_Character_Set;

```

```ada
   function To_Set (Singleton : in Wide_Wide_Character)
         return Wide_Wide_Character_Set;

```

```ada
   function To_Sequence (Set : in Wide_Wide_Character_Set)
         return Wide_Wide_Character_Sequence;

```

```ada
{AI12-0399-1}    -- Representation for a Wide_Wide_Character to Wide_Wide_Character
   -- mapping:
   type Wide_Wide_Character_Mapping is private
      with Preelaborable_Initialization;

```

```ada
   function Value (Map     : in Wide_Wide_Character_Mapping;
                   Element : in Wide_Wide_Character)
         return Wide_Wide_Character;

```

```ada
   Identity : constant Wide_Wide_Character_Mapping;

```

```ada
   function To_Mapping (From, To : in Wide_Wide_Character_Sequence)
         return Wide_Wide_Character_Mapping;

```

```ada
   function To_Domain (Map : in Wide_Wide_Character_Mapping)
         return Wide_Wide_Character_Sequence;

```

```ada
   function To_Range (Map : in Wide_Wide_Character_Mapping)
         return Wide_Wide_Character_Sequence;

```

```ada
   type Wide_Wide_Character_Mapping_Function is
         access function (From : in Wide_Wide_Character)
         return Wide_Wide_Character;

```

```ada
private
   ... -- not specified by the language
end Ada.Strings.Wide_Wide_Maps;

```

{AI95-00285-01} The context clause for each of the packages Strings.Wide_Wide_Fixed, Strings.Wide_Wide_Bounded, and Strings.Wide_Wide_Unbounded identifies Strings.Wide_Wide_Maps instead of Strings.Maps.

{AI05-0223-1} Types Wide_Wide_Character_Set and Wide_Wide_Character_Mapping need finalization.

{AI95-00285-01} {AI05-0286-1} For each of the packages Strings.Fixed, Strings.Bounded, Strings.Unbounded, and Strings.Maps.Constants, and for library functions Strings.Hash, Strings.Fixed.Hash, Strings.Bounded.Hash, Strings.Unbounded.Hash, Strings.Hash_Case_Insensitive, Strings.Fixed.Hash_Case_Insensitive, Strings.Bounded.Hash_Case_Insensitive, Strings.Unbounded.Hash_Case_Insensitive, Strings.Equal_Case_Insensitive, Strings.Fixed.Equal_Case_Insensitive, Strings.Bounded.Equal_Case_Insensitive, and Strings.Unbounded.Equal_Case_Insensitive, the corresponding wide wide string package or function has the same contents except that

Wide_Wide_Space replaces Space

Wide_Wide_Character replaces Character

Wide_Wide_String replaces String

Wide_Wide_Character_Set replaces Character_Set

Wide_Wide_Character_Mapping replaces Character_Mapping

Wide_Wide_Character_Mapping_Function replaces Character_Mapping_Function

Wide_Wide_Maps replaces Maps

Bounded_Wide_Wide_String replaces Bounded_String

Null_Bounded_Wide_Wide_String replaces Null_Bounded_String

To_Bounded_Wide_Wide_String replaces To_Bounded_String

To_Wide_Wide_String replaces To_String

{AI95-00301-01} Set_Bounded_Wide_Wide_String replaces Set_Bounded_String

Unbounded_Wide_Wide_String replaces Unbounded_String

Null_Unbounded_Wide_Wide_String replaces Null_Unbounded_String

Wide_Wide_String_Access replaces String_Access

To_Unbounded_Wide_Wide_String replaces To_Unbounded_String

{AI95-00301-01} Set_Unbounded_Wide_Wide_String replaces Set_Unbounded_String

{AI95-00285-01} {AI95-00395-01} The following additional declarations are present in Strings.Wide_Wide_Maps.Wide_Wide_Constants:

```ada
Character_Set : constant Wide_Wide_Maps.Wide_Wide_Character_Set;
-- Contains each Wide_Wide_Character value WWC such that
-- Characters.Conversions.Is_Character(WWC) is True
Wide_Character_Set : constant Wide_Wide_Maps.Wide_Wide_Character_Set;
-- Contains each Wide_Wide_Character value WWC such that
-- Characters.Conversions.Is_Wide_Character(WWC) is True

```

{AI95-00395-01} Each Wide_Wide_Character_Set constant in the package Strings.Wide_Wide_Maps.Wide_Wide_Constants contains no values outside the Character portion of Wide_Wide_Character. Similarly, each Wide_Wide_Character_Mapping constant in this package is the identity mapping when applied to any element outside the Character portion of Wide_Wide_Character.

{AI95-00395-01} {AI12-0302-1} {AI12-0414-1} Aspect Pure is replaced by aspects Preelaborate, Nonblocking, Global =&gt in out synchronized in Strings.Wide_Wide_Maps.Wide_Wide_Constants.

NOTE 1   {AI95-00285-01} If a null Wide_Wide_Character_Mapping_Function is passed to any of the Wide_Wide_String handling subprograms, Constraint_Error is propagated. 


#### Extensions to Ada 95

{AI95-00285-01} {AI95-00395-01} The double-wide string-handling packages (Strings.Wide_Wide_Maps, Strings.Wide_Wide_Fixed, Strings.Wide_Wide_Bounded, Strings.Wide_Wide_Unbounded, and Strings.Wide_Wide_Maps.Wide_Wide_Constants), and functions Strings.Wide_Wide_Hash and Strings.Wide_Wide_Unbounded.Wide_Wide_Hash are new. 


#### Extensions to Ada 2005

{AI05-0286-1} The case insenstive library functions (Strings.Wide_Wide_Equal_Case_Insensitive, Strings.Wide_Wide_Fixed.Wide_Wide_Equal_Case_Insensitive, Strings.Wide_Wide_Bounded.Wide_Wide_Equal_Case_Insensitive, Strings.Wide_Wide_Unbounded.Wide_Wide_Equal_Case_Insensitive, Strings.Wide_Wide_Hash_Case_Insensitive, Strings.Wide_Wide_Fixed.Wide_Wide_Hash_Case_Insensitive, Strings.Wide_Wide_Bounded.Wide_Wide_Hash_Case_Insensitive, and Strings.Wide_Wide_Unbounded.Wide_Wide_Hash_Case_Insensitive) are new. 


#### Wording Changes from Ada 2005

{AI05-0223-1} Correction: Identified Wide_Wide_Character_Set and Wide_Wide_Character_Mapping as needing finalization. It is likely that they are implemented with a controlled type, so this change is unlikely to make any difference in practice. 


## A.4.9  String Hashing


#### Static Semantics

{AI95-00302-03} The library function Strings.Hash has the following declaration: 

```ada
{AI05-0298-1} {AI12-0414-1} with Ada.Containers;
function Ada.Strings.Hash (Key : String) return Containers.Hash_Type
   with Pure;

```

Returns an implementation-defined value which is a function of the value of Key. If A and B are strings such that A equals B, Hash(A) equals Hash(B). 

Implementation defined: The values returned by Strings.Hash.

{AI95-00302-03} The library function Strings.Fixed.Hash has the following declaration: 

```ada
{AI05-0298-1} with Ada.Containers, Ada.Strings.Hash;
function Ada.Strings.Fixed.Hash (Key : String) return Containers.Hash_Type
   renames Ada.Strings.Hash;

```

{AI95-00302-03} The generic library function Strings.Bounded.Hash has the following declaration: 

```ada
{AI05-0298-1} {AI12-0241-1} {AI12-0302-1} with Ada.Containers;
generic
   with package Bounded is
      new Ada.Strings.Bounded.Generic_Bounded_Length (&lt&gt);
function Ada.Strings.Bounded.Hash (Key : Bounded.Bounded_String)
   return Containers.Hash_Type
   with Preelaborate, Nonblocking, Global =&gt in out synchronized;

```

{AI05-0001-1} Equivalent to Strings.Hash (Bounded.To_String (Key));

{AI95-00302-03} The library function Strings.Unbounded.Hash has the following declaration: 

```ada
{AI05-0298-1} {AI12-0241-1} {AI12-0302-1} with Ada.Containers;
function Ada.Strings.Unbounded.Hash (Key : Unbounded_String)
   return Containers.Hash_Type
   with Preelaborate, Nonblocking, Global =&gt in out synchronized;

```

{AI05-0001-1} Equivalent to Strings.Hash (To_String (Key));

{AI05-0001-1} {AI05-0298-1} The library function Strings.Hash_Case_Insensitive has the following declaration:

```ada
{AI12-0414-1} with Ada.Containers;
function Ada.Strings.Hash_Case_Insensitive (Key : String)
   return Containers.Hash_Type
   with Pure;

```

Returns an implementation-defined value which is a function of the value of Key, converted to lower case. If A and B are strings such that Strings.Equal_Case_Insensitive (A, B) (see A.4.10) is True, then Hash_Case_Insensitive(A) equals Hash_Case_Insensitive(B). 

{AI05-0001-1} {AI05-0298-1} The library function Strings.Fixed.Hash_Case_Insensitive has the following declaration:

```ada
with Ada.Containers, Ada.Strings.Hash_Case_Insensitive;
function Ada.Strings.Fixed.Hash_Case_Insensitive (Key : String)
   return Containers.Hash_Type renames Ada.Strings.Hash_Case_Insensitive;

```

{AI05-0001-1} {AI05-0298-1} The generic library function Strings.Bounded.Hash_Case_Insensitive has the following declaration:

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Containers;
generic
   with package Bounded is
      new Ada.Strings.Bounded.Generic_Bounded_Length (&lt&gt);
function Ada.Strings.Bounded.Hash_Case_Insensitive
   (Key : Bounded.Bounded_String) return Containers.Hash_Type
   with Preelaborate, Nonblocking, Global =&gt in out synchronized;

```

Equivalent to Strings.Hash_Case_Insensitive (Bounded.To_String (Key)); 

{AI05-0001-1} {AI05-0298-1} The library function Strings.Unbounded.Hash_Case_Insensitive has the following declaration:

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Containers;
function Ada.Strings.Unbounded.Hash_Case_Insensitive
   (Key : Unbounded_String) return Containers.Hash_Type
   with Preelaborate, Nonblocking, Global =&gt in out synchronized;

```

Equivalent to Strings.Hash_Case_Insensitive (To_String (Key)); 


#### Implementation Advice

{AI95-00302-03} The Hash functions should be good hash functions, returning a wide spread of values for different string values. It should be unlikely for similar strings to return the same value. 

Implementation Advice: Strings.Hash should be good a hash function, returning a wide spread of values for different string values, and similar strings should rarely return the same value.

Ramification: The other functions are defined in terms of Strings.Hash, so they don't need separate advice in the Annex. 


#### Extensions to Ada 95

{AI95-00302-03} The Strings.Hash, Strings.Fixed.Hash, Strings.Bounded.Hash, and Strings.Unbounded.Hash functions are new. 


#### Extensions to Ada 2005

{AI05-0001-1} The Strings.Hash_Case_Insensitive, Strings.Fixed.Hash_Case_Insensitive, Strings.Bounded.Hash_Case_Insensitive, and Strings.Unbounded.Hash_Case_Insensitive functions are new. 


## A.4.10  String Comparison


#### Static Semantics

{AI05-0001-1} {AI05-0286-1} {AI05-0298-1} The library function Strings.Equal_Case_Insensitive has the following declaration:

```ada
{AI12-0414-1} function Ada.Strings.Equal_Case_Insensitive (Left, Right : String)
   return Boolean with Pure;

```

{AI12-0263-1} Returns True if the strings consist of the same sequence of characters after applying locale-independent simple case folding, as defined by documents referenced in Clause 2 of ISO/IEC 10646:2017. Otherwise, returns False. This function uses the same method as is used to determine whether two identifiers are the same.

Discussion: {AI05-0286-1} For String, this is equivalent to converting to lower case and comparing. Not so for other string types. For Wide_Strings and Wide_Wide_Strings, note that this result is a more accurate comparison than converting the strings to lower case and comparing the results; it is possible that the lower case conversions are the same but this routine will report the strings as different. Additionally, Unicode says that the result of this function will never change for strings made up solely of defined code points; there is no such guarantee for case conversion to lower case.

{AI12-0263-1} The "documents referenced" means Unicode, Chapter 4 (specifically, section 4.2 - Case). See the Implementation Notes in subclause 2.3 for a source for machine-readable definitions of these properties. 

{AI05-0001-1} {AI05-0248-1} {AI05-0298-1} The library function Strings.Fixed.Equal_Case_Insensitive has the following declaration:

```ada
with Ada.Strings.Equal_Case_Insensitive;
function Ada.Strings.Fixed.Equal_Case_Insensitive
   (Left, Right : String) return Boolean
      renames Ada.Strings.Equal_Case_Insensitive;

```

{AI05-0001-1} {AI05-0248-1} {AI05-0298-1} The generic library function Strings.Bounded.Equal_Case_Insensitive has the following declaration:

```ada
{AI12-0241-1} {AI12-0302-1} generic
   with package Bounded is
      new Ada.Strings.Bounded.Generic_Bounded_Length (&lt&gt);
function Ada.Strings.Bounded.Equal_Case_Insensitive
   (Left, Right : Bounded.Bounded_String) return Boolean
   with Preelaborate, Nonblocking, Global =&gt in out synchronized;

```

Equivalent to Strings.Equal_Case_Insensitive (Bounded.To_String (Left), Bounded.To_String (Right)); 

{AI05-0001-1} {AI05-0248-1} {AI05-0298-1} The library function Strings.Unbounded.Equal_Case_Insensitive has the following declaration:

```ada
{AI12-0241-1} {AI12-0302-1} function Ada.Strings.Unbounded.Equal_Case_Insensitive
   (Left, Right : Unbounded_String) return Boolean
   with Preelaborate, Nonblocking, Global =&gt in out synchronized;

```

Equivalent to Strings.Equal_Case_Insensitive (To_String (Left), To_String (Right)); 

{AI05-0001-1} {AI05-0298-1} The library function Strings.Less_Case_Insensitive has the following declaration:

```ada
{AI12-0414-1} function Ada.Strings.Less_Case_Insensitive (Left, Right : String)
   return Boolean with Pure;

```

Performs a lexicographic comparison of strings Left and Right, converted to lower case. 

{AI05-0001-1} {AI05-0248-1} {AI05-0298-1} The library function Strings.Fixed.Less_Case_Insensitive has the following declaration:

```ada
with Ada.Strings.Less_Case_Insensitive;
function Ada.Strings.Fixed.Less_Case_Insensitive
   (Left, Right : String) return Boolean
      renames Ada.Strings.Less_Case_Insensitive;

```

{AI05-0001-1} {AI05-0248-1} {AI05-0298-1} The generic library function Strings.Bounded.Less_Case_Insensitive has the following declaration:

```ada
{AI12-0241-1} {AI12-0302-1} generic
   with package Bounded is
      new Ada.Strings.Bounded.Generic_Bounded_Length (&lt&gt);
function Ada.Strings.Bounded.Less_Case_Insensitive
  (Left, Right : Bounded.Bounded_String) return Boolean
   with Preelaborate, Nonblocking, Global =&gt in out synchronized;

```

Equivalent to Strings.Less_Case_Insensitive (Bounded.To_String (Left), Bounded.To_String (Right)); 

{AI05-0001-1} {AI05-0248-1} {AI05-0298-1} The library function Strings.Unbounded.Less_Case_Insensitive has the following declaration:

```ada
{AI12-0241-1} {AI12-0302-1} function Ada.Strings.Unbounded.Less_Case_Insensitive
  (Left, Right : Unbounded_String) return Boolean
   with Preelaborate, Nonblocking, Global =&gt in out synchronized;

```

Equivalent to Strings.Less_Case_Insensitive (To_String (Left), To_String (Right)); 


#### Extensions to Ada 2005

{AI05-0001-1} {AI05-0286-1} The Strings.Equal_Case_Insensitive, Strings.Fixed.Equal_Case_Insensitive, Strings.Bounded.Equal_Case_Insensitive, Strings.Unbounded.Equal_Case_Insensitive, Strings.Less_Case_Insensitive, Strings.Fixed.Less_Case_Insensitive, Strings.Bounded.Less_Case_Insensitive, Strings.Unbounded.Less_Case_Insensitive functions are new. 


## A.4.11  String Encoding

{AI05-0137-2} Facilities for encoding, decoding, and converting strings in various character encoding schemes are provided by packages Strings.UTF_Encoding, Strings.UTF_Encoding.Conversions, Strings.UTF_Encoding.Strings, Strings.UTF_Encoding.Wide_Strings, and Strings.UTF_Encoding.Wide_Wide_Strings. 


#### Static Semantics

{AI05-0137-2} The encoding library packages have the following declarations:

```ada
{AI05-0137-2} {AI12-0414-1} package Ada.Strings.UTF_Encoding
  with Pure is

```

```ada
   -- Declarations common to the string encoding packages
   type Encoding_Scheme is (UTF_8, UTF_16BE, UTF_16LE);

```

```ada
   subtype UTF_String is String;

```

```ada
   subtype UTF_8_String is String;

```

```ada
   subtype UTF_16_Wide_String is Wide_String;

```

```ada
   Encoding_Error : exception;

```

```ada
   BOM_8    : constant UTF_8_String :=
                Character'Val(16#EF#) &
                Character'Val(16#BB#) &
                Character'Val(16#BF#);

```

```ada
   BOM_16BE : constant UTF_String :=
                Character'Val(16#FE#) &
                Character'Val(16#FF#);

```

```ada
   BOM_16LE : constant UTF_String :=
                Character'Val(16#FF#) &
                Character'Val(16#FE#);

```

```ada
   BOM_16   : constant UTF_16_Wide_String :=
               (1 =&gt Wide_Character'Val(16#FEFF#));

```

```ada
   function Encoding (Item    : UTF_String;
                      Default : Encoding_Scheme := UTF_8)
      return Encoding_Scheme;

```

```ada
end Ada.Strings.UTF_Encoding;

```

```ada
{AI05-0137-2} {AI12-0414-1} package Ada.Strings.UTF_Encoding.Conversions
   with Pure is

```

```ada
   -- Conversions between various encoding schemes
   function Convert (Item          : UTF_String;
                     Input_Scheme  : Encoding_Scheme;
                     Output_Scheme : Encoding_Scheme;
                     Output_BOM    : Boolean := False) return UTF_String;

```

```ada
   function Convert (Item          : UTF_String;
                     Input_Scheme  : Encoding_Scheme;
                     Output_BOM    : Boolean := False)
      return UTF_16_Wide_String;

```

```ada
   function Convert (Item          : UTF_8_String;
                     Output_BOM    : Boolean := False)
      return UTF_16_Wide_String;

```

```ada
   function Convert (Item          : UTF_16_Wide_String;
                     Output_Scheme : Encoding_Scheme;
                     Output_BOM    : Boolean := False) return UTF_String;

```

```ada
   function Convert (Item          : UTF_16_Wide_String;
                     Output_BOM    : Boolean := False) return UTF_8_String;

```

```ada
end Ada.Strings.UTF_Encoding.Conversions;

```

```ada
{AI05-0137-2} {AI12-0414-1} package Ada.Strings.UTF_Encoding.Strings
   with Pure is

```

```ada
   -- Encoding / decoding between String and various encoding schemes
   function Encode (Item          : String;
                    Output_Scheme : Encoding_Scheme;
                    Output_BOM    : Boolean  := False) return UTF_String;

```

```ada
   function Encode (Item       : String;
                    Output_BOM : Boolean  := False) return UTF_8_String;

```

```ada
   function Encode (Item       : String;
                    Output_BOM : Boolean  := False)
      return UTF_16_Wide_String;

```

```ada
   function Decode (Item         : UTF_String;
                    Input_Scheme : Encoding_Scheme) return String;

```

```ada
   function Decode (Item : UTF_8_String) return String;

```

```ada
   function Decode (Item : UTF_16_Wide_String) return String;

```

```ada
end Ada.Strings.UTF_Encoding.Strings;

```

```ada
{AI05-0137-2} {AI12-0414-1} package Ada.Strings.UTF_Encoding.Wide_Strings
   with Pure is

```

```ada
   -- Encoding / decoding between Wide_String and various encoding schemes
   function Encode (Item          : Wide_String;
                    Output_Scheme : Encoding_Scheme;
                    Output_BOM    : Boolean  := False) return UTF_String;

```

```ada
   function Encode (Item       : Wide_String;
                    Output_BOM : Boolean  := False) return UTF_8_String;

```

```ada
   function Encode (Item       : Wide_String;
                    Output_BOM : Boolean  := False)
      return UTF_16_Wide_String;

```

```ada
   function Decode (Item         : UTF_String;
                    Input_Scheme : Encoding_Scheme) return Wide_String;

```

```ada
   function Decode (Item : UTF_8_String) return Wide_String;

```

```ada
   function Decode (Item : UTF_16_Wide_String) return Wide_String;

```

```ada
end Ada.Strings.UTF_Encoding.Wide_Strings;

```

```ada
{AI05-0137-2} {AI12-0414-1} package Ada.Strings.UTF_Encoding.Wide_Wide_Strings
   with Pure is

```

```ada
   -- Encoding / decoding between Wide_Wide_String and various encoding schemes
   function Encode (Item          : Wide_Wide_String;
                    Output_Scheme : Encoding_Scheme;
                    Output_BOM    : Boolean  := False) return UTF_String;

```

```ada
   function Encode (Item       : Wide_Wide_String;
                    Output_BOM : Boolean  := False) return UTF_8_String;

```

```ada
   function Encode (Item       : Wide_Wide_String;
                    Output_BOM : Boolean  := False)
      return UTF_16_Wide_String;

```

```ada
   function Decode (Item         : UTF_String;
                    Input_Scheme : Encoding_Scheme) return Wide_Wide_String;

```

```ada
   function Decode (Item : UTF_8_String) return Wide_Wide_String;

```

```ada
   function Decode (Item : UTF_16_Wide_String) return Wide_Wide_String;

```

```ada
end Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

```

{AI05-0137-2} {AI05-0262-1} The type Encoding_Scheme defines encoding schemes. UTF_8 corresponds to the UTF-8 encoding scheme defined by Annex D of ISO/IEC 10646. UTF_16BE corresponds to the UTF-16 encoding scheme defined by Annex C of ISO/IEC 10646 in 8 bit, big-endian order; and UTF_16LE corresponds to the UTF-16 encoding scheme in 8 bit, little-endian order.

{AI05-0137-2} The subtype UTF_String is used to represent a String of 8-bit values containing a sequence of values encoded in one of three ways (UTF-8, UTF-16BE, or UTF-16LE). The subtype UTF_8_String is used to represent a String of 8-bit values containing a sequence of values encoded in UTF-8. The subtype UTF_16_Wide_String is used to represent a Wide_String of 16-bit values containing a sequence of values encoded in UTF-16.

{AI05-0137-2} {AI05-0262-1} The BOM_8, BOM_16BE, BOM_16LE, and BOM_16 constants correspond to values used at the start of a string to indicate the encoding.

{AI05-0262-1} {AI05-0269-1} Each of the Encode functions takes a String, Wide_String, or Wide_Wide_String Item parameter that is assumed to be an array of unencoded characters. Each of the Convert functions takes a UTF_String, UTF_8_String, or UTF_16_String Item parameter that is assumed to contain characters whose position values correspond to a valid encoding sequence according to the encoding scheme required by the function or specified by its Input_Scheme parameter.

{AI05-0137-2} {AI05-0262-1} {AI05-0269-1} Each of the Convert and Encode functions returns a UTF_String, UTF_8_String, or UTF_16_String value whose characters have position values that correspond to the encoding of the Item parameter according to the encoding scheme required by the function or specified by its Output_Scheme parameter. For UTF_8, no overlong encoding is returned. A BOM is included at the start of the returned string if the Output_BOM parameter is set to True. The lower bound of the returned string is 1.

{AI05-0137-2} {AI05-0262-1} Each of the Decode functions takes a UTF_String, UTF_8_String, or UTF_16_String Item parameter which is assumed to contain characters whose position values correspond to a valid encoding sequence according to the encoding scheme required by the function or specified by its Input_Scheme parameter, and returns the corresponding String, Wide_String, or Wide_Wide_String value. The lower bound of the returned string is 1.

{AI05-0137-2} {AI05-0262-1} For each of the Convert and Decode functions, an initial BOM in the input that matches the expected encoding scheme is ignored, and a different initial BOM causes Encoding_Error to be propagated.

{AI05-0137-2} The exception Encoding_Error is also propagated in the following situations: 

{AI12-0088-1} By a Convert or Decode function when a UTF encoded string contains an invalid encoding sequence.

To be honest: {AI12-0088-1} An overlong encoding is not invalid for the purposes of this check, and this does not depend on the character set version in use. Some recent character set standards declare overlong encodings to be invalid; it would be unnecessary and unfriendly to users for Convert or Decode to raise an exception for an overlong encoding. 

{AI12-0088-1} By a Convert or Decode function when the expected encoding is UTF-16BE or UTF-16LE and the input string has an odd length.

{AI05-0262-1} By a Decode function yielding a String when the decoding of a sequence results in a code point whose value exceeds 16#FF#.

By a Decode function yielding a Wide_String when the decoding of a sequence results in a code point whose value exceeds 16#FFFF#.

{AI05-0262-1} By an Encode function taking a Wide_String as input when an invalid character appears in the input. In particular, the characters whose position is in the range 16#D800# .. 16#DFFF# are invalid because they conflict with UTF-16 surrogate encodings, and the characters whose position is 16#FFFE# or 16#FFFF# are also invalid because they conflict with BOM codes. 

```ada
{AI05-0137-2} function Encoding (Item    : UTF_String;
                   Default : Encoding_Scheme := UTF_8)
   return Encoding_Scheme;

```

{AI05-0137-2} {AI05-0269-1} Inspects a UTF_String value to determine whether it starts with a BOM for UTF-8, UTF-16BE, or UTF_16LE. If so, returns the scheme corresponding to the BOM; otherwise, returns the value of Default.

```ada
{AI05-0137-2} function Convert (Item          : UTF_String;
                  Input_Scheme  : Encoding_Scheme;
                  Output_Scheme : Encoding_Scheme;
                  Output_BOM    : Boolean := False) return UTF_String;

```

Returns the value of Item (originally encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Input_Scheme) encoded in one of these three schemes as specified by Output_Scheme.

```ada
{AI05-0137-2} function Convert (Item          : UTF_String;
                  Input_Scheme  : Encoding_Scheme;
                  Output_BOM    : Boolean := False)
   return UTF_16_Wide_String;

```

Returns the value of Item (originally encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Input_Scheme) encoded in UTF-16.

```ada
{AI05-0137-2} function Convert (Item          : UTF_8_String;
                  Output_BOM    : Boolean := False)
   return UTF_16_Wide_String;

```

Returns the value of Item (originally encoded in UTF-8) encoded in UTF-16.

```ada
{AI05-0137-2} function Convert (Item          : UTF_16_Wide_String;
                  Output_Scheme : Encoding_Scheme;
                  Output_BOM    : Boolean := False) return UTF_String;

```

Returns the value of Item (originally encoded in UTF-16) encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Output_Scheme.

```ada
{AI05-0137-2} function Convert (Item          : UTF_16_Wide_String;
                  Output_BOM    : Boolean := False) return UTF_8_String;

```

Returns the value of Item (originally encoded in UTF-16) encoded in UTF-8.

```ada
{AI05-0137-2} function Encode (Item          : String;
                 Output_Scheme : Encoding_Scheme;
                 Output_BOM    : Boolean  := False) return UTF_String;

```

{AI05-0262-1} Returns the value of Item encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Output_Scheme.

```ada
{AI05-0137-2} function Encode (Item       : String;
                 Output_BOM : Boolean  := False) return UTF_8_String;

```

Returns the value of Item encoded in UTF-8.

```ada
{AI05-0137-2} function Encode (Item       : String;
                 Output_BOM : Boolean  := False) return UTF_16_Wide_String;

```

Returns the value of Item encoded in UTF_16.

```ada
{AI05-0137-2} function Decode (Item         : UTF_String;
                 Input_Scheme : Encoding_Scheme) return String;

```

Returns the result of decoding Item, which is encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Input_Scheme.

```ada
{AI05-0137-2} function Decode (Item : UTF_8_String) return String;

```

Returns the result of decoding Item, which is encoded in UTF-8.

```ada
{AI05-0137-2} function Decode (Item : UTF_16_Wide_String) return String;

```

Returns the result of decoding Item, which is encoded in UTF-16.

```ada
{AI05-0137-2} function Encode (Item          : Wide_String;
                 Output_Scheme : Encoding_Scheme;
                 Output_BOM    : Boolean  := False) return UTF_String;

```

{AI05-0262-1} Returns the value of Item encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Output_Scheme.

```ada
{AI05-0137-2} function Encode (Item       : Wide_String;
                 Output_BOM : Boolean  := False) return UTF_8_String;

```

Returns the value of Item encoded in UTF-8.

```ada
{AI05-0137-2} function Encode (Item       : Wide_String;
                 Output_BOM : Boolean  := False) return UTF_16_Wide_String;

```

Returns the value of Item encoded in UTF_16.

```ada
{AI05-0137-2} function Decode (Item         : UTF_String;
                 Input_Scheme : Encoding_Scheme) return Wide_String;

```

Returns the result of decoding Item, which is encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Input_Scheme.

```ada
{AI05-0137-2} function Decode (Item : UTF_8_String) return Wide_String;

```

Returns the result of decoding Item, which is encoded in UTF-8.

```ada
{AI05-0137-2} function Decode (Item : UTF_16_Wide_String) return Wide_String;

```

Returns the result of decoding Item, which is encoded in UTF-16.

```ada
{AI05-0137-2} function Encode (Item          : Wide_Wide_String;
                 Output_Scheme : Encoding_Scheme;
                 Output_BOM    : Boolean  := False) return UTF_String;

```

{AI05-0262-1} Returns the value of Item encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Output_Scheme.

```ada
{AI05-0137-2} function Encode (Item       : Wide_Wide_String;
                 Output_BOM : Boolean  := False) return UTF_8_String;

```

Returns the value of Item encoded in UTF-8.

```ada
{AI05-0137-2} function Encode (Item       : Wide_Wide_String;
                 Output_BOM : Boolean  := False) return UTF_16_Wide_String;

```

Returns the value of Item encoded in UTF_16.

```ada
{AI05-0137-2} function Decode (Item         : UTF_String;
                 Input_Scheme : Encoding_Scheme) return Wide_Wide_String;

```

Returns the result of decoding Item, which is encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Input_Scheme.

```ada
{AI05-0137-2} function Decode (Item : UTF_8_String) return Wide_Wide_String;

```

Returns the result of decoding Item, which is encoded in UTF-8.

```ada
{AI05-0137-2} function Decode (Item : UTF_16_Wide_String) return Wide_Wide_String;

```

Returns the result of decoding Item, which is encoded in UTF-16.


#### Implementation Advice

{AI05-0137-2} If an implementation supports other encoding schemes, another similar child of Ada.Strings should be defined. 

Implementation Advice: If an implementation supports other string encoding schemes, a child of Ada.Strings similar to UTF_Encoding should be defined.

NOTE 1   {AI05-0137-2} A BOM (Byte-Order Mark, code position 16#FEFF#) can be included in a file or other entity to indicate the encoding; it is skipped when decoding. Typically, only the first line of a file or other entity contains a BOM. When decoding, the Encoding function can be called on the first line to determine the encoding; this encoding will then be used in subsequent calls to Decode to convert all of the lines to an internal format. 


#### Extensions to Ada 2005

{AI05-0137-2} The packages Strings.UTF_Encoding, Strings.UTF_Encoding.Conversions, Strings.UTF_Encoding.Strings, Strings.UTF_Encoding.Wide_Strings, and Strings.UTF_Encoding.Wide_Wide_Strings are new. 


#### Wording Changes from Ada 2012

{AI12-0088-1} Corrigendum: Fixed the omission that Convert routines make the same checks on input as Decode routines. 


## A.4.12  Universal Text Buffers

{AI12-0340-1} {AI12-0439-1} A universal text buffer can be used to save and retrieve text of any language-defined string type. The types used to save and retrieve the text can be different. 


#### Static Semantics

{AI12-0340-1} The text buffer library packages have the following declarations:

```ada
{AI12-0340-1} {AI12-0384-2} with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
package Ada.Strings.Text_Buffers
   with Pure is

```

```ada
   type Text_Buffer_Count is range 0 .. implementation-defined;

```

```ada
   New_Line_Count : constant Text_Buffer_Count := implementation-defined;

```

```ada
   type Root_Buffer_Type is abstract tagged private
      with Default_Initial_Condition =&gt
             Current_Indent (Root_Buffer_Type) = 0;

```

```ada
   procedure Put (
      Buffer : in out Root_Buffer_Type;
      Item   : in     String) is abstract;

```

```ada
   procedure Wide_Put (
      Buffer : in out Root_Buffer_Type;
      Item   : in     Wide_String) is abstract;

```

```ada
   procedure Wide_Wide_Put (
      Buffer : in out Root_Buffer_Type;
      Item   : in     Wide_Wide_String) is abstract;

```

```ada
   procedure Put_UTF_8 (
      Buffer : in out Root_Buffer_Type;
      Item   : in     UTF_Encoding.UTF_8_String) is abstract;

```

```ada
   procedure Wide_Put_UTF_16 (
      Buffer : in out Root_Buffer_Type;
      Item   : in     UTF_Encoding.UTF_16_Wide_String) is abstract;

```

```ada
   procedure New_Line (Buffer : in out Root_Buffer_Type) is abstract;

```

```ada
   Standard_Indent : constant Text_Buffer_Count := 3;

```

```ada
   function Current_Indent (
      Buffer : Root_Buffer_Type) return Text_Buffer_Count;

```

```ada
   procedure Increase_Indent (
      Buffer : in out Root_Buffer_Type;
      Amount : in     Text_Buffer_Count := Standard_Indent)
      with Post'Class =&gt
         Current_Indent (Buffer) = Current_Indent (Buffer)'Old + Amount;

```

```ada
   procedure Decrease_Indent (
      Buffer : in out Root_Buffer_Type;
      Amount : in     Text_Buffer_Count := Standard_Indent)
      with Pre'Class =&gt
              Current_Indent (Buffer) &gt= Amount
                 or else raise Constraint_Error,
           Post'Class =&gt
              Current_Indent (Buffer) =
                 Current_Indent (Buffer)'Old - Amount;

```

```ada
private
   ... -- not specified by the language
end Ada.Strings.Text_Buffers;

```

```ada
{AI12-0340-1} {AI12-0384-2} package Ada.Strings.Text_Buffers.Unbounded
   with Preelaborate, Nonblocking, Global =&gt null is

```

```ada
   type Buffer_Type is new Root_Buffer_Type with private;

```

```ada
   function Get (
      Buffer : in out Buffer_Type)
      return String
      with Post'Class =&gt 
         Get'Result'First = 1 and then Current_Indent (Buffer) = 0;

```

```ada
   function Wide_Get (
      Buffer : in out Buffer_Type)
      return Wide_String
      with Post'Class =&gt
         Wide_Get'Result'First = 1 and then Current_Indent (Buffer) = 0;

```

```ada
   function Wide_Wide_Get (
      Buffer : in out Buffer_Type)
      return Wide_Wide_String
      with Post'Class =&gt
         Wide_Wide_Get'Result'First = 1 
            and then Current_Indent (Buffer) = 0;

```

```ada
   function Get_UTF_8 (
      Buffer : in out Buffer_Type)
      return UTF_Encoding.UTF_8_String
      with Post'Class =&gt
         Get_UTF_8'Result'First = 1 and then Current_Indent (Buffer) = 0;

```

```ada
   function Wide_Get_UTF_16 (
      Buffer : in out Buffer_Type)
      return UTF_Encoding.UTF_16_Wide_String
      with Post'Class =&gt
         Wide_Get_UTF_16'Result'First = 1
            and then Current_Indent (Buffer) = 0;

```

```ada
private
   ... -- not specified by the language, but will include nonabstract
       -- overridings of all inherited subprograms that require overriding.
end Ada.Strings.Text_Buffers.Unbounded;

```

```ada
{AI12-0340-1} {AI12-0384-2} package Ada.Strings.Text_Buffers.Bounded
   with Pure, Nonblocking, Global =&gt null is

```

```ada
   type Buffer_Type (Max_Characters : Text_Buffer_Count)
      is new Root_Buffer_Type with private
      with Default_Initial_Condition =&gt not Text_Truncated (Buffer_Type);

```

```ada
   function Text_Truncated (Buffer : in Buffer_Type) return Boolean;

```

```ada
   -- Get, Wide_Get, Wide_Wide_Get, Get_UTF_8, and Wide_Get_UTF_16
   -- are declared here just as in the Unbounded child.

```

```ada
private
   ... -- not specified by the language, but will include nonabstract
       -- overridings of all inherited subprograms that require overriding.
end Ada.Strings.Text_Buffers.Bounded;

```

{AI12-0340-1} Character_Count returns the number of characters currently stored in a text buffer.

Ramification: {AI12-0340-1} This is lower-case "characters". The representation isn't considered, so it is irrelevant what type of character (Character, Wide_Character, or Wide_Wide_Character) was stored. 

{AI12-0340-1} {AI12-0384-2} New_Line stores New_Line_Count characters that represent a new line into a text buffer. Current_Indent returns the current indentation associated with the buffer, with zero meaning there is no indentation in effect; Increase_Indent and Decrease_Indent increase or decrease the indentation associated with the buffer.

{AI12-0340-1} {AI12-0384-2} A call to Put, Wide_Put, Wide_Wide_Put, Put_UTF_8, or Wide_Put_UTF_16 stores a sequence of characters into the text buffer, preceded by Current_Indent(Buffer) spaces (Wide_Wide_Characters with position 32) if there is at least one character in Item and it would have been the first character on the current line.

{AI12-0340-1} {AI12-0384-2} A call to function Get, Wide_Get, Wide_Wide_Get, Get_UTF_8, or Wide_Get_UTF_16 returns the same sequence of characters as was present in the calls that stored the characters into the buffer, if representable. For a call to Get, if any character in the sequence is not defined in Character, the result is implementation defined. Similarly, for a call to Wide_Get, if any character in the sequence is not defined in Wide_Character, the result is implementation defined. As part of a call on any of the Get functions, the buffer is reset to an empty state, with no stored characters.

Implementation defined: The value returned by a call to a Text_Buffer Get procedure if any character in the returned sequence is not defined in Character.

Implementation defined: The value returned by a call to a Text_Buffer Wide_Get procedure if any character in the returned sequence is not defined in Wide_Character.

{AI12-0384-2} In the case of a Buf of type Text_Buffers.Bounded.Buffer_Type, Text_Truncated (Buf) returns True if the various Put procedures together have attempted to store more than Buf.Max_Characters into Buf. If this function returns True, then the various Get functions return a representation of only the first Buf.Max_Characters characters that were stored in Buf.


#### Implementation Advice

{AI12-0340-1} Bounded buffer objects should be implemented without dynamic allocation.

Implementation Advice: Bounded buffer objects should be implemented without dynamic allocation.


#### Extensions to Ada 2012

{AI12-0340-1} {AI12-0384-2} The packages Strings.Text_Buffers, Strings.Text_Buffers.Unbounded, and Strings.Text_Buffers.Bounded are new. 

