---
sidebar_position:  204
---

# J.14  Character and Wide_Character Conversion Functions


#### Static Semantics

{AI95-00395-01} The following declarations exist in the declaration of package Ada.Characters.Handling:

```ada
   function Is_Character (Item : in Wide_Character) return Boolean
      renames Conversions.Is_Character;
   function Is_String    (Item : in Wide_String)    return Boolean
      renames Conversions.Is_String;

```

```ada
   function To_Character (Item       : in Wide_Character;
                         Substitute : in Character := ' ')
                         return Character
      renames Conversions.To_Character;

```

```ada
   function To_String    (Item       : in Wide_String;
                          Substitute : in Character := ' ')
                          return String
      renames Conversions.To_String;

```

```ada
   function To_Wide_Character (Item : in Character) return Wide_Character
      renames Conversions.To_Wide_Character;

```

```ada
   function To_Wide_String    (Item : in String)    return Wide_String
      renames Conversions.To_Wide_String;

```


#### Wording Changes from Ada 95

{AI95-00394-01} {AI05-0299-1} This subclause is new. These subprograms were moved to Characters.Conversions (see A.3.4). 

