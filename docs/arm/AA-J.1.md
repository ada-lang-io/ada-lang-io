---
sidebar_position:  191
---

# J.1  Renamings of Library Units


#### Static Semantics

The following [library_unit_renaming_declaration](./AA-10.1#S0289)s exist: 

```ada
with Ada.Unchecked_Conversion;
generic function Unchecked_Conversion renames Ada.Unchecked_Conversion;

```

```ada
with Ada.Unchecked_Deallocation;
generic procedure Unchecked_Deallocation renames Ada.Unchecked_Deallocation;

```

```ada
with Ada.Sequential_IO;
generic package Sequential_IO renames Ada.Sequential_IO;

```

```ada
with Ada.Direct_IO;
generic package Direct_IO renames Ada.Direct_IO;

```

```ada
with Ada.Text_IO;
package Text_IO renames Ada.Text_IO;

```

```ada
with Ada.IO_Exceptions;
package IO_Exceptions renames Ada.IO_Exceptions;

```

```ada
with Ada.Calendar;
package Calendar renames Ada.Calendar;

```

```ada
with System.Machine_Code;
package Machine_Code renames System.Machine_Code; -- If supported.

```

Discussion: {AI05-0004-1} These library units correspond to those declared in Ada 83, which did not have the child unit concept or the parent package Ada. 


#### Implementation Requirements

The implementation shall allow the user to replace these renamings. 

