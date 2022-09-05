---
sidebar_position:  126
---

# A.9  The Generic Package Storage_IO

The generic package Storage_IO provides for reading from and writing to an in-memory buffer. This generic package supports the construction of user-defined input-output packages. 

Reason: This package exists to allow the portable construction of user-defined direct-access-oriented input-output packages. The Write procedure writes a value of type Element_Type into a Storage_Array of size Buffer_Size, flattening out any implicit levels of indirection used in the representation of the type. The Read procedure reads a value of type Element_Type from the buffer, reconstructing any implicit levels of indirection used in the representation of the type. It also properly initializes any type tags that appear within the value, presuming that the buffer was written by a different program and that tag values for the"same" type might vary from one executable to another. 


#### Static Semantics

The generic library package Storage_IO has the following declaration: 

```ada
{AI12-0302-1} with Ada.IO_Exceptions;
with System.Storage_Elements;
generic
   type Element_Type is private;
package Ada.Storage_IO
   with Preelaborate, Global =&gt in out synchronized is

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

Reason: As with Direct_IO, the Element_Type formal of Storage_IO does not have an [unknown_discriminant_part](./AA-3.7#S0060) so that there is a well-defined upper bound on the size of the buffer needed to hold the content of an object of the formal subtype (i.e. Buffer_Size). If there are no implicit levels of indirection, Buffer_Size will typically equal: 

```ada
(Element_Type'Size + System.Storage_Unit - 1) / System.Storage_Unit

```

Implementation defined: The value of Buffer_Size in Storage_IO.

NOTE 1   A buffer used for Storage_IO holds only one element at a time; an external file used for Direct_IO holds a sequence of elements. 


#### Extensions to Ada 83

{AI05-0005-1} Storage_IO is new in Ada 95. 

