---
sidebar_position:  129
---

# A.12  Stream Input-Output

{AI95-00285-01} The packages Streams.Stream_IO, Text_IO.Text_Streams, Wide_Text_IO.Text_Streams, and Wide_Wide_Text_IO.Text_Streams provide stream-oriented operations on files. 


#### Wording Changes from Ada 95

{AI95-00285-01} Included package Wide_Wide_Text_IO.Text_Streams in this description. 


## A.12.1  The Package Streams.Stream_IO

[The subprograms in the child package Streams.Stream_IO provide control over stream files. Access to a stream file is either sequential, via a call on Read or Write to transfer an array of stream elements, or positional (if supported by the implementation for the given file), by specifying a relative index for an element. Since a stream file can be converted to a Stream_Access value, calling stream-oriented attribute subprograms of different element types with the same Stream_Access value provides heterogeneous input-output.] See 13.13 for a general discussion of streams. 


#### Static Semantics

{8652/0055} {AI95-00026-01} The elements of a stream file are stream elements. If positioning is supported for the specified external file, a current index and current size are maintained for the file as described in A.8. If positioning is not supported, a current index is not maintained, and the current size is implementation defined. 

Implementation defined: Current size for a stream file for which positioning is not supported.

The library package Streams.Stream_IO has the following declaration: 

```ada
{AI05-0283-1} {AI12-0302-1} with Ada.IO_Exceptions;
package Ada.Streams.Stream_IO
    with Preelaborate, Global =&gt in out synchronized is

```

```ada
    type Stream_Access is access all Root_Stream_Type'Class;

```

```ada
{AI12-0102-1} {AI12-0399-1}     type File_Type is limited private
        with Preelaborable_Initialization;

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
This paragraph was deleted.

```

```ada
{AI12-0302-1}     -- Read array of stream elements from file
    procedure Read (File : in  File_Type;
                    Item : out Stream_Element_Array;
                    Last : out Stream_Element_Offset;
                    From : in  Positive_Count)
        with Global =&gt overriding in out File;

```

```ada
{AI12-0302-1}     procedure Read (File : in  File_Type;
                    Item : out Stream_Element_Array;
                    Last : out Stream_Element_Offset)
        with Global =&gt overriding in out File;

```

```ada
This paragraph was deleted.

```

```ada
{AI12-0302-1}     -- Write array of stream elements into file
    procedure Write (File : in File_Type;
                     Item : in Stream_Element_Array;
                     To   : in Positive_Count)
        with Global =&gt overriding in out File;

```

```ada
{AI12-0302-1}     procedure Write (File : in File_Type;
                     Item : in Stream_Element_Array)
        with Global =&gt overriding in out File;

```

```ada
This paragraph was deleted.

```

```ada
    -- Operations on position within file

```

```ada
{AI12-0302-1}     procedure Set_Index(File : in File_Type; To : in Positive_Count)
        with Global =&gt overriding in out File;

```

```ada
    function Index(File : in File_Type) return Positive_Count;
    function Size (File : in File_Type) return Count;

```

```ada
    procedure Set_Mode(File : in out File_Type; Mode : in File_Mode);

```

```ada
{8652/0051} {AI95-00057-01}     procedure Flush(File : in File_Type);

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
{AI12-0021-1}    package Wide_File_Names is

```

```ada
      -- File management

```

```ada
      procedure Create (File : in out File_Type;
                        Mode : in File_Mode := Out_File;
                        Name : in Wide_String := "";
                        Form : in Wide_String := "");

```

```ada
      procedure Open (File : in out File_Type;
                      Mode : in File_Mode;
                      Name : in Wide_String;
                      Form : in Wide_String := "");

```

```ada
      function Name (File : in File_Type) return Wide_String;

```

```ada
      function Form (File : in File_Type) return Wide_String;

```

```ada
   end Wide_File_Names;

```

```ada
{AI12-0021-1}    package Wide_Wide_File_Names is

```

```ada
      -- File management

```

```ada
      procedure Create (File : in out File_Type;
                        Mode : in File_Mode := Out_File;
                        Name : in Wide_Wide_String := "";
                        Form : in Wide_Wide_String := "");

```

```ada
      procedure Open (File : in out File_Type;
                      Mode : in File_Mode;
                      Name : in Wide_Wide_String;
                      Form : in Wide_Wide_String := "");

```

```ada
      function Name (File : in File_Type) return Wide_Wide_String;

```

```ada
      function Form (File : in File_Type) return Wide_Wide_String;

```

```ada
   end Wide_Wide_File_Names;

```

```ada
private
   ... -- not specified by the language
end Ada.Streams.Stream_IO;

```

{AI95-00360-01} The type File_Type needs finalization (see 7.6).

{AI95-00283-01} {AI12-0130-1} The subprograms given in subclause A.8.2 for the control of external files (Create, Open, Close, Delete, Reset, Mode, Name, Form, Is_Open, and Flush) are available for stream files.

{AI95-00283-01} The End_Of_File function: 

Propagates Mode_Error if the mode of the file is not In_File;

{AI05-0264-1} If positioning is supported for the given external file, the function returns True if the current index exceeds the size of the external file; otherwise, it returns False;

{AI05-0264-1} If positioning is not supported for the given external file, the function returns True if no more elements can be read from the given file; otherwise, it returns False. 

{8652/0055} {AI95-00026-01} {AI95-00085-01} The Set_Mode procedure sets the mode of the file. If the new mode is Append_File, the file is positioned to its end; otherwise, the position in the file is unchanged.

This paragraph was deleted.{8652/0055} {AI95-00026-01} {AI12-0130-1} 

{8652/0056} {AI95-00001-01} The Stream function returns a Stream_Access result from a File_Type object, thus allowing the stream-oriented attributes Read, Write, Input, and Output to be used on the same file for multiple types. Stream propagates Status_Error if File is not open.

{AI95-00256-01} The procedures Read and Write are equivalent to the corresponding operations in the package Streams. Read propagates Mode_Error if the mode of File is not In_File. Write propagates Mode_Error if the mode of File is not Out_File or Append_File. The Read procedure with a Positive_Count parameter starts reading at the specified index. The Write procedure with a Positive_Count parameter starts writing at the specified index. For a file that supports positioning, Read without a Positive_Count parameter starts reading at the current index, and Write without a Positive_Count parameter starts writing at the current index.

{8652/0055} {AI95-00026-01} The Size function returns the current size of the file.

{8652/0055} {AI95-00026-01} The Index function returns the current index. 

This paragraph was deleted.

The Set_Index procedure sets the current index to the specified value.

{8652/0055} {AI95-00026-01} If positioning is supported for the external file, the current index is maintained as follows:

{8652/0055} {AI95-00026-01} For Open and Create, if the Mode parameter is Append_File, the current index is set to the current size of the file plus one; otherwise, the current index is set to one.

{8652/0055} {AI95-00026-01} For Reset, if the Mode parameter is Append_File, or no Mode parameter is given and the current mode is Append_File, the current index is set to the current size of the file plus one; otherwise, the current index is set to one.

{8652/0055} {AI95-00026-01} For Set_Mode, if the new mode is Append_File, the current index is set to current size plus one; otherwise, the current index is unchanged.

{8652/0055} {AI95-00026-01} For Read and Write without a Positive_Count parameter, the current index is incremented by the number of stream elements read or written.

{8652/0055} {AI95-00026-01} For Read and Write with a Positive_Count parameter, the value of the current index is set to the value of the Positive_Count parameter plus the number of stream elements read or written. 

If positioning is not supported for the given file, then a call of Index or Set_Index propagates Use_Error. Similarly, a call of Read or Write with a Positive_Count parameter propagates Use_Error.

Implementation Note: {AI95-00085-01} It is permissible for an implementation to implement mode Append_File using the Unix append mode (the O_APPEND bit). Such an implementation does not support positioning when the mode is Append_File, and therefore the operations listed above must raise Use_Error. This is acceptable as there is no requirement that any particular file support positioning; therefore it is acceptable that a file support positioning when opened with mode Out_File, and the same file not support positioning when opened with mode Append_File. But it is not acceptable for a file to support positioning (by allowing the above operations), but to do something other than the defined semantics (that is, always write at the end, even when explicitly commanded to write somewhere else). 

Paragraphs 34 through 36 were deleted. 


#### Erroneous Execution

{8652/0056} {AI95-00001-01} If the File_Type object passed to the Stream function is later closed or finalized, and the stream-oriented attributes are subsequently called (explicitly or implicitly) on the Stream_Access value returned by Stream, execution is erroneous. This rule applies even if the File_Type object was opened again after it had been closed. 

Reason: These rules are analogous to the rule for the result of the Current_Input, Current_Output, and Current_Error functions. These rules make it possible to represent a value of (some descendant of) Root_Stream_Type which represents a file as an access value, with a null value corresponding to a closed file. 


#### Inconsistencies With Ada 95

{AI95-00283-01} {AI05-0005-1} Amendment Correction: The description of the subprograms for managing files was corrected so that they do not require truncation of the external file - a stream file is not a sequential file. An Ada 95 program that expects truncation of the stream file might not work under Ada 2005. Note that the Ada 95 standard was ambiguous on this point (the normative wording seemed to require truncation, but didn't explain where; the AARM notes seemed to expect behavior like Direct_IO), and implementations varied widely. Therefore, as a practical matter, code that depends on stream truncation might not work even in Ada 95; deleting the file before opening it provides truncation that works in both Ada 95 and Ada 2005. 


#### Incompatibilities With Ada 95

{AI95-00360-01} Amendment Correction: Stream_IO.File_Type is defined to need finalization. If the restriction No_Nested_Finalization (see D.7) applies to the partition, and File_Type does not have a controlled part, it will not be allowed in local objects in Ada 2005 whereas it would be allowed in original Ada 95. Such code is not portable, as another Ada compiler may have a controlled part in File_Type, and thus would be illegal. 


#### Wording Changes from Ada 95

{8652/0051} {AI95-00057-01} Corrigendum: Corrected the parameter mode of Flush; otherwise it could not be used on Standard_Output.

{8652/0055} {AI95-00026-01} {AI95-00256-01} Corrigendum: Added wording to describe the effects of the various operations on the current index. The Amendment adds an explanation of the use of current index for Read and Write.

{8652/0056} {AI95-00001-01} Corrigendum: Clarified that Stream can raise Status_Error, and clarified that using a Stream_Access whose file has been closed is erroneous.

{AI95-00085-01} Clarified that Set_Mode can be called with the current mode. 


#### Extensions to Ada 2005

{AI05-0283-1} Package Ada.Streams.Stream_IO is now preelaborated, allowing it to be used in more contexts (including in distributed systems). Note that it is not a remote types package; File_Type objects cannot be passed between partitions. 


#### Incompatibilities With Ada 2012

{AI12-0021-1} The Wide_File_Names and Wide_Wide_File_Names nested packages are newly added to Ada.Stream_IO. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0102-1} Corrigendum: Type Ada.Streams.Stream_IO.File_Type now has preelaborable initialization. This allows declaring library-level file objects in preelaborable packages (an oversight from the change above). 


#### Wording Changes from Ada 2012

{AI12-0130-1} Corrigendum: The definition of the Flush procedure was moved to A.8.2, so that it could be shared by all of the I/O packages. 


## A.12.2  The Package Text_IO.Text_Streams

The package Text_IO.Text_Streams provides a function for treating a text file as a stream. 


#### Static Semantics

The library package Text_IO.Text_Streams has the following declaration: 

```ada
{AI12-0302-1} with Ada.Streams;
package Ada.Text_IO.Text_Streams
   with Global =&gt in out synchronized is
   type Stream_Access is access all Streams.Root_Stream_Type'Class;

```

```ada
   function Stream (File : in File_Type) return Stream_Access;
end Ada.Text_IO.Text_Streams;

```

The Stream function has the same effect as the corresponding function in Streams.Stream_IO. 

NOTE 1   The ability to obtain a stream for a text file allows Current_Input, Current_Output, and Current_Error to be processed with the functionality of streams, including the mixing of text and binary input-output, and the mixing of binary input-output for different types.

NOTE 2   Performing operations on the stream associated with a text file does not affect the column, line, or page counts. 


## A.12.3  The Package Wide_Text_IO.Text_Streams

The package Wide_Text_IO.Text_Streams provides a function for treating a wide text file as a stream. 


#### Static Semantics

The library package Wide_Text_IO.Text_Streams has the following declaration: 

```ada
{AI12-0302-1} with Ada.Streams;
package Ada.Wide_Text_IO.Text_Streams
   with Global =&gt in out synchronized is
   type Stream_Access is access all Streams.Root_Stream_Type'Class;

```

```ada
   function Stream (File : in File_Type) return Stream_Access;
end Ada.Wide_Text_IO.Text_Streams;

```

The Stream function has the same effect as the corresponding function in Streams.Stream_IO. 


## A.12.4  The Package Wide_Wide_Text_IO.Text_Streams

{AI95-00285-01} The package Wide_Wide_Text_IO.Text_Streams provides a function for treating a wide wide text file as a stream. 


#### Static Semantics

{AI95-00285-01} The library package Wide_Wide_Text_IO.Text_Streams has the following declaration: 

```ada
{AI12-0302-1} with Ada.Streams;
package Ada.Wide_Wide_Text_IO.Text_Streams
   with Global =&gt in out synchronized is
   type Stream_Access is access all Streams.Root_Stream_Type'Class;

```

```ada
   function Stream (File : in File_Type) return Stream_Access;
end Ada.Wide_Wide_Text_IO.Text_Streams;

```

{AI95-00285-01} The Stream function has the same effect as the corresponding function in Streams.Stream_IO. 


#### Extensions to Ada 95

{AI95-00285-01} Package Wide_Wide_Text_IO.Text_Streams is new. 

