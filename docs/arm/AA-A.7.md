---
sidebar_position:  124
---

# A.7  External Files and File Objects


#### Static Semantics

Values input from the external environment of the program, or output to the external environment, are considered to occupy external files. An external file can be anything external to the program that can produce a value to be read or receive a value to be written. An external file is identified by a string (the name). A second string (the form) gives further system-dependent characteristics that may be associated with the file, such as the physical organization or access rights. The conventions governing the interpretation of such strings shall be documented.

{AI05-0299-1} Input and output operations are expressed as operations on objects of some file type, rather than directly in terms of the external files. In the remainder of this clause, the term file is always used to refer to a file object; the term external file is used otherwise.

Input-output for sequential files of values of a single element type is defined by means of the generic package Sequential_IO. In order to define sequential input-output for a given element type, an instantiation of this generic unit, with the given type as actual parameter, has to be declared. The resulting package contains the declaration of a file type (called File_Type) for files of such elements, as well as the operations applicable to these files, such as the Open, Read, and Write procedures.

{AI95-00285-01} Input-output for direct access files is likewise defined by a generic package called Direct_IO. Input-output in human-readable form is defined by the (nongeneric) packages Text_IO for Character and String data, Wide_Text_IO for Wide_Character and Wide_String data, and Wide_Wide_Text_IO for Wide_Wide_Character and Wide_Wide_String data. Input-output for files containing streams of elements representing values of possibly different types is defined by means of the (nongeneric) package Streams.Stream_IO.

Before input or output operations can be performed on a file, the file first has to be associated with an external file. While such an association is in effect, the file is said to be open, and otherwise the file is said to be closed.

The language does not define what happens to external files after the completion of the main program and all the library tasks (in particular, if corresponding files have not been closed). The effect of input-output for access types is unspecified.

An open file has a current mode, which is a value of one of the following enumeration types: 

```ada
type File_Mode is (In_File, Inout_File, Out_File);  --  for Direct_IO

```

These values correspond respectively to the cases where only reading, both reading and writing, or only writing are to be performed. 

```ada
{AI95-00285-01} type File_Mode is (In_File, Out_File, Append_File);
--  for Sequential_IO, Text_IO, Wide_Text_IO, Wide_Wide_Text_IO, and Stream_IO

```

These values correspond respectively to the cases where only reading, only writing, or only appending are to be performed.

The mode of a file can be changed. 

{AI95-00285-01} Several file management operations are common to Sequential_IO, Direct_IO, Text_IO, Wide_Text_IO, and Wide_Wide_Text_IO. These operations are described in subclause A.8.2 for sequential and direct files. Any additional effects concerning text input-output are described in subclause A.10.2.

{AI05-0299-1} The exceptions that can be propagated by the execution of an input-output subprogram are defined in the package IO_Exceptions; the situations in which they can be propagated are described following the description of the subprogram (and in subclause A.13). The exceptions Storage_Error and Program_Error may be propagated. (Program_Error can only be propagated due to errors made by the caller of the subprogram.) Finally, exceptions can be propagated in certain implementation-defined situations. 

This paragraph was deleted.

Discussion: The last sentence here is referring to the documentation requirements in A.13, "Exceptions in Input-Output", and the documentation summary item is provided there. 

NOTE 1   {AI95-00285-01} Each instantiation of the generic packages Sequential_IO and Direct_IO declares a different type File_Type. In the case of Text_IO, Wide_Text_IO, Wide_Wide_Text_IO, and Streams.Stream_IO, the corresponding type File_Type is unique.

NOTE 2   {AI12-0440-1} A bidirectional device can often be modeled as two sequential files associated with the device, one of mode In_File, and one of mode Out_File. An implementation can restrict the number of files that can be associated with a given external file. 


#### Wording Changes from Ada 95

{AI95-00285-01} Included package Wide_Wide_Text_IO in this description. 

