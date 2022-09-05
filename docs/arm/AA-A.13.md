---
sidebar_position:  130
---

# A.13  Exceptions in Input-Output

{AI12-0445-1} The package IO_Exceptions defines the exceptions used by the predefined input-output packages. 


#### Static Semantics

The library package IO_Exceptions has the following declaration: 

```ada
{AI12-0414-1} package Ada.IO_Exceptions
   with Pure is

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

{AI05-0262-1} These exceptions are also propagated by various other language-defined packages and operations, see the definition of those entities for other reasons that these exceptions are propagated.

Reason: {AI05-0299-1} This subclause is based in Ada 95. Later versions of Ada (starting with Technical Corrigendum 1) have added a number of additional places and reasons that cause these exceptions. In particular, TC1 says that stream attributes need to raise End_Error in some circumstances; Amendment 1 adds Ada.Directories and a number of new places and reasons that Name_Error and Use_Error are raised. There are more. We don't want to try to update this text (or even this note!) for every possible reason and place that might raise one of these exceptions, so we add this blanket statement. 


#### Documentation Requirements

The implementation shall document the conditions under which Name_Error, Use_Error and Device_Error are propagated. 

Documentation Requirement: The conditions under which Io_Exceptions.Name_Error, Io_Exceptions.Use_Error, and Io_Exceptions.Device_Error are propagated.


#### Implementation Permissions

{AI12-0444-1} When the associated check is complex, it is optional to propagate Data_Error as part of a procedure Read (or the Read attribute) when the value read cannot be interpreted as a value of the required subtype. 

Ramification: An example where the implementation may choose not to perform the check is an enumeration type with a representation clause with "holes" in the range of internal codes.


#### Erroneous Execution

[If the element read by the procedure Read (or by the Read attribute) cannot be interpreted as a value of the required subtype, but this is not detected and Data_Error is not propagated, then the resulting value can be abnormal, and subsequent references to the value can lead to erroneous execution, as explained in 13.9.1. ]

