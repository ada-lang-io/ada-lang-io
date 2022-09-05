---
sidebar_position:  133
---

# A.16  The Package Directories

{AI95-00248-01} The package Directories provides operations for manipulating files and directories, and their names. 

Discussion: {AI05-0299-1} The notes for this subclause contain the expected interpretations of some of the operations on various target systems. "Unix" refers to the UNIX® operating system, and in most cases also covers Unix-like systems such as Linux and POSIX. "Windows®" refers to the Microsoft® Windows® 2000 operating system and usually also covers most other versions that use the Win32 API. 


#### Static Semantics

{AI95-00248-01} The library package Directories has the following declaration: 

```ada
{AI12-0302-1} with Ada.IO_Exceptions;
with Ada.Calendar;
package Ada.Directories 
   with Global =&gt in out synchronized is

```

```ada
   -- Directory and file operations:

```

```ada
   function Current_Directory return String;

```

```ada
   procedure Set_Directory (Directory : in String);

```

```ada
   procedure Create_Directory (New_Directory : in String;
                               Form          : in String := "");

```

```ada
   procedure Delete_Directory (Directory : in String);

```

```ada
   procedure Create_Path (New_Directory : in String;
                          Form          : in String := "");

```

```ada
   procedure Delete_Tree (Directory : in String);

```

```ada
   procedure Delete_File (Name : in String);

```

```ada
   procedure Rename (Old_Name, New_Name : in String);

```

```ada
   procedure Copy_File (Source_Name,
                        Target_Name : in String;
                        Form        : in String := "");

```

```ada
   -- File and directory name operations:

```

```ada
{AI12-0241-1}    function Full_Name (Name : in String) return String
      with Nonblocking;

```

```ada
{AI12-0241-1}    function Simple_Name (Name : in String) return String
      with Nonblocking;

```

```ada
{AI12-0241-1}    function Containing_Directory (Name : in String) return String
      with Nonblocking;

```

```ada
{AI12-0241-1}    function Extension (Name : in String) return String
      with Nonblocking;

```

```ada
{AI12-0241-1}    function Base_Name (Name : in String) return String
      with Nonblocking;

```

```ada
{AI12-0241-1}    function Compose (Containing_Directory : in String := "";
                     Name                 : in String;
                     Extension            : in String := "") return String
      with Nonblocking;

```

```ada
{AI05-0049-1}    type Name_Case_Kind is
      (Unknown, Case_Sensitive, Case_Insensitive, Case_Preserving);

```

```ada
{AI05-0049-1}    function Name_Case_Equivalence (Name : in String) return Name_Case_Kind;

```

```ada
   -- File and directory queries:

```

```ada
   type File_Kind is (Directory, Ordinary_File, Special_File);

```

```ada
   type File_Size is range 0 .. implementation-defined;

```

```ada
   function Exists (Name : in String) return Boolean;

```

```ada
   function Kind (Name : in String) return File_Kind;

```

```ada
   function Size (Name : in String) return File_Size;

```

```ada
   function Modification_Time (Name : in String) return Ada.Calendar.Time;

```

```ada
   -- Directory searching:

```

```ada
   type Directory_Entry_Type is limited private;

```

```ada
   type Filter_Type is array (File_Kind) of Boolean;

```

```ada
   type Search_Type is limited private;

```

```ada
   procedure Start_Search (Search    : in out Search_Type;
                           Directory : in String;
                           Pattern   : in String;
                           Filter    : in Filter_Type := (others =&gt True));

```

```ada
   procedure End_Search (Search : in out Search_Type);

```

```ada
   function More_Entries (Search : in Search_Type) return Boolean;

```

```ada
   procedure Get_Next_Entry (Search : in out Search_Type;
                             Directory_Entry : out Directory_Entry_Type);

```

```ada
{AI12-0286-1}    procedure Search (
      Directory : in String;
      Pattern   : in String;
      Filter    : in Filter_Type := (others =&gt True);
      Process   : not null access procedure (
          Directory_Entry : in Directory_Entry_Type))
      with Allows_Exit;

```

```ada
   -- Operations on Directory Entries:

```

```ada
   function Simple_Name (Directory_Entry : in Directory_Entry_Type)
       return String;

```

```ada
   function Full_Name (Directory_Entry : in Directory_Entry_Type)
       return String;

```

```ada
   function Kind (Directory_Entry : in Directory_Entry_Type)
       return File_Kind;

```

```ada
   function Size (Directory_Entry : in Directory_Entry_Type)
       return File_Size;

```

```ada
   function Modification_Time (Directory_Entry : in Directory_Entry_Type)
       return Ada.Calendar.Time;

```

```ada
   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;

```

```ada
{AI05-0092-1} private
    ... -- not specified by the language
end Ada.Directories;

```

{AI95-00248-01} External files may be classified as directories, special files, or ordinary files. A directory is an external file that is a container for files on the target system. A special file is an external file that cannot be created or read by a predefined Ada input-output package. External files that are not special files or directories are called ordinary files. 

Ramification: A directory is an external file, although it may not have a name on some targets. A directory is not a special file, as it can be created and read by Directories. 

Discussion: Devices and soft links are examples of special files on Windows® and Unix.

Even if an implementation provides a package to create and read soft links, such links are still special files. 

{AI95-00248-01} A file name is a string identifying an external file. Similarly, a directory name is a string identifying a directory. The interpretation of file names and directory names is implementation defined. 

Implementation defined: The interpretation of file names and directory names.

{AI95-00248-01} The full name of an external file is a full specification of the name of the file. If the external environment allows alternative specifications of the name (for example, abbreviations), the full name should not use such alternatives. A full name typically will include the names of all of the directories that contain the item. The simple name of an external file is the name of the item, not including any containing directory names. Unless otherwise specified, a file name or directory name parameter in a call to a predefined Ada input-output subprogram can be a full name, a simple name, or any other form of name supported by the implementation. 

Discussion: The full name on Unix is a complete path to the root. For Windows®, the full name includes a complete path, as well as a disk name ("C:") or network share name. For both systems, the simple name is the part of the name following the last '/' (or '\' for Windows®). For example, in the name "/usr/randy/ada-directories.ads", "ada-directories.ads" is the simple name. 

Ramification: It is possible for a file or directory name to be neither a full name nor a simple name. For instance, the Unix name "../parent/myfile" is neither a full name nor a simple name. 

{AI12-0337-1} A root directory is a directory that has no containing directory.

Implementation Note: For Unix and Unix-like systems, "/" is the root. For Windows®, "C:\" and "\\Computer\Share" are roots. 

{AI95-00248-01} The default directory is the directory that is used if a directory or file name is not a full name (that is, when the name does not fully identify all of the containing directories). 

Discussion: The default directory is the one maintained by the familiar "cd" command on Unix and Windows®. Note that Windows® maintains separate default directories for each disk drive; implementations should use the natural implementation. 

{AI95-00248-01} A directory entry is a single item in a directory, identifying a single external file (including directories and special files). 

{AI95-00248-01} For each function that returns a string, the lower bound of the returned value is 1.

{AI95-00248-01} The following file and directory operations are provided:

```ada
function Current_Directory return String;

```

Returns the full directory name for the current default directory. The name returned shall be suitable for a future call to Set_Directory. The exception Use_Error is propagated if a default directory is not supported by the external environment.

```ada
procedure Set_Directory (Directory : in String);

```

Sets the current default directory. The exception Name_Error is propagated if the string given as Directory does not identify an existing directory. The exception Use_Error is propagated if the external environment does not support making Directory (in the absence of Name_Error) a default directory.

```ada
procedure Create_Directory (New_Directory : in String;
                            Form          : in String := "");

```

Creates a directory with name New_Directory. The Form parameter can be used to give system-dependent characteristics of the directory; the interpretation of the Form parameter is implementation defined. A null string for Form specifies the use of the default options of the implementation of the new directory. The exception Name_Error is propagated if the string given as New_Directory does not allow the identification of a directory. The exception Use_Error is propagated if the external environment does not support the creation of a directory with the given name (in the absence of Name_Error) and form.

```ada
procedure Delete_Directory (Directory : in String);

```

{AI05-0231-1} Deletes an existing empty directory with name Directory. The exception Name_Error is propagated if the string given as Directory does not identify an existing directory. The exception Use_Error is propagated if the directory is not empty or the external environment does not support the deletion of the directory with the given name (in the absence of Name_Error).

```ada
procedure Create_Path (New_Directory : in String;
                       Form          : in String := "");

```

{AI05-0271-1} Creates zero or more directories with name New_Directory. Each nonexistent directory named by New_Directory is created.[ For example, on a typical Unix system, Create_Path ("/usr/me/my"); would create directory "me" in directory "usr", then create directory "my" in directory "me".] The Form parameter can be used to give system-dependent characteristics of the directory; the interpretation of the Form parameter is implementation defined. A null string for Form specifies the use of the default options of the implementation of the new directory. The exception Name_Error is propagated if the string given as New_Directory does not allow the identification of any directory. The exception Use_Error is propagated if the external environment does not support the creation of any directories with the given name (in the absence of Name_Error) and form. If Use_Error is propagated, it is unspecified whether a portion of the directory path is created.

```ada
procedure Delete_Tree (Directory : in String);

```

Deletes an existing directory with name Directory. The directory and all of its contents (possibly including other directories) are deleted. The exception Name_Error is propagated if the string given as Directory does not identify an existing directory. The exception Use_Error is propagated if the external environment does not support the deletion of the directory or some portion of its contents with the given name (in the absence of Name_Error). If Use_Error is propagated, it is unspecified whether a portion of the contents of the directory is deleted.

```ada
procedure Delete_File (Name : in String);

```

Deletes an existing ordinary or special file with name Name. The exception Name_Error is propagated if the string given as Name does not identify an existing ordinary or special external file. The exception Use_Error is propagated if the external environment does not support the deletion of the file with the given name (in the absence of Name_Error).

```ada
procedure Rename (Old_Name, New_Name : in String);

```

{AI05-0231-1} Renames an existing external file (including directories) with name Old_Name to New_Name. The exception Name_Error is propagated if the string given as Old_Name does not identify an existing external file or if the string given as New_Name does not allow the identification of an external file. The exception Use_Error is propagated if the external environment does not support the renaming of the file with the given name (in the absence of Name_Error). In particular, Use_Error is propagated if a file or directory already exists with name New_Name.

Implementation Note: This operation is expected to work within a single directory, and implementers are encouraged to support it across directories on a single device. Copying files from one device to another is discouraged (that's what Copy_File is for). However, there is no requirement to detect file copying by the target system. If the target system has an API that gives that for "free", it can be used. For Windows®, for instance, MoveFile can be used to implement Rename. 

```ada
{AI05-0092-1} procedure Copy_File (Source_Name,
                     Target_Name : in String;
                     Form        : in String := "");

```

{AI05-0271-1} Copies the contents of the existing external file with name Source_Name to an external file with name Target_Name. The resulting external file is a duplicate of the source external file. The Form parameter can be used to give system-dependent characteristics of the resulting external file; the interpretation of the Form parameter is implementation defined. Exception Name_Error is propagated if the string given as Source_Name does not identify an existing external ordinary or special file, or if the string given as Target_Name does not allow the identification of an external file. The exception Use_Error is propagated if the external environment does not support creating the file with the name given by Target_Name and form given by Form, or copying of the file with the name given by Source_Name (in the absence of Name_Error). If Use_Error is propagated, it is unspecified whether a portion of the file is copied. 

Ramification: Name_Error is always raised if Source_Name identifies a directory. It is up to the implementation whether special files can be copied, or if Use_Error will be raised. 

{AI95-00248-01} The following file and directory name operations are provided:

```ada
function Full_Name (Name : in String) return String;

```

Returns the full name corresponding to the file name specified by Name. The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file (including directories and special files). 

Discussion: Full name means that no abbreviations are used in the returned name, and that it is a full specification of the name. Thus, for Unix and Windows®, the result should be a full path that does not contain any "." or ".." directories. Typically, the default directory is used to fill in any missing information. 

```ada
function Simple_Name (Name : in String) return String;

```

{AI12-0337-1} {AI12-0433-1} Returns the simple name portion of the file name specified by Name. The simple name of a root directory is a name of the root itself. The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file (including directories and special files).

Discussion: {AI12-0337-1} The result of Simple_Name corresponds to the result of the "basename" command on Linux and Unix. If the filename ends with a '/', and is not a root, then "basename" returns the part in front of all of the trailing '/'s. It returns a root intact. The null string is never returned. Similar rules should be used for Windows filenames. 

```ada
function Containing_Directory (Name : in String) return String;

```

Returns the name of the containing directory of the external file (including directories) identified by Name. (If more than one directory can contain Name, the directory name returned is implementation defined.) The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file. The exception Use_Error is propagated if the external file does not have a containing directory. 

Discussion: This is purely a string manipulation function. If Name is not given as a full name, the containing directory probably won't be one, either. For example, if Containing_Directory ("..\AARM\RM-A-8") is called on Windows®, the result should be "..\AARM". If there is no path at all on the name, the result should be "." (which represents the current directory). Use Full_Name on the result of Containing_Directory if the full name is needed. 

Ramification: {AI12-0337-1} Containing_Directory raises Use_Error when passed a string representing a root directory. 

```ada
function Extension (Name : in String) return String;

```

Returns the extension name corresponding to Name. The extension name is a portion of a simple name (not including any separator characters), typically used to identify the file class. If the external environment does not have extension names, then the null string is returned. The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file. 

Discussion: For Unix and Windows®, the extension is the portion of the simple name following the rightmost period. For example, in the simple name "RM-A-8.html", the extension is "html". 

```ada
function Base_Name (Name : in String) return String;

```

Returns the base name corresponding to Name. The base name is the remainder of a simple name after removing any extension and extension separators. The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file (including directories and special files). 

Discussion: For Unix and Windows®, the base name is the portion of the simple name preceding the rightmost period (except for the special directory names "." and "..", whose Base_Name is "." and ".."). For example, in the simple name "RM-A-8.html", the base name is "RM-A-8". 

```ada
function Compose (Containing_Directory : in String := "";
                  Name                 : in String;
                  Extension            : in String := "") return String;

```

{AI05-0264-1} {AI12-0337-1} Returns the name of the external file with the specified Containing_Directory, Name, and Extension. If Extension is the null string, then Name is interpreted as a simple name; otherwise, Name is interpreted as a base name. The exception Name_Error is propagated if: 

the string given as Containing_Directory is not null and does not allow the identification of a directory;

the string given as Extension is not null and is not a possible extension;

the string given as Name is not a possible simple name (if Extension is null) or base name (if Extension is nonnull); or

{AI12-0337-1} the string given as Name is a root directory, and Containing_Directory or Extension is nonnull. 

Ramification: The above definition implies that if the Extension is null, for Unix and Windows® no '.' is added to Name. 

Discussion: If Name is null, Name_Error should be raised, as nothing is not a possible simple name or base name.

Generally, Compose(Containing_Directory(F), Base_Name(F),Extension(F)) = F. However, this is not true on Unix or Windows® for file names that end with a '.'; Compose(Base_Name("Fooey."),Extension("Fooey.")) = "Fooey". This is not a problem for Windows®, as the names have the same meaning with or without the '.', but these are different names for Unix. Thus, care needs to be taken on Unix; if Extension is null, Base_Name should be avoided. (That's not usually a problem with file names generated by a program.) 

```ada
{AI05-0049-1} function Name_Case_Equivalence (Name : in String) return Name_Case_Kind;

```

{AI05-0049-1} {AI05-0248-1} Returns the file name equivalence rule for the directory containing Name. Raises Name_Error if Name is not a full name. Returns Case_Sensitive if file names that differ only in the case of letters are considered different names. If file names that differ only in the case of letters are considered the same name, then Case_Preserving is returned if names have the case of the file name used when a file is created; and Case_Insensitive is returned otherwise. Returns Unknown if the file name equivalence is not known. 

Implementation Note: Unix, Linux, and their relatives are Case_Sensitive systems. Microsoft® Windows® is a Case_Preserving system (unless the rarely used POSIX mode is used). Ancient systems like CP/M and early MS-DOS were Case_Insensitive systems (file names were always in UPPER CASE). Unknown is provided in case it is impossible to tell (such as could be the case for network files). 

{AI95-00248-01} The following file and directory queries and types are provided:

```ada
type File_Kind is (Directory, Ordinary_File, Special_File);

```

The type File_Kind represents the kind of file represented by an external file or directory.

```ada
type File_Size is range 0 .. implementation-defined;

```

The type File_Size represents the size of an external file. 

Implementation defined: The maximum value for a file size in Directories.

```ada
function Exists (Name : in String) return Boolean;

```

Returns True if an external file represented by Name exists, and False otherwise. The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file (including directories and special files).

```ada
function Kind (Name : in String) return File_Kind;

```

Returns the kind of external file represented by Name. The exception Name_Error is propagated if the string given as Name does not allow the identification of an existing external file.

```ada
function Size (Name : in String) return File_Size;

```

Returns the size of the external file represented by Name. The size of an external file is the number of stream elements contained in the file. If the external file is not an ordinary file, the result is implementation defined. The exception Name_Error is propagated if the string given as Name does not allow the identification of an existing external file. The exception Constraint_Error is propagated if the file size is not a value of type File_Size. 

Implementation defined: The result for Directories.Size for a directory or special file.

Discussion: We allow raising Constraint_Error, so that an implementation for a system with 64-bit file sizes does not need to support full numerics on 64-bit integers just to implement this package. Of course, if 64-bit integers are available on such a system, they should be used when defining type File_Size. 

```ada
function Modification_Time (Name : in String) return Ada.Calendar.Time;

```

Returns the time that the external file represented by Name was most recently modified. If the external file is not an ordinary file, the result is implementation defined. The exception Name_Error is propagated if the string given as Name does not allow the identification of an existing external file. The exception Use_Error is propagated if the external environment does not support reading the modification time of the file with the name given by Name (in the absence of Name_Error). 

Implementation defined: The result for Directories.Modification_Time for a directory or special file.

{AI95-00248-01} The following directory searching operations and types are provided:

```ada
type Directory_Entry_Type is limited private;

```

The type Directory_Entry_Type represents a single item in a directory. These items can only be created by the Get_Next_Entry procedure in this package. Information about the item can be obtained from the functions declared in this package. A default-initialized object of this type is invalid; objects returned from Get_Next_Entry are valid.

```ada
type Filter_Type is array (File_Kind) of Boolean;

```

The type Filter_Type specifies which directory entries are provided from a search operation. If the Directory component is True, directory entries representing directories are provided. If the Ordinary_File component is True, directory entries representing ordinary files are provided. If the Special_File component is True, directory entries representing special files are provided.

```ada
type Search_Type is limited private;

```

The type Search_Type contains the state of a directory search. A default-initialized Search_Type object has no entries available (function More_Entries returns False). Type Search_Type needs finalization (see 7.6).

```ada
procedure Start_Search (Search    : in out Search_Type;
                        Directory : in String;
                        Pattern   : in String;
                        Filter    : in Filter_Type := (others =&gt True));

```

{AI05-0092-1} {AI05-0262-1} Starts a search in the directory named by Directory for entries matching Pattern and Filter. Pattern represents a pattern for matching file names. If Pattern is the null string, all items in the directory are matched; otherwise, the interpretation of Pattern is implementation defined. Only items that match Filter will be returned. After a successful call on Start_Search, the object Search may have entries available, but it may have no entries available if no files or directories match Pattern and Filter. The exception Name_Error is propagated if the string given by Directory does not identify an existing directory, or if Pattern does not allow the identification of any possible external file or directory. The exception Use_Error is propagated if the external environment does not support the searching of the directory with the given name (in the absence of Name_Error). When Start_Search propagates Name_Error or Use_Error, the object Search will have no entries available. 

Implementation defined: The interpretation of a nonnull search pattern in Directories.

```ada
procedure End_Search (Search : in out Search_Type);

```

Ends the search represented by Search. After a successful call on End_Search, the object Search will have no entries available.

Ramification: The only way that a call to End_Search could be unsuccessful if Device_Error (see A.13) is raised because of an underlying failure (or bug). 

```ada
function More_Entries (Search : in Search_Type) return Boolean;

```

Returns True if more entries are available to be returned by a call to Get_Next_Entry for the specified search object, and False otherwise.

```ada
procedure Get_Next_Entry (Search : in out Search_Type;
                          Directory_Entry : out Directory_Entry_Type);

```

{AI05-0262-1} Returns the next Directory_Entry for the search described by Search that matches the pattern and filter. If no further matches are available, Status_Error is raised. It is implementation defined as to whether the results returned by this subprogram are altered if the contents of the directory are altered while the Search object is valid (for example, by another program). The exception Use_Error is propagated if the external environment does not support continued searching of the directory represented by Search. 

Implementation defined: The results of a Directories search if the contents of the directory are altered while a search is in progress.

```ada
{AI12-0286-1} procedure Search (
   Directory : in String;
   Pattern   : in String;
   Filter    : in Filter_Type := (others =&gt True);
   Process   : not null access procedure (
       Directory_Entry : in Directory_Entry_Type))
   with Allows_Exit;

```

{AI05-0092-1} {AI05-0262-1} Searches in the directory named by Directory for entries matching Pattern and Filter. The subprogram designated by Process is called with each matching entry in turn. Pattern represents a pattern for matching file names. If Pattern is the null string, all items in the directory are matched; otherwise, the interpretation of Pattern is implementation defined. Only items that match Filter will be returned. The exception Name_Error is propagated if the string given by Directory does not identify an existing directory, or if Pattern does not allow the identification of any possible external file or directory. The exception Use_Error is propagated if the external environment does not support the searching of the directory with the given name (in the absence of Name_Error). 

Discussion: "In turn" means that the calls to the subprogram designated by Process are not made in parallel; they can be made in any order but must be in sequence. 

```ada
function Simple_Name (Directory_Entry : in Directory_Entry_Type)
     return String;

```

Returns the simple external name of the external file (including directories) represented by Directory_Entry. The format of the name returned is implementation defined. The exception Status_Error is propagated if Directory_Entry is invalid.

```ada
function Full_Name (Directory_Entry : in Directory_Entry_Type)
     return String;

```

Returns the full external name of the external file (including directories) represented by Directory_Entry. The format of the name returned is implementation defined. The exception Status_Error is propagated if Directory_Entry is invalid.

```ada
function Kind (Directory_Entry : in Directory_Entry_Type)
     return File_Kind;

```

Returns the kind of external file represented by Directory_Entry. The exception Status_Error is propagated if Directory_Entry is invalid.

```ada
function Size (Directory_Entry : in Directory_Entry_Type)
     return File_Size;

```

Returns the size of the external file represented by Directory_Entry. The size of an external file is the number of stream elements contained in the file. If the external file represented by Directory_Entry is not an ordinary file, the result is implementation defined. The exception Status_Error is propagated if Directory_Entry is invalid. The exception Constraint_Error is propagated if the file size is not a value of type File_Size.

```ada
function Modification_Time (Directory_Entry : in Directory_Entry_Type)
     return Ada.Calendar.Time;

```

Returns the time that the external file represented by Directory_Entry was most recently modified. If the external file represented by Directory_Entry is not an ordinary file, the result is implementation defined. The exception Status_Error is propagated if Directory_Entry is invalid. The exception Use_Error is propagated if the external environment does not support reading the modification time of the file represented by Directory_Entry.


#### Implementation Requirements

For Copy_File, if Source_Name identifies an existing external ordinary file created by a predefined Ada input-output package, and Target_Name and Form can be used in the Create operation of that input-output package with mode Out_File without raising an exception, then Copy_File shall not propagate Use_Error.

Discussion: This means that Copy_File will copy any file that the Ada programmer could copy (by writing some possibly complicated Ada code). 


#### Implementation Advice

If other information about a file (such as the owner or creation date) is available in a directory entry, the implementation should provide functions in a child package Directories.Information to retrieve it. 

Implementation Advice: Package Directories.Information should be provided to retrieve other information about a file.

Implementation Note: For Windows®, Directories.Information should contain at least the following routines:

```ada
{AI12-0302-1} package Ada.Directories.Information 
   with Global =&gt in out synchronized is
    -- System-specific directory information.
    -- Version for the Microsoft® Windows® operating system.

```

```ada
    function Creation_Time (Name : in String) return Ada.Calendar.Time;

```

```ada
    function Last_Access_Time (Name : in String) return Ada.Calendar.Time;

```

```ada
    function Is_Read_Only (Name : in String) return Boolean;

```

```ada
    function Needs_Archiving (Name : in String) return Boolean;
        -- This generally means that the file needs to be backed up.
        -- The flag is only cleared by backup programs.

```

```ada
    function Is_Compressed (Name : in String) return Boolean;

```

```ada
    function Is_Encrypted (Name : in String) return Boolean;

```

```ada
    function Is_Hidden (Name : in String) return Boolean;

```

```ada
    function Is_System (Name : in String) return Boolean;

```

```ada
    function Is_Offline (Name : in String) return Boolean;

```

```ada
    function Is_Temporary (Name : in String) return Boolean;

```

```ada
    function Is_Sparse (Name : in String) return Boolean;

```

```ada
    function Is_Not_Indexed (Name : in String) return Boolean;

```

```ada
    function Creation_Time (Directory_Entry : in Directory_Entry_Type)
         return Ada.Calendar.Time;

```

```ada
    function Last_Access_Time (Directory_Entry : in Directory_Entry_Type)
         return Ada.Calendar.Time;

```

```ada
    function Is_Read_Only (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Needs_Archiving (Directory_Entry : in Directory_Entry_Type) return Boolean;
        -- This generally means that the file needs to be backed up.
        -- The flag is only cleared by backup programs.

```

```ada
    function Is_Compressed (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Is_Encrypted (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Is_Hidden (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Is_System (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Is_Offline (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Is_Temporary (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Is_Sparse (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Is_Not_Indexed (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    -- Additional implementation-defined subprograms allowed here.
end Ada.Directories.Information;

```

For Unix-like systems (Unix, POSIX, Linux, etc.), Directories.Information should contain at least the following routines:

```ada
{AI12-0302-1} package Ada.Directories.Information 
   with Global =&gt in out synchronized is
    -- System-specific directory information.
    -- Unix and similar systems version.

```

```ada
    function Last_Access_Time (Name : in String) return Ada.Calendar.Time;

```

```ada
    function Last_Status_Change_Time (Name : in String) return Ada.Calendar.Time;

```

```ada
    type Permission is
      (Others_Execute, Others_Write, Others_Read,
       Group_Execute,  Group_Write,  Group_Read,
       Owner_Execute,  Owner_Write,  Owner_Read,
       Set_Group_ID,   Set_User_ID);

```

```ada
    type Permission_Set_Type is array (Permission) of Boolean;

```

```ada
    function Permission_Set (Name : in String) return Permission_Set_Type;

```

```ada
    function Owner (Name : in String) return String;
        -- Returns the image of the User_Id. If a definition of User_Id
        -- is available, an implementation-defined version of Owner
        -- returning User_Id should also be defined.

```

```ada
{AI05-0005-1}     function Group (Name : in String) return String;
        -- Returns the image of the Group_Id. If a definition of Group_Id
        -- is available, an implementation-defined version of Group
        -- returning Group_Id should also be defined.

```

```ada
    function Is_Block_Special_File (Name : in String) return Boolean;

```

```ada
    function Is_Character_Special_File (Name : in String) return Boolean;

```

```ada
    function Is_FIFO (Name : in String) return Boolean;

```

```ada
    function Is_Symbolic_Link (Name : in String) return Boolean;

```

```ada
    function Is_Socket (Name : in String) return Boolean;

```

```ada
    function Last_Access_Time (Directory_Entry : in Directory_Entry_Type)
       return Ada.Calendar.Time;

```

```ada
    function Last_Status_Change_Time (Directory_Entry : in Directory_Entry_Type)
       return Ada.Calendar.Time;

```

```ada
    function Permission_Set (Directory_Entry : in Directory_Entry_Type)
       return Permission_Set_Type;

```

```ada
    function Owner (Directory_Entry : in Directory_Entry_Type) return String;
       -- See Owner above.

```

```ada
    function Group (Directory_Entry : in Directory_Entry_Type) return String;
       -- See Group above.

```

```ada
    function Is_Block_Special_File (Directory_Entry : in Directory_Entry_Type)
       return Boolean;

```

```ada
    function Is_Character_Special_File (Directory_Entry : in Directory_Entry_Type)
       return Boolean;

```

```ada
    function Is_FIFO (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    function Is_Symbolic_Link (Directory_Entry : in Directory_Entry_Type)
       return Boolean;

```

```ada
    function Is_Socket (Directory_Entry : in Directory_Entry_Type) return Boolean;

```

```ada
    -- Additional implementation-defined subprograms allowed here.
end Ada.Directories.Information;

```

We give these definitions to give guidance so that every implementation for a given target is not unnecessarily different. Implementers are encouraged to make packages for other targets as similar to these as possible.

{AI05-0231-1} {AI12-0439-1} Start_Search and Search should raise Name_Error if Pattern is malformed, but not if it can represent a file in the directory but does not actually do so.

Implementation Advice: Directories.Start_Search and Directories.Search should raise Name_Error for malformed patterns.

Rename should be supported at least when both New_Name and Old_Name are simple names and New_Name does not identify an existing external file. 

Implementation Advice: Directories.Rename should be supported at least when both New_Name and Old_Name are simple names and New_Name does not identify an existing external file.

Discussion: "Supported" includes raising an exception if either name is malformed, the file to rename doesn't exist, insufficient permission for the operation exists, or similar problems. But this advice requires implementations to document what they do, and tells implementers that simply raising Use_Error isn't acceptable. 

NOTE 1   {AI12-0442-1} The operations Containing_Directory, Full_Name, Simple_Name, Base_Name, Extension, and Compose operate on file names, not external files. The files identified by these operations do not necessarily exist. Name_Error is raised only if the file name is malformed and cannot possibly identify a file. Of these operations, only the result of Full_Name depends on the current default directory; the result of the others depends only on their parameters.

NOTE 2   Using access types, values of Search_Type and Directory_Entry_Type can be saved and queried later. However, another task or application can modify or delete the file represented by a Directory_Entry_Type value or the directory represented by a Search_Type value; such a value can only give the information valid at the time it is created. Therefore, long-term storage of these values is not recommended.

NOTE 3   If the target system does not support directories inside of directories, then Kind will never return Directory and Containing_Directory will always raise Use_Error.

NOTE 4   If the target system does not support creation or deletion of directories, then Create_Directory, Create_Path, Delete_Directory, and Delete_Tree will always propagate Use_Error.

NOTE 5   {AI12-0442-1} To move a file or directory to a different location, use Rename. Most target systems will allow renaming of files from one directory to another. If the target file or directory can already exist, delete it first. 

Discussion: While Rename is only guaranteed to work for name changes within a single directory, its unlikely that implementers would purposely prevent functionality present in the underlying system from working. To move a file totally portably, it's necessary to handle failure of the Rename and fall back to Copy_File and Delete: 

```ada
begin
   Rename (Source, Target);
exception
   when Use_Error =&gt
      Copy_File (Source, Target);
      Delete (Source);
end;

```


#### Extensions to Ada 95

{AI95-00248-01} Package Ada.Directories is new. 


#### Inconsistencies With Ada 2005

{AI05-0231-1} Correction: Clarified when and which exceptions are raised for Start_Search, Search, Delete_Directory, and Rename. If an implementation followed the original incorrect wording, it might raise Use_Error instead of Name_Error for Start_Search and Search, Name_Error instead of Use_Error for Rename, and might have deleted a nonempty directory instead of raising Use_Error for Delete_Directory. The first two cases are very unlikely to matter in practice, and it unlikely that an implementation would have followed the latter implementation strategy, as it would be more work and would make Delete_Directory identical to Delete_Tree (which is obvious nonsense). 


#### Incompatibilities With Ada 2005

{AI05-0049-1} A new enumeration type Name_Case_Kind and a new function Name_Case_Equivalence is added to Directories. If Directories is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) of one of the new entities is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Wording Changes from Ada 2005

{AI05-0271-1} Correction: We now explicitly say that the behavior of Create_Path and Copy_File is unspecified when Use_Error is raised. Nothing has changed here, as the behavior was (implicitly) unspecified in the 2007 Amendment. 


#### Wording Changes from Ada 2012

{AI12-0337-1} {AI12-0433-1} Correction: Clarified the meaning of Simple_Name in the case that the parameter is a root directory. This was not previously described. 


## A.16.1  The Package Directories.Hierarchical_File_Names

{AI05-0049-1} The library package Directories.Hierarchical_File_Names is an optional package providing operations for file name construction and decomposition for targets with hierarchical file naming. 


#### Static Semantics

{AI05-0049-1} If provided, the library package Directories.Hierarchical_File_Names has the following declaration:

```ada
{AI12-0241-1} {AI12-0302-1} package Ada.Directories.Hierarchical_File_Names
   with Nonblocking, Global =&gt in out synchronized is

```

```ada
   function Is_Simple_Name (Name : in String) return Boolean;

```

```ada
   function Is_Root_Directory_Name (Name : in String) return Boolean;

```

```ada
   function Is_Parent_Directory_Name (Name : in String) return Boolean;

```

```ada
   function Is_Current_Directory_Name (Name : in String) return Boolean;

```

```ada
   function Is_Full_Name (Name : in String) return Boolean;

```

```ada
   function Is_Relative_Name (Name : in String) return Boolean;

```

```ada
   function Simple_Name (Name : in String) return String
      renames Ada.Directories.Simple_Name;

```

```ada
   function Containing_Directory (Name : in String) return String
      renames Ada.Directories.Containing_Directory;

```

```ada
   function Initial_Directory (Name : in String) return String;

```

```ada
   function Relative_Name (Name : in String) return String;

```

```ada
   function Compose (Directory      : in String := "";
                     Relative_Name  : in String;
                     Extension      : in String := "") return String;

```

```ada
end Ada.Directories.Hierarchical_File_Names;

```

{AI05-0049-1} {AI05-0269-1} In addition to the operations provided in package Directories.Hierarchical_File_Names, the operations in package Directories can be used with hierarchical file names. In particular, functions Full_Name, Base_Name, and Extension provide additional capabilities for hierarchical file names.

```ada
function Is_Simple_Name (Name : in String) return Boolean;

```

Returns True if Name is a simple name, and returns False otherwise.

Ramification: {AI12-0337-1} Root directories are considered simple names, so this function will return True if Name represents a root. Use Is_Root_Directory if it is necessary to distinguish roots and other simple names. 

```ada
function Is_Root_Directory_Name (Name : in String) return Boolean;

```

Returns True if Name is syntactically a root (a directory that cannot be decomposed further), and returns False otherwise.

```ada
function Is_Parent_Directory_Name (Name : in String) return Boolean;

```

Returns True if Name can be used to indicate symbolically the parent directory of any directory, and returns False otherwise.

Implementation Note: Is_Parent_Directory_Name returns True if and only if Name is ".." for both Unix and Windows®. 

```ada
function Is_Current_Directory_Name (Name : in String) return Boolean;

```

Returns True if Name can be used to indicate symbolically the directory itself for any directory, and returns False otherwise.

Implementation Note: Is_Current_Directory_Name returns True if and only if Name is "." for both Unix and Windows®. 

```ada
function Is_Full_Name (Name : in String) return Boolean;

```

Returns True if the leftmost directory part of Name is a root, and returns False otherwise.

```ada
function Is_Relative_Name (Name : in String) return Boolean;

```

{AI05-0049-1} {AI05-0269-1} Returns True if Name allows the identification of an external file (including directories and special files) but is not a full name, and returns False otherwise.

Ramification: {AI12-0337-1} Relative names include simple names other than root directories as a special case. This function returns False if the syntax of the name is incorrect. 

```ada
function Initial_Directory (Name : in String) return String;

```

{AI05-0049-1} {AI05-0248-1} Returns the leftmost directory part in Name. [That is, it returns a root directory name (for a full name), or one of a parent directory name, a current directory name, or a simple name (for a relative name).] The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file (including directories and special files).

```ada
function Relative_Name (Name : in String) return String;

```

Returns the entire file name except the Initial_Directory portion. The exception Name_Error is propagated if the string given as Name does not allow the identification of an external file (including directories and special files), or if Name has a single part (this includes if any of Is_Simple_Name, Is_Root_Directory_Name, Is_Parent_Directory_Name, or Is_Current_Directory_Name are True).

Ramification: The result might be a simple name. 

```ada
function Compose (Directory      : in String := "";
                  Relative_Name  : in String;
                  Extension      : in String := "") return String;

```

Returns the name of the external file with the specified Directory, Relative_Name, and Extension. The exception Name_Error is propagated if the string given as Directory is not the null string and does not allow the identification of a directory, or if Is_Relative_Name (Relative_Name) is False, or if the string given as Extension is not the null string and is not a possible extension, or if Extension is not the null string and Simple_Name (Relative_Name) is not a base name.

The result of Compose is a full name if Is_Full_Name (Directory) is True; result is a relative name otherwise.

Ramification: Name_Error is raised by Compose if Directory is not the null string, and both Is_Full_Name and Is_Relative_Name return False. 

Discussion: A common security problem is to include a parent directory name in the middle of a file name; this is often used to navigate outside of an intended root directory. We considered attempting to prevent that case by having Compose detect it and raise an exception. But the extra rules necessary were more confusing than helpful.

{AI12-0337-1} We can say more about the details of these operations by adopting the notation of a subscript to specify how many path fragments a particular result has. Then, we can abbreviate "Full Name" as "Full" and "Relative Name" as "Rel". In this notation, Unix file name "a/b" is a Rel(2), "../c/d" is a Rel(3), and "/a/b" is a Full(2). Rel(1) is equivalent to a simple name that is not a root; thus we don't have to describe that separately.

In this notation, 

```ada
  
For N&gt1,
Containing_Directory(Rel(N)) = Leftmost Rel(N-1),
Containing_Directory(Full(N)) = Leftmost Full(N-1),
Else if N = 1, raise Use_Error.
  

```

{AI12-0337-1} Similarly, 

```ada
For N&gt1,
Relative_Name(Rel(N)) = Rightmost Rel(N-1),
Relative_Name(Full(N)) = Rightmost Full(N-1),
Else if N = 1, raise Name_Error.
  

```

Finally, for Compose (ignoring the extension here): 

```ada
Compose (Directory =&gt Full(N), Relative_Name =&gt Rel(M)) =&gt Full(N+M)
Compose (Directory =&gt Rel(N), Relative_Name =&gt Rel(M)) =&gt Rel(N+M)
Name_Error if Relative_Name is a Full(M).
  

```

We didn't try to write wording to reflect these details of these functions. 


#### Implementation Advice

{AI05-0049-1} Directories.Hierarchical_File_Names should be provided for systems with hierarchical file naming, and should not be provided on other systems. 

Implementation Advice: Directories.Hierarchical_File_Names should be provided for systems with hierarchical file naming, and should not be provided on other systems.

Implementation Note: This package should be provided when targeting Microsoft® Windows®, Unix, Linux, and most Unix-like systems. 

NOTE 1   {AI05-0049-1} {AI12-0442-1} These operations operate on file names, not external files. The files identified by these operations do not necessarily exist. Name_Error is raised only as specified or if the file name is malformed and cannot possibly identify a file. The result of these operations depends only on their parameters.

NOTE 2   {AI05-0049-1} Containing_Directory raises Use_Error if Name does not have a containing directory, including when any of Is_Simple_Name, Is_Root_Directory_Name, Is_Parent_Directory_Name, or Is_Current_Directory_Name are True.

Ramification: In particular, the default directory is not used to find the containing directory either when Is_Parent_Directory_Name or Is_Current_Directory_Name is True. As noted above, these functions operate purely on the syntax of the file names and do not attempt to interpret them. If interpretation is needed, Directories.Full_Name can be to expand any shorthands used before calling Containing_Directory. 


#### Extensions to Ada 2005

{AI05-0049-1} Package Ada.Directories.Hierarchical_File_Names is new. 


## A.16.2  The Packages Wide_Directories and Wide_Wide_Directories

{AI12-0021-1} The packages Wide_Directories and Wide_Wide_Directories provide operations for manipulating files and directories, and their names. 


#### Static Semantics

{AI12-0021-1} The specification of package Wide_Directories is the same as for Directories (including its optional child packages Information and Hierarchical_File_Names), except that each occurrence of String is replaced by Wide_String. 

{AI12-0021-1} The specification of package Wide_Wide_Directories is the same as for Directories (including its optional child packages Information and Hierarchical_File_Names), except that each occurrence of String is replaced by Wide_Wide_String. 


#### Extensions to Ada 2012

{AI12-0021-1} These six packages are new. 

