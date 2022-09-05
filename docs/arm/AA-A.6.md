---
sidebar_position:  123
---

# A.6  Input-Output

{AI95-00285-01} {AI12-0445-1} [ Input-output is provided through language-defined packages, each of which is a child of the root package Ada. The generic packages Sequential_IO and Direct_IO define input-output operations applicable to files containing elements of a given type. The generic package Storage_IO supports reading from and writing to an in-memory buffer. Additional operations for text input-output are supplied in the packages Text_IO, Wide_Text_IO, and Wide_Wide_Text_IO. Heterogeneous input-output is provided through the child packages Streams.Stream_IO and Text_IO.Text_Streams (see also 13.13). The package IO_Exceptions defines the exceptions used by the predefined input-output packages.] 


#### Inconsistencies With Ada 83

The introduction of Append_File as a new element of the enumeration type File_Mode in Sequential_IO and Text_IO, and the introduction of several new declarations in Text_IO, may result in name clashes in the presence of use clauses. 


#### Extensions to Ada 83

Text_IO enhancements (Get_Immediate, Look_Ahead, Standard_Error, Modular_IO, Decimal_IO), Wide_Text_IO, and the stream input-output facilities are new in Ada 95. 


#### Wording Changes from Ada 83

RM83-14.6, "Low Level Input-Output", is removed. This has no semantic effect, since the package was entirely implementation defined, nobody actually implemented it, and if they did, they can always provide it as a vendor-supplied package. 


#### Wording Changes from Ada 95

{AI95-00285-01} Included package Wide_Wide_Text_IO in this description. 

