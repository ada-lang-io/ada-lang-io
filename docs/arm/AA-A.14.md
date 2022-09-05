---
sidebar_position:  131
---

# A.14  File Sharing


#### Dynamic Semantics

It is not specified by the language whether the same external file can be associated with more than one file object. If such sharing is supported by the implementation, the following effects are defined: 

Operations on one text file object do not affect the column, line, and page numbers of any other file object.

This paragraph was deleted.{8652/0057} {AI95-00050-01} 

For direct and stream files, the current index is a property of each file object; an operation on one file object does not affect the current index of any other file object.

For direct and stream files, the current size of the file is a property of the external file. 

All other effects are identical. 


#### Wording Changes from Ada 95

{8652/0057} {AI95-00050-01} Corrigendum: Removed the incorrect statement that the external files associated with the standard input, standard output, and standard error files are distinct. 

