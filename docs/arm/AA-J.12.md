---
sidebar_position:  202
---

# J.12  Pragma Interface


#### Syntax

{AI95-00284-02} In addition to an identifier, the reserved word interface is allowed as a pragma name, to provide compatibility with a prior edition of this Reference Manual. 

Implementation Note: {AI95-00284-02} All implementations need to at least recognize and ignore this pragma. A syntax error is not an acceptable implementation of this pragma. 


#### Wording Changes from Ada 95

{AI95-00326-01} {AI05-0299-1} This subclause is new. This is necessary as interface is now a reserved word, which would prevent pragma Interface from being an implementation-defined pragma. We don't define any semantics for this pragma, as we expect that implementations will continue to use whatever they currently implement - requiring any changes would be counter-productive. 

