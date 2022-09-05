---
sidebar_position:  119
---

# A.2  The Package Ada


#### Static Semantics

The following language-defined library package exists: 

```ada
{AI12-0414-1} package Ada
   with Pure is
end Ada;

```

{AI12-0414-1} Ada serves as the parent of most of the other language-defined library units; its declaration is empty. 


#### Legality Rules

In the standard mode, it is illegal to compile a child of package Ada. 

Reason: The intention is that mentioning, say, Ada.Text_IO in a [with_clause](./AA-10.1#S0294) is guaranteed (at least in the standard mode) to refer to the standard version of Ada.Text_IO. The user can compile a root library unit Text_IO that has no relation to the standard version of Text_IO. 

Ramification: Note that Ada can have non-language-defined grandchildren, assuming the implementation allows it. Also, packages System and Interfaces can have children, assuming the implementation allows it. 

Implementation Note: An implementation will typically support a nonstandard mode in which compiling the language defined library units is allowed. Whether or not this mode is made available to users is up to the implementer.

An implementation could theoretically have private children of Ada, since that would be semantically neutral. However, a programmer cannot compile such a library unit. 


#### Extensions to Ada 83

{AI05-0299-1} This subclause is new to Ada 95. 

