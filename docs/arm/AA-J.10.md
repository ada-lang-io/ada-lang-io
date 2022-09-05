---
sidebar_position:  200
---

# J.10  Specific Suppression of Checks

{AI95-00224-01} Pragma Suppress can be used to suppress checks on specific entities. 


#### Syntax

{AI95-00224-01} The form of a specific Suppress [pragma](./AA-2.8#S0019) is as follows: 

  pragma Suppress([identifier](./AA-2.3#S0002), [On =&gt] [name](./AA-4.1#S0091)); 


#### Legality Rules

{AI95-00224-01} The [identifier](./AA-2.3#S0002) shall be the name of a check (see 11.5). The [name](./AA-4.1#S0091) shall statically denote some entity.

{AI95-00224-01} For a specific Suppress [pragma](./AA-2.8#S0019) that is immediately within a [package_specification](./AA-7.1#S0230), the [name](./AA-4.1#S0091) shall denote an entity (or several overloaded subprograms) declared immediately within the [package_specification](./AA-7.1#S0230). 


#### Static Semantics

{AI95-00224-01} A specific Suppress [pragma](./AA-2.8#S0019) applies to the named check from the place of the [pragma](./AA-2.8#S0019) to the end of the innermost enclosing declarative region, or, if the [pragma](./AA-2.8#S0019) is given in a [package_specification](./AA-7.1#S0230), to the end of the scope of the named entity. The [pragma](./AA-2.8#S0019) applies only to the named entity, or, for a subtype, on objects and values of its type. A specific Suppress [pragma](./AA-2.8#S0019) suppresses the named check for any entities to which it applies (see 11.5). Which checks are associated with a specific entity is not defined by this document.

Discussion: The language doesn't specify exactly which entities control whether a check is performed. For example, in 

```ada
pragma Suppress (Range_Check, On =&gt A);
A := B;

```

whether or not the range check is performed is not specified. The compiler may require that checks are suppressed on B or on the type of A in order to omit the range check. 


#### Implementation Permissions

{AI95-00224-01} An implementation is allowed to place restrictions on specific Suppress [pragma](./AA-2.8#S0019)s. 

NOTE 1   {AI95-00224-01} {AI12-0440-1} An implementation can support a similar On parameter on [pragma](./AA-2.8#S0019) Unsuppress (see 11.5). 


#### Wording Changes from Ada 95

{AI95-00224-01} {AI05-0299-1} This subclause is new. This feature was moved here because it is important for pragma Unsuppress that there be an unambiguous meaning for each checking pragma. For instance, in the example 

```ada
pragma Suppress (Range_Check);
pragma Unsuppress (Range_Check, On =&gt A);
A := B;

```

the user needs to be able to depend on the range check being made on the assignment. But a compiler survey showed that the interpretation of this feature varied widely; trying to define this carefully was likely to cause a lot of user and implementer pain. Thus the feature was moved here, to emphasize that its use is not portable. 

