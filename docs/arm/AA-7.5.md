---
sidebar_position:  62
---

# 7.5  Limited Types

{AI95-00287-01} [A limited type is (a view of) a type for which copying (such as for an [assignment_statement](./AA-5.2#S0173)) is not allowed. A nonlimited type is a (view of a) type for which copying is allowed.] 

Discussion: The concept of the value of a limited type is difficult to define, since the abstract value of a limited type often extends beyond its physical representation. In some sense, values of a limited type cannot be divorced from their object. The value is the object.

{AI95-00318-02} In Ada 83, in the two places where limited types were defined by the language, namely tasks and files, an implicit level of indirection was implied by the semantics to avoid the separation of the value from an associated object. In Ada 95, most limited types are passed by reference, and even return-ed by reference. In Ada 2005, most limited types are built-in-place upon return, rather than returned by reference. Thus the object "identity" is part of the logical value of most limited types. 

To be honest: {AI95-00287-01} {AI95-00419-01} For a limited partial view whose full view is nonlimited, copying is possible on parameter passing and function return. To prevent any copying whatsoever, one should make both the partial and full views limited. 

Glossary entry: A limited type is a type for which copying (such as in an [assignment_statement](./AA-5.2#S0173)) is not allowed. A nonlimited type is a type for which copying is allowed.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[limited type], Def=[a type for which copying (such as in an [assignment_statement](./AA-5.2#S0173)) is not allowed], Note1=[A nonlimited type is a type for which copying is allowed.] 


#### Legality Rules

{AI95-00419-01} If a tagged record type has any limited components, then the reserved word limited shall appear in its [record_type_definition](./AA-3.8#S0066). [If the reserved word limited appears in the definition of a [derived_type_definition](./AA-3.4#S0035), its parent type and any progenitor interfaces shall be limited.] 

Proof: {AI95-00419-01} {AI12-0005-1} The rule about the parent type being required to be limited can be found in 3.4. Rules about progenitor interfaces can be found in 3.9.4; specifically, a nonlimited interface can appear only on a nonlimited type. We repeat these rules here to gather these scattered rules in one obvious place. 

Reason: This prevents tagged limited types from becoming nonlimited. Otherwise, the following could happen: 

```ada
package P is
    type T is limited private;
    type R is tagged
        record -- Illegal!
               -- This should say "limited record".
            X : T;
        end record;
private
    type T is new Integer; -- R becomes nonlimited here.
end P;

```

```ada
package Q is
    type R2 is new R with
        record
            Y : Some_Task_Type;
        end record;
end Q;

```

{AI95-00230-01} If the above were legal, then assignment would be defined for R'Class in the body of P, which is bad news, given the task. 

{AI95-00287-01} {AI95-00318-02} {AI05-0147-1} {AI12-0172-1} {AI12-0236-1} {AI12-0317-1} In the following contexts, an [expression](./AA-4.4#S0132) of a limited type is permitted only if each of its operative constituents is newly constructed (see 4.4): 

the initialization [expression](./AA-4.4#S0132) of an [object_declaration](./AA-3.3#S0032) (see 3.3.1)

the [default_expression](./AA-3.7#S0063) of a [component_declaration](./AA-3.8#S0070) (see 3.8)

the [expression](./AA-4.4#S0132) of a [record_component_association](./AA-4.3#S0109) (see 4.3.1)

the [expression](./AA-4.4#S0132) for an [ancestor_part](./AA-4.3#S0112) of an [extension_aggregate](./AA-4.3#S0111) (see 4.3.2)

an [expression](./AA-4.4#S0132) of a [positional_array_aggregate](./AA-4.3#S0114) or the [expression](./AA-4.4#S0132) of an [array_component_association](./AA-4.3#S0118) (see 4.3.3)

{AI12-0127-1} the base_[expression](./AA-4.4#S0132) of a [record_delta_aggregate](./AA-4.3#S0121) (see 4.3.4) 

Ramification: {AI12-0127-1} We don't need to mention the base_[expression](./AA-4.4#S0132) of an [array_delta_aggregate](./AA-4.3#S0122) here, as its type cannot be limited (see 4.3.4), and thus neither can its base_[expression](./AA-4.4#S0132). Similarly, we do not need any rules for components of a [delta_aggregate](./AA-4.3#S0120) nor the elements of a [container_aggregate](./AA-4.3#S0123), as neither are allowed to be limited (see 4.3.4 and 4.3.5) 

the [qualified_expression](./AA-4.7#S0163) of an initialized allocator (see 4.8)

the [expression](./AA-4.4#S0132) of a return statement (see 6.5)

{AI05-0177-1} {AI12-0157-1} the return expression of an expression function (see 6.8)

the [default_expression](./AA-3.7#S0063) or actual parameter for a formal object of mode in (see 12.4)

Discussion: All of these contexts normally require copying; by restricting the uses as above, we can require the new object to be built-in-place. 


#### Static Semantics

{AI95-00419-01} {AI05-0178-1} A view of a type is limited if it is one of the following: 

{AI95-00411-01} {AI95-00419-01} a type with the reserved word limited, synchronized, task, or protected in its definition; 

Ramification: Note that there is always a "definition", conceptually, even if there is no syntactic category called "..._definition".

{AI95-00419-01} This includes interfaces of the above kinds, derived types with the reserved word limited, as well as task and protected types. 

{AI95-00419-01} {AI05-0087-1} a class-wide type whose specific type is limited;

{AI95-00419-01} a composite type with a limited component;

{AI05-0178-1} an incomplete view;

{AI95-00419-01} a derived type whose parent is limited and is not an interface.

Ramification: {AI95-00419-01} Limitedness is not inherited from interfaces; it must be explicitly specified when the parent is an interface. 

To be honest: {AI95-00419-01} A derived type can become nonlimited if limited does not appear and the derivation takes place in the visible part of a child package, and the parent type is nonlimited as viewed from the private part or body of the child package. 

Reason: {AI95-00419-01} We considered a rule where limitedness was always inherited from the parent for derived types, but in the case of a type whose parent is an interface, this meant that the first interface is treated differently than other interfaces. It also would have forced users to declare dummy nonlimited interfaces just to get the limitedness right. We also considered a syntax like not limited to specify nonlimitedness when the parent was limited, but that was unsavory. The rule given is more uniform and simpler to understand.

{AI95-00419-01} The rules for interfaces are asymmetrical, but the language is not: if the parent interface is limited, the presence of the word limited determines the limitedness, and nonlimited progenitors are illegal by the rules in 3.9.4 if limited is present. If the parent interface is nonlimited, the word limited is illegal by the rules in 3.4. The net effect is that the order of the interfaces doesn't matter. 

Otherwise, the type is nonlimited.

[There are no predefined equality operators for a limited type.]

{AI05-0052-1} A type is immutably limited if it is one of the following:

An explicitly limited record type;

{AI05-0217-1} A record extension with the reserved word limited;

A nonformal limited private type that is tagged or has at least one access discriminant with a [default_expression](./AA-3.7#S0063);

Reason: The full type in both of these cases must necessarily be immutably limited. We need to include private types as much as possible so that we aren't unintentionally discouraging the use of private types. 

A task type, a protected type, or a synchronized interface;

A type derived from an immutably limited type.

Discussion: An immutably limited type is a type that cannot become nonlimited subsequently in a private part or in a child unit. If a view of the type makes it immutably limited, then no copying (assignment) operations are ever available for objects of the type. This allows other properties; for instance, it is safe for such objects to have access discriminants that have defaults or designate other limited objects. 

Ramification: A nonsynchronized limited interface type is not immutably limited; a type derived from it can be nonlimited. 

{AI05-0052-1} A descendant of a generic formal limited private type is presumed to be immutably limited except within the body of a generic unit or a body declared within the declarative region of a generic unit, if the formal type is declared within the formal part of the generic unit.

Ramification: In an instance, a type is descended from the actual type corresponding to the formal, and all rules are rechecked in the specification. Bodies are excepted so that we assume the worst there; the complex wording is required to handle children of generics and unrelated bodies properly. 

NOTE 1   {AI95-00287-01} {AI95-00318-02} {AI05-0067-1} {AI12-0442-1} While it is allowed to write initializations of limited objects, such initializations never copy a limited object. The source of such an assignment operation will be an [aggregate](./AA-4.3#S0106) or [function_call](./AA-6.4#S0218), and such [aggregate](./AA-4.3#S0106)s and [function_call](./AA-6.4#S0218)s will be built directly in the target object (see 7.6). 

To be honest: This isn't quite true if the type can become nonlimited (see below); [function_call](./AA-6.4#S0218)s only are required to be build-in-place for "really" limited types. 

Paragraphs 10 through 15 were deleted. 

NOTE 2   As illustrated in 7.3.1, an untagged limited type can become nonlimited under certain circumstances. 

Ramification: Limited private types do not become nonlimited; instead, their full view can be nonlimited, which has a similar effect.

It is important to remember that a single nonprivate type can be both limited and nonlimited in different parts of its scope. In other words, "limited" is a property that depends on where you are in the scope of the type. We don't call this a "view property" because there is no particular declaration to declare the nonlimited view.

Tagged types never become nonlimited. 


#### Examples

Example of a package with a limited type: 

```ada
package IO_Package is
   type File_Name is limited private;

```

```ada
   procedure Open (F : in out File_Name);
   procedure Close(F : in out File_Name);
   procedure Read (F : in File_Name; Item : out Integer);
   procedure Write(F : in File_Name; Item : in  Integer);
private
   type File_Name is
      limited record
         Internal_Name : Integer := 0;
      end record;
end IO_Package;

```

```ada
package body IO_Package is
   Limit : constant := 200;
   type File_Descriptor is record  ...  end record;
   Directory : array (1 .. Limit) of File_Descriptor;
   ...
   procedure Open (F : in out File_Name) is  ...  end;
   procedure Close(F : in out File_Name) is  ...  end;
   procedure Read (F : in File_Name; Item : out Integer) is ... end;
   procedure Write(F : in File_Name; Item : in  Integer) is ... end;
begin
   ...
end IO_Package;

```

NOTE 3   {AI12-0440-1} Notes on the example: In the example above, an outside subprogram making use of IO_Package can obtain a file name by calling Open and later use it in calls to Read and Write. Thus, outside the package, a file name obtained from Open acts as a kind of password; its internal properties (such as containing a numeric value) are not known and no other operations (such as addition or comparison of internal names) can be performed on a file name. Most importantly, clients of the package cannot make copies of objects of type File_Name.

This example is characteristic of any case where complete control over the operations of a type is desired. Such packages serve a dual purpose. They prevent a user from making use of the internal structure of the type. They also implement the notion of an encapsulated data type where the only operations on the type are those given in the package specification.

{AI95-00318-02} The fact that the full view of File_Name is explicitly declared limited means that parameter passing will always be by reference and function results will always be built directly in the result object (see 6.2 and 6.5).


#### Extensions to Ada 83

The restrictions in RM83-7.4.4(4), which disallowed out parameters of limited types in certain cases, are removed. 


#### Wording Changes from Ada 83

{AI05-0299-1} Since limitedness and privateness are orthogonal in Ada 95 (and to some extent in Ada 83), this is now its own subclause rather than being a subclause of 7.3, "Private Types and Private Extensions". 


#### Extensions to Ada 95

{AI95-00287-01} {AI95-00318-02} Limited types now have an assignment operation, but its use is restricted such that all uses are build-in-place. This is accomplished by restricting uses to [aggregate](./AA-4.3#S0106)s and [function_call](./AA-6.4#S0218)s. [Aggregate](./AA-4.3#S0106)s were not allowed to have a limited type in Ada 95, which causes a compatibility issue discussed in 4.3, "Aggregates". Compatibility issues with return statements for limited [function_call](./AA-6.4#S0218)s are discussed in 6.5, "Return Statements". 


#### Wording Changes from Ada 95

{AI95-00411-01} {AI95-00419-01} Rewrote the definition of limited to ensure that interfaces are covered, but that limitedness is not inherited from interfaces. Derived types that explicitly include limited are now also covered. 


#### Wording Changes from Ada 2005

{AI05-0052-1} {AI05-0217-1} Correction: Added a definition for immutably limited types, so that the fairly complex definition does not need to be repeated in rules elsewhere in the Reference Manual.

{AI05-0067-1} {AI05-0299-1} Correction: The built-in-place rules are consolidated in 7.6, and thus they are removed from this subclause.

{AI05-0087-1} Correction: Fixed an oversight: class-wide types were never defined to be limited, even if their associated specific type is. It is thought that this oversight was never implemented incorrectly by any compiler, thus we have not classified it as an incompatibility.

{AI05-0147-1} Allowed [conditional_expression](./AA-4.5#S0148)s in limited constructor contexts - we want to treat these as closely to parentheses as possible.

{AI05-0177-1} Added wording so that expression functions can return limited entities.

{AI05-0178-1} Correction: Added incomplete views to the list of reasons for a view of a type to be limited. This is not a change as the definition already was in 3.10.1. But it is much better to have all of the reasons for limitedness together. 


#### Extensions to Ada 2012

{AI12-0172-1} Correction: A [raise_expression](./AA-11.3#S0309) can be used in an expression used in a limited context. This is harmless (no object will be created if an exception is raised instead), useful, and thus appears to have been an omission when [raise_expression](./AA-11.3#S0309)s were added to the language. 


#### Wording Changes from Ada 2012

{AI12-0127-1} Added the base_[expression](./AA-4.4#S0132) of a [delta_aggregate](./AA-4.3#S0120) as a limited context. 

