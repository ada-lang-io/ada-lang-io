---
sidebar_position:  50
---

# 6.2  Formal Parameter Modes

[A [parameter_specification](./AA-6.1#S0207) declares a formal parameter of mode in, in out, or out.] 


#### Static Semantics

A parameter is passed either by copy or by reference. [When a parameter is passed by copy, the formal parameter denotes a separate object from the actual parameter, and any information transfer between the two occurs only before and after executing the subprogram. When a parameter is passed by reference, the formal parameter denotes (a view of) the object denoted by the actual parameter; reads and updates of the formal parameter directly reference the actual parameter object.]

{AI05-0142-4} {AI05-0262-1} A type is a by-copy type if it is an elementary type, or if it is a descendant of a private type whose full type is a by-copy type. A parameter of a by-copy type is passed by copy, unless the formal parameter is explicitly aliased.

A type is a by-reference type if it is a descendant of one of the following: 

a tagged type;

a task or protected type;

{AI05-0096-1} an explicitly limited record type; 

This paragraph was deleted.{AI05-0096-1} 

a composite type with a subcomponent of a by-reference type;

a private type whose full type is a by-reference type. 

{AI05-0142-4} {AI05-0188-1} {AI12-0027-1} {AI12-0236-1} {AI12-0317-1} A parameter of a by-reference type is passed by reference, as is an explicitly aliased parameter of any type. Each value of a by-reference type has an associated object. For a value conversion, the associated object is the anonymous result object if such an object is created (see 4.6); otherwise it is the associated object of the operand.  In other cases, the object associated with the evaluated operative constituent of the [name](./AA-4.1#S0091) or [expression](./AA-4.4#S0132) (see 4.4) determines its associated object.

Ramification: By-reference parameter passing makes sense only if there is an object to reference; hence, we define such an object for each case.

Since tagged types are by-reference types, this implies that every value of a tagged type has an associated object. This simplifies things, because we can define the tag to be a property of the object, and not of the value of the object, which makes it clearer that object tags never change.

{AI12-0317-1} A construct like parenthesized expression or [qualified_expression](./AA-4.7#S0163) is ignored for the purposes of determining the associated object; for a [conditional_expression](./AA-4.5#S0148), it is relevant only in that it determines which dependent_[expression](./AA-4.4#S0132) defines the associated object.

We considered simplifying things even more by making every value (and therefore every expression) have an associated object. After all, there is little semantic difference between a constant object and a value. However, this would cause problems for untagged types. In particular, we would have to do a constraint check on every read of a type conversion (or a renaming thereof) in certain cases.

{AI95-00318-02} We do not want this definition to depend on the view of the type; privateness is essentially ignored for this definition. Otherwise, things would be confusing (does the rule apply at the call site, at the site of the declaration of the subprogram, at the site of the return statement?), and requiring different calls to use different mechanisms would be an implementation burden.

C.6, "Shared Variable Control" says that a composite type with an atomic or volatile subcomponent is a by-reference type, among other things.

Every value of a limited by-reference type is the value of one and only one limited object. The associated object of a value of a limited by-reference type is the object whose value it represents. Two values of a limited by-reference type are the same if and only if they represent the value of the same object.

We say "by-reference" above because these statements are not always true for limited private types whose underlying type is nonlimited (unfortunately). 

{AI05-0240-1} For other parameters, it is unspecified whether the parameter is passed by copy or by reference. 

Discussion: {AI05-0005-1} There is no need to incorporate the discussion of AI83-00178, which requires pass-by-copy for certain kinds of actual parameters, while allowing pass-by-reference for others. This is because we explicitly indicate that a function creates an anonymous constant object for its result (see 6.5). We also provide a special dispensation for instances of Unchecked_Conversion to return by reference (see 13.9). 


#### Bounded (Run-Time) Errors

{AI05-0240-1} If one [name](./AA-4.1#S0091) denotes a part of a formal parameter, and a second [name](./AA-4.1#S0091) denotes a part of a distinct formal parameter or an object that is not part of a formal parameter, then the two [name](./AA-4.1#S0091)s are considered distinct access paths. If an object is of a type for which the parameter passing mechanism is not specified and is not an explicitly aliased parameter, then it is a bounded error to assign to the object via one access path, and then read the value of the object via a distinct access path, unless the first access path denotes a part of a formal parameter that no longer exists at the point of the second access [(due to leaving the corresponding callable construct).] The possible consequences are that Program_Error is raised, or the newly assigned value is read, or some old value of the object is read. 

Discussion: For example, if we call "P(X =&gt Global_Variable, Y =&gt Global_Variable)", then within P, the names "X", "Y", and "Global_Variable" are all distinct access paths. If Global_Variable's type is neither pass-by-copy nor pass-by-reference, then it is a bounded error to assign to Global_Variable and then read X or Y, since the language does not specify whether the old or the new value would be read. On the other hand, if Global_Variable's type is pass-by-copy, then the old value would always be read, and there is no error. Similarly, if Global_Variable's type is defined by the language to be pass-by-reference, then the new value would always be read, and again there is no error. 

Reason: We are saying assign here, not update, because updating any subcomponent is considered to update the enclosing object.

The "still exists" part is so that a read after the subprogram returns is OK.

If the parameter is of a by-copy type, then there is no issue here - the formal is not a view of the actual. If the parameter is of a by-reference type, then the programmer may depend on updates through one access path being visible through some other access path, just as if the parameter were of an access type. 

Implementation Note: The implementation can keep a copy in a register of a parameter whose parameter-passing mechanism is not specified. If a different access path is used to update the object (creating a bounded error situation), then the implementation can still use the value of the register, even though the in-memory version of the object has been changed. However, to keep the error properly bounded, if the implementation chooses to read the in-memory version, it has to be consistent -- it cannot then assume that something it has proven about the register is true of the memory location. For example, suppose the formal parameter is L, the value of L(6) is now in a register, and L(6) is used in an [indexed_component](./AA-4.1#S0096) as in "A(L(6)) := 99;", where A has bounds 1..3. If the implementation can prove that the value for L(6) in the register is in the range 1..3, then it need not perform the constraint check if it uses the register value. However, if the memory value of L(6) has been changed to 4, and the implementation uses that memory value, then it had better not alter memory outside of A.

Note that the rule allows the implementation to pass a parameter by reference and then keep just part of it in a register, or, equivalently, to pass part of the parameter by reference and another part by copy. 

Reason: We do not want to go so far as to say that the mere presence of aliasing is wrong. We wish to be able to write the following sorts of things in standard Ada: 

```ada
procedure Move ( Source  : in  String;
                 Target  : out String;
                 Drop    : in  Truncation := Error;
                 Justify : in  Alignment  := Left;
                 Pad     : in  Character  := Space);
-- Copies elements from Source to Target (safely if they overlap)

```

This is from the standard string handling package. It would be embarrassing if this couldn't be written in Ada!

The "then" before "read" in the rule implies that the implementation can move a read to an earlier place in the code, but not to a later place after a potentially aliased assignment. Thus, if the subprogram reads one of its parameters into a local variable, and then updates another potentially aliased one, the local copy is safe - it is known to have the old value. For example, the above-mentioned Move subprogram can be implemented by copying Source into a local variable before assigning into Target.

For an [assignment_statement](./AA-5.2#S0173) assigning one array parameter to another, the implementation has to check which direction to copy at run time, in general, in case the actual parameters are overlapping slices. For example: 

```ada
procedure Copy(X : in out String; Y: String) is
begin
    X := Y;
end Copy;

```

It would be wrong for the compiler to assume that X and Y do not overlap (unless, of course, it can prove otherwise). 

NOTE 1   {AI12-0056-1} The mode of a formal parameter describes the direction of information transfer to or from the [subprogram_body](./AA-6.3#S0216) (see 6.1).

NOTE 2   A formal parameter of mode in is a constant view (see 3.3); it cannot be updated within the [subprogram_body](./AA-6.3#S0216).

NOTE 3   {AI12-0056-1} {AI12-0440-1} A formal parameter of mode out can be uninitialized at the start of the [subprogram_body](./AA-6.3#S0216) (see 6.4.1). 


#### Extensions to Ada 83

The value of an out parameter may be read. An out parameter is treated like a declared variable without an explicit initial expression. 


#### Wording Changes from Ada 83

Discussion of copy-in for parts of out parameters is now covered in 6.4.1, "Parameter Associations".

The concept of a by-reference type is new to Ada 95.

We now cover in a general way in 3.7.2 the rule regarding erroneous execution when a discriminant is changed and one of the parameters depends on the discriminant. 


#### Wording Changes from Ada 2005

{AI05-0096-1} Correction: Corrected so that limited derived types are by-reference only if their parent is.

{AI05-0142-4} Defined that explicitly aliased parameters (see 6.1) are always passed by reference. 


#### Wording Changes from Ada 2012

{AI05-0027-1} Corrigendum: Corrected so that value conversions that are copies are the "associated object" for parameter passing of by-reference types. This can only happen if the conversion is between unrelated non-limited types, and it is necessary just so the correct object is defined. 

