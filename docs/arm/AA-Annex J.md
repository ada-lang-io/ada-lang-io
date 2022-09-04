---
sidebar_position:  23
---

# Annex J Obsolescent Features

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
[ This Annex contains descriptions of features of the language whose functionality is largely redundant with other features defined by this Reference Manual. Use of these features is not recommended in newly written programs. ]

Ramification: These features are still part of the language, and have to be implemented by conforming implementations. The primary reason for putting these descriptions here is to get redundant features out of the way of most readers. The designers of the next version of Ada after Ada 95 will have to assess whether or not it makes sense to drop these features from the language. 


#### Wording Changes from Ada 83

The following features have been removed from the language, rather than declared to be obsolescent: 

The package Low_Level_IO (see A.6).

The Epsilon, Mantissa, Emax, Small, Large, Safe_Emax, Safe_Small, and Safe_Large attributes of floating point types (see A.5.3).

The pragma Interface (see B.1).

The pragmas System_Name, Storage_Unit, and Memory_Size (see 13.7).

The pragma Shared (see C.6). 

Implementations can continue to support the above features for upward compatibility. 


## J.1  Renamings of Ada 83 Library Units


#### Static Semantics

The following library_unit_renaming_declarations exist: 

```ada
with Ada.Unchecked_Conversion;
generic function Unchecked_Conversion renames Ada.Unchecked_Conversion;

```

```ada
with Ada.Unchecked_Deallocation;
generic procedure Unchecked_Deallocation renames Ada.Unchecked_Deallocation;

```

```ada
with Ada.Sequential_IO;
generic package Sequential_IO renames Ada.Sequential_IO;

```

```ada
with Ada.Direct_IO;
generic package Direct_IO renames Ada.Direct_IO;

```

```ada
with Ada.Text_IO;
package Text_IO renames Ada.Text_IO;

```

```ada
with Ada.IO_Exceptions;
package IO_Exceptions renames Ada.IO_Exceptions;

```

```ada
with Ada.Calendar;
package Calendar renames Ada.Calendar;

```

```ada
with System.Machine_Code;
package Machine_Code renames System.Machine_Code; -- If supported.

```


#### Implementation Requirements

The implementation shall allow the user to replace these renamings. 


## J.2  Allowed Replacements of Characters


#### Syntax

The following replacements are allowed for the vertical line, number sign, and quotation mark characters: 

A vertical line character (|) can be replaced by an exclamation mark (!) where used as a delimiter.

The number sign characters (#) of a based_literal can be replaced by colons (:) provided that the replacement is done for both occurrences. 

To be honest: The intent is that such a replacement works in the Value and Wide_Value attributes, and in the Get procedures of Text_IO}, so that things like "16:.123:" is acceptable. 

The quotation marks (") used as string brackets at both ends of a string literal can be replaced by percent signs (%) provided that the enclosed sequence of characters contains no quotation mark, and provided that both string brackets are replaced. Any percent sign within the sequence of characters shall then be doubled and each such doubled percent sign is interpreted as a single percent sign character value. 

These replacements do not change the meaning of the program. 

Reason: The original purpose of this feature was to support hardware (for example, teletype machines) that has long been obsolete. The feature is no longer necessary for that reason. Another use of the feature has been to replace the vertical line character (|) when using certain hardware that treats that character as a (non-English) letter. The feature is no longer necessary for that reason, either, since Ada 95 has full support for international character sets. Therefore, we believe this feature is no longer necessary.

Users of equipment that still uses | to represent a letter will continue to do so. Perhaps by next the time Ada is revised, such equipment will no longer be in use.

Note that it was never legal to use this feature as a convenient method of including double quotes in a string without doubling them - the string literal: 

```ada
%"This is quoted."%

```

is not legal in Ada 83, nor will it be in Ada 95. One has to write: 

```ada
"""This is quoted."""

```


## J.3  Reduced Accuracy Subtypes

A digits_constraint may be used to define a floating point subtype with a new value for its requested decimal precision, as reflected by its Digits attribute. Similarly, a delta_constraint may be used to define an ordinary fixed point subtype with a new value for its delta, as reflected by its Delta attribute. 

Discussion: It might be more direct to make these attributes specifiable via an attribute_definition_clause, and eliminate the syntax for these _constraints. 


#### Syntax

delta_constraint ::= delta static_expression [range_constraint]


#### Name Resolution Rules

The expression of a delta_constraint is expected to be of any real type. 


#### Legality Rules

The expression of a delta_constraint shall be static.

For a subtype_indication with a delta_constraint, the subtype_mark shall denote an ordinary fixed point subtype.

For a subtype_indication with a digits_constraint, the subtype_mark shall denote either a decimal fixed point subtype or a floating point subtype (notwithstanding the rule given in 3.5.9 that only allows a decimal fixed point subtype). 

Discussion: We may need a better way to deal with obsolescent features with rules that contradict those of the nonobsolescent parts of the standard. 


#### Static Semantics

A subtype_indication with a subtype_mark that denotes an ordinary fixed point subtype and a delta_constraint defines an ordinary fixed point subtype with a delta given by the value of the expression of the delta_constraint. If the delta_constraint includes a range_constraint, then the ordinary fixed point subtype is constrained by the range_constraint.

A subtype_indication with a subtype_mark that denotes a floating point subtype and a digits_constraint defines a floating point subtype with a requested decimal precision (as reflected by its Digits attribute) given by the value of the expression of the digits_constraint. If the digits_constraint includes a range_constraint, then the floating point subtype is constrained by the range_constraint. 


#### Dynamic Semantics

A delta_constraint is compatible with an ordinary fixed point subtype if the value of the expression is no less than the delta of the subtype, and the range_constraint, if any, is compatible with the subtype.

A digits_constraint is compatible with a floating point subtype if the value of the expression is no greater than the requested decimal precision of the subtype, and the range_constraint, if any, is compatible with the subtype.

The elaboration of a delta_constraint consists of the elaboration of the range_constraint, if any. 

Reason: A numeric subtype is considered "constrained" only if a range constraint applies to it. The only effect of a digits_constraint or a delta_constraint without a range_constraint is to specify the value of the corresponding Digits or Delta attribute in the new subtype. The set of values of the subtype is not "constrained" in any way by such _constraints. 


#### Wording Changes from Ada 83

In Ada 83, a delta_constraint is called a fixed_point_constraint, and a digits_constraint is called a floating_point_constraint. We have adopted other terms because digits_constraints apply primarily to decimal fixed point types now (they apply to floating point types only as an obsolescent feature). 


## J.4  The Constrained Attribute


#### Static Semantics

For every private subtype S, the following attribute is defined: 

Discussion: This includes generic formal private subtypes. 

S'ConstrainedYields the value False if S denotes an unconstrained nonformal private subtype with discriminants; also yields the value False if S denotes a generic formal private subtype, and the associated actual subtype is either an unconstrained subtype with discriminants or an unconstrained array subtype; yields the value True otherwise. The value of this attribute is of the predefined subtype Boolean. 

Reason: Because Ada 95 has unknown_discriminant_parts, the Constrained attribute of private subtypes is obsolete. This is fortunate, since its Ada 83 definition was confusing, as explained below. Because this attribute is obsolete, we do not bother to extend its definition to private extensions.

The Constrained attribute of an object is not obsolete.

Note well: S'Constrained matches the Ada 95 definition of "constrained" only for composite subtypes. For elementary subtypes, S'Constrained is always true, whether or not S is constrained. (The Constrained attribute of an object does not have this problem, as it is only defined for objects of a discriminated type.) So one should think of its designator as being 'Constrained_Or_Elementary. 


## J.5  ASCII


#### Static Semantics

The following declaration exists in the declaration of package Standard: 

```ada
package ASCII is

```

```ada
  --  Control characters:

```

```ada
  NUL   : constant Character := nul; 	SOH   : constant Character := soh;
  STX   : constant Character := stx; 	ETX   : constant Character := etx;
  EOT   : constant Character := eot; 	ENQ   : constant Character := enq;
  ACK   : constant Character := ack; 	BEL   : constant Character := bel;
  BS    : constant Character := bs; 	HT    : constant Character := ht;
  LF    : constant Character := lf; 	VT    : constant Character := vt;
  FF    : constant Character := ff; 	CR    : constant Character := cr;
  SO    : constant Character := so; 	SI    : constant Character := si;
  DLE   : constant Character := dle; 	DC1   : constant Character := dc1;
  DC2   : constant Character := dc2; 	DC3   : constant Character := dc3;
  DC4   : constant Character := dc4; 	NAK   : constant Character := nak;
  SYN   : constant Character := syn; 	ETB   : constant Character := etb;
  CAN   : constant Character := can; 	EM    : constant Character := em;
  SUB   : constant Character := sub; 	ESC   : constant Character := esc;
  FS    : constant Character := fs; 	GS    : constant Character := gs;
  RS    : constant Character := rs; 	US    : constant Character := us;
  DEL   : constant Character := del;

```

```ada
  -- Other characters:

```

```ada
  Exclam   : constant Character:= '!';	Quotation : constant Character:= '"';
  Sharp    : constant Character:= '#';	Dollar    : constant Character:= '$';
  Percent  : constant Character:= '%';	Ampersand : constant Character:= '&';
  Colon    : constant Character:= ':';	Semicolon : constant Character:= ';';
  Query    : constant Character:= '?';	At_Sign   : constant Character:= '@';
  L_Bracket: constant Character:= '[';	Back_Slash: constant Character:= '\';
  R_Bracket: constant Character:= ']';	Circumflex: constant Character:= '^';
  Underline: constant Character:= '_';	Grave     : constant Character:= '`';
  L_Brace  : constant Character:= '{';	Bar       : constant Character:= '|';
  R_Brace  : constant Character:= '}';	Tilde     : constant Character:= '~';

```

```ada
  -- Lower case letters:

```

```ada
  LC_A: constant Character:= 'a';
  ...
  LC_Z: constant Character:= 'z';

```

```ada
end ASCII;

```


## J.6  Numeric_Error


#### Static Semantics

The following declaration exists in the declaration of package Standard: 

```ada
Numeric_Error : exception renames Constraint_Error;

```

Discussion: This is true even though it is not shown in A.1. 

Reason: In Ada 83, it was unclear which situations should raise Numeric_Error, and which should raise Constraint_Error. The permissions of RM83-11.6 could often be used to allow the implementation to raise Constraint_Error in a situation where one would normally expect Numeric_Error. To avoid this confusion, all situations that raise Numeric_Error in Ada 83 are changed to raise Constraint_Error in Ada 95. Numeric_Error is changed to be a renaming of Constraint_Error to avoid most of the upward compatibilities associated with this change.

In new code, Constraint_Error should be used instead of Numeric_Error. 


## J.7  At Clauses


#### Syntax

at_clause ::= for direct_name use at expression;


#### Static Semantics

An at_clause of the form "for x use at y;" is equivalent to an attribute_definition_clause of the form "for x'Address use y;". 

Reason: The preferred syntax for specifying the address of an entity is an attribute_definition_clause specifying the Address attribute. Therefore, the special-purpose at_clause syntax is now obsolete.

The above equivalence implies, for example, that only one at_clause is allowed for a given entity. Similarly, it is illegal to give both an at_clause and an attribute_definition_clause specifying the Address attribute. 


#### Extensions to Ada 83

We now allow to define the address of an entity using an attribute_definition_clause. This is because Ada 83's at_clause is so hard to remember: programmers often tend to write "for X'Address use...;". 


#### Wording Changes from Ada 83

Ada 83's address_clause is now called an at_clause to avoid confusion with the new term "Address clause" (that is, an attribute_definition_clause for the Address attribute). 


### J.7.1  Interrupt Entries

[Implementations are permitted to allow the attachment of task entries to interrupts via the address clause. Such an entry is referred to as an interrupt entry.

The address of the task entry corresponds to a hardware interrupt in an implementation-defined manner. (See Ada.Interrupts.Reference in C.3.2.)] 


#### Static Semantics

The following attribute is defined:

For any task entry X: 

X'Address For a task entry whose address is specified (an interrupt entry), the value refers to the corresponding hardware interrupt. For such an entry, as for any other task entry, the meaning of this value is implementation defined. The value of this attribute is of the type of the subtype System.Address.

Address may be specified for single entries via an attribute_definition_clause. 

Reason: Because of the equivalence of at_clauses and attribute_definition_clauses, an interrupt entry may be specified via either notation. 


#### Dynamic Semantics

As part of the initialization of a task object, the address clause for an interrupt entry is elaborated[, which evaluates the expression of the address clause]. A check is made that the address specified is associated with some interrupt to which a task entry may be attached. If this check fails, Program_Error is raised. Otherwise, the interrupt entry is attached to the interrupt associated with the specified address.

Upon finalization of the task object, the interrupt entry, if any, is detached from the corresponding interrupt and the default treatment is restored.

While an interrupt entry is attached to an interrupt, the interrupt is reserved (see C.3).

An interrupt delivered to a task entry acts as a call to the entry issued by a hardware task whose priority is in the System.Interrupt_Priority range. It is implementation defined whether the call is performed as an ordinary entry call, a timed entry call, or a conditional entry call; which kind of call is performed can depend on the specific interrupt.


#### Bounded (Run-Time) Errors

It is a bounded error to evaluate E'Caller (see C.7.1) in an accept_statement for an interrupt entry. The possible effects are the same as for calling Current_Task from an entry body. 


#### Documentation Requirements

The implementation shall document to which interrupts a task entry may be attached. 

The implementation shall document whether the invocation of an interrupt entry has the effect of an ordinary entry call, conditional call, or a timed call, and whether the effect varies in the presence of pending interrupts. 


#### Implementation Permissions

The support for this subclause is optional.

Interrupts to which the implementation allows a task entry to be attached may be designated as reserved for the entire duration of program execution[; that is, not just when they have an interrupt entry attached to them].

Interrupt entry calls may be implemented by having the hardware execute directly the appropriate accept body. Alternatively, the implementation is allowed to provide an internal interrupt handler to simulate the effect of a normal task calling the entry.

The implementation is allowed to impose restrictions on the specifications and bodies of tasks that have interrupt entries.

It is implementation defined whether direct calls (from the program) to interrupt entries are allowed.

If a select_statement contains both a terminate_alternative and an accept_alternative for an interrupt entry, then an implementation is allowed to impose further requirements for the selection of the terminate_alternative in addition to those given in 9.3. 

NOTE 1   Queued interrupts correspond to ordinary entry calls. Interrupts that are lost if not immediately processed correspond to conditional entry calls. It is a consequence of the priority rules that an accept body executed in response to an interrupt can be executed with the active priority at which the hardware generates the interrupt, taking precedence over lower priority tasks, without a scheduling action.

NOTE 2   Control information that is supplied upon an interrupt can be passed to an associated interrupt entry as one or more parameters of mode in. 


#### Examples

Example of an interrupt entry: 

```ada
task Interrupt_Handler is
  entry Done;
  for Done'Address use Ada.Interrupts.Reference(Ada.Interrupts.Names.Device_Done);
end Interrupt_Handler;

```


#### Wording Changes from Ada 83

RM83-13.5.1 did not adequately address the problems associate with interrupts. This feature is now obsolescent and is replaced by the Ada 95 interrupt model as specified in the Systems Programming Annex. 


## J.8  Mod Clauses


#### Syntax

mod_clause ::= at mod static_expression;


#### Static Semantics

A record_representation_clause of the form: 

```ada
for r use
    record at mod a
        ...
    end record;

```

is equivalent to: 

```ada
for r'Alignment use a;
for r use
    record
        ...
    end record;

```

Reason: The preferred syntax for specifying the alignment of an entity is an attribute_definition_clause specifying the Alignment attribute. Therefore, the special-purpose mod_clause syntax is now obsolete.

The above equivalence implies, for example, that it is illegal to give both a mod_clause and an attribute_definition_clause specifying the Alignment attribute for the same type. 


#### Wording Changes from Ada 83

Ada 83's alignment_clause is now called a mod_clause to avoid confusion with the new term "Alignment clause" (that is, an attribute_definition_clause for the Alignment attribute). 


## J.9  The Storage_Size Attribute


#### Static Semantics

For any task subtype T, the following attribute is defined: 

T'Storage_Size Denotes an implementation-defined value of type universal_integer representing the number of storage elements reserved for a task of the subtype T. 

To be honest: T'Storage_Size cannot be particularly meaningful in the presence of a pragma Storage_Size, especially when the expression is dynamic, or depends on a discriminant of the task, because the Storage_Size will be different for different objects of the type. Even without such a pragma, the Storage_Size can be different for different objects of the type, and in any case, the value is implementation defined. Hence, it is always implementation defined. 

Storage_Size may be specified for a task first subtype via an attribute_definition_clause. 


#### Syntax




#### Syntax




#### Syntax




#### Syntax




#### Syntax








#### Syntax




#### Syntax






#### Syntax














#### Syntax




#### Syntax




#### Syntax






#### Syntax




#### Syntax




#### Syntax










#### Syntax










#### Static Semantics

A pragma Remote_Call_Interface specifies that a library unit is a remote call interface, namely that the Remote_Call_Interface aspect (see E.2.3) of the library unit is True.

A pragma All_Calls_Remote specifies that the All_Calls_Remote aspect (see E.2.3) of the library unit is True.

