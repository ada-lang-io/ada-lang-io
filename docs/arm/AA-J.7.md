---
sidebar_position:  197
---

# J.7  At Clauses


#### Syntax

at_clause<a id="S0368"></a> ::= for [direct_name](./AA-4.1#S0092) use at [expression](./AA-4.4#S0132);


#### Static Semantics

An [at_clause](./AA-J.7#S0368) of the form "for x use at y;" is equivalent to an [attribute_definition_clause](./AA-13.3#S0349) of the form "for x'Address use y;". 

Reason: The preferred syntax for specifying the address of an entity is an [attribute_definition_clause](./AA-13.3#S0349) specifying the Address attribute. Therefore, the special-purpose [at_clause](./AA-J.7#S0368) syntax is now obsolete.

The above equivalence implies, for example, that only one [at_clause](./AA-J.7#S0368) is allowed for a given entity. Similarly, it is illegal to give both an [at_clause](./AA-J.7#S0368) and an [attribute_definition_clause](./AA-13.3#S0349) specifying the Address attribute. 


#### Extensions to Ada 83

We now allow to define the address of an entity using an [attribute_definition_clause](./AA-13.3#S0349). This is because Ada 83's [at_clause](./AA-J.7#S0368) is so hard to remember: programmers often tend to write "for X'Address use...;". 


#### Wording Changes from Ada 83

Ada 83's address_clause is now called an [at_clause](./AA-J.7#S0368) to avoid confusion with the new term "Address clause" (that is, an [attribute_definition_clause](./AA-13.3#S0349) for the Address attribute). 


## J.7.1  Interrupt Entries

[Implementations are permitted to allow the attachment of task entries to interrupts via the address clause. Such an entry is referred to as an interrupt entry.

The address of the task entry corresponds to a hardware interrupt in an implementation-defined manner. (See Ada.Interrupts.Reference in C.3.2.)] 


#### Static Semantics

The following attribute is defined:

For any task entry X: 

X'Address For a task entry whose address is specified (an interrupt entry), the value refers to the corresponding hardware interrupt. For such an entry, as for any other task entry, the meaning of this value is implementation defined. The value of this attribute is of the type of the subtype System.Address.

Address may be specified for single entries via an [attribute_definition_clause](./AA-13.3#S0349). 

Reason: Because of the equivalence of [at_clause](./AA-J.7#S0368)s and [attribute_definition_clause](./AA-13.3#S0349)s, an interrupt entry may be specified via either notation. 


#### Dynamic Semantics

As part of the initialization of a task object, the address clause for an interrupt entry is elaborated[, which evaluates the [expression](./AA-4.4#S0132) of the address clause]. A check is made that the address specified is associated with some interrupt to which a task entry may be attached. If this check fails, Program_Error is raised. Otherwise, the interrupt entry is attached to the interrupt associated with the specified address.

Upon finalization of the task object, the interrupt entry, if any, is detached from the corresponding interrupt and the default treatment is restored.

While an interrupt entry is attached to an interrupt, the interrupt is reserved (see C.3).

An interrupt delivered to a task entry acts as a call to the entry issued by a hardware task whose priority is in the System.Interrupt_Priority range. It is implementation defined whether the call is performed as an ordinary entry call, a timed entry call, or a conditional entry call; which kind of call is performed can depend on the specific interrupt.


#### Bounded (Run-Time) Errors

It is a bounded error to evaluate E'Caller (see C.7.1) in an [accept_statement](./AA-9.5#S0258) for an interrupt entry. The possible effects are the same as for calling Current_Task from an entry body. 


#### Documentation Requirements

The implementation shall document to which interrupts a task entry may be attached. 

Documentation Requirement: The interrupts to which a task entry may be attached.

The implementation shall document whether the invocation of an interrupt entry has the effect of an ordinary entry call, conditional call, or a timed call, and whether the effect varies in the presence of pending interrupts. 

Documentation Requirement: The type of entry call invoked for an interrupt entry.


#### Implementation Permissions

The support for this subclause is optional.

Interrupts to which the implementation allows a task entry to be attached may be designated as reserved for the entire duration of program execution[; that is, not just when they have an interrupt entry attached to them].

{8652/0077} {AI95-00111-01} Interrupt entry calls may be implemented by having the hardware execute directly the appropriate [accept_statement](./AA-9.5#S0258). Alternatively, the implementation is allowed to provide an internal interrupt handler to simulate the effect of a normal task calling the entry.

The implementation is allowed to impose restrictions on the specifications and bodies of tasks that have interrupt entries.

It is implementation defined whether direct calls (from the program) to interrupt entries are allowed.

If a [select_statement](./AA-9.7#S0269) contains both a [terminate_alternative](./AA-9.7#S0275) and an [accept_alternative](./AA-9.7#S0273) for an interrupt entry, then an implementation is allowed to impose further requirements for the selection of the [terminate_alternative](./AA-9.7#S0275) in addition to those given in 9.3. 

NOTE 1   {8652/0077} {AI95-00111-01} Queued interrupts correspond to ordinary entry calls. Interrupts that are lost if not immediately processed correspond to conditional entry calls. It is a consequence of the priority rules that an [accept_statement](./AA-9.5#S0258) executed in response to an interrupt can be executed with the active priority at which the hardware generates the interrupt, taking precedence over lower priority tasks, without a scheduling action.

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

{AI95-00114-01} RM83-13.5.1 did not adequately address the problems associated with interrupts. This feature is now obsolescent and is replaced by the Ada 95 interrupt model as specified in the Systems Programming Annex. 


#### Wording Changes from Ada 95

{8652/0077} {AI95-00111-01} Corrigendum: The undefined term accept body was replaced by [accept_statement](./AA-9.5#S0258). 

