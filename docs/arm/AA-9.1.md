---
sidebar_position:  72
---

# 9.1  Task Units and Task Objects

A task unit is declared by a task declaration, which has a corresponding [task_body](./AA-9.1#S0248). A task declaration may be a [task_type_declaration](./AA-9.1#S0244), in which case it declares a named task type; alternatively, it may be a [single_task_declaration](./AA-9.1#S0245), in which case it defines an anonymous task type, as well as declaring a named task object of that type. 


#### Syntax

{AI95-00345-01} {AI05-0183-1} task_type_declaration<a id="S0244"></a> ::= 
   task type [defining_identifier](./AA-3.1#S0022) [[known_discriminant_part](./AA-3.7#S0061)]
        [[aspect_specification](./AA-13.1#S0346)] [is
     [new [interface_list](./AA-3.9#S0078) with]
     [task_definition](./AA-9.1#S0246)];

{AI95-00399-01} {AI05-0183-1} single_task_declaration<a id="S0245"></a> ::= 
   task [defining_identifier](./AA-3.1#S0022) 
        [[aspect_specification](./AA-13.1#S0346)][is
     [new [interface_list](./AA-3.9#S0078) with]
     [task_definition](./AA-9.1#S0246)];

task_definition<a id="S0246"></a> ::= 
     {[task_item](./AA-9.1#S0247)}
  [ private
     {[task_item](./AA-9.1#S0247)}]
  end [task_[identifier](./AA-2.3#S0002)]

{8652/0009} {AI95-00137-01} task_item<a id="S0247"></a> ::= [entry_declaration](./AA-9.5#S0257) | [aspect_clause](./AA-13.1#S0343)

{AI05-0267-1} task_body<a id="S0248"></a> ::= 
   task body [defining_identifier](./AA-3.1#S0022)
        [[aspect_specification](./AA-13.1#S0346)] is
     [declarative_part](./AA-3.11#S0086)
   begin
     [handled_sequence_of_statements](./AA-11.2#S0304)
   end [task_[identifier](./AA-2.3#S0002)];

If a task_[identifier](./AA-2.3#S0002) appears at the end of a [task_definition](./AA-9.1#S0246) or [task_body](./AA-9.1#S0248), it shall repeat the [defining_identifier](./AA-3.1#S0022). 

This paragraph was deleted.

Paragraph 8 was deleted. 


#### Static Semantics

A [task_definition](./AA-9.1#S0246) defines a task type and its first subtype. The first list of [task_item](./AA-9.1#S0247)s of a [task_definition](./AA-9.1#S0246), together with the [known_discriminant_part](./AA-3.7#S0061), if any, is called the visible part of the task unit. [ The optional list of [task_item](./AA-9.1#S0247)s after the reserved word private is called the private part of the task unit.] 

Proof: {AI05-0299-1} Private part is defined in Clause 8. 

{8652/0029} {AI95-00116-01} For a task declaration without a [task_definition](./AA-9.1#S0246), a [task_definition](./AA-9.1#S0246) without [task_item](./AA-9.1#S0247)s is assumed.

{AI95-00345-01} {AI95-00397-01} {AI95-00399-01} {AI95-00419-01} {AI05-0042-1} For a task declaration with an [interface_list](./AA-3.9#S0078), the task type inherits user-defined primitive subprograms from each progenitor type (see 3.9.4), in the same way that a derived type inherits user-defined primitive subprograms from its progenitor types (see 3.4). If the first parameter of a primitive inherited subprogram is of the task type or an access parameter designating the task type, and there is an [entry_declaration](./AA-9.5#S0257) for a single entry with the same identifier within the task declaration, whose profile is type conformant with the prefixed view profile of the inherited subprogram, the inherited subprogram is said to be implemented by the conforming task entry using an implicitly declared nonabstract subprogram which has the same profile as the inherited subprogram and which overrides it. 

Ramification: The inherited subprograms can only come from an interface given as part of the task declaration. 

Reason: {AI05-0042-1} The part about the implicitly declared subprogram is needed so that a subprogram implemented by an entry is considered to be overridden for the purpose of the other rules of the language. Without it, it would for instance be illegal for an abstract subprogram to be implemented by an entry, because the abstract subprogram would not be overridden. The Legality Rules below ensure that there is no conflict between the implicit overriding subprogram and a user-defined overriding subprogram. 


#### Legality Rules

{AI95-00345-01} A task declaration requires a completion[, which shall be a [task_body](./AA-9.1#S0248),] and every [task_body](./AA-9.1#S0248) shall be the completion of some task declaration. 

To be honest: {AI05-0229-1} If the implementation supports it, the task body can be imported (using aspect Import, see B.1), in which case no explicit [task_body](./AA-9.1#S0248) is allowed. 

{AI95-00345-01} {AI95-00399-01} [Each interface_[subtype_mark](./AA-3.2#S0028) of an [interface_list](./AA-3.9#S0078) appearing within a task declaration shall denote a limited interface type that is not a protected interface.] 

Proof: 3.9.4 requires that an [interface_list](./AA-3.9#S0078) only name interface types, and limits the descendants of the various kinds of interface types. Only a limited, task, or synchronized interface can have a task type descendant. Nonlimited or protected interfaces are not allowed, as they offer operations that a task does not have. 

{AI95-00397-01} {AI05-0090-1} The prefixed view profile of an explicitly declared primitive subprogram of a tagged task type shall not be type conformant with any entry of the task type, if the subprogram has the same defining name as the entry and the first parameter of the subprogram is of the task type or is an access parameter designating the task type. 

Reason: This prevents the existence of two operations with the same name and profile which could be called with a prefixed view. If the operation was inherited, this would be illegal by the following rules; this rule puts inherited and noninherited routines on the same footing. Note that this only applies to tagged task types (that is, those with an interface in their declaration); we do that as there is no problem with prefixed view calls of primitive operations for "normal" task types, and having this rule apply to all tasks would be incompatible with Ada 95. 

{AI95-00345-01} {AI95-00399-01} For each primitive subprogram inherited by the type declared by a task declaration, at most one of the following shall apply:

{AI95-00345-01} the inherited subprogram is overridden with a primitive subprogram of the task type, in which case the overriding subprogram shall be subtype conformant with the inherited subprogram and not abstract; or

{AI95-00345-01} {AI95-00397-01} the inherited subprogram is implemented by a single entry of the task type; in which case its prefixed view profile shall be subtype conformant with that of the task entry. 

Ramification: An entry may implement two subprograms from the ancestors, one whose first parameter is of type T and one whose first parameter is of type access T. That doesn't cause implementation problems because "implemented by" (unlike "overridden') probably entails the creation of wrappers. 

If neither applies, the inherited subprogram shall be a null procedure. In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit. 

Reason: Each inherited subprogram can only have a single implementation (either from overriding a subprogram or implementing an entry), and must have an implementation unless the subprogram is a null procedure. 


#### Dynamic Semantics

[ The elaboration of a task declaration elaborates the [task_definition](./AA-9.1#S0246). The elaboration of a [single_task_declaration](./AA-9.1#S0245) also creates an object of an (anonymous) task type.] 

Proof: This is redundant with the general rules for the elaboration of a [full_type_declaration](./AA-3.2#S0024) and an [object_declaration](./AA-3.3#S0032). 

[The elaboration of a [task_definition](./AA-9.1#S0246) creates the task type and its first subtype;] it also includes the elaboration of the [entry_declaration](./AA-9.5#S0257)s in the given order.

{8652/0009} {AI95-00137-01} As part of the initialization of a task object, any [aspect_clause](./AA-13.1#S0343)s and any per-object constraints associated with [entry_declaration](./AA-9.5#S0257)s of the corresponding [task_definition](./AA-9.1#S0246) are elaborated in the given order. 

Reason: The only [aspect_clause](./AA-13.1#S0343)s defined for task entries are ones that specify the Address of an entry, as part of defining an interrupt entry. These clearly need to be elaborated per-object, not per-type. Normally the address will be a function of a discriminant, if such an Address clause is in a task type rather than a single task declaration, though it could rely on a parameterless function that allocates sequential interrupt vectors.

We do not mention representation pragmas, since each pragma may have its own elaboration rules. 

The elaboration of a [task_body](./AA-9.1#S0248) has no effect other than to establish that tasks of the type can from then on be activated without failing the Elaboration_Check.

[The execution of a [task_body](./AA-9.1#S0248) is invoked by the activation of a task of the corresponding type (see 9.2).]

The content of a task object of a given task type includes: 

The values of the discriminants of the task object, if any;

An entry queue for each entry of the task object; 

Ramification: "For each entry" implies one queue for each single entry, plus one for each entry of each entry family. 

A representation of the state of the associated task. 

NOTE 1   {AI95-00382-01} Other than in an [access_definition](./AA-3.10#S0084), the name of a task unit within the declaration or body of the task unit denotes the current instance of the unit (see 8.6), rather than the first subtype of the corresponding task type (and thus the name cannot be used as a [subtype_mark](./AA-3.2#S0028)). 

Discussion: {AI95-00382-01} It can be used as a [subtype_mark](./AA-3.2#S0028) in an anonymous access type. In addition, it is possible to refer to some other subtype of the task type within its body, presuming such a subtype has been declared between the [task_type_declaration](./AA-9.1#S0244) and the [task_body](./AA-9.1#S0248). 

NOTE 2   The notation of a [selected_component](./AA-4.1#S0098) can be used to denote a discriminant of a task (see 4.1.3). Within a task unit, the name of a discriminant of the task type denotes the corresponding discriminant of the current instance of the unit.

NOTE 3   {AI95-00287-01} {AI12-0442-1} A task type is a limited type (see 7.5), and hence precludes use of [assignment_statement](./AA-5.2#S0173)s and predefined equality operators. If a programmer wants to write an application that stores and exchanges task identities, they can do so by defining an access type designating the corresponding task objects and by using access values for identification purposes. Assignment is available for such an access type as for any access type. Alternatively, if the implementation supports the Systems Programming Annex, the Identity attribute can be used for task identification (see C.7.1). 


#### Examples

Examples of declarations of task types: 

```ada
task type Server is
   entry Next_Work_Item(WI : in Work_Item);
   entry Shut_Down;
end Server;

```

```ada
{AI95-00433-01} task type Keyboard_Driver(ID : Keyboard_ID := New_ID) is
      new Serial_Device with  -- see 3.9.4
   entry Read (C : out Character);
   entry Write(C : in  Character);
end Keyboard_Driver;

```

Examples of declarations of single tasks: 

```ada
task Controller is
   entry Request(Level)(D : Item);  --  a family of entries
end Controller;

```

```ada
task Parser is
   entry Next_Lexeme(L : in  Lexical_Element);
   entry Next_Action(A : out Parser_Action);
end;

```

```ada
task User;  --  has no entries

```

Examples of task objects: 

```ada
Agent    : Server;
Teletype : Keyboard_Driver(TTY_ID);
Pool     : array(1 .. 10) of Keyboard_Driver;

```

Example of access type designating task objects: 

```ada
type Keyboard is access Keyboard_Driver;
Terminal : Keyboard := new Keyboard_Driver(Term_ID);

```


#### Extensions to Ada 83

The syntax rules for task declarations are modified to allow a [known_discriminant_part](./AA-3.7#S0061), and to allow a private part. They are also modified to allow [entry_declaration](./AA-9.5#S0257)s and [aspect_clause](./AA-13.1#S0343)s to be mixed. 


#### Wording Changes from Ada 83

The syntax rules for tasks have been split up according to task types and single tasks. In particular: The syntax rules for task_declaration and task_specification are removed. The syntax rules for [task_type_declaration](./AA-9.1#S0244), [single_task_declaration](./AA-9.1#S0245), [task_definition](./AA-9.1#S0246) and [task_item](./AA-9.1#S0247) are new.

The syntax rule for [task_body](./AA-9.1#S0248) now uses the nonterminal [handled_sequence_of_statements](./AA-11.2#S0304).

The [declarative_part](./AA-3.11#S0086) of a [task_body](./AA-9.1#S0248) is now required; that doesn't make any real difference, because a [declarative_part](./AA-3.11#S0086) can be empty. 


#### Extensions to Ada 95

{AI95-00345-01} {AI95-00397-01} {AI95-00399-01} {AI95-00419-01} Task types and single tasks can be derived from one or more interfaces. Entries of the task type can implement the primitive operations of an interface. [Overriding_indicator](./AA-8.3#S0234)s can be used to specify whether or not an entry implements a primitive operation. 


#### Wording Changes from Ada 95

{8652/0029} {AI95-00116-01} Corrigendum: Clarified that a task type has an implicit empty [task_definition](./AA-9.1#S0246) if none is given.

{8652/0009} {AI95-00137-01} Corrigendum: Changed representation clauses to aspect clauses to reflect that they are used for more than just representation.

{AI95-00287-01} Revised the note on operations of task types to reflect that limited types do have an assignment operation, but not copying ([assignment_statement](./AA-5.2#S0173)s).

{AI95-00382-01} Revised the note on use of the name of a task type within itself to reflect the exception for anonymous access types. 


#### Extensions to Ada 2005

{AI05-0183-1} {AI05-0267-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [task_type_declaration](./AA-9.1#S0244), a [single_task_declaration](./AA-9.1#S0245), and a [task_body](./AA-9.1#S0248). This is described in 13.1.1. 


#### Wording Changes from Ada 2005

{AI05-0042-1} Correction: Clarified that an inherited procedure of a progenitor is overridden when it is implemented by an entry.

{AI05-0090-1} Correction: Added the missing defining name in the no conflicting primitive operation rule. 

