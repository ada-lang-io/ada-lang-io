---
sidebar_position:  10
---

# 9 Tasks and Synchronization

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
The execution of an Ada program consists of the execution of one or more tasks. Each task represents a separate thread of control that proceeds independently and concurrently between the points where it interacts with other tasks. The various forms of task interaction are described in this section, and include: 

To be honest: The execution of an Ada program consists of the execution of one or more partitions (see 10.2), each of which in turn consists of the execution of an environment task and zero or more subtasks. 

Version=[5],Kind=(AddedNormal),Group=[R],Term=[logical thread of control], Def=[an activity within the execution of a program that can proceed in parallel with other activities of the same task, or of separate tasks] 

the activation and termination of a task;

a call on a protected subprogram of a protected object, providing exclusive read-write access, or concurrent read-only access to shared data;

a call on an entry, either of another task, allowing for synchronous communication with that task, or of a protected object, allowing for asynchronous communication with one or more other tasks using that same protected object;

a timed operation, including a simple delay statement, a timed entry call or accept, or a timed asynchronous select statement (see next item);

an asynchronous transfer of control as part of an asynchronous select statement, where a task stops what it is doing and begins execution at a different point in response to the completion of an entry call or the expiration of a delay;

an abort statement, allowing one task to cause the termination of another task. 

In addition, tasks can communicate indirectly by reading and updating (unprotected) shared variables, presuming the access is properly synchronized through some other kind of task interaction.


#### Static Semantics

The properties of a task are defined by a corresponding task declaration and task_body, which together define a program unit called a task unit. 


#### Dynamic Semantics

Over time, tasks proceed through various states. A task is initially inactive; upon activation, and prior to its termination it is either blocked (as part of some task interaction) or ready to run. While ready, a task competes for the available execution resources that it requires to run. 

Discussion: The means for selecting which of the ready tasks to run, given the currently available execution resources, is determined by the task dispatching policy in effect, which is generally implementation defined, but may be controlled by pragmas and operations defined in the Real-Time Annex (see D.2 and D.5). 

NOTE 1   Concurrent task execution may be implemented on multicomputers, multiprocessors, or with interleaved execution on a single physical processor. On the other hand, whenever an implementation can determine that the required semantic effects can be achieved when parts of the execution of a given task are performed by different physical processors acting in parallel, it may choose to perform them in this way.


#### Wording Changes from Ada 83

The introduction has been rewritten.

We use the term "concurrent" rather than "parallel" when talking about logically independent execution of threads of control. The term "parallel" is reserved for referring to the situation where multiple physical processors run simultaneously. 


## 9.1  Task Units and Task Objects

A task unit is declared by a task declaration, which has a corresponding task_body. A task declaration may be a task_type_declaration, in which case it declares a named task type; alternatively, it may be a single_task_declaration, in which case it defines an anonymous task type, as well as declaring a named task object of that type. 


#### Syntax

task_type_declaration ::= 
   task type defining_identifier [known_discriminant_part] [is task_definition];

single_task_declaration ::= 
   task defining_identifier [is task_definition];

task_definition ::= 
     {task_item}
  [ private
     {task_item}]
  end [task_identifier]

task_item ::= entry_declaration | representation_clause

task_body ::= 
   task body defining_identifier is
     declarative_part
   begin
     handled_sequence_of_statements
   end [task_identifier];

If a task_identifier appears at the end of a task_definition or task_body, it shall repeat the defining_identifier. 


#### Legality Rules

A task declaration requires a completion[, which shall be a task_body,] and every task_body shall be the completion of some task declaration. 

To be honest: The completion can be a pragma Import, if the implementation supports it. 


#### Static Semantics

A task_definition defines a task type and its first subtype. The first list of task_items of a task_definition, together with the known_discriminant_part, if any, is called the visible part of the task unit. [ The optional list of task_items after the reserved word private is called the private part of the task unit.] 

Proof: Private part is defined in Section 8. 


#### Dynamic Semantics

[ The elaboration of a task declaration elaborates the task_definition. The elaboration of a single_task_declaration also creates an object of an (anonymous) task type.] 

Proof: This is redundant with the general rules for the elaboration of a full_type_declaration and an object_declaration. 

[The elaboration of a task_definition creates the task type and its first subtype;] it also includes the elaboration of the entry_declarations in the given order.

As part of the initialization of a task object, any representation_clauses and any per-object constraints associated with entry_declarations of the corresponding task_definition are elaborated in the given order. 

Reason: The only representation_clauses defined for task entries are ones that specify the Address of an entry, as part of defining an interrupt entry. These clearly need to be elaborated per-object, not per-type. Normally the address will be a function of a discriminant, if such an Address clause is in a task type rather than a single task declaration, though it could rely on a parameterless function that allocates sequential interrupt vectors.

We do not mention representation pragmas, since each pragma may have its own elaboration rules. 

The elaboration of a task_body has no effect other than to establish that tasks of the type can from then on be activated without failing the Elaboration_Check.

[The execution of a task_body is invoked by the activation of a task of the corresponding type (see 9.2).]

The content of a task object of a given task type includes: 

The values of the discriminants of the task object, if any;

An entry queue for each entry of the task object; 

Ramification: "For each entry" implies one queue for each single entry, plus one for each entry of each entry family. 

A representation of the state of the associated task. 

NOTE 1   Within the declaration or body of a task unit, the name of the task unit denotes the current instance of the unit (see 8.6), rather than the first subtype of the corresponding task type (and thus the name cannot be used as a subtype_mark). 

Discussion: However, it is possible to refer to some other subtype of the task type within its body, presuming such a subtype has been declared between the task_type_declaration and the task_body. 

NOTE 2   The notation of a selected_component can be used to denote a discriminant of a task (see 4.1.3). Within a task unit, the name of a discriminant of the task type denotes the corresponding discriminant of the current instance of the unit.

NOTE 3   A task type is a limited type (see 7.5), and hence has neither an assignment operation nor predefined equality operators. If an application needs to store and exchange task identities, it can do so by defining an access type designating the corresponding task objects and by using access values for identification purposes. Assignment is available for such an access type as for any access type. Alternatively, if the implementation supports the Systems Programming Annex, the Identity attribute can be used for task identification (see C.7). 


#### Examples

Examples of declarations of task types: 

```ada
task type Server is
   entry Next_Work_Item(WI : in Work_Item);
   entry Shut_Down;
end Server;

```

```ada
task type Keyboard_Driver(ID : Keyboard_ID := New_ID) is
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

The syntax rules for task declarations are modified to allow a known_discriminant_part, and to allow a private part. They are also modified to allow entry_declarations and representation_clauses to be mixed. 


#### Wording Changes from Ada 83

The syntax rules for tasks have been split up according to task types and single tasks. In particular: The syntax rules for task_declaration and task_specification are removed. The syntax rules for task_type_declaration, single_task_declaration, task_definition and task_item are new.

The syntax rule for task_body now uses the nonterminal handled_sequence_of_statements.

The declarative_part of a task_body is now required; that doesn't make any real difference, because a declarative_part can be empty. 


## 9.2  Task Execution - Task Activation


#### Dynamic Semantics

The execution of a task of a given task type consists of the execution of the corresponding task_body. The initial part of this execution is called the activation of the task; it consists of the elaboration of the declarative_part of the task_body. Should an exception be propagated by the elaboration of its declarative_part, the activation of the task is defined to have failed, and it becomes a completed task.

A task object (which represents one task) can be created either as part of the elaboration of an object_declaration occurring immediately within some declarative region, or as part of the evaluation of an allocator. All tasks created by the elaboration of object_declarations of a single declarative region (including subcomponents of the declared objects) are activated together. Similarly, all tasks created by the evaluation of a single allocator are activated together. The activation of a task is associated with the innermost allocator or object_declaration that is responsible for its creation. 

Discussion: The initialization of an object_declaration or allocator can indirectly include the creation of other objects that contain tasks. For example, the default expression for a subcomponent of an object created by an allocator might call a function that evaluates a completely different allocator. Tasks created by the two allocators are not activated together. 

For tasks created by the elaboration of object_declarations of a given declarative region, the activations are initiated within the context of the handled_sequence_of_statements (and its associated exception_handlers if any - see 11.2), just prior to executing the statements of the _sequence. [For a package without an explicit body or an explicit handled_sequence_of_statements, an implicit body or an implicit null_statement is assumed, as defined in 7.2.] 

Ramification: If Tasking_Error is raised, it can be handled by handlers of the handled_sequence_of_statements. 

For tasks created by the evaluation of an allocator, the activations are initiated as the last step of evaluating the allocator, after completing any initialization for the object created by the allocator, and prior to returning the new access value.

The task that created the new tasks and initiated their activations (the activator) is blocked until all of these activations complete (successfully or not). Once all of these activations are complete, if the activation of any of the tasks has failed [(due to the propagation of an exception)], Tasking_Error is raised in the activator, at the place at which it initiated the activations. Otherwise, the activator proceeds with its execution normally. Any tasks that are aborted prior to completing their activation are ignored when determining whether to raise Tasking_Error. 

Ramification: Note that a task created by an allocator does not necessarily depend on its activator; in such a case the activator's termination can precede the termination of the newly created task. 

Discussion: Tasking_Error is raised only once, even if two or more of the tasks being activated fail their activation. 

Should the task that created the new tasks never reach the point where it would initiate the activations (due to an abort or the raising of an exception), the newly created tasks become terminated are never activated. 

NOTE 1   An entry of a task can be called before the task has been activated.

NOTE 2   If several tasks are activated together, the execution of any of these tasks need not await the end of the activation of the other tasks.

NOTE 3   A task can become completed during its activation either because of an exception or because it is aborted (see 9.8).


#### Examples

Example of task activation: 

```ada
procedure P is
   A, B : Server;    --  elaborate the task objects A, B
   C    : Server;    --  elaborate the task object C
begin
   --  the tasks A, B, C are activated together before the first statement
   ...
end;

```


#### Wording Changes from Ada 83

We have replaced the term suspended with blocked, since we didn't want to consider a task blocked when it was simply competing for execution resources. "Suspended" is sometimes used more generally to refer to tasks that are not actually running on some processor, due to the lack of resources.

This clause has been rewritten in an attempt to improve presentation. 


## 9.3  Task Dependence - Termination of Tasks


#### Dynamic Semantics

Each task (other than an environment task - see 10.2) depends on one or more masters (see 7.6.1), as follows: 

If the task is created by the evaluation of an allocator for a given access type, it depends on each master that includes the elaboration of the declaration of the ultimate ancestor of the given access type.

If the task is created by the elaboration of an object_declaration, it depends on each master that includes this elaboration.

Furthermore, if a task depends on a given master, it is defined to depend on the task that executes the master, and (recursively) on any master of that task. 

Discussion: Don't confuse these kinds of dependences with the dependences among compilation units defined in 10.1.1, "Compilation Units - Library Units". 

A task is said to be completed when the execution of its corresponding task_body is completed. A task is said to be terminated when any finalization of the task_body has been performed (see 7.6.1). [The first step of finalizing a master (including a task_body) is to wait for the termination of any tasks dependent on the master.] The task executing the master is blocked until all the dependents have terminated. [Any remaining finalization is then performed and the master is left.]

Completion of a task (and the corresponding task_body) can occur when the task is blocked at a select_statement with an an open terminate_alternative (see 9.7.1); the open terminate_alternative is selected if and only if the following conditions are satisfied: 

The task depends on some completed master;

Each task that depends on the master considered is either already terminated or similarly blocked at a select_statement with an open terminate_alternative. 

When both conditions are satisfied, the task considered becomes completed, together with all tasks that depend on the master considered that are not yet completed. 

Ramification: Any required finalization is performed after the selection of terminate_alternatives. The tasks are not callable during the finalization. In some ways it is as though they were aborted. 

NOTE 1   The full view of a limited private type can be a task type, or can have subcomponents of a task type. Creation of an object of such a type creates dependences according to the full type.

NOTE 2   An object_renaming_declaration defines a new view of an existing entity and hence creates no further dependence.

NOTE 3   The rules given for the collective completion of a group of tasks all blocked on select_statements with open terminate_alternatives ensure that the collective completion can occur only when there are no remaining active tasks that could call one of the tasks being collectively completed.

NOTE 4   If two or more tasks are blocked on select_statements with open terminate_alternatives, and become completed collectively, their finalization actions proceed concurrently.

NOTE 5   The completion of a task can occur due to any of the following: 

the raising of an exception during the elaboration of the declarative_part of the corresponding task_body;

the completion of the handled_sequence_of_statements of the corresponding task_body;

the selection of an open terminate_alternative of a select_statement in the corresponding task_body;

the abort of the task. 


#### Examples

Example of task dependence: 

```ada
declare
   type Global is access Server;        --  see 9.1
   A, B : Server;
   G    : Global;
begin
   --  activation of A and B
   declare
      type Local is access Server;
      X : Global := new Server;  --  activation of X.all
      L : Local  := new Server;  --  activation of L.all
      C : Server;
   begin
      --  activation of C
      G := X;  --  both G and X designate the same task object
      ...
   end;  --  await termination of C and L.all (but not X.all)
   ...
end;  --  await termination of A, B, and G.all

```


#### Wording Changes from Ada 83

We have revised the wording to be consistent with the definition of master now given in 7.6.1, "Completion and Finalization".

Tasks that used to depend on library packages in Ada 83, now depend on the (implicit) task_body of the environment task (see 10.2). Therefore, the environment task has to wait for them before performing library level finalization and terminating the partition. In Ada 83 the requirement to wait for tasks that depended on library packages was not as clear.

What was "collective termination" is now "collective completion" resulting from selecting terminate_alternatives. This is because finalization still occurs for such tasks, and this happens after selecting the terminate_alternative, but before termination. 


## 9.4  Protected Units and Protected Objects

A protected object provides coordinated access to shared data, through calls on its visible protected operations, which can be protected subprograms or protected entries. A protected unit is declared by a protected declaration, which has a corresponding protected_body. A protected declaration may be a protected_type_declaration, in which case it declares a named protected type; alternatively, it may be a single_protected_declaration, in which case it defines an anonymous protected type, as well as declaring a named protected object of that type. 


#### Syntax

protected_type_declaration ::= 
  protected type defining_identifier [known_discriminant_part] is protected_definition;

single_protected_declaration ::= 
  protected defining_identifier is protected_definition;

protected_definition ::= 
    { protected_operation_declaration }
[ private
    { protected_element_declaration } ]
  end [protected_identifier]

protected_operation_declaration ::= subprogram_declaration
     | entry_declaration
     | representation_clause

protected_element_declaration ::= protected_operation_declaration
     | component_declaration

Reason: We allow the operations and components to be mixed because that's how other things work (for example, package declarations). We have relaxed the ordering rules for the items inside declarative_parts and task_definitions as well. 

protected_body ::= 
  protected body defining_identifier is
   { protected_operation_item }
  end [protected_identifier];

protected_operation_item ::= subprogram_declaration
     | subprogram_body
     | entry_body
     | representation_clause

If a protected_identifier appears at the end of a protected_definition or protected_body, it shall repeat the defining_identifier. 


#### Legality Rules

A protected declaration requires a completion[, which shall be a protected_body,] and every protected_body shall be the completion of some protected declaration. 

To be honest: The completion can be a pragma Import, if the implementation supports it. 


#### Static Semantics

A protected_definition defines a protected type and its first subtype. The list of protected_operation_declarations of a protected_definition, together with the known_discriminant_part, if any, is called the visible part of the protected unit. [ The optional list of protected_element_declarations after the reserved word private is called the private part of the protected unit.] 

Proof: Private part is defined in Section 8. 


#### Dynamic Semantics

[The elaboration of a protected declaration elaborates the protected_definition. The elaboration of a single_protected_declaration also creates an object of an (anonymous) protected type.] 

Proof: This is redundant with the general rules for the elaboration of a full_type_declaration and an object_declaration. 

[The elaboration of a protected_definition creates the protected type and its first subtype;] it also includes the elaboration of the component_declarations and protected_operation_declarations in the given order.

[As part of the initialization of a protected object, any per-object constraints (see 3.8) are elaborated.] 

Discussion: We do not mention pragmas since each pragma has its own elaboration rules. 

The elaboration of a protected_body has no other effect than to establish that protected operations of the type can from then on be called without failing the Elaboration_Check.

The content of an object of a given protected type includes: 

The values of the components of the protected object, including (implicitly) an entry queue for each entry declared for the protected object; 

Ramification: "For each entry" implies one queue for each single entry, plus one for each entry of each entry family. 

A representation of the state of the execution resource associated with the protected object (one such resource is associated with each protected object). 

[The execution resource associated with a protected object has to be acquired to read or update any components of the protected object; it can be acquired (as part of a protected action - see 9.5.1) either for concurrent read-only access, or for exclusive read-write access.]

As the first step of the finalization of a protected object, each call remaining on any entry queue of the object is removed from its queue and Program_Error is raised at the place of the corresponding entry_call_statement. 

Reason: This is analogous to the raising of Tasking_Error in callers of a task that completes before accepting the calls. This situation can only occur due to a requeue (ignoring premature unchecked_deallocation), since any task that has accessibility to a protected object is awaited before finalizing the protected object. For example: 

```ada
procedure Main is
    task T is
        entry E;
    end T;

```

```ada
    task body T is
        protected PO is
            entry Ee;
        end PO;

```

```ada
        protected body PO is
            entry Ee when False is
            begin
                null;
            end Ee;
        end PO;
    begin
        accept E do
            requeue PO.Ee;
        end E;
    end T;
begin
    T.E;
end Main;

```

The environment task is queued on PO.EE when PO is finalized.

In a real example, a server task might park callers on a local protected object for some useful purpose, so we didn't want to disallow this case. 

NOTE 1   Within the declaration or body of a protected unit, the name of the protected unit denotes the current instance of the unit (see 8.6), rather than the first subtype of the corresponding protected type (and thus the name cannot be used as a subtype_mark). 

Discussion: However, it is possible to refer to some other subtype of the protected type within its body, presuming such a subtype has been declared between the protected_type_declaration and the protected_body. 

NOTE 2   A selected_component can be used to denote a discriminant of a protected object (see 4.1.3). Within a protected unit, the name of a discriminant of the protected type denotes the corresponding discriminant of the current instance of the unit.

NOTE 3   A protected type is a limited type (see 7.5), and hence has neither an assignment operation nor predefined equality operators.

NOTE 4   The bodies of the protected operations given in the protected_body define the actions that take place upon calls to the protected operations.

NOTE 5   The declarations in the private part are only visible within the private part and the body of the protected unit. 

Reason: Component_declarations are disallowed in a protected_body because, for efficiency, we wish to allow the compiler to determine the size of protected objects (when not dynamic); the compiler cannot necessarily see the body. Furthermore, the semantics of initialization of such objects would be problematic - we do not wish to give protected objects complex initialization semantics similar to task activation.

The same applies to entry_declarations, since an entry involves an implicit component - the entry queue. 


#### Examples

Example of declaration of protected type and corresponding body: 

```ada
protected type Resource is
   entry Seize;
   procedure Release;
private
   Busy : Boolean := False;
end Resource;

```

```ada
protected body Resource is
   entry Seize when not Busy is
   begin
      Busy := True;
   end Seize;

```

```ada
   procedure Release is
   begin
      Busy := False;
   end Release;
end Resource;

```

Example of a single protected declaration and corresponding body: 

```ada
protected Shared_Array is
   --  Index, Item, and Item_Array are global types
   function  Component    (N : in Index) return Item;
   procedure Set_Component(N : in Index; E : in  Item);
private
   Table : Item_Array(Index) := (others =&gt Null_Item);
end Shared_Array;

```

```ada
protected body Shared_Array is
   function Component(N : in Index) return Item is
   begin
      return Table(N);
   end Component;

```

```ada
   procedure Set_Component(N : in Index; E : in Item) is
   begin
      Table(N) := E;
   end Set_Component;
end Shared_Array;

```

Examples of protected objects: 

```ada
Control  : Resource;
Flags    : array(1 .. 100) of Resource;

```


#### Extensions to Ada 83

This entire clause is new; protected units do not exist in Ada 83. 


## 9.5  Intertask Communication

The primary means for intertask communication is provided by calls on entries and protected subprograms. Calls on protected subprograms allow coordinated access to shared data objects. Entry calls allow for blocking the caller until a given condition is satisfied (namely, that the corresponding entry is open - see 9.5.3), and then communicating data or control information directly with another task or indirectly via a shared protected object.


#### Static Semantics

Any call on an entry or on a protected subprogram identifies a target object for the operation, which is either a task (for an entry call) or a protected object (for an entry call or a protected subprogram call). The target object is considered an implicit parameter to the operation, and is determined by the operation name (or prefix) used in the call on the operation, as follows: 

If it is a direct_name or expanded name that denotes the declaration (or body) of the operation, then the target object is implicitly specified to be the current instance of the task or protected unit immediately enclosing the operation; such a call is defined to be an internal call;

If it is a selected_component that is not an expanded name, then the target object is explicitly specified to be the task or protected object denoted by the prefix of the name; such a call is defined to be an external call; 

Discussion: For example: 

```ada
protected type Pt is
  procedure Op1;
  procedure Op2;
end Pt;

```

```ada
PO : Pt;
Other_Object : Some_Other_Protected_Type;

```

```ada
protected body Pt is
  procedure Op1 is begin ... end Op1;

```

```ada
  procedure Op2 is
  begin
    Op1; -- An internal call.
    Pt.Op1; -- Another internal call.
    PO.Op1; -- An external call. It the current instance is PO, then
            -- this is a bounded error (see 9.5.1).
    Other_Object.Some_Op; -- An external call.
  end Op2;
end Pt;

```

If the name or prefix is a dereference (implicit or explicit) of an access-to-protected-subprogram value, then the target object is determined by the prefix of the Access attribute_reference that produced the access value originally, and the call is defined to be an external call;

If the name or prefix denotes a subprogram_renaming_declaration, then the target object is as determined by the name of the renamed entity.

A corresponding definition of target object applies to a requeue_statement (see 9.5.4), with a corresponding distinction between an internal requeue and an external requeue.


#### Dynamic Semantics

Within the body of a protected operation, the current instance (see 8.6) of the immediately enclosing protected unit is determined by the target object specified (implicitly or explicitly) in the call (or requeue) on the protected operation. 

To be honest: The current instance is defined in the same way within the body of a subprogram declared immediately within a protected_body. 

Any call on a protected procedure or entry of a target protected object is defined to be an update to the object, as is a requeue on such an entry. 

Reason: Read/write access to the components of a protected object is granted while inside the body of a protected procedure or entry. Also, any protected entry call can change the value of the Count attribute, which represents an update. Any protected procedure call can result in servicing the entries, which again might change the value of a Count attribute. 


#### Static Semantics

} 


### 9.5.1  Protected Subprograms and Protected Actions

A protected subprogram is a subprogram declared immediately within a protected_definition. Protected procedures provide exclusive read-write access to the data of a protected object; protected functions provide concurrent read-only access to the data. 

Ramification: A subprogram declared immediately within a protected_body is not a protected subprogram; it is an intrinsic subprogram. See 6.3.1, "Conformance Rules". 


#### Static Semantics

[Within the body of a protected function (or a function declared immediately within a protected_body), the current instance of the enclosing protected unit is defined to be a constant (that is, its subcomponents may be read but not updated). Within the body of a protected procedure (or a procedure declared immediately within a protected_body), and within an entry_body, the current instance is defined to be a variable (updating is permitted).] 

Ramification: The current instance is like an implicit parameter, of mode in for a protected function, and of mode in out for a protected procedure (or protected entry). 


#### Dynamic Semantics

For the execution of a call on a protected subprogram, the evaluation of the name or prefix and of the parameter associations, and any assigning back of in out or out parameters, proceeds as for a normal subprogram call (see 6.4). If the call is an internal call (see 9.5), the body of the subprogram is executed as for a normal subprogram call. If the call is an external call, then the body of the subprogram is executed as part of a new protected action on the target protected object; the protected action completes after the body of the subprogram is executed. [A protected action can also be started by an entry call (see 9.5.3).]

A new protected action is not started on a protected object while another protected action on the same protected object is underway, unless both actions are the result of a call on a protected function. This rule is expressible in terms of the execution resource associated with the protected object: 

Starting a protected action on a protected object corresponds to acquiring the execution resource associated with the protected object, either for concurrent read-only access if the protected action is for a call on a protected function, or for exclusive read-write access otherwise;

Completing the protected action corresponds to releasing the associated execution resource. 

[After performing an operation on a protected object other than a call on a protected function, but prior to completing the associated protected action, the entry queues (if any) of the protected object are serviced (see 9.5.3).]


#### Bounded (Run-Time) Errors

During a protected action, it is a bounded error to invoke an operation that is potentially blocking. The following are defined to be potentially blocking operations: 

Reason: Some of these operations are not directly blocking. However, they are still treated as bounded errors during a protected action, because allowing them might impose an undesirable implementation burden. 

a select_statement;

an accept_statement;

an entry_call_statement;

a delay_statement;

an abort_statement;

task creation or activation;

an external call on a protected subprogram (or an external requeue) with the same target object as that of the protected action; 

Reason: This is really a deadlocking call, rather than a blocking call, but we include it in this list for simplicity. 

a call on a subprogram whose body contains a potentially blocking operation. 

Reason: This allows an implementation to check and raise Program_Error as soon as a subprogram is called, rather than waiting to find out whether it actually reaches the potentially blocking operation. This in turn allows the potentially blocking operation check to be performed prior to run time in some environments. 

If the bounded error is detected, Program_Error is raised. If not detected, the bounded error might result in deadlock or a (nested) protected action on the same target object.

Certain language-defined subprograms are potentially blocking. In particular, the subprograms of the language-defined input-output packages that manipulate files (implicitly or explicitly) are potentially blocking. Other potentially blocking subprograms are identified where they are defined. When not specified as potentially blocking, a language-defined subprogram is nonblocking. 

NOTE 1   If two tasks both try to start a protected action on a protected object, and at most one is calling a protected function, then only one of the tasks can proceed. Although the other task cannot proceed, it is not considered blocked, and it might be consuming processing resources while it awaits its turn. There is no language-defined ordering or queuing presumed for tasks competing to start a protected action - on a multiprocessor such tasks might use busy-waiting; for monoprocessor considerations, see D.3, "Priority Ceiling Locking". 

Discussion: The intended implementation on a multi-processor is in terms of "spin locks" - the waiting task will spin. 

NOTE 2   The body of a protected unit may contain declarations and bodies for local subprograms. These are not visible outside the protected unit.

NOTE 3   The body of a protected function can contain internal calls on other protected functions, but not protected procedures, because the current instance is a constant. On the other hand, the body of a protected procedure can contain internal calls on both protected functions and procedures.

NOTE 4   From within a protected action, an internal call on a protected subprogram, or an external call on a protected subprogram with a different target object is not considered a potentially blocking operation. 

Reason: This is because a task is not considered blocked while attempting to acquire the execution resource associated with a protected object. The acquisition of such a resource is rather considered part of the normal competition for execution resources between the various tasks that are ready. External calls that turn out to be on the same target object are considered potentially blocking, since they can deadlock the task indefinitely. 


#### Examples

Examples of protected subprogram calls (see 9.4): 

```ada
Shared_Array.Set_Component(N, E);
E := Shared_Array.Component(M);
Control.Release;

```


### 9.5.2  Entries and Accept Statements

Entry_declarations, with the corresponding entry_bodies or accept_statements, are used to define potentially queued operations on tasks and protected objects. 


#### Syntax

entry_declaration ::= 
   entry defining_identifier [(discrete_subtype_definition)] parameter_profile;

accept_statement ::= 
   accept entry_direct_name [(entry_index)] parameter_profile [do
     handled_sequence_of_statements
   end [entry_identifier]];

Reason: We cannot use defining_identifier for accept_statements. Although an accept_statement is sort of like a body, it can appear nested within a block_statement, and therefore be hidden from its own entry by an outer homograph. 

entry_index ::= expression

entry_body ::= 
    entry defining_identifier entry_body_formal_partentry_barrier is
       declarative_part
    begin
       handled_sequence_of_statements
    end [entry_identifier];

entry_body_formal_part ::= [(entry_index_specification)] parameter_profile

entry_barrier ::= when condition

entry_index_specification ::= for defining_identifier in discrete_subtype_definition

If an entry_identifier appears at the end of an accept_statement, it shall repeat the entry_direct_name. If an entry_identifier appears at the end of an entry_body, it shall repeat the defining_identifier.

[An entry_declaration is allowed only in a protected or task declaration.] 

Proof: This follows from the BNF. 


#### Name Resolution Rules

In an accept_statement, the expected profile for the entry_direct_name is that of the entry_declaration; the expected type for an entry_index is that of the subtype defined by the discrete_subtype_definition of the corresponding entry_declaration.

Within the handled_sequence_of_statements of an accept_statement, if a selected_component has a prefix that denotes the corresponding entry_declaration, then the entity denoted by the prefix is the accept_statement, and the selected_component is interpreted as an expanded name (see 4.1.3)[; the selector_name of the selected_component has to be the identifier for some formal parameter of the accept_statement]. 

Proof: The only declarations that occur immediately within the declarative region of an accept_statement are those for its formal parameters. 


#### Legality Rules

An entry_declaration in a task declaration shall not contain a specification for an access parameter (see 3.10). 

Reason: Access parameters for task entries would require a complex implementation. For example: 

```ada
task T is
   entry E(Z : access Integer); -- Illegal!
end T;

```

```ada
task body T is
begin
   declare
      type A is access all Integer;
      X : A;
      Int : aliased Integer;
      task Inner;
      task body Inner is
      begin
         T.E(Int'Access);
      end Inner;
   begin
      accept E(Z : access Integer) do
         X := A(Z); -- Accessibility_Check
      end E;
   end;
end T;

```

Implementing the Accessibility_Check inside the accept_statement for E is difficult, since one does not know whether the entry caller is calling from inside the immediately enclosing declare block or from outside it. This means that the lexical nesting level associated with the designated object is not sufficient to determine whether the Accessibility_Check should pass or fail.

Note that such problems do not arise with protected entries, because entry_bodies are always nested immediately within the protected_body; they cannot be further nested as can accept_statements, nor can they be called from within the protected_body (since no entry calls are permitted inside a protected_body). 

For an accept_statement, the innermost enclosing body shall be a task_body, and the entry_direct_name shall denote an entry_declaration in the corresponding task declaration; the profile of the accept_statement shall conform fully to that of the corresponding entry_declaration. An accept_statement shall have a parenthesized entry_index if and only if the corresponding entry_declaration has a discrete_subtype_definition.

An accept_statement shall not be within another accept_statement that corresponds to the same entry_declaration, nor within an asynchronous_select inner to the enclosing task_body. 

Reason: Accept_statements are required to be immediately within the enclosing task_body (as opposed to being in a nested subprogram) to ensure that a nested task does not attempt to accept the entry of its enclosing task. We considered relaxing this restriction, either by making the check a run-time check, or by allowing a nested task to accept an entry of its enclosing task. However, neither change seemed to provide sufficient benefit to justify the additional implementation burden.

Nested accept_statements for the same entry (or entry family) are prohibited to ensure that there is no ambiguity in the resolution of an expanded name for a formal parameter of the entry. This could be relaxed by allowing the inner one to hide the outer one from all visibility, but again the small added benefit didn't seem to justify making the change for Ada 95.

Accept_statements are not permitted within asynchronous_select statements to simplify the semantics and implementation: an accept_statement in an abortable_part could result in Tasking_Error being propagated from an entry call even though the target task was still callable; implementations that use multiple tasks implicitly to implement an asynchronous_select might have trouble supporting "up-level" accepts. Furthermore, if accept_statements were permitted in the abortable_part, a task could call its own entry and then accept it in the abortable_part, leading to rather unusual and possibly difficult-to-specify semantics. 

An entry_declaration of a protected unit requires a completion[, which shall be an entry_body,] and every entry_body shall be the completion of an entry_declaration of a protected unit. The profile of the entry_body shall conform fully to that of the corresponding declaration. 

Ramification: An entry_declaration, unlike a subprogram_declaration, cannot be completed with a renaming_declaration. 

To be honest: The completion can be a pragma Import, if the implementation supports it. 

Discussion: The above applies only to protected entries, which are the only ones completed with entry_bodies. Task entries have corresponding accept_statements instead of having entry_bodies, and we do not consider an accept_statement to be a "completion," because a task entry_declaration is allowed to have zero, one, or more than one corresponding accept_statements. 

An entry_body_formal_part shall have an entry_index_specification if and only if the corresponding entry_declaration has a discrete_subtype_definition. In this case, the discrete_subtype_definitions of the entry_declaration and the entry_index_specification shall fully conform to one another (see 6.3.1). 

A name that denotes a formal parameter of an entry_body is not allowed within the entry_barrier of the entry_body.


#### Static Semantics

The parameter modes defined for parameters in the parameter_profile of an entry_declaration are the same as for a subprogram_declaration and have the same meaning (see 6.2). 

Discussion: Note that access parameters are not allowed for task entries (see above). 

An entry_declaration with a discrete_subtype_definition (see 3.6) declares a family of distinct entries having the same profile, with one such entry for each value of the entry index subtype defined by the discrete_subtype_definition. [A name for an entry of a family takes the form of an indexed_component, where the prefix denotes the entry_declaration for the family, and the index value identifies the entry within the family.] The term single entry is used to refer to any entry other than an entry of an entry family.

In the entry_body for an entry family, the entry_index_specification declares a named constant whose subtype is the entry index subtype defined by the corresponding entry_declaration; the value of the named entry index identifies which entry of the family was called. 

Ramification: The discrete_subtype_definition of the entry_index_specification is not elaborated; the subtype of the named constant declared is defined by the discrete_subtype_definition of the corresponding entry_declaration, which is elaborated, either when the type is declared, or when the object is created, if its constraint is per-object. 


#### Dynamic Semantics

For the elaboration of an entry_declaration for an entry family, if the discrete_subtype_definition contains no per-object expressions (see 3.8), then the discrete_subtype_definition is elaborated. Otherwise, the elaboration of the entry_declaration consists of the evaluation of any expression of the discrete_subtype_definition that is not a per-object expression (or part of one). The elaboration of an entry_declaration for a single entry has no effect. 

Discussion: The elaboration of the declaration of a protected subprogram has no effect, as specified in clause 6.1. The default initialization of an object of a task or protected type is covered in 3.3.1. 

[The actions to be performed when an entry is called are specified by the corresponding accept_statements (if any) for an entry of a task unit, and by the corresponding entry_body for an entry of a protected unit.]

For the execution of an accept_statement, the entry_index, if any, is first evaluated and converted to the entry index subtype; this index value identifies which entry of the family is to be accepted. Further execution of the accept_statement is then blocked until a caller of the corresponding entry is selected (see 9.5.3), whereupon the handled_sequence_of_statements, if any, of the accept_statement is executed, with the formal parameters associated with the corresponding actual parameters of the selected entry call. of the handled_sequence_of_statements, the accept_statement completes and is left. When an exception is propagated from the handled_sequence_of_statements of an accept_statement, the same exception is also raised by the execution of the corresponding entry_call_statement. 

Ramification: This is in addition to propagating it to the construct containing the accept_statement. In other words, for a rendezvous, the raising splits in two, and continues concurrently in both tasks.

The caller gets a new occurrence; this isn't considered propagation.

Note that we say "propagated from the handled_sequence_of_statements of an accept_statement", not "propagated from an accept_statement". The latter would be wrong - we don't want exceptions propagated by the entry_index to be sent to the caller (there is none yet!).

The above interaction between a calling task and an accepting task is called a rendezvous. [After a rendezvous, the two tasks continue their execution independently.]

[An entry_body is executed when the condition of the entry_barrier evaluates to True and a caller of the corresponding single entry, or entry of the corresponding entry family, has been selected (see 9.5.3).] For the execution of the entry_body, the declarative_part of the entry_body is elaborated, and the handled_sequence_of_statements of the body is executed, as for the execution of a subprogram_body. The value of the named entry index, if any, is determined by the value of the entry index specified in the entry_name of the selected entry call (or intermediate requeue_statement - see 9.5.4). 

To be honest: If the entry had been renamed as a subprogram, and the call was a procedure_call_statement using the name declared by the renaming, the entry index (if any) comes from the entry name specified in the subprogram_renaming_declaration. 

NOTE 1   A task entry has corresponding accept_statements (zero or more), whereas a protected entry has a corresponding entry_body (exactly one).

NOTE 2   A consequence of the rule regarding the allowed placements of accept_statements is that a task can execute accept_statements only for its own entries.

NOTE 3   A return_statement (see 6.5) or a requeue_statement (see 9.5.4) may be used to complete the execution of an accept_statement or an entry_body. 

Ramification: An accept_statement need not have a handled_sequence_of_statements even if the corresponding entry has parameters. Equally, it can have a handled_sequence_of_statements even if the corresponding entry has no parameters. 

Ramification: A single entry overloads a subprogram, an enumeration literal, or another single entry if they have the same defining_identifier. Overloading is not allowed for entry family names. A single entry or an entry of an entry family can be renamed as a procedure as explained in 8.5.4. 

NOTE 4   The condition in the entry_barrier may reference anything visible except the formal parameters of the entry. This includes the entry index (if any), the components (including discriminants) of the protected object, the Count attribute of an entry of that protected object, and data global to the protected unit.

The restriction against referencing the formal parameters within an entry_barrier ensures that all calls of the same entry see the same barrier value. If it is necessary to look at the parameters of an entry call before deciding whether to handle it, the entry_barrier can be "when True" and the caller can be requeued (on some private entry) when its parameters indicate that it cannot be handled immediately. 


#### Examples

Examples of entry declarations: 

```ada
entry Read(V : out Item);
entry Seize;
entry Request(Level)(D : Item);  --  a family of entries

```

Examples of accept statements: 

```ada
accept Shut_Down;

```

```ada
accept Read(V : out Item) do
   V := Local_Item;
end Read;

```

```ada
accept Request(Low)(D : Item) do
   ...
end Request;

```


#### Extensions to Ada 83

The syntax rule for entry_body is new.

Accept_statements can now have exception_handlers. 


### 9.5.3  Entry Calls

[An entry_call_statement (an entry call) can appear in various contexts.] A simple entry call is a stand-alone statement that represents an unconditional call on an entry of a target task or a protected object. [Entry calls can also appear as part of select_statements (see 9.7).] 


#### Syntax

entry_call_statement ::= entry_name [actual_parameter_part];


#### Name Resolution Rules

The entry_name given in an entry_call_statement shall resolve to denote an entry. The rules for parameter associations are the same as for subprogram calls (see 6.4 and 6.4.1). 


#### Static Semantics

[The entry_name of an entry_call_statement specifies (explicitly or implicitly) the target object of the call, the entry or entry family, and the entry index, if any (see 9.5).] 


#### Dynamic Semantics

Under certain circumstances (detailed below), an entry of a task or protected object is checked to see whether it is open or closed: 

An entry of a task is open if the task is blocked on an accept_statement that corresponds to the entry (see 9.5.2), or on a selective_accept (see 9.7.1) with an open accept_alternative that corresponds to the entry; otherwise it is closed.

An entry of a protected object is open if the condition of the entry_barrier of the corresponding entry_body evaluates to True; otherwise it is closed. If the evaluation of the condition propagates an exception, the exception Program_Error is propagated to all current callers of all entries of the protected object. 

Reason: An exception during barrier evaluation is considered essentially a fatal error. All current entry callers are notified with a Program_Error. In a fault-tolerant system, a protected object might provide a Reset protected procedure, or equivalent, to support attempts to restore such a "broken" protected object to a reasonable state. 

Discussion: Note that the definition of when a task entry is open is based on the state of the (accepting) task, whereas the "openness" of a protected entry is defined only when it is explicitly checked, since the barrier expression needs to be evaluated. Implementation permissions are given (below) to allow implementations to evaluate the barrier expression more or less often than it is checked, but the basic semantic model presumes it is evaluated at the times when it is checked. 

For the execution of an entry_call_statement, evaluation of the name and of the parameter associations is as for a subprogram call (see 6.4). The entry call is then issued: For a call on an entry of a protected object, a new protected action is started on the object (see 9.5.1). The named entry is checked to see if it is open; if open, the entry call is said to be selected immediately, and the execution of the call proceeds as follows: 

For a call on an open entry of a task, the accepting task becomes ready and continues the execution of the corresponding accept_statement (see 9.5.2).

For a call on an open entry of a protected object, the corresponding entry_body is executed (see 9.5.2) as part of the protected action. 

If the accept_statement or entry_body completes other than by a requeue (see 9.5.4), return is made to the caller (after servicing the entry queues - see below); any necessary assigning back of formal to actual parameters occurs, as for a subprogram call (see 6.4.1); such assignments take place outside of any protected action. 

Ramification: The return to the caller will generally not occur until the protected action completes, unless some other thread of control is given the job of completing the protected action and releasing the associated execution resource. 

If the named entry is closed, the entry call is added to an entry queue (as part of the protected action, for a call on a protected entry), and the call remains queued until it is selected or cancelled; there is a separate (logical) entry queue for each entry of a given task or protected object [(including each entry of an entry family)].

When a queued call is selected, it is removed from its entry queue. Selecting a queued call from a particular entry queue is called servicing the entry queue. An entry with queued calls can be serviced under the following circumstances: 

When the associated task reaches a corresponding accept_statement, or a selective_accept with a corresponding open accept_alternative;

If after performing, as part of a protected action on the associated protected object, an operation on the object other than a call on a protected function, the entry is checked and found to be open. 

If there is at least one call on a queue corresponding to an open entry, then one such call is selected according to the entry queuing policy in effect (see below), and the corresponding accept_statement or entry_body is executed as above for an entry call that is selected immediately.

The entry queuing policy controls selection among queued calls both for task and protected entry queues. The default entry queuing policy is to select calls on a given entry queue in order of arrival. If calls from two or more queues are simultaneously eligible for selection, the default entry queuing policy does not specify which queue is serviced first. Other entry queuing policies can be specified by pragmas (see D.4).

For a protected object, the above servicing of entry queues continues until there are no open entries with queued calls, at which point the protected action completes. 

Discussion: While servicing the entry queues of a protected object, no new calls can be added to any entry queue of the object, except due to an internal requeue (see 9.5.4). This is because the first step of a call on a protected entry is to start a new protected action, which implies acquiring (for exclusive read-write access) the execution resource associated with the protected object, which cannot be done while another protected action is already in progress. 

For an entry call that is added to a queue, and that is not the triggering_statement of an asynchronous_select (see 9.7.4), the calling task is blocked until the call is cancelled, or the call is selected and a corresponding accept_statement or entry_body completes without requeuing. In addition, the calling task is blocked during a rendezvous.

Ramification: For a call on a protected entry, the caller is not blocked if the call is selected immediately, unless a requeue causes the call to be queued.

An attempt can be made to cancel an entry call upon an abort (see 9.8) and as part of certain forms of select_statement (see 9.7.2, 9.7.3, and 9.7.4). The cancellation does not take place until a point (if any) when the call is on some entry queue, and not protected from cancellation as part of a requeue (see 9.5.4); at such a point, the call is removed from the entry queue and the call completes due to the cancellation. The cancellation of a call on an entry of a protected object is a protected action[, and as such cannot take place while any other protected action is occurring on the protected object. Like any protected action, it includes servicing of the entry queues (in case some entry barrier depends on a Count attribute).] 

Implementation Note: In the case of an attempted cancellation due to abort, this removal might have to be performed by the calling task itself if the ceiling priority of the protected object is lower than the task initiating the abort. 

A call on an entry of a task that has already completed its execution raises the exception Tasking_Error at the point of the call; similarly, this exception is raised at the point of the call if the called task completes its execution or becomes abnormal before accepting the call or completing the rendezvous (see 9.8). This applies equally to a simple entry call and to an entry call as part of a select_statement.


#### Implementation Permissions

An implementation may perform the sequence of steps of a protected action using any thread of control; it need not be that of the task that started the protected action. If an entry_body completes without requeuing, then the corresponding calling task may be made ready without waiting for the entire protected action to complete. 

Reason: These permissions are intended to allow flexibility for implementations on multiprocessors. On a monoprocessor, which thread of control executes the protected action is essentially invisible, since the thread is not abortable in any case, and the "current_task" function is not guaranteed to work during a protected action (see C.7). 

When the entry of a protected object is checked to see whether it is open, the implementation need not reevaluate the condition of the corresponding entry_barrier if no variable or attribute referenced by the condition (directly or indirectly) has been altered by the execution (or cancellation) of a protected procedure or entry call on the object since the condition was last evaluated. 

Ramification: Changes to variables referenced by an entry barrier that result from actions outside of a protected procedure or entry call on the protected object need not be "noticed". For example, if a global variable is referenced by an entry barrier, it should not be altered (except as part of a protected action on the object) any time after the barrier is first evaluated. In other words, globals can be used to "parameterize" a protected object, but they cannot reliably be used to control it after the first use of the protected object. 

Implementation Note: Note that even if a global variable is volatile, the implementation need only reevaluate a barrier if the global is updated during a protected action on the protected object. This ensures that an entry-open bit-vector implementation approach is possible, where the bit-vector is computed at the end of a protected action, rather than upon each entry call. 

An implementation may evaluate the conditions of all entry_barriers of a given protected object any time any entry of the object is checked to see if it is open. 

Ramification: In other words, any side effects of evaluating an entry barrier should be innocuous, since an entry barrier might be evaluated more or less often than is implied by the "official" dynamic semantics. 

Implementation Note: It is anticipated that when the number of entries is known to be small, all barriers will be evaluated any time one of them needs to be, to produce an "entry-open bit-vector". The appropriate bit will be tested when the entry is called, and only if the bit is false will a check be made to see whether the bit-vector might need to be recomputed. This should allow an implementation to maximize the performance of a call on an open entry, which seems like the most important case.

In addition to the entry-open bit-vector, an "is-valid" bit is needed per object, which indicates whether the current bit-vector setting is valid. A "depends-on-Count-attribute" bit is needed per type. The "is-valid" bit is set to false (as are all the bits of the bit-vector) when the protected object is first created, as well as any time an exception is propagated from computing the bit-vector. Is-valid would also be set false any time the Count is changed and "depends-on-Count-attribute" is true for the type, or a protected procedure or entry returns indicating it might have updated a variable referenced in some barrier.

A single procedure can be compiled to evaluate all of the barriers, set the entry-open bit-vector accordingly, and set the is-valid bit to true. It could have a "when others" handler to set them all false, and call a routine to propagate Program_Error to all queued callers.

For protected types where the number of entries is not known to be small, it makes more sense to evaluate a barrier only when the corresponding entry is checked to see if it is open. It isn't worth saving the state of the entry between checks, because of the space that would be required. Furthermore, the entry queues probably want to take up space only when there is actually a caller on them, so rather than an array of all entry queues, a linked list of nonempty entry queues make the most sense in this case, with the first caller on each entry queue acting as the queue header. 

When an attempt is made to cancel an entry call, the implementation need not make the attempt using the thread of control of the task (or interrupt) that initiated the cancellation; in particular, it may use the thread of control of the caller itself to attempt the cancellation, even if this might allow the entry call to be selected in the interim. 

Reason: Because cancellation of a protected entry call is a protected action (which helps make the Count attribute of a protected entry meaningful), it might not be practical to attempt the cancellation from the thread of control that initiated the cancellation. For example, if the cancellation is due to the expiration of a delay, it is unlikely that the handler of the timer interrupt could perform the necessary protected action itself (due to being on the interrupt level). Similarly, if the cancellation is due to an abort, it is possible that the task initiating the abort has a priority higher than the ceiling priority of the protected object (for implementations that support ceiling priorities). Similar considerations could apply in a multiprocessor situation. 

NOTE 1   If an exception is raised during the execution of an entry_body, it is propagated to the corresponding caller (see 11.4).

NOTE 2   For a call on a protected entry, the entry is checked to see if it is open prior to queuing the call, and again thereafter if its Count attribute (see 9.9) is referenced in some entry barrier. 

Ramification: Given this, extra care is required if a reference to the Count attribute of an entry appears in the entry's own barrier. 

Reason: An entry is checked to see if it is open prior to queuing to maximize the performance of a call on an open entry. 

NOTE 3   In addition to simple entry calls, the language permits timed, conditional, and asynchronous entry calls (see 9.7.2, 9.7.3, and see 9.7.4). 

Ramification: A task can call its own entries, but the task will deadlock if the call is a simple entry call. 

NOTE 4   The condition of an entry_barrier is allowed to be evaluated by an implementation more often than strictly necessary, even if the evaluation might have side effects. On the other hand, an implementation need not reevaluate the condition if nothing it references was updated by an intervening protected action on the protected object, even if the condition references some global variable that might have been updated by an action performed from outside of a protected action. 


#### Examples

Examples of entry calls: 

```ada
Agent.Shut_Down;                      --  see 9.1
Parser.Next_Lexeme(E);                --  see 9.1
Pool(5).Read(Next_Char);              --  see 9.1
Controller.Request(Low)(Some_Item);   --  see 9.1
Flags(3).Seize;                       --  see 9.4

```


### 9.5.4  Requeue Statements

[A requeue_statement can be used to complete an accept_statement or entry_body, while redirecting the corresponding entry call to a new (or the same) entry queue. Such a requeue can be performed with or without allowing an intermediate cancellation of the call, due to an abort or the expiration of a delay. ]


#### Syntax

requeue_statement ::= requeue entry_name [with abort];


#### Name Resolution Rules

The entry_name of a requeue_statement shall resolve to denote an entry (the target entry)that either has no parameters, or that has a profile that is type conformant (see 6.3.1) with the profile of the innermost enclosing entry_body or accept_statement. 


#### Legality Rules

A requeue_statement shall be within a callable construct that is either an entry_body or an accept_statement, and this construct shall be the innermost enclosing body or callable construct.

If the target entry has parameters, then its profile shall be subtype conformant with the profile of the innermost enclosing callable construct. 

In a requeue_statement of an accept_statement of some task unit, either the target object shall be a part of a formal parameter of the accept_statement, or the accessibility level of the target object shall not be equal to or statically deeper than any enclosing accept_statement of the task unit. In a requeue_statement of an entry_body of some protected unit, either the target object shall be a part of a formal parameter of the entry_body, or the accessibility level of the target object shall not be statically deeper than that of the entry_declaration.

Ramification: In the entry_body case, the intent is that the target object can be global, or can be a component of the protected unit, but cannot be a local variable of the entry_body. 

Reason: These restrictions ensure that the target object of the requeue outlives the completion and finalization of the enclosing callable construct. They also prevent requeuing from a nested accept_statement on a parameter of an outer accept_statement, which could create some strange "long-distance" connections between an entry caller and its server.

Note that in the strange case where a task_body is nested inside an accept_statement, it is permissible to requeue from an accept_statement of the inner task_body on parameters of the outer accept_statement. This is not a problem because all calls on the inner task have to complete before returning from the outer accept_statement, meaning no "dangling calls" will be created. 

Implementation Note: By disallowing certain requeues, we ensure that the normal terminate_alternative rules remain sensible, and that explicit clearing of the entry queues of a protected object during finalization is rarely necessary. In particular, such clearing of the entry queues is necessary only (ignoring premature Unchecked_Deallocation) for protected objects declared in a task_body (or created by an allocator for an access type declared in such a body) containing one or more requeue_statements. Protected objects declared in subprograms, or at the library level, will never need to have their entry queues explicitly cleared during finalization. 


#### Dynamic Semantics

The execution of a requeue_statement proceeds by first evaluating the entry_name[, including the prefix identifying the target task or protected object and the expression identifying the entry within an entry family, if any]. The entry_body or accept_statement enclosing the requeue_statement is then completed[, finalized, and left (see 7.6.1)].

For the execution of a requeue on an entry of a target task, after leaving the enclosing callable construct, the named entry is checked to see if it is open and the requeued call is either selected immediately or queued, as for a normal entry call (see 9.5.3).

For the execution of a requeue on an entry of a target protected object, after leaving the enclosing callable construct: 

if the requeue is an internal requeue (that is, the requeue is back on an entry of the same protected object - see 9.5), the call is added to the queue of the named entry and the ongoing protected action continues (see 9.5.1); 

Ramification: Note that for an internal requeue, the call is queued without checking whether the target entry is open. This is because the entry queues will be serviced before the current protected action completes anyway, and considering the requeued call immediately might allow it to "jump" ahead of existing callers on the same queue. 

if the requeue is an external requeue (that is, the target protected object is not implicitly the same as the current object - see 9.5), a protected action is started on the target object and proceeds as for a normal entry call (see 9.5.3). 

If the new entry named in the requeue_statement has formal parameters, then during the execution of the accept_statement or entry_body corresponding to the new entry, the formal parameters denote the same objects as did the corresponding formal parameters of the callable construct completed by the requeue. [In any case, no parameters are specified in a requeue_statement; any parameter passing is implicit.]

If the requeue_statement includes the reserved words with abort (it is a requeue-with-abort), then: 

if the original entry call has been aborted (see 9.8), then the requeue acts as an abort completion point for the call, and the call is cancelled and no requeue is performed;

if the original entry call was timed (or conditional), then the original expiration time is the expiration time for the requeued call. 

If the reserved words with abort do not appear, then the call remains protected against cancellation while queued as the result of the requeue_statement. 

Ramification: This protection against cancellation lasts only until the call completes or a subsequent requeue-with-abort is performed on the call. 

Reason: We chose to protect a requeue, by default, against abort or cancellation. This seemed safer, since it is likely that extra steps need to be taken to allow for possible cancellation once the servicing of an entry call has begun. This also means that in the absence of with abort the usual Ada 83 behavior is preserved, namely that once an entry call is accepted, it cannot be cancelled until it completes. 

NOTE 1   A requeue is permitted from a single entry to an entry of an entry family, or vice-versa. The entry index, if any, plays no part in the subtype conformance check between the profiles of the two entries; an entry index is part of the entry_name for an entry of a family. 


#### Examples

Examples of requeue statements: 

```ada
requeue Request(Medium) with abort;
                    -- requeue on a member of an entry family of the current task, see 9.1

```

```ada
requeue Flags(I).Seize;
                    -- requeue on an entry of an array component, see 9.4

```


#### Extensions to Ada 83

The requeue_statement is new. 


## 9.6  Delay Statements, Duration, and Time

[ A delay_statement is used to block further execution until a specified expiration time is reached. The expiration time can be specified either as a particular point in time (in a delay_until_statement), or in seconds from the current time (in a delay_relative_statement). The language-defined package Calendar provides definitions for a type Time and associated operations, including a function Clock that returns the current time. ]


#### Syntax

delay_statement ::= delay_until_statement | delay_relative_statement

delay_until_statement ::= delay until delay_expression;

delay_relative_statement ::= delay delay_expression;


#### Name Resolution Rules

The expected type for the delay_expression in a delay_relative_statement is the predefined type Duration. The delay_expression in a delay_until_statement is expected to be of any nonlimited type.


#### Legality Rules

There can be multiple time bases, each with a corresponding clock, and a corresponding time type. The type of the delay_expression in a delay_until_statement shall be a time type - either the type Time defined in the language-defined package Calendar (see below), or some other implementation-defined time type (see D.8). 

Implementation defined: Any implementation-defined time types.


#### Static Semantics

[There is a predefined fixed point type named Duration, declared in the visible part of package Standard;] a value of type Duration is used to represent the length of an interval of time, expressed in seconds. [The type Duration is not specific to a particular time base, but can be used with any time base.]

A value of the type Time in package Calendar, or of some other implementation-defined time type, represents a time as reported by a corresponding clock.

The following language-defined library package exists: 

```ada

package Ada.Calendar is
  type Time is private;

```

```ada
  subtype Year_Number  is Integer range 1901 .. 2099;
  subtype Month_Number is Integer range 1 .. 12;
  subtype Day_Number   is Integer range 1 .. 31;
  subtype Day_Duration is Duration range 0.0 .. 86_400.0;

```

```ada
  function Clock return Time;

```

```ada
  function Year   (Date : Time) return Year_Number;
  function Month  (Date : Time) return Month_Number;
  function Day    (Date : Time) return Day_Number;
  function Seconds(Date : Time) return Day_Duration;

```

```ada
  procedure Split (Date  : in Time;
                   Year    : out Year_Number;
                   Month   : out Month_Number;
                   Day     : out Day_Number;
                   Seconds : out Day_Duration);

```

```ada
  function Time_Of(Year  : Year_Number;
                   Month   : Month_Number;
                   Day     : Day_Number;
                   Seconds : Day_Duration := 0.0)
    return Time;

```

```ada
  function "+" (Left : Time;   Right : Duration) return Time;
  function "+" (Left : Duration; Right : Time) return Time;
  function "-" (Left : Time;   Right : Duration) return Time;
  function "-" (Left : Time;   Right : Time) return Duration;

```

```ada
  function "&lt" (Left, Right : Time) return Boolean;
  function "&lt="(Left, Right : Time) return Boolean;
  function "&gt" (Left, Right : Time) return Boolean;
  function "&gt="(Left, Right : Time) return Boolean;

```

```ada
  Time_Error : exception;

```

```ada
private
   ... -- not specified by the language
end Ada.Calendar;

```


#### Dynamic Semantics

For the execution of a delay_statement, the delay_expression is first evaluated. For a delay_until_statement, the expiration time for the delay is the value of the delay_expression, in the time base associated with the type of the expression. For a delay_relative_statement, the expiration time is defined as the current time, in the time base associated with relative delays, plus the value of the delay_expression converted to the type Duration, and then rounded up to the next clock tick. The time base associated with relative delays is as defined in D.9, "Delay Accuracy" or is implementation defined. 

Implementation defined: The time base associated with relative delays.

Ramification: Rounding up to the next clock tick means that the reading of the delay-relative clock when the delay expires should be no less than the current reading of the delay-relative clock plus the specified duration. 

The task executing a delay_statement is blocked until the expiration time is reached, at which point it becomes ready again. If the expiration time has already passed, the task is not blocked. 

Discussion: For a delay_relative_statement, this case corresponds to when the value of the delay_expression is zero or negative.

Even though the task is not blocked, it might be put back on the end of its ready queue. See D.2, "Priority Scheduling". 

If an attempt is made to cancel the delay_statement [(as part of an asynchronous_select or abort - see 9.7.4 and 9.8)], the _statement is cancelled if the expiration time has not yet passed, thereby completing the delay_statement. 

Reason: This is worded this way so that in an asynchronous_select where the triggering_statement is a delay_statement, an attempt to cancel the delay when the abortable_part completes is ignored if the expiration time has already passed, in which case the optional statements of the triggering_alternative are executed. 

The time base associated with the type Time of package Calendar is implementation defined. The function Clock of package Calendar returns a value representing the current time for this time base. [The implementation-defined value of the named number System.Tick (see 13.7) is an approximation of the length of the real-time interval during which the value of Calendar.Clock remains constant.] 

Implementation defined: The time base of the type Calendar.Time.

The functions Year, Month, Day, and Seconds return the corresponding values for a given value of the type Time, as appropriate to an implementation-defined timezone; the procedure Split returns all four corresponding values. Conversely, the function Time_Of combines a year number, a month number, a day number, and a duration, into a value of type Time. The operators "+" and "" for addition and subtraction of times and durations, and the relational operators for times, have the conventional meaning. 

Implementation defined: The timezone used for package Calendar operations.

If Time_Of is called with a seconds value of 86_400.0, the value returned is equal to the value of Time_Of for the next day with a seconds value of 0.0. The value returned by the function Seconds or through the Seconds parameter of the procedure Split is always less than 86_400.0.

The exception Time_Error is raised by the function Time_Of if the actual parameters do not form a proper date. This exception is also raised by the operators "+" and "" if the result is not representable in the type Time or Duration, as appropriate. This exception is also raised by the function Yearor the procedure Split if the year number of the given date is outside of the range of the subtype Year_Number. 

To be honest: By "proper date" above we mean that the given year has a month with the given day. For example, February 29th is a proper date only for a leap year. 

Reason: We allow Year and Split to raise Time_Error because the arithmetic operators are allowed (but not required) to produce times that are outside the range of years from 1901 to 2099. This is similar to the way integer operators may return values outside the base range of their type so long as the value is mathematically correct. 


#### Implementation Requirements

The implementation of the type Duration shall allow representation of time intervals (both positive and negative) up to at least 86400 seconds (one day); Duration'Small shall not be greater than twenty milliseconds. The implementation of the type Time shall allow representation of all dates with year numbers in the range of Year_Number[; it may allow representation of other dates as well (both earlier and later).] 


#### Implementation Permissions

An implementation may define additional time types (see D.8).

An implementation may raise Time_Error if the value of a delay_expression in a delay_until_statement of a select_statement represents a time more than 90 days past the current time. The actual limit, if any, is implementation-defined. 

Implementation defined: Any limit on delay_until_statements of select_statements.

Implementation Note: This allows an implementation to implement select_statement timeouts using a representation that does not support the full range of a time type. In particular 90 days of seconds can be represented in 23 bits, allowing a signed 24-bit representation for the seconds part of a timeout. There is no similar restriction allowed for stand-alone delay_until_statements, as these can be implemented internally using a loop if necessary to accommodate a long delay. 


#### Implementation Advice

Whenever possible in an implementation, the value of Duration'Small should be no greater than 100 microseconds. 

Implementation Note: This can be satisfied using a 32-bit 2's complement representation with a small of 2.0**(14) - that is, 61 microseconds - and a range of ± 2.0**17 - that is, 131_072.0. 

The time base for delay_relative_statements should be monotonic; it need not be the same time base as used for Calendar.Clock. 

NOTE 1   A delay_relative_statement with a negative value of the delay_expression is equivalent to one with a zero value.

NOTE 2   A delay_statement may be executed by the environment task; consequently delay_statements may be executed as part of the elaboration of a library_item or the execution of the main subprogram. Such statements delay the environment task (see 10.2).

NOTE 3   A delay_statement is an abort completion point and a potentially blocking operation, even if the task is not actually blocked.

NOTE 4   There is no necessary relationship between System.Tick (the resolution of the clock of package Calendar) and Duration'Small (the small of type Duration). 

Ramification: The inaccuracy of the delay_statement has no relation to System.Tick. In particular, it is possible that the clock used for the delay_statement is less accurate than Calendar.Clock.

We considered making Tick a run-time-determined quantity, to allow for easier configurability. However, this would not be upward compatible, and the desired configurability can be achieved using functionality defined in Annex D, "Real-Time Systems". 

NOTE 5   Additional requirements associated with delay_statements are given in D.9, "Delay Accuracy".


#### Examples

Example of a relative delay statement: 

```ada
delay 3.0;  -- delay 3.0 seconds

```

Example of a periodic task: 

```ada
declare
   use Ada.Calendar;
   Next_Time : Time := Clock + Period;
                      -- Period is a global constant of type Duration
begin
   loop               -- repeated every Period seconds
      delay until Next_Time;
      ... -- perform some actions
      Next_Time := Next_Time + Period;
   end loop;
end;

```


#### Inconsistencies With Ada 83

For programs that raise Time_Error on "+" or "" in Ada 83,the exception might be deferred until a call on Split or Year_Number, or might not be raised at all (if the offending time is never Split after being calculated). This should not affect typical programs, since they deal only with times corresponding to the relatively recent past or near future. 


#### Extensions to Ada 83

The syntax rule for delay_statement is modified to allow delay_until_statements.

The type Time may represent dates with year numbers outside of Year_Number. Therefore, the operations "+" and "" need only raise Time_Error if the result is not representable in Time (or Duration); also, Split or Year will now raise Time_Error if the year number is outside of Year_Number. This change is intended to simplify the implementation of "+" and "" (allowing them to depend on overflow for detecting when to raise Time_Error) and to allow local timezone information to be considered at the time of Split rather than Clock (depending on the implementation approach). For example, in a POSIX environment, it is natural for the type Time to be based on GMT, and the results of procedure Split (and the functions Year, Month, Day, and Seconds) to depend on local time zone information. In other environments, it is more natural for the type Time to be based on the local time zone, with the results of Year, Month, Day, and Seconds being pure functions of their input.

We anticipate that implementations will provide child packages of Calendar to provide more explicit control over time zones and other environment-dependent time-related issues. These would be appropriate for standardization in a given environment (such as POSIX). 


## 9.7  Select Statements

[There are four forms of the select_statement. One form provides a selective wait for one or more select_alternatives. Two provide timed and conditional entry calls. The fourth provides asynchronous transfer of control.] 


#### Syntax

select_statement ::= 
   selective_accept
  | timed_entry_call
  | conditional_entry_call
  | asynchronous_select


#### Examples

Example of a select statement: 

```ada
select
   accept Driver_Awake_Signal;
or
   delay 30.0*Seconds;
   Stop_The_Train;
end select;

```


#### Extensions to Ada 83

Asynchronous_select is new. 


### 9.7.1  Selective Accept

[This form of the select_statement allows a combination of waiting for, and selecting from, one or more alternatives. The selection may depend on conditions associated with each alternative of the selective_accept. ]


#### Syntax

selective_accept ::= 
  select
   [guard]
     select_alternative
{ or
   [guard]
     select_alternative }
[ else
   sequence_of_statements ]
  end select;

guard ::= when condition =&gt

select_alternative ::= 
   accept_alternative
  | delay_alternative
  | terminate_alternative

accept_alternative ::= 
  accept_statement [sequence_of_statements]

delay_alternative ::= 
  delay_statement [sequence_of_statements]

terminate_alternative ::= terminate;

A selective_accept shall contain at least one accept_alternative. In addition, it can contain: 

a terminate_alternative (only one); or

one or more delay_alternatives; or

an else part (the reserved word else followed by a sequence_of_statements). 

These three possibilities are mutually exclusive. 


#### Legality Rules

If a selective_accept contains more than one delay_alternative, then all shall be delay_relative_statements, or all shall be delay_until_statements for the same time type. 

Reason: This simplifies the implementation and the description of the semantics. 


#### Dynamic Semantics

A select_alternative is said to be open if it is not immediately preceded by a guard, or if the condition of its guard evaluates to True. It is said to be closed otherwise.

For the execution of a selective_accept, any guard conditions are evaluated; open alternatives are thus determined. For an open delay_alternative, the delay_expression is also evaluated. Similarly, for an open accept_alternative for an entry of a family, the entry_index is also evaluated. These evaluations are performed in an arbitrary order, except that a delay_expression or entry_index is not evaluated until after evaluating the corresponding condition, if any. Selection and execution of one open alternative, or of the else part, then completes the execution of the selective_accept; the rules for this selection are described below.

Open accept_alternatives are first considered. Selection of one such alternative takes place immediately if the corresponding entry already has queued calls. If several alternatives can thus be selected, one of them is selected according to the entry queuing policy in effect (see 9.5.3 and D.4). When such an alternative is selected, the selected call is removed from its entry queue and the handled_sequence_of_statements (if any) of the corresponding accept_statement is executed; after the rendezvous completes any subsequent sequence_of_statements of the alternative is executed. If no selection is immediately possible (in the above sense) and there is no else part, the task blocks until an open alternative can be selected.

Selection of the other forms of alternative or of an else part is performed as follows: 

An open delay_alternative is selected when its expiration time is reached if no accept_alternative or other delay_alternative can be selected prior to the expiration time. If several delay_alternatives have this same expiration time, one of them is selected according to the queuing policy in effect (see D.4); the default queuing policy chooses arbitrarily among the delay_alternatives whose expiration time has passed.

The else part is selected and its sequence_of_statements is executed if no accept_alternative can immediately be selected; in particular, if all alternatives are closed.

An open terminate_alternative is selected if the conditions stated at the end of clause 9.3 are satisfied. 

Ramification: In the absence of a requeue_statement, the conditions stated are such that a terminate_alternative cannot be selected while there is a queued entry call for any entry of the task. In the presence of requeues from a task to one of its subtasks, it is possible that when a terminate_alternative of the subtask is selected, requeued calls (for closed entries only) might still be queued on some entry of the subtask. Tasking_Error will be propagated to such callers, as is usual when a task completes while queued callers remain. 

The exception Program_Error is raised if all alternatives are closed and there is no else part.

NOTE 1   A selective_accept is allowed to have several open delay_alternatives. A selective_accept is allowed to have several open accept_alternatives for the same entry.


#### Examples

Example of a task body with a selective accept: 

```ada
task body Server is
   Current_Work_Item : Work_Item;
begin
   loop
      select
         accept Next_Work_Item(WI : in Work_Item) do
            Current_Work_Item := WI;
         end;
         Process_Work_Item(Current_Work_Item);
      or
         accept Shut_Down;
         exit;       -- Premature shut down requested
      or
         terminate;  -- Normal shutdown at end of scope
      end select;
   end loop;
end Server;

```


#### Wording Changes from Ada 83

The name of selective_wait was changed to selective_accept to better describe what is being waited for. We kept select_alternative as is, because selective_accept_alternative was too easily confused with accept_alternative. 


### 9.7.2  Timed Entry Calls

[A timed_entry_call issues an entry call that is cancelled if the call (or a requeue-with-abort of the call) is not selected before the expiration time is reached. ]


#### Syntax

timed_entry_call ::= 
  select
   entry_call_alternative
  or
   delay_alternative
  end select;

entry_call_alternative ::= 
  entry_call_statement [sequence_of_statements]


#### Dynamic Semantics

For the execution of a timed_entry_call, the entry_name and any actual parameters are evaluated, as for a simple entry call (see 9.5.3). The expiration time (see 9.6) for the call is determined by evaluating the delay_expression of the delay_alternative; the entry call is then issued.

If the call is queued (including due to a requeue-with-abort), and not selected before the expiration time is reached, an attempt to cancel the call is made. If the call completes due to the cancellation, the optional sequence_of_statements of the delay_alternative is executed; if the entry call completes normally, the optional sequence_of_statements of the entry_call_alternative is executed. 

Ramification: The fact that the syntax calls for an entry_call_statement means that this fact is used in overload resolution. For example, if there is a procedure X and an entry X (both with no parameters), then "select X; ..." is legal, because overload resolution knows that the entry is the one that was meant. 


#### Examples

Example of a timed entry call: 

```ada
select
   Controller.Request(Medium)(Some_Item);
or
   delay 45.0;
   --  controller too busy, try something else
end select;

```


#### Wording Changes from Ada 83

This clause comes before the one for Conditional Entry Calls, so we can define conditional entry calls in terms of timed entry calls. 


### 9.7.3  Conditional Entry Calls

[A conditional_entry_call issues an entry call that is then cancelled if it is not selected immediately (or if a requeue-with-abort of the call is not selected immediately).] 

To be honest: In the case of an entry call on a protected object, it is OK if the entry is closed at the start of the corresponding protected action, so long as it opens and the call is selected before the end of that protected action (due to changes in the Count attribute). 


#### Syntax

conditional_entry_call ::= 
  select
   entry_call_alternative
  else
   sequence_of_statements
  end select;


#### Dynamic Semantics

The execution of a conditional_entry_call is defined to be equivalent to the execution of a timed_entry_call with a delay_alternative specifying an immediate expiration time and the same sequence_of_statements as given after the reserved word else. 

NOTE 1   A conditional_entry_call may briefly increase the Count attribute of the entry, even if the conditional call is not selected.


#### Examples

Example of a conditional entry call: 

```ada
procedure Spin(R : in Resource) is
begin
   loop
      select
         R.Seize;
         return;
      else
         null;  --  busy waiting
      end select;
   end loop;
end;

```


#### Wording Changes from Ada 83

This clause comes after the one for Timed Entry Calls, so we can define conditional entry calls in terms of timed entry calls. We do that so that an "expiration time" is defined for both, thereby simplifying the definition of what happens on a requeue-with-abort. 


### 9.7.4  Asynchronous Transfer of Control

[An asynchronous select_statement provides asynchronous transfer of control upon completion of an entry call or the expiration of a delay.] 


#### Syntax

asynchronous_select ::= 
  select
   triggering_alternative
  then abort
   abortable_part
  end select;

triggering_alternative ::= triggering_statement [sequence_of_statements]

triggering_statement ::= entry_call_statement | delay_statement

abortable_part ::= sequence_of_statements


#### Dynamic Semantics

For the execution of an asynchronous_select whose triggering_statement is an entry_call_statement, the entry_name and actual parameters are evaluated as for a simple entry call (see 9.5.3), and the entry call is issued. If the entry call is queued (or requeued-with-abort), then the abortable_part is executed. [If the entry call is selected immediately, and never requeued-with-abort, then the abortable_part is never started.]

For the execution of an asynchronous_select whose triggering_statement is a delay_statement, the delay_expression is evaluated and the expiration time is determined, as for a normal delay_statement. If the expiration time has not already passed, the abortable_part is executed.

If the abortable_part completes and is left prior to completion of the triggering_statement, an attempt to cancel the triggering_statement is made. If the attempt to cancel succeeds (see 9.5.3 and 9.6), the asynchronous_select is complete.

If the triggering_statement completes other than due to cancellation, the abortable_part is aborted (if started but not yet completed - see 9.8). If the triggering_statement completes normally, the optional sequence_of_statements of the triggering_alternative is executed after the abortable_part is left. 

Discussion: We currently don't specify when the by-copy [in] out parameters are assigned back into the actuals. We considered requiring that to happen after the abortable_part is left. However, that doesn't seem useful enough to justify possibly overspecifying the implementation approach, since some of the parameters are passed by reference anyway.

In an earlier description, we required that the sequence_of_statements of the triggering_alternative execute after aborting the abortable_part, but before waiting for it to complete and finalize, to provide more rapid response to the triggering event in case the finalization was unbounded. However, various reviewers felt that this created unnecessary complexity in the description, and a potential for undesirable concurrency (and nondeterminism) within a single task. We have now reverted to simpler, more deterministic semantics, but anticipate that further discussion of this issue might be appropriate during subsequent reviews. One possibility is to leave this area implementation defined, so as to encourage experimentation. The user would then have to assume the worst about what kinds of actions are appropriate for the sequence_of_statements of the triggering_alternative to achieve portability. 


#### Examples

Example of a main command loop for a command interpreter: 

```ada
loop
   select
      Terminal.Wait_For_Interrupt;
      Put_Line("Interrupted");
   then abort
      -- This will be abandoned upon terminal interrupt
      Put_Line("-&gt ");
      Get_Line(Command, Last);
      Process_Command(Command(1..Last));
   end select;
end loop;

```

Example of a time-limited calculation: 

```ada
select
   delay 5.0;
   Put_Line("Calculation does not converge");
then abort
   -- This calculation should finish in 5.0 seconds;
   --  if not, it is assumed to diverge.
   Horribly_Complicated_Recursive_Function(X, Y);
end select;

```


#### Extensions to Ada 83

Asynchronous_select is new. 


## 9.8  Abort of a Task - Abort of a Sequence of Statements

[An abort_statement causes one or more tasks to become abnormal, thus preventing any further interaction with such tasks. The completion of the triggering_statement of an asynchronous_select causes a sequence_of_statements to be aborted.] 


#### Syntax

abort_statement ::= abort task_name {, task_name};


#### Name Resolution Rules

Each task_name is expected to be of any task type[; they need not all be of the same task type.]


#### Dynamic Semantics

For the execution of an abort_statement, the given task_names are evaluated in an arbitrary order. Each named task is then aborted, which consists of making the task abnormal and aborting the execution of the corresponding task_body, unless it is already completed. 

Ramification: Note that aborting those tasks is not defined to be an abort-deferred operation. Therefore, if one of the named tasks is the task executing the abort_statement, or if the task executing the abort_statement depends on one of the named tasks, then it is possible for the execution of the abort_statement to be aborted, thus leaving some of the tasks unaborted. This allows the implementation to use either a sequence of calls to an "abort task" RTS primitive, or a single call to an "abort list of tasks" RTS primitive. 

When the execution of a construct is aborted (including that of a task_body or of a sequence_of_statements), the execution of every construct included within the aborted execution is also aborted, except for executions included within the execution of an abort-deferred operation; the execution of an abort-deferred operation continues to completion without being affected by the abort; the following are the abort-deferred operations: 

a protected action;

waiting for an entry call to complete (after having initiated the attempt to cancel it - see below);

waiting for the termination of dependent tasks;

the execution of an Initialize procedure as the last step of the default initialization of a controlled object;

the execution of a Finalize procedure as part of the finalization of a controlled object;

an assignment operation to an object with a controlled part. 

[The last three of these are discussed further in 7.6.] 

Reason: Deferring abort during Initialize and finalization allows, for example, the result of an allocator performed in an Initialize operation to be assigned into an access object without being interrupted in the middle, which would cause storage leaks. For an object with several controlled parts, each individual Initialize is abort-deferred. Note that there is generally no semantic difference between making each Finalize abort-deferred, versus making a group of them abort-deferred, because if the task gets aborted, the first thing it will do is complete any remaining finalizations. Individual objects are finalized prior to an assignment operation (if nonlimited controlled) and as part of Unchecked_Deallocation. 

Ramification: Abort is deferred during the entire assignment operation to an object with a controlled part, even if only some subcomponents are controlled. Note that this says "assignment operation", not "assignment_statement". Explicit calls to Initialize, Finalize, or Adjust are not abort-deferred. 

When a master is aborted, all tasks that depend on that master are aborted.

The order in which tasks become abnormal as the result of an abort_statement or the abort of a sequence_of_statements is not specified by the language.

If the execution of an entry call is aborted, an immediate attempt is made to cancel the entry call (see 9.5.3). If the execution of a construct is aborted at a time when the execution is blocked, other than for an entry call, at a point that is outside the execution of an abort-deferred operation, then the execution of the construct completes immediately. For an abort due to an abort_statement, these immediate effects occur before the execution of the abort_statement completes. Other than for these immediate cases, the execution of a construct that is aborted does not necessarily complete before the abort_statement completes. However, the execution of the aborted construct completes no later than its next abort completion point (if any) that occurs outside of an abort-deferred operation; the following are abort completion points for an execution: 

the point where the execution initiates the activation of another task;

the end of the activation of a task;

the start or end of the execution of an entry call, accept_statement, delay_statement, or abort_statement; 

Ramification: Although the abort completion point doesn't occur until the end of the entry call or delay_statement, these operations might be cut short because an abort attempts to cancel them. 

the start of the execution of a select_statement, or of the sequence_of_statements of an exception_handler. 

Reason: The start of an exception_handler is considered an abort completion point simply because it is easy for an implementation to check at such points. 

Implementation Note: Implementations may of course check for abort more often than at each abort completion point; ideally, a fully preemptive implementation of abort will be provided. If preemptive abort is not supported in a given environment, then supporting the checking for abort as part of subprogram calls and loop iterations might be a useful option. 


#### Bounded (Run-Time) Errors

An attempt to execute an asynchronous_select as part of the execution of an abort-deferred operation is a bounded error. Similarly, an attempt to create a task that depends on a master that is included entirely within the execution of an abort-deferred operation is a bounded error. In both cases, Program_Error is raised if the error is detected by the implementation; otherwise the operations proceed as they would outside an abort-deferred operation, except that an abort of the abortable_part or the created task might or might not have an effect. 

Reason: An asynchronous_select relies on an abort of the abortable_part to effect the asynchronous transfer of control. For an asynchronous_select within an abort-deferred operation, the abort might have no effect.

Creating a task dependent on a master included within an abort-deferred operation is considered an error, because such tasks could be aborted while the abort-deferred operation was still progressing, undermining the purpose of abort-deferral. Alternatively, we could say that such tasks are abort-deferred for their entire execution, but that seems too easy to abuse. Note that task creation is already a bounded error in protected actions, so this additional rule only applies to local task creation as part of Initialize, Finalize, or Adjust. 


#### Erroneous Execution

If an assignment operation completes prematurely due to an abort, the assignment is said to be disrupted; the target of the assignment or its parts can become abnormal, and certain subsequent uses of the object can be erroneous, as explained in 13.9.1. 

NOTE 1   An abort_statement should be used only in situations requiring unconditional termination.

NOTE 2   A task is allowed to abort any task it can name, including itself.

NOTE 3   Additional requirements associated with abort are given in D.6, "Preemptive Abort". 


#### Wording Changes from Ada 83

This clause has been rewritten to accommodate the concept of aborting the execution of a construct, rather than just of a task. 


## 9.9  Task and Entry Attributes


#### Dynamic Semantics

For a prefix T that is of a task type [(after any implicit dereference)], the following attributes are defined: 

T'CallableYields the value True when the task denoted by T is callable, and False otherwise; a task is callable unless it is completed or abnormal. The value of this attribute is of the predefined type Boolean.

T'TerminatedYields the value True if the task denoted by T is terminated, and False otherwise. The value of this attribute is of the predefined type Boolean. 

For a prefix E that denotes an entry of a task or protected unit, the following attribute is defined. This attribute is only allowed within the body of the task or protected unit, but excluding, in the case of an entry of a task unit, within any program unit that is, itself, inner to the body of the task unit. 

E'CountYields the number of calls presently queued on the entry E of the current instance of the unit. The value of this attribute is of the type universal_integer.

NOTE 1   For the Count attribute, the entry can be either a single entry or an entry of a family. The name of the entry or entry family can be either a direct_name or an expanded name.

NOTE 2   Within task units, algorithms interrogating the attribute E'Count should take precautions to allow for the increase of the value of this attribute for incoming entry calls, and its decrease, for example with timed_entry_calls. Also, a conditional_entry_call may briefly increase this value, even if the conditional call is not accepted.

NOTE 3   Within protected units, algorithms interrogating the attribute E'Count in the entry_barrier for the entry E should take precautions to allow for the evaluation of the condition of the barrier both before and after queuing a given caller. 


## 9.10  Shared Variables


#### Static Semantics

If two different objects, including nonoverlapping parts of the same object, are independently addressable, they can be manipulated concurrently by two different tasks without synchronization. Normally, any two nonoverlapping objects are independently addressable. However, if packing, record layout, or Component_Size is specified for a given composite object, then it is implementation defined whether or not two nonoverlapping parts of that composite object are independently addressable. 

This paragraph was deleted.Implementation defined: Whether or not two nonoverlapping parts of a composite object are independently addressable, in the case where packing, record layout, or Component_Size is specified for the object.

Implementation Note: Independent addressability is the only high level semantic effect of a pragma Pack. If two objects are independently addressable, the implementation should allocate them in such a way that each can be written by the hardware without writing the other. For example, unless the user asks for it, it is generally not feasible to choose a bit-packed representation on a machine without an atomic bit field insertion instruction, because there might be tasks that update neighboring subcomponents concurrently, and locking operations on all subcomponents is generally not a good idea.

Even if packing or one of the other above-mentioned aspects is specified, subcomponents should still be updated independently if the hardware efficiently supports it. 


#### Dynamic Semantics

[Separate tasks normally proceed independently and concurrently with one another. However, task interactions can be used to synchronize the actions of two or more tasks to allow, for example, meaningful communication by the direct updating and reading of variables shared between the tasks.] The actions of two different tasks are synchronized in this sense when an action of one task signals an action of the other task; an action A1 is defined to signal an action A2 under the following circumstances: 

If A1 and A2 are part of the execution of the same task, and the language rules require A1 to be performed before A2;

If A1 is the action of an activator that initiates the activation of a task, and A2 is part of the execution of the task that is activated;

If A1 is part of the activation of a task, and A2 is the action of waiting for completion of the activation;

If A1 is part of the execution of a task, and A2 is the action of waiting for the termination of the task;

If A1 is the action of issuing an entry call, and A2 is part of the corresponding execution of the appropriate entry_body or accept_statement. 

Ramification: Evaluating the entry_index of an accept_statement is not synchronized with a corresponding entry call, nor is evaluating the entry barrier of an entry_body. 

If A1 is part of the execution of an accept_statement or entry_body, and A2 is the action of returning from the corresponding entry call;

If A1 is part of the execution of a protected procedure body or entry_body for a given protected object, and A2 is part of a later execution of an entry_body for the same protected object; 

Reason: The underlying principle here is that for one action to "signal" a second, the second action has to follow a potentially blocking operation, whose blocking is dependent on the first action in some way. Protected procedures are not potentially blocking, so they can only be "signalers", they cannot be signaled. 

Ramification: Protected subprogram calls are not defined to signal one another, which means that such calls alone cannot be used to synchronize access to shared data outside of a protected object. 

Reason: The point of this distinction is so that on multiprocessors with inconsistent caches, the caches only need to be refreshed at the beginning of an entry body, and forced out at the end of an entry body or protected procedure that leaves an entry open. Protected function calls, and protected subprogram calls for entryless protected objects do not require full cache consistency. Entryless protected objects are intended to be treated roughly like atomic objects - each operation is indivisible with respect to other operations (unless both are reads), but such operations cannot be used to synchronize access to other nonvolatile shared variables. 

If A1 signals some action that in turn signals A2. 

Given an action of assigning to an object, and an action of reading or updating a part of the same object (or of a neighboring object if the two are not independently addressable), then the execution of the actions is erroneous unless the actions are sequential. Two actions are sequential if one of the following is true: 

One action signals the other;

Both actions occur as part of the execution of the same task; 

Reason: Any two actions of the same task are sequential, even if one does not signal the other because they can be executed in an "arbitrary" (but necessarily equivalent to some "sequential") order. 

Both actions occur as part of protected actions on the same protected object, and at most one of the actions is part of a call on a protected function of the protected object. 

Reason: Because actions within protected actions do not always imply signaling, we have to mention them here explicitly to make sure that actions occurring within different protected actions of the same protected object are sequential with respect to one another (unless both are part of calls on protected functions). 

Ramification: It doesn't matter whether or not the variable being assigned is actually a subcomponent of the protected object; globals can be safely updated from within the bodies of protected procedures or entries. 

A pragma Atomic or Atomic_Components may also be used to ensure that certain reads and updates are sequential - see C.6. 

Ramification: If two actions are "sequential" it is known that their executions don't overlap in time, but it is not necessarily specified which occurs first. For example, all actions of a single task are sequential, even though the exact order of execution is not fully specified for all constructs. 

Discussion: Note that if two assignments to the same variable are sequential, but neither signals the other, then the program is not erroneous, but it is not specified which assignment ultimately prevails. Such a situation usually corresponds to a programming mistake, but in some (rare) cases, the order makes no difference, and for this reason this situation is not considered erroneous nor even a bounded error. In Ada 83, this was considered an "incorrect order dependence" if the "effect" of the program was affected, but "effect" was never fully defined. In Ada 95, this situation represents a potential nonportability, and a friendly compiler might want to warn the programmer about the situation, but it is not considered an error. An example where this would come up would be in gathering statistics as part of referencing some information, where the assignments associated with statistics gathering don't need to be ordered since they are just accumulating aggregate counts, sums, products, etc. 


#### Syntax




## 9.11  Example of Tasking and Synchronization


#### Examples

The following example defines a buffer protected object to smooth variations between the speed of output of a producing task and the speed of input of some consuming task. For instance, the producing task might have the following structure:

```ada
task Producer;

```

```ada
task body Producer is
   Char : Character;
begin
   loop
      ... --  produce the next character Char
      Buffer.Write(Char);
      exit when Char = ASCII.EOT;
   end loop;
end Producer;

```

and the consuming task might have the following structure:

```ada
task Consumer;

```

```ada
task body Consumer is
   Char : Character;
begin
   loop
      Buffer.Read(Char);
      exit when Char = ASCII.EOT;
      ... --  consume the character Char
   end loop;
end Consumer;

```

The buffer object contains an internal pool of characters managed in a round-robin fashion. The pool has two indices, an In_Index denoting the space for the next input character and an Out_Index denoting the space for the next output character.

```ada
protected Buffer is
   entry Read (C : out Character);
   entry Write(C : in  Character);
private
   Pool      : String(1 .. 100);
   Count     : Natural := 0;
   In_Index, Out_Index : Positive := 1;
end Buffer;

```

```ada
protected body Buffer is
   entry Write(C : in Character)
      when Count &lt Pool'Length is
   begin
      Pool(In_Index) := C;
      In_Index := (In_Index mod Pool'Length) + 1;
      Count    := Count + 1;
   end Write;

```

```ada
   entry Read(C : out Character)
      when Count &gt 0 is
   begin
      C := Pool(Out_Index);
      Out_Index := (Out_Index mod Pool'Length) + 1;
      Count     := Count - 1;
   end Read;
end Buffer;

```

