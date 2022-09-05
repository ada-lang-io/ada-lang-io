---
sidebar_position:  75
---

# 9.4  Protected Units and Protected Objects

A protected object provides coordinated access to shared data, through calls on its visible protected operations, which can be protected subprograms or protected entries. A protected unit is declared by a protected declaration, which has a corresponding [protected_body](./AA-9.4#S0254). A protected declaration may be a [protected_type_declaration](./AA-9.4#S0249), in which case it declares a named protected type; alternatively, it may be a [single_protected_declaration](./AA-9.4#S0250), in which case it defines an anonymous protected type, as well as declaring a named protected object of that type. 


#### Syntax

{AI95-00345-01} {AI05-0183-1} protected_type_declaration<a id="S0249"></a> ::= 
  protected type [defining_identifier](./AA-3.1#S0022) [[known_discriminant_part](./AA-3.7#S0061)]
        [[aspect_specification](./AA-13.1#S0346)] is
     [new [interface_list](./AA-3.9#S0078) with]
     [protected_definition](./AA-9.4#S0251);

{AI95-00399-01} {AI05-0183-1} single_protected_declaration<a id="S0250"></a> ::= 
  protected [defining_identifier](./AA-3.1#S0022)
        [[aspect_specification](./AA-13.1#S0346)] is
     [new [interface_list](./AA-3.9#S0078) with]
     [protected_definition](./AA-9.4#S0251);

protected_definition<a id="S0251"></a> ::= 
    { [protected_operation_declaration](./AA-9.4#S0252) }
[ private
    { [protected_element_declaration](./AA-9.4#S0253) } ]
  end [protected_[identifier](./AA-2.3#S0002)]

{8652/0009} {AI95-00137-01} protected_operation_declaration<a id="S0252"></a> ::= [subprogram_declaration](./AA-6.1#S0195)
     | [entry_declaration](./AA-9.5#S0257)
     | [aspect_clause](./AA-13.1#S0343)

protected_element_declaration<a id="S0253"></a> ::= [protected_operation_declaration](./AA-9.4#S0252)
     | [component_declaration](./AA-3.8#S0070)

Reason: We allow the operations and components to be mixed because that's how other things work (for example, package declarations). We have relaxed the ordering rules for the items inside [declarative_part](./AA-3.11#S0086)s and [task_definition](./AA-9.1#S0246)s as well. 

{AI05-0267-1} protected_body<a id="S0254"></a> ::= 
  protected body [defining_identifier](./AA-3.1#S0022)
        [[aspect_specification](./AA-13.1#S0346)] is
   { [protected_operation_item](./AA-9.4#S0255) }
  end [protected_[identifier](./AA-2.3#S0002)];

{8652/0009} {AI95-00137-01} {AI12-0147-1} protected_operation_item<a id="S0255"></a> ::= [subprogram_declaration](./AA-6.1#S0195)
     | [subprogram_body](./AA-6.3#S0216)
     | [null_procedure_declaration](./AA-6.7#S0227)
     | [expression_function_declaration](./AA-6.8#S0228)
     | [entry_body](./AA-9.5#S0260)
     | [aspect_clause](./AA-13.1#S0343)

If a protected_[identifier](./AA-2.3#S0002) appears at the end of a [protected_definition](./AA-9.4#S0251) or [protected_body](./AA-9.4#S0254), it shall repeat the [defining_identifier](./AA-3.1#S0022). 

This paragraph was deleted.

Paragraph 10 was deleted. 


#### Static Semantics

{AI95-00345-01} {AI95-00401-01} A [protected_definition](./AA-9.4#S0251) defines a protected type and its first subtype. The list of [protected_operation_declaration](./AA-9.4#S0252)s of a [protected_definition](./AA-9.4#S0251), together with the [known_discriminant_part](./AA-3.7#S0061), if any, is called the visible part of the protected unit. [ The optional list of [protected_element_declaration](./AA-9.4#S0253)s after the reserved word private is called the private part of the protected unit.] 

Proof: {AI05-0299-1} Private part is defined in Clause 8. 

{AI95-00345-01} {AI95-00397-01} {AI95-00399-01} {AI95-00419-01} {AI05-0042-1} For a protected declaration with an [interface_list](./AA-3.9#S0078), the protected type inherits user-defined primitive subprograms from each progenitor type (see 3.9.4), in the same way that a derived type inherits user-defined primitive subprograms from its progenitor types (see 3.4). If the first parameter of a primitive inherited subprogram is of the protected type or an access parameter designating the protected type, and there is a [protected_operation_declaration](./AA-9.4#S0252) for a protected subprogram or single entry with the same identifier within the protected declaration, whose profile is type conformant with the prefixed view profile of the inherited subprogram, the inherited subprogram is said to be implemented by the conforming protected subprogram or entry using an implicitly declared nonabstract subprogram which has the same profile as the inherited subprogram and which overrides it. 

Ramification: The inherited subprograms can only come from an interface given as part of the protected declaration. 

Reason: {AI05-0042-1} The part about the implicitly declared subprogram is needed so that a subprogram implemented by an entry or subprogram is considered to be overridden for the purpose of the other rules of the language. Without it, it would for instance be illegal for an abstract subprogram to be implemented by an entry, because the abstract subprogram would not be overridden. The Legality Rules below ensure that there is no conflict between the implicit overriding subprogram and a user-defined overriding subprogram. 


#### Legality Rules

{AI95-00345-01} A protected declaration requires a completion[, which shall be a [protected_body](./AA-9.4#S0254),] and every [protected_body](./AA-9.4#S0254) shall be the completion of some protected declaration. 

To be honest: {AI05-0229-1} If the implementation supports it, the protected body can be imported (using aspect Import, see B.1), in which case no explicit [protected_body](./AA-9.4#S0254) is allowed. 

{AI95-00345-01} {AI95-00399-01} [Each interface_[subtype_mark](./AA-3.2#S0028) of an [interface_list](./AA-3.9#S0078) appearing within a protected declaration shall denote a limited interface type that is not a task interface.] 

Proof: 3.9.4 requires that an [interface_list](./AA-3.9#S0078) only name interface types, and limits the descendants of the various kinds of interface types. Only a limited, protected, or synchronized interface can have a protected type descendant. Nonlimited or task interfaces are not allowed, as they offer operations that a protected type does not have. 

{AI95-00397-01} {AI05-0042-1} The prefixed view profile of an explicitly declared primitive subprogram of a tagged protected type shall not be type conformant with any protected operation of the protected type, if the subprogram has the same defining name as the protected operation and the first parameter of the subprogram is of the protected type or is an access parameter designating the protected type. 

Reason: This prevents the existence of two operations with the same name and profile which could be called with a prefixed view. If the operation was inherited, this would be illegal by the following rules; this rule puts inherited and noninherited routines on the same footing. Note that this only applies to tagged protected types (that is, those with an interface in their declaration); we do that as there is no problem with prefixed view calls of primitive operations for "normal" protected types, and having this rule apply to all protected types would be incompatible with Ada 95. 

{AI95-00345-01} {AI95-00399-01} For each primitive subprogram inherited by the type declared by a protected declaration, at most one of the following shall apply:

{AI95-00345-01} the inherited subprogram is overridden with a primitive subprogram of the protected type, in which case the overriding subprogram shall be subtype conformant with the inherited subprogram and not abstract; or

{AI95-00345-01} {AI95-00397-01} the inherited subprogram is implemented by a protected subprogram or single entry of the protected type, in which case its prefixed view profile shall be subtype conformant with that of the protected subprogram or entry. 

If neither applies, the inherited subprogram shall be a null procedure. In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit. 

Reason: Each inherited subprogram can only have a single implementation (either from overriding a subprogram, implementing a subprogram, or implementing an entry), and must have an implementation unless the subprogram is a null procedure. 

{AI95-00345-01} {AI05-0291-1} If an inherited subprogram is implemented by a protected procedure or an entry, then the first parameter of the inherited subprogram shall be of mode out or in out, or an access-to-variable parameter. If an inherited subprogram is implemented by a protected function, then the first parameter of the inherited subprogram shall be of mode in, but not an access-to-variable parameter. 

Reason: For a protected procedure or entry, the protected object can be read or written (see 9.5.1). A subprogram that is implemented by a protected procedure or entry must have a profile which reflects that in order to avoid confusion. Similarly, a protected function has a parameter that is a constant, and the inherited routine should reflect that. 

{AI95-00397-01} If a protected subprogram declaration has an [overriding_indicator](./AA-8.3#S0234), then at the point of the declaration:

if the [overriding_indicator](./AA-8.3#S0234) is overriding, then the subprogram shall implement an inherited subprogram;

if the [overriding_indicator](./AA-8.3#S0234) is not overriding, then the subprogram shall not implement any inherited subprogram.

In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit.

Discussion: These rules are subtly different than those for subprograms (see 8.3.1) because there cannot be "late" inheritance of primitives from interfaces. Hidden (that is, private) interfaces are prohibited explicitly (see 7.3), as are hidden primitive operations (as private operations of public abstract types are prohibited - see 3.9.3). 


#### Dynamic Semantics

[The elaboration of a protected declaration elaborates the [protected_definition](./AA-9.4#S0251). The elaboration of a [single_protected_declaration](./AA-9.4#S0250) also creates an object of an (anonymous) protected type.] 

Proof: This is redundant with the general rules for the elaboration of a [full_type_declaration](./AA-3.2#S0024) and an [object_declaration](./AA-3.3#S0032). 

[The elaboration of a [protected_definition](./AA-9.4#S0251) creates the protected type and its first subtype;] it also includes the elaboration of the [component_declaration](./AA-3.8#S0070)s and [protected_operation_declaration](./AA-9.4#S0252)s in the given order.

[As part of the initialization of a protected object, any per-object constraints (see 3.8) are elaborated.] 

Discussion: We do not mention pragmas since each pragma has its own elaboration rules. 

The elaboration of a [protected_body](./AA-9.4#S0254) has no other effect than to establish that protected operations of the type can from then on be called without failing the Elaboration_Check.

The content of an object of a given protected type includes: 

The values of the components of the protected object, including (implicitly) an entry queue for each entry declared for the protected object; 

Ramification: "For each entry" implies one queue for each single entry, plus one for each entry of each entry family. 

A representation of the state of the execution resource associated with the protected object (one such resource is associated with each protected object). 

[The execution resource associated with a protected object has to be acquired to read or update any components of the protected object; it can be acquired (as part of a protected action - see 9.5.1) either for concurrent read-only access, or for exclusive read-write access.]

As the first step of the finalization of a protected object, each call remaining on any entry queue of the object is removed from its queue and Program_Error is raised at the place of the corresponding [entry_call_statement](./AA-9.5#S0264). 

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

{AI05-0005-1} The environment task is queued on PO.Ee when PO is finalized.

In a real example, a server task might park callers on a local protected object for some useful purpose, so we didn't want to disallow this case. 


#### Bounded (Run-Time) Errors

{AI95-00280-01} It is a bounded error to call an entry or subprogram of a protected object after that object is finalized. If the error is detected, Program_Error is raised. Otherwise, the call proceeds normally, which may leave a task queued forever. 

Reason: This is very similar to the finalization rule. It is a bounded error so that an implementation can avoid the overhead of the check if it can ensure that the call still will operate properly. Such an implementation cannot need to return resources (such as locks) to an executive that it needs to execute calls.

This case can happen (and has happened in production code) when a protected object is accessed from the Finalize routine of a type. For example: 

```ada
with Ada.Finalization.Controlled;
package Window_Manager is
    ...
    type Root_Window is new Ada.Finalization.Controlled with private;
    type Any_Window is access all Root_Window;
    ...
private
    ...
    procedure Finalize (Object : in out Root_Window);
    ...
end Window_Manager;

```

```ada
package body Window_Manager is
   protected type Lock is
       entry Get_Lock;
       procedure Free_Lock;
   ...
   end Lock;

```

```ada
   Window_Lock : Lock;

```

```ada
   procedure Finalize (Object : in out Root_Window) is
   begin
       Window_Lock.Get_Lock;
       ...
       Window_Lock.Free_Lock;
   end Finalize;
   ...
   A_Window : Any_Window := new Root_Window;
end Window_Manager;

```

The environment task will call Window_Lock for the object allocated for A_Window when the collection for Any_Window is finalized, which will happen after the finalization of Window_Lock (because finalization of the package body will occur before that of the package specification). 

NOTE 1   {AI95-00382-01} Within the declaration or body of a protected unit other than in an [access_definition](./AA-3.10#S0084), the name of the protected unit denotes the current instance of the unit (see 8.6), rather than the first subtype of the corresponding protected type (and thus the name cannot be used as a [subtype_mark](./AA-3.2#S0028)). 

Discussion: {AI95-00382-01} It can be used as a [subtype_mark](./AA-3.2#S0028) in an anonymous access type. In addition, it is possible to refer to some other subtype of the protected type within its body, presuming such a subtype has been declared between the [protected_type_declaration](./AA-9.4#S0249) and the [protected_body](./AA-9.4#S0254). 

NOTE 2   A [selected_component](./AA-4.1#S0098) can be used to denote a discriminant of a protected object (see 4.1.3). Within a protected unit, the name of a discriminant of the protected type denotes the corresponding discriminant of the current instance of the unit.

NOTE 3   {AI95-00287-01} A protected type is a limited type (see 7.5), and hence precludes use of [assignment_statement](./AA-5.2#S0173)s and predefined equality operators.

NOTE 4   The bodies of the protected operations given in the [protected_body](./AA-9.4#S0254) define the actions that take place upon calls to the protected operations.

NOTE 5   The declarations in the private part are only visible within the private part and the body of the protected unit. 

Reason: [Component_declaration](./AA-3.8#S0070)s are disallowed in a [protected_body](./AA-9.4#S0254) because, for efficiency, we wish to allow the compiler to determine the size of protected objects (when not dynamic); the compiler cannot necessarily see the body. Furthermore, the semantics of initialization of such objects would be problematic - we do not wish to give protected objects complex initialization semantics similar to task activation.

The same applies to [entry_declaration](./AA-9.5#S0257)s, since an entry involves an implicit component - the entry queue. 


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
{AI12-0430-1} protected Shared_Array is
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

{AI05-0299-1} This entire subclause is new; protected units do not exist in Ada 83. 


#### Extensions to Ada 95

{AI95-00345-01} {AI95-00397-01} {AI95-00399-01} {AI95-00401-01} {AI95-00419-01} Protected types and single protected objects can be derived from one or more interfaces. Operations declared in the protected type can implement the primitive operations of an interface. [Overriding_indicator](./AA-8.3#S0234)s can be used to specify whether or not a protected operation implements a primitive operation. 


#### Wording Changes from Ada 95

{8652/0009} {AI95-00137-01} Corrigendum: Changed representation clauses to aspect clauses to reflect that they are used for more than just representation.

{AI95-00280-01} Described what happens when an operation of a finalized protected object is called.

{AI95-00287-01} Revised the note on operations of protected types to reflect that limited types do have an assignment operation, but not copying ([assignment_statement](./AA-5.2#S0173)s).

{AI95-00382-01} Revised the note on use of the name of a protected type within itself to reflect the exception for anonymous access types. 


#### Incompatibilities With Ada 2005

{AI05-0291-1} Correction: When an inherited subprogram is implemented by a protected function, the first parameter has to be an in parameter, but not an access-to-variable type. Original Ada 2005 allowed access-to-variable parameters in this case; the parameter will need to be changed to access-to-constant with the addition of the constant keyword. 


#### Extensions to Ada 2005

{AI05-0183-1} {AI05-0267-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [protected_type_declaration](./AA-9.4#S0249), a [single_protected_declaration](./AA-9.4#S0250), and a [protected_body](./AA-9.4#S0254). This is described in 13.1.1. 


#### Wording Changes from Ada 2005

{AI05-0042-1} Correction: Clarified that an inherited subprogram of a progenitor is overridden when it is implemented by an entry or subprogram.

{AI05-0090-1} Correction: Added the missing defining name in the no conflicting primitive operation rule. 


#### Extensions to Ada 2012

{AI12-0147-1} {AI12-0005-1} Corrigendum: Null procedures and expression functions are allowed in protected bodies. We consider this an omission, as there is no reason why the convenient shorthand notations should not be allowed in this context. 

