---
sidebar_position:  76
---

# 9.5  Intertask Communication

The primary means for intertask communication is provided by calls on entries and protected subprograms. Calls on protected subprograms allow coordinated access to shared data objects. Entry calls allow for blocking the caller until a given condition is satisfied (namely, that the corresponding entry is open - see 9.5.3), and then communicating data or control information directly with another task or indirectly via a shared protected object.


#### Static Semantics

{AI05-0225-1} {AI05-0291-1} When a [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) denotes an entry, protected subprogram, or a prefixed view of a primitive subprogram of a limited interface whose first parameter is a controlling parameter, the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) determines a target object, as follows: 

To be honest: {AI05-0291-1} This wording uses "denotes" to mean "denotes a view of an entity" (when the term is used in Legality Rules), and "denotes an entity" (when the term is used in Dynamic Semantics rules). It does not mean "view of a declaration", as that would not include renames (a renames is not an entry or protected subprogram). 

{AI05-0291-1} If it is a [direct_name](./AA-4.1#S0092) or expanded name that denotes the declaration (or body) of the operation, then the target object is implicitly specified to be the current instance of the task or protected unit immediately enclosing the operation; a call using such a name is defined to be an internal call;

{AI05-0291-1} If it is a [selected_component](./AA-4.1#S0098) that is not an expanded name, then the target object is explicitly specified to be the  object denoted by the [prefix](./AA-4.1#S0093) of the [name](./AA-4.1#S0091); a call using such a name is defined to be an external call; 

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

{AI05-0291-1} If the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) is a dereference (implicit or explicit) of an access-to-protected-subprogram value, then the target object is determined by the [prefix](./AA-4.1#S0093) of the Access [attribute_reference](./AA-4.1#S0100) that produced the access value originally; a call using such a name is defined to be an external call;

If the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) denotes a [subprogram_renaming_declaration](./AA-8.5#S0242), then the target object is as determined by the [name](./AA-4.1#S0091) of the renamed entity.

{AI05-0291-1} A call on an entry or a protected subprogram either uses a [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) that determines a target object implicitly, as above, or is a call on (a non-prefixed view of) a primitive subprogram of a limited interface whose first parameter is a controlling parameter, in which case the target object is identified explicitly by the first parameter. This latter case is an external call.

A corresponding definition of target object applies to a [requeue_statement](./AA-9.5#S0265) (see 9.5.4), with a corresponding distinction between an internal requeue and an external requeue.


#### Legality Rules

{AI95-00345-01} {AI05-0225-1} {AI05-0291-1} If a [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) determines a target object, and the name denotes a protected entry or procedure, then the target object shall be a variable, unless the [prefix](./AA-4.1#S0093) is for an [attribute_reference](./AA-4.1#S0100) to the Count attribute (see 9.9). 

Reason: {AI05-0225-1} The point is to prevent any calls to such a [name](./AA-4.1#S0091) whose target object is a constant view of a protected object, directly, or via an access value, renames, or generic formal subprogram. It is, however, legal to say P'Count in a protected function body, even though the protected object is a constant view there. 

Ramification: {AI05-0291-1} This rule does not apply to calls that are not to a prefixed view. Specifically a "normal" call to a primitive operation of a limited interface is not covered by this rule. In that case, the normal parameter passing mode checks will prevent passing a constant protected object to an operation implemented by a protected entry or procedure as the mode is required to be in out or out. 

{AI12-0166-1} An internal call on a protected function shall not occur within a precondition expression (see 6.1.1) of a protected operation nor within a [default_expression](./AA-3.7#S0063) of a [parameter_specification](./AA-6.1#S0207) of a protected operation.

Reason: {AI125-0166-1} These calls will be made before the start of the protected action, and thus would not be subject to the expected mutual exclusion. As such, they would be an automatic race condition (the state of the called object could change before the start of the protected action for the call on the protected entry or subprogram). 

To be honest: {AI125-0166-1} 6.1.1 actually defines "specific precondition expression" and "class-wide precondition expression". This rule is intended to apply to both. 


#### Dynamic Semantics

Within the body of a protected operation, the current instance (see 8.6) of the immediately enclosing protected unit is determined by the target object specified (implicitly or explicitly) in the call (or requeue) on the protected operation. 

To be honest: The current instance is defined in the same way within the body of a subprogram declared immediately within a [protected_body](./AA-9.4#S0254). 

Any call on a protected procedure or entry of a target protected object is defined to be an update to the object, as is a requeue on such an entry. 

Reason: Read/write access to the components of a protected object is granted while inside the body of a protected procedure or entry. Also, any protected entry call can change the value of the Count attribute, which represents an update. Any protected procedure call can result in servicing the entries, which again might change the value of a Count attribute. 


#### Syntax

{AI05-0030-2} {AI05-0215-1} synchronization_kind<a id="S0256"></a> ::= By_Entry | By_Protected_Procedure | Optional


#### Static Semantics

{AI05-0215-1} For the declaration of a primitive procedure of a synchronized tagged type the following language-defined representation aspect may be specified with an [aspect_specification](./AA-13.1#S0346) (see 13.1.1):

SynchronizationIf specified, the aspect definition shall be a [synchronization_kind](./AA-9.5#S0256).

Aspect Description for Synchronization: Defines whether a given primitive operation of a synchronized interface will be implemented by an entry or protected procedure.

{AI05-0030-2} {AI05-0215-1} Inherited subprograms inherit the Synchronization aspect, if any, from the corresponding subprogram of the parent or progenitor type. If an overriding operation does not have a directly specified Synchronization aspect then the Synchronization aspect of the inherited operation is inherited by the overriding operation. 


#### Legality Rules

{AI05-0030-2} {AI05-0215-1} The [synchronization_kind](./AA-9.5#S0256) By_Protected_Procedure shall not be applied to a primitive procedure of a task interface.

{AI05-0030-2} {AI05-0215-1} A procedure for which the specified [synchronization_kind](./AA-9.5#S0256) is By_Entry shall be implemented by an entry. A procedure for which the specified [synchronization_kind](./AA-9.5#S0256) is By_Protected_Procedure shall be implemented by a protected procedure. A procedure for which the specified [synchronization_kind](./AA-9.5#S0256) is Optional may be implemented by an entry or by a procedure (including a protected procedure).

{AI05-0030-2} {AI05-0215-1} If a primitive procedure overrides an inherited operation for which the Synchronization aspect has been specified to be By_Entry or By_Protected_Procedure, then any specification of the aspect Synchronization applied to the overriding operation shall have the same [synchronization_kind](./AA-9.5#S0256).

{AI05-0030-2} In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit. 


#### Static Semantics

{AI12-0064-2} {AI12-0374-2} For a program unit, task entry, formal package, formal subprogram, formal object of an anonymous access-to-subprogram type, enumeration literal, and for a subtype (including a formal subtype), the following language-defined operational aspect is defined:

NonblockingThis aspect specifies the blocking restriction for the entity; it shall be specified by a static Boolean expression. [The [aspect_definition](./AA-13.1#S0348) can be omitted from the specification of this aspect; in that case, the aspect for the entity is True.]

Aspect Description for Nonblocking: Specifies that an associated subprogram does not block.

Proof: 13.1.1 allows omitting the aspect [expression](./AA-4.4#S0132) for any aspect with type Boolean; we take advantage of that here. 

{AI12-0064-2} The Nonblocking aspect may be specified for all entities for which it is defined, except for protected operations and task entries. In particular, Nonblocking may be specified for generic formal parameters.

Ramification: The Nonblocking aspect cannot be specified for predefined operators or enumeration literals but we don't need to mention that above. One would have to declare a subprogram in order to specify the aspect in those cases, but that defines a user-defined subprogram which is itself not a predefined operator or an enumeration literal. 

{AI12-0064-2} {AI12-0374-2} {AI12-0439-1} } When aspect Nonblocking is False for an entity, the entity can contain a potentially blocking operation; such an entity allows blocking. If the aspect is True for an entity, the entity is said to be nonblocking.

Ramification: Specifying Nonblocking as False imposes no restrictions. Specifying Nonblocking as True imposes additional compile-time checks to prevent blocking, but does not prevent deadlock. A pragma Detect_Blocking can be used to ensure that Program_Error is raised in a deadlock situation. 

{AI12-0064-2} {AI12-0374-2} For a generic instantiation and entities declared within such an instance, the aspect is determined by the Nonblocking aspect for the corresponding entity of the generic unit, anded with the Nonblocking aspects of the actual generic parameters used by the entity. If the aspect is directly specified for an instance, the specified expression shall have the same value as the Nonblocking aspect of the instance (after anding with the aspects of the used actual parameters). In the absence of a Use_Formal aspect, all actual generic parameters are presumed to be used by an entity (see H.7.1).

Reason: We want to allow confirming aspects for instances, but nothing else. The Legality Rules of the generic body were checked assuming the Nonblocking aspect of the generic unit combined with the Nonblocking aspects of the formals where they are used, and if that is overridden on the instance, the instance body might make calls that allow blocking in subprograms that are declared nonblocking. 

{AI12-0064-2} {AI12-0374-2} For a (protected or task) entry, the Nonblocking aspect is False.

Reason: An entry can be renamed as a procedure, so the value of the aspect has to be well-defined (as the attribute can be applied to a procedure). We do not want a nonblocking subprogram to be able to call an entry, no matter how it occurs, so the value ought to be False. Moreover, we do not want a subprogram that renames an entry to be able to override a nonblocking subprogram. We could have used individual rules for these cases, but there were already many of them, and this solution avoids the need for extra rules for entries. 

{AI12-0064-2} {AI12-0374-2} For an enumeration literal, the Nonblocking aspect is True.

Reason: Enumeration literals can be renamed as functions, and passed to generic formal functions, so we need to define the value of the aspect to ensure the other rules are meaningful. 

{AI12-0064-2} {AI12-0374-2} For a predefined operator of an elementary type, the Nonblocking aspect is True. For a predefined operator of a composite type, the Nonblocking aspect of the operator is the same as the Nonblocking aspect for the type.

Reason: Predefined operators of elementary types can never include any potentially blocking operations, so we want them to declare that. Record equality can be composed of operations including user-defined "=" operators, which might allow blocking. Array equality might use some record equality. So we have to have the possibility of allowing blocking for them. We don't just copy the Nonblocking aspect of the type in every case, as someone could declare an elementary type to allow blocking. 

Ramification: It's not possible to specify the nonblocking expression of a predefined operator; if an operator is declared in order to do that, it is no longer predefined. 

{AI12-0064-2} For a dereference of an access-to-subprogram type, the Nonblocking aspect of the designated subprogram is that of the access-to-subprogram type.

{AI12-0374-2} For the base subtype of a scalar (sub)type, the Nonblocking aspect is True.

Reason: The first subtype of a scalar type can allow blocking (which can be useful so a predicate can allow blocking), but the base subtype is always Nonblocking. We need this so the Nonblocking value is well-defined for any subtype that is built from the base subtype (T'Base). T'Base of any scalar type, including a generic formal type, is always nonblocking. 

{AI12-0064-2} For an inherited primitive dispatching subprogram that is null or abstract, the subprogram is nonblocking if and only if a corresponding subprogram of at least one ancestor is nonblocking. For any other inherited subprogram, it is nonblocking if and only if the corresponding subprogram of the parent is nonblocking.

{AI12-0064-2} Unless directly specified, overridings of dispatching operations inherit this aspect.

{AI12-0064-2} {AI12-0374-2} Unless directly specified, for a formal subtype, formal package, or formal subprogram, the Nonblocking aspect is that of the actual subtype, package, or subprogram.

Reason: This means that Nonblocking legality checking for the actual parameters of the instance is only necessary when the aspect is explicitly specified for the formal type. 

{AI12-0064-2} {AI12-0374-2} Unless directly specified, for a non-first subtype S, the Nonblocking aspect is that of the subtype identified in the subtype_indication defining S; unless directly specified for the first subtype of a derived type, the Nonblocking aspect is that of the ancestor subtype.

Discussion: The expressions that can be specified for a such a subtype are limited by a Legality Rule, see below. 

{AI12-0064-2} Unless directly specified, for any other program unit, first  subtype, or formal object, the Nonblocking aspect of the entity is determined by the Nonblocking aspect for the innermost program unit enclosing the entity.

{AI12-0064-2} {AI12-0374-2} If not specified for a library unit, the Nonblocking aspect is True if the library unit is declared pure, or False otherwise.

{AI12-0064-2} {AI12-0247-1} The following are defined to be potentially blocking operations: 

Reason: The primary purpose of these rules is to define what operations are not allowed in a protected operation (blocking is not allowed). Some of these operations are not directly blocking. However, they are still treated as potentially blocking, because allowing them in a protected action might impose an undesirable implementation burden. 

a [select_statement](./AA-9.7#S0269);

an [accept_statement](./AA-9.5#S0258);

an [entry_call_statement](./AA-9.5#S0264), or a call on a procedure that renames or is implemented by an entry;

a [delay_statement](./AA-9.6#S0266);

an [abort_statement](./AA-9.8#S0284);

task creation or activation;

during a protected action, an external call on a protected subprogram (or an external requeue) with the same target object as that of the protected action.

Reason: This is really a deadlocking call, rather than a blocking call, but we include it in this list for simplicity. 

{AI12-0064-2} If a language-defined subprogram allows blocking, then a call on the subprogram is a potentially blocking operation.

Ramification: Calls on other subprograms that allow blocking are not themselves potentially blocking; the execution of the body could execute a potentially blocking operation.

A user-defined instance of a language-defined generic creates user-defined subprograms for the purpose of this rule. A dispatching call to a language-defined abstract subprogram always calls a user-defined concrete subprogram, so that too is not potentially blocking for the purposes of this rule. 


#### Legality Rules

{AI12-0064-2} {AI12-0267-1} {AI12-0374-2} A portion of program text is called a nonblocking region if it is anywhere within a parallel construct, or if the innermost enclosing program unit is nonblocking. A nonblocking region shall not contain any of the following:

a [select_statement](./AA-9.7#S0269);

an [accept_statement](./AA-9.5#S0258);

a [delay_statement](./AA-9.6#S0266);

an [abort_statement](./AA-9.8#S0284);

task creation or activation. 

{AI12-0374-2} Furthermore, a parallel construct shall neither contain a call on a callable entity for which the Nonblocking aspect is False, nor shall it contain a call on a callable entity declared within a generic unit that uses a generic formal parameter with Nonblocking aspect False (see Use_Formal aspect in H.7.1).

{AI12-0374-2} Finally, a nonblocking region that is outside of a parallel construct shall not contain a call on a callable entity for which the Nonblocking aspect is False, unless the region is within a generic unit and the callable entity is associated with a generic formal parameter of the generic unit, or the call is within the [aspect_definition](./AA-13.1#S0348) of an assertion aspect for an entity that allows blocking.

Reason: A generic unit or entity declared within one is presumed to use its "used" generic formal parameters at least once each time it is invoked, and this passes through to the parallel construct check. 

Ramification: Implicit calls for finalization, storage pools, and the like are covered by the above prohibition. The rules above say "a call", not "an explicit call". Such calls are considered statically bound when that is possible, that is, when the controlling object has a known specific type (even if the actual implementation uses dispatching). 

Discussion: We don't need to worry specially about entry calls (even if the entry has been renamed as a procedure), as they will be detected by the prohibition against calls to entities with the Nonblocking aspect False.

Similarly, we don't need to specially worry about subprograms of limited interfaces that are implemented by entries, as any such subprogram necessarily has the value statically False for the Nonblocking aspect, and thus is already covered by the prohibition against calling such subprograms. 

{AI12-0064-2} {AI12-0374-2} For the purposes of the above rules, an [entry_body](./AA-9.5#S0260) is considered nonblocking if the immediately enclosing protected unit is nonblocking.

Reason: An entry declaration always allows blocking (by rule); but we want to be able to compile-time check for most violations of the prohibition against potentially blocking operations in a protected action (see 9.5.1). We do that by using the nonblocking status of the protected unit as the controlling factor, and enforce that by not allowing the specification of the Nonblocking aspect for any protected operation. We can't do this checking unconditionally, as that would be incompatible: existing Ada protected units might call subprograms that allow blocking. Thus a protected unit that allows blocking (which is the default) must allow calling any subprogram from an entry body. 

{AI12-0374-2} For a subtype for which aspect Nonblocking is True, any predicate expression that applies to the subtype shall only contain constructs that are allowed immediately within a nonblocking program unit.

{AI12-0064-2} A subprogram shall be nonblocking if it overrides a nonblocking dispatching operation. An entry shall not implement a nonblocking procedure. If an inherited dispatching subprogram allows blocking, then the corresponding subprogram of each ancestor shall allow blocking.

Discussion: Rules elsewhere in the standard (4.6 and 3.10.2) ensure that access-to-subprogram conversion and the Access attribute enforce nonblocking. 

Ramification: A nonblocking subprogram can override one that allows blocking, but the reverse is illegal. Thus one can declare a Finalize subprogram to be nonblocking, even though it overrides a routine that allows blocking. (This works because a nonblocking subprogram allows a strict subset of the operations allowed in allows blocking subprograms, so calling such a subprogram as if it allows blocking - as is necessary in a dispatching call - is harmless.) 

{AI12-0064-2} {AI12-0374-2} {AI12-0396-1} {AI12-0399-1} It is illegal to directly specify aspect Nonblocking for the first subtype of the full view of a type that has a partial view. If the Nonblocking aspect of the full view is inherited, it shall have the same value as that of the partial view, or have the value True.

Reason: We need completions to agree on nonblocking with the original view. One reason this is necessary to prevent the predefined equality operator from being nonblocking in the partial view and allowing blocking in the full view. 

{AI12-0064-2} {AI12-0374-2} Aspect Nonblocking shall be directly specified for the first subtype of a derived type only if it has the same value as the Nonblocking aspect of the ancestor subtype or if it is specified True. Aspect Nonblocking shall be directly specified for a nonfirst subtype S only if it has the same value as the Nonblocking aspect of the subtype identified in the [subtype_indication](./AA-3.2#S0027) defining S or if it is specified True.

Reason: Boolean-valued aspects have a similar rule to the first rule here (see 13.1.1), we want this one to work similarly. We need non-first subtypes to allow blocking only if the original first subtype allows blocking, as that allows the programmer to know that any operation on any subtype of a type are nonblocking if the first subtype is nonblocking. 

{AI12-0319-1} For an access-to-object type that is nonblocking, the Allocate, Deallocate, and Storage_Size operations on its storage pool shall be nonblocking.

Ramification: Standard storage pools always have nonblocking operations by definition (see 13.11), so this rule only can fail for user-defined storage pools. 

{AI12-0319-1} For a composite type that is nonblocking:

All component subtypes shall be nonblocking;

For a record type or extension, every call in the [default_expression](./AA-3.7#S0063) of a component (including discriminants) shall call an operation that is nonblocking;

For a controlled type, the Initialize, Finalize, and Adjust (if any) subprograms shall be nonblocking.

Reason: These rules ensure that if a type is nonblocking, the default initialization, finalization, and assignment of the type are also nonblocking. This ensures that if a generic formal type is nonblocking, all of the basic operations of the actual type are nonblocking.

Default initialization, finalization, and assignment of elementary types are always nonblocking, so we don't need any rules for those. 

{AI12-0064-2} {AI12-0374-2} The predefined equality operator for a composite type, unless it is for a record type or record extension and the operator is overridden by a primitive equality operator, is illegal if it is nonblocking and:

for a record type or record extension, the parent primitive "=" allows blocking; or

some component is of a type T, and:

T is a record type or record extension that has a primitive "=" that allows blocking; or

T is neither a record type nor a record extension, and T has a predefined "=" that allows blocking. 

Ramification: This applies to both record and array "=".

This check occurs when the equality operator is declared, so this rule effectively makes the type illegal if the rule is violated. 

Reason: We don't need to check this when the operator is overridden for a record type, as the body of the new definition of equality will enforce the rules, and there is no case where the predefined operator will re-emerge. We do have to check this for array types even if the operator is overridden, as the predefined operator will re-emerge in generics and record equality. 

{AI12-0064-2} {AI12-0374-2} In a generic instantiation:

the actual subprogram corresponding to a nonblocking formal subprogram shall be nonblocking [(an actual that is an entry is not permitted in this case)];

the actual subtype corresponding to a nonblocking formal subtype shall be nonblocking;

Ramification: We require matching for formal scalar or access-to-object types, even though their predefined operators are always nonblocking (and they re-emerge in the generic unit) - because a "blocking" predicate might apply to the actual subtype, which will also be enforced on operations of the formal type. 

the actual object corresponding to a formal object of a nonblocking access-to-subprogram type shall be of a nonblocking access-to-subprogram type;

the actual instance corresponding to a nonblocking formal package shall be nonblocking.

{AI12-0064-2} In addition to the places where Legality Rules normally apply (see 12.3), the above rules also apply in the private part of an instance of a generic unit.

Ramification: For a generic formal parameter to be nonblocking (thus, for these rules to apply), it has to explicitly specify aspect Nonblocking to be True. However, if not specified True, these rules do apply in the instance of the specification of the generic unit (the normal re-checking is needed). For instance, the body of an expression function might make a prohibited call. 

NOTE 1   {AI05-0030-2} {AI05-0215-1} The [synchronization_kind](./AA-9.5#S0256) By_Protected_Procedure implies that the operation will not block. 


#### Wording Changes from Ada 95

{AI95-00345-01} Added a Legality Rule to make it crystal-clear that the protected object of an entry or procedure call must be a variable. This rule was implied by the Dynamic Semantics here, along with the Static Semantics of 3.3, but it is much better to explicitly say it. While many implementations have gotten this wrong, this is not an incompatibility - allowing updates of protected constants has always been wrong. 


#### Extensions to Ada 2005

{AI05-0030-2} {AI05-0215-1} Added the Synchronization aspect to allow specifying that an interface procedure is really an entry or a protected procedure. 


#### Wording Changes from Ada 2005

{AI05-0225-1} Correction: Clarified that the target object of any name denoted a protected procedure or entry can never be a constant (other than for the 'Count attribute). This closes holes involving calls to access-to-protected, renaming as a procedure, and generic formal subprograms. 


#### Inconsistencies With Ada 2012

{AI12-0064-2} Calls on procedures that rename an entry or are implemented by an entry are now defined to be potentially blocking. This means that such a call now might raise Program_Error. However, it never made sense for some entry calls to be excluded from being potentially blocking, and we expect that most implementations already treated all entry calls the same way. Thus do not expect this wording change to actually change the behavior of any implementation, and thus no program will change. 


#### Incompatibilities With Ada 2012

{AI12-0166-1} Correction: Internal protected calls are now prohibited in preconditions and default expressions of protected operations. These were allowed in Ada 2012, but as they cause race conditions and as most existing Ada 95 compilers crash when given such a default parameter, we expect such code to be extremely rare. 


#### Extensions to Ada 2012

{AI12-0064-2} {AI12-0319-1} {AI12-0374-2} Aspect Nonblocking is new; it allows compile-time checks to prevent using potentially blocking operations in contexts where that is not allowed. 


## 9.5.1  Protected Subprograms and Protected Actions

A protected subprogram is a subprogram declared immediately within a [protected_definition](./AA-9.4#S0251). Protected procedures provide exclusive read-write access to the data of a protected object; protected functions provide concurrent read-only access to the data. 

Ramification: A subprogram declared immediately within a [protected_body](./AA-9.4#S0254) is not a protected subprogram; it is an intrinsic subprogram. See 6.3.1, "Conformance Rules". 


#### Static Semantics

[Within the body of a protected function (or a function declared immediately within a [protected_body](./AA-9.4#S0254)), the current instance of the enclosing protected unit is defined to be a constant (that is, its subcomponents may be read but not updated). Within the body of a protected procedure (or a procedure declared immediately within a [protected_body](./AA-9.4#S0254)), and within an [entry_body](./AA-9.5#S0260), the current instance is defined to be a variable (updating is permitted).] 

Proof: {AI05-0120-1} All constant views are defined in 3.3, "Objects and Named Numbers", anything not named there is a variable view. 

Ramification: The current instance is like an implicit parameter, of mode in for a protected function, and of mode in out for a protected procedure (or protected entry). 

{AI12-0129-1} For a type declared by a [protected_type_declaration](./AA-9.4#S0249) or for the anonymous type of an object declared by a [single_protected_declaration](./AA-9.4#S0250), the following language-defined type-related representation aspect may be specified:

Exclusive_FunctionsThe type of aspect Exclusive_Functions is Boolean. If not specified (including by inheritance), the aspect is False.

A value of True for this aspect indicates that protected functions behave in the same way as protected procedures with respect to mutual exclusion and queue servicing (see below).

Aspect Description for Exclusive_Functions: Specifies mutual exclusion behavior of protected functions in a protected type.

{AI12-0129-1} A protected procedure or entry is an exclusive protected operation. A protected function of a protected type P is an exclusive protected operation if the Exclusive_Functions aspect of P is True.


#### Dynamic Semantics

For the execution of a call on a protected subprogram, the evaluation of the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) and of the parameter associations, and any assigning back of in out or out parameters, proceeds as for a normal subprogram call (see 6.4). If the call is an internal call (see 9.5), the body of the subprogram is executed as for a normal subprogram call. If the call is an external call, then the body of the subprogram is executed as part of a new protected action on the target protected object; the protected action completes after the body of the subprogram is executed. [A protected action can also be started by an entry call (see 9.5.3).]

{AI12-0129-1} A new protected action is not started on a protected object while another protected action on the same protected object is underway, unless both actions are the result of a call on a nonexclusive protected function. This rule is expressible in terms of the execution resource associated with the protected object: 

{AI12-0129-1} Starting a protected action on a protected object corresponds to acquiring the execution resource associated with the protected object, either for exclusive read-write access if the protected action is for a call on an exclusive protected operation, or for concurrent read-only access otherwise;

Completing the protected action corresponds to releasing the associated execution resource. 

{AI12-0129-1} [After performing an exclusive protected operation on a protected object, but prior to completing the associated protected action, the entry queues (if any) of the protected object are serviced (see 9.5.3).]

{AI12-0119-1} If a parallel construct occurs within a protected action, no new logical threads of control are created. Instead, each element of the parallel construct that would have become a separate logical thread of control executes on the logical thread of control that is performing the protected action. If there are multiple such elements initiated at the same point, they execute in an arbitrary order.

Reason: It would be feasible to allow multiple logical threads of control within a protected action, but it would significantly complicate the definition of "sequential" and "concurrent" actions, since we generally presume that everything occuring within protected actions of a given protected object is sequential. We could simply disallow any use of parallel constructs, but that seems unnecessary, particularly as a parallel construct might be buried within a subprogram that is declared Nonblocking. 


#### Bounded (Run-Time) Errors

{AI12-0064-2} During a protected action, it is a bounded error to invoke an operation that is potentially blocking (see 9.5). 

Paragraphs 9 through 16 were moved to 9.5. 

{AI12-0439-1} If the bounded error is detected, Program_Error is raised. If not detected, the bounded error can result in deadlock or a (nested) protected action on the same target object.

Discussion: {AI95-00305-01} By "nested protected action", we mean that an additional protected action can be started by another task on the same protected object. This means that mutual exclusion may be broken in this bounded error case. A way to ensure that this does not happen is to use pragma Detect_Blocking (see H.5). 

{AI12-0064-2} {AI12-0247-1} During a protected action, a call on a subprogram whose body contains a potentially blocking operation is a bounded error. If the bounded error is detected, Program_Error is raised; otherwise, the call proceeds normally. 

Reason: {AI12-0247-1} This allows an implementation to check and raise Program_Error as soon as a subprogram is called, rather than waiting to find out whether it actually reaches the potentially blocking operation. If the call proceeds normally, reaching the potentially blocking operation is a separate bounded error, covered by the previous rules. 

NOTE 1   {AI12-0276-1} {AI12-0440-1} If two tasks both try to start a protected action on a protected object, and at most one is calling a protected nonexclusive function, then only one of the tasks can proceed. Although the other task cannot proceed, it is not considered blocked, and it can be consuming processing resources while it awaits its turn. Unless there is an admission policy (see D.4.1) in effect, there is no language-defined ordering or queuing presumed for tasks competing to start a protected action - on a multiprocessor such tasks can use busy-waiting; for further monoprocessor and multiprocessor considerations, see D.3, "Priority Ceiling Locking". 

Discussion: The intended implementation on a multi-processor is in terms of "spin locks" - the waiting task will spin. 

NOTE 2   {AI12-0440-1} The body of a protected unit can contain declarations and bodies for local subprograms. These are not visible outside the protected unit.

NOTE 3   The body of a protected function can contain internal calls on other protected functions, but not protected procedures, because the current instance is a constant. On the other hand, the body of a protected procedure can contain internal calls on both protected functions and procedures.

NOTE 4   From within a protected action, an internal call on a protected subprogram, or an external call on a protected subprogram with a different target object is not considered a potentially blocking operation. 

Reason: This is because a task is not considered blocked while attempting to acquire the execution resource associated with a protected object. The acquisition of such a resource is rather considered part of the normal competition for execution resources between the various tasks that are ready. External calls that turn out to be on the same target object are considered potentially blocking, since they can deadlock the task indefinitely. 

NOTE 5   {AI95-00305-01} {AI12-0064-2} {AI12-0440-1} The aspect Nonblocking can be specified True on the definition of a protected unit in order to reject most attempts to use potentially blocking operations within the protected unit (see 9.5). The [pragma](./AA-2.8#S0019) Detect_Blocking can be used to ensure that any remaining executions of potentially blocking operations during a protected action raise Program_Error. See H.5. 

Discussion: {AI12-0064-2} The deadlock case cannot be detected at compile-time, so pragma Detect_Blocking is needed to give it consistent behavior. 


#### Examples

Examples of protected subprogram calls (see 9.4): 

```ada
Shared_Array.Set_Component(N, E);
E := Shared_Array.Component(M);
Control.Release;

```


#### Wording Changes from Ada 95

{AI95-00305-01} Added a note pointing out the existence of [pragma](./AA-2.8#S0019) Detect_Blocking. This pragma can be used to ensure portable (somewhat pessimistic) behavior of protected actions by converting the Bounded Error into a required check. 


#### Extensions to Ada 2012

{AI12-0129-1} Corrigendum: Aspect Exclusive_Functions is new. The term "exclusive protected operations" is new. 


#### Wording Changes from Ada 2012

{AI12-0064-2} Moved the definition of potentially blocking operations to 9.5, so it could be integrated into the definition of the Nonblocking aspect.

{AI12-0247-1} Correction: Added a separate bounded error for a subprogram containing a blocking operation, to keep compatibility with Ada 95 rules without requiring a correct implementation of pragma Detect_Blocking to do full program analysis. 


## 9.5.2  Entries and Accept Statements

[Entry_declaration](./AA-9.5#S0257)s, with the corresponding entry_bodies or [accept_statement](./AA-9.5#S0258)s, are used to define potentially queued operations on tasks and protected objects. 


#### Syntax

{AI95-00397-01} {AI05-0183-1} entry_declaration<a id="S0257"></a> ::= 
   [[overriding_indicator](./AA-8.3#S0234)]
   entry [defining_identifier](./AA-3.1#S0022) [([discrete_subtype_definition](./AA-3.6#S0055))] [parameter_profile](./AA-6.1#S0204)
      [[aspect_specification](./AA-13.1#S0346)];

accept_statement<a id="S0258"></a> ::= 
   accept entry_[direct_name](./AA-4.1#S0092) [([entry_index](./AA-9.5#S0259))] [parameter_profile](./AA-6.1#S0204) [do
     [handled_sequence_of_statements](./AA-11.2#S0304)
   end [entry_[identifier](./AA-2.3#S0002)]];

Reason: We cannot use [defining_identifier](./AA-3.1#S0022) for [accept_statement](./AA-9.5#S0258)s. Although an [accept_statement](./AA-9.5#S0258) is sort of like a body, it can appear nested within a [block_statement](./AA-5.6#S0191), and therefore be hidden from its own entry by an outer homograph. 

entry_index<a id="S0259"></a> ::= [expression](./AA-4.4#S0132)

{AI12-0169-1} entry_body<a id="S0260"></a> ::= 
    entry [defining_identifier](./AA-3.1#S0022) [entry_body_formal_part](./AA-9.5#S0261)
       [[aspect_specification](./AA-13.1#S0346)]
    [entry_barrier](./AA-9.5#S0262) is
       [declarative_part](./AA-3.11#S0086)
    begin
       [handled_sequence_of_statements](./AA-11.2#S0304)
    end [entry_[identifier](./AA-2.3#S0002)];

Discussion: {AI95-00397-01} We don't allow an [overriding_indicator](./AA-8.3#S0234) on an [entry_body](./AA-9.5#S0260) because entries always implement procedures at the point of the type declaration; there is no late implementation. And we don't want to have to think about [overriding_indicator](./AA-8.3#S0234)s on [accept_statement](./AA-9.5#S0258)s. 

entry_body_formal_part<a id="S0261"></a> ::= [([entry_index_specification](./AA-9.5#S0263))] [parameter_profile](./AA-6.1#S0204)

entry_barrier<a id="S0262"></a> ::= when [condition](./AA-4.5#S0150)

{AI12-0398-1} entry_index_specification<a id="S0263"></a> ::= for [defining_identifier](./AA-3.1#S0022) in [discrete_subtype_definition](./AA-3.6#S0055) [[aspect_specification](./AA-13.1#S0346)] 

If an entry_[identifier](./AA-2.3#S0002) appears at the end of an [accept_statement](./AA-9.5#S0258), it shall repeat the entry_[direct_name](./AA-4.1#S0092). If an entry_[identifier](./AA-2.3#S0002) appears at the end of an [entry_body](./AA-9.5#S0260), it shall repeat the [defining_identifier](./AA-3.1#S0022).

[An [entry_declaration](./AA-9.5#S0257) is allowed only in a protected or task declaration.] 

Proof: This follows from the BNF. 

{AI95-00397-01} An [overriding_indicator](./AA-8.3#S0234) is not allowed in an [entry_declaration](./AA-9.5#S0257) that includes a [discrete_subtype_definition](./AA-3.6#S0055). 

Reason: An entry family can never implement something, so allowing an indicator is felt by the majority of the ARG to be redundant. 


#### Name Resolution Rules

In an [accept_statement](./AA-9.5#S0258), the expected profile for the entry_[direct_name](./AA-4.1#S0092) is that of the [entry_declaration](./AA-9.5#S0257); the expected type for an [entry_index](./AA-9.5#S0259) is that of the subtype defined by the [discrete_subtype_definition](./AA-3.6#S0055) of the corresponding [entry_declaration](./AA-9.5#S0257).

Within the [handled_sequence_of_statements](./AA-11.2#S0304) of an [accept_statement](./AA-9.5#S0258), if a [selected_component](./AA-4.1#S0098) has a [prefix](./AA-4.1#S0093) that denotes the corresponding [entry_declaration](./AA-9.5#S0257), then the entity denoted by the [prefix](./AA-4.1#S0093) is the [accept_statement](./AA-9.5#S0258), and the [selected_component](./AA-4.1#S0098) is interpreted as an expanded name (see 4.1.3)[; the [selector_name](./AA-4.1#S0099) of the [selected_component](./AA-4.1#S0098) has to be the [identifier](./AA-2.3#S0002) for some formal parameter of the [accept_statement](./AA-9.5#S0258)]. 

Proof: The only declarations that occur immediately within the declarative region of an [accept_statement](./AA-9.5#S0258) are those for its formal parameters. 


#### Legality Rules

An [entry_declaration](./AA-9.5#S0257) in a task declaration shall not contain a specification for an access parameter (see 3.10). 

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

Implementing the Accessibility_Check inside the [accept_statement](./AA-9.5#S0258) for E is difficult, since one does not know whether the entry caller is calling from inside the immediately enclosing declare block or from outside it. This means that the lexical nesting level associated with the designated object is not sufficient to determine whether the Accessibility_Check should pass or fail.

Note that such problems do not arise with protected entries, because entry_bodies are always nested immediately within the [protected_body](./AA-9.4#S0254); they cannot be further nested as can [accept_statement](./AA-9.5#S0258)s, nor can they be called from within the [protected_body](./AA-9.4#S0254) (since no entry calls are permitted inside a [protected_body](./AA-9.4#S0254)). 

{AI95-00397-01} If an [entry_declaration](./AA-9.5#S0257) has an [overriding_indicator](./AA-8.3#S0234), then at the point of the declaration: 

if the [overriding_indicator](./AA-8.3#S0234) is overriding, then the entry shall implement an inherited subprogram;

if the [overriding_indicator](./AA-8.3#S0234) is not overriding, then the entry shall not implement any inherited subprogram.

In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit.

Discussion: These rules are subtly different than those for subprograms (see 8.3.1) because there cannot be "late" inheritance of primitives from interfaces. Hidden (that is, private) interfaces are prohibited explicitly (see 7.3), as are hidden primitive operations (as private operations of public abstract types are prohibited - see 3.9.3). 

For an [accept_statement](./AA-9.5#S0258), the innermost enclosing body shall be a [task_body](./AA-9.1#S0248), and the entry_[direct_name](./AA-4.1#S0092) shall denote an [entry_declaration](./AA-9.5#S0257) in the corresponding task declaration; the profile of the [accept_statement](./AA-9.5#S0258) shall conform fully to that of the corresponding [entry_declaration](./AA-9.5#S0257). An [accept_statement](./AA-9.5#S0258) shall have a parenthesized [entry_index](./AA-9.5#S0259) if and only if the corresponding [entry_declaration](./AA-9.5#S0257) has a [discrete_subtype_definition](./AA-3.6#S0055).

An [accept_statement](./AA-9.5#S0258) shall not be within another [accept_statement](./AA-9.5#S0258) that corresponds to the same [entry_declaration](./AA-9.5#S0257), nor within an [asynchronous_select](./AA-9.7#S0280) inner to the enclosing [task_body](./AA-9.1#S0248). 

Reason: [Accept_statement](./AA-9.5#S0258)s are required to be immediately within the enclosing [task_body](./AA-9.1#S0248) (as opposed to being in a nested subprogram) to ensure that a nested task does not attempt to accept the entry of its enclosing task. We considered relaxing this restriction, either by making the check a run-time check, or by allowing a nested task to accept an entry of its enclosing task. However, neither change seemed to provide sufficient benefit to justify the additional implementation burden.

Nested [accept_statement](./AA-9.5#S0258)s for the same entry (or entry family) are prohibited to ensure that there is no ambiguity in the resolution of an expanded name for a formal parameter of the entry. This could be relaxed by allowing the inner one to hide the outer one from all visibility, but again the small added benefit didn't seem to justify making the change for Ada 95.

[Accept_statement](./AA-9.5#S0258)s are not permitted within [asynchronous_select](./AA-9.7#S0280) statements to simplify the semantics and implementation: an [accept_statement](./AA-9.5#S0258) in an [abortable_part](./AA-9.7#S0283) could result in Tasking_Error being propagated from an entry call even though the target task was still callable; implementations that use multiple tasks implicitly to implement an [asynchronous_select](./AA-9.7#S0280) might have trouble supporting "up-level" accepts. Furthermore, if [accept_statement](./AA-9.5#S0258)s were permitted in the [abortable_part](./AA-9.7#S0283), a task could call its own entry and then accept it in the [abortable_part](./AA-9.7#S0283), leading to rather unusual and possibly difficult-to-specify semantics. 

An [entry_declaration](./AA-9.5#S0257) of a protected unit requires a completion[, which shall be an [entry_body](./AA-9.5#S0260),] and every [entry_body](./AA-9.5#S0260) shall be the completion of an [entry_declaration](./AA-9.5#S0257) of a protected unit. The profile of the [entry_body](./AA-9.5#S0260) shall conform fully to that of the corresponding declaration. 

Ramification: An [entry_declaration](./AA-9.5#S0257), unlike a [subprogram_declaration](./AA-6.1#S0195), cannot be completed with a [renaming_declaration](./AA-8.5#S0238). 

To be honest: {AI05-0229-1} If the implementation supports it, the entry body can be imported (using aspect Import, see B.1), in which case no explicit [entry_body](./AA-9.5#S0260) is allowed. 

Discussion: The above applies only to protected entries, which are the only ones completed with entry_bodies. Task entries have corresponding [accept_statement](./AA-9.5#S0258)s instead of having entry_bodies, and we do not consider an [accept_statement](./AA-9.5#S0258) to be a "completion," because a task [entry_declaration](./AA-9.5#S0257) is allowed to have zero, one, or more than one corresponding [accept_statement](./AA-9.5#S0258)s. 

An [entry_body_formal_part](./AA-9.5#S0261) shall have an [entry_index_specification](./AA-9.5#S0263) if and only if the corresponding [entry_declaration](./AA-9.5#S0257) has a [discrete_subtype_definition](./AA-3.6#S0055). In this case, the [discrete_subtype_definition](./AA-3.6#S0055)s of the [entry_declaration](./AA-9.5#S0257) and the [entry_index_specification](./AA-9.5#S0263) shall fully conform to one another (see 6.3.1). 

A name that denotes a formal parameter of an [entry_body](./AA-9.5#S0260) is not allowed within the [entry_barrier](./AA-9.5#S0262) of the [entry_body](./AA-9.5#S0260).


#### Static Semantics

The parameter modes defined for parameters in the [parameter_profile](./AA-6.1#S0204) of an [entry_declaration](./AA-9.5#S0257) are the same as for a [subprogram_declaration](./AA-6.1#S0195) and have the same meaning (see 6.2). 

Discussion: Note that access parameters are not allowed for task entries (see above). 

An [entry_declaration](./AA-9.5#S0257) with a [discrete_subtype_definition](./AA-3.6#S0055) (see 3.6) declares a family of distinct entries having the same profile, with one such entry for each value of the entry index subtype defined by the [discrete_subtype_definition](./AA-3.6#S0055). [A name for an entry of a family takes the form of an [indexed_component](./AA-4.1#S0096), where the [prefix](./AA-4.1#S0093) denotes the [entry_declaration](./AA-9.5#S0257) for the family, and the index value identifies the entry within the family.] The term single entry is used to refer to any entry other than an entry of an entry family.

In the [entry_body](./AA-9.5#S0260) for an entry family, the [entry_index_specification](./AA-9.5#S0263) declares a named constant whose subtype is the entry index subtype defined by the corresponding [entry_declaration](./AA-9.5#S0257); the value of the named entry index identifies which entry of the family was called. 

Ramification: The [discrete_subtype_definition](./AA-3.6#S0055) of the [entry_index_specification](./AA-9.5#S0263) is not elaborated; the subtype of the named constant declared is defined by the [discrete_subtype_definition](./AA-3.6#S0055) of the corresponding [entry_declaration](./AA-9.5#S0257), which is elaborated, either when the type is declared, or when the object is created, if its constraint is per-object. 


#### Dynamic Semantics

{8652/0002} {AI95-00171-01} The elaboration of an [entry_declaration](./AA-9.5#S0257) for an entry family consists of the elaboration of the [discrete_subtype_definition](./AA-3.6#S0055), as described in 3.8. The elaboration of an [entry_declaration](./AA-9.5#S0257) for a single entry has no effect. 

Discussion: {AI05-0299-1} The elaboration of the declaration of a protected subprogram has no effect, as specified in subclause 6.1. The default initialization of an object of a task or protected type is covered in 3.3.1. 

[The actions to be performed when an entry is called are specified by the corresponding [accept_statement](./AA-9.5#S0258)s (if any) for an entry of a task unit, and by the corresponding [entry_body](./AA-9.5#S0260) for an entry of a protected unit.]

{AI12-0193-1} The interaction between a task that calls an entry and an accepting task is called a rendezvous.

{AI12-0193-1} For the execution of an [accept_statement](./AA-9.5#S0258), the [entry_index](./AA-9.5#S0259), if any, is first evaluated and converted to the entry index subtype; this index value identifies which entry of the family is to be accepted. Further execution of the [accept_statement](./AA-9.5#S0258) is then blocked until a caller of the corresponding entry is selected (see 9.5.3), whereupon the [handled_sequence_of_statements](./AA-11.2#S0304), if any, of the [accept_statement](./AA-9.5#S0258) is executed, with the formal parameters associated with the corresponding actual parameters of the selected entry call. Execution of the rendezvous consists of the execution of the [handled_sequence_of_statements](./AA-11.2#S0304),  performance of any postcondition or type invariant checks associated with the entry, and any initialization or finalization associated with these checks, as described in 6.1.1 and 7.3.2. After execution of the rendezvous, the [accept_statement](./AA-9.5#S0258) completes and is left. [The two tasks then proceed independently.] When an exception is propagated from the [handled_sequence_of_statements](./AA-11.2#S0304) of an [accept_statement](./AA-9.5#S0258), the same exception is also raised by the execution of the corresponding [entry_call_statement](./AA-9.5#S0264). 

Ramification: This is in addition to propagating it to the construct containing the [accept_statement](./AA-9.5#S0258). In other words, for a rendezvous, the raising splits in two, and continues concurrently in both tasks.

The caller gets a new occurrence; this isn't considered propagation.

Note that we say "propagated from the [handled_sequence_of_statements](./AA-11.2#S0304) of an [accept_statement](./AA-9.5#S0258)", not "propagated from an [accept_statement](./AA-9.5#S0258)". The latter would be wrong - we don't want exceptions propagated by the [entry_index](./AA-9.5#S0259) to be sent to the caller (there is none yet!).

{AI12-0193-1} Execution of the rendezvous does not include any checks associated with parameter copy back or any post-call subtype predicate check for a parameter which is passed by reference. These checks are performed by the caller after the execution of the rendezvous. 

This paragraph was deleted.{AI12-0193-1} 

[An [entry_body](./AA-9.5#S0260) is executed when the [condition](./AA-4.5#S0150) of the [entry_barrier](./AA-9.5#S0262) evaluates to True and a caller of the corresponding single entry, or entry of the corresponding entry family, has been selected (see 9.5.3).] For the execution of the [entry_body](./AA-9.5#S0260), the [declarative_part](./AA-3.11#S0086) of the [entry_body](./AA-9.5#S0260) is elaborated, and the [handled_sequence_of_statements](./AA-11.2#S0304) of the body is executed, as for the execution of a [subprogram_body](./AA-6.3#S0216). The value of the named entry index, if any, is determined by the value of the entry index specified in the entry_[name](./AA-4.1#S0091) of the selected entry call (or intermediate [requeue_statement](./AA-9.5#S0265) - see 9.5.4). 

To be honest: If the entry had been renamed as a subprogram, and the call was a [procedure_call_statement](./AA-6.4#S0217) using the name declared by the renaming, the entry index (if any) comes from the entry [name](./AA-4.1#S0091) specified in the [subprogram_renaming_declaration](./AA-8.5#S0242). 

NOTE 1   A task entry has corresponding accept_statements (zero or more), whereas a protected entry has a corresponding entry_body (exactly one).

NOTE 2   A consequence of the rule regarding the allowed placements of [accept_statement](./AA-9.5#S0258)s is that a task can execute [accept_statement](./AA-9.5#S0258)s only for its own entries.

NOTE 3   {AI95-00318-02} {AI12-0440-1} A return statement (see 6.5) or a [requeue_statement](./AA-9.5#S0265) (see 9.5.4) can be used to complete the execution of an [accept_statement](./AA-9.5#S0258) or an [entry_body](./AA-9.5#S0260). 

Ramification: An [accept_statement](./AA-9.5#S0258) need not have a [handled_sequence_of_statements](./AA-11.2#S0304) even if the corresponding entry has parameters. Equally, it can have a [handled_sequence_of_statements](./AA-11.2#S0304) even if the corresponding entry has no parameters. 

Ramification: A single entry overloads a subprogram, an enumeration literal, or another single entry if they have the same [defining_identifier](./AA-3.1#S0022). Overloading is not allowed for entry family names. A single entry or an entry of an entry family can be renamed as a procedure as explained in 8.5.4. 

NOTE 4   {AI12-0440-1} The [condition](./AA-4.5#S0150) in the [entry_barrier](./AA-9.5#S0262) can reference anything visible except the formal parameters of the entry. This includes the entry index (if any), the components (including discriminants) of the protected object, the Count attribute of an entry of that protected object, and data global to the protected unit.

The restriction against referencing the formal parameters within an [entry_barrier](./AA-9.5#S0262) ensures that all calls of the same entry see the same barrier value. If it is necessary to look at the parameters of an entry call before deciding whether to handle it, the [entry_barrier](./AA-9.5#S0262) can be "when True" and the caller can be requeued (on some private entry) when its parameters indicate that it cannot be handled immediately. 


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

The syntax rule for [entry_body](./AA-9.5#S0260) is new.

[Accept_statement](./AA-9.5#S0258)s can now have [exception_handler](./AA-11.2#S0305)s. 


#### Wording Changes from Ada 95

{8652/0002} {AI95-00171-01} Corrigendum: Clarified the elaboration of per-object constraints.

{AI95-00397-01} [Overriding_indicator](./AA-8.3#S0234)s can be used on entries; this is only useful when a task or protected type inherits from an interface. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in an [entry_declaration](./AA-9.5#S0257). This is described in 13.1.1. 


#### Extensions to Ada 2012

{AI12-0169-1} Correction: An optional [aspect_specification](./AA-13.1#S0346) can be used in an [entry_body](./AA-9.5#S0260). All other kinds of bodies allow (only) implementation-defined aspects, we need to be consistent.

{AI12-0398-1} Named entry indexes now can have an [aspect_specification](./AA-13.1#S0346), allowing the specification of (implementation-defined) aspects for a named entry index. 


#### Wording Changes from Ada 2012

{AI12-0193-1} Correction: Clarified that postcondition and invariant checks are clearly part of the rendezvous for an entry call. 6.1.1 already said this, so the intent was clear and this is not an inconsistency. 


## 9.5.3  Entry Calls

[An [entry_call_statement](./AA-9.5#S0264) (an entry call) can appear in various contexts.] A simple entry call is a stand-alone statement that represents an unconditional call on an entry of a target task or a protected object. [Entry calls can also appear as part of [select_statement](./AA-9.7#S0269)s (see 9.7).] 


#### Syntax

entry_call_statement<a id="S0264"></a> ::= entry_[name](./AA-4.1#S0091) [[actual_parameter_part](./AA-6.4#S0219)];


#### Name Resolution Rules

The entry_[name](./AA-4.1#S0091) given in an [entry_call_statement](./AA-9.5#S0264) shall resolve to denote an entry. The rules for parameter associations are the same as for subprogram calls (see 6.4 and 6.4.1). 


#### Static Semantics

[The entry_[name](./AA-4.1#S0091) of an [entry_call_statement](./AA-9.5#S0264) specifies (explicitly or implicitly) the target object of the call, the entry or entry family, and the entry index, if any (see 9.5).] 


#### Dynamic Semantics

Under certain circumstances (detailed below), an entry of a task or protected object is checked to see whether it is open or closed: 

{AI05-0264-1} An entry of a task is open if the task is blocked on an [accept_statement](./AA-9.5#S0258) that corresponds to the entry (see 9.5.2), or on a [selective_accept](./AA-9.7#S0270) (see 9.7.1) with an open [accept_alternative](./AA-9.7#S0273) that corresponds to the entry; otherwise, it is closed.

{AI05-0264-1} An entry of a protected object is open if the [condition](./AA-4.5#S0150) of the [entry_barrier](./AA-9.5#S0262) of the corresponding [entry_body](./AA-9.5#S0260) evaluates to True; otherwise, it is closed. If the evaluation of the [condition](./AA-4.5#S0150) propagates an exception, the exception Program_Error is propagated to all current callers of all entries of the protected object. 

Reason: An exception during barrier evaluation is considered essentially a fatal error. All current entry callers are notified with a Program_Error. In a fault-tolerant system, a protected object might provide a Reset protected procedure, or equivalent, to support attempts to restore such a "broken" protected object to a reasonable state. 

Discussion: Note that the definition of when a task entry is open is based on the state of the (accepting) task, whereas the "openness" of a protected entry is defined only when it is explicitly checked, since the barrier expression needs to be evaluated. Implementation permissions are given (below) to allow implementations to evaluate the barrier expression more or less often than it is checked, but the basic semantic model presumes it is evaluated at the times when it is checked. 

For the execution of an [entry_call_statement](./AA-9.5#S0264), evaluation of the [name](./AA-4.1#S0091) and of the parameter associations is as for a subprogram call (see 6.4). The entry call is then issued: For a call on an entry of a protected object, a new protected action is started on the object (see 9.5.1). The named entry is checked to see if it is open; if open, the entry call is said to be selected immediately, and the execution of the call proceeds as follows: 

For a call on an open entry of a task, the accepting task becomes ready and continues the execution of the corresponding [accept_statement](./AA-9.5#S0258) (see 9.5.2).

For a call on an open entry of a protected object, the corresponding [entry_body](./AA-9.5#S0260) is executed (see 9.5.2) as part of the protected action. 

If the [accept_statement](./AA-9.5#S0258) or [entry_body](./AA-9.5#S0260) completes other than by a requeue (see 9.5.4), return is made to the caller (after servicing the entry queues - see below); any necessary assigning back of formal to actual parameters occurs, as for a subprogram call (see 6.4.1); such assignments take place outside of any protected action. 

Ramification: The return to the caller will generally not occur until the protected action completes, unless some other thread of control is given the job of completing the protected action and releasing the associated execution resource. 

If the named entry is closed, the entry call is added to an entry queue (as part of the protected action, for a call on a protected entry), and the call remains queued until it is selected or cancelled; there is a separate (logical) entry queue for each entry of a given task or protected object [(including each entry of an entry family)].

When a queued call is selected, it is removed from its entry queue. Selecting a queued call from a particular entry queue is called servicing the entry queue. An entry with queued calls can be serviced under the following circumstances: 

When the associated task reaches a corresponding [accept_statement](./AA-9.5#S0258), or a [selective_accept](./AA-9.7#S0270) with a corresponding open [accept_alternative](./AA-9.7#S0273);

{AI12-0129-1} If after performing, as part of a protected action on the associated protected object, an exclusive protected operation on the object, the entry is checked and found to be open. 

If there is at least one call on a queue corresponding to an open entry, then one such call is selected according to the entry queuing policy in effect (see below), and the corresponding [accept_statement](./AA-9.5#S0258) or [entry_body](./AA-9.5#S0260) is executed as above for an entry call that is selected immediately.

The entry queuing policy controls selection among queued calls both for task and protected entry queues. The default entry queuing policy is to select calls on a given entry queue in order of arrival. If calls from two or more queues are simultaneously eligible for selection, the default entry queuing policy does not specify which queue is serviced first. Other entry queuing policies can be specified by [pragma](./AA-2.8#S0019)s (see D.4).

For a protected object, the above servicing of entry queues continues until there are no open entries with queued calls, at which point the protected action completes. 

Discussion: While servicing the entry queues of a protected object, no new calls can be added to any entry queue of the object, except due to an internal requeue (see 9.5.4). This is because the first step of a call on a protected entry is to start a new protected action, which implies acquiring (for exclusive read-write access) the execution resource associated with the protected object, which cannot be done while another protected action is already in progress. 

For an entry call that is added to a queue, and that is not the [triggering_statement](./AA-9.7#S0282) of an [asynchronous_select](./AA-9.7#S0280) (see 9.7.4), the calling task is blocked until the call is cancelled, or the call is selected and a corresponding [accept_statement](./AA-9.5#S0258) or [entry_body](./AA-9.5#S0260) completes without requeuing. In addition, the calling task is blocked during a rendezvous.

Ramification: For a call on a protected entry, the caller is not blocked if the call is selected immediately, unless a requeue causes the call to be queued.

An attempt can be made to cancel an entry call upon an abort (see 9.8) and as part of certain forms of [select_statement](./AA-9.7#S0269) (see 9.7.2, 9.7.3, and 9.7.4). The cancellation does not take place until a point (if any) when the call is on some entry queue, and not protected from cancellation as part of a requeue (see 9.5.4); at such a point, the call is removed from the entry queue and the call completes due to the cancellation. The cancellation of a call on an entry of a protected object is a protected action[, and as such cannot take place while any other protected action is occurring on the protected object. Like any protected action, it includes servicing of the entry queues (in case some entry barrier depends on a Count attribute).] 

Implementation Note: {AI95-00114-01} In the case of an attempted cancellation due to abort, this removal might have to be performed by the calling task itself if the ceiling priority of the protected object is lower than the priority of the task initiating the abort. 

A call on an entry of a task that has already completed its execution raises the exception Tasking_Error at the point of the call; similarly, this exception is raised at the point of the call if the called task completes its execution or becomes abnormal before accepting the call or completing the rendezvous (see 9.8). This applies equally to a simple entry call and to an entry call as part of a [select_statement](./AA-9.7#S0269).


#### Implementation Permissions

{AI12-0444-1} An implementation may perform the sequence of steps of a protected action using any thread of control; it can be a thread other than that of the task that started the protected action. If an [entry_body](./AA-9.5#S0260) completes without requeuing, then the corresponding calling task may be made ready without waiting for the entire protected action to complete. 

Reason: These permissions are intended to allow flexibility for implementations on multiprocessors. On a monoprocessor, which thread of control executes the protected action is essentially invisible, since the thread is not abortable in any case, and the "current_task" function is not guaranteed to work during a protected action (see C.7.1). 

{AI12-0129-1} {AI12-0444-1} When the entry of a protected object is checked to see whether it is open, the implementation can bypass reevaluating the [condition](./AA-4.5#S0150) of the corresponding [entry_barrier](./AA-9.5#S0262) if no variable or attribute referenced by the [condition](./AA-4.5#S0150) (directly or indirectly) has been altered by the execution (or cancellation) of a call to an exclusive protected operation of the object since the [condition](./AA-4.5#S0150) was last evaluated. 

Ramification: Changes to variables referenced by an entry barrier that result from actions outside of a call to an exclusive protected operation of the protected object need not be "noticed". For example, if a global variable is referenced by an entry barrier, it should not be altered (except as part of a protected action on the object) any time after the barrier is first evaluated. In other words, globals can be used to "parameterize" a protected object, but they cannot reliably be used to control it after the first use of the protected object. 

Implementation Note: Note that even if a global variable is volatile, the implementation need only reevaluate a barrier if the global is updated during a protected action on the protected object. This ensures that an entry-open bit-vector implementation approach is possible, where the bit-vector is computed at the end of a protected action, rather than upon each entry call. 

An implementation may evaluate the [condition](./AA-4.5#S0150)s of all [entry_barrier](./AA-9.5#S0262)s of a given protected object any time any entry of the object is checked to see if it is open. 

Ramification: In other words, any side effects of evaluating an entry barrier should be innocuous, since an entry barrier might be evaluated more or less often than is implied by the "official" dynamic semantics. 

Implementation Note: It is anticipated that when the number of entries is known to be small, all barriers will be evaluated any time one of them needs to be, to produce an "entry-open bit-vector". The appropriate bit will be tested when the entry is called, and only if the bit is false will a check be made to see whether the bit-vector might need to be recomputed. This should allow an implementation to maximize the performance of a call on an open entry, which seems like the most important case.

In addition to the entry-open bit-vector, an "is-valid" bit is needed per object, which indicates whether the current bit-vector setting is valid. A "depends-on-Count-attribute" bit is needed per type. The "is-valid" bit is set to false (as are all the bits of the bit-vector) when the protected object is first created, as well as any time an exception is propagated from computing the bit-vector. Is-valid would also be set false any time the Count is changed and "depends-on-Count-attribute" is true for the type, or a protected procedure or entry returns indicating it might have updated a variable referenced in some barrier.

A single procedure can be compiled to evaluate all of the barriers, set the entry-open bit-vector accordingly, and set the is-valid bit to true. It could have a "when others" handler to set them all false, and call a routine to propagate Program_Error to all queued callers.

For protected types where the number of entries is not known to be small, it makes more sense to evaluate a barrier only when the corresponding entry is checked to see if it is open. It isn't worth saving the state of the entry between checks, because of the space that would be required. Furthermore, the entry queues probably want to take up space only when there is actually a caller on them, so rather than an array of all entry queues, a linked list of nonempty entry queues make the most sense in this case, with the first caller on each entry queue acting as the queue header. 

{AI12-0444-1} When an attempt is made to cancel an entry call, the implementation can use a thread of control other than that of the task (or interrupt) that initiated the cancellation; in particular, it may use the thread of control of the caller itself to attempt the cancellation, even if this can allow the entry call to be selected in the interim. 

Reason: Because cancellation of a protected entry call is a protected action (which helps make the Count attribute of a protected entry meaningful), it might not be practical to attempt the cancellation from the thread of control that initiated the cancellation. For example, if the cancellation is due to the expiration of a delay, it is unlikely that the handler of the timer interrupt could perform the necessary protected action itself (due to being on the interrupt level). Similarly, if the cancellation is due to an abort, it is possible that the task initiating the abort has a priority higher than the ceiling priority of the protected object (for implementations that support ceiling priorities). Similar considerations could apply in a multiprocessor situation. 

NOTE 1   If an exception is raised during the execution of an [entry_body](./AA-9.5#S0260), it is propagated to the corresponding caller (see 11.4).

NOTE 2   For a call on a protected entry, the entry is checked to see if it is open prior to queuing the call, and again thereafter if its Count attribute (see 9.9) is referenced in some entry barrier. 

Ramification: Given this, extra care is required if a reference to the Count attribute of an entry appears in the entry's own barrier. 

Reason: An entry is checked to see if it is open prior to queuing to maximize the performance of a call on an open entry. 

NOTE 3   In addition to simple entry calls, the language permits timed, conditional, and asynchronous entry calls (see 9.7.2, 9.7.3, and see 9.7.4). 

Ramification: A task can call its own entries, but the task will deadlock if the call is a simple entry call. 

NOTE 4   {AI12-0442-1} The [condition](./AA-4.5#S0150) of an [entry_barrier](./AA-9.5#S0262) is allowed to be evaluated by an implementation more often than strictly necessary, even if the evaluation can have side effects. On the other hand, an implementation can avoid reevaluating the [condition](./AA-4.5#S0150) if nothing it references was updated by an intervening protected action on the protected object, even if the [condition](./AA-4.5#S0150) references some global variable that is updated by an action performed from outside of a protected action. 


#### Examples

Examples of entry calls: 

```ada
Agent.Shut_Down;                      --  see 9.1
Parser.Next_Lexeme(E);                --  see 9.1
Pool(5).Read(Next_Char);              --  see 9.1
Controller.Request(Low)(Some_Item);   --  see 9.1
Flags(3).Seize;                       --  see 9.4

```


#### Wording Changes from Ada 2012

{AI12-0129-1} Corrigendum: Revised wording to talk about "exclusive protected operations" (see 9.5.1). 


## 9.5.4  Requeue Statements

[A [requeue_statement](./AA-9.5#S0265) can be used to complete an [accept_statement](./AA-9.5#S0258) or [entry_body](./AA-9.5#S0260), while redirecting the corresponding entry call to a new (or the same) entry queue. Such a requeue can be performed with or without allowing an intermediate cancellation of the call, due to an abort or the expiration of a delay. ]


#### Syntax

{AI05-0030-2} requeue_statement<a id="S0265"></a> ::= requeue procedure_or_entry_[name](./AA-4.1#S0091) [with abort];


#### Name Resolution Rules

{AI05-0030-2} {AI05-0215-1} The procedure_or_entry_[name](./AA-4.1#S0091) of a [requeue_statement](./AA-9.5#S0265) shall resolve to denote a procedure or an entry (the requeue target). The profile of the entry, or the profile or prefixed profile of the procedure, shall either have no parameters, or be type conformant (see 6.3.1) with the profile of the innermost enclosing [entry_body](./AA-9.5#S0260) or [accept_statement](./AA-9.5#S0258). 


#### Legality Rules

A [requeue_statement](./AA-9.5#S0265) shall be within a callable construct that is either an [entry_body](./AA-9.5#S0260) or an [accept_statement](./AA-9.5#S0258), and this construct shall be the innermost enclosing body or callable construct.

{AI05-0030-2} {AI05-0215-1} If the requeue target has parameters, then its (prefixed) profile shall be subtype conformant with the profile of the innermost enclosing callable construct. 

{AI12-0090-1} Given a requeue_statement where the innermost enclosing callable construct is for an entry E1, for every [specific or class-wide ]postcondition expression P1 that applies to E1, there shall exist a postcondition expression P2 that applies to the requeue target E2 such that 

P1 is fully conformant with the expression produced by replacing each reference in P2 to a formal parameter of E2 with a reference to the corresponding formal paramter of E1; and

if P1 is enabled, then P2 is also enabled. 

Discussion: Roughly speaking, the postcondition of the requeue target is required to imply that of the enclosing callable construct. 

{AI12-0090-1} {AI12-0143-1} The requeue target shall not have an applicable specific or class-wide postcondition that includes an Old or Index [attribute_reference](./AA-4.1#S0100).

{AI12-0090-1} If the requeue target is declared immediately within the [task_definition](./AA-9.1#S0246) of a named task type or the [protected_definition](./AA-9.4#S0251) of a named protected type, and if the requeue statement occurs within the body of that type, and if the requeue is an external requeue, then the requeue target shall not have a specific or class-wide postcondition which includes a name denoting either the current instance of that type or any entity declared within the declaration of that type.

Discussion: The above pair of rules always apply; they don't depend on whether or not any of the postconditions are enabled. 

{AI05-0030-2} {AI05-0215-1} {AI12-0090-1} If the target is a procedure, the name shall denote a renaming of an entry, or shall denote a view or a prefixed view of a primitive subprogram of a synchronized interface, where the first parameter of the unprefixed view of the primitive subprogram shall be a controlling parameter, and the Synchronization aspect shall be specified with [synchronization_kind](./AA-9.5#S0256) By_Entry for the primitive subprogram.

{AI05-0030-2} In a [requeue_statement](./AA-9.5#S0265) of an [accept_statement](./AA-9.5#S0258) of some task unit, either the target object shall be a part of a formal parameter of the [accept_statement](./AA-9.5#S0258), or the accessibility level of the target object shall not be equal to or statically deeper than any enclosing [accept_statement](./AA-9.5#S0258) of the task unit. In a [requeue_statement](./AA-9.5#S0265) of an [entry_body](./AA-9.5#S0260) of some protected unit, either the target object shall be a part of a formal parameter of the [entry_body](./AA-9.5#S0260), or the accessibility level of the target object shall not be statically deeper than that of the [entry_declaration](./AA-9.5#S0257) for the [entry_body](./AA-9.5#S0260).

Ramification: In the [entry_body](./AA-9.5#S0260) case, the intent is that the target object can be global, or can be a component of the protected unit, but cannot be a local variable of the [entry_body](./AA-9.5#S0260). 

Reason: These restrictions ensure that the target object of the requeue outlives the completion and finalization of the enclosing callable construct. They also prevent requeuing from a nested [accept_statement](./AA-9.5#S0258) on a parameter of an outer [accept_statement](./AA-9.5#S0258), which could create some strange "long-distance" connections between an entry caller and its server.

Note that in the strange case where a [task_body](./AA-9.1#S0248) is nested inside an [accept_statement](./AA-9.5#S0258), it is permissible to requeue from an [accept_statement](./AA-9.5#S0258) of the inner [task_body](./AA-9.1#S0248) on parameters of the outer [accept_statement](./AA-9.5#S0258). This is not a problem because all calls on the inner task have to complete before returning from the outer [accept_statement](./AA-9.5#S0258), meaning no "dangling calls" will be created. 

Implementation Note: By disallowing certain requeues, we ensure that the normal [terminate_alternative](./AA-9.7#S0275) rules remain sensible, and that explicit clearing of the entry queues of a protected object during finalization is rarely necessary. In particular, such clearing of the entry queues is necessary only (ignoring premature Unchecked_Deallocation) for protected objects declared in a [task_body](./AA-9.1#S0248) (or created by an allocator for an access type declared in such a body) containing one or more [requeue_statement](./AA-9.5#S0265)s. Protected objects declared in subprograms, or at the library level, will never need to have their entry queues explicitly cleared during finalization. 


#### Dynamic Semantics

{AI05-0030-2} {AI12-0090-1} {AI12-0335-1} The execution of a [requeue_statement](./AA-9.5#S0265) begins with the following sequence of steps:

a)The procedure_or_entry_[name](./AA-4.1#S0091) is evaluated. This includes evaluation of the [prefix](./AA-4.1#S0093) (if any) identifying the target task or protected object and of the [expression](./AA-4.4#S0132) (if any) identifying the entry within an entry family.

b){AI12-0335-1} If the target object is not a part of a formal parameter of the innermost enclosing callable construct, a check is made that the accessibility level of the target object is not equal to or deeper than the level of the innermost enclosing callable construct. If this check fails, Program_Error is raised.

c){AI12-0090-1} Precondition checks are performed as for a call to the requeue target.

d)The [entry_body](./AA-9.5#S0260) or [accept_statement](./AA-9.5#S0258) enclosing the [requeue_statement](./AA-9.5#S0265) is then completed[, finalized, and left (see 7.6.1)].

For the execution of a requeue on an entry of a target task, after leaving the enclosing callable construct, the named entry is checked to see if it is open and the requeued call is either selected immediately or queued, as for a normal entry call (see 9.5.3).

For the execution of a requeue on an entry of a target protected object, after leaving the enclosing callable construct: 

if the requeue is an internal requeue (that is, the requeue is back on an entry of the same protected object - see 9.5), the call is added to the queue of the named entry and the ongoing protected action continues (see 9.5.1); 

Ramification: Note that for an internal requeue, the call is queued without checking whether the target entry is open. This is because the entry queues will be serviced before the current protected action completes anyway, and considering the requeued call immediately might allow it to "jump" ahead of existing callers on the same queue. 

if the requeue is an external requeue (that is, the target protected object is not implicitly the same as the current object - see 9.5), a protected action is started on the target object and proceeds as for a normal entry call (see 9.5.3). 

{AI05-0030-2} {AI05-0090-1} If the requeue target named in the [requeue_statement](./AA-9.5#S0265) has formal parameters, then during the execution of the [accept_statement](./AA-9.5#S0258) or [entry_body](./AA-9.5#S0260) corresponding to the new entry and during the checking of any preconditions of the new entry, the formal parameters denote the same objects as did the corresponding formal parameters of the callable construct completed by the requeue. [In any case, no parameters are specified in a [requeue_statement](./AA-9.5#S0265); any parameter passing is implicit.]

If the [requeue_statement](./AA-9.5#S0265) includes the reserved words with abort (it is a requeue-with-abort), then: 

if the original entry call has been aborted (see 9.8), then the requeue acts as an abort completion point for the call, and the call is cancelled and no requeue is performed;

if the original entry call was timed (or conditional), then the original expiration time is the expiration time for the requeued call. 

If the reserved words with abort do not appear, then the call remains protected against cancellation while queued as the result of the [requeue_statement](./AA-9.5#S0265). 

Ramification: This protection against cancellation lasts only until the call completes or a subsequent requeue-with-abort is performed on the call. 

Reason: We chose to protect a requeue, by default, against abort or cancellation. This seemed safer, since it is likely that extra steps need to be taken to allow for possible cancellation once the servicing of an entry call has begun. This also means that in the absence of with abort the usual Ada 83 behavior is preserved, namely that once an entry call is accepted, it cannot be cancelled until it completes. 

NOTE 1   {AI12-0300-1} A requeue is permitted from a single entry to an entry of an entry family, or vice versa. The entry index, if any, plays no part in the subtype conformance check between the profiles of the two entries; an entry index is part of the entry_[name](./AA-4.1#S0091) for an entry of a family. 


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

The [requeue_statement](./AA-9.5#S0265) is new. 


#### Extensions to Ada 2005

{AI05-0030-2} {AI05-0215-1} Added the ability to requeue on operations of synchronized interfaces that are declared to be an entry. 


#### Inconsistencies With Ada 2012

{AI12-0090-1} Corrigendum: We now define that any preconditions of the requeue target are evaluated as part of a [requeue_statement](./AA-9.5#S0265). Original Ada 2012 did not specify this, so a program that requeues when the preconditions fail will raise an exception when none would happen in original Ada 2012. We don't expect this to be a problem, as in that case, the entry body would be called with some of its preconditions evaluating as False; the body is likely to assume that they are true and probably will have failed in some other way anyway.

{AI12-0335-1} Correction: We now include an accessibility check on requeues. This means Program_Error could be raised for a requeue that worked in Ada 2012. This can only fail for an object for which the statically deeper relationship does not apply, for instance a stand-alone object of an anonymous access type. Most programs that are affected are erroneous anyway (as they will eventually use a nonexistent object), so we do not believe this will matter in practice. 


#### Incompatibilities With Ada 2012

{AI12-0090-1} Corrigendum: If a requeue target has a different postcondition than the original entry, the requeue is now illegal. In such a case, the original postcondition would never have been evaluated, and assumptions that the caller relied upon might not be true. A requeue should be invisible to the caller with respect to any postconditions; thus we only allow it when the original entry has no postconditions or the requeue target has (at least) the same postconditions. 


#### Wording Changes from Ada 2012

{AI12-0143-1} Added a Legality Rule for the new Index attribute (see 6.1.1). 

