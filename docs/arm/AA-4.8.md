---
sidebar_position:  36
---

# 4.8  Allocators

[The evaluation of an [allocator](./AA-4.8#S0164) creates an object and yields an access value that designates the object. ]


#### Syntax

{AI05-0111-3} allocator<a id="S0164"></a> ::= 
   new [[subpool_specification](./AA-4.8#S0165)] [subtype_indication](./AA-3.2#S0027)
 | new [[subpool_specification](./AA-4.8#S0165)] [qualified_expression](./AA-4.7#S0163)

{AI05-0111-3} subpool_specification<a id="S0165"></a> ::= (subpool_handle_[name](./AA-4.1#S0091))

{AI05-0104-1} For an [allocator](./AA-4.8#S0164) with a [subtype_indication](./AA-3.2#S0027), the [subtype_indication](./AA-3.2#S0027) shall not specify a [null_exclusion](./AA-3.10#S0083).

Reason: Such an uninitialized [allocator](./AA-4.8#S0164) would necessarily raise Constraint_Error, as the default value is null. Also note that the syntax does not allow a [null_exclusion](./AA-3.10#S0083) in an initialized [allocator](./AA-4.8#S0164), so it makes sense to make the uninitialized case illegal as well. 


#### Name Resolution Rules

{8652/0010} {AI95-00127-01} {AI05-0111-3} {AI05-0269-1} The expected type for an [allocator](./AA-4.8#S0164) shall be a single access-to-object type with designated type D such that either D covers the type determined by the [subtype_mark](./AA-3.2#S0028) of the [subtype_indication](./AA-3.2#S0027) or [qualified_expression](./AA-4.7#S0163), or the expected type is anonymous and the determined type is D'Class. A subpool_handle_[name](./AA-4.1#S0091) is expected to be of any type descended from Subpool_Handle, which is the type used to identify a subpool, declared in package System.Storage_Pools.Subpools (see 13.11.4). 

Discussion: See 8.6, "The Context of Overload Resolution" for the meaning of "shall be a single ... type whose ...". 

Ramification: {8652/0010} {AI95-00127-01} An [allocator](./AA-4.8#S0164) is allowed as a controlling parameter of a dispatching call (see 3.9.2). 


#### Legality Rules

An initialized allocator is an [allocator](./AA-4.8#S0164) with a [qualified_expression](./AA-4.7#S0163). An uninitialized allocator is one with a [subtype_indication](./AA-3.2#S0027). In the [subtype_indication](./AA-3.2#S0027) of an uninitialized allocator, a [constraint](./AA-3.2#S0029) is permitted only if the [subtype_mark](./AA-3.2#S0028) denotes an [unconstrained] composite subtype; if there is no [constraint](./AA-3.2#S0029), then the [subtype_mark](./AA-3.2#S0028) shall denote a definite subtype. 

Ramification: For example, ... new S'Class ... (with no initialization expression) is illegal, but ... new S'Class'(X) ... is legal, and takes its tag and constraints from the initial value X. (Note that the former case cannot have a constraint.) 

{AI95-00287-01} If the type of the [allocator](./AA-4.8#S0164) is an access-to-constant type, the [allocator](./AA-4.8#S0164) shall be an initialized allocator. 

This paragraph was deleted.{AI95-00287-01} 

{AI05-0111-3} If a [subpool_specification](./AA-4.8#S0165) is given, the type of the storage pool of the access type shall be a descendant of Root_Storage_Pool_With_Subpools.

{AI95-00344-01} If the designated type of the type of the [allocator](./AA-4.8#S0164) is class-wide, the accessibility level of the type determined by the [subtype_indication](./AA-3.2#S0027) or [qualified_expression](./AA-4.7#S0163) shall not be statically deeper than that of the type of the [allocator](./AA-4.8#S0164). 

Reason: This prevents the allocated object from outliving its type. 

{AI95-00416-01} {AI05-0051-1} If the subtype determined by the [subtype_indication](./AA-3.2#S0027) or [qualified_expression](./AA-4.7#S0163) of the [allocator](./AA-4.8#S0164) has one or more access discriminants, then the accessibility level of the anonymous access type of each access discriminant shall not be statically deeper than that of the type of the [allocator](./AA-4.8#S0164) (see 3.10.2). 

Reason: This prevents the allocated object from outliving its discriminants. 

{AI95-00366-01} {AI05-0052-1} {AI05-0157-1} An [allocator](./AA-4.8#S0164) shall not be of an access type for which the Storage_Size has been specified by a static expression with value zero or is defined by the language to be zero. 

Reason: An [allocator](./AA-4.8#S0164) for an access type that has Storage_Size specified to be zero is required to raise Storage_Error anyway. It's better to detect the error at compile-time, as the [allocator](./AA-4.8#S0164) might be executed infrequently. This also simplifies the rules for Pure units, where we do not want to allow any allocators for library-level access types, as they would represent state.

{AI05-0157-1} We don't need a special rule to cover generic formals (unlike many other similar Legality Rules). There are only two cases of interest. For formal access types, the Storage_Size property is not known in the generic, and surely isn't static, so this Legality Rule can never apply. For a formal derived type, this Legality Rule can only be triggered by a parent type having one of the appropriate properties. But Storage_Size can never be specified for a derived access type, so it always has the same value for all child types; additionally, a type derived from a remote access type (which has Storage_Size defined to be zero) is also a remote access type. That means that any actual that would match the formal derived type necessarily has the same Storage_Size properties, so it is harmless (and preferable) to check them in the body - they are always known in that case. For other formal types,[allocator](./AA-4.8#S0164)s are not allowed, so we don't need to consider them. So we don't need an assume-the-best rule here. 

{AI05-0052-1} If the designated type of the type of the [allocator](./AA-4.8#S0164) is limited, then the [allocator](./AA-4.8#S0164) shall not be used to define the value of an access discriminant, unless the discriminated type is immutably limited (see 7.5).

Reason: Because coextensions work very much like parts, we don't want users creating limited coextensions for nonlimited types. This would be similar to extending a nonlimited type with a limited component. We check this on the [allocator](./AA-4.8#S0164). Note that there is an asymmetry in what types are considered limited; this is required to preserve privacy. We have to assume that the designated type might be limited as soon as we see a limited partial view, but we want to ensure that the containing object is of a type that is always limited. 

{AI05-0052-1} In addition to the places where Legality Rules normally apply (see 12.3), these rules apply also in the private part of an instance of a generic unit. 

Discussion: This applies to all of the Legality Rules of this subclause. 


#### Static Semantics

{AI95-00363-01} {AI05-0041-1} If the designated type of the type of the [allocator](./AA-4.8#S0164) is elementary, then the subtype of the created object is the designated subtype. If the designated type is composite, then the subtype of the created object is the designated subtype when the designated subtype is constrained or there is an ancestor of the designated type that has a constrained partial view; otherwise, the created object is constrained by its initial value [(even if the designated subtype is unconstrained with defaults)]. 

Discussion: See AI83-00331. 

Reason: {AI95-00363-01} All objects created by an [allocator](./AA-4.8#S0164) are aliased, and most aliased composite objects need to be constrained so that access subtypes work reasonably. Problematic access subtypes are prohibited for types with a constrained partial view. 

Discussion: {AI95-00363-01} If there is a constrained partial view of the type, this allows the objects to be unconstrained. This eliminates privacy breaking (we don't want the objects to act differently simply because they're allocated). Such a created object is effectively constrained by its initial value if the access type is an access-to-constant type, or the designated type is limited (in all views), but we don't need to state that here. It is implicit in other rules. Note, however, that a value of an access-to-constant type can designate a variable object via 'Access or conversion, and the variable object might be assigned by some other access path, and that assignment might alter the discriminants. 


#### Dynamic Semantics

{AI95-00373-01} For the evaluation of an initialized allocator, the evaluation of the [qualified_expression](./AA-4.7#S0163) is performed first. An object of the designated type is created and the value of the [qualified_expression](./AA-4.7#S0163) is converted to the designated subtype and assigned to the object. 

Ramification: The conversion might raise Constraint_Error. 

For the evaluation of an uninitialized allocator, the elaboration of the [subtype_indication](./AA-3.2#S0027) is performed first. Then: 

{AI95-00373-01} If the designated type is elementary, an object of the designated subtype is created and any implicit initial value is assigned;

{8652/0002} {AI95-00171-01} {AI95-00373-01} If the designated type is composite, an object of the designated type is created with tag, if any, determined by the [subtype_mark](./AA-3.2#S0028) of the [subtype_indication](./AA-3.2#S0027). This object is then initialized by default (see 3.3.1) using the [subtype_indication](./AA-3.2#S0027) to determine its nominal subtype. A check is made that the value of the object belongs to the designated subtype. Constraint_Error is raised if this check fails. This check and the initialization of the object are performed in an arbitrary order.

Discussion: AI83-00150. 

{AI95-00344-01} {AI95-00416-01} {AI05-0024-1} {AI05-0051-1} {AI05-0234-1} For any [allocator](./AA-4.8#S0164), if the designated type of the type of the [allocator](./AA-4.8#S0164) is class-wide, then a check is made that the master of the type determined by the [subtype_indication](./AA-3.2#S0027), or by the tag of the value of the [qualified_expression](./AA-4.7#S0163), includes the elaboration of the type of the [allocator](./AA-4.8#S0164). If any part of the subtype determined by the [subtype_indication](./AA-3.2#S0027) or [qualified_expression](./AA-4.7#S0163) of the [allocator](./AA-4.8#S0164) (or by the tag of the value if the type of the [qualified_expression](./AA-4.7#S0163) is class-wide) has one or more access discriminants, then a check is made that the accessibility level of the anonymous access type of each access discriminant is not deeper than that of the type of the [allocator](./AA-4.8#S0164). Program_Error is raised if either such check fails. 

Reason: {AI95-00344-01} {AI05-0024-1} The master check on class-wide types prevents the allocated object from outliving its type. We need the run-time check in instance bodies, or when the type of the [qualified_expression](./AA-4.7#S0163) is class-wide (other cases are statically detected).

{AI05-0024-1} We can't use the normal accessibility level "deeper than" check here because we may have "incomparable" levels if the appropriate master and the type declaration belong to two different tasks. This can happen when checking the master of the tag for an allocator initialized by a parameter passed in to an accept statement, if the type of the allocator is an access type declared in the enclosing task body. For example:

```ada
task body TT is
   type Acc_TC is access T'Class;
   P : Acc_TC;
begin
   accept E(X : T'Class) do
      P := new T'Class'(X);
         --  Master check on tag of X.
         --  Can't use "accessibility levels" since they might be incomparable.
         --  Must revert to checking that the master of the type identified by
         --  X'tag includes the elaboration of Acc_TC, so it is sure to outlive it.
   end E;

```

{AI95-00416-01} The accessibility check on access discriminants prevents the allocated object from outliving its discriminants.

{AI95-00280-01} If the object to be created by an [allocator](./AA-4.8#S0164) has a controlled or protected part, and the finalization of the collection of the type of the [allocator](./AA-4.8#S0164) (see 7.6.1) has started, Program_Error is raised. 

Reason: If the object has a controlled or protected part, its finalization is likely to be nontrivial. If the allocation was allowed, we could not know whether the finalization would actually be performed. That would be dangerous to otherwise safe abstractions, so we mandate a check here. On the other hand, if the finalization of the object will be trivial, we do not require (but allow) the check, as no real harm could come from late allocation. 

Discussion: This check can only fail if an [allocator](./AA-4.8#S0164) is evaluated in code reached from a Finalize routine for a type declared in the same master. That's highly unlikely; Finalize routines are much more likely to be deallocating objects than allocating them. 

{AI95-00280-01} If the object to be created by an [allocator](./AA-4.8#S0164) contains any tasks, and the master of the type of the [allocator](./AA-4.8#S0164) is completed, and all of the dependent tasks of the master are terminated (see 9.3), then Program_Error is raised. 

Reason: A task created after waiting for tasks has finished could depend on freed data structures, and certainly would never be awaited. 

{AI05-0111-3} If the [allocator](./AA-4.8#S0164) includes a subpool_handle_[name](./AA-4.1#S0091), Constraint_Error is raised if the subpool handle is null. Program_Error is raised if the subpool does not belong (see 13.11.4) to the storage pool of the access type of the [allocator](./AA-4.8#S0164). 

Implementation Note: This can be implemented by comparing the result of Pool_of_Subpool to a reference to the storage pool object. Pool_of_Subpool's parameter is not null, so the check for null falls out naturally. 

Reason: This detects cases where the subpool belongs to another pool, or to no pool at all. This includes detecting dangling subpool handles so long as the subpool object (the object designated by the handle) still exists. (If the subpool object has been deallocated, execution is erroneous; it is likely that this check will still detect the problem, but there cannot be a guarantee.) 

[If the created object contains any tasks, they are activated (see 9.2).] Finally, an access value that designates the created object is returned. 


#### Bounded (Run-Time) Errors

{AI95-00280-01}  It is a bounded error if the finalization of the collection of the type (see 7.6.1) of the [allocator](./AA-4.8#S0164) has started. If the error is detected, Program_Error is raised. Otherwise, the allocation proceeds normally. 

Discussion: This check is required in some cases; see above. 

NOTE 1   Allocators cannot create objects of an abstract type. See 3.9.3.

NOTE 2   If any part of the created object is controlled, the initialization includes calls on corresponding Initialize or Adjust procedures. See 7.6.

NOTE 3   {AI12-0440-1} As explained in 13.11, "Storage Management", the storage for an object allocated by an [allocator](./AA-4.8#S0164) comes from a storage pool (possibly user defined). The exception Storage_Error is raised by an [allocator](./AA-4.8#S0164) if there is not enough storage. Instances of Unchecked_Deallocation can be used to explicitly reclaim storage.

NOTE 4   {AI05-0229-1} {AI12-0442-1} Implementations can, if desired, provide garbage collection. 

Ramification: Note that in an [allocator](./AA-4.8#S0164), the exception Constraint_Error can be raised by the evaluation of the [qualified_expression](./AA-4.7#S0163), by the elaboration of the [subtype_indication](./AA-3.2#S0027), or by the initialization. 

Discussion: By default, the implementation provides the storage pool. The user may exercise more control over storage management by associating a user-defined pool with an access type. 


#### Examples

Examples of allocators: 

```ada
new Cell'(0, null, null)                          -- initialized explicitly, see 3.10.1
new Cell'(Value =&gt 0, Succ =&gt null, Pred =&gt null) -- initialized explicitly
new Cell                                          -- not initialized

```

```ada
new Matrix(1 .. 10, 1 .. 20)                      -- the bounds only are given
new Matrix'(1 .. 10 =&gt (1 .. 20 =&gt 0.0))          -- initialized explicitly

```

```ada
new Buffer(100)                                   -- the discriminant only is given
new Buffer'(Size =&gt 80, Pos =&gt 0, Value =&gt (1 .. 80 =&gt 'A')) -- initialized explicitly

```

```ada
Expr_Ptr'(new Literal)                  -- allocator for access-to-class-wide type, see 3.9.1
Expr_Ptr'(new Literal'(Expression with 3.5))      -- initialized explicitly

```


#### Incompatibilities With Ada 83

The [subtype_indication](./AA-3.2#S0027) of an uninitialized allocator may not have an explicit [constraint](./AA-3.2#S0029) if the designated type is an access type. In Ada 83, this was permitted even though the [constraint](./AA-3.2#S0029) had no effect on the subtype of the created object. 


#### Extensions to Ada 83

Allocators creating objects of type T are now overloaded on access types designating T'Class and all class-wide types that cover T.

Implicit array subtype conversion (sliding) is now performed as part of an initialized allocator. 


#### Wording Changes from Ada 83

We have used a new organization, inspired by the ACID document, that makes it clearer what is the subtype of the created object, and what subtype conversions take place.

Discussion of storage management issues, such as garbage collection and the raising of Storage_Error, has been moved to 13.11, "Storage Management". 


#### Inconsistencies With Ada 95

{AI95-00363-01} If the designated type has a constrained partial view, the allocated object can be unconstrained. This might cause the object to take up a different amount of memory, and might cause the operations to work where they previously would have raised Constraint_Error. It's unlikely that the latter would actually matter in a real program (Constraint_Error usually indicates a bug that would be fixed, not left in a program.) The former might cause Storage_Error to be raised at a different time than in an Ada 95 program. 


#### Incompatibilities With Ada 95

{AI95-00366-01} An [allocator](./AA-4.8#S0164) for an access type that has Storage_Size specified to be zero is now illegal. Ada 95 allowed the [allocator](./AA-4.8#S0164), but it had to raise Storage_Error if executed. The primary impact of this change should be to detect bugs. 


#### Extensions to Ada 95

{8652/0010} {AI95-00127-01} Corrigendum: An [allocator](./AA-4.8#S0164) can be a controlling parameter of a dispatching call. This was an oversight in Ada 95.

{AI95-00287-01} Initialized [allocator](./AA-4.8#S0164)s are allowed when the designated type is limited. 


#### Wording Changes from Ada 95

{8652/0002} {AI95-00171-01} Corrigendum: Clarified the elaboration of per-object constraints for an uninitialized allocator.

{AI95-00280-01} Program_Error is now raised if the [allocator](./AA-4.8#S0164) occurs after the finalization of the collection or the waiting for tasks. This is not listed as an incompatibility as the Ada 95 behavior was unspecified, and Ada 95 implementations tend to generate programs that crash in this case.

{AI95-00344-01} Added accessibility checks to class-wide [allocator](./AA-4.8#S0164)s. These checks could not fail in Ada 95 (as all of the designated types had to be declared at the same level, so the access type would necessarily have been at the same level or more nested than the type of allocated object).

{AI95-00373-01} Revised the description of evaluation of uninitialized allocators to use "initialized by default" so that the ordering requirements are the same for all kinds of objects that are default-initialized.

{AI95-00416-01} Added accessibility checks to access discriminants of [allocator](./AA-4.8#S0164)s. These checks could not fail in Ada 95 as the discriminants always have the accessibility of the object. 


#### Incompatibilities With Ada 2005

{AI05-0052-1} Correction: Added a rule to prevent limited coextensions of nonlimited types. Allowing this would have far-reaching implementation costs. Because of those costs, it seems unlikely that any implementation ever supported it properly and thus it is unlikely that any existing code depends on this capability.

{AI05-0104-1} Correction: Added a rule to make [null_exclusion](./AA-3.10#S0083)s illegal for uninitialized [allocator](./AA-4.8#S0164)s, as such an [allocator](./AA-4.8#S0164) would always raise Constraint_Error. Programs that depend on the unconditional raising of a predefined exception should be very rare. 


#### Extensions to Ada 2005

{AI05-0111-3} Subpool handles (see 13.11.4) can be specified in an [allocator](./AA-4.8#S0164). 


#### Wording Changes from Ada 2005

{AI05-0024-1} Correction: Corrected the master check for tags since the masters may be for different tasks and thus incomparable.

{AI05-0041-1} Correction: Corrected the rules for when a designated object is constrained by its initial value so that types derived from a partial view are handled properly.

{AI05-0051-1} {AI05-0234-1} Correction: Corrected the accessibility check for access discriminants so that it does not depend on the designated type (which might not have discriminants when the allocated type does). 

