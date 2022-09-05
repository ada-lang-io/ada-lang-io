---
sidebar_position:  63
---

# 7.6  Assignment and Finalization

[ Three kinds of actions are fundamental to the manipulation of objects: initialization, finalization, and assignment. Every object is initialized, either explicitly or by default, after being created (for example, by an [object_declaration](./AA-3.3#S0032) or [allocator](./AA-4.8#S0164)). Every object is finalized before being destroyed (for example, by leaving a [subprogram_body](./AA-6.3#S0216) containing an [object_declaration](./AA-3.3#S0032), or by a call to an instance of Unchecked_Deallocation). An assignment operation is used as part of [assignment_statement](./AA-5.2#S0173)s, explicit initialization, parameter passing, and other operations. 

Default definitions for these three fundamental operations are provided by the language, but a controlled type gives the user additional control over parts of these operations. In particular, the user can define, for a controlled type, an Initialize procedure which is invoked immediately after the normal default initialization of a controlled object, a Finalize procedure which is invoked immediately before finalization of any of the components of a controlled object, and an Adjust procedure which is invoked as the last step of an assignment to a (nonlimited) controlled object.] 

Glossary entry: A controlled type supports user-defined assignment and finalization. Objects are always finalized before being destroyed.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[controlled type], Def=[a type that supports user-defined assignment and finalization], Note1=[Objects are always finalized before being destroyed.] 

Ramification: {AI95-00114-01} {AI95-00287-01} Here's the basic idea of initialization, value adjustment, and finalization, whether or not user defined: When an object is created, if it is explicitly assigned an initial value, the object is either built-in-place from an [aggregate](./AA-4.3#S0106) or function call (in which case neither Adjust nor Initialize is applied), or the assignment copies and adjusts the initial value. Otherwise, Initialize is applied to it (except in the case of an [aggregate](./AA-4.3#S0106) as a whole). An [assignment_statement](./AA-5.2#S0173) finalizes the target before copying in and adjusting the new value. Whenever an object goes away, it is finalized. Calls on Initialize and Adjust happen bottom-up; that is, components first, followed by the containing object. Calls on Finalize happen top-down; that is, first the containing object, and then its components. These ordering rules ensure that any components will be in a well-defined state when Initialize, Adjust, or Finalize is applied to the containing object. 


#### Static Semantics

The following language-defined library package exists: 

```ada
{8652/0020} {AI95-00126-01} {AI05-0212-1} {AI12-0241-1} package Ada.Finalization
    with Pure, Nonblocking =&gt False is

```

```ada
{AI95-00161-01} {AI12-0399-1}     type Controlled is abstract tagged private
       with Preelaborable_Initialization;

```

```ada
{AI95-00348-01}     procedure Initialize (Object : in out Controlled) is null;
    procedure Adjust     (Object : in out Controlled) is null;
    procedure Finalize   (Object : in out Controlled) is null;

```

```ada
{AI95-00161-01} {AI12-0399-1}     type Limited_Controlled is abstract tagged limited private
       with Preelaborable_Initialization;

```

```ada
{AI95-00348-01}     procedure Initialize (Object : in out Limited_Controlled) is null;
    procedure Finalize   (Object : in out Limited_Controlled) is null;
private
    ... -- not specified by the language
end Ada.Finalization;

```

Reason: {AI12-0241-1} This package must allow blocking (Nonblocking =&gt False) for compatibility. The purpose of this package is to provide a template for overriding user-defined routines; and such routines can only allow blocking if the root type does so. Users can still declare their overridding routines nonblocking if they wish. 

{AI95-00348-01} A controlled type is a descendant of Controlled or Limited_Controlled. The predefined "=" operator of type Controlled always returns True, [since this operator is incorporated into the implementation of the predefined equality operator of types derived from Controlled, as explained in 4.5.2.] The type Limited_Controlled is like Controlled, except that it is limited and it lacks the primitive subprogram Adjust. 

Discussion: We say "nonlimited controlled type" (rather than just "controlled type";) when we want to talk about descendants of Controlled only. 

Reason: We considered making Adjust and Finalize abstract. However, a reasonable coding convention is e.g. for Finalize to always call the parent's Finalize after doing whatever work is needed for the extension part. (Unlike CLOS, we have no way to do that automatically in Ada 95.) For this to work, Finalize cannot be abstract. In a generic unit, for a generic formal abstract derived type whose ancestor is Controlled or Limited_Controlled, calling the ancestor's Finalize would be illegal if it were abstract, even though the actual type might have a concrete version.

Types Controlled and Limited_Controlled are abstract, even though they have no abstract primitive subprograms. It is not clear that they need to be abstract, but there seems to be no harm in it, and it might make an implementation's life easier to know that there are no objects of these types - in case the implementation wishes to make them "magic" in some way.

{AI95-00251-01} For Ada 2005, we considered making these types interfaces. That would have the advantage of allowing them to be added to existing trees. But that was rejected both because it would cause massive disruptions to existing implementations, and because it would be very incompatible due to the "no hidden interfaces" rule. The latter rule would prevent a tagged private type from being completed with a derivation from Controlled or Limited_Controlled - a very common idiom. 

{AI95-00360-01} A type is said to need finalization if:

it is a controlled type, a task type or a protected type; or

{AI05-0092-1} it has a component whose type  needs finalization; or

{AI05-0013-1} it is a class-wide type; or

{AI05-0026-1} it is a partial view whose full view needs finalization; or

it is one of a number of language-defined types that are explicitly defined to need finalization.

Ramification: The fact that a type needs finalization does not require it to be implemented with a controlled type. It just has to be recognized by the No_Nested_Finalization restriction.

This property is defined for the type, not for a particular view. That's necessary as restrictions look in private parts to enforce their restrictions; the point is to eliminate all controlled parts, not just ones that are visible. 


#### Dynamic Semantics

{AI95-00373-01} During the elaboration or evaluation of a construct that causes an object to be initialized by default, for every controlled subcomponent of the object that is not assigned an initial value (as defined in 3.3.1), Initialize is called on that subcomponent. Similarly, if the object that is initialized by default as a whole is controlled, Initialize is called on the object.

{8652/0021} {AI95-00182-01} {AI95-00373-01} For an [extension_aggregate](./AA-4.3#S0111) whose [ancestor_part](./AA-4.3#S0112) is a [subtype_mark](./AA-3.2#S0028) denoting a controlled subtype, the Initialize procedure of the ancestor type is called, unless that Initialize procedure is abstract. 

Discussion: Example: 

```ada
type T1 is new Controlled with
    record
        ... -- some components might have defaults
    end record;

```

```ada
type T2 is new Controlled with
    record
        X : T1; -- no default
        Y : T1 := ...; -- default
    end record;

```

```ada
A : T2;
B : T2 := ...;

```

As part of the elaboration of A's declaration, A.Y is assigned a value; therefore Initialize is not applied to A.Y. Instead, Adjust is applied to A.Y as part of the assignment operation. Initialize is applied to A.X and to A, since those objects are not assigned an initial value. The assignment to A.Y is not considered an assignment to A.

For the elaboration of B's declaration, Initialize is not called at all. Instead the assignment adjusts B's value; that is, it applies Adjust to B.X, B.Y, and B.

{8652/0021} {AI95-00182-01} {AI95-00373-01} The [ancestor_part](./AA-4.3#S0112) of an [extension_aggregate](./AA-4.3#S0111), &lt&gt in aggregates, and the return object of an [extended_return_statement](./AA-6.5#S0225) are handled similarly. 

Initialize and other initialization operations are done in an arbitrary order, except as follows. Initialize is applied to an object after initialization of its subcomponents, if any [(including both implicit initialization and Initialize calls)]. If an object has a component with an access discriminant constrained by a per-object expression, Initialize is applied to this component after any components that do not have such discriminants. For an object with several components with such a discriminant, Initialize is applied to them in order of their [component_declaration](./AA-3.8#S0070)s. For an [allocator](./AA-4.8#S0164), any task activations follow all calls on Initialize. 

Reason: The fact that Initialize is done for subcomponents first allows Initialize for a composite object to refer to its subcomponents knowing they have been properly initialized.

The fact that Initialize is done for components with access discriminants after other components allows the Initialize operation for a component with a self-referential access discriminant to assume that other components of the enclosing object have already been properly initialized. For multiple such components, it allows some predictability. 

When a target object with any controlled parts is assigned a value, [either when created or in a subsequent [assignment_statement](./AA-5.2#S0173),] the assignment operation proceeds as follows: 

The value of the target becomes the assigned value.

The value of the target is adjusted. 

Ramification: If any parts of the object are controlled, abort is deferred during the assignment operation. 

{AI05-0067-1} To adjust the value of a composite object, the values of the components of the object are first adjusted in an arbitrary order, and then, if the object is nonlimited controlled, Adjust is called. Adjusting the value of an elementary object has no effect[, nor does adjusting the value of a composite object with no controlled parts.] 

Ramification: {AI05-0067-1} Adjustment is never actually performed for values of an immutably limited type, since all assignment operations for such types are required to be built-in-place. Even so, we still define adjustment for all types in order that the canonical semantics is well-defined. 

Reason: {AI05-0005-1} The verbiage in the Initialize rule about access discriminants constrained by per-object expressions is not necessary here, since such types are either limited or do not have defaults, so the discriminant can only be changed by an assignment to an outer object. Such an assignment could happen only before any adjustments or (if part of an outer Adjust) only after any inner (component) adjustments have completed. 

For an [assignment_statement](./AA-5.2#S0173), [ after the [name](./AA-4.1#S0091) and [expression](./AA-4.4#S0132) have been evaluated, and any conversion (including constraint checking) has been done,] an anonymous object is created, and the value is assigned into it; [that is, the assignment operation is applied]. [(Assignment includes value adjustment.)] The target of the [assignment_statement](./AA-5.2#S0173) is then finalized. The value of the anonymous object is then assigned into the target of the [assignment_statement](./AA-5.2#S0173). Finally, the anonymous object is finalized. [As explained below, the implementation may eliminate the intermediate anonymous object, so this description subsumes the one given in 5.2, "Assignment Statements".] 

Reason: An alternative design for user-defined assignment might involve an Assign operation instead of Adjust: 

```ada
procedure Assign(Target : in out Controlled; Source : in out Controlled);

```

Or perhaps even a syntax like this: 

```ada
procedure ":="(Target : in out Controlled; Source : in out Controlled);

```

Assign (or ":=") would have the responsibility of doing the copy, as well as whatever else is necessary. This would have the advantage that the Assign operation knows about both the target and the source at the same time - it would be possible to do things like reuse storage belonging to the target, for example, which Adjust cannot do. However, this sort of design would not work in the case of unconstrained discriminated variables, because there is no way to change the discriminants individually. For example: 

```ada
type Mutable(D : Integer := 0) is
    record
        X : Array_Of_Controlled_Things(1..D);
        case D is
            when 17 =&gt Y : Controlled_Thing;
            when others =&gt null;
        end D;
    end record;

```

An assignment to an unconstrained variable of type Mutable can cause some of the components of X, and the component Y, to appear and/or disappear. There is no way to write the Assign operation to handle this sort of case.

Forbidding such cases is not an option - it would cause generic contract model violations. 

{AI05-0067-1} {AI12-0439-1} When a function call or [aggregate](./AA-4.3#S0106) is used to initialize an object, the result of the function call or [aggregate](./AA-4.3#S0106) is an anonymous object, which is assigned into the newly-created object. For such an assignment, the anonymous object may be built in place, in which case the assignment does not involve any copying. Under certain circumstances, the anonymous object is required to be built in place. In particular:

Discussion: {AI05-0067-1} We say assignment to built-in-place objects does not involve copying, which matches the intended implementation (see below). Of course, the implementation can do any copying it likes, if it can make such copying semantically invisible (by patching up access values to point to the copy, and so forth). 

If the full type of any part of the object is immutably limited, the anonymous object is built in place.

Reason: {AI05-0067-1} We talk about the full types being immutably limited, as this is independent of the view of a type (in the same way that it is for determining the technique of parameter passing). That is, privacy is ignored for this purpose.

{AI05-0005-1} {AI05-0067-1} For function calls, we only require building in place for immutably limited types. These are the types that would have been return-by-reference types in Ada 95. We limited the requirement because we want to minimize disruption to Ada 95 implementations and users. 

To be honest: {AI05-0232-1} {AI12-0005-1} This is a dynamic property and is determined by the specific type of the parts of the actual object. In particular, if a part has a class-wide type, the tag of the object might need to be examined in order to determine if build-in-place is required. However, we expect that most Ada implementations will determine this property at compile-time using some assume-the-worst algorithm in order to choose the appropriate method to implement a given call or aggregate. In addition, there is no attribute or other method for a program to determine if a particular object has this property (or not), so there is no value to a more careful description of this rule. 

In the case of an [aggregate](./AA-4.3#S0106), if the full type of any part of the newly-created object is controlled, the anonymous object is built in place.

Reason: {AI05-0067-1} This is necessary to prevent elaboration problems with deferred constants of controlled types. Consider: 

```ada
package P is
   type Dyn_String is private;
   Null_String : constant Dyn_String;
   ...
private
   type Dyn_String is new Ada.Finalization.Controlled with ...
   procedure Finalize(X : in out Dyn_String);
   procedure Adjust(X : in out Dyn_String);

   Null_String : constant Dyn_String :=
      (Ada.Finalization.Controlled with ...);
   ...
end P;

```

When Null_String is elaborated, the bodies of Finalize and Adjust clearly have not been elaborated. Without this rule, this declaration would necessarily raise Program_Error (unless the permissions given below are used by the implementation). 

Ramification: An [aggregate](./AA-4.3#S0106) with a controlled part used in the return expression of a [simple_return_statement](./AA-6.5#S0222) has to be built in place in the anonymous return object, as this is similar to an object declaration. (This is a change from Ada 95, but it is not an inconsistency as it only serves to restrict implementation choices.) But this only covers the [aggregate](./AA-4.3#S0106); a separate anonymous return object can still be used unless it too is required to be built in place.

Similarly, an [aggregate](./AA-4.3#S0106) that has a controlled part but is not itself controlled and that is used to initialize an object also has to be built in place. This is also a change from Ada 95, but it is not an inconsistency as it only serves to restrict implementation choices. This avoids problems if a type like Dyn_String (in the example above) is used as a component in a type used as a deferred constant in package P. 

In other cases, it is unspecified whether the anonymous object is built in place.

Reason: This is left unspecified so the implementation can use any appropriate criteria for determining when to build in place. That includes making the decision on a call-by-call basis. Reasonable programs will not care what decision is made here anyway. 

{AI05-0067-1} Notwithstanding what this document says elsewhere, if an object is built in place:

Upon successful completion of the return statement or [aggregate](./AA-4.3#S0106), the anonymous object mutates into the newly-created object; that is, the anonymous object ceases to exist, and the newly-created object appears in its place.

Finalization is not performed on the anonymous object.

Adjustment is not performed on the newly-created object.

All access values that designate parts of the anonymous object now designate the corresponding parts of the newly-created object.

All renamings of parts of the anonymous object now denote views of the corresponding parts of the newly-created object.

Coextensions of the anonymous object become coextensions of the newly-created object. 

To be honest: {AI12-0005-1} This "mutating" does not necessarily happen atomically with respect to abort and other tasks. For example, if a function call is used as the ancestor part of an [extension_aggregate](./AA-4.3#S0111), then the tag of the anonymous object (the function result) will be different from the tag of the newly-created object (the ancestor part of the [extension_aggregate](./AA-4.3#S0111)). In implementation terms, this involves modifying the tag field. If the current task is aborted during this modification, the object might become abnormal. Likewise, if some other task accesses the tag field during this modification, it constitutes improper use of shared variables, and is erroneous. 

Implementation Note: The intended implementation is that the anonymous object is allocated at the same address as the newly-created object. Thus, no run-time action is required to cause all the access values and renamings to point to the right place. They just point to the newly-created object, which is what the return object has magically "mutated into".

There is no requirement that 'Address of the return object is equal to 'Address of the newly-created object, but that will be true in the intended implementation.

For a function call, if the size of the newly-created object is known at the call site, the object is allocated there, and the address is implicitly passed to the function; the return object is created at that address. Otherwise, a storage pool is implicitly passed to the function; the size is determined at the point of the return statement, and passed to the Allocate procedure. The address returned by the storage pool is returned from the function, and the newly-created object uses that same address. If the return statement is left without returning (via an exception or a goto, for example), then Deallocate is called.  The storage pool might be a dummy pool that represents "allocate on the stack".

The Tag of the newly-created object may be different from that of the result object. Likewise, the master and accessibility level may be different.

An alternative implementation model might allow objects to move around to different addresses. In this case, access values and renamings would need to be modified at run time. It seems that this model requires the full power of tracing garbage collection. 


#### Implementation Permissions

{AI05-0067-1} An implementation is allowed to relax the above rules for [assignment_statement](./AA-5.2#S0173)s in the following ways: 

This paragraph was deleted.{AI05-0067-1} 

Ramification: {AI05-0067-1} {AI12-0005-1} The relaxations apply only to nonlimited types, as [assignment_statement](./AA-5.2#S0173)s are not allowed for limited types. This is important so that the programmer can count on stricter semantics for limited controlled types. 

{AI05-0067-1} {AI12-0444-1} If an object is assigned the value of that same object, the implementation may omit the entire assignment. 

Ramification: In other words, even if an object is controlled and a combination of Finalize and Adjust on the object might have a net side effect, they need not be performed. 

{AI05-0067-1} For assignment of a noncontrolled type, the implementation may finalize and assign each component of the variable separately (rather than finalizing the entire variable and assigning the entire new value) unless a discriminant of the variable is changed by the assignment. 

Reason: For example, in a slice assignment, an anonymous object is not necessary if the slice is copied component-by-component in the right direction, since array types are not controlled (although their components may be). Note that the direction, and even the fact that it's a slice assignment, can in general be determined only at run time. 

Ramification: {AI05-0005-1} This potentially breaks a single assignment operation into many, and thus abort deferral (see 9.8) needs to last only across an individual component assignment when the component has a controlled part. It is only important that the copy step is not separated (by an abort) from the adjust step, so aborts between component assignments is not harmful. 

{AI95-00147-01} {AI05-0067-1} {AI12-0444-1} The implementation may avoid creating an anonymous object if the value being assigned is the result of evaluating a [name](./AA-4.1#S0091) denoting an object (the source object) whose storage cannot overlap with the target. If the source object can overlap with the target object, then the implementation can avoid the need for an intermediary anonymous object by exercising one of the above permissions and perform the assignment one component at a time (for an overlapping array assignment), or not at all (for an assignment where the target and the source of the assignment are the same object). 

Ramification: {AI05-0005-1} If the anonymous object is eliminated by this permission,  there is no anonymous object to be finalized and thus the Finalize call on it is eliminated.

{AI95-00147-01} {AI05-0005-1} Note that if the anonymous object is eliminated but the new value is not built in place in the target object, that Adjust must be called directly on the target object as the last step of the assignment, since some of the subcomponents may be self-referential or otherwise position-dependent. This Adjust can be eliminated only by using one of the following permissions. 

{AI95-00147-01} Furthermore, an implementation is permitted to omit implicit Initialize, Adjust, and Finalize calls and associated assignment operations on an object of a nonlimited controlled type provided that:

any omitted Initialize call is not a call on a user-defined Initialize procedure, and 

To be honest: This does not apply to any calls to a user-defined Initialize routine that happen to occur in an Adjust or Finalize routine. It is intended that it is never necessary to look inside of an Adjust or Finalize routine to determine if the call can be omitted. 

Reason: We don't want to eliminate objects for which the Initialize might have side effects (such as locking a resource). 

any usage of the value of the object after the implicit Initialize or Adjust call and before any subsequent Finalize call on the object does not change the external effect of the program, and

after the omission of such calls and operations, any execution of the program that executes an Initialize or Adjust call on an object or initializes an object by an [aggregate](./AA-4.3#S0106) will also later execute a Finalize call on the object and will always do so prior to assigning a new value to the object, and

the assignment operations associated with omitted Adjust calls are also omitted. 

This permission applies to Adjust and Finalize calls even if the implicit calls have additional external effects. 

Reason: The goal of the above permissions is to allow typical dead assignment and dead variable removal algorithms to work for nonlimited controlled types. We require that "pairs" of Initialize/Adjust/Finalize operations are removed. (These aren't always pairs, which is why we talk about "any execution of the program".) 


#### Extensions to Ada 83

Controlled types and user-defined finalization are new to Ada 95. (Ada 83 had finalization semantics only for masters of tasks.) 


#### Extensions to Ada 95

{AI95-00161-01} Amendment Correction: Types Controlled and Limited_Controlled now have Preelaborable_Initialization, so that objects of types derived from these types can be used in preelaborated packages. 


#### Wording Changes from Ada 95

{8652/0020} {AI95-00126-01} Corrigendum: Clarified that Ada.Finalization is a remote types package.

{8652/0021} {AI95-00182-01} Corrigendum: Added wording to clarify that the default initialization (whatever it is) of an ancestor part is used.

{8652/0022} {AI95-00083-01} Corrigendum: Clarified that Adjust is never called on an [aggregate](./AA-4.3#S0106) used for the initialization of an object or subaggregate, or passed as a parameter.

{AI95-00147-01} Additional optimizations are allowed for nonlimited controlled types. These allow traditional dead variable elimination to be applied to such types.

{AI95-00318-02} Corrected the build-in-place requirement for controlled [aggregate](./AA-4.3#S0106)s to be consistent with the requirements for limited types.

{AI95-00348-01} The operations of types Controlled and Limited_Controlled are now declared as null procedures (see 6.7) to make the semantics clear (and to provide a good example of what null procedures can be used for).

{AI95-00360-01} Types that need finalization are defined; this is used by the No_Nested_Finalization restriction (see D.7, "Tasking Restrictions").

{AI95-00373-01} Generalized the description of objects that have Initialize called for them to say that it is done for all objects that are initialized by default. This is needed so that all of the new cases are covered. 


#### Extensions to Ada 2005

{AI05-0212-1} Package Ada.Finalization now has Pure categorization, so it can be mentioned for any package. Note that this does not change the preelaborability of objects descended from Controlled and Limited_Controlled. 


#### Wording Changes from Ada 2005

{AI05-0013-1} Correction: Eliminated coextensions from the "needs finalization" rules, as this cannot be determined in general in the compilation unit that declares the type. (The designated type of the coextension may have been imported as a limited view.) Uses of "needs finalization" need to ensure that coextensions are handled by other means (such as in No_Nested_Finalization  see D.7) or that coextensions cannot happen.

{AI05-0013-1} Correction: Corrected the "needs finalization" rules to include class-wide types, as a future extension can include a part that needs finalization.

{AI05-0026-1} Correction: Corrected the "needs finalization" rules to clearly say that they ignore privacy.

{AI05-0067-1} Correction: Changed "built in place" to Dynamic Semantics and centralized the rules here. This eliminates the fiction that built in place is just a combination of a permission and a requirement; it clearly has noticeable semantic effects. This wording change is not intended to change the semantics of any correct Ada program. 


## 7.6.1  Completion and Finalization

[This subclause defines completion and leaving of the execution of constructs and entities. A master is the execution of a construct that includes finalization of local objects after it is complete (and after waiting for any local tasks - see 9.3), but before leaving. Other constructs and entities are left immediately upon completion. ]


#### Dynamic Semantics

{AI95-00318-02} The execution of a construct or entity is complete when the end of that execution has been reached, or when a transfer of control (see 5.1) causes it to be abandoned. Completion due to reaching the end of execution, or due to the transfer of control of an [exit_statement](./AA-5.7#S0193), return statement, [goto_statement](./AA-5.8#S0194), or [requeue_statement](./AA-9.5#S0265) or of the selection of a [terminate_alternative](./AA-9.7#S0275) is normal completion. Completion is abnormal otherwise [- when control is transferred out of a construct due to abort or the raising of an exception]. 

Discussion: Don't confuse the run-time concept of completion with the compile-time concept of completion defined in 3.11.1. 

{AI95-00162-01} {AI95-00416-01} {AI12-0406-1} After execution of a construct or entity is complete, it is left, meaning that execution continues with the next action, as defined for the execution that is taking place. Leaving an execution happens immediately after its completion, except in the case of the execution of a master construct: a body other than a [package_body](./AA-7.2#S0231); a [statement](./AA-5.1#S0167); or an [expression](./AA-4.4#S0132), [function_call](./AA-6.4#S0218), or [range](./AA-3.5#S0037) that is not part of an enclosing [expression](./AA-4.4#S0132), [function_call](./AA-6.4#S0218), [range](./AA-3.5#S0037), or [simple_statement](./AA-5.1#S0168) other than a [simple_return_statement](./AA-6.5#S0222). The term master by itself refers to the execution of a master construct. A master is finalized after it is complete, and before it is left.

Reason: {AI95-00162-01} {AI95-00416-01} [Expression](./AA-4.4#S0132)s and [statement](./AA-5.1#S0167)s are masters so that objects created by subprogram calls (in [aggregate](./AA-4.3#S0106)s, [allocator](./AA-4.8#S0164)s for anonymous access-to-object types, and so on) are finalized and have their tasks awaited before the [expression](./AA-4.4#S0132)s or [statement](./AA-5.1#S0167)s are left. Note that [expression](./AA-4.4#S0132)s like the [condition](./AA-4.5#S0150) of an [if_statement](./AA-5.3#S0175) are masters, because they are not enclosed by a [simple_statement](./AA-5.1#S0168). Similarly, a [function_call](./AA-6.4#S0218) which is renamed is a master, as it is not in a [simple_statement](./AA-5.1#S0168).

{AI95-00416-01} We have to include [function_call](./AA-6.4#S0218)s in the contexts that do not cause masters to occur so that [expression](./AA-4.4#S0132)s contained in a [function_call](./AA-6.4#S0218) (that is not part of an [expression](./AA-4.4#S0132) or [simple_statement](./AA-5.1#S0168)) do not individually become masters. We certainly do not want the parameter [expression](./AA-4.4#S0132)s of a [function_call](./AA-6.4#S0218) to be separate masters, as they would then be finalized before the function is called. 

Ramification: {AI95-00416-01} The fact that a [function_call](./AA-6.4#S0218) is a master does not change the accessibility of the return object denoted by the [function_call](./AA-6.4#S0218); that depends on the use of the [function_call](./AA-6.4#S0218). The [function_call](./AA-6.4#S0218) is the master of any short-lived entities (such as [aggregate](./AA-4.3#S0106)s used as parameters of types with task or controlled parts). 

Glossary entry: A master is the execution of a master construct. Each object and task is associated with a master. When a master is left, associated tasks are awaited and associated objects are finalized.

Version=[5],Kind=(AddedNormal),Group=[R],Term=[master], Def=[the execution of a master construct], Note1=[Each object and task is associated with a master. When a master is left, associated tasks are awaited and associated objects are finalized.] 

Glossary entry: A master construct is one of certain executable constructs listed in 7.6.1. Execution of a master construct is a master, with which objects and tasks are associated for the purposes of waiting and finalization.

Version=[5],Kind=(AddedNormal),Group=[C],Term=[master construct], Def=[one of certain executable constructs for which there can be objects or tasks whose lifetime ends when the construct completes], Note1=[Execution of a master construct is a master, with which objects and tasks are associated for the purposes of waiting and finalization.] For the finalization of a master, dependent tasks are first awaited, as explained in 9.3. Then each object whose accessibility level is the same as that of the master is finalized if the object was successfully initialized and still exists. [These actions are performed whether the master is left by reaching the last statement or via a transfer of control.] When a transfer of control causes completion of an execution, each included master is finalized in order, from innermost outward. 

Ramification: As explained in 3.10.2, the set of objects with the same accessibility level as that of the master includes objects declared immediately within the master, objects declared in nested packages, objects created by [allocator](./AA-4.8#S0164)s (if the ultimate ancestor access type is declared in one of those places) and subcomponents of all of these things. If an object was already finalized by Unchecked_Deallocation, then it is not finalized again when the master is left.

Note that any object whose accessibility level is deeper than that of the master would no longer exist; those objects would have been finalized by some inner master. Thus, after leaving a master, the only objects yet to be finalized are those whose accessibility level is less deep than that of the master.

To be honest: Subcomponents of objects due to be finalized are not finalized by the finalization of the master; they are finalized by the finalization of the containing object. 

Reason: We need to finalize subcomponents of objects even if the containing object is not going to get finalized because it was not fully initialized. But if the containing object is finalized, we don't want to require repeated finalization of the subcomponents, as might normally be implied by the recursion in finalization of a master and the recursion in finalization of an object. 

To be honest: Formally, completion and leaving refer to executions of constructs or entities. However, the standard sometimes (informally) refers to the constructs or entities whose executions are being completed. Thus, for example, "the subprogram call or task is complete" really means "the execution of the subprogram call or task is complete". 

For the finalization of an object: 

{AI05-0099-1} If the full type of the object is an elementary type, finalization has no effect; 

Reason: {AI05-0099-1} We say "full type" in this and the following bullets as privacy is ignored for the purpose of determining the finalization actions of an object; that is as expected for Dynamic Semantics rules. 

{AI05-0099-1} If the full type of the object is a tagged type, and the tag of the object identifies a controlled type, the Finalize procedure of that controlled type is called;

{AI05-0099-1} If the full type of the object is a protected type, or if the full type of the object is a tagged type and the tag of the object identifies a protected type, the actions defined in 9.4 are performed;

{AI95-00416-01} {AI05-0099-1} If the full type of the object is a composite type, then after performing the above actions, if any, every component of the object is finalized in an arbitrary order, except as follows: if the object has a component with an access discriminant constrained by a per-object expression, this component is finalized before any components that do not have such discriminants; for an object with several components with such a discriminant, they are finalized in the reverse of the order of their [component_declaration](./AA-3.8#S0070)s; 

Reason: This allows the finalization of a component with an access discriminant to refer to other components of the enclosing object prior to their being finalized. 

To be honest: {AI05-0099-1} {AI12-0005-1} The components discussed here are all of the components that the object actually has, not just those components that are statically identified by the type of the object. These can be different if the object has a class-wide type. 

{AI95-00416-01} If the object has coextensions (see 3.10.2), each coextension is finalized after the object whose access discriminant designates it.

Ramification: {AI05-0066-1} In the case of an [aggregate](./AA-4.3#S0106) or function call that is used (in its entirety) to directly initialize a part of an object, the coextensions of the result of evaluating the [aggregate](./AA-4.3#S0106) or function call are transfered to become coextensions of the object being initialized and are not finalized until the object being initialized is ultimately finalized, even if an anonymous object is created as part of the operation. 

Immediately before an instance of Unchecked_Deallocation reclaims the storage of an object, the object is finalized. [If an instance of Unchecked_Deallocation is never applied to an object created by an [allocator](./AA-4.8#S0164), the object will still exist when the corresponding master completes, and it will be finalized then.]

{AI95-00280-01} {AI05-0051-1} {AI05-0190-1} The finalization of a master performs finalization of objects created by declarations in the master in the reverse order of their creation. After the finalization of a master is complete, the objects finalized as part of its finalization cease to exist, as do any types and subtypes defined and created within the master. 

This paragraph was deleted.{AI05-0190-1} 

This paragraph was deleted.{AI05-0190-1} 

This paragraph was deleted.{AI05-0190-1} 

This paragraph was deleted.{AI05-0190-1} 

Ramification: Note that a deferred constant declaration does not create the constant; the full constant declaration creates it. Therefore, the order of finalization depends on where the full constant declaration occurs, not the deferred constant declaration.

An imported object is not created by its declaration. It is neither initialized nor finalized. 

Implementation Note: An implementation has to ensure that the storage for an object is not reclaimed when references to the object are still possible (unless, of course, the user explicitly requests reclamation via an instance of Unchecked_Deallocation). This implies, in general, that objects cannot be deallocated one by one as they are finalized; a subsequent finalization might reference an object that has been finalized, and that object had better be in its (well-defined) finalized state. 

{AI05-0190-1} Each nonderived access type T has an associated collection, which is the set of objects created by [allocator](./AA-4.8#S0164)s of T, or of types derived from T. Unchecked_Deallocation removes an object from its collection. Finalization of a collection consists of finalization of each object in the collection, in an arbitrary order. The collection of an access type is an object implicitly declared at the following place:

Ramification: {AI05-0190-1} The place of the implicit declaration determines when allocated objects are finalized. For multiple collections declared at the same place, we do not define the order of their implicit declarations.

{AI05-0190-1} Finalization of allocated objects is done according to the (ultimate ancestor) [allocator](./AA-4.8#S0164) type, not according to the storage pool in which they are allocated. Pool finalization might reclaim storage (see 13.11, "Storage Management"), but has nothing (directly) to do with finalization of the pool elements.

{AI05-0190-1} Note that finalization is done only for objects that still exist; if an instance of Unchecked_Deallocation has already gotten rid of a given pool element, that pool element will not be finalized when the master is left. 

Reason: {AI05-0190-1} Note that we talk about the type of the [allocator](./AA-4.8#S0164) here. There may be access values of a (general) access type pointing at objects created by [allocator](./AA-4.8#S0164)s for some other type; these are not (necessarily) finalized at this point. 

For a named access type, the first freezing point (see 13.14) of the type.

Reason: {AI05-0190-1} The freezing point of the ultimate ancestor access type is chosen because before that point, pool elements cannot be created, and after that point, access values designating (parts of) the pool elements can be created. This is also the point after which the pool object cannot have been declared. We don't want to finalize the pool elements until after anything finalizing objects that contain access values designating them. Nor do we want to finalize pool elements after finalizing the pool object itself. 

For the type of an access parameter, the call that contains the [allocator](./AA-4.8#S0164).

For the type of an access result, within the master of the call (see 3.10.2). 

To be honest: {AI05-0005-1} {AI05-0190-1} We mean at a place within the master consistent with the execution of the call within the master. We don't say that normatively, as it is difficult to explain that when the master of the call need not be the master that immediately includes the call (such as when an anonymous result is converted to a named access type). 

For any other anonymous access type, the first freezing point of the innermost enclosing declaration. 

{AI95-00256-01} The target of an [assignment_statement](./AA-5.2#S0173) is finalized before copying in the new value, as explained in 7.6.

{8652/0021} {AI95-00182-01} {AI95-00162-01} {AI05-0066-1} {AI05-0142-4} {AI05-0269-1} The master of an object is the master enclosing its creation whose accessibility level (see 3.10.2) is equal to that of the object, except in the case of an anonymous object representing the result of an [aggregate](./AA-4.3#S0106) or function call. If such an anonymous object is part of the result of evaluating the actual parameter expression for an explicitly aliased parameter of a function call, the master of the object is the innermost master enclosing the evaluation of the [aggregate](./AA-4.3#S0106) or function call, excluding the [aggregate](./AA-4.3#S0106) or function call itself. Otherwise, the master of such an anonymous object is the innermost master enclosing the evaluation of the [aggregate](./AA-4.3#S0106) or function call, which may be the [aggregate](./AA-4.3#S0106) or function call itself. 

This paragraph was deleted.{AI95-00162-01} 

```ada
This paragraph was deleted.

```

This paragraph was deleted.

Reason: {AI95-00162-01} This effectively imports all of the special rules for the accessibility level of renames, [allocator](./AA-4.8#S0164)s, and so on, and applies them to determine where objects created in them are finalized. For instance, the master of a rename of a subprogram is that of the renamed subprogram.

{AI05-0066-1} In 3.10.2 we assign an accessibility level to the result of an [aggregate](./AA-4.3#S0106) or function call that is used to directly initialize a part of an object based on the object being initialized. This is important to ensure that any access discriminants denote objects that live at least as long as the object being initialized. However, if the result of the [aggregate](./AA-4.3#S0106) or function call is not built directly in the target object, but instead is built in an anonymous object that is then assigned to the target, the anonymous object needs to be finalized after the assignment rather than persisting until the target object is finalized (but not its coextensions). (Note than an implementation is never required to create such an anonymous object, and in some cases is required to not have such a separate object, but rather to build the result directly in the target.)

{AI05-0142-4} The special case for explicitly aliased parameters of functions is needed for the same reason, as access discriminants of the returned object may designate one of these parameters. In that case, we want to lengthen the lifetime of the anonymous objects as long as the possible lifetime of the result.

{AI05-0142-4} We don't do a similar change for other kinds of calls, because the extended lifetime of the parameters adds no value, but could constitute a storage leak. For instance, such an anonymous object created by a procedure call in the elaboration part of a package body would have to live until the end of the program, even though it could not be used after the procedure returns (other than via Unchecked_Access). 

Ramification: {AI05-0142-4} Note that the lifetime of the master given to anonymous objects in explicitly aliased parameters of functions is not necessarily as long as the lifetime of the master of the object being initialized (if the function call is used to initialize an [allocator](./AA-4.8#S0164), for instance). In that case, the accessibility check on explicitly aliased parameters will necessarily fail if any such anonymous objects exist. This is necessary to avoid requiring the objects to live as long as the access type or having the implementation complexity of an implicit coextension. 

{8652/0023} {AI95-00169-01} {AI95-00162-01} {AI05-0066-1} {AI05-0262-1} In the case of an [expression](./AA-4.4#S0132) that is a master, finalization of any (anonymous) objects occurs after completing evaluation of the [expression](./AA-4.4#S0132) and all use of the objects, prior to starting the execution of any subsequent construct.


#### Bounded (Run-Time) Errors

{8652/0023} {AI95-00169-01} It is a bounded error for a call on Finalize or Adjust that occurs as part of object finalization or assignment to propagate an exception. The possible consequences depend on what action invoked the Finalize or Adjust operation: 

Ramification: It is not a bounded error for Initialize to propagate an exception. If Initialize propagates an exception, then no further calls on Initialize are performed, and those components that have already been initialized (either explicitly or by default) are finalized in the usual way.

{8652/0023} {AI95-00169-01} It also is not a bounded error for an explicit call to Finalize or Adjust to propagate an exception. We do not want implementations to have to treat explicit calls to these routines specially. 

For a Finalize invoked as part of an [assignment_statement](./AA-5.2#S0173), Program_Error is raised at that point.

{8652/0024} {AI95-00193-01} {AI95-00256-01} {AI12-0445-1} For an Adjust invoked as part of assignment operations other than those invoked as part of an [assignment_statement](./AA-5.2#S0173), some of the adjustments due to be performed can be performed, and then Program_Error is raised. During its propagation, finalization may be applied to objects whose Adjust failed. For an Adjust invoked as part of an [assignment_statement](./AA-5.2#S0173), any other adjustments due to be performed are performed, and then Program_Error is raised. 

Reason: {8652/0024} {AI95-00193-01} {AI95-00256-01} In the case of assignments that are part of initialization, there is no need to complete all adjustments if one propagates an exception, as the object will immediately be finalized. So long as a subcomponent is not going to be finalized, it need not be adjusted, even if it is initialized as part of an enclosing composite assignment operation for which some adjustments are performed. However, there is no harm in an implementation making additional Adjust calls (as long as any additional components that are adjusted are also finalized), so we allow the implementation flexibility here. On the other hand, for an [assignment_statement](./AA-5.2#S0173), it is important that all adjustments be performed, even if one fails, because all controlled subcomponents are going to be finalized. Other kinds of assignment are more like initialization than [assignment_statement](./AA-5.2#S0173)s, so we include them as well in the permission. 

Ramification: {8652/0024} {AI95-00193-01} Even if an Adjust invoked as part of the initialization of a controlled object propagates an exception, objects whose initialization (including any Adjust or Initialize calls) successfully completed will be finalized. The permission above only applies to objects whose Adjust failed. Objects for which Adjust was never even invoked must not be finalized. 

For a Finalize invoked as part of a call on an instance of Unchecked_Deallocation, any other finalizations due to be performed are performed, and then Program_Error is raised. 

Discussion: {8652/0104} {AI95-00179-01} The standard does not specify if storage is recovered in this case. If storage is not recovered (and the object continues to exist), Finalize may be called on the object again (when the [allocator](./AA-4.8#S0164)'s master is finalized). 

This paragraph was deleted.{8652/0023} {AI95-00169-01} {AI05-0064-1} 

{8652/0023} {AI95-00169-01} For a Finalize invoked due to reaching the end of the execution of a master, any other finalizations associated with the master are performed, and Program_Error is raised immediately after leaving the master.

Discussion: {AI05-0064-1} This rule covers both ordinary objects created by a declaration, and anonymous objects created as part of evaluating an [expression](./AA-4.4#S0132). All contexts that create objects that need finalization are defined to be masters. 

{AI95-00318-02} For a Finalize invoked by the transfer of control of an [exit_statement](./AA-5.7#S0193), return statement, [goto_statement](./AA-5.8#S0194), or [requeue_statement](./AA-9.5#S0265), Program_Error is raised no earlier than after the finalization of the master being finalized when the exception occurred, and no later than the point where normal execution would have continued. Any other finalizations due to be performed up to that point are performed before raising Program_Error. 

Ramification: For example, upon leaving a [block_statement](./AA-5.6#S0191) due to a [goto_statement](./AA-5.8#S0194), the Program_Error would be raised at the point of the target statement denoted by the label, or else in some more dynamically nested place, but not so nested as to allow an [exception_handler](./AA-11.2#S0305) that has visibility upon the finalized object to handle it. For example, 

```ada
procedure Main is
begin
    &lt&ltThe_Label&gt&gt
    Outer_Block_Statement : declare
        X : Some_Controlled_Type;
    begin
        Inner_Block_Statement : declare
            Y : Some_Controlled_Type;
            Z : Some_Controlled_Type;
        begin
            goto The_Label;
        exception
            when Program_Error =&gt ... -- Handler number 1.
        end;
    exception
        when Program_Error =&gt ... -- Handler number 2.
    end;
exception
    when Program_Error =&gt ... -- Handler number 3.
end Main;

```

{AI12-0005-1} The [goto_statement](./AA-5.8#S0194) will first cause Finalize(Z) to be called. Suppose that Finalize(Z) propagates an exception. Program_Error will be raised after leaving Inner_Block_Statement, but before leaving Main. Thus, handler number 1 cannot handle this Program_Error; it will be handled either by handler number 2 or handler number 3. If it is handled by handler number 2, then Finalize(Y) will be done before executing the handler. If it is handled by handler number 3, then Finalize(Y) and Finalize(X) will both be done before executing the handler. 

For a Finalize invoked by a transfer of control that is due to raising an exception, any other finalizations due to be performed for the same master are performed; Program_Error is raised immediately after leaving the master. 

Ramification: {AI12-0005-1} If, in the above example, the [goto_statement](./AA-5.8#S0194) were replaced by a [raise_statement](./AA-11.3#S0308), then the Program_Error would be handled by handler number 2, and Finalize(Y) would be done before executing the handler. 

Reason: We considered treating this case in the same way as the others, but that would render certain [exception_handler](./AA-11.2#S0305)s useless. For example, suppose the only [exception_handler](./AA-11.2#S0305) is one for others in the main subprogram. If some deeply nested call raises an exception, causing some Finalize operation to be called, which then raises an exception, then normal execution "would have continued" at the beginning of the [exception_handler](./AA-11.2#S0305). Raising Program_Error at that point would cause that handler's code to be skipped. One would need two nested [exception_handler](./AA-11.2#S0305)s to be sure of catching such cases!

On the other hand, the [exception_handler](./AA-11.2#S0305) for a given master should not be allowed to handle exceptions raised during finalization of that master. 

For a Finalize invoked by a transfer of control due to an abort or selection of a terminate alternative, the exception is ignored; any other finalizations due to be performed are performed. 

Ramification: This case includes an asynchronous transfer of control. 

To be honest: This violates the general principle that it is always possible for a bounded error to raise Program_Error (see ). 


#### Implementation Permissions

{AI05-0107-1} If the execution of an [allocator](./AA-4.8#S0164) propagates an exception, any parts of the allocated object that were successfully initialized may be finalized as part of the finalization of the innermost master enclosing the [allocator](./AA-4.8#S0164).

Reason: This allows deallocating the memory for the allocated object at the innermost master, preventing a storage leak. Otherwise, the object would have to stay around until the finalization of the collection that it belongs to, which could be the entire life of the program if the associated access type is library level. 

{AI05-0111-3} {AI05-0262-1} The implementation may finalize objects created by [allocator](./AA-4.8#S0164)s for an access type whose storage pool supports subpools (see 13.11.4) as if the objects were created (in an arbitrary order) at the point where the storage pool was elaborated instead of at the first freezing point of the access type.

Ramification: This allows the finalization of such objects to occur later than they otherwise would, but still as part of the finalization of the same master. Accessibility rules in 13.11.4 ensure that it is the same master (usually that of the environment task). 

Implementation Note: {AI12-0005-1} This permission is intended to allow the allocated objects to "belong" to the subpool objects and to allow those objects to be finalized at the time that the storage pool is finalized (if they are not finalized earlier). This is expected to ease implementation, as the remaining yet-to-be deallocated objects will only need to be accessible at run time from the subpool header and not also from the overall access type collection header. That is, they only need to belong to a single list, rather than two. 

NOTE 1   {AI05-0299-1} The rules of Clause 10 imply that immediately prior to partition termination, Finalize operations are applied to library-level controlled objects (including those created by [allocator](./AA-4.8#S0164)s of library-level access types, except those already finalized). This occurs after waiting for library-level tasks to terminate. 

Discussion: We considered defining a pragma that would apply to a controlled type that would suppress Finalize operations for library-level objects of the type upon partition termination. This would be useful for types whose finalization actions consist of simply reclaiming global heap storage, when this is already provided automatically by the environment upon program termination. 

NOTE 2   A constant is only constant between its initialization and finalization. Both initialization and finalization are allowed to change the value of a constant.

NOTE 3   Abort is deferred during certain operations related to controlled types, as explained in 9.8. Those rules prevent an abort from causing a controlled object to be left in an ill-defined state.

NOTE 4   The Finalize procedure is called upon finalization of a controlled object, even if Finalize was called earlier, either explicitly or as part of an assignment; hence, if a controlled type is visibly controlled (implying that its Finalize primitive is directly callable), or is nonlimited (implying that assignment is allowed), its Finalize procedure is ideally designed to have no ill effect if it is applied a second time to the same object. 

Discussion: Or equivalently, a Finalize procedure should be "idempotent"; applying it twice to the same object should be equivalent to applying it once. 

Reason: A user-written Finalize procedure should be idempotent since it can be called explicitly by a client (at least if the type is "visibly" controlled). Also, Finalize is used implicitly as part of the [assignment_statement](./AA-5.2#S0173) if the type is nonlimited, and an abort is permitted to disrupt an [assignment_statement](./AA-5.2#S0173) between finalizing the left-hand side and assigning the new value to it (an abort is not permitted to disrupt an assignment operation between copying in the new value and adjusting it). 

Discussion: {AI95-00287-01} Either Initialize or Adjust, but not both, is applied to (almost) every controlled object when it is created: Initialize is done when no initial value is assigned to the object, whereas Adjust is done as part of assigning the initial value. The one exception is the object initialized by an [aggregate](./AA-4.3#S0106) (both the anonymous object created for an aggregate, or an object initialized by an [aggregate](./AA-4.3#S0106) that is built-in-place); Initialize is not applied to the [aggregate](./AA-4.3#S0106) as a whole, nor is the value of the [aggregate](./AA-4.3#S0106) or object adjusted.

All of the following use the assignment operation, and thus perform value adjustment: 

the [assignment_statement](./AA-5.2#S0173) (see 5.2);

explicit initialization of a stand-alone object (see 3.3.1) or of a pool element (see 4.8);

default initialization of a component of a stand-alone object or pool element (in this case, the value of each component is assigned, and therefore adjusted, but the value of the object as a whole is not adjusted);

{AI95-00318-02} function return, when the result is not built-in-place (adjustment of the result happens before finalization of the function);

predefined operators (although the only one that matters is concatenation; see 4.5.3);

generic formal objects of mode in (see 12.4); these are defined in terms of constant declarations; and

{AI95-00287-01} {AI12-0005-1} [aggregate](./AA-4.3#S0106)s (see 4.3), when the result is not built-in-place (in this case, the value of each component, and the ancestor part, for an [extension_aggregate](./AA-4.3#S0111), is assigned, and therefore adjusted, but the value of the [aggregate](./AA-4.3#S0106) as a whole is not adjusted; neither is Initialize called); 

The following also use the assignment operation, but adjustment never does anything interesting in these cases: 

By-copy parameter passing uses the assignment operation (see 6.4.1), but controlled objects are always passed by reference, so the assignment operation never does anything interesting in this case. If we were to allow by-copy parameter passing for controlled objects, we would need to make sure that the actual is finalized before doing the copy back for [in] out parameters. The finalization of the parameter itself needs to happen after the copy back (if any), similar to the finalization of an anonymous function return object or [aggregate](./AA-4.3#S0106) object.

For loops use the assignment operation (see 5.5), but since the type of the loop parameter is never controlled, nothing interesting happens there, either.

{AI95-00318-02} Objects initialized by function results and [aggregate](./AA-4.3#S0106)s that are built-in-place. In this case, the assignment operation is never executed, and no adjustment takes place. While built-in-place is always allowed, it is required for some types - see 7.5 and 7.6 - and that's important since limited types have no Adjust to call. 

This paragraph was deleted.{AI95-00287-01} 

Finalization of the parts of a protected object are not done as protected actions. It is possible (in pathological cases) to create tasks during finalization that access these parts in parallel with the finalization itself. This is an erroneous use of shared variables. 

Implementation Note: One implementation technique for finalization is to chain the controlled objects together on a per-task list. When leaving a master, the list can be walked up to a marked place. The links needed to implement the list can be declared (privately) in types Controlled and Limited_Controlled, so they will be inherited by all controlled types.

Another implementation technique, which we refer to as the "PC-map" approach essentially implies inserting exception handlers at various places, and finalizing objects based on where the exception was raised.

The PC-map approach is for the compiler/linker to create a map of code addresses; when an exception is raised, or abort occurs, the map can be consulted to see where the task was executing, and what finalization needs to be performed. This approach was given in the Ada 83 Rationale as a possible implementation strategy for exception handling - the map is consulted to determine which exception handler applies.

If the PC-map approach is used, the implementation must take care in the case of arrays. The generated code will generally contain a loop to initialize an array. If an exception is raised part way through the array, the components that have been initialized must be finalized, and the others must not be finalized.

It is our intention that both of these implementation methods should be possible. 


#### Wording Changes from Ada 83

{AI05-0299-1} Finalization depends on the concepts of completion and leaving, and on the concept of a master. Therefore, we have moved the definitions of these concepts here, from where they used to be in Clause 9. These concepts also needed to be generalized somewhat. Task waiting is closely related to user-defined finalization; the rules here refer to the task-waiting rules of Clause 9. 


#### Inconsistencies With Ada 95

{AI05-0066-1} Ada 2012 Correction: Changed the definition of the master of an anonymous object used to directly initialize an object, so it can be finalized immediately rather than having to hang around as long as the object. In this case, the Ada 2005 definition was inconsistent with Ada 95, and Ada 2012 changes it back. It is unlikely that many compilers implemented the rule as written in Amendment 1, so an inconsistency is unlikely to arise in practice. 


#### Wording Changes from Ada 95

{8652/0021} {AI95-00182-01} Corrigendum: Fixed the wording to say that anonymous objects aren't finalized until the object can't be used anymore.

{8652/0023} {AI95-00169-01} Corrigendum: Added wording to clarify what happens when Adjust or Finalize raises an exception; some cases had been omitted.

{8652/0024} {AI95-00193-01} {AI95-00256-01} Corrigendum: Stated that if Adjust raises an exception during initialization, nothing further is required. This is corrected in Ada 2005 to include all kinds of assignment other than [assignment_statement](./AA-5.2#S0173)s.

{AI95-00162-01} {AI95-00416-01} Revised the definition of master to include [expression](./AA-4.4#S0132)s and [statement](./AA-5.1#S0167)s, in order to cleanly define what happens for tasks and controlled objects created as part of a subprogram call. Having done that, all of the special wording to cover those cases is eliminated (at least until the Ada comments start rolling in).

{AI95-00280-01} We define finalization of the collection here, so as to be able to conveniently refer to it in other rules (especially in 4.8, "Allocators").

{AI95-00416-01} Clarified that a coextension is finalized at the same time as the outer object. (This was intended for Ada 95, but since the concept did not have a name, it was overlooked.) 


#### Inconsistencies With Ada 2005

{AI05-0051-1} {AI05-0190-1} Correction: Better defined when objects allocated from anonymous access types are finalized. This could be inconsistent if objects are finalized in a different order than in an Ada 2005 implementation and that order caused different program behavior; however programs that depend on the order of finalization within a single master are already fragile and hopefully are rare. 


#### Wording Changes from Ada 2005

{AI05-0064-1} Correction: Removed a redundant rule, which is now covered by the additional places where masters are defined.

{AI05-0099-1} {AI12-0005-1} Correction: Clarified the finalization rules so that there is no doubt that privacy is ignored, and to ensure that objects of class-wide interface types are finalized based on their specific concrete type.

{AI05-0107-1} Correction: Allowed premature finalization of parts of failed [allocator](./AA-4.8#S0164)s. This could be an inconsistency, but the previous behavior is still allowed and there is no requirement that implementations take advantage of the permission.

{AI05-0111-3} Added a permission to finalize an object allocated from a subpool later than usual.

{AI05-0142-4} Added text to specially define the master of anonymous objects which are passed as explicitly aliased parameters (see 6.1) of functions. The model for these parameters is explained in detail in 6.4.1. 


#### Wording Changes from Ada 2012

{AI12-0406-1} Correction: Defined the term "master construct", so as to put static accessibility rules on a firmer basis, including ensuring that those rules apply inside of generic bodies. 

