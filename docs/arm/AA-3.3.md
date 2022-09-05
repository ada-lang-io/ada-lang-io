---
sidebar_position:  19
---

# 3.3  Objects and Named Numbers

[Objects are created at run time and contain a value of a given type. An object can be created and initialized as part of elaborating a declaration, evaluating an [allocator](./AA-4.8#S0164), [aggregate](./AA-4.3#S0106), or [function_call](./AA-6.4#S0218), or passing a parameter by copy. Prior to reclaiming the storage for an object, it is finalized if necessary (see 7.6.1).] 


#### Static Semantics

All of the following are objects: 

Glossary entry: An object is either a constant or a variable. An object contains a value. An object is created by an [object_declaration](./AA-3.3#S0032) or by an [allocator](./AA-4.8#S0164). A formal parameter is (a view of) an object. A subcomponent of an object is an object.

Version=[5],Kind=(AddedNormal),Group=[T],Term=[object], Def=[an entity that contains a value, and is either a constant or a variable], Note1=[An object is created by an [object_declaration](./AA-3.3#S0032) or by an [allocator](./AA-4.8#S0164). A formal parameter is (a view of) an object. A subcomponent of an object is an object.]

the entity declared by an [object_declaration](./AA-3.3#S0032);

a formal parameter of a subprogram, entry, or generic subprogram;

a generic formal object;

a loop parameter;

{AI12-0061-1} the index parameter of an [iterated_component_association](./AA-4.3#S0119);

{AI12-0308-1} the chunk parameter of a [chunk_specification](./AA-5.5#S0180);

a choice parameter of an [exception_handler](./AA-11.2#S0305);

an entry index of an [entry_body](./AA-9.5#S0260);

the result of dereferencing an access-to-object value (see 4.1);

{AI95-00416-01} {AI05-0015-1} the return object of a function;

the result of evaluating an [aggregate](./AA-4.3#S0106);

{AI05-0003-1} {AI12-0226-1} a value conversion or [qualified_expression](./AA-4.7#S0163) whose operand denotes an object;

a component, slice, or view conversion of another object. 

{AI05-0054-2} An object is either a constant object or a variable object. Similarly, a view of an object is either a constant or a variable. All views of a constant elementary object are constant. All views of a constant composite object are constant, except for parts that are of controlled or immutably limited types; variable views of those parts and their subcomponents may exist. In this sense, objects of controlled and immutably limited types are inherently mutable. A constant view of an object cannot be used to modify its value. The terms constant and variable by themselves refer to constant and variable views of objects.

Ramification: {AI12-0422-1} If some part of an object has a variable view, then the object as a whole has a variable view, and not all views of the object are constant. That's true even if only a subcomponent has a variable view. 

{AI12-0422-1} A constant object is known to have no variable views if it does not have a part that is immutably limited, or of a controlled type, private type, or private extension.

Reason: {AI12-0422-1} This definition can be used in Legality Rules as it respects privacy. It is an assume-the-worst rule, as all private types and private extensions might have a controlled component. 

The value of an object is read when the value of any part of the object is evaluated, or when the value of an enclosing object is evaluated. The value of a variable is updated when an assignment is performed to any part of the variable, or when an assignment is performed to an enclosing object. 

Ramification: Reading and updating are intended to include read/write references of any kind, even if they are not associated with the evaluation of a particular construct. Consider, for example, the expression "X.all(F)", where X is an access-to-array object, and F is a function. The implementation is allowed to first evaluate "X.all" and then F. Finally, a read is performed to get the value of the F'th component of the array. Note that the array is not necessarily read as part of the evaluation of "X.all". This is important, because if F were to free X using Unchecked_Deallocation, we want the execution of the final read to be erroneous. 

{AI12-0392-1} Whether a view of an object is constant or variable is determined by the definition of the view. The following (and no others) represent variables: 

{AI12-0392-1} an object declared by an [object_declaration](./AA-3.3#S0032) without the reserved word constant;

To be honest: {AI95-00385-01} We mean the word constant as defined by the grammar for [object_declaration](./AA-3.3#S0032), not some random word constant. Thus, 

```ada
X : access constant T;

```

is not a constant. 

{AI12-0392-1} a formal parameter of mode in out or out;

{AI12-0392-1} a generic formal object of mode in out;

{AI12-0392-1} a non-discriminant component of a variable;

Ramification: {AI12-0392-1} This includes both [selected_component](./AA-4.1#S0098)s and [indexed_component](./AA-4.1#S0096)s. 

{AI12-0392-1} a [slice](./AA-4.1#S0097) of a variable;

{AI05-0262-1} {AI12-0392-1} a loop parameter that is specified to be a variable for a generalized loop (see 5.5.2);

{AI05-0262-1} {AI12-0392-1} a view conversion of a variable;

{AI12-0392-1} a dereference of an access-to-variable value;

{AI05-0015-1} {AI12-0392-1} the return object declared by an [extended_return_statement](./AA-6.5#S0225) without the reserved word constant;

{AI05-0015-1} {AI12-0392-1} the current instance of a type other than a protected type[, if the current instance is an object and not a value (see 8.6)];

Reason: {AI12-0392-1} We exclude current instances of protected types as they are protected units and the next bullet applies. 

Proof: {AI12-0392-1} This list of bullets only applies to views of objects, so current instances that are not objects are not considered here. 

This paragraph was deleted.{AI05-0003-1} {AI12-0392-1} 

{AI05-0120-1} {AI12-0392-1} the current instance of a protected unit except within the body of a protected function of that protected unit, or within a function declared immediately within the body of the protected unit;

{AI12-0392-1} an [attribute_reference](./AA-4.1#S0100) where the attribute is defined to denote a variable (for example, the Storage_Pool attribute  see 13.11).

Ramification: {AI12-0392-1} In particular, this implies that the following are not variables: 

{AI12-0226-1} {AI12-0392-1} the result of evaluating a [function_call](./AA-6.4#S0218), an [aggregate](./AA-4.3#S0106), a value conversion, a [qualified_expression](./AA-4.7#S0163), a [conditional_expression](./AA-4.5#S0148), a [raise_expression](./AA-11.3#S0309), or a parenthesized expression;

{AI12-0125-3} a [target_name](./AA-5.2#S0174) of an [assignment_statement](./AA-5.2#S0173) (see 5.2.1);

{AI12-0061-1} the index parameter of an [iterated_component_association](./AA-4.3#S0119);

{AI12-0392-1} a choice parameter or entry index;

{AI12-0308-1} a chunk parameter of a [chunk_specification](./AA-5.5#S0180). 

{AI12-0392-1} This list of constructs that yield constant views is not exhaustive. 

{AI05-0264-1} {AI12-0191-1} {AI12-0294-1} At the place where a view of an object is defined, a nominal subtype is associated with the view. The nominal type of a view is the type of the nominal subtype of the view. The object's actual subtype (that is, its subtype) can be more restrictive than the nominal subtype of the view; it always is more restrictive if the nominal subtype is an indefinite subtype. A subtype is an indefinite subtype if it is an unconstrained array subtype, or if it has unknown discriminants or unconstrained discriminants without defaults (see 3.7); otherwise, the subtype is a definite subtype [(all elementary subtypes are definite subtypes)]. [A class-wide subtype is defined to have unknown discriminants, and is therefore an indefinite subtype. An indefinite subtype does not by itself provide enough information to create an object; an additional [constraint](./AA-3.2#S0029) or explicit initialization [expression](./AA-4.4#S0132) is necessary (see 3.3.1). A component cannot have an indefinite nominal subtype.]

Glossary entry: The nominal subtype of a view of an object is the subtype specified when the view is defined.

Version=[5],Kind=(AddedNormal),Group=[T], Term=[nominal subtype of a view of an object], Def=[the subtype specified when the view is defined]

{AI05-0008-1} A view of a composite object is known to be constrained if:

{AI12-0401-1} its nominal subtype is constrained and not an untagged partial view, and it is neither a value conversion nor a [qualified_expression](./AA-4.7#S0163); or

its nominal subtype is indefinite; or

{AI05-0008-1} {AI05-0093-1} its type is immutably limited (see 7.5); or

it is part of a stand-alone constant (including a generic formal object of mode in); or

it is part of a formal parameter of mode in; or

it is part of the object denoted by a [function_call](./AA-6.4#S0218) or [aggregate](./AA-4.3#S0106); or

{AI12-0226-1} {AI12-0228-1} it is a value conversion or [qualified_expression](./AA-4.7#S0163) where the operand denotes a view of a composite object that is known to be constrained; or

{AI12-0228-1} it is part of a constant return object of an [extended_return_statement](./AA-6.5#S0225); or

{AI05-0008-1} {AI05-0041-1} {AI12-0228-1} it is a dereference of a pool-specific access type, and there is no ancestor of its type that has a constrained partial view.

Discussion: We do not include dereferences of general access types because they might denote stand-alone aliased unconstrained variables. That's true even for access-to-constant types (the denoted object does not have to be a constant).

{AI05-0005-1} {AI05-0008-1} {AI12-0228-1} We don't mention view conversions as there are no mutable tagged types (discriminant defaults are allowed only if the type is immutably limited), so all tagged view conversions are either of an indefinite type (if it has discriminants without defaults), an immutably limited type (if the discriminants do have defaults), or constrained (if there are no discriminants). This matches the first three bullets here, so all tagged view conversions are known to be constrained without needing to mention them explicitly. Untagged view conversions only can occur in parameter passing (as actuals to in out or out parameters), and "known to be constrained" is not used there.

{AI12-0228-1} We don't need to mention the current instance of a (sub)type, either. If a current instance of a type or subtype appears in an aspect specification, it represents a value, so whether or not it is known to be constrained is irrelevant (the term is only defined for composite objects). Otherwise, the current instance of a type can only be used in an immutably limited type, so all such instances are known to be constrained by the third bullet. 

{AI05-0008-1} {AI05-0041-1} {AI12-0228-1} For the purposes of determining within a generic body whether an object is known to be constrained: 

if a subtype is a descendant of an untagged generic formal private or derived type, and the subtype is not an unconstrained array subtype, it is not considered indefinite and is considered to have a constrained partial view;

if a subtype is a descendant of a formal access type, it is not considered pool-specific.

A named number provides a name for a numeric value known at compile time. It is declared by a [number_declaration](./AA-3.3#S0034). 

NOTE 1   A constant cannot be the target of an assignment operation, nor be passed as an in out or out parameter, between its initialization and finalization, if any.

NOTE 2   {AI05-0054-2} The value of a constant object cannot be changed after its initialization, except in some cases where the object has a controlled or immutably limited part (see 7.5, 7.6, and 13.9.1).

NOTE 3   {AI05-0264-1} The nominal and actual subtypes of an elementary object are always the same. For a discriminated or array object, if the nominal subtype is constrained, then so is the actual subtype. 


#### Extensions to Ada 83

There are additional kinds of objects (choice parameters and entry indices of entry bodies).

The result of a function and of evaluating an aggregate are considered (constant) objects. This is necessary to explain the action of finalization on such things. Because a [function_call](./AA-6.4#S0218) is also syntactically a [name](./AA-4.1#S0091) (see 4.1), the result of a [function_call](./AA-6.4#S0218) can be renamed, thereby allowing repeated use of the result without calling the function again. 


#### Wording Changes from Ada 83

{AI05-0299-1} This subclause now follows the subclauses on types and subtypes, to cut down on the number of forward references.

The term nominal subtype is new. It is used to distinguish what is known at compile time about an object's constraint, versus what its "true" run-time constraint is.

The terms definite and indefinite (which apply to subtypes) are new. They are used to aid in the description of generic formal type matching, and to specify when an explicit initial value is required in an [object_declaration](./AA-3.3#S0032).

We have moved the syntax for [object_declaration](./AA-3.3#S0032) and [number_declaration](./AA-3.3#S0034) down into their respective subclauses, to keep the syntax close to the description of the associated semantics.

We talk about variables and constants here, since the discussion is not specific to [object_declaration](./AA-3.3#S0032)s, and it seems better to have the list of the kinds of constants juxtaposed with the kinds of objects.

We no longer talk about indirect updating due to parameter passing. Parameter passing is handled in 6.2 and 6.4.1 in a way that there is no need to mention it here in the definition of read and update. Reading and updating now includes the case of evaluating or assigning to an enclosing object. 


#### Wording Changes from Ada 95

{AI95-00416-01} Clarified that the return object is the object created by a function call. 


#### Extensions to Ada 2005

{AI05-0015-1} Added wording to allow return objects to be declared as constants, and corrected the definition of return objects as objects. 


#### Wording Changes from Ada 2005

{AI05-0008-1} {AI05-0041-1} {AI05-0093-1} Correction: Added a definition of known to be constrained, for use in other rules.

{AI05-0054-2} Correction: We now recognize the fact that not all declared constant objects are immutable; for those that a variable view can be constructed, they can be changed via that view.

{AI05-0120-1} Correction: Added the current instance of a protected object to the list of constant views; since the list claims to include all possibilities, it had better include that one.

{AI05-0003-1} The result of a [qualified_expression](./AA-4.7#S0163) is defined to be a constant view and is defined to be an object if the operand of the [qualified_expression](./AA-4.7#S0163) is an object. These definitions, combined with some grammar changes, allow [qualified_expression](./AA-4.7#S0163)s to be used in more places. See 4.1 for details. 


#### Incompatibilities With Ada 2012

{AI12-0401-1} Correction: Corrected the definition of "known to be constrained" so that the status of the operand of value conversions and [qualified_expression](./AA-4.7#S0163)s is always used to determine whether the property exists. As the rules are ored together, a value conversion or [qualified_expression](./AA-4.7#S0163) with a constrained nominal subtype would have always met the requirements in Ada 2012, regardless of the operand. This change will mean that some conversions or qualifications (mostly of variables) will no longer be considered "known to be constrained" and therefore 'Access and renaming of such [prefix](./AA-4.1#S0093)es will now be illegal. This is necessary to meet the design goal that subsequent execution cannot cause a renaming or 'Access to cause erroneous execution. 


#### Extensions to Ada 2012

{AI12-0228-1} Correction: A [qualified_expression](./AA-4.7#S0163) of an object that is known to be constrained is now also known to be constrained. This allows qualification to be used to disambiguate a function call used as a prefix in a [renaming_declaration](./AA-8.5#S0238) without making the [renaming_declaration](./AA-8.5#S0238) illegal.

{AI12-0226-1} A value conversion of an object is an object; this makes value conversions consistent with qualified expressions. 


#### Wording Changes from Ada 2012

{AI12-0392-1} Correction: Changed from a list of constants to a list of variables. This makes the default to be a constant, which is the more common case, and eliminates issues caused by omissions from the list (such as parenthesized expressions).

{AI12-0422-1} Added the term "known to have no variable views" in order to have a definition that can be used in Legality Rules without breaking privacy. 


## 3.3.1  Object Declarations

{AI05-0262-1} An [object_declaration](./AA-3.3#S0032) declares a stand-alone object with a given nominal subtype and, optionally, an explicit initial value given by an initialization expression. For an array, access, task, or protected object, the [object_declaration](./AA-3.3#S0032) may include the definition of the (anonymous) type of the object. 


#### Syntax

{AI95-00385-01} {AI95-00406-01} {AI05-0183-1} object_declaration<a id="S0032"></a> ::= 
    [defining_identifier_list](./AA-3.3#S0033) : [aliased] [constant] [subtype_indication](./AA-3.2#S0027) [:= [expression](./AA-4.4#S0132)]
        [[aspect_specification](./AA-13.1#S0346)];
  | [defining_identifier_list](./AA-3.3#S0033) : [aliased] [constant] [access_definition](./AA-3.10#S0084) [:= [expression](./AA-4.4#S0132)]
        [[aspect_specification](./AA-13.1#S0346)];
  | [defining_identifier_list](./AA-3.3#S0033) : [aliased] [constant] [array_type_definition](./AA-3.6#S0051) [:= [expression](./AA-4.4#S0132)]
        [[aspect_specification](./AA-13.1#S0346)];
  | [single_task_declaration](./AA-9.1#S0245)
  | [single_protected_declaration](./AA-9.4#S0250)

defining_identifier_list<a id="S0033"></a> ::= 
  [defining_identifier](./AA-3.1#S0022) {, [defining_identifier](./AA-3.1#S0022)}


#### Name Resolution Rules

For an [object_declaration](./AA-3.3#S0032) with an [expression](./AA-4.4#S0132) following the compound delimiter :=, the type expected for the [expression](./AA-4.4#S0132) is that of the object. This [expression](./AA-4.4#S0132) is called the initialization expression. 


#### Legality Rules

{AI95-00287-01} An [object_declaration](./AA-3.3#S0032) without the reserved word constant declares a variable object. If it has a [subtype_indication](./AA-3.2#S0027) or an [array_type_definition](./AA-3.6#S0051) that defines an indefinite subtype, then there shall be an initialization expression. 


#### Static Semantics

{AI05-0264-1} {AI05-0299-1} An [object_declaration](./AA-3.3#S0032) with the reserved word constant declares a constant object. If it has an initialization expression, then it is called a full constant declaration. Otherwise, it is called a deferred constant declaration. The rules for deferred constant declarations are given in subclause 7.4. The rules for full constant declarations are given in this subclause.

Any declaration that includes a [defining_identifier_list](./AA-3.3#S0033) with more than one [defining_identifier](./AA-3.1#S0022) is equivalent to a series of declarations each containing one [defining_identifier](./AA-3.1#S0022) from the list, with the rest of the text of the declaration copied for each declaration in the series, in the same order as the list. The remainder of this Reference Manual relies on this equivalence; explanations are given for declarations with a single [defining_identifier](./AA-3.1#S0022).

{AI95-00385-01} The [subtype_indication](./AA-3.2#S0027), [access_definition](./AA-3.10#S0084), or full type definition of an [object_declaration](./AA-3.3#S0032) defines the nominal subtype of the object. The [object_declaration](./AA-3.3#S0032) declares an object of the type of the nominal subtype. 

Discussion: {AI95-00385-01} The phrase "full type definition" here includes the case of an anonymous array, access, task, or protected type. 

{AI95-00373-01} {AI12-0192-1} A component of an object is said to require late initialization if: 

{AI12-0192-1} {AI12-0404-1} it has an access discriminant value constrained by a per-object expression; or

{AI12-0192-1} it has an initialization expression that includes a name denoting an access discriminant; or

{AI12-0192-1} it has an initialization expression that includes a reference to the current instance of the type either by name or implicitly as the target object of a call. 

Reason: Such components can depend on the values of other components of the object. We want to initialize them as late and as reproducibly as possible. 


#### Dynamic Semantics

{AI95-00363-01} If a composite object declared by an [object_declaration](./AA-3.3#S0032) has an unconstrained nominal subtype, then if this subtype is indefinite or the object is constant the actual subtype of this object is constrained. The constraint is determined by the bounds or discriminants (if any) of its initial value; the object is said to be constrained by its initial value. When not constrained by its initial value, the actual and nominal subtypes of the object are the same. If its actual subtype is constrained, the object is called a constrained object.

For an [object_declaration](./AA-3.3#S0032) without an initialization expression, any initial values for the object or its subcomponents are determined by the implicit initial values defined for its nominal subtype, as follows: 

The implicit initial value for an access subtype is the null value of the access type.

{AI05-0228-1} {AI12-0439-1} The implicit initial value for a scalar subtype that has the Default_Value aspect specified is the value of that aspect converted to the nominal subtype (which can raise Constraint_Error - see 4.6, "Type Conversions");

Ramification: This is a Dynamic Semantics rule, so the visibility of the [aspect_specification](./AA-13.1#S0346) is not relevant - if the full type for a private type has the Default_Value aspect specified, partial views of the type also have this implicit initial value. 

The implicit initial (and only) value for each discriminant of a constrained discriminated subtype is defined by the subtype.

{AI05-0228-1} {AI12-0439-1} For a (definite) composite subtype, the implicit initial value of each component with a [default_expression](./AA-3.7#S0063) is obtained by evaluation of this expression and conversion to the component's nominal subtype (which can raise Constraint_Error), unless the component is a discriminant of a constrained subtype (the previous case), or is in an excluded [variant](./AA-3.8#S0072) (see 3.8.1). For each component that does not have a [default_expression](./AA-3.7#S0063), if the composite subtype has the Default_Component_Value aspect specified, the implicit initial value is the value of that aspect converted to the component's nominal subtype; otherwise, any implicit initial values are those determined by the component's nominal subtype.

For a protected or task subtype, there is an implicit component (an entry queue) corresponding to each entry, with its implicit initial value being an empty queue. 

Implementation Note: The implementation may add implicit components for its own use, which might have implicit initial values. For a task subtype, such components might represent the state of the associated thread of control. For a type with dynamic-sized components, such implicit components might be used to hold the offset to some explicit component. 

The elaboration of an [object_declaration](./AA-3.3#S0032) proceeds in the following sequence of steps: 

a){AI95-00385-01} The [subtype_indication](./AA-3.2#S0027), [access_definition](./AA-3.10#S0084), [array_type_definition](./AA-3.6#S0051), [single_task_declaration](./AA-9.1#S0245), or [single_protected_declaration](./AA-9.4#S0250) is first elaborated. This creates the nominal subtype (and the anonymous type in the last four cases).

b){AI12-0439-1} If the [object_declaration](./AA-3.3#S0032) includes an initialization expression, the (explicit) initial value is obtained by evaluating the expression and converting it to the nominal subtype (which can raise Constraint_Error - see 4.6). 

c){8652/0002} {AI95-00171-01} {AI95-00373-01} The object is created, and, if there is not an initialization expression, the object is initialized by default. When an object is initialized by default, any per-object constraints (see 3.8) are elaborated and any implicit initial values for the object or for its subcomponents are obtained as determined by the nominal subtype. Any initial values (whether explicit or implicit) are assigned to the object or to the corresponding subcomponents. As described in 5.2 and 7.6, Initialize and Adjust procedures can be called. 

Discussion: For a per-object constraint that contains some per-object expressions and some non-per-object expressions, the values used for the constraint consist of the values of the non-per-object expressions evaluated at the point of the [type_declaration](./AA-3.2#S0023), and the values of the per-object expressions evaluated at the point of the creation of the object.

The elaboration of per-object constraints was presumably performed as part of the dependent compatibility check in Ada 83. If the object is of a limited type with an access discriminant, the [access_definition](./AA-3.10#S0084) is elaborated at this time (see 3.7). 

Reason: The reason we say that evaluating an explicit initialization expression happens before creating the object is that in some cases it is impossible to know the size of the object being created until its initial value is known, as in "X: String := Func_Call(...);". The implementation can create the object early in the common case where the size can be known early, since this optimization is semantically neutral. 

This paragraph was deleted.{AI95-00373-01} 

Ramification: Since the initial values have already been converted to the appropriate nominal subtype, the only Constraint_Errors that might occur as part of these assignments are for values outside their base range that are used to initialize unconstrained numeric subcomponents. See 3.5. 

{AI95-00373-01} For the third step above, evaluations and assignments are performed in an arbitrary order subject to the following restrictions: 

{AI95-00373-01} Assignment to any part of the object is preceded by the evaluation of the value that is to be assigned. 

Reason: Duh. But we ought to say it. Note that, like any rule in the Reference Manual, it doesn't prevent an "as-if" optimization; as long as the semantics as observed from the program are correct, the compiler can generate any code it wants. 

{AI95-00373-01} The evaluation of a [default_expression](./AA-3.7#S0063) that includes the name of a discriminant is preceded by the assignment to that discriminant. 

Reason: Duh again. But we have to say this, too. It's odd that Ada 95 only required the default expressions to be evaluated before the discriminant is used; it says nothing about discriminant values that come from [subtype_indication](./AA-3.2#S0027)s. 

{AI95-00373-01} The evaluation of the [default_expression](./AA-3.7#S0063) for any component that depends on a discriminant is preceded by the assignment to that discriminant. 

Reason: For example: 

```ada
type R(D : Integer := F) is
    record
        S : String(1..D) := (others =&gt G);
    end record;

```

```ada
X : R;

```

For the elaboration of the declaration of X, it is important that F be evaluated before the aggregate. 

{AI95-00373-01} {AI05-0092-1} The assignments to any components, including implicit components, not requiring late initialization precede the initial value evaluations for any components requiring late initialization; if two components both require late initialization, then assignments to parts of the component occurring earlier in the order of the component declarations precede the initial value evaluations of the component occurring later. 

Reason: Components that require late initialization can refer to the entire object during their initialization. We want them to be initialized as late as possible to reduce the chance that their initialization depends on uninitialized components. For instance: 

```ada
type T (D : Natural) is
  limited record
    C1 : T1 (T'Access);
    C2 : Natural := F (D);
    C3 : String (1 .. D) := (others =&gt ' ');
  end record;

```

Component C1 requires late initialization. The initialization could depend on the values of any component of T, including D, C2, or C3. Therefore, we want to it to be initialized last. Note that C2 and C3 do not require late initialization; they only have to be initialized after D.

It is possible for there to be more than one component that requires late initialization. In this case, the language can't prevent problems, because all of the components can't be the last one initialized. In this case, we specify the order of initialization for components requiring late initialization; by doing so, programmers can arrange their code to avoid accessing uninitialized components, and such arrangements are portable. Note that if the program accesses an uninitialized component, 13.9.1 defines the execution to be erroneous. 

{AI05-0228-1} {AI12-0439-1} [There is no implicit initial value defined for a scalar subtype unless the Default_Value aspect has been specified for the type.] In the absence of an explicit initialization or the specification of the Default_Value aspect, a newly created scalar object can have a value that does not belong to its subtype (see 13.9.1 and H.1). 

To be honest: It could even be represented by a bit pattern that doesn't actually represent any value of the type at all, such as an invalid internal code for an enumeration type, or a NaN for a floating point type. It is a generally a bounded error to reference scalar objects with such "invalid representations", as explained in 13.9.1, "Data Validity". 

Ramification: There is no requirement that two objects of the same scalar subtype have the same implicit initial "value" (or representation). It might even be the case that two elaborations of the same [object_declaration](./AA-3.3#S0032) produce two different initial values. However, any particular uninitialized object is default-initialized to a single value (or invalid representation). Thus, multiple reads of such an uninitialized object will produce the same value each time (if the implementation chooses not to detect the error). 

NOTE 1   Implicit initial values are not defined for an indefinite subtype, because if an object's nominal subtype is indefinite, an explicit initial value is required.

NOTE 2   {AI05-0092-1} {AI05-0255-1} {AI12-0061-1} {AI12-0308-1} As indicated above, a stand-alone object is an object declared by an [object_declaration](./AA-3.3#S0032). Similar definitions apply to "stand-alone constant" and "stand-alone variable". A subcomponent of an object is not a stand-alone object, nor is an object that is created by an [allocator](./AA-4.8#S0164). An object declared by a [loop_parameter_specification](./AA-5.5#S0181), [iterator_specification](./AA-5.5#S0183), [iterated_component_association](./AA-4.3#S0119), [chunk_specification](./AA-5.5#S0180), [parameter_specification](./AA-6.1#S0207), [entry_index_specification](./AA-9.5#S0263), [choice_parameter_specification](./AA-11.2#S0306), [extended_return_statement](./AA-6.5#S0225), or a [formal_object_declaration](./AA-12.4#S0319) of mode in out is not considered a stand-alone object.

NOTE 3   The type of a stand-alone object cannot be abstract (see 3.9.3). 


#### Examples

Example of a multiple object declaration: 

```ada
--  the multiple object declaration 

```

```ada
{AI95-00433-01} John, Paul : not null Person_Name := new Person(Sex =&gt M);  --  see 3.10.1

```

```ada
--  is equivalent to the two single object declarations in the order given

```

```ada
{AI95-00433-01} John : not null Person_Name := new Person(Sex =&gt M);
Paul : not null Person_Name := new Person(Sex =&gt M);

```

Examples of variable declarations: 

```ada
{AI95-00433-01} {AI12-0430-1} Count, Sum  : Integer;
Size        : Integer range 0 .. 10_000 := 0;
Sorted      : Boolean := False;
Color_Table : array(1 .. Max) of Color;
Option      : Bit_Vector(1 .. 10) := (others =&gt True); -- see 3.6
Hello       : aliased String := "Hi, world.";
,         : Float range - .. +;

```

Examples of constant declarations: 

```ada
{AI95-00433-01} {AI12-0425-1} Limit     : constant Integer := 10_000;
Low_Limit : constant Integer := Limit/10;
Tolerance : constant Real := Dispersion(1.15);
A_String  : constant String := "A";
Hello_Msg : constant access String := Hello'Access; -- see 3.10.2

```


#### Extensions to Ada 83

The syntax rule for [object_declaration](./AA-3.3#S0032) is modified to allow the aliased reserved word.

A variable declared by an [object_declaration](./AA-3.3#S0032) can be constrained by its initial value; that is, a variable of a nominally unconstrained array subtype, or discriminated type without defaults, can be declared so long as it has an explicit initial value. In Ada 83, this was permitted for constants, and for variables created by allocators, but not for variables declared by [object_declaration](./AA-3.3#S0032)s. This is particularly important for tagged class-wide types, since there is no way to constrain them explicitly, and so an initial value is the only way to provide a constraint. It is also important for generic formal private types with unknown discriminants.

We now allow an [unconstrained_array_definition](./AA-3.6#S0052) in an [object_declaration](./AA-3.3#S0032). This allows an object of an anonymous array type to have its bounds determined by its initial value. This is for uniformity: If one can write "X: constant array(Integer range 1..10) of Integer := ...;" then it makes sense to also allow "X: constant array(Integer range &lt&gt) of Integer := ...;". (Note that if anonymous array types are ever sensible, a common situation is for a table implemented as an array. Tables are often constant, and for constants, there's usually no point in forcing the user to count the number of elements in the value.) 


#### Wording Changes from Ada 83

We have moved the syntax for [object_declaration](./AA-3.3#S0032)s into this subclause.

Deferred constants no longer have a separate syntax rule, but rather are incorporated in [object_declaration](./AA-3.3#S0032) as constants declared without an initialization expression. 


#### Inconsistencies With Ada 95

{AI95-00363-01} Unconstrained aliased objects of types with discriminants with defaults are no longer constrained by their initial values. This means that a program that raised Constraint_Error from an attempt to change the discriminants will no longer do so. The change only affects programs that depended on the raising of Constraint_Error in this case, so the inconsistency is unlikely to occur outside of the ACATS. This change may however cause compilers to implement these objects differently, possibly taking additional memory or time. This is unlikely to be worse than the differences caused by any major compiler upgrade. 


#### Extensions to Ada 95

{AI95-00287-01} A constant may have a limited type; the initialization [expression](./AA-4.4#S0132) has to be built-in-place (see 7.5).

{AI95-00385-01} {AI95-00406-01} A stand-alone object may have an anonymous access type. 


#### Wording Changes from Ada 95

{8652/0002} {AI95-00171-01} Corrigendum: Corrected wording to say that per-object constraints are elaborated (not evaluated).

{AI95-00373-01} The rules for evaluating default initialization have been tightened. In particular, components whose default initialization can refer to the rest of the object are required to be initialized last.

{AI95-00433-01} Added examples of various new constructs. 


#### Extensions to Ada 2005

{AI05-0183-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in an [object_declaration](./AA-3.3#S0032). This is described in 13.1.1. 


#### Wording Changes from Ada 2005

{AI05-0228-1} Implicit initial values can now be given for scalar types and for scalar array components, using the Default_Value (see 3.5) and Default_Component_Value (see 3.6) aspects; the extension is documented there. 


#### Inconsistencies With Ada 2012

{AI12-0192-1} Correction: Components of a protected type require late initialization if their initialization includes (implicitly) the current instance of the type. This means that the components could end up being initialized in a different order. In most cases, this will have no visible effect, or will even fix bugs. Most code for which this is an issue depends on the (unspecified) order of initialization, so it is at risk of failing with a new compiler version regardless of Ada rule changes. However, there do exist very unlikely cases where legal, portable Ada 2012 code would become erroneous. (See the discussion section of AI12-0192-1 for an example.) These are so unlikely that it is expected that they only exist in the minds of Ada lawyers. 


## 3.3.2  Number Declarations

A [number_declaration](./AA-3.3#S0034) declares a named number. 

Discussion: {AI05-0299-1} If a value or other property of a construct is required to be static that means it is required to be determined prior to execution. A static expression is an expression whose value is computed at compile time and is usable in contexts where the actual value might affect the legality of the construct. This is fully defined in subclause 4.9. 


#### Syntax

number_declaration<a id="S0034"></a> ::= 
     [defining_identifier_list](./AA-3.3#S0033) : constant := static_[expression](./AA-4.4#S0132);


#### Name Resolution Rules

The static_[expression](./AA-4.4#S0132) given for a [number_declaration](./AA-3.3#S0034) is expected to be of any numeric type.

{AI12-0394-1} A [name](./AA-4.1#S0091) that denotes a [number_declaration](./AA-3.3#S0034) is interpreted as a value of a universal type, unless the expected type for the [name](./AA-4.1#S0091) is a non-numeric type with an Integer_Literal or Real_Literal aspect, in which case it is interpreted to be of its expected type.

To be honest: This is only a Name Resolution Rule; all named numbers are values of a universal type (see the Static Semantics below). We need this rule so that named numbers can match types with user-defined literals; we need the other rules so the value of the named number is well-defined in all cases. 


#### Legality Rules

{AI05-0299-1} The static_[expression](./AA-4.4#S0132) given for a number declaration shall be a static expression, as defined by subclause 4.9. 


#### Static Semantics

The named number denotes a value of type universal_integer if the type of the static_[expression](./AA-4.4#S0132) is an integer type. The named number denotes a value of type universal_real if the type of the static_[expression](./AA-4.4#S0132) is a real type.

The value denoted by the named number is the value of the static_[expression](./AA-4.4#S0132), converted to the corresponding universal type. 


#### Dynamic Semantics

The elaboration of a [number_declaration](./AA-3.3#S0034) has no effect. 

Proof: Since the static_[expression](./AA-4.4#S0132) was evaluated at compile time. 


#### Examples

Examples of number declarations: 

```ada
Two_Pi        : constant := 2.0*Ada.Numerics.Pi;   -- a real number (see A.5)

```

```ada
{AI95-00433-01} Max           : constant := 500;                   -- an integer number
Max_Line_Size : constant := Max/6;                 -- the integer 83
Power_16      : constant := 2**16;                 -- the integer 65_536
One, Un, Eins : constant := 1;                     -- three different names for 1

```


#### Extensions to Ada 83

We now allow a static expression of any numeric type to initialize a named number. For integer types, it was possible in Ada 83 to use 'Pos to define a named number, but there was no way to use a static expression of some nonuniversal real type to define a named number. This change is upward compatible because of the preference rule for the operators of the root numeric types. 


#### Wording Changes from Ada 83

We have moved the syntax rule into this subclause.

AI83-00263 describes the elaboration of a number declaration in words similar to that of an [object_declaration](./AA-3.3#S0032). However, since there is no expression to be evaluated and no object to be created, it seems simpler to say that the elaboration has no effect. 


#### Extensions to Ada 2012

{AI12-0394-1} Named numbers now can be used with (non-numeric) types that define user-defined literals (see 4.2.1). 

