---
sidebar_position:  53
---

# 6.5  Return Statements

{AI95-00318-02} A [simple_return_statement](./AA-6.5#S0222) or [extended_return_statement](./AA-6.5#S0225) (collectively called a return statement)  is used to complete the execution of the innermost enclosing [subprogram_body](./AA-6.3#S0216), [entry_body](./AA-9.5#S0260), or [accept_statement](./AA-9.5#S0258). 


#### Syntax

{AI95-00318-02} simple_return_statement<a id="S0222"></a><a id="S0223"></a> ::= return [[expression](./AA-4.4#S0132)];

{AI05-0277-1} {AI12-0398-1} extended_return_object_declaration<a id="S0224"></a> ::= 
    [defining_identifier](./AA-3.1#S0022) : [aliased][constant] [return_subtype_indication](./AA-6.5#S0226) [:= [expression](./AA-4.4#S0132)]
        [[aspect_specification](./AA-13.1#S0346)] 

{AI95-00318-02} {AI05-0015-1} {AI05-0053-1} {AI05-0277-1} {AI05-0299-1} extended_return_statement<a id="S0225"></a> ::= 
    return [extended_return_object_declaration](./AA-6.5#S0224) [do
        [handled_sequence_of_statements](./AA-11.2#S0304)
    end return];

{AI95-00318-02} return_subtype_indication<a id="S0226"></a> ::= [subtype_indication](./AA-3.2#S0027) | [access_definition](./AA-3.10#S0084)


#### Name Resolution Rules

{AI95-00318-02} {AI12-0173-1} The result subtype of a function is the subtype denoted by the [subtype_mark](./AA-3.2#S0028), or defined by the [access_definition](./AA-3.10#S0084), after the reserved word return in the profile of the function. The expected type for the [expression](./AA-4.4#S0132), if any, of a [simple_return_statement](./AA-6.5#S0222) is the result type of the corresponding function. The expected type for the [expression](./AA-4.4#S0132) of an [extended_return_object_declaration](./AA-6.5#S0224) is that of the [return_subtype_indication](./AA-6.5#S0226). 

To be honest: The same applies to generic functions. 


#### Legality Rules

{AI95-00318-02} A return statement shall be within a callable construct, and it applies to the innermost callable construct or [extended_return_statement](./AA-6.5#S0225) that contains it. A return statement shall not be within a body that is within the construct to which the return statement applies.

To be honest: {AI12-0089-1} The above also applies to generic subprograms, even though they are not callable constructs. (An instance of a generic subprogram is a callable construct, but not a generic subprogram itself.) 

{AI95-00318-02} {AI05-0015-1} {AI12-0173-1} A function body shall contain at least one return statement that applies to the function body, unless the function contains [code_statement](./AA-13.8#S0357)s. A [simple_return_statement](./AA-6.5#S0222) shall include an [expression](./AA-4.4#S0132) if and only if it applies to a function body. An [extended_return_statement](./AA-6.5#S0225) shall apply to a function body. An [extended_return_object_declaration](./AA-6.5#S0224) with the reserved word constant shall include an [expression](./AA-4.4#S0132).

Reason: {AI95-00318-02} {AI12-0022-1} The requirement that a function body has to have at least one return statement is a "helpful" restriction. There has been some interest in lifting this restriction, or allowing a raise statement to substitute for the return statement. However, there was enough interest in leaving it as is that we decided not to change it. Note that for Ada 2012, Corrigendum 1, a return statement whose expression is a [raise_expression](./AA-11.3#S0309) can be given in any function body (the [raise_expression](./AA-11.3#S0309) will match any type), so there is much less need to eliminate this rule. 

Ramification: {AI95-00318-02} A return statement can apply to an [extended_return_statement](./AA-6.5#S0225), so a [simple_return_statement](./AA-6.5#S0222) without an [expression](./AA-4.4#S0132) can be given in one. However, neither [simple_return_statement](./AA-6.5#S0222) with an [expression](./AA-4.4#S0132) nor an [extended_return_statement](./AA-6.5#S0225) can be given inside an [extended_return_statement](./AA-6.5#S0225), as they must apply (directly) to a function body.

{AI12-0089-1} Since a "function body" includes a generic function body, this rule and all of the following Legality Rules apply to generic function bodies as well as non-generic function bodies. This is true even though a generic function is not a function. 

{AI12-0173-1} {AI12-0418-1} The [expression](./AA-4.4#S0132) of an [extended_return_statement](./AA-6.5#S0225) is the [expression](./AA-4.4#S0132) (if any) of the [extended_return_object_declaration](./AA-6.5#S0224) of the [extended_return_statement](./AA-6.5#S0225).

Ramification: {AI12-0173-1} {AI12-0418-1} The [expression](./AA-4.4#S0132) of a return statement is either the [expression](./AA-4.4#S0132) of a [simple_return_statement](./AA-6.5#S0222) or the [expression](./AA-4.4#S0132) of an [extended_return_statement](./AA-6.5#S0225) as defined above. 

{AI95-00318-02} For an [extended_return_statement](./AA-6.5#S0225) that applies to a function body:

{AI95-00318-02} {AI05-0032-1} {AI05-0103-1} If the result subtype of the function is defined by a [subtype_mark](./AA-3.2#S0028), the [return_subtype_indication](./AA-6.5#S0226) shall be a [subtype_indication](./AA-3.2#S0027). The type of the [subtype_indication](./AA-3.2#S0027) shall be covered by the result type of the function. The subtype defined by the [subtype_indication](./AA-3.2#S0027) shall be statically compatible with the result subtype of the function; if the result type of the function is elementary, the two subtypes shall statically match. If the result subtype of the function is indefinite, then the subtype defined by the [subtype_indication](./AA-3.2#S0027) shall be a definite subtype, or there shall be an [expression](./AA-4.4#S0132).

{AI95-00318-02} If the result subtype of the function is defined by an [access_definition](./AA-3.10#S0084), the [return_subtype_indication](./AA-6.5#S0226) shall be an [access_definition](./AA-3.10#S0084). The subtype defined by the [access_definition](./AA-3.10#S0084) shall statically match the result subtype of the function. [The accessibility level of this anonymous access subtype is that of the result subtype.] 

Proof: {AI12-0070-1} The accessibility of such anonymous access types is defined in the Heart of Darkness (aka 3.10.2). 

{AI05-0032-1} If the result subtype of the function is class-wide, the accessibility level of the type of the subtype defined by the [return_subtype_indication](./AA-6.5#S0226) shall not be statically deeper than that of the master that elaborated the function body. 

Reason: In this case, the [return_subtype_indication](./AA-6.5#S0226) could be a specific type initialized by default; in that case there is no [expression](./AA-4.4#S0132) to check. 

{AI95-00318-02} {AI05-0032-1} For any return statement that applies to a function body:

{AI95-00318-02} {AI05-0188-1} [If the result subtype of the function is limited, then the [expression](./AA-4.4#S0132) of the return statement (if any) shall meet the restrictions described in 7.5.] 

This paragraph was deleted.{AI05-0188-1} 

{AI95-00416-01} {AI05-0032-1} {AI05-0051-1} If the result subtype of the function is class-wide, the accessibility level of the type of the [expression](./AA-4.4#S0132) (if any) of the return statement shall not be statically deeper than that of the master that elaborated the function body.

Discussion: {AI05-0032-1} {AI05-0051-1} {AI12-0005-1} If the result type is class-wide, then there must be an [expression](./AA-4.4#S0132) of the return statement unless this is an [extended_return_statement](./AA-6.5#S0225) whose [return_subtype_indication](./AA-6.5#S0226) is a specific type. We have a separate rule to cover that case. Note that if an [extended_return_statement](./AA-6.5#S0225) has an [expression](./AA-4.4#S0132), then both this rule and the next one must be satisfied. 

{AI05-0051-1} If the subtype determined by the [expression](./AA-4.4#S0132) of the [simple_return_statement](./AA-6.5#S0222) or by the [return_subtype_indication](./AA-6.5#S0226) has one or more access discriminants, the accessibility level of the anonymous access type of each access discriminant shall not be statically deeper than that of the master that elaborated the function body.

Discussion: We use the type used by the return statement rather than from the function return type since we want to check whenever the return object has access discriminants, even if the function return type doesn't have any (mostly for a class-wide type). 

{AI05-0277-1} {AI12-0426-1} If the reserved word aliased is present in an [extended_return_object_declaration](./AA-6.5#S0224), the type of the extended return object shall be immutably limited. 


#### Static Semantics

{AI95-00318-02} {AI05-0015-1} {AI05-0144-2} Within an [extended_return_statement](./AA-6.5#S0225), the return object is declared with the given [defining_identifier](./AA-3.1#S0022), with the nominal subtype defined by the [return_subtype_indication](./AA-6.5#S0226). An [extended_return_statement](./AA-6.5#S0225) with the reserved word constant is a full constant declaration that declares the return object to be a constant object. 


#### Dynamic Semantics

{AI95-00318-02} {AI95-00416-01} {AI05-0032-1} {AI12-0439-1} For the execution of an [extended_return_statement](./AA-6.5#S0225), the [subtype_indication](./AA-3.2#S0027) or [access_definition](./AA-3.10#S0084) is elaborated. This creates the nominal subtype of the return object. If there is an [expression](./AA-4.4#S0132), it is evaluated and converted to the nominal subtype (which can raise Constraint_Error - see 4.6); the return object is created and the converted value is assigned to the return object. Otherwise, the return object is created and initialized by default as for a stand-alone object of its nominal subtype (see 3.3.1). If the nominal subtype is indefinite, the return object is constrained by its initial value. A check is made that the value of the return object belongs to the function result subtype. Constraint_Error is raised if this check fails. 

Ramification: If the result type is controlled or has a controlled part, appropriate calls on Initialize or Adjust are performed prior to executing the [handled_sequence_of_statements](./AA-11.2#S0304), except when the initial expression is an [aggregate](./AA-4.3#S0106) (which requires build-in-place with no call on Adjust).

{AI05-0005-1} If the return statement is left without resulting in a return (for example, due to an exception propagated from the [expression](./AA-4.4#S0132) or the [handled_sequence_of_statements](./AA-11.2#S0304), or a goto out of the [handled_sequence_of_statements](./AA-11.2#S0304)), if the return object has been created, it is finalized prior to leaving the return statement. If it has not been created when the return statement is left, it is not created or finalized.

{AI05-0032-1} Other rules ensure that the check required by this rule cannot fail unless the function has a class-wide result subtype where the associated specific subtype is constrained. In other cases, either the subtypes have to match or the function's subtype is unconstrained and needs no checking. 

{AI95-00318-02} For the execution of a [simple_return_statement](./AA-6.5#S0222), the [expression](./AA-4.4#S0132) (if any) is first evaluated, converted to the result subtype, and then is assigned to the anonymous return object. 

Ramification: The conversion might raise Constraint_Error - (see 4.6). 

{AI95-00318-02} {AI95-00416-01} [If the return object has any parts that are tasks, the activation of those tasks does not occur until after the function returns (see 9.2).] 

Proof: This is specified by the rules in 9.2. 

Reason: Only the caller can know when task activations should take place, as it depends on the context of the call. If the function is being used to initialize the component of some larger object, then that entire object must be initialized before any task activations. Even after the outer object is fully initialized, task activations are still postponed until the begin at the end of the declarative part if the function is being used to initialize part of a declared object. 

{AI95-00318-02} {AI95-00344-01} {AI05-0024-1} {AI05-0032-1} {AI12-0097-1} {AI12-0418-1} If the result type of a function is a specific tagged type, the tag of the return object is that of the result type. If the result type is class-wide, the tag of the return object is that of the value of the [expression](./AA-4.4#S0132) of the return statement, unless the return object is defined by an [extended_return_object_declaration](./AA-6.5#S0224) with a [subtype_indication](./AA-3.2#S0027) that is specific, in which case it is that of the type of the [subtype_indication](./AA-3.2#S0027). A check is made that the master of the type identified by the tag of the result includes the elaboration of the master that elaborated the function body. If this check fails, Program_Error is raised. 

Ramification: {AI95-00318-02} The first sentence is true even if the tag of the [expression](./AA-4.4#S0132) is different, which could happen if the [expression](./AA-4.4#S0132) were a view conversion or a dereference of an access value. Note that for a limited type, because of the restriction to [aggregate](./AA-4.3#S0106)s and function calls (and no conversions), the tag will already match. 

Reason: {AI95-00318-02} The first rule ensures that a function whose result type is a specific tagged type always returns an object whose tag is that of the result type. This is important for dispatching on controlling result, and allows the caller to allocate the appropriate amount of space to hold the value being returned (assuming there are no discriminants).

The master check prevents the returned object from outliving its type. Note that this check cannot fail for a specific tagged type, as the tag represents the function's type, which necessarily must be declared outside of the function.

We can't use the normal accessibility level "deeper than" check here because we may have "incomparable" levels if the masters belong to two different tasks. This can happen when an accept statement calls a function declared in the enclosing task body, and the function returns an object passed to it from the accept statement, and this object was itself a parameter to the accept statement. 

{AI95-00318-02} {AI05-0058-1} {AI12-0343-1} For the execution of an [extended_return_statement](./AA-6.5#S0225), the [handled_sequence_of_statements](./AA-11.2#S0304) is executed. Within this [handled_sequence_of_statements](./AA-11.2#S0304), the execution of a [simple_return_statement](./AA-6.5#S0222) that applies to the [extended_return_statement](./AA-6.5#S0225) causes a transfer of control that completes the [extended_return_statement](./AA-6.5#S0225). Upon completion of a return statement that applies to a callable construct by the normal completion of a [simple_return_statement](./AA-6.5#S0222) or by reaching the end return of an [extended_return_statement](./AA-6.5#S0225), a transfer of control is performed which completes the execution of the callable construct, and returns to the caller.

Ramification: {AI05-0058-1} A transfer of control that completes an [extended_return_statement](./AA-6.5#S0225) (such as an exit or goto) does not cause a return to the caller unless it is caused by [simple_return_statement](./AA-6.5#S0222) (that is, triggers the second sentence of this paragraph). The return to the caller occurs for the [simple_return_statement](./AA-6.5#S0222) that applies to an [extended_return_statement](./AA-6.5#S0225) because the last sentence says "the normal completion of a [simple_return_statement](./AA-6.5#S0222)", which includes the one nested in the [extended_return_statement](./AA-6.5#S0225).

{AI12-0343-1} The check on the tag of the object occurs when the object is created (before any [sequence_of_statements](./AA-5.1#S0166)); the checks which follow occur after the execution of any [sequence_of_statements](./AA-5.1#S0166). This is implicit in the order of definition of these Dynamic Semantics. 

{AI05-0073-1} {AI12-0343-1} If the result subtype of the function is defined by an [access_definition](./AA-3.10#S0084) designating a specific tagged type T, a check is made that the result value is null or the tag of the object designated by the result value identifies T. Constraint_Error is raised if this check fails. 

Reason: This check is needed so that dispatching on controlling access results works for tag-indeterminate functions. If it was not made, it would be possible for such functions to return an access to a descendant type, meaning the function could return an object with a tag different than the one assumed by the dispatching rules. 

Paragraphs 9 through 20 were deleted. 

{AI95-00318-02} {AI95-00402-01} {AI95-00416-01} {AI05-0051-1} If any part of the specific type of the return object of a function (or coextension thereof) has one or more access discriminants whose value is not constrained by the result subtype of the function, a check is made that the accessibility level of the anonymous access type of each access discriminant, as determined by the [expression](./AA-4.4#S0132) or the [return_subtype_indication](./AA-6.5#S0226) of the return statement, is not deeper than the level of the master of the call (see 3.10.2). If this check fails, Program_Error is raised. 

This paragraph was deleted.

Reason: The check prevents the returned object (for a nonlimited type) from outliving the object designated by one of its discriminants. The check is made on the values of the discriminants, which may come from the [return_subtype_indication](./AA-6.5#S0226) (if constrained), or the [expression](./AA-4.4#S0132), but it is never necessary to check both. 

Implementation Note: {AI05-0234-1} The reason for saying "any part of the specific type" is to simplify implementation. In the case of class-wide result objects, this allows the testing of a simple flag in the tagged type descriptor that indicates whether the specific type has any parts with access discriminants. By basing the test on the type of the object rather than the object itself, we avoid concerns about whether subcomponents in variant parts and of arrays (which might be empty) are present. 

Discussion: {AI05-0234-1} For a function with a class-wide result type, the access values that need to be checked are determined by the tag of the return object. In order to implement this accessibility check in the case where the tag of the result is not known statically at the point of the return statement, an implementation may need to somehow associate with the tag of a specific tagged type an indication of whether the type has unconstrained access discriminants (explicit or inherited) or has any subcomponents with such discriminants. If an implementation is already maintaining a statically initialized descriptor of some kind for each specific tagged type, then an additional Boolean could be added to this descriptor.

{AI05-0005-1} {AI05-0234-1} Note that the flag should only be queried in the case where the result object might have access discriminants that might have subtypes with "bad" accessibility levels (as determined by the rules of 3.10.2 for determining the accessibility level of the type of an access discriminant in the [expression](./AA-4.4#S0132) or [return_subtype_indication](./AA-6.5#S0226) of a return statement).

Thus, in a case like

```ada
type Global is access T'Class;
function F (Ptr : Global) return T'Class is
begin
   return Ptr.all;
end F;

```

there is no need for a run-time accessibility check. While an object of T'Class "might have" access discriminants, the accessibility of those potential discriminants cannot be bad. The setting of the bit doesn't matter and there is no need to query it.

On the other hand, given 

```ada
function F return T'Class is
   Local : T'Class := ... ;
begin
   return Local;
end F;

```

In this case, a check would typically be required.

The need for including subcomponents in this check is illustrated by the following example: 

```ada
X : aliased Integer;

```

```ada
type Component_Type (Discrim : access Integer := X'Access)
   is limited null record;

```

```ada
type Undiscriminated is record
   Fld : Component_Type;
end record;

```

```ada
function F return Undiscriminated is
   Local : aliased Integer;
begin
   return X : Undiscriminated := (Fld =&gt (Discrim =&gt Local'Access)) do
      Foo;
   end return;
   -- raises Program_Error after calling Foo.
end F;

```

Ramification: {AI05-0234-1} In the case where the tag of the result is not known statically at the point of the return statement and the run-time accessibility check is needed, discriminant values and array bounds play no role in performing this check. That is, array components are assumed to have nonzero length and components declared within variant parts are assumed to be present. Thus, the check may be implemented simply by testing the aforementioned descriptor bit and conditionally raising Program_Error. 

{AI95-00318-02} {AI05-0058-1} {AI12-0343-1} A check is performed that the return value satisfies the predicates of the return subtype. If this check fails, the effect is as defined in subclause 3.2.4, "Subtype Predicates".

This paragraph was deleted.{AI05-0058-1} {AI12-0343-1} 

Implementation Note: {AI12-0005-1} {AI12-0343-1} The subtype conversion of the return expression for a [simple_return_statement](./AA-6.5#S0222) performs this same check. The permissions of 11.4.2 ensure that duplicate evaluation of a predicate at a single point is not required (other than pathological, not portable cases), so a single evaluation of the predicate is enough in this case. 

{AI95-00318-02} In the case of a function, the [function_call](./AA-6.4#S0218) denotes a constant view of the return object. 


#### Implementation Permissions

{AI95-00416-01} {AI05-0050-1} For a function call used to initialize a composite object with a constrained nominal subtype or used to initialize a return object that is built in place into such an object:

{AI05-0050-1} If the result subtype of the function is constrained, and conversion of an object of this subtype to the subtype of the object being initialized would raise Constraint_Error, then Constraint_Error may be raised before calling the function.

{AI05-0050-1} If the result subtype of the function is unconstrained, and a return statement is executed such that the return object is known to be constrained, and conversion of the return object to the subtype of the object being initialized would raise Constraint_Error, then Constraint_Error may be raised at the point of the call (after abandoning the execution of the function body). 

Reason: {AI95-00416-01} {AI05-0050-1} Without such a permission, it would be very difficult to implement "built-in-place" semantics. The intention is that the exception is raised at the same point that it would have been raised without the permission; it should not change handlers if the implementation switches between return-by-copy and built-in-place. This means that the exception is not handleable within the function, because in the return-by-copy case, the constraint check to verify that the result satisfies the constraints of the object being initialized happens after the function returns. This implies further that upon detecting such a situation, the implementation may need to simulate a goto to a point outside any local exception handlers prior to raising the exception. 

Ramification: {AI95-00416-01} {AI05-0050-1} These permissions do not apply in the case of an extended return object with mutable discriminants. That's necessary because in that case a return object can be created with the "wrong" discriminants and then changed to the "right" discriminants later (but before returning). We don't want this case raising an exception when the canonical semantics will not do so.

{AI05-0050-1} It's still possible to write a program that will raise an exception using this permission that would not in the canonical semantics. That could happen if a return statement with the "wrong" discriminants or bounds is abandoned (via an exception, or for an extended_return_statement, via an exit or goto statement), and then a return statement with the "right" discriminants or bounds is executed. The only solution for this problem is to not have the permission at all, but this is too unusual of a case to worry about the effects of the permission, especially given the implementation difficulties for built-in-place objects that this permission is intended to ease.

{AI05-0050-1} Note that the mutable-discriminant case only happens when built-in-place initialization is optional. This means that any difficulties associated with implementing built-in-place initialization without these permissions can be sidestepped by not building in place.


#### Examples

Examples of return statements: 

```ada
{AI95-00318-02} return;                         -- in a procedure body, [entry_body](./AA-9.5#S0260),
                                -- [accept_statement](./AA-9.5#S0258), or [extended_return_statement](./AA-6.5#S0225)

```

```ada
return Key_Value(Last_Index);   -- in a function body

```

```ada
{AI95-00318-02} return Node : Cell do           -- in a function body, see 3.10.1 for Cell
   Node.Value := Result;
   Node.Succ := Next_Node;
end return;

```


#### Incompatibilities With Ada 83

{AI95-00318-02} In Ada 95, if the result type of a function has a part that is a task, then an attempt to return a local variable will raise Program_Error. This is illegal in Ada 2005, see below. In Ada 83, if a function returns a local variable containing a task, execution is erroneous according to AI83-00867. However, there are other situations where functions that return tasks (or that return a variant record only one of whose variants includes a task) are correct in Ada 83 but will raise Program_Error according to the new rules.

The rule change was made because there will be more types (protected types, limited controlled types) in Ada 95 for which it will be meaningless to return a local variable, and making all of these erroneous is unacceptable. The current rule was felt to be the simplest that kept upward incompatibilities to situations involving returning tasks, which are quite rare. 


#### Wording Changes from Ada 83

{AI05-0299-1} This subclause has been moved here from chapter 5, since it has mainly to do with subprograms.

A function now creates an anonymous object. This is necessary so that controlled types will work.

{AI95-00318-02} We have clarified that a return statement applies to a callable construct, not to a callable entity.

{AI95-00318-02} There is no need to mention generics in the rules about where a return statement can appear and what it applies to; the phrase "body of a subprogram or generic subprogram" is syntactic, and refers exactly to "[subprogram_body](./AA-6.3#S0216)". 


#### Inconsistencies With Ada 95

{AI95-0416-1} {AI05-0005-1} {AI05-0050-1} Added an Implementation Permission allowing early raising of Constraint_Error if the result cannot fit in the ultimate object. This gives implementations more flexibility to do built-in-place returns, and is essential for limited types (which cannot be built in a temporary). However, it allows raising Constraint_Error in some cases where it would not be raised if the permission was not used. See Inconsistencies With Ada 2005 for additional changes. This case is potentially inconsistent with Ada 95, but a compiler does not have to take advantage of these permissions for any Ada 95 code, so there should be little practical impact. 


#### Incompatibilities With Ada 95

{AI95-00318-02}  The entire business about return-by-reference types has been dropped. Instead, the [expression](./AA-4.4#S0132) of a return statement of a limited type can only be an [aggregate](./AA-4.3#S0106) or [function_call](./AA-6.4#S0218) (see 7.5). This means that returning a global object or [type_conversion](./AA-4.6#S0162), legal in Ada 95, is now illegal. Such functions can be converted to use anonymous access return types by adding access in the function definition and return statement, adding .all in uses, and adding aliased in the object declarations. This has the advantage of making the reference return semantics much clearer to the casual reader.

We changed these rules so that functions, combined with the new rules for limited types (7.5), can be used as build-in-place constructors for limited types. This reduces the differences between limited and nonlimited types, which will make limited types useful in more circumstances. 


#### Extensions to Ada 95

{AI95-00318-02} The [extended_return_statement](./AA-6.5#S0225) is new. This provides a name for the object being returned, which reduces the copying needed to return complex objects (including no copying at all for limited objects). It also allows component-by-component construction of the return object. 


#### Wording Changes from Ada 95

{AI95-00318-02} The wording was updated to support anonymous access return subtypes.

{AI95-00318-02} The term "return expression" was dropped because reviewers found it confusing when applied to the default [expression](./AA-4.4#S0132) of an [extended_return_statement](./AA-6.5#S0225).

{AI95-00344-01} {AI95-00416-01} Added accessibility checks to class-wide return statements. These checks could not fail in Ada 95 (as all of the types had to be declared at the same level, so the tagged type would necessarily have been at the same level as the type of the object).

{AI95-00402-01} {AI95-00416-01} Added accessibility checks to return statements for types with access discriminants. Since such types have to be limited in Ada 95, the [expression](./AA-4.4#S0132) of a return statement would have been illegal in order for this check to fail. 


#### Inconsistencies With Ada 2005

{AI05-0050-1} Correction: The Implementation Permission allowing early raising of Constraint_Error was modified to remove the most common of these cases from the permission (returning an object with mutable discriminants, where the return object is created with one set of discriminants and then changed to another). (The permission was also widened to allow the early check for constrained functions when that constraint is wrong.) However, there still is an unlikely case where the permission would allow an exception to be raised when none would be raised by the canonical semantics (when a return statement is abandoned). These changes can only remove the raising of an exception (or change the place where it is raised) compared to Ada 2005, so programs that depend on the previous behavior should be very rare.

{AI05-0051-1} {AI05-0234-1} Correction: Accessibility checks for access discriminants now depend on the master of the call rather than the point of declaration of the function. This will result in cases that used to raise Program_Error now running without raising any exception. This is technically inconsistent with Ada 2005 (as defined by Amendment 1), but it is unlikely that any real code depends on the raising of this exception.

{AI05-0073-1} Correction: Added a tag check for functions returning anonymous access-to-tagged types, so that dispatching of tag-indeterminate function works as expected. This is technically inconsistent with Ada 2005 (as defined by Amendment 1), but as the feature in question was newly added to Ada 2005, there should be little code that depends on the behavior that now raises an exception. 


#### Incompatibilities With Ada 2005

{AI05-0053-1} {AI05-0277-1} Correction: The aliased keyword can now only appear on extended return objects with an immutably limited type. Other types would provide a way to get an aliased view of an object that is not necessarily aliased, which would be very bad. This is incompatible, but since the feature was added in Ada 2005, the keyword had no defined meaning in Ada 2005 (a significant oversight), and most sensible uses involve immutably limited types, it is unlikely that it appears meaningfully in existing programs.

{AI05-0103-1} Correction: Added wording to require static matching for unconstrained access types in extended return statements. This disallows adding or omitting null exclusions, and adding access constraints, in the declaration of the return object. While this is incompatible, the incompatible cases in question are either useless (access constraints  the constraint can be given on an [allocator](./AA-4.8#S0164) if necessary, and still must be given there even if given on the return object) or wrong (null exclusions  null could be returned from a function declared to be null excluding), so we expect them to be extremely rare in practice. 


#### Extensions to Ada 2005

{AI05-0015-1} {AI05-0144-2} The return object of an [extended_return_statement](./AA-6.5#S0225) can be declared constant; this works similarly to a constant object declaration.

{AI05-0032-1} Added wording to allow the [return_subtype_indication](./AA-6.5#S0226) to have a specific type if the return subtype of the function is class-wide. Specifying the (specific) type of the return object is awkward without this change, and this is consistent with the way [allocator](./AA-4.8#S0164)s work. 


#### Wording Changes from Ada 2005

{AI05-0024-1} Correction: Corrected the master check for tags since the masters may be for different tasks and thus incomparable.

{AI05-0058-1} Correction: Corrected the wording defining returns for [extended_return_statement](./AA-6.5#S0225)s, since leaving by an exit or goto is considered "normal" completion of the statement.

{AI05-0205-1} {AI05-0277-1} Correction: Added the [extended_return_object_declaration](./AA-6.5#S0224) to make other rules easier to write and eliminate the problem described in AI05-0205-1. 


#### Inconsistencies With Ada 2012

{AI12-0343-1} Correction: Reordered the checks associated with return statements to clarify when they happen relative to the [sequence_of_statements](./AA-5.1#S0166) of an [extended_return_statement](./AA-6.5#S0225), and to add an additional predicate check on the returned value. This latter check can fail if the value to be returned is changed in the statements of an extended return. Thus, the following function now raises Assertion_Error, while no exception would be raised in Ada 2012: 

```ada
type Rec is record
  Count, Max : Natural;
end record with Dynamic_Predicate =&gt Rec.Count &lt= Rec.Max;

```

```ada
function Foo return Rec is
begin
    return Result : Rec := (Count =&gt 5, Max =&gt 10) do
       Result.Max := 0;
    end return;
end Foo;

```

This will detect a program bug (returning a value that does not meet the predicate of the return subtype) rather than leaving it hidden, so the inconsistency is tolerable. 


#### Extensions to Ada 2012

{AI12-0398-1} Extended return objects now can have an [aspect_specification](./AA-13.1#S0346), allowing the specification of (implementation-defined) aspects for return objects. 


#### Wording Changes from Ada 2012

{AI05-0097-1} Corrigendum: Clarified the wording so that it is clear where the tag of the return object comes from. While a literal reading of the original Ada 2012 rule could have caused some weird results (by using some nearby [subtype_indication](./AA-3.2#S0027) to provide the tag in the case of a [simple_return_statement](./AA-6.5#S0222)), such a reading would be so unlike the rest of the language that we do not believe anyone would ever have thought it was intended. As such, we do not believe any implementation ever did this wrong (at least because of the old wording), and thus do not document this as a possible inconsistency.

{AI12-0173-1} Correction: Added a definition of the term [expression](./AA-4.4#S0132) of an [extended_return_statement](./AA-6.5#S0225). That wording was commonly used before the [extended_return_object_declaration](./AA-6.5#S0224) was split into a separate syntax production, leaving the [extended_return_statement](./AA-6.5#S0225) without any [expression](./AA-4.4#S0132) of its own. Moreover, the wording often just uses "[expression](./AA-4.4#S0132) of a return statement" to cover both kinds of return statement. Changing the wording in more than a dozen places was unappealing, and some of the changes would be awkward to read, so we defined the term and left the majority of the wording unchanged. 


## 6.5.1  Nonreturning Subprograms

{AI95-00329-01} {AI95-00414-01} {AI05-0229-1} {AI12-0269-1} {AI12-0418-1} Specifying aspect No_Return to have the value True indicates that a subprogram cannot return normally[; it may, for example, propagate an exception or loop forever]. 

Discussion: Aspect No_Deposit will have to wait for Ada 2028. :-) 

Paragraphs 2 and 3 were moved to Annex J, "Obsolescent Features". 


#### Static Semantics

{AI05-0229-1} {AI12-0269-1} For a subprogram or generic subprogram, the following language-defined representation aspect may be specified: 

No_ReturnThe type of aspect No_Return is Boolean. When aspect No_Return is True for an entity, the entity is said to be nonreturning.

{AI12-0423-1} If directly specified, the [aspect_definition](./AA-13.1#S0348) shall be a static expression. When not directly specified, if the subprogram is a primitive subprogram inherited by a derived type, then the aspect is True if any corresponding subprogram of the parent or progenitor types is nonreturning. Otherwise, the aspect is False.

Aspect Description for No_Return: A subprogram will not return normally.

{AI05-0229-1} {AI12-0269-1} If a generic subprogram is nonreturning, then so are its instances. If a subprogram declared within a generic unit is nonreturning, then so are the corresponding copies of that subprogram in instances.


#### Legality Rules

{AI95-00329-01} {AI95-00414-01} {AI05-0229-1} Aspect No_Return shall not be specified for a null procedure nor an instance of a generic unit.

Reason: A null procedure cannot have the appropriate nonreturning semantics, as it does not raise an exception or loop forever. 

Ramification: {AI05-0229-1} {AI12-0269-1} The procedure can be abstract. If a nonreturning subprogram is renamed (anywhere) calls through the new name still have the nonreturning semantics. 

{AI95-00329-01} {AI95-00414-01} A return statement shall not apply to a nonreturning procedure or generic procedure.

{AI12-0269-1} Any return statement that applies to a nonreturning function or generic function shall be a [simple_return_statement](./AA-6.5#S0222) with an [expression](./AA-4.4#S0132) that is a [raise_expression](./AA-11.3#S0309), a call on a nonreturning function, or a parenthesized [expression](./AA-4.4#S0132) of one of these.

Ramification: We still require at least one return statement in a function; we just require that all such return statements don't actually return a value. 

{AI95-00414-01} {AI12-0269-1} A subprogram shall be nonreturning if it overrides a dispatching nonreturning subprogram. In addition to the places where Legality Rules normally apply (see 12.3), this rule applies also in the private part of an instance of a generic unit.

Reason: This ensures that dispatching calls to nonreturning subprograms will, in fact, not return. 

{AI95-00414-01} {AI12-0269-1} If a renaming-as-body completes a nonreturning subprogram declaration, then the renamed subprogram shall be nonreturning.

Reason: This ensures that no extra code is needed to implement the renames (that is, no wrapper is needed) as the body has the same property. 

Paragraph 8 was deleted. 


#### Dynamic Semantics

{AI95-00329-01} {AI95-00414-01} If the body of a nonreturning procedure completes normally, Program_Error is raised at the point of the call. 

Discussion: {AI12-0269-1} Note that there is no name for suppressing this check, since the check represents a bug, imposes no time overhead, and minimal space overhead (since it can usually be statically eliminated as dead code). We don't need a similar rule for nonreturning functions as this is standard semantics for all functions that normally complete without a transfer of control (such as a return statement). 

Implementation Note: If a nonreturning procedure tries to return, we raise Program_Error. This is stated as happening at the call site, because we do not wish to allow the procedure to handle the exception (and then, perhaps, try to return again!). However, the expected run-time model is that the compiler will generate raise Program_Error at the end of the procedure body (but not handleable by the procedure itself), as opposed to doing it at the call site. (This is just like the typical run-time model for functions that fall off the end without returning a value). The reason is indirect calls: in P.all(...);, the compiler cannot know whether P designates a nonreturning procedure or a normal one. Putting the raise Program_Error in the procedure's generated code solves this problem neatly.

Similarly, if one passes a nonreturning procedure to a generic formal parameter, the compiler cannot know this at call sites (in shared code implementations); the raise-in-body solution deals with this neatly. 


#### Examples

{AI12-0429-1} Example of a specification of a No_Return aspect: 

```ada
{AI95-00433-01} {AI05-0229-1} procedure Fail(Msg : String)  -- raises Fatal_Error exception
   with No_Return;
   -- Inform compiler and reader that procedure never returns normally

```


#### Extensions to Ada 95

{AI95-00329-01} {AI95-00414-01} [Pragma](./AA-2.8#S0019) No_Return is new. 


#### Extensions to Ada 2005

{AI05-0229-1} Aspect No_Return is new; [pragma](./AA-2.8#S0019) No_Return is now obsolescent. 


#### Incompatibilities With Ada 2012

{AI12-0423-1} Correction: Clarified that the nonreturning property is inherited by derivation. A literal implementation of the Ada 2012 would mean that no overriding would ever be rejected, as the inherited routine would never be nonreturning. Ada 2022 requires overridings of dispatching nonreturning subprograms to be rejected. This is formally incompatible, but practically such overridings have been rejected in practice. 


#### Extensions to Ada 2012

{AI12-0269-1} Aspect No_Return can now be applied to functions as well as procedures. 

