---
sidebar_position:  52
---

# 6.4  Subprogram Calls

A subprogram call is either a [procedure_call_statement](./AA-6.4#S0217) or a [function_call](./AA-6.4#S0218); [it invokes the execution of the [subprogram_body](./AA-6.3#S0216). The call specifies the association of the actual parameters, if any, with formal parameters of the subprogram.] 


#### Syntax

procedure_call_statement<a id="S0217"></a> ::= 
    procedure_[name](./AA-4.1#S0091);
  | procedure_[prefix](./AA-4.1#S0093) [actual_parameter_part](./AA-6.4#S0219);

function_call<a id="S0218"></a> ::= 
    function_[name](./AA-4.1#S0091)
  | function_[prefix](./AA-4.1#S0093) [actual_parameter_part](./AA-6.4#S0219)

To be honest: {AI05-0005-1} For the purpose of non-syntax rules, infix operator calls are considered [function_call](./AA-6.4#S0218)s. See 6.6. 

actual_parameter_part<a id="S0219"></a> ::= 
    ([parameter_association](./AA-6.4#S0220) {, [parameter_association](./AA-6.4#S0220)})

parameter_association<a id="S0220"></a> ::= 
   [formal_parameter_[selector_name](./AA-4.1#S0099) =&gt] [explicit_actual_parameter](./AA-6.4#S0221)

explicit_actual_parameter<a id="S0221"></a> ::= [expression](./AA-4.4#S0132) | variable_[name](./AA-4.1#S0091)

{AI12-0416-1} A [parameter_association](./AA-6.4#S0220) is named or positional according to whether or not the formal_parameter_[selector_name](./AA-4.1#S0099) is specified. For the [parameter_association](./AA-6.4#S0220)s of a single [actual_parameter_part](./AA-6.4#S0219) or [iterator_actual_parameter_part](./AA-5.5#S0188), any positional associations shall precede any named associations. Named associations are not allowed if the [prefix](./AA-4.1#S0093) in a subprogram call is an [attribute_reference](./AA-4.1#S0100). 

Ramification: This means that the formal parameter names used in describing predefined attributes are to aid presentation of their semantics, but are not intended for use in actual calls. 


#### Name Resolution Rules

{AI95-00310-01} The [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) given in a [procedure_call_statement](./AA-6.4#S0217) shall resolve to denote a callable entity that is a procedure, or an entry renamed as (viewed as) a procedure. The [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) given in a [function_call](./AA-6.4#S0218) shall resolve to denote a callable entity that is a function. The [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) shall not resolve to denote an abstract subprogram unless it is also a dispatching subprogram. [When there is an [actual_parameter_part](./AA-6.4#S0219), the [prefix](./AA-4.1#S0093) can be an [implicit_dereference](./AA-4.1#S0095) of an access-to-subprogram value.] 

Discussion: {AI95-00310-01} This rule is talking about dispatching operations (which is a static concept) and not about dispatching calls (which is a dynamic concept). 

Ramification: The function can be an operator, enumeration literal, attribute that is a function, etc. 

A subprogram call shall contain at most one association for each formal parameter. Each formal parameter without an association shall have a [default_expression](./AA-3.7#S0063) (in the profile of the view denoted by the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093)). [This rule is an overloading rule (see 8.6).] 

Proof: {AI05-0240-1} All Name Resolution Rules are overloading rules, see 8.6. 


#### Static Semantics

{AI95-00407-01} {AI12-0416-1} If the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) of a subprogram call denotes a prefixed view (see 4.1.3), the subprogram call is equivalent to a call on the underlying subprogram, with the first actual parameter being provided by the [prefix](./AA-4.1#S0093) of the prefixed view (or the Access attribute of this [prefix](./AA-4.1#S0093) if the first formal parameter is an access parameter), and the remaining actual parameters given by the [actual_parameter_part](./AA-6.4#S0219), if any. 


#### Dynamic Semantics

{AI95-00345-01} For the execution of a subprogram call, the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) of the call is evaluated, and each [parameter_association](./AA-6.4#S0220) is evaluated (see 6.4.1). If a [default_expression](./AA-3.7#S0063) is used, an implicit [parameter_association](./AA-6.4#S0220) is assumed for this rule. These evaluations are done in an arbitrary order. The [subprogram_body](./AA-6.3#S0216) is then executed, or a call on an entry or protected subprogram is performed (see 3.9.2). Finally, if the subprogram completes normally, then after it is left, any necessary assigning back of formal to actual parameters occurs (see 6.4.1).

Discussion: The implicit association for a default is only for this run-time rule. At compile time, the visibility rules are applied to the default at the place where it occurs, not at the place of a call. 

To be honest: If the subprogram is inherited, see 3.4, "Derived Types and Classes".

If the subprogram is protected, see 9.5.1, "Protected Subprograms and Protected Actions".

If the subprogram is really a renaming of an entry, see 9.5.3, "Entry Calls".

{AI95-00345-01} If the subprogram is implemented by an entry or protected subprogram, it will be treated as a dispatching call to the corresponding entry (see 9.5.3, "Entry Calls") or protected subprogram (see 9.5.1, "Protected Subprograms and Protected Actions").

{AI95-00348-01} Normally, the [subprogram_body](./AA-6.3#S0216) that is executed by the above rule is the one for the subprogram being called. For an enumeration literal, implicitly declared (but noninherited) subprogram, null procedure, or an attribute that is a subprogram, an implicit body is assumed. For a dispatching call, 3.9.2, "Dispatching Operations of Tagged Types" defines which [subprogram_body](./AA-6.3#S0216) is executed. 

{AI95-00318-02} The exception Program_Error is raised at the point of a [function_call](./AA-6.4#S0218) if the function completes normally without executing a return statement. 

Discussion: We are committing to raising the exception at the point of call, for uniformity - see AI83-00152. This happens after the function is left, of course.

Note that there is no name for suppressing this check, since the check imposes no time overhead and minimal space overhead (since it can usually be statically eliminated as dead code). 

{AI95-00231-01} A [function_call](./AA-6.4#S0218) denotes a constant, as defined in 6.5; the nominal subtype of the constant is given by the nominal subtype of the function result. 


#### Examples

Examples of procedure calls: 

```ada
Traverse_Tree;                                               --  see 6.1
Print_Header(128, Title, True);                              --  see 6.1

```

```ada
Switch(From =&gt X, To =&gt Next);                               --  see 6.1
Print_Header(128, Header =&gt Title, Center =&gt True);          --  see 6.1
Print_Header(Header =&gt Title, Center =&gt True, Pages =&gt 128); --  see 6.1

```

Examples of function calls: 

```ada
Dot_Product(U, V)   --  see 6.1 and 6.3
Clock               --  see 9.6
F.all               --  presuming F is of an access-to-subprogram type - see 3.10

```

Examples of procedures with default expressions: 

```ada
procedure Activate(Process : in Process_Name;
                   After   : in Process_Name := No_Process;
                   Wait    : in Duration := 0.0;
                   Prior   : in Boolean := False);

```

```ada
{AI05-0299-1} procedure Pair(Left, Right : in Person_Name := new Person(M));   --  see 3.10.1

```

Examples of their calls: 

```ada
Activate(X);
Activate(X, After =&gt Y);
Activate(X, Wait =&gt 60.0, Prior =&gt True);
Activate(X, Y, 10.0, False);

```

```ada
{AI05-0299-1} Pair;
Pair(Left =&gt new Person(F), Right =&gt new Person(M));

```

NOTE 1   If a [default_expression](./AA-3.7#S0063) is used for two or more parameters in a multiple [parameter_specification](./AA-6.1#S0207), the [default_expression](./AA-3.7#S0063) is evaluated once for each omitted parameter. Hence in the above examples, the two calls of Pair are equivalent. 


#### Examples

Examples of overloaded subprograms: 

```ada
procedure Put(X : in Integer);
procedure Put(X : in String);

```

```ada
procedure Set(Tint   : in Color);
procedure Set(Signal : in Light);

```

Examples of their calls: 

```ada
Put(28);
Put("no possible ambiguity here");

```

```ada
Set(Tint   =&gt Red);
Set(Signal =&gt Red);
Set(Color'(Red));

```

```ada
{AI12-0440-1} --  Set(Red) would be ambiguous since Red can
--  denote a value either of type Color or of type Light

```


#### Wording Changes from Ada 83

We have gotten rid of parameters "of the form of a type conversion" (see RM83-6.4.1(3)). The new view semantics of [type_conversion](./AA-4.6#S0162)s allows us to use normal [type_conversion](./AA-4.6#S0162)s instead.

We have moved wording about run-time semantics of parameter associations to 6.4.1.

We have moved wording about raising Program_Error for a function that falls off the end to here from RM83-6.5. 


#### Extensions to Ada 95

{AI95-00310-01} Nondispatching abstract operations are no longer considered when resolving a subprogram call. That makes it possible to use abstract to "undefine" a predefined operation for an untagged type. That's especially helpful when defining custom arithmetic packages. 


#### Wording Changes from Ada 95

{AI95-00231-01} Changed the definition of the nominal subtype of a [function_call](./AA-6.4#S0218) to use the nominal subtype wording of 6.1, to take into account [null_exclusion](./AA-3.10#S0083)s and access result types.

{AI95-00345-01} Added wording to clarify that the meaning of a call on a subprogram "implemented by" an entry or protected operation is defined by 3.9.2.

{AI95-00407-01} Defined the meaning of a call on a prefixed view of a subprogram (see 4.1.3). 


#### Wording Changes from Ada 2012

{AI12-0416-1} Moved the equivalence definition for a prefixed view to Static Semantics, in order that it is clear Legality Rules that apply to calls and the parameters of calls apply to prefixed views (including the [prefix](./AA-4.1#S0093)). 


## 6.4.1  Parameter Associations

[ A parameter association defines the association between an actual parameter and a formal parameter.] 


#### Language Design Principles

The parameter passing rules for out parameters are designed to ensure that the parts of a type that have implicit initial values (see 3.3.1) don't become "de-initialized" by being passed as an out parameter.

{AI05-0142-4} For explicitly aliased parameters of functions, we will ensure at the call site that a part of the parameter can be returned as part of the function result without creating a dangling pointer. We do this with accessibility checks at the call site that all actual objects of explicitly aliased parameters live at least as long as the function result; then we can allow them to be returned as access discriminants or anonymous access results, as those have the master of the function result. 


#### Name Resolution Rules

{AI05-0118-1} The formal_parameter_[selector_name](./AA-4.1#S0099) of a named [parameter_association](./AA-6.4#S0220) shall resolve to denote a [parameter_specification](./AA-6.1#S0207) of the view being called; this is the formal parameter of the association. The formal parameter for a positional [parameter_association](./AA-6.4#S0220) is the parameter with the corresponding position in the formal part of the view being called.

To be honest: {AI05-0118-1} For positional parameters, the "corresponding position" is calculated after any transformation of prefixed views. 

The actual parameter is either the [explicit_actual_parameter](./AA-6.4#S0221) given in a [parameter_association](./AA-6.4#S0220) for a given formal parameter, or the corresponding [default_expression](./AA-3.7#S0063) if no [parameter_association](./AA-6.4#S0220) is given for the formal parameter. The expected type for an actual parameter is the type of the corresponding formal parameter. 

To be honest: The corresponding [default_expression](./AA-3.7#S0063) is the one of the corresponding formal parameter in the profile of the view denoted by the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) of the call. 

If the mode is in, the actual is interpreted as an [expression](./AA-4.4#S0132); otherwise, the actual is interpreted only as a [name](./AA-4.1#S0091), if possible. 

Ramification: {AI12-0005-1} This formally resolves the ambiguity present in the syntax rule for [explicit_actual_parameter](./AA-6.4#S0221). This matters as an [expression](./AA-4.4#S0132) that is a [name](./AA-4.1#S0091) is evaluated and represents a value while a [name](./AA-4.1#S0091) by itself can be an object; if the mode is not in, we want the parameter to interpreted as an object. Note that we don't actually require that the actual be a [name](./AA-4.1#S0091) if the mode is not in; we do that below.

{AI12-0005-1} This wording uses "interpreted as" rather than "shall be" so that this rule is not used to resolve overloading; it is solely about evaluation as described above. We definitely do not want to allow oddities like the presence of parentheses requiring the selection of an in formal parameter as opposed to an otherwise matching in out parameter. 


#### Legality Rules

If the mode is in out or out, the actual shall be a [name](./AA-4.1#S0091) that denotes a variable. 

Discussion: {AI12-0005-1} We no longer need "or a [type_conversion](./AA-4.6#S0162) whose argument is the [name](./AA-4.1#S0091) of a variable", because a [type_conversion](./AA-4.6#S0162) is now a [name](./AA-4.1#S0091), and a view conversion of a variable is a variable while any other conversion (which should not be legal here) is a constant. 

Reason: The requirement that the actual be a (variable) [name](./AA-4.1#S0091) is not an overload resolution rule, since we don't want the difference between [expression](./AA-4.4#S0132) and [name](./AA-4.1#S0091) to be used to resolve overloading. For example: 

```ada
procedure Print(X : in Integer; Y : in Boolean := True);
procedure Print(Z : in out Integer);
. . .
Print(3); -- Ambiguous!
  

```

The above call to Print is ambiguous even though the call is not compatible with the second Print which requires an actual that is a (variable) [name](./AA-4.1#S0091) ("3" is an [expression](./AA-4.4#S0132), not a [name](./AA-4.1#S0091)). This requirement is a legality rule, so overload resolution fails before it is considered, meaning that the call is ambiguous. 

{AI12-0074-1} {AI12-0159-1} {AI12-0377-1} If the mode is out, the actual parameter is a view conversion, and the type of the formal parameter is a scalar type, then 

neither the target type nor the operand type has the Default_Value aspect specified; or

both the target type and the operand type shall have the Default_Value aspect specified, and there shall exist a type (other than a root numeric type) that is an ancestor of both the target type and the operand type. 

{AI12-0074-1} {AI12-0159-1} In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit.

Reason: These rules are needed in order to ensure that a well-defined parameter value is passed. 

{AI05-0102-1} {AI05-0142-4} If the formal parameter is an explicitly aliased parameter, the type of the actual parameter shall be tagged or the actual parameter shall be an aliased view of an object. Further, if the formal parameter subtype F is untagged:

the subtype F shall statically match the nominal subtype of the actual object; or

the subtype F shall be unconstrained, discriminated in its full view, and unconstrained in any partial view. 

Ramification: Tagged objects (and tagged [aggregate](./AA-4.3#S0106)s for in parameters) do not need to be aliased. This matches the behavior of unaliased formal parameters of tagged types, which allow 'Access to be taken of the formal parameter regardless of the form of the actual parameter. 

Reason: We need the subtype check on untagged actual parameters so that the requirements of 'Access are not lost. 'Access makes its checks against the nominal subtype of its prefix, and parameter passing can change that subtype. But we don't want this parameter passing to change the objects that would be allowed as the prefix of 'Access. This is particularly important for arrays, where we don't want to require any additional implementation burden. 

Discussion: {AI12-0095-1} {AI12-0005-1} We assume the worst in a generic body regarding whether a formal subtype has a constrained partial view; specifically, in a generic body a discriminated subtype is considered to have a constrained partial view if it is a descendant of an untagged generic formal private or derived type (see 12.5.1 for the formal definition of this rule). 

{AI12-0095-1} In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit.

{AI05-0142-4} {AI05-0234-1} In a function call, the accessibility level of the actual object for each explicitly aliased parameter shall not be statically deeper than the accessibility level of the master of the call (see 3.10.2).

Discussion: Since explicitly aliased parameters are either tagged or required to be objects, there is always an object (possibly anonymous) to talk about. This is discussing the static accessibility level of the actual object; it does not depend on any runtime information (for instance when the actual object is a formal parameter of another subprogram, it does not depend on the actual parameter of that other subprogram). 

Ramification: {AI12-0095-1} This accessibility check (and its dynamic cousin as well) can only fail if the master of the function call (which is defined in the Heart of Darkness, or 3.10.2 if you prefer) is different than the master directly enclosing the call. The most likely place where this will occur is in the initializer of an [allocator](./AA-4.8#S0164); in almost all other cases this check will always pass. 

{AI05-0144-2} Two [name](./AA-4.1#S0091)s are known to denote the same object if:

both [name](./AA-4.1#S0091)s statically denote the same stand-alone object or parameter; or

both [name](./AA-4.1#S0091)s are [selected_component](./AA-4.1#S0098)s, their [prefix](./AA-4.1#S0093)es are known to denote the same object, and their [selector_name](./AA-4.1#S0099)s denote the same component; or

both [name](./AA-4.1#S0091)s are dereferences (implicit or explicit) and the dereferenced [name](./AA-4.1#S0091)s are known to denote the same object; or

both [name](./AA-4.1#S0091)s are [indexed_component](./AA-4.1#S0096)s, their [prefix](./AA-4.1#S0093)es are known to denote the same object, and each of the pairs of corresponding index values are either both static expressions with the same static value or both [name](./AA-4.1#S0091)s that are known to denote the same object; or

both [name](./AA-4.1#S0091)s are [slice](./AA-4.1#S0097)s, their [prefix](./AA-4.1#S0093)es are known to denote the same object, and the two [slice](./AA-4.1#S0097)s have statically matching index constraints; or

one of the two [name](./AA-4.1#S0091)s statically denotes a renaming declaration whose renamed object_[name](./AA-4.1#S0091) is known to denote the same object as the other, the [prefix](./AA-4.1#S0093) of any dereference within the renamed object_[name](./AA-4.1#S0091) is not a variable, and any [expression](./AA-4.4#S0132) within the renamed object_[name](./AA-4.1#S0091) contains no references to variables nor calls on nonstatic functions. 

Reason: This exposes known renamings of slices, indexing, and so on to this definition. In particular, if we have 

```ada
C : Character renames S(1);

```

then C and S(1) are known to denote the same object.

We need the requirement that no variables occur in the [prefix](./AA-4.1#S0093)es of dereferences and in (index) [expression](./AA-4.4#S0132)s of the renamed object in order to avoid problems from later changes to those parts of renamed names. Consider:

```ada
   type Ref is access Some_Type;
   Ptr : Ref := new Some_Type'(...);
   X : Some_Type renames Ptr.all;
begin
   Ptr := new Some_Type'(...);
   P (Func_With_Out_Params (Ptr.all), X);

```

X and Ptr.all should not be known to denote the same object, since they denote different allocated objects (and this is not an unreasonable thing to do). 

To be honest: The exclusion of variables from renamed object_names is not enough to prevent altering the value of the name or expression by another access path. For instance, both in parameters passed by reference and access-to-constant values can designate variables. For the intended use of "known to be the same object", this is OK; the modification via another access path is very tricky and it is OK to reject code that would be buggy except for the tricky code. Assuming Element is an elementary type, consider the following example: 

```ada
Global : Tagged_Type;

```

```ada
procedure Foo (Param : in Tagged_Type := Global) is
   X : Element renames Some_Global_Array (Param.C);
begin
   Global.C := Global.C + 1;
   Swap (X, Some_Global_Array (Param.C));

```

The rules will flag the call of procedure Swap as illegal, since X and Some_Global_Array (Parameter.C) are known to denote the same object (even though they will actually represent different objects if Param = Global). But this is only incorrect if the parameter actually is Global and not some other value; the error could exist for some calls. So this flagging seems harmless.

Similar examples can be constructed using stand-alone composite constants with controlled or immutably limited components, and (as previously noted) with dereferences of access-to-constant values. Even when these examples flag a call incorrectly, that call depends on very tricky code (modifying the value of a constant); the code is likely to confuse future maintainers as well and thus we do not mind rejecting it. 

Discussion: Whether or not [name](./AA-4.1#S0091)s or [prefix](./AA-4.1#S0093)es are known to denote the same object is determined statically. If the name contains some dynamic portion other than a dereference, [indexed_component](./AA-4.1#S0096), or [slice](./AA-4.1#S0097), it is not "known to denote the same object".

These rules make no attempt to handle slices of objects that are known to be the same when the slices have dynamic bounds (other than the trivial case of bounds being defined by the same subtype), even when the bounds could be proven to be the same, as it is just too complex to get right and these rules are intended to be conservative. 

Ramification: "Known to denote the same object" is intended to be an equivalence relationship, that is, it is reflexive, symmetric, and transitive. We believe this follows from the rules. For instance, given the following declarations: 

```ada
S   : String(1..10);
ONE : constant Natural := 1;
R   : Character renames S(1);

```

the names R and S(1) are known to denote the same object by the sixth bullet, and S(1) and S(ONE) are known to denote the same object by the fourth bullet, so using the sixth bullet on R and S(ONE), we simply have to test S(1) vs. S(ONE), which we already know denote the same object. 

{AI05-0144-2} Two [name](./AA-4.1#S0091)s are known to refer to the same object if 

The two [name](./AA-4.1#S0091)s are known to denote the same object; or

One of the [name](./AA-4.1#S0091)s is a [selected_component](./AA-4.1#S0098), [indexed_component](./AA-4.1#S0096), or [slice](./AA-4.1#S0097) and its [prefix](./AA-4.1#S0093) is known to refer to the same object as the other [name](./AA-4.1#S0091); or

One of the two [name](./AA-4.1#S0091)s statically denotes a renaming declaration whose renamed object_[name](./AA-4.1#S0091) is known to refer to the same object as the other [name](./AA-4.1#S0091). 

Reason: This ensures that names Prefix.Comp and Prefix are known to refer to the same object for the purposes of the rules below. This intentionally does not include dereferences; we only want to worry about accesses to the same object, and a dereference changes the object in question. (There is nothing shared between an access value and the object it designates.) 

{AI05-0144-2} If a call C has two or more parameters of mode in out or out that are of an elementary type, then the call is legal only if:

{AI12-0216-1} {AI12-0324-1} For each [name](./AA-4.1#S0091) N denoting an object of an elementary type that is passed as a parameter of mode in out or out to the call C, there is no other [name](./AA-4.1#S0091) among the other parameters of mode in out or out to C that is known to denote the same object.

To be honest: This means visibly an elementary type; it does not include partial views of elementary types (partial views are always composite). That's necessary to avoid having Legality Rules depend on the contents of the private part. 

{AI05-0144-2} If a construct C has two or more direct constituents that are [name](./AA-4.1#S0091)s or [expression](./AA-4.4#S0132)s whose evaluation may occur in an arbitrary order, at least one of which contains a function call with an in out or out parameter, then the construct is legal only if:

Ramification: All of the places where the language allows an arbitrary order can be found by looking in the index under "arbitrary order, allowed". Note that this listing includes places that don't involve [name](./AA-4.1#S0091)s or [expression](./AA-4.4#S0132)s (such as checks or finalization). 

For each name N that is passed as a parameter of mode in out or out to some inner function call C2 (not including the construct C itself), there is no other [name](./AA-4.1#S0091) anywhere within a direct constituent of the construct C other than the one containing C2, that is known to refer to the same object. 

Ramification: This requirement cannot fail for a procedure or entry call alone; there must be at least one function with an in out or out parameter called as part of a parameter expression of the call in order for it to fail. 

Reason: These rules prevent obvious cases of dependence on the order of evaluation of [name](./AA-4.1#S0091)s or [expression](./AA-4.4#S0132)s. Such dependence is usually a bug, and in any case, is not portable to another implementation (or even another optimization setting).

In the case that the top-level construct C is a call, these rules do not require checks for most in out parameters, as the rules about evaluation of calls prevent problems. Similarly, we do not need checks for short circuit operations or other operations with a defined order of evaluation. The rules about arbitrary order (see ) allow evaluating parameters and writing parameters back in an arbitrary order, but not interleaving of evaluating parameters of one call with writing parameters back from another - that would not correspond to any allowed sequential order. 

{AI05-0144-2} For the purposes of checking this rule:

For an array [aggregate](./AA-4.3#S0106), an [expression](./AA-4.4#S0132) associated with a [discrete_choice_list](./AA-3.8#S0073) that has two or more discrete choices, or that has a nonstatic range, is considered as two or more separate occurrences of the [expression](./AA-4.4#S0132);

For a record [aggregate](./AA-4.3#S0106):

The [expression](./AA-4.4#S0132) of a [record_component_association](./AA-4.3#S0109) is considered to occur once for each associated component; and

The [default_expression](./AA-3.7#S0063) for each [record_component_association](./AA-4.3#S0109) with &lt&gt for which the associated component has a [default_expression](./AA-3.7#S0063) is considered part of the [aggregate](./AA-4.3#S0106); 

For a call, any [default_expression](./AA-3.7#S0063) evaluated as part of the call is considered part of the call. 

Ramification: We do not check expressions that are evaluated only because of a component initialized by default in an aggregate (via &lt&gt). 


#### Dynamic Semantics

For the evaluation of a [parameter_association](./AA-6.4#S0220): 

The actual parameter is first evaluated.

For an access parameter, the [access_definition](./AA-3.10#S0084) is elaborated, which creates the anonymous access type.

For a parameter [(of any mode)] that is passed by reference (see 6.2), a view conversion of the actual parameter to the nominal subtype of the formal parameter is evaluated, and the formal parameter denotes that conversion. 

Discussion: We are always allowing sliding, even for [in] out by-reference parameters. 

For an in or in out parameter that is passed by copy (see 6.2), the formal parameter object is created, and the value of the actual parameter is converted to the nominal subtype of the formal parameter and assigned to the formal. 

Ramification: The conversion mentioned here is a value conversion. 

For an out parameter that is passed by copy, the formal parameter object is created, and: 

{AI05-0153-3} {AI05-0196-1} {AI12-0378-1} For an access type, the formal parameter is initialized from the value of the actual, without checking whether the value satisfies any constraints, predicates, or null exclusions, but including any[ dynamic] accessibility checks associated with a conversion to the type of the formal parameter.

Reason: This preserves the Language Design Principle that an object of an access type is always initialized with a "reasonable" value. 

Ramification: {AI12-0378-1} The permission to pass null (see below) can be used in any case where an accessibility check could fail, rather than making a check. 

{AI05-0153-3} {AI05-0228-1} {AI12-0074-1} {AI12-0159-1} {AI12-0377-1} For a scalar type that has the Default_Value aspect specified, the formal parameter is initialized from the value of the actual, without checking that the value satisfies any constraint or any predicate.

Reason: This preserves the Language Design Principle that all objects of a type with an implicit initial value are initialized. This is important so that a programmer can guarantee that all objects of a scalar type have a valid value with a carefully chosen Default_Value. 

Implementation Note: This rule means that out parameters of a subtype T with a specified Default_Value need to be large enough to support any possible value of the base type of T. In contrast, a type that does not have a Default_Value only need support the size of the subtype (since no values are passed in). 

{AI12-0333-1} For a composite type with discriminants or that has implicit initial values for any subcomponents (see 3.3.1), the behavior is as for an in out parameter passed by copy[, except that no predicate check is performed]. 

Reason: This ensures that no part of an object of such a type can become "de-initialized" by being part of an out parameter. 

Ramification: This includes an array type whose component type is an access type, and a record type with a component that has a [default_expression](./AA-3.7#S0063), among other things. 

Proof: {AI12-0333-1} No predicate check follows from the definition of subtype conversion in 4.6. 

{AI12-0439-1} For any other type, the formal parameter is uninitialized. If composite, a view conversion of the actual parameter to the nominal subtype of the formal is evaluated [(which can raise Constraint_Error)], and the actual subtype of the formal is that of the view conversion. If elementary, the actual subtype of the formal is given by its nominal subtype. 

Ramification: {AI05-0228-1} This case covers scalar types that do not have Default_Value specified, and composite types whose subcomponent's subtypes do not have any implicit initial values. The view conversion for composite types ensures that if the lengths don't match between an actual and a formal array parameter, the Constraint_Error is raised before the call, rather than after. 

{AI12-0377-1} Furthermore, if the type is a scalar type, and the actual parameter is a view conversion, then Program_Error is raised if either the target or the operand type has the Default_Value aspect specified, unless they both have the Default_Value aspect specified, and there is a type (other than a root numeric type) that is an ancestor of both the target type and the operand type.

Discussion: This can only occur in the body of an instance of a generic unit. Legality Rules will catch all other cases. Implementations that macro-expand generics can always detect this case when the enclosing instance body is expanded. 

{AI05-0142-4} {AI05-0234-1} In a function call, for each explicitly aliased parameter, a check is made that the accessibility level of the master of the actual object is not deeper than that of the  master of the call (see 3.10.2). 

Ramification: If the actual object to a call C is a formal parameter of some function call F, no dynamic check against the master of the actual parameter of F is necessary. Any case which could fail the dynamic check is already statically illegal (either at the call site of F, or at the call site C). This is important, as it would require nasty distributed overhead to accurately know the dynamic accessibility of a formal parameter (all tagged and explicitly aliased parameters would have to carry accessibility levels). 

A formal parameter of mode in out or out with discriminants is constrained if either its nominal subtype or the actual parameter is constrained.

After normal completion and leaving of a subprogram, for each in out or out parameter that is passed by copy, the value of the formal parameter is converted to the subtype of the variable given as the actual parameter and assigned to it. These conversions and assignments occur in an arbitrary order. 

Ramification: The conversions mentioned above during parameter passing might raise Constraint_Error - (see 4.6). 

Ramification: If any conversion or assignment as part of parameter passing propagates an exception, the exception is raised at the place of the subprogram call; that is, it cannot be handled inside the [subprogram_body](./AA-6.3#S0216). 

Proof: Since these checks happen before or after executing the [subprogram_body](./AA-6.3#S0216), the execution of the [subprogram_body](./AA-6.3#S0216) does not dynamically enclose them, so it can't handle the exceptions. 

Discussion: The variable we're talking about is the one denoted by the variable_[name](./AA-4.1#S0091) given as the [explicit_actual_parameter](./AA-6.4#S0221). If this variable_[name](./AA-4.1#S0091) is a [type_conversion](./AA-4.6#S0162), then the rules in 4.6 for assigning to a view conversion apply. That is, if X is of subtype S1, and the actual is S2(X), the above-mentioned conversion will convert to S2, and the one mentioned in 4.6 will convert to S1. 


#### Erroneous Execution

{AI05-0008-1} If the nominal subtype of a formal parameter with discriminants is constrained or indefinite, and the parameter is passed by reference, then the execution of the call is erroneous if the value of any discriminant of the actual is changed while the formal parameter exists (that is, before leaving the corresponding callable construct). 


#### Implementation Permissions

{AI12-0378-1} If the actual parameter in a [parameter_association](./AA-6.4#S0220) with mode out is a view conversion between two access types that do not share a common ancestor type, the implementation may pass in the null value of the type of the formal parameter instead of the value of the actual parameter. It is implementation-defined under what circumstances the implementation passes in the null value. 

Implementation defined: The circumstances in which the implementation passes in the null value for a view conversion of an access type used as an out parameter.


#### Extensions to Ada 83

In Ada 95, a program can rely on the fact that passing an object as an out parameter does not "de-initialize" any parts of the object whose subtypes have implicit initial values. (This generalizes the RM83 rule that required copy-in for parts that were discriminants or of an access type.) 


#### Wording Changes from Ada 83

{AI05-0299-1} We have eliminated the subclause on Default Parameters, as it is subsumed by earlier subclauses. 


#### Inconsistencies With Ada 2005

{AI05-0196-1} Correction: Clarified that out parameters of an access type are not checked for null exclusions when they are passed in (which is similar to the behavior for constraints). This was unspecified in Ada 2005, so a program which depends on the behavior of an implementation which does check the exclusion may malfunction. But a program depending on an exception being raised is unlikely. 


#### Incompatibilities With Ada 2005

{AI05-0144-2} Additional rules have been added to make illegal passing the same elementary object to more than one in out or out parameters of the same call. In this case, the result in the object could depend on the compiler version, optimization settings, and potentially the phase of the moon, so this check will mostly reject programs that are nonportable and could fail with any change. Even when the result is expected to be the same in both parameters, the code is unnecessarily tricky. Programs which fail this new check should be rare and are easily fixed by adding a temporary object. 


#### Wording Changes from Ada 2005

{AI05-0008-1} Correction: A missing rule was added to cover cases that were missed in Ada 95 and Ada 2005; specifically, that an in parameter passed by reference might have its discriminants changed via another path. Such cases are erroneous as requiring compilers to detect such errors would be expensive, and requiring such cases to work would be a major change of the user model (in parameters with discriminants could no longer be assumed constant). This is not an inconsistency, as compilers are not required to change any current behavior.

{AI05-0102-1} Correction: Moved implicit conversion Legality Rule to 8.6.

{AI05-0118-1} Correction: Added a definition for positional parameters, as this is missing from Ada 95 and later.

{AI05-0142-4} Rules have been added defining the legality and dynamic checks needed for explicitly aliased parameters (see 6.1).

{AI05-0144-2} Additional rules have been added such that passing an object to an in out or out parameter of a function is illegal if it is used elsewhere in a construct which allows evaluation in an arbitrary order. Such calls are not portable (since the results may depend on the evaluation order), and the results could even vary because of optimization settings and the like. Thus they've been banned. 


#### Inconsistencies With Ada 2012

{AI12-0378-1} Correction: Added a permission to pass null so that value passed into an out parameter for access types is well-defined in the case of a view conversion. Null may be passed for any view conversion between unrelated access types; this is important for conversions that may have problematic accessibility or tags. If the permission is used and the out parameter is read before it is written (perhaps to read a bound or discriminant), Constraint_Error may be raised by Ada 2022 when it would not have been in Ada 2012. Additionally, if the called subprogram does not write the out parameter at all, the actual object will be overwritten with null (and possibly raise Constraint_Error if the object is null excluding), while the object would be unchanged in Ada 2012. Such cases are thought to be rare, as most out parameters of access types are overwritten before being read. In addition, at least one widely-used Ada compiler already passes null in these cases. 


#### Incompatibilities With Ada 2012

{AI12-0074-1} {AI12-0159-1} {AI12-0377-1} {AI12-0378-1} Corrigendum: Added rules to ensure that the value passed into an out parameter for scalar types is well-defined in the case of a view conversion. The new rules can be incompatible. View conversions from/to an unrelated type with the Default_Value aspect specified are unlikely to occur in existing code, as the aspect is new in Ada 2012. Declaring and passing a temporary rather than a view conversion will eliminate the problem.

{AI12-0095-1} Corrigendum: Because of a rule added in 12.5.1, the checks for the passing of an object to an explicitly aliased parameter in a generic body were strengthened to use an assume the worst rule. This case is rather unlikely as a formal private or derived type with discriminants is required along with an explicitly aliased parameter whose type doesn't statically match the formal type. Such a program is very unlikely, especially as explicitly aliased parameters are a new Ada 2012 feature. 


#### Wording Changes from Ada 2012

{AI12-0216-1} Correction: The in out parameter rule only applies to actual parameters of elementary types. While this allows additional programs (and thus could be considered an extension), it is unlikely to change anything in a real program (it could only matter in a call with 4 or more parameters, and then only if two composite parameters have matching actuals). Thus we document it as a wording change.

{AI12-0333-1} Correction: Predicate checks are not made for inbound out parameters. The actual rule change that has this effect is found in 4.6 and is documented there. 

