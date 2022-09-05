---
sidebar_position:  189
---

# H.7  Extensions to Global and Global'Class Aspects

{AI12-0079-3} In addition to the entities specified in 6.1.2, the Global aspect may be specified for a subtype (including a formal subtype), formal package, formal subprogram, and formal object of an anonymous access-to-subprogram type. 


#### Syntax

{AI12-0079-3} {AI12-0380-1} The following additional syntax is provided to override the mode of a formal parameter to reflect indirect effects on variables reachable from the formal parameter by one or more access-value dereferences:

extended_global_mode<a id="S0361"></a> ::= 
    overriding [basic_global_mode](./AA-6.1#S0212)


#### Name Resolution Rules

{AI12-0079-3} The object_[name](./AA-4.1#S0091) that is associated with an overriding mode shall resolve to statically denote a formal object, or a formal parameter of the associated entity. 


#### Static Semantics

{AI12-0079-3} The presence of the reserved word overriding in a global mode indicates that the specification is overriding the mode of a formal parameter with another mode to reflect the overall effect of an invocation of the callable entity on the state associated with the corresponding actual parameter.

{AI12-0380-1} [As described in 6.1.2, the following rules are defined in terms of operations that are performed by or on behalf of an entity.]

{AI12-0079-3} {AI12-0431-1} {AI12-0439-1} The Global aspect for a subtype identifies the global variables that can be referenced during default initialization, adjustment as part of assignment, finalization of an object of the subtype, or conversion to the subtype, including the evaluation of any assertion expressions that apply. If not specified for the first subtype of a derived type, the aspect defaults to that of the ancestor subtype; if not specified for a nonderived composite first subtype the aspect defaults to that of the enclosing library unit; if not specified for a nonderived elementary first subtype (or scalar base subtype), the aspect defaults to null in the absence of a predicate (or when the predicate is statically True), and to that of the enclosing library unit otherwise. If not specified for a nonfirst subtype S, the Global aspect defaults to that of the subtype identified in the [subtype_indication](./AA-3.2#S0027) defining S.

{AI12-0079-3} The Global'Class aspect may be specified for the first subtype of a tagged type T, indicating an upper bound on the Global aspect of any descendant of T. If not specified, it defaults to Unspecified. 


#### Legality Rules

{AI12-0079-3} For a tagged subtype T, each mode of its Global aspect shall identify a subset of the variables identified either by the corresponding mode, or by the in out mode, of the Global'Class aspect of the first subtype of any ancestor of T. 


#### Extensions to Ada 2012

{AI12-0079-3} {AI12-0380-1} These extensions to the Global aspect are new. 


## H.7.1  The Use_Formal and Dispatching Aspects

{AI12-0380-1} The Use_Formal and Dispatching aspects are provided to more precisely describe the use of generic formal parameters and dispatching calls within the execution of an operation, enabling more precise checking of conformance with the Nonblocking and global aspects that apply at the point of invocation of the operation.

{AI12-0079-3} {AI12-0380-1} For any declaration within a generic unit for which a global or Nonblocking aspect may be specified, other than a [generic_formal_parameter_declaration](./AA-12.1#S0314), the following aspect may be specified to indicate which generic formal parameters are used by the associated entity:

{AI12-0380-1} Use_FormalThe aspect is specified with a [formal_parameter_set](./AA-H.7#S0362), with the following form:

Aspect Description for Use_Formal: Generic formal parameters used in the implementation of an entity.

formal_parameter_set<a id="S0362"></a> ::= 
    [formal_group_designator](./AA-H.7#S0363)
  | [formal_parameter_name](./AA-H.7#S0364)
  | ([formal_parameter_name](./AA-H.7#S0364){, [formal_parameter_name](./AA-H.7#S0364)})

formal_group_designator<a id="S0363"></a> ::= null | all

formal_parameter_name<a id="S0364"></a> ::= 
    formal_[subtype_mark](./AA-3.2#S0028)
  | formal_subprogram_[name](./AA-4.1#S0091)
  | formal_access_to_subprogram_object_[name](./AA-4.1#S0091)

{AI12-0079-3} {AI12-0380-1} For any declaration for which a global or Nonblocking aspect may be specified, other than for a library package, a generic library package, or a generic formal, the following aspect may be specified:

{AI12-0380-1} Dispatching The aspect is specified with a [dispatching_operation_set](./AA-H.7#S0365), with the following form:

Aspect Description for Dispatching: Generic formal parameters used in the implementation of an entity.

dispatching_operation_set<a id="S0365"></a> ::= 
    [dispatching_operation_specifier](./AA-H.7#S0366)
  | ([dispatching_operation_specifier](./AA-H.7#S0366){, [dispatching_operation_specifier](./AA-H.7#S0366)})

dispatching_operation_specifier<a id="S0366"></a> ::= 
    dispatching_operation_[name](./AA-4.1#S0091) (object_[name](./AA-4.1#S0091))


#### Name Resolution Rules

{AI12-0380-1} A [formal_parameter_name](./AA-H.7#S0364) in a Use_Formal aspect shall resolve to statically denote a formal subtype, a formal subprogram, or a formal object of an anonymous access-to-subprogram type[ of an enclosing generic unit or visible formal package].

{AI12-0380-1} The object_[name](./AA-4.1#S0091) of a [dispatching_operation_specifier](./AA-H.7#S0366) shall resolve to statically name an object (including possibly a formal parameter) of a tagged class-wide type T'Class, or of an access type designating a tagged class-wide type T'Class; the dispatching_operation_[name](./AA-4.1#S0091) of the [dispatching_operation_specifier](./AA-H.7#S0366) shall resolve to statically denote a dispatching operation associated with T. 


#### Static Semantics

{AI12-0380-1} {AI12-0439-1} The formal parameter set is identified by a set of [formal_parameter_name](./AA-H.7#S0364)s. Alternatively, the reserved word null may be used to indicate none of the generic formal parameters, or all to indicate all of the generic formal parameters, of any enclosing generic unit (or visible formal package) can be used within the execution of the operation. If there is no formal parameter set specified for an entity declared within a generic unit, it defaults to all.

{AI12-0380-1} {AI12-0404-1} {AI12-0444-1} The dispatching operation set is identified by a set of [dispatching_operation_specifier](./AA-H.7#S0366)s. It indicates that the Nonblocking and global effects of dispatching calls that match one of the specifiers, rather than being accounted for by the Nonblocking or global aspect, are instead to be accounted for by the invoker of the operation. A dispatching call matches a [dispatching_operation_specifier](./AA-H.7#S0366) if the [name](./AA-4.1#S0091) or [prefix](./AA-4.1#S0093) of the call statically denotes the same operation(s) as that of the [dispatching_operation_specifier](./AA-H.7#S0366), and at least one of the objects controlling the call is denoted by, or designated by, a [name](./AA-4.1#S0091) that statically names the same object as that denoted by the object_[name](./AA-4.1#S0091) of the [dispatching_operation_specifier](./AA-H.7#S0366).

Ramification: The object "controlling the call" is not necessarily a controlling parameter of the call if the call is a function with a controlling result or has parameters that is such a function. It is one of the objects that provide the dispatching tag used for the call; that could, for example, be a parameter of a function used as a parameter to the call, or an object being assigned to, or a parameter of an enclosing call. 

{AI12-0380-1} {AI12-0404-1} In the absence of any [dispatching_operation_specifier](./AA-H.7#S0366)s, or if none of them match a dispatching call C within an operation P, Nonblocking and global aspects checks are performed at the point of the call C within P using the Nonblocking and Global'Class aspects that apply to the dispatching operation named in call C. If there is a match, any global access or potential blocking within the subprogram body invoked by the call C is ignored at the point of call within P. Instead, when the operation P itself is invoked, Nonblocking and global aspect checks are performed presuming each named dispatching operation is called at least once (with the named object controlling the call), but similarly ignoring those dispatching calls that would match a [dispatching_operation_specifier](./AA-H.7#S0366) applicable at the point of invocation of P.


#### Legality Rules

{AI12-0380-1} Within an operation to which a Use_Formal aspect applies, if the formal parameter set is anything but all, then the only generic formal subtypes that may be used, the only formal subprograms that may be called, and the only formal objects of an anonymous access-to-subprogram type that may be dereferenced as part of a call or passed as the actual for an access parameter, are those included in the formal parameter set.

{AI12-0380-1} When an operation (or instance thereof) to which a Use_Formal aspect applies is invoked, Nonblocking and global aspect checks are performed presuming each generic formal parameter (or corresponding actual parameter) of the formal parameter set is used at least once. 


#### Examples

{AI12-0430-1} An example of use of the Dispatching aspect: 

```ada
procedure My_Write(  --  see 13.13.2
   Stream : not null access Ada.Streams.Root_Stream_Type'Class;
   Item   : My_Integer'Base)
   with Dispatching =&gt Write(Stream);
for My_Integer'Write use My_Write;

```

{AI12-0430-1} For examples of use of the Use_Formal aspect, see the Element functions of Hashed_Sets in A.18.8.


#### Extensions to Ada 2012

{AI12-0079-3} The aspects Use_Formal and Dispatching are new. 

