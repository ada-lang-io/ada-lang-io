---
sidebar_position:  147
---

# C.4  Preelaboration Requirements

{AI05-0299-1} {AI12-0417-1} [This subclause specifies additional implementation and documentation requirements for the Preelaborate aspect (see 10.2.1).] 


#### Implementation Requirements

The implementation shall not incur any run-time overhead for the elaboration checks of subprograms and protected_bodies declared in preelaborated library units. 

The implementation shall not execute any memory write operations after load time for the elaboration of constant objects declared immediately within the declarative region of a preelaborated library package, so long as the subtype and initial expression (or default initial expressions if initialized by default) of the [object_declaration](./AA-3.3#S0032) satisfy the following restrictions. The meaning of load time is implementation defined. 

Discussion: On systems where the image of the partition is initially copied from disk to RAM, or from ROM to RAM, prior to starting execution of the partition, the intention is that "load time" consist of this initial copying step. On other systems, load time and run time might actually be interspersed. 

Any [subtype_mark](./AA-3.2#S0028) denotes a statically constrained subtype, with statically constrained subcomponents, if any;

{AI95-00161-01} no [subtype_mark](./AA-3.2#S0028) denotes a controlled type, a private type, a private extension, a generic formal private type, a generic formal derived type, or a descendant of such a type;

Reason: For an implementation that uses the registration method of finalization, a controlled object will require some code executed to register the object at the appropriate point. The other types are those that might have a controlled component. None of these types were allowed in preelaborated units in Ada 95. These types are covered by the Implementation Advice, of course, so they should still execute as little code as possible. 

any [constraint](./AA-3.2#S0029) is a static constraint;

any [allocator](./AA-4.8#S0164) is for an access-to-constant type;

any uses of predefined operators appear only within static expressions;

any primaries that are [name](./AA-4.1#S0091)s, other than [attribute_reference](./AA-4.1#S0100)s for the Access or Address attributes, appear only within static expressions; 

Ramification: This cuts out [attribute_reference](./AA-4.1#S0100)s that are not static, except for Access and Address. 

{AI12-0368-1} any [name](./AA-4.1#S0091) that is not part of a static expression is an expanded name or [direct_name](./AA-4.1#S0092) that statically names some entity; 

Ramification: {AI12-0368-1} This cuts out [function_call](./AA-6.4#S0218)s and [type_conversion](./AA-4.6#S0162)s that are not static, including calls on attribute functions like 'Image and 'Value. We do allow components if those components don't require any evaluation or checks. 

any [discrete_choice](./AA-3.8#S0074) of an [array_aggregate](./AA-4.3#S0113) is static;

no language-defined check associated with the elaboration of the [object_declaration](./AA-3.3#S0032) can fail. 

Reason: {AI95-00114-01} The intent is that aggregates all of whose scalar subcomponents are static and all of whose access subcomponents are null, allocators for access-to-constant types, or X'Access, will be supported with no run-time code generated. 


#### Documentation Requirements

The implementation shall document any circumstances under which the elaboration of a preelaborated package causes code to be executed at run time. 

Documentation Requirement: Any circumstances when the elaboration of a preelaborated package causes code to be executed.

The implementation shall document whether the method used for initialization of preelaborated variables allows a partition to be restarted without reloading. 

Documentation Requirement: Whether a partition can be restarted without reloading.

This paragraph was deleted.

Discussion: {AI95-00114-01} This covers the issue of the run-time system itself being restartable, so that need not be a separate Documentation Requirement. 


#### Implementation Advice

It is recommended that preelaborated packages be implemented in such a way that there should be little or no code executed at run time for the elaboration of entities not already covered by the Implementation Requirements.

Implementation Advice: Preelaborated packages should be implemented such that little or no code is executed at run time for the elaboration of entities.


#### Wording Changes from Ada 95

{AI95-00161-01} Added wording to exclude the additional kinds of types allowed in preelaborated units from the Implementation Requirements. 


#### Wording Changes from Ada 2012

{AI12-0368-1} Added wording to allow components so long as no evaluation or checks are required for the reference. 

