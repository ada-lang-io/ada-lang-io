---
sidebar_position:  25
---

# Annex L Language-Defined Pragmas

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
This Annex summarizes the definitions given elsewhere of the language-defined pragmas. 

pragma All_Calls_Remote[(library_unit_name)]; - See E.2.3.

pragma Asynchronous(local_name); - See E.4.1.

pragma Atomic(local_name); - See C.6.

pragma Atomic_Components(array_local_name); - See C.6.

pragma Attach_Handler(handler_name, expression); - See C.3.1.

pragma Controlled(first_subtype_local_name); - See 13.11.3.

pragma Convention([Convention =&gt] convention_identifier,[Entity =&gt] local_name); - See B.1.

pragma Discard_Names[([On =&gt ] local_name)]; - See C.5.

pragma Elaborate(library_unit_name{, library_unit_name}); - See 10.2.1.

pragma Elaborate_All(library_unit_name{, library_unit_name}); - See 10.2.1.

pragma Elaborate_Body[(library_unit_name);] - See 10.2.1.

pragma Export(
     [Convention =&gt] convention_identifier, [Entity =&gt] local_name
  [, [External_Name =&gt] string_expression] [, [Link_Name =&gt] string_expression]); - See B.1.

pragma Import(
     [Convention =&gt] convention_identifier, [Entity =&gt] local_name
  [, [External_Name =&gt] string_expression] [, [Link_Name =&gt] string_expression]); - See B.1.

pragma Inline(name {, name}); - See 6.3.2.

pragma Inspection_Point[(object_name {, object_name})]; - See H.3.2.

pragma Interrupt_Handler(handler_name); - See C.3.1.

pragma Interrupt_Priority[(expression);] - See D.1.

pragma Linker_Options(string_expression); - See B.1.

pragma List(identifier); - See 2.8.

pragma Locking_Policy(policy_identifier); - See D.3.

- See 6.5.1.

pragma Normalize_Scalars; - See H.1.

pragma Optimize(identifier); - See 2.8.

pragma Pack(first_subtype_local_name); - See 13.2.

pragma Page; - See 2.8.

- See 10.2.1.

pragma Preelaborate[(library_unit_name)]; - See 10.2.1.

pragma Priority(expression); - See D.1.

- See D.13.

pragma Pure[(library_unit_name)]; - See 10.2.1.

pragma Queuing_Policy(policy_identifier); - See D.4.

- See D.2.2.

pragma Remote_Call_Interface[(library_unit_name)]; - See E.2.3.

pragma Remote_Types[(library_unit_name)]; - See E.2.2.

pragma Restrictions(restriction{, restriction}); - See 13.12.

pragma Reviewable; - See H.3.1.

pragma Shared_Passive[(library_unit_name)]; - See E.2.1.

pragma Storage_Size(expression); - See 13.3.

pragma Suppress(identifier [, [On =&gt] name]); - See 11.5.

pragma Task_Dispatching_Policy(policy_identifier); - See D.2.2.

- See B.3.3.

pragma Volatile(local_name); - See C.6.

pragma Volatile_Components(array_local_name); - See C.6.


#### Wording Changes from Ada 83

Pragmas List, Page, and Optimize are now officially defined in 2.8, "Pragmas". 

