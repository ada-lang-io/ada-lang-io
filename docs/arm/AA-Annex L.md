---
sidebar_position:  209
---

# Annex L Language-Defined Pragmas

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
This Annex summarizes the definitions given elsewhere of the language-defined pragmas. 

pragma Admission_Policy (policy_[identifier](./AA-2.3#S0002)); - See D.4.1.

This paragraph was deleted. 

pragma All_Calls_Remote[(library_unit_[name](./AA-4.1#S0091))]; - See J.15.15.

pragma Assert([Check =&gt] boolean_[expression](./AA-4.4#S0132)[, [Message =&gt] string_[expression](./AA-4.4#S0132)]); - See 11.4.2.

pragma Assertion_Policy(policy_[identifier](./AA-2.3#S0002)); - See 11.4.2.

pragma Assertion_Policy(
         assertion_[aspect_mark](./AA-13.1#S0347) =&gt policy_[identifier](./AA-2.3#S0002)
     {, assertion_[aspect_mark](./AA-13.1#S0347) =&gt policy_[identifier](./AA-2.3#S0002)}); - See 11.4.2.

This paragraph was deleted. 

pragma Asynchronous ([local_name](./AA-13.1#S0345)); - See J.15.13.

This paragraph was deleted. 

pragma Atomic ([local_name](./AA-13.1#S0345)); - See J.15.8.

This paragraph was deleted. 

pragma Atomic_Components (array_[local_name](./AA-13.1#S0345)); - See J.15.8.

This paragraph was deleted. 

pragma Attach_Handler (handler_[name](./AA-4.1#S0091), [expression](./AA-4.4#S0132)); - See J.15.7.

pragma Conflict_Check_Policy (policy_[identifier](./AA-2.3#S0002)[, policy_[identifier](./AA-2.3#S0002)]); - See 9.10.1.

This paragraph was deleted. 

This paragraph was deleted. 

pragma Convention([Convention =&gt] convention_[identifier](./AA-2.3#S0002),[Entity =&gt] [local_name](./AA-13.1#S0345)); - See J.15.5.

pragma CPU ([expression](./AA-4.4#S0132)); - See J.15.9.

pragma Default_Storage_Pool ([storage_pool_indicator](./AA-13.11#S0358)); - See 13.11.3.

pragma Detect_Blocking; - See H.5.

pragma Discard_Names[([On =&gt ] [local_name](./AA-13.1#S0345))]; - See C.5.

pragma Dispatching_Domain ([expression](./AA-4.4#S0132)); - See J.15.10.

pragma Elaborate(library_unit_[name](./AA-4.1#S0091){, library_unit_[name](./AA-4.1#S0091)}); - See 10.2.1.

pragma Elaborate_All(library_unit_[name](./AA-4.1#S0091){, library_unit_[name](./AA-4.1#S0091)}); - See 10.2.1.

This paragraph was deleted. 

pragma Elaborate_Body[(library_unit_[name](./AA-4.1#S0091))]; - See J.15.14.

This paragraph was deleted. 

pragma Export(
     [Convention =&gt] convention_[identifier](./AA-2.3#S0002), [Entity =&gt] [local_name](./AA-13.1#S0345)
  [, [External_Name =&gt] external_name_string_[expression](./AA-4.4#S0132)]
  [, [Link_Name =&gt] link_name_string_[expression](./AA-4.4#S0132)]); - See J.15.5.

pragma Generate_Deadlines; - See D.2.6.

This paragraph was deleted. 

pragma Import(
     [Convention =&gt] convention_[identifier](./AA-2.3#S0002), [Entity =&gt] [local_name](./AA-13.1#S0345)
  [, [External_Name =&gt] external_name_string_[expression](./AA-4.4#S0132)]
  [, [Link_Name =&gt] link_name_string_[expression](./AA-4.4#S0132)]); - See J.15.5.

pragma Independent (component_[local_name](./AA-13.1#S0345)); - See J.15.8.

pragma Independent_Components ([local_name](./AA-13.1#S0345)); - See J.15.8.

This paragraph was deleted. 

pragma Inline ([name](./AA-4.1#S0091){, [name](./AA-4.1#S0091)}); - See J.15.1.

pragma Inspection_Point[(object_[name](./AA-4.1#S0091) {, object_[name](./AA-4.1#S0091)})]; - See H.3.2.

This paragraph was deleted. 

pragma Interrupt_Handler (handler_[name](./AA-4.1#S0091)); - See J.15.7.

This paragraph was deleted. 

pragma Interrupt_Priority [([expression](./AA-4.4#S0132));] - See J.15.11.

pragma Linker_Options(string_[expression](./AA-4.4#S0132)); - See B.1.

pragma List([identifier](./AA-2.3#S0002)); - See 2.8.

pragma Locking_Policy(policy_[identifier](./AA-2.3#S0002)); - See D.3.

This paragraph was deleted. 

pragma No_Return (subprogram_[local_name](./AA-13.1#S0345){, subprogram_[local_name](./AA-13.1#S0345)}); - See J.15.2.

pragma Normalize_Scalars; - See H.1.

pragma Optimize([identifier](./AA-2.3#S0002)); - See 2.8.

This paragraph was deleted. 

pragma Pack (first_subtype_[local_name](./AA-13.1#S0345)); - See J.15.3.

pragma Page; - See 2.8.

pragma Partition_Elaboration_Policy (policy_[identifier](./AA-2.3#S0002)); - See H.6.

This paragraph was deleted. 

pragma Preelaborable_Initialization([direct_name](./AA-4.1#S0092)); - See J.15.14.

This paragraph was deleted. 

pragma Preelaborate[(library_unit_[name](./AA-4.1#S0091))]; - See J.15.14.

This paragraph was deleted. 

pragma Priority ([expression](./AA-4.4#S0132)); - See J.15.11.

pragma Priority_Specific_Dispatching (
     policy_[identifier](./AA-2.3#S0002), first_priority_[expression](./AA-4.4#S0132), last_priority_[expression](./AA-4.4#S0132)); - See D.2.2.

pragma Profile (profile_[identifier](./AA-2.3#S0002) {, profile_[pragma_argument_association](./AA-2.8#S0020)}); - See 13.12.

This paragraph was deleted. 

This paragraph was deleted. 

pragma Pure[(library_unit_[name](./AA-4.1#S0091))]; - See J.15.14.

pragma Queuing_Policy(policy_[identifier](./AA-2.3#S0002)); - See D.4.

This paragraph was deleted. 

pragma Relative_Deadline (relative_deadline_[expression](./AA-4.4#S0132)); - See J.15.12.

This paragraph was deleted. 

pragma Remote_Call_Interface[(library_unit_[name](./AA-4.1#S0091))]; - See J.15.15.

This paragraph was deleted. 

pragma Remote_Types[(library_unit_[name](./AA-4.1#S0091))]; - See J.15.15.

pragma Restrictions([restriction](./AA-13.12#S0359){, [restriction](./AA-13.12#S0359)}); - See 13.12.

pragma Reviewable; - See H.3.1.

This paragraph was deleted. 

pragma Shared_Passive[(library_unit_[name](./AA-4.1#S0091))]; - See J.15.15.

This paragraph was deleted. 

pragma Storage_Size ([expression](./AA-4.4#S0132)); - See J.15.4.

pragma Suppress([identifier](./AA-2.3#S0002)); - See 11.5.

pragma Task_Dispatching_Policy(policy_[identifier](./AA-2.3#S0002)); - See D.2.2.

This paragraph was deleted. 

pragma Unchecked_Union (first_subtype_[local_name](./AA-13.1#S0345)); - See J.15.6.

pragma Unsuppress([identifier](./AA-2.3#S0002)); - See 11.5.

This paragraph was deleted. 

pragma Volatile ([local_name](./AA-13.1#S0345)); - See J.15.8.

This paragraph was deleted. 

pragma Volatile_Components (array_[local_name](./AA-13.1#S0345)); - See J.15.8.


#### Wording Changes from Ada 83

Pragmas List, Page, and Optimize are now officially defined in 2.8, "Pragmas". 

