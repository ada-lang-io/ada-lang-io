---
sidebar_position:  80
---

# 9.9  Task and Entry Attributes


#### Dynamic Semantics

For a [prefix](./AA-4.1#S0093) T that is of a task type [(after any implicit dereference)], the following attributes are defined: 

T'CallableYields the value True when the task denoted by T is callable, and False otherwise; a task is callable unless it is completed or abnormal. The value of this attribute is of the predefined type Boolean.

T'TerminatedYields the value True if the task denoted by T is terminated, and False otherwise. The value of this attribute is of the predefined type Boolean. 

For a [prefix](./AA-4.1#S0093) E that denotes an entry of a task or protected unit, the following attribute is defined. This attribute is only allowed within the body of the task or protected unit, but excluding, in the case of an entry of a task unit, within any program unit that is, itself, inner to the body of the task unit. 

E'CountYields the number of calls presently queued on the entry E of the current instance of the unit. The value of this attribute is of the type universal_integer.

NOTE 1   For the Count attribute, the entry can be either a single entry or an entry of a family. The name of the entry or entry family can be either a [direct_name](./AA-4.1#S0092) or an expanded name.

NOTE 2   {AI12-0442-1} Within task units, by interrogating the attribute E'Count an algorithm can allow for the increase of the value of this attribute for incoming entry calls, and its decrease, for example with [timed_entry_call](./AA-9.7#S0276)s. A [conditional_entry_call](./AA-9.7#S0279) can also briefly increase this value, even if the conditional call is not accepted.

NOTE 3   {AI12-0442-1} Within protected units, by interrogating the attribute E'Count in the [entry_barrier](./AA-9.5#S0262) for the entry E an algorithm can allow for the evaluation of the [condition](./AA-4.5#S0150) of the barrier both before and after queuing a given caller. 

