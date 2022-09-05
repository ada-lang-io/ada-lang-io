---
sidebar_position:  73
---

# 9.2  Task Execution - Task Activation


#### Dynamic Semantics

The execution of a task of a given task type consists of the execution of the corresponding [task_body](./AA-9.1#S0248). The initial part of this execution is called the activation of the task; it consists of the elaboration of the [declarative_part](./AA-3.11#S0086) of the [task_body](./AA-9.1#S0248). Should an exception be propagated by the elaboration of its [declarative_part](./AA-3.11#S0086), the activation of the task is defined to have failed, and it becomes a completed task.

{AI95-00416-01} A task object (which represents one task) can be a part of a stand-alone object, of an object created by an [allocator](./AA-4.8#S0164), or of an anonymous object of a limited type, or a coextension of one of these. All tasks that are part or coextensions of any of the stand-alone objects created by the elaboration of [object_declaration](./AA-3.3#S0032)s (or [generic_association](./AA-12.3#S0317)s of formal objects of mode in) of a single declarative region are activated together. All tasks that are part or coextensions of a single object that is not a stand-alone object are activated together. 

Discussion: The initialization of an [object_declaration](./AA-3.3#S0032) or [allocator](./AA-4.8#S0164) can indirectly include the creation of other objects that contain tasks. For example, the default expression for a subcomponent of an object created by an [allocator](./AA-4.8#S0164) might call a function that evaluates a completely different [allocator](./AA-4.8#S0164). Tasks created by the two allocators are not activated together. 

{AI95-00416-01} For the tasks of a given declarative region, the activations are initiated within the context of the [handled_sequence_of_statements](./AA-11.2#S0304) (and its associated [exception_handler](./AA-11.2#S0305)s if any - see 11.2), just prior to executing the statements of the [handled_sequence_of_statements](./AA-11.2#S0304). [For a package without an explicit body or an explicit [handled_sequence_of_statements](./AA-11.2#S0304), an implicit body or an implicit [null_statement](./AA-5.1#S0170) is assumed, as defined in 7.2.] 

Ramification: If Tasking_Error is raised, it can be handled by handlers of the [handled_sequence_of_statements](./AA-11.2#S0304). 

{AI95-00416-01} For tasks that are part or coextensions of a single object that is not a stand-alone object, activations are initiated after completing any initialization of the outermost object enclosing these tasks, prior to performing any other operation on the outermost object. In particular, for tasks that are part or coextensions of the object created by the evaluation of an [allocator](./AA-4.8#S0164), the activations are initiated as the last step of evaluating the [allocator](./AA-4.8#S0164), prior to returning the new access value. For tasks that are part or coextensions of an object that is the result of a function call, the activations are not initiated until after the function returns.

Discussion: {AI95-00416-01} The intent is that "temporary" objects with task parts (or coextensions) are treated similarly to an object created by an allocator. The "whole" object is initialized, and then all of the task parts (including the coextensions) are activated together. Each such "whole" object has its own task activation sequence, involving the activating task being suspended until all the new tasks complete their activation. 

The task that created the new tasks and initiated their activations (the activator) is blocked until all of these activations complete (successfully or not). Once all of these activations are complete, if the activation of any of the tasks has failed [(due to the propagation of an exception)], Tasking_Error is raised in the activator, at the place at which it initiated the activations. Otherwise, the activator proceeds with its execution normally. Any tasks that are aborted prior to completing their activation are ignored when determining whether to raise Tasking_Error. 

Ramification: Note that a task created by an [allocator](./AA-4.8#S0164) does not necessarily depend on its activator; in such a case the activator's termination can precede the termination of the newly created task. 

Discussion: Tasking_Error is raised only once, even if two or more of the tasks being activated fail their activation. 

To be honest: {AI95-00265-01} The pragma Partition_Elaboration_Policy (see H.6) can be used to defer task activation to a later point, thus changing many of these rules. 

{AI05-0045-1} If the master that directly encloses the point where the activation of a task T would be initiated, completes before the activation of T is initiated, T becomes terminated and is never activated. Furthermore, if a return statement is left such that the return object is not returned to the caller, any task that was created as a part of the return object or one of its coextensions immediately becomes terminated and is never activated. 

Ramification: {AI05-0045-1} The first case can only happen if the activation point of T is not reached due to an exception being raised or a task or statement being aborted. Note that this is exclusive; if the master completes normally and starts finalization, we're already past the activation point.

{AI05-0045-1} The second case can happen with an exception being raised in a return statement, by an exit or goto from an [extended_return_statement](./AA-6.5#S0225), or by a return statement being aborted. Any tasks created for the return object of such a return statement are never activated. 

NOTE 1   An entry of a task can be called before the task has been activated.

NOTE 2   {AI12-0442-1} If several tasks are activated together, the execution of any of these tasks can proceed without waiting until the end of the activation of the other tasks.

NOTE 3   A task can become completed during its activation either because of an exception or because it is aborted (see 9.8).


#### Examples

Example of task activation: 

```ada
procedure P is
   A, B : Server;    --  elaborate the task objects A, B
   C    : Server;    --  elaborate the task object C
begin
   --  the tasks A, B, C are activated together before the first statement
   ...
end;

```


#### Wording Changes from Ada 83

We have replaced the term suspended with blocked, since we didn't want to consider a task blocked when it was simply competing for execution resources. "Suspended" is sometimes used more generally to refer to tasks that are not actually running on some processor, due to the lack of resources.

{AI05-0299-1} This subclause has been rewritten in an attempt to improve presentation. 


#### Wording Changes from Ada 95

{AI95-00416-01} Adjusted the wording for activating tasks to handle the case of anonymous function return objects. This is critical; we don't want to be waiting for the tasks in a return object when we exit the function normally. 


#### Wording Changes from Ada 2005

{AI05-0045-1} Correction: Corrected the wording that handles tasks that are never activated to ensure that no lookahead is implied and to make it clear that tasks created by return statements that never return are never activated. 

