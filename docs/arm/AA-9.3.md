---
sidebar_position:  74
---

# 9.3  Task Dependence - Termination of Tasks


#### Dynamic Semantics

Each task (other than an environment task - see 10.2) depends on one or more masters (see 7.6.1), as follows: 

{AI12-0070-1} If the task is created by the evaluation of an [allocator](./AA-4.8#S0164) for a given named access type, it depends on each master that includes the elaboration of the declaration of the ultimate ancestor of the given access type.

If the task is created by the elaboration of an [object_declaration](./AA-3.3#S0032), it depends on each master that includes this elaboration.

{AI95-00416-01} Otherwise, the task depends on the master of the outermost object of which it is a part (as determined by the accessibility level of that object - see 3.10.2 and 7.6.1), as well as on any master whose execution includes that of the master of the outermost object. 

Ramification: {AI95-00416-01} The master of a task created by a return statement changes when the accessibility of the return object changes. Note that its activation happens, if at all, only after the function returns and all accessibility level changes have occurred. 

Furthermore, if a task depends on a given master, it is defined to depend on the task that executes the master, and (recursively) on any master of that task. 

Discussion: Don't confuse these kinds of dependences with the dependences among compilation units defined in 10.1.1, "Compilation Units - Library Units". 

A task is said to be completed when the execution of its corresponding [task_body](./AA-9.1#S0248) is completed. A task is said to be terminated when any finalization of the [task_body](./AA-9.1#S0248) has been performed (see 7.6.1). [The first step of finalizing a master (including a [task_body](./AA-9.1#S0248)) is to wait for the termination of any tasks dependent on the master.] The task executing the master is blocked until all the dependents have terminated. [Any remaining finalization is then performed and the master is left.]

Completion of a task (and the corresponding [task_body](./AA-9.1#S0248)) can occur when the task is blocked at a [select_statement](./AA-9.7#S0269) with an open [terminate_alternative](./AA-9.7#S0275) (see 9.7.1); the open [terminate_alternative](./AA-9.7#S0275) is selected if and only if the following conditions are satisfied: 

{AI95-00415-01} The task depends on some completed master; and

Each task that depends on the master considered is either already terminated or similarly blocked at a [select_statement](./AA-9.7#S0269) with an open [terminate_alternative](./AA-9.7#S0275). 

When both conditions are satisfied, the task considered becomes completed, together with all tasks that depend on the master considered that are not yet completed. 

Ramification: Any required finalization is performed after the selection of [terminate_alternative](./AA-9.7#S0275)s. The tasks are not callable during the finalization. In some ways it is as though they were aborted. 

NOTE 1   The full view of a limited private type can be a task type, or can have subcomponents of a task type. Creation of an object of such a type creates dependences according to the full type.

NOTE 2   An [object_renaming_declaration](./AA-8.5#S0239) defines a new view of an existing entity and hence creates no further dependence.

NOTE 3   {AI12-0440-1} The rules given for the collective completion of a group of tasks all blocked on [select_statement](./AA-9.7#S0269)s with open [terminate_alternative](./AA-9.7#S0275)s ensure that the collective completion can occur only when there are no remaining active tasks that can call one of the tasks being collectively completed.

NOTE 4   If two or more tasks are blocked on [select_statement](./AA-9.7#S0269)s with open [terminate_alternative](./AA-9.7#S0275)s, and become completed collectively, their finalization actions proceed concurrently.

NOTE 5   The completion of a task can occur due to any of the following: 

the raising of an exception during the elaboration of the [declarative_part](./AA-3.11#S0086) of the corresponding [task_body](./AA-9.1#S0248);

the completion of the [handled_sequence_of_statements](./AA-11.2#S0304) of the corresponding [task_body](./AA-9.1#S0248);

the selection of an open [terminate_alternative](./AA-9.7#S0275) of a [select_statement](./AA-9.7#S0269) in the corresponding [task_body](./AA-9.1#S0248);

the abort of the task. 


#### Examples

Example of task dependence: 

```ada
declare
   type Global is access Server;        --  see 9.1
   A, B : Server;
   G    : Global;
begin
   --  activation of A and B
   declare
      type Local is access Server;
      X : Global := new Server;  --  activation of X.all
      L : Local  := new Server;  --  activation of L.all
      C : Server;
   begin
      --  activation of C
      G := X;  --  both G and X designate the same task object
      ...
   end;  --  await termination of C and L.all (but not X.all)
   ...
end;  --  await termination of A, B, and G.all

```


#### Wording Changes from Ada 83

We have revised the wording to be consistent with the definition of master now given in 7.6.1, "Completion and Finalization".

Tasks that used to depend on library packages in Ada 83, now depend on the (implicit) [task_body](./AA-9.1#S0248) of the environment task (see 10.2). Therefore, the environment task has to wait for them before performing library level finalization and terminating the partition. In Ada 83 the requirement to wait for tasks that depended on library packages was not as clear.

What was "collective termination" is now "collective completion" resulting from selecting [terminate_alternative](./AA-9.7#S0275)s. This is because finalization still occurs for such tasks, and this happens after selecting the [terminate_alternative](./AA-9.7#S0275), but before termination. 


#### Wording Changes from Ada 95

{AI95-00416-01} Added missing wording that explained the master of tasks that are neither [object_declaration](./AA-3.3#S0032)s nor [allocator](./AA-4.8#S0164)s, such as function returns. 


#### Wording Changes from Ada 2012

{AI12-0070-1} Corrigendum: Ensured that the master of tasks that are not [allocator](./AA-4.8#S0164)s of named access types is correctly determined. (Ignoring the accessibility rules of 3.10.2 could not be intended.) 

