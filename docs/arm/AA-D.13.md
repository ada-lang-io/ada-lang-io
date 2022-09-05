---
sidebar_position:  164
---

# D.13  The Ravenscar and Jorvik Profiles

{AI95-00249-01} {AI05-0246-1} {AI05-0299-1} {AI12-0291-1} [This subclause defines the Ravenscar and Jorvik profiles.] 

Paragraphs 2 and 3 were moved to 13.12, "Pragma Restrictions and Pragma Profile". 


#### Legality Rules

{AI95-00249-01} {AI05-0246-1} {AI12-0291-1} The profile_[identifier](./AA-2.3#S0002) Ravenscar and profile_[identifier](./AA-2.3#S0002) Jorvik are usage profiles (see 13.12). For usage profiles Ravenscar and Jorvik, there shall be no profile_[pragma_argument_association](./AA-2.8#S0020)s. 


#### Static Semantics

{AI95-00249-01} {AI05-0246-1} The usage profile Ravenscar is equivalent to the following set of pragmas:

```ada
{AI95-00249-01} {AI95-00297-01} {AI95-00394-01} {AI05-0171-1} {AI05-0246-1} {AI12-0055-1} {AI12-0073-1} pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy (Ceiling_Locking);
pragma Detect_Blocking;
pragma Restrictions (
              No_Abort_Statements,
              No_Dynamic_Attachment,
              No_Dynamic_CPU_Assignment,
              No_Dynamic_Priorities,
              No_Implicit_Heap_Allocations,
              No_Local_Protected_Objects,
              No_Local_Timing_Events,
              No_Protected_Type_Allocators,
              No_Relative_Delay,
              No_Requeue_Statements,
              No_Select_Statements,
              No_Specific_Termination_Handlers,
              No_Task_Allocators,
              No_Task_Hierarchy,
              No_Task_Termination,
              Simple_Barriers,
              Max_Entry_Queue_Length =&gt 1,
              Max_Protected_Entries =&gt 1,
              Max_Task_Entries =&gt 0,
              No_Dependence =&gt Ada.Asynchronous_Task_Control,
              No_Dependence =&gt Ada.Calendar,
              No_Dependence =&gt Ada.Execution_Time.Group_Budgets,
              No_Dependence =&gt Ada.Execution_Time.Timers,
              No_Dependence =&gt Ada.Synchronous_Barriers,
              No_Dependence =&gt Ada.Task_Attributes,
              No_Dependence =&gt System.Multiprocessors.Dispatching_Domains);

```

Discussion: The Ravenscar profile is named for the location of the meeting that defined its initial version. The name is now in widespread use, so we stick with existing practice, rather than using a more descriptive name.

The Jorvik profile is named for the city of York, UK, near where the Ravenscar profile was created. "Jorvik" (pronounced "Yorvick") was the Viking name for York. 

{AI12-0291-1} The usage profile Jorvik is equivalent to the following set of pragmas:

```ada
{AI12-0291-1} pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy (Ceiling_Locking);
pragma Detect_Blocking;
pragma Restrictions (
              No_Abort_Statements,
              No_Dynamic_Attachment,
              No_Dynamic_CPU_Assignment,
              No_Dynamic_Priorities,
              No_Local_Protected_Objects,
              No_Local_Timing_Events,
              No_Protected_Type_Allocators,
              No_Requeue_Statements,
              No_Select_Statements,
              No_Specific_Termination_Handlers,
              No_Task_Allocators,
              No_Task_Hierarchy,
              No_Task_Termination,
              Pure_Barriers,
              Max_Task_Entries =&gt 0,
              No_Dependence =&gt Ada.Asynchronous_Task_Control,
              No_Dependence =&gt Ada.Execution_Time.Group_Budgets,
              No_Dependence =&gt Ada.Execution_Time.Timers,
              No_Dependence =&gt Ada.Task_Attributes,
              No_Dependence =&gt System.Multiprocessors.Dispatching_Domains);

```

Discussion: The Jorvik profile removes a number of restrictions from the Ravenscar profile to allow additional applications to benefit from predictability and low overhead. Specifically, the following restrictions are removed: 

    No_Implicit_Heap_Allocations
    No_Relative_Delay
    Max_Entry_Queue_Length =&gt 1
    Max_Protected_Entries =&gt 1
    No_Dependence =&gt Ada.Calendar
    No_Dependence =&gt Ada.Synchronous_Barriers

Jorvik also replaces restriction Simple_Barriers with Pure_Barriers (a weaker requirement than Simple_Barriers). 

Paragraph 7 and 8 were deleted. 


#### Implementation Advice

{AI05-0171-1} {AI12-0291-1} On a multiprocessor system, an implementation should support a fully partitioned approach if one of these profiles is specified. Each processor should have separate and disjoint ready queues.

Implementation Advice: On a multiprocessor system, each processor should have a separate and disjoint ready queue.

NOTE 1   {AI95-00249-01} {AI05-0246-1} {AI12-0291-1} For the Ravenscar profile, the effect of the restriction Max_Entry_Queue_Length =&gt 1 applies only to protected entry queues due to the accompanying restriction Max_Task_Entries =&gt 0. The restriction Max_Entry_Queue_Length is not applied by the Jorvik profile.

NOTE 2   {AI12-0055-1} {AI12-0291-1} When the Ravenscar or Jorvik profile is in effect (via the effect of the No_Dynamic_CPU_Assignment restriction), all of the tasks in the partition will execute on a single CPU unless the programmer explicitly uses aspect CPU to specify the CPU assignments for tasks. The use of multiple CPUs requires care, as many guarantees of single CPU scheduling no longer apply.

NOTE 3   {AI12-0055-1} {AI12-0291-1} It is not recommended to specify the CPU of a task to be Not_A_Specific_CPU when the Ravenscar or Jorvik profile is in effect. How a partition executes strongly depends on the assignment of tasks to CPUs.

NOTE 4   {AI12-0291-1} Any unit that meets the requirements of the Ravenscar profile also meets the requirements of the Jorvik profile. 


#### Extensions to Ada 95

{AI95-00249-01} {AI05-0246-1} The Ravenscar profile is new; it was moved here by Ada 2012. 


#### Wording Changes from Ada 2005

{AI05-0171-1} How Ravenscar behaves on a multiprocessor system is now defined. 


#### Incompatibilities With Ada 2012

{AI05-0073-1} Corrigendum: The Ravenscar profile no longer allows the use of package Synchronous_Barriers, as this package violates the fundamental Ravenscar requirement that each waiting point can only block (and release) a single task. This is incompatible with the published Ada 2012 standard, but it is unlikely that any existing Ravenscar runtime ever usefully supported barriers.

{AI05-0055-1} Corrigendum:The Ravenscar profile (via the effect of the new restriction No_Dynamic_CPU_Assignment) no longer allows setting the CPU aspect of a task to a non-static value. While this was allowed, an implementation would have had to come up with a creative interpretation of the Ada 2012 requirement to define the association of tasks to processors statically. As such, the new check is more likely to catch bugs than break a working program. 


#### Extensions to Ada 2012

{AI12-0291-1} The Jorvik profile is new. 

