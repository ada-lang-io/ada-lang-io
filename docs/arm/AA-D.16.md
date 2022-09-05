---
sidebar_position:  167
---

# D.16  Multiprocessor Implementation

{AI05-0171-1} {AI05-0299-1} This subclause allows implementations on multiprocessor platforms to be configured. 


#### Static Semantics

{AI05-0171-1} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} package System.Multiprocessors
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
   type CPU_Range is range 0 .. implementation-defined;
   Not_A_Specific_CPU : constant CPU_Range := 0;
   subtype CPU is CPU_Range range 1 .. CPU_Range'Last;

```

Implementation defined: The value of CPU_Range'Last in System.Multiprocessors.

```ada
   function Number_Of_CPUs return CPU;
end System.Multiprocessors;

```

{AI05-0171-1} A call of Number_Of_CPUs returns the number of processors available to the program. Within a given partition, each call on Number_Of_CPUs will return the same value.

{AI05-0229-1} {AI12-0281-1} For a task type (including the anonymous type of a [single_task_declaration](./AA-9.1#S0245)), protected type (including the anonymous type of a [single_protected_declaration](./AA-9.4#S0250)), or subprogram, the following language-defined representation aspect may be specified:

CPUThe aspect CPU is an [expression](./AA-4.4#S0132), which shall be of type System.Multiprocessors.CPU_Range.

Aspect Description for CPU: Processor on which a given task, or calling task for a protected operation, should run.


#### Legality Rules

{AI05-0171-1} {AI05-0229-1} If the CPU aspect is specified for a subprogram, the [expression](./AA-4.4#S0132) shall be static.

{AI05-0229-1} {AI12-0281-1} The CPU aspect shall not be specified on a task or protected interface type.


#### Dynamic Semantics

{AI05-0171-1} {AI05-0229-1} {AI12-0081-1} {AI12-0281-1} The [expression](./AA-4.4#S0132) specified for the CPU aspect of a task or protected type is evaluated each time an object of the corresponding type is created (see 9.1 and 9.4). The CPU value is then associated with the object.

{AI05-0171-1} {AI05-0229-1} The CPU aspect has no effect if it is specified for a subprogram other than the main subprogram; the CPU value is not associated with any task.

{AI05-0171-1} {AI05-0229-1} The CPU value is associated with the environment task if the CPU aspect is specified for the main subprogram. If the CPU aspect is not specified for the main subprogram it is implementation defined on which processor the environment task executes. 

Implementation defined: The processor on which the environment task executes in the absence of a value for the aspect CPU.

{AI05-0171-1} {AI05-0264-1} {AI12-0281-1} For a task, the CPU value determines the processor on which the task will activate and execute; the task is said to be assigned to that processor. If the CPU value is Not_A_Specific_CPU, then the task is not assigned to a processor. A task without a CPU aspect specified will activate and execute on the same processor as its activating task if the activating task is assigned a processor. If the CPU value is not in the range of System.Multiprocessors.CPU_Range or is greater than Number_Of_CPUs the task is defined to have failed, and it becomes a completed task (see 9.2).

{AI12-0281-1} For a protected type, the CPU value determines the processor on which calling tasks will execute; the protected object is said to be assigned to that processor. If the CPU value is Not_A_Specific_CPU, then the protected object is not assigned to a processor. A call to a protected object that is assigned to a processor from a task that is not assigned a processor or is assigned a different processor raises Program_Error.

Discussion: {AI12-0005-1} When a protected object is assigned to a CPU, only tasks also assigned to that CPU can call it. In contrast, a protected object that is not assigned to a specific CPU can be called by any task on any processor (subject, of course, to visibility and ceiling priority restrictions). As noted below, when the tasks and protected object are necessarily on the same CPU, a simpler implementation can be used. 


#### Implementation Advice

{AI12-0281-1} {AI12-0323-1} Starting a protected action on a protected object statically assigned to a processor should be implemented without busy-waiting.

Reason: Busy-waiting is a form of lock and can be a source of deadlock. Busy-waiting is typically needed for starting protected actions on multiprocessors, but if all tasks calling a protected object execute on the same CPU, this locking isn't needed and the source of deadlock and associated overhead can be eliminated. 

Implementation Advice: Starting a protected action on a protected object statically assigned to a processor should not use busy-waiting.


#### Extensions to Ada 2005

{AI05-0171-1} {AI05-0229-1} The package System.Multiprocessors and the CPU aspect are new. 


#### Extensions to Ada 2012

{AI12-0281-1} Aspect CPU can now be applied to protected types, in order to avoid the overhead and deadlock potential of multiprocessor execution. 


#### Wording Changes from Ada 2012

{AI12-0081-1} Corrigendum: Clarified when the CPU aspect expression is evaluated. 


## D.16.1  Multiprocessor Dispatching Domains

{AI05-0167-1} {AI05-0299-1} This subclause allows implementations on multiprocessor platforms to be partitioned into distinct dispatching domains during program startup.


#### Static Semantics

{AI05-0167-1} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Real_Time;
with Ada.Task_Identification;
package System.Multiprocessors.Dispatching_Domains
   with Nonblocking, Global =&gt in out synchronized is

```

```ada
   Dispatching_Domain_Error : exception;

```

```ada
   type Dispatching_Domain (&lt&gt) is limited private;

```

```ada
   System_Dispatching_Domain : constant Dispatching_Domain;

```

```ada
{AI12-0033-1}    function Create (First : CPU; Last : CPU_Range) return Dispatching_Domain;

```

```ada
   function Get_First_CPU (Domain : Dispatching_Domain) return CPU;

```

```ada
{AI12-0033-1}    function Get_Last_CPU  (Domain : Dispatching_Domain) return CPU_Range;

```

```ada
{AI12-0033-1}    type CPU_Set is array(CPU range &lt&gt) of Boolean;

```

```ada
{AI12-0033-1}    function Create (Set : CPU_Set) return Dispatching_Domain;

```

```ada
{AI12-0033-1}    function Get_CPU_Set (Domain : Dispatching_Domain) return CPU_Set;

```

```ada
   function Get_Dispatching_Domain
      (T   : Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task)
           return Dispatching_Domain;

```

```ada
   procedure Assign_Task
      (Domain : in out Dispatching_Domain;
       CPU    : in     CPU_Range := Not_A_Specific_CPU;
       T      : in     Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task);

```

```ada
   procedure Set_CPU
      (CPU : in CPU_Range;
       T   : in Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task);

```

```ada
   function Get_CPU
      (T   : Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task)
           return CPU_Range;

```

```ada
   procedure Delay_Until_And_Set_CPU
      (Delay_Until_Time : in Ada.Real_Time.Time; CPU : in CPU_Range);

```

```ada
private
   ... -- not specified by the language
end System.Multiprocessors.Dispatching_Domains;

```

{AI05-0167-1} {AI12-0082-1} A dispatching domain represents a set of processors on which a task may execute. Each processor is contained within exactly one dispatching domain. An object of type Dispatching_Domain identifies a dispatching domain. System_Dispatching_Domain identifies a domain that contains the processor or processors on which the environment task executes. At program start-up all processors are contained within this domain.

{AI05-0167-1} For a task type (including the anonymous type of a [single_task_declaration](./AA-9.1#S0245)), the following language-defined representation aspect may be specified:

Dispatching_DomainThe value of aspect Dispatching_Domain is an [expression](./AA-4.4#S0132), which shall be of type Dispatching_Domains.Dispatching_Domain. This aspect is the domain to which the task (or all objects of the task type) are assigned.

Aspect Description for Dispatching_Domain: Domain (group of processors) on which a given task should run.


#### Legality Rules

{AI05-0167-1} The Dispatching_Domain aspect shall not be specified for a task interface.


#### Dynamic Semantics

{AI05-0167-1} {AI12-0033-1} The expression specified for the Dispatching_Domain aspect of a task type is evaluated each time an object of the task type is created (see 9.1). If the identified dispatching domain is empty, then Dispatching_Domain_Error is raised; otherwise the newly created task is assigned to the domain identified by the value of the expression.

{AI05-0167-1} If a task is not explicitly assigned to any domain, it is assigned to that of the activating task. A task always executes on some CPU in its domain.

{AI05-0167-1} {AI12-0082-1} If both the dispatching domain and CPU are specified for a task, and the CPU value is not contained within the set of processors for the domain (and is not Not_A_Specific_CPU), the activation of the task is defined to have failed, and it becomes a completed task (see 9.2).

{AI05-0167-1} {AI12-0033-1} The function Create with First and Last parameters creates and returns a dispatching domain containing all the processors in the range First .. Last. The function Create with a Set parameter creates and returns a dispatching domain containing the processors for which Set(I) is True. These processors are removed from System_Dispatching_Domain. A call of Create will raise Dispatching_Domain_Error if any designated processor is not currently in System_Dispatching_Domain, or if the system cannot support a distinct domain over the processors identified, or if a processor has a task assigned to it, or if the allocation would leave System_Dispatching_Domain empty. A call of Create will raise Dispatching_Domain_Error if the calling task is not the environment task, or if Create is called after the call to the main subprogram.

{AI05-0167-1} {AI12-0033-1} The function Get_First_CPU returns the first CPU in Domain, or CPU'First if Domain is empty; Get_Last_CPU returns the last CPU in Domain, or CPU_Range'First if Domain is empty. The function Get_CPU_Set(D) returns an array whose low bound is Get_First_CPU(D), whose high bound is Get_Last_CPU(D), with True values in the Set corresponding to the CPUs that are in the given Domain.

{AI05-0167-1} {AI12-0082-1} The function Get_Dispatching_Domain returns the dispatching domain on which the task is assigned.

{AI05-0167-1} {AI05-0278-1} {AI12-0033-1} A call of the procedure Assign_Task assigns task T to the CPU within the dispatching domain Domain. Task T can now execute only on CPU, unless CPU designates Not_A_Specific_CPU in which case it can execute on any processor within Domain. The exception Dispatching_Domain_Error is propagated if Domain is empty, T is already assigned to a dispatching domain other than System_Dispatching_Domain, or if CPU is not one of the processors of Domain (and is not Not_A_Specific_CPU). A call of Assign_Task is a task dispatching point for task T unless T is inside of a protected action, in which case the effect on task T is delayed until its next task dispatching point. If T is the Current_Task the effect is immediate if T is not inside a protected action, otherwise the effect is as soon as practical. Assigning a task already assigned to System_Dispatching_Domain to that domain has no effect.

{AI05-0167-1} {AI05-0278-1} {AI12-0082-1} A call of procedure Set_CPU assigns task T to the CPU. Task T can now execute only on CPU, unless CPU designates Not_A_Specific_CPU, in which case it can execute on any processor within its dispatching domain. The exception Dispatching_Domain_Error is propagated if CPU is not one of the processors of the dispatching domain on which T is assigned (and is not Not_A_Specific_CPU). A call of Set_CPU is a task dispatching point for task T unless T is inside of a protected action, in which case the effect on task T is delayed until its next task dispatching point. If T is the Current_Task the effect is immediate if T is not inside a protected action, otherwise the effect is as soon as practical.

{AI05-0167-1} The function Get_CPU returns the processor assigned to task T, or Not_A_Specific_CPU if the task is not assigned to a processor.

{AI05-0167-1} {AI12-0082-1} A call of Delay_Until_And_Set_CPU delays the calling task for the designated time and then assigns the task to the specified processor when the delay expires. The exception Dispatching_Domain_Error is propagated if P is not one of the processors of the calling task's dispatching domain (and is not Not_A_Specific_CPU).


#### Implementation Requirements

{AI05-0167-1} The implementation shall perform the operations Assign_Task, Set_CPU, Get_CPU and Delay_Until_And_Set_CPU atomically with respect to any of these operations on the same dispatching_domain, processor or task.

{AI12-0048-1} Any task that belongs to the system dispatching domain can execute on any CPU within that domain, unless the assignment of the task has been specified.

Reason: This ensures that priorities and deadlines are respected within the system dispatching domain. There is no such guarantee between different domains.

We only need to talk about the system dispatching domain here, because Assign_Task and Set_CPU already have such wording for tasks that are assigned explicitly to a dispatching domain and specify Not_a_Specific_CPU. 

Ramification: If no dispatching domains are created, all tasks can execute on all processors. (As always, implementation-defined dispatching policies may have other rules, so a partition that does not specify any language-defined dispatching policy may do anything at all and in particular does not need to follow this rule. 

Discussion: A task can be assigned to a specific CPU by specifying the aspect CPU for a task, or by calling a dynamic operation like Set_CPU or Assign_Task. 


#### Implementation Advice

{AI05-0167-1} Each dispatching domain should have separate and disjoint ready queues.

Implementation Advice: Each dispatching domain should have separate and disjoint ready queues.

To be honest: {AI12-0048-1} "Ready queue" here doesn't mean the conceptual "ready queue" as defined in D.2.1 (one per processor); this rule is talking about the ready queues used by the implementation. 


#### Documentation Requirements

{AI05-0167-1} The implementation shall document the processor(s) on which the clock interrupt is handled and hence where delay queue and ready queue manipulations occur. For any Interrupt_Id whose handler can execute on more than one processor the implementation shall also document this set of processors.

Documentation Requirement: The processor(s) on which the clock interrupt is handled; the processors on which each Interrupt_Id can be handled.


#### Implementation Permissions

{AI05-0167-1} An implementation may limit the number of dispatching domains that can be created and raise Dispatching_Domain_Error if an attempt is made to exceed this number.

{AI12-0119-1} The implementation may defer the effect of a Set_CPU or an Assign_Task operation until the specified task leaves an ongoing parallel construct.

Reason: {AI12-0119-1} These operations can change the set of CPUs that a parallel operation is allowed to use. This could require the ability to move or suspend one or more threads to execute them on a different CPU. However, parallel constructs are primarily intended to improve performance of code, and the overhead needed to support such a rarely used operation could be substantial. Therefore, rather than requiring support we allow the implementation to wait to implement these operations until the parallel construct (and thus the extra threads) have completed. 


#### Extensions to Ada 2005

{AI05-0167-1} {AI05-0278-1} The package System.Multiprocessors.Dispatching_Domains and the aspect Dispatching_Domains are new. 


#### Inconsistencies With Ada 2012

{AI12-0033-1} Corrigendum: We now explicitly allow empty dispatching domains, as it would be difficult to avoid declaring them when a system is configured at runtime. Therefore, assigning a task to an empty domain now raises Dispatching_Domain_Error; creating such a domain should not raise Dispatching_Domain_Error. If an implementation does something different in these cases, and a program depends on that difference, the program could malfunction. This seems very unlikely (if no exception is ever raised, the task assigned to the empty domain could never run; if the exception is raised earlier, the program can't do anything useful). 


#### Incompatibilities With Ada 2012

{AI05-0033-1} {AI05-0005-1} Corrigendum: The subtypes of the parameter or result of several routines were changed to support empty domains. These changes will cause rules requiring subtype conformance to fail on these routines (such as 'Access). We believe such uses are unlikely. In addition, type CPU_Set and function Get_CPU_Set, along with an overloaded Create are newly added to this package. As such, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Wording Changes from Ada 2012

{AI12-0048-1} Corrigendum: Added wording to clarify that all tasks can execute on all CPUs of the system dispatching domain by default.

{AI12-0082-1} Corrigendum: Added a definition to clarify that a "dispatching domain" is a concept which is identified by an object of type Dispatching_Domain; more than one object might identify the same dispatching domain (for instance, the result of function Get_Dispatching_Domain is a different object but identifies the same dispatching domain). 

