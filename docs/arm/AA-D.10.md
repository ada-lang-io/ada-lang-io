---
sidebar_position:  161
---

# D.10  Synchronous Task Control

{AI05-0299-1} [This subclause describes a language-defined private semaphore (suspension object), which can be used for two-stage suspend operations and as a simple building block for implementing higher-level queues.] 


#### Static Semantics

The following language-defined package exists: 

```ada
{AI95-00362-01} {AI12-0241-1} {AI12-0302-1} package Ada.Synchronous_Task_Control
  with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI12-0241-1}   type Suspension_Object is limited private;
  procedure Set_True(S : in out Suspension_Object);
  procedure Set_False(S : in out Suspension_Object);
  function Current_State(S : Suspension_Object) return Boolean;
  procedure Suspend_Until_True(S : in out Suspension_Object)
     with Nonblocking =&gt False;
private
     ... -- not specified by the language
end Ada.Synchronous_Task_Control;

```

The type Suspension_Object is a by-reference type.

Implementation Note: {AI95-00318-02} The implementation can ensure this by, for example, making the full view an explicitly limited record type.

{AI05-0168-1} The following language-defined package exists: 

```ada
{AI05-0168-1} {AI12-0241-1} {AI12-0302-1} with Ada.Real_Time;
package Ada.Synchronous_Task_Control.EDF
   with Nonblocking, Global =&gt in out synchronized is
   procedure Suspend_Until_True_And_Set_Deadline
      (S  : in out Suspension_Object;
       TS : in     Ada.Real_Time.Time_Span)
      with Nonblocking =&gt False;
end Ada.Synchronous_Task_Control.EDF;

```


#### Dynamic Semantics

{AI95-00114-01} An object of the type Suspension_Object has two visible states: True and False. Upon initialization, its value is set to False. 

Discussion: This object is assumed to be private to the declaring task, i.e. only that task will call Suspend_Until_True on this object, and the count of callers is at most one. Other tasks can, of course, change and query the state of this object. 

{AI95-00114-01} The operations Set_True and Set_False are atomic with respect to each other and with respect to Suspend_Until_True; they set the state to True and False respectively.

Current_State returns the current state of the object. 

Discussion: This state can change immediately after the operation returns. 

{AI95-00114-01} The procedure Suspend_Until_True blocks the calling task until the state of the object S is True; at that point the task becomes ready and the state of the object becomes False.

{AI12-0241-1} Program_Error is raised upon calling Suspend_Until_True if another task is already waiting on that suspension object.

{AI05-0168-1} {AI05-0269-1} {AI12-0241-1} The procedure Suspend_Until_True_And_Set_Deadline blocks the calling task until the state of the object S is True; at that point the task becomes ready with a deadline of Ada.Real_Time.Clock + TS, and the state of the object becomes False. Program_Error is raised upon calling Suspend_Until_True_And_Set_Deadline if another task is already waiting on that suspension object. 


#### Bounded (Run-Time) Errors

{AI12-0171-1} {AI12-0439-1} It is a bounded error for two or more tasks to call Suspend_Until_True on the same Suspension_Object concurrently. For each task, Program_Error can be raised, the task can proceed without suspending, or the task can suspend, potentially indefinitely. The state of the suspension object can end up either True or False. 


#### Implementation Requirements

The implementation is required to allow the calling of Set_False and Set_True during any protected action, even one that has its ceiling priority in the Interrupt_Priority range.

NOTE 1   {AI05-0168-1} More complex schemes, such as setting the deadline relative to when Set_True is called, can be programmed using a protected object. 


#### Extensions to Ada 95

{AI95-00362-01} Synchronous_Task_Control is now Preelaborated, so it can be used in preelaborated units. 


#### Extensions to Ada 2005

{AI05-0168-1} Child package Ada.Synchronous_Task_Control.EDF is new. 


#### Wording Changes from Ada 2012

{AI12-0171-1} Correction: Clarified that Suspend_Until_True should only be called from a single task, and what happens if that is violated. 


## D.10.1  Synchronous Barriers

{AI05-0174-1} {AI05-0299-1} This subclause introduces a language-defined package to synchronously release a group of tasks after the number of blocked tasks reaches a specified count value. 


#### Static Semantics

{AI05-0174-1} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} package Ada.Synchronous_Barriers
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
   subtype Barrier_Limit is Positive range 1 .. implementation-defined;

```

Implementation defined: The value of Barrier_Limit'Last in Synchronous_Barriers.

```ada
   type Synchronous_Barrier (Release_Threshold : Barrier_Limit) is limited private;

```

```ada
{AI12-0241-1}    procedure Wait_For_Release (The_Barrier : in out Synchronous_Barrier;
                               Notified    :    out Boolean)
      with Nonblocking =&gt False;

```

```ada
private
   -- not specified by the language
end Ada.Synchronous_Barriers;

```

{AI05-0174-1} Type Synchronous_Barrier needs finalization (see 7.6). 


#### Dynamic Semantics

{AI05-0174-1} Each call to Wait_For_Release blocks the calling task until the number of blocked tasks associated with the Synchronous_Barrier object is equal to Release_Threshold, at which time all blocked tasks are released. Notified is set to True for one of the released tasks, and set to False for all other released tasks.

{AI05-0174-1} The mechanism for determining which task sets Notified to True is implementation defined.

{AI05-0174-1} Once all tasks have been released, a Synchronous_Barrier object may be reused to block another Release_Threshold number of tasks.

{AI05-0174-1} As the first step of the finalization of a Synchronous_Barrier, each blocked task is unblocked and Program_Error is raised at the place of the call to Wait_For_Release.

{AI05-0174-1} It is implementation defined whether an abnormal task which is waiting on a Synchronous_Barrier object is aborted immediately or aborted when the tasks waiting on the object are released. 

Implementation defined: When an aborted task that is waiting on a Synchronous_Barrier is aborted.

This paragraph was deleted.{AI05-0174-1} {AI12-0241-1} 


#### Bounded (Run-Time) Errors

{AI05-0174-1} It is a bounded error to call Wait_For_Release on a Synchronous_Barrier object after that object is finalized. If the error is detected, Program_Error is raised. Otherwise, the call proceeds normally, which may leave a task blocked forever. 


#### Extensions to Ada 2005

{AI05-0174-1} The package Ada.Synchronous_Barriers is new. 

