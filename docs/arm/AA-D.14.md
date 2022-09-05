---
sidebar_position:  165
---

# D.14  Execution Time

{AI95-00307-01} {AI05-0299-1} This subclause describes a language-defined package to measure execution time. 


#### Static Semantics

{AI95-00307-01} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Task_Identification;
with Ada.Real_Time; use Ada.Real_Time;
package Ada.Execution_Time
   with Nonblocking, Global =&gt in out synchronized is

```

```ada
   type CPU_Time is private;
   CPU_Time_First : constant CPU_Time;
   CPU_Time_Last  : constant CPU_Time;
   CPU_Time_Unit  : constant := implementation-defined-real-number;
   CPU_Tick : constant Time_Span;

```

```ada
   function Clock
     (T : Ada.Task_Identification.Task_Id
          := Ada.Task_Identification.Current_Task)
     return CPU_Time;

```

```ada
   function "+"  (Left : CPU_Time; Right : Time_Span) return CPU_Time;
   function "+"  (Left : Time_Span; Right : CPU_Time) return CPU_Time;
   function "-"  (Left : CPU_Time; Right : Time_Span) return CPU_Time;
   function "-"  (Left : CPU_Time; Right : CPU_Time)  return Time_Span;

```

```ada
   function "&lt"  (Left, Right : CPU_Time) return Boolean;
   function "&lt=" (Left, Right : CPU_Time) return Boolean;
   function "&gt"  (Left, Right : CPU_Time) return Boolean;
   function "&gt=" (Left, Right : CPU_Time) return Boolean;

```

```ada
   procedure Split
     (T : in CPU_Time; SC : out Seconds_Count; TS : out Time_Span);

```

```ada
   function Time_Of (SC : Seconds_Count;
                     TS : Time_Span := Time_Span_Zero) return CPU_Time;

```

```ada
{AI05-0170-1}    Interrupt_Clocks_Supported : constant Boolean := implementation-defined;

```

```ada
{AI05-0170-1}    Separate_Interrupt_Clocks_Supported : constant Boolean :=
     implementation-defined;

```

```ada
{AI05-0170-1}    function Clock_For_Interrupts return CPU_Time;

```

```ada
private
   ... -- not specified by the language
end Ada.Execution_Time;

```

{AI95-00307-01} {AI05-0170-1} {AI05-0269-1} The execution time or CPU time of a given task is defined as the time spent by the system executing that task, including the time spent executing run-time or system services on its behalf. The mechanism used to measure execution time is implementation defined. The Boolean constant Interrupt_Clocks_Supported is set to True if the implementation separately accounts for the execution time of interrupt handlers. If it is set to False it is implementation defined which task, if any, is charged the execution time that is consumed by interrupt handlers. The Boolean constant Separate_Interrupt_Clocks_Supported is set to True if the implementation separately accounts for the execution time of individual interrupt handlers (see D.14.3). 

Discussion: The implementation-defined properties above and of the values declared in the package are repeated in Documentation Requirements, so we don't mark them as implementation-defined. 

{AI95-00307-01} The type CPU_Time represents the execution time of a task. The set of values of this type corresponds one-to-one with an implementation-defined range of mathematical integers.

{AI95-00307-01} The CPU_Time value I represents the half-open execution-time interval that starts with I*CPU_Time_Unit and is limited by (I+1)*CPU_Time_Unit, where CPU_Time_Unit is an implementation-defined real number. For each task, the execution time value is set to zero at the creation of the task.

Ramification: Since it is implementation-defined which task is charged execution time for system services, the execution time value may become nonzero even before the start of the activation of the task. 

{AI95-00307-01} CPU_Time_First and CPU_Time_Last are the smallest and largest values of the CPU_Time type, respectively. 

{AI05-0170-1} The execution time value for the function Clock_For_Interrupts is initialized to zero.


#### Dynamic Semantics

{AI95-00307-01} CPU_Time_Unit is the smallest amount of execution time representable by the CPU_Time type; it is expressed in seconds. A CPU clock tick is an execution time interval during which the clock value (as observed by calling the Clock function) remains constant. CPU_Tick is the average length of such intervals.

{AI95-00307-01} The effects of the operators on CPU_Time and Time_Span are as for the operators defined for integer types.

{AI95-00307-01} The function Clock returns the current execution time of the task identified by T; Tasking_Error is raised if that task has terminated; Program_Error is raised if the value of T is Task_Identification.Null_Task_Id.

{AI95-00307-01} The effects of the Split and Time_Of operations are defined as follows, treating values of type CPU_Time, Time_Span, and Seconds_Count as mathematical integers. The effect of Split (T, SC, TS) is to set SC and TS to values such that T*CPU_Time_Unit = SC*1.0 + TS*CPU_Time_Unit, and 0.0 &lt= TS*CPU_Time_Unit &lt 1.0. The value returned by Time_Of(SC,TS) is the execution-time value T such that T*CPU_Time_Unit=SC*1.0 + TS*CPU_Time_Unit.

{AI05-0170-1} The function Clock_For_Interrupts returns the total cumulative time spent executing within all interrupt handlers. This time is not allocated to any task execution time clock. If Interrupt_Clocks_Supported is set to False the function raises Program_Error.


#### Erroneous Execution

{AI95-00307-01} For a call of Clock, if the task identified by T no longer exists, the execution of the program is erroneous. 


#### Implementation Requirements

{AI95-00307-01} The range of CPU_Time values shall be sufficient to uniquely represent the range of execution times from the task start-up to 50 years of execution time later. CPU_Tick shall be no greater than 1 millisecond. 


#### Documentation Requirements

{AI95-00307-01} The implementation shall document the values of CPU_Time_First, CPU_Time_Last, CPU_Time_Unit, and CPU_Tick. 

Documentation Requirement: The values of CPU_Time_First, CPU_Time_Last, CPU_Time_Unit, and CPU_Tick of package Execution_Time.

{AI95-00307-01} The implementation shall document the properties of the underlying mechanism used to measure execution times, such as the range of values supported and any relevant aspects of the underlying hardware or operating system facilities used. 

Documentation Requirement: The properties of the mechanism used to implement package Execution_Time, including the values of the constants defined in the package.


#### Metrics

{AI95-00307-01} The implementation shall document the following metrics:

An upper bound on the execution-time duration of a clock tick. This is a value D such that if t1 and t2 are any execution times of a given task such that t1 &lt t2 and Clockt1 = Clockt2 then t2  t1 &lt= D.

An upper bound on the size of a clock jump. A clock jump is the difference between two successive distinct values of an execution-time clock (as observed by calling the Clock function with the same Task_Id).

An upper bound on the execution time of a call to the Clock function, in processor clock cycles.

Upper bounds on the execution times of the operators of the type CPU_Time, in processor clock cycles. 

Documentation Requirement: The metrics for execution time.


#### Implementation Permissions

{AI95-00307-01} {AI12-0444-1} Implementations targeted to machines with word size smaller than 32 bits may omit support for the full range and granularity of the CPU_Time type.


#### Implementation Advice

{AI95-00307-01} When appropriate, implementations should provide configuration mechanisms to change the value of CPU_Tick. 

Implementation Advice: When appropriate, implementations should provide configuration mechanisms to change the value of Execution_Time.CPU_Tick.


#### Extensions to Ada 95

{AI95-00307-01} The package Execution_Time is new. 


#### Incompatibilities With Ada 2005

{AI05-0170-1} Function Clock_For_Interrupts, and constants Interrupt_Clocks_Supported and Separate_Interrupt_Clocks_Supported are added to Execution_Time. If Execution_Time is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with a [defining_identifier](./AA-3.1#S0022) of one of the added entities is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Wording Changes from Ada 2005

{AI05-0170-1} If Interrupt_Clocks_Supported is True, it is now possible to determine the execution time of interrupt handlers. This is not an inconsistency, as not charging any task for such time was a legitimate implementation for Ada 2005. 


## D.14.1  Execution Time Timers

{AI95-00307-01} {AI05-0299-1} This subclause describes a language-defined package that provides a facility for calling a handler when a task has used a defined amount of CPU time. 


#### Static Semantics

{AI95-00307-01} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} with System;
package Ada.Execution_Time.Timers
   with Nonblocking, Global =&gt in out synchronized is

```

```ada
   type Timer (T : not null access constant
                       Ada.Task_Identification.Task_Id) is
      tagged limited private;

```

```ada
{AI12-0241-1}    type Timer_Handler is
      access protected procedure (TM : in out Timer)
      with Nonblocking =&gt False;

```

```ada
   Min_Handler_Ceiling : constant System.Any_Priority :=
   implementation-defined;

```

```ada
   procedure Set_Handler (TM      : in out Timer;
                          In_Time : in Time_Span;
                          Handler : in Timer_Handler);
   procedure Set_Handler (TM      : in out Timer;
                          At_Time : in CPU_Time;
                          Handler : in Timer_Handler);
   function Current_Handler (TM : Timer) return Timer_Handler;
   procedure Cancel_Handler (TM        : in out Timer;
                             Cancelled :    out Boolean);

```

```ada
   function Time_Remaining (TM : Timer) return Time_Span;

```

```ada
   Timer_Resource_Error : exception;

```

```ada
private
   ... -- not specified by the language
end Ada.Execution_Time.Timers;

```

{AI95-00307-01} The type Timer represents an execution-time event for a single task and is capable of detecting execution-time overruns. The access discriminant T identifies the task concerned. The type Timer needs finalization (see 7.6).

{AI95-00307-01} An object of type Timer is said to be set if it is associated with a nonnull value of type Timer_Handler and cleared otherwise. All Timer objects are initially cleared. 

{AI95-00307-01} The type Timer_Handler identifies a protected procedure to be executed by the implementation when the timer expires. Such a protected procedure is called a handler. 

Discussion: Type Timer is tagged. This makes it possible to share a handler between several events. In simple cases, 'Access can be used to compare the parameter with a specific timer object (this works because a tagged type is a by-reference type). In more complex cases, a type extension of type Timer can be declared; a double type conversion can be used to access the extension data. An example of how this can be done can be found for the similar type Timing_Event, see D.15. 


#### Dynamic Semantics

{AI95-00307-01} When a Timer object is created, or upon the first call of a Set_Handler procedure with the timer as parameter, the resources required to operate an execution-time timer based on the associated execution-time clock are allocated and initialized. If this operation would exceed the available resources, Timer_Resource_Error is raised.

{AI95-00307-01} {AI05-0264-1} The procedures Set_Handler associate the handler Handler with the timer TM: if Handler is null, the timer is cleared; otherwise, it is set. The first procedure Set_Handler loads the timer TM with an interval specified by the Time_Span parameter. In this mode, the timer TM expires when the execution time of the task identified by TM.T.all has increased by In_Time; if In_Time is less than or equal to zero, the timer expires immediately. The second procedure Set_Handler loads the timer TM with the absolute value specified by At_Time. In this mode, the timer TM expires when the execution time of the task identified by TM.T.all reaches At_Time; if the value of At_Time has already been reached when Set_Handler is called, the timer expires immediately.

Implementation Note: Since an access-to-constant can designate a variable, the Task_Id value designated by the discriminant of a Timer object can be changed after the object is created. Thus, an implementation cannot use the value of the Task_Id other than where this Reference Manual specifies. For instance, the Task_Id should be read when the timer is set, but it should not be used when the timer expires (as it may designate a different task at that point). 

{AI95-00307-01} A call of a procedure Set_Handler for a timer that is already set replaces the handler and the (absolute or relative) execution time; if Handler is not null, the timer remains set.

{AI95-00307-01} When a timer expires, the associated handler is executed, passing the timer as parameter. The initial action of the execution of the handler is to clear the event.

{AI95-00307-01} {AI05-0264-1} The function Current_Handler returns the handler associated with the timer TM if that timer is set; otherwise, it returns null.

{AI95-00307-01} {AI05-0264-1} The procedure Cancel_Handler clears the timer if it is set. Cancelled is assigned True if the timer was set prior to it being cleared; otherwise, it is assigned False.

{AI95-00307-01} {AI05-0264-1} The function Time_Remaining returns the execution time interval that remains until the timer TM would expire, if that timer is set; otherwise, it returns Time_Span_Zero.

{AI95-00307-01} The constant Min_Handler_Ceiling is the minimum ceiling priority required for a protected object with a handler to ensure that no ceiling violation will occur when that handler is invoked.

{AI95-00307-01} As part of the finalization of an object of type Timer, the timer is cleared.

{AI95-00307-01} For all the subprograms defined in this package, Tasking_Error is raised if the task identified by TM.T.all has terminated, and Program_Error is raised if the value of TM.T.all is Task_Identification.Null_Task_Id.

{AI95-00307-01} An exception propagated from a handler invoked as part of the expiration of a timer has no effect.


#### Erroneous Execution

{AI95-00307-01} For a call of any of the subprograms defined in this package, if the task identified by TM.T.all no longer exists, the execution of the program is erroneous.


#### Implementation Requirements

{AI95-00307-01} For a given Timer object, the implementation shall perform the operations declared in this package atomically with respect to any of these operations on the same Timer object. The replacement of a handler by a call of Set_Handler shall be performed atomically with respect to the execution of the handler.

Reason: This prevents various race conditions. In particular it ensures that if an event occurs when Set_Handler is changing the handler then either the new or old handler is executed in response to the appropriate event. It is never possible for a new handler to be executed in response to an old event 

{AI95-00307-01} When an object of type Timer is finalized, the system resources used by the timer shall be deallocated.


#### Implementation Permissions

{AI95-00307-01} {AI05-0264-1} Implementations may limit the number of timers that can be defined for each task. If this limit is exceeded, then Timer_Resource_Error is raised.

NOTE 1   {AI95-00307-01} A Timer_Handler can be associated with several Timer objects.


#### Extensions to Ada 95

{AI95-00307-01} The package Execution_Time.Timers is new. 


## D.14.2  Group Execution Time Budgets

{AI95-00354-01} {AI05-0299-1} This subclause describes a language-defined package to assign execution time budgets to groups of tasks. 


#### Static Semantics

{AI95-00354-01} The following language-defined library package exists: 

```ada
{AI05-0169-1} {AI12-0241-1} {AI12-0302-1} with System;
with System.Multiprocessors;
package Ada.Execution_Time.Group_Budgets
  with Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI05-0092-1} {AI05-0169-1}   type Group_Budget(CPU : System.Multiprocessors.CPU :=
                             System.Multiprocessors.CPU'First)
    is tagged limited private;

```

```ada
{AI12-0241-1}   type Group_Budget_Handler is access
       protected procedure (GB : in out Group_Budget)
       with Nonblocking =&gt False;

```

```ada
  type Task_Array is array (Positive range &lt&gt) of
                                  Ada.Task_Identification.Task_Id;

```

```ada
  Min_Handler_Ceiling : constant System.Any_Priority :=
    implementation-defined;

```

Implementation defined: The value of Min_Handler_Ceiling in Execution_Time.Group_Budgets.

```ada
  procedure Add_Task (GB : in out Group_Budget;
                      T  : in Ada.Task_Identification.Task_Id);
  procedure Remove_Task (GB: in out Group_Budget;
                         T  : in Ada.Task_Identification.Task_Id);
  function Is_Member (GB : Group_Budget;
                      T : Ada.Task_Identification.Task_Id) return Boolean;
  function Is_A_Group_Member
     (T : Ada.Task_Identification.Task_Id) return Boolean;
  function Members (GB : Group_Budget) return Task_Array;

```

```ada
  procedure Replenish (GB : in out Group_Budget; To : in Time_Span);
  procedure Add (GB : in out Group_Budget; Interval : in Time_Span);
  function Budget_Has_Expired (GB : Group_Budget) return Boolean;
  function Budget_Remaining (GB : Group_Budget) return Time_Span;

```

```ada
  procedure Set_Handler (GB      : in out Group_Budget;
                         Handler : in Group_Budget_Handler);
  function Current_Handler (GB : Group_Budget)
     return Group_Budget_Handler;
  procedure Cancel_Handler (GB        : in out Group_Budget;
                            Cancelled : out Boolean);

```

```ada
  Group_Budget_Error : exception;

```

```ada
private
    --  not specified by the language
end Ada.Execution_Time.Group_Budgets;

```

{AI95-00354-01} The type Group_Budget represents an execution time budget to be used by a group of tasks. The type Group_Budget needs finalization (see 7.6). A task can belong to at most one group. Tasks of any priority can be added to a group.

{AI95-00354-01} An object of type Group_Budget has an associated nonnegative value of type Time_Span known as its budget, which is initially Time_Span_Zero. The type Group_Budget_Handler identifies a protected procedure to be executed by the implementation when the budget is exhausted, that is, reaches zero. Such a protected procedure is called a handler. 

{AI95-00354-01} An object of type Group_Budget also includes a handler, which is a value of type Group_Budget_Handler. The handler of the object is said to be set if it is not null and cleared otherwise. The handler of all Group_Budget objects is initially cleared. 

Discussion: Type Group_Budget is tagged. This makes it possible to share a handler between several events. In simple cases, 'Access can be used to compare the parameter with a specific group budget object (this works because a tagged type is a by-reference type). In more complex cases, a type extension of type Group_Budget can be declared; a double type conversion can be used to access the extension data. An example of how this can be done can be found for the similar type Timing_Event, see D.15. 


#### Dynamic Semantics

{AI95-00354-01} The procedure Add_Task adds the task identified by T to the group GB; if that task is already a member of some other group, Group_Budget_Error is raised.

{AI95-00354-01} The procedure Remove_Task removes the task identified by T from the group GB; if that task is not a member of the group GB, Group_Budget_Error is raised. After successful execution of this procedure, the task is no longer a member of any group.

{AI95-00354-01} {AI05-0264-1} The function Is_Member returns True if the task identified by T is a member of the group GB; otherwise, it returns False.

{AI95-00354-01} {AI05-0264-1} The function Is_A_Group_Member returns True if the task identified by T is a member of some group; otherwise, it returns False.

{AI95-00354-01} The function Members returns an array of values of type Task_Identification.Task_Id identifying the members of the group GB. The order of the components of the array is unspecified.

{AI95-00354-01} {AI05-0092-1} {AI05-0169-1} The procedure Replenish loads the group budget GB with To as the Time_Span value. The exception Group_Budget_Error is raised if the Time_Span value To is nonpositive. Any execution on CPU of any member of the group of tasks results in the budget counting down, unless exhausted. When the budget becomes exhausted (reaches Time_Span_Zero), the associated handler is executed if the handler of group budget GB is set. Nevertheless, the tasks continue to execute.

{AI95-00354-01} The procedure Add modifies the budget of the group GB. A positive value for Interval increases the budget. A negative value for Interval reduces the budget, but never below Time_Span_Zero. A zero value for Interval has no effect. A call of procedure Add that results in the value of the budget going to Time_Span_Zero causes the associated handler to be executed if the handler of the group budget GB is set.

{AI95-00354-01} {AI05-0264-1} The function Budget_Has_Expired returns True if the budget of group GB is exhausted (equal to Time_Span_Zero); otherwise, it returns False.

{AI95-00354-01} The function Budget_Remaining returns the remaining budget for the group GB. If the budget is exhausted it returns Time_Span_Zero. This is the minimum value for a budget.

{AI95-00354-01} {AI05-0264-1} The procedure Set_Handler associates the handler Handler with the Group_Budget GB: if Handler is null, the handler of Group_Budget is cleared; otherwise, it is set.

{AI95-00354-01} A call of Set_Handler for a Group_Budget that already has a handler set replaces the handler; if Handler is not null, the handler for Group_Budget remains set.

{AI95-00354-01} {AI05-0264-1} The function Current_Handler returns the handler associated with the group budget GB if the handler for that group budget is set; otherwise, it returns null.

{AI95-00354-01} {AI05-0264-1} The procedure Cancel_Handler clears the handler for the group budget if it is set. Cancelled is assigned True if the handler for the group budget was set prior to it being cleared; otherwise, it is assigned False.

{AI95-00354-01} The constant Min_Handler_Ceiling is the minimum ceiling priority required for a protected object with a handler to ensure that no ceiling violation will occur when that handler is invoked.

{AI95-00354-01} The precision of the accounting of task execution time to a Group_Budget is the same as that defined for execution-time clocks from the parent package.

{AI95-00354-01} As part of the finalization of an object of type Group_Budget all member tasks are removed from the group identified by that object.

{AI95-00354-01} {AI05-0264-1} If a task is a member of a Group_Budget when it terminates, then as part of the finalization of the task it is removed from the group.

{AI95-00354-01} For all the operations defined in this package, Tasking_Error is raised if the task identified by T has terminated, and Program_Error is raised if the value of T is Task_Identification.Null_Task_Id.

{AI95-00354-01} An exception propagated from a handler invoked when the budget of a group of tasks becomes exhausted has no effect.


#### Erroneous Execution

{AI95-00354-01} For a call of any of the subprograms defined in this package, if the task identified by T no longer exists, the execution of the program is erroneous. 


#### Implementation Requirements

{AI95-00354-01} For a given Group_Budget object, the implementation shall perform the operations declared in this package atomically with respect to any of these operations on the same Group_Budget object. The replacement of a handler, by a call of Set_Handler, shall be performed atomically with respect to the execution of the handler. 

Reason: This prevents various race conditions. In particular it ensures that if the budget is exhausted when Set_Handler is changing the handler then either the new or old handler is executed and the exhausting event is not lost. 

NOTE 1   {AI95-00354-01} Clearing or setting of the handler of a group budget does not change the current value of the budget. Exhaustion or loading of a budget does not change whether the handler of the group budget is set or cleared.

NOTE 2   {AI95-00354-01} A Group_Budget_Handler can be associated with several Group_Budget objects. 


#### Extensions to Ada 95

{AI95-00354-01} The package Execution_Time.Group_Budgets is new. 


#### Inconsistencies With Ada 2005

{AI05-0169-1} A Group_Budget is now defined to work on a single processor. If an implementation managed to make this package work for programs running on a multiprocessor system, and a program depends on that fact, it could fail when ported to Ada 2012. We believe it is unlikely that such an implementation exists because of the difficulty of signalling other processors when the time reaches zero; in any case, depending on such an implementation is not portable. 


## D.14.3  Execution Time of Interrupt Handlers

{AI05-0170-1} {AI05-0299-1} This subclause describes a language-defined package to measure the execution time of interrupt handlers. 


#### Static Semantics

{AI05-0170-1} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} with Ada.Interrupts;
package Ada.Execution_Time.Interrupts
   with Nonblocking, Global =&gt in out synchronized is
   function Clock (Interrupt : Ada.Interrupts.Interrupt_Id)
        return CPU_Time;
   function Supported (Interrupt : Ada.Interrupts.Interrupt_Id)
        return Boolean;
end Ada.Execution_Time.Interrupts;

```

{AI05-0170-1} The execution time or CPU time of a given interrupt Interrupt is defined as the time spent by the system executing interrupt handlers identified by Interrupt, including the time spent executing run-time or system services on its behalf. The mechanism used to measure execution time is implementation defined. Time spent executing interrupt handlers is distinct from time spent executing any task. 

Discussion: The implementation-defined mechanism here is the same as that covered by the Documentation Requirements of D.14, so we don't repeat that requirement here. 

{AI05-0170-1} For each interrupt, the execution time value is initially set to zero. 


#### Dynamic Semantics

{AI05-0170-1} The function Clock returns the current cumulative execution time of the interrupt identified by Interrupt. If Separate_Interrupt_Clocks_Supported is set to False the function raises Program_Error.

{AI05-0170-1} {AI05-0264-1} The function Supported returns True if the implementation is monitoring the execution time of the interrupt identified by Interrupt; otherwise, it returns False. For any Interrupt_Id Interrupt for which Supported(Interrupt) returns False, the function Clock(Interrupt) will return a value equal to Ada.Execution_Time.Time_Of(0).


#### Extensions to Ada 2005

{AI05-0170-1} The package Execution_Time.Interrupts is new. 

