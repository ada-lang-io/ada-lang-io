---
sidebar_position:  211
---

# M.1  Specific Documentation Requirements

{AI12-0442-1} In addition to implementation-defined characteristics, each Ada implementation is required to document various properties of the implementation: 

Ramification: Most of the items in this list require documentation only for implementations that conform to Specialized Needs Annexes. 

The behavior of implementations in implementation-defined situations shall be documented - see M.2, "Implementation-Defined Characteristics" for a listing. See 1.1.1(77).

The set of values that a user-defined Allocate procedure needs to accept for the Alignment parameter. How the standard storage pool is chosen, and how storage is allocated by standard storage pools. See 13.11(23).

The algorithm used for random number generation, including a description of its period. See A.5.2(44).

The minimum time interval between calls to the time-dependent Reset procedure that is guaranteed to initiate different random number sequences. See A.5.2(45).

The conditions under which Io_Exceptions.Name_Error, Io_Exceptions.Use_Error, and Io_Exceptions.Device_Error are propagated. See A.13(15).

The behavior of package Environment_Variables when environment variables are changed by external mechanisms. See A.17(30/2).

The overhead of calling machine-code or intrinsic subprograms. See C.1(6).

The types and attributes used in machine code insertions. See C.1(7).

The subprogram calling conventions for all supported convention identifiers. See C.1(8/3).

The mapping between the Link_Name or Ada designator and the external link name. See C.1(9).

The treatment of interrupts. See C.3(22).

The metrics for interrupt handlers. See C.3.1(16).

If the Ceiling_Locking policy is in effect, the default ceiling priority for a protected object that specifies an interrupt handler aspect. See C.3.2(24/5).

Any circumstances when the elaboration of a preelaborated package causes code to be executed. See C.4(12).

Whether a partition can be restarted without reloading. See C.4(13).

The effect of calling Current_Task from an entry body or interrupt handler. See C.7.1(19).

For package Task_Attributes, limits on the number and size of task attributes, and how to configure any limits. See C.7.2(19).

The metrics for the Task_Attributes package. See C.7.2(27).

The details of the configuration used to generate the values of all metrics. See D(2).

The maximum priority inversion a user task can experience from the implementation. See D.2.3(12/2).

The amount of time that a task can be preempted for processing on behalf of lower-priority tasks. See D.2.3(13/2).

The quantum values supported for round robin dispatching. See D.2.5(16/2).

The accuracy of the detection of the exhaustion of the budget of a task for round robin dispatching. See D.2.5(17/2).

Any conditions that cause the completion of the setting of the deadline of a task to be delayed for a multiprocessor. See D.2.6(32/2).

Any conditions that cause the completion of the setting of the priority of a task to be delayed for a multiprocessor. See D.5.1(12.1/2).

The metrics for Set_Priority. See D.5.1(14).

The metrics for setting the priority of a protected object. See D.5.2(10).

On a multiprocessor, any conditions that cause the completion of an aborted construct to be delayed later than what is specified for a single processor. See D.6(3).

The metrics for aborts. See D.6(8).

The values of Time_First, Time_Last, Time_Span_First, Time_Span_Last, Time_Span_Unit, and Tick for package Real_Time. See D.8(33).

The properties of the underlying time base used in package Real_Time. See D.8(34).

Any synchronization of package Real_Time with external time references. See D.8(35).

Any aspects of the external environment that can interfere with package Real_Time. See D.8(36/5).

The metrics for package Real_Time. See D.8(45).

The minimum value of the delay expression of a [delay_relative_statement](./AA-9.6#S0268) that causes a task to actually be blocked. See D.9(7).

The minimum difference between the value of the delay expression of a [delay_until_statement](./AA-9.6#S0267) and the value of Real_Time.Clock, that causes the task to actually be blocked. See D.9(8).

The metrics for delay statements. See D.9(13).

The upper bound on the duration of interrupt blocking caused by the implementation. See D.12(5).

The metrics for entry-less protected objects. See D.12(12).

The values of CPU_Time_First, CPU_Time_Last, CPU_Time_Unit, and CPU_Tick of package Execution_Time. See D.14(21/2).

The properties of the mechanism used to implement package Execution_Time, including the values of the constants defined in the package. See D.14(22/2).

The metrics for execution time. See D.14(27).

The metrics for timing events. See D.15(24).

The processor(s) on which the clock interrupt is handled; the processors on which each Interrupt_Id can be handled. See D.16.1(32).

Whether the RPC-receiver is invoked from concurrent tasks, and if so, the number of such tasks. See E.5(25).

Any techniques used to reduce cancellation errors in Numerics.Generic_Real_Arrays shall be documented. See G.3.1(86/2).

Any techniques used to reduce cancellation errors in Numerics.Generic_Complex_Arrays shall be documented. See G.3.2(155/2).

If a [pragma](./AA-2.8#S0019) Normalize_Scalars applies, the implicit initial values of scalar subtypes shall be documented. Such a value should be an invalid representation when possible; any cases when is it not shall be documented. See H.1(5/2).

The range of effects for each bounded error and each unspecified effect. If the effects of a given erroneous construct are constrained, the constraints shall be documented. See H.2(1).

For each inspection point, a mapping between each inspectable object and the machine resources where the object's value can be obtained shall be provided. See H.3.2(8).

If a pragma Restrictions(No_Exceptions) is specified, the effects of all constructs where language-defined checks are still performed. See H.4(25).

The interrupts to which a task entry may be attached. See J.7.1(12).

The type of entry call invoked for an interrupt entry. See J.7.1(13).

