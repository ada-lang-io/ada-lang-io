---
sidebar_position:  166
---

# D.15  Timing Events

{AI95-00297-01} {AI05-0299-1} {AI12-0445-1} This subclause describes a language-defined package to allow user-defined protected procedures to be executed at a specified time without the use of a task or a delay statement. 


#### Static Semantics

{AI95-00297-01} The following language-defined library package exists: 

```ada
{AI12-0241-1} {AI12-0302-1} package Ada.Real_Time.Timing_Events
  with Nonblocking, Global =&gt in out synchronized is

```

```ada
{AI12-0241-1}   type Timing_Event is tagged limited private;
  type Timing_Event_Handler
       is access protected procedure (Event : in out Timing_Event)
       with Nonblocking =&gt False;

```

```ada
  procedure Set_Handler (Event   : in out Timing_Event;
                         At_Time : in Time;
                         Handler : in Timing_Event_Handler);
  procedure Set_Handler (Event   : in out Timing_Event;
                         In_Time : in Time_Span;
                         Handler : in Timing_Event_Handler);
  function Current_Handler (Event : Timing_Event)
       return Timing_Event_Handler;
  procedure Cancel_Handler (Event     : in out Timing_Event;
                            Cancelled : out Boolean);

```

```ada
  function Time_Of_Event (Event : Timing_Event) return Time;

```

```ada
private
  ... -- not specified by the language
end Ada.Real_Time.Timing_Events;

```

{AI95-00297-01} The type Timing_Event represents a time in the future when an event is to occur. The type Timing_Event needs finalization (see 7.6).

{AI95-00297-01} An object of type Timing_Event is said to be set if it is associated with a nonnull value of type Timing_Event_Handler and cleared otherwise. All Timing_Event objects are initially cleared. 

{AI95-00297-01} The type Timing_Event_Handler identifies a protected procedure to be executed by the implementation when the timing event occurs. Such a protected procedure is called a handler. 

Discussion: Type Timing_Event is tagged. This makes it possible to share a handler between several events. In simple cases, 'Access can be used to compare the parameter with a specific timing event object (this works because a tagged type is a by-reference type). In more complex cases, a type extension of type Timing_Event can be declared; a double type conversion can be used to access the extension data. For example:

```ada
type Toaster_Timing_Event is new Timing_Event with record
   Slot : Natural;
end record;

```

```ada
...

```

```ada
protected body Toaster is

```

```ada
   procedure Timer (Event : in out Timing_Event) is
   begin
      Pop_Up_Toast (Toaster_Timing_Event(Timing_Event'Class(Event)).Slot);
   end Timer;

```

```ada
   ...
end Toaster;

```

The extra conversion to the class-wide type is necessary to make the conversions legal. While this usage is clearly ugly, we think that the need for this sort of usage will be rare, so we can live with it. It's certainly better than having no way to associate data with an event. 


#### Dynamic Semantics

{AI95-00297-01} {AI05-0264-1} The procedures Set_Handler associate the handler Handler with the event Event: if Handler is null, the event is cleared; otherwise, it is set. The first procedure Set_Handler sets the execution time for the event to be At_Time. The second procedure Set_Handler sets the execution time for the event to be Real_Time.Clock + In_Time.

{AI95-00297-01} A call of a procedure Set_Handler for an event that is already set replaces the handler and the time of execution; if Handler is not null, the event remains set.

{AI95-00297-01} As soon as possible after the time set for the event, the handler is executed, passing the event as parameter. The handler is only executed if the timing event is in the set state at the time of execution. The initial action of the execution of the handler is to clear the event.

Reason: The second sentence of this paragraph is because of a potential race condition. The time might expire and yet before the handler is executed, some task could call Cancel_Handler (or equivalently call Set_Handler with a null parameter) and thus clear the handler. 

{AI95-00297-01} If the Ceiling_Locking policy (see D.3) is in effect when a procedure Set_Handler is called, a check is made that the ceiling priority of Handler.all is Interrupt_Priority'Last. If the check fails, Program_Error is raised.

{AI95-00297-01} {AI05-0094-1} {AI05-0264-1} If a procedure Set_Handler is called with zero or negative In_Time or with At_Time indicating a time in the past, then the handler is executed as soon as possible after the completion of the call of Set_Handler.

Ramification: {AI05-0094-1} The handler will still be executed. Under no circumstances is a scheduled call of a handler lost. 

Discussion: {AI05-0094-1} We say "as soon as possible" so that we do not deadlock if we are executing the handler when Set_Handler is called. In that case, the current invocation of the handler must complete before the new handler can start executing. 

{AI95-00297-01} {AI05-0264-1} The function Current_Handler returns the handler associated with the event Event if that event is set; otherwise, it returns null.

{AI95-00297-01} {AI05-0264-1} The procedure Cancel_Handler clears the event if it is set. Cancelled is assigned True if the event was set prior to it being cleared; otherwise, it is assigned False.

{AI95-00297-01} {AI05-0264-1} The function Time_Of_Event returns the time of the event if the event is set; otherwise, it returns Real_Time.Time_First.

{AI95-00297-01} As part of the finalization of an object of type Timing_Event, the Timing_Event is cleared.

Implementation Note: This is the only finalization defined by the language that has a visible effect; but an implementation may have other finalization that it needs to perform. Implementations need to ensure that the event is cleared before anything else is finalized that would prevent a set event from being triggered. 

{AI95-00297-01} If several timing events are set for the same time, they are executed in FIFO order of being set.

{AI95-00297-01} An exception propagated from a handler invoked by a timing event has no effect.


#### Implementation Requirements

{AI95-00297-01} For a given Timing_Event object, the implementation shall perform the operations declared in this package atomically with respect to any of these operations on the same Timing_Event object. The replacement of a handler by a call of Set_Handler shall be performed atomically with respect to the execution of the handler.

Reason: This prevents various race conditions. In particular it ensures that if an event occurs when Set_Handler is changing the handler then either the new or old handler is executed in response to the appropriate event. It is never possible for a new handler to be executed in response to an old event. 


#### Metrics

{AI95-00297-01} The implementation shall document the following metric: 

{AI05-0210-1} An upper bound on the lateness of the execution of a handler. That is, the maximum time between the time specified for the event and when a handler is actually invoked assuming no other handler or task is executing during this interval.

Documentation Requirement: The metrics for timing events.


#### Implementation Advice

{AI95-00297-01} The protected handler procedure should be executed directly by the real-time clock interrupt mechanism.

Implementation Advice: For a timing event, the handler should be executed directly by the real-time clock interrupt mechanism.

NOTE 1   {AI95-00297-01} Since a call of Set_Handler is not a potentially blocking operation, it can be called from within a handler.

NOTE 2   {AI95-00297-01} A Timing_Event_Handler can be associated with several Timing_Event objects.


#### Extensions to Ada 95

{AI95-00297-01} The package Real_Time.Timing_Events is new. 


#### Wording Changes from Ada 2005

{AI05-0094-1} Correction: Reworded to eliminate a deadlock condition if the event time is in the past and a handler is currently executing. This is technically an inconsistency, but only if a program is depending on deadlocking; since it is impossible to imagine how that could be useful, we have not documented this as an inconsistency.

{AI05-0210-1} Correction: Clarified the metric for lateness of a timing event to exclude interference from other handlers and tasks. This change might change the documentation of an implementation, but not the implementation itself, so there is no inconsistency. 

