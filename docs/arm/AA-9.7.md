---
sidebar_position:  78
---

# 9.7  Select Statements

[There are four forms of the [select_statement](./AA-9.7#S0269). One form provides a selective wait for one or more [select_alternative](./AA-9.7#S0272)s. Two provide timed and conditional entry calls. The fourth provides asynchronous transfer of control.] 


#### Syntax

select_statement<a id="S0269"></a> ::= 
   [selective_accept](./AA-9.7#S0270)
  | [timed_entry_call](./AA-9.7#S0276)
  | [conditional_entry_call](./AA-9.7#S0279)
  | [asynchronous_select](./AA-9.7#S0280)


#### Examples

Example of a select statement: 

```ada
select
   accept Driver_Awake_Signal;
or
   delay 30.0*Seconds;
   Stop_The_Train;
end select;

```


#### Extensions to Ada 83

[Asynchronous_select](./AA-9.7#S0280) is new. 


## 9.7.1  Selective Accept

[This form of the [select_statement](./AA-9.7#S0269) allows a combination of waiting for, and selecting from, one or more alternatives. The selection may depend on conditions associated with each alternative of the [selective_accept](./AA-9.7#S0270). ]


#### Syntax

selective_accept<a id="S0270"></a> ::= 
  select
   [[guard](./AA-9.7#S0271)]
     [select_alternative](./AA-9.7#S0272)
{ or
   [[guard](./AA-9.7#S0271)]
     [select_alternative](./AA-9.7#S0272) }
[ else
   [sequence_of_statements](./AA-5.1#S0166) ]
  end select;

guard<a id="S0271"></a> ::= when [condition](./AA-4.5#S0150) =&gt

select_alternative<a id="S0272"></a> ::= 
   [accept_alternative](./AA-9.7#S0273)
  | [delay_alternative](./AA-9.7#S0274)
  | [terminate_alternative](./AA-9.7#S0275)

accept_alternative<a id="S0273"></a> ::= 
  [accept_statement](./AA-9.5#S0258) [[sequence_of_statements](./AA-5.1#S0166)]

delay_alternative<a id="S0274"></a> ::= 
  [delay_statement](./AA-9.6#S0266) [[sequence_of_statements](./AA-5.1#S0166)]

terminate_alternative<a id="S0275"></a> ::= terminate;

A [selective_accept](./AA-9.7#S0270) shall contain at least one [accept_alternative](./AA-9.7#S0273). In addition, it can contain: 

a [terminate_alternative](./AA-9.7#S0275) (only one); or

one or more [delay_alternative](./AA-9.7#S0274)s; or

an else part (the reserved word else followed by a [sequence_of_statements](./AA-5.1#S0166)). 

These three possibilities are mutually exclusive. 


#### Legality Rules

If a [selective_accept](./AA-9.7#S0270) contains more than one [delay_alternative](./AA-9.7#S0274), then all shall be [delay_relative_statement](./AA-9.6#S0268)s, or all shall be [delay_until_statement](./AA-9.6#S0267)s for the same time type. 

Reason: This simplifies the implementation and the description of the semantics. 


#### Dynamic Semantics

A [select_alternative](./AA-9.7#S0272) is said to be open if it is not immediately preceded by a [guard](./AA-9.7#S0271), or if the [condition](./AA-4.5#S0150) of its [guard](./AA-9.7#S0271) evaluates to True. It is said to be closed otherwise.

For the execution of a [selective_accept](./AA-9.7#S0270), any [guard](./AA-9.7#S0271) [condition](./AA-4.5#S0150)s are evaluated; open alternatives are thus determined. For an open [delay_alternative](./AA-9.7#S0274), the delay_[expression](./AA-4.4#S0132) is also evaluated. Similarly, for an open [accept_alternative](./AA-9.7#S0273) for an entry of a family, the [entry_index](./AA-9.5#S0259) is also evaluated. These evaluations are performed in an arbitrary order, except that a delay_[expression](./AA-4.4#S0132) or [entry_index](./AA-9.5#S0259) is not evaluated until after evaluating the corresponding [condition](./AA-4.5#S0150), if any. Selection and execution of one open alternative, or of the else part, then completes the execution of the [selective_accept](./AA-9.7#S0270); the rules for this selection are described below.

Open [accept_alternative](./AA-9.7#S0273)s are first considered. Selection of one such alternative takes place immediately if the corresponding entry already has queued calls. If several alternatives can thus be selected, one of them is selected according to the entry queuing policy in effect (see 9.5.3 and D.4). When such an alternative is selected, the selected call is removed from its entry queue and the [handled_sequence_of_statements](./AA-11.2#S0304) (if any) of the corresponding [accept_statement](./AA-9.5#S0258) is executed; after the rendezvous completes any subsequent [sequence_of_statements](./AA-5.1#S0166) of the alternative is executed. If no selection is immediately possible (in the above sense) and there is no else part, the task blocks until an open alternative can be selected.

Selection of the other forms of alternative or of an else part is performed as follows: 

An open [delay_alternative](./AA-9.7#S0274) is selected when its expiration time is reached if no [accept_alternative](./AA-9.7#S0273) or other [delay_alternative](./AA-9.7#S0274) can be selected prior to the expiration time. If several [delay_alternative](./AA-9.7#S0274)s have this same expiration time, one of them is selected according to the queuing policy in effect (see D.4); the default queuing policy chooses arbitrarily among the [delay_alternative](./AA-9.7#S0274)s whose expiration time has passed.

The else part is selected and its [sequence_of_statements](./AA-5.1#S0166) is executed if no [accept_alternative](./AA-9.7#S0273) can immediately be selected; in particular, if all alternatives are closed.

{AI05-0299-1} An open [terminate_alternative](./AA-9.7#S0275) is selected if the conditions stated at the end of subclause 9.3 are satisfied. 

Ramification: In the absence of a [requeue_statement](./AA-9.5#S0265), the conditions stated are such that a [terminate_alternative](./AA-9.7#S0275) cannot be selected while there is a queued entry call for any entry of the task. In the presence of requeues from a task to one of its subtasks, it is possible that when a [terminate_alternative](./AA-9.7#S0275) of the subtask is selected, requeued calls (for closed entries only) might still be queued on some entry of the subtask. Tasking_Error will be propagated to such callers, as is usual when a task completes while queued callers remain. 

The exception Program_Error is raised if all alternatives are closed and there is no else part.

NOTE 1   A [selective_accept](./AA-9.7#S0270) is allowed to have several open [delay_alternative](./AA-9.7#S0274)s. A [selective_accept](./AA-9.7#S0270) is allowed to have several open [accept_alternative](./AA-9.7#S0273)s for the same entry.


#### Examples

Example of a task body with a selective accept: 

```ada
task body Server is
   Current_Work_Item : Work_Item;
begin
   loop
      select
         accept Next_Work_Item(WI : in Work_Item) do
            Current_Work_Item := WI;
         end;
         Process_Work_Item(Current_Work_Item);
      or
         accept Shut_Down;
         exit;       -- Premature shut down requested
      or
         terminate;  -- Normal shutdown at end of scope
      end select;
   end loop;
end Server;

```


#### Wording Changes from Ada 83

The name of selective_wait was changed to [selective_accept](./AA-9.7#S0270) to better describe what is being waited for. We kept [select_alternative](./AA-9.7#S0272) as is, because selective_accept_alternative was too easily confused with [accept_alternative](./AA-9.7#S0273). 


## 9.7.2  Timed Entry Calls

{AI95-00345-01} {AI12-0439-1} [A [timed_entry_call](./AA-9.7#S0276) issues an entry call that is cancelled if the call (or a requeue-with-abort of the call) is not selected before the expiration time is reached. A procedure call may appear rather than an entry call for cases where the procedure can be implemented by an entry. ]


#### Syntax

timed_entry_call<a id="S0276"></a> ::= 
  select
   [entry_call_alternative](./AA-9.7#S0277)
  or
   [delay_alternative](./AA-9.7#S0274)
  end select;

{AI95-00345-01} entry_call_alternative<a id="S0277"></a> ::= 
  [procedure_or_entry_call](./AA-9.7#S0278) [[sequence_of_statements](./AA-5.1#S0166)]

{AI95-00345-01} procedure_or_entry_call<a id="S0278"></a> ::= 
  [procedure_call_statement](./AA-6.4#S0217) | [entry_call_statement](./AA-9.5#S0264)


#### Legality Rules

{AI95-00345-01} If a [procedure_call_statement](./AA-6.4#S0217) is used for a [procedure_or_entry_call](./AA-9.7#S0278), the procedure_[name](./AA-4.1#S0091) or procedure_[prefix](./AA-4.1#S0093) of the [procedure_call_statement](./AA-6.4#S0217) shall statically denote an entry renamed as a procedure or (a view of) a primitive subprogram of a limited interface whose first parameter is a controlling parameter (see 3.9.2). 

Reason: This would be a confusing way to call a procedure, so we only allow it when it is possible that the procedure is actually an entry. We could have allowed formal subprograms here, but we didn't because we'd have to allow all formal subprograms, and it would increase the difficulty of generic code sharing.

We say "statically denotes" because an access-to-subprogram cannot be primitive, and we don't have anything like access-to-entry. So only names of entries or procedures are possible. 


#### Dynamic Semantics

{AI95-00345-01} For the execution of a [timed_entry_call](./AA-9.7#S0276), the entry_[name](./AA-4.1#S0091), procedure_[name](./AA-4.1#S0091), or procedure_[prefix](./AA-4.1#S0093), and any actual parameters are evaluated, as for a simple entry call (see 9.5.3) or procedure call (see 6.4). The expiration time (see 9.6) for the call is determined by evaluating the delay_[expression](./AA-4.4#S0132) of the [delay_alternative](./AA-9.7#S0274). If the call is an entry call or a call on a procedure implemented by an entry, the entry call is then issued. Otherwise, the call proceeds as described in 6.4 for a procedure call, followed by the [sequence_of_statements](./AA-5.1#S0166) of the [entry_call_alternative](./AA-9.7#S0277); the [sequence_of_statements](./AA-5.1#S0166) of the [delay_alternative](./AA-9.7#S0274) is ignored.

If the call is queued (including due to a requeue-with-abort), and not selected before the expiration time is reached, an attempt to cancel the call is made. If the call completes due to the cancellation, the optional [sequence_of_statements](./AA-5.1#S0166) of the [delay_alternative](./AA-9.7#S0274) is executed; if the entry call completes normally, the optional [sequence_of_statements](./AA-5.1#S0166) of the [entry_call_alternative](./AA-9.7#S0277) is executed. 

This paragraph was deleted.{AI95-00345-01} 


#### Examples

Example of a timed entry call: 

```ada
select
   Controller.Request(Medium)(Some_Item);
or
   delay 45.0;
   --  controller too busy, try something else
end select;

```


#### Wording Changes from Ada 83

{AI05-0299-1} This subclause comes before the one for Conditional Entry Calls, so we can define conditional entry calls in terms of timed entry calls. 


#### Incompatibilities With Ada 95

{AI95-00345-01} {AI05-0005-1} A procedure call can be used as the [entry_call_alternative](./AA-9.7#S0277) in a timed or conditional entry call, if the procedure might actually be an entry. Since the fact that something is an entry could be used in resolving these calls in Ada 95, it is possible for timed or conditional entry calls that resolved in Ada 95 to be ambiguous in Ada 2005. That could happen if both an entry and procedure with the same name and profile exist, which should be rare. 


## 9.7.3  Conditional Entry Calls

{AI95-00345-01} {AI12-0439-1} [A [conditional_entry_call](./AA-9.7#S0279) issues an entry call that is then cancelled if it is not selected immediately (or if a requeue-with-abort of the call is not selected immediately). A procedure call may appear rather than an entry call for cases where the procedure can be implemented by an entry.] 

To be honest: In the case of an entry call on a protected object, it is OK if the entry is closed at the start of the corresponding protected action, so long as it opens and the call is selected before the end of that protected action (due to changes in the Count attribute). 


#### Syntax

conditional_entry_call<a id="S0279"></a> ::= 
  select
   [entry_call_alternative](./AA-9.7#S0277)
  else
   [sequence_of_statements](./AA-5.1#S0166)
  end select;


#### Dynamic Semantics

The execution of a [conditional_entry_call](./AA-9.7#S0279) is defined to be equivalent to the execution of a [timed_entry_call](./AA-9.7#S0276) with a [delay_alternative](./AA-9.7#S0274) specifying an immediate expiration time and the same [sequence_of_statements](./AA-5.1#S0166) as given after the reserved word else. 

NOTE 1   {AI12-0440-1} A [conditional_entry_call](./AA-9.7#S0279) can briefly increase the Count attribute of the entry, even if the conditional call is not selected.


#### Examples

Example of a conditional entry call: 

```ada
{AI12-0178-1} procedure Spin(R : in out Resource) is  -- see 9.4
begin
   loop
      select
         R.Seize;
         return;
      else
         null;  --  busy waiting
      end select;
   end loop;
end;

```


#### Wording Changes from Ada 83

{AI05-0299-1} This subclause comes after the one for Timed Entry Calls, so we can define conditional entry calls in terms of timed entry calls. We do that so that an "expiration time" is defined for both, thereby simplifying the definition of what happens on a requeue-with-abort. 


## 9.7.4  Asynchronous Transfer of Control

[An asynchronous [select_statement](./AA-9.7#S0269) provides asynchronous transfer of control upon completion of an entry call or the expiration of a delay.] 


#### Syntax

asynchronous_select<a id="S0280"></a> ::= 
  select
   [triggering_alternative](./AA-9.7#S0281)
  then abort
   [abortable_part](./AA-9.7#S0283)
  end select;

triggering_alternative<a id="S0281"></a> ::= [triggering_statement](./AA-9.7#S0282) [[sequence_of_statements](./AA-5.1#S0166)]

{AI95-00345-01} triggering_statement<a id="S0282"></a> ::= [procedure_or_entry_call](./AA-9.7#S0278) | [delay_statement](./AA-9.6#S0266)

abortable_part<a id="S0283"></a> ::= [sequence_of_statements](./AA-5.1#S0166)


#### Dynamic Semantics

{AI95-00345-01} For the execution of an [asynchronous_select](./AA-9.7#S0280) whose [triggering_statement](./AA-9.7#S0282) is a [procedure_or_entry_call](./AA-9.7#S0278), the entry_[name](./AA-4.1#S0091), procedure_[name](./AA-4.1#S0091), or procedure_[prefix](./AA-4.1#S0093), and actual parameters are evaluated as for a simple entry call (see 9.5.3) or procedure call (see 6.4). If the call is an entry call or a call on a procedure implemented by an entry, the entry call is issued. If the entry call is queued (or requeued-with-abort), then the [abortable_part](./AA-9.7#S0283) is executed. [If the entry call is selected immediately, and never requeued-with-abort, then the [abortable_part](./AA-9.7#S0283) is never started.] If the call is on a procedure that is not implemented by an entry, the call proceeds as described in 6.4, followed by the [sequence_of_statements](./AA-5.1#S0166) of the [triggering_alternative](./AA-9.7#S0281)[; the [abortable_part](./AA-9.7#S0283) is never started].

For the execution of an [asynchronous_select](./AA-9.7#S0280) whose [triggering_statement](./AA-9.7#S0282) is a [delay_statement](./AA-9.6#S0266), the delay_[expression](./AA-4.4#S0132) is evaluated and the expiration time is determined, as for a normal [delay_statement](./AA-9.6#S0266). If the expiration time has not already passed, the [abortable_part](./AA-9.7#S0283) is executed.

If the [abortable_part](./AA-9.7#S0283) completes and is left prior to completion of the [triggering_statement](./AA-9.7#S0282), an attempt to cancel the [triggering_statement](./AA-9.7#S0282) is made. If the attempt to cancel succeeds (see 9.5.3 and 9.6), the [asynchronous_select](./AA-9.7#S0280) is complete.

If the [triggering_statement](./AA-9.7#S0282) completes other than due to cancellation, the [abortable_part](./AA-9.7#S0283) is aborted (if started but not yet completed - see 9.8). If the [triggering_statement](./AA-9.7#S0282) completes normally, the optional [sequence_of_statements](./AA-5.1#S0166) of the [triggering_alternative](./AA-9.7#S0281) is executed after the [abortable_part](./AA-9.7#S0283) is left. 

Discussion: We currently don't specify when the by-copy [in] out parameters are assigned back into the actuals. We considered requiring that to happen after the [abortable_part](./AA-9.7#S0283) is left. However, that doesn't seem useful enough to justify possibly overspecifying the implementation approach, since some of the parameters are passed by reference anyway.

In an earlier description, we required that the [sequence_of_statements](./AA-5.1#S0166) of the [triggering_alternative](./AA-9.7#S0281) execute after aborting the [abortable_part](./AA-9.7#S0283), but before waiting for it to complete and finalize, to provide more rapid response to the triggering event in case the finalization was unbounded. However, various reviewers felt that this created unnecessary complexity in the description, and a potential for undesirable concurrency (and nondeterminism) within a single task. We have now reverted to simpler, more deterministic semantics, but anticipate that further discussion of this issue might be appropriate during subsequent reviews. One possibility is to leave this area implementation defined, so as to encourage experimentation. The user would then have to assume the worst about what kinds of actions are appropriate for the [sequence_of_statements](./AA-5.1#S0166) of the [triggering_alternative](./AA-9.7#S0281) to achieve portability. 


#### Examples

Example of a main command loop for a command interpreter: 

```ada
loop
   select
      Terminal.Wait_For_Interrupt;
      Put_Line("Interrupted");
   then abort
      -- This will be abandoned upon terminal interrupt
      Put_Line("-&gt ");
      Get_Line(Command, Last);
      Process_Command(Command(1..Last));
   end select;
end loop;

```

Example of a time-limited calculation: 

```ada
{AI12-0442-1} select
   delay 5.0;
   Put_Line("Calculation does not converge");
then abort
   -- This calculation is expected to finish in 5.0 seconds;
   --  if not, it is assumed to diverge.
   Horribly_Complicated_Recursive_Function(X, Y);
end select;

```

{AI12-0098-1} {AI12-0418-1} Note that these examples presume that there are abort completion points (see 9.8) within the execution of the [abortable_part](./AA-9.7#S0283). 


#### Extensions to Ada 83

[Asynchronous_select](./AA-9.7#S0280) is new. 


#### Extensions to Ada 95

{AI95-00345-01} A procedure can be used as the [triggering_statement](./AA-9.7#S0282) of an [asynchronous_select](./AA-9.7#S0280), if the procedure might actually be an entry. 

