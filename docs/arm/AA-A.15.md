---
sidebar_position:  132
---

# A.15  The Package Command_Line

The package Command_Line allows a program to obtain the values of its arguments and to set the exit status code to be returned on normal termination. 

Implementation defined: The meaning of Argument_Count, Argument, and Command_Name for package Command_Line. The bounds of type Command_Line.Exit_Status.


#### Static Semantics

The library package Ada.Command_Line has the following declaration: 

```ada
{AI12-0241-1} package Ada.Command_Line
  with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
  function Argument_Count return Natural;

```

```ada
  function Argument (Number : in Positive) return String;

```

```ada
  function Command_Name return String;

```

```ada
  type Exit_Status is implementation-defined integer type;

```

```ada
  Success : constant Exit_Status;
  Failure : constant Exit_Status;

```

```ada
  procedure Set_Exit_Status (Code : in Exit_Status);

```

```ada
private
  ... -- not specified by the language
end Ada.Command_Line;


```

```ada
function Argument_Count return Natural;

```

{AI05-0264-1} If the external execution environment supports passing arguments to a program, then Argument_Count returns the number of arguments passed to the program invoking the function. Otherwise, it returns 0. The meaning of "number of arguments" is implementation defined.

```ada
function Argument (Number : in Positive) return String;

```

{AI12-0259-1} If the external execution environment supports passing arguments to a program, then Argument returns an implementation-defined value with lower bound 1 corresponding to the argument at relative position Number. If Number is outside the range 1..Argument_Count, then Constraint_Error is propagated. 

Ramification: If the external execution environment does not support passing arguments to a program, then Argument(N) for any N will raise Constraint_Error, since Argument_Count is 0.

```ada
function Command_Name return String;

```

{AI05-0264-1} {AI12-0259-1} If the external execution environment supports passing arguments to a program, then Command_Name returns an implementation-defined value with lower bound 1 corresponding to the name of the command invoking the program; otherwise, Command_Name returns the null string.

```ada
type Exit_Status is implementation-defined integer type;

```

The type Exit_Status represents the range of exit status values supported by the external execution environment. The constants Success and Failure correspond to success and failure, respectively.

```ada
procedure Set_Exit_Status (Code : in Exit_Status);

```

If the external execution environment supports returning an exit status from a program, then Set_Exit_Status sets Code as the status. Normal termination of a program returns as the exit status the value most recently set by Set_Exit_Status, or, if no such value has been set, then the value Success. If a program terminates abnormally, the status set by Set_Exit_Status is ignored, and an implementation-defined exit status value is set.

If the external execution environment does not support returning an exit value from a program, then Set_Exit_Status does nothing. 


#### Implementation Permissions

An alternative declaration is allowed for package Command_Line if different functionality is appropriate for the external execution environment. 

NOTE   Argument_Count, Argument, and Command_Name correspond to the C language's argc, argv[n] (for n&gt0) and argv[0], respectively. 

To be honest: The correspondence of Argument_Count to argc is not direct - argc would be one more than Argument_Count, since the argc count includes the command name, whereas Argument_Count does not. 


#### Extensions to Ada 83

{AI05-0299-1} This subclause is new in Ada 95. 


#### Wording Changes from Ada 2012

{AI12-0259-1} Correction: Defined the lower bound of functions Argument and Command_Name. This could be inconsistent if someone depended on the lower bound of these routines (and it wasn't 1), but such code was never portable (even to later versions of the same implementation). Thus we don't document it as an inconsistency. 


## A.15.1  The Packages Wide_Command_Line and Wide_Wide_Command_Line

{AI12-0021-1} The packages Wide_Command_Line and Wide_Wide_Command_Line allow a program to obtain the values of its arguments and to set the exit status code to be returned on normal termination. 


#### Static Semantics

{AI12-0021-1} The specification of package Wide_Command_Line is the same as for Command_Line, except that each occurrence of String is replaced by Wide_String.

{AI12-0021-1} The specification of package Wide_Wide_Command_Line is the same as for Command_Line, except that each occurrence of String is replaced by Wide_Wide_String.


#### Extensions to Ada 2012

{AI12-0021-1} These packages are new. 

