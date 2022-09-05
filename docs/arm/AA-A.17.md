---
sidebar_position:  134
---

# A.17  The Package Environment_Variables

{AI95-00370-01} The package Environment_Variables allows a program to read or modify environment variables. Environment variables are name-value pairs, where both the name and value are strings. The definition of what constitutes an environment variable, and the meaning of the name and value, are implementation defined. 

Implementation defined: The definition and meaning of an environment variable.


#### Static Semantics

{AI95-00370-01} The library package Environment_Variables has the following declaration: 

```ada
{AI12-0241-1} {AI12-0302-1} package Ada.Environment_Variables
   with Preelaborate, Nonblocking, Global =&gt in out synchronized is

```

```ada
   function Value (Name : in String) return String;

```

```ada
{AI05-0285-1}    function Value (Name : in String; Default : in String) return String;

```

```ada
   function Exists (Name : in String) return Boolean;

```

```ada
   procedure Set (Name : in String; Value : in String);

```

```ada
   procedure Clear (Name : in String);
   procedure Clear;

```

```ada
{AI05-0248-1} {AI12-0286-1}    procedure Iterate
      (Process : not null access procedure (Name, Value : in String))
      with Allows_Exit;

```

```ada
end Ada.Environment_Variables;

```

```ada
function Value (Name : in String) return String;

```

{AI95-00370-01} If the external execution environment supports environment variables, then Value returns the value of the environment variable with the given name. If no environment variable with the given name exists, then Constraint_Error is propagated. If the execution environment does not support environment variables, then Program_Error is propagated.

```ada
function Value (Name : in String; Default : in String) return String;

```

{AI05-0285-1} If the external execution environment supports environment variables and an environment variable with the given name currently exists, then Value returns its value; otherwise, it returns Default.

```ada
function Exists (Name : in String) return Boolean;

```

{AI95-00370-01} {AI05-0264-1} If the external execution environment supports environment variables and an environment variable with the given name currently exists, then Exists returns True; otherwise, it returns False.

```ada
procedure Set (Name : in String; Value : in String);

```

{AI95-00370-01} {AI05-0264-1} If the external execution environment supports environment variables, then Set first clears any existing environment variable with the given name, and then defines a single new environment variable with the given name and value. Otherwise, Program_Error is propagated.

If implementation-defined circumstances prohibit the definition of an environment variable with the given name and value, then Constraint_Error is propagated. 

Implementation defined: The circumstances where an environment variable cannot be defined.

It is implementation defined whether there exist values for which the call Set(Name, Value) has the same effect as Clear (Name). 

Implementation defined: Environment names for which Set has the effect of Clear.

```ada
procedure Clear (Name : in String);

```

{AI95-00370-01} {AI05-0264-1} {AI05-0269-1} If the external execution environment supports environment variables, then Clear deletes all existing environment variables with the given name. Otherwise, Program_Error is propagated.

```ada
procedure Clear;

```

{AI95-00370-01} {AI05-0264-1} If the external execution environment supports environment variables, then Clear deletes all existing environment variables. Otherwise, Program_Error is propagated.

```ada
{AI05-0248-1} {AI12-0286-1} procedure Iterate
   (Process : not null access procedure (Name, Value : in String))
      with Allows_Exit;

```

{AI95-00370-01} {AI05-0264-1} If the external execution environment supports environment variables, then Iterate calls the subprogram designated by Process for each existing environment variable, passing the name and value of that environment variable. Otherwise, Program_Error is propagated.

If several environment variables exist that have the same name, Process is called once for each such variable.


#### Bounded (Run-Time) Errors

{AI95-00370-01} It is a bounded error to call Value if more than one environment variable exists with the given name; the possible outcomes are that: 

one of the values is returned, and that same value is returned in subsequent calls in the absence of changes to the environment; or

Program_Error is propagated. 


#### Erroneous Execution

{AI95-00370-01} Making calls to the procedures Set or Clear concurrently with calls to any subprogram of package Environment_Variables, or to any instantiation of Iterate, results in erroneous execution.

Making calls to the procedures Set or Clear in the actual subprogram corresponding to the Process parameter of Iterate results in erroneous execution. 


#### Documentation Requirements

{AI95-00370-01} An implementation shall document how the operations of this package behave if environment variables are changed by external mechanisms (for instance, calling operating system services). 

Documentation Requirement: The behavior of package Environment_Variables when environment variables are changed by external mechanisms.


#### Implementation Permissions

{AI95-00370-01} An implementation running on a system that does not support environment variables is permitted to define the operations of package Environment_Variables with the semantics corresponding to the case where the external execution environment does support environment variables. In this case, it shall provide a mechanism to initialize a nonempty set of environment variables prior to the execution of a partition. 


#### Implementation Advice

{AI95-00370-01} If the execution environment supports subprocesses, the currently defined environment variables should be used to initialize the environment variables of a subprocess. 

Implementation Advice: If the execution environment supports subprocesses, the current environment variables should be used to initialize the environment variables of a subprocess.

Changes to the environment variables made outside the control of this package should be reflected immediately in the effect of the operations of this package. Changes to the environment variables made using this package should be reflected immediately in the external execution environment. This package should not perform any buffering of the environment variables. 

Implementation Advice: Changes to the environment variables made outside the control of Environment_Variables should be reflected immediately.


#### Extensions to Ada 95

{AI95-00370-01} Package Environment_Variables is new. 


#### Incompatibilities With Ada 2005

{AI05-0285-1} A new overloaded function Value is added to Environment_Variables. If Environment_Variables is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the name Value is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


## A.17.1  The Packages Wide_Environment_Variables and Wide_Wide_Environment_Variables

{AI12-0021-1} The packages Wide_Environment_Variables and Wide_Wide_Environment_Variables allow a program to read or modify environment variables. 


#### Static Semantics

{AI12-0021-1} The specification of package Wide_Environment_Variables is the same as for Environment_Variables, except that each occurrence of String is replaced by Wide_String.

{AI12-0021-1} The specification of package Wide_Wide_Environment_Variables is the same as for Environment_Variables, except that each occurrence of String is replaced by Wide_Wide_String. 


#### Extensions to Ada 2012

{AI12-0021-1} These packages are new. 

