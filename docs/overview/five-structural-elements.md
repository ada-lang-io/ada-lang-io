---
sidebar_position: 1
---

The Big Five Structural Elements
==============================================================================

Ada provides five major elements for program organization.

Unline many languages, general types are not used as namespaces for writing
functions or procedures.  While type are important in Ada, they play a
different role, and are not used as a way to structure program text.  Instead
related types and their operations go into a package. 

- packages
    - group entities
    - unit of compilation
- subprogram
    - reusable sequences of instructions (algorithms)
- generics
    - Allow writing a package or subprogram across arbitrary types, packages
      and subprograms which meet given specifications.
- tasks
    - define operations done in parallel
- protected objects
    - coordinate access to shared data behind possibly complex guard conditions


|             |   **Linear**   |   **Concurrent**  |
|-------------|----------------|-------------------|
| **Passive** |    Packages    | Protected Objects |
| **Active**  |   Subprograms  |          Tasks    |
                     
Packages and protected objects are passive, whereas tasks and subprograms are
active program behavior.

Packages provide separation into compilation units and act as
container for all entitites.  Subprograms, whether procedures functions provide
reusable algorithms and behaviors.

It is sometimes useful to write a package around unknown types and subprograms
to be specified later.  Generics provide this mechanism for both packages and subprograms.  
Generics do not provide structure on their own, but expand the capabilities
of packages and subprograms to apply behavior to arbitrarily defined types,
subprograms and even packages.

Two structures assist in and provide concurrency.  Tasks do concurrent computation,
selection of operations from a blocked state, and conditional and timed waits.
Coordination of shared resources is given by protected objects.
Both one-off and instantiable versions of tasks and protected types can be created.  These
types also following scoping rules and hence have access to elements where they
were declared.