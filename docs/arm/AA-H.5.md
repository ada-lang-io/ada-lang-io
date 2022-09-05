---
sidebar_position:  187
---

# H.5  Pragma Detect_Blocking

{AI95-00305-01} {AI12-0267-1} The following [pragma](./AA-2.8#S0019) requires an implementation to detect potentially blocking operations during the execution of a protected operation or a parallel construct. 


#### Syntax

{AI95-00305-01} The form of a [pragma](./AA-2.8#S0019) Detect_Blocking is as follows: 

  pragma Detect_Blocking; 


#### Post-Compilation Rules

{AI95-00305-01} A [pragma](./AA-2.8#S0019) Detect_Blocking is a configuration pragma. 


#### Dynamic Semantics

{AI95-00305-01} {AI12-0247-1} {AI12-0267-1} An implementation is required to detect a potentially blocking operation that occurs during the execution of a protected operation or a parallel construct defined within a compilation unit to which the pragma applies, and to raise Program_Error (see 9.5). 


#### Implementation Permissions

{AI95-00305-01} {AI12-0267-1} An implementation is allowed to reject a [compilation_unit](./AA-10.1#S0286) to which a pragma Detect_Blocking applies if a potentially blocking operation is present directly within an [entry_body](./AA-9.5#S0260), the body of a protected subprogram, or a parallel construct occurring within the compilation unit. 

NOTE 1   {AI95-00305-01} {AI12-0442-1} An operation that causes a task to be blocked within a foreign language domain is not defined to be potentially blocking, and is unlikely to be detected. 


#### Extensions to Ada 95

{AI95-00305-01} Pragma Detect_Blocking is new. 


#### Extensions to Ada 2012

{AI12-0267-1} Pragma Detect_Blocking now applies to parallel constructs as well as protected actions. 

