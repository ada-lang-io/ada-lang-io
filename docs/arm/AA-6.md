---
sidebar_position:  48
---

# 6 Subprograms

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
A subprogram is a program unit or intrinsic operation whose execution is invoked by a subprogram call. There are two forms of subprogram: procedures and functions. A procedure call is a [statement](./AA-5.1#S0167); a function call is an expression and returns a value. The definition of a subprogram can be given in two parts: a subprogram declaration defining its interface, and a [subprogram_body](./AA-6.3#S0216) defining its execution. [Operators and enumeration literals are functions.] 

To be honest: A function call is an expression, but more specifically it is a [name](./AA-4.1#S0091). 

Glossary entry: A subprogram is a section of a program that can be executed in various contexts. It is invoked by a subprogram call that may qualify the effect of the subprogram through the passing of parameters. There are two forms of subprograms: functions, which return values, and procedures, which do not.

Glossary entry: A function is a form of subprogram that returns a result and can be called as part of an expression.

Glossary entry: A procedure is a form of subprogram that does not return a result and can only be called by a [statement](./AA-5.1#S0167).

Version=[5],Kind=(AddedNormal),Group=[S],Term=[subprogram], Def=[a unit of a program that can be brought into execution in various contexts, with the invocation being a subprogram call that can parameterize the effect of the subprogram through the passing of operands], Note1=[There are two forms of subprograms: functions, which return values, and procedures, which do not.] Version=[5],Kind=(AddedNormal),Group=[S],Term=[function], Def=[a form of subprogram that returns a result and can be called as part of an expression] Version=[5],Kind=(AddedNormal),Group=[S],Term=[procedure], Def=[a form of subprogram that does not return a result and can only be invoked by a statement]

{AI05-0299-1} A callable entity is a subprogram or entry (see Section 9). A callable entity is invoked by a call; that is, a subprogram call or entry call. A callable construct is a construct that defines the action of a call upon a callable entity: a [subprogram_body](./AA-6.3#S0216), [entry_body](./AA-9.5#S0260), or [accept_statement](./AA-9.5#S0258). 

Ramification: Note that "callable entity" includes predefined operators, enumeration literals, and abstract subprograms. "Call" includes calls of these things. They do not have callable constructs, since they don't have completions. 

