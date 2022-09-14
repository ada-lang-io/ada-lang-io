```yaml
sidebar_position: 1
```

# Why Ada?

## Robust

During the 1970s, the *US Department of Defense* contracted *Bull Information Systems* to design a new programming language to consolidate the over 459 languages in use throughout its infrastructure. The result was Ada, a state-of-the-art programming language with robust features used across a wide range of industries, including aviation, transportation, medical, energy, space, and defense. 

While primarily a procedural language, it’s a highly expressive language combining elements of functional, object-oriented, and contract programming paradigms. Many elements of its unique features are just beginning to appear in other popular languages. 

## Reliable

Ada was designed for maximum reliability. It offers a strong statically typed system with the unique ability to specify constraints, such as numeric ranges on values. Since it is impossible to detect all errors at compile time, Ada also performs optional runtime checks. It also supports design by contract for validating data before, during, and after the execution of a function or procedure.

## Readable and Maintainable

As a trait from its Algol heritage, Ada is a readable language. It favors self-explanatory words as keywords over symbols. It does, however, manage to strike a balance between the two. Common and well-understood everyday symbols, borrowed from basic arithmetic and punctuation since they're shorter and just as intelligible. The result is a language so intuitive, that even non-programmers can read and follow programs written in Ada.

## Fast and compact

It is often mistakenly presumed Ada code is less performant due to its high-level facilities and its extensive emphasis on safety. A key requirement during the design and development of Ada included support for embedded devices. Unlike typical computers, embedded devices run on much slower microcontrollers with tiny memory and storage footprints. Ada excels at supporting such devices without trading off any of its strengths in exchange for higher performance. For decades, Ada has been the go-to industry language when performance and reliability are critical.

| Ada Construct | Near Equivalent |
| --- | --- |
| Forced namespacing | ("packages"). |
| Function overloading. |     |
| Sum types ("variants", "discriminants"). |     |
| Static polymorphism | (monomorphism, "generics"); |
| Dynamic polymorphism | (dynamic dispatch, virtual functions). |
| Compiler and runtime checked constraints on ranges of numerical types | ("ranges", "constraints"). |
| User-specified runtime type invariant checking on assignment and usage as parameters | ("Type_Invariant", "Static_Predicate", "Dynamic_Predicate"). |
| Semantic types, saying two things are the same backing type, but not the same type of "thing", think of "Miles" vs "Kilometers" both stored as floats, which cannot be assigned to each other without a cast | ("derived types"). |
| Deterministic construction and destruction of objects | (RAII, a term from C++, "controlled types") |
| Design-by-contract | ("precondition" and "postcondition" "aspects"). |
| Lifetime checks | ("accessibility" of "access types", very similar to, but not as extensive as Rust). |
| Task definition with defined synchronization and queueing strategies. | ("rendezvous", "entry", "select", "accept", "abort", "Ravenscar") |
| Concurrency types | ("protected", which provides mutual exclusion, and "task"). |
| Exceptions. | Deterministic and configurable static initialization order |
| ("preelaborate","elaborate", "elaborate_body", "elaborate_all") |     |
| ML-style signatures | ("packages", "generics") |
| Formal verification, by enabling `SPARK_Mode` for parts of the program anwriting in SPARK, a language which is an Ada subset. | Think of this along the lines of using `extern "C"`, except for "provable" parts of your code base. |

Ada is missing:

- A preprocessor (GNAT has one, but it's not standard).
- A (sanitary) macro system.
- Reflection.
- A concept of "move".
- Variadic functions. (coming in Ada 202x)
- Variadic templates.
- Async/Await (it has tasks/entries instead)
- Mixed-mode arithmetic and the related implicit numerical casts.
- An equivalent of template parameter pack