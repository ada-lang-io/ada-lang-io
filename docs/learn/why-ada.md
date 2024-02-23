---
sidebar_position: 1
---

# Why Ada?

## Ada is easy to set up and get started

The [Alire][alire] tool can [install an Ada toolchain][alire-toolchains] and
acts as a build and dependency management tool.
This simplifies compiling and running your code, while also providing access to a
[growing set of libraries and programs][alire-crates].

Use the [plugin for Visual Studio Code][vscode-plugin] or the [language server][language-server] with your favorite editor.

## No company "owns" Ada

The Ada language is just an ISO-standard. Though AdaCore initially helped develop
GNAT and continue to contribute back to FSF GNAT, they do not own the Ada
language.

While there are also commercial compilers, Ada has a front-end for GCC called [GNAT][gnat].
The version from the Free Software Foundation (FSF) is often referred to as "FSF GNAT".

## Ada continues to be developed

Ada has multiple released standards Ada 83, Ada 95, Ada 2005, Ada 2012, and Ada 2022.

## Ada runs on more than embedded systems

Ada provides low-level control and interfacing to C to develop programs which
can run on embedded or on consumer computers. This includes specifying binary
alignment and layout of types (representation clauses), calling compiling
intrinsics, running assembly code, and controlling memory allocation.

You can also do the things you'd expect in a systems programming language, in
addition to having higher level constructs such as built-in containers,
a module system (packages), polymorphism, and concurrency features.

## Feature Overview

Covering every Ada feature would be dilute the conceptual overview of Ada, so
related Ada-specific terminology is quoted in parentheses for those wanting to
do their own targeted research.

Ada supports:

- Forced namespacing ("packages").
- Function overloading.
- Sum types ("variants", "discriminants").
- Static polymorphism (monomorphism, "generics");
- Dynamic polymorphism (dynamic dispatch, virtual functions).
- Compiler and runtime checked constraints on ranges of numerical types ("ranges", "constraints").
- User-specified runtime type invariant checking on assignment and usage as parameters ("Type_Invariant", "Static_Predicate", "Dynamic_Predicate").
- Semantic types, saying two things are the same backing type, but not the same
  type of "thing", think of "Miles" vs "Kilometers" both stored as floats, which
  cannot be assigned to each other without a cast ("derived types").
- Deterministic construction and destruction of objects (RAII, a term from C++, "controlled types")
- Design-by-contract ("precondition" and "postcondition" "aspects").
- Lifetime checks ("accessibility" of "access types", similar to, but not as extensive as Rust).
- Task definition with defined synchronization and queueing strategies.
  ("rendezvous", "entry", "select", "accept", "abort", "Ravenscar")
- Concurrency types ("protected", which provides mutual exclusion, and "task").
- Exceptions.
- Deterministic and configurable static initialization order ("preelaborate",
  "elaborate", "elaborate_body", "elaborate_all")
- ML-style signatures ("packages", "generics")
- Formal verification, by enabling `SPARK_Mode` for parts of the program and
  writing in SPARK, a language which is an Ada subset. Think of this along the
  lines of using `extern "C"`, except for "provable" parts of your code base.

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

[alire]: https://alire.ada.dev
[alire-crates]: https://alire.ada.dev/crates.html
[alire-releases]: https://github.com/alire-project/alire/releases
[alire-toolchains]: https://alire.ada.dev/docs/#toolchain-management
[gnat]: https://gcc.gnu.org/wiki/GNAT
[language-server]: https://github.com/AdaCore/ada_language_server
[vscode-plugin]: https://marketplace.visualstudio.com/items?itemName=AdaCore.ada
