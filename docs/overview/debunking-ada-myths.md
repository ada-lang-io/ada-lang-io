Debunking Ada Myths
===================

These are written in the truthful form, rather than stating the myth.

:::caution

The information here is for educational purposes only and not for the
purpose of providing legal advice. You should consult with a qualified
and licensed legal professional prior to making any legal decisions.

:::

Ada is not a dead language
--------------------------

Ada versions appeared as Ada 83, Ada 95, Ada 2005, Ada 2012, and Ada 2022.

Ada now has a build and dependency management tool called 
[Alire](https://alire.ada.dev).  This simplifies building and running your code,
while also providing access to a [growing set of libraries and programs](https://alire.ada.dev/crates.html).
[Alire](https://github.com/alire-project/alire/releases)
also supports installing of toolchains.

Ada can be used for free
------------------------

Ada has a free front-end for GCC called [GNAT](https://gcc.gnu.org/wiki/GNAT).
Just like using GCC to compile C or C++ code, it's free as in freedom (open-source)
and as in beer (free to use).  This version comes from the Free Software Foundation (FSF)
and often referred to as "FSF GNAT".

Yes, there are also commercial compilers which are not free to use.

Ada code does not have to be GPL
--------------------------------

FSF GNAT is licensed with the GCC Runtime exception:

- `Runtime Library Exception http://www.gnu.org/licenses/gpl-faq.html#LibGCCException _
- `FAQ About GCC Runtime Exception http://www.gnu.org/licenses/gcc-exception-3.1-faq.html _

Alire 1.1 supports downloading and installing
the FSF GNAT toolchain for you.

:::caution

   However, there's a version of GNAT released by AdaCore called "GNAT Community
   Edition" which is similar to FSF GNAT but **does not provide the runtime exception.**

:::

No company "owns" Ada
--------------------------

The Ada language is just an ISO-standard.  Though AdaCore initially helped develop
GNAT and continue to contribute back to FSF GNAT, they do not own the Ada
language.

Ada runs on more than embedded systems
--------------------------------------

Ada provides low-level control and interfacing to C to develop programs which
can run on embedded or on consumer computers.  This includes specifying binary
alignment and layout of types (representation clauses), calling compiling
intrinsics, running assembly code, and controlling memory allocation.

You can also do the things you'd expect in a systems programming language, in
addition to having higher level constructs such as built-in containers,
a module system (packages), polymorphism, and concurrency features.
