---
sidebar_position: 1
---

# Projects to Work on

In this section, some projects, issues, improvements or ideas are presented for
the larger Ada community to work together and make Ada better. However, if you
are a newcomer, this section is also for you! If you are looking for practical
topics to train your skill and help the community, this is a great starting
point.

## How is this list structured?

The proposals are ordered by topic. Within each topic, they are ordered in terms
of difficulty, that way, regardless of your experience with Ada or with the
topic at hand, there should always be something for everybody.

Notice that the selection of projects here is not exhaustive and will never
be. The criteria for adding new points to this list is that the projects or
topics should have a noticeable impact on the community. Individual ideas or
personal projects will not be promoted here, but feel free to work on those too!

# List of projects

## Ada Advocacy

Ada does not have a lot going in the marketing department, let's fix that! All
these proposals apply regardless of your Ada experience, each person will have
their own take and point of view.

- **Helping newcomers:** Ada has a constant influx of new and curious people
  wanting to try the language and learn it. One of the most important parts of
  the learning experience is the community and how it welcomes and aids
  _newbies._ The Ada community could use a few more people willing to help and
  guide new programmers with their first few steps into the language. You will
  find links to various sites where learners frequently ask questions in the
  **Community** column of the ada-lang.io footer.
- **Blog posts:** writing blogs is always a nice way to share knowledge,
  experience, tips and anything in between. The Ada community could have a
  little more of that. And don't forget to share the blog posts with the
  community afterwards!
- **Forums, social media and language discussions:** there are a lot of general
  discussions going on in the Internet, we should make Ada have a bit more
  presence there (if it is suitable!).
  - **Example:** the [Awesome WASM Lang
    repository](https://github.com/appcypher/awesome-wasm-langs) lists all the
    languages that support targetting WASM. Ada supports WASM! ~~but it is not
    on the list. It would be great if someone did a Pull Request.~~ Thanks to
    [mgrojo](https://github.com/mgrojo) for his [pull
    request](https://github.com/appcypher/awesome-wasm-langs/pull/135). It
    didn't even require writing a single line of Ada code, it is that simple!
- **Conferences:** if you feel confident with your speaking and teaching skill,
  going to a conference and showing off Ada is a great way to attract the
  curious eyes of the audience. The topics here are also quite broad, ranging
  from simple and short tutorial all the way to niche and advanced features of
  the language.

## Documentation

Documentation can easily make or break projects, luckily, Ada is one of the best
documented languages out there, thanks to it being an ISO standard and having
[extensive compiler and tooling
documentation](https://docs.adacore.com/live/wave/gnat_rm/html/gnat_rm/gnat_rm.html).
Nonetheless, things could be much better, specially for individual projects and
libraries. Documentation for individual tools will be listed in their respective
categories. Here are the main places which could use help with their
documentation:

- **Ada-Lang.io:** you can help grow the documentation, guides, experience of
  this webpage, which tends to be one of the first resources that new people to
  the language take. The [Learn section](./learn/why-ada/) could use some extra
  hands. The addition of SPARK
  documentation would also be greatly welcomed.
- **Ada Wikibook:** the [Ada
  Wikibook](https://en.wikibooks.org/wiki/Ada_Programming) has been a great
  source of knowledge for many years and many people. Nonetheless, it could use
  some clean-ups, completion and updates related to _Ada 2022._
- **SPARK:** SPARK does not have as much documentation as Ada does. This is just
  natural as it is a newer system and it is also a much more complex and
  advanced one. Therefore, SPARK could use some extra examples, guides and help
  from the community.
- **Improve the Standard Library documentation.** Ada has an advanced Standard
  Library that is carefully documented in the Ada standard and in the [GNAT
  Reference
  Manual](https://docs.adacore.com/live/wave/gnat_rm/html/gnat_rm/gnat_rm.html). However,
  there are not many examples regarding its use. It is also not easy to
  read/digest documents describing the Standard Library. Better documentation
  would be useful for the larger Ada community and newcomers. Writing examples
  and documentation for the Standatd Library would also be very useful for new
  Ada programmers, as it makes for a wonderful practical learning experience.
- **Translations:** the Ada community is all over the world and there are many
  people who are not fluent in English. If you speak a different language and
  you feel like translating content to it would help other people, go ahead!
  Luckily, nowadays, most people are somewhat fluent in English to be able to
  read it and understand it, so translation work is not as necessary as it used
  to be.

## Teaching

While Ada is a very readable and easy to learn language, its community does not
shine when it comes to diverse and omnipresent documentation. If you would like
to help fix this, here are a few ideas.

For beginners and new Ada programmers:

- **[Rosetta Code](https://rosettacode.org/wiki/Rosetta_Code):** Rosetta
  provides examples and solutions to common problems and tasks in many
  programming languages. It serves as a learning resource as well as a
  comparison tool between languages. Ada already has a lot of examples, but
  still, some are missing compared to other languages. And some of the examples
  are quite old and they could be updated and cleaned up. These tasks should be
  fairly easy to carry out and they will help you learn Ada along the way!
- **[GeeksforGeeks](https://www.geeksforgeeks.org/)** and **[Tutorials
  Point](https://www.tutorialspoint.com/):** these well-known websites have
  resources on different languages and topics about them, ranging between simple
  syntax elements to solving specific tasks in them. Ada has little to no
  presence in them. It would be nice if we could document Ada in them or point
  people to use [Learn Ada](https://learn.adacore.com/) for more information.

For intermediate and seasoned Ada and SPARK users:

- **[Alice](https://github.com/alice-adventures/Alice)** is a work-in-progress
  platform that is focused on teaching Ada and SPARK by focusing on high quality
  lessons and solutions (excellence as it is indicated in Alice's
  documentation). One could help develop its infrastructure, testing and adding
  lessons and tasks to it.
- **[Exercism](https://exercism.org/)** is a learning platform where students
  solve problems (with varying degrees of complexity) and then a tutor or
  reviewer can give them feedback if necessary. Ada used to have an entry in the
  website [see deprecated
  repo](https://github.com/exercism/DEPRECATED-ada). However, due to the lack of
  exercises and people willing to carry out the work maintaining it and helping
  students, it is no longer available. It would be great if it could be brought
  back. Nonetheless, Alice, see above, is trying to achive a similar goal while
  being focused on Ada/SPARK.

## Toolchains

- **[Alire](https://alire.ada.dev/)** is one of the most important tools for the
  Ada community, regardless of the experience that one may have with the
  language. Alire is always looking for testers for new releases, feedback,
  improvements and fixes. If you are looking for a project to help with, Alire
  would be great!
  - **Add new crates:** one of the easiest ways to help Alire is by adding Ada
    applications and libraries (called crates) to the index and making them
    available for the entire community.
  - **Help with the documentation.**
  - **Help build Alire in new architectures (for example RISC-V).**
  - **Improve the [compiler
    packaging](https://github.com/alire-project/GNAT-FSF-builds)** of Alire by
    adding a new architecture and testing it more.
  - **Package [GNAT-LLVM](https://github.com/AdaCore/gnat-llvm/).**
- **Operating System's Packages:** Ada is not widely packaged in a lot of
  distributions' default package manager. A lot of people initially try to use
  the system's provided package only to find out that Ada is not packaged or
  poorly packaged. It would be very benefitial to a lot of new (and seasoned)
  Ada users to have a nice out-of-the-box experience with their operating
  system.
  - **Guix:** help package a working toolchain for the supported
    architectures. Package Alire.
  - **OpenSUSE:** extend the `gcc-ada` packaging to all the supported
    architectures. Create cross-compilation toolchains. Package Alire.
  - **RedHat and Fedora:** improve the support of the Ada toolchain. Package
    Alire.
  - **FreeBSD:** improve the current port and extend support to other
    architectures. Update the packaged version.
  - **NetBSD:** update the compiler and patches. Check support for NetBSD 10.
- **[Ravenports](http://www.ravenports.com/)** is a package manager similar to
  [pkgsrc](https://www.pkgsrc.org/). Help test it, package programs in it and
  help grow its OS support.

## Libraries and Tools

### Bindings

- **Create bindings to well-known tools and libraries:** not a lot of tools are
  implemented in Ada and not all need to be! But it is always more convenient if
  Ada users could just start using a wider variety of tools and libraries. For
  that reason, it would be greatly benefitial to have more Ada bindings to
  well-known and widely-used projects so that both, new and seasoned Ada
  programers, can easily pick them up and start using their functionality.
  - **Ideas:**
    - **[ImGUI](https://github.com/ocornut/imgui)** is a widely used GUI
      library. There is already an
      [Ada-binding](https://github.com/michael-hardeman/ImGui-Ada), but it could
      be updated and added to Alire.
    - **[stb](https://github.com/nothings/stb)** is a collection of C
      header-only utilities in the public domain.
    - **[raylib](https://github.com/raysan5/raylib)** is a collection of simple
      commonly-used utilities and libraries geared towards creating videogames.
    - **[ccv](https://github.com/liuliu/ccv)** and
      **[OpenCV](https://opencv.org/)** are libraries geared towards computer
      vision and processing.
    - **And many more!**
- **[SWIG4Ada](https://github.com/charlie5/swig4ada):** Ada has outstanding
  support for creating bindings to other languages, specially C, Fortran and
  Cobol. GCC also has a method for creating bindings to C++ projects, however,
  it is quite limited with what it can do. SWIG4Ada implements support for Ada
  into the SWIG binding generator. It is still incomplete and it could use some
  more hands.

### Graphics and Games

- **[SDLAda](https://github.com/ada-game-framework/sdlada):** the bindings to SDL still
  have some missing pieces/functions that need to be finalised, which can be
  found in the link's README page. This could be a great start for people
  wanting to get into game/graphics programming with Ada.
- **[GTKAda](https://github.com/AdaCore/gtkada)** is the GTK binding to Ada. It
  is poorly documented, there are not many tutorials nor blogposts about using
  it. Therefore, documentation would be a great starting point for someone
  wanting to get into GTK and Ada. Moreover, the bindings still target GTK3. If
  you are feeling adventurous, you may want to try and help update it to GTK4!

### Web development

- **[AWS (Ada Web Services)](https://github.com/AdaCore/aws)** is the standard
  web framework for Ada. It has some rough edges regarding TLS certificates,
  supporting newer web standards and documentation. All of these points could
  make use of a few willing hands.
- **[AdaWebpack (Ada-WASM)](https://github.com/godunko/adawebpack/):** WASM is
  the new cool kid in the web world and not without reason. It allows to run
  traditionally compiled languages in the browser, and Ada is no
  exception. AdaWebpack is a project that supports compiling Ada to WASM. Its
  support could be improved and more apps could be created with it. [Here is a
  cool example](https://github.com/reznikmm/android_app_demo) of a spinning cube
  (running on WebGL) and the Ada app compiled to WASM in Android!
- **[AWA (Ada Web Applications)](https://github.com/stcarrez/ada-awa)** is a
  collection of tools in order to build Web applications using Ada. Help by
  using it, testing and documenting it!

### Cryptography and safety

- **[WolfSSL](https://www.wolfssl.com/)** is a complete and certifiable, among
  others, TLS library. It recently [added Ada binding
  support](https://github.com/wolfSSL/wolfssl/tree/master/wrapper/Ada/). It
  would be nice for the community to test it, improve it if necessary,
  potentially create SPARK code around it and package it into more
  places. Promoting it would also be a benefit.
- **[SPARKNaCl](https://github.com/rod-chapman/SPARKNaCl/)** is a cryptographic
  library fully proven in SPARK with no runtime nor external dependencies. It
  would be nice seeing it receive a bit more marketing in non-Ada environments
  and projects implemented with it!

### Miscellaneous

- **[Ressource-Embedder](https://gitlab.com/stcarrez/resource-embedder)** is a
  resource embedder for Ada, Go and C programs. It allows the user to load data
  as part of the executable! You can help by utilising and promoting it.

## Operating Systems

The Ada ecosystem sports some of the most interesting kernels/OSes out
there. Operating System development tends to be labeled as an advanced
topic. However, if you are learning, thanks to Ada's clear syntax, it becomes
much easier to understand them, use them and even contribute to an OS. Here is a
list of kernels/OSes written in pure Ada/SPARK that may greatly benefit from the
community:

- **[Ironclad](https://ironclad.cx/)** is a fairly new kernel that strives to be
  POSIX compatible. It partially written in SPARK, making it much more robust
  and safe by default. It aims to support x86, ARM and RISC-V architectures. You
  can contribute to it or you can help grow the
  [Gloire](https://github.com/streaksu/Gloire) OS distribution, which uses
  Ironclad as its kernel.
- **[HiRTOS](https://github.com/jgrivera67/HiRTOS)** is a high integrity RTOS
  kernel as well as a separation kernel written in SPARK. Go and give it a try!
  One can even install it using Alire! It currently targets the ARM Cortex-R52
  CPU. One could try and port it to a different CPU.
- **[Muen](https://muen.codelabs.ch/)** is a separation kernel fully written in
  SPARK. It has been certified by some large military bodies for their internal
  use. It also sports some drivers fully written in SPARK!
- **[M2OS](https://m2os.unican.es/)** is a small RTOS that was created from the
  same people behind the Ada OS [Marte](https://marte.unican.es/).

There are some well known Operating Systems that allow users to run Ada programs
in them directly even if they are written in a different language. You may want
to use them, improve the Ada support they have or write examples or blog posts
about their use:

- **[RTEMS](https://www.rtems.org)** is a POSIX RTOS that is widely used in
  the aerospace industry. It has official Ada support. However, its use is not
  clearly documented nor widely known about; this could be fixed.

## Embedded Programming

- **[Ada Drivers Library](https://github.com/AdaCore/Ada_Drivers_Library)** is a
  collection of drivers for microcontollers and embedded boards. You can help by
  utilising it, testing the built-in drivers and specially by helping improve
  drivers and boards that are not yet present.

- **Create more HALs:** HAL (Hardware Abstraction Layer) is a piece of code that
  abstracts the device away from the code. This is what generally drivers do. By
  implementing a HAL the low level aspects of the hardware are not needed in
  order to program the device, which makes the experience of embedded
  programming a lot more pleasant and comprehensible. Ada has quite a few HALs
  (see the Ada Drivers Library), but we are nowhere near to other communities,
  even though we exceed at embedded programming.

- **Improve [svd2ada](https://github.com/AdaCore/svd2ada) and
  [startup-gen](https://github.com/AdaCore/startup-gen).** These two tools allow
  for the quick creation of a initial HAL layer. There are a few issues and
  limitations in their Github page which could be improved.

- **Help the [SweetAda](https://github.com/gabriele-galeotti/SweetAda)
  development framework.** SweetAda is a terrific framework designed to bring
  Ada support to as many architectures as possible. It would greatly benefit
  from more testing and specially documentation.

- **Improve support for tasking profiles in embedded OSes (Jorvik, Ravenscar).**
  This is a very advanced topic, but an important one. Embedded systems
  generally do not support tasking due to the limited support that the runtime
  implementation has for them. Therefore, specialised profiles that allow for
  some basic, real-time and safe tasking abilities need to be created. It would
  be great to see more CPUs/Boards with support for the Jorvik and Ravenscar
  profiles.

- **[AVR-Ada](https://github.com/RREE/AVRAda_Doc)** is an embedded development
  library that brings support for a large number of AVR devices to the Ada
  programming language. You can help by creating more examples, promoting it and
  expanding its capabilities.

## Performance and benchmarks

Ada tends to be shown in a lot of benchmarks, and that should be no surprise, as
it is a highly optimizable, compiled programming language that does not use
garbage collection and leverages some of the best compiler infrastructure in the
world (GCC and LLVM). However, we do not tend to rank at the top. "Why is that?"
one may ask... Well... Most Ada code used for benchmarking purposes has not been
receiving the same amount of effort and care that other programming languages
have. Let's fix this!

The following proposals are meant to be undertaken by people familiar with Ada
and HPC (High Performance Computing):

- **[The Computer Language Benchmarks
  Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html):**
  Ada programs could be optimised to utilise newer language constructs,
  algorithmic design and, once available and if useful, the new Ada 2022
  `parallel` keyword. This should yield better results than the current, already
  performant programs. I believe we can improve our times quite a bit!
- **[Programming Language and Compiler
  Benchmarks](https://programming-language-benchmarks.vercel.app/):** Ada is not
  even listed in this competition! The task would require providing performant
  Ada examples for this language benchmark comparison.

## Compilers

The proposals here are quite advanced, therefore, they are not recommended for
people unfamiliar with the Ada language.

- **Mainlining more patches to upstream GNAT:** GNAT/GCC-Ada is a wonderful
  piece of technology, and thanks to it being based on the GCC compiler
  infrastructure, it can target a large number of Operating Systems and
  architectures. However, GNAT/GCC-Ada requires a bit of setup and additional
  files that are needed in order to compile Ada code for these targets. The most
  common architecture/OS pairs are already present inside GCC, but some
  communities maintain their own set of patches in order to support GNAT. It
  would be ideal if those patches could become part of the GCC source tree. Here
  are some resources and patches that would need mainlining:
  - **[Android](https://source.android.com/):** in the past there were some
    versions of GNAT that had support for Android and there is still Android
    code in the GCC/GNAT source code. However, it has not been tested nor used
    in a long while and it seems to have fallen in disrepair. Some patches for
    Android support can be found in [this
    repository](https://github.com/search?q=repo%3Ajrmarino%2Fdraco%20path%3A%2F%5Emisc%5C%2Fflux13%5C%2F%2F%20android&type=code). Nonetheless,
    Ada also supports Android with WASM, [see this
    demo](https://blog.adacore.com/android-application-with-ada-and-webassembly).
  - **[OpenBSD](https://www.openbsd.org/)** has supported GNAT for a few
    architectures for a long while, however, the patches to add support have
    never been mainlined. The patches can be found in their ports tree or in
    [this Github
    mirror](https://github.com/openbsd/ports/tree/master/lang/gcc/11/patches)
  - **[NetBSD](http://netbsd.org/):** updated GCC patches to add support for
    NetBSD (amd64) can be found
    [here](https://github.com/jrmarino/draco/tree/master/misc/flux13). Adding
    support for other architectures can somewhat easily be done. Thanks to
    J. Marino for his work.
  - **[FreeBSD](https://www.freebsd.org/)** support within GNAT could
    use quite a bit of help. This is [the current patch used by
    FreeBSD](https://gitlab.com/FreeBSD/freebsd-ports/-/tree/main/lang/gnat12/files).
- **Adding support for more OSes:** support could be added to the following
  "full-fledged" Operating Systems:
  - **[Ironclad](https://ironclad.cx/)** is a POSIX compatible RTOS completely
    written in Ada. It currently has no RTS (RunTime System) support for
    compiling Ada programs targetting it.
  - **[Illumos](https://www.illumos.org/)** is the continuation of OpenSolaris
    as an open source project. It currently has no Ada support within GNAT.
  - **[Gnode](https://genode.org/)** is an OS framework that is based on
    components and separation of concerns and security.
  - **[Haiku](https://www.haiku-os.org/)** is a reemplementation of BeOS.
  - **Microkernel and RTOS Systems:** there are quite a few other OSes that we
    could support. They are either widely used or best in class for their target
    audience.
    - **[Zephyr](https://www.zephyrproject.org/)** is an RTOS that has been
      gaining a lot of traction and support. Initial support for the Zephyr RTOS
      was started [here](https://github.com/ila-embsys/zephyr-ada-gnat-rts).
    - **[se4L](https://sel4.systems/)** is a secure, verified RTOS microkernel
      for x86, ARM and RISC-V devices.
    - **[FreeRTOS](https://freertos.org/)** is a widely used RTOS for
      microcontrollers. There are [some Ada
      ports](https://github.com/simonjwright/cortex-gnat-rts) that have support
      for it. However, mainline support would be preferred.
    - **[Azure ThreadX](https://github.com/eclipse-threadx/threadx)** is a
      certified RTOS for embedded applications.
    - **[Apache Nuttx](https://nuttx.apache.org/)** is a RTOS for embedded
      devices.
    - **[Minix 3](https://www.minix3.org/)** is one of the most well-known
      microkernels. It has enough POSIX support that is able to run the NetBSD
      userland on top of it.
    - **[RT-Thread](https://www.rt-thread.io/)** is a new RTOS for embedded
      devices.
    - **[RIOT](https://www.osrtos.com/rtos/riot/)** is an RTOS for small
      embedded systems.
  - **Unikernels:** these kernels have been created to run a single application
    and be the most secure possible as they have the smallest surface. The Ada
    community should ensure that Ada applications can run on these kernels.
    - **[Mirage OS](https://mirage.io/)** is a well-known unikernel written in
      OCaml. See also **[Solo5](https://github.com/Solo5/solo5).** For example,
      the [Muen](https://muen.codelabs.ch/) website runs on it.
    - **[Unikraft](https://unikraft.org/)** is a unikernel focused on the cloud
      and security.
    - **[Toro](https://torokernel.io/)** is a unikernel written for
      microservices.
- **[GNAT-LLVM](https://github.com/AdaCore/gnat-llvm/):** the GNAT-LLVM project
  is fairly new but very promising. It would be nice to test it, help with the
  documentation and create cool demos, see [this Ada-WASM
  example](https://github.com/godunko/adawebpack).
- **Bootstrapping GNAT:** GNAT currently requires a previous version to compile
  itself. Sadly, there was never an original public version of GNAT that did not
  have such requirement. This means, that GNAT cannot be compiled without
  already having a working copy of a GNAT installation. The [GNAT Bootstrapping
  project](https://fossil.irvise.xyz/gnat-bootstrap/home) aims to create a
  small, minimal compiler together with the [live-bootstrap
  project](https://github.com/fosslinux/live-bootstrap/) in order to build an
  initial GNAT compiler.
  - **Requirements:** Scheme and compiler programming knowledge.
- **Improve Ada 2022 support in GCC/GNAT:** there are still some Ada 2022
  features that are yet to be implemented in GCC/GNAT, mainly the `parallel`
  construct. You can help develop new features, tests them and improve the
  current ones. For more information about the implementation of `parallel` in
  GCC/GNAT, see [this forum
  thread](https://forum.ada-lang.io/t/lightweight-parallelism-library-based-on-ada-2022-features/516/4)
- **[HAC](https://github.com/zertovitch/hac)** is a small and limited Ada
  compiler, but it could make use of some help for those interested in compilers
  and Ada.
- **A component based Ada compiler** based upon LLVM which provides Ada with
  the same kind of tooling that CLang provides to the C-likes, i.e.
  - Construction of Ada AST's programmatically from other projects.
  - JIT compiled Ada from a REPL or from within other projects.
  - Importing of foreign libraries written in other languages by using the CLang
    libraries for that import, adding a `pragma C[PP]_Header ("path/filename.h[pp]");`
    after a `with` clause could help.
  - The compiler-compilers below could aid in the creation of such tooling.

### Compiler-compilers

These are tools for generating compilers and interpreters quicker than hand
implementing then, which is useful for prototyping.

There are versions of [YACC](https://github.com/Ada-France/ayacc) and
[FLex](https://github.com/Ada-France/aflex) dating back to the 1980's which
haven't been improved too much.

There is also a unicode version of FLex within the
[League](https://alire.ada.dev/crates/matreshka_league) string library.

- There is a port of the [Lemon](https://github.com/Lucretia/Cherry) parser
  generator which is dead, the original tree has been deleted and with it, it's
  dependency which contains a string type, this could be resurrected.

- [ANTLR4](https://www.antlr.org/) has a re-targetable back end based on
  [StringTemplate](https://www.stringtemplate.org/)'s. There was a start of
  an ANTLR v3 back end which was never completed.

This is a particular area where Ada is lacking compared to other languages.
