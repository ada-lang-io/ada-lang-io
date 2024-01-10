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

Ada does not have a lot going in the marketing department, lets fix that! All
these proposals apply regardless of your Ada experience, each person will have
their own take and point of view.

- **Helping newcomers:** Ada has a constant influx of new and curious people
  wanting to try the language and learn it. One of the most important parts of
  the learning experience is the community and how it welcomes and aids
  _newbies._ The Ada community could use a few more people willing to help and
  guide new programmers with their first few steps into the language.
- **Blog posts:** writing blogs is always a nice way to share knowledge,
  experience, tips and anything in between. Ada could use a little more of
  that. And don't forget to share the blog posts with the community afterwards!
- **Forums, social media and language discussions:** there are a lot of general
  discussions going on in the internet, we should make Ada have a bit more
  presence there (if it is suitable!).
  - **Example:** the [Awesome WASM Lang
    repository](https://github.com/appcypher/awesome-wasm-langs) lists all the
    languages that support targetting WASM. Ada supports WASM! ~~but it is not
    on the list. It would be great if someone did a Pull Request.~~ Thanks to
    [mgrojo](https://github.com/mgrojo) for his [pull
    request](https://github.com/appcypher/awesome-wasm-langs/pull/135). It did't
    even require writing a single line of Ada code, it is that simple!
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
  hands, specially with the addition completion of [How-Tos](./category/how-tos)
  and [Tips and Tricks](./category/tips-and-tricks). The addition of SPARK
  documentation would also be greatly welcomed.
- **Ada Wikibook:** the [Ada
  Wikibook](https://en.wikibooks.org/wiki/Ada_Programming) has been a great
  source of knowledge for many years and many people. Nonetheless, it could use
  some clean-ups, completion and updates related to _Ada 2022._
- **SPARK:** SPARK does not have as much documentation as Ada does. This is just
  natural as it is a newer system and it is also a much more complex and
  advanced one. Therefore, SPARK could use some extra examples, guides and help
  from the community.
- **Translations:** the Ada community is all over the world and there are many
  people whose are not fluent in english. If you speak a different language and
  you feel like translating content to it would help other people, go ahead!
  Luckily, nowdays, most people are somewhat fluent in english to be able to
  read it and understand it, so translation work is not as necessary as it used
  to be.

## Teaching

While Ada is a very readable and easy to learn language, its community does not
shine when it comes do diverse and ever-present documentation. If you would like
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

- **[Alice](https://github.com/alice-adventures/Alice):** Alice is
  work-in-progress platform that is focused on teaching Ada and SPARK by
  focusing on high quality lessons and solutions (excelence as it is indicated
  in Alice's documentation). One could help develop its infraestructure, testing
  and adding lessons and tasks to it.
- **[Exercism](https://exercism.org/):** Execism is a learning platform where
  students solve problems (with varying degrees of complexity) and then a tutor
  or reviewer can give them feedback if necessary. Ada used to have an entry in
  the website [see deprecated
  repo](https://github.com/exercism/DEPRECATED-ada). However, due to the lack of
  exercises and people willing to carry out the work maintaining it and helping
  students, it is no longer available. I would be great if it could be brought
  back. Nonetheless, Alice, see above, is trying to achive a similar goal while
  being focused on Ada/SPARK.

## Toolchains

- **[Alire](https://alire.ada.dev/):** Alire is one of the most important tools
  for the Ada community, regardless of the experience that one may have with the
  language. Alire is always looking for testers for new releases, feedback,
  improvements and fixes. If you are looking for a project to help with, Alire
  would be great!
  - **Add new packages:** one of the easiest ways to help Alire is by adding Ada
    projects and packages to the index and making them available for the entire
    community.
  - **Help with the documentation.**
  - **Help build and test Alire** in new architectures.
  - **Improve the [compiler
    packaging](https://github.com/alire-project/GNAT-FSF-builds)** of Alire by
    adding a new architecture and testing it more.
  - **Package [GNAT-LLVM](https://github.com/AdaCore/gnat-llvm/).**

## Libraries and Tools

### Bindings

- **Create bindings to well-known tools and libraries:** not a lot of tools are
  implemented in Ada and not all need to be! But it is always more convenient if
  Ada users could just start using a wider variety of tools and libraries. For
  that reason, it would be greatly benefitial to have more Ada bindings to
  well-known and widely-used projects so that both, new and seasoned Ada
  programers, can easily pick them up and start using their functionality.
  - **Ideas:**
    - **[ImGUI](https://github.com/ocornut/imgui)** is a widely use GUI
      library. If C++ bindings prove too much, then
      [cImGUI](https://github.com/cimgui/cimgui) could be used as a base.
    - **[stb](https://github.com/nothings/stb)** is a collection of C
      header-only utilities in the public domain.
    - **[raylib](https://github.com/raysan5/raylib)** is a collection of simple
      commonly-used utilities and libraries geared towards creating videogames.
    - **[ccv](https://github.com/liuliu/ccv)** and
      **[OpenCV](https://opencv.org/)** are libraries geared towards computer
      vision and processing.
- **[SWIG4Ada](https://github.com/charlie5/swig4ada):** Ada has outstanding
  support for creating bindings to other languages, specially C, Fortran and
  Cobol. GCC also has a method for creating bindings to C++ projects, however,
  it is quite limited with what it can do. SWIG4Ada implements support for Ada
  into the SWIG binding generator. It is still incomplete and it could use some
  more hands.

### Graphics and Games

TODO: Mention SDL2Ada

TODO: Mention GTK-Ada

### Web development

TODO: Mention AWS

TODO: AdaWebpack (Ada-WASM)

### Cryptography and safety

- **[WolfSSL](https://www.wolfssl.com/)** is a complete and certifiable, among
  others, TLS library. It recently [added Ada binding
  support](https://github.com/wolfSSL/wolfssl/tree/master/wrapper/Ada/). It
  would be nice for the community to test it, improve it if necessary,
  potentially create SPARK code around it and package it into more
  places. Promoting it would also be a benefit.
- **[SPARKNaCl](https://github.com/rod-chapman/SPARKNaCl/)** is a cryptographic
  library fully proven in SPARK with no runtime nor external dependencies. It
  would be nice seing it receive a bit more marketing in non-Ada environments
  and projects implemented with it!

## Operating Systems

The Ada ecosystem sports some of the most interesting kernels/OSes out
there. Operating System development tends to be labeled as an advance
topic. However, if you are learning, thanks to Ada's clear syntax, it becomes
much easier to understand them, use them and even contribute to an OS. Here is a
list of kernels/OSes written in pure Ada/SPARK that may greatly benefit from the
community:

- **[Ironclad](https://ironclad.cx/):** it is a fairly new kernel that strives
  to be POSIX compatible. It partially written in SPARK, making it much more
  robust and safe by default. It aims to support x86, ARM and RISC-V
  architectures. You can contribute to it or you can help grow the
  [Gloire](https://github.com/streaksu/Gloire) OS distribution, which uses
  Ironclad as its kernel.
- **[HiRTOS](https://github.com/jgrivera67/HiRTOS):** it is a high integrity
  RTOS kernel as well as a separation kernel written in SPARK. Go and give it a
  try! One can even install it using Alire! It currently targets the ARM
  Cortex-R52 CPU. One could try and port it to a different CPU.
- **[Muen](https://muen.codelabs.ch/):** it is a separation kernel fully written
  in SPARK. It has been certified by some large military bodies for their internal
  use. It also sports some drivers fully written in SPARK!
- **[M2OS](https://m2os.unican.es/):** it is a small RTOS that was created from
  the same people behind the Ada OS [Marte](https://marte.unican.es/).

There are some well known Operating Systems that allow users to run Ada programs
in them directly even if they are written in a different language. You may want
to use them, improve the Ada support they have or write examples or blog posts
about their use:

- **[RTEMS](https://www.rtems.org):** it is a POSIX RTOS that is widely used in
  the aerospace industry. It has official Ada support. However, its use is not
  clearly documented nor widely known about; this could be fixed.

## Embedded Programming

TODO: Add Ada_Drivers_Library

TODO: Create more HALs

TODO: Improve svd2ada and startup-gen

TODO: Add SweetAda as a multiarch development platform. Docs could use a bit of help.

TODO: improve support for limited profiles in embedded OSes (Jorvik, Ravenscar)

TODO: Mention AVR-Ada?

## Performance and benchmarks

Ada tends to be shown in a lot of benchmarks, and that should be no surprise, as
we are a highly optimizable, compiled programming language that does nor use
garbage collection and leverages some of the best compiler infraestructure in
the world (GCC and LLVM). However, we do not tend to rank at the top. "Why is
that?" one may ask... Well... Most Ada code used for benchmarking porpuses has
not been receiving the same amount of effort and care that other programming
languages have. Let's fix this!

The following proposals are meant to be undertaken by people familiar with Ada
and HPC (High Performance Computing):

- **[The Computer Language Benchmarks
  Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html):**
  Ada programs could be optimised to utilise newer language constructs,
  algorithmic design and, once available and if useful, the new Ada 2022
  `parallel` keyword. This should yield better results than the already (and
  performant) programs. I believe we can improve our times quite a bit!
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
    repository](https://github.com/search?q=repo%3Ajrmarino%2Fdraco%20path%3A%2F%5Emisc%5C%2Fflux13%5C%2F%2F%20android&type=code). Nontheless,
    Ada also supports Android with WASM, [see this
    demo](https://blog.adacore.com/android-application-with-ada-and-webassembly).
  - **[OpenBSD](https://www.openbsd.org/):** OpenBSD has supported GNAT for a
    few architectures for a long while, however, the patches to add support have
    never been mainlined. The patches can be found in their ports tree or in
    [this Github
    mirror](https://github.com/openbsd/ports/tree/master/lang/gcc/11/patches)
  - **[NetBSD](http://netbsd.org/):** updated GCC patches to add support for
    NetBSD (amd64) can be found
    [here](https://github.com/jrmarino/draco/tree/master/misc/flux13). Adding
    support for other architectures can somewhat easily be done. Thank to
    J. Marino for his work.
  - **[FreeBSD](https://www.freebsd.org/):** FreeBSD support within GNAT could
    use quite a bit of help. This is [the current patch used by
    FreeBSD](https://gitlab.com/FreeBSD/freebsd-ports/-/tree/main/lang/gnat12/files).
- **Adding support for more OSes:** support could be added to the following
  "full-fledged" Operating Systems:
  - **[Ironclad](https://ironclad.cx/):** it is a POSIX compatible RTOS
    completely written in Ada. It currently has no RTS (RunTime System) support
    for compiling Ada programs targetting it.
  - **[Illumos](https://www.illumos.org/):** it is the continuation of
    OpenSolaris as an open source project. It currently has no Ada support
    within GNAT.
  - **[Gnode](https://genode.org/):** it is an OS framework that is based on
    components and separation of concerns and security.
  - **[Haiku](https://www.haiku-os.org/):** it is a reemplementation of BeOS.
  - **Microkernel and RTOS Systems:** there are quite a few other OSes that we
    could support. They are either widely used or best in class for their target
    audience.
    - **[Zephyr](https://www.zephyrproject.org/):** it is an RTOS that has been
      gaining a lot of traction and support. Initial support for the Zephyr RTOS
      was started [here](https://github.com/ila-embsys/zephyr-ada-gnat-rts).
    - **[se4L](https://sel4.systems/)** is a secure, verified RTOS microkernel
      for x86, ARM and RISC-V devices.
    - **[FreeRTOS](https://freertos.org/)** is a widely use RTOS for
      microcontrollers. There are [some Ada
      ports](https://github.com/simonjwright/cortex-gnat-rts) that have support
      for it. However, mainline support would be preferred.
    - **[Azure ThreadX](https://github.com/eclipse-threadx/threadx)** is a
      certified RTOS for embedded applications.
    - **[Apache Nuttx](https://nuttx.apache.org/)** is a RTOS for embedded
      devices.
    - **[Minix 3](https://www.minix3.org/)** is one of the most well-known
      microkernels. It has enough POSIX support that is can run the NetBSD
      userland on top of it.
    - **[RT-Thread](https://www.rt-thread.io/)** is a new RTOS for embedded
      devices.
    - **[RIOT](https://www.osrtos.com/rtos/riot/)** is a RTOS for small embedded
      systems.
  - **Unikernels:** these kernels have been created to run a single application
    and be the most secure possible as they have the smallest surface. The Ada
    community should ensure that Ada applications can run on these kernels.
    - **[Mirage OS](https://mirage.io/)** is a well-known unikernel written in
      OCaml. See also **[Solo5](https://github.com/Solo5/solo5).** For example,
      the [Muen](https://muen.codelabs.ch/) website runs on it.
    - **[Unikraft](https://unikraft.org/)** is a unikernel focused on the
      cloud and in security.
    - **[Toro](https://torokernel.io/)** is a unikernel written for
      microservices.
- **[GNAT-LLVM](https://github.com/AdaCore/gnat-llvm/):** the GNAT-LLVM project
  is fairly new but very promising. It would be nice to test it, help with the
  documentation and create cool demos, see [this Ada-WASM
  example](https://github.com/godunko/adawebpack).
- **Bootstrapping GNAT:** GNAT currently requires a previous version to compile
  itself. Sadly, there was never an original public version of GNAT that did
  not have such requirement. This means, that GNAT cannot be compiled without
  already having a working copy of a GNAT installation. The [GNAT Bootstrapping
  project](https://fossil.irvise.xyz/gnat-bootstrap/home) aims to create a
  small, minimal compiler together with the [live-bootstrap
  project](https://github.com/fosslinux/live-bootstrap/) in order to build an
  initial GNAT compiler.
  - **Requirements:** Scheme and compiler programming knowledge.
- **[HAC](https://github.com/zertovitch/hac):** HAC is a small and limited Ada
  compiler, but it could make use of some help for those interested in compilers
  and Ada.
