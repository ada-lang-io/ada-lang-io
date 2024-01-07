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
  _newbees._ The Ada community could use a few more people willing to help and
  guide new programmers with their first few steps into the language.
- **Blog posts:** writing blogs is always a nice way to share knowledge,
  experience, tips and anything in between. Ada could use a little more of
  that. And don't forget to share the blog posts with the community afterwards!
- **Forums, social media and language discussions:** there are a lot of general
  discussions going on in the internet, we should make Ada have a bit more
  presence there (if it is suitable!).
  - **Example:** the [Awesome WASM Lang
    repository](https://github.com/appcypher/awesome-wasm-langs) lists all the
    languages that support targetting WASM. Ada supports WASM! but it is not on
    the list. It would be great if someone did a Pull Request.
- **Conferences:** if you feel confident with your speaking and teaching skill,
  going to a conference and showing off Ada is a great way to attrack the
  curious eyes of the audience. The topics here are also quite broad, ranging
  from simple and short tutorial all the way to niche and advance features of
  the language.

## Documentation

Documentation can easily make or break projects, luckily, Ada is one of the best
documented languages out there, thanks to it being an ISO standard and having
[extensive compiler and tooling
documentation](https://docs.adacore.com/live/wave/gnat_rm/html/gnat_rm/gnat_rm.html).
Nonetheless, things could be much better, specially for individual projects and
libraries. Documentation for individual tools will be listed in their respective
categories. Here are the main places which could use help with their
documentation

- **Ada-Lang.io:** you can help grow the documentation, guides, experience of
  this webpage, which tends to be one of the first resources that new people to
  the language make. The [Learn section](./learn/why-ada/) could use some extra
  hands, specially with the addition completion of [How-Tos](./category/how-tos)
  and [Tips and Tricks](./category/tips-and-tricks). The addition of SPARK
  documentation would also be greatly welcomed.
- **Ada Wikibook:** the [Ada
  Wikibook](https://en.wikibooks.org/wiki/Ada_Programming) has been a great
  source of knowledge for many years and many people. Nonetheless, it could use
  some clean-ups, completition and updates related to _Ada 2022._

## Toolchains

## Graphics and Games

## Libraries and Tools

## Operating Systems

## Embedded Systems

## Compilers

The proposals here are quite advance, therefore, they are not recommended for people unfamiliar with the Ada language.

- **Mainlining more patches to upstream GNAT:** GNAT/GCC-Ada is a wonderful
  piece of technology, and thanks to it being based on the GCC compiler
  infraestructure, it can target a large number of Operating Systems and
  architectures. However, GNAT/GCC-Ada requires a bit of setup and additional
  files that are needed in order to compile Ada code for these targets. The most
  common architecture/OS pairs are already present inside GCC, but some
  communities maintain their own set of patches in order to support GNAT. It
  would be ideal if those patches could become part of the GCC source tree. Here
  are some resources and patches that would need mainlining:
  - **[Android](https://source.android.com/):** in the past there were some
    versions of GNAT that had support for Android. Some patches for Android
    support can be found in [this
    repository](https://github.com/search?q=repo%3Ajrmarino%2Fdraco%20path%3A%2F%5Emisc%5C%2Fflux13%5C%2F%2F%20android&type=code). Nontheless, Ada also supports Android with WASM, [see this demo](https://blog.adacore.com/android-application-with-ada-and-webassembly).
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
  Operating Systems:
  - **[Zephyr](https://www.zephyrproject.org/):** it is an RTOS that has been
    gaining a lot of traction and support. Initial support for the Zephyr RTOS
    was started [here](https://github.com/ila-embsys/zephyr-ada-gnat-rts).
  - **[Ironclad](https://ironclad.cx/):** it is a POSIX compatible RTOS
    completely written in Ada. It currently has no RTS (RunTime System) support
    for compiling Ada programs targetting it.
  - **[Illumos](https://www.illumos.org/):** Illumos is the continuation of
    OpenSolaris as an open source project. It currently has no Ada support
    within GNAT.
- **[GNAT-LLVM](https://github.com/AdaCore/gnat-llvm/):** the GNAT-LLVM project is fairly new but very promissing. It would be nice to test it, help with the documentation and create cool demos, see [this Ada-WASM example](https://github.com/godunko/adawebpack).
- **Bootstrapping GNAT:** GNAT currently requires the a previous version to compile itself. Sadly, there was never an originial public version of GNAT that did not have such requirement. This means, that GNAT cannot be compiled without already having a working copy of a GNAT installation. The [GNAT Bootstrapping project](https://fossil.irvise.xyz/gnat-bootstrap/home) aims to create a small, minimal compiler together with the [live-bootstrap project](https://github.com/fosslinux/live-bootstrap/) in order to build an initial GNAT compiler.
  - **Requirements:** Scheme and compiler programming knowledge.
