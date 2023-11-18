---
sidebar_position: 3
draft: true
---

# Alire vs Apple silicon

Alire releases available from the [Alire website](https://github.com/alire-project/alire/releases) are all built for Intel silicon. For pure Ada work, this has no effect, whether or not you're working on Apple silicon.

One area where there's a considerable impact is when your work involves "[external releases](https://alire.ada.dev/docs/#external-releases)". These are external libraries which Alire manages as required using your system's "package manager". An example is the crate `sdlada`, which depends on `libsdl2` amongst others. On a Debian system, Alire will load the package `libsdl2-dev`; on macOS with Homebrew, `sdl2`.

For macOS, the package managers supported are [Homebrew](https://brew.sh) and [MacPorts](https://www.macports.org) - if you have both installed (not really recommended) Alire will choose Homebrew. Homebrew is the one that this page concentrates on.

If yours is an `x86_64` Mac, Homebrew will load `x86_64` binaries under `/usr/local/`. If it's an `aarch64` Mac, Homebrew will load `aarch64` binaries under `/opt/homebrew/` (this is by default; trying to mix architectures is likely to be at best confusing).

So, if you're on an `aarch64` Mac with an `x86_64` GNAT compiler, you won't be able to use external libraries, because the linker will refuse to link your `x86_64` binaries with Homebrew's `aarch64` ones.
