---
sidebar_position: 4
draft: true
---

# Crates for macOS

There's a Mac-special index which holds crate versions that

- support Apple silicon,
- overcome some issues that arise from macOS features:
  - _gprbuild_ can try to build static standalone libraries, where the 'standalone' part means that the library will be automatically elaborated. It does this using features available in GNU binutils, but not in Mach-O binary. Alternative versions of crates that normally would attempt this are provided (`libadalang`, `langkit_support`).

The crates provided are:

- Tools
  - `gnat_macos_aarch64=13.1.1`, native GNAT for Apple silicon.
  - `gprbuild=23.0.1-mac-aarch64`, matching gprbuild (avoids you having to say `--target=aarch64-apple-darwin` at every compilation!)
- Libraries
  - `langkit_support-23.0.1`, builds a plain static library.
  - `libadalang-23.0.1`, likewise.

To install:

```
$ alr index \
   --add=git+https://github.com/simonjwright/alire-index.mac.git \
   --before=community \
   --name=index_for_mac
```
