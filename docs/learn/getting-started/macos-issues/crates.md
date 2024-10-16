---
sidebar_position: 040
---

# Crates for macOS

There's a Mac-special [Alire repository on Github](https://github.com/simonjwright/alire-index.mac.git) which provides for macOS-related issues.

## Workarounds

_gprbuild_ can try to build static standalone libraries, where the 'standalone' part means that the library will be automatically elaborated. It does this using features available in GNU binutils, but not in Mach-O binary -- on either machine architecture. Alternative versions of crates that normally would attempt this are provided in the [Mac-related index](#installing-the-mac-special-alire-index):

- `langkit_support-24.0.1`, builds a plain static library.
- `libadalang-24.0.1`, likewise.

## Installing the Mac-special Alire index

To install:

```sh
$ alr index --reset-community
$ alr index \
   --add=git+https://github.com/simonjwright/alire-index.mac.git \
   --before=community \
   --name=index_for_mac
```
