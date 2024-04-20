---
sidebar_position: 040
---

# Crates for macOS

There's a Mac-special [Alire repository on Github](https://github.com/simonjwright/alire-index.mac.git) which provides three things, two of which are tool-related and hopefully temporary.

- [Alire](#alire) itself
- [Workrounds](#workrounds) for macOS-related issues
- [Toolchain items](#toolchain) (compiler, gprbuild)

## <a name="alire">Alire</a>

At 2024-04-19, the official Alire site only supports an Intel (x86_64) build of Alire, which won't support native development on Apple silicon.

You can find an Apple (aarch64) build of Alire 2.0.1 [here](https://github.com/simonjwright/alire-index.mac/releases/tag/alr-2.0.1-bin-aarch64-macos).

## <a name="workrounds">Workrounds</a>

_gprbuild_ can try to build static standalone libraries, where the 'standalone' part means that the library will be automatically elaborated. It does this using features available in GNU binutils, but not in Mach-O binary -- on either machine architecture. Alternative versions of crates that normally would attempt this are provided in the [Mac-related index](#index):

- `langkit_support-24.0.1`, builds a plain static library.
- `libadalang-24.0.1`, likewise.

## <a name="toolchain">Toolchain items</a>

The crates in the [Mac-related index](#index) are

- `gnat_macos_aarch64=13.2.2`, native GNAT for Apple silicon.
- `gprbuild=24.0.1-mac-aarch64`, matching gprbuild (avoids you having to say `--target=aarch64-apple-darwin` at every compilation!)

Note that Alire's community index already (2024-04-19) provides `gnat_native=13.2.2` for macOS; no doubt an equivalent _gprbuild_ will be available shortly.

## <a name="index">Installing the Mac-special Alire index</a>

To install:

```sh
$ alr index --reset-community
$ alr index \
   --add=git+https://github.com/simonjwright/alire-index.mac.git \
   --before=community \
   --name=index_for_mac
```
