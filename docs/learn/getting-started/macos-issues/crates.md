---
sidebar_position: 040
---

# Crates for macOS

There's a Mac-special [Alire repository on Github](https://github.com/simonjwright/alire-index.mac.git) which provides three things, two of which are tool-related and hopefully temporary.

- [Alire](#alire) itself
- [Workarounds](#workarounds) for macOS-related issues
- [Toolchain items](#toolchain-items) (compiler, gprbuild)

## <a name="alire">Alire</a>

At 2024-07-31, the official Alire site only supports an Intel (x86_64) build of Alire, which won't support native development on Apple silicon (there is an aarch64 version in the [nightly](https://github.com/alire-project/alire/releases/tag/nightly) builds).

You can find an Apple (aarch64) build of Alire 2.0.1 [here](https://github.com/simonjwright/alire-index.mac/releases/tag/alr-2.0.1-bin-aarch64-macos).

## <a name="workarounds">Workarounds</a>

_gprbuild_ can try to build static standalone libraries, where the 'standalone' part means that the library will be automatically elaborated. It does this using features available in GNU binutils, but not in Mach-O binary -- on either machine architecture. Alternative versions of crates that normally would attempt this are provided in the [Mac-related index](#installing-the-mac-special-alire-index):

- `langkit_support-24.0.1`, builds a plain static library.
- `libadalang-24.0.1`, likewise.

## <a name="toolchain-items">Toolchain items</a>

The toolchain crates in the [Mac-related index](#installing-the-mac-special-alire-index) are

- `gnat_macos_aarch64=14.1.0-1`, native GNAT for Apple silicon.
- `gprbuild=24.0.1-mac-aarch64`, matching gprbuild (avoids you having to say `--target=aarch64-apple-darwin` at every compilation!)

However, you should not need to use them, because Alire's community index already (2024-07-31) provides `gnat_native=14.1.3` and `gprbuild=24.0.1` for macOS, in both x86_64 and aarch64 versions; if you have the appropriate version of `alr` it will select the appropriate versions of the tools.

Note that the 14.1.3 compiler should more properly be identified as 14.1.0-3, since the base compiler is still FSF GCC 14.1.0, and this is the third released build.

## <a name="installing-the-mac-special-alire-index">Installing the Mac-special Alire index</a>

To install:

```sh
$ alr index --reset-community
$ alr index \
   --add=git+https://github.com/simonjwright/alire-index.mac.git \
   --before=community \
   --name=index_for_mac
```
