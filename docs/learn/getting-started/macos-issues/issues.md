---
sidebar_position: 100
---

# Issues

- Issues
  - [C compilation failure](#c-compilation-failure)
  - [Linking error](#linking-error) (`ld: Assertion failed`)
  - [Unhandled exceptions](#unhandled-exceptions)

## <a name="c-compilation-failure">C compilation failure</a>

If you get apparently self-contradictory C- or C++-related errors like

```sh
$ gcc -c hello.c
In file included from hello.c:1:
/opt/gcc-14.2.0-1-aarch64/lib/gcc/aarch64-apple-darwin21/14.2.0/include-fixed/stdio.h:83:8:
 error: unknown type name ‘FILE’
   83 | extern FILE *__stdinp;
      |        ^~~~
/opt/gcc-14.2.0-1-aarch64/lib/gcc/aarch64-apple-darwin21/14.2.0/include-fixed/stdio.h:81:1:
 note: ‘FILE’ is defined in header ‘<stdio.h>’; this is probably
 fixable by adding ‘#include <stdio.h>’
   80 | #include <sys/_types/_seek_set.h>
  +++ |+#include <stdio.h>
   81 |
```

it's because your Software Development Kit (SDK: Xcode or the Command Line Tools) has been upgraded to version 16, and your installed compiler hasn't been built to handle it; see [GCC PR 116980](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=116980).

Until that's resolved, the only general solution is to revert to SDK15.

## <a name="linking-error">Linking error</a>

If you're running macOS 13.5 (Ventura) or later, you'll probably have been offered updates to version 15 of Xcode/the Command Line Tools. With version 15.0, we encountered the, rather fundamental, problem that all links failed, with a slew of messages including

```
ld: Assertion failed: (resultIndex < sectData.atoms.size()), function findAtom, file Relocations.cpp, line 1336.
```

This happened because Apple had introduced a new linker (`ld`). A workround for this issue turned out to be to use a version of the older linker included in the SDK, `ld-classic`. You can tell `gprbuild` or `gnatmake` to do this from the command line by adding

```
-largs -Wl,-ld_classic
```

(notice the change from dash to underscore!) This fix doesn't work with older SDKs that don't have `ld-classic` - they would interpret it as looking for a library `libd_classic`.

The updated SDKs (version 15.1) fixed this issue, so you no longer needed the workround; but, if you do, see the notes on the [transparent solution](#transparent-solution).

## <a name="unhandled-exceptions">Unhandled exceptions</a>

It turns out that there's a more subtle problem than the blatant failure to link: exception handling can be unreliable. If you get unhandled exceptions from code with a clearly visible exception handler, this is what's going on.

This issue is also solved by using the classic linker: see the notes on the [transparent solution](#transparent-solution). Alternatively, upgrade to a released GCC 14 compiler.

### <a name="transparent-solution">Transparent solution</a>

We have a solution which transparently invokes `ld-classic` if it's present in the SDK. The solution is to place a 'shim' named `ld` where GCC will look for it and invoke it instead of directly calling `/usr/bin/ld`.

The latest release of the installer can be found [here](https://github.com/simonjwright/xcode_15_fix/releases).
