---
sidebar_position: 100
---

# Issues

- [Linking error](#linking-error)
- [Unhandled exceptions](#unhandled-exceptions)
- [Transparent solution](#transparent-solution)
- [The future](#the-future)

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

The updated SDKs (version 15.1) fixed this issue, so you no longer needed the workround.

## <a name="unhandled-exceptions">Unhandled exceptions</a>

It turns out that there's a more subtle problem than the blatant failure to link: exception handling can be unreliable. If you get unhandled exceptions from code with a clearly visible exception handler, this is what's going on.

This issue is also solved by using the classic linker.

## <a name="transparent-solution">Transparent solution</a>

We have a solution which transparently invokes `ld-classic` if it's present in the SDK. The solution is to place a 'shim' named `ld` where GCC will look for it and invoke it instead of directly calling `/usr/bin/ld`.

The latest release of the installer can be found [here](https://github.com/simonjwright/xcode_15_fix/releases).

## <a name="the-future">The future</a>

It turns out that the reason for the issue is that GCC mishandles the Darwin ABI by placing exception handling data in the wrong segment of the executable. This has been fixed in the GCC 14.0.1 pre-release, and it's hoped that it will be backported to GCC 13.3.
