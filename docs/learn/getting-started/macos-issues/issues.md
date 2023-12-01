---
sidebar_position: 100
---

# Issues

- [Linking error](#linking-error)

## <a name="linking-error">Linking error</a>

As of November 2023, people have been getting errors like

```
❯ alr build
ⓘ Building myproj/myproj.gpr...
Link
   [link]         myproj.adb
0  0x10034af43  __assert_rtn + 64
1  0x10024cf43  ld::AtomPlacement::findAtom(unsigned char, unsigned long long, ld::AtomPlacement::AtomLoc const*&, long long&) const + 1411
2  0x100269431  ld::InputFiles::SliceParser::parseObjectFile(mach_o::Header const*) const + 19745
3  0x100279e44  ld::InputFiles::parseAllFiles(void (ld::AtomFile const*) block_pointer)::$_7::operator()(unsigned long, ld::FileInfo const&) const + 1380
4  0x7ff8051315cd  _dispatch_client_callout2 + 8
5  0x7ff805141e3e  _dispatch_apply_invoke + 214
6  0x7ff80513159a  _dispatch_client_callout + 8
7  0x7ff80514099d  _dispatch_root_queue_drain + 879
8  0x7ff805140f22  _dispatch_worker_thread2 + 152
9  0x7ff8052d5c06  _pthread_wqthread + 262
ld: Assertion failed: (resultIndex < sectData.atoms.size()), function findAtom, file Relocations.cpp, line 1336.
collect2: error: ld returned 1 exit status
gprbuild: link of myproj.adb failed
gprbuild: failed command was: /users/sdey02/.config/alire/cache/dependencies/gnat_native_13.2.1_c21501ad/bin/gcc myproj.o b__myproj.o -L/Users/sdey02/myproj/obj/development/ -L/Users/sdey02/myproj/obj/development/ -L/users/sdey02/.config/alire/cache/dependencies/gnat_native_13.2.1_c21501ad/lib/gcc/x86_64-apple-darwin21.6.0/13.2.0/adalib/ /users/sdey02/.config/alire/cache/dependencies/gnat_native_13.2.1_c21501ad/lib/gcc/x86_64-apple-darwin21.6.0/13.2.0/adalib/libgnat.a -Wl,-rpath,@executable_path/..//obj/development -Wl,-rpath,@executable_path/../..//.config/alire/cache/dependencies/gnat_native_13.2.1_c21501ad/lib/gcc/x86_64-apple-darwin21.6.0/13.2.0/adalib -o /Users/sdey02/myproj/bin//myproj
error: Command ["gprbuild", "-s", "-j0", "-p", "-P", "/Users/sdey02/myproj/myproj.gpr"] exited with code 4
error: Compilation failed.
```

This is a result of an update of the developer toolkit (Xcode, or the _much_ smaller Command Line Tools) to version 15.

### Immediate workround

An immediate workround is to use what's called the "classic linker" by using this Alire build command:

```
alr build -- -largs -Wl,-ld_classic
```

or with _gprbuild_

```
gprbuild -P my_proj -largs -Wl,-ld_classic
```

(similarly for _gnatmake_)

A slightly less intrusive alternative would be to alter your GNAT Project (`.gpr`) file by including a 'linker' package:

```
package Linker is
   for Default_Switches ("Ada") use ("-Wl,-ld_classic");
end Linker;
```

Of course, if you already have a `package Linker` it'll need adjusting.

### Unintrusive workround

Until version 15.1 of the developer tools is released, you can install a beta version which doesn't have this problem.

Find out which set of developer tools you have installed by `xcrun --show-sdk-path`: if the result starts `/Applications/Xcode.app/` it's Xcode, if it starts `/Library/Developer/CommandLineLTools/` it's the Command Line Tools.

Go to the [Apple Developer "More Downloads"](https://developer.apple.com/download/all/) page (you'll need a free account, you may need a separate Apple ID) and select the beta package matching your current developer tools: download and install it. The _beta 2_ set is known to work, _beta 3_ is available (18 November 2003).

> Note for the Xcode betas: they come as `.xip` files, which need to be extracted using Apple's proprietary _xip_ tool: `xip --expand package.xip`. You'll need to tell macOS to use the beta using `xcode-select --switch`.
