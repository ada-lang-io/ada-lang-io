---
sidebar_position: 1
---

# Hardware

The first thing is, what sort of Mac do you have? Newer machines use Apple silicon (M1, M2, now M3 ... referred to as `aarch64` - Apple's name is `arm64`). Older machines use Intel silicon (`x86_64`).

Programs (which includes the toolchain) built for Intel silicon can run on Apple silicon, using Apple's "just in time" translation technology [Rosetta 2](<https://en.wikipedia.org/wiki/Rosetta_(software)#Rosetta_2>) - you will need to [enable it](https://support.apple.com/en-gb/HT211861) the first time you run an Intel program, after that it's automatic. Programs built for Apple silicon can't run on Intel.
