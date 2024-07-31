---
sidebar_position: 020
---

# Alire

By far the easiest way to get hold of a Mac toolchain is to use the Ada package manager [Alire](https://alire.ada.dev/docs/#introduction).

## Preparation

Before doing that, there is a preparatory step required for any development on a Mac.

You **must** have either _Xcode_ or the Command Line Tools installed (the CLTs are a lot smaller). **If you have version 15.0 installed, you need to update to at least version 15.1**.

_Xcode_ can be downloaded from the App Store.
Install the Command Line Tools by `sudo xcode-select --install`.

If you suspect your copy of the Command Line Tools is old, you can delete it by

```
sudo rm -rf /Library/Developer/CommandLineTools
```

and re-install.

You can find the version of the last installed CLTs by

```
softwareupdate --history | grep Command
```

## Installing `alr`

The Alire website's [Releases page](https://github.com/alire-project/alire/releases) provides Intel builds:

- the current stable build, [v2.0.1](https://github.com/alire-project/alire/releases/tag/v2.0.1)
- a [nightly build](https://github.com/alire-project/alire/releases/tag/nightly).

Any of these can be installed as described [here](https://alire.ada.dev/docs/#alr-on-macos).

## Next steps

Follow up with these [first steps](https://alire.ada.dev/docs/#first-steps) (this will have the added effect of installing a toolchain for you!)
