---
sidebar_position: 1
---

# Installation

An Ada development environment consists basically of a toolchain: an Ada compiler and a build manager. In the case of GNAT, the basic build manager [`gnatmake`](https://docs.adacore.com/live/wave/gnat_ugn/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#building-with-gnatmake) comes with the compiler, while a more capable one is the separate GNAT Project Manager [`gprbuild`](https://docs.adacore.com/live/wave/gprbuild/html/gprbuild_ug/gprbuild_ug.html).

By far the easiest way to get hold of an Ada toolchain is to use the Ada package manager [Alire](https://alire.ada.dev/docs/#introduction).

## Alire

The Alire website's [Releases page](https://github.com/alire-project/alire/releases) provides builds:

- the current stable build, [v2.1.0](https://github.com/alire-project/alire/releases/tag/v2.1.0),
- a [nightly build](https://github.com/alire-project/alire/releases/tag/nightly).

Any of these can be installed as described [here](https://alire.ada.dev/docs/#installation); follow up with these [first steps](https://alire.ada.dev/docs/#first-steps) (this will have the added effect of installing a toolchain for you!)

## Toolchain

Once you have Alire installed, you have a choice of toolchains. You'll normally want to use the latest native toolchain (the one that generates code to run on the machine you're developing on).

`alr toolchain --select` will present you with a list of choices, something like this:

```none
  1. gnat_native=13.2.1
  2. None
  3. gnat_arm_elf=13.2.1
  4. gnat_avr_elf=13.2.1
  5. gnat_riscv64_elf=13.2.1
  6. gnat_arm_elf=13.1.0
  7. gnat_avr_elf=13.1.0
  8. gnat_native=13.1.0
  9. gnat_riscv64_elf=13.1.0
  0. gnat_arm_elf=12.2.1
  a. (See more choices...)
Enter your choice index (first is default):
>
```

Press `<return>` to choose the latest compiler. The selection tool then looks for a matching `gprbuild`:

```none
Please select the gprbuild version for use with this configuration
  1. gprbuild=22.0.1
  2. None
  3. gprbuild=21.0.2
  4. gprbuild=21.0.1
Enter your choice index (first is default):
>
```

Again, press `<return>`. If necessary, Alire will download the selected tools and install them in the Alire environment.

<!--
I was going to say something about cross-toolchains here, but I don't think you'd set them up using alr toolchain --select (--local)?

A bit advanced.

### Cross-compilers

If your project is for an ARM microcontroller, such as the [STM32F4](https://www.st.com/en/microcontrollers-microprocessors/stm32f4-series.html),
-->

<!--
### gnatprove

TODO
-->
