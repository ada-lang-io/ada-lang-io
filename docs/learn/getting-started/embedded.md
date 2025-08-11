---
sidebar_position: 6
draft: false
---

# Embedded Systems

Embedded systems are the invisible workhorses of modern technology. They are specialized computing systems designed for a specific function within a larger device, from smartwatches and home appliances to critical automotive and aerospace systems. Each system is an integrated unit, consisting of a CPU, memory, I/O, and peripherals selected based on criteria such as size, cost, production volume, etc.

Embedded system software development presents a unique challenges. Engineers must contend with strict constraints on processing power, memory, and energy consumption. Their software must be exceptionally reliable where errors can lead to catastrophic results. Furthermore, the hardware itself is often a moving target, with component availability forcing design changes mid-cycle.

## Why Use Ada?

The Ada programming language was conceived from the ground up to address these very challenges, making it an excellent choice for embedded development. In other languages, such as C and C++, performance and control is paramount. Ada's design philosophy, however, prioritizes the following:

- **Reliability and Safety:** Ada's strong, static type system and compile-time checks catch a wide range of common programming errors before the code ever runs. This focus on correctness is invaluable for building software that must not fail.
- **Efficiency:** The language is designed to compile to fast, compact machine code. Its features are implemented with a keen awareness of hardware limitations, enabling efficient use of constrained resources and reducing power consumption.
- **Concurrency:** Ada has powerful concurrency features, like tasks and protected objects, built directly into the language. This allows developers to write sophisticated, multi-threaded applications without relying on an external Real-Time Operating System (RTOS), even on small microcontrollers.
- **Expressiveness and Maintainability:** Ada's syntax allows developers to model real-world problems clearly and accurately. Features like packages, generics, and a rich type system lead to code that is readable, maintainable, and easier to verify.
- **Portability:** Ada's standardized features, supported by a platform-specific runtime, allow for a high degree of code reuse across different target platforms without sacrificing performance or access to special hardware capabilities.

## Ada's Development Environment

A typical Ada development environment is a collection of programs that work together to turn your source code into a functioning program on your target hardware.

### 1. The Toolchain

A **toolchain** is a set of programming tools used to create software. For embedded systems, we use a **cross-compiler**, which runs on your development machine (the _host_, e.g., a Windows PC or MacBook) but generates executable code for a different architecture (the _target_, e.g., an ARM microcontroller).

We will use the free and open-source **GNAT** compiler, which is part of the **GNU Compiler Collection (GCC)**. GNAT is available for nearly every major embedded CPU architecture, including:

- ARM (e.g., Cortex-M series)
- RISC-V
- AVR
- Xtensa (ESP32)

GCC toolchains are identified by a **target triplet**, which follows the format:

_architecture_-_vendor_-_os_

| Triplet Segment | Description                                                                                                                                           | Example            |
| :-------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------- | :----------------- |
| _architecture_  | The CPU architecture, such as `arm` or `riscv64`.                                                                                                     | `arm` `esp32`      |
| _vendor_        | The toolchain provider, often `unknown` or omitted (_none_).                                                                                          | `none`             |
| _os_ / _abi_    | The target operating system (`linux-gnu`) or Application Binary Interface (`eabi`). For bare-metal embedded systems, `eabi` (Embedded ABI) is common. | `eabi` `linux-gnu` |

For our STM32 board, the target triplet is `arm-none-eabi`, often shortened to `arm-eabi`. The resulting toolchain name in Alire is `gnat_arm_elf`.

### 2. The Runtime Library (RTL)

The Ada **Runtime Library (RTL)** is a crucial library that links with your code to provide the services necessary for an Ada program to run. It's the bridge between high-level language features and the low-level hardware. The RTL handles memory management, tasking (concurrency), exception handling, and implementations for standard library functions.

Because every hardware platform is different, each requires a specific RTL. Ada addresses the diverse needs of embedded systems by offering different RTL **profiles**:

- **Light:** A minimal runtime for severely constrained devices. It omits support for resource-intensive features like tasking and exception handling, resulting in a very small memory footprint.
- **Light-Tasking:** A popular choice for real-time systems. It provides a deterministic, efficient implementation of Ada's concurrency features (the Ravenscar and Jorvik profiles) suitable for systems that require multitasking without a full-blown OS.
- **Embedded:** A more complete runtime that supports the full range of Ada's features, suitable for more powerful embedded systems.

Choosing the right profile is a trade-off between features and resources. For this guide, we'll use `light-tasking-stm32f4`.

### 3. The Ada Driver Library (ADL)

To control hardware like a GPIO pin or a UART serial port, you need to write to specific memory-mapped registers on the microcontroller. A **Hardware Abstraction Layer (HAL)** provides a standardized software interface to these hardware components. It hides the complex, low-level details, allowing you to write more portable and readable code. Instead of `write(0x40020C14, 0x00002000)`, for instance, you can write `LED.Turn_On`.

The **Ada Drivers Library (ADL)** is a comprehensive, open-source collection of drivers and Hardware Abstraction Layers (HALs) for various microcontrollers and peripherals. Additionally, it includes a collection of preconfigured drivers as platforms for targeting vendor development boards, such as Raspberry Pi Foundation’s RP2040 or the STMicroelectronics Discovery boards. Licensed under the permissive BSD license, the ADL is free to use in any project, including commercial products.

### 4. Alire: The Package and Build Manager

**Alire** is the essential tool that ties this entire ecosystem together. It's a "Swiss Army knife" for Ada development that functions as both a package manager and a build tool.

- As a **package manager**, Alire fetches all the dependencies your project needs, including toolchains, runtimes, and libraries (which it calls _crates_).
- As a **build tool**, Alire provides a user-friendly front-end to GNAT's underlying build system (`gprbuild`). It automates the entire process of compiling, linking, and generating the final executable.

> [!IMPORTANT]
>
> The only program you need to manually install for the following section is Alire. Download it from https://alire.ada.dev and follow the installation instructions on the page.

## Hands-On: Blinking an LED with Ada on STM32

Let's walk through the end-to-end process of creating, building, and deploying a "blinky" application for an STM32F4-based board.

### Prerequisites

1. **Alire** installed on your system.
2. An **STM32F4-based board** (e.g., STM32F4VE or STM32F407 Discovery).
3. An **ST-Link v2** programmer/debugger.
4. **ST-Link tools** (`st-flash`) or **OpenOCD** installed for flashing the board.

### Step 1: Create and Initialize the Project

First, create a new binary (executable) project using Alire. Open your terminal and run:

```shell
# Initialize a new binary project named "blinky_demo"
alr init --bin blinky_demo

# Alire will ask for project details; press Enter to accept the defaults.

# Navigate into the new project directory
cd blinky_demo
```

### Step 2: Configure the Project for the Target

Now, we need to tell Alire about our target hardware by editing two files.

1. **Add the ARM Toolchain Dependency:**
   Alire can fetch the correct cross-compiler for us.

   ```shell
   # Add the latest GNAT ARM ELF toolchain as a project dependency
   alr with gnat_arm_elf
   ```

2. **Specify the Target and Runtime:**
   Next, edit the GNAT Project file, `blinky_demo.gpr`, to specify our target architecture and the runtime library profile.

   ```ada
   -- Add these two lines to your .gpr file
   for Target use "arm-eabi";
   for Runtime ("Ada") use "light-tasking-stm32f4";
   ```

### Step 3: Add the Ada Drivers Library (ADL)

To control the LEDs, we need the hardware drivers from ADL. The modern way to do this with Alire is to `pin` a specific board support package from the ADL GitHub repository. The **subdir** argument specifies the target board. Appendix B list several preconfigured and ready to use boards.

```shell
# Pin the ADL board support crate for the stm32f4ve
alr pin stm32_f4ve_sfp --use=https://github.com/AdaCore/Ada_Drivers_Library --subdir=boards/stm32_f4ve
```

Alire will download the repository and make the `stm32_f4ve` project available as a dependency.

### Step 4: Write the Blinky Application

Replace the contents of `src/blinky_demo.adb` with the following code. This program initializes the board's LEDs and then enters an infinite loop, toggling them every half-second.

```ada
-- src/blinky_demo.adb
with STM32.Board;  -- HAL for board-specific features (like LEDs)
with STM32.GPIO;   -- HAL for General-Purpose Input/Output pins

procedure Blinky_Demo is
begin
   -- Initialize the board's hardware, including the pins connected to LEDs
   STM32.Board.Initialize_LEDs;

   loop
      -- Toggle the state of all configured LEDs (On -> Off, Off -> On)
      STM32.GPIO.Toggle (STM32.Board.All_LEDs);

      -- The "delay" statement is an Ada language feature supported by our
      -- light-tasking runtime. It provides a precise, non-busy wait.
      delay 0.5; -- Wait for 500 milliseconds
   end loop;
end Blinky_Demo;
```

### Step 5: Build, Prepare, and Flash the Executable

1. **Build the Project:**
   This command compiles and links your code, the runtime, and the drivers.

   ```shell
   alr build
   ```

2. **Create a Raw Binary:**
   Flash memory requires a raw binary file. We use the `objcopy` tool from our toolchain to create it.

   ```shell
   # Use 'alr exec' to run a tool from the project's toolchain
   alr exec -- arm-eabi-objcopy -Obinary bin/blinky_demo bin/blinky_demo.bin
   ```

3. **Flash to the Board:**
   Finally, write the binary to the microcontroller's flash memory. The address `0x08000000` is the standard starting address for flash on most STM32 MCUs, but you should always verify this for your specific hardware.

   Using `st-flash`:

   ```shell
   st-flash --connect-under-reset write bin/blinky_demo.bin 0x08000000
   ```

   Or using `openocd`:

   ```shell
   openocd -f interface/stlink.cfg -f target/stm32f4x.cfg -c 'program bin/blinky_demo.bin verify reset exit 0x08000000'
   ```

After the command completes, the board will reset, and your LEDs should begin blinking!

## Using GNAT Studio IDE

While the command line is powerful, many developers prefer an Integrated Development Environment (IDE). GNAT Studio has excellent support for embedded development.

1. **Install GNAT Studio:** Download a release from the [GNAT Studio GitHub page](https://github.com/AdaCore/gnatstudio/releases/).

2. **Configure the Project for the IDE:** Add an `Ide` package to your `blinky_demo.gpr` file to tell GNAT Studio how to connect to the debugger.

   For **OpenOCD**:

   ```gpr
   package Ide is
      for Program_Host use "localhost:3333";
      for Communication_Protocol use "remote";
      for Connection_Tool use "st-util";
   end Ide;
   ```

3. **Launch GNAT Studio:** From your project directory, run:

   ```shell
   gnatstudio
   ```

   Alire launches the IDE with the correct environment variables set. GNAT Studio will detect the embedded configuration and provide buttons to build, flash, and debug your application with a full graphical interface.

## Conclusion

We've journeyed from the foundational concepts of embedded systems to deploying a functional Ada application on real hardware. As we've demonstrated, Ada is an excellent choice. We encourage you to explore the examples in the Ada Drivers Library and continue with your journey in exploring Ada.

## Appendix A: Supported ARM Runtime Libraries

This is a partial list of ARM-based runtimes available with the GNAT compiler.

### Embedded and/or Light-Tasking:

| Name              |
| :---------------- |
| feather_stm32f405 |
| microbit          |
| nrf52832          |
| nrf52833          |
| nrf52840          |
| nucleo_f401re     |
| openmv2           |
| rpi2              |
| rpi-pico          |
| rpi-pico-smp      |
| sam4s             |
| samg55            |
| samv71            |
| stm32f4           |
| stm32f429disco    |
| stm32f469disco    |
| stm32f746disco    |
| stm32f769disco    |
| tms570            |
| zynq7000          |

### Light Profile Only (`zfp`):

| Name        |
| :---------- |
| cortex-m0   |
| cortex-m0p  |
| cortex-m1   |
| cortex-m23  |
| cortex-m3   |
| cortex-m33f |
| cortex-m4f  |
| cortex-m7f  |

## Appendix B: Selected Ada Drivers Library Boards

The ADL supports dozens of boards. This table shows the `.gpr` project file name to use with `alr pin` for a few popular ones. The suffix denotes the runtime profile (`sfp` for light-tasking, `full` for embedded, `zfp` for light).

| Board (ADL Directory) | Light-Tasking (`sfp`)       | Embedded (`full`)            | Light (`zfp`)         |
| :-------------------- | :-------------------------- | :--------------------------- | :-------------------- |
| feather_stm32f405     | feather_stm32f405_sfp.gpr   | feather_stm32f405_full.gpr   |                       |
| HiFive1_rev_B         |                             |                              | hifive1_rev_b_zfp.gpr |
| MicroBit              |                             |                              | microbit_zfp.gpr      |
| NRF52_DK              |                             |                              | nrf52_dk_zfp.gpr      |
| stm32f407_discovery   | stm32f407_discovery_sfp.gpr | stm32f407_discovery_full.gpr |                       |
| stm32f429_discovery   | stm32f429_discovery_sfp.gpr | stm32f429_discovery_full.gpr |                       |
| stm32_f4ve            | stm32_f4ve_sfp.gpr          | stm32_f4ve_full.gpr          |                       |
| stm32f746_discovery   | stm32f746_discovery_sfp.gpr | stm32f746_discovery_full.gpr |                       |
