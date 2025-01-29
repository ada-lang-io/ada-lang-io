---
sidebar_position: 3
---

# Editors

export const GPR_FILE = "{GPR_FILE}";

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

## Setting up your editor

The command `alr edit` launches your editor on the current crate, with the appropriate enviroment variables and settings.
You can adjust the editor that gets started by `alr edit` with Alire's
`editor.cmd` setting.

### Ada Language Server

The [Ada Language Server](https://github.com/AdaCore/ada_language_server) is an implementation of Microsoft's Language Server Protocol customised for Ada.
Some of the editors can be configured or have a plugin to use this so you might
not need to install it directly.

### GNAT Studio

<Tabs groupId="operating-systems">
  <TabItem value="linux" label="Linux">

Alire is configured to work with GNAT Studio by default if you have it
installed.

  </TabItem>
  <TabItem value="win" label="Windows">

Alire is configured to work with GNAT Studio by default if you have it
installed.

  </TabItem>
  <TabItem value="mac" label="macOS">

Find a community-provided version of GNAT Studio for macOS [here](https://sourceforge.net/projects/gnuada/files/GNAT_GPL%20Mac%20OS%20X/2024-ventura/).

There's a slight problem with macOS Sequoia.

For earlier versions of macOS, use this:

```bash
alr settings --set --global editor.cmd 'open -n -a gnatstudio -- ${GPR_FILE}'
```

For later versions, use this workround (you'll need to change the `/Applications` part if you've installed GNATStudio somewhere else):

```bash
alr settings --set --global editor.cmd '/Applications/GNATStudio.app/Contents/MacOS/gnatstudio_launcher ${GPR_FILE}'
```

  </TabItem>
</Tabs>

To edit your project, run this from your project directory:

```bash
alr edit
```

### Visual Studio Code

See the main [Visual Studio Code](https://code.visualstudio.com) site.

You can get the [Ada Language Server Plugin](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada) from the Visual Studio Marketplace.

On the command line, the command to open VSCode in the current directory is `code .`, so that's what we tell `alr edit`.

<Tabs groupId="operating-systems">
  <TabItem value="win" label="Windows">

```bash
alr settings --set --global editor.cmd "code ."
```

  </TabItem>
 <TabItem value="linux" label="Linux">

```bash
alr settings --set --global editor.cmd "code ."
```

   </TabItem>
  <TabItem value="mac" label="macOS">

On macOS, VSCode installs to `/Applications/Visual Studio Code`, and those spaces mean that Alire can't use that location directly. Instead, see the guide for [launching VS Code from the command line](https://code.visualstudio.com/docs/setup/mac#_launch-vs-code-from-the-command-line), which create a command `code ` in `/usr/local/bin`, which should be on your PATH.

Then,

```bash
alr settings --set --global editor.cmd "code ."
```

  </TabItem>
</Tabs>

Now you can edit your project with:

```bash
alr edit
```

### Vim

- [Ada Bundle](https://github.com/krischik/vim-ada) - A set of plugins for
  using Ada with VIM.

### Emacs

There are two Emacs extensions (modes) that support Ada. The older is Emacs Ada Mode, the newer is Ada TS Mode. Ada TS Mode uses the Ada Language Server.

<Tabs>
  <TabItem value="ada-ts-mode" label="Ada TS Mode" default>

See [this note](https://forward-in-code.blogspot.com/2025/01/ada-ts-mode.html) for how to install the `ada-ts-mode` extension.

Set Alire to use Emacs when invoking `alr edit`:

  <Tabs groupId="operating-systems">
    <TabItem value="linux" label="Linux">

```bash
alr settings --set --global editor.cmd 'emacs --eval=(ada-build-prompt-select-prj-file"${GPR_FILE}") ${GPR_FILE}'
```

    </TabItem>
    <TabItem value="mac" label="macOS">

```bash
alr settings --set --global editor.cmd 'open -n -a emacs .'
```

    </TabItem>

  </Tabs>

  </TabItem>
  
  <TabItem value="emacs-ada-mode" label="Emacs Ada Mode">

Install the `ada-mode` extension from [GNU ELPA](https://elpa.gnu.org/packages/ada-mode.html).

Set Alire to use Emacs when invoking `alr edit`:

  <Tabs groupId="operating-systems">
    <TabItem value="linux" label="Linux">

```bash
alr settings --set --global editor.cmd 'emacs --eval=(ada-build-prompt-select-prj-file"${GPR_FILE}") ${GPR_FILE}'
```

    </TabItem>
    <TabItem value="mac" label="macOS">

```bash
alr settings --set --global editor.cmd 'open -n -a emacs ${GPR_FILE}'
```

Note, you still need to find one of the project's Ada source files and then select the relevant GPR file. Investigations continue.
</TabItem>
</Tabs>

  </TabItem>

</Tabs>

### Zed

From [Zed](https://zed.dev) extensions interface, search for an extension called [Ada](https://github.com/wisn/zed-ada-language) and click install, make sure that you have the LSP installed, you may need to restart your IDE.
