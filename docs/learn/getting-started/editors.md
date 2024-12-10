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

### [Language server](https://github.com/AdaCore/ada_language_server)

[Ada has a language server](https://github.com/AdaCore/ada_language_server).
Some of the editors can be configured or have a plugin to use this so you might
not need to install this directly.

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
alr settings --set editor.cmd 'open -n -a gnatstudio -- ${GPR_FILE}'
```

For later versions, use this workround (you'll need to change the `/Applications` part if you've installed GNATStudio somewhere else):

```bash
alr settings --set editor.cmd '/Applications/GNATStudio.app/Contents/MacOS/gnatstudio_launcher ${GPR_FILE}'
```

  </TabItem>
</Tabs>

To edit your project, run this from your project directory:

```bash
alr edit
```

### [Visual Studio Code](https://code.visualstudio.com)

- [Ada Language Server Plugin](https://marketplace.visualstudio.com/items?itemName=AdaCore.ada)

Make a `workspace.code-workspace` in your project folder with the name of your gpr file.

```json
{
  "folders": [
    {
      "path": "."
    }
  ],
  "settings": {
    "ada.projectFile": "my_project_name.gpr"
  }
}
```

Set Alire to look for a workspace.code-workspace in whatever directory you're
trying to open.

<Tabs groupId="operating-systems">
  <TabItem value="win" label="Windows">

```bash
alr settings --set editor.cmd "code workspace.code-workspace"
```

  </TabItem>
 <TabItem value="linux" label="Linux">

```bash
alr settings --set editor.cmd "code workspace.code-workspace"
```

  </TabItem>
  <TabItem value="mac" label="macOS">

```bash
alr settings --set editor.cmd "/Applications/VisualStudioCode.app/Contents/Resources/app/bin/code workspace.code-workspace"
```

  </TabItem>
</Tabs>

As long as you make a workspace file, you can now edit your projects with:

```bash
alr edit
```

### Vim

- [Ada Bundle](https://github.com/krischik/vim-ada) - A set of plugins for
  using Ada with VIM.

### Emacs

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
alr settings --set editor.cmd 'open -n -a emacs ${GPR_FILE}'
```

Note, you still need to find one of the project's Ada source files and then select the relevant GPR file. Investigations continue.
</TabItem>
</Tabs>

### [Zed]("https://zed.dev")
from Zed extensions interface, search for an extension called [Ada]("https://github.com/wisn/zed-ada-language") and click install, make sure that you have the LSP installed, you may need to restart your IDE.
