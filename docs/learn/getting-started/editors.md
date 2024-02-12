---
sidebar_position: 3
---

# Editors

export const GPR_FILE = "{GPR_FILE}";

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

## Setting up your editor

You can adjust the editor that gets started by `alr edit` with Alire's
`editor.cmd` setting. Launching your editor this way with `alr edit`
causes Alire to start it with the appropriate environment variables and
settings.

### [Language server](https://github.com/AdaCore/ada_language_server)

[Ada has a language server](https://github.com/AdaCore/ada_language_server).
Some of the editors can be configured or have a plugin to use this so you might
not need to install this directly.

### GNAT Studio

Alire is configured to work with GNAT Studio by default if you have it
installed.

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
alr config --set editor.cmd "code workspace.code-workspace"
```

  </TabItem>
 <TabItem value="linux" label="Linux">

```bash
alr config --set editor.cmd "code workspace.code-workspace"
```

  </TabItem>
  <TabItem value="mac" label="macOS">

```bash
alr config --set editor.cmd "/Applications/VisualStudioCode.app/Contents/Resources/app/bin/code workspace.code-workspace"
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
alr config --set --global editor.cmd 'emacs --eval=(ada-build-prompt-select-prj-file"${GPR_FILE}") ${GPR_FILE}'
```

  </TabItem>
  <TabItem value="mac" label="macOS">

```bash
alr config --set editor.cmd 'open -n -a emacs ${GPR_FILE}'
```

Note, you still need to find one of the project's Ada source files and then select the relevant GPR file. Investigations continue.
</TabItem>
</Tabs>
