---
sidebar_position: 100
---

# Contribute

[ada-lang.io](https://ada-lang.io) is built using [Docusaurus](https://docusaurus.io/).
This gives us a great looking site while also supporting both Markdown and also
React for more control where needed.

## Run and edit locally

1. Install [Node][url-node] and [Yarn][url-yarn]. `yarn --version` should show version 2 or higher.

2. Then install the dependencies of this package.

```bash
cd ada-lang-io
yarn install
```

3. Run the site locally. This will open up your web browser to `http://localhost:3000`.

```bash
yarn start
```

It will update as you make changes.

4. Install the postcommit hooks to format your code appropriately.

```bash
yarn postinstall
```

5. When you're satisfied with your work, send a [pull request][url-ghpr] to ask to
   have your work added.

[url-node]: https://nodejs.org/en/download/
[url-yarn]: https://yarnpkg.com/getting-started/install
[url-ghpr]: https://github.com/ada-lang-io/ada-lang-io/pulls
