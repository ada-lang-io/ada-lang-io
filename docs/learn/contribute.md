---
sidebar_position: 100
---

# Contribute

[ada-lang.io](https://ada-lang.io) is built using [Docusaurus](https://docusaurus.io/).
This gives us a great looking site while also supporting both Markdown and also
React for more control where needed.

## Setup

1. Install [Node][url-node] and [Yarn][url-yarn]. `yarn --version` should show version 2 or higher.

2. Then install the dependencies of this package. You might have to rerun this command after pulling the main branch if dependencies change.

```bash
cd ada-lang-io
yarn install
```

3. Install the postcommit hooks to format your code appropriately.

```bash
yarn postinstall
```

## Run and edit locally

1. Run the site locally. This will open up your web browser to `http://localhost:3000`.

```bash
yarn start
```

2. It will update as you make changes.

3. Verify a clean build before you submit. This ensures that there are no broken links.

```bash
yarn build
```

4. When you're satisfied with your work, send a [pull request][url-ghpr] to ask to
   have your work added.

# Generated docs

## Ada Reference Manual

This is built with [ada-auth][ada-auth].

Don't edit `docs/arm/*.mdx` files.

## Ada Quality and Style Guide

The guide is converted from [WiKi Book][style-guide].

Don't edit `docs/style-guide/*.mdx` files.
Use the [converter][style-converter] instead.

[ada-auth]: https://github.com/ada-lang-io/ada-auth
[url-node]: https://nodejs.org/en/download/
[url-yarn]: https://yarnpkg.com/learn/install
[url-ghpr]: https://github.com/ada-lang-io/ada-lang-io/pulls
[style-guide]: https://en.wikibooks.org/wiki/Ada_Style_Guide
[style-converter]: https://github.com/ada-lang-io/import-style-guide
