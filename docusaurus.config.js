// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const {themes} = require('prism-react-renderer')

const gitHubUserName = "ada-lang-io"
const gitHubProjectName = "ada-lang-io"

const alireDefaultVersion = "v1.2.1"
const alireGitHubLatestRelease = "https://api.github.com/repos/alire-project/alire/releases/latest"

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Ada Programming Language",
  tagline: "Readable, correct, performant",
  url: "https://ada-lang.io",
  baseUrl: "/",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/favicon.ico",

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: gitHubUserName, // Usually your GitHub org/user name.
  projectName: gitHubProjectName, // Usually your repo name.

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: "en",
    locales: ["en"]
  },

  customFields: {
    description: "A programming language for readable, correct, and performant software."
  },

  plugins: [
    "docusaurus-plugin-sass",
    (context, options) => ({
      name: "ada-lang-alire-version",
      async loadContent() {
        try {
          const req = await fetch(alireGitHubLatestRelease)

          if (req.ok) {
            const res = await req.json()
            return res.tag_name
          }
        } catch {
          return alireDefaultVersion
        }
      },
      async contentLoaded({ content, actions }) {
        const { setGlobalData } = actions
        setGlobalData({ alireVersion: content ?? alireDefaultVersion })
      }
    })
  ],

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve("./sidebars.js")
        },
        blog: false,
        theme: {
          customCss: require.resolve("./src/css/custom.scss")
        }
      })
    ]
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    {
      image: "img/ada_logo.svg", // Used for og:image and twitter:iamge
      colorMode: {
        respectPrefersColorScheme: true
      },
      navbar: {
        logo: {
          alt: "Ada",
          src: "img/ada_logo.svg",
          width: 32,
          height: 32
        },
        hideOnScroll: true,
        style: "dark",
        items: [
          {
            type: "doc",
            docId: "learn/why-ada",
            position: "left",
            label: "Learn"
          },
          {
            to: "/docs/arm",
            position: "left",
            label: "Reference Manual"
          },
          {
            type: "doc",
            docId: "style-guide/Ada_Style_Guide",
            position: "left",
            label: "Style Guide"
          },
          {
            to: "https://forum.ada-lang.io",
            position: "left",
            label: "Forum"
          },
          {
            type: "doc",
            docId: "projects-to-work-on",
            position: "left",
            label: "Projects to Work on"
          },
          {
            // See https://github.com/facebook/docusaurus/issues/7227 for custom components
            type: "custom-armAnnotations",
            position: "right"
          },
          {
            href: `https://github.com/${gitHubUserName}/${gitHubProjectName}`,
            position: "right",
            className: "fa-icon header__image header-github-link",
            "aria-label": "GitHub repository"
          }
        ]
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Learning",
            items: [
              {
                label: "Tutorial",
                to: "/docs/learn/why-ada"
              },
              {
                label: "Books",
                to: "/docs/learn/books"
              },
              {
                label: "Learn Ada (by AdaCore)",
                to: "https://learn.adacore.com"
              },
              {
                label: "Learn SPARK (by AdaCore)",
                to: "https://learn.adacore.com/courses/intro-to-spark/index.html"
              }
            ]
          },
          {
            title: "Docs",
            items: [
              {
                label: "Reference Manual",
                to: "/docs/arm"
              },
              {
                label: "Style Guide",
                href: "/docs/style-guide/Ada_Style_Guide"
              }
            ]
          },
          {
            title: "Community",
            items: [
              {
                label: "Forum",
                href: "https://forum.ada-lang.io",
                className: "fa-icon footer__image footer-discourse-link"
              },
              {
                label: "Stack Overflow",
                href: "https://stackoverflow.com/questions/tagged/ada",
                className: "fa-icon footer__image footer-stackoverflow-link"
              },
              {
                label: "Reddit",
                href: "https://www.reddit.com/r/ada/",
                className: "fa-icon footer__image footer-reddit-link"
              },
              {
                label: "Gitter",
                href: "https://gitter.im/ada-lang/Lobby",
                className: "fa-icon footer__image footer-gitter-link"
              },
              {
                label: "Telegram",
                href: "https://t.me/ada_lang",
                className: "fa-icon footer__image footer-telegram-link"
              },
              {
                label: "ARG Community Input",
                href: "https://arg.adaic.org/community-input"
              }
            ]
          },
          {
            title: "Ecosystem",
            items: [
              {
                label: "Alire (Ada package manager)",
                href: "https://alire.ada.dev/"
              },
              {
                label: "Awesome Ada",
                href: "https://github.com/ohenley/awesome-ada"
              },
              {
                label: "News, by Ada Planet",
                href: "https://ada-planet.blogspot.com/"
              },
              {
                label: "Ada Rapporteur Group",
                href: "https://arg.adaic.org/home"
              },
              {
                label: "Ada Europe",
                href: "https://ada-europe.org/"
              },
              {
                label: "GitHub",
                href: `https://github.com/${gitHubUserName}/${gitHubProjectName}`,
                className: "fa-icon footer__image footer-github-link"
              }
            ]
          }
        ],
        logo: {
          alt: "Alt",
          src: "img/ada_logo.svg",
          width: 128,
          height: 128
        },
        copyright: `Copyright © ${new Date().getFullYear()} and licensed under Apache 2.0 unless otherwise noted, by the ${gitHubProjectName} developers. Built with <a href="https://docusaurus.io/">Docusaurus</a>.`
      },
      prism: {
        theme: themes.github,
        darkTheme: themes.okaidia,
        defaultLanguage: "ada",
        additionalLanguages: ["ada"]
      },
      algolia: {
        // The application ID provided by Algolia
        appId: "L84ZBWQFIW",

        // Public API key: it is safe to commit it
        apiKey: "3ef66437cc70666c01852b9d2f4fde7b",

        indexName: "ada-lang",

        // Optional: see doc section below
        contextualSearch: true,

        // Optional: Algolia search parameters
        // See https://www.algolia.com/doc/api-reference/search-api-parameters/
        searchParameters: {},

        // Optional: path for search page that enabled by default (`false` to disable it)
        searchPagePath: "search"
      }
    }
}

module.exports = config
