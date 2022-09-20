// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github")
const darkCodeTheme = require("prism-react-renderer/themes/okaidia")

const gitHubUserName = "ada-lang-io"
const gitHubProjectName = "ada-lang-io"

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

  // https://github.com/facebook/docusaurus/issues/4765#issuecomment-841135926
  webpack: {
    jsLoader: (isServer) => ({
      loader: require.resolve("esbuild-loader"),
      options: {
        loader: "tsx",
        format: isServer ? "cjs" : undefined,
        target: isServer ? "node12" : "es2017"
      }
    })
  },

  plugins: ["docusaurus-plugin-sass"],

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
      image: "img/ada_saw_coin.svg", // Used for og:image and twitter:iamge
      colorMode: {
        respectPrefersColorScheme: true
      },
      announcementBar: {
        id: 'wip',
        content:
          'Please thank Jay on HN or lobsters for announcing this website, which is still work in progress! Thanks Jay! 😂',
        backgroundColor: '#fcb316',
        textColor: '#fafbfc',
        isCloseable: false,
      },
      navbar: {
        title: "Ada",
        logo: {
          alt: "Ada",
          src: "img/ada_saw_coin.svg",
          width: 32,
          height: 32
        },
        hideOnScroll: true,
        style: "dark",
        items: [
          {
            type: "doc",
            docId: "getting-started/why-ada",
            position: "left",
            label: "Getting Started"
          },
          {
            to: "/docs/arm/AA-Title",
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
                to: "/docs/getting-started/why-ada"
              },
              {
                label: "Books",
                to: "/docs/getting-started/books",
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
                to: "/docs/arm/AA-Title"
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
                label: "Gitter",
                href: "https://gitter.im/ada-lang/Lobby",
                className: "fa-icon footer__image footer-gitter-link"
              },
              {
                label: "Reddit",
                href: "https://www.reddit.com/r/ada/",
                className: "fa-icon footer__image footer-reddit-link"
              },
              {
                label: "Telegram",
                href: "https://t.me/ada_lang",
                className: "fa-icon footer__image footer-telegram-link"
              },
              {
                label: "Stack Overflow",
                href: "https://stackoverflow.com/questions/tagged/ada",
                className: "fa-icon footer__image footer-stackoverflow-link"
              }
            ]
          },
          {
            title: "More",
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
                href: "https://www.laeran.pl/adaplanet/i/"
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
          src: "img/ada_saw_coin.svg",
          width: 128,
          height: 128
        },
        copyright: `Copyright © ${new Date().getFullYear()} and licensed under Apache 2.0 unless otherwise noted, by the ${gitHubProjectName} developers. Built with Docusaurus.`
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        defaultLanguage: "ada",
        additionalLanguages: ["ada"]
      },
      algolia: {
        // The application ID provided by Algolia
        appId: 'L84ZBWQFIW',

        // Public API key: it is safe to commit it
        apiKey: '3ef66437cc70666c01852b9d2f4fde7b',

        indexName: 'ada-lang',

        // Optional: see doc section below
        contextualSearch: true,

        // Optional: Algolia search parameters
        // See https://www.algolia.com/doc/api-reference/search-api-parameters/
        searchParameters: {},

        // Optional: path for search page that enabled by default (`false` to disable it)
        searchPagePath: 'search',
      },
    }
}

module.exports = config
