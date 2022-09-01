// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github")
const darkCodeTheme = require("prism-react-renderer/themes/okaidia")

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
  organizationName: "ada-lang-io", // Usually your GitHub org/user name.
  projectName: "ada-lang-io", // Usually your repo name.

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: "en",
    locales: ["en"]
  },

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve("./sidebars.js")
        },
        blog: {
          showReadingTime: true
        },
        theme: {
          customCss: require.resolve("./src/css/custom.css")
        }
      })
    ]
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    {
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
            docId: "why-ada",
            position: "left",
            label: "Docs"
          },
          {
            to: "/blog",
            label: "Blog",
            position: "left"
          },
          {
            href: "https://github.com/ada-lang-io/ada-lang-io",
            label: "GitHub",
            position: "right"
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
                to: "/docs/why-ada"
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
                label: "Wikibook",
                href: "https://en.wikibooks.org/wiki/Ada_Programming"
              },
              {
                label: "Style Guide",
                href: "https://en.wikibooks.org/wiki/Ada_Style_Guide"
              },
              {
                label: "Ada 2012 Reference Manual",
                href: "http://ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html"
              }
            ]
          },
          {
            title: "Community",
            items: [
              {
                label: "Gitter",
                href: "https://gitter.im/ada-lang/Lobby"
              },
              {
                label: "Reddit",
                href: "https://www.reddit.com/r/ada/"
              },
              {
                label: "Telegram",
                href: "https://t.me/ada_lang"
              },
              {
                label: "Stack Overflow",
                href: "https://stackoverflow.com/questions/tagged/ada"
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
                href: "https://github.com/ada-lang-io/ada-lang-io"
              }
            ]
          }
        ],
        copyright: `Copyright © ${new Date().getFullYear()} ada-lang-io developers. Built with Docusaurus.`
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        defaultLanguage: "ada",
        additionalLanguages: ["ada"]
      }
    }
}

module.exports = config
