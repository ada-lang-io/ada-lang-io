// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Ada',
  tagline: 'Readable, correct, performance',
  url: 'https://ada-lang.io',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'pyjarrett', // Usually your GitHub org/user name.
  projectName: 'ada-lang-io', // Usually your repo name.

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
        },
        blog: {
          showReadingTime: true,
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'Ada lang dev',
        logo: {
          alt: 'Ada lang dev',
          src: 'img/ada_saw_coin.svg',
        },
        items: [
          {
            type: 'doc',
            docId: 'intro',
            position: 'left',
            label: 'Tutorial',
          },
          {to: '/blog', label: 'Blog', position: 'left'},
          {
            href: 'https://ada-lang.io/',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Learning',
            items: [
              {
                label: 'learn.adacore.com',
                to: 'https://learn.adacore.com',
              },
              {
                label: 'Learn Spark',
                to: 'https://learn.adacore.com/courses/intro-to-spark/index.html'
              }
            ]
          },
          {
            title: 'Docs',
            items: [
              {
                label: 'Tutorial',
                to: '/docs/intro',
              },
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Stack Overflow',
                href: 'https://stackoverflow.com/questions/tagged/ada',
              },
              {
                label: 'Twitter',
                href: 'https://twitter.com/adaprogrammers',
              },
              {
                label: 'Gitter',
                href: 'https://gitter.im/ada-lang/Lobby',
              }
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Blog',
                to: '/blog',
              },
              {
                label: 'GitHub',
                href: 'https://github.com/pyjarrett/adalang-io-prototype',
              },
              {
                label: 'News (from Ada Planet)',
                href: 'https://www.laeran.pl/adaplanet/i/',
              }
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Ada lang dev, Inc. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ['ada'],
      },
    }),
};

module.exports = config;
