import React from "react"

import Link from "@docusaurus/Link"
import useDocusaurusContext from "@docusaurus/useDocusaurusContext"
import useIsBrowser from "@docusaurus/useIsBrowser"
import Layout from "@theme/Layout"

import HomepageFeatures from "@site/src/components/HomepageFeatures"
import DownloadIcon from "@site/static/img/fontawesome/solid/download.svg"

import {
  Button,
  Container,
  Group,
  MantineProvider,
  Overlay,
  Tabs,
  Text,
  Title
} from "@mantine/core"
import { useOs } from "@mantine/hooks"
import { Prism } from "@mantine/prism"

import styles from "./index.module.scss"

import sampleA from "!!raw-loader!./code/code-basic.adb"
import sampleC from "!!raw-loader!./code/code-rp2040.adb"
import sampleB from "!!raw-loader!./code/code-spark.ads"

const samples = [
  { key: "ada", code: sampleA },
  { key: "spark", code: sampleB },
  { key: "embedded", code: sampleC }
]

// sampleB from src/xoshiro128.ads in https://github.com/onox/xoshiro
//
// Copyright (c) 2022 onox
// SPDX-License-Identifier: Apache-2.0

// sampleC from ravenscar_blink/src/main.adb in https://github.com/JeremyGrosser/pico_examples
//
// Copyright (c) 2021 Jeremy Grosser
// SPDX-License-Identifier: BSD-3-Clause

const installTargets = new Map([
  ["windows", { label: "Windows", urlSuffix: "installer-x86_64-windows.exe" }],
  ["macos", { label: "macOS", urlSuffix: "bin-x86_64-macos.zip" }],
  ["linux", { label: "Linux", urlSuffix: "bin-x86_64-linux.zip" }],
  ["appimage", { label: "AppImage", urlSuffix: "x86_64.AppImage" }]
])

const join = (values, sep) =>
  values.reduce((a, b) => (a.length > 0 ? a.concat([sep, b]) : [b]), [])

// TODO: This version number should come from a "latest Alire" note somewhere.
// Version number and assets could be fetched from
// https://api.github.com/repos/alire-project/alire/releases/latest
const alireVersion = "1.2.1"

const gitHubProjectPage = "https://github.com/alire-project/alire"
const gitHubReleasePage = `${gitHubProjectPage}/releases`

function getInstallTarget(version, suffix) {
  return `${gitHubProjectPage}/releases/download/v${version}/alr-${version}-${suffix}`
}

function CodeBlock({ showLineNumbers, children }) {
  return (
    <Prism withLineNumbers={showLineNumbers} language="ada">
      {children}
    </Prism>
  )
}

export function HomepageHeader({ title, description }) {
  const isBrowser = useIsBrowser()
  const os = useOs()

  const platformKey = isBrowser ? os : null

  const platform = platformKey !== null ? installTargets.get(platformKey) : null
  const platformLabel = platform !== null ? ` for ${platform.label}` : ""

  const platformDownloadURL =
    platform !== null ? getInstallTarget(alireVersion, platform.urlSuffix) : gitHubReleasePage

  const allPlatformLinks = Array.from(installTargets.values()).map(({ label, urlSuffix }) => (
    <Link to={getInstallTarget(alireVersion, urlSuffix)}>{label}</Link>
  ))
  const platformLinks = join(allPlatformLinks, ", ")

  const linkOthers = <Link to={gitHubReleasePage}>others</Link>

  return (
    <header className={styles.heroWrapper}>
      <Overlay color="#000" opacity={0.65} zIndex={1} />

      <div className={styles.heroInner}>
        <div className={styles.columns}>
          <div>
            <Title className={styles.title}>{title}</Title>

            <Container size={640}>
              <Text size="lg" className={styles.description}>
                {description}
              </Text>
              <Text size="lg" className={styles.description}>
                Get started with Alire, the Ada package manager.
              </Text>
            </Container>

            <Group position="center" className={styles.controls}>
              <Button
                className={styles.controlPrimary}
                variant="white"
                size="md"
                component={Link}
                to={platformDownloadURL}
                variant="gradient"
                //gradient={{ from: 'green', to: 'teal' }}
                gradient={{ from: "blue", to: "cyan" }}
                leftIcon={<DownloadIcon className={styles.downloadIcon} />}
              >
                Download Alire {alireVersion.slice(0)} {platformLabel}
              </Button>
              <Button
                className={styles.controlSecondary}
                size="md"
                component={Link}
                to="/docs/getting-started/why-ada"
              >
                Get Started
              </Button>
            </Group>

            <Text size="xs" className={styles.textDownloadLinks}>
              Download for{" "}
              {platformLinks.map((item, i) => (
                <span key={i}>{item}</span>
              ))}
              , or {linkOthers}
            </Text>
          </div>
          <div>
            <div className={styles.heroTabs}>
              <Tabs color="blue" variant="pills" defaultValue="ada">
                <Tabs.List>
                  <Tabs.Tab value="ada">Ada</Tabs.Tab>
                  <Tabs.Tab value="spark">SPARK</Tabs.Tab>
                  <Tabs.Tab value="embedded">Embedded</Tabs.Tab>
                </Tabs.List>

                <div className={styles.codeTabPanel}>
                  {samples.map(({ key, code }) => (
                    <Tabs.Panel key={key} value={key} pt="xs">
                      <CodeBlock showLineNumbers={true}>{code}</CodeBlock>
                    </Tabs.Panel>
                  ))}
                </div>
              </Tabs>
            </div>
          </div>
        </div>
      </div>
    </header>
  )
}

export default function Home() {
  const { siteConfig } = useDocusaurusContext()

  return (
    <MantineProvider
      theme={{
        colorScheme: "dark",
        fontFamily: "var(--ada-lang-font-family)"
      }}
    >
      <Layout title={siteConfig.title} description={siteConfig.customFields.description}>
        <HomepageHeader
          title={siteConfig.title}
          description={siteConfig.customFields.description}
        />
        <main>
          <HomepageFeatures />
        </main>
      </Layout>
    </MantineProvider>
  )
}
