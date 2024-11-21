import React from "react"

import { FaDownload as DownloadIcon } from "react-icons/fa"

import Link from "@docusaurus/Link"
import type { DocusaurusContext } from "@docusaurus/types"
import useDocusaurusContext from "@docusaurus/useDocusaurusContext"
import { usePluginData } from "@docusaurus/useGlobalData"
import useIsBrowser from "@docusaurus/useIsBrowser"
import CodeBlock from "@theme/CodeBlock"
import Layout from "@theme/Layout"

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
import type { OS } from "@mantine/hooks"

import HomepageFeatures from "@site/src/components/HomepageFeatures"

import type { Language } from "prism-react-renderer"

import styles from "./index.module.scss"

import sampleA from "!!raw-loader!./code/code-basic.adb"
import sampleC from "!!raw-loader!./code/code-rp2040.adb"
import sampleB from "!!raw-loader!./code/code-spark.ads"

type Sample = {
  key: string
  code: string
}

const samples: Sample[] = [
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

export type Target = {
  label: string
  urlSuffix: string
}

export const installTargets: Map<string, Target> = new Map([
  ["windows", { label: "Windows", urlSuffix: "installer-x86_64-windows.exe" }],
  ["macos", { label: "macOS", urlSuffix: "bin-aarch64-macos.zip" }],
  ["linux", { label: "Linux", urlSuffix: "bin-x86_64-linux.zip" }],
  ["appimage", { label: "AppImage", urlSuffix: "x86_64.AppImage" }]
])

function join<Type>(values: Type[], sep: string): (Type | string)[] {
  const concat = (a: (Type | string)[], b: Type) => (a.length > 0 ? a.concat([sep, b]) : [b])
  return values.reduce(concat, [])
}

export const gitHubProjectPage = "https://github.com/alire-project/alire"
export const gitHubReleasePage = `${gitHubProjectPage}/releases`

export function getInstallTarget(version: string, suffix: string): string {
  return `${gitHubProjectPage}/releases/download/${version}/alr-${version.slice(1)}-${suffix}`
}

interface HomepageHeaderProps {
  readonly title: string
  readonly description: string
}

export function HomepageHeader({ title, description }: HomepageHeaderProps): JSX.Element {
  const isBrowser: boolean = useIsBrowser()
  const os: OS = useOs()
  const { alireVersion } = usePluginData("ada-lang-alire-version") as PluginDataAlireVersion

  const platformKey: OS | null = isBrowser && installTargets.has(os) ? os : null

  const platform: Target | null =
    platformKey !== null ? (installTargets.get(platformKey) as Target) : null
  const platformLabel: string = platform !== null ? ` for ${platform.label}` : ""

  const platformDownloadURL: string =
    platform !== null ? getInstallTarget(alireVersion, platform.urlSuffix) : gitHubReleasePage

  const allPlatformLinks = Array.from(installTargets.values()).map(({ label, urlSuffix }, i) => (
    <Link key={i} to={getInstallTarget(alireVersion, urlSuffix)}>
      {label}
    </Link>
  ))
  const platformLinks = join(allPlatformLinks, ", ") as JSX.Element[]

  const linkOthers: JSX.Element = <Link to={gitHubReleasePage}>others</Link>

  // Photos from Unsplash (licensed under Unsplash License (https://unsplash.com/license))
  // - https://unsplash.com/photos/Q1p7bh3SHj8
  // - https://unsplash.com/photos/gYwfpVI2JzM
  // Video of GESTE from https://github.com/Fabien-Chouteau/GESTE (CC-BY 4.0 with permission)
  return (
    <header className={styles.heroWrapper} data-animate="true">
      <div className={styles.heroBackground}>
        <picture className={styles.background1}>
          <source srcSet="/img/unsplash/yZygONrUBe8.avif" type="image/avif" />
          <source srcSet="/img/unsplash/yZygONrUBe8.webp" type="image/webp" />
          <img src="/img/unsplash/yZygONrUBe8.jpg" alt="Background image of Earth and satellite" />
        </picture>
        <div className={styles.background2}>
          <video autoPlay loop muted playsInline>
            <source src="/img/geste-av1.webm" type="video/webm; codecs=av1" />
            <source src="/img/geste-vp9.webm" type="video/webm; codecs=vp9" />
          </video>
        </div>
        <picture className={styles.background3}>
          <source srcSet="/img/unsplash/gYwfpVI2JzM.avif" type="image/avif" />
          <source srcSet="/img/unsplash/gYwfpVI2JzM.webp" type="image/webp" />
          <img src="/img/unsplash/gYwfpVI2JzM.jpg" alt="Background image of Earth and astronaut" />
        </picture>
      </div>
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
                size="md"
                component={Link}
                to={platformDownloadURL}
                variant="gradient"
                //gradient={{ from: 'green', to: 'teal' }}
                gradient={{ from: "blue", to: "cyan" }}
                leftIcon={<DownloadIcon className={styles.downloadIcon} />}
              >
                Download Alire {alireVersion}
                {platformLabel}
              </Button>
              <Button
                className={styles.controlSecondary}
                size="md"
                component={Link}
                to="/docs/learn/getting-started/installation"
              >
                Get Started
              </Button>
            </Group>

            <Text size="xs" className={styles.textDownloadLinks}>
              Download for{" "}
              {platformLinks.map((item: JSX.Element, i: number) => (
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

                <div className={styles.codeTabPanels}>
                  {samples.map(({ key, code }) => (
                    <Tabs.Panel key={key} value={key} pt="xs" className={styles.codeTabPanel}>
                      <CodeBlock
                        showLineNumbers={true}
                        language="ada"
                        className={styles.codeBlockDocusaurus}
                      >
                        {code}
                      </CodeBlock>
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

export default function Home(): JSX.Element {
  const { siteConfig }: DocusaurusContext = useDocusaurusContext()

  const description: string = siteConfig.customFields?.description as string

  return (
    <MantineProvider
      theme={{
        colorScheme: "dark",
        fontFamily: "var(--ada-lang-font-family)"
      }}
    >
      <Layout title={siteConfig.title} description={description}>
        <HomepageHeader title={siteConfig.title} description={description} />
        <main>
          <HomepageFeatures />
        </main>
      </Layout>
    </MantineProvider>
  )
}
