import React from "react"

import Link from "@docusaurus/Link"
import useDocusaurusContext from "@docusaurus/useDocusaurusContext"
import useIsBrowser from "@docusaurus/useIsBrowser"
import CodeBlock from "@theme/CodeBlock"
import Layout from "@theme/Layout"
import TabItem from "@theme/TabItem"
import Tabs from "@theme/Tabs"

import HomepageFeatures from "@site/src/components/HomepageFeatures"

import sampleA from "!!raw-loader!./code-basic.adb"
import sampleC from "!!raw-loader!./code-rp2040.adb"
import sampleB from "!!raw-loader!./code-spark.ads"
import clsx from "clsx"

import styles from "./index.module.css"

// sampleB from src/xoshiro128.ads in https://github.com/onox/xoshiro
//
// Copyright (c) 2022 onox
// SPDX-License-Identifier: Apache-2.0

// sampleC from ravenscar_blink/src/main.adb in https://github.com/JeremyGrosser/pico_examples
//
// Copyright (c) 2021 Jeremy Grosser
// SPDX-License-Identifier: BSD-3-Clause

const targets = [
  ["Win", "windows"],
  ["Linux", "linux"],
  ["Mac", "macos"]
]

function getPlatformKey(platform) {
  for (const [prefix, key] of targets) {
    if (platform.startsWith(prefix)) {
      return key
    }
  }

  return null
}

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

function HomepageHeader() {
  const isBrowser = useIsBrowser()
  const { siteConfig } = useDocusaurusContext()

  const platformKey = isBrowser
    ? getPlatformKey(navigator.userAgentData?.platform || navigator.platform)
    : null
  const platform = platformKey !== null ? installTargets.get(platformKey) : null

  const platformLabel = platform !== null ? ` for ${platform.label}` : ""

  const platformDownloadURL =
    platform !== null ? getInstallTarget(alireVersion, platform.urlSuffix) : gitHubReleasePage

  const allPlatformLinks = Array.from(installTargets.values()).map(({ label, urlSuffix }) => (
    <Link className={styles.heroLink} to={getInstallTarget(alireVersion, urlSuffix)}>
      {label}
    </Link>
  ))

  const linkOthers = (
    <Link className={styles.heroLink} to={gitHubReleasePage}>
      others
    </Link>
  )

  return (
    <header className={clsx("hero hero--primary", styles.heroBanner)}>
      <div className={clsx("container", styles.columns)}>
        <div className="container">
          <h1 className="hero__title">{siteConfig.title}</h1>
          <p className="hero__subtitle">{siteConfig.tagline}</p>
          <p>Get started with Alire, the Ada package manager</p>
          <div className={styles.buttons}>
            <Link className="button button--secondary button--lg" to={platformDownloadURL}>
              Download Alire {alireVersion.slice(0)} {platformLabel}
            </Link>
          </div>
          <div className="container">
            <small>
              Download for {join(allPlatformLinks, <span>, </span>)}, or {linkOthers}
            </small>
          </div>
        </div>
        <div className={clsx("container", styles.heroTabs)}>
          <Tabs>
            <TabItem value="basic" label="Basic">
              <CodeBlock showLineNumbers>{sampleA}</CodeBlock>
            </TabItem>
            <TabItem value="spark" label="SPARK">
              <CodeBlock showLineNumbers>{sampleB}</CodeBlock>
            </TabItem>
            <TabItem value="embedded" label="Embedded">
              <CodeBlock showLineNumbers>{sampleC}</CodeBlock>
            </TabItem>
          </Tabs>
        </div>
      </div>
    </header>
  )
}

function Home() {
  const { siteConfig } = useDocusaurusContext()

  return (
    <Layout title={siteConfig.title} description="Ada Programming Language">
      <HomepageHeader />
      <main>
        <HomepageFeatures />
      </main>
    </Layout>
  )
}

export default Home
