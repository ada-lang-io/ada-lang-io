import React, { useEffect, useState } from "react"

import Link from "@docusaurus/Link"
import useDocusaurusContext from "@docusaurus/useDocusaurusContext"
import Layout from "@theme/Layout"

import HomepageFeatures from "@site/src/components/HomepageFeatures"

import clsx from "clsx"

import styles from "./index.module.css"

function platformTarget(platform) {
  if (platform.indexOf("Win") === 0) {
    return "Windows"
  }

  if (platform.indexOf("Linux") === 0) {
    return "Linux"
  }

  if (platform.indexOf("Mac") === 0) {
    return "Mac"
  }

  return "Unknown"
}

// TODO: This version number should come from a "latest Alire" note somewhere.
const currentAlireVersion = "1.2.1"
const alireReleaseDir =
  "https://github.com/alire-project/alire/releases/download/v" + currentAlireVersion + "/"
const installTargets = new Map([
  ["Windows", alireReleaseDir + "alr-" + currentAlireVersion + "-installer-x86_64-windows.exe"],
  ["Mac", alireReleaseDir + "alr-" + currentAlireVersion + "-bin-x86_64-macos.zip"],
  ["Linux", alireReleaseDir + "alr-" + currentAlireVersion + "-bin-x86_64-linux.zip"],
  ["Unknown", "https://github.com/alire-project/alire/releases"]
])

function HomepageHeader() {
  const [platform, setPlatform] = useState("unknown")

  useEffect(() => {
    // Not sure of the right way to to this.
    setPlatform(navigator?.userAgent?.platform || navigator?.platform || "unknown")
  }, [])

  const { siteConfig } = useDocusaurusContext()
  return (
    <header className={clsx("hero hero--primary", styles.heroBanner)}>
      <div className="container">
        <h1 className="hero__title">{siteConfig.title}</h1>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <p>Get started with Alire, the Ada package manager</p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to={installTargets.get(platformTarget(platform))}
          >
            Get Alire{" "}
            {platformTarget(platform) !== "Unknown" ? " for " + platformTarget(platform) : ""}
          </Link>
        </div>
        <div className="container">
          <Link className="button button--primary" to="https://alire.ada.dev/docs/#installation">
            Alire Installation Help
          </Link>
        </div>
      </div>
    </header>
  )
}

export default function Home() {
  const { siteConfig } = useDocusaurusContext()
  return (
    <Layout title={`${siteConfig.title}`} description="Ada Programming Language">
      <HomepageHeader />
      <main>
        <HomepageFeatures />
      </main>
    </Layout>
  )
}
