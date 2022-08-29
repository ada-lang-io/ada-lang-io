import React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import HomepageFeatures from '@site/src/components/HomepageFeatures';

import styles from './index.module.css';

// Not sure of the right way to to this.
const platform = navigator?.userAgent?.platform || navigator?.platform || 'unknown';

// TODO: This version number should come from a "latest Alire" note somewhere.
const currentAlireVersion = '1.2.1'
const alireReleaseDir = 'https://github.com/alire-project/alire/releases/download/v' + currentAlireVersion + '/';
const installTargets = new Map([
  ['Windows', alireReleaseDir + 'alr-' + currentAlireVersion + '-installer-x86_64-windows.exe'],
  ['Mac', alireReleaseDir + 'alr-' + currentAlireVersion + '-bin-x86_64-macos.zip'],
  ['Linux', alireReleaseDir + 'alr-' + currentAlireVersion + '-bin-x86_64-linux.zip'],
  ['Unknown', 'https://github.com/alire-project/alire/releases'],
]);

function platformTarget() {
  if (platform.indexOf('Win') === 0) {
    return 'Windows';
  }

  if (platform.indexOf('Linux') === 0) {
    return 'Linux';
  }

  if (platform.indexOf('Mac') === 0) {
    return 'Mac';
  }

  return 'Unknown';
}

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <h1 className="hero__title">{siteConfig.title}</h1>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <p>Get started with Alire, the Ada package manager</p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to={installTargets.get(platformTarget())}>
              Get Alire {platformTarget() !== 'Unknown' ? ' for ' + platformTarget() : ''}
          </Link>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title={`${siteConfig.title}`}
      description="Ada Programming Language">
      <HomepageHeader />
      <main>
        <HomepageFeatures />
      </main>
    </Layout>
  );
}
