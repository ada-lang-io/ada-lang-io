import React, { useState } from "react"
import { FaTerminal } from "react-icons/fa"
import {
  MdAutoStories,
  MdCode,
  MdDone,
  MdFileDownload,
  MdLooks3,
  MdLooks4,
  MdLooks5,
  MdLooksOne,
  MdLooksTwo,
  MdSpeed,
  MdVerified
} from "react-icons/md"

import Link from "@docusaurus/Link"
import useIsBrowser from "@docusaurus/useIsBrowser"

import { Container, SimpleGrid, Text, ThemeIcon, Timeline, Title } from "@mantine/core"
import { useOs } from "@mantine/hooks"
import { Prism } from "@mantine/prism"

import clsx from "clsx"

import classes from "./index.module.scss"

import codeAlrInit from "!!raw-loader!./code/alr-init.sh"

import {
  alireVersion,
  getInstallTarget,
  gitHubReleasePage,
  installTargets
} from "../../pages/index.js"
import features from "./features.json"

function TimelineItemText({ children }) {
  return (
    <Text size="sm" className={classes.timelineItemTitle}>
      {children}
    </Text>
  )
}

function AlireInstallInstructions() {
  const isBrowser = useIsBrowser()
  const os = useOs()

  const platformKey = isBrowser && installTargets.has(os) ? os : null

  const platform = platformKey !== null ? installTargets.get(platformKey) : null
  const platformLabel = platform !== null ? ` for ${platform.label}` : ""

  const platformDownloadURL =
    platform !== null ? getInstallTarget(alireVersion, platform.urlSuffix) : gitHubReleasePage

  const [step, setStep] = useState(0)

  const onClickDownloadLink = () => {
    setStep(1)
  }

  const otherDownloadText = (
    <span>
      {" "}
      or view other options on the{" "}
      <Link onClick={onClickDownloadLink} to={gitHubReleasePage}>
        release page
      </Link>
    </span>
  )

  return (
    <Timeline active={step} bulletSize={32} lineWidth={3} className={classes.timeline}>
      <Timeline.Item
        bullet={<MdFileDownload size={16} />}
        title={<TimelineItemText>Download Alire</TimelineItemText>}
      >
        <Text color="dimmed">
          Download{" "}
          <Link onClick={onClickDownloadLink} to={platformDownloadURL}>
            Alire {alireVersion.slice(0)}
            {platformLabel}
          </Link>
          {platform !== null && otherDownloadText}.
        </Text>
      </Timeline.Item>

      <Timeline.Item
        bullet={<FaTerminal size={12} />}
        title={<TimelineItemText>Install toolchain</TimelineItemText>}
      >
        <Prism language="shell">alr toolchain --select</Prism>
        <Text color="dimmed">Select gnat_native and gprbuild.</Text>
      </Timeline.Item>

      <Timeline.Item
        bullet={<MdCode size={16} />}
        title={<TimelineItemText>Start coding</TimelineItemText>}
      >
        <Prism language="shell">{codeAlrInit}</Prism>
      </Timeline.Item>

      <Timeline.Item
        bullet={<MdDone size={16} />}
        title={<TimelineItemText>Run your application</TimelineItemText>}
      >
        <Prism language="shell">alr run</Prism>
      </Timeline.Item>
    </Timeline>
  )
}

// TODO Getting these indices right is error-prone
features[1].children = <AlireInstallInstructions />
features[2].className = classes.spark

// See https://react-icons.github.io/react-icons/ for all icons
const icons = {
  "feat-readable": <MdAutoStories />,
  "feat-correct": <MdVerified />,
  "feat-performant": <MdSpeed />,
  "spark-stone": <MdLooksOne />,
  "spark-bronze": <MdLooksTwo />,
  "spark-silver": <MdLooks3 />,
  "spark-gold": <MdLooks4 />,
  "spark-platinum": <MdLooks5 />
}

function FeatureItem({ title, description, icon }) {
  return (
    <div className={classes.itemWrapper}>
      <ThemeIcon variant="light" className={classes.itemIcon} size={60} radius="md">
        {icons[icon]}
      </ThemeIcon>

      <div>
        <Text weight={700} size="lg" className={classes.itemTitle}>
          {title}
        </Text>
        <Text className={classes.itemDescription} color="dimmed">
          {Array.isArray(description)
            ? description.map((paragraph, i) => <p key={i}>{paragraph}</p>)
            : description}
        </Text>
      </div>
    </div>
  )
}

function Feature({ title, subTitle, description, items, className, children, columns = 2 }) {
  return (
    <section className={clsx(classes.sectionWrapper, { [className]: !!className })}>
      <Container size={700}>
        <Text className={classes.title}>{title}</Text>
        <Title className={classes.subTitle} order={2}>
          {subTitle}
        </Title>

        <Container size={660} p={0}>
          <Text color="dimmed" className={classes.description}>
            {description}
          </Text>
        </Container>

        <SimpleGrid cols={Number(!!items) + Number(!!children)} spacing="md">
          {!!items && (
            <SimpleGrid
              cols={columns}
              spacing={48}
              breakpoints={[{ maxWidth: 550, cols: 1, spacing: 32 }]}
            >
              {items.map((item, i) => (
                <FeatureItem key={i} {...item} />
              ))}
            </SimpleGrid>
          )}
          {children}
        </SimpleGrid>
      </Container>
    </section>
  )
}

export default function HomepageFeatures() {
  return (
    <div className={classes.features}>
      {features.map((feature, index) => (
        <Feature key={index} {...feature} />
      ))}
    </div>
  )
}
