import React from "react"
//import {
//  BsBookHalf,
//  BsFillPatchCheckFill,
//  BsSkipForwardFill,
//  BsSpeedometer2
//} from "react-icons/bs"
//import { FaBookReader, FaFastForward, FaGraduationCap } from "react-icons/fa"
import {
  MdAutoStories,
  MdLooks3,
  MdLooks4,
  MdLooks5,
  MdLooksOne,
  MdLooksTwo,
  MdSpeed,
  MdVerified
} from "react-icons/md"

import Link from "@docusaurus/Link"
import IconExternalLink from "@theme/Icon/ExternalLink"

import InfoHover from "@site/src/components/InfoHover"
import DownloadIcon from "@site/static/img/fontawesome/solid/download.svg"
import sparkGuidanceCover from "@site/static/img/implementation-guidance-spark-cover.png"

import { Button, Card, Container, Image, SimpleGrid, Text, ThemeIcon, Title } from "@mantine/core"

import clsx from "clsx"

import classes from "./index.module.scss"

import features from "./features.json"

function SparkGuidanceBooklet() {
  return (
    <Card withBorder shadow="sm" radius="md" p="md" className={classes.cardSparkGuidance}>
      <Card.Section className={classes.imageSparkGuidance}>
        <Image
          src={sparkGuidanceCover}
          fit="contain"
          alt={"Implementation Guidance for the Adoption of SPARK"}
          caption={
            <div style={{ display: "flex" }}>
              <span>{"A booklet providing guidance on how to reach the desired levels."}</span>
              <InfoHover
                text={[
                  "Image from booklet.",
                  "Copyright (C) 2016-2020, AdaCore and Thales",
                  "Licensed under Creative Commons Attribution 4.0 International"
                ]}
                width={200}
              />
            </div>
          }
        />
      </Card.Section>
      <Button
        radius="md"
        component={Link}
        href={"https://www.adacore.com/books/implementation-guidance-spark"}
        className={classes.linkSparkGuidance}
      >
        Visit page of booklet <IconExternalLink />
      </Button>
    </Card>
  )
}

features[1].className = classes.spark
features[1].children = <SparkGuidanceBooklet />

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
            ? description.map((paragraph) => <p>{paragraph}</p>)
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

        <SimpleGrid cols={!!children ? 2 : 1} spacing="md">
          {!!items && (
            <SimpleGrid
              cols={columns}
              spacing={48}
              breakpoints={[{ maxWidth: 550, cols: 1, spacing: 32 }]}
            >
              {items.map((item) => (
                <FeatureItem {...item} />
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
      {features.map((feature) => (
        <Feature {...feature} />
      ))}
    </div>
  )
}
