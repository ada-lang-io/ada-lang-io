import React from "react"

import Link from "@docusaurus/Link"
import IconExternalLink from "@theme/Icon/ExternalLink"

import DownloadIcon from "@site/static/img/fontawesome/solid/download.svg"
import sparkGuidanceCover from "@site/static/img/implementation-guidance-spark-cover.png"

import { Button, Card, Container, Image, SimpleGrid, Text, ThemeIcon, Title } from "@mantine/core"

import clsx from "clsx"

import classes from "./index.module.scss"

function FeatureItem({ title, description, icon }) {
  return (
    <div className={classes.itemWrapper}>
      <ThemeIcon variant="light" className={classes.itemIcon} size={60} radius="md">
        {icon}
      </ThemeIcon>

      <div>
        <Text weight={700} size="lg" className={classes.itemTitle}>
          {title}
        </Text>
        <Text className={classes.itemDescription} color="dimmed">
          {description}
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

        <SimpleGrid cols={!!children ? 2 : 1} spacing={16}>
          {!!items && (
            <SimpleGrid
              cols={columns}
              spacing={48}
              breakpoints={[{ maxWidth: 550, cols: 1, spacing: 32 }]}
            >
              {items}
            </SimpleGrid>
          )}
          {children}
        </SimpleGrid>
      </Container>
    </section>
  )
}

export default function HomepageFeatures() {
  const itemsReadability = [
    <FeatureItem
      title="Separate specification and bodies"
      description="Types and functions, and their properties and documentation, are separated from their implementation. Control which parts are visible and to whom."
      icon={<DownloadIcon />}
    />,
    <FeatureItem
      title="Ranges and precision"
      description="Specify a valid range of a discrete type or the required precision for floating-point and fixed-point types."
      icon={<DownloadIcon />}
    />,
    <FeatureItem
      title="Predicates and contracts"
      description="Add predicates in the form of boolean expressions to your types and pre/post-conditions to your functions."
      icon={<DownloadIcon />}
    />,
    <FeatureItem
      title="Representation clauses"
      description="Separate the high-level specification of types and their properties from the representation at the bit level."
      icon={<DownloadIcon />}
    />
  ]

  const itemsCorrectness = [
    <FeatureItem title="Title 1" description="TODO." icon={<DownloadIcon />} />,
    <FeatureItem title="Title 2" description="TODO." icon={<DownloadIcon />} />,
    <FeatureItem title="Title 3" description="TODO." icon={<DownloadIcon />} />
  ]

  const itemsPerformance = [
    <FeatureItem title="Import from C/C++" description="TODO." icon={<DownloadIcon />} />,
    <FeatureItem title="Intrinsics" description="TODO." icon={<DownloadIcon />} />,
    <FeatureItem title="Assembly" description="TODO." icon={<DownloadIcon />} />
  ]

  const itemsSpark2014 = [
    <FeatureItem
      title="Stone - Valid SPARK"
      description="Restrict Ada packages to the SPARK subset. Avoids side-effects in functions and parameter aliasing."
      icon={<DownloadIcon />}
    />,
    <FeatureItem
      title="Bronze - Initialization and correct data flow"
      description="No uninitialized variables are read or undesired access to globals occurs."
      icon={<DownloadIcon />}
    />,
    <FeatureItem
      title="Silver - Absence of run-time errors"
      description="No buffer and arithmetic overflow, division by zero, or values out of range, among others, can occur."
      icon={<DownloadIcon />}
    />,
    //    <FeatureItem title="Gold - Key integrity properties" description="Proof maintaining type invariants and subprogram contracts." icon={<DownloadIcon />} />,
    <FeatureItem
      title="Gold - Key integrity properties"
      description="Verify integrity of data and valid state transitions."
      icon={<DownloadIcon />}
    />,
    <FeatureItem
      title="Platinum - Functional correctness"
      description="Full proof of functional correctness."
      icon={<DownloadIcon />}
    />
  ]

  return (
    <div className={classes.features}>
      <Feature
        title="Readability"
        subTitle="Express intent with explicitness and keywords over symbols and special structures"
        description="Express concepts like meaning in integers. Use built-in design by contract with pre/post-conditions and invariants. Model problems with typechecks and range constraints."
        items={itemsReadability}
      />
      <Feature
        title="Correctness"
        subTitle="Build with technology used in 40 years of reliability in planes, trains, and satellites"
        description="Use the SPARK subset to formally verify part or all of your program, and integrate existing SPARK crates available in the Alire package manager."
        items={itemsCorrectness}
      />
      <Feature
        title="Performance"
        subTitle="Build native applications and take advantage of other libraries through binding to C and C++"
        description="Use inline assembly or compiler intrinsics when you need it. Control resources with scope-based resource control (RAII) and your own memory allocators."
        items={itemsPerformance}
      />
      <Feature
        title="SPARK 2014"
        subTitle="From memory safety to functional correctness. One level at a time"
        description="Gradually adopt the SPARK subset to reach various levels of assurance. Higher levels take more effort, but give more benefits and stronger guarantees."
        items={itemsSpark2014}
        columns={1}
        className={classes.spark}
      >
        <Card withBorder radius="md" p="md" className={classes.cardSparkGuidance}>
          <Card.Section className={classes.imageSparkGuidance}>
            <Image
              src={sparkGuidanceCover}
              fit="contains"
              alt="Implementation Guidance for the Adoption of SPARK"
              caption="A booklet providing guidance on how to reach the desired levels."
            />
          </Card.Section>
          <Button
            radius="md"
            component={Link}
            href="https://www.adacore.com/books/implementation-guidance-spark"
            className={classes.linkSparkGuidance}
          >
            Visit page of booklet <IconExternalLink />
          </Button>
        </Card>
      </Feature>
    </div>
  )
}
