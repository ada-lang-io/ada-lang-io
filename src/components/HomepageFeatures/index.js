import React from "react"

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

import { Container, SimpleGrid, Text, ThemeIcon, Title } from "@mantine/core"

import AlireInstallInstructions from "@site/src/components/AlireInstallInstructions"

import clsx from "clsx"

import classes from "./index.module.scss"

import features from "./features.json"

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
              breakpoints={[
                { maxWidth: 475, cols: 1, spacing: 16 },
                { maxWidth: 768, cols: 2, spacing: 24 },
                { maxWidth: 996, cols: Math.min(3, columns), spacing: 32 }
              ]}
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
