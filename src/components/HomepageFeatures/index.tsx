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

// See https://react-icons.github.io/react-icons/ for all icons
const icons: Record<string, JSX.Element> = {
  "feat-readable": <MdAutoStories />,
  "feat-correct": <MdVerified />,
  "feat-performant": <MdSpeed />,
  "spark-stone": <MdLooksOne />,
  "spark-bronze": <MdLooksTwo />,
  "spark-silver": <MdLooks3 />,
  "spark-gold": <MdLooks4 />,
  "spark-platinum": <MdLooks5 />
}

interface FeatureItemProps {
  readonly title: string
  readonly description: string | string[]
  readonly icon: string
}

function FeatureItem({ title, description, icon }: FeatureItemProps): JSX.Element {
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
            ? description.map((paragraph: string, i: number) => <p key={i}>{paragraph}</p>)
            : description}
        </Text>
      </div>
    </div>
  )
}

interface FeatureProps {
  readonly title: string
  readonly subTitle: string
  readonly description: string
  readonly items?: FeatureItemProps[]
  readonly columns?: number
  className?: string
  children?: JSX.Element
}

function Feature({
  title,
  subTitle,
  description,
  items,
  className,
  children,
  columns = 2
}: FeatureProps): JSX.Element {
  return (
    <section className={clsx(classes.sectionWrapper, { [className as string]: !!className })}>
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
              {items.map((item: FeatureItemProps, i: number) => (
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

export default function HomepageFeatures(): JSX.Element {
  const feats: FeatureProps[] = features

  // TODO Getting these indices right is error-prone
  feats[1].children = <AlireInstallInstructions />
  feats[2].className = classes.spark

  return (
    <div className={classes.features}>
      {feats.map((feature: FeatureProps, i: number) => (
        <Feature key={i} {...feature} />
      ))}
    </div>
  )
}
