import React from "react"

import iconPerformance from "@site/static/img/chip-svgrepo-com.svg"
import iconCorrectness from "@site/static/img/plane-svgrepo-com.svg"
import iconReadability from "@site/static/img/student-reading-svgrepo-com.svg"

import { Container, SimpleGrid, Text, Title } from "@mantine/core"

import classes from "./index.module.scss"

function Feature({ title, subTitle, description, items }) {
  return (
    <section className={classes.sectionWrapper}>
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

        {!!items && (
          <SimpleGrid
            cols={2}
            spacing={50}
            breakpoints={[{ maxWidth: 550, cols: 1, spacing: 40 }]}
            style={{ marginTop: 30 }}
          >
            {items}
          </SimpleGrid>
        )}
      </Container>
    </section>
  )
}

export default function HomepageFeatures() {
  return (
    <div className={classes.features}>
      <Feature
        title="Readability"
        subTitle="Express intent with explicitness and keywords over symbols and special structures"
        description="Express concepts like meaning in integers. Use built-in design by contract with pre/post-conditions and invariants. Model problems with typechecks and range constraints."
        item={undefined}
      />
      <Feature
        title="Correctness"
        subTitle="Build with technology used in 40 years of reliability in planes, trains, and spaceships"
        description="Use the SPARK subset to formally verify part or all of your program, and integrate existing SPARK crates available in the Alire package manager."
        item={undefined}
      />
      <Feature
        title="Performance"
        subTitle="Build native applications and take advantage of other libraries through binding to C and C++"
        description="Use inline assembly or compiler intrinsics when you need it. Control resources with scope-based resource control (RAII) and your own memory allocators."
        item={undefined}
      />
    </div>
  )
}
