import React from "react"

import iconPerformance from "@site/static/img/chip-svgrepo-com.svg"
import iconCorrectness from "@site/static/img/plane-svgrepo-com.svg"
import iconReadability from "@site/static/img/student-reading-svgrepo-com.svg"

import clsx from "clsx"

import styles from "./styles.module.css"

function Feature({ icon, title, children }) {
  const Svg = icon

  return (
    <div className={clsx("col col--4")}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className={clsx("padding-horiz--md", styles.featureText)}>
        <h3>{title}</h3>
        {children}
      </div>
    </div>
  )
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          <Feature icon={iconReadability} title="Readability">
            <p>
              Express intent with explicitness and keywords over symbols and special structures.
            </p>
            <p>
              Express concepts like meaning in integers. Use built-in design by contract with
              pre/post-conditions and invariants. Model problems with typechecks and range
              constraints.
            </p>
          </Feature>
          <Feature icon={iconCorrectness} title="Correctness">
            <p>
              Build with technology used in 40 years of reliability in planes, trains, and
              spaceships.
            </p>
            <p>
              Use the SPARK subset to formally verify part or all of your program, and integrate
              existing SPARK crates available in the Alire package manager.
            </p>
          </Feature>
          <Feature icon={iconPerformance} title="Performance">
            <p>
              Build native applications and take advantage of other libraries through binding to C
              and C++.
            </p>
            <p>
              Use inline assembly or compiler intrinsics when you need it. Control resources with
              scope-based resource control (RAII) and your own memory allocators.
            </p>
          </Feature>
        </div>
      </div>
    </section>
  )
}
