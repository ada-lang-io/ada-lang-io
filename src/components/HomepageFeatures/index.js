import React from "react"

import CodeBlock from "@theme/CodeBlock"

import clsx from "clsx"

import styles from "./styles.module.css"

const readabilitySample = `type Console_Input_Mode is
   array (Console_Input_Flags)
      of Boolean
         with Pack,
            Size => 32;`

const correctnessSample = `type Line is private
   with Type_Invariant =>
      Get_Cursor_Index (Line)
         in 1 .. Length (Line) + 1;`

const performanceSample = `function tcgetattr (
   File_Descriptor : FD;
   Terminal : System.Address)
return BOOL
   with Import => True,
        Convention => C;`

const FeatureList = [
  {
    title: "Readability",
    Svg: require("@site/static/img/student-reading-svgrepo-com.svg").default,
    description: (
      <>Express intent with explicitness and keywords over symbols and special structures.</>
    ),
    extended_description: (
      <>
        Express concepts like meaning in integers. Use built-in design by contract with
        pre/post-conditions and invariants. Model problems with typechecks and range constraints.
      </>
    ),
    sample: readabilitySample
  },
  {
    title: "Correctness",
    Svg: require("@site/static/img/plane-svgrepo-com.svg").default,
    description: (
      <>Build with technology used in 40 years of reliability in planes, trains, and spaceships.</>
    ),
    extended_description: (
      <>
        Use the SPARK subset to formally verify part or all of your program, and integrate existing
        SPARK crates available in the Alire package manager.
      </>
    ),
    sample: correctnessSample
  },
  {
    title: "Performance",
    Svg: require("@site/static/img/chip-svgrepo-com.svg").default,
    description: (
      <>
        Build native applications and take advantage of other libraries through binding to C and
        C++.
      </>
    ),
    extended_description: (
      <>
        Use inline assembly or compiler intrinsics when you need it. Control resources with
        scope-based resource control (RAII) and your own memory allocators.
      </>
    ),
    sample: performanceSample
  }
]

function Feature({ Svg, title, description, extended_description, sample }) {
  return (
    <div className={clsx("col col--4")}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
        <p>{extended_description}</p>
      </div>
      <CodeBlock language="ada">{sample}</CodeBlock>
    </div>
  )
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  )
}
