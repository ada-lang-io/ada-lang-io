import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Readability',
    Svg: require('@site/static/img/student-reading-svgrepo-com.svg').default,
    description: (
      <>
        Express intent with explicitness and keywords over symbols and special structures.
      </>
    ),
    extended_description: (
      <>
      Express concepts like meaning in integers.
      Use built-in design by contract with pre/post-conditions and invariants.
      Model problems with typechecks and range constraints.   
      </>
    )
  },
  {
    title: 'Correctness',
    Svg: require('@site/static/img/plane-svgrepo-com.svg').default,
    description: (
      <>
        Build with technology used in 40 years of reliability in planes, trains, and spaceships.
      </>
    ),
    extended_description: (
      <>
        Use the SPARK subset to formally verify part or all of your program, and integrate existing SPARK
        crates available in the Alire package manager.
      </>
    )
  },
  {
    title: 'Performance',
    Svg: require('@site/static/img/chip-svgrepo-com.svg').default,
    description: (
      <>
        Build native applications and take advantage of other libraries
        through binding to C and C++.
      </>
    ),
    extended_description: (
      <>
        Use inline assembly or compiler intrinsics when you need it.
        Control resources with scope-based resource control (RAII) and your
        own memory allocators.
      </>
    )
  },
];

function Feature({Svg, title, description, extended_description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
        <p>{extended_description}</p>
      </div>
    </div>
  );
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
  );
}
