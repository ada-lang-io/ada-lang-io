import React from "react"

import classes from "./index.module.scss"

interface AnnotatedOnlyProps {
  readonly children: JSX.Element
}

export default function AnnotatedOnly({ children }: AnnotatedOnlyProps): JSX.Element {
  return <div className={classes.wrapper}>{children}</div>
}
