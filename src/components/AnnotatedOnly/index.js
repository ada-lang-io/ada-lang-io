import React from "react"

import classes from "./index.module.scss"

export default function AnnotatedOnly({ children }) {
  return <div className={classes.wrapper}>{children}</div>
}
