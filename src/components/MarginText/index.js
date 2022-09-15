import React from "react"

import classes from "./index.module.scss"

export default function MarginText({ children }) {
  return <span className={classes.text}>{children}</span>
}
