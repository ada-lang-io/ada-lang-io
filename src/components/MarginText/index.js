import React from "react"

import classes from "./index.module.scss"

export default function MarginText({ children }) {
  if (!children) {
    return null
  }

  const id = `p${children.split("/", 1)[0]}`
  return (
    <a name={id} href={`#${id}`} className={classes.text}>
      {children}
    </a>
  )
}
