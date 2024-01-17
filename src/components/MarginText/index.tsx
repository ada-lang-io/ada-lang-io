import React from "react"

import classes from "./index.module.scss"

interface MarginTextProps {
  readonly children: string
}

export default function MarginText({ children }: MarginTextProps): JSX.Element | null {
  if (!children) {
    return null
  }

  // Format is a/b_subclause
  const id = `p${children.replace("/", "_")}`
  return (
    <a id={id} href={`#${id}`} className={classes.text}>
      {children.split("_", 1)[0]}
    </a>
  )
}
