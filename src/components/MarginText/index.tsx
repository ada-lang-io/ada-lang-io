import React from "react"

import classes from "./index.module.scss"

interface MarginTextProps {
  readonly children: string
}

export default function MarginText({ children }: MarginTextProps): JSX.Element | null {
  if (!children) {
    return null
  }

  const id = `p${children.split("/", 1)[0]}`
  return (
    <a id={id} href={`#${id}`} className={classes.text}>
      {children}
    </a>
  )
}
