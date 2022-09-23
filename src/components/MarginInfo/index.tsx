import React from "react"

import InfoHover from "@site/src/components/InfoHover"

import classes from "./index.module.scss"

interface MarginInfoProps {
  readonly items: string[]
}

export default function MarginInfo({ items }: MarginInfoProps): JSX.Element {
  return (
    <div className={classes.wrapper}>
      <InfoHover text={items} width={150} />
    </div>
  )
}
