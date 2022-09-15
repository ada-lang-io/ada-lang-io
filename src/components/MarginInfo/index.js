import React from "react"

import InfoHover from "@site/src/components/InfoHover"

import classes from "./index.module.scss"

export default function MarginInfo({ items }) {
  return (
    <div className={classes.wrapper}>
      <InfoHover text={items} width={150} />
    </div>
  )
}
