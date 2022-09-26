import React from "react"

import InfoHover from "@site/src/components/InfoHover"

import classes from "./index.module.scss"

const adaAuthCVSPrefix: string = "http://www.ada-auth.org/cgi-bin/cvsweb.cgi"

interface MarginInfoProps {
  readonly items: string[]
}

export default function MarginInfo({ items }: MarginInfoProps): JSX.Element {
  const lines: JSX.Element[] = items.map((text: string): JSX.Element => {
    if (text.startsWith("AI12")) {
      return <a href={`${adaAuthCVSPrefix}/AI12s/${text}.TXT`}>{text}</a>
    } else if (text.startsWith("AI05")) {
      return <a href={`${adaAuthCVSPrefix}/AI05s/${text}.TXT`}>{text}</a>
    } else if (text.startsWith("AI95")) {
      return <a href={`${adaAuthCVSPrefix}/AIs/AI-${text.substring(5, 10)}.TXT`}>{text}</a>
    } else {
      return <span>{text}</span>
    }
  })

  return (
    <div className={classes.wrapper}>
      <InfoHover text={lines} width={150} />
    </div>
  )
}
