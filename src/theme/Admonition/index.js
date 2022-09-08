import React from "react"

import Admonition from "@theme-original/Admonition"

import styles from "./styles.module.scss"

// Possible types are: note, tip, info, caution, danger
const aarmTypes = {
  reason: "info",
  discussion: "info",
  "implementation-advice": "tip",
  "implementation-defined": "caution",
  "implementation-note": "caution"
}

export default function AdmonitionWrapper(props) {
  if (props.type === "aarm") {
    return (
      <div className={styles.aarm}>
        <Admonition {...props} type={aarmTypes[props.aarm] ?? "note"} />
      </div>
    )
  }
  return <Admonition {...props} />
}
