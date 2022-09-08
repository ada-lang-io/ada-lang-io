import React from "react"

import Admonition from "@theme-original/Admonition"

import styles from "./styles.module.scss"

// Possible types are: note, tip, info, caution, danger
const aarmTypes = {
  reason: "info",
  discussion: "info",
  ramification: "note",
  "glossary-entry": "tip",
  correction: "caution",
  "implementation-advice": "tip",
  "implementation-defined": "caution",
  "implementation-note": "caution"
}

export default function AdmonitionWrapper(props) {
  if (props.type === "aarm") {
    return (
      <div className={styles.aarm}>
        <Admonition
          {...props}
          type={aarmTypes[props.aarm] ?? "note"}
          title={!!aarmTypes[props.aarm] ? props.aarm.replace("-", " ") : props.title}
        />
      </div>
    )
  }
  return <Admonition {...props} />
}
