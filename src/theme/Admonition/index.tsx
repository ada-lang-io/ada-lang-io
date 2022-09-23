import React from "react"

import type { WrapperProps } from "@docusaurus/types"
import type AdmonitionType from "@theme/Admonition"

import Admonition from "@theme-original/Admonition"

import styles from "./styles.module.scss"

// Possible types are: note, tip, info, caution, danger
const aarmTypes: Record<string, string> = {
  reason: "info",
  discussion: "info",
  ramification: "note",
  "glossary-entry": "tip",
  proof: "info",
  correction: "caution",
  "implementation-advice": "tip",
  "implementation-defined": "caution",
  "implementation-note": "caution"
}

type OriginalAdmonitionWrapperProps = WrapperProps<typeof AdmonitionType>

interface ExtraAdmonitionWrapperProps {
  readonly aarm: string
  readonly type: OriginalAdmonitionWrapperProps["type"] | "aarm"
}

type Overwrite<T, U> = Omit<T, keyof U> & U
type AdmonitionWrapperProps = Overwrite<
  OriginalAdmonitionWrapperProps,
  ExtraAdmonitionWrapperProps
>

export default function AdmonitionWrapper(props: AdmonitionWrapperProps): JSX.Element {
  if (props.type === "aarm") {
    return (
      <div className={styles.aarm}>
        <Admonition
          {...props}
          type={aarmTypes[props.aarm] ?? "note"}
          title={aarmTypes[props.aarm] ? props.aarm.replace("-", " ") : props.title}
        />
      </div>
    )
  }
  return <Admonition {...props} />
}
