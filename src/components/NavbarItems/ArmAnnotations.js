import React from "react"

import { MdClose, MdOutlineInfo } from "react-icons/md"

import { translate } from "@docusaurus/Translate"
import { useLocation } from "@docusaurus/router"

import { Switch } from "@mantine/core"
import { useLocalStorage } from "@mantine/hooks"

import classes from "./ArmAnnotations.module.scss"

export default function ArmAnnotations() {
  const location = useLocation()
  const [checked, setChecked] = useLocalStorage({ key: "arm-annotations", defaultValue: false })

  const isOnReferenceManual = location.pathname.startsWith("/docs/arm")

  const title = translate(
    {
      message: "Show or hide annotations in the reference manual (currently {mode})",
      id: "app.armAnnotations.ariaLabel",
      description: "The ARIA label for the ARM annotations toggle"
    },
    {
      mode: checked
        ? translate({
            message: "visible",
            id: "app.armAnnotations.ariaLabel.mode.visible",
            description: "The state of visible annotations"
          })
        : translate({
            message: "hidden",
            id: "app.armAnnotations.ariaLabel.mode.hidden",
            description: "The state of hidden annotations"
          })
    }
  )

  return (
    <>
      {isOnReferenceManual && (
        <div title={title}>
          <Switch
            size="md"
            onLabel={<MdOutlineInfo size={18} stroke={3} />}
            offLabel={<MdClose size={18} stroke={3} />}
            aria-label={title}
            checked={checked}
            onChange={() => setChecked((value) => !value)}
            className={classes.control}
          />
        </div>
      )}
    </>
  )
}
