import React, { useEffect } from "react"

import { MdClose, MdOutlineInfo } from "react-icons/md"

import BrowserOnly from "@docusaurus/BrowserOnly"
import ExecutionEnvironment from "@docusaurus/ExecutionEnvironment"
import { translate } from "@docusaurus/Translate"
import { useLocation } from "@docusaurus/router"

import { Switch } from "@mantine/core"
import { useLocalStorage } from "@mantine/hooks"

import classes from "./ArmAnnotations.module.scss"

export default function ArmAnnotations() {
  // On the server we want to render the annotations, but in the browser
  // (when scripting is enabled), annotations are hidden because they add
  // a lot of noise to the text.
  const defaultValue = ExecutionEnvironment.canUseDOM
    ? Boolean(document.documentElement.getAttribute("data-arm-annotations") || false)
    : true

  const location = useLocation()
  const [checked, setChecked] = useLocalStorage({ key: "arm-annotations", defaultValue })

  useEffect(() => {
    document.documentElement.setAttribute("data-arm-annotations", checked)
  }, [checked])

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
    <BrowserOnly>
      {() => (
        <>
          {isOnReferenceManual && (
            <div title={title}>
              <Switch
                size="md"
                onLabel={<MdOutlineInfo size={18} />}
                offLabel={<MdClose size={18} />}
                aria-label={title}
                checked={checked}
                onChange={() => setChecked((value) => !value)}
                className={classes.control}
              />
            </div>
          )}
        </>
      )}
    </BrowserOnly>
  )
}
