import React, { useCallback, useState } from "react"

import { FaTerminal } from "react-icons/fa"
import { MdCode, MdDone, MdFileDownload } from "react-icons/md"

import Link from "@docusaurus/Link"
import useIsBrowser from "@docusaurus/useIsBrowser"

import { Text, Timeline } from "@mantine/core"
import { useEventListener, useOs } from "@mantine/hooks"
import { Prism } from "@mantine/prism"

import {
  alireVersion,
  getInstallTarget,
  gitHubReleasePage,
  installTargets
} from "@site/src/pages/index.js"

import classes from "./index.module.scss"

function TimelineItemText({ children }) {
  return (
    <Text size="sm" className={classes.timelineItemTitle}>
      {children}
    </Text>
  )
}

// Return a ref that listens for click events on a button
function useCodeBlockClickRef(callback) {
  const eventCallback = useCallback(
    (e) => {
      const isButton = (target) => target.type === "button" && target.onclick !== null
      if (
        isButton(e.target) ||
        isButton(e.target.parentNode) ||
        isButton(e.target.parentNode.parentNode)
      ) {
        callback()
      }
    },
    [callback]
  )
  return useEventListener("click", eventCallback)
}

export default function AlireInstallInstructions() {
  const isBrowser = useIsBrowser()
  const os = useOs()

  const platformKey = isBrowser && installTargets.has(os) ? os : null

  const platform = platformKey !== null ? installTargets.get(platformKey) : null
  const platformLabel = platform !== null ? ` for ${platform.label}` : ""

  const platformDownloadURL =
    platform !== null ? getInstallTarget(alireVersion, platform.urlSuffix) : gitHubReleasePage

  const [step, setStep] = useState(-1)

  const onClickDownloadLink = useCallback(() => {
    setStep(0)
  }, [setStep])

  const onClickButtonStep2 = useCallback(() => {
    setStep(1)
  }, [setStep])

  const onClickButtonStep3 = useCallback(() => {
    setStep(2)
  }, [setStep])

  const onClickButtonStep4 = useCallback(() => {
    setStep(3)
  }, [setStep])

  // Get some click event listeners. Assumes each <Prism> has only 1 button.
  // This is needed because there is no other way to detect clicks on the "Copy code" button
  const refStep2 = useCodeBlockClickRef(onClickButtonStep2)
  const refStep31 = useCodeBlockClickRef(onClickButtonStep2)
  const refStep32 = useCodeBlockClickRef(onClickButtonStep3)
  const refStep4 = useCodeBlockClickRef(onClickButtonStep4)

  const otherDownloadText = (
    <span>
      {" "}
      or view other options on the{" "}
      <Link onClick={onClickDownloadLink} to={gitHubReleasePage}>
        release page
      </Link>
    </span>
  )

  return (
    <Timeline active={step} bulletSize={32} lineWidth={3} className={classes.timeline}>
      <Timeline.Item
        bullet={<MdFileDownload size={16} />}
        title={<TimelineItemText>Download Alire</TimelineItemText>}
      >
        <Text color="dimmed">
          Download{" "}
          <Link onClick={onClickDownloadLink} to={platformDownloadURL}>
            Alire {alireVersion.slice(0)}
            {platformLabel}
          </Link>
          {platform !== null && otherDownloadText}.
        </Text>
      </Timeline.Item>

      <Timeline.Item
        bullet={<FaTerminal size={12} />}
        title={<TimelineItemText>Install toolchain</TimelineItemText>}
      >
        <Prism ref={refStep2} language="shell">
          alr toolchain --select
        </Prism>
        <Text color="dimmed">Select gnat_native and gprbuild.</Text>
      </Timeline.Item>

      <Timeline.Item
        bullet={<MdCode size={16} />}
        title={<TimelineItemText>Start coding</TimelineItemText>}
      >
        Create a crate:
        <Prism ref={refStep31} language="shell">
          alr init --bin mycrate && cd mycrate
        </Prism>
        Build the crate:
        <Prism ref={refStep32} language="shell">
          alr build
        </Prism>
      </Timeline.Item>

      <Timeline.Item
        bullet={<MdDone size={16} />}
        title={<TimelineItemText>Run your application</TimelineItemText>}
      >
        <Prism ref={refStep4} language="shell">
          alr run
        </Prism>
      </Timeline.Item>
    </Timeline>
  )
}
