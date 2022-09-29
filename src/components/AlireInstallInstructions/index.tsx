import React, { useCallback, useState } from "react"

import { FaTerminal } from "react-icons/fa"
import { MdCode, MdDone, MdFileDownload } from "react-icons/md"

import Link from "@docusaurus/Link"
import { usePluginData } from "@docusaurus/useGlobalData"
import useIsBrowser from "@docusaurus/useIsBrowser"

import { Stack, Text, Timeline } from "@mantine/core"
import { useEventListener, useOs } from "@mantine/hooks"
import type { OS } from "@mantine/hooks"
import { Prism } from "@mantine/prism"

import { getInstallTarget, gitHubReleasePage, installTargets } from "@site/src/pages/index"
import type { Target } from "@site/src/pages/index"

import classes from "./index.module.scss"

const targetInstructions = new Map([
  [
    "macos",
    {
      download: (
        <>
          <Text color="dimmed">
            On macOS, remove the quarantine attribute to avoid getting a message suggesting to move
            the file to the bin because the developer cannot be verified:
          </Text>
          <Prism language="bash">xattr -d com.apple.quarantine bin/alr</Prism>
        </>
      )
    }
  ]
])

interface TimelineItemTextProps {
  readonly children: string
}

function TimelineItemText({ children }: TimelineItemTextProps): JSX.Element {
  return (
    <Text size="sm" className={classes.timelineItemTitle}>
      {children}
    </Text>
  )
}

// Return a ref that listens for click events on a button
function useCodeBlockClickRef(callback: () => void) {
  const eventCallback = useCallback(
    (e: MouseEvent) => {
      const isButton = (target: HTMLButtonElement, i: number): boolean =>
        (target.type === "button" && target.onclick !== null) ||
        (i > 0 &&
          target.parentNode !== null &&
          isButton(target.parentNode as HTMLButtonElement, i - 1))
      if (e.target !== null && isButton(e.target as HTMLButtonElement, 2)) {
        callback()
      }
    },
    [callback]
  )
  return useEventListener("click", eventCallback)
}

export default function AlireInstallInstructions(): JSX.Element {
  const isBrowser: boolean = useIsBrowser()
  const os: OS = useOs()
  const { alireVersion } = usePluginData("ada-lang-alire-version") as PluginDataAlireVersion

  const platformKey: OS | null = isBrowser && installTargets.has(os) ? os : null

  const platform: Target | null = installTargets.get(platformKey as string) ?? null
  const platformLabel: string = platform !== null ? ` for ${platform.label}` : ""

  const platformDownloadURL: string =
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

  const otherDownloadText: JSX.Element = (
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
        <Stack spacing="sm">
          <Text color="dimmed">
            Download{" "}
            <Link onClick={onClickDownloadLink} to={platformDownloadURL}>
              Alire {alireVersion}
              {platformLabel}
            </Link>
            {platform !== null && otherDownloadText}.
          </Text>
          {platformKey !== null && targetInstructions.get(platformKey as string)?.download}
        </Stack>
      </Timeline.Item>

      <Timeline.Item
        bullet={<FaTerminal size={12} />}
        title={<TimelineItemText>Install toolchain</TimelineItemText>}
      >
        <Stack spacing="sm">
          <Prism ref={refStep2} language="bash">
            alr toolchain --select
          </Prism>
          <Text color="dimmed">Select gnat_native and gprbuild.</Text>
        </Stack>
      </Timeline.Item>

      <Timeline.Item
        bullet={<MdCode size={16} />}
        title={<TimelineItemText>Start coding</TimelineItemText>}
      >
        <Stack spacing="sm">
          <Text color="dimmed">Create a crate:</Text>
          <Prism ref={refStep31} language="bash">
            alr init --bin mycrate && cd mycrate
          </Prism>
          <Text color="dimmed">Build the crate:</Text>
          <Prism ref={refStep32} language="bash">
            alr build
          </Prism>
        </Stack>
      </Timeline.Item>

      <Timeline.Item
        bullet={<MdDone size={16} />}
        title={<TimelineItemText>Run your application</TimelineItemText>}
      >
        <Stack spacing="sm">
          <Prism ref={refStep4} language="bash">
            alr run
          </Prism>
        </Stack>
      </Timeline.Item>
    </Timeline>
  )
}
